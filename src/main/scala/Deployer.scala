import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.Deploy.Sonatype
import zio.*
import zio.direct.*
import zio.http.Client
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import zio.compress.*
import zio.stream.*

import java.io.{ByteArrayOutputStream, File, PipedInputStream, PipedOutputStream}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64

trait Deployer[Env]:

  def upload(filename: String, bytes: Chunk[Byte]): ZIO[Env, DeployError, Unit]

  def ascSign(toSign: Chunk[Byte]): IO[DeployError, Option[Chunk[Byte]]]

  protected def checkArtifactExists(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Client & Scope, DeployError, Boolean] =
    MavenCentral.artifactExists(groupId, artifactId, version)
      .catchAll(_ => ZIO.succeed(false))

  def skillsJar(version: MavenCentral.Version, licenses: NonEmptyChunk[License])(skillLocation: SkillLocation, skillDir: File): ZIO[Any, DeployError, (MavenCentral.GroupArtifactVersion, Chunk[Byte], Chunk[Byte])] =
    defer:
      ZIO.debug(skillLocation -> skillDir).run
      val content = ZStream.fromPath(java.io.File(skillDir, "SKILL.md").toPath)
        .runCollect
        .mapBoth(e => DeployError.PublishFailed(s"Failed to read SKILL.md: ${e.getMessage}"), chunk => String(chunk.toArray, "UTF-8"))
        .run
      val meta = SkillParser.parse(java.io.File(skillDir, "SKILL.md").getPath, content).run
      val groupId = Models.groupId
      val artifactId = artifactIdFor(skillLocation.org, skillLocation.repo, skillLocation.path).run
      val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, version)

      val pom = PomGenerator.generate(groupId, artifactId, version, meta.name, meta.description, licenses, skillLocation.org, skillLocation.repo)
      val jar = JarCreator.create(skillDir, skillLocation.org, skillLocation.repo, skillLocation.path, pom, groupId, artifactId, version).run

      (gav, pom, jar)

  def deployFrom(org: Org, repo: Repo, version: MavenCentral.Version, repoInfo: GitService.RepoInfo): ZIO[Env & Scope & Client, DeployError, DeployOutcome] =
    def artifactBasePath(gav: MavenCentral.GroupArtifactVersion): String =
      val groupPath = gav.groupId.toString.replace('.', '/')
      s"$groupPath/${gav.artifactId}/${gav.version}/${gav.artifactId}-${gav.version}"

    def hexDigest(algorithm: String, data: Chunk[Byte]): Chunk[Byte] =
      val digest = MessageDigest.getInstance(algorithm).digest(data.toArray)
      val hex = digest.iterator.map(b => f"$b%02x").mkString
      Chunk.fromArray(hex.getBytes(StandardCharsets.UTF_8))

    def sha1(data: Chunk[Byte]): Chunk[Byte] =
      hexDigest("SHA-1", data)

    def md5(data: Chunk[Byte]): Chunk[Byte] =
      hexDigest("MD5", data)

    def skillFiles(gavsRef: Ref[Set[MavenCentral.GroupArtifactVersion]], repoInfo: GitService.RepoInfo)(location: SkillLocation, skillDir: File): ZIO[Scope, DeployError, Chunk[(String, Chunk[Byte])]] =
      defer:
        val skillFile = java.io.File(skillDir, "SKILL.md")
        val content = ZStream
          .fromPath(skillFile.toPath)
          .runCollect
          .mapBoth(
            e => DeployError.PublishFailed(s"Failed to read SKILL.md: ${e.getMessage}"),
            chunk => String(chunk.toArray, StandardCharsets.UTF_8),
          )
          .run

        val meta = SkillParser.parse(skillFile.getPath, content).run
        val groupId = Models.groupId
        val artifactId = artifactIdFor(location.org, location.repo, location.path).run
        val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, version)

        val skillDirLicenses = GitService.detectLicenses(skillDir)
        val skillLevelLicenses = meta.licenses ++ skillDirLicenses
        val resolvedList =
          if skillLevelLicenses.nonEmpty then skillLevelLicenses
          else if repoInfo.licenses.nonEmpty then repoInfo.licenses
          else
            // Fallback: check for a LICENSE file with non-SPDX content
            val repoUrl = s"https://github.com/$org/$repo"
            GitService.findLicenseFile(skillDir) match
              case Some(fileName) =>
                val licenseName = meta.rawLicense.getOrElse("See LICENSE file")
                if location.path.isEmpty then
                  List(License(licenseName, s"$repoUrl/blob/main/$fileName"))
                else
                  List(License(licenseName, s"$repoUrl/blob/main/${location.path.mkString("/")}/$fileName"))
              case None => Nil
        val licenses = ZIO.fromOption(NonEmptyChunk.fromIterableOption(resolvedList))
          .orElseFail(DeployError.NoLicense(org, repo, location.skillName)).run
        val pom = PomGenerator.generate(groupId, artifactId, version, meta.name, meta.description, licenses, org, repo)
        val jar = JarCreator.create(skillDir, org, repo, location.path, pom, groupId, artifactId, version).run

        val maybePomAsc = ascSign(pom).run
        val maybeJarAsc = ascSign(jar).run

        gavsRef.update(_ + gav).run

        val base = artifactBasePath(gav)

        val files: Chunk[(String, Chunk[Byte])] =
          Chunk(
            s"$base.pom" -> pom,
            s"$base.pom.sha1" -> sha1(pom),
            s"$base.pom.md5" -> md5(pom),
            s"$base.jar" -> jar,
            s"$base.jar.sha1" -> sha1(jar),
            s"$base.jar.md5" -> md5(jar),
          ) ++
            maybePomAsc.map(asc => Chunk(s"$base.pom.asc" -> asc)).getOrElse(Chunk.empty) ++
            maybeJarAsc.map(asc => Chunk(s"$base.jar.asc" -> asc)).getOrElse(Chunk.empty)

        files

    defer:
      // Check all skills for existing artifacts upfront
      val gavsByLocation = ZIO.foreach(repoInfo.skills): (location, _) =>
        artifactIdFor(location.org, location.repo, location.path)
          .map(aid => location -> MavenCentral.GroupArtifactVersion(Models.groupId, aid, version))
      .run

      val duplicates = ZIO.filter(gavsByLocation): (_, gav) =>
        checkArtifactExists(gav.groupId, gav.artifactId, gav.version)
      .run.map(_._2).toSet

      val duplicateLocations = gavsByLocation.filter((_, gav) => duplicates.contains(gav)).map(_._1).toSet
      val skillsToProcess = repoInfo.skills.filterNot((location, _) => duplicateLocations.contains(location))

      val gavsRef = Ref.make(Set.empty[MavenCentral.GroupArtifactVersion]).run
      val skippedRef = Ref.make(List.empty[SkippedSkill]).run

      val zipBytes =
        ZStream
          .fromIterable(skillsToProcess)
          .mapZIO: (location, skillDir) =>
            skillFiles(gavsRef, repoInfo)(location, skillDir).catchSome:
              case DeployError.NoLicense(_, _, skillName) =>
                val reason = "No license found. Add a LICENSE file or specify license in SKILL.md frontmatter."
                skippedRef.update(_ :+ SkippedSkill(skillName, reason)).as(Chunk.empty)
          .flatMap(ZStream.fromChunk)
          .map: (name, content) =>
            (ArchiveEntry(name = name), ZStream.fromChunk(content))
          .via(ZipArchiver.archive.mapError(t => DeployError.PublishFailed(s"Failed to create ZIP: ${t.getMessage}")))
          .runCollect
          .run

      val published = gavsRef.get.run
      val skipped = skippedRef.get.run

      if published.nonEmpty then
        val filename = s"$org-$repo-${version}.zip"
        upload(filename, zipBytes).run

      DeployOutcome(published, skipped, duplicates)


class LiveDeployer(
  gpgKey: String,
  maybeGpgPass: Option[String],
  sonatype: MavenCentral.Deploy.Sonatype,
) extends Deployer[Sonatype]:

  override def upload(filename: String, bytes: Chunk[Byte]): ZIO[Sonatype, DeployError, Unit] =
    MavenCentral.Deploy.uploadVerifyAndPublish(filename, bytes.toArray)
      .mapError(e => DeployError.PublishFailed(e.getMessage))

  override def ascSign(toSign: Chunk[Byte]): IO[DeployError, Option[Chunk[Byte]]] =
    Deployer.ascSignWith(gpgKey, maybeGpgPass)(toSign).asSome


object Deployer:

  private def signError(e: Throwable): DeployError =
    DeployError.PublishFailed(s"Signing failed: ${e.getMessage}")

  private final class ChunkBuilderOutputStream(builder: ChunkBuilder[Byte]) extends java.io.OutputStream:
    override def write(b: Int): Unit =
      builder += b.toByte

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      var i = off
      val end = off + len
      while i < end do
        builder += b(i)
        i += 1

  def ascSignWith(gpgKey: String, gpgPass: Option[String])(toSign: Chunk[Byte]): IO[DeployError, Chunk[Byte]] =
    ZIO
      .attempt:
        val keyBytes = Base64.getDecoder.decode(gpgKey)
        val secretKeyRing = PGPSecretKeyRing(keyBytes, JcaKeyFingerprintCalculator())
        val pass = gpgPass.getOrElse("").toCharArray
        val privKey = secretKeyRing.getSecretKey.extractPrivateKey(JcePBESecretKeyDecryptorBuilder().build(pass))
        val signerBuilder = JcaPGPContentSignerBuilder(secretKeyRing.getPublicKey().getAlgorithm, HashAlgorithmTags.SHA256)
        val sGen = PGPSignatureGenerator(signerBuilder, secretKeyRing.getPublicKey())
        sGen.init(PGPSignature.BINARY_DOCUMENT, privKey)
        sGen
      .mapError(e => DeployError.PublishFailed(s"GPG initialization failed: ${e.getMessage}"))
      .flatMap: sGen =>
        ZIO.attempt:
          val builder = ChunkBuilder.make[Byte]()
          val rawOut = ChunkBuilderOutputStream(builder)
          val armor = ArmoredOutputStream(rawOut)
          val bOut = BCPGOutputStream(armor)
          sGen.update(toSign.toArray)
          sGen.generate().encode(bOut)
          bOut.close()
          armor.close()
          builder.result()
        .mapError(signError)

  val live: ZLayer[Sonatype, Nothing, Deployer[Sonatype]] =
    ZLayer.fromZIO:
      defer:
        val gpgKey = ZIO.systemWith(_.env("OSS_GPG_KEY")).someOrFail(Throwable("OSS_GPG_KEY not set")).orDie.run
        val gpgPass = ZIO.systemWith(_.env("OSS_GPG_PASS")).orDie.run
        val sonatype = ZIO.service[MavenCentral.Deploy.Sonatype].run
        LiveDeployer(gpgKey, gpgPass, sonatype)

