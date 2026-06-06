import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.Deploy.Sonatype
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import zio.*
import zio.direct.*
import zio.http.Client
import zio.compress.*
import zio.stream.*

import java.io.File
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

trait Deployer[Env]:

  def upload(filename: String, bytes: Chunk[Byte]): ZIO[Env, DeployJobError, Unit]

  def ascSign(toSign: Chunk[Byte]): IO[DeployJobError, Option[Chunk[Byte]]]

  def checkArtifactExists(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[MavenCentralRepo, Nothing, Boolean] =
    MavenCentral.artifactExists(groupId, artifactId, version)
      .catchAll(_ => ZIO.succeed(false))

  def validateSkill(org: Org, repo: Repo, version: MavenCentral.Version, location: SkillLocation,
                    skillDir: File, repoLicenses: List[License]): ZIO[MavenCentralRepo, SkillError, ValidatedSkill] =
    defer:
      val skillFile = java.io.File(skillDir, "SKILL.md")
      val content = ZStream
        .fromPath(skillFile.toPath)
        .runCollect
        .mapBoth(
          e => SkillError.InvalidSkillMd(s"Failed to read SKILL.md: ${e.getMessage}"),
          chunk => String(chunk.toArray, StandardCharsets.UTF_8),
        )
        .run

      val meta = SkillParser.parse(content).run
      val groupId = Models.groupId
      val artifactId = artifactIdFor(location.org, location.repo, location.path).run
      val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, version)

      val exists = checkArtifactExists(gav.groupId, gav.artifactId, gav.version).run
      ZIO.fail(SkillError.DuplicateVersion(gav)).when(exists).run

      val skillDirLicenses = GitService.detectLicenses(skillDir)
      val skillLevelLicenses = meta.licenses ++ skillDirLicenses
      val resolvedList =
        if skillLevelLicenses.nonEmpty then skillLevelLicenses
        else if repoLicenses.nonEmpty then repoLicenses
        else
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
        .orElseFail(SkillError.NoLicense).run

      ValidatedSkill(location, skillDir, meta, gav, licenses)

  def deployValidated(org: Org, repo: Repo, version: MavenCentral.Version,
                      skills: List[ValidatedSkill]): ZIO[Env & Scope & Client, DeployJobError, Map[SkillName, SkillResult]] =
    def artifactBasePath(gav: MavenCentral.GroupArtifactVersion): String =
      val groupPath = gav.groupId.toString.replace('.', '/')
      s"$groupPath/${gav.artifactId}/${gav.version}/${gav.artifactId}-${gav.version}"

    def hexDigest(algorithm: String, data: Chunk[Byte]): Chunk[Byte] =
      val digest = MessageDigest.getInstance(algorithm).digest(data.toArray)
      val hex = digest.iterator.map(b => f"$b%02x").mkString
      Chunk.fromArray(hex.getBytes(StandardCharsets.UTF_8))

    def sha1(data: Chunk[Byte]): Chunk[Byte] = hexDigest("SHA-1", data)
    def md5(data: Chunk[Byte]): Chunk[Byte] = hexDigest("MD5", data)

    def buildArtifacts(validated: ValidatedSkill): ZIO[Client & Scope, SkillError, (MavenCentral.GroupArtifactVersion, Chunk[Byte], Chunk[Byte])] =
      defer:
        val pom = PomGenerator.generate(validated.gav.groupId, validated.gav.artifactId, version,
          validated.meta.name, validated.meta.description, validated.licenses,
          validated.location.org, validated.location.repo, validated.meta.allowedTools)
        val jar = JarCreator.create(validated.skillDir, validated.location.org, validated.location.repo,
          validated.location.path, pom, validated.gav.groupId, validated.gav.artifactId, version)
          .mapError(e => SkillError.InvalidSkillMd(s"JAR creation failed: ${e.getMessage}")).run
        (validated.gav, pom, jar)

    defer:
      val resultsRef = Ref.make(Map.empty[SkillName, SkillResult]).run

      val zipBytes =
        ZStream
          .fromIterable(skills)
          .mapZIO: validated =>
            buildArtifacts(validated).foldZIO(
              error =>
                resultsRef.update(_ + (validated.location.skillName -> SkillResult.Skipped(error))).as(Chunk.empty),
              { case (gav, pom, jar) =>
                defer:
                  val maybePomAsc = ascSign(pom).run
                  val maybeJarAsc = ascSign(jar).run

                  resultsRef.update(_ + (validated.location.skillName -> SkillResult.Success(gav))).run

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
              }
            )
          .flatMap(ZStream.fromChunk)
          .map: (name, content) =>
            (ArchiveEntry(name = name), ZStream.fromChunk(content))
          .via(ZipArchiver.archive.mapError(t => DeployJobError.PublishFailed(s"Failed to create ZIP: ${t.getMessage}")))
          .runCollect
          .run

      val results = resultsRef.get.run

      val hasPublished = results.values.exists { case SkillResult.Success(_) => true; case _ => false }
      if !hasPublished then
        val skipped = results.collect { case (name, SkillResult.Skipped(error)) => name -> error }
        ZIO.fail(DeployJobError.NoPublishableSkills(skipped)).run

      val filename = s"$org-$repo-${version}.zip"
      upload(filename, zipBytes).run

      results


class LiveDeployer(
  signer: MavenCentral.Signer,
  sonatype: MavenCentral.Deploy.Sonatype,
) extends Deployer[Sonatype]:

  override def upload(filename: String, bytes: Chunk[Byte]): ZIO[Sonatype, DeployJobError, Unit] =
    MavenCentral.Deploy.uploadVerifyAndPublish(filename, bytes.toArray)
      .mapError(e => DeployJobError.PublishFailed(e.getMessage))

  override def ascSign(toSign: Chunk[Byte]): IO[DeployJobError, Option[Chunk[Byte]]] =
    signer.ascSign(toSign)
      .mapError(e => DeployJobError.PublishFailed(s"Signing failed: ${e.getMessage}"))


object Deployer:

  private val signerLayer: ZLayer[Any, Nothing, MavenCentral.Signer] =
    ZLayer.fromZIO:
      defer:
        val gpgKey = ZIO.systemWith(_.env("OSS_GPG_KEY")).someOrFail(Throwable("OSS_GPG_KEY not set")).orDie.run
        val gpgPass = ZIO.systemWith(_.env("OSS_GPG_PASS")).orDie.run
        (gpgKey, gpgPass)
    .flatMap: env =>
      val (gpgKey, gpgPass) = env.get
      MavenCentral.Signer.make(gpgKey, gpgPass).orDie

  val live: ZLayer[Sonatype, Nothing, Deployer[Sonatype]] =
    ZLayer.makeSome[Sonatype, Deployer[Sonatype]](
      signerLayer,
      ZLayer.fromFunction(LiveDeployer(_, _)),
    )
