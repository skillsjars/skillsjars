import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*

import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}

import zio.compress.*
import zio.stream.*

import java.io.ByteArrayOutputStream
import java.security.MessageDigest
import java.util.Base64

trait Deployer:
  def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
              jar: Array[Byte], pom: String): IO[DeployError, Unit]

object Deployer:

  def deployRepo(org: Org, repo: Repo): ZIO[Deployer & Scope, DeployError, GitService.RepoInfo] =
    defer:
      val repoInfo = GitService.cloneAndScan(org, repo).run
      val deployer = ZIO.service[Deployer].run
      ZIO.foreach(repoInfo.skills): (location, skillDir) =>
        defer:
          val content = ZIO.attemptBlocking(String(java.nio.file.Files.readAllBytes(java.io.File(skillDir, "SKILL.md").toPath))).orDie.run
          val meta = SkillParser.parse(java.io.File(skillDir, "SKILL.md").getPath, content).run
          val groupId = groupIdFor(location.org, location.repo, location.subPath)
          val artifactId = artifactIdFor(location.skillName)
          val version = repoInfo.version
          val license = meta.maybeLicense.orElse(repoInfo.maybeLicense)
          val pom = PomGenerator.generate(groupId, artifactId, version, meta.name, meta.description, license, org, repo)
          val jar = JarCreator.create(skillDir, org, repo, location.skillName, pom, groupId, artifactId, version, location.subPath).orDie.run
          deployer.publish(groupId, artifactId, version, jar, pom).run
      .run
      repoInfo

  val live: ZLayer[MavenCentral.Deploy.Sonatype, Nothing, Deployer] =
    ZLayer.fromZIO:
      defer:
        val gpgKey = ZIO.systemWith(_.env("OSS_GPG_KEY")).someOrFail(Throwable("OSS_GPG_KEY not set")).orDie.run
        val gpgPass = ZIO.systemWith(_.env("OSS_GPG_PASS")).orDie.run
        val sonatype = ZIO.service[MavenCentral.Deploy.Sonatype].run
        LiveDeployer(gpgKey, gpgPass, sonatype)

  private class LiveDeployer(
    gpgKey: String,
    maybeGpgPass: Option[String],
    sonatype: MavenCentral.Deploy.Sonatype,
  ) extends Deployer:

    override def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
                         jar: Array[Byte], pom: String): IO[DeployError, Unit] =
      val pomBytes = pom.getBytes("UTF-8")
      defer:
        val jarAsc = ZIO.attempt(ascSign(gpgKey, maybeGpgPass.getOrElse(""), jar))
          .mapError(e => DeployError.PublishFailed(s"GPG signing failed: ${e.getMessage}")).run
        val pomAsc = ZIO.attempt(ascSign(gpgKey, maybeGpgPass.getOrElse(""), pomBytes))
          .mapError(e => DeployError.PublishFailed(s"GPG signing failed: ${e.getMessage}")).run
        val gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, version)
        val path = artifactPath(gav)
        val files = ZIO.succeed:
          Map(
            s"$path.jar" -> jar,
            s"$path.jar.sha1" -> sha1(jar).getBytes,
            s"$path.jar.md5" -> md5(jar).getBytes,
            s"$path.jar.asc" -> jarAsc.getBytes,
            s"$path.pom" -> pomBytes,
            s"$path.pom.sha1" -> sha1(pomBytes).getBytes,
            s"$path.pom.md5" -> md5(pomBytes).getBytes,
            s"$path.pom.asc" -> pomAsc.getBytes,
          )
        .run
        val zip = createZip(files).mapError(e => DeployError.PublishFailed(s"ZIP creation failed: ${e.getMessage}")).run
        val filename = s"$groupId:$artifactId:$version.zip"
        MavenCentral.Deploy.uploadVerifyAndPublish(filename, zip)
          .provideEnvironment(ZEnvironment(sonatype))
          .mapError(e => DeployError.PublishFailed(e.getMessage)).run

    private def artifactPath(gav: MavenCentral.GroupArtifactVersion): String =
      val groupPath = gav.groupId.toString.replace('.', '/')
      s"$groupPath/${gav.artifactId}/${gav.version}/${gav.artifactId}-${gav.version}"

    private def ascSign(gpgKeyBase64: String, passphrase: String, toSign: Array[Byte]): String =
      // todo: make this a zio with managed resources
      val keyBytes = Base64.getDecoder.decode(gpgKeyBase64)
      val secretKeyRing = PGPSecretKeyRing(keyBytes, JcaKeyFingerprintCalculator())
      val pass = passphrase.toCharArray
      val privKey = secretKeyRing.getSecretKey.extractPrivateKey(JcePBESecretKeyDecryptorBuilder().build(pass))
      val signerBuilder = JcaPGPContentSignerBuilder(secretKeyRing.getPublicKey().getAlgorithm, HashAlgorithmTags.SHA1)
      val sGen = PGPSignatureGenerator(signerBuilder, secretKeyRing.getPublicKey())
      sGen.init(PGPSignature.BINARY_DOCUMENT, privKey)
      val bos = ByteArrayOutputStream()
      val armor = ArmoredOutputStream(bos)
      val bOut = BCPGOutputStream(armor)
      sGen.update(toSign)
      sGen.generate().encode(bOut)
      bOut.close()
      armor.close()
      bos.close()
      String(bos.toByteArray)

    private def sha1(data: Array[Byte]): String =
      MessageDigest.getInstance("SHA-1").digest(data).map("%02x".format(_)).mkString

    private def md5(data: Array[Byte]): String =
      MessageDigest.getInstance("MD5").digest(data).map("%02x".format(_)).mkString

    private def createZip(files: Map[String, Array[Byte]]): ZIO[Any, Throwable, Array[Byte]] =
      ZStream.fromIterable(files)
        .map: (name, content) =>
          (ArchiveEntry(name = name), ZStream.fromChunk(Chunk.fromArray(content)))
        .via(ZipArchiver.archive)
        .runCollect
        .map(_.toArray)
