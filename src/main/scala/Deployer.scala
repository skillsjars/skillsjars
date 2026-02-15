import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*

import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}

import java.io.ByteArrayOutputStream
import java.security.MessageDigest
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}

trait Deployer:
  def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
              jar: Array[Byte], pom: String): IO[DeployError, Unit]

object Deployer:

  val live: ZLayer[MavenCentral.Deploy.Sonatype, Nothing, Deployer] =
    ZLayer.fromZIO:
      // todo: use defer syntax
      for
        gpgKey <- ZIO.systemWith(_.env("OSS_GPG_KEY")).someOrFail(Throwable("OSS_GPG_KEY not set")).orDie
        gpgPass <- ZIO.systemWith(_.env("OSS_GPG_PASS")).orDie
        sonatype <- ZIO.service[MavenCentral.Deploy.Sonatype]
      yield LiveDeployer(gpgKey, gpgPass, sonatype)

  private class LiveDeployer(
    gpgKey: String,
    maybeGpgPass: Option[String],
    sonatype: MavenCentral.Deploy.Sonatype,
  ) extends Deployer:

    override def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
                         jar: Array[Byte], pom: String): IO[DeployError, Unit] =
      val pomBytes = pom.getBytes("UTF-8")
      // todo: defer syntax
      for
        jarAsc <- ZIO.attempt(ascSign(gpgKey, maybeGpgPass.getOrElse(""), jar))
          .mapError(e => DeployError.PublishFailed(s"GPG signing failed: ${e.getMessage}"))
        pomAsc <- ZIO.attempt(ascSign(gpgKey, maybeGpgPass.getOrElse(""), pomBytes))
          .mapError(e => DeployError.PublishFailed(s"GPG signing failed: ${e.getMessage}"))
        gav = MavenCentral.GroupArtifactVersion(groupId, artifactId, version)
        path = artifactPath(gav)
        files = Map(
          s"$path.jar" -> jar,
          s"$path.jar.sha1" -> sha1(jar).getBytes,
          s"$path.jar.md5" -> md5(jar).getBytes,
          s"$path.jar.asc" -> jarAsc.getBytes,
          s"$path.pom" -> pomBytes,
          s"$path.pom.sha1" -> sha1(pomBytes).getBytes,
          s"$path.pom.md5" -> md5(pomBytes).getBytes,
          s"$path.pom.asc" -> pomAsc.getBytes,
        )
        zip = createZip(files)
        filename = s"$groupId:$artifactId:$version.zip"
        _ <- MavenCentral.Deploy.uploadVerifyAndPublish(filename, zip)
          .provideEnvironment(ZEnvironment(sonatype))
          .mapError(e => DeployError.PublishFailed(e.getMessage))
      yield ()

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

    // todo: use zio-streams-compress-zip
    private def createZip(files: Map[String, Array[Byte]]): Array[Byte] =
      val baos = ByteArrayOutputStream()
      val zos = ZipOutputStream(baos)
      files.foreach: (filename, content) =>
        zos.putNextEntry(ZipEntry(filename))
        zos.write(content)
        zos.closeEntry()
      zos.close()
      baos.toByteArray
