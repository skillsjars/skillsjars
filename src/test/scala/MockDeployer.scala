import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.http.Client

import java.nio.file.Files


class MockDeployer(checkMavenCentral: Boolean = false) extends Deployer[Any]:

  var upload: Option[(String, Chunk[Byte])] = None

  override def upload(filename: String, bytes: Chunk[Byte]): ZIO[Any, DeployJobError, Unit] =
    ZIO.attempt:
      this.upload = Some(filename -> bytes)
      val tmpFile = Files.createTempFile("upload-", s"-$filename")
      Files.write(tmpFile, bytes.toArray)
    .debug
    .unit
    .mapError(e => DeployJobError.PublishFailed(e.getMessage))

  override def ascSign(toSign: Chunk[Byte]): IO[DeployJobError, Option[Chunk[Byte]]] =
    ZIO.systemWith(_.env("OSS_GPG_KEY")).flatMap:
      case Some(gpgKey) =>
        ZIO.systemWith(_.env("OSS_GPG_PASS")).flatMap: maybeGpgPass =>
          Deployer.ascSignWith(gpgKey, maybeGpgPass)(toSign).asSome
      case None =>
        ZIO.none
    .mapError(e => DeployJobError.PublishFailed(s"Signing failed: ${e.getMessage}"))

  override def checkArtifactExists(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Client & Scope, Nothing, Boolean] =
    if checkMavenCentral then super.checkArtifactExists(groupId, artifactId, version)
    else ZIO.succeed(false)

object MockDeployer:
  val layer: ZLayer[Any, Nothing, Deployer[Any]] = ZLayer.succeed(MockDeployer())
  val layerWithCheck: ZLayer[Any, Nothing, Deployer[Any]] = ZLayer.succeed(MockDeployer(checkMavenCentral = true))
