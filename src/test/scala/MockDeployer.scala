import Models.*
import zio.*

import java.nio.file.Files


class MockDeployer extends Deployer[Any]:

  var upload: Option[(String, Chunk[Byte])] = None

  override def upload(filename: String, bytes: Chunk[Byte]): ZIO[Any, DeployError, Unit] =
    upload = Some(filename -> bytes)
    ZIO.attempt:
      val tmpFile = Files.createTempFile("upload-", s"-$filename")
      Files.write(tmpFile, bytes.toArray)
    .mapError(e => DeployError.PublishFailed(s"Failed to write tmp file: ${e.getMessage}"))
    .debug
    .unit

  override def ascSign(toSign: Chunk[Byte]): IO[DeployError, Option[Chunk[Byte]]] =
    ZIO.systemWith(_.env("OSS_GPG_KEY")).flatMap:
      case Some(gpgKey) =>
        ZIO.systemWith(_.env("OSS_GPG_PASS")).flatMap: maybeGpgPass =>
          Deployer.ascSignWith(gpgKey, maybeGpgPass)(toSign).asSome
      case None =>
        ZIO.none
    .catchAll:
      case e: DeployError => ZIO.fail(e)
      case e: Throwable => ZIO.fail(DeployError.PublishFailed(e.getMessage))

object MockDeployer:
  val layer = ZLayer.succeed(MockDeployer())

