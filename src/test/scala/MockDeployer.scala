import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.*

case class PublishedArtifact(
  groupId: MavenCentral.GroupId,
  artifactId: MavenCentral.ArtifactId,
  version: MavenCentral.Version,
  jar: Array[Byte],
  pom: String,
)

object MockDeployer:

  def layer: ZLayer[Any, Nothing, Deployer & ConcurrentMap[String, PublishedArtifact]] =
    ZLayer.fromZIO:
      ConcurrentMap.empty[String, PublishedArtifact].map: published =>
        val deployer = new Deployer:
          override def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
                               jar: Array[Byte], pom: String): IO[DeployError, Unit] =
            val key = s"$groupId:$artifactId:$version"
            published.put(key, PublishedArtifact(groupId, artifactId, version, jar, pom)).unit
        (deployer, published)
    .flatMap: env =>
      val (deployer, published) = env.get[(Deployer, ConcurrentMap[String, PublishedArtifact])]
      ZLayer.succeed(deployer) ++ ZLayer.succeed(published)
