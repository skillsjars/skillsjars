import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.*

object MockDeployer:

  def layer: ZLayer[Any, Nothing, Deployer & ConcurrentMap[String, MavenCentral.GroupArtifactVersion]] =
    ZLayer.fromZIO:
      ConcurrentMap.empty[String, MavenCentral.GroupArtifactVersion].map: published =>
        val deployer = new Deployer:
          override def publish(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version,
                               jar: Array[Byte], pom: String): IO[DeployError, Unit] =
            val key = s"$groupId:$artifactId:$version"
            published.put(key, MavenCentral.GroupArtifactVersion(groupId, artifactId, version)).unit
        (deployer, published)
    .flatMap: env =>
      val (deployer, published) = env.get[(Deployer, ConcurrentMap[String, MavenCentral.GroupArtifactVersion])]
      ZLayer.succeed(deployer) ++ ZLayer.succeed(published)
