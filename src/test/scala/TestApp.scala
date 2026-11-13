import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.http.*

object TestApp extends ZIOAppDefault:

  def run =
    Server.serve(App.appRoutes[Any](WebJarsDev()) ++ SkillsMcp.routes).provide(
      Server.default,
      Client.default,
      MockDeployer.layerWithCheck,
      SkillsJarService.cacheLayer,
      JarFetcher.live,
      SkillsMcp.layer,
      PluginVersions.live,
      DeployJobs.live,
      HerokuInference.live.orElse(HerokuInferenceFake.layer),
      MavenCentral.MavenCentralRepo.live,
    )
