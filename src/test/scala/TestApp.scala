import zio.*
import zio.http.*

object TestApp extends ZIOAppDefault:

  def run =
    Server.serve(App.appRoutes(WebJarsDev())).provide(
      Server.default,
      Client.default,
      MockDeployer.layer,
      SkillsJarService.cacheLayer,
    )
