import zio.*
import zio.direct.*
import zio.http.*
import zio.test.*
import Models.*

object DeployHandlerSpec extends ZIOSpecDefault:
  def spec = suite("DeployHandlerSpec")(
    test("deployHandler should parse form body instead of query params"):
      val body = Body.fromURLEncodedForm(Form(FormField.textField("org", "myorg"), FormField.textField("repo", "myrepo")))
      val request = Request.post(URL.decode("/deploy").toOption.get, body)

      defer:
        val response = App.appRoutes(WebJarsDev()).runZIO(request).run
        val bodyString = response.body.asString.run

        assertTrue(
          !bodyString.contains("Missing org parameter"),
          !bodyString.contains("Missing repo parameter"),
        )
    .provide(MockDeployer.layer, SkillsJarService.cacheLayer, Client.default, Scope.default)
    ,
    // todo: this test assumes the artifact has been deployed already which wouldn't be the case if there is a new upstream commit
    //       so we should make this more stable with a hidden version param or something
    test("deploy of existing artifacts shows duplicate versions"):
      val body = Body.fromURLEncodedForm(Form(FormField.textField("org", "anthropics"), FormField.textField("repo", "skills")))
      val request = Request.post(URL.decode("/deploy").toOption.get, body)

      defer:
        val response = App.appRoutes(WebJarsDev()).runZIO(request).run
        val bodyString = response.body.asString.run

        assertTrue(
          bodyString.contains("Duplicate version"),
          bodyString.contains("algorithmic-art"),
        )
    .provide(MockDeployer.layerWithCheck, SkillsJarService.cacheLayer, Client.default, Scope.default)
  )
