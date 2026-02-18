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
//        _ <- ZIO.debug(s"Body: $bodyString")
        
        // Since we are using MockDeployer, it should succeed or fail depending on what's mocked.
        // In MockDeployer (from DeploySpec), it seems to handle "anthropics/skills".
        // If it was trying to get from query params, it would fail with "Missing org parameter"
        assertTrue(
          !bodyString.contains("Missing org parameter"),
          !bodyString.contains("Missing repo parameter"),
        )
  ).provide(
    MockDeployer.layer,
    SkillsJarService.cacheLayer,
    Client.default,
    Scope.default
  )
