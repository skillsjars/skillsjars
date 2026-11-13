import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.*
import zio.test.*
import Models.*

object EndpointsSpec extends ZIOSpecDefault:

  private val sampleJar =
    SkillsJar(
      groupId = Models.groupId,
      artifactId = MavenCentral.ArtifactId("myorg__myrepo__myskill"),
      versions = Seq(MavenCentral.Version("2026_02_13-1af0a2e"), MavenCentral.Version("2026_01_01-0000000")),
      name = "My Skill",
      description = "Does a useful thing",
      securityScanned = true,
      allowedTools = Map("Bash" -> "git status"),
    )

  private val otherJar =
    sampleJar.copy(artifactId = MavenCentral.ArtifactId("other__repo__thing"), name = "Other", description = "Unrelated")

  private def cacheLayer(lookup: IO[SkillsJarService.ServiceError, Seq[SkillsJar]]): ULayer[SkillsJarCache] =
    ZLayer.fromZIO:
      Cache
        .makeWith(1, Lookup((_: String) => lookup)):
          case Exit.Success(_) => Duration.Infinity
          case Exit.Failure(_) => Duration.Zero
        .map(SkillsJarCache(_))

  private val fakeCacheLayer = cacheLayer(ZIO.succeed(Seq(sampleJar, otherJar)))

  private val failingCacheLayer = cacheLayer(ZIO.fail(SkillsJarService.ServiceError.FetchFailed("boom")))

  private def get(path: String, accept: Option[String]) =
    val request = Request.get(URL.decode(path).toOption.get)
    defer:
      val routes = App.appRoutes[Any](WebJarsDev()).sandbox
      val response = routes.runZIO(accept.fold(request)(request.addHeader("accept", _))).run
      val body = response.body.asString.run
      (response, body)

  private def contentType(response: Response): Option[String] =
    response.headers.rawHeader("content-type")

  def spec = suite("EndpointsSpec")(
    suite("search")(
      test("Accept: application/json returns matching SkillsJars as JSON"):
        defer:
          val (response, body) = get("/?q=useful", Some("application/json")).run
          assertTrue(
            response.status.code == 200,
            contentType(response).exists(_.contains("application/json")),
            body.contains(""""query":"useful""""),
            body.contains(""""artifactId":"myorg__myrepo__myskill""""),
            body.contains(""""versions":["2026_02_13-1af0a2e","2026_01_01-0000000"]"""),
            body.contains(""""allowedTools":{"Bash":"git status"}"""),
            !body.contains("other__repo__thing"),
          )
      ,
      test("JSON without a query lists all SkillsJars"):
        defer:
          val (_, body) = get("/", Some("application/json")).run
          assertTrue(
            body.contains("myorg__myrepo__myskill"),
            body.contains("other__repo__thing"),
          )
      ,
      test("a browser Accept header gets HTML"):
        defer:
          val (response, body) = get("/?q=useful&bt=gradle", Some("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")).run
          assertTrue(
            contentType(response).exists(_.contains("text/html")),
            body.startsWith("<!DOCTYPE html>"),
            body.contains("My Skill"),
            body.contains("runtimeOnly("),
          )
      ,
      // zio-http's Endpoint default when the header is absent; browsers and curl always send one.
      test("no Accept header gets JSON"):
        defer:
          val (response, _) = get("/", None).run
          assertTrue(contentType(response).exists(_.contains("application/json")))
      ,
      test("Accept: */* defaults to HTML"):
        defer:
          val (response, _) = get("/", Some("*/*")).run
          assertTrue(contentType(response).exists(_.contains("text/html")))
      ,
      test("an invalid build tool is a 400 with a JSON error"):
        defer:
          val (response, body) = get("/?bt=ant", Some("application/json")).run
          assertTrue(
            response.status.code == 400,
            contentType(response).exists(_.contains("application/json")),
            body == """{"error":"Invalid build tool: ant"}""",
          )
      ,
      test("an invalid build tool is a 400 error page for browsers"):
        defer:
          val (response, body) = get("/?bt=ant", Some("text/html")).run
          assertTrue(
            response.status.code == 400,
            contentType(response).exists(_.contains("text/html")),
            body.contains("Invalid build tool: ant"),
            !body.contains("DecodeError"),
          )
      ,
    ).provide(fakeCacheLayer, MockDeployer.layer, Client.default, Scope.default, DeployJobs.live, HerokuInferenceFake.layer, MavenCentral.MavenCentralRepo.live),
    suite("search failure")(
      test("JSON clients get a 502 with a JSON error"):
        defer:
          val (response, body) = get("/", Some("application/json")).run
          assertTrue(
            response.status.code == 502,
            contentType(response).exists(_.contains("application/json")),
            body == """{"error":"Failed to fetch SkillsJars from Maven Central"}""",
          )
      ,
      test("HTML clients get the error page"):
        defer:
          val (response, body) = get("/", Some("text/html")).run
          assertTrue(
            response.status.code == 502,
            body.contains("Failed to fetch SkillsJars"),
          )
      ,
    ).provide(failingCacheLayer, MockDeployer.layer, Client.default, Scope.default, DeployJobs.live, HerokuInferenceFake.layer, MavenCentral.MavenCentralRepo.live),
    suite("latest")(
      test("returns the latest version as JSON"):
        defer:
          val (response, body) = get("/com.skillsjars/myorg__myrepo__myskill/latest", None).run
          assertTrue(
            response.status.code == 200,
            contentType(response).exists(_.contains("application/json")),
            body == """{"groupId":"com.skillsjars","artifactId":"myorg__myrepo__myskill","version":"2026_02_13-1af0a2e"}""",
          )
      ,
      test("an unknown artifact is a 404 with a JSON error"):
        defer:
          val (response, body) = get("/com.skillsjars/nope/latest", Some("application/json")).run
          assertTrue(
            response.status.code == 404,
            body == """{"error":"No SkillsJar found for com.skillsjars:nope"}""",
          )
      ,
    ).provide(fakeCacheLayer, MockDeployer.layer, Client.default, Scope.default, DeployJobs.live, HerokuInferenceFake.layer, MavenCentral.MavenCentralRepo.live),
    suite("latestOf")(
      test("picks the head version of the matching SkillsJar"):
        assertTrue(
          SkillsJarService.latestOf(Seq(sampleJar, otherJar), Models.groupId, sampleJar.artifactId).contains(MavenCentral.Version("2026_02_13-1af0a2e")),
          SkillsJarService.latestOf(Seq(sampleJar), MavenCentral.GroupId("com.other"), sampleJar.artifactId).isEmpty,
          SkillsJarService.latestOf(Seq(sampleJar.copy(versions = Seq.empty)), Models.groupId, sampleJar.artifactId).isEmpty,
        )
    ),
  )
