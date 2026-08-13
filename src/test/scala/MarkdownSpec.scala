import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.*
import zio.test.*
import Models.*

object MarkdownSpec extends ZIOSpecDefault:

  private val sampleJar =
    SkillsJar(
      groupId = Models.groupId,
      artifactId = MavenCentral.ArtifactId("myorg__myrepo__myskill"),
      versions = Seq(MavenCentral.Version("2026_02_13-1af0a2e"), MavenCentral.Version("2026_01_01-0000000")),
      name = "My Skill",
      description = "Does a useful thing",
      securityScanned = true,
    )

  // A cache preloaded with a fixed skill so the GET / route test does not hit Maven Central.
  private val fakeCacheLayer: ULayer[SkillsJarCache] =
    ZLayer.fromZIO:
      Cache
        .makeWith(1, Lookup((_: String) => ZIO.succeed(Seq(sampleJar)): IO[SkillsJarService.ServiceError, Seq[SkillsJar]])):
          case Exit.Success(_) => Duration.Infinity
          case Exit.Failure(_) => Duration.Zero
        .map(SkillsJarCache(_))

  def spec = suite("MarkdownSpec")(
    suite("accepts")(
      test("detects text/markdown in an Accept header"):
        assertTrue(
          Markdown.accepts(Some("text/markdown")),
          Markdown.accepts(Some("text/html, text/markdown;q=0.9")),
          Markdown.accepts(Some("text/x-markdown")),
          !Markdown.accepts(Some("text/html")),
          !Markdown.accepts(Some("application/json")),
          !Markdown.accepts(None),
        )
    ),
    suite("skillsList")(
      test("renders name, description, and coordinates using the latest version"):
        val md = Markdown.skillsList(Seq(sampleJar), None)
        assertTrue(
          md.startsWith("# SkillsJars"),
          md.contains("## My Skill"),
          md.contains("Does a useful thing"),
          md.contains("`com.skillsjars:myorg__myrepo__myskill:2026_02_13-1af0a2e`"),
        )
      ,
      test("reflects the search query and empty results"):
        val md = Markdown.skillsList(Seq.empty, Some("foo"))
        assertTrue(
          md.contains("""matching "foo""""),
          md.contains("No SkillsJars found"),
        )
    ),
    suite("setup")(
      test("covers all three build tools, dependency selection, and AGENTS.md"):
        val md = Markdown.setup
        assertTrue(
          md.contains("id(\"com.skillsjars.gradle-plugin\") version \"0.0.2\""),
          md.contains("<artifactId>maven-plugin</artifactId>"),
          md.contains("<version>0.0.7</version>"),
          md.contains("addSbtPlugin(\"com.skillsjars\" % \"skillsjars-sbt-plugin\" % \"0.0.9\")"),
          md.contains("./mvnw skillsjars:extract -Ddir="),
          md.contains("./gradlew extractSkillsJars -Pdir="),
          md.contains("./sbt extractSkillsJars"),
          md.contains("otherwise `mvn`"),
          md.contains("`PATH`"),
          md.contains("\"$MVN\""),
          md.contains("Accept: text/markdown"),
          md.contains("AGENTS.md"),
          md.contains("javadocs.dev"),
          md.contains("/latest"),
        )
    ),
    suite("routes")(
      test("GET /setup returns markdown instructions"):
        defer:
          val routes = App.appRoutes[Any](WebJarsDev()).sandbox
          val response = routes.runZIO(Request.get(URL.decode("/setup").toOption.get)).run
          val body = response.body.asString.run
          assertTrue(
            response.status.code == 200,
            response.headers.rawHeader("content-type").exists(_.contains("text/markdown")),
            body.contains("# Setting up SkillsJars in your project"),
          )
      ,
      test("GET / with Accept: text/markdown returns the skills list as markdown"):
        val request = Request.get(URL.decode("/").toOption.get).addHeader("accept", "text/markdown")
        defer:
          val routes = App.appRoutes[Any](WebJarsDev()).sandbox
          val response = routes.runZIO(request).run
          val body = response.body.asString.run
          assertTrue(
            response.headers.rawHeader("content-type").exists(_.contains("text/markdown")),
            body.startsWith("# SkillsJars"),
            body.contains("`com.skillsjars:myorg__myrepo__myskill:2026_02_13-1af0a2e`"),
          )
      ,
      test("GET / without a markdown Accept header still returns HTML"):
        val request = Request.get(URL.decode("/").toOption.get).addHeader("accept", "text/html")
        defer:
          val routes = App.appRoutes[Any](WebJarsDev()).sandbox
          val response = routes.runZIO(request).run
          val body = response.body.asString.run
          assertTrue(
            response.headers.rawHeader("content-type").exists(_.contains("text/html")),
            body.contains("<!DOCTYPE html>") || body.contains("<html"),
            body.contains("Set up SkillsJars in your AI agent"),
            body.contains("Setup SkillsJars in my project by following the instructions at: https://skillsjars.com/setup"),
          )
      ,
      test("GET /docs shows updated plugin version and agent instructions"):
        defer:
          val routes = App.appRoutes[Any](WebJarsDev()).sandbox
          val response = routes.runZIO(Request.get(URL.decode("/docs").toOption.get)).run
          val body = response.body.asString.run
          assertTrue(
            body.contains("0.0.7"),
            !body.contains("0.0.6"),
            body.contains("For AI agents"),
            body.contains("/setup"),
            body.contains("Accept: text/markdown"),
          )
    ).provide(MockDeployer.layer, fakeCacheLayer, Client.default, Scope.default, DeployJobs.live, HerokuInferenceFake.layer, MavenCentral.MavenCentralRepo.live)
  )
