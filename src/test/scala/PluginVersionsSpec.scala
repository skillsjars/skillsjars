import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.{MavenCentralRepo, Version}
import zio.*
import zio.http.*
import zio.test.*

object PluginVersionsSpec extends ZIOSpecDefault:

  /** Serves `maven-metadata.xml` for the artifacts in `versions`; anything else is a 404. */
  final case class FakeRepo(versions: Map[String, Seq[String]]) extends MavenCentralRepo:
    def request(path: Path, method: Method, headers: Headers, content: Body): ZIO[Any, Throwable, (Response, URL)] =
      val segments = path.segments.toList
      val response = segments.reverse match
        case "maven-metadata.xml" :: artifactId :: _ =>
          versions.get(artifactId).fold(Response.status(Status.NotFound)): vs =>
            Response.text(s"<metadata><versioning><versions>${vs.map(v => s"<version>$v</version>").mkString}</versions></versioning></metadata>")
        case _ => Response.status(Status.NotFound)
      ZIO.succeed(response -> URL.empty)

  private def repo(versions: Map[String, Seq[String]]): ULayer[MavenCentralRepo] = ZLayer.succeed(FakeRepo(versions))

  def spec = suite("PluginVersionsSpec")(
    test("looks up the latest release of each plugin after startup"):
      val expected = PluginVersions(Version("9.0.0"), Version("9.1.0"), Version("9.2.0"))
      PluginVersions.get.repeatUntil(_ != PluginVersions.fallback)
        .map(found => assertTrue(found == expected))
        .provide(
          PluginVersions.live,
          repo(Map(
            "maven-plugin" -> Seq("0.0.1", "9.0.0", "10.0.0-RC1"),
            "gradle-plugin" -> Seq("9.1.0"),
            "skillsjars-sbt-plugin_2.12_1.0" -> Seq("9.2.0"),
          )),
        )
    ,
    test("a plugin whose lookup fails keeps its fallback version"):
      PluginVersions.get.repeatUntil(_.maven != PluginVersions.fallback.maven)
        .map(found => assertTrue(found == PluginVersions.fallback.copy(maven = Version("9.0.0"))))
        .provide(PluginVersions.live, repo(Map("maven-plugin" -> Seq("9.0.0"))))
    ,
    test("pages show the fallback versions until a lookup succeeds"):
      PluginVersions.get
        .map(found => assertTrue(found == PluginVersions.fallback))
        .provide(PluginVersions.live, repo(Map.empty))
    ,
  ) @@ TestAspect.timeout(30.seconds)
