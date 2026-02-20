import zio.*
import zio.direct.*
import zio.http.*
import zio.test.*

object SkillsJarServiceSpec extends ZIOSpecDefault:

  private val loggingClient: ZLayer[Any, Throwable, Client] =
    Client.default.map(env => ZEnvironment(env.get[Client] @@ ZClientAspect.requestLogging()))

  def spec = suite("SkillsJarServiceSpec")(
    test("fetches all skillsjars from Maven Central"):
      defer:
        val skillsJars = SkillsJarService.list.run
        assertTrue(
          skillsJars.nonEmpty,
          skillsJars.forall(_.groupId.toString == "com.skillsjars"),
        )
    .provide(SkillsJarService.cacheLayer, loggingClient)
  ) @@ TestAspect.withLiveClock
