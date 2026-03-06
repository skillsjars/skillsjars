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
        val groupIds = skillsJars.map(_.groupId.toString).toSet
        assertTrue(
          skillsJars.nonEmpty,
          groupIds.contains("com.skillsjars"),
          skillsJars.forall(sj => groupIds.contains(sj.groupId.toString)),
        )
    .provide(SkillsJarService.cacheLayer, loggingClient)
  ) @@ TestAspect.withLiveClock
