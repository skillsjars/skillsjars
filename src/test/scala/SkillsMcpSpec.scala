import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifactVersion
import com.jamesward.ziohttp.mcp.*
import com.jamesward.ziohttp.mcp.client.*
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.*
import zio.test.*

import java.nio.file.{Files, Path}

object SkillsMcpSpec extends ZIOSpecDefault:

  private val latest = MavenCentral.Version("2026_02_13-1af0a2e")
  private val older = MavenCentral.Version("2026_01_01-0000000")

  private def skillsJar(org: String, repo: String, skill: String): SkillsJar =
    SkillsJar(Models.groupId, MavenCentral.ArtifactId(s"${org}__${repo}__$skill"), Seq(latest, older), skill, s"$skill description", true)

  private val mySkill = skillsJar("myorg", "myrepo", "myskill")
  private val brokenSkill = skillsJar("broken", "repo", "gone")
  // Same skill directory as `mySkill`, published under another groupId, which sorts after it.
  private given CanEqual[zio.LogLevel, zio.LogLevel] = CanEqual.derived

  private val conflictingSkill = mySkill.copy(groupId = MavenCentral.GroupId("org.other"))

  /** Builds each jar with [[JarCreator]], its `SKILL.md` naming the version; fails for `broken`. */
  final case class FakeJarFetcher(fetched: Ref[Chunk[GroupArtifactVersion]]) extends JarFetcher:
    def fetch(gav: GroupArtifactVersion, target: Path): IO[MavenCentral.NotFoundError | Throwable, Unit] =
      fetched.update(_ :+ gav) *>
        (gav.artifactId.toString.split("__").toList match
          case List("broken", _, _) => ZIO.fail(MavenCentral.NotFoundError(gav.groupId, gav.artifactId, gav.version))
          case List(org, repo, skill) => writeJar(Org(org), Repo(repo), skill, gav, target)
          case _ => ZIO.fail(IllegalArgumentException(s"not a SkillsJar artifactId: ${gav.artifactId}")))

    private def writeJar(org: Org, repo: Repo, skill: String, gav: GroupArtifactVersion, target: Path): Task[Unit] =
      defer:
        val skillDir = ZIO.attemptBlockingIO(Files.createTempDirectory("skill").nn).run
        ZIO.attemptBlockingIO(Files.writeString(skillDir.resolve("SKILL.md"), s"---\nname: $skill\ndescription: version ${gav.version}\n---\n\nDo the thing.\n")).run
        val jar = JarCreator.create(skillDir.toFile, org, repo, List(skill), Chunk.empty, gav.groupId, gav.artifactId, gav.version).run
        ZIO.attemptBlockingIO(Files.write(target, jar.toArray)).unit.run

  private val fetchedLayer = ZLayer.fromZIO(Ref.make(Chunk.empty[GroupArtifactVersion]))

  private def catalogLayer(skillsJars: Seq[SkillsJar]): ULayer[SkillsJarCache] =
    ZLayer.fromZIO:
      Cache
        .makeWith(1, Lookup((_: String) => ZIO.succeed(skillsJars): IO[SkillsJarService.ServiceError, Seq[SkillsJar]])):
          case Exit.Success(_) => Duration.Infinity
          case Exit.Failure(_) => Duration.Zero
        .map(SkillsJarCache(_))

  private def config(port: Int, version: ProtocolVersion) =
    McpClientConfig(s"http://localhost:$port/mcp", preferredVersion = version)

  private val skillUri = McpSkillUri.parse("skill://myorg/myrepo/myskill/SKILL.md").toOption.get

  private def env(skillsJars: Seq[SkillsJar]) =
    ZLayer.make[SkillsJarCache & SkillsMcp & Ref[Chunk[GroupArtifactVersion]] & Server & Client](
      catalogLayer(skillsJars),
      fetchedLayer,
      ZLayer.fromFunction(FakeJarFetcher(_)),
      SkillsMcp.layer,
      Server.defaultWith(_.onAnyOpenPort),
      Client.default,
    )

  def spec = suite("SkillsMcpSpec")(
    test("serves the latest version of each SkillsJar over the Skills extension"):
      defer:
        val port = Server.install(SkillsMcp.routes).run
        val skills = McpSkillsClient.connect(config(port, ProtocolVersion.V2026_07_28)).run
        val listed = skills.list().run.skills
        val text = skills.readSkill(skillUri).run.flatMap(_.text)
        assertTrue(
          listed.map(_.uri) == Chunk(skillUri),
          text.exists(_.contains(s"version $latest")),
        )
      .provideSome[Scope](env(Seq(mySkill)))
    ,
    test("a client without the Skills extension finds each SKILL.md in resources/list"):
      defer:
        val port = Server.install(SkillsMcp.routes).run
        val client = McpClient.connect(config(port, ProtocolVersion.V2025_06_18), McpClientExtensions.empty).run
        val resources = client.listResources.run
        val read = client.readResource(skillUri.value).run.flatMap(_.text)
        assertTrue(
          resources.map(_.uri) == Chunk(skillUri.value),
          read.exists(_.contains("Do the thing.")),
        )
      .provideSome[Scope](env(Seq(mySkill)))
    ,
    test("a SkillsJar that can't be fetched is skipped and the rest are served"):
      defer:
        val listed = SkillsMcp.current.run.entries
        assertTrue(listed.map(_.uri) == Chunk(skillUri))
      .provide(env(Seq(brokenSkill, mySkill)))
    ,
    test("a skill directory in two SkillsJars is served once, from the first, and logged as an error"):
      defer:
        val skills = SkillsMcp.current.run
        val errors = ZTestLogger.logOutput.run.filter(_.logLevel == zio.LogLevel.Error).map(_.message())
        assertTrue(
          skills.entries.map(_.uri) == Chunk(skillUri),
          errors.exists(m => m.contains("META-INF/skills/myorg/myrepo/myskill") && m.contains("serving only com.skillsjars/")),
        )
      .provide(env(Seq(conflictingSkill, mySkill)))
    ,
    test("jars are fetched once while the catalog is unchanged"):
      defer:
        SkillsMcp.current.run
        SkillsMcp.current.run
        val fetched = ZIO.serviceWithZIO[Ref[Chunk[GroupArtifactVersion]]](_.get).run
        assertTrue(fetched == Chunk(GroupArtifactVersion(mySkill.groupId, mySkill.artifactId, latest)))
      .provide(env(Seq(mySkill)))
    ,
  ) @@ TestAspect.withLiveClock
