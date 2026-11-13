import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.{GroupArtifactVersion, MavenCentralRepo}
import com.jamesward.ziohttp.mcp.*
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.{Client, Response, Routes}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Downloads a SkillsJar's jar file. */
trait JarFetcher:
  def fetch(gav: GroupArtifactVersion, target: Path): IO[MavenCentral.NotFoundError | Throwable, Unit]

object JarFetcher:
  final case class Live(client: Client, repo: MavenCentralRepo) extends JarFetcher:
    def fetch(gav: GroupArtifactVersion, target: Path): IO[MavenCentral.NotFoundError | Throwable, Unit] =
      MavenCentral.jarUri(gav.groupId, gav.artifactId, gav.version)
        .flatMap(url => MavenCentral.downloadJar(url, target.toFile))
        .unit
        .provideEnvironment(ZEnvironment(client, repo))

  val live: URLayer[Client & MavenCentralRepo, JarFetcher] = ZLayer.fromFunction(Live(_, _))

/**
 * The latest version of every SkillsJar the site lists, served at `/mcp` over MCP's Skills
 * extension (`skills/list`, `skills/get`, `resources/directory/read`) and as resources
 * (`resources/list` has each `SKILL.md`, `resources/read` every skill file).
 *
 * Each skill is addressed by its directory in the jar, `skill://<org>/<repo>/<path>/SKILL.md`.
 * When two jars have the same directory, the first by coordinates wins and the conflict is
 * logged as an error.
 *
 * The skills are loaded when the catalog first differs from what was last loaded: the cache
 * key is the set of latest versions, so a new release reloads them.
 */
final case class SkillsMcp(cache: Cache[Set[GroupArtifactVersion], McpSkillsSourceError, SkillsMcp.Loaded])

object SkillsMcp:

  /** `complete` is false when a jar couldn't be fetched, so the load is retried sooner. */
  final case class Loaded(skills: McpSkillsJars, complete: Boolean)

  private val parallelism = 16
  private val incompleteTtl = 5.minutes

  val layer: URLayer[JarFetcher, SkillsMcp] =
    ZLayer.fromZIO:
      Cache.makeWith(1, Lookup(load)):
        case Exit.Success(loaded) => if loaded.complete then Duration.Infinity else incompleteTtl
        case Exit.Failure(_) => Duration.Zero
      .map(SkillsMcp(_))

  def latestVersions(skillsJars: Seq[Models.SkillsJar]): Set[GroupArtifactVersion] =
    skillsJars.flatMap(sj => SkillsJarService.latestOf(Seq(sj), sj.groupId, sj.artifactId).map(GroupArtifactVersion(sj.groupId, sj.artifactId, _))).toSet

  def current: ZIO[SkillsJarCache & SkillsMcp, McpSkillsSourceError, McpSkillsJars] =
    defer:
      val catalog = SkillsJarService.list
        .tapError(e => ZIO.logError(s"MCP skills: listing SkillsJars failed: $e"))
        .orElseFail(McpSkillsSourceError.Internal("SkillsJars could not be fetched from Maven Central"))
        .run
      ZIO.serviceWithZIO[SkillsMcp](_.cache.get(latestVersions(catalog))).run.skills

  private def load(gavs: Set[GroupArtifactVersion]): ZIO[JarFetcher, McpSkillsSourceError, Loaded] =
    ZIO.scoped:
      defer:
        val dir = tempDir.run
        val fetched = ZIO.foreachPar(Chunk.fromIterable(gavs).sortBy(_.toString))(fetchOne(dir, _)).withParallelism(parallelism).run
        val jars = fetched.flatten
        logConflicts(jars).run
        val skills = McpSkillsJars.load(jars.map(_._2)).run
        ZIO.logInfo(s"MCP skills: serving ${skills.entries.size} skill(s) from ${jars.size} of ${gavs.size} SkillsJar(s)").run
        Loaded(skills, fetched.forall(_.isDefined))
    .tapErrorCause(cause => ZIO.logErrorCause("MCP skills: loading SkillsJars failed", cause))
    .orElseFail(McpSkillsSourceError.Internal("SkillsJars could not be loaded"))

  /** Fetch `gav`'s jar into `dir`; None if that failed. */
  private def fetchOne(dir: Path, gav: GroupArtifactVersion): URIO[JarFetcher, Option[(GroupArtifactVersion, Path)]] =
    val jar = dir.resolve(s"${gav.groupId}-${gav.artifactId}-${gav.version}.jar").nn
    ZIO.serviceWithZIO[JarFetcher](_.fetch(gav, jar))
      .as(Some(gav -> jar))
      .catchAllCause(cause => ZIO.logWarningCause(s"MCP skills: fetching $gav failed; skipped", cause).as(None))

  /** A skill directory in more than one jar is served from the first jar (in `jars` order) only. */
  private def logConflicts(jars: Chunk[(GroupArtifactVersion, Path)]): Task[Unit] =
    ZIO.foreach(jars)((gav, jar) => SkillsJarLayout.entryNames(jar).map(names => gav -> SkillsJarLayout.skillRoots(names)))
      .flatMap: roots =>
        ZIO.foreachDiscard(SkillsJarLayout.conflicts(roots).toSeq.sortBy(_._1)): (root, owners) =>
          ZIO.logError(s"MCP skills: META-INF/skills/$root is in ${owners.mkString(", ")}; serving only ${owners.head}")

  private val tempDir: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attemptBlockingIO(Files.createTempDirectory("skillsjars-mcp").nn)): dir =>
      ZIO.attemptBlockingIO:
        val paths = Files.walk(dir).nn
        try paths.iterator.nn.asScala.toList.reverse.foreach(Files.deleteIfExists(_))
        finally paths.close()
      .ignoreLogged

  private def toolError(error: McpSkillsSourceError): ToolError = error match
    case McpSkillsSourceError.InvalidParams(m) => ToolError(m)
    case McpSkillsSourceError.Internal(m) => ToolError(m)

  private type Env = SkillsJarCache & SkillsMcp

  private val source: McpSkillsSource[Env] = new McpSkillsSource[Env]:
    def list(params: McpSkillsListParams, ctx: McpRequestContext) = current.flatMap(_.source.list(params, ctx))
    def get(uri: McpSkillUri, ctx: McpRequestContext) = current.flatMap(_.source.get(uri, ctx))

  private val directory: McpSkillsDirectorySource[Env] = new McpSkillsDirectorySource[Env]:
    def read(params: ResourceDirectoryReadParams, ctx: McpRequestContext) = current.flatMap(_.directory.read(params, ctx))

  private val resources: McpResourceSource[Env] = new McpResourceSource[Env]:
    // A failed load already logged its cause; the list is empty until a later load succeeds.
    def listResources(ctx: McpToolContext) = current.foldZIO(_ => ZIO.succeed(Chunk.empty), _.resources.listResources(ctx))
    def listResourceTemplates(ctx: McpToolContext) = ZIO.succeed(Chunk.empty)
    def readResource(uri: String, ctx: McpToolContext) = current.mapError(toolError).flatMap(_.resources.readResource(uri, ctx))

  private val instructions: InstructionsSource[Any] = InstructionsSource: ctx =>
    ZIO.succeed:
      Some:
        if ctx.extensionCapabilities.contains(McpSkills.Id) then
          "Every SkillsJar listed on https://www.skillsjars.com (the latest version of each) is served through the MCP Skills extension."
        else
          "Every SkillsJar listed on https://www.skillsjars.com (the latest version of each) is served as resources: " +
            "`resources/list` lists each skill's `SKILL.md`, and `resources/read` reads it and the files it refers to."

  val server: McpServer[Env] =
    McpServer("skillsjars", "1.0.0")
      .instructions(instructions)
      .withExtensions(McpSkills.withDirectory(source, directory))
      .resourceSource(resources)

  val routes: Routes[Env, Response] = server.statelessRoutes
