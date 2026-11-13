import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.MavenCentralRepo
import zio.*
import zio.direct.*
import zio.http.*

object App extends ZIOAppDefault:

  private def searchHandler(maybeQuery: Option[String], maybeBuildTool: Option[BuildTool]): ZIO[SkillsJarCache, SkillsJarService.ServiceError, Endpoints.SearchResults] =
    val query = maybeQuery.filter(_.nonEmpty)
    // Maven is the UI's default build tool selection.
    val buildTool = maybeBuildTool.getOrElse(BuildTool.Maven)
    query.fold(SkillsJarService.list)(SkillsJarService.search)
      .map(Endpoints.SearchResults(query, _, buildTool))
      .tapErrorCause(cause => ZIO.logErrorCause(s"search failed q=$query", cause))

  private def latestHandler(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[SkillsJarCache, SkillsJarService.ServiceError | SkillsJarService.NotFound, Endpoints.LatestVersion] =
    SkillsJarService.latest(groupId, artifactId)
      .map(Endpoints.LatestVersion(groupId, artifactId, _))
      .tapErrorCause: cause =>
        cause.failureOption match
          case Some(_: SkillsJarService.NotFound) => ZIO.logWarning(s"latest not found $groupId:$artifactId")
          case _ => ZIO.logErrorCause(s"latest failed $groupId:$artifactId", cause)

  private def deployHandler[A : Tag](request: Request, tailwind: URL): ZIO[DeployJobs & Deployer[A] & A & Client & MavenCentralRepo & HerokuInference, Nothing, Response] =
    defer:
      val deployJobs = ZIO.service[DeployJobs].run
      val formResult = request.body.asURLEncodedForm.mapError(e => s"Invalid form: ${e.getMessage}").run
      val org = ZIO.fromOption(formResult.get("org").flatMap(_.stringValue).map(Org(_)))
        .orElseFail("Missing org parameter").run
      val repo = ZIO.fromOption(formResult.get("repo").flatMap(_.stringValue).map(Repo(_)))
        .orElseFail("Missing repo parameter").run
      val maybeVersion = formResult.get("version").flatMap(_.stringValue).map(MavenCentral.Version(_))
      val version = deployJobs.start[A](org, repo, maybeVersion).run
      deployJobs.get(org, repo, version).run match
        case Some(DeployJobStatus.Done(results)) =>
          Response.html(UI.deployDone(results, tailwind))
        case Some(DeployJobStatus.Failed(error)) =>
          Response.html(UI.deployJobError(error, tailwind))
        case _ =>
          Response.seeOther(zio.http.URL.decode(s"/deploy/$org/$repo/$version").toOption.get)
    .catchAll:
      case error: DeployError => ZIO.succeed(Response.html(UI.deployError(error, tailwind)))
      case reason: String => ZIO.succeed(Response.html(UI.formError(reason, tailwind)))

  private def deployStatusHandler(org: String, repo: String, version: String, tailwind: URL): ZIO[DeployJobs, Nothing, Response] =
    defer:
      val deployJobs = ZIO.service[DeployJobs].run
      deployJobs.get(Org(org), Repo(repo), MavenCentral.Version(version)).run match
        case Some(DeployJobStatus.Running) =>
          Response.html(UI.deployInProgress(org, repo, tailwind))
        case Some(DeployJobStatus.Done(results)) =>
          Response.html(UI.deployDone(results, tailwind))
        case Some(DeployJobStatus.Failed(error)) =>
          Response.html(UI.deployJobError(error, tailwind))
        case None =>
          Response.html(UI.deployNotFound(org, repo, tailwind))

  def appRoutes[A : Tag](webJars: WebJars): Routes[DeployJobs & Deployer[A] & A & SkillsJarCache & Client & MavenCentralRepo & HerokuInference, Nothing] =
    val tailwind = webJars.url("tailwindcss__browser", "/dist/index.global.js")

    Routes(
      Endpoints.search(tailwind).implement(searchHandler),
      Endpoints.latest.implement(latestHandler),
      Method.GET / "setup" -> handler(Markdown.response(Markdown.setup)),
      Method.GET / "docs" -> handler(Response.html(UI.docs(tailwind))),
      Method.GET / "favicon.ico" -> Handler.fromResource("public/favicon.ico").orDie,
      Method.GET / "favicon.png" -> Handler.fromResource("public/favicon.png").orDie,
      Method.POST / "deploy" -> Handler.fromFunctionZIO[Request]: request =>
        deployHandler[A](request, tailwind)
      ,
      Method.GET / "deploy" / string("org") / string("repo") / string("version") -> handler: (org: String, repo: String, version: String, _: Request) =>
        deployStatusHandler(org, repo, version, tailwind)
      ,
    ) ++ webJars.routes

  private val serverLayer =
    ZLayer.fromZIO:
      ZIO.systemWith(_.env("PORT")).map: maybePort =>
        maybePort.flatMap(_.toIntOption).fold(Server.default)(Server.defaultWithPort)
    .flatten

  private val loggingClient: ZLayer[Any, Throwable, Client] =
    Client.default.map(env => ZEnvironment(env.get[Client] @@ ZClientAspect.requestLogging()))

  def run =
    Server.serve(appRoutes[MavenCentral.Deploy.Sonatype](WebJarsLive())).provide(
      serverLayer,
      loggingClient,
      MavenCentral.Deploy.Sonatype.Live,
      MavenCentral.MavenCentralRepo.live,
      Deployer.live,
      SkillsJarService.cacheLayer,
      DeployJobs.live,
      HerokuInference.live,
    )
