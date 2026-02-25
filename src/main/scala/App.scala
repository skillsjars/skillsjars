import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.*

object App extends ZIOAppDefault:

  private def indexHandler(request: Request, buildTool: BuildTool, tailwind: URL): ZIO[SkillsJarCache, SkillsJarService.ServiceError, Response] =
    defer:
      val maybeQuery = request.queryParam("q").filter(_.nonEmpty)
      val skillsJars = maybeQuery.fold(SkillsJarService.list)(SkillsJarService.search).run
      Response.html(UI.index(skillsJars, maybeQuery, buildTool, tailwind))


  private def deployHandler[A : Tag](request: Request, tailwind: URL): ZIO[DeployJobs & Deployer[A] & A & Client, DeployError, Response] =
    defer:
      val deployJobs = ZIO.service[DeployJobs].run
      val form = request.body.asURLEncodedForm.mapError(e => DeployError.PublishFailed(e.getMessage)).run
      val org = ZIO.fromOption(form.get("org").flatMap(_.stringValue).map(Org(_)))
        .orElseFail(DeployError.PublishFailed("Missing org parameter")).run
      val repo = ZIO.fromOption(form.get("repo").flatMap(_.stringValue).map(Repo(_)))
        .orElseFail(DeployError.PublishFailed("Missing repo parameter")).run
      val version = deployJobs.start[A](org, repo).run
      // If the job already completed, render results directly instead of redirecting to the spinner
      deployJobs.get(org, repo, version).run match
        case Some(DeployJobStatus.Done(results)) =>
          Response.html(UI.deployResult(results, tailwind))
        case Some(DeployJobStatus.Failed(error)) =>
          Response.html(UI.deployResult(Seq(DeployResult.Failure(error)), tailwind))
        case _ =>
          Response.seeOther(zio.http.URL.decode(s"/deploy/$org/$repo/$version").toOption.get)

  private def deployStatusHandler(org: String, repo: String, version: String, tailwind: URL): ZIO[DeployJobs, Nothing, Response] =
    defer:
      val deployJobs = ZIO.service[DeployJobs].run
      deployJobs.get(Org(org), Repo(repo), MavenCentral.Version(version)).run match
        case Some(DeployJobStatus.Running) =>
          Response.html(UI.deployInProgress(org, repo, tailwind))
        case Some(DeployJobStatus.Done(results)) =>
          Response.html(UI.deployResult(results, tailwind))
        case Some(DeployJobStatus.Failed(error)) =>
          Response.html(UI.deployResult(Seq(DeployResult.Failure(error)), tailwind))
        case None =>
          Response.html(UI.deployResult(Seq(DeployResult.Failure(DeployError.PublishFailed("Unknown deploy job"))), tailwind))

  def appRoutes[A : Tag](webJars: WebJars): Routes[DeployJobs & Deployer[A] & A & SkillsJarCache & Client, Nothing] =
    val tailwind = webJars.url("tailwindcss__browser", "/dist/index.global.js")

    Routes(
      Method.GET / Root -> Handler.fromFunctionZIO[Request]: request =>
        val buildTool = BuildTool.fromParam(request.queryParam("bt").getOrElse("maven"))
        indexHandler(request, buildTool, tailwind).catchAll: error =>
          ZIO.succeed(Response.html(UI.index(Seq.empty, None, buildTool, tailwind, Some(error))))
      ,
      Method.GET / "docs" -> handler(Response.html(UI.docs(tailwind))),
      Method.GET / "favicon.ico" -> Handler.fromResource("public/favicon.ico").orDie,
      Method.GET / "favicon.png" -> Handler.fromResource("public/favicon.png").orDie,
      Method.POST / "deploy" -> Handler.fromFunctionZIO[Request]: request =>
        deployHandler[A](request, tailwind).catchAll: error =>
          ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(error)), tailwind)))
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
      Deployer.live,
      SkillsJarService.cacheLayer,
      DeployJobs.live,
    )
