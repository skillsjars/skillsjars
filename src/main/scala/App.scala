import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.*

object App extends ZIOAppDefault:

  private def indexHandler(request: Request, tailwind: URL): ZIO[SkillsJarCache, SkillsJarService.ServiceError, Response] =
    defer:
      val maybeQuery = request.queryParam("q").filter(_.nonEmpty)
      val skillsJars = maybeQuery.fold(SkillsJarService.list)(SkillsJarService.search).run
      Response.html(UI.index(skillsJars, maybeQuery, tailwind))


  private def deployHandler[A : Tag](request: Request, tailwind: URL): ZIO[Deployer[A] & A, DeployError, Response] =
    defer:
      val deployer = ZIO.service[Deployer[A]].run
      val form = request.body.asURLEncodedForm.mapError(e => DeployError.PublishFailed(e.getMessage)).run
      val org = ZIO.fromOption(form.get("org").flatMap(_.stringValue).map(Org(_)))
        .orElseFail(DeployError.PublishFailed("Missing org parameter")).run
      val repo = ZIO.fromOption(form.get("repo").flatMap(_.stringValue).map(Repo(_)))
        .orElseFail(DeployError.PublishFailed("Missing repo parameter")).run
      val outcome = ZIO.scoped(deployer.deploy(org, repo)).run
      val successes = outcome.published.toSeq.map: gav =>
        DeployResult.Success(gav.groupId, gav.artifactId, gav.version)
      val skipped = outcome.skipped.map: s =>
        DeployResult.Skipped(s.skillName, s.reason)
      Response.html(UI.deployResult(successes ++ skipped, tailwind))

  def appRoutes[A : Tag](webJars: WebJars): Routes[Deployer[A] & A & SkillsJarCache, Nothing] =
    val tailwind = webJars.url("tailwindcss__browser", "/dist/index.global.js")

    Routes(
      Method.GET / Root -> Handler.fromFunctionZIO[Request]: request =>
        indexHandler(request, tailwind).catchAll: error =>
          ZIO.succeed(Response.html(UI.index(Seq.empty, None, tailwind, Some(error))))
      ,
      Method.POST / "deploy" -> Handler.fromFunctionZIO[Request]: request =>
        deployHandler[A](request, tailwind).catchAll: error =>
          ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(error)), tailwind)))
      ,
    ) ++ webJars.routes

  private val serverLayer =
    ZLayer.fromZIO:
      ZIO.systemWith(_.env("PORT")).map: maybePort =>
        maybePort.flatMap(_.toIntOption).fold(Server.default)(Server.defaultWithPort)
    .flatten

  def run =
    Server.serve(appRoutes[MavenCentral.Deploy.Sonatype](WebJarsLive())).provide(
      serverLayer,
      Client.default,
      MavenCentral.Deploy.Sonatype.Live,
      Deployer.live,
      SkillsJarService.cacheLayer,
    )
