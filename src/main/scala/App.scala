import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.*

object App extends ZIOAppDefault:

  private def indexHandler(request: Request, tailwind: URL): ZIO[SkillsJarCache, Throwable, Response] =
    defer:
      val maybeQuery = request.url.queryParams.getAll("q").headOption.filter(_.nonEmpty)
      val skillsJars = maybeQuery.fold(SkillsJarService.list)(SkillsJarService.search).run
      Response.html(UI.index(skillsJars, maybeQuery, tailwind))

  private def deployHandler(request: Request, tailwind: URL): ZIO[Deployer, DeployError, Response] =
    defer:
      val org = ZIO.fromOption(request.url.queryParams.getAll("org").headOption.map(Org(_)))
        .orElseFail(DeployError.PublishFailed("Missing org parameter")).run
      val repo = ZIO.fromOption(request.url.queryParams.getAll("repo").headOption.map(Repo(_)))
        .orElseFail(DeployError.PublishFailed("Missing repo parameter")).run
      val repoInfo = ZIO.scoped(Deployer.deployRepo(org, repo)).run
      val results = repoInfo.skills.map: (location, _) =>
        val groupId = groupIdFor(location.org, location.repo, location.subPath)
        val artifactId = artifactIdFor(location.skillName)
        DeployResult.Success(groupId, artifactId, repoInfo.version)
      Response.html(UI.deployResult(results, tailwind))

  def appRoutes(webJars: WebJars): Routes[Deployer & SkillsJarCache, Nothing] =
    val tailwind = webJars.url("tailwindcss__browser", "/dist/index.global.js")

    Routes(
      Method.GET / Root -> Handler.fromFunctionZIO[Request]: request =>
        indexHandler(request, tailwind).catchAll: error =>
          ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(error.toString)), tailwind), Status.InternalServerError))
      ,
      Method.POST / "deploy" -> Handler.fromFunctionZIO[Request]: request =>
        deployHandler(request, tailwind).catchAll: error =>
          val message = error match
            case DeployError.RepoNotFound(org, repo) => s"Repository not found: $org/$repo"
            case DeployError.NoSkillsDirectory(org, repo) => s"No skills directory in $org/$repo"
            case DeployError.InvalidSkillMd(path, reason) => s"Invalid SKILL.md at $path: $reason"
            case DeployError.DuplicateVersion(gid, aid, v) => s"Duplicate version: $gid:$aid:$v"
            case DeployError.PublishFailed(reason) => s"Publish failed: $reason"
          ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(message)), tailwind)))
      ,
    ) ++ webJars.routes

  def run =
    Server.serve(appRoutes(WebJarsLive())).provide(
      Server.default,
      Client.default,
      Deployer.live,
      MavenCentral.Deploy.Sonatype.Live,
      SkillsJarService.cacheLayer,
    )
