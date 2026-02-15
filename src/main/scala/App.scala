import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.*

object App extends ZIOAppDefault:

  private def indexHandler(request: Request): ZIO[SkillsJarCache, Throwable, Response] =
    defer:
      val maybeQuery = request.url.queryParams.getAll("q").headOption.filter(_.nonEmpty)
      val skillsJars = maybeQuery.fold(SkillsJarService.list)(SkillsJarService.search).run
      Response.html(UI.index(skillsJars, maybeQuery))

  private def deploySkill(
    deployer: Deployer,
    location: SkillLocation,
    skillDir: java.io.File,
    repoInfo: GitService.RepoInfo,
    org: Org,
    repo: Repo,
  ): ZIO[Client, DeployError, DeployResult] = {
    // todo: use defer syntax
    for
      content <- ZIO.attemptBlocking(String(java.nio.file.Files.readAllBytes(java.io.File(skillDir, "SKILL.md").toPath))).orDie
      meta <- SkillParser.parse(java.io.File(skillDir, "SKILL.md").getPath, content)
      groupId = groupIdFor(location.org, location.repo, location.subPath)
      artifactId = artifactIdFor(location.skillName)
      version = repoInfo.version
      alreadyExists <- ZIO.scoped(SkillsJarService.exists(groupId, artifactId, version))
        .catchAll(_ => ZIO.succeed(false))
      _ <- ZIO.when(alreadyExists)(ZIO.fail(DeployError.DuplicateVersion(groupId, artifactId, version)))
      license = meta.maybeLicense.orElse(repoInfo.maybeLicense)
      pom = PomGenerator.generate(groupId, artifactId, version, meta.name, meta.description, license, org, repo)
      jar = JarCreator.create(skillDir, org, repo, location.skillName, pom, groupId, artifactId, version)
      _ <- deployer.publish(groupId, artifactId, version, jar, pom)
    yield DeployResult.Success(groupId, artifactId, version)
  }

  private def deployHandler(request: Request): ZIO[Deployer & Client, DeployError, Response] = {
    // todo: use defer syntax
    for
      org <- ZIO.fromOption(request.url.queryParams.getAll("org").headOption.map(Org(_)))
        .orElseFail(DeployError.PublishFailed("Missing org parameter"))
      repo <- ZIO.fromOption(request.url.queryParams.getAll("repo").headOption.map(Repo(_)))
        .orElseFail(DeployError.PublishFailed("Missing repo parameter"))
      deployer <- ZIO.service[Deployer]
      results <- ZIO.scoped:
        for
          repoInfo <- GitService.cloneAndScan(org, repo)
          deployResults <- ZIO.foreach(repoInfo.skills): (location, skillDir) =>
            deploySkill(deployer, location, skillDir, repoInfo, org, repo)
        yield deployResults
    yield Response.html(UI.deployResult(results))
  }

  val appRoutes: Routes[Deployer & Client & SkillsJarCache, Nothing] = Routes(
    Method.GET / Root -> Handler.fromFunctionZIO[Request]: request =>
      indexHandler(request).catchAll: error =>
        ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(error.toString))), Status.InternalServerError))
    ,
    Method.POST / "deploy" -> Handler.fromFunctionZIO[Request]: request =>
      deployHandler(request).catchAll: error =>
        val message = error match
          case DeployError.RepoNotFound(org, repo) => s"Repository not found: $org/$repo"
          case DeployError.NoSkillsDirectory(org, repo) => s"No skills directory in $org/$repo"
          case DeployError.InvalidSkillMd(path, reason) => s"Invalid SKILL.md at $path: $reason"
          case DeployError.DuplicateVersion(gid, aid, v) => s"Duplicate version: $gid:$aid:$v"
          case DeployError.PublishFailed(reason) => s"Publish failed: $reason"
        ZIO.succeed(Response.html(UI.deployResult(Seq(DeployResult.Failure(message)))))
    ,
  )

  def run =
    Server.serve(appRoutes).provide(
      Server.default,
      Client.default,
      Deployer.live,
      MavenCentral.Deploy.Sonatype.Live,
      SkillsJarService.cacheLayer,
    )
