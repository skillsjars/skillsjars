import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.ConcurrentMap
import zio.direct.*
import zio.http.Client

import java.io.File

class DeployJobs(
  jobs: ConcurrentMap[(Org, Repo, MavenCentral.Version), DeployJobStatus],
):

  def start[A: Tag](org: Org, repo: Repo): ZIO[Deployer[A] & A & Client & HerokuInference, DeployError, MavenCentral.Version] =
    defer:
      val versionPromise = Promise.make[DeployError, MavenCentral.Version].run
      cloneAndDeploy[A](org, repo, versionPromise).disconnect.forkDaemon.run
      versionPromise.await.run

  def get(org: Org, repo: Repo, version: MavenCentral.Version): UIO[Option[DeployJobStatus]] =
    jobs.get((org, repo, version))

  private def cloneAndDeploy[A: Tag](org: Org, repo: Repo, versionPromise: Promise[DeployError, MavenCentral.Version]): ZIO[Deployer[A] & A & Client & HerokuInference, Nothing, Unit] =
    ZIO.scoped:
      defer:
        val repoInfo = GitService.cloneAndScan(org, repo).tapError(e => versionPromise.fail(e)).run
        val version = repoInfo.version
        val key = (org, repo, version)

        versionPromise.succeed(version).run
        jobs.get(key).run match
          case Some(_) => ()
          case None =>
            jobs.putIfAbsent(key, DeployJobStatus.Running).run match
              case Some(_) => ()
              case None    => runDeploy[A](key, org, repo, version, repoInfo).run
    .catchAll: error =>
      versionPromise.fail(error).unit

  private def passed(findings: Map[String, List[SecurityFinding]]): Boolean =
    !findings.values.flatten.exists(f => f.severity == Severity.Critical || f.severity == Severity.High)

  private def scanSkill(location: SkillLocation, skillDir: File): ZIO[HerokuInference & Client & Scope, Nothing, Option[(SkillName, SkillError)]] =
    SecurityScanner.scan(skillDir).fold(
      error => Some((location.skillName, error)),
      findings =>
        if passed(findings) then None
        else Some((location.skillName, SkillError.SecurityBlocked(findings)))
    )

  private def runDeploy[A: Tag](key: (Org, Repo, MavenCentral.Version), org: Org, repo: Repo, version: MavenCentral.Version, repoInfo: GitService.RepoInfo): ZIO[Deployer[A] & A & Scope & Client & HerokuInference, Nothing, Unit] =
    defer:
      val deployer = ZIO.service[Deployer[A]].run

      // Phase 1: Cheap validations (SKILL.md parsing, duplicate check, license)
      val validationResults = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
        deployer.validateSkill(org, repo, version, location, skillDir, repoInfo.licenses)
          .fold(
            error => Left((location.skillName, error)),
            validated => Right(validated),
          )
      .run

      val validated = validationResults.collect { case Right(v) => v }
      val validationSkipped = validationResults.collect { case Left((n, e)) => n -> e }.toMap

      // Phase 2: Security scan only validated skills
      val scanResults = ZIO.foreach(validated): skill =>
        scanSkill(skill.location, skill.skillDir)
      .run

      val securityBlocked = scanResults.flatten.toMap
      val passedSkills = validated.filterNot(v => securityBlocked.contains(v.location.skillName))
      val allSkipped = validationSkipped ++ securityBlocked

      if passedSkills.isEmpty then
        // Phase 3: Early exit if nothing valid
        jobs.put(key, DeployJobStatus.Failed(DeployJobError.NoPublishableSkills(allSkipped))).unit.run
      else
        // Phase 4: Build and deploy
        val done: ZIO[Deployer[A] & A & Scope & Client, DeployJobError, Unit] =
          defer:
            val deployResults = deployer.deployValidated(org, repo, version, passedSkills).run
            val allResults = allSkipped.map((name, error) => name -> SkillResult.Skipped(error)) ++ deployResults
            jobs.put(key, DeployJobStatus.Done(allResults)).unit.run
        done.run
    .catchAll: (error: DeployJobError) =>
      jobs.put(key, DeployJobStatus.Failed(error)).unit


object DeployJobs:

  val live: ZLayer[Any, Nothing, DeployJobs] =
    ZLayer.fromZIO:
      ConcurrentMap.empty[(Org, Repo, MavenCentral.Version), DeployJobStatus].map(DeployJobs(_))
