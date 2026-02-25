import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.ConcurrentMap
import zio.direct.*
import zio.http.Client

class DeployJobs(jobs: ConcurrentMap[(Org, Repo, MavenCentral.Version), DeployJobStatus]):

  def start[A: Tag](org: Org, repo: Repo): ZIO[Deployer[A] & A & Client, DeployError, MavenCentral.Version] =
    defer:
      val versionPromise = Promise.make[DeployError, MavenCentral.Version].run
      cloneAndDeploy[A](org, repo, versionPromise).disconnect.forkDaemon.run
      versionPromise.await.run

  def get(org: Org, repo: Repo, version: MavenCentral.Version): UIO[Option[DeployJobStatus]] =
    jobs.get((org, repo, version))

  private def cloneAndDeploy[A: Tag](org: Org, repo: Repo, versionPromise: Promise[DeployError, MavenCentral.Version]): ZIO[Deployer[A] & A & Client, Nothing, Unit] =
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

  private def runDeploy[A: Tag](key: (Org, Repo, MavenCentral.Version), org: Org, repo: Repo, version: MavenCentral.Version, repoInfo: GitService.RepoInfo): ZIO[Deployer[A] & A & Scope & Client, Nothing, Unit] =
    defer:
      val deployer = ZIO.service[Deployer[A]].run
      val outcome = deployer.deployFrom(org, repo, version, repoInfo).run
      outcome
    .foldZIO(
      error   => jobs.put(key, DeployJobStatus.Failed(error)),
      outcome => jobs.put(key, DeployJobStatus.Done(outcome.toResults))
    ).unit


object DeployJobs:

  val live: ZLayer[Any, Nothing, DeployJobs] =
    ZLayer.fromZIO:
      ConcurrentMap.empty[(Org, Repo, MavenCentral.Version), DeployJobStatus].map(DeployJobs(_))
