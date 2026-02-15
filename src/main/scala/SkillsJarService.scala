import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.Client

// why do we need this?
case class SkillsJarCache(cache: Cache[String, Throwable, Seq[SkillsJar]])

// all of this seems off
// todo: integration tests
object SkillsJarService:

  given CanEqual[String, String] = CanEqual.derived

  private val cacheKey = "all"

  def list: ZIO[SkillsJarCache, Throwable, Seq[SkillsJar]] =
    ZIO.serviceWithZIO[SkillsJarCache](_.cache.get(cacheKey))

  def search(query: String): ZIO[SkillsJarCache, Throwable, Seq[SkillsJar]] =
    defer:
      val all = list.run
      val lower = query.toLowerCase
      all.filter: sj =>
        sj.name.toLowerCase.contains(lower) || sj.description.toLowerCase.contains(lower)

  def exists(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Client & Scope, Throwable, Boolean] =
    MavenCentral.artifactExists(groupId, artifactId, version)

  val cacheLayer: ZLayer[Client, Nothing, SkillsJarCache] =
    ZLayer.fromZIO:
      Cache.makeWith(1, Lookup((_: String) => fetchAllSkillsJars)):
        case Exit.Success(_) => 1.hour
        case Exit.Failure(_) => Duration.Zero
      .map(SkillsJarCache(_))

  private def fetchAllSkillsJars: ZIO[Client, Throwable, Seq[SkillsJar]] =
    ZIO.scoped:
      defer:
        val topLevel = scanDirectory(MavenCentral.GroupId("com.skillsjars")).run
        val skillsJars = ZIO.foreach(topLevel)(orgId =>
          ZIO.scoped(scanOrgForSkillsJars(MavenCentral.GroupId(s"com.skillsjars.$orgId")))
        ).run
        skillsJars.flatten

  private def scanDirectory(groupId: MavenCentral.GroupId): ZIO[Client & Scope, Throwable, Seq[String]] =
    MavenCentral.searchArtifacts(groupId)
      .map(_.value.map(_.toString))
      .catchAll(_ => ZIO.succeed(Seq.empty))

  private def scanOrgForSkillsJars(orgGroupId: MavenCentral.GroupId): ZIO[Client & Scope, Throwable, Seq[SkillsJar]] =
    defer:
      val repos = scanDirectory(orgGroupId).run
      val results = ZIO.foreach(repos)(repoName =>
        scanRepoForSkillsJars(MavenCentral.GroupId(s"$orgGroupId.$repoName"))
      ).run
      results.flatten

  private def scanRepoForSkillsJars(repoGroupId: MavenCentral.GroupId): ZIO[Client & Scope, Throwable, Seq[SkillsJar]] =
    for
      entries <- MavenCentral.searchArtifacts(repoGroupId)
        .map(_.value)
        .catchAll(_ => ZIO.succeed(Seq.empty))
      results <- ZIO.foreach(entries)(entry => processEntry(repoGroupId, entry))
    yield results.flatten

  private def processEntry(repoGroupId: MavenCentral.GroupId, entry: MavenCentral.ArtifactId): ZIO[Client & Scope, Throwable, Seq[SkillsJar]] =
    MavenCentral.isArtifact(repoGroupId, entry)
      .catchAll(_ => ZIO.succeed(false))
      .flatMap: isArtifact =>
        if isArtifact then
          fetchSkillsJar(repoGroupId, entry).map(Seq(_))
            .catchAll(_ => ZIO.succeed(Seq.empty))
        else
          val subGroupId = MavenCentral.GroupId(s"$repoGroupId.$entry")
          scanRepoForSkillsJars(subGroupId)
            .catchAll(_ => ZIO.succeed(Seq.empty))

  private def fetchSkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Scope, Throwable, SkillsJar] =
    for
      versions <- MavenCentral.searchVersions(groupId, artifactId).map(_.value)
        .catchAll(_ => ZIO.succeed(Seq.empty[MavenCentral.Version]))
      nameAndDesc <- versions.headOption.fold(ZIO.succeed((artifactId.toString, ""))): latestVersion =>
        MavenCentral.pom(groupId, artifactId, latestVersion).map: pomXml =>
          val n = (pomXml \ "name").text
          val d = (pomXml \ "description").text
          (if n.nonEmpty then n else artifactId.toString, if d.nonEmpty then d else "")
        .catchAll(_ => ZIO.succeed((artifactId.toString, "")))
    yield SkillsJar(groupId, artifactId, versions, nameAndDesc._1, nameAndDesc._2)
