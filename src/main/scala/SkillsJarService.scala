import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.cache.*
import zio.direct.*
import zio.http.Client

// why do we need this?
case class SkillsJarCache(cache: Cache[String, SkillsJarService.ServiceError, Seq[SkillsJar]])

object SkillsJarService:

  given CanEqual[String, String] = CanEqual.derived

  private val cacheKey = "all"

  enum ServiceError:
    case FetchFailed(reason: String)

  def list: ZIO[SkillsJarCache, ServiceError, Seq[SkillsJar]] =
    ZIO.serviceWithZIO[SkillsJarCache](_.cache.get(cacheKey))

  def search(query: String): ZIO[SkillsJarCache, ServiceError, Seq[SkillsJar]] =
    defer:
      val all = list.run
      val lower = query.toLowerCase
      all.filter: sj =>
        sj.name.toLowerCase.contains(lower) || sj.description.toLowerCase.contains(lower)

  def exists(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version): ZIO[Client & Scope, Throwable, Boolean] =
    MavenCentral.artifactExists(groupId, artifactId, version)

  val cacheLayer: ZLayer[Client, Nothing, SkillsJarCache] =
    ZLayer.fromZIO:
      Cache.makeWith(1, Lookup((_: String) => fetchAllSkillsJars.mapError(e => e))):
        case Exit.Success(_) => 1.hour
        case Exit.Failure(_) => Duration.Zero
      .map(SkillsJarCache(_))

  private def fetchAllSkillsJars: ZIO[Client, ServiceError, Seq[SkillsJar]] =
    ZIO.scoped:
      defer:
        val gid = Models.groupId
        val entries = MavenCentral.searchArtifacts(gid)
          .map(_.value)
          .catchAll(_ => ZIO.succeed(Seq.empty[MavenCentral.ArtifactId]))
          .run
        val results = ZIO.foreachPar(entries): entry =>
          ZIO.scoped:
            fetchSkillsJar(gid, entry).map(Some(_)).catchAll(_ => ZIO.none)
        .run
        results.flatten

  private def fetchSkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId): ZIO[Client & Scope, Throwable, SkillsJar] =
    defer:
      val versions = MavenCentral.searchVersions(groupId, artifactId)
        .mapBoth(e => RuntimeException(s"Failed to fetch versions for $artifactId: $e"), _.value).run
      val nameAndDesc = versions.headOption.fold(ZIO.succeed((artifactId.toString, ""))): latestVersion =>
        MavenCentral.pom(groupId, artifactId, latestVersion).map: pomXml =>
          val n = (pomXml \ "name").text
          val d = (pomXml \ "description").text
          (if n.nonEmpty then n else artifactId.toString, if d.nonEmpty then d else "")
        .catchAll(_ => ZIO.succeed((artifactId.toString, "")))
      .run
      SkillsJar(groupId, artifactId, versions, nameAndDesc._1, nameAndDesc._2)
