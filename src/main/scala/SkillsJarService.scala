import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.cache.*
import zio.config.typesafe.TypesafeConfigProvider
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

  private def loadSources: Task[List[SkillsJarSource]] =
    ZIO.config(Models.skillsJarSourcesConfig)
      .withConfigProvider(TypesafeConfigProvider.fromResourcePath())

  private def fetchAllSkillsJars: ZIO[Client, ServiceError, Seq[SkillsJar]] =
    ZIO.scoped:
      defer:
        val sources = loadSources.catchAll(e => ZIO.succeed(List(SkillsJarSource(Models.groupId, None, Nil, true)))).run
        val allResults = ZIO.foreachPar(sources): source =>
          fetchForSource(source).catchAll(_ => ZIO.succeed(Seq.empty[SkillsJar]))
        .run
        allResults.flatten

  private def fetchForSource(source: SkillsJarSource): ZIO[Client & Scope, Throwable, Seq[SkillsJar]] =
    source.artifactId match
      case Some(aid) =>
        fetchSkillsJar(source.groupId, aid, source.securityScanned).map(Seq(_))
      case None =>
        defer:
          val entries = MavenCentral.searchArtifacts(source.groupId)
            .map(_.value.filterNot(source.isExcluded))
            .catchAll(_ => ZIO.succeed(Seq.empty[MavenCentral.ArtifactId]))
            .run
          val results = ZIO.foreachPar(entries): entry =>
            ZIO.scoped:
              fetchSkillsJar(source.groupId, entry, source.securityScanned).map(Some(_)).catchAll(_ => ZIO.none)
          .run
          results.flatten

  private def extractAllowedTools(pomXml: scala.xml.Elem): Map[String, String] =
    val prefix = PomGenerator.propertyPrefix
    (pomXml \ "properties").flatMap(_.child).collect:
      case e: scala.xml.Elem if e.label.startsWith(prefix) && e.label.endsWith(".allowed-tools") =>
        val skillName = e.label.stripPrefix(prefix).stripSuffix(".allowed-tools")
        skillName -> e.text
    .toMap

  private def fetchSkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, securityScanned: Boolean): ZIO[Client & Scope, Throwable, SkillsJar] =
    defer:
      val versions = MavenCentral.searchVersions(groupId, artifactId)
        .mapBoth(e => RuntimeException(s"Failed to fetch versions for $artifactId: $e"), _.value).run
      val nameDescAndTools = versions.headOption.fold(ZIO.succeed((artifactId.toString, "", Map.empty[String, String]))): latestVersion =>
        MavenCentral.pom(groupId, artifactId, latestVersion).map: pomXml =>
          val n = (pomXml \ "name").text
          val d = (pomXml \ "description").text
          val tools = extractAllowedTools(pomXml)
          (if n.nonEmpty then n else artifactId.toString, if d.nonEmpty then d else "", tools)
        .catchAll(_ => ZIO.succeed((artifactId.toString, "", Map.empty[String, String])))
      .run
      SkillsJar(groupId, artifactId, versions, nameDescAndTools._1, nameDescAndTools._2, securityScanned, nameDescAndTools._3)
