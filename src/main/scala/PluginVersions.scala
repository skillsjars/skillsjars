import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.{ArtifactId, GroupId, MavenCentralRepo, Version}
import zio.*
import zio.direct.*

/** The versions of the SkillsJars build plugins that the docs and the setup guide show. */
final case class PluginVersions(maven: Version, gradle: Version, sbt: Version) derives CanEqual

object PluginVersions:

  enum Plugin(val groupId: GroupId, val artifactId: ArtifactId):
    case Maven extends Plugin(GroupId("com.skillsjars"), ArtifactId("maven-plugin"))
    // The Gradle plugin id resolves through a marker on the Gradle Plugin Portal; the plugin
    // itself is released to Maven Central under this artifactId with the same versions.
    case Gradle extends Plugin(GroupId("com.skillsjars"), ArtifactId("gradle-plugin"))
    // sbt 1 and sbt 2 builds of the plugin are released together.
    case Sbt extends Plugin(GroupId("com.skillsjars"), ArtifactId("skillsjars-sbt-plugin_2.12_1.0"))

  /** Shown until the first lookup on Maven Central succeeds, and kept for a plugin whose lookup fails. */
  val fallback: PluginVersions = PluginVersions(Version("0.0.8"), Version("0.2.0"), Version("0.1.0"))

  private val refreshEvery = 1.hour

  /** Holds the latest versions found; [[live]] looks them up in the background. */
  final case class Latest(ref: Ref[PluginVersions]):
    def get: UIO[PluginVersions] = ref.get

  def get: URIO[Latest, PluginVersions] = ZIO.serviceWithZIO[Latest](_.get)

  def fixed(versions: PluginVersions): ULayer[Latest] = ZLayer.fromZIO(Ref.make(versions).map(Latest(_)))

  /** Starts with [[fallback]] and looks the versions up after startup and every hour, off the startup path. */
  val live: URLayer[MavenCentralRepo, Latest] =
    ZLayer.scoped:
      defer:
        val ref = Ref.make(fallback).run
        refresh(ref).repeat(Schedule.spaced(refreshEvery)).forkScoped.run
        Latest(ref)

  private def refresh(ref: Ref[PluginVersions]): URIO[MavenCentralRepo, Unit] =
    defer:
      val current = ref.get.run
      val maven = lookup(Plugin.Maven, current.maven).run
      val gradle = lookup(Plugin.Gradle, current.gradle).run
      val sbt = lookup(Plugin.Sbt, current.sbt).run
      ref.set(PluginVersions(maven, gradle, sbt)).run

  private def lookup(plugin: Plugin, previous: Version): URIO[MavenCentralRepo, Version] =
    val name = s"${plugin.groupId}:${plugin.artifactId}"
    MavenCentral.latest(plugin.groupId, plugin.artifactId)
      .flatMap:
        case Some(version) => ZIO.succeed(version)
        case None => ZIO.logWarning(s"plugin versions: $name has no versions on Maven Central; keeping $previous").as(previous)
      .catchAllCause(cause => ZIO.logWarningCause(s"plugin versions: looking up $name failed; keeping $previous", cause).as(previous))
