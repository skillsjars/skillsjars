import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifactVersion
import zio.*

import java.nio.file.Path
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*

/** The skills in a SkillsJar, under `META-INF/skills/`. */
object SkillsJarLayout:

  private val prefix = "META-INF/skills/"
  private val skillFile = "/SKILL.md"

  /** Each skill's directory relative to `META-INF/skills/` (e.g. `org/repo/skill`), from a jar's entry names. */
  def skillRoots(entryNames: Chunk[String]): Chunk[String] =
    entryNames.collect:
      case name if name.startsWith(prefix) && name.endsWith(skillFile) => name.stripPrefix(prefix).stripSuffix(skillFile)

  /** The skill directories in more than one jar, with those jars in the order given. */
  def conflicts(roots: Chunk[(GroupArtifactVersion, Chunk[String])]): Map[String, Chunk[GroupArtifactVersion]] =
    roots
      .flatMap((gav, skills) => skills.distinct.map(_ -> gav))
      .groupBy(_._1)
      .collect { case (root, owners) if owners.size > 1 => root -> owners.map(_._2) }

  def entryNames(jar: Path): Task[Chunk[String]] =
    ZIO.scoped:
      ZIO.fromAutoCloseable(ZIO.attemptBlockingIO(ZipFile(jar.toFile))).flatMap: zip =>
        ZIO.attemptBlockingIO(Chunk.fromIterable(zip.entries.nn.asScala.map(_.getName.nn).toSeq))
