import Models.*
import zio.*

import org.yaml.snakeyaml.Yaml
import java.util.{Map as JMap}
import scala.jdk.CollectionConverters.*

object SkillParser:

  private val frontmatterDelimiter = "---"

  def extractFrontmatter(content: String): Option[String] =
    val lines = content.linesIterator.toList
    lines match
      case head :: tail if head.trim == frontmatterDelimiter =>
        val endIdx = tail.indexWhere(_.trim == frontmatterDelimiter)
        if endIdx >= 0 then Some(tail.take(endIdx).mkString("\n"))
        else None
      case _ => None

  def parse(path: String, content: String): IO[DeployError, SkillMeta] = {
    // todo: use defer syntax
    for
      yamlStr <- ZIO.fromOption(extractFrontmatter(content))
        .orElseFail(DeployError.InvalidSkillMd(path, "No YAML frontmatter found"))
      yaml = Yaml()
      parsed <- ZIO.attempt(yaml.load[JMap[String, Any]](yamlStr))
        .mapError(e => DeployError.InvalidSkillMd(path, s"Invalid YAML: ${e.getMessage}"))
      map <- ZIO.fromOption(Option(parsed).map(_.asScala.toMap))
        .orElseFail(DeployError.InvalidSkillMd(path, "Empty YAML frontmatter"))
      name <- ZIO.fromOption(map.get("name").collect { case s: String => s })
        .orElseFail(DeployError.InvalidSkillMd(path, "Missing required field: name"))
      description <- ZIO.fromOption(map.get("description").collect { case s: String => s })
        .orElseFail(DeployError.InvalidSkillMd(path, "Missing required field: description"))
      maybeLicense = map.get("license").collect { case s: String => s }
    yield SkillMeta(SkillName(name), description, maybeLicense)
  }
