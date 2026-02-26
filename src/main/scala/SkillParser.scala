import Models.*
import org.virtuslab.yaml.*
import org.virtuslab.yaml.Node.*
import zio.*
import zio.direct.*

object SkillParser:

  private val frontmatterDelimiter = "---"

  def extractFrontmatter(content: String): Option[String] =
    val lines = content.linesIterator.toList
    lines match
      case head :: tail if head.trim == frontmatterDelimiter =>
        val endIdx = tail.indexWhere(_.trim == frontmatterDelimiter)
        Option.when(endIdx >= 0)(tail.take(endIdx).mkString("\n"))
      case _ => None

  private def fail(reason: String) = SkillError.InvalidSkillMd(reason)

  private def requireScalar(mappings: Map[Node, Node], field: String): IO[SkillError, String] =
    ZIO.fromOption:
      mappings.collectFirst:
        case (ScalarNode(key, _), ScalarNode(value, _)) if key == field => value
    .orElseFail(fail(s"Missing required field: $field"))

  def parse(content: String): IO[SkillError, SkillMeta] =
    defer:
      val yamlStr = ZIO.fromOption(extractFrontmatter(content))
        .orElseFail(fail("No YAML frontmatter found")).run
      val node = ZIO.fromEither(yamlStr.asNode)
        .mapError(e => fail(s"Invalid YAML: ${e.msg}")).run
      val mappings = node match
        case MappingNode(m, _) => ZIO.succeed(m).run
        case _                 => ZIO.fail(fail("YAML frontmatter must be a mapping")).run

      val name = requireScalar(mappings, "name").run
      val description = requireScalar(mappings, "description").run

      val licenseNode = mappings.collectFirst:
        case (ScalarNode(key, _), value) if key == "license" => value
      val (spdxIds, rawLicense) = licenseNode match
        case Some(ScalarNode(id, _))       => (List(id), Some(id))
        case Some(SequenceNode(nodes, _))  => (nodes.toList.collect { case ScalarNode(v, _) => v }, None)
        case _                             => (Nil, None)

      val licenses = spdxIds.flatMap(Models.licenseFromSpdxId)
      SkillMeta(SkillName(name), description, licenses, rawLicense)
