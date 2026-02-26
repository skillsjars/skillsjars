import Models.*
import zio.*
import zio.test.*
import zio.http.*

import java.io.File
import java.nio.file.Files

object SecurityScannerSpec extends ZIOSpecDefault:

  private def makeSkillDir(files: Map[String, String]): File =
    val dir = Files.createTempDirectory("scan-test-").toFile
    files.foreach: (name, content) =>
      val parts = name.split('/')
      val parent = parts.init.foldLeft(dir)((d, p) => { val sub = File(d, p); sub.mkdirs(); sub })
      val f = File(parent, parts.last)
      Files.write(f.toPath, content.getBytes("UTF-8"))
    dir

  private def deleteRecursive(f: File): Unit =
    if f.isDirectory then Option(f.listFiles()).getOrElse(Array.empty[File]).foreach(deleteRecursive)
    f.delete()

  private val cleanSkillMd =
    """---
      |name: my-skill
      |description: A helpful skill
      |---
      |# My Skill
      |This skill helps with coding tasks.
      |""".stripMargin

  def spec = suite("SecurityScannerSpec")(
    test("clean skill returns no findings"):
      val dir = makeSkillDir(Map("SKILL.md" -> cleanSkillMd))
      ZIO.scoped(SecurityScanner.scan(dir))
        .map(findings => assertTrue(findings.values.flatten.isEmpty))
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(HerokuInferenceFake.layer, Client.default)
    ,
    test("malicious skill detected by LLM"):
      val dir = makeSkillDir(Map("SKILL.md" -> "Sneaky content"))
      ZIO.scoped(SecurityScanner.scan(dir))
        .map: findings =>
          val all = findings.values.flatten.toList
          assertTrue(
            all.length == 1,
            all.head.category == ScanCategory.SubtleInjection,
            all.head.severity == Severity.High,
            findings.contains("SKILL.md"),
          )
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(
      HerokuInferenceFake.layerWith(_ => """[{"category": "SUBTLE_INJECTION", "severity": "High", "description": "Hidden instruction in markdown"}]"""),
      Client.default,
    )
    ,
    test("LLM failure propagates as InferenceError"):
      val dir = makeSkillDir(Map("SKILL.md" -> cleanSkillMd))
      ZIO.scoped(SecurityScanner.scan(dir))
        .flip
        .map:
          case SkillError.ScanInferenceError(reason) => assertTrue(reason.contains("Unexpected"))
          case other => assertTrue(false)
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(HerokuInferenceFake.layerWith(_ => throw RuntimeException("Unexpected LLM error")), Client.default)
    ,
    test("unparseable LLM response propagates as ParseError"):
      val dir = makeSkillDir(Map("SKILL.md" -> cleanSkillMd))
      ZIO.scoped(SecurityScanner.scan(dir))
        .flip
        .map:
          case SkillError.ScanParseError(_) => assertTrue(true)
          case other => assertTrue(false)
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(HerokuInferenceFake.layerWith(_ => "not json at all {{{"), Client.default)
    ,
    test("multiple findings parsed correctly"):
      val dir = makeSkillDir(Map("SKILL.md" -> "Some content"))
      ZIO.scoped(SecurityScanner.scan(dir))
        .map: findings =>
          val all = findings.values.flatten.toList
          assertTrue(
            all.length == 2,
            all.exists(_.category == ScanCategory.SemanticMismatch),
            all.exists(_.category == ScanCategory.PromptInjection),
          )
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(
      HerokuInferenceFake.layerWith(_ =>
        """[
          |  {"category": "SEMANTIC_MISMATCH", "severity": "High", "description": "Mismatch detected"},
          |  {"category": "PROMPT_INJECTION", "severity": "Critical", "description": "Injection found"}
          |]""".stripMargin),
      Client.default,
    )
    ,
    test("unknown categories produce ParseError"):
      val dir = makeSkillDir(Map("SKILL.md" -> "Some content"))
      ZIO.scoped(SecurityScanner.scan(dir))
        .flip
        .map:
          case SkillError.ScanParseError(_) => assertTrue(true)
          case other => assertTrue(false)
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(
      HerokuInferenceFake.layerWith(_ => """[{"category": "UNKNOWN_CAT", "severity": "High", "description": "Something"}]"""),
      Client.default,
    )
    ,
    test("markdown-wrapped JSON is parsed correctly"):
      val dir = makeSkillDir(Map("SKILL.md" -> "Some content"))
      ZIO.scoped(SecurityScanner.scan(dir))
        .map: findings =>
          val all = findings.values.flatten.toList
          assertTrue(
            all.length == 1,
            all.head.category == ScanCategory.PromptInjection,
          )
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(
      HerokuInferenceFake.layerWith(_ =>
        "```json\n[{\"category\": \"PROMPT_INJECTION\", \"severity\": \"High\", \"description\": \"Injection found\"}]\n```"),
      Client.default,
    )
    ,
    test("Medium findings returned"):
      val dir = makeSkillDir(Map("SKILL.md" -> cleanSkillMd))
      ZIO.scoped(SecurityScanner.scan(dir))
        .map: findings =>
          assertTrue(findings.values.flatten.nonEmpty)
        .ensuring(ZIO.succeed(deleteRecursive(dir)))
    .provide(
      HerokuInferenceFake.layerWith(_ => """[{"category": "SEMANTIC_MISMATCH", "severity": "Medium", "description": "Minor mismatch"}]"""),
      Client.default,
    )
  )
