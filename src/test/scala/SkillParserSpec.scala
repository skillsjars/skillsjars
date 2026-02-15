import Models.*
import zio.*
import zio.test.*
import zio.direct.*

object SkillParserSpec extends ZIOSpecDefault:

  def spec = suite("SkillParser")(
    test("parses valid SKILL.md with all fields"):
      defer:
        val content =
          """---
            |name: my-skill
            |description: A useful skill
            |license: MIT
            |---
            |# My Skill
            |Some content here
            |""".stripMargin
        val meta = SkillParser.parse("skills/my-skill/SKILL.md", content).run
        assertTrue(
          meta.name == SkillName("my-skill"),
          meta.description == "A useful skill",
          meta.maybeLicense.contains("MIT"),
        )
    ,
    test("parses valid SKILL.md without license"):
      defer:
        val content =
          """---
            |name: another-skill
            |description: Another skill
            |---
            |# Content
            |""".stripMargin
        val meta = SkillParser.parse("skills/another-skill/SKILL.md", content).run
        assertTrue(
          meta.name == SkillName("another-skill"),
          meta.description == "Another skill",
          meta.maybeLicense.isEmpty,
        )
    ,
    test("fails when no frontmatter"):
      defer:
        val content = "# Just markdown\nNo frontmatter here"
        val result = SkillParser.parse("SKILL.md", content).exit.run
        assertTrue(result.isFailure)
    ,
    test("fails when missing name"):
      defer:
        val content =
          """---
            |description: A skill without a name
            |---
            |""".stripMargin
        val result = SkillParser.parse("SKILL.md", content).exit.run
        assertTrue(result match
          case Exit.Failure(cause) =>
            cause.failureOption.exists:
              case DeployError.InvalidSkillMd(_, reason) => reason.contains("name")
              case _ => false
          case _ => false
        )
    ,
    test("fails when missing description"):
      defer:
        val content =
          """---
            |name: my-skill
            |---
            |""".stripMargin
        val result = SkillParser.parse("SKILL.md", content).exit.run
        assertTrue(result match
          case Exit.Failure(cause) =>
            cause.failureOption.exists:
              case DeployError.InvalidSkillMd(_, reason) => reason.contains("description")
              case _ => false
          case _ => false
        )
    ,
    test("fails with invalid YAML"):
      defer:
        val content =
          """---
            |name: [invalid yaml
            |---
            |""".stripMargin
        val result = SkillParser.parse("SKILL.md", content).exit.run
        assertTrue(result.isFailure)
    ,
  )
