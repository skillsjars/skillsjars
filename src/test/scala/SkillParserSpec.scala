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
        val meta = SkillParser.parse(content).run
        assertTrue(
          meta.name == SkillName("my-skill"),
          meta.description == "A useful skill",
          meta.licenses == List(License("MIT License", "https://opensource.org/licenses/MIT")),
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
        val meta = SkillParser.parse(content).run
        assertTrue(
          meta.name == SkillName("another-skill"),
          meta.description == "Another skill",
          meta.licenses.isEmpty,
        )
    ,
    test("fails when no frontmatter"):
      defer:
        val content = "# Just markdown\nNo frontmatter here"
        val result = SkillParser.parse(content).exit.run
        assertTrue(result.isFailure)
    ,
    test("fails when missing name"):
      defer:
        val content =
          """---
            |description: A skill without a name
            |---
            |""".stripMargin
        val result = SkillParser.parse(content).exit.run
        assertTrue(result match
          case Exit.Failure(cause) =>
            cause.failureOption.exists:
              case SkillError.InvalidSkillMd(reason) => reason.contains("name")
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
        val result = SkillParser.parse(content).exit.run
        assertTrue(result match
          case Exit.Failure(cause) =>
            cause.failureOption.exists:
              case SkillError.InvalidSkillMd(reason) => reason.contains("description")
              case _ => false
          case _ => false
        )
    ,
    test("parses list-valued license"):
      defer:
        val content =
          """---
            |name: multi-license-skill
            |description: A skill with multiple licenses
            |license: [MIT, Apache-2.0]
            |---
            |# Content
            |""".stripMargin
        val meta = SkillParser.parse(content).run
        assertTrue(
          meta.licenses == List(
            License("MIT License", "https://opensource.org/licenses/MIT"),
            License("Apache License 2.0", "https://www.apache.org/licenses/LICENSE-2.0"),
          ),
        )
    ,
    test("ignores unknown SPDX license"):
      defer:
        val content =
          """---
            |name: bad-license-skill
            |description: A skill
            |license: UNKNOWN-LICENSE
            |---
            |""".stripMargin
        val meta = SkillParser.parse(content).run
        assertTrue(meta.licenses.isEmpty)
    ,
    test("fails with invalid YAML"):
      defer:
        val content =
          """---
            |name: [invalid yaml
            |---
            |""".stripMargin
        val result = SkillParser.parse(content).exit.run
        assertTrue(result.isFailure)
    ,
    suite("skill name validation")(
      test("accepts single character name"):
        defer:
          val content =
            """---
              |name: a
              |description: A skill
              |---
              |""".stripMargin
          val meta = SkillParser.parse(content).run
          assertTrue(meta.name == SkillName("a"))
      ,
      test("accepts max length name"):
        defer:
          val name = "a" * 64
          val content =
            s"""---
               |name: $name
               |description: A skill
               |---
               |""".stripMargin
          val meta = SkillParser.parse(content).run
          assertTrue(meta.name == SkillName(name))
      ,
      test("rejects name longer than 64 characters"):
        defer:
          val name = "a" * 65
          val content =
            s"""---
               |name: $name
               |description: A skill
               |---
               |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name must be 1-64 characters"))
            case _ => false
          )
      ,
      test("rejects name starting with hyphen"):
        defer:
          val content =
            """---
              |name: -my-skill
              |description: A skill
              |---
              |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name must not start or end with a hyphen"))
            case _ => false
          )
      ,
      test("rejects name ending with hyphen"):
        defer:
          val content =
            """---
              |name: my-skill-
              |description: A skill
              |---
              |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name must not start or end with a hyphen"))
            case _ => false
          )
      ,
      test("rejects name with consecutive hyphens"):
        defer:
          val content =
            """---
              |name: my--skill
              |description: A skill
              |---
              |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name must not contain consecutive hyphens"))
            case _ => false
          )
      ,
      test("rejects name with uppercase characters"):
        defer:
          val content =
            """---
              |name: MySkill
              |description: A skill
              |---
              |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name may only contain lowercase alphanumeric characters and hyphens"))
            case _ => false
          )
      ,
      test("rejects name with special characters"):
        defer:
          val content =
            """---
              |name: my_skill
              |description: A skill
              |---
              |""".stripMargin
          val result = SkillParser.parse(content).exit.run
          assertTrue(result match
            case Exit.Failure(cause) =>
              cause.failureOption.contains(SkillError.InvalidSkillMd("Skill name may only contain lowercase alphanumeric characters and hyphens"))
            case _ => false
          )
      ,
      test("accepts name with digits"):
        defer:
          val content =
            """---
              |name: skill123
              |description: A skill
              |---
              |""".stripMargin
          val meta = SkillParser.parse(content).run
          assertTrue(meta.name == SkillName("skill123"))
      ,
    ),
  )
