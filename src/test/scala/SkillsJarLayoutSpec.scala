import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifactVersion
import zio.*
import zio.test.*

object SkillsJarLayoutSpec extends ZIOSpecDefault:

  private def gav(groupId: String, artifactId: String) =
    GroupArtifactVersion(MavenCentral.GroupId(groupId), MavenCentral.ArtifactId(artifactId), MavenCentral.Version("1.0"))

  def spec = suite("SkillsJarLayoutSpec")(
    test("skillRoots finds each directory holding a SKILL.md, nested or not"):
      val roots = SkillsJarLayout.skillRoots(Chunk(
        "META-INF/MANIFEST.MF",
        "META-INF/skills/google-gemini/gemini-cli/packages/sdk/pirate-skill/",
        "META-INF/skills/google-gemini/gemini-cli/packages/sdk/pirate-skill/SKILL.md",
        "META-INF/skills/google-gemini/gemini-cli/packages/sdk/pirate-skill/refs/a.md",
        "META-INF/skills/jamesward/skills/zen-of-scala/SKILL.md",
      ))
      assertTrue(roots == Chunk("google-gemini/gemini-cli/packages/sdk/pirate-skill", "jamesward/skills/zen-of-scala"))
    ,
    test("conflicts lists each skill directory in more than one jar, in jar order"):
      val first = gav("com.a", "x")
      val second = gav("com.b", "x")
      val other = gav("com.c", "y")
      val conflicts = SkillsJarLayout.conflicts(Chunk(
        first -> Chunk("org/repo/skill", "org/repo/only-a"),
        second -> Chunk("org/repo/skill"),
        other -> Chunk("org/other/skill"),
      ))
      assertTrue(conflicts == Map("org/repo/skill" -> Chunk(first, second)))
    ,
  )
