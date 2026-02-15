import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.*
import zio.direct.*
import zio.test.*

import java.io.File
import java.nio.file.Files

object DeploySpec extends ZIOSpecDefault:

  private def createTempSkillDir(name: String, skillMd: String, extraFiles: Map[String, String] = Map.empty): File =
    val tmpDir = Files.createTempDirectory("skill-test-").toFile
    val skillDir = File(tmpDir, s"skills/$name")
    skillDir.mkdirs()
    Files.writeString(File(skillDir, "SKILL.md").toPath, skillMd)
    extraFiles.foreach: (fileName, content) =>
      Files.writeString(File(skillDir, fileName).toPath, content)
    skillDir

  def spec = suite("DeploySpec")(
    test("JarCreator produces valid JAR bytes"):
      val skillMd =
        """---
          |name: test-skill
          |description: A test skill
          |---
          |# Test Skill
          |""".stripMargin
      val skillDir = createTempSkillDir("test-skill", skillMd)
      val org = Org("testorg")
      val repo = Repo("testrepo")
      val skillName = SkillName("test-skill")
      val groupId = groupIdFor(org, repo)
      val artifactId = artifactIdFor(skillName)
      val version = MavenCentral.Version("2026_02_13-abc1234")
      val pom = PomGenerator.generate(groupId, artifactId, version, SkillName("test-skill"), "A test skill", Some("MIT"), org, repo)
      val jar = JarCreator.create(skillDir, org, repo, skillName, pom, groupId, artifactId, version)

      assertTrue(
        jar.length > 100
      )
    ,
    test("JarCreator includes SKILL.md in correct path"):
      val skillMd =
        """---
          |name: my-skill
          |description: My skill
          |---
          |content
          |""".stripMargin
      val skillDir = createTempSkillDir("my-skill", skillMd)
      val org = Org("myorg")
      val repo = Repo("myrepo")
      val skillName = SkillName("my-skill")
      val groupId = groupIdFor(org, repo)
      val artifactId = artifactIdFor(skillName)
      val version = MavenCentral.Version("2026_02_13-abc1234")
      val pom = PomGenerator.generate(groupId, artifactId, version, SkillName("my-skill"), "My skill", None, org, repo)
      val jar = JarCreator.create(skillDir, org, repo, skillName, pom, groupId, artifactId, version)

      // todo: use zio-streams-compress-zip
      val jarInputStream = java.util.jar.JarInputStream(java.io.ByteArrayInputStream(jar))
      var entries = List.empty[String]
      var entry = jarInputStream.getNextJarEntry
      while entry != null do
        entries = entries :+ entry.getName
        entry = jarInputStream.getNextJarEntry
      jarInputStream.close()

      assertTrue(
        entries.exists(_.contains("META-INF/resources/skills/myorg/myrepo/my-skill/SKILL.md")),
        entries.exists(_.contains("META-INF/maven/")),
      )
    ,
    test("PomGenerator includes required fields"):
      val pom = PomGenerator.generate(
        MavenCentral.GroupId("com.skillsjars.myorg.myrepo"),
        MavenCentral.ArtifactId("my-skill"),
        MavenCentral.Version("2026_02_13-abc1234"),
        SkillName("My Skill"),
        "A great skill",
        Some("MIT"),
        Org("myorg"),
        Repo("myrepo"),
      )
      assertTrue(
        pom.contains("<groupId>com.skillsjars.myorg.myrepo</groupId>"),
        pom.contains("<artifactId>my-skill</artifactId>"),
        pom.contains("<version>2026_02_13-abc1234</version>"),
        pom.contains("<name>My Skill</name>"),
        pom.contains("<description>A great skill</description>"),
        pom.contains("<name>MIT</name>"),
        pom.contains("https://github.com/myorg/myrepo"),
      )
    ,
    test("PomGenerator works without license"):
      val pom = PomGenerator.generate(
        MavenCentral.GroupId("com.skillsjars.org.repo"),
        MavenCentral.ArtifactId("skill"),
        MavenCentral.Version("2026_01_01-1234567"),
        SkillName("skill"),
        "desc",
        None,
        Org("org"),
        Repo("repo"),
      )
      assertTrue(
        pom.contains("<groupId>com.skillsjars.org.repo</groupId>"),
        !pom.contains("<license>"),
      )
    ,
    test("MockDeployer records published artifacts"):
      defer:
        val deployer = ZIO.service[Deployer].run
        val published = ZIO.service[ConcurrentMap[String, MavenCentral.GroupArtifactVersion]].run

        val groupId = MavenCentral.GroupId("com.skillsjars.test.repo")
        val artifactId = MavenCentral.ArtifactId("test-skill")
        val version = MavenCentral.Version("2026_02_13-abc1234")

        deployer.publish(groupId, artifactId, version, Array.emptyByteArray, "<pom/>").run

        val result = published.get(s"$groupId:$artifactId:$version").run
        assertTrue(result.isDefined)
    .provide(MockDeployer.layer)
    ,
    test("groupIdFor with no subpath"):
      val gid = groupIdFor(Org("myorg"), Repo("myrepo"))
      assertTrue(gid.toString == "com.skillsjars.myorg.myrepo")
    ,
    test("groupIdFor with subpath"):
      val gid = groupIdFor(Org("myorg"), Repo("myrepo"), List("sub", "path"))
      assertTrue(gid.toString == "com.skillsjars.myorg.myrepo.sub.path")
    ,
  )
