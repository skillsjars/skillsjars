import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.Client
import zio.test.*

import zio.compress.*
import zio.stream.*

object DeploySpec extends ZIOSpecDefault:

  private def readJarEntries(jar: Chunk[Byte]): ZIO[Any, Throwable, Map[String, Chunk[Byte]]] =
    ZStream.fromChunk(jar)
      .via(ZipUnarchiver.unarchive)
      .mapZIO: (entry, stream) =>
        stream.runCollect.map: bytes =>
          entry.name -> bytes
      .runCollect
      .map(_.toMap)

  def spec = suite("DeploySpec")(
    suite("PomGenerator")(
      test("includes required fields"):
        val pom = PomGenerator.generate(
          MavenCentral.GroupId("com.skillsjars"),
          MavenCentral.ArtifactId("myorg__myrepo__my-skill"),
          MavenCentral.Version("2026_02_13-abc1234"),
          SkillName("My Skill"),
          "A great skill",
          NonEmptyChunk(License("MIT License", "https://opensource.org/licenses/MIT")),
          Org("myorg"),
          Repo("myrepo"),
        ).asString

        assertTrue(
          pom.contains("<groupId>com.skillsjars</groupId>"),
          pom.contains("<artifactId>myorg__myrepo__my-skill</artifactId>"),
          pom.contains("<version>2026_02_13-abc1234</version>"),
          pom.contains("<name>My Skill</name>"),
          pom.contains("<description>A great skill</description>"),
          pom.contains("<name>MIT License</name>"),
          pom.contains("<url>https://opensource.org/licenses/MIT</url>"),
          pom.contains("https://github.com/myorg/myrepo"),
        )
    ),
    suite("deploy integration")(
      test("nonexistent repo returns RepoNotFound"):
        defer:
          val deployer = ZIO.service[Deployer[Any]].run
          val result = ZIO.scoped(deployer.deploy(Org("nonexistent-org-abc123"), Repo("nonexistent-repo"))).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.RepoNotFound])
            case _ => false
          )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("publishes one artifact per skill"):
        // brittle - should be able to specify a commit id for consistency
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val outcome = deployer.deploy(Org("anthropics"), Repo("skills")).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val canvasGav = outcome.published.find(_.artifactId.toString.contains("canvas-design")).get
            val skillsJar = readJarEntries(files.find(_._1.endsWith(".jar")).get._2).run

            assertTrue(
              outcome.published.nonEmpty,
              outcome.published.forall(_.groupId.toString == "com.skillsjars"),
              outcome.skipped.exists(_.skillName == SkillName("doc-coauthoring")),
              files.size == outcome.published.size * 6, // pom, jar, pom.md5, jar.md5, pom.sha1, jar.sha1
              files.keys.count(_.startsWith(s"com/skillsjars/${canvasGav.artifactId}/")) == 6,
              files.contains(s"com/skillsjars/${canvasGav.artifactId}/${canvasGav.version}/${canvasGav.artifactId}-${canvasGav.version}.jar"),
              skillsJar.keys.exists(_.endsWith("SKILL.md")),
              skillsJar.keys.exists(_.startsWith("META-INF/skills/anthropics/skills")),
            )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("publish with signing"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val outcome = deployer.deploy(Org("anthropics"), Repo("skills")).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]

            val files = readJarEntries(mockDeployer.upload.get._2).run.keys

            assertTrue(
              outcome.published.nonEmpty,
              files.exists(_.endsWith(".asc"))
            )
      .provide(MockDeployer.layer, Client.default) @@ TestAspect.withLiveSystem @@ TestAspect.ifEnvSet("OSS_GPG_KEY")
      ,
      test("repo-level license is used when skill has none"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val outcome = deployer.deploy(Org("brunoborges"), Repo("jdb-agentic-debugger")).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val gav = outcome.published.find(_.artifactId.toString.contains("jdb-debugger")).get
            val pomBytes = files(s"com/skillsjars/${gav.artifactId}/${gav.version}/${gav.artifactId}-${gav.version}.pom")
            val pom = ZIO.succeed(pomBytes.asString).run

            assertTrue(
              outcome.published.size == 1,
              outcome.skipped.isEmpty,
              pom.contains("<name>MIT License</name>"),
              pom.contains("<url>https://opensource.org/licenses/MIT</url>"),
            )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("publishes root-level SKILL.md as org__repo"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val outcome = deployer.deploy(Org("jdubois"), Repo("dr-jskill")).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val gav = outcome.published.find(_.artifactId.toString == "jdubois__dr-jskill").get
            val skillsJar = readJarEntries(files.find(_._1.endsWith(".jar")).get._2).run

            assertTrue(
              outcome.published.size == 1,
              gav.artifactId.toString == "jdubois__dr-jskill",
              skillsJar.keys.exists(_.endsWith("SKILL.md")),
              skillsJar.keys.exists(_.startsWith("META-INF/skills/jdubois/dr-jskill/")),
              !skillsJar.keys.exists(_.contains(".git")),
            )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("detects artifacts that already exist on Maven Central"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val existingVersion = MavenCentral.Version("2026_02_06-1ed29a0")
            val outcome = deployer.deploy(Org("anthropics"), Repo("skills"), Some(existingVersion)).run

            assertTrue(
              outcome.duplicates.exists(_.artifactId.toString.contains("algorithmic-art")),
              outcome.published.isEmpty,
            )
      .provide(MockDeployer.layerWithCheck, Client.default)
    ),
    suite("artifactIdFor")(
      test("basic artifact id"):
        defer:
          val aid = artifactIdFor(Org("myorg"), Repo("myrepo"), List("my-skill")).run
          assertTrue(aid.toString == "myorg__myrepo__my-skill")
      ,
      test("root skill produces org__repo"):
        defer:
          val aid = artifactIdFor(Org("jdubois"), Repo("dr-jskill")).run
          assertTrue(aid.toString == "jdubois__dr-jskill")
      ,
      test("with path"):
        defer:
          val aid = artifactIdFor(Org("myorg"), Repo("myrepo"), List("sub", "path", "myskill")).run
          assertTrue(aid.toString == "myorg__myrepo__sub__path__myskill")
      ,
      test("uppercase is lowercased"):
        defer:
          val aid = artifactIdFor(Org("MyOrg"), Repo("MyRepo"), List("My-Skill")).run
          assertTrue(aid.toString == "myorg__myrepo__my-skill")
      ,
      test("special characters are dropped"):
        defer:
          val aid = artifactIdFor(Org("my.org"), Repo("my.repo"), List("my_skill!@#name")).run
          assertTrue(aid.toString == "myorg__myrepo__my_skillname")
      ,
      test("leading and trailing hyphens/underscores are stripped"):
        defer:
          val aid = artifactIdFor(Org("myorg"), Repo("myrepo"), List("-my-skill-")).run
          assertTrue(aid.toString == "myorg__myrepo__my-skill")
      ,
      test("path components are sanitized"):
        defer:
          val aid = artifactIdFor(Org("org"), Repo("repo"), List("Sub.Path", "LEVEL_2", "skill")).run
          assertTrue(aid.toString == "org__repo__subpath__level_2__skill")
      ,
      test("already clean names pass through unchanged"):
        defer:
          val aid = artifactIdFor(Org("myorg"), Repo("myrepo"), List("my-skill")).run
          assertTrue(aid.toString == "myorg__myrepo__my-skill")
      ,
      test("fails when org contains __"):
        defer:
          val result = artifactIdFor(Org("my__org"), Repo("myrepo"), List("myskill")).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.InvalidComponent])
            case _ => false
          )
      ,
      test("fails when repo contains __"):
        defer:
          val result = artifactIdFor(Org("myorg"), Repo("my__repo"), List("myskill")).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.InvalidComponent])
            case _ => false
          )
      ,
      test("fails when path contains __"):
        defer:
          val result = artifactIdFor(Org("myorg"), Repo("myrepo"), List("my__skill")).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.InvalidComponent])
            case _ => false
          )
      ,
    ),
    suite("validateNoOverlaps")(
      test("no overlap with disjoint paths"):
        defer:
          val org = Org("o")
          val repo = Repo("r")
          val skills = List(
            (SkillLocation(org, repo, List("a")), java.io.File(".")),
            (SkillLocation(org, repo, List("b")), java.io.File(".")),
          )
          GitService.validateNoOverlaps(org, repo, skills).run
          assertCompletes
      ,
      test("fails when root overlaps with subdir"):
        defer:
          val org = Org("o")
          val repo = Repo("r")
          val skills = List(
            (SkillLocation(org, repo, List.empty), java.io.File(".")),
            (SkillLocation(org, repo, List("sub")), java.io.File(".")),
          )
          val result = GitService.validateNoOverlaps(org, repo, skills).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.OverlappingSkills])
            case _ => false
          )
      ,
      test("fails when parent path overlaps with child"):
        defer:
          val org = Org("o")
          val repo = Repo("r")
          val skills = List(
            (SkillLocation(org, repo, List("a")), java.io.File(".")),
            (SkillLocation(org, repo, List("a", "b")), java.io.File(".")),
          )
          val result = GitService.validateNoOverlaps(org, repo, skills).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists(_.isInstanceOf[DeployError.OverlappingSkills])
            case _ => false
          )
      ,
    ),
  )
