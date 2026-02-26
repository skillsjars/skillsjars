import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import zio.http.Client
import zio.test.*

import zio.compress.*
import zio.stream.*

object DeploySpec extends ZIOSpecDefault:

  private def deploy(deployer: Deployer[Any], org: Org, repo: Repo): ZIO[Scope & Client, DeployError, Map[SkillName, SkillResult]] =
    defer:
      val repoInfo = GitService.cloneAndScan(org, repo).run
      val version = repoInfo.version
      val validationResults = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
        deployer.validateSkill(org, repo, version, location, skillDir, repoInfo.licenses)
          .fold(
            error => Left((location.skillName, error)),
            validated => Right(validated),
          )
      .run
      val validated = validationResults.collect { case Right(v) => v }
      val skipped = validationResults.collect { case Left((n, e)) => n -> SkillResult.Skipped(e) }.toMap
      val deployResults = deployer.deployValidated(org, repo, version, validated)
        .mapError(e => DeployError(org, repo, RepoErrorKind.NotFound)).run
      skipped ++ deployResults

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
        val pomBytes = PomGenerator.generate(
          MavenCentral.GroupId("com.skillsjars"),
          MavenCentral.ArtifactId("myorg__myrepo__my-skill"),
          MavenCentral.Version("2026_02_13-abc1234"),
          SkillName("My Skill"),
          "A great skill",
          NonEmptyChunk(License("MIT License", "https://opensource.org/licenses/MIT")),
          Org("myorg"),
          Repo("myrepo"),
        )
        val pom = scala.xml.XML.loadString(pomBytes.asString)
        val gid = (pom \ "groupId").text
        val aid = (pom \ "artifactId").text
        val ver = (pom \ "version").text
        val pomName = (pom \ "name").text
        val desc = (pom \ "description").text
        val licName = (pom \ "licenses" \ "license" \ "name").text
        val licUrl = (pom \ "licenses" \ "license" \ "url").text
        val scmUrl = (pom \ "scm" \ "url").text

        assertTrue(
          gid == "com.skillsjars",
          aid == "myorg__myrepo__my-skill",
          ver == "2026_02_13-abc1234",
          pomName == "My Skill",
          desc == "A great skill",
          licName == "MIT License",
          licUrl == "https://opensource.org/licenses/MIT",
          scmUrl == "https://github.com/myorg/myrepo",
        )
      ,
      test("escapes XML special characters in description"):
        val description = "Developer and Non-Developer tracks, plus on-demand Q&A. That's <great> for \"everyone\""
        val pomBytes = PomGenerator.generate(
          MavenCentral.GroupId("com.skillsjars"),
          MavenCentral.ArtifactId("github__awesome-copilot__copilot-cli-quickstart"),
          MavenCentral.Version("2026_02_25-8cf8dd6"),
          SkillName("copilot-cli-quickstart"),
          description,
          NonEmptyChunk(License("MIT License", "https://opensource.org/licenses/MIT")),
          Org("github"),
          Repo("awesome-copilot"),
        )
        val pom = scala.xml.XML.loadString(pomBytes.asString)
        val desc = (pom \ "description").text

        assertTrue(
          desc == description,
        )
    ),
    suite("deploy integration")(
      test("nonexistent repo returns RepoNotFound"):
        defer:
          val deployer = ZIO.service[Deployer[Any]].run
          val result = ZIO.scoped(deploy(deployer, Org("nonexistent-org-abc123"), Repo("nonexistent-repo"))).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists { case DeployError(_, _, RepoErrorKind.NotFound) => true; case _ => false }
            case _ => false
          )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("publishes one artifact per skill"):
        // brittle - should be able to specify a commit id for consistency
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val repoInfo = GitService.cloneAndScan(Org("anthropics"), Repo("skills")).run
            val version = repoInfo.version
            val validationResults = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
              deployer.validateSkill(Org("anthropics"), Repo("skills"), version, location, skillDir, repoInfo.licenses)
                .fold(
                  error => Left((location.skillName, error)),
                  validated => Right(validated),
                )
            .run
            val validated = validationResults.collect { case Right(v) => v }
            val validationSkipped = validationResults.collect { case Left((n, e)) => n -> SkillResult.Skipped(e) }.toMap
            val deployResults = deployer.deployValidated(Org("anthropics"), Repo("skills"), version, validated).catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val results = validationSkipped ++ deployResults

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val published = results.collect { case (name, SkillResult.Success(_)) => name }.toSet
            val skipped = results.collect { case (name, SkillResult.Skipped(_)) => name }
            val canvasAid = artifactIdFor(Org("anthropics"), Repo("skills"), List("canvas-design")).catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val canvasGav = MavenCentral.GroupArtifactVersion(Models.groupId, canvasAid, version)
            val skillsJar = readJarEntries(files.find(_._1.endsWith(".jar")).get._2).run

            assertTrue(
              published.nonEmpty,
              skipped.exists(_ == SkillName("doc-coauthoring")),
              files.size == published.size * 6, // pom, jar, pom.md5, jar.md5, pom.sha1, jar.sha1
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
            val repoInfo = GitService.cloneAndScan(Org("anthropics"), Repo("skills")).run
            val version = repoInfo.version
            val validationResults = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
              deployer.validateSkill(Org("anthropics"), Repo("skills"), version, location, skillDir, repoInfo.licenses)
                .fold(
                  error => Left((location.skillName, error)),
                  validated => Right(validated),
                )
            .run
            val validated = validationResults.collect { case Right(v) => v }
            val results = deployer.deployValidated(Org("anthropics"), Repo("skills"), version, validated).catchAll(e => ZIO.die(RuntimeException(e.toString))).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]

            val files = readJarEntries(mockDeployer.upload.get._2).run.keys
            val published = results.collect { case (_, SkillResult.Success(_)) => () }

            assertTrue(
              published.nonEmpty,
              files.exists(_.endsWith(".asc"))
            )
      .provide(MockDeployer.layer, Client.default) @@ TestAspect.withLiveSystem @@ TestAspect.ifEnvSet("OSS_GPG_KEY")
      ,
      test("repo-level license is used when skill has none"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val repoInfo = GitService.cloneAndScan(Org("brunoborges"), Repo("jdb-agentic-debugger")).run
            val version = repoInfo.version
            val validated = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
              deployer.validateSkill(Org("brunoborges"), Repo("jdb-agentic-debugger"), version, location, skillDir, repoInfo.licenses)
            .catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val results = deployer.deployValidated(Org("brunoborges"), Repo("jdb-agentic-debugger"), version, validated).catchAll(e => ZIO.die(RuntimeException(e.toString))).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val published = results.collect { case (name, SkillResult.Success(_)) => name }.toSet
            val skipped = results.collect { case (_, SkillResult.Skipped(_)) => () }
            val aid = artifactIdFor(Org("brunoborges"), Repo("jdb-agentic-debugger"), List("jdb-debugger")).catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val gavForSkill = MavenCentral.GroupArtifactVersion(Models.groupId, aid, version)
            val pomBytes = files(s"com/skillsjars/${gavForSkill.artifactId}/${gavForSkill.version}/${gavForSkill.artifactId}-${gavForSkill.version}.pom")
            val pom = scala.xml.XML.loadString(pomBytes.asString)
            val licName = (pom \ "licenses" \ "license" \ "name").text
            val licUrl = (pom \ "licenses" \ "license" \ "url").text

            assertTrue(
              published.size == 1,
              skipped.isEmpty,
              licName == "MIT License",
              licUrl == "https://opensource.org/licenses/MIT",
            )
      .provide(MockDeployer.layer, Client.default)
      ,
      test("publishes root-level SKILL.md as org__repo"):
        ZIO.scoped:
          defer:
            val deployer = ZIO.service[Deployer[Any]].run
            val repoInfo = GitService.cloneAndScan(Org("jdubois"), Repo("dr-jskill")).run
            val version = repoInfo.version
            val validated = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
              deployer.validateSkill(Org("jdubois"), Repo("dr-jskill"), version, location, skillDir, repoInfo.licenses)
            .catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val results = deployer.deployValidated(Org("jdubois"), Repo("dr-jskill"), version, validated).catchAll(e => ZIO.die(RuntimeException(e.toString))).run

            val mockDeployer = deployer.asInstanceOf[MockDeployer]
            val files = readJarEntries(mockDeployer.upload.get._2).run

            val published = results.collect { case (name, SkillResult.Success(_)) => name }.toSet
            val aid = artifactIdFor(Org("jdubois"), Repo("dr-jskill")).catchAll(e => ZIO.die(RuntimeException(e.toString))).run
            val skillGav = MavenCentral.GroupArtifactVersion(Models.groupId, aid, version)
            val skillsJar = readJarEntries(files.find(_._1.endsWith(".jar")).get._2).run

            assertTrue(
              published.size == 1,
              skillGav.artifactId.toString == "jdubois__dr-jskill",
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
            val repoInfo = GitService.cloneAndScan(Org("anthropics"), Repo("skills")).run
            val validationResults = ZIO.foreach(repoInfo.skills): (location, skillDir) =>
              deployer.validateSkill(Org("anthropics"), Repo("skills"), existingVersion, location, skillDir, repoInfo.licenses)
                .fold(
                  error => Left((location.skillName, error)),
                  validated => Right(validated),
                )
            .run
            val validated = validationResults.collect { case Right(v) => v }
            val skipped = validationResults.collect { case Left((n, e)) => n -> e }.toMap

            val result = deployer.deployValidated(Org("anthropics"), Repo("skills"), existingVersion, validated).exit.run

            val allDuplicates = skipped.collect { case (name, SkillError.DuplicateVersion(_)) => name }
            val deployDuplicates = result match
              case Exit.Failure(cause) =>
                cause.failureOption match
                  case Some(DeployJobError.NoPublishableSkills(s)) =>
                    s.collect { case (name, SkillError.DuplicateVersion(_)) => name }
                  case _ => Iterable.empty
              case Exit.Success(results) =>
                results.collect { case (name, SkillResult.Skipped(SkillError.DuplicateVersion(_))) => name }
            val duplicates = allDuplicates ++ deployDuplicates

            assertTrue(
              duplicates.exists(_ == SkillName("algorithmic-art")),
              duplicates.nonEmpty,
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
            case Exit.Failure(c) => c.failureOption.exists { case SkillError.InvalidComponent(_, _) => true; case _ => false }
            case _ => false
          )
      ,
      test("fails when repo contains __"):
        defer:
          val result = artifactIdFor(Org("myorg"), Repo("my__repo"), List("myskill")).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists { case SkillError.InvalidComponent(_, _) => true; case _ => false }
            case _ => false
          )
      ,
      test("fails when path contains __"):
        defer:
          val result = artifactIdFor(Org("myorg"), Repo("myrepo"), List("my__skill")).exit.run
          assertTrue(result match
            case Exit.Failure(c) => c.failureOption.exists { case SkillError.InvalidComponent(_, _) => true; case _ => false }
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
            case Exit.Failure(c) => c.failureOption.exists { case DeployError(_, _, RepoErrorKind.OverlappingSkills(_, _)) => true; case _ => false }
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
            case Exit.Failure(c) => c.failureOption.exists { case DeployError(_, _, RepoErrorKind.OverlappingSkills(_, _)) => true; case _ => false }
            case _ => false
          )
      ,
    ),
  )
