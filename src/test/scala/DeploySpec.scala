import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.concurrent.*
import zio.direct.*
import zio.test.*

import zio.compress.*
import zio.stream.*

object DeploySpec extends ZIOSpecDefault:

  private def readJarEntries(jar: Array[Byte]): ZIO[Any, Throwable, Set[String]] =
    ZStream.fromChunk(Chunk.fromArray(jar))
      .via(ZipUnarchiver.unarchive)
      .map(_._1.name)
      .runCollect
      .map(_.toSet)

  def spec = suite("DeploySpec")(
    suite("PomGenerator")(
      test("includes required fields"):
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
      test("works without license"):
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
    ),
    suite("groupIdFor")(
      test("with no subpath"):
        val gid = groupIdFor(Org("myorg"), Repo("myrepo"))
        assertTrue(gid.toString == "com.skillsjars.myorg.myrepo")
      ,
      test("with subpath"):
        val gid = groupIdFor(Org("myorg"), Repo("myrepo"), List("sub", "path"))
        assertTrue(gid.toString == "com.skillsjars.myorg.myrepo.sub.path")
      ,
    ),
    suite("deploy integration")(
      test("nonexistent repo returns RepoNotFound"):
        defer:
          val result = ZIO.scoped(Deployer.deployRepo(Org("nonexistent-org-abc123"), Repo("nonexistent-repo"))).exit.run
          assertTrue(result match
            case Exit.Failure(cause) => cause.failureOption.exists(_.isInstanceOf[DeployError.RepoNotFound])
            case _ => false
          )
      .provide(MockDeployer.layer)
      ,
      test("publishes one artifact per skill"):
        ZIO.scoped:
          defer:
            val repoInfo = Deployer.deployRepo(Org("anthropics"), Repo("skills")).run
            val published = ZIO.service[ConcurrentMap[String, PublishedArtifact]].run
            val allPublished = published.toList.run
            val expectedSkillNames = repoInfo.skills.map(_._1.skillName)
            val publishedArtifactIds = allPublished.map(_._2.artifactId)
            assertTrue(
              allPublished.size == repoInfo.skills.size,
              expectedSkillNames.forall(name => publishedArtifactIds.contains(MavenCentral.ArtifactId(name.toString))),
            )
      .provide(MockDeployer.layer)
      ,
      test("all published artifacts share the same version"):
        ZIO.scoped:
          defer:
            val repoInfo = Deployer.deployRepo(Org("anthropics"), Repo("skills")).run
            val published = ZIO.service[ConcurrentMap[String, PublishedArtifact]].run
            val allPublished = published.toList.run
            val versions = allPublished.map(_._2.version).distinct
            assertTrue(
              versions.size == 1,
              versions.head == repoInfo.version,
            )
      .provide(MockDeployer.layer)
      ,
      test("each published JAR contains SKILL.md and maven metadata"):
        ZIO.scoped:
          defer:
            Deployer.deployRepo(Org("anthropics"), Repo("skills")).run
            val published = ZIO.service[ConcurrentMap[String, PublishedArtifact]].run
            val allPublished = published.toList.run
            val results = ZIO.foreach(allPublished): (_, artifact) =>
              readJarEntries(artifact.jar).map: entries =>
                val mavenPrefix = s"META-INF/maven/${artifact.groupId}/${artifact.artifactId}/"
                (
                  entries.exists(_.endsWith("SKILL.md")),
                  entries.contains(s"${mavenPrefix}pom.xml"),
                  entries.contains(s"${mavenPrefix}pom.properties"),
                )
            .run
            assertTrue(
              results.forall(_._1),
              results.forall(_._2),
              results.forall(_._3),
            )
      .provide(MockDeployer.layer)
      ,
      test("published pom contains correct groupId and scm url"):
        ZIO.scoped:
          defer:
            Deployer.deployRepo(Org("browser-use"), Repo("browser-use")).run
            val published = ZIO.service[ConcurrentMap[String, PublishedArtifact]].run
            val allPublished = published.toList.run
            val results = allPublished.map: (_, artifact) =>
              (
                artifact.pom.contains(s"<groupId>${artifact.groupId}</groupId>"),
                artifact.pom.contains(s"<artifactId>${artifact.artifactId}</artifactId>"),
                artifact.pom.contains("https://github.com/browser-use/browser-use"),
              )
            assertTrue(
              allPublished.nonEmpty,
              results.forall(_._1),
              results.forall(_._2),
              results.forall(_._3),
            )
      .provide(MockDeployer.layer)
      ,
      test("nested skill has subpath in groupId and resource path"):
        ZIO.scoped:
          defer:
            Deployer.deployRepo(Org("vercel-labs"), Repo("agent-skills")).run
            val published = ZIO.service[ConcurrentMap[String, PublishedArtifact]].run
            val allPublished = published.toList.run
            val deploySkill = allPublished.find(_._2.artifactId == MavenCentral.ArtifactId("vercel-deploy-claimable"))
            assertTrue(deploySkill.isDefined) &&
            {
              val artifact = deploySkill.get._2
              val entries = readJarEntries(artifact.jar).run
              assertTrue(
                artifact.groupId == MavenCentral.GroupId("com.skillsjars.vercel-labs.agent-skills.claude.ai"),
                entries.exists(e => e.contains("claude.ai/vercel-deploy-claimable/SKILL.md")),
              )
            }
      .provide(MockDeployer.layer)
      ,
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds),
  )
