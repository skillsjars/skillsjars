import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.compress.*
import zio.stream.*
import zio.test.*
import zio.test.Assertion.*

import java.io.File
import java.nio.file.Files

object JarCreatorSpec extends ZIOSpecDefault:

  private def readJarEntries(jar: Chunk[Byte]): ZIO[Any, Throwable, Map[String, Chunk[Byte]]] =
    ZStream.fromChunk(jar)
      .via(ZipUnarchiver.unarchive)
      .mapZIO: (entry, stream) =>
        stream.runCollect.map(content => entry.name -> content)
      .runCollect
      .map(_.toMap)

  def spec = suite("JarCreatorSpec")(
    test("validates the contents of a Jar according to SPEC.md"):
      ZIO.scoped:
        for
          tempDir <- ZIO.attempt(Files.createTempDirectory("skillDir")).map(_.toFile)
          _ <- ZIO.attempt:
            val skillMd = new File(tempDir, "SKILL.md")
            Files.writeString(skillMd.toPath, "test skill")
            val otherFile = new File(tempDir, "other.txt")
            Files.writeString(otherFile.toPath, "other content")
            val subDir = new File(tempDir, "subdir")
            subDir.mkdir()
            val nestedFile = new File(subDir, "nested.txt")
            Files.writeString(nestedFile.toPath, "nested content")

          org = Org("myorg")
          repo = Repo("myrepo")
          pom = Chunk.fromArray("<project>...</project>".getBytes)
          groupId = MavenCentral.GroupId("com.skillsjars")
          artifactId = MavenCentral.ArtifactId("myorg__myrepo__myskill")
          version = MavenCentral.Version("2026_02_16-abc1234")

          jarBytes <- JarCreator.create(
            tempDir,
            org,
            repo,
            List("myskill"),
            pom,
            groupId,
            artifactId,
            version
          )

          entries <- readJarEntries(jarBytes)
        yield
          val resourcePrefix = "META-INF/resources/skills/myorg/myrepo/myskill/"
          val mavenPrefix = "META-INF/maven/com.skillsjars/myorg__myrepo__myskill/"

          assertTrue(
            entries.contains("META-INF/"),
            entries.contains("META-INF/MANIFEST.MF"),
            entries.contains("META-INF/resources/"),
            entries.contains("META-INF/resources/skills/"),
            entries.contains("META-INF/resources/skills/myorg/"),
            entries.contains("META-INF/resources/skills/myorg/myrepo/"),
            entries.contains(s"${resourcePrefix}SKILL.md"),
            entries.contains(s"${resourcePrefix}other.txt"),
            entries.contains(s"${resourcePrefix}subdir/"),
            entries.contains(s"${resourcePrefix}subdir/nested.txt"),
            entries.contains("META-INF/maven/"),
            entries.contains("META-INF/maven/com.skillsjars/"),
            entries.contains(s"${mavenPrefix}pom.xml"),
            entries.contains(s"${mavenPrefix}pom.properties"),
            String(entries(s"${resourcePrefix}SKILL.md").toArray) == "test skill",
            entries(s"${mavenPrefix}pom.xml") == pom
          )
      ,
    test("validates the contents of a Jar with subPath according to SPEC.md"):
      ZIO.scoped:
        for
          tempDir <- ZIO.attempt(Files.createTempDirectory("skillDirSubPath")).map(_.toFile)
          _ <- ZIO.attempt:
            val skillMd = new File(tempDir, "SKILL.md")
            Files.writeString(skillMd.toPath, "subpath skill")

          org = Org("myorg")
          repo = Repo("myrepo")
          pom = Chunk.fromArray("<project>...</project>".getBytes)
          groupId = MavenCentral.GroupId("com.skillsjars")
          artifactId = MavenCentral.ArtifactId("myorg__myrepo__sub__path__myskill")
          version = MavenCentral.Version("2026_02_16-abc1234")

          jarBytes <- JarCreator.create(
            tempDir,
            org,
            repo,
            List("sub", "path", "myskill"),
            pom,
            groupId,
            artifactId,
            version
          )

          entries <- readJarEntries(jarBytes)
        yield
          val resourcePrefix = "META-INF/resources/skills/myorg/myrepo/sub/path/myskill/"
          val mavenPrefix = "META-INF/maven/com.skillsjars/myorg__myrepo__sub__path__myskill/"

          assertTrue(
            entries.contains("META-INF/"),
            entries.contains("META-INF/resources/"),
            entries.contains("META-INF/resources/skills/"),
            entries.contains("META-INF/resources/skills/myorg/"),
            entries.contains("META-INF/resources/skills/myorg/myrepo/"),
            entries.contains("META-INF/resources/skills/myorg/myrepo/sub/"),
            entries.contains("META-INF/resources/skills/myorg/myrepo/sub/path/"),
            entries.contains(s"${resourcePrefix}SKILL.md"),
            entries.contains(s"${mavenPrefix}pom.xml"),
            String(entries(s"${resourcePrefix}SKILL.md").toArray) == "subpath skill"
          )
  )
