import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*

object PomGenerator:

  def generate(
    groupId: MavenCentral.GroupId,
    artifactId: MavenCentral.ArtifactId,
    version: MavenCentral.Version,
    name: SkillName,
    description: String,
    licenses: NonEmptyChunk[License],
    org: Org,
    repo: Repo,
  ): Chunk[Byte] =
    val scmUrl = s"https://github.com/$org/$repo"

    val pom =
      <project xmlns="http://maven.apache.org/POM/4.0.0"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
               xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
        <modelVersion>4.0.0</modelVersion>
        <packaging>jar</packaging>
        <groupId>{groupId}</groupId>
        <artifactId>{artifactId}</artifactId>
        <version>{version}</version>
        <name>{name}</name>
        <description>{description}</description>
        <url>https://skillsjars.com</url>
        <scm>
          <url>{scmUrl}</url>
          <connection>scm:git:{scmUrl}.git</connection>
        </scm>
        <licenses>
          {licenses.toList.map: license =>
            <license>
              <name>{license.name}</name>
              <url>{license.url}</url>
              <distribution>repo</distribution>
            </license>
          }
        </licenses>
        <developers>
          <developer>
            <id>skillsjars</id>
            <name>SkillsJars</name>
            <url>https://skillsjars.com</url>
          </developer>
        </developers>
      </project>

    // todo: probably a better way to create the Chunk
    val writer = new java.io.StringWriter()
    scala.xml.XML.write(writer, pom, "UTF-8", xmlDecl = true, doctype = null)
    Chunk.fromArray(writer.toString.getBytes("UTF-8"))
