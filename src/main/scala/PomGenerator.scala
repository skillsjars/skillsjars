import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*

object PomGenerator:

  // todo: maybe use scala xml?
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
    val licenseXml =
      val entries = licenses.map: license =>
        s"""    <license>
           |      <name>${license.name}</name>
           |      <url>${license.url}</url>
           |      <distribution>repo</distribution>
           |    </license>""".stripMargin
      s"  <licenses>\n${entries.mkString("\n")}\n  </licenses>"

    val scmUrl = s"https://github.com/$org/$repo"

    val pom = s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project xmlns="http://maven.apache.org/POM/4.0.0"
       |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
       |  <modelVersion>4.0.0</modelVersion>
       |  <packaging>jar</packaging>
       |  <groupId>$groupId</groupId>
       |  <artifactId>$artifactId</artifactId>
       |  <version>$version</version>
       |  <name>$name</name>
       |  <description>$description</description>
       |  <url>https://skillsjars.com</url>
       |  <scm>
       |    <url>$scmUrl</url>
       |    <connection>scm:git:$scmUrl.git</connection>
       |  </scm>
       |$licenseXml
       |  <developers>
       |    <developer>
       |      <id>skillsjars</id>
       |      <name>SkillsJars</name>
       |      <url>https://skillsjars.com</url>
       |    </developer>
       |  </developers>
       |</project>
       |""".stripMargin

    Chunk.fromArray(pom.getBytes)
