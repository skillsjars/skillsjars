import Models.*
import com.jamesward.zio_mavencentral.MavenCentral

object PomGenerator:

  // todo: maybe use scala xml?
  def generate(
    groupId: MavenCentral.GroupId,
    artifactId: MavenCentral.ArtifactId,
    version: MavenCentral.Version,
    name: SkillName,
    description: String,
    maybeLicense: Option[String],
    org: Org,
    repo: Repo,
  ): String =
    val licenseXml = maybeLicense.fold(""):
      license =>
        s"""  <licenses>
           |    <license>
           |      <name>$license</name>
           |      <distribution>repo</distribution>
           |    </license>
           |  </licenses>""".stripMargin

    val scmUrl = s"https://github.com/$org/$repo"

    s"""<?xml version="1.0" encoding="UTF-8"?>
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
