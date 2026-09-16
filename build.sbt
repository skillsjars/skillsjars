enablePlugins(JavaAppPackaging)

// Hack Alert: This is the default when not in buildpacks (i.e. `default`)
// In buildpacks it is javadoccentral which puts it alphabetically after dev.zio.zio-constraintless_3-0.3.1.jar
// This causes the wrong Main-Class to get picked up.
// https://github.com/paketo-buildpacks/executable-jar/issues/206
organization := "default"

name := "skillsjars"

// so we don't have to wait on Maven Central sync
//resolvers += "OSS Staging" at "https://oss.sonatype.org/content/groups/staging"

scalacOptions ++= Seq(
  //"-Yexplicit-nulls", // doesn't seem to work anymore
  "-language:strictEquality",
  // "-Xfatal-warnings", // doesn't seem to work anymore
)

scalaVersion := "3.9.0"

Test / fork := true

val zioVersion = "2.1.26"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"                    % zioVersion,
  "dev.zio" %% "zio-concurrent"         % zioVersion,
  "dev.zio" %% "zio-cache"              % "0.2.8",
  "dev.zio" %% "zio-logging"            % "2.5.3",
  "dev.zio" %% "zio-direct"             % "1.0.0-RC7",
  "dev.zio" %% "zio-http"               % "3.11.4",
  "com.jamesward" %% "zio-mavencentral" % "0.14.0",
  // Previously transitive via zio-mavencentral <= 0.5.4; now declared
  // directly because zio-mavencentral dropped its zip dep.
  "dev.zio" %% "zio-streams-compress-zip" % "2.1.4",

  "org.eclipse.jgit" % "org.eclipse.jgit" % "7.7.1.202607240634-r",

  "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
  "org.virtuslab" %% "scala-yaml" % "0.3.3",
  "dev.zio" %% "zio-config"           % "4.1.0",
  "dev.zio" %% "zio-config-typesafe"  % "4.1.0",

  "org.webjars" % "webjars-locator-lite" % "1.1.4",
  "org.webjars.npm" % "tailwindcss__browser" % "4.3.3",

  "org.slf4j" % "slf4j-simple" % "2.0.19",

  "dev.zio" %% "zio-test"           % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"       % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia"  % zioVersion % Test,
)

Test / run / mainClass := Some("TestApp")

// SkillsJars — Agent Skills unpacked onto the filesystem (see AGENTS.md).
// Declared in the plugin's `Skills` config so they stay off the compile/runtime classpath.
skillsJarsOutputDir := Some(file(".kiro/skills"))
libraryDependencies += "com.jamesward" % "skills" % "0.0.3" % Skills
