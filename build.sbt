enablePlugins(LauncherJarPlugin)

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

scalaVersion := "3.8.1"

Test / fork := true

val zioVersion = "2.1.24"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"                   % zioVersion,
  "dev.zio" %% "zio-concurrent"        % zioVersion,
  "dev.zio" %% "zio-cache"             % "0.2.7",
  "dev.zio" %% "zio-logging"           % "2.5.3",
  "dev.zio" %% "zio-direct"            % "1.0.0-RC7",
  "dev.zio" %% "zio-http"              % "3.8.1",
  "com.jamesward" %% "zio-mavencentral" % "0.5.3",

  "org.eclipse.jgit" % "org.eclipse.jgit" % "7.5.0.202512021534-r",

  "org.yaml" % "snakeyaml" % "2.5",

  "org.bouncycastle" % "bcpg-jdk18on" % "1.83",

  "org.webjars" % "webjars-locator-lite" % "1.1.3",
  "org.webjars.npm" % "tailwindcss__browser" % "4.1.18",

  "org.slf4j" % "slf4j-simple" % "2.0.17",

  "dev.zio" %% "zio-test"           % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"       % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia"  % zioVersion % Test,
)

lazy val reStartTest =
  inputKey[spray.revolver.AppProcess]("re-start, but test")

reStartTest :=
  Def.inputTask {
    spray.revolver.Actions.restartApp(
      streams.value,
      reLogTag.value,
      thisProjectRef.value,
      reForkOptions.value,
      Some("TestApp"),
      (Test / fullClasspath).value,
      reStartArgs.value,
      spray.revolver.Actions.startArgsParser.parsed
    )
  }.dependsOn(Compile / products).evaluated
