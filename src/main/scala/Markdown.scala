import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.http.*

/** Plain-text / markdown surface of SkillsJars, aimed at AI agents that fetch
  * pages with `Accept: text/markdown` rather than rendering HTML.
  */
object Markdown:

  // Single source of truth for the current SkillsJars build-plugin versions.
  private val gradlePluginVersion = "0.0.2"
  private val mavenPluginVersion = "0.0.7"
  private val sbtPluginVersion = "0.0.9"

  private val siteUrl = "https://skillsjars.com"

  val mediaType: MediaType = MediaType("text", "markdown")

  /** Does the client prefer markdown? Pure so it can be unit tested without a
    * live request. Matches the raw `Accept` header value.
    */
  def accepts(acceptHeader: Option[String]): Boolean =
    acceptHeader.exists: value =>
      val lower = value.toLowerCase
      lower.contains("text/markdown") || lower.contains("text/x-markdown")

  /** Build an `Ok` response carrying markdown with the right content type. */
  def response(markdown: String): Response =
    Response(
      status = Status.Ok,
      headers = Headers(Header.ContentType(mediaType)),
      body = Body.fromString(markdown),
    )

  private def coordinates(sj: SkillsJar): String =
    val version = sj.versions.headOption.map(_.toString).getOrElse("<version>")
    s"${sj.groupId}:${sj.artifactId}:$version"

  /** Render the catalog of SkillsJars as markdown, one entry per jar. */
  def skillsList(skillsJars: Seq[SkillsJar], maybeQuery: Option[String]): String =
    val header =
      maybeQuery match
        case Some(q) => s"# SkillsJars matching \"$q\"\n"
        case None    => "# SkillsJars\n"

    val intro =
      s"""|${skillsJars.size} Agent Skill(s) packaged as JARs on Maven Central (groupId `${Models.groupId}`).
          |
          |To wire SkillsJars into a project, fetch the setup guide: `curl -H "Accept: text/markdown" $siteUrl/setup`
          |""".stripMargin

    val body =
      if skillsJars.isEmpty then "\n_No SkillsJars found._\n"
      else
        skillsJars
          .sortBy(_.name.toLowerCase)
          .map: sj =>
            val desc = if sj.description.nonEmpty then s"\n${sj.description}\n" else "\n"
            s"""|## ${sj.name}
                |$desc
                |`${coordinates(sj)}`
                |""".stripMargin
          .mkString("\n")

    s"$header\n$intro\n$body"

  /** Instructions for an AI agent to set SkillsJars up in the current project. */
  val setup: String =
    s"""|# Setting up SkillsJars in your project
        |
        |SkillsJars packages [Agent Skills](https://agentskills.io/) as JARs on Maven Central
        |under the `${Models.groupId}` groupId. This guide walks you (an AI coding agent) through
        |wiring SkillsJars into a project: adding the extraction build plugin, choosing skills,
        |extracting them onto the filesystem, and recording the workflow in `AGENTS.md` so it
        |repeats on every checkout.
        |
        |## 1. Detect the project's build tool
        |
        |Look at the project root and pick the matching section below:
        |
        |- `pom.xml` present -> **Maven**
        |- `build.gradle` / `build.gradle.kts` / `settings.gradle(.kts)` present -> **Gradle**
        |- `build.sbt` present -> **sbt**
        |
        |Only follow the one section that matches the project.
        |
        |## 2. Add the SkillsJars extraction plugin
        |
        |### Maven
        |
        |Add the plugin to the `<build><plugins>` section of `pom.xml`. SkillsJar dependencies go
        |*inside* the plugin's own `<dependencies>` so they never leak onto your app's classpath:
        |
        |```xml
        |<build>
        |    <plugins>
        |        <plugin>
        |            <groupId>com.skillsjars</groupId>
        |            <artifactId>maven-plugin</artifactId>
        |            <version>$mavenPluginVersion</version>
        |            <dependencies>
        |                <!-- SkillsJar dependencies (see step 3) -->
        |                <dependency>
        |                    <groupId>com.skillsjars</groupId>
        |                    <artifactId>SKILLJAR_ARTIFACT_ID</artifactId>
        |                    <version>SKILLJAR_VERSION</version>
        |                </dependency>
        |            </dependencies>
        |        </plugin>
        |    </plugins>
        |</build>
        |```
        |
        |### Gradle
        |
        |Apply the plugin in `build.gradle.kts` (or `build.gradle`) and declare SkillsJar
        |dependencies. Use a resolvable-but-isolated configuration such as `testRuntimeOnly`
        |so the skills stay off your main runtime classpath — the plugin scans every resolvable
        |configuration for the `com.skillsjars` group:
        |
        |```kotlin
        |plugins {
        |    id("com.skillsjars.gradle-plugin") version "$gradlePluginVersion"
        |}
        |
        |dependencies {
        |    // SkillsJar dependencies (see step 3)
        |    testRuntimeOnly("com.skillsjars:SKILLJAR_ARTIFACT_ID:SKILLJAR_VERSION")
        |}
        |```
        |
        |### sbt
        |
        |Add the plugin to `project/plugins.sbt`:
        |
        |```scala
        |addSbtPlugin("com.skillsjars" % "skillsjars-sbt-plugin" % "$sbtPluginVersion")
        |```
        |
        |Then, in `build.sbt`, set an output directory and declare SkillsJar dependencies in the
        |plugin's `Skills` configuration (which keeps them off the compile/runtime classpath):
        |
        |```scala
        |skillsJarsOutputDir := Some(file(".kiro/skills"))
        |
        |// SkillsJar dependencies (see step 3)
        |libraryDependencies += "com.skillsjars" % "SKILLJAR_ARTIFACT_ID" % "SKILLJAR_VERSION" % Skills
        |```
        |
        |### Finding the latest plugin (or any library) version
        |
        |The plugin versions above were current when this guide was written. To resolve the latest
        |version of any Maven Central artifact, request `javadocs.dev` — it returns JSON like
        |`{"version":"0.4.0"}`:
        |
        |```bash
        |# https://www.javadocs.dev/<groupId>/<artifactId>/latest
        |curl https://www.javadocs.dev/com.jamesward/zio-http-mcp_3/latest
        |```
        |
        |For example, `curl https://www.javadocs.dev/com.skillsjars/maven-plugin/latest` returns the
        |newest Maven plugin version, and `.../com.skillsjars/gradle-plugin/latest` the newest Gradle
        |plugin version. (For the sbt plugin, use the cross-versioned artifactId, e.g.
        |`skillsjars-sbt-plugin_sbt2_3`.)
        |
        |## 3. Choose which SkillsJars to add
        |
        |List the available skills as markdown and pick the ones relevant to the project:
        |
        |```bash
        |curl -H "Accept: text/markdown" $siteUrl/
        |```
        |
        |You can also search: `curl -H "Accept: text/markdown" "$siteUrl/?q=KEYWORD"`.
        |
        |Each entry prints its Maven coordinates in `groupId:artifactId:version` form, e.g.
        |`com.skillsjars:anthropics__skills__pdf:2026_02_06-1ed29a0`. For every skill you want,
        |replace the `SKILLJAR_ARTIFACT_ID` / `SKILLJAR_VERSION` placeholders from step 2 with the
        |`artifactId` and `version` from its coordinates (add one dependency entry per skill).
        |
        |> Security note: Agent Skills can do harmful things. Vet each skill before adding it — the
        |> automated scan on SkillsJars.com is not a substitute for review.
        |
        |## 4. Extract the skills to the filesystem
        |
        |First choose the build launcher. Prefer a project-local wrapper committed to the repo (it
        |pins the build-tool version), and fall back to a globally installed launcher on your `PATH`
        |only when no wrapper is present:
        |
        |- Maven: use `./mvnw` if it exists in the project root, otherwise `mvn`.
        |- Gradle: use `./gradlew` if it exists, otherwise `gradle`.
        |- sbt: use `./sbt` if it exists, otherwise `sbt`.
        |
        |On Windows the wrappers are `mvnw.cmd`, `gradlew.bat`, and `sbt.bat`. A POSIX shell can pick
        |the right one automatically (Maven shown):
        |
        |```bash
        |MVN=$$([ -x ./mvnw ] && echo ./mvnw || echo mvn)
        |"$$MVN" --version
        |```
        |
        |Then run the extraction command for your build tool, pointing at the directory your agent
        |reads skills from (e.g. `.kiro/skills`, `.claude/skills`, or `.agent/skills`):
        |
        |```bash
        |# Maven   (use ./mvnw if present, else mvn)
        |./mvnw skillsjars:extract -Ddir=.kiro/skills
        |
        |# Gradle  (use ./gradlew if present, else gradle)
        |./gradlew extractSkillsJars -Pdir=.kiro/skills
        |
        |# sbt     (use ./sbt if present, else sbt; uses skillsJarsOutputDir set above, or pass a path)
        |./sbt extractSkillsJars
        |```
        |
        |Extraction clears the target directory and writes each skill under
        |`skillsjars__<org>__<repo>__<skill>/SKILL.md`. Add that directory to `.gitignore` —
        |it is regenerated from the build.
        |
        |## 5. Record the workflow in AGENTS.md
        |
        |So the skills are always present, tell agents to run extraction before working on the
        |project. Create `AGENTS.md` in the project root if it does not exist, or merge the
        |following into the existing file (adjust the command to the build tool and the skills
        |directory you chose):
        |
        |    ## Agent Skills (SkillsJars)
        |
        |    This project pulls in Agent Skills as SkillsJars build dependencies. Before working,
        |    extract them so they are available on the filesystem (prefer the project's wrapper —
        |    `./mvnw` / `./gradlew` / `./sbt` — and fall back to `mvn` / `gradle` / `sbt` on PATH):
        |
        |        # Maven:  ./mvnw skillsjars:extract -Ddir=.kiro/skills    (or mvn)
        |        # Gradle: ./gradlew extractSkillsJars -Pdir=.kiro/skills  (or gradle)
        |        # sbt:    ./sbt extractSkillsJars                         (or sbt)
        |
        |    Read the extracted `SKILL.md` files under `.kiro/skills/` and follow any that are
        |    relevant to the task. To add more skills, browse $siteUrl and add the dependency,
        |    then re-run extraction.
        |
        |If the project uses a different agent instructions file (`CLAUDE.md`, `.cursorrules`,
        |`.github/copilot-instructions.md`, etc.), add the same guidance there instead of, or in
        |addition to, `AGENTS.md`.
        |""".stripMargin
