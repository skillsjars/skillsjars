import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.http.URL
import zio.http.template2.*

import java.net.URLEncoder

object UI:

  private def navLink(href0: String, label: String, active: Boolean): Dom =
    val cls =
      if active then "px-3 py-1.5 rounded-md text-sm font-medium bg-blue-600 text-white"
      else "px-3 py-1.5 rounded-md text-sm font-medium text-gray-600 hover:text-gray-900 hover:bg-gray-100"
    a(href := href0, `class` := cls, label)

  def page(pageTitle: String, pageContent: Dom, tailwind: URL, currentPath: String = "/"): Dom =
    html(
      head(
        title(pageTitle),
        meta(charset := "UTF-8"),
        meta(name := "viewport", Dom.attr("content", "width=device-width, initial-scale=1.0")),
        script(src := tailwind),
        link(rel := "icon", href := "/favicon.ico", size := "any"),
        link(rel := "icon", href := "/favicon.png", `type` := "image/png"),
        link(rel := "apple-touch-icon", href := "/favicon.png", `type` := "image/png"),
      ),
      body(
        `class` := "bg-gray-50 min-h-screen",
        div(
          `class` := "max-w-4xl mx-auto px-4 py-8",
          header(
            `class` := "mb-8",
            div(
              `class` := "flex items-center justify-between",
              h1(
                `class` := "text-3xl font-bold text-gray-900",
                a(href := "/", "SkillsJars"),
              ),
              nav(
                `class` := "flex gap-1 bg-gray-200 rounded-lg p-1",
                navLink("/", "Find SkillsJars", currentPath == "/"),
                navLink("/docs", "Documentation", currentPath == "/docs"),
              ),
            ),
            p(`class` := "text-gray-600 mt-1", "Agent Skills on Maven Central"),
          ),
          pageContent,
          footer(
            `class` := "mt-12 pt-6 border-t border-gray-200 text-gray-500 text-sm",
            a(href := "https://github.com/skillsjars/skillsjars", "github.com/skillsjars/skillsjars"),
          ),
        ),
      ),
    )

  def index(skillsJars: Seq[SkillsJar], maybeQuery: Option[String], buildTool: BuildTool, tailwind: URL, maybeError: Option[SkillsJarService.ServiceError] = None): Dom =
    page(
      "SkillsJars",
      div(
        maybeError.map(errorCard).getOrElse(Dom.empty),
        searchForm(maybeQuery, buildTool),
        deployForm,
        buildToolSelector(buildTool, maybeQuery),
        skillsJarList(skillsJars, buildTool),
        snippetScript(buildTool),
      ),
      tailwind
    )

  private def errorCard(error: SkillsJarService.ServiceError): Dom =
    val message = error match
      case SkillsJarService.ServiceError.FetchFailed(reason) => s"Failed to fetch SkillsJars: $reason"

    div(
      `class` := "mb-6 bg-red-50 border border-red-200 rounded-lg p-4",
      p(`class` := "font-semibold text-red-800", "Error"),
      p(`class` := "text-sm text-red-700 mt-1", message),
    )

  private def searchForm(maybeQuery: Option[String], buildTool: BuildTool): Dom =
    form(
      `class` := "mb-6",
      action := "/",
      method := "get",
      input(`type` := "hidden", name := "bt", value := buildTool.param),
      div(
        `class` := "flex gap-2",
        input(
          `type` := "text",
          name := "q",
          placeholder := "Search skills by name or description...",
          `class` := "flex-1 px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500",
          maybeQuery.map(q => value := q).getOrElse(Dom.empty),
        ),
        button(
          `type` := "submit",
          `class` := "px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 cursor-pointer",
          "Search",
        ),
        maybeQuery.fold(Dom.empty): _ =>
          a(
            href := s"/?bt=${buildTool.param}",
            `class` := "px-6 py-2 bg-gray-200 text-gray-700 rounded-lg hover:bg-gray-300 cursor-pointer flex items-center",
            "Clear",
          ),
      ),
    )

  private val deployForm: Dom =
    details(
      `class` := "mb-6 bg-white rounded-lg shadow p-4",
      summary(`class` := "cursor-pointer font-semibold text-gray-700", "Publish a SkillsJar"),
      form(
        `class` := "mt-4",
        action := "/deploy",
        method := "post",
        div(
          `class` := "flex gap-2 items-end",
          div(
            `class` := "flex-1",
            label(`class` := "block text-sm font-medium text-gray-700 mb-1", `for` := "org", "GitHub Org"),
            input(
              `type` := "text",
              name := "org",
              id := "org",
              required,
              placeholder := "myorg",
              `class` := "w-full px-3 py-2 border border-gray-300 rounded-lg",
            ),
          ),
          div(
            `class` := "flex-1",
            label(`class` := "block text-sm font-medium text-gray-700 mb-1", `for` := "repo", "GitHub Repo"),
            input(
              `type` := "text",
              name := "repo",
              id := "repo",
              required,
              placeholder := "myrepo",
              `class` := "w-full px-3 py-2 border border-gray-300 rounded-lg",
            ),
          ),
          button(
            `type` := "submit",
            `class` := "px-6 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 cursor-pointer",
            "Deploy",
          ),
        ),
      ),
    )

  private def buildToolSelector(buildTool: BuildTool, maybeQuery: Option[String]): Dom =
    val bts = BuildTool.values.toSeq
    div(
      `class` := "mb-4 inline-flex rounded-lg overflow-hidden border border-gray-300",
      bts.zipWithIndex.map: (bt, idx) =>
        val queryPart = maybeQuery.fold("")(q => s"&q=${URLEncoder.encode(q, "UTF-8")}")
        val borderCls = if idx < bts.size - 1 then " border-r border-gray-300" else ""
        a(
          href := s"/?bt=${bt.param}$queryPart",
          `class` := (
            if bt == buildTool then s"px-4 py-2 bg-blue-600 text-white font-medium$borderCls"
            else s"px-4 py-2 bg-white text-gray-700 hover:bg-gray-50 cursor-pointer$borderCls"
          ),
          bt.label,
        ),
    )

  private def skillsJarList(skillsJars: Seq[SkillsJar], buildTool: BuildTool): Dom =
    if skillsJars.isEmpty then
      p(`class` := "text-gray-500 text-center py-8", "No SkillsJars found.")
    else
      div(
        `class` := "space-y-3",
        skillsJars.map(sj => skillsJarCard(sj, buildTool)),
      )

  private def skillsJarCard(sj: SkillsJar, buildTool: BuildTool): Dom =
    val gid = s"${sj.groupId}"
    val aid = s"${sj.artifactId}"
    val latestVersion = sj.versions.headOption.map(_.toString).getOrElse("")
    div(
      `class` := "bg-white rounded-lg shadow p-4",
      div(
        `class` := "flex justify-between items-start",
        div(
          h3(`class` := "font-semibold text-gray-900", sj.name),
          p(`class` := "text-sm text-gray-600 mt-1", sj.description),
        ),
        div(
          `class` := "ml-4",
          versionSelect(sj),
        ),
      ),
      if sj.versions.nonEmpty then
        div(
          `class` := "mt-3 relative",
          pre(
            id := s"snippet-$aid",
            `class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto pr-16",
            snippetText(gid, aid, latestVersion, buildTool),
          ),
          button(
            `type` := "button",
            Dom.attr("onclick", s"copySnippet(this,'$aid')"),
            `class` := "absolute top-2 right-2 px-2 py-1 bg-white border border-gray-300 rounded text-xs text-gray-600 hover:bg-gray-50 cursor-pointer",
            "Copy",
          ),
        )
      else Dom.empty,
    )

  private def versionSelect(sj: SkillsJar): Dom =
    if sj.versions.isEmpty then
      span(`class` := "text-gray-400 text-sm", "No versions")
    else
      select(
        `class` := "px-2 py-1 border border-gray-300 rounded text-sm",
        Dom.attr("onchange", s"updateSnippet(this,'${sj.groupId}','${sj.artifactId}')"),
        sj.versions.map: v =>
          option(value := v.toString, v.toString),
      )

  private def snippetText(groupId: String, artifactId: String, version: String, buildTool: BuildTool): String =
    buildTool match
      case BuildTool.Maven =>
        s"<dependency>\n    <groupId>$groupId</groupId>\n    <artifactId>$artifactId</artifactId>\n    <version>$version</version>\n</dependency>"
      case BuildTool.Gradle =>
        s"runtimeOnly(\"$groupId:$artifactId:$version\")"
      case BuildTool.Sbt =>
        s""""$groupId" % "$artifactId" % "$version""""

  private def snippetScript(buildTool: BuildTool): Dom =
    script.inlineJs(s"""
      var buildTool = '${buildTool.param}';
      function updateSnippet(sel, gid, aid) {
        var v = sel.value;
        var el = document.getElementById('snippet-' + aid);
        if (!el) return;
        if (buildTool === 'maven') {
          el.textContent = '<dependency>\\n    <groupId>' + gid + '</groupId>\\n    <artifactId>' + aid + '</artifactId>\\n    <version>' + v + '</version>\\n</dependency>';
        } else if (buildTool === 'gradle') {
          el.textContent = "runtimeOnly(\\\"" + gid + ':' + aid + ':' + v + "\\\")";
        } else {
          el.textContent = '"' + gid + '" % "' + aid + '" % "' + v + '"';
        }
      }
      function copySnippet(btn, aid) {
        var el = document.getElementById('snippet-' + aid);
        if (!el) return;
        navigator.clipboard.writeText(el.textContent).then(function() {
          var orig = btn.textContent;
          btn.textContent = 'Copied!';
          setTimeout(function() { btn.textContent = orig; }, 2000);
        });
      }
    """)

  def docs(tailwind: URL): Dom =
    page(
      "SkillsJars - Docs",
      div(
        // Intro
        div(
          `class` := "prose max-w-none",
          h2(`class` := "text-2xl font-bold text-gray-900 mt-6 mb-4", "Using SkillsJars"),
          p(`class` := "text-gray-700 mb-4",
            """SkillsJars are Agent Skills packaged as JARs on Maven Central.
              |They can be used with AI code assistants, custom agents, and frameworks like Spring AI.
              |Managing Agent Skills as packaged dependencies enables versioning, grouping as transitive dependencies, and avoiding copy & pasting files.
              |""".stripMargin
          ),
        ),

        // AI Code Assistants
        div(
          `class` := "bg-white rounded-lg shadow p-6 mt-6",
          h3(`class` := "text-xl font-bold text-gray-900 mb-3", "AI Code Assistants"),
          p(`class` := "text-gray-700 mb-4",
            "Most AI code assistants expect Agent Skills as files on the filesystem. The SkillsJars build plugins extract skills from your project dependencies into a directory your assistant can read.",
          ),

          // Step 1
          p(`class` := "font-semibold text-gray-800 mb-2", "1. Add SkillsJar dependencies"),
          p(`class` := "text-gray-700 mb-4",
            "Browse Agent Skills on ",
            a(href := "/", `class` := "text-blue-600 hover:underline", "SkillsJars.com"),
            " and add them to your project using the dependency snippet for your build tool.",
          ),

          // Step 2 - Gradle
          p(`class` := "font-semibold text-gray-800 mb-2", "2. Add the extraction plugin"),

          p(`class` := "text-gray-700 mt-4 mb-1", "Gradle:"),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            """plugins {
    id("com.skillsjars.gradle-plugin") version "0.0.2"
}""",
          ),

          p(`class` := "text-gray-700 mb-1", "Maven:"),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            """<build>
    <plugins>
        <plugin>
            <groupId>com.skillsjars</groupId>
            <artifactId>maven-plugin</artifactId>
            <version>0.0.2</version>
        </plugin>
    </plugins>
</build>""",
          ),

          // Step 3
          p(`class` := "font-semibold text-gray-800 mb-2", "3. Extract skills"),
          p(`class` := "text-gray-700 mb-1",
            "Run the extraction command, specifying the directory your AI assistant expects:",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-2",
            "# Gradle\n./gradlew extractSkillsJars -Pdir=.kiro/skills\n\n# Maven\n./mvnw skillsjars:extract -Ddir=.kiro/skills",
          ),
          p(`class` := "text-sm text-gray-500 mb-4",
            "Replace ", code(`class` := "bg-gray-100 px-1 rounded", ".kiro/skills"), " with the path your AI assistant uses for skills.",
          ),

          // AGENTS.md
          div(
            `class` := "bg-blue-50 border border-blue-200 rounded-lg p-4 mt-4",
            p(`class` := "font-semibold text-blue-800 mb-1", "Tip: AGENTS.md"),
            p(`class` := "text-sm text-blue-700",
              "Your project's ", code(`class` := "bg-blue-100 px-1 rounded", "AGENTS.md"),
              " can instruct AI agents to run the extraction command before working with the project. This way, skills are always available without manual setup.",
            ),
          ),
        ),

        // Custom Agents
        div(
          `class` := "bg-white rounded-lg shadow p-6 mt-6",
          h3(`class` := "text-xl font-bold text-gray-900 mb-3", "Custom Agents"),

          // Spring AI
          div(
            `class` := "mb-6",
            h4(`class` := "text-lg font-semibold text-gray-800 mb-2", "Spring AI"),
            p(`class` := "text-gray-700 mb-2",
              "The ",
              a(href := "https://github.com/spring-ai-community/spring-ai-agent-utils", `class` := "text-blue-600 hover:underline", "Spring AI Agent Utils"),
              " project provides a SkillsTool that integrates Agent Skills directly with Spring AI agents. Support for SkillsJars is coming soon.",
            ),
          ),

          // Direct JAR reading
          div(
            h4(`class` := "text-lg font-semibold text-gray-800 mb-2", "Reading SkillsJars Directly"),
            p(`class` := "text-gray-700 mb-2",
              "Custom agents on the JVM can read skills directly from SkillsJar dependencies on the classpath. Skills are located at a well-known path inside the JAR:",
            ),
            pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-2",
              "META-INF/skills/<org>/<repo>/<skill>/SKILL.md",
            ),
            p(`class` := "text-gray-700",
              "Each ", code(`class` := "bg-gray-100 px-1 rounded", "SKILL.md"),
              " follows the ", a(href := "https://agentskills.io/specification", `class` := "text-blue-600 hover:underline", "Agent Skills specification"),
              " with YAML front-matter containing the skill name, description, and other metadata. Additional files referenced by the skill are included alongside the ", code(`class` := "bg-gray-100 px-1 rounded", "SKILL.md"), ".",
            ),
          ),
        ),
      ),
      tailwind,
      currentPath = "/docs",
    )

  def deployResult(results: Seq[DeployResult], tailwind: URL): Dom =
    page(
      "Deploy Results",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "space-y-3 mt-4",
          results.map(deployResultCard),
        ),
      ),
      tailwind,
    )

  private def deployResultCard(result: DeployResult): Dom =
    result match
      case DeployResult.Success(groupId, artifactId, version) =>
        div(
          `class` := "bg-green-50 border border-green-200 rounded-lg p-4",
          p(`class` := "font-semibold text-green-800", "Deployed successfully. It will take ~1hr for the artifact to be available on Maven Central."),
          p(`class` := "text-sm font-mono text-green-700 mt-1", s"$groupId:$artifactId:$version"),
        )
      case DeployResult.Skipped(skillName, reason) =>
        div(
          `class` := "bg-amber-50 border border-amber-200 rounded-lg p-4",
          p(`class` := "font-semibold text-amber-800", s"Skipped: $skillName"),
          p(`class` := "text-sm text-amber-700 mt-1", reason),
        )
      case DeployResult.Failure(error) =>
        val message = error match
          case DeployError.RepoNotFound(org, repo) => s"Repository not found: $org/$repo"
          case DeployError.NoSkillsDirectory(org, repo) => s"No skills directory in $org/$repo"
          case DeployError.InvalidSkillMd(path, reason) => s"Invalid SKILL.md at $path: $reason"
          case DeployError.InvalidComponent(component, reason) => s"Invalid component '$component': $reason"
          case DeployError.DuplicateVersion(gid, aid, v) => s"Duplicate version: $gid:$aid:$v"
          case DeployError.NoLicense(org, repo, skillName) => s"No license found for skill '$skillName' in $org/$repo. Add a LICENSE file or specify license in SKILL.md frontmatter."
          case DeployError.OverlappingSkills(org, repo, path1, path2) =>
            val p1 = if path1.isEmpty then "/" else path1.mkString("/")
            val p2 = if path2.isEmpty then "/" else path2.mkString("/")
            s"Overlapping skills in $org/$repo: '$p1' and '$p2'. A skill directory cannot be an ancestor of another."
          case DeployError.NoPublishableSkills(org, repo, skipped) =>
            val reasons = skipped.map(s => s"${s.skillName}: ${s.reason}").mkString("; ")
            s"No skills could be published from $org/$repo. $reasons"
          case DeployError.PublishFailed(reason) => s"Publish failed: $reason"

        div(
          `class` := "bg-red-50 border border-red-200 rounded-lg p-4",
          p(`class` := "font-semibold text-red-800", "Deploy failed"),
          p(`class` := "text-sm text-red-700 mt-1", message),
        )

enum DeployResult:
  case Success(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version)
  case Skipped(skillName: SkillName, reason: String)
  case Failure(error: DeployError)
