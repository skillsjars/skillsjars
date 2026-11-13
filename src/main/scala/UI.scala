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
              `class` := "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-3",
              h1(
                `class` := "text-3xl font-bold text-gray-900",
                a(href := "/", "SkillsJars"),
              ),
              nav(
                `class` := "flex gap-1 bg-gray-200 rounded-lg p-1 self-start sm:self-auto",
                navLink("/", "Find SkillsJars", currentPath == "/"),
                navLink("/docs", "Documentation", currentPath == "/docs"),
              ),
            ),
            p(`class` := "text-gray-600 mt-1", "Agent Skills on Maven Central"),
          ),
          pageContent,
          pageScript,
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
        agentSetupBanner,
        searchForm(maybeQuery, buildTool),
        deployForm,
        buildToolSelector(buildTool, maybeQuery),
        skillsJarList(skillsJars, buildTool),
        snippetScript(buildTool),
        script.inlineJs("window.addEventListener('pageshow',function(e){if(e.persisted){var b=document.querySelector('form[action=\"/deploy\"] button[type=submit]');if(b){b.disabled=false;b.textContent='Deploy'}}})"),
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

  private lazy val agentSetupBanner: Dom =
    val prompt = "Setup SkillsJars in my project by following the instructions at: https://skillsjars.com/setup"
    div(
      `class` := "mb-6 bg-indigo-50 border border-indigo-200 rounded-lg p-4",
      p(`class` := "font-semibold text-indigo-800 mb-3", "Set up SkillsJars in your AI agent"),
      p(`class` := "text-sm font-semibold text-indigo-800 mb-1", "Option 1: Build-driven"),
      p(`class` := "text-sm text-indigo-700 mb-2",
        "Declare SkillsJars as dependencies in your build, which extracts them for your agent. Paste the following into your AI coding agent:",
      ),
      copyBlock("setup-prompt", prompt),
      p(`class` := "text-sm font-semibold text-indigo-800 mt-4 mb-1", "Option 2: MCP"),
      p(`class` := "text-sm text-indigo-700 mb-2",
        "Connect your agent to the SkillsJars MCP server, which serves the latest version of every SkillsJar as Skills over MCP. Use this with agents that support Skills over MCP.",
      ),
      mcpSetupTabs,
    )

  /** A `pre` with a Copy button; `id` must be unique on the page. */
  private def copyBlock(id0: String, text: String): Dom =
    div(
      `class` := "relative",
      pre(
        id := s"snippet-$id0",
        `class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto whitespace-pre-wrap pr-16",
        text,
      ),
      button(
        `type` := "button",
        Dom.attr("onclick", s"copyText(this,'snippet-$id0')"),
        `class` := "absolute top-2 right-2 px-2 py-1 bg-white border border-gray-300 rounded text-xs text-gray-600 hover:bg-gray-50 cursor-pointer",
        "Copy",
      ),
    )

  val mcpUrl = "https://www.skillsjars.com/mcp"

  private final case class McpClientSetup(id: String, label: String, setup: String, note: Option[String] = None)

  private lazy val mcpClients: List[McpClientSetup] = List(
    McpClientSetup("claude-code", "Claude Code", s"claude mcp add --transport http skillsjars $mcpUrl"),
    McpClientSetup("codex", "Codex", s"codex mcp add skillsjars --url $mcpUrl"),
    McpClientSetup("cursor", "Cursor",
      s"""|{
          |  "mcpServers": {
          |    "skillsjars": {
          |      "url": "$mcpUrl"
          |    }
          |  }
          |}""".stripMargin,
      Some("Add to .cursor/mcp.json in your project, or ~/.cursor/mcp.json for all projects."),
    ),
    McpClientSetup("copilot", "GitHub Copilot",
      s"""code --add-mcp '{"name":"skillsjars","type":"http","url":"$mcpUrl"}'""",
      Some("Adds the server to VS Code for GitHub Copilot's agent mode."),
    ),
    McpClientSetup("gemini-cli", "Gemini CLI", s"gemini mcp add --transport http skillsjars $mcpUrl"),
    McpClientSetup("kiro", "Kiro", s"kiro-cli mcp add --name skillsjars --url $mcpUrl"),
    McpClientSetup("devin-desktop", "Devin Desktop",
      s"""|{
          |  "mcpServers": {
          |    "skillsjars": {
          |      "url": "$mcpUrl",
          |      "transport": "http"
          |    }
          |  }
          |}""".stripMargin,
      Some("Formerly Windsurf. Add to .devin/mcp_config.json in your project, or ~/.config/devin/mcp_config.json for all projects."),
    ),
    McpClientSetup("opencode", "OpenCode",
      s"""|{
          |  "$$schema": "https://opencode.ai/config.json",
          |  "mcp": {
          |    "skillsjars": {
          |      "type": "remote",
          |      "url": "$mcpUrl"
          |    }
          |  }
          |}""".stripMargin,
      Some("Add to opencode.json in your project, or ~/.config/opencode/opencode.json for all projects."),
    ),
    McpClientSetup("cline", "Cline",
      s"""|{
          |  "mcpServers": {
          |    "skillsjars": {
          |      "type": "streamableHttp",
          |      "url": "$mcpUrl"
          |    }
          |  }
          |}""".stripMargin,
      Some("In the Cline panel, open MCP Servers, then Configure MCP Servers, and add this to cline_mcp_settings.json."),
    ),
  )

  private val activeTab = "px-3 py-1.5 rounded-md text-sm font-medium bg-white text-gray-900 shadow cursor-pointer"
  private val inactiveTab = "px-3 py-1.5 rounded-md text-sm font-medium text-gray-600 hover:text-gray-900 cursor-pointer"

  private final case class Tab(id: String, label: String, content: Dom)

  /** A tab bar over `tabs`, the first selected; `group` must be unique on the page. */
  private def tabs(group: String, tabs: List[Tab]): Dom =
    div(
      div(
        `class` := "flex flex-wrap gap-1 bg-gray-200 rounded-lg p-1 mb-2",
        Dom.attr("role", "tablist"),
        tabs.zipWithIndex.map: (tab, i) =>
          button(
            `type` := "button",
            id := s"$group-tab-${tab.id}",
            Dom.attr("role", "tab"),
            Dom.attr("aria-selected", (i == 0).toString),
            Dom.attr("data-tab-group", group),
            Dom.attr("data-tab", tab.id),
            Dom.attr("onclick", s"selectTab('$group','${tab.id}')"),
            `class` := (if i == 0 then activeTab else inactiveTab),
            tab.label,
          ),
      ),
      tabs.zipWithIndex.map: (tab, i) =>
        div(
          id := s"$group-panel-${tab.id}",
          Dom.attr("role", "tabpanel"),
          Dom.attr("data-panel-group", group),
          Dom.attr("data-panel", tab.id),
          `class` := (if i == 0 then "" else "hidden"),
          tab.content,
        ),
    )

  /** `selectTab` for [[tabs]] and `copyText` for [[copyBlock]], once per page. */
  private val pageScript: Dom =
    script.inlineJs(
      s"""
         |function selectTab(group, id) {
         |  document.querySelectorAll('[data-tab-group="' + group + '"]').forEach(function(t) {
         |    var on = t.getAttribute('data-tab') === id;
         |    t.className = on ? '$activeTab' : '$inactiveTab';
         |    t.setAttribute('aria-selected', on ? 'true' : 'false');
         |  });
         |  document.querySelectorAll('[data-panel-group="' + group + '"]').forEach(function(p) {
         |    p.classList.toggle('hidden', p.getAttribute('data-panel') !== id);
         |  });
         |}
         |function copyText(btn, id) {
         |  var el = document.getElementById(id);
         |  if (!el) return;
         |  navigator.clipboard.writeText(el.textContent).then(function() {
         |    var orig = btn.textContent;
         |    btn.textContent = 'Copied!';
         |    setTimeout(function() { btn.textContent = orig; }, 2000);
         |  });
         |}
      """.stripMargin
    )

  /** One tab per agent with the command that adds the SkillsJars MCP server. */
  private lazy val mcpSetupTabs: Dom =
    tabs("mcp", mcpClients.map: client =>
      Tab(client.id, client.label, div(
        copyBlock(s"mcp-${client.id}", client.setup),
        client.note.fold(Dom.empty)(note => p(`class` := "text-xs text-gray-500 mt-1", note)),
      ))
    )

  private val codeInline = "bg-gray-100 px-1 rounded"
  private val codeBlock = "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4"
  private val step = "font-semibold text-gray-800 mb-2"

  /** The extraction steps for each build tool, Maven first. */
  private def buildToolTabs(versions: PluginVersions): Dom =
    val browse: Dom = span("Browse Agent Skills on ", a(href := "/", `class` := "text-blue-600 hover:underline", "SkillsJars.com"), " and copy the dependency snippet for your build tool. ")
    tabs("build-tool", List(
      Tab("maven", "Maven", div(
        p(`class` := step, "1. Add the extraction plugin"),
        p(`class` := "text-gray-700 mb-1", "In ", code(`class` := codeInline, "pom.xml"), ":"),
        pre(`class` := codeBlock,
          s"""|<build>
              |    <plugins>
              |        <plugin>
              |            <groupId>com.skillsjars</groupId>
              |            <artifactId>maven-plugin</artifactId>
              |            <version>${versions.maven}</version>
              |            <dependencies>
              |                <!-- Your SkillsJars -->
              |                <dependency>
              |                    <groupId>com.skillsjars</groupId>
              |                    <artifactId>SKILLJAR_ARTIFACT_ID</artifactId>
              |                    <version>SKILLJAR_VERSION</version>
              |                </dependency>
              |            </dependencies>
              |        </plugin>
              |    </plugins>
              |</build>""".stripMargin,
        ),
        p(`class` := step, "2. Add SkillsJar dependencies"),
        p(`class` := "text-gray-700 mb-4", browse,
          "Add them inside the plugin's ", code(`class` := codeInline, "<dependencies>"), ", which keeps them off your project's classpath."),
        p(`class` := step, "3. Extract skills"),
        p(`class` := "text-gray-700 mb-1", "Run the extraction, specifying the directory your AI assistant expects:"),
        pre(`class` := codeBlock, "./mvnw skillsjars:extract -Ddir=.kiro/skills"),
      )),
      Tab("gradle", "Gradle", div(
        p(`class` := step, "1. Add the extraction plugin"),
        p(`class` := "text-gray-700 mb-1", "In ", code(`class` := codeInline, "build.gradle.kts"), ":"),
        pre(`class` := codeBlock,
          s"""|plugins {
              |    id("com.skillsjars.gradle-plugin") version "${versions.gradle}"
              |}""".stripMargin,
        ),
        p(`class` := step, "2. Add SkillsJar dependencies"),
        p(`class` := "text-gray-700 mb-1", browse,
          "Declare them in a configuration such as ", code(`class` := codeInline, "testRuntimeOnly"), ", which keeps them off your main runtime classpath:"),
        pre(`class` := codeBlock,
          """|dependencies {
             |    testRuntimeOnly("com.skillsjars:SKILLJAR_ARTIFACT_ID:SKILLJAR_VERSION")
             |}""".stripMargin,
        ),
        p(`class` := step, "3. Extract skills"),
        p(`class` := "text-gray-700 mb-1", "Run the extraction, specifying the directory your AI assistant expects:"),
        pre(`class` := codeBlock, "./gradlew extractSkillsJars -Pdir=.kiro/skills"),
      )),
      Tab("sbt", "sbt", div(
        p(`class` := step, "1. Add the extraction plugin"),
        p(`class` := "text-gray-700 mb-1", "In ", code(`class` := codeInline, "project/plugins.sbt"), ":"),
        pre(`class` := codeBlock,
          s"""addSbtPlugin("com.skillsjars" % "skillsjars-sbt-plugin" % "${versions.sbt}")""",
        ),
        p(`class` := step, "2. Add SkillsJar dependencies"),
        p(`class` := "text-gray-700 mb-1", browse,
          "In ", code(`class` := codeInline, "build.sbt"), ", set the directory to extract skills into, and declare SkillsJars in the plugin's ",
          code(`class` := codeInline, "Skills"), " configuration (add ", code(`class` := codeInline, "% Skills"),
          " to the snippet), which keeps them off your compile and runtime classpath:"),
        pre(`class` := codeBlock,
          """|skillsJarsOutputDir := Some(file(".kiro/skills"))
             |
             |libraryDependencies += "com.skillsjars" % "SKILLJAR_ARTIFACT_ID" % "SKILLJAR_VERSION" % Skills""".stripMargin,
        ),
        p(`class` := step, "3. Extract skills"),
        p(`class` := "text-gray-700 mb-1", "Run the extraction. It uses ", code(`class` := codeInline, "skillsJarsOutputDir"), ", or pass a directory:"),
        pre(`class` := codeBlock, "sbt extractSkillsJars\nsbt \"extractSkillsJars .kiro/skills\""),
      )),
      Tab("sjm", "No build (sjm)", div(
        p(`class` := "text-gray-700 mb-4",
          a(href := "https://github.com/skillsjars/skillsjars-cli", `class` := "text-blue-600 hover:underline", "sjm"),
          " (SkillsJars Manager) installs SkillsJars into any project, without a JVM or a build tool.",
        ),
        p(`class` := step, "1. Install sjm"),
        pre(`class` := codeBlock,
          """|# Linux (x64, arm64; glibc) and macOS (Apple silicon)
             |curl -fsSL https://raw.githubusercontent.com/skillsjars/skillsjars-cli/main/install.sh | sh
             |
             |# Windows (x64), in PowerShell
             |irm https://raw.githubusercontent.com/skillsjars/skillsjars-cli/main/install.ps1 | iex""".stripMargin,
        ),
        p(`class` := step, "2. Add SkillsJars"),
        p(`class` := "text-gray-700 mb-1",
          "Find skills on ", a(href := "/", `class` := "text-blue-600 hover:underline", "SkillsJars.com"),
          " or with ", code(`class` := codeInline, "sjm search"), ", and install each one by its artifactId, giving the directory your AI assistant expects:",
        ),
        pre(`class` := codeBlock,
          """|sjm search pdf
             |sjm --dir .kiro/skills install anthropics__skills__pdf""".stripMargin,
        ),
        p(`class` := "text-gray-700 mb-4",
          code(`class` := codeInline, "sjm install"), " writes ", code(`class` := codeInline, "skillsjars.json"),
          ", which pins the versions and the skills directory. Commit it, and add the skills directory to ",
          code(`class` := codeInline, ".gitignore"), ". Without ", code(`class` := codeInline, "--dir"),
          ", skills go to ", code(`class` := codeInline, ".agents/skills"), ".",
        ),
        p(`class` := step, "3. Extract skills"),
        p(`class` := "text-gray-700 mb-1", "After a checkout, extract everything in ", code(`class` := codeInline, "skillsjars.json"), ":"),
        pre(`class` := codeBlock, "sjm install"),
      )),
    ))

  private def searchForm(maybeQuery: Option[String], buildTool: BuildTool): Dom =
    form(
      `class` := "mb-6",
      action := "/",
      method := "get",
      input(`type` := "hidden", name := "bt", value := buildTool.param),
      div(
        `class` := "flex gap-2",
        div(
          `class` := "flex-1 relative",
          input(
            `type` := "text",
            name := "q",
            placeholder := "Search skills by name or description...",
            `class` := s"w-full px-4 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500${maybeQuery.fold("")(_ => " pr-8")}",
            maybeQuery.map(q => value := q).getOrElse(Dom.empty),
          ),
          maybeQuery.fold(Dom.empty): _ =>
            a(
              href := s"/?bt=${buildTool.param}",
              `class` := "absolute right-2 top-1/2 -translate-y-1/2 text-gray-400 hover:text-gray-600 cursor-pointer",
              "✕",
            ),
        ),
        button(
          `type` := "submit",
          `class` := "px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 cursor-pointer",
          "Search",
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
        Dom.attr("onsubmit", "var b=this.querySelector('button[type=submit]');b.disabled=true;b.textContent='Deploying...'"),
        div(
          `class` := "flex flex-col sm:flex-row gap-2 sm:items-end",
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
        `class` := "flex flex-col sm:flex-row sm:justify-between sm:items-start gap-2",
        div(
          div(
            `class` := "flex flex-wrap items-center gap-2",
            h3(`class` := "font-semibold text-gray-900", sj.name),
            if sj.securityScanned then
              span(`class` := "inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800",
                // shield checkmark svg
                Dom.raw("""<svg class="w-3 h-3" fill="currentColor" viewBox="0 0 20 20"><path fill-rule="evenodd" d="M10 1a1 1 0 01.78.375l5 6.5A1 1 0 0116 8.5V14a4 4 0 01-4 4H8a4 4 0 01-4-4V8.5a1 1 0 01.22-.625l5-6.5A1 1 0 0110 1zm-1.293 9.707a1 1 0 010-1.414L10 8l1.293 1.293a1 1 0 01-1.414 1.414L10 10.414l-.293.293a1 1 0 01-1.414 0z" clip-rule="evenodd"/></svg>"""),
                "Security Scanned",
              )
            else Dom.empty,
          ),
          p(`class` := "text-sm text-gray-600 mt-1", sj.description),
          if sj.allowedTools.nonEmpty then
            div(
              `class` := "mt-2 flex flex-wrap gap-1",
              span(`class` := "text-xs text-gray-500", "Allowed tools:"),
              sj.allowedTools.values.toSeq.flatMap(_.split("\\s+")).filter(_.nonEmpty).distinct.map: tool =>
                span(`class` := "inline-flex items-center px-2 py-0.5 rounded text-xs font-mono bg-purple-100 text-purple-800", tool),
            )
          else Dom.empty,
        ),
        div(
          `class` := "sm:ml-4 self-start",
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
    script.inlineJs(
      s"""
         |var buildTool = '${buildTool.param}';
         |function updateSnippet(sel, gid, aid) {
         |  var v = sel.value;
         |  var el = document.getElementById('snippet-' + aid);
         |  if (!el) return;
         |  if (buildTool === 'maven') {
         |    el.textContent = '<dependency>\\n    <groupId>' + gid + '</groupId>\\n    <artifactId>' + aid + '</artifactId>\\n    <version>' + v + '</version>\\n</dependency>';
         |  } else if (buildTool === 'gradle') {
         |    el.textContent = "runtimeOnly(\\\"" + gid + ':' + aid + ':' + v + "\\\")";
         |  } else {
         |    el.textContent = '"' + gid + '" % "' + aid + '" % "' + v + '"';
         |  }
         |}
         |function copySnippet(btn, aid) {
         |  var el = document.getElementById('snippet-' + aid);
         |  if (!el) return;
         |  navigator.clipboard.writeText(el.textContent).then(function() {
         |    var orig = btn.textContent;
         |    btn.textContent = 'Copied!';
         |    setTimeout(function() { btn.textContent = orig; }, 2000);
         |  });
         |}
      """.stripMargin
    )

  def docs(tailwind: URL, versions: PluginVersions): Dom =
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
          div(
            `class` := "bg-amber-50 border border-amber-200 rounded-lg p-4",
            p(`class` := "font-semibold text-amber-800", "Security Warning"),
            p(`class` := "text-sm text-amber-700 mt-1",
              """Agent Skills can be dangerous / do malicous things and should be vetted before use.
                |While SkillsJars does a basic security scan of the Skills before they are published, it is not a substitute for proper security reviews.
                |""".stripMargin
            ),
          )
        ),

        // For AI Agents
        div(
          `class` := "bg-green-50 border border-green-200 rounded-lg p-4 mt-6",
          p(`class` := "font-semibold text-green-800 mb-1", "For AI agents"),
          p(`class` := "text-sm text-green-700 mb-2",
            "If you are an AI coding agent setting SkillsJars up in a project, fetch the machine-readable setup guide. It detects the build tool, adds the extraction plugin, helps you pick skills, and sets up ",
            code(`class` := "bg-green-100 px-1 rounded", "AGENTS.md"),
            ":",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-2",
            "curl -H \"Accept: text/markdown\" https://skillsjars.com/setup",
          ),
          p(`class` := "text-sm text-green-700",
            "The skill catalog is also available as markdown — ",
            code(`class` := "bg-green-100 px-1 rounded", "curl -H \"Accept: text/markdown\" https://skillsjars.com/"),
            " (add ", code(`class` := "bg-green-100 px-1 rounded", "?q=keyword"), " to search). Or just open ",
            a(href := "/setup", `class` := "text-blue-600 hover:underline", "/setup"),
            " to read the guide.",
          ),
        ),

        // AI Code Assistants
        div(
          `class` := "bg-white rounded-lg shadow p-6 mt-6",
          h3(`class` := "text-xl font-bold text-gray-900 mb-3", "AI Code Assistants"),
          p(`class` := "text-gray-700 mb-4",
            "Most AI code assistants expect Agent Skills as files on the filesystem. The SkillsJars build plugins extract skills from your project dependencies into a directory your assistant can read. For a project without a Maven, Gradle, or sbt build, the sjm CLI does the same.",
          ),

          buildToolTabs(versions),
          p(`class` := "text-sm text-gray-500 mb-4",
            "Replace ", code(`class` := codeInline, ".kiro/skills"), " with the path your AI assistant uses for skills.",
          ),

          // AGENTS.md
          div(
            `class` := "bg-blue-50 border border-blue-200 rounded-lg p-4 mt-4",
            p(`class` := "font-semibold text-blue-800 mb-1", "Tip: AGENTS.md"),
            p(`class` := "text-sm text-blue-700",
              "Your project's ", code(`class` := "bg-blue-100 px-1 rounded", "AGENTS.md"),
              " can instruct AI agents to run the extraction command before working with the project. This way, skills are always available without manual setup. The ",
              a(href := "/setup", `class` := "text-blue-600 hover:underline", "setup guide"),
              " includes a ready-to-paste ", code(`class` := "bg-blue-100 px-1 rounded", "AGENTS.md"), " snippet.",
            ),
          ),
        ),

        // MCP
        div(
          `class` := "bg-white rounded-lg shadow p-6 mt-6",
          h3(`class` := "text-xl font-bold text-gray-900 mb-3", "Skills over MCP"),
          p(`class` := "text-gray-700 mb-4",
            "SkillsJars.com runs an MCP server that serves the latest version of every SkillsJar listed on the site, with no build changes or extraction step. Use it with agents that support Skills over MCP. The server is stateless (Streamable HTTP) and needs no authentication:",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4", mcpUrl),

          p(`class` := "font-semibold text-gray-800 mb-2", "Add the server to your agent"),
          div(`class` := "mb-4", mcpSetupTabs),

          p(`class` := "font-semibold text-gray-800 mb-2", "How skills are served"),
          ul(
            `class` := "list-disc pl-6 text-gray-700 mb-4 space-y-1",
            li(
              "Agents that support the MCP Skills extension (", code(`class` := "bg-gray-100 px-1 rounded", "io.modelcontextprotocol/skills"),
              ") discover skills with ", code(`class` := "bg-gray-100 px-1 rounded", "skills/list"),
              " and ", code(`class` := "bg-gray-100 px-1 rounded", "skills/get"),
              ", and browse a skill's files with ", code(`class` := "bg-gray-100 px-1 rounded", "resources/directory/read"), ".",
            ),
            li(
              "Other MCP clients find each skill's ", code(`class` := "bg-gray-100 px-1 rounded", "SKILL.md"),
              " in ", code(`class` := "bg-gray-100 px-1 rounded", "resources/list"),
              " and read it, and the files beside it, with ", code(`class` := "bg-gray-100 px-1 rounded", "resources/read"), ".",
            ),
            li(
              "Each skill is addressed by its directory in the JAR: ",
              code(`class` := "bg-gray-100 px-1 rounded", "skill://<org>/<repo>/<skill>/SKILL.md"), ".",
            ),
          ),
          p(`class` := "text-sm text-gray-500",
            "Only the latest version of each SkillsJar is served. To pin versions or choose a subset of skills, declare SkillsJars in your build instead (see AI Code Assistants above).",
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
              " project provides a SkillsTool that integrates Agent Skills directly with Spring AI agents. SkillsJars work out of the box — skills are read directly from the classpath with no extraction step needed.",
            ),

            p(`class` := "font-semibold text-gray-800 mt-4 mb-2", "1. Add dependencies"),
            p(`class` := "text-gray-700 mb-1", "Add the Spring AI Agent Utils library and any SkillsJar dependencies to your project:"),
            pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
              """|<dependency>
                 |    <groupId>org.springaicommunity</groupId>
                 |    <artifactId>spring-ai-agent-utils</artifactId>
                 |    <version>0.5.0</version>
                 |</dependency>
                 |
                 |<!-- SkillsJar dependencies, for example -->
                 |<dependency>
                 |    <groupId>com.skillsjars</groupId>
                 |    <artifactId>browser-use__browser-use__browser-use</artifactId>
                 |    <version>2026_02_23-1d154e1</version>
                 |</dependency>""".stripMargin,
            ),

            p(`class` := "font-semibold text-gray-800 mb-2", "2. Configure the skills path"),
            p(`class` := "text-gray-700 mb-1",
              "In ", code(`class` := "bg-gray-100 px-1 rounded", "application.properties"), ", point to the classpath location where SkillsJars store their skills:",
            ),
            pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
              "agent.skills.paths=classpath:/META-INF/skills",
            ),

            p(`class` := "font-semibold text-gray-800 mb-2", "3. Wire up the SkillsTool"),
            p(`class` := "text-gray-700 mb-1", "Use the SkillsTool builder to load skills and add them to your ChatClient:"),
            pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
              """|@Value("${agent.skills.paths}") List<Resource> skillPaths;
                 |
                 |ChatClient chatClient = chatClientBuilder
                 |        .defaultToolCallbacks(
                 |                SkillsTool.builder().addSkillsResources(skillPaths).build()
                 |        )
                 |        .build();""".stripMargin,
            ),

            div(
              `class` := "bg-blue-50 border border-blue-200 rounded-lg p-4 mt-2",
              p(`class` := "font-semibold text-blue-800 mb-1", "Example project"),
              p(`class` := "text-sm text-blue-700",
                "See the ",
                a(href := "https://github.com/skillsjars/skillsjars-example-spring-ai", `class` := "text-blue-600 hover:underline", "skillsjars-example-spring-ai"),
                " repository for a complete working example.",
              ),
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

        // Creating SkillsJars
        div(
          `class` := "bg-white rounded-lg shadow p-6 mt-6",
          h3(`class` := "text-xl font-bold text-gray-900 mb-3", "Creating SkillsJars"),
          p(`class` := "text-gray-700 mb-4",
            "You can package your own Agent Skills as SkillsJars and publish them to Maven Central via ",
            a(href := "/", `class` := "text-blue-600 hover:underline", "SkillsJars.com"),
            ".",
          ),

          p(`class` := "font-semibold text-gray-800 mb-2", "1. Create a skills directory"),
          p(`class` := "text-gray-700 mb-1",
            "Add a ", code(`class` := "bg-gray-100 px-1 rounded", "skills"),
            " directory to your project root. Each subdirectory is a skill and must contain a ",
            code(`class` := "bg-gray-100 px-1 rounded", "SKILL.md"),
            " marker file following the ",
            a(href := "https://agentskills.io/specification", `class` := "text-blue-600 hover:underline", "Agent Skills specification"),
            ".",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            """|skills/
               |├── my-skill/
               |│   ├── SKILL.md
               |│   └── helpers.py
               |└── another-skill/
               |    └── SKILL.md""".stripMargin,
          ),

          p(`class` := "font-semibold text-gray-800 mb-2", "2. SKILL.md frontmatter"),
          p(`class` := "text-gray-700 mb-1",
            "Each ", code(`class` := "bg-gray-100 px-1 rounded", "SKILL.md"),
            " must include YAML frontmatter with ", code(`class` := "bg-gray-100 px-1 rounded", "name"),
            " and ", code(`class` := "bg-gray-100 px-1 rounded", "description"),
            ". Optionally include ", code(`class` := "bg-gray-100 px-1 rounded", "allowed-tools"),
            " (a space-delimited list of pre-approved tools) and ", code(`class` := "bg-gray-100 px-1 rounded", "license"),
            ".",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            """|---
               |name: my-skill
               |description: A useful skill
               |allowed-tools: Bash Read Edit
               |license: MIT
               |---
               |# My Skill
               |...""".stripMargin,
          ),
          p(`class` := "text-sm text-gray-500 mb-4",
            "When published via SkillsJars.com, ", code(`class` := "bg-gray-100 px-1 rounded", "allowed-tools"),
            " is automatically stored in the POM as a property named ", code(`class` := "bg-gray-100 px-1 rounded", "skillsjars.skill.<name>.allowed-tools"),
            " so consumers can read it without extracting the JAR.",
          ),
          p(`class` := "text-gray-700 mb-1",
            "If you are packaging SkillsJars yourself with the Maven plugin, you must add matching properties to your ", code(`class` := "bg-gray-100 px-1 rounded", "pom.xml"),
            ". The plugin validates that the property value matches the SKILL.md and fails the build on any mismatch.",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            """|<properties>
               |    <skillsjars.skill.my-skill.allowed-tools>Bash Read Edit</skillsjars.skill.my-skill.allowed-tools>
               |</properties>""".stripMargin,
          ),

          p(`class` := "font-semibold text-gray-800 mb-2", "3. Add the Maven plugin"),
          p(`class` := "text-gray-700 mb-1",
            "Add the SkillsJars Maven plugin with the ", code(`class` := "bg-gray-100 px-1 rounded", "package"), " goal:",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            s"""|<build>
               |    <plugins>
               |        <plugin>
               |            <groupId>com.skillsjars</groupId>
               |            <artifactId>maven-plugin</artifactId>
               |            <version>${versions.maven}</version>
               |            <executions>
               |                <execution>
               |                    <goals>
               |                        <goal>package</goal>
               |                    </goals>
               |                </execution>
               |            </executions>
               |        </plugin>
               |    </plugins>
               |</build>""".stripMargin,
          ),

          p(`class` := "font-semibold text-gray-800 mb-2", "4. Build the JAR"),
          p(`class` := "text-gray-700 mb-1",
            "The plugin runs during ", code(`class` := "bg-gray-100 px-1 rounded", "mvn package"),
            " and places skills into ", code(`class` := "bg-gray-100 px-1 rounded", "META-INF/skills/"),
            " inside your JAR. If your project has GitHub SCM configured in the POM, it uses the org/repo from the URL. Otherwise it uses the project's groupId.",
          ),
          pre(`class` := "bg-gray-100 rounded p-3 text-sm font-mono overflow-x-auto mb-4",
            "mvn package",
          ),

          p(`class` := "font-semibold text-gray-800 mb-2", "5. Publish to Maven Central"),
          p(`class` := "text-gray-700 mb-4",
            "Push your skills to a public GitHub repository and use the ",
            a(href := "/", `class` := "text-blue-600 hover:underline", "Publish a SkillsJar"),
            " form on the homepage to deploy them to Maven Central.",
          ),

          div(
            `class` := "bg-blue-50 border border-blue-200 rounded-lg p-4 mt-2",
            p(`class` := "font-semibold text-blue-800 mb-1", "Tip: Custom skills directory"),
            p(`class` := "text-sm text-blue-700",
              "By default the plugin looks for skills in ", code(`class` := "bg-blue-100 px-1 rounded", "skills/"),
              " at the project root. You can customize this with the ", code(`class` := "bg-blue-100 px-1 rounded", "skillsDir"),
              " configuration parameter.",
            ),
          ),
        ),
      ),
      tailwind,
      currentPath = "/docs",
    )

  def deployNotFound(org: String, repo: String, tailwind: URL): Dom =
    page(
      "Deploy Not Found",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "bg-amber-50 border border-amber-200 rounded-lg p-4 mt-4",
          p(`class` := "font-semibold text-amber-800", "Deploy job not found"),
          p(`class` := "text-sm text-amber-700 mt-1", s"No deploy found for $org/$repo at this version."),
        ),
        form(
          `class` := "mt-6",
          action := "/deploy",
          method := "post",
          Dom.attr("onsubmit", "var b=this.querySelector('button[type=submit]');b.disabled=true;b.textContent='Deploying...'"),
          input(`type` := "hidden", name := "org", value := org),
          input(`type` := "hidden", name := "repo", value := repo),
          button(
            `type` := "submit",
            `class` := "px-6 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 cursor-pointer",
            s"Deploy $org/$repo",
          ),
        ),
      ),
      tailwind,
    )

  def deployInProgress(org: String, repo: String, tailwind: URL): Dom =
    page(
      "Deploying...",
      div(
        `class` := "flex flex-col items-center justify-center py-16",
        style("""
          @keyframes spin { to { transform: rotate(360deg); } }
        """),
        div(
          Dom.attr("style", "width:48px;height:48px;border:4px solid #e5e7eb;border-top-color:#2563eb;border-radius:50%;animation:spin 1s linear infinite"),
        ),
        p(`class` := "mt-6 text-lg font-semibold text-gray-700", s"Deploying $org/$repo..."),
        p(`class` := "mt-2 text-sm text-gray-500", "This page will refresh automatically."),
        meta(Dom.attr("http-equiv", "refresh"), Dom.attr("content", "3")),
      ),
      tailwind,
    )

  def deployDone(results: Map[SkillName, SkillResult], tailwind: URL): Dom =
    page(
      "Deploy Results",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "bg-green-50 border border-green-200 rounded-lg p-4 mt-4",
          p(`class` := "font-semibold text-green-800", "Deployed successfully. It will take ~1hr for the artifact to be available on Maven Central."),
        ),
        div(
          `class` := "space-y-3 mt-4",
          results.toSeq.map((name, result) => skillResultCard(name, result)),
        ),
      ),
      tailwind,
    )

  private def skillResultCard(name: SkillName, result: SkillResult): Dom =
    result match
      case SkillResult.Success(gav) =>
        div(
          `class` := "bg-green-50 border border-green-200 rounded-lg p-4",
          p(`class` := "font-semibold text-green-800", s"$name"),
          p(`class` := "text-sm font-mono text-green-700 mt-1", s"${gav.groupId}:${gav.artifactId}:${gav.version}"),
        )
      case SkillResult.Skipped(error) =>
        error match
          case SkillError.SecurityBlocked(findings) =>
            div(
              `class` := "bg-red-50 border border-red-200 rounded-lg p-4",
              p(`class` := "font-semibold text-red-800", s"Security blocked: $name"),
              div(
                `class` := "mt-2 space-y-2",
                findings.toSeq.collect { case (fileName, fileFindings) if fileFindings.nonEmpty => (fileName, fileFindings) }.map: (fileName, fileFindings) =>
                  div(
                    p(`class` := "text-red-400 text-xs font-mono", fileName),
                    div(
                      `class` := "space-y-1 ml-2",
                      fileFindings.map: finding =>
                        val severityCls = finding.severity match
                          case Severity.Critical => "bg-red-600 text-white"
                          case Severity.High     => "bg-orange-500 text-white"
                          case Severity.Medium   => "bg-yellow-400 text-gray-900"
                          case Severity.Low      => "bg-gray-300 text-gray-700"
                        div(
                          `class` := "flex items-start gap-2 text-sm",
                          span(`class` := s"px-1.5 py-0.5 rounded text-xs font-medium $severityCls", finding.severity.toString),
                          span(`class` := "text-red-700", finding.description),
                        ),
                    ),
                  ),
              ),
            )
          case _ =>
            val message = skillErrorMessage(error)
            div(
              `class` := "bg-amber-50 border border-amber-200 rounded-lg p-4",
              p(`class` := "font-semibold text-amber-800", s"Skipped: $name"),
              p(`class` := "text-sm text-amber-700 mt-1", message),
            )

  private def skillErrorMessage(error: SkillError): String =
    error match
      case SkillError.InvalidSkillMd(reason) => s"Invalid SKILL.md: $reason"
      case SkillError.InvalidComponent(component, reason) => s"Invalid component '$component': $reason"
      case SkillError.DuplicateVersion(gav) => s"Already published: ${gav.groupId}:${gav.artifactId}:${gav.version}"
      case SkillError.NoLicense => "No license found. Add a LICENSE file or specify license in SKILL.md frontmatter."
      case SkillError.ScanInferenceError(_) => "Security scan is temporarily unavailable. Please try again later."
      case SkillError.ScanParseError(_) => "Security scan returned an unexpected response. Please try again later."
      case SkillError.SecurityBlocked(findings) => s"Security blocked: ${findings.values.map(_.size).sum} finding(s)"

  def deployError(error: DeployError, tailwind: URL): Dom =
    val message = error.kind match
      case RepoErrorKind.NotFound => s"Repository not found: ${error.org}/${error.repo}"
      case RepoErrorKind.NoSkillsDirectory => s"No skills directory in ${error.org}/${error.repo}"
      case RepoErrorKind.OverlappingSkills(path1, path2) =>
        val p1 = if path1.isEmpty then "/" else path1.mkString("/")
        val p2 = if path2.isEmpty then "/" else path2.mkString("/")
        s"Overlapping skills in ${error.org}/${error.repo}: '$p1' and '$p2'. A skill directory cannot be an ancestor of another."

    page(
      "Deploy Error",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "bg-red-50 border border-red-200 rounded-lg p-4 mt-4",
          p(`class` := "font-semibold text-red-800", "Deploy failed"),
          p(`class` := "text-sm text-red-700 mt-1", message),
        ),
      ),
      tailwind,
    )

  def deployJobError(error: DeployJobError, tailwind: URL): Dom =
    error match
      case DeployJobError.NoPublishableSkills(skipped) =>
        page(
          "Deploy Error",
          div(
            a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
            div(
              `class` := "bg-red-50 border border-red-200 rounded-lg p-4 mt-4",
              p(`class` := "font-semibold text-red-800", "No skills could be published"),
            ),
            div(
              `class` := "space-y-3 mt-4",
              skipped.toSeq.map((name, error) => skillResultCard(name, SkillResult.Skipped(error))),
            ),
          ),
          tailwind,
        )
      case DeployJobError.PublishFailed(reason) =>
        page(
          "Deploy Error",
          div(
            a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
            div(
              `class` := "bg-red-50 border border-red-200 rounded-lg p-4 mt-4",
              p(`class` := "font-semibold text-red-800", "Deploy failed"),
              p(`class` := "text-sm text-red-700 mt-1", s"Publish failed: $reason"),
            ),
          ),
          tailwind,
        )

  def requestError(reason: String, tailwind: URL): Dom =
    page(
      "Invalid Request",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "bg-red-50 border border-red-200 rounded-lg p-4 mt-4",
          p(`class` := "font-semibold text-red-800", "Invalid request"),
          p(`class` := "text-sm text-red-700 mt-1", reason),
        ),
      ),
      tailwind,
    )

  def formError(reason: String, tailwind: URL): Dom =
    page(
      "Deploy Error",
      div(
        a(href := "/", `class` := "text-blue-600 hover:underline mb-4 inline-block", "Back to list"),
        div(
          `class` := "bg-red-50 border border-red-200 rounded-lg p-4 mt-4",
          p(`class` := "font-semibold text-red-800", "Invalid request"),
          p(`class` := "text-sm text-red-700 mt-1", reason),
        ),
      ),
      tailwind,
    )
