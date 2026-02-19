import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.http.URL
import zio.http.template2.*

object UI:

  def page(pageTitle: String, pageContent: Dom, tailwind: URL): Dom =
    html(
      head(
        title(pageTitle),
        meta(charset := "UTF-8"),
        meta(name := "viewport", Dom.attr("content", "width=device-width, initial-scale=1.0")),
        script(src := tailwind),
      ),
      body(
        `class` := "bg-gray-50 min-h-screen",
        div(
          `class` := "max-w-4xl mx-auto px-4 py-8",
          header(
            `class` := "mb-8",
            h1(
              `class` := "text-3xl font-bold text-gray-900",
              a(href := "/", "SkillsJars"),
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

  def index(skillsJars: Seq[SkillsJar], maybeQuery: Option[String], tailwind: URL, maybeError: Option[SkillsJarService.ServiceError] = None): Dom =
    page(
      "SkillsJars",
      div(
        maybeError.map(errorCard).getOrElse(Dom.empty),
        searchForm(maybeQuery),
        deployForm,
        skillsJarList(skillsJars),
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

  private def searchForm(maybeQuery: Option[String]): Dom =
    form(
      `class` := "mb-6",
      action := "/",
      method := "get",
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

  private def skillsJarList(skillsJars: Seq[SkillsJar]): Dom =
    if skillsJars.isEmpty then
      p(`class` := "text-gray-500 text-center py-8", "No SkillsJars found.")
    else
      div(
        `class` := "space-y-3",
        skillsJars.map(skillsJarCard),
      )

  private def skillsJarCard(sj: SkillsJar): Dom =
    div(
      `class` := "bg-white rounded-lg shadow p-4",
      div(
        `class` := "flex justify-between items-start",
        div(
          h3(`class` := "font-semibold text-gray-900", sj.name),
          p(`class` := "text-sm text-gray-600 mt-1", sj.description),
          p(
            `class` := "text-xs text-gray-400 mt-2 font-mono",
            s"${sj.groupId}:${sj.artifactId}",
          ),
        ),
        div(
          `class` := "ml-4",
          versionSelect(sj),
        ),
      ),
    )

  private def versionSelect(sj: SkillsJar): Dom =
    if sj.versions.isEmpty then
      span(`class` := "text-gray-400 text-sm", "No versions")
    else
      select(
        `class` := "px-2 py-1 border border-gray-300 rounded text-sm",
        sj.versions.map: v =>
          option(value := v.toString, v.toString),
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
          p(`class` := "font-semibold text-green-800", "Deployed successfully"),
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
