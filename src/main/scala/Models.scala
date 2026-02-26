import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.schema.*
import zio.schema.annotation.*

object Models:

  opaque type Org = String
  object Org:
    def apply(s: String): Org = s

  opaque type Repo = String
  object Repo:
    def apply(s: String): Repo = s

  opaque type SkillName = String
  object SkillName:
    def apply(s: String): SkillName = s

  case class License(name: String, url: String)

  val spdxLicenses: Map[String, License] = Map(
    "MIT" -> License("MIT License", "https://opensource.org/licenses/MIT"),
    "Apache-2.0" -> License("Apache License 2.0", "https://www.apache.org/licenses/LICENSE-2.0"),
    "GPL-2.0" -> License("GNU General Public License v2.0", "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"),
    "GPL-3.0" -> License("GNU General Public License v3.0", "https://www.gnu.org/licenses/gpl-3.0.html"),
    "BSD-2-Clause" -> License("BSD 2-Clause License", "https://opensource.org/licenses/BSD-2-Clause"),
    "BSD-3-Clause" -> License("BSD 3-Clause License", "https://opensource.org/licenses/BSD-3-Clause"),
    "MPL-2.0" -> License("Mozilla Public License 2.0", "https://www.mozilla.org/en-US/MPL/2.0/"),
    "ISC" -> License("ISC License", "https://opensource.org/licenses/ISC"),
    "LGPL-2.1" -> License("GNU Lesser General Public License v2.1", "https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html"),
    "LGPL-3.0" -> License("GNU Lesser General Public License v3.0", "https://www.gnu.org/licenses/lgpl-3.0.html"),
    "AGPL-3.0" -> License("GNU Affero General Public License v3.0", "https://www.gnu.org/licenses/agpl-3.0.html"),
    "Unlicense" -> License("The Unlicense", "https://unlicense.org/"),
  )

  def licenseFromSpdxId(id: String): Option[License] = spdxLicenses.get(id)

  case class SkillMeta(name: SkillName, description: String, licenses: List[License], rawLicense: Option[String] = None)

  case class ValidatedSkill(
    location: SkillLocation,
    skillDir: java.io.File,
    meta: SkillMeta,
    gav: MavenCentral.GroupArtifactVersion,
    licenses: NonEmptyChunk[License],
  )

  case class SkillLocation(org: Org, repo: Repo, path: List[String]):
    def skillName: SkillName = if path.isEmpty then repo else path.last

  case class SkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[MavenCentral.Version], name: String, description: String)

  enum Severity derives Schema:
    case Critical, High, Medium, Low

  enum ScanCategory derives Schema:
    @caseName("PROMPT_INJECTION") case PromptInjection
    @caseName("SEMANTIC_MISMATCH") case SemanticMismatch
    @caseName("INTENT_CLASSIFICATION") case IntentClassification
    @caseName("SUBTLE_INJECTION") case SubtleInjection
    @caseName("CROSS_SKILL_POISONING") case CrossSkillPoisoning

  case class SecurityFinding(category: ScanCategory, severity: Severity, description: String) derives Schema

  case class DeployError(org: Org, repo: Repo, kind: RepoErrorKind)

  // these are errors that can happen before the DeployJob is created
  enum RepoErrorKind:
    case NotFound
    case NoSkillsDirectory
    case OverlappingSkills(path1: List[String], path2: List[String])

  // these are errors that can happen after the DeployJob is created
  enum DeployJobError:
    case NoPublishableSkills(results: Map[SkillName, SkillError])
    case PublishFailed(reason: String)

  enum SkillResult:
    case Success(gav: MavenCentral.GroupArtifactVersion)
    case Skipped(reason: SkillError)

  enum SkillError:
    case InvalidSkillMd(reason: String)
    case InvalidComponent(component: String, reason: String)
    case DuplicateVersion(gav: MavenCentral.GroupArtifactVersion)
    case NoLicense
    case ScanInferenceError(reason: String)
    case ScanParseError(reason: String)
    case SecurityBlocked(findings: Map[String, List[SecurityFinding]])

  enum DeployJobStatus:
    case Running
    case Done(results: Map[SkillName, SkillResult])
    case Failed(error: DeployJobError)


  enum BuildTool(val label: String, val param: String):
    case Maven extends BuildTool("Maven", "maven")
    case Gradle extends BuildTool("Gradle", "gradle")
    case Sbt extends BuildTool("sbt", "sbt")

  object BuildTool:
    def fromParam(s: String): BuildTool = s.toLowerCase match
      case "gradle" => BuildTool.Gradle
      case "sbt" => BuildTool.Sbt
      case "maven" => BuildTool.Maven
      case _ => throw new IllegalArgumentException(s"Invalid build tool: $s")

  given CanEqual[Org, Org] = CanEqual.derived
  given CanEqual[Repo, Repo] = CanEqual.derived
  given CanEqual[SkillName, SkillName] = CanEqual.derived
  given CanEqual[License, License] = CanEqual.derived
  given CanEqual[DeployError, DeployError] = CanEqual.derived
  given CanEqual[RepoErrorKind, RepoErrorKind] = CanEqual.derived
  given CanEqual[BuildTool, BuildTool] = CanEqual.derived
  given CanEqual[DeployJobStatus, DeployJobStatus] = CanEqual.derived
  given CanEqual[Severity, Severity] = CanEqual.derived
  given CanEqual[ScanCategory, ScanCategory] = CanEqual.derived
  given CanEqual[SkillResult, SkillResult] = CanEqual.derived
  given CanEqual[SkillError, SkillError] = CanEqual.derived

  private def sanitize(s: String): String =
    s.toLowerCase.replaceAll("[^a-z0-9_-]", "").replaceAll("^[-_]+|[-_]+$", "")

  val groupId: MavenCentral.GroupId = MavenCentral.GroupId("com.skillsjars")

  def artifactIdFor(org: Org, repo: Repo, path: List[String] = List.empty): IO[SkillError.InvalidComponent, MavenCentral.ArtifactId] =
    val components = List(org: String, repo: String) ++ path
    components.find(_.contains("__")) match
      case Some(c) => ZIO.fail(SkillError.InvalidComponent(c, "contains '__'"))
      case None => ZIO.succeed(MavenCentral.ArtifactId(components.map(sanitize).mkString("__")))
