import com.jamesward.zio_mavencentral.MavenCentral
import zio.*

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

  case class SkillLocation(org: Org, repo: Repo, subPath: List[String], skillName: SkillName)

  case class SkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[MavenCentral.Version], name: String, description: String)

  case class SkippedSkill(skillName: SkillName, reason: String)

  case class DeployOutcome(published: Set[MavenCentral.GroupArtifactVersion], skipped: List[SkippedSkill], duplicates: Set[MavenCentral.GroupArtifactVersion] = Set.empty)

  enum DeployError:
    case RepoNotFound(org: Org, repo: Repo)
    case NoSkillsDirectory(org: Org, repo: Repo)
    case InvalidSkillMd(path: String, reason: String)
    case InvalidComponent(component: String, reason: String)
    case DuplicateVersion(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version)
    case NoLicense(org: Org, repo: Repo, skillName: SkillName)
    case NoPublishableSkills(org: Org, repo: Repo, skipped: List[SkippedSkill])
    case PublishFailed(reason: String)

  given CanEqual[Org, Org] = CanEqual.derived
  given CanEqual[Repo, Repo] = CanEqual.derived
  given CanEqual[SkillName, SkillName] = CanEqual.derived
  given CanEqual[License, License] = CanEqual.derived
  given CanEqual[DeployError, DeployError] = CanEqual.derived

  private def sanitize(s: String): String =
    s.toLowerCase.replaceAll("[^a-z0-9_-]", "").replaceAll("^[-_]+|[-_]+$", "")

  val groupId: MavenCentral.GroupId = MavenCentral.GroupId("com.skillsjars")

  def artifactIdFor(org: Org, repo: Repo, skillName: SkillName, subPath: List[String] = List.empty): IO[DeployError, MavenCentral.ArtifactId] =
    val components = List(org: String, repo: String) ++ subPath ++ List(skillName: String)
    components.find(_.contains("__")) match
      case Some(c) => ZIO.fail(DeployError.InvalidComponent(c, "contains '__'"))
      case None => ZIO.succeed(MavenCentral.ArtifactId(components.map(sanitize).mkString("__")))
