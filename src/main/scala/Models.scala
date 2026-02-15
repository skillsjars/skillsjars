import com.jamesward.zio_mavencentral.MavenCentral
//import scala.annotation.targetName

object Models:

  opaque type Org = String
  object Org:
    def apply(s: String): Org = s
//  extension (org: Org)
//    @targetName("orgValue")
//    def value: String = org

  opaque type Repo = String
  object Repo:
    def apply(s: String): Repo = s
//  extension (repo: Repo)
//    @targetName("repoValue")
//    def value: String = repo

  opaque type SkillName = String
  object SkillName:
    def apply(s: String): SkillName = s
//  extension (skillName: SkillName)
//    @targetName("skillNameValue")
//    def value: String = skillName

  case class SkillMeta(name: SkillName, description: String, maybeLicense: Option[String])

  case class SkillLocation(org: Org, repo: Repo, subPath: List[String], skillName: SkillName)

  case class SkillsJar(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, versions: Seq[MavenCentral.Version], name: String, description: String)

  enum DeployError:
    case RepoNotFound(org: Org, repo: Repo)
    case NoSkillsDirectory(org: Org, repo: Repo)
    case InvalidSkillMd(path: String, reason: String)
    case DuplicateVersion(groupId: MavenCentral.GroupId, artifactId: MavenCentral.ArtifactId, version: MavenCentral.Version)
    case PublishFailed(reason: String)

  given CanEqual[Org, Org] = CanEqual.derived
  given CanEqual[Repo, Repo] = CanEqual.derived
  given CanEqual[SkillName, SkillName] = CanEqual.derived
  given CanEqual[DeployError, DeployError] = CanEqual.derived

  def groupIdFor(org: Org, repo: Repo, subPath: List[String] = List.empty): MavenCentral.GroupId =
    val parts = List("com", "skillsjars", org: String, repo: String) ++ subPath
    MavenCentral.GroupId(parts.mkString("."))

  def artifactIdFor(skillName: SkillName): MavenCentral.ArtifactId =
    MavenCentral.ArtifactId(skillName: String)
