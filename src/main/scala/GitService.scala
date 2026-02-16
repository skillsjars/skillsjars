import Models.*
import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.direct.*
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevWalk

import java.io.File
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import scala.jdk.CollectionConverters.*

object GitService:

  case class RepoInfo(
    skills: List[(SkillLocation, File)],
    version: MavenCentral.Version,
    maybeLicense: Option[String],
  )

  // any reason to keep the dir around longer?
  def cloneAndScan(org: Org, repo: Repo): ZIO[Scope, DeployError, RepoInfo] =
    defer:
      val tmpDir = ZIO.acquireRelease(
        ZIO.attemptBlocking(Files.createTempDirectory("skillsjars-")).orDie
      )(dir => ZIO.attemptBlocking(deleteRecursive(dir.toFile)).ignoreLogged).run

      val repoUrl = s"https://github.com/$org/$repo.git"

      val git = ZIO.attemptBlocking:
        Git.cloneRepository()
          .setURI(repoUrl)
          .setDirectory(tmpDir.toFile)
          .setDepth(1)
          .call()
      .orElseFail(DeployError.RepoNotFound(org, repo)).run

      val version = extractVersion(git).run

      val skillsDir = File(tmpDir.toFile, "skills")
      ZIO.fail(DeployError.NoSkillsDirectory(org, repo)).when(!skillsDir.isDirectory).run

      val skillLocations = findSkills(skillsDir, org, repo)
      ZIO.fail(DeployError.NoSkillsDirectory(org, repo)).when(skillLocations.isEmpty).run

      val maybeLicense = detectLicense(tmpDir.toFile)
      RepoInfo(skillLocations, version, maybeLicense)

  private def extractVersion(git: Git): IO[DeployError, MavenCentral.Version] =
    ZIO.attemptBlocking:
      val repo = git.getRepository
      val head = repo.resolve("HEAD")
      val walk = RevWalk(repo)
      val commit = walk.parseCommit(head)
      val authorDate = commit.getAuthorIdent.getWhenAsInstant
      val shortHash = head.abbreviate(7).name()
      val dateStr = DateTimeFormatter.ofPattern("yyyy_MM_dd")
        .withZone(ZoneOffset.UTC)
        .format(authorDate)
      walk.dispose()
      MavenCentral.Version(s"$dateStr-$shortHash")
    .orDie

  private def findSkills(skillsDir: File, org: Org, repo: Repo): List[(SkillLocation, File)] =
    def scan(dir: File, pathParts: List[String]): List[(SkillLocation, File)] =
      val files = Option(dir.listFiles()).getOrElse(Array.empty[File]).toList
      val hasSkillMd = files.exists(f => f.isFile && f.getName == "SKILL.md")
      val thisSkill =
        if hasSkillMd then
          val skillName = SkillName(dir.getName)
          List((SkillLocation(org, repo, pathParts.dropRight(1), skillName), dir))
        else
          Nil
      val subSkills = files.filter(_.isDirectory).flatMap(subDir => scan(subDir, pathParts :+ subDir.getName))
      thisSkill ++ subSkills

    val subDirs = Option(skillsDir.listFiles()).getOrElse(Array.empty[File]).filter(_.isDirectory).toList
    subDirs.flatMap(dir => scan(dir, List(dir.getName)))

  private def detectLicense(repoDir: File): Option[String] =
    val licenseNames = List("LICENSE", "LICENSE.md", "LICENSE.txt", "LICENCE", "LICENCE.md", "LICENCE.txt")
    licenseNames.flatMap: name =>
      val file = File(repoDir, name)
      if file.isFile then
        val content = String(Files.readAllBytes(file.toPath))
        Some(detectLicenseType(content))
      else
        None
    .headOption

  private def detectLicenseType(content: String): String =
    val lower = content.toLowerCase
    if lower.contains("apache license") && lower.contains("version 2.0") then "Apache-2.0"
    else if lower.contains("mit license") || lower.contains("permission is hereby granted, free of charge") then "MIT"
    else if lower.contains("gnu general public license") && lower.contains("version 3") then "GPL-3.0"
    else if lower.contains("gnu general public license") && lower.contains("version 2") then "GPL-2.0"
    else if lower.contains("bsd 3-clause") || lower.contains("redistribution and use in source and binary forms") then "BSD-3-Clause"
    else if lower.contains("bsd 2-clause") then "BSD-2-Clause"
    else if lower.contains("mozilla public license") then "MPL-2.0"
    else if lower.contains("isc license") then "ISC"
    else "Unknown"

  private def deleteRecursive(file: File): Unit =
    if file.isDirectory then
      Option(file.listFiles()).getOrElse(Array.empty[File]).foreach(deleteRecursive)
    file.delete()
