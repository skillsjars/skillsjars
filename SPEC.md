## Purpose

This is a website like https://www.webjars.org but for Agent Skills (https://agentskills.io/).
SkillsJars contain Agent Skills and are published to Maven Central.

Users can browse Agent Skills that live on Maven Central under `com.skillsjars`.
Users can get the Maven coordinates for a specific version of a SkillJar.
Users can list the files in a SkillJar.
Users can publish new versions of SkillJars.

## SkillJar Structure

Skills have Maven coordinates like `com.skillsjars:myorg__myrepo__myskill:2026_02_13-1af0a2e`

The groupId is always `com.skillsjars`. The artifactId is constructed from the org, repo, and skill path joined with `__` (each component is lowercased with non-alphanumeric characters except `-` and `_` removed).

Each version has files that contains one skill from a GitHub repo.
For instance, the `com.skillsjars:myorg__myrepo__myskill:2026_02_13-1af0a2e` SkillsJar would be based on the GitHub repo `myorg/myrepo` containing the file:
```
skills/myskill/SKILL.md
```

The resulting SkillsJar would contain this file:
```
/META-INF/skills/myorg/myrepo/myskill/SKILL.md
```

Given a GitHub repo `myorg/myrepo` which has a skills directory containing multiple skills, multiple SkillsJars would be created.
For example, if `myorg/myrepo` contained `skills/skill1/SKILL.md` and `skills/skillgroup/skill1/SKILL.md` then there would be two SkillsJars:
`com.skillsjars:myorg__myrepo__skill1:2026_02_13-1af0a2e`
`com.skillsjars:myorg__myrepo__skillgroup__skill1:2026_02_13-1af0a2e`

If a repo has a root-level `SKILL.md` (and no `skills/` subdir), the artifactId is just `org__repo` (e.g. `com.skillsjars:jdubois__dr-jskill:2026_02_19-a951a5e`).

Some skills may contain additional files beyond the `SKILL.md` file. These are included in the SkillsJar.

## Deployment

When deploying a new version of a SkillJar (via a web form or POST request):
- POST to `/deploy` with form body containing `org` and `repo` parameters
- The GitHub public repo is shallow-cloned (depth 1)
- If a `skills/` subdir exists, scan it for SKILL.md files; otherwise scan the repo root
- Fail if no SKILL.md files are found
- Validate no overlapping skill paths (e.g. both `/a` and `/a/b` having SKILL.md)
- For each `SKILL.md` (no limit to depth), create a SkillsJar with the version consisting of the latest repo commit author date & short commit hash (e.g. `2026_02_13-1af0a2e`)
- Check for existing SkillsJars with the same coordinates on Maven Central (duplicates are skipped)
- The `pom.xml` metadata should use the `name` and `description` from the `SKILL.md` file using the https://agentskills.io/specification format
- License resolution order: SKILL.md frontmatter license field, then skill directory LICENSE file, then GitHub API license endpoint, then repo root LICENSE file. Skills without a license are skipped (not failed).
- JAR and POM files are signed with GPG when `OSS_GPG_KEY` env var is set
- All artifacts are bundled into a single ZIP and uploaded to Maven Central

## Web UI

- Cache the list of SkillJars in memory (1 hour TTL for successful fetches, no caching on failure)
- Display the list of SkillJars (with a drop down for version selection)
- Build tool selector (Maven/Gradle/sbt) to show appropriate dependency snippets
- Filter / Search for SkillJars (by name or description with case-insensitive substring matching)
- No pagination for now

## Technologies & Style

Scala 3
- Significant indentation syntax
- Use type class & derives syntax
- NO MUTABILITY EVER!!
- Use ADTs and Enums
- Always create type aliases for meaningful primitive types
- When type aliases can be constrained, create opaque types

Scala ZIO
- Defer/Direct syntax
- Effects should use non-Exception errors (transform Throwables when needed)
- Effect errors should be something the user should be aware of or something that can be recovered from.
- Other types of errors should cause the Effect to die.

ZIO HTTP (HTTP client & server)
- Template2 for web UI
- TailwindCSS for styling (via a WebJar)

zio-mavencentral
- For querying & deploying to Maven Central

ZIO Cache

Testing
- Use ZIO Test smart assertions
- Use a mock implementation of the Maven Central deployer for testing (provided layer in test scope)

org.eclipse.jgit

## Dev Usage

Start an auto-reloading dev server:
```
./sbt ~reStartTest
```

Run tests:
```
./sbt test
```
