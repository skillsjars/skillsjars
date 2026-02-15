## Purpose

This is a website like https://www.webjars.org but for Agent Skills (https://agentskills.io/).
SkillsJars contain Agent Skills and are published to Maven Central.

Users can browse Agent Skills that live on Maven Central under `com.skillsjars`.
Users can get the Maven coordinates for a specific version of a SkillJar.
Users can list the files in a SkillJar.
Users can publish new versions of SkillJars.

## SkillJar Structure

Skills have Maven coordinates like `com.skillsjars.myorg.myrepo:myskill:2026_02_13-1af0a2e`

Each version has files that contains one skill from a GitHub repo.
For instance, the `com.skillsjars.myorg.myrepo:myskill:2026_02_13-1af0a2e` SkillsJar would be based on the GitHub repo `myorg/myrepo` containing the file:
```
skills/myskill/SKILL.md
```

The resulting SkillsJar would contain this file:
```
/META-INF/resources/skills/myorg/myrepo/myskill/SKILL.md
```

Given a GitHub repo `myorg/myrepo` which has a skills directory containing multiple skills, multiple SkillsJars would be created.
For example, if `myorg/myrepo` contained `skills/skill1/SKILL.md` and `skills/skillgroup/skill1/SKILL.md` then there would be two SkillsJars:
`com.skillsjars.myorg.myrepo:skill1:2026_02_13-1af0a2e`
`com.skillsjars.myorg.myrepo.skillgroup:skill1:2026_02_13-1af0a2e`

Some skills may contain additional files beyond the `SKILL.md` file. These are included in the SkillsJar.

## Deployment

When deploying a new version of a SkillJar (via a web form or POST request):
- POST request has an empty body and two query parameters: `org` and `repo`
- The GitHub public repo is cloned
- Check for a `skills` subdir and fail if it doesn't exist
- For each `SKILL.md` under the `skills` subdir (no limit to depth), create a SkillsJar with the version consisting of the latest repo commit author date & short commit hash (e.g. `2026_02_13-1af0a2e`)
- The Maven groupId and artifactId are derived from the GitHub org/repo and skill directory name (which can be nested)
- Check for existing SkillsJars with the same coordinates (duplicates are an error)
- The `pom.xml` metadata should use the `name` and `description` from the `SKILL.md` file using the https://agentskills.io/specification format
- The license specified in the `pom.xml` should use the license in the SKILL.md if specified or the GitHub repo's license
- Publish the SkillsJars to Maven Central

## Web UI

- Cache the list of SkillJars in memory
- Display the list of SkillJars (with a drop down for version selection)
- Filter / Search for SkillJars (by name or description with basic substring matching)
- No pagination for now
- DO NOT USE JavaScript

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
