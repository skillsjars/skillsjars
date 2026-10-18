This project uses sbt-reload, fetch the instructions for using it from: https://github.com/jamesward/sbt-reload/blob/main/README.md

## Agent Skills (SkillsJars)

This project pulls in Agent Skills as SkillsJars build dependencies (declared in `build.sbt`
under the plugin's `Skills` config). Before working, extract them so they are available on the
filesystem (prefer the project's `./sbta` wrapper, fall back to `sbt` on PATH):

    ./sbta extractSkillsJars   # (or: sbt extractSkillsJars)

This writes each skill to `.kiro/skills/skillsjars__<owner>__<repo>__<skill>/SKILL.md`. Read the
extracted `SKILL.md` files and follow any that are relevant to the task.

To add more skills, browse https://skillsjars.com (or `curl -H "Accept: text/markdown" https://skillsjars.com/`),
add the dependency to `build.sbt` with `% Skills`, then re-run extraction.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbta` (this build's `./sbta --mcp`, configured in `.mcp.json` and `.kiro/settings/mcp.json`, approved in `.claude/settings.json`): use it for all sbt work: `compile`, `test` (`full` for all), `run`, `check` after editing Scala 3 sources, `glob-search` / `inspect` / `symbol-location` for symbols, `sbt-task` for any other command. Its tools are deferred in Claude Code: load them with ToolSearch (search `sbta`). If it's unavailable, say so and use `./sbta <command>`. Logs: `project/target/sbta/mcp.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
