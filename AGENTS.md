This project uses sbt-reload, fetch the instructions for using it from: https://github.com/jamesward/sbt-reload/blob/main/README.md

## Agent Skills (SkillsJars)

This project pulls in Agent Skills as SkillsJars build dependencies (declared in `build.sbt`
under the plugin's `Skills` config). Before working, extract them so they are available on the
filesystem (prefer the project's `./sbt` wrapper, fall back to `sbt` on PATH):

    ./sbt extractSkillsJars   # (or: sbt extractSkillsJars)

This writes each skill to `.kiro/skills/skillsjars__<owner>__<repo>__<skill>/SKILL.md`. Read the
extracted `SKILL.md` files and follow any that are relevant to the task.

To add more skills, browse https://skillsjars.com (or `curl -H "Accept: text/markdown" https://skillsjars.com/`),
add the dependency to `build.sbt` with `% Skills`, then re-run extraction.

## Agent tooling

- Follow the `zen-of-projects` Skill (extract with `./sbt extractSkillsJars` into the gitignored `.kiro/skills/`); this file records only project-specific facts and exceptions.
- MCP server `sbt-mcp-skillsjars` (sbt-mcp) listens on `http://127.0.0.1:5112/`. Kiro uses the HTTP entry in `.kiro/settings/mcp.json`; start sbt first. Claude Code uses `.mcp.json`, which runs `.claude/sbt-mcp-stdio.sh` (approved in `.claude/settings.json`). That stdio bridge starts a foreground sbt in cloud sessions (`CLAUDE_CODE_REMOTE=true`), and locally only connects to an sbt you already started. Its tools are deferred: load them with ToolSearch (search `sbt-mcp-skillsjars`). Diagnostics go to `/tmp/sbt-mcp-stdio.log` and `/tmp/sbt-mcp-server.log`.
- Maintenance routine: `.factory/MAINTENANCE.md` (weekly), following the `zen-of-projects` Skill.
