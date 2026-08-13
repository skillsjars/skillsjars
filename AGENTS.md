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
