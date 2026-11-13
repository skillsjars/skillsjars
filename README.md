SkillsJars
----------

This is the webapp for https://www.skillsjars.com

## API

### Search

`GET /` lists all SkillsJars. Add `q` to filter them by a case-insensitive substring match on name or description.

The `Accept` header picks the response format:

| `Accept`                           | Response                 |
|------------------------------------|--------------------------|
| `application/json`                 | JSON (below)             |
| `text/markdown`, `text/x-markdown` | Markdown list for agents |
| `text/html`, `*/*`, anything else  | The web page             |

If the request has no `Accept` header, the response is JSON.

```sh
curl -H "Accept: application/json" "https://www.skillsjars.com/?q=interface"
```

```json
{
  "query": "interface",
  "skillsJars": [
    {
      "groupId": "com.skillsjars",
      "artifactId": "addyosmani__agent-skills__api-and-interface-design",
      "name": "api-and-interface-design",
      "description": "Guides stable API and interface design. ...",
      "versions": ["2026_04_03-a806c63"],
      "securityScanned": true,
      "allowedTools": {}
    }
  ]
}
```

- `query` is `null` when `q` is absent or empty.
- `versions` is ordered newest first.
- `allowedTools` maps each skill name in the jar to its `allowed-tools` value from `SKILL.md`, a space-separated list of tools.

The `bt` parameter (`maven`, `gradle`, or `sbt`) only changes the dependency snippets in the HTML page.

### Latest version

`GET /{groupId}/{artifactId}/latest` returns the newest version of a SkillsJar as JSON:

```sh
curl https://www.skillsjars.com/com.skillsjars/addyosmani__agent-skills__api-and-interface-design/latest
```

```json
{"groupId":"com.skillsjars","artifactId":"addyosmani__agent-skills__api-and-interface-design","version":"2026_04_03-a806c63"}
```

This endpoint only knows the SkillsJars that the site lists. The list is cached for up to an hour, so a version published within the last hour may not show up yet.

### MCP

`/mcp` is a stateless MCP server (streamable HTTP) with the latest version of every SkillsJar the site lists, as Agent Skills:

- Clients that support the Skills extension (`io.modelcontextprotocol/skills`) get them through `skills/list`, `skills/get` and `resources/directory/read`.
- Every client can find each skill's `SKILL.md` in `resources/list` and read it and the files beside it with `resources/read`.

Skills are addressed by their directory under `META-INF/skills/` in the jar, `skill://<org>/<repo>/<path>/SKILL.md`, e.g. `skill://addyosmani/agent-skills/api-and-interface-design/SKILL.md`. If two SkillsJars have the same skill directory, only the first by coordinates is served, and the conflict is logged as an error. The jars are loaded from Maven Central on the first request and again whenever the list of SkillsJars or their latest versions change. If a jar can't be fetched, it's left out and the load is retried after 5 minutes.

### Errors

JSON errors have the form `{"error": "<message>"}`. Search returns errors in the format the `Accept` header asked for.

| Status | Cause                                                     |
|--------|-----------------------------------------------------------|
| 400    | Invalid request, e.g. `bt=ant` → `Invalid build tool: ant` |
| 404    | `/latest` for a SkillsJar the site doesn't list           |
| 502    | SkillsJars could not be fetched from Maven Central        |
