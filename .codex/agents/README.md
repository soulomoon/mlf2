# Project-local Codex Agents

These agents were installed from
`VoltAgent/awesome-codex-subagents` into `.codex/agents/` so they are scoped to
this repository.

The selected set is intentionally small and matches the repo's actual work:

- `code-mapper`: trace ownership, entry points, and execution flow before
  Haskell changes.
- `reviewer`: PR-style correctness/regression review.
- `debugger`: isolate failing tests, runtime divergences, and root causes.
- `test-automator`: add or tighten targeted Hspec regression coverage.
- `build-engineer`: handle Cabal/build graph, compiler pipeline, and CI issues.
- `documentation-engineer`: keep `AGENTS.md`, `TODO.md`,
  `implementation_notes.md`, `CHANGELOG.md`, and operator docs aligned.
- `research-analyst`: investigate thesis, paper, and design questions with
  explicit evidence boundaries.
- `refactoring-specialist`: perform behavior-preserving structural cleanups.

Local normalization:

- All installed agents were normalized to `model = "gpt-5.4"` and
  `model_reasoning_effort = "xhigh"` to match this repository's subagent
  policy in `AGENTS.md`.
- Upstream source: <https://github.com/VoltAgent/awesome-codex-subagents>
