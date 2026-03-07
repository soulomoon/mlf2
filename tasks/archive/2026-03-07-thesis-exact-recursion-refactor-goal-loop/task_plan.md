# Task Plan — 2026-03-07 thesis-exact recursion refactor goal loop

## Goal
Design a thesis-exact goal/mechanism table and improving-loop orchestrator artifacts for a Haskell-focused refactor/simplification campaign, grounded in the implementation and `papers/these-finale-english.txt`.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Initialize task workspace | complete | Created planning files and gathered prior artifact references |
| 2. Study thesis source of truth | complete | Extracted Chapter 4/9/10/15 obligations governing binding, propagation, and translation |
| 3. Audit codebase mechanisms | complete | Identified existing recursion-schemes adoption plus graph-layer no-go zones and manual hotspots |
| 4. Synthesize thesis-exact mechanism list | in_progress | Drafting mechanism rows and scoping the campaign boundary |
| 5. Design orchestrator loop artifacts | complete | Wrote the simplified prompt, mechanism table, and JSONL log template |
| 6. Review with user before finalizing | complete | User approved the broad scope, 8-row mechanism set, evidence model, simplified roles, and artifact paths |

## Constraints
- Source of truth is `papers/these-finale-english.txt`; use `papers/xmlf.txt` only if thesis is silent.
- Goal table must be thesis exact, not just code-cleanup oriented.
- Requested skills: `haskell-pro`, `recursion-schemes-refactor`, `goal-table-orchestrator-loop`.
- Follow file-planning workflow and avoid overwriting existing artifacts.

## Error Log
- 2026-03-07: Initial `rg` recursion-hotspot shell command had a quoting error in zsh; reran with a simpler pattern set and completed the audit.

## Outputs
- `/Volumes/src/mlf4/docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
- `/Volumes/src/mlf4/docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md`
- `/Volumes/src/mlf4/docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl`
- `/Volumes/src/mlf4/docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop-design.md`
- `/Volumes/src/mlf4/docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop.md`
