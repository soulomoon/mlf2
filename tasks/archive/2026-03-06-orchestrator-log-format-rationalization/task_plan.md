# Task Plan: 2026-03-06 Orchestrator Log Format Rationalization

## Goal
Choose a single authoritative orchestrator log format, remove the duplicated markdown-vs-JSONL contract from the live workflow, and update the related planning/scaffolding docs consistently.

## Scope
- Repository: `/Volumes/src/mlf4`
- Live workflow docs:
  - `docs/plans/2026-03-06-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-2.md`
  - `docs/prompts/improving-loop-agent.prompt2.md`
  - `docs/prompts/goal-improving-loop-agent.prompt.md`
- Reusable skill/scaffolding:
  - `.codex/skills/goal-table-orchestrator-loop/SKILL.md`
  - `.codex/skills/goal-table-orchestrator-loop/references/orchestrated-round-template.md`
  - `.codex/skills/goal-table-orchestrator-loop/references/orchestrator-prompt-template.md`
  - `.codex/skills/goal-table-orchestrator-loop/scripts/scaffold_goal_loop_docs.py`
- Project docs:
  - `CHANGELOG.md`

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Audit current dual-log contract and choose authoritative format | complete | JSONL is the better fit for strict YES/NO gate replay; markdown log duplicates information already covered by findings/progress. |
| 2. Write design and implementation plan artifacts | complete | Added short design and implementation plan docs under `docs/plans/`. |
| 3. Update live docs and scaffolding | complete | Updated the live plan/prompts plus goal-loop skill references and scaffold script to use JSONL-only orchestrator logging. |
| 4. Verify references and summarize | complete | Verified no live `orchestrator-log.md` requirements remain; py_compile and scaffold smoke test passed. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None | - | Verification and scaffold smoke test passed without errors. |

## Decisions
- `orchestrator-log.jsonl` is the single authoritative orchestration log.
- Human-readable summaries belong in `findings.md` and `progress.md`, not in a second canonical log file.
- Archived historical artifacts may retain prior wording when preserved for provenance; the live contract and scaffolding must be consistent.
