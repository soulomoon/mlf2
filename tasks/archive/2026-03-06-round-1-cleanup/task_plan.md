# Task Plan: 2026-03-06 Round-1 Artifact Cleanup

## Goal
Clean up obsolete round-1 improving-loop artifacts by archiving superseded source docs, moving completed round-1 task folders out of `tasks/todo/`, and updating the small set of references that would otherwise break.

## Scope
- Repository: `/Volumes/src/mlf4`
- Archived source docs:
  - `docs/plans/archive/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`
  - `docs/prompts/archive/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`
- Archived completed task folders:
  - `tasks/archive/2026-03-05-tmt-improving-loop-orchestrator-fresh`
  - `tasks/archive/2026-03-05-tmt-thesis-exact-improving-loop-agent-runner`
- Preserve historical content; do not rewrite execution outcomes.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inventory round-1 artifacts and live references | complete | Identified round-1 source docs and completed task folders still in `tasks/todo/`. |
| 2. Archive obsolete artifacts | complete | Moved round-1 docs into archive locations and moved completed round-1 task folders into `tasks/archive/`. |
| 3. Update broken references and cleanup notes | complete | Repointed live references and added archive notes to superseded round-1 docs. |
| 4. Verify final layout | complete | Targeted search shows only the preserved historical `tasks/todo/...` literal inside the archived round-1 plan; live references are clean. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None | - | Final verification completed without errors. |

## Decisions
- Treat `docs/plans/2026-03-06-...round-2.md` as the live successor for orchestration guidance.
- Preserve historical round-1 materials by archiving rather than deleting.
- Limit reference edits to files that would otherwise point at moved artifacts.
