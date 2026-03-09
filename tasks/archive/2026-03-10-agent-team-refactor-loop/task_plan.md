# Agent-Team Refactor Loop Task Plan

## Summary
Goal: stabilize and land the already-split live tree through a one-primary-target loop that advances only when `Builder = done`, `Boundary Reviewer = approve`, and `QA = green`.

## Baseline
- Timestamp (UTC): 2026-03-09T19:47:10Z
- Baseline commit: `fa959d5ec3ba4056bcacff8d962a218cfd0a9227`
- Base branch: `master`
- Current branch: `master`
- Live-tree assumption: the large façade + child-module splits are already present in the working tree, so this campaign starts from the current tree rather than the pre-split baseline.

## Repository Safety Snapshot
- Dirty tracked files at start: `CHANGELOG.md`, `README.md`, `app/Main.hs`, `docs/architecture.md`, `implementation_notes.md`, `mlf2.cabal`, `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`, `src/MLF/Constraint/Presolution/Base.hs`, `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/Expansion.hs`, `src/MLF/Constraint/Presolution/TestSupport.hs`, `src/MLF/Constraint/Solve.hs`, `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Phi/Omega.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Types.hs`, `src/MLF/Reify/Core.hs`, `test/ConstraintGenSpec.hs`, `test/ElaborationSpec.hs`, `test/FrontendParseSpec.hs`, `test/PipelineSpec.hs`, `test/PublicSurfaceSpec.hs`, `test/RepoGuardSpec.hs`, `test/ThesisFixDirectionSpec.hs`
- Untracked files/directories at start: `src/MLF/Constraint/Presolution/EdgeUnify/`, `src/MLF/Constraint/Solve/Finalize.hs`, `src/MLF/Constraint/Solve/Harmonize.hs`, `src/MLF/Constraint/Solve/Worklist.hs`, `src/MLF/Elab/Elaborate/`, `src/MLF/Elab/Phi/Omega/`, `src/MLF/Reify/Bound.hs`, `src/MLF/Reify/Cache.hs`, `src/MLF/Reify/Named.hs`, `src/MLF/Reify/Type.hs`, `tasks/archive/2026-03-10-remaining-refactor-loop/`
- Off-limits rule for this planning task: respect the live split edits already in the tree and only add the new planning artifacts plus synchronized status-note updates.

## Orchestrator Artifacts
- Mechanism table: `tasks/todo/2026-03-10-agent-team-refactor-loop/mechanism_table.md`
- Orchestrator prompt: `tasks/todo/2026-03-10-agent-team-refactor-loop/orchestrator_prompt.md`
- Orchestrator log: `tasks/todo/2026-03-10-agent-team-refactor-loop/orchestrator-log.jsonl`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Initialize task artifacts | complete | Created the live-tree stabilization task packet and synced the planning/status docs |
| 2. Loop 0 — Baseline freeze | complete | `cabal build all && cabal test` passed from the live split tree; the campaign stayed in stabilize-and-land mode |
| 3. Loop 1 — Warning-free cleanup | complete | No additional production cleanup was needed beyond the already warning-free split tree |
| 4. Loop 2 — Façade ownership guards | complete | Added explicit thin-façade guards in `test/PipelineSpec.hs` and `test/RepoGuardSpec.hs` for all five split owners |
| 5. Loop 3 — Public API hard-cut stabilization | complete | `PublicSurfaceSpec` / `RepoGuardSpec` and the docs agree that `MLF.Pipeline` is runtime-only and `MLF.API` is frontend-only |
| 6. Loop 4 — Cabal and module-graph hygiene | complete | Added a `RepoGuardSpec` check that the split child modules remain implementation-only Cabal entries |
| 7. Loop 5 — Split-specific regression sweep | complete | Ran the ordered owner slices and kept the full gate green afterwards |
| 8. Loop 6 — Final landing | complete | Synced docs/task notes and prepared the task for archive after a final green full gate |

## Decisions
- Treat the current live tree as the source of truth for the remaining queue; do not plan against the pre-split baseline.
- Keep one builder only per loop; no parallel builders on the same loop target.
- Allow `Boundary Reviewer`, `QA Runner`, and `Docs Sync` to run only after the builder reports done for the current loop.
- Keep the public topology fixed: `MLF.Pipeline` is the canonical runtime/execution surface, `MLF.API` stays frontend-only, and `MLF.XMLF` remains the explicit xMLF surface.
- Keep the shared low-risk plumbing already introduced (`EdgeArtifacts`, `TraceCopyArtifacts`, `prepareTraceCopyArtifacts`, `mkInitialPresolutionState`, `tyExpNodeIds`) and treat the split façades as export owners while child modules remain implementation-only.
- If a loop uncovers a real thesis-faithfulness bug, pause the stabilization loop and spin a bugfix sub-loop before resuming the queue.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| The local `goal-table-orchestrator-loop` scaffold script crashed on `datetime.UTC` under the available Python runtime | 1 | Created the mechanism table, orchestrator prompt, and initial JSONL log manually instead of patching the shared skill scaffold during this planning task |
| The first owner-sweep command shape passed Hspec match strings through Cabal incorrectly | 1 | Switched Loop 5 targeted slices to the built `mlf2-test` binary so `-m/--match` patterns with spaces survived intact |
