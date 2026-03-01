# Task Plan — 2026-03-01 TMT3 Wave 1 Pods

## Goal
Complete Wave 1 integration for:
- Pod A `phi-source-domain`
- Pod C `solve-no-rewrite-layer`

## Pod A Goal
Remove `Solved.canonical`-driven runtime identity decisions in Phi translation path and enforce strict trace/replay-domain keyed behavior without fallback policy.

## Pod A Scope (Owned Files)
- src/MLF/Elab/Phi/Translate.hs
- src/MLF/Elab/Phi/Omega.hs
- src/MLF/Elab/Phi/IdentityBridge.hs
- test/Phi/IdentityBridgeSpec.hs
- test/Phi/AlignmentSpec.hs
- test/ElaborationSpec.hs

## Pod C Goal
Remove runtime load-bearing dependence on `rewriteConstraintWithUF` as a boundary layer, ensure solve outputs are canonical for downstream runtime consumers, and preserve solver/type behavior.

## Pod C Scope (Owned Files)
- src/MLF/Constraint/Solve.hs
- src/MLF/Constraint/Presolution/Driver.hs
- src/MLF/Elab/Run/Pipeline.hs
- test/SolveSpec.hs
- test/PipelineSpec.hs

## Phases
1. [completed] Baseline scan + initialize notes
2. [completed] Add/adjust tests (RED)
3. [completed] Implement strict trace/replay behavior + solve-boundary cleanup (GREEN)
4. [completed] Run targeted acceptance checks
5. [in_progress] Wave 1 integration merge + full gate

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `tasks/todo/2026-03-01-tmt3-thesis-exact` missing in pod worktrees | 1 | Created folder and initialized `task_plan.md`, `findings.md`, and `progress.md`. |
| Add/add merge conflicts on task files during Wave 1 integration | 1 | Consolidated pod notes into merged campaign-level task artifacts. |

## Pod D Wave 2 Addendum
### Goal
Deliver DEV-TMT-ELAB-SOLVED-PROJECTION, DEV-TMT-RESULT-TYPE-CONTEXT, and DEV-TMT-DUAL-PATH-GUARDRAIL outcomes under owned Pod D files.

### Status
1. [completed] Remove runtime `Solved.fromPresolutionResult` dependency in `MLF.Elab.Run` path.
2. [completed] Remove `ResultTypeContext` identifier from run-path result-type API (`ResultTypeInputs` replacement).
3. [completed] Retire active `DualPathSpec` wiring from test harness/cabal.
4. [completed] Run focused acceptance checks and prepare commit.
