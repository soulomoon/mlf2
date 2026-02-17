# Task Plan — BUG-2026-02-16-007/008 SchemeFreeVars regression

## Goal
Fix the shared root cause behind BUG-2026-02-16-007 and BUG-2026-02-16-008 so BUG-003-V1 and BUG-003-V2 return to the expected thesis-aligned sentinel failure class (instead of `SchemeFreeVars ... "__rigid24"`), while preserving existing Phase 6 behavior.

## Scope
- Reproduce 007/008 failures exactly.
- Perform root-cause investigation on Generalize/Elaborate path.
- Add/adjust regression tests as needed.
- Implement minimal root-cause fix.
- Re-verify targeted suites and update tracker/docs.

## Phases
| Phase | Description | Status |
|---|---|---|
| 1 | Reproduce failures and collect evidence | complete |
| 2 | Trace root cause through Generalize/Elaborate | complete |
| 3 | Implement minimal fix with regression guard | complete |
| 4 | Verify targeted tests + update docs/trackers | complete |

## Decisions
- Work on BUG-2026-02-16-007 and BUG-2026-02-16-008 in one pass because they share the same error class and likely root cause.
- Treat plain `SchemeFreeVars` as fallback-equivalent to `BindingTreeError GenSchemeFreeVars` in pipeline/result-type generalization paths to avoid masking underlying BUG-003 failure class.
- Keep BUG-2026-02-11-004 open as the active bounded-alias umbrella; close only the sentinel-drift split bugs (007/008).

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `renameFile ... Util.o.tmp ... does not exist` during parallel cabal test runs | 1 | Re-ran repro commands sequentially (no concurrent Cabal writes). |
