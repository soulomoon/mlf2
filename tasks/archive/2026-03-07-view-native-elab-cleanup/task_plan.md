# Task Plan: Finish χp/View-Native Elaboration Cleanup

## Metadata
- Date: 2026-03-07
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Remove the remaining non-test/non-legacy `Solved -> PresolutionView` wrapper APIs from elaboration/runtime and reification, making `PresolutionView` the primary internal surface and leaving `fromSolved` only in the presolution boundary, legacy code, and tests.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize task files and inspect wrappers | complete | Mapped remaining wrappers and affected callers |
| 2. Add failing source-guard updates | complete | Added confinement guard for runtime/reify modules |
| 3. Refactor runtime APIs to PresolutionView | complete | Switched runtime/reify surfaces to explicit PresolutionView |
| 4. Migrate tests and legacy callers | complete | Updated tests and preserved legacy compatibility path |
| 5. Run targeted and full verification | complete | Focused slices and full gate passed |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
