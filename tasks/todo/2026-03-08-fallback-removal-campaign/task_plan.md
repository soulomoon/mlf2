# Task Plan: Fallback Removal Campaign

## Metadata
- Date: 2026-03-08
- Execution mode: thesis-exact cleanup campaign
- Skills in use: using-superpowers, planning-with-files, haskell-pro

## Goal
Remove the remaining live fallback mechanisms identified by the fallback audit, with strict fail-fast behavior replacing compatibility recovery where appropriate.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Guard-first fallback capture | in_progress | Add tests proving the targeted fallback surfaces are gone |
| 2. Generalization ladder removal | pending | Elaborate, Pipeline, ResultType.Util, Generalize |
| 3. reifyInst fallback removal | pending | Remove trace/expansion fallback synthesis |
| 4. Planner fallback removal | pending | Remove synthesized-wrapper scheme-owner fallback |
| 5. Instantiation fallback removal | pending | Remove generic heuristic fallback in inferInstAppArgsFromScheme |
| 6. Doc sync and closeout | pending | Record any keepers and final verification evidence |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
