# Task Plan

## Goal
Remove the apparently dead `instEdgeOwnerM` export from `MLF.Constraint.Presolution.StateAccess` with a bounded, thesis-safe cleanup driven by a failing guard test first.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize cleanup tracker | complete | Created the active task folder and recovered current context |
| Confirm dead-export boundary | complete | Verified the definition/export and confirmed there are no live call sites |
| Add failing guard test | complete | Added and observed the expected failing `PipelineSpec` source guard |
| Remove dead export | complete | Removed the dead export/definition with no consumer migration required |
| Verify and sync docs | complete | Targeted slices and the full Cabal gate passed; docs/task surfaces are synced |

## Decisions
- Keep the scope minimal: target `instEdgeOwnerM` only unless verification uncovers a directly coupled dead surface.
- Treat this as owner-boundary cleanup, not an algorithmic presolution change.
- Follow TDD strictly: guard first, watch it fail, then patch.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None yet | 0 | n/a |
