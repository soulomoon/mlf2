# Task Plan

## Goal
Retire the remaining `pendingWeakenOwners` façade re-export from `MLF.Constraint.Presolution.EdgeUnify` by importing the helper directly from its `Omega` owner, while keeping the cleanup thesis-safe and tightly bounded.

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize cleanup tracker | complete | Seeded from the backlog-reset audit and explorer findings |
| Confirm exact ownership boundary | complete | Verified live exports/imports and captured the failing red guard for the remaining façade re-export |
| Apply bounded façade cleanup | complete | Removed the re-export and switched the two live consumers to direct `Omega` imports |
| Run targeted verification | complete | Focused guard plus presolution façade / repo guard / Phase 4 slices all passed |
| Sync task docs | complete | Updated `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, and task artifacts |

## Decisions
- Keep the first pass minimal: target only the `pendingWeakenOwners` façade re-export and its direct imports unless verification shows a tiny adjacent dead-export cleanup is obviously safe.
- Preserve thesis-sensitive owner-boundary behavior in `Omega`, `StateAccess`, and queue-stamping logic; this task is about owner/module boundaries, not algorithmic semantics.
- Treat `instEdgeOwnerM` as optional adjacent follow-up only if the bounded pass and tests remain straightforward.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None yet | 0 | n/a |
