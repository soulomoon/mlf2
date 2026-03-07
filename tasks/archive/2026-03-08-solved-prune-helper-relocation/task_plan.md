# Task Plan: Relocate pruneBindParentsSolved to Finalize

## Metadata
- Date: 2026-03-08
- Execution mode: owner-local helper relocation
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Remove `pruneBindParentsSolved` from the public `Solved` facade and keep it only behind `MLF.Constraint.Finalize`, its actual owner-local use site.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm owner set | completed | Finalize was the only live production owner and one test caller remained |
| 2. Relocate facade usage | completed | Removed from the facade and routed the test use through `Finalize.stepPruneSolvedBindParents` |
| 3. Sync docs/notes | completed | Architecture doc, implementation notes, changelog, and TODO updated |
| 4. Verify | completed | Full gate and focused solved/elaboration/guard slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- `pruneBindParentsSolved` is no longer part of the public `Solved` facade.
- The remaining table work is the production read-query cluster and reify-facing canonical helpers.
