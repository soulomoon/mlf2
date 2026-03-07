# Task Plan: Relocate Remaining Solved Compatibility Builders

## Metadata
- Date: 2026-03-08
- Execution mode: semantics-preserving API narrowing
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Relocate the remaining shared compatibility builders `fromConstraintAndUf` and `rebuildWithConstraint` out of the public `MLF.Constraint.Solved` facade into internal owner-local usage, while preserving `Solved` opacity and leaving `fromSolved`/`solvedFromView` local where they already are.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm relocation shape | completed | Finalize/Reify were the only live callers and opacity required a facade/internal split |
| 2. Split facade from implementation | completed | Added `MLF.Constraint.Solved.Internal` and rewrote `MLF.Constraint.Solved` as a thin facade |
| 3. Redirect owner modules/tests | completed | Finalize/Reify now use internal builders and tests reflect the narrower facade |
| 4. Sync docs/notes | completed | Architecture doc, implementation notes, changelog, and TODO updated |
| 5. Verify | completed | Full gate and focused solved/pipeline/presolution slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Initial internal-module write failed because `src/MLF/Constraint/Solved/` did not exist | 1 | Created the directory and reapplied the facade/internal split cleanly |

## Conclusion
- The shared compatibility builders no longer live on the public `MLF.Constraint.Solved` facade.
- Remaining compat seams are now truly owner-local (`fromSolved`, `solvedFromView`) rather than facade-level builders.
