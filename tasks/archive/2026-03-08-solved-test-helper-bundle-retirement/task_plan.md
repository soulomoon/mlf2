# Task Plan: Retire Solved Test/Audit Helper Bundle

## Metadata
- Date: 2026-03-08
- Execution mode: table-driven facade narrowing
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Move the remaining test/audit-only `Solved` helpers behind a test utility and remove them from the public `Solved` facade: `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement`.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Create test utility | completed | Added `test/SolvedFacadeTestUtil.hs` with stable replacements built from public/query APIs |
| 2. Remove helper bundle from `Solved` | completed | Removed the bundle from the public facade; internal implementation remains hidden |
| 3. Patch tests/docs | completed | Patched affected tests and synced architecture/notes/changelog/TODO |
| 4. Verify | completed | Full gate and focused solved/witness/scope/guard slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- The public `Solved` facade no longer carries the test/audit-only helper bundle.
- The next smallest production-facing cleanup is moving `pruneBindParentsSolved` to its real owner in `MLF.Constraint.Finalize`.
