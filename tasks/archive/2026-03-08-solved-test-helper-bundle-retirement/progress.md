# Progress Log

## 2026-03-08
- Started the cleanup task for retiring the remaining test/audit-only helper bundle from the public `Solved` facade.
- Added `test/SolvedFacadeTestUtil.hs` as the test-only home for `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement`, then removed that bundle from the public `Solved` facade.
