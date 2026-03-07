# Task Plan: Retire Solved Enumeration Helpers

## Metadata
- Date: 2026-03-08
- Execution mode: table-driven facade narrowing
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Retire the test/audit-only enumeration helpers `allNodes` and `instEdges` from the public `Solved` facade and move their test usage to direct constraint inspection.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm caller set | completed | `allNodes` and `instEdges` were only used from tests |
| 2. Retire helpers from facade | completed | Removed both helpers from the public facade and switched tests to direct constraint inspection |
| 3. Sync docs/notes | completed | Architecture doc, implementation notes, changelog, and TODO updated |
| 4. Verify | completed | Full gate and focused solved/alignment/guard slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- The public `Solved` facade no longer exposes the enumeration helpers `allNodes` and `instEdges`.
- The next strongest table candidate is moving the remaining test/audit-only helpers (`mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, `validateOriginalCanonicalAgreement`) behind test utilities instead of the facade.
