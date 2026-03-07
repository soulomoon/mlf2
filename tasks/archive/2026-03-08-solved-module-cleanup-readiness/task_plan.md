# Task Plan: Solved Module Cleanup Readiness

## Metadata
- Date: 2026-03-08
- Execution mode: audit-first
- Skills in use: using-superpowers, planning-with-files, haskell-pro

## Goal
Determine whether the `Solved` module is ready for cleanup by auditing its current responsibilities, call sites, and coupling points, then identify any blockers or prerequisites before refactoring.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Initialize audit context | completed | Created task folder, checked session catchup, inspected local diff |
| 2. Inspect `Solved` module and usages | completed | Module surface, imports, and docs/tests audited |
| 3. Assess cleanup blockers | completed | Remaining blockers reduced to a few compatibility/builder seams |
| 4. Summarize recommendation | completed | Ready for targeted cleanup, not wholesale removal |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- Verdict: `MLF.Constraint.Solved` is ready for targeted cleanup/splitting, but not yet for aggressive deletion or a semantics-changing rewrite.
- Safe first moves: trim dead exports/helpers, replace `GeneralizeEnv.geRes :: Solved` with the canonical map it actually uses, and centralize the remaining `PresolutionView -> Solved` compatibility builders.
- Main caution: `Solved` still owns production construction/finalization and reify compatibility helpers, so cleanup should preserve that boundary or migrate those callers first.
