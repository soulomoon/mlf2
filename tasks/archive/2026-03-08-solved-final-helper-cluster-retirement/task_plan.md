# Task Plan: Retire Final Solved Helper Cluster

## Metadata
- Date: 2026-03-08
- Execution mode: final table-driven facade narrowing
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Retire the last non-must-stay helper cluster from the public `Solved` facade: `lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, and `canonicalizedBindParents`, by replacing them with direct owner-local logic in `Reify.Core`, `Presolution.View`, and tests.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm remaining cluster | completed | Verified this was the last non-must-stay public helper cluster |
| 2. Replace owner-local logic | completed | Replaced owner-local use with direct constraint/canonical logic |
| 3. Remove cluster from facade | completed | Removed the final helper cluster from the public facade |
| 4. Sync docs/notes | completed | Architecture doc, implementation notes, changelog, and TODO updated |
| 5. Verify | completed | Full gate and focused solved/pipeline/presolution slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- The public `Solved` facade is now reduced to the thesis-relevant core only.
- There is nothing left to do for the current 3-column classification table except keep the guard stack green.
