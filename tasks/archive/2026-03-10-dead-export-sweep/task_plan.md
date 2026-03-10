# Task Plan

## Goal
Perform one more repo-wide sweep for similarly dead exports, validate the strongest candidates against live call sites and boundary expectations, and leave one clearly recommended next cleanup (if any).

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| Initialize sweep tracker | complete | Created task folder and captured current workspace context |
| Scan internal export surfaces | complete | Automated first pass produced a shortlist of internal exports with no external source hits |
| Validate strongest candidates | complete | Narrowed the shortlist to high-confidence stale exports versus likely intentional surfaces |
| Recommend next cleanup | complete | Best next candidate is the unused `chiLookupBindParent` / `chiBindParents` `ChiQuery` surface |

## Decisions
- Treat this pass as analysis-only unless the user later asks to land a discovered cleanup.
- Respect the existing uncommitted `AGENTS.md` change and do not modify it during the sweep.
- Prefer internal `src/` owner modules over public `src-public/` surfaces when looking for dead exports.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| None yet | 0 | n/a |
