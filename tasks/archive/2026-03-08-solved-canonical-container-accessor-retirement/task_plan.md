# Task Plan: Retire Raw Canonical Container Accessors

## Metadata
- Date: 2026-03-08
- Execution mode: table-driven low-risk API retirement
- Skills in use: using-superpowers, brainstorming, planning-with-files, haskell-pro, verification-before-completion

## Goal
Retire the dead raw canonical container accessors `canonicalBindParents` and `canonicalGenNodes` from the `Solved` surface.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm no live callers | completed | No live code/test callers remained; only docs/history mentioned the accessors |
| 2. Remove accessors | completed | Removed both accessors from the facade and internal implementation |
| 3. Add guard and sync notes | completed | Added a facade-absence guard and synced the docs/notes |
| 4. Verify | completed | Full gate and focused solved/new-guard slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |

## Conclusion
- The raw canonical container accessors are no longer part of the `Solved` surface.
- The next smallest table item is retiring the remaining enumeration helpers `allNodes` and `instEdges` from the public facade.
