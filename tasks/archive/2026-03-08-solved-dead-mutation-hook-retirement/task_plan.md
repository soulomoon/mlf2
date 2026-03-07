# Task Plan: Solved Dead Mutation Hook Retirement

## Metadata
- Date: 2026-03-08
- Execution mode: low-risk API cleanup
- Skills in use: using-superpowers, planning-with-files, haskell-pro, verification-before-completion

## Goal
Retire the dead `Solved` mutation hooks identified by the solved-ecosystem classification table: `rebuildWithNodes`, `rebuildWithBindParents`, and `rebuildWithGenNodes`.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Confirm dead-hook status | completed | No live code callers remained; only historical docs/audit notes mentioned the hooks |
| 2. Remove hooks from `Solved` | completed | Removed exports and definitions for the three dead mutation hooks |
| 3. Sync docs/notes | completed | Synced architecture note, implementation notes, changelog, and TODO |
| 4. Verify | completed | Full gate and targeted solved slices are green |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Shell backticks expanded during initial note creation | 1 | Rewrote the planning files using single-quoted heredocs |

## Conclusion
- The first low-risk cleanup implied by the solved classification table is now implemented: the dead write-style `Solved` hooks are removed.
- The next cleanup candidates remain narrowing `geRes :: Solved` and relocating compatibility builders into their local owner modules.
