# Task Plan: Remove View Alias Duplicates

## Metadata
- Date: 2026-03-07
- Execution mode: local implementation
- Skills in use: planning-with-files, haskell-pro

## Goal
Remove duplicate `...View` / `...FromView` aliases now that the unsuffixed APIs are already `PresolutionView`-typed, leaving one canonical name per helper.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Audit remaining aliases and callers | complete | Alias defs and callers mapped |
| 2. Migrate callers to unsuffixed names | complete | Updated runtime/test call sites |
| 3. Remove duplicate alias exports/defs | complete | Removed duplicate alias exports/definitions |
| 4. Run focused alias checks | complete | Focused alias/row2 checks passed |
| 5. Full verification and archive | complete | Full gate passed; docs synced; task archived |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
