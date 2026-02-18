# Task Plan: A3 (P2) API cleanup legacy elaboration exposure

## Goal
Quarantine legacy elaboration helper exposure from public API surfaces by removing legacy helper exports from public-facing pipeline modules.

## Scope
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/API.hs`
- `src-public/MLF/Pipeline.hs`
- tracking updates: `TODO.md`, `CHANGELOG.md`

## Phases
| Phase | Description | Status |
|---|---|---|
| 1 | Audit current exports/imports for legacy helper leakage | completed |
| 2 | Apply API export cleanup edits | completed |
| 3 | Validate with build/test target | completed |
| 4 | Update tracking docs (TODO/changelog/task logs) | completed |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None | - | - |
