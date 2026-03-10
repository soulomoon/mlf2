# Warning-Free Closeout Task Plan

## Goal
Remove all current repository warning sites surfaced by the latest `cabal build all && cabal test` run, keeping behavior unchanged and restoring a warning-free build for the current live tree.

## Baseline
- Timestamp (UTC): 2026-03-10T10:01:39Z
- Baseline commit: `19dcea45f0efac3a38f40a1e31710c6ecb1fd115`
- Base branch: `master`
- Current branch: `master`

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Initialize task artifacts | complete | Created focused warning-cleanup task files |
| 2. Reproduce warnings with fresh evidence | complete | Rebuild surfaced redundant-import, name-shadowing, missing-field, incomplete-pattern, and unused-binding/import warnings |
| 3. Fix warning sites | complete | Patched the exact warning sites without changing semantics |
| 4. Verify warning-free build | complete | `cabal build all --ghc-options=-fforce-recomp` produced zero warnings and `cabal test` passed |
| 5. Sync docs and close task | complete | Synced task/docs and prepared the completed task for archive |

## Decisions
- Treat the existing warning output as a lead, but re-run a fresh build before fixing anything.
- Keep fixes minimal and semantics-preserving.
- Prefer owner-local initialization/helpers over suppressing warnings.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Initial warning capture via a live `tee` session did not preserve the full build log | 1 | Re-ran verification with a redirected forced-rebuild log and used the fresh merge-verification warnings as the source list |
