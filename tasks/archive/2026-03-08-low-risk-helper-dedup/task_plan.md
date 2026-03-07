# Task Plan: Low-Risk Helper Dedup

## Metadata
- Date: 2026-03-08
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Deduplicate the remaining low-risk pure helper pairs by extracting `freshNameLike` into `MLF.Util.Names` and `mapBound` into `MLF.Elab.Types`, while preserving behavior and adding focused guards against re-duplication.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inspect duplicate helpers and target homes | complete | Confirmed helper pairs and low-risk extraction targets |
| 2. Add focused dedup guards | complete | Added focused source guards for both helper homes |
| 3. Extract shared helper functions | complete | Rewired all four call sites to the shared helpers |
| 4. Run targeted verification | complete | Focused guard and behavior slices passed |
| 5. Full gate and tracker sync | complete | Full gate passed; docs synced; task archived |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
