# Task Plan: Thesis-Exact Phi Identity Cleanup

## Metadata
- Date: 2026-03-07
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Remove the stale compiled `MLF.Elab.Phi.Binder` reconciliation surface, keep runtime `Omega` direct/fail-fast, and align code/tests/docs with the accepted thesis-exact identity model.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inspect Phi surfaces and task setup | complete | Reading live modules/tests and initializing task files |
| 2. Add failing guard tests | complete | Added source guard and direct-target regressions |
| 3. Remove Binder and tighten docs | complete | Removed Binder, tightened Phi/IdentityBridge/Omega notes, and synced trackers |
| 4. Targeted verification | complete | Focused Phi slices all passed |
| 5. Full verification and archive | complete | Full gate passed; task ready to archive |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
