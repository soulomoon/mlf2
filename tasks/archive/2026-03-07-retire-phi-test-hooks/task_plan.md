# Task Plan: Retire Phi Test Hooks from Library

## Metadata
- Date: 2026-03-07
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Remove `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge` from the main library, move pure witness-domain helpers into the test suite, and keep runtime Omega semantics unchanged.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Audit current Phi test hooks | complete | Reading library/test usage and current guards |
| 2. Move witness helpers into test suite | complete | Added test-local witness-domain helper and renamed unit spec |
| 3. Remove library test-only modules | complete | Deleted modules, exposed Generalize, and localized Omega diagnostics |
| 4. Update specs and source guards | complete | Tests now use production APIs or test-local helpers only |
| 5. Verify and archive | complete | Targeted tests and full gate passed; task archived |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
