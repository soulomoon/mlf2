# Task Plan: Guard-First Surface Expr Fold Refactor

## Metadata
- Date: 2026-03-08
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro, recursion-schemes-refactor

## Goal
Add direct row1 desugaring guards, then refactor `MLF.Frontend.Desugar.desugarSurface` to use recursion-schemes over `Expr 'Surface ty` only, while preserving exact thesis-aligned preprocessing semantics.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Create task files and inspect frontend surface | complete | Confirmed row1 gaps and current desugar shape |
| 2. Add direct desugaring guards | complete | Added `FrontendDesugarSpec` and wired it into the test suite |
| 3. Add surface Expr recursion-schemes support | complete | Added manual base functor and recursion-schemes instances for `Expr 'Surface ty` |
| 4. Refactor desugarSurface | complete | Replaced explicit recursion with a local `cata` algebra |
| 5. Verify, sync docs, archive | complete | Targeted row1 slices and full gate passed |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None yet | 0 | N/A |
