# Task Plan: Shared frontend/XMLF parser scaffolding

## Metadata
- Date: 2026-03-08
- Execution mode: local implementation
- Skills in use: planning-with-files, test-driven-development, haskell-pro

## Goal
Commonize shared lexer/type-parser scaffolding between `MLF.Frontend.Parse` and `MLF.XMLF.Parse` without forcing their full term/computation grammars into one parser.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Inspect parser duplication and target home | complete | Reviewed both parser modules and current tests |
| 2. Add focused parser dedup guards | complete | Added parser scaffolding source guard |
| 3. Extract shared parser scaffolding | complete | Added `MLF.Parse.Common` and `MLF.Parse.Type` |
| 4. Migrate frontend and XMLF parsers | complete | Rewired both parsers while preserving grammar-specific entrypoints |
| 5. Run targeted and full verification | complete | Targeted parser slices and full gate passed |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Generic shared forall parser greedily consumed XMLF body binder as another binder | 1 | Added `tpcForallBinders` hook so XMLF keeps its distinct binder-list stopping rule |
