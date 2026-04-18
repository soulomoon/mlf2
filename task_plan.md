# Task Plan

## Summary
Goal: finish the remaining `.mlfp` ProgramSpec regression repairs while
preserving the exact old eMLF surface route. No new surface forms, no direct
`.mlfp -> ElabTerm` fallback, no permissive `EUnroll`, and no broad
`TypeCheck` weakening.

## Phases
1. Confirm current baseline and failure clusters. - completed
2. Fix Phase 7 binder hygiene for `TCTypeAbsVarInScope "b"`. - completed
3. Fix remaining recursive-ADT Phase 6 regressions locally. - completed
4. Fix remaining CLI/helper and module-ordering regressions. - completed
5. Run serial focused and full Cabal validation. - completed

## Errors Encountered
| Error | Attempt | Resolution |
|------|---------|------------|
| `recursive-list-tail` fails with `TCTypeAbsVarInScope "b"` | 1 | Target producer-side freshening in `MLF.Elab.Run.Pipeline` first |
| `recursive-existential` fails with `GenSchemeFreeVars` | 1 | Strip vacuous constructor foralls after recursive ADT lowering so the handler scheme no longer owns an outer Church result variable |
| public `authoritative-case-analysis` regressed to a Church result mismatch | 1 | Keep canonical constructor result naming only for non-existential constructor groups and retain explicit result names for existential groups |
| fail-fast reaches `\y. let id = (\x. x) in id y` with `forall a1. forall b. a1 -> a1` | 1 | Prune vacuous leading type abstractions after final closure freshening; targeted `let id` slice is green |
| fail-fast reaches stale xMLF goldens for `church-true` and `choose` | 1 | Rebaseline the golden files to the non-vacuous leading-forall output produced by the current closure cleanup |
