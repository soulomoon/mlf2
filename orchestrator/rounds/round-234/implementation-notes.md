### Changes Made
- `src/MLF/Elab/Sigma.hs`: replaced the partial-indexing patch with a single `checkedReorderBinderPair` boundary that drives both `bubbleReorderTo` and `bubbleReorderToFromSpine` without `!!`. Desired-list exhaustion now ends the reorder loop explicitly, and a short source binder list fails closed with `InstantiationError`.
- `test/ElaborationSpec.hs`: tightened the Σ(g) regression block to assert the exact `InstantiationError` message for missing binders, added a spine-helper short-source failure case, and kept the live non-front-binder Φ preservation path covered.

### Tests
- `git diff --check`: passed.
- `cabal build mlf2-test`: passed.
- `cabal test mlf2-test --test-options='--match "Σ(g) quantifier reordering"'`: passed (`13` examples, `0` failures).
- `cabal test mlf2-test --test-options='--match "scheme-aware Φ can target a non-front binder"'`: passed (`1` example, `0` failures).
- `rg -n "!!" src/MLF/Elab/Sigma.hs`: no matches (command exited `1` as expected for an empty result).
- `sed -n '44,62p' src/MLF/Constraint/Types/Witness.hs`: confirmed `ForallSpec` remains list-shaped and `forallSpecBinderCount = length . fsBounds`.
- `cabal build all && cabal test`: passed; `mlf2-test` finished in `360.6769` seconds with `2572` examples and `0` failures.

### Notes
- `ForallSpec` was left unchanged; binder count still derives from `fsBounds`.
- `src/MLF/Elab/Phi/TestSupport.hs` and `orchestrator/state.json` were already dirty in this worktree and were left untouched.
- No `implementation-record.json` convention is present under `orchestrator/rounds/`; this round only updates `implementation-notes.md`.
