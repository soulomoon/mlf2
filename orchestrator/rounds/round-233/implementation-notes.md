### Changes Made
- `src/MLF/Elab/Phi/VSpine.hs`: added total binder-spine read helpers that return `PhiInvariantError` on out-of-range access instead of relying on partial `!!` indexing.
- `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`: switched the selected reorder, graft, raise, and binder-name Phi paths to checked `VSpine` access; added `assertSpineSync` before reorder so binder/type mismatches fail explicitly; removed partial binder indexing from the touched production Phi path.
- `src/MLF/Elab/Phi/TestSupport.hs`: introduced a narrow internal test-support seam for binder-spine helpers so regression tests can exercise the checked boundary without exposing `MLF.Elab.Phi.VSpine` directly.
- `mlf2.cabal`: registered `MLF.Elab.Phi.TestSupport` in the private internal library export set for test-only use.
- `test/ElaborationSpec.hs`: added binder-spine regression coverage for mismatch detection, out-of-range failure, and valid binder read preservation.

### Tests
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "lookupBinderIndex"`: verified focused WitnessDomain binder-index lookup behavior.
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "graft-weaken"`: verified focused graft/weaken normalization and translation coverage adjacent to the touched Phi path.
- `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "scheme-aware Φ can translate Raise (raise a binder to the front)"`: verified the direct Raise translation path still succeeds.
- `cabal test mlf2-test --test-options='--match "binder-spine safety"'`: verified the new binder-spine mismatch/out-of-range/preservation regression block.
- `git diff --check`: passed after the scoped edits.
- `cabal build mlf2-test`: passed.
- `cabal build all`: passed.
- `cabal test`: passed (`2570` examples, `0` failures).

### Notes
`ForallSpec` remains list-shaped and binder count still derives from `fsBounds`; this round did not widen public API or touch milestone-5 witness-constructor work. `orchestrator/state.json` was left untouched because it is controller-owned.
