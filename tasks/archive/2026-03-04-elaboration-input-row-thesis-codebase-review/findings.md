# Findings — 2026-03-04 Elaboration Input Row Review

## Evidence Log
- Thesis requirement verified:
  - `papers/these-finale-english.txt:14087-14097` defines per-edge translation `T(e)` from a chosen propagation witness and allows non-deterministic witness choice.
  - `papers/these-finale-english.txt:14112-14117` states elaboration consumes translatable `χp` and defines translation inductively on term shape.
- Code path verified as chi-native:
  - `src/MLF/Elab/Run/Pipeline.hs:110-141` computes `presolutionViewForGen`, builds `generalizeAtWithBuilderView` over `PresolutionView`, and passes `eePresolutionView` into `elaborateWithEnv`.
  - `src/MLF/Elab/Elaborate.hs:97-119` consumes `eePresolutionView` directly.
  - `src/MLF/Elab/Phi/Translate.hs:247-260` is strict fail-fast on missing trace (`MissingEdgeTrace`).
  - `src/MLF/Elab/Phi/TestOnly.hs:39-75` test-only helpers use chi-native `GeneralizeAtWith` signatures (no `Solved` argument) and no-trace helper remains fail-fast.
- Guard evidence verified:
  - `test/ElaborationSpec.hs:331-356` and `test/PipelineSpec.hs:176-203` assert absence of solved-typed Elaborate/Phi/TestOnly API markers.
  - `test/ElaborationSpec.hs:1901-1928` asserts no-trace helper fails fast with `MissingEdgeTrace`.
- Validation commands run (all PASS):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'` (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'` (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'` (`4 examples, 0 failures`)
  - `cabal build all && cabal test` (`931 examples, 0 failures`, from suite log)
- Conclusion:
  - Under the table’s strict criterion (includes test-only paths), row `Elaboration input` remains thesis-exact (`Yes`).
