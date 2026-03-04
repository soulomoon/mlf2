# Findings — 2026-03-04 Elaboration Input Row Review

## Discoveries
- Thesis contract remains unchanged and explicit:
  - Def. 15.3.12 defines edge translation `T(e)` by selecting a propagation witness `I`, with intentional non-determinism ("pick any propagation witness"). (`papers/these-finale-english.txt:14087-14097`)
  - §15.3.6 defines elaboration as translation over translatable presolution `χp` with inductive term-shape translation. (`papers/these-finale-english.txt:14112-14117`)
- Current codebase aligns with chi-native elaboration input:
  - Pipeline builds `presolutionViewForGen`, constructs `generalizeAtWithBuilderView` over that view, and passes `eePresolutionView` into elaboration. (`src/MLF/Elab/Run/Pipeline.hs:110-141`)
  - `elaborateWithEnv` consumes `eePresolutionView` directly. (`src/MLF/Elab/Elaborate.hs:97-119`)
  - `phiFromEdgeWitnessWithTrace` requires `PresolutionView` and fails fast with `MissingEdgeTrace` when trace is absent. (`src/MLF/Elab/Phi/Translate.hs:247-260`)
  - Test-only Φ helper signatures remain chi-native (`GeneralizeAtWith`) and `phiFromEdgeWitnessNoTrace` is explicit fail-fast. (`src/MLF/Elab/Phi/TestOnly.hs:39-75`)
- Guard coverage still asserts absence of solved-typed API markers and fail-fast no-trace behavior:
  - `test/ElaborationSpec.hs:331-356`
  - `test/ElaborationSpec.hs:1901-1928`
  - `test/PipelineSpec.hs:176-203`
- Focused verification command passed:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
  - Result: `2 examples, 0 failures`.
