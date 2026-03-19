# Round 047 Implementation Notes

- Implemented the bounded `G2` local multi-inst slice in
  `src/MLF/Elab/Run/ResultType/Fallback.hs` by introducing the explicit
  `rootLocalMultiInst` proof and threading it through the selected
  `keepTargetFinal` / `targetC` lane only.
- Added exactly one new positive `ARI-C1` example:
  `keeps local multi-inst fallback on the local TypeRef lane`.
- Added exactly one matched fail-closed contrast:
  `keeps the same multi-inst wrapper fail-closed once it leaves the local TypeRef lane`.
- The selected slice leaves `instArgRootMultiBase`, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening, and non-local widening
  out of scope.
- Focused verification:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass (`13 examples, 0 failures`) after the red local multi-inst/source-guard
  failure was resolved.
- Full repo gate:
  `cabal build all && cabal test`
  -> pass.
- Final verification command details are recorded in the canonical `G2` artifact.
