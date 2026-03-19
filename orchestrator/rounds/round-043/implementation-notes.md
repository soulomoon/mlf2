# Round 043 Implementation Notes

- Implemented the bounded `F2` local scheme-alias/base-like slice in
  `src/MLF/Elab/Run/ResultType/Fallback.hs` by introducing the explicit
  `rootLocalSchemeAliasBaseLike` proof and threading it through the selected
  `keepTargetFinal` / `targetC` lane only.
- Added exactly one new positive `ARI-C1` example:
  `keeps local scheme-alias/base-like fallback on the local TypeRef lane`.
- Added exactly one matched fail-closed contrast:
  `keeps the same scheme-alias/base-like wrapper fail-closed once it leaves the local TypeRef lane`.
- The new examples treat `boundVarTarget` as absent for this slice.
- `rootHasMultiInst` and `instArgRootMultiBase` remained unchanged and out of
  scope.
- Focused verification:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass (`11 examples, 0 failures`) after the red source-guard failure was
  resolved.
- Full repo gate:
  `cabal build all && cabal test`
  -> pass (`1132 examples, 0 failures`).
- Final verification command details are recorded in the canonical `F2` artifact.
