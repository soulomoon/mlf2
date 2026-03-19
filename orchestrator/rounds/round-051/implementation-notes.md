# Round 051 Implementation Notes

- Implemented only the bounded `H2` local-binding `instArgRootMultiBase`
  `keepTargetFinal` / `targetC` slice for repaired `URI-R2-C1`.
- `src/MLF/Elab/Run/ResultType/Fallback.hs` now names the selected local proof
  `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`
  and routes the local `keepTargetFinal` / `targetC -> rootFinal` path through
  that proof without relaxing the existing fail-closed `baseTarget` guards.
- `test/PipelineSpec.hs` adds one same-lane local multi-base success example,
  one matched non-local fail-closed contrast, and refreshes the source guard so
  the selected authority is explicit.
- TDD evidence:
  - focused red:
    `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
    -> fail (`15 examples, 1 failure`, missing explicit local proof/source guard)
  - focused green:
    same command -> pass (`15 examples, 0 failures`)
- Verification:
  - baseline contract checks -> pass
  - H2 anchor grep -> pass
  - `cabal build all && cabal test` -> pass (`1136 examples, 0 failures`)
