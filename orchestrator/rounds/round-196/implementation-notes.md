# Round 196 Implementation Notes

- Scope result: the current workspace already carried the selected bounded
  retained-child guard-cluster behavior on the authoritative pipeline
  entrypoints, so this round promoted that lane into the frozen `P5`
  harnesses instead of widening production code.
- Test changes:
  `test/Research/P5ClearBoundarySpec.hs` now names
  `sameLaneAliasFrameClearBoundaryExpr`, adds the selected alias-frame packet
  to the `P5 clear-boundary retained-child probes`, and uses a packet-specific
  `extractFirstApp` / `sameLaneAliasFrameClearBoundaryFallbackType` helper so
  the fallback probe stays on the same retained-child seam as the existing
  control.
  `test/PipelineSpec.hs` adds a focused source guard that keeps the selected
  lane tied to `boundHasForallFrom`,
  `preserveRetainedChildAliasBoundary`, and
  `preserveRetainedChildAuthoritativeResult`.
- Focused RED:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
  failed before the helper refinement because the newly added alias-frame
  fallback assertion called `fallbackType sameLaneAliasFrameClearBoundaryExpr`
  directly and observed `containsMu False`.
- Focused GREEN after the bounded test-harness fix:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'`
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "reports PhiTranslatabilityError at pipeline entrypoints as a downstream consequence of correct non-recursive nested-forall outcome"'`
- Final verification:
  `git diff --check`
  `cabal build all && cabal test`
  Both passed; the full gate finished with `1338 examples, 0 failures`.
