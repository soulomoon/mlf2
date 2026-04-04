# Round 188 Implementation Notes

## Result

Implemented the exact `sameLaneSeptupleAliasFrameClearBoundaryExpr` packet in
the round worktree only.

- Added the selected septuple packet and authoritative-output assertions to
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
- Added the matching authoritative-entrypoint regression and a selected-packet
  `TermClosure` mechanism guard to `test/PipelineSpec.hs`.
- Narrowed the predecessor sextuple mechanism guard back to the shared seam so
  the new helper-budget ownership lives only on the selected septuple packet.
- Applied the only authorized production move in
  `src/MLF/Elab/TermClosure.hs`: raised
  `hasRetainedChildClearBoundaryWithAliasBudget source term` from `2` to `3`
  while keeping the outer `hasRetainedChildAliasBoundary v body 2 =` seam
  unchanged.

## Evidence

- Baseline before the production edit:
  sextuple still succeeded on `runPipelineElab` and `runPipelineElabChecked`,
  while the selected septuple packet and a fresh octuple control both failed
  with `PipelineTypeCheckError (TCLetTypeMismatch ...)`.
- Focused red phase after adding the selected tests:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'`
  failed only on the selected packet and the new `source term 3` guard.
- Post-fix focused replay:
  the same selected match passed.
- Post-fix REPL replay:
  sextuple and septuple returned `Right ...` on both authoritative entrypoints,
  while the fresh octuple control still returned the same fail-closed
  `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))`.
