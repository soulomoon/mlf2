# Round 215 Implementation Notes

## Change Summary

- Promoted `sameLaneQuadrupleAliasFrameClearBoundaryExpr` to the next explicit
  milestone-3 representative anchor on the research surface in
  `test/Research/P5ClearBoundarySpec.hs` by adding the quadruple-alias packet,
  its fallback `containsMu` probe, and the matching
  `runPipelineElab` / `runPipelineElabChecked` assertion while preserving the
  first three merged anchors, alias-frame predecessor truth, and the selected
  same-wrapper nested-`forall` packet ordering.
- Tightened the existing quadruple-alias pipeline row in
  `test/PipelineSpec.hs` so it now names the live milestone-3 promotion after
  the merged triple-alias anchor without widening the quintuple/deeper alias
  continuity rows or the depth-3 boundary guard.
- Added the exact-edge authoritative-instantiation guard for the quadruple
  packet in `test/ElaborationSpec.hs`, immediately after the triple-alias
  guard, locking the observed authoritative translation to
  `ExpInstantiate [NodeId 43]` and
  `Elab.InstSeq (Elab.InstApp (Elab.TVar "t44")) (Elab.InstApp (Elab.TVar "t50"))`.

## Scope / Boundary Notes

- The round stayed inside the approved writable slice and remained test-only.
- No production edits were needed in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`, or
  `src/MLF/Elab/Legacy.hs`.
- Quintuple/deeper alias shells remain outside this extraction and continue to
  serve continuity-only / boundary evidence.

## Touched Files

- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`
- `test/ElaborationSpec.hs`

## Verification

- `git diff --check`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  -> `6 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'`
  -> `5 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
  -> `5 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'`
  -> `3 examples, 0 failures`
- `cabal build all && cabal test`
  -> `1350 examples, 0 failures`
- `./scripts/thesis-conformance-gate.sh`
  -> `PASS: thesis conformance anchors are green`
