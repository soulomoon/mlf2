# Round 219 Implementation Notes

- Promoted `sameLaneOctupleAliasFrameClearBoundaryExpr` onto the explicit
  milestone-3 research surface in
  `test/Research/P5ClearBoundarySpec.hs` by adding the octuple alias packet,
  its fallback-type probe, and the paired authoritative-entrypoint assertion
  immediately after the merged septuple anchor.
- Tightened the existing octuple pipeline packet in
  `test/PipelineSpec.hs` so the row now records it as the next explicit
  milestone-3 representative broader-positive clear-boundary packet after the
  merged septuple anchor, while leaving nonuple alias shells as
  continuity-only evidence.
- Added the exact-edge authoritative-instantiation guard for the octuple
  packet in `test/ElaborationSpec.hs`, including the concrete
  `ExpInstantiate [NodeId 55]` and
  `phiFromEdgeWitnessWithTrace = InstSeq (InstApp (TVar "t56")) (InstApp (TVar "t62"))`
  witness emitted by the merged baseline mechanism.
- Production fallback was not needed. The packet passed as a test-only
  promotion, so `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`, and `src/MLF/Elab/Legacy.hs` remained
  untouched.
- Focused verification stayed green across the promoted packet and preserved
  boundary cluster:
  `sameLaneOctupleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneSeptupleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneSextupleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneQuintupleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneTripleAliasFrameClearBoundaryExpr` (7 examples),
  `sameLaneDoubleAliasFrameClearBoundaryExpr` (6 examples),
  `sameLaneClearBoundaryExpr` (5 examples),
  `sameLaneAliasFrameClearBoundaryExpr` predecessor truth (5 examples),
  selected same-wrapper nested-`forall` (3 examples), and
  nonuple continuity boundary (4 examples).
- Diff/scope checks passed: `git diff --check` was clean; implementation-owned
  edits stayed limited to
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`; no production-slice files changed; and
  nonuple/deeper alias names remained absent from the research and elaboration
  milestone-3 surfaces.
- Full gates passed:
  `./scripts/thesis-conformance-gate.sh` reported
  `[thesis-gate] PASS: thesis conformance anchors are green`, and
  `cabal build all && cabal test` finished with `1362 examples, 0 failures`.
