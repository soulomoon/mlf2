# Round 220 Implementation Notes

- Promoted `sameLaneNonupleAliasFrameClearBoundaryExpr` onto the explicit
  milestone-3 research surface in
  `test/Research/P5ClearBoundarySpec.hs` by adding the nonuple alias packet,
  its fallback-type probe, and the paired authoritative-entrypoint assertion
  immediately after the merged octuple anchor.
- Tightened the existing nonuple pipeline packet in
  `test/PipelineSpec.hs` so the row now records it as the next explicit
  milestone-3 representative broader-positive clear-boundary packet after the
  merged octuple anchor, while leaving deeper alias shells as
  continuity-only evidence.
- Added the exact-edge authoritative-instantiation guard for the nonuple
  packet in `test/ElaborationSpec.hs`, including the concrete
  `ExpInstantiate [NodeId 58]` and
  `phiFromEdgeWitnessWithTrace = InstSeq (InstApp (TVar "t59")) (InstApp (TVar "t65"))`
  witness emitted by the merged baseline mechanism.
- Production fallback was not needed. The packet passed as a test-only
  promotion, so `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`, and `src/MLF/Elab/Legacy.hs` remained
  untouched.
- Focused verification stayed green on the promoted packet:
  `sameLaneNonupleAliasFrameClearBoundaryExpr` matched `7 examples, 0 failures`
  across the research, pipeline, representative-gap, and elaboration surfaces.
- Diff/scope checks passed: `git diff --check` was clean; implementation-owned
  edits stayed limited to
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`; no production-slice files changed; and deeper
  alias-shell names remained absent from the research and elaboration
  milestone-3 surfaces.
