# Round 217 Implementation Notes

- Promoted `sameLaneSextupleAliasFrameClearBoundaryExpr` onto the explicit
  milestone-3 research surface in
  `test/Research/P5ClearBoundarySpec.hs` by adding the sextuple alias packet,
  its fallback-type probe, and the paired authoritative-entrypoint assertion
  immediately after the merged quintuple anchor.
- Tightened the existing sextuple pipeline packet in
  `test/PipelineSpec.hs` so the row now records it as the next explicit
  milestone-3 representative broader-positive clear-boundary packet after the
  merged quintuple anchor, while leaving septuple/deeper alias shells as
  continuity-only evidence.
- Added the exact-edge authoritative-instantiation guard for the sextuple
  packet in `test/ElaborationSpec.hs`, including the concrete
  `ExpInstantiate [NodeId 49]` and
  `phiFromEdgeWitnessWithTrace = InstSeq (InstApp (TVar "t50")) (InstApp (TVar "t56"))`
  witness expected from the merged baseline mechanism.
- Production fallback was not needed. The packet passed as a test-only
  promotion, so `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`, and `src/MLF/Elab/Legacy.hs` remained
  untouched.
- Focused verification stayed green across the promoted packet and preserved
  boundary cluster:
  `sameLaneSextupleAliasFrameClearBoundaryExpr` (7 examples),
  quintuple/quadruple/triple/double/clear boundary anchors (7/7/7/6/5
  examples),
  predecessor alias-frame truth (5 examples),
  selected same-wrapper nested-`forall` packet (3 examples),
  septuple continuity boundary (4 examples),
  checked-authoritative parity (4 examples),
  `BUG-2026-02-06-002` (10 examples),
  `BUG-2026-02-17-002` (1 example),
  correct-semantic `g g` failure (1 example),
  representative let-polymorphism cluster (4 examples),
  nested-let fail-fast guards (1 and 1 examples),
  `Phi alignment` (7 examples),
  `Thesis alignment invariants` (21 examples),
  `Frozen parity artifact baseline` (1 example), and
  the fail-closed shell probes (4 examples).
- Diff/scope checks passed against merge-base `21fddba470a78569c20ffee7fb98ff5011053ff5`:
  implementation-owned edits stayed limited to
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/ElaborationSpec.hs`;
  `git diff --check` was clean; septuple/octuple/nonuple names remained absent
  from the research and elaboration milestone-3 surfaces.
- Full gates passed:
  `./scripts/thesis-conformance-gate.sh` reported
  `[thesis-gate] PASS: thesis conformance anchors are green`, and
  `cabal build all && cabal test` finished with `1356 examples, 0 failures`.
