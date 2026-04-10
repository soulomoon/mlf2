# Round 216 Implementation Notes

## Change Summary

- Promoted `sameLaneQuintupleAliasFrameClearBoundaryExpr` to the next explicit
  milestone-3 representative anchor on the research surface in
  `test/Research/P5ClearBoundarySpec.hs` by adding the quintuple-alias packet,
  its fallback `containsMu` probe, and the matching
  `runPipelineElab` / `runPipelineElabChecked` assertion while preserving the
  merged first four anchors, alias-frame predecessor truth, and the selected
  same-wrapper nested-`forall` packet ordering.
- Tightened the existing quintuple-alias pipeline row in
  `test/PipelineSpec.hs` so it now names the live milestone-3 promotion after
  the merged quadruple-alias anchor without widening the sextuple/deeper
  alias-shell continuity rows or the bounded helper guard.
- Added the exact-edge authoritative-instantiation guard for the quintuple
  packet in `test/ElaborationSpec.hs`, immediately after the quadruple-alias
  guard, locking the observed authoritative translation to
  `ExpInstantiate [NodeId 46]` and
  `Elab.InstSeq (Elab.InstApp (Elab.TVar "t47")) (Elab.InstApp (Elab.TVar "t53"))`.

## Scope / Boundary Notes

- The round stayed inside the approved writable slice and remained test-only.
- No production fallback was needed in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`, or
  `src/MLF/Elab/Legacy.hs`.
- Sextuple/deeper alias shells remain continuity-only evidence and stayed
  absent from the research and elaboration milestone-3 surfaces.

## Touched Files

- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`
- `test/ElaborationSpec.hs`

## Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  -> `6 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'`
  -> `5 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'`
  -> `3 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`
  -> `4 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
  -> `4 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'`
  -> `4 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'`
  -> `10 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'`
  -> `4 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'`
  -> `1 example, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment"'`
  -> `7 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Thesis alignment invariants"'`
  -> `21 examples, 0 failures`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
  -> `1 example, 0 failures`
- `rg -n 'sameLaneSextupleAliasFrameClearBoundaryExpr|sameLaneSeptupleAliasFrameClearBoundaryExpr|sameLaneOctupleAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr' test/Research/P5ClearBoundarySpec.hs test/ElaborationSpec.hs test/PipelineSpec.hs`
  -> only `test/PipelineSpec.hs` contains sextuple/deeper alias-shell names
- `git -C orchestrator/worktrees/round-216 diff --name-only 1b62ad599fab59824bed738afc69822d106ae5cc -- test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Legacy.hs`
  -> diff limited to the three test files
- `git -C orchestrator/worktrees/round-216 diff --name-only 1b62ad599fab59824bed738afc69822d106ae5cc -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  -> no output
- `git -C orchestrator/worktrees/round-216 diff --check 1b62ad599fab59824bed738afc69822d106ae5cc --`
  -> clean
- `./scripts/thesis-conformance-gate.sh`
  -> `[thesis-gate] PASS: thesis conformance anchors are green`
- `cabal build all && cabal test`
  -> `1353 examples, 0 failures`
