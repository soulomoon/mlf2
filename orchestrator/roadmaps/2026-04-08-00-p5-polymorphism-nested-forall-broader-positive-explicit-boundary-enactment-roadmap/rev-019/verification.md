# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-019`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap lineage, pointer, and preserved-history consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle.
   - Confirm `selection.md` records matching
     `roadmap_id`, `roadmap_revision`, `roadmap_dir`,
     `milestone_id`, `direction_id`, and `extracted_item_id`.
   - Confirm final `review-record.json` records the same lineage fields.
   - Confirm `roadmap_item_id` is absent unless a compatibility mirror is
     explicitly required for a legacy reader.
   - Confirm the parent-workspace pointer stubs
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     match the active `roadmap_id`, `roadmap_revision`, and `roadmap_dir`.
   - Confirm the canonical round-worktree pointer stubs
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     also match the active `roadmap_id`, `roadmap_revision`, and
     `roadmap_dir`.
   - Confirm prior roadmap families and revisions remain unchanged.
   - Confirm blocked `round-208`, `round-209`, and `round-210` artifacts remain
     immutable predecessor evidence if the round cites them.
   - Confirm accepted `round-211`, merged as `5b775b2`, accepted
     `round-212`, merged as `9bb2229`, accepted `round-213`, merged as
     `2091c39`, accepted `round-214`, merged as `ed66291`, and accepted
     `round-215`, merged as `1b62ad5`, are treated as the merged
     predecessor baseline chain rather than as unmerged live-round diffs.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
4. **Thesis conformance gate**
   - Run `./scripts/thesis-conformance-gate.sh`.
5. **Broader-positive boundary discipline**
   - Confirm the round stays inside the selected milestone/direction scope and
     does not silently widen into cyclic search, multi-SCC behavior,
     equi-recursive reasoning, fallback rescue, or a second interface.
   - Confirm the retained-child clear-boundary lane remains predecessor truth
     rather than being silently upgraded into whole-frontier closure.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed unless the active milestone
     explicitly and honestly reclassifies them.
6. **Authoritative-entrypoint discipline**
   - When a round claims broader-positive support, confirm the evidence is
     visible on both `runPipelineElab` and `runPipelineElabChecked`.

## Task-Specific Checks

- **milestone-3**
  - Verify the diff stays inside the preserved writable slice:
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `src/MLF/Elab/Elaborate/Algebra.hs`,
    `test/ElaborationSpec.hs`,
    `test/PipelineSpec.hs`, and
    `test/Research/P5ClearBoundarySpec.hs`.
  - Verify the merged `1b62ad5` baseline wins remain green:
    `sameLaneClearBoundaryExpr` as the first explicit milestone-3
    clear-boundary anchor,
    `sameLaneDoubleAliasFrameClearBoundaryExpr` as the next explicit
    milestone-3 clear-boundary anchor,
    `sameLaneTripleAliasFrameClearBoundaryExpr` as the next explicit
    milestone-3 clear-boundary anchor after that,
    `sameLaneQuadrupleAliasFrameClearBoundaryExpr` as the next explicit
    milestone-3 clear-boundary anchor after that,
    the selected same-wrapper nested-`forall` packet on both authoritative
    entrypoints,
    `sameLaneAliasFrameClearBoundaryExpr` as preserved predecessor truth on
    both authoritative entrypoints,
    checked-authoritative parity,
    `BUG-2026-02-06-002`,
    the retained-child exact packet,
    `BUG-2026-02-17-002`,
    the correct semantic `g g` failure,
    the A6 / nested-let / representative let-polymorphism cluster,
    `./scripts/thesis-conformance-gate.sh`, and
    `cabal build all && cabal test`.
  - Verify the round broadens the representative broader-positive corpus
    honestly beyond the merged first four clear-boundary anchors by making
    `sameLaneQuintupleAliasFrameClearBoundaryExpr` explicit on
    `test/Research/P5ClearBoundarySpec.hs`, the matching
    `test/PipelineSpec.hs` rows, and an exact-edge authoritative-instantiation
    guard in `test/ElaborationSpec.hs`.
  - Verify the round keeps `sameLaneAliasFrameClearBoundaryExpr` as preserved
    predecessor truth, keeps
    `sameLaneDoubleAliasFrameClearBoundaryExpr` as the merged next anchor,
    keeps `sameLaneTripleAliasFrameClearBoundaryExpr` as the merged next
    anchor after that, keeps
    `sameLaneQuadrupleAliasFrameClearBoundaryExpr` as the merged next anchor
    after that, and does not widen the live extraction into sextuple-/deeper-
    alias shells.
  - Verify the current quantified fail-closed contrasts stay honest unless the
    round explicitly and successfully reclassifies them under the active
    milestone.
  - Verify direct success remains real on both authoritative entrypoints and
    on the authoritative-instantiation guard, and does not depend on
    helper-only output or compatibility shims.
  - Verify
    `src/MLF/Elab/TermClosure.hs`,
    `src/MLF/Elab/Run/Pipeline.hs`,
    `src/MLF/Elab/Pipeline.hs`,
    `src-public/MLF/Pipeline.hs`,
    `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
    `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
    remain untouched unless a later accepted roadmap revision explicitly
    authorizes more.
  - Verify `cabal build all && cabal test` passed.
  - Verify `./scripts/thesis-conformance-gate.sh` passed.
- **milestone-4**
  - Verify the closeout artifact records the enacted behavior and evidence
    surface honestly.
  - Verify repo-facing notes and thesis-deviation records were updated only
    when the accepted evidence requires them.
  - Verify `cabal build all && cabal test` passed for any code/test-bearing
    round and `./scripts/thesis-conformance-gate.sh` passed when required by
    the active milestone.

## Approval Criteria

Approval requires all applicable baseline and task-specific checks to pass,
the review evidence to match the observed diff and command output, and the
result to stay inside the enacted broader-positive boundary without silently
widening semantics, interfaces, or architecture.
