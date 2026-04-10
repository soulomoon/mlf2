# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-026`

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
   - Confirm stale `rev-022` and `rev-023` remain untouched and unactivated if
     they are present in the repo.
   - Confirm blocked `round-208`, `round-209`, and `round-210` artifacts
     remain immutable predecessor evidence if the round cites them.
   - Confirm accepted `round-211`, merged as `5b775b2`, accepted
     `round-212`, merged as `9bb2229`, accepted `round-213`, merged as
     `2091c39`, accepted `round-214`, merged as `ed66291`, accepted
     `round-215`, merged as `1b62ad5`, accepted `round-216`, merged as
     `21fddba`, accepted `round-217`, merged as `f405079`, accepted
     `round-218`, merged as `7a127e2`, accepted `round-219`, merged as
     `7616109`, and accepted `round-220`, merged as `ea8db76`, are treated as
     the merged predecessor baseline chain rather than as unmerged live-round
     diffs.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
4. **Thesis conformance gate**
   - Run `./scripts/thesis-conformance-gate.sh` when the round changes thesis-
     facing behavior or relies on fresh thesis-conformance evidence.
5. **Broader-positive boundary discipline**
   - Confirm the round stays inside the selected milestone/direction scope and
     does not silently widen into cyclic search, multi-SCC behavior,
     equi-recursive reasoning, fallback rescue, or a second interface.
   - Confirm the retained-child clear-boundary lane remains predecessor truth
     rather than being silently upgraded into whole-frontier closure.
   - Confirm the accepted decuple fail-closed frontier and deeper alias shells
     remain closed unless the active milestone explicitly and honestly
     reclassifies them.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed unless the active milestone
     explicitly and honestly reclassifies them.
6. **Authoritative-entrypoint discipline**
   - When a round claims broader-positive support, confirm the evidence is
     visible on both `runPipelineElab` and `runPipelineElabChecked`.

## Task-Specific Checks

- **milestone-4**
  - Verify the closeout artifact records the enacted behavior and evidence
    surface honestly.
  - Verify the artifact cites the merged `ea8db76` baseline honestly:
    the selected same-wrapper nested-`forall` packet remains successful on
    both authoritative entrypoints; the explicit clear-boundary anchors from
    `sameLaneClearBoundaryExpr` through
    `sameLaneNonupleAliasFrameClearBoundaryExpr` are the current positive
    same-lane frontier; and accepted `round-220` records
    `./scripts/thesis-conformance-gate.sh` plus
    `cabal build all && cabal test` with `1365 examples, 0 failures`.
  - Verify the artifact keeps
    `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only and
    records the accepted decuple fail-closed frontier and deeper alias shells
    as still closed outside the live positive extraction.
  - Verify repo-facing notes and thesis-deviation records were updated only
    when the accepted evidence requires them.
  - Verify no code/test/build files were changed for a docs-only closeout
    round unless a later accepted revision explicitly authorizes more.
  - Verify `cabal build all && cabal test` passed for any code/test-bearing
    round and `./scripts/thesis-conformance-gate.sh` passed when required by
    the active milestone.

## Approval Criteria

Approval requires all applicable baseline and task-specific checks to pass,
the review evidence to match the observed diff and command output, and the
result to stay inside the enacted broader-positive boundary without silently
widening semantics, interfaces, or architecture.
