# Round 132 Plan (`item-1` Post-`P5` Repo-Scope Freeze)

## Objective

Freeze exactly which accepted authority survives after the settled exact `P5`
lane and separate live repo-scope authority from historical evidence and
non-authoritative planning context.

This round is `attempt-1` with `retry: null`.

## Inputs

- `orchestrator/rounds/round-132/selection.md`
- `docs/plans/2026-03-28-post-p5-polymorphism-nested-forall-successor-gate-and-immediate-handoff-decision.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-28-post-c1-p2-successor-gate-and-immediate-handoff-decision.md`
- `docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- the active roadmap bundle and verification contract

## Round Type

This is an aggregate docs-only round.
No production or test behavior may change here.

## Task List

1. Freeze the live control-plane authority.
   - Record the active roadmap bundle as the live controller surface.
   - Distinguish live successor authority from historical aggregate evidence.

2. Freeze the authoritative predecessor chain.
   - Carry forward the baseline contract, capability contract, full-pipeline
     visibility contract, the March 27 repo-scope refresh as historical
     aggregate evidence, and the accepted settled same-lane, exact `C1`,
     exact `P1`, and exact `P5` chains as settled predecessor truth only.

3. Freeze the non-widening boundary.
   - Keep settled predecessor pockets closed.
   - Keep explicit-only, iso-recursive, non-equi-recursive, non-cyclic,
     no-fallback, and one-interface-only boundaries unchanged.

4. Record the next lawful move.
   - Make item `2` the next lawful move.
   - Keep this round docs-only and explicitly skip the full Cabal gate.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"controller_stage":|"max_parallel_rounds":|"active_rounds":|"pending_merge_rounds":|"retry": null|"retry": \{' orchestrator/state.json`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md && test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md && test -f docs/plans/2026-03-28-post-p5-polymorphism-nested-forall-successor-gate-and-immediate-handoff-decision.md`
- `git diff --check`

## Exit Criteria

This round is complete only when:

- one canonical docs artifact freezes the post-`P5` repo-scope successor
  authority, predecessor chain, and non-widening boundary;
- the artifact keeps settled predecessor packets closed as predecessor truth
  only;
- the round remains docs-only; and
- the round is ready for aggregate review on item `1`.
