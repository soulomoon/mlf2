# Round 130 Plan (`item-3` `P5` Settlement Surface)

## Objective

Publish the accepted post-item-2 read for the exact frozen `P5` packet
`nestedForallContrastExpr` as one authoritative settlement surface and exact
repo-impact read.

This round is `attempt-1` with `retry: null`.

## Inputs

- `orchestrator/rounds/round-130/selection.md`
- `docs/plans/2026-03-28-p5-polymorphism-nested-forall-successor-authority-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-129/implementation-notes.md`
- `orchestrator/rounds/round-129/review.md`
- `orchestrator/rounds/round-129/review-record.json`
- `test/Research/P5ClearBoundarySpec.hs`
- the active roadmap bundle and verification contract

## Round Type

This is an aggregate docs-only round.
No production or test behavior may change here.

## Task List

1. Republish the exact post-item-2 read for the frozen packet only.
   - Record that the exact quantified-crossing packet
     `nestedForallContrastExpr` remains non-recursive on the internal fallback
     route.
   - Record that both authoritative entrypoints
     `runPipelineElab` and `runPipelineElabChecked` fail for that exact packet
     with the same Phase 6 `PhiTranslatabilityError`.
   - Record that the accepted round-129 route audit found no lawful recursive
     carrier for that exact packet inside the frozen writable slice.

2. Validate provenance without widening the claim.
   - Bind the settlement surface to the accepted item-1 freeze, round-129
     implementation notes, round-129 review, round-129 review record, and the
     strengthened exact-packet regression in
     `test/Research/P5ClearBoundarySpec.hs`.
   - Record the accepted focused rerun and accepted serialized full-gate
     result from round-129 as predecessor evidence only.

3. State the exact repo-impact read.
   - Keep the inherited architecture unchanged.
   - State whether the exact packet is now recursively visible or remains
     below positive `P5` success on the current authoritative surfaces.
   - Do not silently upgrade one exact-packet fail-closed result into general
     `P5` family settlement or repo-level readiness.

4. Record the docs-only verification posture.
   - Run docs/controller hygiene checks only.
   - Explicitly skip the full Cabal gate for this round because no code or
     test files are changed here.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md && test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md && test -f docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- `git diff --check`

## Exit Criteria

This round is complete only when:

- one canonical docs artifact republishes the exact post-item-2 `P5` read;
- provenance is bound to the accepted round-129 evidence chain;
- the exact repo-impact read is stated without widening;
- the round remains docs-only; and
- the round is ready for an aggregate review decision on item `3`.
