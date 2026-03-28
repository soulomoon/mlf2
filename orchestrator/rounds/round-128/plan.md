# Round 128 Plan (`item-1` `P5` Freeze)

## Objective

Freeze the direct predecessor authority chain, the exact `P5` control and
contrast packet, the exact item-2 success bar, and the writable slice for the
next bounded current-architecture `P5` attempt.

This round is `attempt-1` with `retry: null`.

## Inputs

- `orchestrator/rounds/round-128/selection.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- `test/Research/P5ClearBoundarySpec.hs`
- `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`

## Round Type

This is an aggregate docs-only freeze round.
No production or test behavior may change here.

## Task List

1. Freeze the direct predecessor authority chain.
2. Bind the exact `P5` live subject:
   - clear-boundary control: `sameLaneClearBoundaryExpr`
   - quantified-crossing contrast: `nestedForallContrastExpr`
3. Freeze the current carry-forward read:
   - clear-boundary fallback control remains recursive with `containsMu True`
   - nested-`forall` contrast currently fails closed on the fallback surface
     with `containsMu False`
4. Freeze the exact item-2 success bar and writable slice.
5. Record item `2` as the next lawful move.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md && test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md && test -f docs/plans/2026-03-28-post-p1-local-recursive-shape-successor-gate-and-immediate-handoff-decision.md`
- `git diff --check`

## Exit Criteria

This round is complete only when one canonical docs artifact freezes the
authority chain, exact packet, exact success bar, exact writable slice, and
next lawful move for the bounded `P5` lane.
