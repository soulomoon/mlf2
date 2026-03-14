# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only research rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
  Why: the successor roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `cabal build all && cabal test`
  Why: this repo’s full gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against inherited evidence
  Why: each round must record whether it preserved the completed automatic-recursive-inference rounds (`orchestrator/rounds/round-001` through `round-005`), the approved successor design spec, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- gap-map/doc consistency against `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`;
- candidate-selection checks proving exactly one bounded subset stays active and alternatives are explicitly deferred or rejected;
- invariant checks proving acyclicity, binding, occurs-check/termination, reconstruction/reification/witness replay, and principality obligations remain explicit when item 3 is active;
- feasibility-decision checks proving item 4 records explicit `feasible-continue` or `not-yet-go` evidence without forbidden widening;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or task artifacts.

## Approval Criteria

- Every baseline check passes.
- Every task-specific check passes.
- `review.md` records evidence for the round.
- The reviewer decision is explicit.
- The round preserves continuity with inherited evidence unless the plan explicitly authorized a human-facing summary update without rewriting old authoritative records.

## Reviewer Record Format

### Round `<round-id>`

- Baseline checks:
- Task-specific checks:
- Decision:
- Evidence:
