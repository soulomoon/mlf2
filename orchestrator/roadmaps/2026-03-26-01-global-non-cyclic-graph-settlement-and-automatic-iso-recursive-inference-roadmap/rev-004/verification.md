# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  Why: the live controller must preserve the v2 retry schema and the
  revisioned-roadmap locator contract.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle
  through `orchestrator/state.json`.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  Why: the active roadmap bundle must keep a parseable ordered item list with
  explicit status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited explicit-only / iso-recursive / non-equi-recursive /
  no-fallback baseline remains the live boundary.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-amendment-handoff-decision.md`
  Why: accepted rev-003 item `4` is the direct authority for the rev-004
  successor lane.
- Command: `test -f orchestrator/rounds/round-112/review-record.json`
  Why: the accepted round-112 record is required continuity evidence for the
  rev-004 successor boundary.

## Task-Specific Checks

- post-amendment-freeze checks proving item `1` narrows only to the same
  exact `C2` / `C5` / `C7` pocket, the same exact packet and tuple, one
  exact current-result surface anchored in accepted rev-003 item `3`, one
  exact future writable docs boundary, and explicit predecessor-artifact
  immutability;
- bounded-docs-settlement checks proving item `2` creates only new bounded
  post-amendment settlement artifacts, stays exact-pocket-only, and does not
  silently rewrite accepted predecessor artifacts or widen into repo-level
  success claims;
- same-pocket-validation checks proving item `3` validates the new rev-004
  settlement surfaces against the accepted rev-003 item-3 result while
  preserving predecessor immutability and exact-pocket-only language;
- post-settlement-decision checks proving item `4` records exactly one
  follow-on outcome for rev-004 and does not by implication reopen blocked
  work or broad rollout;
- retry-subloop checks proving retry-capable reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for aggregate-only items `1`
  and `4`;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`;
- roadmap-update checks proving accepted roadmap edits preserve completed-item
  truth, keep the next unfinished item concrete, and do not silently widen
  the exact-pocket post-amendment settlement lane; and
- docs-diff review when a round intentionally changes only `orchestrator/`
  and `docs/`, plus an explicit skip note when Cabal gates are out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the exact-pocket post-amendment settlement boundary,
  inherited keep axes, predecessor immutability, and stage sequencing unless
  an accepted roadmap update explicitly changes the live plan.
- No unresolved blocking issue remains.
