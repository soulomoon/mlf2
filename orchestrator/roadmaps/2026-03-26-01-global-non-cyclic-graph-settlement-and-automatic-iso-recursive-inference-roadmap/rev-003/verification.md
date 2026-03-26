# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only lane-freeze rounds and bounded
  implementation-validation rounds.
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
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  Why: the accepted capability contract still defines the representative
  family matrix and positive / negative evidence bar for this family.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  Why: the accepted audit still records the inherited architecture
  classification set and preserves `non-cyclic-graph` reopening as bounded
  predecessor evidence rather than settled broad capability.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: this contract still defines the reviewable solver / elaboration /
  reconstruction / output-surface evidence bar.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  Why: the accepted representative campaign remains predecessor evidence for
  bounded subset feasibility and unresolved settlement debt.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Why: the accepted strategic decision remains predecessor context for why
  the repo stayed inside the inherited architecture before the reopened
  non-cyclic-graph lane.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  Why: accepted `round-103` remains the authoritative reopen decision that
  later same-family revisions must preserve.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
  Why: accepted rev-002 item `1` still freezes the one candidate boundary
  that later same-family revisions may only narrow.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
  Why: accepted rev-002 item `2` still freezes the one exact live pocket.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md`
  Why: accepted rev-002 item `3` still freezes the lawful bar vocabulary and
  fail-closed boundary for that exact pocket.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md`
  Why: accepted rev-002 item `4` still freezes the exact rows, packet,
  commands, module set, and six review-visible surfaces that rev-003 must
  stay within.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-amendment-lane-open-or-stop-decision.md`
  Why: accepted rev-002 item `5` is the authoritative lane-open decision
  that lawfully created this successor revision.
- Command: `test -f orchestrator/rounds/round-108/review-record.json`
  Why: the accepted round-108 record is required continuity evidence for the
  rev-003 successor boundary.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item,
  especially:
- lane-freeze checks proving item `1` narrows from accepted round-108 only
  to the same exact `C2` / `C5` / `C7` pocket, the same exact packet,
  anchor, route, owner-local frame, and clear-boundary-only status, and
  freezes one exact
  `runPipelineElabWith` / `checkedAuthoritative` /
  `typeCheck termClosed` architecture-amendment slice with one exact
  writable module boundary and no silent broadening;
- bounded-implementation checks proving item `2` edits only the writable
  slice frozen by item `1`, keeps one public interface, keeps the same
  helper-route ownership, and does not silently enable multi-SCC search,
  second interfaces, fallback widening, equi-recursive semantics, implicit
  unfolding, or broad capability claims;
- same-pocket-validation checks proving item `3` reruns only the exact rows,
  exact packet, exact command set, and exact six review-visible surfaces
  frozen by accepted rev-002 item `4`, and records the resulting internal /
  public continuity read honestly without subject drift or family widening;
- post-amendment-decision checks proving item `4` records exactly one
  follow-on outcome for the exact pocket only and does not by implication
  reopen rev-001 items `6` through `8`, authorize hardening, production
  rollout, second interfaces, fallback widening, or repo-level capability
  claims;
- predecessor-continuity checks proving accepted rounds `round-099` through
  `round-108`, the accepted rev-002 item chain, and earlier same-pocket
  blocker-proof artifacts are treated as bounded predecessor evidence only;
- inherited-boundary checks proving rounds do not silently enable
  equi-recursive reasoning, implicit unfolding, multi-SCC search, second
  interfaces, or fallback behavior unless an accepted roadmap item
  explicitly makes that lawful;
- retry-subloop checks proving retry-capable reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for aggregate-only items `1`
  and `4`;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen the exact-pocket architecture-amendment lane;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded campaign artifacts, plus an explicit skip note when
  Cabal gates are out of scope; and
- bounded-code-diff review when a round touches `src/`, `src-public/`,
  `app/`, `test/`, or `mlf2.cabal`, including proof that the writable slice
  and one-interface boundary were respected.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the exact-pocket architecture-amendment boundary,
  inherited explicit-only / iso-recursive / non-equi-recursive /
  no-fallback constraints, predecessor continuity, and stage sequencing
  unless an accepted roadmap update explicitly changes the live plan.
- No unresolved blocking issue remains.

## Reviewer Record Format

### Round `<round-id>` Attempt `<n>`

- Baseline checks:
- Task-specific checks:
- Implemented stage result:
- Attempt verdict:
- Stage action:
- Retry reason:
- Fix hypothesis:
- Decision summary:
- Evidence summary:
