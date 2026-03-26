# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only reopen and roadmap-amendment rounds.
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
  classification set and preserves the earlier `non-cyclic-graph = unknown`
  posture as predecessor evidence.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: this contract still defines the reviewable solver / elaboration /
  reconstruction / output-surface evidence bar.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  Why: the accepted representative campaign remains predecessor evidence for
  bounded subset feasibility and unresolved settlement debt.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Why: the accepted strategic decision remains predecessor context for why
  the repo stayed inside the inherited architecture before rev-001 item `5`.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  Why: accepted `round-103` is the authoritative reopen decision that this
  revision must preserve.
- Command: `test -f orchestrator/rounds/round-103/review-record.json`
  Why: the accepted item-5 review record is required continuity evidence for
  the reopened same-family revision.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- reopened-authority-freeze checks proving item `1` preserves accepted
  rev-001 truth, keeps rev-001 items `6` through `8` blocked, and narrows
  the reopened work to one planning-only candidate boundary without silently
  authorizing code work or broad architecture widening;
- reopened-subject-selection checks proving item `2` selects exactly one
  bounded live subject grounded in accepted representative rows and does not
  widen into multiple packets, unrelated routes, or broad family search;
- reopened-safety-contract checks proving item `3` defines a reviewer-auditable
  acceptance bar for
  `acyclic still sufficient`,
  `single-component cyclic-structure successor lane justified`, or
  `stop`, while preserving `iso-recursive = keep`,
  `non-equi-recursive = keep`, `no-fallback = keep`, and the no-multi-SCC
  boundary unless an accepted item explicitly says otherwise;
- reopened-target-bind checks proving item `4` freezes exact accepted rows,
  packets, modules, and output surfaces for the selected subject without
  authorizing implementation by implication;
- reopened-lane-decision checks proving item `5` records exactly one outcome
  for whether to open one bounded architecture-amendment lane or stop, and
  does not silently authorize multi-SCC search, second interfaces, fallback
  behavior, production implementation, or repo-level capability claims;
- predecessor-continuity checks proving accepted rounds `round-094` through
  `round-103` and the accepted rev-001 item chain are treated as bounded
  predecessor evidence only;
- inherited-boundary checks proving rounds do not silently enable
  equi-recursive reasoning, second interfaces, fallback behavior, or
  multi-SCC search unless an accepted roadmap item explicitly makes that
  lawful;
- retry-subloop checks proving retry-capable reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for aggregate-only items `1`
  and `5`;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen the reopened subject or re-open blocked implementation
  items; and
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded campaign artifacts, plus an explicit skip note when
  Cabal gates are out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the reopened same-family subject boundary, inherited
  explicit-only / iso-recursive / non-equi-recursive / no-fallback
  constraints, predecessor continuity, and stage sequencing unless an
  accepted roadmap update explicitly changes the live plan.
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
