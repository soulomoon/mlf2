# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only settlement and architecture-gate rounds.
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
  non-cyclic-graph / no-fallback boundary remains the live baseline.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  Why: the accepted capability contract still defines the representative
  family matrix and the positive / negative evidence bar for global
  settlement.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  Why: the accepted strategic audit still records
  `non-cyclic-graph = unknown` at repo scope before this new family.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: this contract still defines the reviewable solver / elaboration /
  reconstruction / output-surface evidence bar.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  Why: the accepted representative campaign remains the predecessor evidence
  for bounded subset feasibility and unresolved settlement debt.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Why: the accepted strategic item-7 result remains the last general
  repo-level architecture choice before this new family.
- Command: `test -f docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
  Why: the accepted exact-pocket item-5 decision remains the immediate
  predecessor architecture-pressure record.
- Command: `test -f orchestrator/rounds/round-098/review-record.json`
  Why: the immediate predecessor round record must remain present as
  continuity evidence.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- settlement-contract-freeze checks proving item `1` freezes the exact global
  `keep` versus `reopen` bar, the unresolved family ledger, and the lawful
  predecessor evidence chain without overclaiming settlement;
- propagation-and-placement proof-slice checks proving item `2` produces
  representative production-surface evidence for `P2`, `P3`, and `P4`
  inside the inherited acyclic model without silent widening into cyclic
  search, multi-SCC search, second interfaces, or fallback paths;
- polymorphism-and-output proof-slice checks proving item `3` produces
  representative production-surface evidence for `P5` and `P6` on the
  existing pipeline and public-output surfaces rather than on helper-only
  paths;
- representative-campaign checks proving item `4` reruns the family matrix
  honestly across positive and required negative or bounded families and does
  not hide unresolved blocker debt, ambiguity, or termination pressure;
- global-settlement-gate checks proving item `5` records exactly one outcome,
  that `non-cyclic-graph = keep` is backed by accepted production-surface
  evidence across the representative matrix, and that `reopen` is used if
  required positive-family evidence crosses the current architecture
  boundary;
- post-settlement-implementation checks proving item `6` runs only after
  accepted item `5` records `non-cyclic-graph = keep` and stays inside the
  settled architecture and existing pipeline surfaces;
- hardening checks proving item `7` strengthens regression coverage,
  fail-closed behavior, and operator docs without silently widening the
  settled scope;
- capability-claim-gate checks proving item `8` records exactly one honest
  repo-level claim outcome and does not overclaim beyond the accepted settled
  scope;
- inherited-boundary checks proving rounds do not silently enable
  equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, or convenience fallbacks unless an accepted roadmap
  item explicitly makes that lawful;
- predecessor-continuity checks proving accepted strategic items `2`, `5`,
  `6`, and `7`, accepted rounds `round-094` through `round-098`, and
  completed rounds `round-001` through `round-093` are treated as bounded
  predecessor evidence only;
- retry-subloop checks proving retry-capable reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for aggregate-only items `5`
  and `8`;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen global settlement or capability claims;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded campaign artifacts; and
- skip-note review when a round does not trigger focused or full Cabal gates,
  including the exact reason the reviewer judged those code-path gates out of
  scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the current live-subject boundary, inherited
  explicit-only / non-equi-recursive / non-cyclic-graph constraints,
  predecessor continuity, and stage gate sequencing unless an accepted
  roadmap update explicitly changes the live plan.
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
