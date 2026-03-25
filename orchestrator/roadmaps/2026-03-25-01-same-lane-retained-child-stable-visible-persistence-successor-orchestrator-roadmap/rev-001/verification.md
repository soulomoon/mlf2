# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only persistence-gate rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this refreshed successor control plane must continue
  under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-001/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit
  status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited automatic-recursive baseline remains the live boundary
  contract.
- Command: `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  Why: accepted `N14` remains the exact predecessor packet for the bounded
  same-lane retained-child pocket under review.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  Why: the accepted item-2 `non-cyclic-graph = unknown` classification remains
  the only lawful architecture-pressure question this successor loop may
  reopen.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  Why: item-5 defines the persistence tuple, ledger, and outcome vocabulary
  that govern this bounded gate.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  Why: item-6 records the admitted-family coverage result and names the
  same-lane retained-child pocket as blocker debt rather than accepted
  success.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  Why: item-7 is the authoritative source for the selected bounded successor
  gate and the `continue within the current architecture` posture.
- Command: `test -f orchestrator/rounds/round-081/review-record.json`
  Why: the predecessor same-lane retained-child evidence chain must remain
  present as bounded historical evidence.
- Command: `test -f orchestrator/rounds/round-088/review-record.json`
  Why: the final accepted strategic architecture decision must remain present
  as continuity evidence for this successor loop.
- Command: `test -f orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for
  retry-state, artifact, and transition rules.
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  Why: any round that touches the selected same-lane retained-child code/test
  path or asserts new runtime persistence behavior must rerun the bounded
  prototype characterization for the exact pocket under review.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `088`, the
  inherited automatic-recursive boundary docs, the exact same-lane
  retained-child predecessor packet, the accepted strategic item-5 / item-6 /
  item-7 record, and the no-silent-widening rule unless an accepted roadmap
  update explicitly changes the live plan.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- contract-freeze checks proving item 1 identifies exactly one same-lane
  retained-child pocket, freezes the full persistence tuple, and names the
  exact solver / elaboration / reconstruction / internal-output /
  public-output evidence later rounds must preserve;
- breakpoint-audit checks proving item 2 distinguishes already-satisfied
  ledger rows from the first actual continuity break without reopening
  general family search or neighboring routes;
- implementation-slice checks proving item 3 stays limited to the exact
  same-lane retained-child route, its supporting reconstruction/output path,
  and corresponding focused tests/docs, without silently touching the
  alias-bound family, nested-`forall` crossing cases, cyclic search, second
  interfaces, or fallback widening;
- persistence-validation checks proving item 4 records solver, elaboration,
  reconstruction, internal-output, public-output, and reviewer-visible
  evidence for the same frozen pocket and uses exactly one item-5 / item-6
  outcome token for that pocket;
- successor-decision checks proving item 5 records exactly one bounded
  follow-on outcome and reopens `non-cyclic-graph` only if the bounded
  persistence evidence itself specifically forces that question;
- inherited-boundary checks proving rounds do not silently enable broad
  automatic recursive inference, equi-recursive reasoning, cyclic structural
  graphs, multi-SCC search, second interfaces, or convenience fallbacks
  unless the accepted roadmap item explicitly makes that lawful;
- predecessor-continuity checks proving accepted `N14`, accepted strategic
  items `2`, `5`, `6`, and `7`, and completed rounds `001` through `088` are
  treated as bounded predecessor evidence rather than as implicit clearance
  for a wider live subject;
- retry-subloop checks proving retry-capable item reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`,
  and that `accepted + retry` is never used for the final bounded-decision
  item;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl`
  rather than being silently overwritten;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen the live subject into the alias-bound family, nested-`forall`
  success, or general automatic recursive inference;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded-campaign artifacts;
- skip-note review when a round does not trigger the focused or full Cabal
  gates, including the exact reason the reviewer judged those code-path gates
  out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the current live-subject boundary, inherited
  non-equi-recursive / non-cyclic-graph constraints, predecessor-evidence
  continuity, and bounded-gate framing unless an accepted roadmap update
  explicitly changes the live plan.
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
