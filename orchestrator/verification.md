# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only strategic rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this refreshed successor control plane must continue
  under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit
  status markers.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  Why: the refreshed control plane is derived from the human-facing strategic
  roadmap and must not silently drift away from it.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited automatic-recursive baseline remains the live boundary
  contract.
- Command: `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  Why: accepted `N14` is the immediate predecessor result whose
  `continue-bounded` outcome this strategic program now succeeds.
- Command: `test -f orchestrator/rounds/round-081/review-record.json`
  Why: the final accepted review record from the predecessor control plane must
  remain present as continuity evidence.
- Command: `test -f orchestrator/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for
  retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `081`, the
  inherited automatic-recursive boundary docs, the predecessor recursive-types
  packet, the exhausted post-`L2` successor loop, and the strategic roadmap
  source unless an accepted roadmap update explicitly changes the live plan.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- capability-definition checks proving item 1 defines a concrete target, a
  representative corpus, and explicit success / failure conditions rather than
  broad aspiration language only;
- constraint-audit checks proving item 2 classifies the inherited boundaries as
  `keep`, `revise`, or `unknown` and explicitly states whether the current
  architecture still appears plausible;
- mechanism-map checks proving item 3 generalizes accepted packet evidence into
  reusable mechanism families instead of selecting another narrow packet;
- search-model checks proving item 4 defines candidate generation, ambiguity
  handling, rejection conditions, and termination discipline without hidden
  heuristic widening;
- reconstruction-contract checks proving item 5 states how inferred recursion
  must survive solver, elaboration, reconstruction, and output surfaces;
- coverage-campaign checks proving item 6 spans representative local /
  non-local, retained-child / alias-bound, nested-`forall`, ambiguity,
  termination, and reconstruction-heavy families rather than a convenience
  subset;
- architecture-decision checks proving item 7 records exactly one explicit
  outcome and does not blur together "continue within current architecture,"
  "revise the boundary," and "stop";
- inherited-boundary checks proving rounds do not silently enable broad
  automatic recursive inference, equi-recursive reasoning, cyclic structural
  graphs, multi-SCC search, second interfaces, or convenience fallbacks unless
  the accepted roadmap item explicitly makes that lawful;
- predecessor-continuity checks proving accepted `N14` and earlier rounds are
  treated as bounded predecessor evidence rather than as implicit clearance for
  a wider live subject;
- retry-subloop checks proving retry-capable item reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and
  that `accepted + retry` is never used for the final aggregate decision item;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl` rather
  than being silently overwritten;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen the strategic live subject;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or strategic campaign artifacts;
- skip-note review when a round does not trigger the full Cabal gate,
  including the exact reason the reviewer judged the code-path gate out of
  scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the current live-subject boundary, inherited
  non-equi-recursive / non-cyclic-graph constraints, predecessor-evidence
  continuity, and strategic-goal framing unless an accepted roadmap update
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
