# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage, including docs-only evidence rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this refreshed successor control plane must continue
  under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit
  status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited automatic-recursive baseline remains the live boundary
  contract.
- Command: `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  Why: the prior unannotated `research-stop` remains required predecessor
  evidence and may not be silently bypassed.
- Command: `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  Why: the repaired `URI-R2-C1` lane remains the accepted predecessor repair
  baseline for the exhausted queue.
- Command: `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  Why: `L1` remains the authoritative fail-closed bind proving the prior queue
  had no fresh lawful exact successor slice.
- Command: `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  Why: `L2` remains the authoritative `stop-blocked` closeout for the prior
  queue.
- Command: `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  Why: the refreshed control plane is derived from the approved post-`L2`
  next-loop packet.
- Command: `test -f orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for
  retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `067`, the
  inherited automatic-recursive boundary docs, the predecessor recursive-types
  packet, and the accepted `L1` / `L2` closeout unless an accepted roadmap
  update explicitly changes the live plan.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- live-subject-authority checks proving the round does not silently reopen the
  exhausted repaired `URI-R2-C1` queue or begin a new subject without accepted
  roadmap authority;
- explicit-only-boundary checks proving the round does not silently enable broad
  unannotated recursive inference outside the selected bounded subject;
- no-equi-recursive checks proving the round does not introduce implicit
  unfolding or equi-recursive equality as a success path;
- no-cyclic-graph checks proving the round does not introduce cyclic structural
  graph encoding or graph-cycle exceptions;
- single-subject / single-family checks proving the round does not widen into
  multi-SCC, cross-family, or search-wide recursive inference unless the
  roadmap explicitly authorizes that change;
- no-second-interface checks proving the round does not add a research-only
  executable entrypoint or alternate production path;
- no-fallback checks proving the round does not introduce a convenience
  fallback, compatibility shim, or default-path widening;
- no-implicit-clearance checks proving the preserved generic scheme-alias /
  base-like `baseTarget` route is not treated as authorized merely because the
  old queue ended `stop-blocked`;
- `N1` checks proving the accepted artifact lawfully reopens post-`L2` work,
  preserves or explicitly revises the inherited boundary, and does not silently
  authorize implementation or verification;
- `N2` checks proving the accepted artifact chooses exactly one thesis-backed
  next live subject and explicitly defers rejected alternatives;
- `N3` checks proving the accepted artifact records the invariant audit,
  acceptance criteria, and no-go conditions required before any code-changing
  recursive-inference slice is authorized;
- `N4` checks proving the accepted artifact binds exactly one bounded target and
  states explicit future ownership / out-of-scope exclusions;
- `N5` checks proving any production diff stays bounded to the exact `N4`
  target, is backed by focused positive and negative examples when code changes
  are involved, and preserves fail-closed out-of-scope behavior;
- `N6` checks proving the accepted artifact records current bounded
  verification/evidence for the exact `N5` slice without silently widening
  beyond the accepted reopened subject;
- `N7` checks proving the final artifact records exactly one of
  `continue-bounded`, `stop-blocked`, or `completed`, and that the outcome
  matches the accumulated accepted evidence and current bounded verification
  while preserving predecessor continuity;
- retry-subloop checks proving `N1` through `N6` reviews record
  `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and
  that `accepted + retry` is never used for `N7`;
- artifact-history checks proving earlier retry attempts remain immutable in
  `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl` rather
  than being silently overwritten;
- roadmap-update checks proving accepted `update-roadmap` edits preserve
  completed-item truth, keep the next unfinished item concrete, and do not
  silently widen the live subject;
- docs-diff review when a round intentionally changes only `orchestrator/`,
  `docs/`, or bounded campaign artifacts;
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
  non-equi-recursive / non-cyclic-graph constraints, and predecessor-evidence
  continuity unless an accepted roadmap update explicitly changes the live plan.
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
