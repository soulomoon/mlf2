# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only evidence rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this successor track must run under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-011/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  Why: every round in this campaign is anchored to the approved successor-roadmap design.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited automatic-recursive baseline remains the live boundary contract for this successor track.
- Command: `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  Why: the prior unannotated `research-stop` remains required predecessor evidence and may not be silently bypassed.
- Command: `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  Why: the repaired `URI-R2-C1` lane is the active inherited starting point for this successor track.
- Command: `test -f orchestrator/roadmaps/2026-03-14-01-unannotated-iso-recursive-inference-successor-roadmap/rev-011/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `027`, the inherited automatic-recursive boundary docs, the completed replay repair track, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- live-subject checks proving the round stays inside the currently bound subject and does not silently widen beyond repaired `URI-R2-C1` unless the roadmap itself is explicitly amended first;
- explicit-only-boundary checks proving the round does not silently enable broad unannotated recursive inference outside the selected bounded subject;
- no-equi-recursive checks proving the round does not introduce implicit unfolding or equi-recursive equality as a success path;
- no-cyclic-graph checks proving the round does not introduce cyclic structural graph encoding or graph-cycle exceptions;
- single-subject / single-family checks proving the round does not widen into multi-SCC, cross-family, or search-wide recursive inference unless the roadmap explicitly authorizes that change;
- no-second-interface checks proving the round does not add a research-only executable entrypoint or alternate production path;
- no-fallback checks proving the round does not introduce a convenience fallback, compatibility shim, or default-path widening;
- `U1` checks proving the accepted artifact binds the inherited baseline, the repaired live subject, and the current stop triggers concretely enough for later rounds;
- `U2` checks proving the accepted artifact either clears or sharply narrows provenance-stable unannotated authority for the live subject without inventing authority by fallback or late repair;
- `U3` checks proving the accepted artifact records whether one admissible local root/cluster exists with stable ownership and no heuristic ranking;
- `U4` checks proving the accepted artifact records whether the current constructor-directed, acyclic, terminating solver discipline can admit the live subject without forbidden widening;
- `U5` checks proving any production diff stays bounded to the currently cleared live subject and is backed by focused positive and negative examples plus preserved out-of-scope rejection behavior;
- `U6` checks proving the final artifact records exactly one of `continue-bounded`, `widen-approved`, or `stop-blocked`, and that the outcome matches the accumulated accepted evidence and current bounded verification;
- retry-subloop checks proving `U1` through `U5` reviews record `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and that `accepted + retry` is never used for `U6`;
- artifact-history checks proving earlier retry attempts remain immutable in `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl` rather than being silently overwritten;
- roadmap-update checks proving accepted `update-roadmap` edits preserve completed-item truth, keep the next unfinished item concrete, and do not silently widen the live subject;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or bounded campaign artifacts;
- skip-note review when a round does not trigger the full Cabal gate, including the exact reason the reviewer judged the code-path gate out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves the current live-subject boundary, inherited non-equi-recursive / non-cyclic-graph constraints, and predecessor-evidence continuity unless an accepted roadmap update explicitly changes the live plan.
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
