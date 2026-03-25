# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only evidence rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this follow-on cycle must continue under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  Why: every round in the refreshed control plane is anchored to the approved next bounded `H`-cycle design.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited automatic-recursive baseline remains the live boundary contract.
- Command: `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  Why: the prior unannotated `research-stop` remains required predecessor evidence and may not be silently bypassed.
- Command: `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  Why: the repaired `URI-R2-C1` lane remains the accepted starting point for all follow-on work.
- Command: `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  Why: the accepted `continue-bounded` result is the controlling predecessor decision for this new cycle.
- Command: `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `033`, the inherited automatic-recursive boundary docs, the completed replay repair track, the completed initial successor cycle, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- live-subject checks proving the round stays inside repaired `URI-R2-C1` and does not silently widen beyond the accepted live subject;
- explicit-only-boundary checks proving the round does not silently enable broad unannotated recursive inference outside the selected bounded subject;
- no-equi-recursive checks proving the round does not introduce implicit unfolding or equi-recursive equality as a success path;
- no-cyclic-graph checks proving the round does not introduce cyclic structural graph encoding or graph-cycle exceptions;
- single-subject / single-family checks proving the round does not widen into multi-SCC, cross-family, or search-wide recursive inference unless the roadmap explicitly authorizes that change;
- no-second-interface checks proving the round does not add a research-only executable entrypoint or alternate production path;
- no-fallback checks proving the round does not introduce a convenience fallback, compatibility shim, or default-path widening;
- continue-bounded checks proving rounds do not reinterpret accepted `U2`/`U3`/`U4` negative findings as if they were already cleared;
- `C1` checks proving the accepted artifact binds the follow-on cycle to repaired `URI-R2-C1` and selects exactly one next bounded target without re-clearing accepted negative evidence;
- `C2` checks proving any production diff stays bounded to the selected target, is backed by focused positive and negative examples, and preserves fail-closed out-of-scope behavior;
- `C3` checks proving the accepted artifact records current bounded verification/evidence for the selected slice without widening beyond repaired `URI-R2-C1`;
- `C4` checks proving the final artifact records exactly one of `continue-bounded`, `widen-approved`, or `stop-blocked`, and that the outcome matches the accumulated accepted evidence and current bounded verification;
- `H1` checks proving the accepted artifact binds the next bounded cycle to repaired `URI-R2-C1` and selects exactly the remaining local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC` family without re-clearing accepted negative evidence or reopening the accepted `G` chain;
- `H2` checks proving any production diff stays bounded to the exact `H1`-selected `instArgRootMultiBase` slice, is backed by focused positive and negative examples, and preserves fail-closed out-of-scope behavior for `rootHasMultiInst`, `boundVarTarget`, non-local widening, and replay paths;
- `H3` checks proving the accepted artifact records current bounded verification/evidence for the exact `H2` `instArgRootMultiBase` slice without widening beyond repaired `URI-R2-C1`;
- `H4` checks proving the final artifact records exactly one of `continue-bounded`, `widen-approved`, or `stop-blocked`, and that the outcome matches the accumulated accepted `H3` evidence while preserving the exact `H1` / `H2` / `H3` boundary;
- retry-subloop checks proving `C1` through `C3` reviews record `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and that `accepted + retry` is never used for `C4`;
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
