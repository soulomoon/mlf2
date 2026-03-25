# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only research rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this successor track must run under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
  Why: every round in this campaign is anchored to the approved repair-track roadmap design.
- Command: `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
  Why: the repair track inherits the approved v2 retry-subloop mechanics.
- Command: `test -f orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `023`, the accepted `D1` through `D4` diagnostic result, the approved repair-track design spec, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- bounded-scenario checks proving the active scope remains exactly `URI-R2-C1` and `uri-r2-c1-only-v1`;
- localized-owner checks proving the round stays bounded to `witness-replay/applyInstantiation-instbot-precondition` and `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch);
- no-second-interface checks proving the repair track does not add a repair-only executable entrypoint or other alternate production path;
- no-fallback checks proving the round does not introduce a convenience fallback, compatibility shim, or silent default-path widening;
- `R1` checks proving the localized repair target is reproduced in implementation-facing terms for the locked scenario;
- `R2` checks proving the production diff stays bounded to the accepted owner area and records one paper-faithful repair direction rather than a broad replay rewrite;
- `R3` checks proving the authoritative replay path succeeds for the locked scenario and the old `InstBot expects ⊥` mismatch no longer occurs;
- `R4` checks proving the final decision is exactly `repair-accepted` or `repair-blocked` and matches the accumulated `R1` through `R3` results and retry-budget outcomes;
- retry-subloop checks proving `R1` through `R3` reviews record `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and that only `accepted + finalize` becomes authoritative carry-forward;
- artifact-history checks proving earlier retry attempts remain immutable in `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl` rather than being silently overwritten;
- predecessor-evidence continuity checks proving completed rounds `round-020` through `round-023` remain immutable and are consumed only as evidence;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, task artifacts, or bounded repair documentation;
- skip-note review when a round does not trigger the full Cabal gate, including the exact reason the reviewer judged the code-path gate out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves bounded repair scope, localized-owner continuity, and predecessor-evidence continuity.
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
