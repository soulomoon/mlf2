# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only research rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  Why: future rounds on this successor track must run under the v2 retry-subloop machine-state contract.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/roadmap.md`
  Why: the live roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`
  Why: every round in this campaign is anchored to the approved replay root-cause roadmap design.
- Command: `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
  Why: the successor track inherits the approved v2 retry-subloop mechanics.
- Command: `test -f orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-001/retry-subloop.md`
  Why: the live control plane needs one repo-local operational source for retry-state, artifact, and transition rules.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must preserve completed rounds `001` through `019`, the accepted prototype-evidence hard-stop result, the approved replay root-cause design spec, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- shared-entrypoint isolation checks proving root-cause evidence enters only through `uri-r2-c1-p2-replay-root-cause-v1` and does not create a second executable interface;
- bounded-scenario checks proving the active scenario remains exactly `uri-r2-c1-only-v1` and does not widen beyond `URI-R2-C1`;
- inherited-subject checks proving the round consumes only the authoritative `P1` subject token and accepted `P2` replay evidence when those are in scope;
- `D1` checks proving the authoritative `P2-W` failure is reproduced or boundedly classified without widened diagnostics or replacement subjects;
- `D2` checks proving one exact bounded divergence boundary and one exact owner account are recorded without widened blame or speculative second causes;
- `D3` checks proving the fixability probe stays paper-faithful and bounded, with no production-path change or scope widening;
- `D4` checks proving the final decision is exactly `reopen-repair-track` or `remain-stop` and matches the accumulated `D1` through `D3` results and retry-budget outcomes;
- retry-subloop checks proving `D1` through `D3` reviews record `attempt_verdict`, `stage_action`, `retry_reason`, and `fix_hypothesis`, and that only `accepted + finalize` becomes authoritative carry-forward;
- artifact-history checks proving earlier retry attempts remain immutable in `reviews/attempt-<n>.md` and the controller-owned `attempt-log.jsonl` rather than being silently overwritten;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or task artifacts;
- skip-note review when a round does not trigger the full Cabal gate, including the exact reason the reviewer judged the code-path gate out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- The round preserves bounded root-cause scope, inherited-subject continuity, and predecessor-evidence continuity.
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
