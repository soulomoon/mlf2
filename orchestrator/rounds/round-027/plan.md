# Round 027 Plan (`R4` Repair Decision Gate)

## Objective

Execute only roadmap item `R4` for the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` lane and produce one terminal aggregate artifact that records exactly one final bounded outcome: `repair-accepted` or `repair-blocked`.

This round is aggregate-only. It must consume the accepted `R1` through `R3` record plus current bounded verification for this round, without reopening production repair work, widening scope, or changing roadmap ordering.

## Locked Round Context

- Round id: `round-027`
- Stage: `plan` for roadmap item `R4`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Stage mode: aggregate-only, terminal
- Subject boundary: `URI-R2-C1`
- Scenario boundary: `uri-r2-c1-only-v1`
- Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
- Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Controlling bug: `BUG-2026-03-16-001`

Inherited authoritative audit that must remain unchanged:

- `P1 = pass`
- `P2 = semantic-negative`
- `D1 = pass`
- `D2 = pass`
- `D3 = pass`
- `D4 = reopen-repair-track`
- `R1 = pass`
- `R2 = pass`
- `R3 = pass`

`R4` follows `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/retry-subloop.md` terminal semantics:

- review may accept this stage only with `stage_action: finalize`;
- review may reject and return the same round to `plan`;
- review may not emit `accepted + retry` for `R4`.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-027/selection.md`
- `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/verification.md`
- `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/retry-subloop.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`
- `orchestrator/rounds/round-024/review-record.json`
- `orchestrator/rounds/round-025/review-record.json`
- `orchestrator/rounds/round-026/review-record.json`
- `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`
- `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
- `docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`
- `Bugs.md` (`BUG-2026-03-16-001`)

## Files Expected In Scope

Primary writable files:

1. `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
2. `orchestrator/rounds/round-027/implementation-notes.md` only if one bounded reviewer-handoff note is strictly needed

Read-only inputs:

1. `orchestrator/rounds/round-024/review-record.json`
2. `orchestrator/rounds/round-025/review-record.json`
3. `orchestrator/rounds/round-026/review-record.json`
4. `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`
5. `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
6. `docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`

Files that must remain untouched in this round:

- `orchestrator/rounds/round-027/state-snapshot.json`
- `orchestrator/attempt-log.jsonl`
- `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/roadmap.md`
- `Bugs.md`
- prior round artifacts under `orchestrator/rounds/round-001/` through `orchestrator/rounds/round-026/`

## Sequential Tasks

### Task 1 - Freeze the `R4` attempt-1 decision contract before aggregation

- Restate inside the `R4` artifact that this is `attempt-1` with `retry: null`.
- Restate that `R4` is aggregate-only and terminal under `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/retry-subloop.md`.
- Reassert the fixed lane:
  - subject `URI-R2-C1`
  - scenario `uri-r2-c1-only-v1`
  - repair boundary `witness-replay/applyInstantiation-instbot-precondition`
  - owner boundary `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Reassert that this round may decide the repair-track outcome only. It may not reopen `R1` through `R3`, authorize new production edits, add a second executable interface, or widen replay scope.

### Task 2 - Consume only the authoritative `R1` through `R3` carry-forward record

- Re-read and summarize the finalized reviewer-owned records for:
  - `R1` from `orchestrator/rounds/round-024/review-record.json`
  - `R2` from `orchestrator/rounds/round-025/review-record.json`
  - `R3` from `orchestrator/rounds/round-026/review-record.json`
- Carry forward only the authoritative attempt for each stage:
  - `R1`: `attempt = 1`, `accepted + finalize`, `pass`
  - `R2`: `attempt = 3`, `accepted + finalize`, `pass`
  - `R3`: `attempt = 1`, `accepted + finalize`, `pass`
- Record the bounded facts that matter for the final gate:
  - `R1` reproduced the localized `InstBot expects ⊥` mismatch at the locked owner boundary and did not widen scope.
  - `R2` landed one bounded paper-faithful `applyInstantiation` / `InstBot` repair and finalized within retry budget, with no second interface or fallback behavior.
  - `R3` verified that the locked replay path now succeeds for `URI-R2-C1` / `uri-r2-c1-only-v1`, the old mismatch no longer occurs on that lane, strict non-replay `InstBot` failures still hold, and no broader replay campaign was reopened.
- Treat non-authoritative retry history as background only. Do not reopen it as live work.

### Task 3 - Run current bounded verification for the terminal gate

- Run the baseline checks required by `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-027/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-027/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
  - `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
  - `test -f orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/retry-subloop.md`
- Reconfirm the authoritative `R1` through `R3` inputs are present and parseable:
  - `python3 -m json.tool orchestrator/rounds/round-024/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-025/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-026/review-record.json >/dev/null`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`
- Reconfirm the stage-status facts needed for the gate:
  - `rg -n '"stage_id": "R1"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"' orchestrator/rounds/round-024/review-record.json`
  - `rg -n '"stage_id": "R2"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"|"attempts_run": 3|"max_attempts": 100' orchestrator/rounds/round-025/review-record.json`
  - `rg -n '"stage_id": "R3"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"' orchestrator/rounds/round-026/review-record.json`
- Treat this as a docs-only aggregate round for gate purposes:
  - do not rerun production repair work;
  - do not require `cabal build all && cabal test` unless this round unexpectedly edits `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
  - if the round remains docs/orchestrator-only, record the exact skip note that the full Cabal gate was inherited from accepted `R3` and is out of scope for `R4`.
- Because the worktree may contain unrelated edits from other agents, prove scope by listing the files changed by this round inside the artifact instead of assuming a globally clean diff outside the mandatory baseline checks.

### Task 4 - Apply the closed decision rule and emit the canonical `R4` artifact

- Write `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`.
- Record exactly one final outcome token:
  - `repair-accepted`
  - `repair-blocked`
- Use this closed rule:
  - choose `repair-accepted` only if all of the following remain true together:
    - `R1` remains authoritative `pass` with localized reproduction continuity at the locked boundary;
    - `R2` remains authoritative `pass` on the bounded `applyInstantiation` / `InstBot` repair, finalized within retry budget, with no second interface or fallback widening;
    - `R3` remains authoritative `pass` proving locked replay success, removal of the old mismatch on the replay lane, preserved non-replay fail-closed behavior, and no broader replay widening;
    - the current bounded verification from Task 3 passes without uncovering a missing artifact, inconsistent authoritative status, or round-local scope violation.
  - otherwise choose `repair-blocked`.
- Fail closed:
  - if any prerequisite artifact is missing, any authoritative record is not `accepted + finalize`, any required check token is absent, or current bounded verification fails, record `repair-blocked`;
  - do not invent a third state and do not convert missing evidence into new implementation work.
- Make the artifact explicit that `repair-blocked` is still a valid final bounded outcome for `R4`; it is not itself a reviewer rejection.

### Task 5 - Prepare reviewer handoff for terminal aggregate semantics

- Ensure the `R4` artifact records:
  - `Attempt: 1`
  - `Retry state: null`
  - the inherited authoritative audit through `R3`
  - the exact decision rule used
  - the exact final outcome token
  - the commands run for current bounded verification
  - the files changed by this round
  - the exact skip note if the full Cabal gate is not run
- Prepare reviewer-facing semantics explicitly:
  - a correct `R4` attempt may finalize as `accepted + finalize` with final outcome `repair-accepted` or `repair-blocked`;
  - if the reviewer finds an aggregation or terminal-contract defect, the only legal retry path is `rejected + retry` back to `plan` in the same round;
  - `accepted + retry` is forbidden for `R4`.

## Non-Goals

- No edits to `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`
- No reopening of `R1`, `R2`, or `R3` as live implementation work
- No change to roadmap ordering or controller-owned files
- No mutation of prior round artifacts, retry history, or `Bugs.md`
- No second executable interface, compatibility fallback, or broadened replay/regression scope
- No outcome token other than `repair-accepted` or `repair-blocked`

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-004/verification.md` still apply.

Round-specific checks:

1. `plan.md` and the resulting `R4` artifact name `attempt-1` explicitly and preserve `retry: null`.
2. The round remains fixed to `URI-R2-C1`, `uri-r2-c1-only-v1`, `witness-replay/applyInstantiation-instbot-precondition`, and `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
3. The round consumes only authoritative `R1` through `R3` records and artifacts; it does not reopen repair implementation or widen scope.
4. The artifact records exactly one final outcome token, `repair-accepted` or `repair-blocked`, and the choice matches the stated closed decision rule.
5. The current bounded verification is recorded concretely, including the exact skip reason if the full Cabal gate is omitted because the round stayed docs/orchestrator-only.
6. The artifact lists the files changed by this round and does not rely on or introduce a second executable interface, fallback path, or broad replay rewrite.
7. Reviewer output stays within `R4` terminal semantics: `accepted + finalize` on a correct final artifact, or `rejected + retry` for a plan/artifact defect; never `accepted + retry`.
