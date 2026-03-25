# Round-023 Plan (`D4` Repair-Track Decision Gate)

## Scope Lock (Contract v2)

- contract version: `2`
- roadmap item: `4` (`D4`)
- active stage: `D4` terminal decision gate
- active attempt: `1`
- retry status at plan time: `null` (fresh attempt)
- `D4` mode: aggregate-only, terminal
- accepted semantic retry output is forbidden for `D4` (`accepted + retry` not allowed)
- shared research entrypoint (if any execution is required): `uri-r2-c1-p2-replay-root-cause-v1`
- scenario boundary (exact, unchanged): `uri-r2-c1-only-v1`
- subject boundary (exact, unchanged): authoritative inherited `P1` subject token only (`URI-R2-C1`)

## Authoritative Inputs To Preserve (Immutable)

- `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
- `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `orchestrator/rounds/round-020/review-record.json` (`accepted`, `stage_action: finalize`)
- `orchestrator/rounds/round-021/review-record.json` (`accepted`, `stage_action: finalize`)
- `orchestrator/rounds/round-022/review-record.json` (`accepted`, `stage_action: finalize`)

## Sequential Execution Plan (Bounded, Aggregate-Only)

1. Reconfirm gate invariants before aggregation.
   - Verify `orchestrator/rounds/round-023/state-snapshot.json` remains at `contract_version: 2`, `stage: plan`, `current_task: D4`, `retry: null`.
   - Reassert that this round is decision-only and does not authorize implementation work.

2. Re-validate authoritative carry-forward eligibility.
   - Confirm D1/D2/D3 inputs above are present and unchanged.
   - Confirm each predecessor review record remains authoritative (`accepted + finalize`) and therefore legally carry-forward under the retry-subloop contract.

3. Execute the D4 aggregate decision rule using only D1-D3 authoritative results.
   - `reopen-repair-track` is allowed only if all of the following hold together:
   - D1 is authoritative `pass` with exact bounded replay-failure reproduction continuity.
   - D2 is authoritative `pass` with one bounded divergence boundary and one bounded owner account.
   - D3 is authoritative `pass` with a bounded `repair-supporting` verdict and no scope widening.
   - Otherwise the only legal outcome is `remain-stop`.
   - Record exactly one final outcome token: `reopen-repair-track` or `remain-stop` (no third state).

4. Emit the terminal D4 artifact.
   - Write `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`.
   - Include:
   - inherited authoritative input list,
   - explicit aggregate decision rule application,
   - final outcome token (exactly one of the two allowed values),
   - explicit statement that no production implementation milestone is authorized by D4 itself.

5. Prepare reviewer handoff for terminal-stage semantics.
   - Provide evidence summary proving bounded scope continuity (`URI-R2-C1`, `uri-r2-c1-only-v1`, shared entrypoint continuity for inherited executable stages).
   - Ensure review fields can be recorded as terminal (`Stage action: finalize` on acceptance; `Retry reason: none`; `Fix hypothesis: none`).
   - If reviewer rejects, return to `plan` in the same round with a targeted correction plan; do not emit or request `accepted + retry` semantics for D4.

## Enforced Non-Goals

- No production implementation milestone.
- No repair-track implementation in this round.
- No widened scenario, subject, or ownership scope.
- No second executable interface.
- No mutation of D1/D2/D3 artifacts or their review records.
- No edits to `orchestrator/rounds/round-023/state-snapshot.json`.
- No code implementation changes in `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
