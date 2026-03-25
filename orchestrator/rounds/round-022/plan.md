# Round-022 Plan (`D3` Bounded Fixability Probe)

## Scope Lock (Contract v2)

- contract version: `2`
- roadmap item: `3` (`D3`)
- active stage: `D3` bounded fixability probe
- active attempt: `1`
- retry status at plan time: `null` (fresh attempt, not a retry-delta plan)
- shared research entrypoint: `uri-r2-c1-p2-replay-root-cause-v1`
- stage selector: `D3-fixability-probe`
- scenario id: `uri-r2-c1-only-v1` (exact, unchanged)
- subject boundary: authoritative inherited `P1` subject token only (`URI-R2-C1`)

## Authoritative Carry-Forward (Must Remain Intact)

- authoritative `P1` subject token from:
  - `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- accepted `P2` failure boundary (`partial-replay`, `InstBot expects ⊥, got: t9 -> t9`) from:
  - `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
  - `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
  - `orchestrator/rounds/round-017/review-record.json`
- authoritative `D1` result (`pass`, `accepted + finalize`) from:
  - `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
  - `orchestrator/rounds/round-020/review-record.json`
- authoritative `D2` localization result (`pass`, `accepted + finalize`) from:
  - `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md`
  - `orchestrator/rounds/round-021/review-record.json`
- inherited localization boundary to probe (no widening):
  - divergence boundary: `witness-replay/applyInstantiation-instbot-precondition`
  - owner account: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch semantics)

## Attempt-1 Fixability Hypothesis (Single, Bounded)

Test exactly one hypothesis for `D3-H`:

- `H1`: the localized `InstBot` precondition mismatch admits one paper-faithful, bounded repair direction at the already-localized boundary (`applyInstantiation` `InstBot` branch), without changing subject/scenario scope or production-path behavior during this stage.

If `H1` cannot be supported under bounded evidence, classify the attempt as bounded-negative or inconclusive; do not introduce a second hypothesis in this attempt.

## Sequential Execution Plan

1. Reconfirm stage and boundary invariants before running `D3`.
   - Verify active stage remains `D3`, attempt remains `1`, and retry is still inactive.
   - Reassert exact entrypoint/scenario/subject tuple and inherited `P2`/`D1`/`D2` authority.

2. Initialize attempt-local `D3` evidence lane.
   - Execute only via:
     - `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D3-fixability-probe, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`
   - Emit outputs only under:
     - `orchestrator/rounds/round-022/evidence/D3/attempt-1/`

3. Execute `D3-H` (fix hypothesis discipline) against the D2-localized boundary only.
   - Probe whether `H1` is supportable using bounded replay-root-cause evidence tied to the localized `InstBot` branch.
   - Reject any execution path that adds new subjects, new scenarios, or alternative divergence owners.

4. Execute `D3-B` (boundary preservation).
   - Prove the probe keeps scope fixed to `URI-R2-C1` + `uri-r2-c1-only-v1` + inherited authorities.
   - Prove the probe does not create a second executable interface and does not require production/default-path semantic change.

5. Execute `D3-V` (verdict classification).
   - Classify attempt result as one of:
     - `repair-supporting` (bounded support for one repair direction),
     - `bounded-negative` (no bounded support),
     - `inconclusive` (bounded evidence cannot stabilize this attempt).
   - Map this classification to stage result (`pass`, `semantic-negative`, or `inconclusive`) without widening claims.

6. Produce canonical `D3` artifact.
   - Write:
     - `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
   - Include:
     - inherited authoritative inputs,
     - exact shared entrypoint tuple,
     - `D3-H`, `D3-B`, `D3-V` outcomes,
     - one-attempt verdict with bounded rationale,
     - explicit statement that this is a fixability probe only (not a repair implementation milestone).

7. Prepare review handoff evidence under contract v2.
   - Ensure reviewable attempt artifacts include stage result evidence plus explicit fields:
     - `Implemented stage result`
     - `Attempt verdict`
     - `Stage action`
     - `Retry reason`
     - `Fix hypothesis`
   - Preserve prior round artifacts and review history as immutable.

## Enforced Non-Goals

- No production implementation milestone.
- No repair-track implementation work in this round.
- No widened scenario, subject, or ownership beyond inherited boundaries.
- No second executable interface.
- No edits to `orchestrator/rounds/round-022/state-snapshot.json`.
- No rewrite of authoritative `P1`, accepted `P2`, authoritative `D1`, or authoritative `D2` artifacts.
