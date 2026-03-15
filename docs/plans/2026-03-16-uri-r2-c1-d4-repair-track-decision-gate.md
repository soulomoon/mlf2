# `D4` Repair-Track Decision Gate For `URI-R2-C1`

Date: 2026-03-16
Roadmap item: 4
Stage: `D4`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: repair-track decision gate (aggregate-only, terminal)

## Inherited Authoritative Inputs

- `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
- `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `orchestrator/rounds/round-020/review-record.json`
- `orchestrator/rounds/round-021/review-record.json`
- `orchestrator/rounds/round-022/review-record.json`

## Decision Method

- Ordered checks: `D4-CONSUME`, `D4-DECISION`.
- `D4` is aggregate-only and terminal; no semantic retry output is permitted.
- Decision rule:
- choose `reopen-repair-track` only if all of the following hold together from authoritative finalized predecessor records:
- `D1` is `pass` with exact bounded replay-failure continuity;
- `D2` is `pass` with one bounded divergence boundary and one bounded owner account;
- `D3` is `pass` with `repair-supporting` bounded fixability and no scope widening.
- otherwise choose `remain-stop`.
- Shared `correlation_id`: `uri-r2-c1-only-v1-d4-attempt-1`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-023/evidence/D4/attempt-1`.
- Stage consumption summary: `orchestrator/rounds/round-023/evidence/D4/attempt-1/stage-consumption.json`.
- Decision verdict: `orchestrator/rounds/round-023/evidence/D4/attempt-1/decision-verdict.json`.
- Trace bundle: `orchestrator/rounds/round-023/evidence/D4/attempt-1/trace-bundle.json`.
- `D4-CONSUME`: `pass` via `orchestrator/rounds/round-023/evidence/D4/attempt-1/check-D4-CONSUME.json`.
- `D4-DECISION`: `pass` via `orchestrator/rounds/round-023/evidence/D4/attempt-1/check-D4-DECISION.json`.
- `D1` authoritative attempt `1` => `pass`, `accepted + finalize`, source `orchestrator/rounds/round-020/review-record.json`.
- `D2` authoritative attempt `1` => `pass`, `accepted + finalize`, source `orchestrator/rounds/round-021/review-record.json`.
- `D3` authoritative attempt `1` => `pass`, `accepted + finalize`, source `orchestrator/rounds/round-022/review-record.json`.
- Aggregate rule evaluation:
- D1 exact-bounded replay continuity present: `true`.
- D2 bounded divergence+owner localization present: `true`.
- D3 bounded repair-supporting probe without widening: `true`.
- Trace refs: `trace://uri-r2-c1/d4/consume/d1-pass-finalize` `trace://uri-r2-c1/d4/consume/d2-pass-finalize` `trace://uri-r2-c1/d4/consume/d3-pass-finalize` `trace://uri-r2-c1/d4/decision/reopen-repair-track`.
- Observations:
- D4 consumed only authoritative D1/D2/D3 artifacts and finalized review records under `URI-R2-C1` and `uri-r2-c1-only-v1`.
- The D4 threshold conditions all evaluated `true` with no scope widening and no alternative subject/scenario.
- D4 remains decision-only: it does not implement repair behavior and does not authorize production-path change by itself.

## Final Outcome

`reopen-repair-track`

## Authorization Boundary

D4 does not authorize a production implementation milestone. Any repair work requires a separate later repair-track roadmap and approval path.
