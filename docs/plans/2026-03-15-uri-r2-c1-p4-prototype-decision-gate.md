# `P4` Prototype Decision Gate For `URI-R2-C1`

Date: 2026-03-15
Roadmap item: 4
Stage: `P4`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: prototype decision gate

## Inherited Inputs

- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md`
- `orchestrator/rounds/round-016/review-record.json`
- `orchestrator/rounds/round-017/review-record.json`
- `orchestrator/rounds/round-018/review-record.json`

## Method

- Ordered checks: `P4-CONSUME`, `P4-DECISION`.
- Decision threshold: `reopen-handoff-track` only when `P1`, `P2`, and `P3` are all `pass` with no unresolved caveat.
- Shared `correlation_id`: `uri-r2-c1-only-v1-p4-attempt-1`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-019/evidence/P4/attempt-1`.
- Stage consumption summary: `orchestrator/rounds/round-019/evidence/P4/attempt-1/stage-consumption.json`.
- Decision verdict: `orchestrator/rounds/round-019/evidence/P4/attempt-1/decision-verdict.json`.
- Trace bundle: `orchestrator/rounds/round-019/evidence/P4/attempt-1/trace-bundle.json`.
- `P1` authoritative attempt `2` => `pass` (terminal_reason `none`), source `orchestrator/rounds/round-016/review-record.json`.
- `P2` authoritative attempt `2` => `semantic-negative` (terminal_reason `none`), source `orchestrator/rounds/round-017/review-record.json`.
- `P3` authoritative attempt `2` => `semantic-negative` (terminal_reason `none`), source `orchestrator/rounds/round-018/review-record.json`.
- `P4-CONSUME`: `pass` via `orchestrator/rounds/round-019/evidence/P4/attempt-1/stage-consumption.json`.
- `P4-DECISION`: `semantic-negative` via `orchestrator/rounds/round-019/evidence/P4/attempt-1/decision-verdict.json`.
- Trace refs: `trace://uri-r2-c1/p4/consume/p1-pass` `trace://uri-r2-c1/p4/consume/p2-semantic-negative` `trace://uri-r2-c1/p4/consume/p3-semantic-negative` `trace://uri-r2-c1/p4/decision/hard-stop-blocking-stop-condition`
- Observations:
- P4 consumed authoritative stage outcomes from round-016, round-017, and round-018 review records.
- Decision threshold respected: reopen requires all pass and no unresolved caveat.
- Given inherited results, terminal decision is hard-stop.
- P1 authoritative attempt 2 => pass (terminal_reason: none)
- P2 authoritative attempt 2 => semantic-negative (terminal_reason: none)
- P3 authoritative attempt 2 => semantic-negative (terminal_reason: none)

## Rejection Triggers

Observed triggers: `blocking-stop-condition`.

`P4-CONSUME`: P4 consumed authoritative P1/P2/P3 review records without widening scope.
`P4-DECISION`: P4 terminal decision: hard-stop.

Normalized vocabulary: `none` `widened-search` `multi-scc` `cross-family` `heuristic-choice` `late-repair` `manufactured-provenance` `surrogate-substitution` `replay-domain-widening` `non-local-salvage` `structural-cycle` `implicit-unfolding` `equi-recursive-reasoning` `termination-weakening` `nondeterministic-output` `inconsistent-trace` `partial-replay` `production-path-dependence` `blocking-stop-condition`.

## Final Decision

`hard-stop`
