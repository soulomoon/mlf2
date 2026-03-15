# `D1` Replay-Reproduction Contract For `URI-R2-C1`

Date: 2026-03-16
Roadmap item: 1
Stage: `D1`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: replay-reproduction contract

## Inherited Authoritative Inputs

- `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `orchestrator/rounds/round-017/review-record.json`

## Stage Input Interface

- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D1-replay-reproduction, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`.
- Correlation id: `uri-r2-c1-only-v1-d1-attempt-1`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-020/evidence/D1/attempt-1`.
- Trace bundle: `orchestrator/rounds/round-020/evidence/D1/attempt-1/trace-bundle.json`.
- `D1-I`: `pass` via `orchestrator/rounds/round-020/evidence/D1/attempt-1/trace-bundle.json`.
- `D1-R`: `pass` via `orchestrator/rounds/round-020/evidence/D1/attempt-1/trace-bundle.json`.
- `D1-M`: `pass` via `orchestrator/rounds/round-020/evidence/D1/attempt-1/trace-bundle.json`.
- Replay classification: `exact-bounded-replay-failure`.
- Accepted mismatch target: `InstBot expects ⊥, got: t9 -> t9`.
- Observed mismatch: `InstBot expects ⊥, got: t9 -> t9`.
- Trace refs: `trace://uri-r2-c1/d1/replay/generalize/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d1/replay/scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d1/replay/reify-no-fallback/uri-r2-c1-cluster-1/t5-t5` `trace://uri-r2-c1/d1/replay/witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n`
- Observations:
- D1-I consumed only authoritative inherited inputs from round-016/P1 attempt-2 and round-017/P2 attempt-2.
- D1-R reran the bounded replay lane (`generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay`) via the root-cause entrypoint.
- D1-M compared the new replay outcome against the accepted P2-W boundary (`partial-replay`, `InstBot expects ⊥, got: t9 -> t9`).
- D1 replay classification: exact-bounded-replay-failure.
- D1-I: Inherited continuity confirmed: subject_id uri-r2-c1/cluster-1, scenario uri-r2-c1-only-v1, P2-W trigger partial-replay, mismatch InstBot expects \8869, got: t9 -> t9.
- D1-R: classification=exact-bounded-replay-failure; witness replay verdict=pass; trigger=partial-replay; details=witness replay => ∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N) (applyInstantiation diagnostic failed: InstantiationError "InstBot expects \8869, got: t9 -> t9")
- D1-M: target trigger=partial-replay; target mismatch=InstBot expects ⊥, got: t9 -> t9; observed trigger=partial-replay; observed mismatch=InstBot expects ⊥, got: t9 -> t9; continuity=ok

## D1 Outcomes

- `D1-I`: `pass`
- `D1-R`: `pass`
- `D1-M`: `pass`
- Reproduced closely enough for D2: `yes`

## Stage Result

`pass`
