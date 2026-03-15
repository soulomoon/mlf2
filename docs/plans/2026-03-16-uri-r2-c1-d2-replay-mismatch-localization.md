# `D2` Replay-Mismatch Localization For `URI-R2-C1`

Date: 2026-03-16
Roadmap item: 2
Stage: `D2`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: replay-mismatch localization

## Inherited Authoritative Inputs

- `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-R.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/trace-bundle.json`
- `orchestrator/rounds/round-020/evidence/D1/attempt-1/check-D1-R.json`
- `orchestrator/rounds/round-020/evidence/D1/attempt-1/stage-verdict.json`
- `orchestrator/rounds/round-020/evidence/D1/attempt-1/trace-bundle.json`
- `orchestrator/rounds/round-020/review-record.json`

## Stage Input Interface

- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D2-mismatch-localization, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`.
- Correlation id: `uri-r2-c1-only-v1-d2-attempt-1`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-021/evidence/D2/attempt-1`.
- Trace bundle: `orchestrator/rounds/round-021/evidence/D2/attempt-1/trace-bundle.json`.
- `D2-T`: `pass` via `orchestrator/rounds/round-021/evidence/D2/attempt-1/trace-bundle.json`.
- `D2-L`: `pass` via `orchestrator/rounds/round-021/evidence/D2/attempt-1/trace-bundle.json`.
- `D2-O`: `pass` via `orchestrator/rounds/round-021/evidence/D2/attempt-1/trace-bundle.json`.
- Divergence boundary statement: `witness-replay/applyInstantiation-instbot-precondition`.
- Owner account statement: `applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch)`.
- Trace refs: `trace://uri-r2-c1/d2/mismatch-localization/generalize/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d2/mismatch-localization/scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d2/mismatch-localization/reify-no-fallback/uri-r2-c1-cluster-1/t5-t5` `trace://uri-r2-c1/d2/mismatch-localization/witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n`
- Observations:
- D2-T aligned P2 and D1 replay trace segments under one correlation id for `generalize -> scheme-to-type -> reify-no-fallback -> witness-replay`.
- D2-L localized the earliest divergence boundary where replay identity first splits from the observed mismatch.
- D2-O assigned one bounded owner account and excluded adjacent non-owner categories for this attempt.
- Divergence boundary: witness-replay/applyInstantiation-instbot-precondition.
- Owner account: applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch).
- D2-T: aligned trace suffixes: generalize/uri-r2-c1-cluster-1/a-b-a-a-b -> scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b -> reify-no-fallback/uri-r2-c1-cluster-1/t5-t5 -> witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n; inherited correlations: uri-r2-c1-only-v1-p2-attempt-2 => uri-r2-c1-only-v1-d1-attempt-1
- D2-L: first divergence boundary=witness-replay/applyInstantiation-instbot-precondition; expected replay lane shape from no-fallback reification=t5 -> t5; observed witness replay path ends at applyInstantiation mismatch=InstBot expects ⊥, got: t9 -> t9
- D2-O: owner=applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch); boundary=witness-replay/applyInstantiation-instbot-precondition; non-owners: witness construction preserved the same witness text across P2 and D1, replay-domain reconstruction preserved aligned trace suffixes, no-fallback reification output remained `t5 -> t5` before witness replay

## D2 Outcomes

- `D2-T`: `pass`
- `D2-L`: `pass`
- `D2-O`: `pass`

## Stage Result

`pass`
