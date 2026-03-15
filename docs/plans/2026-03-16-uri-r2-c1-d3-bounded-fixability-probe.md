# `D3` Bounded Fixability Probe For `URI-R2-C1`

Date: 2026-03-16
Roadmap item: 3
Stage: `D3`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: bounded fixability probe

## Inherited Authoritative Inputs

- `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json`
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/stage-verdict.json`
- `orchestrator/rounds/round-020/evidence/D1/attempt-1/stage-verdict.json`
- `orchestrator/rounds/round-020/review-record.json`
- `orchestrator/rounds/round-021/evidence/D2/attempt-1/check-D2-L.json`
- `orchestrator/rounds/round-021/evidence/D2/attempt-1/check-D2-O.json`
- `orchestrator/rounds/round-021/evidence/D2/attempt-1/stage-verdict.json`
- `orchestrator/rounds/round-021/evidence/D2/attempt-1/trace-bundle.json`
- `orchestrator/rounds/round-021/review-record.json`

## Stage Input Interface

- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D3-fixability-probe, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`.
- Correlation id: `uri-r2-c1-only-v1-d3-attempt-1`.

## Evidence

- Attempt-local evidence directory: `orchestrator/rounds/round-022/evidence/D3/attempt-1`.
- Trace bundle: `orchestrator/rounds/round-022/evidence/D3/attempt-1/trace-bundle.json`.
- `D3-H`: `pass` via `orchestrator/rounds/round-022/evidence/D3/attempt-1/trace-bundle.json`.
- `D3-B`: `pass` via `orchestrator/rounds/round-022/evidence/D3/attempt-1/trace-bundle.json`.
- `D3-V`: `pass` via `orchestrator/rounds/round-022/evidence/D3/attempt-1/trace-bundle.json`.
- Attempt verdict: `repair-supporting`.
- Scope note: `D3` is a bounded fixability probe only; no repair-track implementation is performed in this stage.
- Fix hypothesis: `H1: the localized InstBot precondition mismatch supports one bounded paper-faithful repair direction at applyInstantiation (InstBot) without scope widening or default-path behavior changes.`.
- Bounded repair direction (probe-only): `Bounded direction: align InstBot precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized applyInstantiation boundary, preserving URI-R2-C1 and uri-r2-c1-only-v1 scope.`.
- Divergence boundary statement: `witness-replay/applyInstantiation-instbot-precondition`.
- Owner account statement: `applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch)`.
- Trace refs: `trace://uri-r2-c1/d3/fixability-probe/generalize/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d3/fixability-probe/scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b` `trace://uri-r2-c1/d3/fixability-probe/reify-no-fallback/uri-r2-c1-cluster-1/t5-t5` `trace://uri-r2-c1/d3/fixability-probe/witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n`
- Observations:
- D3-H tested one bounded hypothesis (`H1`) anchored to the D2-localized boundary and owner account.
- D3-B verified scope preservation: URI-R2-C1, uri-r2-c1-only-v1, inherited P1/P2/D1/D2 authorities, and no second executable interface.
- D3-V classified the bounded probe attempt as `repair-supporting`.
- Fix hypothesis: H1: the localized InstBot precondition mismatch supports one bounded paper-faithful repair direction at applyInstantiation (InstBot) without scope widening or default-path behavior changes.
- Repair direction (probe-only): Bounded direction: align InstBot precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized applyInstantiation boundary, preserving URI-R2-C1 and uri-r2-c1-only-v1 scope.
- D3-H: H1 support established at witness-replay/applyInstantiation-instbot-precondition: localized boundary and owner are stable, and one bounded paper-faithful repair direction exists without widening. direction=Bounded direction: align InstBot precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized applyInstantiation boundary, preserving URI-R2-C1 and uri-r2-c1-only-v1 scope.
- D3-B: bounded scope preserved: tuple locked to { research_entrypoint_id=uri-r2-c1-p2-replay-root-cause-v1, stage_selector=D3-fixability-probe, scenario_id=uri-r2-c1-only-v1 }, evidence written only under orchestrator/rounds/round-022/evidence/D3/attempt-1, no second executable interface, no default-path production change
- D3-V: attempt classified repair-supporting: one bounded repair direction is justified and remains within D3 scope

## D3 Outcomes

- `D3-H`: `pass`
- `D3-B`: `pass`
- `D3-V`: `pass`
- Attempt verdict class: `repair-supporting`

## Stage Result

`pass`
