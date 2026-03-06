# Task Plan: 2026-03-06 TMT Improving Loop Orchestrator (Fresh Round 2)

## Goal
Drive the thesis-exact improving loop against the Transformation Mechanism Table using strict YES/NO gates and role-separated execution until the codebase is thesis-exact.

## Baseline
- Baseline commit: `7808933f31f4494c22f37d1e601bd4cf3d2013d7`
- Baseline timestamp (UTC): `2026-03-07T00:00:00Z`
- Baseline inputs:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md`
  - `papers/these-finale-english.txt`
  - `docs/prompts/improving-loop-agent.prompt2.md`

## Phases
1. Initialization — completed.
2. Round 1 verification and row-1 remediation — completed.
3. Round 2 scope-expanded row9-11 remediation — completed.
4. Final full verification sweep — completed.
5. Completion handoff — completed.

## Decisions
- Round 1 anchored on `Elaboration input` because it was the first live `NO`.
- Round 1 remained bounded: remove the last live `resolveContext` fallback swallow and add direct row-1 regression coverage.
- Round 2 widened from row 9 to the coupled set rows 9-11 because canonicalization, identity reconciliation, and non-root raise/weaken resolution shared the same Ω runtime helper layer.
- The accepted Ω shape keeps direct replay/source targets with explicit fail-fast behavior; only forward `etCopyMap` evidence remains as witness-authoritative support for source-domain interior membership.
- The run closed only after a fresh full sweep re-evaluated all 14 mechanisms and every row returned `YES`.
