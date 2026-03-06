# Task Plan: 2026-03-07 TMT Improving Loop Orchestrator (Fresh Round 2)

## Goal
Drive the thesis-exact improving loop against the Transformation Mechanism Table using strict YES/NO gates and role-separated execution until the codebase is thesis-exact.

## Baseline
- Baseline commit: `816eb2e308091506a5b1b10b385a3a0984f92209`
- Baseline timestamp (UTC): `2026-03-06T19:55:12Z`
- Baseline inputs:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md`
  - `papers/these-finale-english.txt`
  - `docs/prompts/improving-loop-agent.prompt2.md`

## Phases
1. Initialization — completed.
2. Round 1 verification sweep — completed.
3. Round-by-round remediation — completed.
4. Final verification sweep — completed.
5. Completion handoff — completed.

## Decisions
- Round 1 anchored on `Result-type context wiring`, the first live `NO` in the current table order.
- The accepted row2 fix removed the live solved-compat adapter path by building finalized `PresolutionView`s directly from snapshot/finalize data and validating result-type views from canonical constraint facts.
- Round 2 anchored on `Translatability normalization` and widened only enough to refresh frozen parity after the intentional all-inert `W`-normalization change.
- The accepted row8 fix applied §15.2.8 all-inert weakening on the live presolution path and refreshed `test/golden/legacy-replay-baseline-v1.json` to freeze the new thesis-exact artifacts.
- A fresh closeout sweep kept rows 1, 3-7, and 9-14 at `YES` while rows 2 and 8 returned to `YES`, restoring a full 14/14 `YES` table.

## Errors
- Round 2 first full-gate attempt failed `Frozen parity artifact baseline` because the new all-inert `W`-normalization intentionally changed frozen solved artifacts (`cWeakenedVars`). Recovery: regenerate `test/golden/legacy-replay-baseline-v1.json` with `frozen-parity-gen`, then rerun `Frozen parity artifact baseline` and `cabal build all && cabal test` to green.
