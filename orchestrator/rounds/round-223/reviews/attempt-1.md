# Round 223 Attempt 1

Date: 2026-05-02
Round: `round-223`
Milestone: `milestone-2`
Direction: `direction-2a-pin-eager-runtime-contract`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Attempt Summary

- Scope matched the plan exactly on the seven authorized repo-facing files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`, and
  `test/RepoGuardSpec.hs`.
- The synchronized row-2 contract is explicit across the changed surfaces:
  `MLF.Backend.IR` owns the eager executable boundary, conversion publishes
  that structure directly, LLVM/native lowering owns only downstream private
  lowering/runtime details, and lazy STG machinery stays out of scope.
- The focused row-2 guard passed with `1 example, 0 failures`.
- The focused backend slices passed with
  `1 example, 0 failures` for `MLF.Backend.IR` and
  `7 examples, 0 failures` for `MLF.Backend.LLVM/native process entrypoint`.
- The mechanism-table gate passed with `row1=YES`, `row2=YES`, and rows 3-7
  still `NO`.
- The full gate passed: `cabal build all && cabal test` reported
  `2340 examples, 0 failures`.
- Controller-owned files remained limited to the expected active-roadmap
  pointer/state files plus the current round lineage artifacts.

## Outcome

The round satisfies the `milestone-2` verification contract and is approved as
`accepted + finalize`.
