# Round 225 Attempt 1

Date: 2026-05-02
Round: `round-225`
Milestone: `milestone-4`
Direction: `direction-4a-freeze-adt-layout-ownership`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Attempt Summary

- Scope matched the plan exactly on the eight authorized repo-facing payload files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- The docs and backend module notes now agree that
  `BackendData`, `BackendConstructor`, `BackendConstruct`, and `BackendCase`
  own semantic ADT/case structure only.
- Lowerer-owned layout policy is explicit and remains private:
  declaration-order zero-based constructor tags,
  tag slot `0`,
  field slots after the tag word,
  closure-record storage for function-like constructor fields, and
  nullary tag-only heap objects.
- The exact repository guard
  `ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen`
  passed with `1 example, 0 failures`.
- Focused baseline and LLVM row-4 slices all passed with `1 example, 0 failures`
  each.
- The mechanism-table gate passed with rows 1-4 at `YES` and rows 5-7 at `NO`.
- The full repo gate passed:
  `cabal build all && cabal test` reported `2341 examples, 0 failures` in
  `329.0688 seconds`.
- Controller-owned files remained limited to the expected roadmap pointer/state
  dirtiness plus active roadmap/round lineage artifacts; they are not part of
  the merge payload.

## Merge Readiness

- Status: `satisfied`
- Payload files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- Residual risks:
  milestone-5 through milestone-7 remain intentionally open and rows 5-7 stay
  `NO` in the mechanism table;
  the accepted result freezes the current private lowerer-owned layout policy,
  so any later runtime-representation redesign will need new evidence rather
  than piggybacking on this row-4 closure.

## Outcome

The round satisfies the `milestone-4` verification contract and is approved as
`accepted + finalize`.
