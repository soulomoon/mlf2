# Round 226 Attempt 1

Date: 2026-05-02
Round: `round-226`
Milestone: `milestone-5`
Direction: `direction-5a-lock-primitive-and-evaluation-order-contract`

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
- The docs and backend module notes now agree that row 5 is the closed
  reserved runtime-binding set
  `__mlfp_and`, `__io_pure`, `__io_bind`, and `__io_putStrLn`,
  still represented through `BackendVar`, `BackendApp`, and `BackendTyApp`,
  with no `BackendPrim`, no second executable IR, no public lowering surface,
  no fallback runtime lane, no lazy STG rescue, and no broad FFI expansion.
- The eager sequencing contract is explicit across those same surfaces:
  let RHS before body,
  case scrutinee before branch selection,
  direct/primitive call arguments in written order, and
  effect sequencing through `__io_bind`.
- `BackendLLVMSpec` now includes the focused native-run written-order proof for
  nested `__io_bind` / `__io_putStrLn` actions and the row passed with
  stdout `first\nsecond\nthird\n`.
- The exact repository guard
  `primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary`
  passed with `1 example, 0 failures` across the five required contract
  surfaces.
- The mechanism-table gate passed with rows 1-5 at `YES`, rows 6-7 at `NO`,
  and the next live blocker pointed at milestone 6 / row 6.
- The full repo gate passed:
  `cabal build all && cabal test` reported `2343 examples, 0 failures` in
  `316.8670 seconds` (`real 319.42` via the timing wrapper).
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
  milestone-6 through milestone-7 remain intentionally open and rows 6-7 stay
  `NO` in the mechanism table;
  the accepted result freezes only the current closed primitive/runtime-binding
  set and eager sequencing contract, so any broader primitive surface, FFI
  expansion, public lowering surface, or polymorphism-lowerability widening
  will need new evidence rather than piggybacking on this row-5 closure.

## Outcome

The round satisfies the `milestone-5` verification contract and is approved as
`accepted + finalize`.
