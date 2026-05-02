# Round 226 Implementation Notes

## Summary

Implemented the mechanism-table row-5 primitive-operation and eager-evaluation-order freeze.

- Synchronized the row-5 contract markers across `docs/architecture.md`,
  `docs/backend-native-pipeline.md`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and `src/MLF/Backend/LLVM/Lower.hs`.
- Kept the primitive surface closed at the existing reserved runtime-binding
  set: `__mlfp_and`, `__io_pure`, `__io_bind`, and `__io_putStrLn`.
- Kept those primitives represented through the existing
  `BackendVar` / `BackendApp` / `BackendTyApp` forms, with no new
  `BackendPrim`, no second executable IR, and no backend-boundary widening.
- Published the eager sequencing contract explicitly:
  let RHS before body, case scrutinee before branch selection,
  direct/primitive call arguments in written order, and effect sequencing
  through `__io_bind`.
- Strengthened the localized `BackendLLVMSpec` primitive evidence, including a
  native-run row proving nested `__io_bind` / `__io_putStrLn` actions execute
  in written order.
- Added the exact repository guard:
  `primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary`.
- Flipped only mechanism-table row 5 to `YES`; rows 1-4 remain `YES`, and rows
  6-7 remain `NO` with the next action pointed at milestone 6 / row 6.

## Changed Files

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`
- `orchestrator/rounds/round-226/implementation-notes.md`

## Verification Outcomes

Final required sequence:

- `git diff --check`
  - PASS, exit 0, no output.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/IO backend contract/"'`
  - PASS, exit 0: `7 examples, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/preserves referenced Prelude bindings and lowers runtime primitive calls/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal build all && cabal test`
  - PASS, exit 0. Full suite summary: `2343 examples, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`

## Intermediate Fixed Outcomes

- The first two focused IO-slice attempts exited 1 because the new nested
  source fixture used primitive continuations that the source-to-backend path
  parsed or typed differently than intended:
  first `BackendApplicationExpectedFunction`, then
  `BackendApplicationArgumentMismatch` / backend typecheck mismatch. Fixed by
  rewriting `ioNestedPrimitiveMainProgram` into a helper-based nested
  `__io_bind` / `__io_putStrLn` chain that still proves written-order native
  execution with stdout `first\nsecond\nthird\n`.
- The first focused repository-guard attempt exited 1 because
  `docs/architecture.md` split the marker `no new \`BackendPrim\`` across
  lines. Fixed by making that row-5 marker contiguous in the docs surfaces.
- The next focused repository-guard attempt exited 1 because
  `closed reserved runtime-binding set` was wrapped across several row-5
  contract owners. Fixed by making that synchronized marker contiguous across
  the docs/module-note surfaces.

## Residual Risks

- The new native sequencing row freezes the current explicit IO primitive lane;
  it does not widen the backend to broader primitive sets, fallback execution,
  or row-6 polymorphism-lowerability claims.
- The row-5 guard intentionally depends on exact synchronized wording in the
  five contract-owner surfaces so future drift fails loudly.
