# Round 225 Implementation Notes

## Summary

Implemented the mechanism-table row-4 ADT/case semantic-boundary freeze.

- Synchronized the row-4 ownership marker across
  `docs/architecture.md`, `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`.
- Kept `BackendData`, `BackendConstructor`, `BackendConstruct`, and
  `BackendCase` semantic-only at the `MLF.Backend.IR` boundary.
- Made the existing LLVM/native layout policy explicit as private lowerer
  policy: declaration-order zero-based tags, tag at object offset `0`, fields
  after the tag word, closure-record storage for function-like constructor
  fields, and tag-only heap objects for nullary constructors.
- Strengthened the four localized `BackendLLVMSpec` row-4 examples for tags,
  field offsets, closure-valued constructor fields, and nullary constructors.
- Added the exact repository guard:
  `ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen`.
- Flipped only mechanism-table row 4 to `YES`; rows 1-3 remain `YES`, rows
  5-7 remain `NO`.

## Changed Files

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`
- `orchestrator/rounds/round-225/implementation-notes.md`

## Verification Outcomes

Final required sequence:

- `git diff --check`
  - PASS, exit 0, no output.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/accepts ADT construction and case analysis through constructor metadata/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/recovers explicit backend constructors and cases from checked ADT paths/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers Nat construction and case analysis to heap tags and switch/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/loads only constructor fields used by a case branch/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers source closure-valued constructor fields through the explicit closure ABI/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers nullary and recursive-list constructors through case/"'`
  - PASS, exit 0: `1 example, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`
- Mechanism-table gate:
  - PASS, exit 0. Rows 1-4 evaluate to `YES`; rows 5-7 evaluate to `NO`.
- `cabal build all && cabal test`
  - PASS, exit 0. Full suite summary: `2341 examples, 0 failures`; `1 of 1 test suites (1 of 1 test cases) passed.`

Intermediate fixed outcomes:

- First focused repository-guard attempt exited 1 during test build because
  the new multiline `unlines` assertion in `test/BackendLLVMSpec.hs` parsed as
  an over-applied `isInfixOf`. Fixed by binding the expected nullary block to
  `nullaryTagOnlyBlock`.
- The first rebuild also surfaced `-Wtype-defaults` warnings in the touched
  `src/MLF/Backend/LLVM/Lower.hs` around local `emitMallocLocal` calls. Fixed
  by adding `emitMallocLocal :: String -> Int -> LowerM LLVMOperand`; the final
  full gate emitted no warnings in the touched modules.
- The next two focused repository-guard attempts exited 1 because the new
  native-pipeline row-4 markers were split by Markdown wrapping:
  `"nullary tag-only representation stay private to"` and
  `"function-like constructor fields store explicit closure records"`. Fixed
  by making those synchronized markers contiguous in
  `docs/backend-native-pipeline.md`.

## Residual Risks

- No behavior redesign was attempted. The evidence freezes the current private
  lowerer-owned layout policy rather than proving an alternate layout.
- The strengthened LLVM assertions intentionally depend on deterministic
  emitted LLVM names for the selected row-4 fixtures; this is the requested
  freeze point for the current layout policy.
