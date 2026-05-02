# Round 227 Implementation Notes

## Change summary

- Published the row-6 polymorphism/lowerability contract across the durable backend surfaces:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`.
- Kept checked `Backend.IR` permissive for `BackendTyAbs` / `BackendTyApp`, while making LLVM/native lowering explicitly own only the specialization-based lowerable subset.
- Added a narrow lowerer hardening path so residual zero-arity polymorphic values fail with the explicit `unspecialized polymorphic binding` diagnostic instead of falling through to a broader escaping lane.
- Added a dedicated `BackendLLVMSpec` `polymorphism lowerability contract` section covering:
  supported top-level complete type application,
  supported local/closure-sensitive specialization,
  supported first-class polymorphic lowering,
  qualified closure-entry specialization,
  and explicit rejection diagnostics for polymorphic main binding, unspecialized polymorphic binding, escaping type abstraction / polymorphic binding, and partial type application.
- Added the repository guard `polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary`.
- Updated only mechanism-table row 6 to `YES`, kept rows 1-5 at `YES`, kept row 7 at `NO`, and pointed the next action at milestone 7 / row 7.

## Changed files

- `docs/architecture.md`
- `docs/backend-native-pipeline.md`
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
- `src/MLF/Backend/Convert.hs`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `test/BackendLLVMSpec.hs`
- `test/RepoGuardSpec.hs`

## Verification

1. `git diff --check`
   Outcome: passed with no output.

2. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/polymorphism lowerability contract/"'`
   Outcome: passed.
   Detail: `8 examples, 0 failures`.

3. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary/"'`
   Outcome: passed.
   Detail: `1 example, 0 failures`.

4. `cabal build all && cabal test`
   Outcome: passed.
   Detail: full build completed; full test suite finished in `331.7310 seconds` with `2352 examples, 0 failures`.

## Residual risk

- The row-6 freeze remains intentionally narrow: runtime polymorphism is still unsupported, and row 7 synchronization/closeout work is still open.
- The new repo guard is marker-based by design; future wording changes across the five contract surfaces will need coordinated updates so the explicit row-6 boundary stays synchronized.
