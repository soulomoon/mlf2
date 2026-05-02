# Round 224 Implementation Notes

## Changed files

- `docs/architecture.md`
  - Published the direct-vs-closure callable contract at the backend boundary.
- `src/MLF/Backend/IR.hs`
  - Added the shared backend-owned callable classifier surface:
    `BackendCallableBindingKind`, `BackendCallableHead`, and `backendCallableHead`.
  - Tightened validation diagnostics so direct-call/closure-call misuse reports the violated callable invariant explicitly.
  - Reused the shared classifier in backend validation and closure-shape tracking.
- `src/MLF/Backend/Convert.hs`
  - Switched callable emission to the shared backend callable contract so direct first-order locals stay on `BackendApp` and closure-valued aliases/captures/case-let selections emit `BackendClosureCall`.
  - Tightened the conversion boundary note to keep lazy runtime and lowerer-private machinery out of scope.
- `src/MLF/Backend/LLVM/Lower.hs`
  - Consumed the shared callable classifier in lowering.
  - Removed lowerer acceptance of malformed `BackendApp` heads that resolve to closure values after let/case peeling.
  - Preserved explicit rejection when malformed backend IR reaches lowering.
- `test/BackendIRSpec.hs`
  - Added callable-shape success/rejection coverage for explicit closure calls and direct-vs-closure misuse diagnostics.
- `test/BackendConvertSpec.hs`
  - Extended conversion coverage so function-valued case fields stay on the closure-call path instead of surfacing as ambiguous direct applications.
- `test/BackendLLVMSpec.hs`
  - Replaced positive acceptance of malformed `BackendApp` closure heads with explicit rejection coverage.
  - Updated representative fixtures to use the explicit closure-call path.
- `test/RepoGuardSpec.hs`
  - Added the exact callable-shape contract guard and synchronized the eager-runtime/lowering wording markers.
- `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
  - Flipped only row 3 (`Direct calls, closure values, and callable shapes`) to `YES` and pointed the next action at milestone 4 / row 4.

Controller-owned orchestrator files were already dirty in the worktree and were left untouched. This artifact is the only orchestrator-owned file added by the round.

## Verification commands and results

- `git diff --check`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/rejects malformed closure IR/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/keeps direct first-order local calls on the direct application path/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/classifies function-valued case pattern fields as closure locals/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers case-selected closure callees through the explicit closure ABI/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers let-selected closure callees through the explicit closure ABI/"'`
  - Pass.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/rejects BackendApp heads that select closure values through let or case/"'`
  - Pass.
- Mechanism-table gate script from `orchestrator/rounds/round-224/plan.md`
  - Pass. Rows 1-3 evaluate to `YES`; rows 4-7 evaluate to `NO`.
- `cabal build all`
  - Pass.
  - GHC emitted pre-existing `-Wtype-defaults` warnings in `src/MLF/Backend/LLVM/Lower.hs` at lines `938-939`, `984-985`, and `1038-1039`.
- `cabal test`
  - Final rerun passed: `2340 examples, 0 failures` in `314.2610s`.

## In-round failure evidence resolved before the final gate

- An earlier `cabal test` run failed while the round was in progress because:
  - `test/BackendLLVMSpec.hs` still contained representative fixtures that encoded malformed `BackendApp` closure heads, and
  - `test/RepoGuardSpec.hs` exact-marker checks still disagreed with the updated contract notes.
- A focused rerun of
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope/"'`
  exposed one final line-wrap mismatch for the marker `Raw LLVM emission and native emission`.
- Those mismatches were fixed in the authorized write scope above; the focused guard, full build, and full test reruns then passed.

## Residual risk

- The new callable classifier is intentionally conservative for unknown local callable provenance; this round settles row 3 semantics without widening the contract into row 4 ADT/layout ownership or broader backend-shape inference.
- The pre-existing `-Wtype-defaults` warnings in `src/MLF/Backend/LLVM/Lower.hs` remain outside this slice.
