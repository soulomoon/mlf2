# Round 224 Attempt 1

Date: 2026-05-02
Round: `round-224`
Milestone: `milestone-3`
Direction: `direction-3a-clarify-direct-vs-closure-callable-shapes`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Attempt Summary

- Scope matched the plan exactly on the nine authorized repo-facing payload files:
  `docs/architecture.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `test/BackendConvertSpec.hs`,
  `test/BackendIRSpec.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- The backend-owned callable classifier now lives in `MLF.Backend.IR`, and both conversion and LLVM lowering consume that shared decision surface instead of preserving lowerer-only callable-shape recovery.
- Malformed or ambiguous closure-headed `BackendApp` forms are rejected explicitly by validator and lowerer diagnostics rather than accepted conventionally.
- The callable-shape repo guard passed with `1 example, 0 failures`.
- Focused backend IR, convert, and LLVM callable-shape slices all passed with `1 example, 0 failures` each.
- The mechanism-table gate passed with rows 1-3 at `YES` and rows 4-7 still `NO`.
- The full repo gate passed: `cabal build all && cabal test` reported `2340 examples, 0 failures` in `315.9484 seconds`.
- Controller-owned files remained limited to the expected roadmap pointer/state dirtiness plus active roadmap/round lineage artifacts; they are not part of the merge payload.

## Merge Readiness

- Status: `satisfied`
- Payload files:
  `docs/architecture.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `test/BackendConvertSpec.hs`,
  `test/BackendIRSpec.hs`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- Residual risks:
  remaining row-4 through row-7 roadmap debt is intentionally still open and remains `NO` in the mechanism table;
  the callable classifier is intentionally conservative for ambiguous callable heads and rejects them rather than widening the row-3 contract.

## Outcome

The round satisfies the `milestone-3` verification contract and is approved as `accepted + finalize`.
