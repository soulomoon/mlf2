# Round 227 Attempt 1

Date: 2026-05-03
Round: `round-227`
Milestone: `milestone-6`
Direction: `direction-6a-freeze-polymorphism-lowerability-contract`

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
- The docs and backend module notes now agree that checked `Backend.IR` may
  carry `BackendTyAbs` / `BackendTyApp`, checked-program conversion may
  preserve them, and LLVM/native lowering owns only the specialization-based
  lowerable subset.
- The only behavior hardening in the lowerer is the narrow
  `unspecialized polymorphic binding` rejection for residual zero-arity
  polymorphic value escape paths; no runtime polymorphism, fallback lowering,
  public lowering API, second IR, or lazy rescue was added.
- `BackendLLVMSpec` now includes the focused
  `polymorphism lowerability contract` section covering:
  supported top-level complete type application,
  supported local/closure-sensitive specialization,
  supported first-class polymorphic lowering,
  qualified closure-entry specialization, and
  explicit rejection diagnostics for polymorphic `main`, unspecialized
  polymorphic bindings, escaping type abstraction / escaping polymorphic
  bindings, and partial type application.
- The exact repository guard
  `polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary`
  passed with `1 example, 0 failures`.
- The mechanism-table gate passed with rows 1-6 at `YES`, row 7 at `NO`, and
  the next live blocker pointed at milestone 7 / row 7.
- The full repo gate passed:
  `cabal build all && cabal test` reported `2352 examples, 0 failures` and the
  suite finished in `331.5107 seconds`.
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
  milestone 7 remains intentionally open and row 7 stays `NO` in the
  mechanism table;
  the accepted result freezes only the current specialization-based row-6
  lowerability boundary, so any broader runtime-polymorphism support, public
  lowering surface, fallback lowering lane, or second executable IR will need
  new evidence and a later accepted round or roadmap revision.

## Outcome

The round satisfies the `milestone-6` verification contract and is approved as
`accepted + finalize`.
