# Findings

- Initialized review task for Task 7 compliance re-check after commit `6c00c8c42dc65b216ce6b2cf8a5cd3573bb8dda4`.
- `6c00c8c4` adds only one test at `test/ElaborationSpec.hs:1662` and does not modify implementation code.
- Task 7 implementation branch from `8fad0bb` remains in `src/MLF/Elab/Phi/Omega.hs:780-792`:
  - trace-source unresolved OpRaise target -> `PhiInvariantError`.
  - non-trace-source unresolved OpRaise target -> no-op (`go ... rest`).
- New regression test asserts the intended error path:
  - calls `phiFromEdgeWitnessWithTrace` with `OpRaise sourceKey` where source key is in trace binder sources and replay target is non-existent.
  - expects `Left (PhiInvariantError msg)` containing `"trace/replay binder key-space mismatch (OpRaise unresolved trace-source target)"`.
- Focused matcher buckets on `HEAD`:
  - `OpRaise`: 22 examples, 2 failures.
  - `A6`: 5 examples, 5 failures.
  - `BUG-002`: 5 examples, 5 failures.
  - `BUG-003`: 3 examples, 3 failures.
  - `MissingEdgeTrace`: pass.
- Causality check on parent `8fad0bb`:
  - Same failing subsets and same failure classes (`WitnessNormalizationError (ReplayMapIncomplete ...)`) in `A6`, `BUG-002`, `BUG-003`, and `OpRaise` buckets.
  - `MissingEdgeTrace` passes.
  - New `OpRaise` regression test is absent there (so `OpRaise` bucket count is 21 instead of 22).
