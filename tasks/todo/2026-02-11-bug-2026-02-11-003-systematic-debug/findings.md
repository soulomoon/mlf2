# Findings

## 2026-02-11
- Target bug: `BUG-2026-02-11-003` from `Bugs.md`.
- Symptom signatures:
  - V2: `PhiInvariantError "PhiReorder: missing binder identity at positions [0,1]"`
  - V4: `TCInstantiationError ... "InstBot expects TBottom, got Int -> Int"`
- Suspected areas listed in bug tracker:
  - `src/MLF/Elab/Phi/Omega.hs`
  - `src/MLF/Elab/Run/ResultType/Ann.hs`
  - `src/MLF/Elab/Inst.hs`
