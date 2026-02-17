# Progress Log: BUG-2026-02-14-003

## 2026-02-14

- Initialized task tracking files.
- Baseline checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1715612721'`
    - Result: `2 examples, 2 failures` (bottomized type mismatch).
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1715612721'`
    - Result: `4 examples, 0 failures`.

- Inspected active deltas and probes:
  - `git diff` on `EdgeProcessing/Witness.hs`, `Phi/Omega.hs`, `Run/Pipeline.hs`.
  - Ran `/tmp/bug003_pres_probe.hs`, `/tmp/bug003_pipeline_trace.hs` in `cabal repl` to capture edge witness/trace and Phi decisions.

- Iteration notes:
  - Reverted an over-aggressive root-graft bound skip path that was forcing `InstId` on edge-0 and driving bottom collapse.
  - Added/validated explicit-bound `InstApp` handling in `/Volumes/src/mlf4/src/MLF/Elab/Inst.hs` for structural bound matches with `⊥` placeholders.
  - Tried an ALam parameter-source tweak in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`; reverted after it produced `TCLetTypeMismatch` with over-expanded lambda parameter types.

- Root fix implementation:
  - Updated `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs` (`ELet` case) to bind let variables in scope with `schTy` (declared scheme type) after `letSchemeAccepts` succeeds.

- Post-fix verification:
  - `--match "BUG-003-V"` -> `2 examples, 0 failures`.
  - `--match "BUG-004"` -> `4 examples, 0 failures`.
  - `--match "Witness normalization invariants"` -> `10 examples, 0 failures`.
  - `--match "R-"` -> `19 examples, 0 failures`.

- Full gate attempt:
  - Started `cabal build all && cabal test`; build completed, full test run stayed CPU-active without emitting output for several minutes.
  - Terminated and continued with targeted guard suites for this bug-focused pass.
