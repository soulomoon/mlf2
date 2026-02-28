# Findings — 2026-03-01 update transformation mechanism table

## Initial
- Target doc exists at `docs/notes/2026-02-27-transformation-mechanism-table.md`.
- Existing table appears dated to 2026-02-27 and includes claims that may have changed (e.g., canonical fallback usage and dual-path runtime behavior).

## Audit discoveries
- Current HEAD is `5c68b20`; note still references `17d12c5`.
- Presolution/solve flow is still native-solved-first (`Solved.fromPresolutionResult`) with no runtime legacy-elab path (`src/MLF/Elab/Run/Pipeline.hs`).
- Presolution edge loop still drains unification closure around each inst edge and enforces boundary checks (`src/MLF/Constraint/Presolution/EdgeProcessing.hs`).
- New replay-map normalization details since previous note refresh:
  - `binderArgsFromExpansion` now filters binders to canonical `TyVar` binders (`src/MLF/Constraint/Presolution/Witness.hs`).
  - Witness normalization constructs deterministic replay-map assignments and persists `etBinderReplayMap` (`src/MLF/Constraint/Presolution/WitnessNorm.hs`).
  - Driver validates replay-map source-domain parity and TyVar codomain (`src/MLF/Constraint/Presolution/Driver.hs`).
  - Phi bridge validates replay-map domain against trace binders and projects replay targets by scheme/trace ordering with fail-fast on out-of-domain targets (`src/MLF/Elab/Phi/Translate.hs`).
- Omega still enforces strict non-root `OpWeaken` binder resolution + fail-fast (`src/MLF/Elab/Phi/Omega.hs`).
