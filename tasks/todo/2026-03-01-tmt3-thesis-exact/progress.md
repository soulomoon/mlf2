# Progress — 2026-03-01 TMT3 Wave 1

## Pod A
- Initialized task folder and planning files.
- Scanned owned Phi modules and targeted specs.
- Updated tests first (RED):
  - `test/Phi/IdentityBridgeSpec.hs`
  - `test/ElaborationSpec.hs`
- Implemented strict-source changes (GREEN):
  - `IdentityBridge`: removed canonical-alias matching from source key and binder-index resolution.
  - `Translate`: replay-map codomain/domain validation now raw replay key-space only.
  - `Omega`: removed canonical-based binder acceptance in replay-target resolution.
- Follow-up fix for parity regressions:
  - Added constrained OpGraft fallback for replay-safe recovery.
  - Removed canonical-equality shortcuts in merge/raise-merge decisions.
  - Added focused C4 locks in `test/Phi/AlignmentSpec.hs`.
- Validation:
  - Focused Pod A tests passed (33 examples, 0 failures).
  - Full suite `cabal test mlf2-test` passed (903 examples, 0 failures).
  - `rg -n "Solved\\.canonical" src/MLF/Elab/Phi` returned no matches.

## Pod C
- Confirmed branch and worktree ownership.
- Ran required audit command `rg -n "rewriteConstraintWithUF" src/MLF/Elab/Run src/MLF/Constraint/Presolution`; found one runtime call in pipeline.
- Added lock assertion in `test/PipelineSpec.hs` that pipeline runtime path does not reference `rewriteConstraintWithUF`.
- Replaced boundary rewrite in pipeline with snapshot replay (`solveResultFromSnapshot` + `SolveSnapshot`) and rebuild from canonical solved constraint.
- Updated `test/SolveSpec.hs` snapshot test to assert replayed solve result equality.
- Validation:
  - Required grep command returned no matches in owned runtime directories.
  - Targeted Pod C tests passed (45 examples, 0 failures).

## Wave 1 Integration
- Created `codex/tmt3-wave1-integration`.
- Merged `codex/tmt3-phi-source-domain-wave1`.
- Merged `codex/tmt3-solve-no-rewrite-layer-wave1`.
- Resolved add/add task-file conflicts by consolidating pod logs.
- Pending: full integration gate (`cabal build all && cabal test`) and focused acceptance checks.

## Pod D (Wave 2)
- Branch/worktree: `codex/tmt3-elab-direct-chi-wave2`.
- Added/kept pipeline invariant test ensuring run path does not reference `Solved.fromPresolutionResult` or `ResultTypeContext`.
- Refactored runtime solved construction to snapshot replay via `Solved.fromPreRewriteState` with local UF sanitization.
- Renamed run result-type boundary record `ResultTypeContext` -> `ResultTypeInputs` across `src/MLF/Elab/Run/ResultType*` and pipeline call sites.
- Retired active dual-path guardrail wiring by removing `DualPathSpec` from `test/Main.hs` and `mlf2.cabal` test-suite modules.
- Validation slices:
  - `Pipeline (Phases 1-5)`: 56 examples, 0 failures.
  - `Phase 5 -- Solve`: 44 examples, 0 failures.
