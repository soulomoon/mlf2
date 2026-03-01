# Progress

## 2026-03-01 Wave 0 Baseline
- Created Wave 0 campaign folder and seed planning files.
- Ran baseline gate: `cabal build all` -> PASS.
- Ran baseline gate: `cabal test` -> PASS.
- Ran targeted slice: `--match "Phi alignment" --match "IdentityBridge" --match "replay-map"` -> PASS.
- Ran targeted slice: `--match "Witness normalization invariants" --match "Driver replay-map boundary validation"` -> PASS.
- Ran targeted slice: `--match "Phase 5 -- Solve" --match "single-solved" --match "Dual-path verification"` -> PASS.

## 2026-03-01 Wave 1 Pod A
- Initialized task folder and planning files.
- Updated Phi/Identity tests first, then implemented strict source-domain behavior.
- Removed canonical-alias binder fallback in `IdentityBridge` and stricter replay-space checks in `Translate`/`Omega`.
- Added focused alignment locks and validated targeted Pod A slices.

## 2026-03-01 Wave 1 Pod C
- Audited and removed runtime-path dependency on `rewriteConstraintWithUF`.
- Replaced pipeline boundary rewrite with snapshot replay solve reconstruction.
- Updated solve/pipeline tests to assert replay equivalence and no runtime rewrite helper dependency.
- Validated targeted Pod C slices.

## 2026-03-01 Wave 1 Integration
- Created `codex/tmt3-wave1-integration`.
- Merged `codex/tmt3-phi-source-domain-wave1` and `codex/tmt3-solve-no-rewrite-layer-wave1`.
- Resolved add/add task-file conflicts by consolidating campaign and pod logs.
- Full integration gate and focused acceptance checks passed in wave branch.

## 2026-03-01 Wave 2 Pod B
- Added regression lock that non-replay normalization keeps omega witness ops under the new normalization contract.
- Removed runtime dependence on `stripForNonReplay`; pruning now lives directly in `WitnessNorm` path.
- Removed compatibility exports/imports for `stripForNonReplay` from `Witness` and `WitnessCanon`.
- Preserved strict replay-map fail-fast behavior and replay boundary checks.

## 2026-03-01 Wave 2 Pod D
- Removed runtime run-path dependence on `Solved.fromPresolutionResult` in `src/MLF/Elab/Run`.
- Renamed run result-type boundary record `ResultTypeContext` -> `ResultTypeInputs`.
- Retired active dual-path guardrail wiring in `test/Main.hs` and `mlf2.cabal`; removed orphan `test/DualPathSpec.hs`.
- Replaced textual-only coverage with behavioral snapshot invariants in `PipelineSpec`.
- Updated `SpecUtil` and frozen parity helpers to use snapshot-shaped solved reconstruction (`fromPreRewriteState`).

## 2026-03-01 Wave 2 Integration
- Merged `codex/tmt3-omega-thesis-order-wave2` and `codex/tmt3-elab-direct-chi-wave2`.
- Resolved task-file content conflicts by preserving Wave 0/1/2 evidence in campaign files.
- Wave 2 chain is ready for Wave 3 docs closeout and final gate on final integration branch.
