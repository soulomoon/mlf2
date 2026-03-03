# Findings

## 2026-03-01 Campaign Baseline
- Campaign initialized and baseline captured.
- Baseline full build/test and targeted TMT3 slices were green.
- Workspace had a pre-existing local edit in `docs/notes/2026-02-27-transformation-mechanism-table.md` before this campaign.

## Wave 1 Pod Findings

### Pod A (`phi-source-domain`)
- `Solved.canonical`-driven alias fallback in Phi path was load-bearing in `Translate`, `Omega`, and `IdentityBridge`.
- Strict replay-map codomain validation now rejects targets that only canonicalize into replay binder domain.
- `IdentityBridge` now keeps key operations in witness raw key-space and drops canonical-alias binder lookup fallback.
- `OpGraft` resolves non-root binders via strict-first replay-space checks, with constrained recovery only from replay-safe mappings.
- Merge and raise-merge decisions use replay/raw identity comparisons instead of canonical-equality shortcuts.

### Pod C (`solve-no-rewrite-layer`)
- A runtime boundary call to `rewriteConstraintWithUF` existed in pipeline.
- Pipeline now rebuilds solved constraint via snapshot replay (`solveResultFromSnapshot`) instead of directly calling rewrite helper.
- Solver snapshot contract is explicitly tested for replay equality and UF rewrite equivalence.
- Required runtime grep invariant for `rewriteConstraintWithUF` in `src/MLF/Elab/Run` and `src/MLF/Constraint/Presolution` is clean.

## Wave 2 Pod Findings

### Pod B (`omega-thesis-order`)
- Non-replay witness pruning was moved into `WitnessNorm`; runtime semantic path no longer depends on `stripForNonReplay` helper boundaries.
- Compatibility exports/imports for `stripForNonReplay` were removed.
- Pipeline now enforces canonical solved rebuild validity with `validateCanonicalGraphStrict` after replayed reconstruction.

### Pod D (`elab-direct-chi`)
- Runtime run-path no longer depends on `Solved.fromPresolutionResult`; solved input derives from presolution snapshot reconstruction.
- `ResultTypeContext` boundary identifier was removed from run-path API in favor of `ResultTypeInputs`.
- Active `DualPathSpec` wiring was retired from `test/Main.hs` and `mlf2.cabal`; single-path behavioral invariants are covered in `PipelineSpec`.

## Integration Notes
- Wave 1 and Wave 2 merge conflicts were limited to task tracking files.
- Code-path integrations are preserved across wave branches and ready for final docs closeout + final gate.

## Wave 3 Pod Findings (`docs-closeout`)
- Transformation Mechanism Table is fully aligned on this branch; DEV-TMT row labels were retired from table alignment cells.
- `docs/thesis-deviations.yaml` now keeps only non-TMT active deviations, while all `DEV-TMT-*` entries are recorded as `resolved` with resolution metadata and evidence.
- `implementation_notes.md`, `CHANGELOG.md`, and `TODO.md` were updated to reflect campaign closure as an all-aligned table state.
