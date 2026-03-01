# Findings — 2026-03-01 TMT3 Wave 1

## Pod A (`phi-source-domain`)
- `Solved.canonical`-driven alias fallback in Phi path was load-bearing in `Translate`, `Omega`, and `IdentityBridge`.
- Strict replay-map codomain validation now rejects targets that only canonicalize into replay binder domain.
- `IdentityBridge` now keeps key operations in witness raw key-space and drops canonical-alias binder lookup fallback.
- `OpGraft` now resolves non-root binders via strict-first replay-space checks, with constrained recovery only from replay-safe mappings.
- Merge and raise-merge decisions now use replay/raw identity comparisons instead of canonical-equality shortcuts.

## Pod C (`solve-no-rewrite-layer`)
- One runtime boundary call existed in pipeline (`rewriteConstraintWithUF`).
- Pipeline now rebuilds solved constraint via snapshot replay (`solveResultFromSnapshot`) instead of directly calling rewrite helper.
- Solver snapshot contract is explicitly tested for replay equality and UF rewrite equivalence.
- Required runtime grep invariant for `rewriteConstraintWithUF` in `src/MLF/Elab/Run` and `src/MLF/Constraint/Presolution` is now clean.

## Integration Notes
- Wave 1 merge conflicts were limited to task tracking files; code merged without semantic conflicts.
- Wave 1 is ready for integration gate execution.

## Pod D (`elab-direct-chi`, Wave 2)
- Runtime run-path no longer depends on `Solved.fromPresolutionResult`; pipeline now projects solved state from presolution snapshots through `Solved.fromPreRewriteState` after local live-node UF sanitization.
- `ResultTypeContext` boundary identifier was removed from `src/MLF/Elab/Run/*`; run-path result-type API now uses `ResultTypeInputs`.
- `DualPathSpec` is no longer wired as an active guardrail in `test/Main.hs` / `mlf2.cabal`; single-path pipeline invariants remain covered by `PipelineSpec`.
