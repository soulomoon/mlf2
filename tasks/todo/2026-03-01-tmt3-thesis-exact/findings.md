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

## Integration Notes
- Wave 1 merge conflicts were limited to task tracking files; code merged without semantic conflicts.
- Wave 1 is ready/used as the base for Wave 2 integration and final closeout.
