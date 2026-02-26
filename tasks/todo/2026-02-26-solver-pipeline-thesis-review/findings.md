# Findings

## 2026-02-26
- Thesis baseline:
  - `papers/these-finale-english.txt:12240-12255` defines `SolveConstraint`: unification + per-edge propagation/unification, outputting a presolution.
  - `papers/these-finale-english.txt:16434-16443`, `17386-17390`, `18038-18043` define elaboration as translation of a translatable presolution (`χp`) via propagation witnesses (`χe_p ⊑ χp`), i.e. presolution-first.
- Code pipeline:
  - `src/MLF/Elab/Run/Pipeline.hs:81-87` runs normalize → acyclicity → `computePresolution` → `solveUnifyWithSnapshot`.
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs:15-24,149-159` executes Ω/GraphOps during presolution edge processing, before the solve call above.
- Review outcome:
  - No evidence that presolution GraphOps are executed after solve; they run inside Phase 4.
  - Thesis-alignment risk remains because elaboration/Φ consumes a `Solved` view produced after `solveUnifyWithSnapshot` (`src/MLF/Elab/Run/Pipeline.hs:134-136`, `src/MLF/Elab/Phi/Translate.hs:67-71`), whereas thesis translation is specified directly from presolution `χp`.
  - Documentation inconsistency: `Solved.originalConstraint` is documented as “pre-solving” (`src/MLF/Constraint/Solved.hs:338-341`) but is built from `snapPreRewriteConstraint` after solve loop convergence (`src/MLF/Constraint/Solve.hs:291-299`, `src/MLF/Constraint/Solved.hs:155-162`).
