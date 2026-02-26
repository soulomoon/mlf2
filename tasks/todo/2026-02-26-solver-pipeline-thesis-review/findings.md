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
  - Stage-specific mutation semantics:
    - Thesis solve/presolution stage *does* alter the graph while solving edges (`papers/these-finale-english.txt:12240-12255`).
    - Thesis translation stage uses propagation witnesses and should be read-only with respect to `χp` (`papers/these-finale-english.txt:16439-16443`, `17804-17813`).
    - Code matches this split for GraphOps placement: GraphOps mutate in presolution/validation (`src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs:151-159`, `src/MLF/Constraint/Presolution/Validation.hs:123-188`) and are not used by elaboration Φ modules.
  - Thesis verification for “unaltered graph + equivalence-class projection”:
    - Not true as a general solve statement: SolveConstraint mutates the current constraint each iteration; thesis explicitly says nodes of `χ` are changed by propagation/unification (`papers/these-finale-english.txt:12295-12303`).
    - Constraint quotienting in Def. 9.2.1 is only for solved self unification edges (`papers/these-finale-english.txt:9636-9637`), not full unification-class projection.
    - Merge/fusion is itself defined via quotient constructions over nodes/binding structure (`papers/these-finale-english.txt:3713-3723`), i.e. graph transformation.
