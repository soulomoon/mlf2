# Findings & Decisions

## Request Summary
- User asked for an audit of duplicated logic in the codebase and opportunities for better abstractions.

## Discovery Notes
- Prior session catchup reported unsynced context from unrelated Phase 7 bug investigation task.
- `git diff --stat` returned no unstaged file changes in workspace.

## Candidate Duplication Areas
- **P1 Binding-path traversal duplicated in multiple modules**
  - `src/MLF/Binding/Queries.hs:88`
  - `src/MLF/Binding/Validation.hs:357`
  - `src/MLF/Constraint/Presolution/Base.hs:300`
  - Pattern: `bindingPathToRoot*` traversal logic appears in near-identical forms.
  - Abstraction: centralize into a shared `MLF.Binding.Path` API with pure and monadic variants.
- **P1 Scope graph reconstruction helpers duplicated**
  - `src/MLF/Binding/Tree.hs:126`
  - `src/MLF/Binding/Validation.hs:468`
  - Pattern: `buildTypeEdgesFrom`, `buildScopeNodesFromPaths`, `rootsForScope` duplicated.
  - Abstraction: `MLF.Binding.ScopeGraph` for shared graph derivation primitives.
- **P1 Bound-children collection duplicated**
  - `src/MLF/Binding/Tree.hs:239`
  - `src/MLF/Binding/Validation.hs:428`
  - Pattern: `collectBoundChildrenWithFlag` and wrappers repeated with same filtering flow.
  - Abstraction: shared `MLF.Binding.Children` helper module.
- **P1 NodeRef enumeration/existence helpers duplicated**
  - `src/MLF/Binding/Queries.hs:45`
  - `src/MLF/Binding/Validation.hs:56`
  - `src/MLF/Binding/Canonicalization.hs:32`
  - Pattern: `allNodeRefs` / `nodeRefExists` repeated in multiple files.
  - Abstraction: central `MLF.Binding.NodeRefs`.
- **P1 Constraint generation scope/binder wiring duplicated**
  - `src/MLF/Frontend/ConstraintGen/Translate.hs:102`
  - `src/MLF/Frontend/ConstraintGen/Translate.hs:163`
  - `src/MLF/Frontend/ConstraintGen/Translate.hs:508`
  - Pattern: repeated `pushScope`/`popScope`/`rebindScopeNodes` plus `setBindParentOverride` sequences.
  - Abstraction: `withScopedBinder` and `attachUnder` helpers in `Translate` or a small internal utility module.
- **P2 AnnExpr cata traversals repeated across Elab run helpers**
  - `src/MLF/Elab/Run/Annotation.hs:18`
  - `src/MLF/Elab/Run/Annotation.hs:33`
  - `src/MLF/Elab/Run/Debug.hs:43`
  - Pattern: repeated constructor-by-constructor `cata` traversal for annotation rewrites/extraction.
  - Abstraction: reusable `foldAnnExpr`/`traverseAnnExpr`.
- **P2 Pipeline test harness steps duplicated**
  - `test/PipelineSpec.hs:378`
  - `test/ElaborationSpec.hs:405`
  - `test/ConstraintGenSpec.hs:714`
  - Pattern: repeated `generateConstraints -> checkAcyclicity -> computePresolution -> solveUnify` chain.
  - Abstraction: shared test harness helpers (e.g., `runToPresolution`, `runToSolved` in `test/SpecUtil.hs`).
- **P3 Repeated small test helpers**
  - `test/ElaborationSpec.hs:108`
  - `test/PipelineSpec.hs:368`
  - Pattern: duplicated `unsafeNormalize` and `firstShow`.
  - Abstraction: move to `test/SpecUtil.hs` and reuse.

## Decisions
| Decision | Rationale |
|----------|-----------|
| Focus on substantial logic duplication, not generated/simple glue code | Improves signal quality and actionability |
| Prioritize semantic-core duplicates over test-only cleanup | Core duplication impacts correctness and paper-faithfulness risk more directly |

## References
- AGENTS.md repository workflow and module map
- Explorer audit (Constraint/Binding, Elab/Frontend, test/public/app)
