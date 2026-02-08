# Findings & Decisions

## Request Summary
- User asked for detailed plans for the identified duplication opportunities, grouped so work can be parallelized.

## Inputs
- Prior audit findings in `tasks/archive/2026-02-08-duplicate-logic-audit/findings.md`
- Backlog tracking item `A7 (P2)` in `TODO.md`

## Candidate Grouping Strategy
- **Group 1: Binding core abstractions**
  - Consolidate duplicated path/node/scope/children helper logic.
  - Main files: `src/MLF/Binding/*.hs`, `src/MLF/Constraint/BindingUtil.hs`, `src/MLF/Constraint/Presolution/Base.hs`.
- **Group 2: Frontend/Elab abstractions**
  - Deduplicate scope push/pop/rebind and bind-parent wiring in `ConstraintGen.Translate`.
  - Deduplicate repeated `AnnExpr` traversals between `Elab.Run.Annotation` and `Elab.Run.Debug`.
- **Group 3: Test harness dedup**
  - Centralize repeated test helpers (`unsafeNormalize`, `firstShow`, solve-chain plumbing) in `test/SpecUtil.hs`.
  - Migrate `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`, and `test/ConstraintGenSpec.hs`.

## Authored Plan Files
- `docs/plans/2026-02-08-a7-parallelization-overview.md`
- `docs/plans/2026-02-08-a7-group-1-binding-core-abstractions-implementation-plan.md`
- `docs/plans/2026-02-08-a7-group-2-frontend-elab-abstractions-implementation-plan.md`
- `docs/plans/2026-02-08-a7-group-3-test-harness-dedup-implementation-plan.md`

## Decisions
| Decision | Rationale |
|----------|-----------|
| Keep group boundaries aligned to directory ownership (`Binding`, `Frontend/Elab`, `test`) | Reduces merge conflicts and review overhead |
