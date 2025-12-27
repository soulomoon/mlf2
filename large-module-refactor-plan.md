# Large Module Refactor Plan

Goal: split the remaining “god modules” into smaller, domain-cohesive internal modules without changing behavior, keeping `-Wall` clean and tests green after every step.

This plan is internal-only refactoring: public API stays `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`, `src-public/MyLib.hs`.

## Why (current pain points)

Largest modules today:
- `src/MLF/Constraint/Presolution/Core.hs` (~2300 LOC): driver + χe copying + edge-local unification + witness normalization + forall intro.
- `src/MLF/Elab/Pipeline.hs` (~1400 LOC): reify/generalize + Φ/Σ translation + pipeline runner.
- `src/MLF/Frontend/ConstraintGen.hs` (~700 LOC): AST translation + scope tracking + graph/binding emission.

Even after the repo-wide dedup sweep, these files remain hard to navigate and hard to safely evolve.

## Guardrails

- No algorithm changes: moves/splits only (plus minimal glue code).
- Every intermediate state must compile and keep `cabal --config-file=.cabal-config test --test-show-details=direct` green.
- Avoid new module cycles (notably around `Binding.Tree` ↔ `Solve`).
- Preserve current exports from internal phase entrypoints:
  - `MLF.Constraint.Presolution` and `MLF.Constraint.Presolution.Core` stay stable for tests.
  - `MLF.Elab.Pipeline` stays stable for tests.
  - `MLF.Frontend.ConstraintGen` stays stable for tests.

## Target structure (end state)

### Presolution (`MLF.Constraint.Presolution.*`)

Keep `MLF.Constraint.Presolution` as the facade; shrink `Core` into a small driver + wiring layer.

Proposed submodules:
- `MLF.Constraint.Presolution.Base` (already exists): shared state/error types + binding-tree queries.
- `MLF.Constraint.Presolution.Ops` (new, foundation): stateful “graph ops” (fresh ids, node lookup/registration, UF root chase).
- `MLF.Constraint.Presolution.Unify` (new, foundation): occurs-check + UF merge + binding-parent harmonization (Raise trace).
- `MLF.Constraint.Presolution.Witness`:
  - `witnessFromExpansion`, `binderArgsFromExpansion`, `normalizeInstanceOps`
- `MLF.Constraint.Presolution.ForallIntro`:
  - `introduceForallFromSpec`, `bindForallBindersFromSpec`, bound copying helpers
- `MLF.Constraint.Presolution.Expansion`:
  - `decideMinimalExpansion`, `mergeExpansions`
  - `applyExpansion` + traced variants (`applyExpansionTraced`, `applyExpansionEdgeTraced`)
- `MLF.Constraint.Presolution.Copy`:
  - `instantiateScheme`/`instantiateSchemeWithTrace`, copy-cache + interior accounting, “bind unbound copied nodes”
- `MLF.Constraint.Presolution.EdgeUnify`:
  - `EdgeUnifyState`, `runEdgeUnifyForTest`, edge-local unify (`unifyAcyclicEdge*`, `unifyStructure*`)
- `MLF.Constraint.Presolution.Driver` (optional): `computePresolution`, `processInstEdge`, “materialize expansions / rewrite constraint” glue.

Dependency rule of thumb:
`Base`/`Ops` are leaf-ish; everything else depends on them; only the driver depends on everything else.

### Elaboration (`MLF.Elab.*`)

Keep `MLF.Elab.Pipeline` as the facade; split by responsibility:
- `MLF.Elab.Reify` (graph → `ElabType`), currently duplicated “reify” code paths.
- `MLF.Elab.Generalize` (compute schemes at binders).
- `MLF.Elab.Phi` (Φ translation from witnesses).
- `MLF.Elab.Sigma` (Σ reorderings).
- `MLF.Elab.Run` (pipeline orchestration).

### Frontend (`MLF.Frontend.*`)

Keep `MLF.Frontend.ConstraintGen` as the facade; split:
- `MLF.Frontend.ConstraintGen.State` (BuildState + monad runner).
- `MLF.Frontend.ConstraintGen.Scope` (scope stack and rebind logic).
- `MLF.Frontend.ConstraintGen.Emit` (node/edge/bind-parent/bound emission helpers).
- `MLF.Frontend.ConstraintGen.Translate` (Expr → graph translation).

## Execution plan (incremental)

### Phase P — Presolution (start here)

[x] P0. Extract low-level state ops (foundation)
- Add `MLF.Constraint.Presolution.Ops` and move:
  - fresh id allocation, node lookup/registration, bind-parent setter, var-bound helpers, UF root chase.
- Keep exports from `MLF.Constraint.Presolution.Core` unchanged (it imports Ops).

[x] P1. Extract witness + normalization helpers
- Move expansion→witness helpers into `MLF.Constraint.Presolution.Witness`:
  - `witnessFromExpansion`, `binderArgsFromExpansion`, `forallIntroSuffixCount`
- Move witness normalization helpers into `MLF.Constraint.Presolution.Witness`:
  - `integratePhase2Ops`, `normalizeInstanceOps`, `coalesceRaiseMerge`
- Keep `Core` behavior unchanged (it imports these helpers).

[x] P2. Extract forall introduction materialization
- Move `introduceForallFromSpec` + binder selection/binding into `MLF.Constraint.Presolution.ForallIntro`.

[x] P3. Extract expansion decision + application
- Moved `decideMinimalExpansion`, `applyExpansion*`, and `mergeExpansions` into `MLF.Constraint.Presolution.Expansion`.
- Extracted UF/occurs-check unification into `MLF.Constraint.Presolution.Unify` so expansion merge doesn’t depend on `Core`.

[x] P4. Extract copy/instantiate machinery (folded into Expansion)
- Moved `instantiateScheme*`, χe copying, and binding-root helpers into `MLF.Constraint.Presolution.Expansion` (no separate `Copy` module yet).

[x] P5. Extract edge-local unification
- Moved `EdgeUnifyState`, `runEdgeUnifyForTest`, `mkOmegaExecEnv`, and `unifyAcyclicEdge*` / `unifyStructureEdge` into `MLF.Constraint.Presolution.EdgeUnify`.

[x] P6. Shrink `Core` into a thin facade
- Moved the remaining driver logic into `MLF.Constraint.Presolution.Driver` and kept `MLF.Constraint.Presolution.Core` as a compatibility re-export.

### Phase E — Elaboration

[x] E0. Extract `Reify` (pure-ish, low risk)
- Added `MLF.Elab.Reify` and moved `reifyType` / `reifyTypeWithNames` (plus `freeVars`) out of `MLF.Elab.Pipeline`.
[x] E1. Extract `Sigma` (already partially deduped)
- Added `MLF.Elab.Inst` (instantiation helpers) and `MLF.Elab.Sigma` (Σ reorderings), re-exporting via `MLF.Elab.Pipeline`.
[x] E2. Extract `Phi` (witness translation)
- Added `MLF.Elab.Phi` and moved:
  - `contextToNodeBound` / `contextToNodeBoundWithOrderKeys`
  - `phiFromEdgeWitness` / `phiFromEdgeWitnessWithTrace`
- Moved `SchemeInfo` into `MLF.Elab.Types` to avoid module cycles.
[x] E3. Extract `Generalize`
- Added `MLF.Elab.Generalize` and moved `generalizeAt` out of `MLF.Elab.Pipeline`.
[x] E4. Keep `Pipeline` as facade
- Added `MLF.Elab.Elaborate` (term elaboration + `expansionToInst`) and `MLF.Elab.Run` (Phase 1–5 runner + redirects).
- `MLF.Elab.Pipeline` now re-exports the stable internal API and stays lightweight.

### Phase F — Frontend

[x] F0. Extract `Scope` (pure state operations)
- Added `MLF.Frontend.ConstraintGen.Types` and `MLF.Frontend.ConstraintGen.State` as foundations.
- Added `MLF.Frontend.ConstraintGen.Scope` and moved the scope stack + `rebindScopeNodes`.
[x] F1. Extract `Emit` (nodes/edges/binds)
- Added `MLF.Frontend.ConstraintGen.Emit` and moved node allocation + inst-edge emission + default bind-parenting helpers.
[x] F2. Extract `Translate` (Expr traversal)
- Added `MLF.Frontend.ConstraintGen.Translate` and moved the `Expr` traversal + annotation internalization helpers out of `MLF.Frontend.ConstraintGen`.
[x] F3. Keep `ConstraintGen` as facade
- `MLF.Frontend.ConstraintGen` now contains only `generateConstraints` wiring and re-exports the public Phase-1 types.

## Validation

- After every checkbox:
  - `cabal --config-file=.cabal-config test --test-show-details=direct`
- For dependency sanity (avoid cycles):
  - `cabal --config-file=.cabal-config build`

## Risks

- Accidental module cycles (especially around Solve/Binding/Order): mitigate by keeping “pure algorithms” in leaf modules (as with `MLF.Util.OrderKey`).
- Changing strictness by accident: prefer moving code verbatim; avoid changing `StateT` strictness/monad stacks while splitting.
