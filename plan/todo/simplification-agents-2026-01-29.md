# Simplification Agents Plan (2026-01-29)

Goal: reduce LOC and improve readability with behavior preserved. Public API changes are only acceptable when explicitly called out; default to internal-only changes while preserving paper-faithful behavior and keeping `-Wall` clean.

Global guardrails:
- No algorithmic changes unless explicitly called out and documented.
- Prefer behavior-preserving refactors: reduce nesting, remove duplication, simplify data flow.
- Keep module exports explicit; favor small helpers over clever pointfree.
- Verify each agent's changes with `cabal test` (and `cabal build` if module moves/splits).

---

## Priority Focus: `src/MLF/Elab/Run/` (3040 lines total)

The complexity is concentrated in two files:
- **Pipeline.hs** (1181 lines): ~1000-line monolithic `runPipelineElabWith`
- **Generalize.hs** (1252 lines): ~1100-line `constraintForGeneralization` with 5 phases

---

## Agent A — Pipeline Result Type Extraction

**Target**: [Pipeline.hs](src/MLF/Elab/Run/Pipeline.hs) lines 305-585 and 591-1177

**Problem**: `runPipelineElabWith` is ~1050 lines. The bulk is two code paths for computing the result type:
1. `typeFromAnn` (lines 305-585): ~280 lines, handles `AAnn` wrapper case
2. Non-annotation fallback (lines 591-1177): ~580 lines, complex root type inference

Both share significant logic but are inlined as local definitions.

**Plan**:
1. Use/adjust existing `src/MLF/Elab/Run/ResultType.hs` with:
   - `ResultTypeContext` record holding: `solvedForGen`, `bindParentsGa`, `planBuilder`, `edgeWitnesses`, `edgeTraces`, `edgeExpansions`, `redirects`, `baseConstraint`
   - `computeAnnotationType :: ResultTypeContext -> AnnExpr -> NodeId -> EdgeId -> Either String ElabType`
   - `computeRootType :: ResultTypeContext -> AnnExpr -> Either String ElabType`

2. Extract shared helpers:
   - `resolveAnnotationScope` (current lines 312-340)
   - `computePhiFromTarget` (current lines 509-535)
   - `normalizeTargetType` (current lines 419-455)

3. Reduce `runPipelineElabWith` to ~200 lines: setup → elaborate → dispatch to result type.

**Verification**: `cabal test`

---

## Agent B — Pipeline Canonicalization Consolidation

**Target**: [Pipeline.hs](src/MLF/Elab/Run/Pipeline.hs) lines 241-299

**Problem**: The same canonicalization pattern appears 6+ times with `canonOp`, `canonStep`, `canonWitness`, `canonTrace`, `canonExpansion` all following identical structure.

**Plan**:
1. Add to [Util.hs](src/MLF/Elab/Run/Util.hs):
   - `makeCanonicalizer :: UnionFind -> IntMap NodeId -> (NodeId -> NodeId)`
   - `canonicalizeWitness :: (NodeId -> NodeId) -> EdgeWitness -> EdgeWitness`
   - `canonicalizeTrace :: (NodeId -> NodeId) -> EdgeTrace -> EdgeTrace`
   - `canonicalizeExpansion :: (NodeId -> NodeId) -> Expansion -> Expansion`

2. Replace inline definitions in Pipeline.hs with calls to these helpers (~60 lines removed).

**Verification**: `cabal test`

---

## Agent C — Debug Block Consolidation

**Target**: [Pipeline.hs](src/MLF/Elab/Run/Pipeline.hs) ~10 scattered debug blocks

**Problem**: Repeated pattern:
```haskell
case if debugGaScopeEnabled then debugGaScope ("msg" ++ show x) () else () of () -> pure ()
```

**Plan**:
1. Add to [Debug.hs](src/MLF/Elab/Run/Debug.hs):
   ```haskell
   debugM :: Monad m => String -> m ()
   debugM msg = if debugGaScopeEnabled then debugGaScope msg (pure ()) else pure ()
   ```

2. Replace all debug blocks in Pipeline.hs (~80 lines of noise removed).

**Verification**: `cabal test`

---

## Agent D — Generalize.hs Phase Extraction

**Target**: [Generalize.hs](src/MLF/Elab/Run/Generalize.hs) `constraintForGeneralization` (lines 132-1237)

**Problem**: Single 1100-line function with 5 phases marked by comments but not separated.

**Plan**:
1. Extract each phase as a separate function:

   - **Phase 1** (lines 138-247): `restoreSchemeNodes`
     - Input: solved/base constraints, redirects
     - Output: restored `nodesSolved` map

   - **Phase 2** (lines 248-268): `buildNodeMappings`
     - Input: nodesSolved, baseNodes, redirects
     - Output: `NodeMapping` record (baseToSolved, solvedToBase)

   - **Phase 3** (lines 335-506): `computeBindParentsBase`
     - Input: base/solved constraints, redirects, instCopyNodes
     - Output: initial bind parents map

   - **Phase 4** (lines 507-1105): `computeSchemeOwnership`
     - Input: bind parents, gen nodes, scheme roots
     - Output: final bind parents with ownership

   - **Phase 5** (lines 1143-1237): `finalizeConstraint`
     - Input: all intermediate results
     - Output: `(Constraint, GaBindParents)`

2. Create intermediate record types to replace 20+ local bindings.

3. Keep `constraintForGeneralization` as thin orchestrator (~50 lines).

**Verification**: `cabal test`

---

## Agent E — Bind Parent Policy Simplification

**Target**: [Generalize.hs](src/MLF/Elab/Run/Generalize.hs) lines 296-417

**Problem**: Three nearly-identical `BindParentPolicy` values and 6 `insertParent*` helpers with subtle differences.

**Plan**:
1. Consolidate into single parameterized function:
   ```haskell
   data InsertMode = KeepOld | Override | SelfOrEmpty
   insertBindParent :: InsertMode -> ... -> BindParents -> BindParents
   ```

2. Replace `policyKeepOld`, `policyInsert`, `policySelfOrEmpty` with `InsertMode` values.

3. Inline the 6 `insertParent*` helpers (~80 lines removed).

**Verification**: `cabal test`

---

## Agent F — Scope Resolution Consolidation

**Target**: Repeated scope resolution in [Pipeline.hs](src/MLF/Elab/Run/Pipeline.hs) (lines 318-324, 579-581, 938-944, 960)

**Problem**: Pattern "get scope from base → prefer gen → canonicalize" appears 4 times.

**Plan**:
1. Add to [Scope.hs](src/MLF/Elab/Run/Scope.hs):
   ```haskell
   resolveCanonicalScope :: Constraint -> SolveResult -> IntMap NodeId -> NodeId -> Either BindingError NodeRef
   ```

2. Replace all 4 occurrences (~40 lines removed).

**Verification**: `cabal test`

---

## Execution Order

Dependencies:
- **C, B, F, E** can run in parallel (no dependencies)
- **D** benefits from E completing first
- **A** benefits from B, C, F completing first

Suggested order: C → B → F → E → D → A

---

## Expected Outcome

| File | Before | After |
|------|--------|-------|
| Pipeline.hs | 1181 | ~400 |
| Generalize.hs | 1252 | ~600 |
| ResultType.hs (existing) | current | ~350 |
| Util.hs | 19 | ~120 |
| Debug.hs | 70 | ~85 |
| Scope.hs | 117 | ~140 |
| **Total** | 3040 | ~2100 |

Net: ~900 lines removed (~30%), no function over 200 lines, clear phase boundaries.

---
---

## Additional Agents (Lower Priority)

## Agent G — Presolution Driver (`src/MLF/Constraint/Presolution/Driver.hs`)

Scope: presolution loop orchestration, constraint rewriting, and witness/trace recording.

Plan:
1. Group functions by responsibility: loop driver (`runPresolutionLoop`, `processInstEdge`), rewrites (`rewriteConstraint`, `rewriteVarSet`, `rewriteGenNodes`), witness/trace (`recordEdgeWitness`, `recordEdgeTrace`, `canonicalizeEdgeTraceInteriorsM`).
2. Introduce a small internal `DriverEnv` record for values threaded through many helpers to reduce argument lists and repeated tuple passing.
3. Collapse `recordEdgeWitness`/`recordEdgeTrace` into a single helper that takes a recorder function; reduce duplicated control flow.
4. Pull `frWith` into a shared util (or local helper) to avoid duplication with `MLF.Constraint.Solve`’s `frWith`.
5. Reduce nested `case` in `processInstEdge` by factoring “no-op / skip” paths into guard helpers.
6. Simplify rewrite functions by unifying common traversal patterns; extract `rewriteNodes` and `rewriteEdges` helpers with explicit names.

Verification: `cabal test`

## Agent H — Elab Run Generalize (`src/MLF/Elab/Run/Generalize.hs`)

Scope: generalization-specific graph operations (`applyBindParent`, `schemeRootsOf`, `pruneBindParentsConstraint`, `instantiationCopyNodes`, `constraintForGeneralization`).

Plan:
1. Identify repeated IntMap/IntSet traversal patterns (`childrenFrom`, `schemeRootsOf`, `isTyVarAt`) and consolidate into 1–2 traversal helpers.
2. Replace nested lookups with `Maybe`/`Either` `do` blocks to clarify failure paths.
3. Extract a single “generalize constraint view” function that feeds both `pruneBindParentsConstraint` and `constraintForGeneralization`.
4. Inline one-off helpers used only once after extraction; prefer a short local `where` block.
5. Introduce small predicate helpers (`isChildBinder`, `isRootBinder`) to replace repeated boolean logic.
6. Review export list: keep only helpers that are used from other modules or tests.

Verification: `cabal test`

## Agent D — Presolution Binder Plan (`src/MLF/Constraint/Presolution/Plan/BinderPlan.hs`)

Scope: binder planning and tracing (`buildBinderPlan`, `bindingScopeGen`, `isQuantifiable`, trace toggles).

Plan:
1. Extract a `BinderPlanConfig` record (trace flag + ordering rules) to reduce argument threading.
2. Simplify `buildBinderPlan` by splitting into “collect candidates” and “finalize order” helpers.
3. Deduplicate trace gating (`traceBinderPlanEnabled`, `traceBinderPlanEnabledM`) into one helper with polymorphic context.
4. Move pure predicates (`isQuantifiable`) into a small local section and reuse across plan phases.
5. Replace deeply nested `case`/`if` with guards and small total helpers.

Verification: `cabal test`

## Agent E — Φ Translation (`src/MLF/Elab/Phi.hs`)

Scope: Φ translation logic, context handling, witness processing (large file with many internal helpers).

Plan:
1. Split the file into explicit sections: context construction, witness normalization, Φ term construction, and debugging/trace. Add section headers for navigation.
2. Identify paired helpers “with trace” vs “without trace”; refactor into a core function + thin wrappers to remove duplication.
3. Introduce a `PhiEnv` record to carry common parameters (ordering keys, bound/alias rules) and shrink function signatures.
4. Isolate scope-aware bound/alias inlining into a single helper (referencing the existing note) and call it from all paths.
5. Replace repeated pattern-matching on witness steps with a `foldM` over a normalized step list.
6. Remove unused local helpers after consolidation and tighten exports.

Verification: `cabal test`

## Agent F — Presolution Plan Facade (`src/MLF/Constraint/Presolution/Plan.hs`)

Scope: orchestration of planning (`planGeneralizeAt`, `planReify`, `mkGeneralizeEnv`, `softenBindParents`, debug flags).

Plan:
1. Split `planGeneralizeAt` into smaller “build env / build binder plan / build target plan / finalize” helpers and keep the facade short.
2. Collapse `mkGeneralizeEnv` + `softenBindParents` into a single “prepare env” helper where possible.
3. Centralize debug gating in one helper; remove repeated `debugGeneralizeEnabled` checks.
4. Reduce `planReify` branching by extracting a `shouldReify` predicate and a `buildReify` helper.

Verification: `cabal test`

## Agent G — Presolution Plan Target (`src/MLF/Constraint/Presolution/Plan/Target.hs`)

Scope: target planning (`buildTargetPlan`, `buildGammaPlan`, `buildTypeRootPlan`, trace).

Plan:
1. Normalize `buildGammaPlan` and `buildTypeRootPlan` to share a single traversal helper that collects targets.
2. Extract a `TargetPlanConfig` record so trace/enabled flags and environment data are not passed repeatedly.
3. Replace nested `case` on binder/edge variants with named predicate helpers.
4. Hoist repeated IntMap/IntSet conversions into helpers and reuse across plan builders.

Verification: `cabal test`

## Agent H — Constraint Solve (`src/MLF/Constraint/Solve.hs`)

Scope: unification solver pipeline and validators (`solveUnify`, `applyUFConstraint`, `rewriteEliminatedBinders`, `validate*`).

Plan:
1. Add section headers aligning with the Notes (paper alignment, algorithm sketch, normalization vs solve, invariants).
2. Extract a small “solve core” function from `solveUnify` that takes a strategy record; keep `solveUnify` as a wrapper.
3. Merge `validateSolvedGraph` and `validateSolvedGraphStrict` into one validator with a mode flag; keep old names as wrappers if needed.
4. Consolidate repeated UF/graph traversal code into helpers (occurs check, bind-parent rewrite, alias handling).
5. Centralize `frWith` in a shared helper (or local `where`) to avoid duplicated logic in Driver.

Verification: `cabal test`

## Agent I — Binding Tree (`src/MLF/Binding/Tree.hs`)

Scope: binding tree utilities (lookup/set/remove, roots, paths, traversal).

Plan:
1. Group functions into: node refs, bind-parent ops, root/path queries, child filters; add section headers.
2. Factor common root/path logic into one core function that accepts a “stop condition,” and implement `bindingPathToRoot`/`bindingPathToRootLocal` as wrappers.
3. Replace repeated IntMap lookups and filters with shared traversal helpers (`foldBindParents`, `collectChildren`).
4. Simplify boolean-heavy predicates (`isBindingRoot`, `tyVarChildFilter`) with small named helpers to improve readability.
5. Remove one-off helpers after refactor; keep only exported ones used elsewhere.

Verification: `cabal test`

## Agent J — Reify Core (`src/MLF/Reify/Core.hs`)

Scope: type reification and cache helpers (`reifyType*`, cache ops).

Plan:
1. Introduce a `ReifyMode`/`ReifyConfig` record capturing “with names”, “no fallback”, and bound/alias policies.
2. Replace the multiple `reifyTypeWithNames*` variants with a single core `reifyTypeWith` and thin wrappers.
3. Pull fallback logic into a dedicated helper so the “no fallback” paths remain short and explicit.
4. Collapse repeated cache lookup/insert patterns with a small `withCache` helper.
5. Align naming for “bound” vs “scheme” helpers and remove redundant versions after consolidation.

Verification: `cabal test`

## Agent K — Elaboration (`src/MLF/Elab/Elaborate.hs`)

Scope: term elaboration, substitutions, instantiation application (`subst*`, `expansionToInst`, `instSeqApps`).

Plan:
1. Split the file into sections: substitution utilities, instantiation building, term elaboration, debugging.
2. Collapse `substInTerm`, `substInTy`, `substInScheme`, `substInInst` into a shared traversal helper (or use `MLF.Util.RecursionSchemes` if already available).
3. Extract instantiation application logic (`instSeqApps`, `expansionToInst`) into small helpers with clear names.
4. Normalize “reify type for inst arg” paths so it is called in exactly one place.
5. Remove ad-hoc debug gates by consolidating `debugElabGeneralize*` checks.

Verification: `cabal test`

## Agent L — Presolution Plan Reify (`src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`)

Scope: `buildReifyPlan` and its internal helpers (large, single-entry file).

Plan:
1. Decompose `buildReifyPlan` into sub-steps: collect roots, compute schemes, build mapping, finalize plan.
2. Extract pure helpers for each step and make data flow explicit with a small local record.
3. Deduplicate similar traversal logic with `Plan.Target` helpers where possible.
4. Replace nested branching with guards and clear predicate helpers.

Verification: `cabal test`

## Agent M — Presolution Base (`src/MLF/Constraint/Presolution/Base.hs`)

Scope: shared presolution utilities (`runPresolutionM`, binder ordering helpers, binding-tree checks).

Plan:
1. Group binder selection helpers into one section and extract a shared “ordered binders” core with mode flags.
2. Consolidate `orderedBindersM` / `orderedBindersRawM` / `instantiationBindersM` / `implicitBindersM` via a small mode enum.
3. Inline single-use helpers after consolidation; reduce duplicated IntMap traversal.
4. Centralize debug gating (`debugBinders`) with a single helper.
5. Ensure `requireValidBindingTree` and `ensureBindingParents` share error/reporting helpers.

Verification: `cabal test`

## Agent N — Presolution Witness (`src/MLF/Constraint/Presolution/Witness.hs`)

Scope: witness normalization, validation, and integration (`normalizeInstanceOps*`, `integratePhase2*`, `coalesceRaiseMergeWithEnv`).

Plan:
1. Split the normalization pipeline into explicit phases: integrate ops, strip exteriors, reorder, validate.
2. Collapse “ops vs steps” duplication by introducing a shared normalized representation and helpers.
3. Extract a `WitnessNormEnv` to reduce parameter threading (order keys, fallback behavior).
4. Replace repeated recursion with a single `foldM` over normalized steps; use small predicates for legality checks.
5. Keep `validateNormalizedWitness` focused and move error construction into helpers.

Verification: `cabal test`

## Agent O — Elab Generalize (`src/MLF/Elab/Generalize.hs`)

Scope: generalization type building (`rigidNameFor`, `buildForallType`, `inlineRigidTypes` + helpers).

Plan:
1. Group the file into “rigid naming”, “forall building”, “inline rigid types” sections for readability.
2. Replace repeated pattern matches on `ElabType` constructors with shared helpers.
3. Extract a small “rigid type map” function to avoid repeated `IntMap` lookups and inserts.
4. Collapse any duplicated recursion into a single traversal helper.

Verification: `cabal test`
