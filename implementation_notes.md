# Implementation Notes

## Summary of Changes

**Current vs target:** The current pipeline records presolution witnesses and produces explicit generalization plans in `MLF.Constraint.Presolution.Plan`; elaboration applies these plans via `MLF.Elab.Generalize` without re-solving. The remaining paper-faithfulness deltas are tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` (constructor types `Cσ` and stricter translatability validation for Φ).

### 2026-02-08 A7 Group 1 binding-core shared-helper consolidation (docs sync)

- [x] Removed duplicated binding-path traversal helpers; canonical module is `MLF.Binding.Path` (`bindingPathToRootWithLookup`, `bindingPathToRoot`, `bindingPathToRootLocal`, `firstGenAncestorFromPath`).
- [x] Removed duplicated node-ref enumeration/existence helpers; canonical module is `MLF.Binding.NodeRefs` (`allNodeRefs`, `nodeRefExists`).
- [x] Removed duplicated scope-graph helper logic; canonical module is `MLF.Binding.ScopeGraph` (`buildTypeEdgesFrom`, `buildScopeNodesFromPaths`, `rootsForScope`).
- [x] Removed duplicated bound-child collection loops; canonical module is `MLF.Binding.Children` (`collectBoundChildrenWithFlag`, `collectBoundChildren`).
- Migration landing points:
  - `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, and `MLF.Binding.Canonicalization` now import the canonical helper modules.
  - `MLF.Constraint.BindingUtil.firstGenAncestorFrom` now delegates to `MLF.Binding.Path.firstGenAncestorFromPath`.
  - `MLF.Constraint.Presolution.Base.bindingPathToRootUnderM` now delegates to `MLF.Binding.Path.bindingPathToRootLocal` after quotient bind-parent canonicalization.
- Behavioral impact: none intended; this was an abstraction-only consolidation.

### 2026-02-09 H15 lambda-parameter source guard (implemented)

- Context:
  - After H13+H14, the `make` reproducer still failed in Phase 7 with a naming mismatch (`t23` vs `b`) even though let-scheme generalization was already correct (`forall a b. a -> b -> a`).
- Root cause:
  - In `MLF.Elab.Elaborate` (`ALam` case), unannotated lambdas could source parameter type reification from `resolvedLambdaParamNode lamNodeId` (copy-derived solved nodes) rather than lexical `paramNode`.
  - In the failing path this produced `ELam "y" (TVar "t23") ...`, while the let scheme stayed `... (TVar "b") ...`, causing `TCLetTypeMismatch`.
- Implemented fix:
  - Added `hasInformativeVarBound` and guarded param-source selection:
    - annotated-lambda desugaring keeps resolved-node behavior;
    - unannotated lambdas use resolved node only when its bound-chain reaches a non-`TyVar` bound (informative structural/base bound);
    - otherwise fall back to lexical `paramNode`.
  - This avoids solved-node-name leakage while preserving prior behavior for application typing paths that require resolved informative bounds.
- Regression coverage:
  - Added `PipelineSpec` test:
    - `does not leak solved-node names in make let mismatch`.
- Verification:
  - `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch"' --test-show-details=direct`
  - `cabal test mlf2-test --test-options='--match "runPipelineElab type matches typeCheck(term) and checked pipeline type"' --test-show-details=direct`
  - `cabal build all && cabal test`

### 2026-02-08 A7 group 2 dedup checklist

- [x] Frontend translate scope/parent wiring now routes through local helpers (`withScopedBuild`, `attachUnder`, `rebindScopeRoot`) across let/coercion/forall-internalization paths.
- [x] Elab run annotation node rewriting now routes through shared `mapAnnNodes`, reused by `applyRedirectsToAnn`, `canonicalizeAnn`, and debug edge-origin traversal.
- Result: duplicated control-flow wiring was collapsed into shared local helpers without changing behavior.

### 2026-02-06 strict checked-authoritative follow-up

- `runPipelineElab` now uses checked type authority end-to-end while keeping reconstruction paths for diagnostics only.
- Top-level closure now falls back to explicit free-variable closure when root generalization yields no binders but the elaborated term is still type-open.
- Shared closure (`MLF.Elab.TermClosure`) now freshens scheme binders against existing `ETyAbs` names and rewrites free type-variable occurrences in term types/instantiations to avoid capture/regressions.
- Annotation elaboration aligns `InstInside (InstBot ...)` with the generalized annotation-bound head when available, reducing bound-erasure in explicit-forall annotation paths.
- Regression expectations in `test/ElaborationSpec.hs` were updated for checked-authoritative term/type shapes (top-level `ETyAbs` wrappers, `Bool`-authoritative result, and closed `∀a. a -> a` fallback for `\\y. let id = ... in id y`).
- Historical note: bounded aliasing requiring thesis Merge/RaiseMerge witness translation was still unresolved at this checkpoint.
- Root-cause clarification at that time: the gap was not pipeline order (desugaring before presolution remained correct), but alias-bound information being erased on a coercion path before edge-local RaiseMerge gating.
- This gap is now resolved by the 2026-02-08 staged-normalization + structural-gating implementation (see `BUG-2026-02-06-003` in `Bugs.md`).

### 2026-02-07 syntax frontend + canonical pretty migration

- Added eMLF parser/pretty modules:
  - `src/MLF/Frontend/Parse.hs`
  - `src/MLF/Frontend/Pretty.hs`
- Added paper-faithful xMLF syntax/parser/pretty modules:
  - `src/MLF/XMLF/Syntax.hs`
  - `src/MLF/XMLF/Parse.hs`
  - `src/MLF/XMLF/Pretty.hs`
- Added public xMLF API module: `src-public/MLF/XMLF.hs`.
- Extended `MLF.API` with explicit eMLF parse/pretty entry points (`parseRawEmlfExpr`, `parseRawEmlfType`, `parseNormEmlfExpr`, `parseNormEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`) and parse error rendering helpers.
- Added canonical syntax spec document: `docs/syntax.md` (legacy output, canonical target grammar, migration deltas, normalization rules, and implementation extensions).
- Migrated `MLF.Elab.Types` pretty-printing to syntax-driven rendering through `MLF.XMLF.Pretty`/`MLF.XMLF.Syntax` conversion helpers:
  - canonical xMLF computation forms are now printed (`ε`, `⊲σ`, `α⊳`, explicit `∀(⩾ ϕ)`/`∀(α ⩾) ϕ`, and derived `InstApp` as `∀(⩾ ⊲σ); N`);
  - unbounded binders are printed with explicit bottom bounds (`⩾ ⊥`);
  - term/type binder syntax now follows canonical parenthesized forms (`λ(x : σ)`, `Λ(α ⩾ σ)`).
- Added parser/pretty coverage tests:
  - `test/FrontendParseSpec.hs`
  - `test/FrontendPrettySpec.hs`
  - `test/XMLFParseSpec.hs`
  - `test/XMLFPrettySpec.hs`
- Updated existing elaboration pretty-output expectations in `test/ElaborationSpec.hs` to canonical syntax forms.

### 2026-02-08 solved-order shadow cutover semantics

- Generalize now treats solved-order as the solved-authoritative output order for reification/quantifier emission.
- After the 5/5 green gate, runtime fallback in `MLF.Elab.Generalize` no longer reifies or compares base-path shadow output.
- Solved-order output is authoritative in runtime generalization fallback (no runtime base-shadow compare).
- Shadow comparator helpers (`shadowCompareTypes`, `selectSolvedOrderWithShadow`) remain available for focused unit tests/debugging.

### 2026-02-08 staged frontend normalization + structural RaiseMerge gating (implemented)

- Implemented staged frontend boundaries:
  - Frontend types are now one indexed family: `SrcTy (n :: SrcNorm) (v :: SrcTopVar)`.
  - Backward-compatible aliases remain: `SrcType`, `NormSrcType`, `StructBound`, `RawSrcType`.
  - Forall bounds use `SrcBound n`; normalized bounds unwrap to `StructBound` via `unNormBound`.
- Implemented explicit normalization boundary:
  - `MLF.Frontend.Normalize` provides `normalizeType`/`normalizeExpr` with capture-avoiding alias inlining and explicit typed errors (`SelfBoundVariable`, `NonStructuralBoundInStructContext`) instead of runtime crashes.
  - Parser API has explicit raw and normalized entrypoints only (`parseRaw*`, `parseNorm*`); legacy compatibility aliases were removed for clean-break alignment.
- Implemented normalized-only compiler contracts:
  - `desugarSurface`, `generateConstraints`, and pipeline graph/elaboration entrypoints accept normalized expressions only.
- Implemented structural RaiseMerge gating:
  - `shouldRecordRaiseMerge` now uses only live canonical bound queries, binding-tree ancestry, edge-interior membership, same-root exclusion, and elimination state.
  - Precomputed binder-bound snapshots (`eusBinderBounds`) were removed from edge-unify state.
- Bounded aliasing baseline is restored end-to-end:
  - `runPipelineElab` and `runPipelineElabChecked` now both elaborate the bounded aliasing baseline to a type alpha-equivalent to `∀a. a -> a -> a`.
  - Regression test anchor: `test/ElaborationSpec.hs` case `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`.
- Tracking:
  - Ralph task: `tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/prd.json`
  - Related bug: `BUG-2026-02-06-003` (resolved in `Bugs.md`)

### 2026-02-08 strict SrcTy indexed model + staged pretty (implemented)

- Consolidated split frontend type declarations into one indexed AST in `MLF.Frontend.Syntax`:
  - `SrcNorm = RawN | NormN`
  - `SrcTopVar = TopVarAllowed | TopVarDisallowed`
  - `SrcTy` constructors (`STVar`, `STArrow`, `STBase`, `STCon`, `STForall`, `STBottom`) shared across raw/normalized paths.
- Added `SrcBound` wrappers and helpers (`mkSrcBound`, `mkNormBound`, `unNormBound`) so normalized forall bounds remain structurally rooted by type.
- Parser/normalizer/constraintgen internals now consume alias-aware wrappers instead of separate concrete `NST*`/`SB*` node declarations.
- Pretty printing is now staged/generic:
  - `prettyEmlfType :: SrcTy n v -> String`
  - `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`
  while preserving canonical output syntax.
- Regression anchors:
  - `test/ElaborationSpec.hs` — `SrcTy indexed aliases compile shape`
  - `test/FrontendParseSpec.hs` — `parses raw forall binder and keeps raw alias type`
  - `test/FrontendPrettySpec.hs` — `pretty-prints normalized staged types`
  - `test/ConstraintGenSpec.hs` — `internalizes normalized forall bounds using indexed StructBound alias`

### 2026-02-08 Phase 6 crash hardening (BUG-2026-02-06-001)

- Before the solved-order cutover, `MLF.Elab.Generalize.reifyWithGaBase` validated `solvedToBasePref` targets before any base-constraint reification.
- After the cutover gate passed, runtime elaboration no longer depends on `reifyWithGaBase`; fallback now reifies from solved-order roots/substitutions.
- The nested let + annotated-lambda reproducer remains covered by `test/ElaborationSpec.hs` and no longer crashes in Phase 6.
- Remaining failure on the same program moved to Phase 7 (`TCLetTypeMismatch`) and is tracked separately as `BUG-2026-02-08-004`.

## Module Structure (Post-Refactor)

The codebase has been refactored for improved navigation and paper-faithfulness auditing:

### Graph Types (`MLF.Constraint.Types.Graph`)

The monolithic `Graph` module has been split into focused submodules:

| Submodule | Contents |
|-----------|----------|
| `Graph.NodeEdge` | Core node and edge definitions (`NodeId`, `TyNode`, `InstEdge`, `UnifyEdge`, etc.) |
| `Graph.Binding` | Binding-related types (`BindFlag`, `BindParents`, `BindingError`) |
| `Graph.Accessors` | Accessor utilities (`maxNodeIdKeyOr0`) |

`MLF.Constraint.Types.Graph` re-exports all submodules as a facade.

### Presolution (`MLF.Constraint.Presolution`)

Presolution modules now use shared state-access helpers:

| Module | Purpose |
|--------|---------|
| `StateAccess` / `Ops` | Shared `MonadPresolution` accessors (`getConstraint`, `modifyConstraint`, `liftBindingError`) |
| `EdgeProcessing` | Edge-local logic with explicit `EdgeCtx` |
| `EdgeProcessing.Witness` | Witness construction helpers |
| `EdgeProcessing.Unify` | Edge-local unification |

### Unification (`MLF.Constraint.Unify`)

Shared unification core for consistent behavior across phases:

| Module | Purpose |
|--------|---------|
| `Unify.Core` | Policy-driven unification with `UnifyStrategy` |
| `Unify.Decompose` | Structural decomposition helpers |

### Elaboration (`MLF.Elab`)

Elaboration now uses structured config records:

| Record | Purpose |
|--------|---------|
| `ElabConfig` | Static configuration (debug flags, etc.) |
| `ElabEnv` | Per-elaboration environment (naming, etc.) |

Legacy code is isolated in `MLF.Elab.Legacy` (e.g., `expansionToInst`).

### Documentation

- `docs/paper-map.md` — Paper-to-code mapping for auditing
- `docs/phase-notes.md` — Phase invariants and test references

### 1. src/MLF/Constraint/Presolution/Driver.hs (+ EdgeUnify/Witness)
- **`unifyStructure` / `unifyStructureEdge`**: Recursively unify structural children (TyArrow, TyForall, plus TyVar bounds) so `Arrow A B ~ Arrow C D` propagates `A~C` and `B~D` (Driver for global merges; EdgeUnify for edge-local χe execution).
- **`processInstEdge`**:
  - Uses `unifyStructure`/`unifyStructureEdge` instead of raw `unifyAcyclic`.
  - Eagerly materializes non-Identity expansions (`applyExpansionEdgeTraced`), binds the expansion root like the target, and unifies the expansion result with the target (plus the original TyExp wrapper).
  - Guards against `Identity` expansion cycles by skipping `TyExp ~ Target` unification when expansion is `Identity` (relying on `decideMinimalExpansion` unifications instead).
- **Per-edge instance witnesses (`Φ` input) + traces**:
  - Presolution records `EdgeWitness` + `EdgeTrace` per instantiation edge (`psEdgeWitnesses` / `psEdgeTraces`, surfaced as `prEdgeWitnesses` / `prEdgeTraces`).
  - Witnesses combine expansion-derived steps (`witnessFromExpansion`) with edge-local unification ops from `EdgeUnify` (Raise/Merge/Weaken).
  - `ExpForall` yields `StepIntro` entries (xMLF quantifier-introduction `O`) in `ewSteps`, not Ω ops; `ExpInstantiate` yields per-binder Ω ops (`OpGraft`/`OpWeaken`/`OpMerge`).
  - Witness steps are normalized in `normalizeEdgeWitnessesM` via `normalizeInstanceStepsFull` (coalesces Raise+Merge into RaiseMerge, enforces “Weaken-last” ordering, avoids double elimination).
  - `ExpInstantiate` witness/application logic skips “vacuous” `TyForall` wrappers (quantifier levels with no binders) so `Φ` construction doesn’t fail on nested/structural ∀ nodes.
  - `ExpInstantiate` witnesses avoid invalid grafts under non-⊥ bounds: if a binder has an instance bound that is another in-scope variable (e.g. `b ⩾ a`), presolution emits `OpMerge(b, a)` rather than `OpGraft` (paper Fig. 10 “alias + eliminate”).
    - Current behavior: RaiseMerge recording uses live structural graph facts (`shouldRecordRaiseMerge`) rather than alias-metadata survivability; this closed `BUG-2026-02-06-003`.
  - When an expansion includes a later `ExpForall`, `ExpInstantiate` witnesses suppress `OpWeaken` so binder metas stay flexible until the new quantifier is introduced (avoids empty Q(n) and lost ∀ in bounded-aliasing cases).
  - Edge-local unification can record `OpRaiseMerge(b, m)` when unification forces a **bounded** binder’s instantiation meta to unify with a `TyVar` bound **above the instantiation-edge root** in the binding tree (recorded as `OpRaise` + `OpMerge`, then normalized to `OpRaiseMerge`), matching the paper’s “escape to bound-above node” shape.
    - Implemented behavior: this emission path is no longer gated by edge-local `binderBounds`; it queries live canonical bounds and structural ancestry/interior predicates directly.
- **Scope tracking (paper `Raise` as graph transformation)**:
  - TyVar/TyVar unions harmonize binding parents by executing the paper `Raise(n)` graph operation as a binding-edge rewrite on `Constraint.cBindParents` (`MLF.Binding.Adjustment` / `MLF.Binding.GraphOps`).
  - During instantiation-edge solving (χe), the same per-step raises are also recorded as `OpRaise` in the edge witness Ω (`unifyAcyclicRawWithRaiseTracePrefer` → `unifyAcyclicEdge` / `unifyAcyclicEdgeNoMerge`), aligning with `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4 / Fig. 10).
  - Variable bounds and eliminations are stored in `Constraint.cVarBounds` / `Constraint.cEliminatedVars` (`MLF.Constraint.VarStore`) and are looked up by canonical `NodeId`, so they stay consistent as binding edges and UF representatives change.
- **`materializeExpansions`**: Avoids duplicating fresh nodes by reusing the already-unified expansion result for non-Identity expansions; Identity expansions still rewrite `TyExp` wrappers to their bodies.
- **`rewriteConstraint`**: Ensures Identity `TyExp` wrappers are erased even when they are not the Union-Find root (redirecting the whole UF class to the wrapper’s body). This fixes over-generalization bugs in paper-alignment baselines like `let id = (\x. x) in id id` and `\y. let id = (\x. x) in id y`.

### 2. src/MLF/Constraint/Normalize.hs
- **`applyUnionFindToConstraint`**: Enhanced to perform "grafting". When a `TyVar` node is unified with a structural node (e.g., `TyBase`), the `TyVar` node in the graph is destructively updated to become a copy of that structure. This ensures that external references to the variable (like the expression root) see the inferred structure.
- **Binding-edge Raise harmonization**: Var-var merging harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.

### 3. src/MLF/Constraint/Solve.hs
- **Binding-edge Raise harmonization**: Phase 5 harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.
- **Elimination rewrite**: `solveUnify` now rewrites eliminated binders into their bounds (or explicit `TyBottom` nodes), removes them from the graph, and clears `cEliminatedVars` before elaboration.
  - The solve-time union-find map is extended with the elimination substitution so witness ops that mention eliminated ids still canonicalize to live nodes.

### 4. src/MLF/Elab/Generalize.hs + src/MLF/Elab/Generalize/* + src/MLF/Elab/Elaborate.hs + src/MLF/Elab/Run.hs (reexported via `MLF.Elab.Pipeline`)
- **Generalize is now an orchestrator**:
  - Phase-oriented logic moved into focused modules: `Generalize/Plan`, `SchemeRoots`, `BinderPlan`, `Ordering`, `ReifyPlan`, `Normalize`, and `Helpers`.
  - The top-level `generalizeAt`/`generalizeAtWith` functions now read as a linear pipeline of plan → binders → ordering → reify → normalize, with local helpers split by concern.
- **`generalizeAt`**:
  - Optimized to handle structural `TyForall` nodes (avoiding double quantification).
  - Returns the `subst` (renaming map) alongside the scheme.
- **Scope follows the solved graph**:
  - Binder discovery is binding-tree driven (`Constraint.cBindParents`): `TyForall` scopes use the body as the ≺ root, while non-Forall scopes use binding-parent paths to the nearest gen ancestor.
  - Presolution rewrite reconstructs binding parents and reattaches unparented nodes to the root gen node, keeping expansion/copy roots in-scope for generalization.
  - `generalizeAt` + `reifyTypeWithNamesNoFallback` rely solely on binding-tree enumeration (no free-variable fallback).
  - Rigid binding edges are treated as inline bounds, and bounds are included in reachability when ordering binders.
  - Elaboration no longer consults `cEliminatedVars`; eliminated binders are already rewritten out of the graph. Vacuous `TyForall` wrappers (no binders) are elided during reification.
- **`substInTerm` / `substInType`**: Implemented in `MLF.Elab.Elaborate` to apply the renaming map from `generalizeAt` to the elaborated term body. This ensures that terms use the same variable names as their type schemes (e.g., `Λa. λx:a. x` instead of `Λa. λx:t0. x`).
- **`elaborate`**: Applies substitution to the RHS of let-bindings.
- **Witness translation (`Φ`) + quantifier reordering (`Σ`)**:
  - Elaboration reifies instantiations from recorded per-edge witnesses (`prEdgeWitnesses`) via `phiFromEdgeWitnessWithTrace` (rather than `expansionToInst`), using `EdgeTrace` for copy maps/interiors. Production elaboration requires trace; no-trace entry points are test/debug-only.
  - `Φ` consumes interleaved `ewSteps` (`StepIntro` for `O`, `StepOmega` for Ω); `OpGraft`+`OpWeaken` maps to `InstApp` (⟨τ⟩), `OpGraft` alone maps to an `InstBot` inside the binder, and `OpMerge`/`OpRaise`/`OpRaiseMerge` map to the paper’s alias/raise instantiations (Fig. 10).
- `phiFromEdgeWitnessWithTrace` targets binders using `InstUnder` contexts (`C{·}`) and prefixes Ω-translation with the ≺-based reordering ϕR/Σ(g) when `Typ` vs `Typexp` disagree (thesis Def. 15.3.4); missing non-spine contexts are errors, and normalized ω ops that violate translatability (e.g. `OpRaise` outside `I(r)`, non-binder targets, rigid-only-on-non-operated-endpoint for Merge/RaiseMerge) are rejected rather than silently skipped. Rigid identity behavior follows the literal thesis condition on operated node `n` for Raise/Merge/RaiseMerge.
  - Implemented explicit quantifier reordering instantiations (`sigmaReorder`) using adjacent swaps per `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4).
  - Implemented `applyInstantiation` (in `MLF.Elab.Inst`, reexported via `MLF.Elab.Pipeline`) to check/apply xMLF instantiations to xMLF types (see `papers/these-finale-english.txt`; `papers/xmlf.txt` Fig. 3), used by tests to validate that `Φ(e)` transforms the source type into the target type.
- **`expansionToInst`**: Kept as a legacy/debug conversion from `Expansion` to `Instantiation` (no longer the main path for elaboration).
- **`runPipelineElab`**: Generalizes the top-level result using the nearest gen ancestor of the expression root (root gen node for top-level), keeps reconstruction checks for diagnostics, and reports the type-checker result as the authoritative pipeline type.

## Testing
- **`test/ElaborationSpec.hs`**: Updated expectations to reflect correct polymorphic behavior and variable naming. Added integration tests for polymorphic instantiation.
- **Witness translation tests**: Added focused tests for `Σ(g)` reordering and for `Φ` soundness (`applyInstantiation source Φ(e) == target` for representative instantiation edges).
- **`test/PresolutionSpec.hs`**: Verified that instantiation edges merge nodes correctly.
- **`test/TypeCheckSpec.hs` + `test/ReduceSpec.hs`**: Cover xMLF type-checking and reduction/instantiation semantics.

Note: `test/ElaborationSpec.hs` also contains **paper-alignment baseline tests** that serve as regression coverage while we continue aligning witnesses toward `papers/these-finale-english.txt` (see also `papers/xmlf.txt`, especially around Merge/RaiseMerge and aliasing behavior).

## `papers/these-finale-english.txt` study: thesis ↔ repo mapping (with `papers/xmlf.txt` cross-reference)

This repo’s design is primarily informed by:

- `papers/these-finale-english.txt` (thesis) for **xMLF**’s explicit types/instantiations/terms and the **elaboration** story; see `papers/xmlf.txt` for supplemental xMLF presentation details and figure numbering.
- The earlier “graphic constraints” papers (ICFP’08 / TLDI’07) for the **solver pipeline** that produces presolutions.

### Paper anchors (from `papers/these-finale-english.txt`; `papers/xmlf.txt` figure numbers for reference)

- **Fig. 1–4**: xMLF grammar, instantiation judgments, instantiation-as-a-function on types, and xMLF term typing rules.
- **§3.1–§3.5 + Fig. 7/9/10**: elaboration from (graphical) eMLF presolutions to xMLF:
  - `/)(g) = Λ(Q(g))` (insert type abstractions for flexible bindings at a level)
  - `Φ(e)` (compute instantiation witnesses from solved instantiation edges)
  - `S/Q/T` (map presolution nodes to xMLF types)
  - `Σ(g)` (quantifier reordering when the expansion’s quantifier order differs)

### Mapping: paper notation → repo types/functions

| Paper | Meaning | Repo |
|------:|---------|------|
| `b` | eMLF surface term | `src/MLF/Frontend/Syntax.hs` (`Expr` + indexed `SrcTy` aliases) |
| `χ` | constraint graph | `src/MLF/Constraint/Types.hs` (`Constraint`) |
| `n` | type node in the graph | `NodeId` + `TyNode` in `Constraint.cNodes` |
| `g` | binding-tree node (generalization site) | `GenNodeId`/`GenNode` + `Constraint.cBindParents` |
| `≤` edge | instantiation constraint | `InstEdge` (`Constraint.cInstEdges`) |
| `=` edge | unification constraint | `UnifyEdge` (`Constraint.cUnifyEdges`) |
| `s·τ` | expansion node / expansion variable | `TyExp{ tnExpVar :: ExpVarId }` + `Expansion` recipes in `Presolution` |
| `χp` | (principal) presolution | `MLF.Constraint.Presolution.PresolutionResult` (plus `prEdgeExpansions`) |
| `τ` | xMLF type | `src/MLF/Elab/Types.hs` (`ElabType`) |
| `φ` | xMLF instantiation witness | `src/MLF/Elab/Types.hs` (`Instantiation`) |
| `a` | xMLF term | `src/MLF/Elab/Types.hs` (`ElabTerm`) |

### Mapping: solver + elaboration phases → modules

| Phase | Role (paper) | Repo entry point |
|------:|--------------|------------------|
| 1 | Constraint generation | `MLF.Frontend.ConstraintGen.generateConstraints` |
| 2 | Local simplification (grafting/merging) | `MLF.Constraint.Normalize.normalize` |
| 3 | Acyclicity / dependency ordering | `MLF.Constraint.Acyclicity.checkAcyclicity` |
| 4 | Presolution (minimal expansions) | `MLF.Constraint.Presolution.computePresolution` |
| 5 | Global unification | `MLF.Constraint.Solve.solveUnify` |
| 6 | Elaborate to xMLF | `MLF.Elab.Pipeline.elaborate` / `MLF.Elab.Pipeline.runPipelineElab` |

### Alignment notes / known gaps vs `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3 for numbering)
- **Witness translation (`Φ`)**: `papers/these-finale-english.txt` translates *normalized instance-operation witnesses* into xMLF instantiations (see `papers/xmlf.txt` Fig. 10). This repo records a per-edge `EdgeWitness` during presolution and translates it to an xMLF `Instantiation` via `MLF.Elab.Pipeline.phiFromEdgeWitnessWithTrace` in production paths (`phiFromEdgeWitnessNoTrace` remains test/debug-only).
  - Quantifier-introduction (`O`) is not part of Ω in the thesis (see `papers/xmlf.txt`); the repo records these steps as `StepIntro` entries in `EdgeWitness.ewSteps` (from `ExpForall`) and translates them interleaved with Ω segments when constructing Φ(e).
  - Ω ops emitted today include `OpGraft`+`OpWeaken`, `OpMerge` (bounded aliasing like `b ⩾ a`, plus unification-induced aliasing during instantiation-edge solving), `OpRaise` (paper-general binding-edge raising on arbitrary interior nodes), and `OpRaiseMerge` for bounded-binder “escape” patterns. χe execution is paper-shaped for binding-tree ops: Raise/Weaken are executable binding-edge rewrites, and `EdgeTrace.etInterior` records the exact paper interior `I(r)` for filtering.
    - Bounded-aliasing caveat (`BUG-2026-02-06-003`) is resolved: RaiseMerge gating now uses structural live-graph predicates, and bounded aliasing elaborates to the thesis-aligned baseline in both checked and unchecked pipelines.
  - Φ requires a representable translation context; missing contexts and other non-translatable cases are hard failures. Rigid identity handling is literal for Raise/Merge/RaiseMerge on operated node `n`; rigid only on the non-operated endpoint is rejected as non-translatable.
- **Trace root/interior coherence**: `EdgeTrace` root/interior refresh and normalization share a single root-selection helper (`traceInteriorRootRef`) so `etRoot`, `etInterior`, and witness normalization all use the same interpretation of `r`/`I(r)`.
- **Witness merge-direction strictness**: Ω normalization rejects malformed merge direction (`MergeDirectionInvalid`) in all normalization entrypoints (helper + production); there is no permissive merge-direction fallback.
- **Context search strictness**: `contextToNodeBound` follows thesis context grammar (under-quantifier / inside-bound) and does not use non-thesis fallback descent through `TyForall` body.
- **Quantifier reification (binding-tree based)**: `Q(n)`/reification quantifies flexibly bound `TyVar` binders using binding-parent edges (bounds included in reachability), so bounds and contexts remain representable in Φ and generalization.
- **Quantifier reordering (`Σ(g)` / `ϕR`)**: implemented via `MLF.Elab.Sigma` / `MLF.Elab.Pipeline.sigmaReorder` (adjacent swaps per `papers/these-finale-english.txt` Def. 15.3.4 / Fig. 15.3.5; see `papers/xmlf.txt` §3.4). Φ translation (`phiFromEdgeWitnessWithTrace` → `phiWithSchemeOmega`) prefixes Ω-translation with this reordering whenever `Typ(a′)` and `Typexp(a′)` disagree in binder order — even when Ω contains no Raise steps — while still targeting binders for Ω using `InstUnder` instantiation contexts (paper’s `C{·}`). The computation is deterministic and fail-fast: missing <P order keys or bound-dependency cycles produce `InstantiationError` messages prefixed `PhiReorder:` rather than silently returning `InstId`.
- **Application elaboration shape**: now matches Fig. 7 — constraint generation emits instantiation edges for both function and argument, and elaboration wraps each side with `ETyInst` when non-identity.
- **Constraint representation differences**: the thesis's graphical presentation (see also `papers/xmlf.txt`) uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). The repo mirrors the same split (`Constraint.cNodes` + `Constraint.cBindParents` with `BindFlex`/`BindRigid`); some paper machinery remains simplified (e.g. witness normalization/ordering is implemented but not yet backed by formal proofs).
- **xMLF Phase 7**: the repo includes type-checking and reduction for xMLF terms/instantiations (`MLF.Elab.TypeCheck`, `MLF.Elab.Reduce`) and uses them in tests, but still lacks a fully formalized/verified connection to the thesis presentation (e.g., proof obligations and full evaluation-context coverage).

## Kiro spec planning
- Paper-faithfulness deltas are captured in `.kiro/specs/paper-faithfulness-remaining-deltas/`, including evidence pointers to the thesis and code, plus a concrete implementation plan.

## 2026-02-10 BUG-2026-02-06-002 staged closure notes

- `MLF.Elab.Phi.Omega` now treats delayed binder-local `OpGraft ... OpWeaken` pairs as a single binder application path when no intervening op touches that binder, and rescues binder-arg `TBottom` reification to binder TVar naming when available.
- `MLF.Elab.Elaborate` let elaboration now computes an env-aware RHS type (`typeCheckWithEnv`) and uses a guarded fallback scheme only when the generalized scheme and RHS-derived generalized scheme are not alpha-equivalent.
- `MLF.Elab.Elaborate` application elaboration extends non-polymorphic-arg repair to `InstApp TForall{}` fun-instantiation payloads, reifying argument type from the argument annotation node.
- Current test evidence:
  - `BUG-2026-02-06-002 strict target matrix`: green (`4/4`).
  - `BUG-2026-02-06-002 thesis target`: green (checked + unchecked).
  - focused guards (make-const generalization, redirected let-use polymorphism, H15 non-leak): green.
  - sentinel matrix has been graduated to strict assertions (no pending cases under `BUG-2026-02-06-002`).


## 2026-02-10 BUG-2026-02-06-002 final closure notes

- Witness normalization now enforces thesis-shape upstream for graft/weaken interactions:
  - canonical ambiguous mapping rejects multiple canonical graft args for one weakened binder,
  - delayed graft/weaken pairs are coalesced safely before Ω translation.
- Ω translation is local again:
  - standalone `OpGraft` no longer performs delayed non-local weaken scan,
  - binder `TBottom` rescue is scoped to adjacent `OpGraft+OpWeaken` only.
- Scheme simplification preserves named structured bounds (`simplifySchemeBindings` blocks structured-bound inline for named binders), preventing Phase 6 dependency/bound erasure regressions.
- ALet fallback now has two scoped branches:
  - existing app/unbounded/Int-codomain path,
  - lambda replacement path with env-aware RHS typing and `subst = IntMap.empty` when replacing the scheme.
- Verification:
  - `BUG-2026-02-06-002 strict target matrix`: PASS (`4/4`)
  - full gate: `cabal build all && cabal test` => PASS (`601 examples, 0 failures`)
