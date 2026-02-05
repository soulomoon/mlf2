# Implementation Notes

## Summary of Changes

**Current vs target:** The current pipeline records presolution witnesses and produces explicit generalization plans in `MLF.Constraint.Presolution.Plan`; elaboration applies these plans via `MLF.Elab.Generalize` without re-solving. The remaining paper-faithfulness deltas are tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` (constructor types `Cσ` and stricter translatability validation for Φ).

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
  - When an expansion includes a later `ExpForall`, `ExpInstantiate` witnesses suppress `OpWeaken` so binder metas stay flexible until the new quantifier is introduced (avoids empty Q(n) and lost ∀ in bounded-aliasing cases).
  - Edge-local unification can record `OpRaiseMerge(b, m)` when unification forces a **bounded** binder’s instantiation meta to unify with a `TyVar` bound **above the instantiation-edge root** in the binding tree (recorded as `OpRaise` + `OpMerge`, then normalized to `OpRaiseMerge`), matching the paper’s “escape to bound-above node” shape.
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
- **`runPipelineElab`**: Generalizes the top-level result using the nearest gen ancestor of the expression root (root gen node for top-level).

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
| `b` | eMLF surface term | `src/MLF/Frontend/Syntax.hs` (`Expr` + `SrcType`) |
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
  - Φ requires a representable translation context; missing contexts and other non-translatable cases are hard failures. Rigid identity handling is literal for Raise/Merge/RaiseMerge on operated node `n`; rigid only on the non-operated endpoint is rejected as non-translatable.
- **Trace root/interior coherence**: `EdgeTrace` root/interior refresh and normalization share a single root-selection helper (`traceInteriorRootRef`) so `etRoot`, `etInterior`, and witness normalization all use the same interpretation of `r`/`I(r)`.
- **Context search strictness**: `contextToNodeBound` follows thesis context grammar (under-quantifier / inside-bound) and does not use non-thesis fallback descent through `TyForall` body.
- **Quantifier reification (binding-tree based)**: `Q(n)`/reification quantifies flexibly bound `TyVar` binders using binding-parent edges (bounds included in reachability), so bounds and contexts remain representable in Φ and generalization.
- **Quantifier reordering (`Σ(g)` / `ϕR`)**: implemented via `MLF.Elab.Sigma` / `MLF.Elab.Pipeline.sigmaReorder` (adjacent swaps per `papers/these-finale-english.txt` Def. 15.3.4 / Fig. 15.3.5; see `papers/xmlf.txt` §3.4). Φ translation (`phiFromEdgeWitnessWithTrace` → `phiWithSchemeOmega`) prefixes Ω-translation with this reordering whenever `Typ(a′)` and `Typexp(a′)` disagree in binder order — even when Ω contains no Raise steps — while still targeting binders for Ω using `InstUnder` instantiation contexts (paper’s `C{·}`). The computation is deterministic and fail-fast: missing <P order keys or bound-dependency cycles produce `InstantiationError` messages prefixed `PhiReorder:` rather than silently returning `InstId`.
- **Application elaboration shape**: now matches Fig. 7 — constraint generation emits instantiation edges for both function and argument, and elaboration wraps each side with `ETyInst` when non-identity.
- **Constraint representation differences**: the thesis's graphical presentation (see also `papers/xmlf.txt`) uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). The repo mirrors the same split (`Constraint.cNodes` + `Constraint.cBindParents` with `BindFlex`/`BindRigid`); some paper machinery remains simplified (e.g. witness normalization/ordering is implemented but not yet backed by formal proofs, and fallback-removal relies on regression tests).
- **xMLF Phase 7**: the repo includes type-checking and reduction for xMLF terms/instantiations (`MLF.Elab.TypeCheck`, `MLF.Elab.Reduce`) and uses them in tests, but still lacks a fully formalized/verified connection to the thesis presentation (e.g., proof obligations and full evaluation-context coverage).

## Kiro spec planning
- Paper-faithfulness deltas are captured in `.kiro/specs/paper-faithfulness-remaining-deltas/`, including evidence pointers to the thesis and code, plus a concrete implementation plan.
