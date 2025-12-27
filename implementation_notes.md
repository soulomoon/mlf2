# Implementation Notes

## Summary of Changes

### 1. src/MLF/Constraint/Presolution/Core.hs
- **`unifyStructure`**: Added a helper function to recursively unify the children of structural nodes (TyArrow, TyForall) when merging them. This ensures that constraints like `Arrow A B ~ Arrow C D` propagate `A~C` and `B~D`.
- **`processInstEdge`**:
  - Updated to use `unifyStructure` instead of `unifyAcyclic`.
  - Added logic to eagerly materialize expansion results for non-Identity expansions and unify them with the target node. This fixes "missing unification" bugs where instantiation happened but the result wasn't linked to the target.
  - Guarded against `Identity` expansion cycles by skipping `TyExp ~ Target` unification when expansion is `Identity` (relying on `decideMinimalExpansion` unifications instead).
- **Per-edge instance witnesses (`Φ` input)**:
  - Presolution now records an `EdgeWitness` for each processed instantiation edge (`psEdgeWitnesses` / `prEdgeWitnesses`).
  - Today this witness is derived conservatively from the chosen `Expansion` recipe plus edge-local unification tracing while solving that instantiation edge.
    - `ExpForall` contributes `EdgeWitness.ewForallIntros` (xMLF quantifier-introduction `O`), not an Ω op (paper `xmlf.txt` Figure 10).
    - `ExpInstantiate` contributes per-binder Ω ops (`OpGraft`/`OpWeaken`/`OpMerge`).
  - Recorded op sequences are normalized (`MLF.Constraint.Presolution.normalizeInstanceOps`) so that binders are not eliminated twice and `Weaken` operations appear after other ops targeting the same binder (aligning with the paper’s “normalized Ω” expectations in Fig. 10).
  - `ExpInstantiate` witness/application logic skips “vacuous” `TyForall` wrappers (quantifier levels with no binders) so `Φ` construction doesn’t fail on nested/structural ∀ nodes.
  - `ExpInstantiate` witnesses now avoid emitting invalid grafts under non-⊥ bounds: if a binder has an instance bound that is another in-scope variable (e.g. `b ⩾ a`), presolution emits `OpMerge(b, a)` rather than `OpGraft` (paper Fig. 10 “alias + eliminate”).
  - Presolution can also record `OpRaiseMerge(b, m)` when unification forces a **bounded** binder’s instantiation meta to unify with a `TyVar` bound **above the instantiation-edge root** in the binding tree, and `m` is not already the binder’s bound. This matches the paper’s “escape to bound-above node” shape.
- **Scope tracking (paper `Raise` as graph transformation)**:
  - TyVar/TyVar unions harmonize binding parents by executing the paper `Raise(n)` graph operation as a binding-edge rewrite on `Constraint.cBindParents` (`MLF.Binding.Adjustment` / `MLF.Binding.GraphOps`).
  - During instantiation-edge solving (χe), the same per-step raises are also recorded as `OpRaise` in the edge witness Ω (`unifyAcyclicRawWithRaiseCounts` → `unifyAcyclicEdge` / `unifyAcyclicEdgeNoMerge`), aligning with `papers/xmlf.txt` §3.4 / Fig. 10.
  - Variable bounds and eliminations are stored in `Constraint.cVarBounds` / `Constraint.cEliminatedVars` (`MLF.Constraint.VarStore`) and are looked up by canonical `NodeId`, so they stay consistent as binding edges and UF representatives change.
- **`materializeExpansions`**: Avoids duplicating fresh nodes by reusing the already-unified expansion result for non-Identity expansions; Identity expansions still rewrite `TyExp` wrappers to their bodies.
- **`rewriteConstraint`**: Ensures Identity `TyExp` wrappers are erased even when they are not the Union-Find root (redirecting the whole UF class to the wrapper’s body). This fixes over-generalization bugs in paper-alignment baselines like `let id = (\x. x) in id id` and `\y. let id = (\x. x) in id y`.

### 2. src/MLF/Constraint/Normalize.hs
- **`applyUnionFindToConstraint`**: Enhanced to perform "grafting". When a `TyVar` node is unified with a structural node (e.g., `TyBase`), the `TyVar` node in the graph is destructively updated to become a copy of that structure. This ensures that external references to the variable (like the expression root) see the inferred structure.
- **Binding-edge Raise harmonization**: Var-var merging harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.

### 3. src/MLF/Constraint/Solve.hs
- **Binding-edge Raise harmonization**: Phase 5 harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.

### 4. src/MLF/Elab/Pipeline.hs + src/MLF/Elab/Types.hs
- **`generalizeAt`**:
  - Optimized to handle structural `TyForall` nodes (avoiding double quantification).
  - Modified to return the `subst` (renaming map) along with the scheme.
- **Scope follows the solved graph**:
  - Elaboration enumerates binders from the binding tree (`Constraint.cBindParents` via `MLF.Binding.Tree.orderedBinders`) and consults `MLF.Constraint.VarStore` for bounds/eliminations; when a site has no direct binders (common for the top-level scheme), it falls back to HM-style free-variable generalization.
- **`substInTerm` / `substInType`**: Implemented substitution functions to apply the renaming map from `generalizeAt` to the elaborated term body. This ensures that terms use the same variable names as their type schemes (e.g., `Λa. λx:a. x` instead of `Λa. λx:t0. x`).
- **`elaborate`**: Updated to apply substitution to the RHS of let-bindings.
- **Witness translation (`Φ`) + quantifier reordering (`Σ`)**:
  - Elaboration now reifies instantiations from recorded per-edge witnesses (`prEdgeWitnesses`) via `phiFromEdgeWitness` (rather than via `expansionToInst`).
  - Current `Φ` support matches what presolution emits today (`ewForallIntros` → `InstIntro` (`O`) appended after translating Ω, and `OpGraft`+`OpWeaken` → `InstApp` (⟨τ⟩)); `phiFromEdgeWitness` also supports `OpMerge`/`OpRaise`/`OpRaiseMerge` (paper Fig. 10) as “alias + eliminate” / “raise” instantiations with paper-faithful binder-raise-first ordering.
  - Implemented explicit quantifier reordering instantiations (`sigmaReorder`) using adjacent swaps per `papers/xmlf.txt` §3.4.
  - Implemented `applyInstantiation` to check/apply xMLF instantiations to xMLF types (xmlf Fig. 3), which is used by tests to validate that `Φ(e)` actually transforms the source type into the target type.
- **`expansionToInst`**: Kept as a legacy/debug conversion from `Expansion` to `Instantiation` (no longer the main path for elaboration).
- **`runPipelineElab`**: Updated to generalize the top-level result, consistent with xMLF's pervasive generalization.

## Testing
- **`test/ElaborationSpec.hs`**: Updated expectations to reflect correct polymorphic behavior and variable naming. Added integration tests for polymorphic instantiation.
- **Witness translation tests**: Added focused tests for `Σ(g)` reordering and for `Φ` soundness (`applyInstantiation source Φ(e) == target` for representative instantiation edges).
- **`test/PresolutionSpec.hs`**: Verified that instantiation edges merge nodes correctly.

Note: `test/ElaborationSpec.hs` also contains **paper-alignment baseline tests** that serve as regression coverage while we continue aligning witnesses toward `papers/xmlf.txt` (especially around Merge/RaiseMerge and aliasing behavior).

## `papers/xmlf.txt` study: paper ↔ repo mapping

This repo’s design is primarily informed by:

- `papers/xmlf.txt` (Rémy & Yakobowski) for **xMLF**’s explicit types/instantiations/terms and the **elaboration** story (§3).
- The earlier “graphic constraints” papers (ICFP’08 / TLDI’07) for the **solver pipeline** that produces presolutions.

### Paper anchors (what we used from `xmlf.txt`)

- **Fig. 1–4**: xMLF grammar, instantiation judgments, instantiation-as-a-function on types, and xMLF term typing rules.
- **§3.1–§3.5 + Fig. 7/9/10**: elaboration from (graphical) eMLF presolutions to xMLF:
  - `/)(g) = Λ(Q(g))` (insert type abstractions for flexible bindings at a level)
  - `Φ(e)` (compute instantiation witnesses from solved instantiation edges)
  - `S/Q/T` (map presolution nodes to xMLF types)
  - `Σ(g)` (quantifier reordering when the expansion’s quantifier order differs)

### Mapping: paper notation → repo types/functions

| Paper | Meaning | Repo |
|------:|---------|------|
| `b` | eMLF surface term | `src/MLF/Frontend/Syntax.hs` (`Expr`, plus `SrcType`/`SrcScheme`) |
| `χ` | constraint graph | `src/MLF/Constraint/Types.hs` (`Constraint`) |
| `n` | type node in the graph | `NodeId` + `TyNode` in `Constraint.cNodes` |
| `g` | binding-tree node (generalization site) | `NodeId` (often a `TyForall` node) + `Constraint.cBindParents` |
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

### Alignment notes / known gaps vs `xmlf.txt` §3
- **Witness translation (`Φ`)**: `xmlf.txt` translates *normalized instance-operation witnesses* (Fig. 10) into xMLF instantiations. This repo records a per-edge `EdgeWitness` during presolution and translates it to an xMLF `Instantiation` via `MLF.Elab.Pipeline.phiFromEdgeWitness`.
  - Quantifier-introduction (`O`) is not part of Ω in `xmlf.txt`; the repo records these steps separately as `EdgeWitness.ewForallIntros` (from `ExpForall`) and appends them directly when constructing Φ(e).
  - Ω ops emitted today include `OpGraft`+`OpWeaken`, `OpMerge` (bounded aliasing like `b ⩾ a`, plus unification-induced aliasing during instantiation-edge solving), `OpRaise` (paper-general binding-edge raising on arbitrary interior nodes), and `OpRaiseMerge` for bounded-binder “escape” patterns. χe execution is paper-shaped for binding-tree ops: Raise/Weaken are executable binding-edge rewrites, and `EdgeTrace.etInterior` records the exact paper interior `I(r)` for filtering.
- **Quantifier reordering (`Σ(g)`)**: implemented as `MLF.Elab.Pipeline.sigmaReorder` (adjacent swaps per `xmlf.txt` §3.4). `phiFromEdgeWitness` targets binders using `InstUnder` instantiation contexts (paper’s `C{·}`) rather than swapping quantifiers; `sigmaReorder` remains available as an explicit/validated Σ construction when reordering is required.
- **Application elaboration shape**: the paper’s Fig. 7 instantiates *both* sides (`b1` and `b2`) according to their edges. The current pipeline attaches an `EdgeId` expansion to the **application’s function position** and elaborates it as `ETyInst f inst` before applying; there is no separate per-argument instantiation edge.
- **Constraint representation differences**: `xmlf.txt`’s graphical presentation uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). The repo mirrors the same split (`Constraint.cNodes` + `Constraint.cBindParents` with `BindFlex`/`BindRigid`); some paper machinery remains simplified (e.g. lightweight witness normalization and a top-level generalization fallback when the root has no direct binders).
- **xMLF Phase 7**: the repo currently has xMLF AST + pretty-printing, but not the full **type-checking rules (Fig. 4)** and **reduction semantics (Fig. 5)** from `xmlf.txt` (useful future work if we want to validate elaboration by re-checking and/or execute reductions).
