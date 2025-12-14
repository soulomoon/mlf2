# Implementation Notes

## Summary of Changes

### 1. src/MLF/Presolution.hs
- **`unifyStructure`**: Added a helper function to recursively unify the children of structural nodes (TyArrow, TyForall) when merging them. This ensures that constraints like `Arrow A B ~ Arrow C D` propagate `A~C` and `B~D`.
- **`processInstEdge`**:
  - Updated to use `unifyStructure` instead of `unifyAcyclic`.
  - Added logic to eagerly materialize expansion results for non-Identity expansions and unify them with the target node. This fixes "missing unification" bugs where instantiation happened but the result wasn't linked to the target.
  - Guarded against `Identity` expansion cycles by skipping `TyExp ~ Target` unification when expansion is `Identity` (relying on `decideMinimalExpansion` unifications instead).
- **Per-edge instance witnesses (`Φ` input)**:
  - Presolution now records an `EdgeWitness` for each processed instantiation edge (`psEdgeWitnesses` / `prEdgeWitnesses`).
  - Today this witness is derived conservatively from the chosen `Expansion` recipe (covers the operations our presolution currently produces: `OpRaise` for `ExpForall`, and `OpGraft` + `OpWeaken` pairs for `ExpInstantiate`).
  - `ExpInstantiate` witness/application logic skips “vacuous” `TyForall` wrappers (quantifier levels with no binders) so `Φ` construction doesn’t fail on nested/structural ∀ nodes.
- **`rewriteConstraint`**: Fixed `canonical` logic (`chaseMap . frWith uf`) to prioritize Union-Find redirection over node removal mapping, ensuring that eagerly unified nodes are correctly resolved.

### 2. src/MLF/Normalize.hs
- **`applyUnionFindToConstraint`**: Enhanced to perform "grafting". When a `TyVar` node is unified with a structural node (e.g., `TyBase`), the `TyVar` node in the graph is destructively updated to become a copy of that structure. This ensures that external references to the variable (like the expression root) see the inferred structure.

### 3. src/MLF/Elab.hs
- **`generalizeAt`**:
  - Optimized to handle structural `TyForall` nodes (avoiding double quantification).
  - Modified to return the `subst` (renaming map) along with the scheme.
- **`substInTerm` / `substInType`**: Implemented substitution functions to apply the renaming map from `generalizeAt` to the elaborated term body. This ensures that terms use the same variable names as their type schemes (e.g., `Λa. λx:a. x` instead of `Λa. λx:t0. x`).
- **`elaborate`**: Updated to apply substitution to the RHS of let-bindings.
- **Witness translation (`Φ`) + quantifier reordering (`Σ`)**:
  - Elaboration now reifies instantiations from recorded per-edge witnesses (`prEdgeWitnesses`) via `phiFromEdgeWitness` (rather than via `expansionToInst`).
  - Current `Φ` support is intentionally small: `OpRaise` → `InstIntro` (`O`), and `OpGraft`+`OpWeaken` → `InstApp` (⟨τ⟩).
  - Implemented explicit quantifier reordering instantiations (`sigmaReorder`) using adjacent swaps per `papers/xmlf.txt` §3.4.
  - Implemented `applyInstantiation` to check/apply xMLF instantiations to xMLF types (xmlf Fig. 3), which is used by tests to validate that `Φ(e)` actually transforms the source type into the target type.
- **`expansionToInst`**: Kept as a legacy/debug conversion from `Expansion` to `Instantiation` (no longer the main path for elaboration).
- **`runPipelineElab`**: Updated to generalize the top-level result, consistent with xMLF's pervasive generalization.

## Testing
- **`test/ElaborationSpec.hs`**: Updated expectations to reflect correct polymorphic behavior and variable naming. Added integration tests for polymorphic instantiation.
- **Witness translation tests**: Added focused tests for `Σ(g)` reordering and for `Φ` soundness (`applyInstantiation source Φ(e) == target` for representative instantiation edges).
- **`test/PresolutionSpec.hs`**: Verified that instantiation edges merge nodes correctly.

All tests passed.

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
| `b` | eMLF surface term | `src/MLF/Syntax.hs` (`Expr`, plus `SrcType`/`SrcScheme`) |
| `χ` | constraint graph | `src/MLF/Types.hs` (`Constraint`) |
| `n` | type node in the graph | `NodeId` + `TyNode` in `Constraint.cNodes` |
| `g` | generalization level (“G-node”) | `GNodeId` + `GNode` in `Constraint.cGNodes` |
| `≤` edge | instantiation constraint | `InstEdge` (`Constraint.cInstEdges`) |
| `=` edge | unification constraint | `UnifyEdge` (`Constraint.cUnifyEdges`) |
| `s·τ` | expansion node / expansion variable | `TyExp{ tnExpVar :: ExpVarId }` + `Expansion` recipes in `Presolution` |
| `χp` | (principal) presolution | `MLF.Presolution.PresolutionResult` (plus `prEdgeExpansions`) |
| `τ` | xMLF type | `src/MLF/Elab.hs` (`ElabType`) |
| `φ` | xMLF instantiation witness | `src/MLF/Elab.hs` (`Instantiation`) |
| `a` | xMLF term | `src/MLF/Elab.hs` (`ElabTerm`) |

### Mapping: solver + elaboration phases → modules

| Phase | Role (paper) | Repo entry point |
|------:|--------------|------------------|
| 1 | Constraint generation | `MLF.ConstraintGen.generateConstraints` |
| 2 | Local simplification (grafting/merging) | `MLF.Normalize.normalize` |
| 3 | Acyclicity / dependency ordering | `MLF.Acyclicity.checkAcyclicity` |
| 4 | Presolution (minimal expansions) | `MLF.Presolution.computePresolution` |
| 5 | Global unification | `MLF.Solve.solveUnify` |
| 6 | Elaborate to xMLF | `MLF.Elab.elaborate` / `MLF.Elab.runPipelineElab` |

### Alignment notes / known gaps vs `xmlf.txt` §3

- **Witness translation (`Φ`)**: `xmlf.txt` translates *normalized instance-operation witnesses* (Fig. 10) into xMLF instantiations. This repo now records a per-edge `EdgeWitness` during presolution and translates it to an xMLF `Instantiation` via `MLF.Elab.phiFromEdgeWitness`. Currently, the recorded witnesses cover the operations induced by the presolution lattice we implement (`Raise`, `Graft`+`Weaken`); `Merge`/`RaiseMerge` are represented at the type level but are not yet emitted by presolution.
- **Quantifier reordering (`Σ(g)`)**: implemented as `MLF.Elab.sigmaReorder` (adjacent swaps per `xmlf.txt` §3.4). `phiFromEdgeWitness` uses the same swap machinery to bring a chosen binder to the front before instantiating it.
- **Application elaboration shape**: the paper’s Fig. 7 instantiates *both* sides (`b1` and `b2`) according to their edges. The current pipeline attaches an `EdgeId` expansion to the **application’s function position** and elaborates it as `ETyInst f inst` before applying; there is no separate per-argument instantiation edge.
- **Constraint representation differences**: `xmlf.txt`’s graphical presentation uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). This repo encodes scope using a separate `GNode` forest plus `tnVarLevel`/`tnOwnerLevel`/`tnQuantLevel` fields on `TyNode`s, and enforces the “no nested G-nodes” shape structurally.
- **xMLF Phase 7**: the repo currently has xMLF AST + pretty-printing, but not the full **type-checking rules (Fig. 4)** and **reduction semantics (Fig. 5)** from `xmlf.txt` (useful future work if we want to validate elaboration by re-checking and/or execute reductions).
