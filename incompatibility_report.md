# Incompatibility Report: `MLF.Elab.Pipeline` vs `papers/xmlf.txt`

## Overview
This report documents the analysis of `src/MLF/Elab/Pipeline.hs` and `papers/xmlf.txt`, identifying incompatibilities and implementation gaps that prevented the Haskell implementation from correctly reflecting the xMLF specification. The focus was on the elaboration pipeline, specifically the transition from constraint solving (presolution) to explicit xMLF terms.

## Identified Incompatibilities & Issues

### 1. Instantiation Logic & Constraint Propagation (Resolved; was Critical)
**Specification**: `xmlf.txt` (Section 3.2) describes the translation of instantiation edges into explicit type instantiations. It assumes that the constraint graph is fully solved, meaning all instantiation edges are processed and their constraints (unifications) are propagated to the underlying types.
**Implementation**: `src/MLF/Constraint/Presolution/Core.hs` (Phase 4) previously failed to propagate unification constraints derived from instantiation edges in certain cases:
- **Issue**: When an instantiation edge `g -> n` required `n` to match a structure (e.g., `Arrow`), the implementation merged the nodes but failed to recursively unify their children (e.g., `Dom` and `Cod`).
- **Impact**: Type inference failed to unify argument types with parameter types in polymorphic applications (e.g., `(\x. x) 1` resulted in type `∀a. a` instead of `Int`), leading to unsound elaboration.
- **Fix**: Implemented `unifyStructure` in `src/MLF/Constraint/Presolution/Core.hs` to recursively unify children of merged structural nodes.

### 2. Variable Grafting in Normalization (Resolved; was Critical)
**Specification**: `xmlf.txt` implies that when a variable is constrained to be equal to a structure (e.g., `α = Int`), the variable effectively "becomes" that structure in the solution.
**Implementation**: `src/MLF/Constraint/Normalize.hs` (Phase 2) updated the Union-Find structure but did not "graft" the structure onto the variable node in the graph itself.
- **Issue**: External references (like the root of the expression) pointed to the original `TyVar` node. Even though UF linked it to `TyBase`, `reifyType` (in Elab) saw the `TyVar` and printed a generic variable (e.g., `a` instead of `Int`).
- **Impact**: Incorrect type reporting for expressions that should have been monomorphized by constraints.
- **Fix**: Modified `applyUnionFindToConstraint` in `src/MLF/Constraint/Normalize.hs` to destructive update `TyVar` nodes to become copies of their canonical structural representatives.

### 3. Generalization & Naming (Resolved; was Major)
**Specification**: `xmlf.txt` (Section 3.2) states that generalization (introducing `Λ`) applies to applications and abstractions, not just let-bindings.
**Implementation**:
- **Issue**: `runPipelineElab` returned the raw type of the expression root, which was often monomorphic (e.g., `t0 -> t0`) even for polymorphic values like `\x. x`.
- **Fix**: Updated `runPipelineElab` to explicitly generalize the root node at the top level (the program’s binding-tree root), consistent with the paper's observation that xMLF generalizes more freely.
- **Issue**: The elaboration of terms (`ELet`, `ELam`) did not apply the variable renaming computed during generalization. A term `let id = \x. x` was generalized to `∀a. a -> a`, but the body remained `\x:t0. x`, creating a disconnect between the quantifier `a` and the usage `t0`.
- **Fix**: Modified `generalizeAt` to return the substitution map, and implemented `substInTerm` to apply this renaming to the elaborated term body.

### 4. Structural Forall Decomposition (Resolved; was Minor)
**Specification**: `xmlf.txt` describes how `Forall` nodes in the constraint graph map to `Λ` binders.
**Implementation**: `generalizeAt` in `src/MLF/Elab/Pipeline.hs` blindly created new `Forall` wrappers even if the target node was already a `TyForall` with the correct level (structural forall).
- **Issue**: "Double quantification" (e.g., `∀a. ∀t3. a -> a`).
- **Fix**: Optimized `generalizeAt` to detect structural foralls and reuse their body, avoiding redundant quantification.

### 5. Witness translation Φ (Fig. 10) and quantifier reordering Σ(g) (Implemented; previously a gap)
**Specification**: `xmlf.txt` §3.4 / Fig. 10 translates **normalized instance-operation witnesses** (grafting/merging/raising/weakening, plus RaiseMerge) into explicit xMLF instantiations \(Φ\), and requires a separate quantifier reordering instantiation \(Σ(g)\) in some cases (“Reordering quantifiers”).

**Implementation**:
- **Phase 4 witness recording**:
  - `src/MLF/Constraint/Types.hs`: `InstanceOp`, `InstanceWitness`, `EdgeWitness`
  - `src/MLF/Constraint/Presolution/Core.hs`: records `prEdgeWitnesses :: IntMap EdgeWitness` (canonicalized during `rewriteConstraint`)
- **Phase 6 translation + checking**:
  - `src/MLF/Elab/Pipeline.hs`: `phiFromEdgeWitness` (Φ), `sigmaReorder` (Σ), and `applyInstantiation` (xmlf Fig. 3) for applying/checking instantiations against xMLF types.
  - Elaboration now reifies instantiations from `prEdgeWitnesses` (instead of relying on `expansionToInst`).

**Remaining mismatch vs the full paper**:
- Witnesses are derived from the chosen presolution expansion recipe (`Expansion`) plus edge-local unification tracing, and therefore cover the operations the solver currently produces (`Raise`, `Graft`+`Weaken`, `Merge`, `RaiseMerge`). Normalization is still lightweight compared to the full Yakobowski’08 normalization machinery, and some instantiation-context cases from the paper remain under-tested.

## Verification
- **Unit Tests**: `PresolutionSpec.hs` verified that instantiation edges now correctly merge nodes and propagate constraints.
- **Integration Tests**: `ElaborationSpec.hs` verified that:
  - Polymorphic let-bindings elaborate to `∀a. a -> a`.
  - Polymorphic instantiation works (`id 1` -> `Int`).
  - Variable names in terms match the scheme quantifiers.
  - Basic expressions like `\x. x` are generalized.
- **Φ/Σ Tests**: `ElaborationSpec.hs` includes focused tests that:
  - validate `Σ(g)` by reordering quantifiers and checking `applyInstantiation src Σ == tgt`;
  - validate `Φ` soundness for representative instantiation edges by checking `applyInstantiation source Φ(e) == target`.

## Conclusion
The implementation in `src/MLF/Elab/Pipeline.hs`, `src/MLF/Constraint/Presolution/Core.hs`, and `src/MLF/Constraint/Normalize.hs` is now substantially closer to the `xmlf.txt` specification: the end-to-end pipeline is stable, and Φ/Σ are present as explicit instantiations derived from recorded per-edge witnesses. The main remaining gap versus the paper is to record and translate the full range of **normalized** instance operations (especially `Merge`/`RaiseMerge` and their instantiation contexts) once the solver begins emitting them.
