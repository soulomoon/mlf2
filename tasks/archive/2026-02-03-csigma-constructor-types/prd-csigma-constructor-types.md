# PRD: Add general constructor application types (`C σ…`) to eMLF/MLF pipeline

## 1. Introduction / Overview

The thesis (`papers/these-finale-english.txt`) defines an algebra of type constructors Σ with arity (arrow has arity 2) and includes constructor application in the type grammar (`σ ::= α | C σ | ∀α. σ`, and later System F restriction includes `C σ`). Today, the repo can *print/represent* constructor application at the xMLF layer (`TCon` exists in `src/MLF/Types/Elab.hs`), but the *surface* type annotations (`SrcType`) and the *constraint graph term-DAG* (`TyNode`) cannot represent general constructor application beyond arrows and nullary base types.

This PRD adds end-to-end support for general constructor application types in the pipeline so that:
- Surface annotations can express `C σ₁ … σₙ` (n ≥ 1),
- Constraint generation internalizes them into the graph,
- Normalization/unification/presolution copying/reification/elaboration preserve them,
- Arity is validated (paper-faithful “Σ with arity”), and a key well-formedness condition is enforced: a forall binder is not in scope for its own bound.

## 2. Goals

- Represent general constructor application (`C σ₁ … σₙ`, n ≥ 1) in surface annotation AST and constraint graph.
- Internalize constructor types in both annotation types and coercion types during Phase 1 (ConstraintGen).
- Enforce arity consistency for constructor symbols within a pipeline run.
- Enforce well-formedness: in `∀(α ⩾ τ). σ`, `α` is **not** in scope in `τ` and `α ∉ ftv(τ)` (reject invalid bounds early).
- Update all affected solver/elaboration phases to handle the new node without partial pattern matches (`-Wall`).
- Add Hspec tests that cover the new functionality across phases.

## 3. User Stories

### US-001: Extend surface type annotations with constructor application
**Description:** As a maintainer, I want `SrcType` to represent constructor application types so that users/tests can express the paper’s `C σ…` forms in annotations.

**Acceptance Criteria:**
- [ ] Add `STCon String (NonEmpty SrcType)` to `SrcType` in `src/MLF/Frontend/Syntax.hs`.
- [ ] Extend `SrcTypeF`/`Recursive`/`Corecursive` to include `STConF String (NonEmpty a)`.
- [ ] Existing uses of `STBase`, `STArrow`, `STForall`, `STBottom` continue to compile unchanged.
- [ ] `cabal build all` succeeds with `-Wall` enabled.

### US-002: Extend constraint graph term-DAG with constructor nodes
**Description:** As a maintainer, I want the constraint term-DAG (`TyNode`) to represent constructor application nodes so that phases 2–6 can preserve `C σ…` structure.

**Acceptance Criteria:**
- [ ] Add `TyCon { tnId :: NodeId, tnCon :: BaseTy, tnArgs :: NonEmpty NodeId }` to `TyNode` in `src/MLF/Constraint/Types/Graph/NodeEdge.hs`.
- [ ] Update `structuralChildren` and `structuralChildrenWithBounds` so `TyCon` children are its args in a stable order.
- [ ] Add `allocCon :: BaseTy -> NonEmpty NodeId -> ConstraintM NodeId` to `src/MLF/Frontend/ConstraintGen/Emit.hs` and ensure it sets default binding parents for all args (like `allocArrow` does).
- [ ] All modules that pattern-match on `TyNode` compile without non-exhaustive matches.

### US-003: Internalize constructor types in ConstraintGen (+ enforce arity + forall-bound well-formedness)
**Description:** As a maintainer, I want Phase 1 to internalize `STCon` types into `TyCon` nodes and reject ill-formed bounds, so that annotations and coercions can use `C σ…` safely and paper-faithfully.

**Acceptance Criteria:**
- [ ] Extend `BuildState` with an arity map `bsTyConArity :: Map BaseTy Int` in `src/MLF/Frontend/ConstraintGen/State.hs` (initialized empty).
- [ ] Extend `ConstraintError` in `src/MLF/Frontend/ConstraintGen/Types.hs` with:
  - [ ] `TypeConstructorArityMismatch BaseTy Int Int` (expected vs actual)
  - [ ] `ForallBoundMentionsBinder String`
- [ ] Implement arity registration during internalization:
  - [ ] `STBase name` registers arity 0 for `BaseTy name`
  - [ ] `STCon con args` registers arity `length args` for `BaseTy con`
  - [ ] Mismatches produce `TypeConstructorArityMismatch`
- [ ] Implement `STCon` internalization in both:
  - [ ] `internalizeSrcTypeWith` (`src/MLF/Frontend/ConstraintGen/Translate.hs`)
  - [ ] `internalizeCoercionCopy` (`src/MLF/Frontend/ConstraintGen/Translate.hs`)
- [ ] Enforce well-formedness of forall bounds:
  - [ ] In `∀(α ⩾ τ). σ`, the binder `α` is **not** in scope in `τ`
  - [ ] If `α ∈ ftv(τ)`, throw `ForallBoundMentionsBinder α`
  - [ ] Internalize the bound `τ` without inserting `α` into the type env for that bound
- [ ] `cabal build all` succeeds.

### US-004: Support constructor nodes in normalization and unification
**Description:** As a maintainer, I want Phase 2 normalization and Phase 5 unification to treat constructor nodes like other structural nodes so that constraints involving `C σ…` are simplified/solved correctly.

**Acceptance Criteria:**
- [ ] Update `src/MLF/Constraint/Normalize.hs`:
  - [ ] Treat `Var ≤ TyCon` as graftable when the head is not polymorphic (`cPolySyms`) (align with existing `Var ≤ Base` restriction).
  - [ ] Implement grafting for `Var ≤ TyCon`: rewrite var to `TyCon` with fresh arg vars and emit unify edges for each arg.
  - [ ] Implement decomposition for `TyCon ≤ TyCon` with same head + same arity.
  - [ ] Keep mismatches as type errors (inst edge retained), consistent with existing behavior.
- [ ] Update `src/MLF/Constraint/Unify/Decompose.hs` to decompose `TyCon/TyCon` into arg equalities (or produce a constructor mismatch).
- [ ] Update `src/MLF/Constraint/Solve.hs` mismatch reporting as needed (either generic `ConstructorClash` or a more specific error; behavior must be test-covered).
- [ ] `cabal test` passes.

### US-005: Preserve constructor nodes through presolution copying, reification, and Φ context traversal
**Description:** As a maintainer, I want presolution copying and reification to preserve constructor application so that elaboration can emit xMLF `TCon` terms/types.

**Acceptance Criteria:**
- [ ] Update presolution copy logic in `src/MLF/Constraint/Presolution/Copy.hs` to copy `TyCon` nodes (placeholder creation + recursive child copy).
- [ ] Update reification in `src/MLF/Reify/Core.hs` to reify `TyCon` into `TCon` with reified args.
- [ ] Update Φ context traversal in `src/MLF/Elab/Phi/Context.hs` to descend into `TyCon` args like it does for arrows (and treat it as a “structural” node for `needsInsideRoot`).
- [ ] Update any “sticky parent / structural node” classifiers to include `TyCon` where appropriate (e.g. `src/MLF/Frontend/ConstraintGen/Scope.hs`, inertness anchors if needed).
- [ ] `cabal test` passes.

### US-006: Add regression tests for constructor types across phases
**Description:** As a maintainer, I want tests covering `C σ…` so that future refactors don’t regress constructor support.

**Acceptance Criteria:**
- [ ] Add ConstraintGen tests in `test/ConstraintGenSpec.hs`:
  - [ ] annotation internalizes `STCon "List" (STBase "Int" :| [])` into a `TyCon` node with the correct head/arg structure
  - [ ] arity mismatch triggers `TypeConstructorArityMismatch`
  - [ ] `∀(a ⩾ a). …` (or another bound that mentions the binder) triggers `ForallBoundMentionsBinder`
- [ ] Add Normalize tests in `test/NormalizeSpec.hs`:
  - [ ] grafting `α ≤ C τ` consumes the inst edge and increases node count appropriately
  - [ ] `C … ≤ C …` decomposes/solves when heads+arity match
- [ ] Add Solve tests in `test/SolveSpec.hs`:
  - [ ] `TyCon` unifies structurally (args unified)
  - [ ] `TyCon` vs `TyBase` / `TyArrow` produces a constructor-clash error
  - [ ] occurs-check fails when unifying a var with a `TyCon` that (transitively) contains it
- [ ] Optionally add a reify/elaboration assertion that a `TyCon` reifies to xMLF `TCon` (if easiest, do this via a direct constraint fixture rather than adding a parser).
- [ ] `cabal test` passes.

## 4. Functional Requirements

1. **FR-1:** The system must provide a surface annotation type constructor for general constructor application: `STCon con args` with `args` non-empty.
2. **FR-2:** The constraint term-DAG must represent constructor application nodes `TyCon con args`, with stable child ordering.
3. **FR-3:** Constraint generation must internalize `STCon` into `TyCon` for both annotation types and coercion types.
4. **FR-4:** Constructor symbols must have consistent arity within a single pipeline run (paper “Σ with arity”); arity mismatches must be rejected deterministically.
5. **FR-5:** Forall bounds must be well-formed: binder not in scope for its bound; bounds mentioning the binder must fail with a dedicated error.
6. **FR-6:** Normalization must support grafting/decomposition involving constructor application nodes, consistent with existing arrow/base rules.
7. **FR-7:** Unification decomposition must support `TyCon/TyCon` and reject head/arity mismatches.
8. **FR-8:** Presolution copying, reification, and Φ context traversal must preserve `TyCon` structure through Phase 6 so xMLF output can contain `TCon`.

## 5. Non-Goals (Out of Scope)

- No parser/frontend syntax work for constructor types (tests can construct `SrcType` directly); this PRD is about core pipeline representation and correctness.
- No kind system or kind inference (arity consistency only).
- No global “Σ definition” configuration file; arity is enforced by “first use defines arity” within a run (any global Σ support can be a follow-up PRD).
- No changes to xMLF type AST (`TCon` already exists).

## 6. Design Considerations

- Keep the existing 0-ary convenience constructors:
  - `STBase String` (surface) and `TyBase BaseTy` (constraint graph)
  - This avoids a large refactor and preserves existing tests/code.
- Use `NonEmpty` for `STCon`/`TyCon` args to model the thesis grammar `C σ` (at least one argument) while keeping `STBase`/`TyBase` as explicit nullary constructors.
- Arity enforcement is per-run and deterministic:
  - First time a constructor symbol appears, record its arity.
  - Subsequent uses must match or fail with `TypeConstructorArityMismatch`.

## 7. Technical Considerations

Primary modules expected to change (non-exhaustive):
- Surface types: `src/MLF/Frontend/Syntax.hs`
- Constraint nodes: `src/MLF/Constraint/Types/Graph/NodeEdge.hs`, `src/MLF/Constraint/Types/Graph.hs`
- ConstraintGen allocators/internalization: `src/MLF/Frontend/ConstraintGen/Emit.hs`, `src/MLF/Frontend/ConstraintGen/State.hs`, `src/MLF/Frontend/ConstraintGen/Types.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`
- Normalization/unification: `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Unify/Decompose.hs`, `src/MLF/Constraint/Solve.hs`
- Presolution copy: `src/MLF/Constraint/Presolution/Copy.hs`
- Reification and Φ contexts: `src/MLF/Reify/Core.hs`, `src/MLF/Elab/Phi/Context.hs`

Paper-faithfulness notes:
- Thesis source of truth: `papers/these-finale-english.txt` §1.5 (Σ + arity; `C σ`), §5.2.4 (Poly), and Ch. 14 syntax restriction including `C σ`.
- If the implementation deviates (e.g., lack of global Σ), document it in-code (Note blocks) and via tests.

## 8. Success Metrics

- A program/test that uses a constructor annotation (e.g. `STCon "List" (STBase "Int" :| [])`) survives the full pipeline and reifies to an xMLF type containing `TCon (BaseTy "List") …`.
- `cabal build all` and `cabal test` pass with `-Wall`.
- New tests provide regression coverage for:
  - constructor internalization
  - arity mismatch detection
  - forall-bound well-formedness rejection
  - normalization/unification on constructor nodes

## 9. Open Questions

1. Should polymorphic symbol handling (`cPolySyms`, thesis `Poly`) apply to `TyCon` heads the same way it applies to `TyBase` today? (Recommended: yes, treat `TyCon` with head in `Poly` as an intrinsically-polymorphic anchor and avoid grafting it in normalization.)
2. Should we eventually replace “first use defines arity” with an explicit Σ/arity configuration (e.g. in `PipelineConfig`)? (Out of scope for this PRD; tracked as a follow-up.)

