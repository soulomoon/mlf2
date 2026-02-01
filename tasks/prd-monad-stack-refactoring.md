# PRD: Monad Stack Simplification and Type Safety Improvements

## Introduction

Refactor the MLF codebase to reduce boilerplate from explicit `lift` calls by extending MTL typeclasses, introduce semantic newtypes to prevent type confusion, and expand test coverage. The codebase currently has 48 `lift $` calls across 5 modules, uses raw `IntMap`/`IntSet` for semantically different purposes, and has gaps in test coverage for error paths and property-based invariants.

## Goals

- Reduce explicit `lift` calls by 75% (from 48 to <12) through MTL typeclass extensions
- Introduce semantic newtypes for `NodeId`-keyed maps to prevent accidental mixing
- Achieve >80% test coverage for presolution error paths
- Add property-based tests for binding tree invariants
- Optionally refactor `Phi.hs` to use explicit environment passing instead of closure

## User Stories

### US-001: Extend MonadPresolution with common operations
**Description:** As a developer, I want frequently-used operations available directly in the `MonadPresolution` typeclass so that I don't need explicit `lift` calls.

**Acceptance Criteria:**
- [ ] Add `getNode :: NodeId -> m (Maybe TyNode)` to `MonadPresolution`
- [ ] Add `getCanonicalNode :: NodeId -> m (Maybe TyNode)` to `MonadPresolution`
- [ ] Add `lookupBindParent :: NodeId -> m (Maybe (NodeRef, BindFlag))` to `MonadPresolution`
- [ ] Add `modifyPresolution :: (Presolution -> Presolution) -> m ()` to `MonadPresolution`
- [ ] Update `PresolutionM` instance with implementations
- [ ] Update `EdgeUnifyM` instance to delegate via `liftPresolution`
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-002: Reduce lift calls in EdgeUnify.hs
**Description:** As a developer, I want to use typeclass methods instead of `lift` in EdgeUnify.hs so that the code is more readable.

**Acceptance Criteria:**
- [ ] Replace `lift $ getConstraint` with `getConstraint` (uses MonadPresolution instance)
- [ ] Replace `lift $ modifyConstraint` with `modifyConstraint`
- [ ] Replace `lift $ getCanonical` with `getCanonical`
- [ ] Replace `lift $ throwPresolutionError` with `throwPresolutionError`
- [ ] Reduce `lift $` count in EdgeUnify.hs from 30 to <8
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-003: Add MonadSolve typeclass for Solve.hs
**Description:** As a developer, I want a `MonadSolve` typeclass so that solve-phase operations don't require explicit lifting.

**Acceptance Criteria:**
- [ ] Create `MonadSolve` typeclass in `Constraint/Solve.hs` with core operations
- [ ] Include `getConstraint`, `modifyConstraint`, `getUnionFind`, `throwSolveError`
- [ ] Create instance for the concrete `SolveM` monad
- [ ] Reduce `lift $` count in Solve.hs from 12 to <4
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-004: Reduce lift calls in Copy.hs and EdgeProcessing.hs
**Description:** As a developer, I want remaining presolution modules to use typeclass methods instead of explicit lifting.

**Acceptance Criteria:**
- [ ] Update Copy.hs to use `MonadPresolution` methods (currently 4 lift calls)
- [ ] Update EdgeProcessing.hs to use `MonadPresolution` methods (currently 1 lift call)
- [ ] Update StateAccess.hs to use `MonadPresolution` methods (currently 1 lift call)
- [ ] Total `lift $` calls across all presolution modules reduced to <10
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-005: Introduce NodeMap newtype
**Description:** As a developer, I want a `NodeMap` newtype so that I can't accidentally use a gen-node-keyed map where a type-node-keyed map is expected.

**Acceptance Criteria:**
- [ ] Create `newtype NodeMap a = NodeMap (IntMap.IntMap a)` in `Constraint/Types.hs`
- [ ] Add `Functor`, `Foldable`, `Traversable` instances
- [ ] Add helper functions: `lookupNode`, `insertNode`, `deleteNode`, `fromListNode`
- [ ] Update `cNodes` field in `Constraint` to use `NodeMap TyNode`
- [ ] Update all call sites that access `cNodes`
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-006: Introduce GenNodeMap newtype
**Description:** As a developer, I want a `GenNodeMap` newtype so that gen-node operations are type-safe.

**Acceptance Criteria:**
- [ ] Create `newtype GenNodeMap a = GenNodeMap (IntMap.IntMap a)` in `Constraint/Types.hs`
- [ ] Add `Functor`, `Foldable`, `Traversable` instances
- [ ] Add helper functions: `lookupGen`, `insertGen`, `deleteGen`, `fromListGen`
- [ ] Update `cGenNodes` field in `Constraint` to use `GenNodeMap GenNode`
- [ ] Update all call sites that access `cGenNodes`
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-007: Introduce InteriorNodes and FrontierNodes newtypes
**Description:** As a developer, I want distinct newtypes for interior and frontier node sets so that I can't accidentally pass one where the other is expected.

**Acceptance Criteria:**
- [ ] Create `newtype InteriorNodes = InteriorNodes IntSet.IntSet` in `Constraint/Presolution/Base.hs`
- [ ] Create `newtype FrontierNodes = FrontierNodes IntSet.IntSet` in `Constraint/Presolution/Base.hs`
- [ ] Add helper functions: `memberInterior`, `memberFrontier`, `toListInterior`, `toListFrontier`
- [ ] Update `InteriorSet` and `FrontierSet` type aliases to use newtypes
- [ ] Update `EdgeTrace` fields to use newtypes
- [ ] Update all call sites in presolution modules
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-008: Introduce CopyMapping newtype
**Description:** As a developer, I want a `CopyMapping` newtype so that copy maps (old ID -> new ID) are distinct from other IntMaps.

**Acceptance Criteria:**
- [ ] Create `newtype CopyMapping = CopyMapping (IntMap.IntMap NodeId)` in `Constraint/Presolution/Base.hs`
- [ ] Add helper functions: `lookupCopy`, `insertCopy`, `copiedNodes`, `originalNodes`
- [ ] Update `CopyMap` type alias to use newtype
- [ ] Update `etCopyMap` in `EdgeTrace` to use `CopyMapping`
- [ ] Update all call sites in presolution and elaboration modules
- [ ] Build passes with no warnings
- [ ] Existing tests pass

### US-009: Add error path tests for PresolutionError variants
**Description:** As a developer, I want tests that verify each `PresolutionError` variant is correctly raised so that error handling is reliable.

**Acceptance Criteria:**
- [ ] Add test case for `UnmatchableTypes` error in `PresolutionSpec.hs`
- [ ] Add test case for `UnresolvedExpVar` error
- [ ] Add test case for `ArityMismatch` error
- [ ] Add test case for `InstantiateOnNonForall` error
- [ ] Add test case for `NodeLookupFailed` error
- [ ] Add test case for `OccursCheckPresolution` error
- [ ] Add test case for `BindingTreeError` error
- [ ] Add test case for `InternalError` error
- [ ] All new tests pass
- [ ] Test coverage for presolution error paths >80%

### US-010: Add property-based tests for binding tree invariants
**Description:** As a developer, I want QuickCheck properties that verify binding tree operations preserve invariants so that regressions are caught automatically.

**Acceptance Criteria:**
- [ ] Add `Arbitrary` instance for small binding trees in `BindingSpec.hs`
- [ ] Add property: `canonicalization preserves tree structure`
- [ ] Add property: `validation detects all parent-child mismatches`
- [ ] Add property: `interior computation is idempotent`
- [ ] Add property: `frontier nodes are exactly the boundary of interior`
- [ ] All properties pass with 100 test cases each
- [ ] No test flakiness observed

### US-011: Add property-based tests for witness operations
**Description:** As a developer, I want QuickCheck properties for witness normalization and canonicalization so that witness invariants are verified.

**Acceptance Criteria:**
- [ ] Add `Arbitrary` instance for `InstanceOp` in `PresolutionSpec.hs`
- [ ] Add property: `witness normalization is idempotent`
- [ ] Add property: `canonicalized witnesses have no redundant ops`
- [ ] Add property: `witness validation accepts all canonicalized witnesses`
- [ ] All properties pass with 100 test cases each

### US-012: Refactor Phi.hs to use explicit PhiEnv (Optional)
**Description:** As a developer, I want `Phi.hs` refactored to use an explicit environment record so that the module can be split into smaller, testable units.

**Acceptance Criteria:**
- [ ] Create `PhiEnv` record type with fields: `peResult`, `peCanonical`, `peGeneralizeAt`, `peTrace`, `peGaParents`, `peCopyMap`, `peInvCopyMap`
- [ ] Create `PhiM` monad as `ReaderT PhiEnv (Either ElabError)`
- [ ] Extract helper functions from `phiFromEdgeWitnessWithTrace` closure to module-level functions taking `PhiEnv`
- [ ] Group related helpers into logical sections with clear boundaries
- [ ] Reduce `phiFromEdgeWitnessWithTrace` from ~950 lines to <200 lines of orchestration
- [ ] Build passes with no warnings
- [ ] Existing tests pass
- [ ] Add unit tests for at least 3 extracted helper functions

## Functional Requirements

- FR-1: `MonadPresolution` typeclass must include `getNode`, `getCanonicalNode`, `lookupBindParent`, `modifyPresolution`
- FR-2: `MonadSolve` typeclass must include `getConstraint`, `modifyConstraint`, `getUnionFind`, `throwSolveError`
- FR-3: `NodeMap` newtype must wrap `IntMap.IntMap` and provide type-safe accessors
- FR-4: `GenNodeMap` newtype must wrap `IntMap.IntMap` and provide type-safe accessors
- FR-5: `InteriorNodes` and `FrontierNodes` newtypes must wrap `IntSet.IntSet`
- FR-6: `CopyMapping` newtype must wrap `IntMap.IntMap NodeId`
- FR-7: All newtype wrappers must derive or implement `Eq`, `Show`, `Semigroup`, `Monoid` where applicable
- FR-8: Error path tests must cover all `PresolutionError` constructors
- FR-9: Property-based tests must use QuickCheck with `Arbitrary` instances

## Non-Goals

- No changes to the core MLF algorithm or paper alignment
- No performance optimization (separate effort)
- No changes to public API modules (`MLF.API`, `MLF.Pipeline`)
- No extraction of `BinderPlan.hs` (recently consolidated, intentionally large)
- No changes to `Generalize.hs` module structure (out of scope for this PRD)

## Technical Considerations

### Typeclass Design

The `MonadPresolution` typeclass should follow MTL conventions:
```haskell
class Monad m => MonadPresolution m where
    getConstraint :: m Constraint
    modifyConstraint :: (Constraint -> Constraint) -> m ()
    getCanonical :: m (NodeId -> NodeId)
    getPresolutionState :: m PresolutionState
    putPresolutionState :: PresolutionState -> m ()
    throwPresolutionError :: PresolutionError -> m a
    -- New operations
    getNode :: NodeId -> m (Maybe TyNode)
    getCanonicalNode :: NodeId -> m (Maybe TyNode)
    lookupBindParent :: NodeId -> m (Maybe (NodeRef, BindFlag))
    modifyPresolution :: (Presolution -> Presolution) -> m ()
```

### Newtype Coercion

Use `coerce` from `Data.Coerce` for zero-cost newtype wrapping/unwrapping in performance-critical paths:
```haskell
import Data.Coerce (coerce)

lookupNode :: NodeId -> NodeMap a -> Maybe a
lookupNode (NodeId k) (NodeMap m) = IntMap.lookup k m
-- Or: lookupNode nid = coerce (IntMap.lookup (getNodeId nid))
```

### Backward Compatibility

Introduce newtypes incrementally:
1. Add newtype with smart constructors
2. Update one module at a time
3. Keep type aliases during transition: `type NodeMapLegacy = IntMap.IntMap TyNode`

## Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| `lift $` calls in presolution | 36 | <10 |
| `lift $` calls in Solve.hs | 12 | <4 |
| Total `lift $` calls | 48 | <15 |
| Semantic newtype coverage | 0% | 60% of IntMap/IntSet uses |
| Presolution error path coverage | ~40% | >80% |
| Property-based test count | 0 | 8+ properties |

## Open Questions

1. Should `NodeMap` use `NodeId` as key type or keep `Int` for performance?
   - Recommendation: Use `Int` internally, provide `NodeId`-based API
2. Should newtypes derive `Generic` for automatic instances?
   - Recommendation: Yes, use `DerivingVia` where beneficial
3. Should `PhiEnv` use `Has` typeclasses for field access?
   - Recommendation: No, keep simple record access for clarity
4. Should we add `MonadReader PhiEnv` constraint or explicit parameter passing for Phi.hs?
   - Recommendation: Use `ReaderT` for cleaner composition

## Implementation Order

1. **Phase 1 (Monad Typeclasses):** US-001, US-002, US-003, US-004
2. **Phase 2 (Semantic Newtypes):** US-005, US-006, US-007, US-008
3. **Phase 3 (Test Coverage):** US-009, US-010, US-011
4. **Phase 4 (Optional Phi.hs):** US-012

Each phase should be completed and verified before starting the next.
