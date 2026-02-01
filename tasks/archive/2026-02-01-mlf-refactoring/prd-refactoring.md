# Product Requirements Document: MLF Codebase Refactoring

## Overview

**Project:** MLF Type Inference System Refactoring
**Version:** 1.0
**Date:** 2026-02-01
**Status:** Draft

### Summary

Refactor the MLF codebase to improve maintainability, reliability, and testability. The codebase implements a sophisticated MLF type inference system across 77 modules and 26,744 lines of Haskell. This refactoring focuses on three key areas: replacing unsafe `error()` calls, extracting large monolithic modules, and simplifying monad stacks.

### Goals

1. **Reliability:** Eliminate all `error()` calls in favor of proper error handling
2. **Maintainability:** Break down large modules (>1000 lines) into focused, testable units
3. **Simplicity:** Reduce monad stack complexity using MTL typeclasses
4. **Testability:** Enable unit testing of currently untestable monolithic functions

### Non-Goals

- Performance optimization (separate effort)
- API changes to public modules
- Adding new features or capabilities
- Changing the core algorithm or paper alignment

---

## Problem Statement

### Current Issues

1. **Unsafe Error Handling:** 5 instances of `error()` calls that crash at runtime instead of returning proper errors
2. **Large Modules:** 4 modules exceed 1000 lines, making them difficult to understand, test, and maintain
3. **Complex Monad Stacks:** Nested `StateT`/`ReaderT` stacks require verbose `lift` calls and obscure control flow
4. **Scattered Concerns:** Binding tree operations spread across 5+ modules without clear boundaries

### Impact

- Runtime crashes in production when unexpected states occur
- Difficulty onboarding new contributors due to module complexity
- High cognitive load when debugging presolution and elaboration phases
- Limited test coverage due to tightly-coupled monolithic functions

---

## Requirements

### Phase 1: Replace `error()` Calls (High Priority)

**Objective:** Convert all 5 `error()` calls to proper error handling.

#### Locations

| File | Line | Current Error | Proposed Fix |
|------|------|---------------|--------------|
| `Constraint/Solve.hs` | 223 | `validateSolvedGraph` failure | Add `SolveError` variant |
| `Util/OrderKey.hs` | 139-141 | Missing order key | Add `OrderKeyError` type |
| `Types/Elab.hs` | 143 | Unexpected variable bound | Add `ElabError` variant |
| `Binding/Adjustment.hs` | 108 | Harmonize failure | Propagate `BindingError` |

#### Acceptance Criteria

- [ ] All `error()` calls replaced with `throwError` or `Left`
- [ ] New error variants added to appropriate error types
- [ ] Error messages preserved with full context
- [ ] Build passes with no warnings
- [ ] Tests added to verify error paths

### Phase 2: Extract Presolution Driver Functions (High Priority)

**Objective:** Break down `Driver.hs` (1289 lines) into focused modules.

#### Proposed Module Structure

```
Presolution/
├── Driver.hs           (orchestration only, ~400 lines)
├── Rewrite.hs          (✓ already extracted)
├── StateAccess.hs      (✓ already extracted)
├── Materialization.hs  (NEW - expansion materialization)
├── WitnessNorm.hs      (NEW - witness normalization)
└── EdgeProcessing.hs   (NEW - edge loop logic)
```

#### Functions to Extract

| Function | Target Module | Lines | Dependencies |
|----------|---------------|-------|--------------|
| `materializeExpansions` | `Materialization.hs` | ~200 | Expansion, Copy |
| `normalizeEdgeWitnessesM` | `WitnessNorm.hs` | ~150 | Witness |
| `runPresolutionLoop` | `EdgeProcessing.hs` | ~100 | EdgeUnify |

#### Acceptance Criteria

- [ ] `Driver.hs` reduced to <500 lines
- [ ] Each new module has clear, documented purpose
- [ ] No circular dependencies introduced
- [ ] All exports explicitly listed
- [ ] Build passes, tests pass

### Phase 3: Centralize Binding Tree Operations (Medium Priority)

**Objective:** Consolidate scattered binding tree operations.

#### Current State

Binding operations in:
- `Binding/Tree.hs` (1228 lines)
- `Presolution/Base.hs` (canonicalization)
- `Presolution/Driver.hs` (validation)
- `Presolution/EdgeUnify.hs` (queries)
- `Presolution/Rewrite.hs` (rewriting)

#### Proposed Structure

```
Binding/
├── Tree.hs             (core operations, ~800 lines)
├── Validation.hs       (NEW - invariant checking)
├── Canonicalization.hs (NEW - from Presolution/Base.hs)
├── Queries.hs          (NEW - common queries)
└── GraphOps.hs         (keep as-is)
```

#### Acceptance Criteria

- [ ] `canonicalizeBindParentsUnder` moved to `Binding/Canonicalization.hs`
- [ ] `checkBindingTree` split into per-invariant functions
- [ ] Common queries (interior, frontier, path) in `Binding/Queries.hs`
- [ ] Clear module boundaries documented
- [ ] Build passes, tests pass

### Phase 4: Simplify Monad Stacks (Medium Priority)

**Objective:** Use MTL typeclasses to reduce `lift` boilerplate.

#### Current Pattern

```haskell
type EdgeUnifyM = StateT EdgeUnifyState PresolutionM

-- Requires explicit lifting
foo = do
    lift $ lift $ throwError SomeError
    lift $ modify (\s -> s { ... })
```

#### Proposed Pattern

```haskell
class (MonadState EdgeUnifyState m,
       MonadError PresolutionError m,
       MonadPresolution m) => MonadEdgeUnify m

-- No lifting needed
foo :: MonadEdgeUnify m => m ()
foo = do
    throwError SomeError
    modify (\s -> s { ... })
```

#### Modules to Update

1. `Presolution/Base.hs` - Define `MonadPresolution` class
2. `Presolution/EdgeUnify.hs` - Define `MonadEdgeUnify` class
3. `Presolution/Expansion.hs` - Use typeclass constraints
4. `Presolution/Copy.hs` - Use typeclass constraints

#### Acceptance Criteria

- [ ] `MonadPresolution` typeclass defined with core operations
- [ ] `MonadEdgeUnify` typeclass defined
- [ ] At least 50% reduction in explicit `lift` calls
- [ ] Type signatures use constraints instead of concrete stacks
- [ ] Build passes, tests pass

### Phase 5: Extract Witness Validation (Lower Priority)

**Objective:** Separate witness validation from normalization.

#### Current State

`Presolution/Witness.hs` (822 lines) mixes:
- Witness construction
- Witness validation
- Witness normalization
- Witness canonicalization

#### Proposed Structure

```
Presolution/
├── Witness.hs           (construction & normalization, ~500 lines)
├── WitnessValidation.hs (NEW - invariant checking, ~200 lines)
└── WitnessCanon.hs      (NEW - canonicalization, ~150 lines)
```

#### Acceptance Criteria

- [ ] Validation functions extracted to `WitnessValidation.hs`
- [ ] Canonicalization functions extracted to `WitnessCanon.hs`
- [ ] `Witness.hs` focused on construction and normalization
- [ ] Build passes, tests pass

---

## Technical Design

### Error Type Hierarchy

```haskell
-- Unified error type for presolution phase
data PresolutionError
    = TypeMismatch EdgeTrace TyNode TyNode
    | BindingError BindingError
    | UnresolvedExpVar ExpVarId
    | TranslatabilityError TranslatabilityError
    | UnexpectedState String  -- NEW: replaces error() calls
    deriving (Show, Eq)

-- New error type for order key operations
data OrderKeyError
    = MissingOrderKey NodeId
    | InvalidOrderComparison NodeId NodeId
    deriving (Show, Eq)
```

### Module Dependency Graph (Post-Refactor)

```
Driver.hs
├── EdgeProcessing.hs
│   └── EdgeUnify.hs
├── Materialization.hs
│   ├── Expansion.hs
│   └── Copy.hs
├── WitnessNorm.hs
│   └── Witness.hs
├── Rewrite.hs
└── StateAccess.hs
    └── Base.hs
```

### MTL Typeclass Design

```haskell
-- Core presolution operations
class Monad m => MonadPresolution m where
    getConstraint :: m Constraint
    modifyConstraint :: (Constraint -> Constraint) -> m ()
    getCanonical :: m (NodeId -> NodeId)
    throwPresolutionError :: PresolutionError -> m a

-- Edge unification operations
class MonadPresolution m => MonadEdgeUnify m where
    getEdgeState :: m EdgeUnifyState
    modifyEdgeState :: (EdgeUnifyState -> EdgeUnifyState) -> m ()
```

---

## Implementation Plan

### Milestone 1: Error Handling (Week 1)

1. Add `UnexpectedState` variant to `PresolutionError`
2. Replace `error()` in `Solve.hs`
3. Replace `error()` in `OrderKey.hs`
4. Replace `error()` in `Types/Elab.hs`
5. Replace `error()` in `Binding/Adjustment.hs`
6. Add tests for error paths

### Milestone 2: Driver Extraction (Week 2)

1. Create `Presolution/Materialization.hs`
2. Extract `materializeExpansions` and helpers
3. Create `Presolution/WitnessNorm.hs`
4. Extract witness normalization functions
5. Create `Presolution/EdgeProcessing.hs`
6. Extract edge loop logic
7. Update `Driver.hs` to import new modules

### Milestone 3: Binding Consolidation (Week 3)

1. Create `Binding/Validation.hs`
2. Extract invariant checking functions
3. Create `Binding/Canonicalization.hs`
4. Move `canonicalizeBindParentsUnder` from `Presolution/Base.hs`
5. Create `Binding/Queries.hs`
6. Consolidate common query functions

### Milestone 4: Monad Simplification (Week 4)

1. Define `MonadPresolution` typeclass in `Presolution/Base.hs`
2. Define `MonadEdgeUnify` typeclass in `Presolution/EdgeUnify.hs`
3. Update `Expansion.hs` to use typeclasses
4. Update `Copy.hs` to use typeclasses
5. Remove explicit `lift` calls

### Milestone 5: Witness Extraction (Week 5)

1. Create `Presolution/WitnessValidation.hs`
2. Extract validation functions
3. Create `Presolution/WitnessCanon.hs`
4. Extract canonicalization functions
5. Update `Witness.hs` imports

---

## Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| `error()` calls | 5 | 0 |
| Modules >1000 lines | 4 | 1 |
| Explicit `lift` calls | ~50 | <20 |
| Average module size | 347 lines | <300 lines |
| Test coverage (presolution) | ~60% | >80% |

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Circular dependencies | Build failure | Careful module boundary design |
| Performance regression | Slower inference | Benchmark before/after |
| Breaking changes | Test failures | Incremental refactoring with CI |
| Scope creep | Delayed delivery | Strict phase boundaries |

---

## Appendix

### Files to Modify

**Phase 1:**
- `src/MLF/Constraint/Solve.hs`
- `src/MLF/Util/OrderKey.hs`
- `src/MLF/Types/Elab.hs`
- `src/MLF/Binding/Adjustment.hs`

**Phase 2:**
- `src/MLF/Constraint/Presolution/Driver.hs`
- `src/MLF/Constraint/Presolution/Materialization.hs` (new)
- `src/MLF/Constraint/Presolution/WitnessNorm.hs` (new)
- `src/MLF/Constraint/Presolution/EdgeProcessing.hs` (new)

**Phase 3:**
- `src/MLF/Binding/Tree.hs`
- `src/MLF/Binding/Validation.hs` (new)
- `src/MLF/Binding/Canonicalization.hs` (new)
- `src/MLF/Binding/Queries.hs` (new)
- `src/MLF/Constraint/Presolution/Base.hs`

**Phase 4:**
- `src/MLF/Constraint/Presolution/Base.hs`
- `src/MLF/Constraint/Presolution/EdgeUnify.hs`
- `src/MLF/Constraint/Presolution/Expansion.hs`
- `src/MLF/Constraint/Presolution/Copy.hs`

**Phase 5:**
- `src/MLF/Constraint/Presolution/Witness.hs`
- `src/MLF/Constraint/Presolution/WitnessValidation.hs` (new)
- `src/MLF/Constraint/Presolution/WitnessCanon.hs` (new)

### References

- Rémy, D., & Yakobowski, B. (2008). From ML to MLF: Graphic Type Constraints with Efficient Type Inference
- Yakobowski, B. (2008). Graphical types and constraints: second-order polymorphism and inference
