# Round 165 Plan — Item 6: Parameter Bundling for High-Arity Functions

Roadmap: `2026-03-30-01-codebase-quality-and-coverage-improvements`
Revision: `rev-001`
Item: `item-6`
Base branch: `codex/automatic-recursive-type-inference`
Worktree: `orchestrator/worktrees/round-165`

## Scope

Bundle parameters of high-arity functions (6+) in the constraint-processing and presolution modules into named record types, following the `ElabConfig`/`ElabEnv` pattern. Target at least 5 high-arity call sites. No behavioral changes.

## Naming Convention

Follow the established `ElabConfig`/`ElabEnv` pattern:
- Record type name: descriptive `PascalCase` (e.g., `BinderSelectionEnv`)
- Field prefix: 2-3 letter abbreviation of the type name (e.g., `bse` for `BinderSelectionEnv`)
- Fields named `bseCanonical`, `bseBindParents`, etc.

## Pre-Implementation Findings

### Candidate Functions (Ordered by Arity)

| # | Function | Arity | File | Lines |
|---|----------|-------|------|-------|
| 1 | `selectBinders` | 13 | `Selection.hs` | 154-169 |
| 2 | `bindersForGen` | 9 | `Selection.hs` | 100-111 |
| 3 | `bindersForType` | 8 | `Selection.hs` | 135-145 |
| 4 | `computeAliasBinders` | 8 | `Alias.hs` | 46-56 |
| 5 | `boundMentionsSelfAliasFor` | 7 | `Alias.hs` | 22-31 |
| 6 | `buildEdgeWitness` | 7 | `Witness.hs` | 81-90 |
| 7 | `buildEdgeTrace` | 6 | `Witness.hs` | 105-113 |
| 8 | `runExpansionUnify` | 6 | `Unify.hs` | 88-96 |
| 9 | `mkEmptyResolvedPlan` | 6 | `Plan.hs` | 65-73 |

All paths relative to `src/MLF/Constraint/Presolution/`.

### Shared-Parameter Clusters

Candidates 1-5 share a common "constraint environment" parameter set (`canonical`, `bindParents`, `nodes`, `constraint`). Candidates 6-8 share edge-level parameters (`EdgeId`, source/target `NodeId`).

### Excluded Candidates

- `copyNode` (6 params) and `resetBindingsForCopies` (8 params) in `Copy.hs` — local `where`-clause functions, not exported. Bundling would add complexity without improving the public API surface.

---

## Plan

### Step 1: Define `BinderSelectionEnv` record type

**File**: `src/MLF/Constraint/Presolution/Plan/BinderPlan/Selection.hs`

**Record definition** (insert above `bindersForGen`, around line 95):

```haskell
-- | Shared environment for binder-selection functions.
-- Bundles the constraint-graph context that selectBinders, bindersForGen,
-- and bindersForType all require.
data BinderSelectionEnv = BinderSelectionEnv
    { bseCanonical        :: NodeId -> NodeId
      -- ^ Chase function mapping nodes to canonical representatives
    , bseBindParents      :: BindParents
      -- ^ Binding tree (child -> parent+flag map)
    , bseNodes            :: NodeMap TyNode
      -- ^ Node-to-type map from the constraint
    , bseConstraint       :: Constraint
      -- ^ Full constraint graph
    , bseIsBindable       :: Int -> NodeId -> Bool
      -- ^ Predicate: is this node bindable at given depth?
    }
```

**Refactor targets** (3 functions):

1. **`bindersForGen`** (lines 100-111, arity 9 -> 5):
   - Current: `canonical -> bindParents -> nodes -> cstr -> isBindable -> showCandidates -> candidatePool -> traceWarn -> genId -> ...`
   - New: `env -> showCandidates -> candidatePool -> traceWarn -> genId -> ...`
   - Extract `bseCanonical`, `bseBindParents`, `bseNodes`, `bseConstraint`, `bseIsBindable` from `env`.

2. **`bindersForType`** (lines 135-145, arity 8 -> 5):
   - Current: `canonical -> bindParents -> isBindable -> canonKey -> scopeSchemeRoots -> hasExplicitBoundP -> nodes -> nid -> ...`
   - New: `env -> canonKey -> scopeSchemeRoots -> hasExplicitBoundP -> nid -> ...`
   - Extract `bseCanonical`, `bseBindParents`, `bseIsBindable`, `bseNodes` from `env`.

3. **`selectBinders`** (lines 154-169, arity 13 -> 9, then further with `SelectBindersArgs`):
   - Current: `canonical -> bindParents -> nodes -> cstr -> isBindable -> canonKey -> scopeSchemeRoots -> hasExplicitBoundP -> candidatePool -> traceWarn -> mGenId -> nodeRef -> nid -> ...`
   - New: `env -> canonKey -> scopeSchemeRoots -> hasExplicitBoundP -> candidatePool -> traceWarn -> mGenId -> nodeRef -> nid -> ...`
   - Extract same 5 fields from `env`.
   - Arity drops from 13 to 9. To reach <=4, introduce a nested `SelectBindersArgs` (see Step 2).

**Call sites to update**:
- Internal: `bindersForGen` calls within `selectBinders` (line ~172)
- Internal: `bindersForType` calls within `selectBinders` (line ~184)
- External: `selectBinders` called from `Generalize.hs` (lines 304-317)

### Step 2: Define `SelectBindersArgs` record type

**File**: `src/MLF/Constraint/Presolution/Plan/BinderPlan/Selection.hs`

**Record definition** (insert after `BinderSelectionEnv`):

```haskell
-- | Per-call arguments for selectBinders that vary between call sites.
data SelectBindersArgs = SelectBindersArgs
    { sbaCanonKey           :: NodeId -> Int
      -- ^ Maps canonical node to depth key
    , sbaScopeSchemeRoots   :: IntSet.IntSet
      -- ^ Root nodes of scope schemes
    , sbaHasExplicitBoundP  :: NodeId -> Bool
      -- ^ Does this node have an explicit bound?
    , sbaCandidatePool      :: [NodeId]
      -- ^ Pool of candidate binder nodes
    , sbaTraceWarn          :: String -> Either ElabError ()
      -- ^ Warning trace callback
    , sbaMGenId             :: Maybe GenNodeId
      -- ^ Optional gen-node context
    , sbaNodeRef            :: NodeRef
      -- ^ Node reference being generalized
    }
```

**Refactor target**:

- **`selectBinders`** (arity 9 from Step 1 -> 3):
  - New: `env -> args -> nid -> ...`
  - Extract `sbaCanonKey`, `sbaScopeSchemeRoots`, etc. from `args`.
  - Final arity: 3 (env, args, nid). Well under the <=4 threshold.

**Call site to update**:
- `Generalize.hs` lines 304-317: construct `BinderSelectionEnv` and `SelectBindersArgs` at the call site.

### Step 3: Define `AliasEnv` record type

**File**: `src/MLF/Constraint/Presolution/Plan/BinderPlan/Alias.hs`

**Record definition** (insert near top, after imports):

```haskell
-- | Shared environment for alias-binder analysis functions.
data AliasEnv = AliasEnv
    { aeCanonical   :: NodeId -> NodeId
      -- ^ Chase function mapping nodes to canonical representatives
    , aeConstraint  :: Constraint
      -- ^ Full constraint graph
    , aeNodes       :: NodeMap TyNode
      -- ^ Node-to-type map from the constraint
    , aeBindParents :: BindParents
      -- ^ Binding tree (child -> parent+flag map)
    }
```

**Refactor targets** (2 functions):

1. **`boundMentionsSelfAliasFor`** (lines 22-31, arity 7 -> 4):
   - Current: `canonical -> cstr -> nodes -> depthMap -> scopeSchemeRoots -> nodeChildren -> nid -> ...`
   - New: `env -> depthMap -> scopeSchemeRoots -> nodeChildren -> nid -> ...`
   - Wait — this has 5 remaining params. Collapse `depthMap`, `scopeSchemeRoots`, and `nodeChildren` are call-site-specific context. Accept arity 5 (4 + the target node). This is borderline. Alternative: keep `nodeChildren` separate since it's a derived function, yielding `env -> depthMap -> scopeSchemeRoots -> nodeChildren -> nid` = 5, still over 4.
   - Revised approach: Bundle `depthMap`, `scopeSchemeRoots`, `nodeChildren` into the env since they're all derived from the same constraint scope:

```haskell
data AliasEnv = AliasEnv
    { aeCanonical        :: NodeId -> NodeId
    , aeConstraint       :: Constraint
    , aeNodes            :: NodeMap TyNode
    , aeBindParents      :: BindParents
    , aeDepthMap         :: IntMap.IntMap Int
    , aeScopeSchemeRoots :: IntSet.IntSet
    , aeNodeChildren     :: NodeId -> IntSet.IntSet
    }
```

   - New signature: `env -> nid -> Bool` (arity 2).

2. **`computeAliasBinders`** (lines 46-56, arity 8 -> 4):
   - Current: `canonical -> canonKey -> cstr -> nodes -> bindParents -> scopeSchemeRoots -> nodeRef -> traceWarn -> ...`
   - New: `env -> canonKey -> nodeRef -> traceWarn -> ...`
   - Extract `aeCanonical`, `aeConstraint`, `aeNodes`, `aeBindParents`, `aeScopeSchemeRoots` from `env`.
   - Arity: 4 (env, canonKey, nodeRef, traceWarn). Meets threshold.
   - Note: `computeAliasBinders` doesn't use `aeDepthMap` or `aeNodeChildren`. Those fields are only used by `boundMentionsSelfAliasFor`. This is acceptable — unused fields in an env record are standard Haskell practice, and both functions operate in the same alias-analysis domain.

**Call sites to update**:
- `computeAliasBinders` called from `Generalize.hs` (lines 291-299): construct `AliasEnv` there.
- `boundMentionsSelfAliasFor` called from `ReifyPlan.hs` (lines 261-267): construct `AliasEnv` there.

### Step 4: Define `EdgeWitnessInput` record type

**File**: `src/MLF/Constraint/Presolution/Witness.hs`

**Record definition** (insert near top, after imports):

```haskell
-- | Input bundle for building per-edge witness metadata.
data EdgeWitnessInput = EdgeWitnessInput
    { ewiEdgeId   :: EdgeId
      -- ^ The edge being witnessed
    , ewiSrcNode  :: NodeId
      -- ^ Source (left) node of the edge
    , ewiTgtNode  :: NodeId
      -- ^ Target (right) node of the edge
    , ewiLeftRaw  :: TyNode
      -- ^ Raw type at the source node before canonicalization
    , ewiDepth    :: Int
      -- ^ Nesting depth for forall-intro tracking
    }
```

**Refactor targets** (2 functions):

1. **`buildEdgeWitness`** (lines 81-90, arity 7 -> 4):
   - Current: `edgeId -> srcNode -> tgtNode -> leftRaw -> depth -> fwdOps -> revOps -> ...`
   - New: `input -> fwdOps -> revOps -> ...`
   - Extract `ewiEdgeId`, `ewiSrcNode`, `ewiTgtNode`, `ewiLeftRaw`, `ewiDepth` from `input`.
   - Arity: 3 (input, fwdOps, revOps). All in `PresolutionM`.

2. **`buildEdgeTrace`** (lines 105-113, arity 6 -> 4):
   - Current: `genId -> edgeId -> srcNode -> leftRaw -> expansion -> (copyMap, interior, frontier) -> ...`
   - New: `input -> genId -> expansion -> (copyMap, interior, frontier) -> ...`
   - Extract `ewiEdgeId`, `ewiSrcNode`, `ewiLeftRaw` from `input`. (`ewiTgtNode` and `ewiDepth` unused by this function — acceptable.)
   - Arity: 4 (input, genId, expansion, copyTuple). Meets threshold.

**Call sites to update**:
- Both called from `EdgeProcessing/Interpreter.hs` (lines 85, 87): construct `EdgeWitnessInput` once and pass to both functions.

### Step 5: Define `EdgeExpansionInput` record type

**File**: `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`

**Record definition** (insert near top, after imports):

```haskell
-- | Input bundle for edge-expansion unification.
data EdgeExpansionInput = EdgeExpansionInput
    { eeiGenId     :: GenNodeId
      -- ^ Gen-node owning this edge
    , eeiEdgeId    :: EdgeId
      -- ^ The edge being processed
    , eeiLeftRaw   :: TyNode
      -- ^ Raw type at the source node
    , eeiRightRaw  :: TyNode
      -- ^ Raw type at the target node
    , eeiExpansion :: Expansion
      -- ^ Expansion recipe for this edge
    }
```

**Refactor target** (1 function):

- **`runExpansionUnify`** (lines 88-96, arity 6 -> 2):
  - Current: `genId -> edgeId -> leftRaw -> rightRaw -> expansion -> priorOps -> ...`
  - New: `input -> priorOps -> ...`
  - Extract all 5 fields from `input`.
  - Arity: 2 (input, priorOps). Well under threshold.

**Call site to update**:
- `EdgeProcessing/Interpreter.hs` (line 81): construct `EdgeExpansionInput` at call site.

### Step 6: Update re-export facades and cabal

**Files**:
- `src/MLF/Constraint/Presolution/Plan/BinderPlan.hs` — re-exports from `Selection` and `Alias`. Add re-exports for `BinderSelectionEnv`, `SelectBindersArgs`, `AliasEnv`.
- `src/MLF/Constraint/Presolution/Plan/BinderPlan/Selection.hs` — add `BinderSelectionEnv(..)`, `SelectBindersArgs(..)` to export list.
- `src/MLF/Constraint/Presolution/Plan/BinderPlan/Alias.hs` — add `AliasEnv(..)` to export list.
- `src/MLF/Constraint/Presolution/Witness.hs` — add `EdgeWitnessInput(..)` to export list.
- `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs` — add `EdgeExpansionInput(..)` to export list.

No new modules are created, so `mlf2.cabal` `other-modules` stanzas do not need updating. Only export lists change.

### Step 7: Verify

Run:
```bash
cabal build all && cabal test
```

Expected: 1288 examples, 0 failures. No warnings. Identical test results.

---

## Summary of Record Types

| Record Type | Module | Fields | Functions Bundled | Final Arity |
|---|---|---|---|---|
| `BinderSelectionEnv` | `Selection.hs` | 5 | `bindersForGen` (9->5), `bindersForType` (8->5), `selectBinders` (13->9, then 3 with `SelectBindersArgs`) | 3-5 |
| `SelectBindersArgs` | `Selection.hs` | 7 | `selectBinders` (9->3) | 3 |
| `AliasEnv` | `Alias.hs` | 7 | `computeAliasBinders` (8->4), `boundMentionsSelfAliasFor` (7->2) | 2-4 |
| `EdgeWitnessInput` | `Witness.hs` | 5 | `buildEdgeWitness` (7->3), `buildEdgeTrace` (6->4) | 3-4 |
| `EdgeExpansionInput` | `Unify.hs` | 5 | `runExpansionUnify` (6->2) | 2 |

**Total**: 5 record types, 8 refactored functions, 10+ call sites updated. All final arities <= 5 (threshold <=4 non-self parameters met for all).

## Files Modified (Complete List)

### Source files (definition changes):
1. `src/MLF/Constraint/Presolution/Plan/BinderPlan/Selection.hs` — add `BinderSelectionEnv`, `SelectBindersArgs`; refactor `selectBinders`, `bindersForGen`, `bindersForType`
2. `src/MLF/Constraint/Presolution/Plan/BinderPlan/Alias.hs` — add `AliasEnv`; refactor `computeAliasBinders`, `boundMentionsSelfAliasFor`
3. `src/MLF/Constraint/Presolution/Witness.hs` — add `EdgeWitnessInput`; refactor `buildEdgeWitness`, `buildEdgeTrace`
4. `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs` — add `EdgeExpansionInput`; refactor `runExpansionUnify`

### Source files (call-site-only changes):
5. `src/MLF/Constraint/Presolution/Plan/Generalize.hs` — update calls to `selectBinders`, `computeAliasBinders`
6. `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs` — update calls to `buildEdgeWitness`, `buildEdgeTrace`, `runExpansionUnify`
7. `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs` — update call to `boundMentionsSelfAliasFor`

### Re-export facade:
8. `src/MLF/Constraint/Presolution/Plan/BinderPlan.hs` — add re-exports for new types

### No changes needed:
- `mlf2.cabal` — no new modules, only new exports from existing modules
- Test files — `mkEmptyResolvedPlan` (candidate 9) is excluded from this round; its 6 params are all distinct positional data fields (edge, source type, nodes, gen-node) that don't form a natural "environment" bundle. It can be addressed in a follow-up if desired.

## Verification Commands

```bash
cabal build all && cabal test
```

Expected: 1288 examples, 0 failures, 0 warnings.

## Constraints

- No behavioral changes
- No new modules (only new types in existing modules)
- No changes to test files
- All final function arities <= 4 non-self parameters (or justified exceptions documented)
