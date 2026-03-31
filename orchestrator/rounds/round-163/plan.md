# Round 163 – Plan: Decompose large modules (>800 lines)

**Roadmap item**: item-4 — Split the top 5 non-research modules by line count  
**Constraint**: Each split parent module becomes a re-export facade (<200 lines). No behavioral changes.

---

## Key observations from source analysis

1. **MLF.Elab.Phi.Omega.Interpret** (1226 lines): Nearly all logic lives inside one giant `where` clause of `phiWithSchemeOmega`. Functions reference closure variables (e.g., `canonicalNode`, `lookupNodePV`, `orderKeys`, `vs`, `constraint`, `debugPhi`, etc.). Extracting helpers into separate modules requires passing a shared environment record. The two groups that can be cleanly separated are (a) the `go` interpretation loop with its op-handlers and (b) the utility/reify helpers, but both need the same closure. **Strategy**: Create an internal `Env` record capturing the shared closure, then split into `Interpret.Env` (env type + construction), `Interpret.Helpers` (utility functions: `reifyTypeArg`, `substSchemeNames`, `containsBottomTy`, `reifyBoundType`, `inferredOmegaInst`, `applyInferredArgs`, etc.), and `Interpret.OmegaLoop` (the `go` loop, `continueRaise`, op-handlers). The parent re-exports `phiWithSchemeOmega` only.

2. **MLF.Constraint.Normalize** (848 lines): Clean separation into logical groups: (a) main entry + loop + reflexive-edge drops (~50 lines), (b) grafting logic (~350 lines), (c) merging/union-find logic (~250 lines), (d) state helpers (~100 lines). State helpers are used by both grafting and merging. **Strategy**: Split into `Normalize.Graft` and `Normalize.Merge`, with shared `NormalizeM` state helpers remaining in the parent or a `Normalize.Internal` module.

3. **MLF.Reify.Type** (822 lines): The massive `reifyWith` function and its `where` clause contain all the logic (~660 lines). The public entry points (`reifyType`, `reifyTypeWithNames`, etc.) and `solvedFromView`/`freeVars` are thin wrappers (~160 lines). **Strategy**: Move the core `reifyWith`/`reifyWithAs` logic into `Type.Core` and keep the public wrappers + `solvedFromView`/`freeVars` in the facade. The `where` clause stays with `reifyWith` in `Core`.

4. **MLF.Elab.Run.ResultType.Fallback** (822 lines): Two clear blocks: (a) entry points + annotated-lambda handler (~175 lines: `computeResultTypeFallback`, `computeResultTypeFallbackWithView`, `computeBodyResultType`), (b) `computeResultTypeFallbackCore` (~645 lines). **Strategy**: Move `computeResultTypeFallbackCore` into `Fallback.Core`.

5. **MLF.Constraint.Presolution.Plan** (821 lines): Already has rich sub-module structure. The main module has: (a) types (`PresolutionEnv`, `GeneralizePlan`, `ReifyPlan`) ~40 lines, (b) `planGeneralizeAt` ~510 lines, (c) `planReify` ~110 lines, (d) `buildGeneralizePlans` ~20 lines, (e) `mkGeneralizeEnv`/`softenBindParents` ~55 lines. **Strategy**: Move `planGeneralizeAt` into `Plan.Generalize` and `planReify` into `Plan.Reify` (note: `Plan.ReifyPlan` already exists as a different module, so use `Plan.Reify` or `Plan.ReifyStep`). Move env setup into `Plan.Env`.

---

## Step 1: Split `MLF.Constraint.Normalize` (848 → facade ~130 lines)

### 1a. Create `src/MLF/Constraint/Normalize/Graft.hs`

**New module**: `MLF.Constraint.Normalize.Graft`

**Exports**: `graftInstEdges`

**Move these definitions** from `Normalize.hs`:
- `graftInstEdges` (lines 219–248) — the `NormalizeM ()` action + its local `partitionResults`
- `partitionGraftable` (lines 252–289)
- `graftEdge` (lines 355–502) — all cases
- `occursIn` (lines 505–509)
- The Note blocks: `[Grafting]` (lines 163–216), `[Grafting Cases]` (lines 291–351)

**Imports needed in Graft**: Same as the subset currently used by these functions — `MLF.Constraint.Normalize.Internal` for `NormalizeM`, `freshVar`, `insertNode`, `setBindParentNorm`, `setBindParentRefNorm`, `findRoot` and state accessors.

### 1b. Create `src/MLF/Constraint/Normalize/Merge.hs`

**New module**: `MLF.Constraint.Normalize.Merge`

**Exports**: `mergeUnifyEdges`, `applyUnionFindToConstraint`

**Move these definitions** from `Normalize.hs`:
- `mergeUnifyEdges` (lines 616–626)
- `processUnifyEdges` (lines 629–636)
- `normalizeFindRoot` (lines 638–641)
- `normalizeLookupNode` (lines 643–646)
- `normalizeUnifyStrategy` (lines 648–655)
- `normalizeForallArity` (lines 657–664)
- `normalizeRepresentative` (lines 679–701) — with its Note block (lines 666–678)
- `unionNodes` (lines 705–707)
- `findRoot` (lines 710–711) — **NOTE**: `findRoot` is also used in `Graft`; move to `Internal`.
- `applyUnionFindToConstraint` (lines 722–773)
- `applyToNode` (lines 778–797)
- `applyToStructure` (lines 800–814)
- `enforcePaperShapedInstEdges` (lines 819–823)
- `wrapInstEdgeLeft` (lines 826–838)
- `inheritWrapperBindParent` (lines 842–848)
- The Note block: `[Merging]` (lines 551–613)

### 1c. Create `src/MLF/Constraint/Normalize/Internal.hs`

**New module**: `MLF.Constraint.Normalize.Internal`

**Exports**: `NormalizeState(..)`, `NormalizeM`, `freshVar`, `freshNodeId`, `freshSynthExpVarNorm`, `insertNode`, `setBindParentNorm`, `setBindParentRefNorm`, `findRoot`, `unionNodes`

**Move these definitions** from `Normalize.hs`:
- `NormalizeState` (lines 108–119)
- `NormalizeM` type alias (line 121)
- `freshVar` (lines 512–518)
- `freshNodeId` (lines 538–542)
- `freshSynthExpVarNorm` (lines 714–719)
- `insertNode` (lines 545–549)
- `setBindParentNorm` (lines 521–527)
- `setBindParentRefNorm` (lines 529–535)
- `findRoot` (lines 710–711)
- `unionNodes` (lines 705–707)

### 1d. Rewrite `src/MLF/Constraint/Normalize.hs` as facade

**Resulting facade** (~80 lines including header/notes):
```haskell
module MLF.Constraint.Normalize (
    normalize,
    NormalizeState (..),
    dropReflexiveInstEdges,
    dropReflexiveUnifyEdges,
    graftInstEdges,
    mergeUnifyEdges
) where

import Control.Monad (when)
import Control.Monad.State.Strict (execState, gets)

import MLF.Constraint.Normalize.Internal
import MLF.Constraint.Normalize.Graft (graftInstEdges)
import MLF.Constraint.Normalize.Merge (mergeUnifyEdges, applyUnionFindToConstraint, enforcePaperShapedInstEdges)
import MLF.Constraint.Types.Graph (Constraint(..), maxNodeIdKeyOr0)
import MLF.Constraint.Types.SynthesizedExpVar (initSynthExpVarSupply)
import qualified Data.IntMap.Strict as IntMap

normalize :: Constraint -> Constraint
-- (same implementation as current, using imports from Internal/Graft/Merge)

normalizeLoop :: NormalizeM ()
-- (same 12-line implementation)

dropReflexiveInstEdges :: Constraint -> Constraint
-- (same 3-line implementation)

dropReflexiveUnifyEdges :: Constraint -> Constraint
-- (same 3-line implementation)
```

### 1e. Update `mlf2.cabal`

In the `other-modules` stanza of `library mlf2-internal`, add:
```
MLF.Constraint.Normalize.Internal,
MLF.Constraint.Normalize.Graft,
MLF.Constraint.Normalize.Merge,
```

**Note**: `MLF.Constraint.Normalize` stays in `exposed-modules` since it's already there.

### 1f. Create directory

```
mkdir -p src/MLF/Constraint/Normalize/
```

### Verification

```bash
cabal build all 2>&1 | head -50
```

---

## Step 2: Split `MLF.Reify.Type` (822 → facade ~170 lines)

### 2a. Create `src/MLF/Reify/Type/Core.hs`

**New module**: `MLF.Reify.Type.Core`

**Exports**: `reifyWith`, `reifyWithAs`, `ReifyRoot(..)`

**Move these definitions** from `Type.hs`:
- `ReifyRoot` data type (lines 40–44)
- `reifyWith` (lines 45–665) — the entire function including its massive `where` clause (`goType`, `goTypeNoFallback`, `goBoundRoot`, `goFull`, `goBound`, `boundIsPoly`, `vChild`, `wrapBinders`, `orderedFlexChildren`, `directFlexChildren`, `isForall`, local `foldrM`)
- `reifyWithAs` (lines 666–676)

**Imports**: Same as current `Type.hs` imports (these are all used by `reifyWith`).

### 2b. Rewrite `src/MLF/Reify/Type.hs` as facade

**Resulting facade** (~160 lines including the public API wrappers):
```haskell
module MLF.Reify.Type (
    reifyType, reifyTypeWithNames, reifyTypeWithNamesNoFallback,
    reifyTypeWithNamesNoFallbackOnConstraint,
    reifyTypeWithNamedSet, reifyTypeWithNamedSetNoFallback,
    reifyWith, reifyWithAs, ReifyRoot(..),
    solvedFromView, freeVars
) where

import MLF.Reify.Type.Core (reifyWith, reifyWithAs, ReifyRoot(..))
-- ... remaining imports for the wrapper functions

-- All reifyType*, reifyTypeWithNames*, etc. wrapper functions (lines 678–778)
-- solvedFromView (lines 780–786)
-- freeVars (lines 788–822)
```

The wrapper functions (`reifyType`, `reifyTypeWithNames`, `reifyTypeWithNamesNoFallback`, `reifyTypeWithNamesNoFallbackOnConstraint`, `reifyTypeWithNamedSet`, `reifyTypeWithNamedSetNoFallback`, `reifyTypeWithNamedSetSolved`, `reifyTypeWithNamedSetNoFallbackSolved`, `reifyTypeWithNamesNoFallbackSolved`) plus `solvedFromView` and `freeVars` stay in the facade.

### 2c. Update `mlf2.cabal`

In the `other-modules` stanza, add:
```
MLF.Reify.Type.Core,
```

### 2d. Create directory

```
mkdir -p src/MLF/Reify/Type/
```

### Verification

```bash
cabal build all 2>&1 | head -50
```

---

## Step 3: Split `MLF.Elab.Run.ResultType.Fallback` (822 → facade ~95 lines)

### 3a. Create `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`

**New module**: `MLF.Elab.Run.ResultType.Fallback.Core`

**Exports**: `computeResultTypeFallbackCore`

**Move these definitions** from `Fallback.hs`:
- `computeResultTypeFallbackCore` (lines 176–822) — the entire function with its `where` clause and all helper logic (`resolveBaseBoundCanonical`, `collectInstApps`, `baseNodeForTy`, `instAppBasesFromWitness`, `argBounds`, `instArgNode`, `boundHasForallFrom`, `boundVarTarget`, etc.)

**Imports**: Same subset of imports as Fallback.hs uses for this function. Key imports: `MLF.Elab.Run.ResultType.Types`, `MLF.Elab.Run.ResultType.View`, `MLF.Elab.Run.ResultType.Util`, `MLF.Elab.Types`, `MLF.Constraint.Types.Graph`, `MLF.Constraint.Presolution`, `MLF.Elab.Run.Scope`, `MLF.Elab.Run.Debug`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.Generalize.Common`, `MLF.Elab.Phi`, `MLF.Elab.Run.ChiQuery`.

### 3b. Rewrite `src/MLF/Elab/Run/ResultType/Fallback.hs` as facade

**Resulting facade** (~90 lines):
```haskell
module MLF.Elab.Run.ResultType.Fallback (
    computeResultTypeFallback,
    computeResultTypeFallbackWithView,
) where

-- imports for the entry points + annotated-lambda handler
import MLF.Elab.Run.ResultType.Fallback.Core (computeResultTypeFallbackCore)

type ResultTypeRecursor = ...  -- (same, lines 64–69)

computeResultTypeFallback :: ...       -- (lines 74–82)
computeResultTypeFallbackWithView :: ...  -- (lines 84–159)
computeBodyResultType :: ...           -- (lines 162–173)
```

Functions remaining in facade:
- `ResultTypeRecursor` type alias (lines 64–69)
- `computeResultTypeFallback` (lines 74–82)
- `computeResultTypeFallbackWithView` (lines 84–159)
- `computeBodyResultType` (lines 162–173)

### 3c. Update `mlf2.cabal`

In the `other-modules` stanza, add:
```
MLF.Elab.Run.ResultType.Fallback.Core,
```

### 3d. Create directory

```
mkdir -p src/MLF/Elab/Run/ResultType/Fallback/
```

### Verification

```bash
cabal build all 2>&1 | head -50
```

---

## Step 4: Split `MLF.Constraint.Presolution.Plan` (821 → facade ~100 lines)

### 4a. Create `src/MLF/Constraint/Presolution/Plan/Env.hs`

**New module**: `MLF.Constraint.Presolution.Plan.Env`

**Exports**: `PresolutionEnv(..)`, `mkGeneralizeEnv`, `softenBindParents`

**Move these definitions** from `Plan.hs`:
- `PresolutionEnv` data type (lines 87–96)
- `lookupNodeInMap` (lines 84–85) — local helper, move here
- `mkGeneralizeEnv` (lines 768–810)
- `softenBindParents` (lines 812–821)

### 4b. Create `src/MLF/Constraint/Presolution/Plan/Generalize.hs`

**New module**: `MLF.Constraint.Presolution.Plan.Generalize`

**Exports**: `GeneralizePlan(..)`, `planGeneralizeAt`

**Move these definitions** from `Plan.hs`:
- `GeneralizePlan` data type (lines 98–112)
- `planGeneralizeAt` (lines 120–631) — the entire function

**Imports needed**: `Plan.Env` for `PresolutionEnv` + `lookupNodeInMap`, plus all the existing imports from `Plan.hs` that `planGeneralizeAt` uses (BinderPlan, Context, Target, SchemeRoots, etc.).

### 4c. Create `src/MLF/Constraint/Presolution/Plan/ReifyStep.hs`

**New module**: `MLF.Constraint.Presolution.Plan.ReifyStep`

**Exports**: `ReifyPlan(..)`, `planReify`

**Move these definitions** from `Plan.hs`:
- `ReifyPlan` data type (lines 114–118)
- `planReify` (lines 633–741)

**Note**: Named `ReifyStep` to avoid confusion with existing `MLF.Constraint.Presolution.Plan.ReifyPlan` module.

### 4d. Rewrite `src/MLF/Constraint/Presolution/Plan.hs` as facade

**Resulting facade** (~65 lines):
```haskell
module MLF.Constraint.Presolution.Plan (
    GeneralizePlan(..),
    ReifyPlan(..),
    buildGeneralizePlans
) where

import MLF.Constraint.Presolution.Plan.Env (PresolutionEnv(..), mkGeneralizeEnv)
import MLF.Constraint.Presolution.Plan.Generalize (GeneralizePlan(..), planGeneralizeAt)
import MLF.Constraint.Presolution.Plan.ReifyStep (ReifyPlan(..), planReify)
-- ... minimal other imports for buildGeneralizePlans

buildGeneralizePlans :: ...  -- (lines 743–766, ~24 lines)
```

### 4e. Update `mlf2.cabal`

In the `other-modules` stanza, add:
```
MLF.Constraint.Presolution.Plan.Env,
MLF.Constraint.Presolution.Plan.Generalize,
MLF.Constraint.Presolution.Plan.ReifyStep,
```

### Verification

```bash
cabal build all 2>&1 | head -50
```

---

## Step 5: Split `MLF.Elab.Phi.Omega.Interpret` (1226 → facade ~90 lines)

This is the hardest split because nearly all logic is in one `where` clause.

### Strategy

Rather than threading a massive `Env` record through extracted functions (which would require significant refactoring of closure-captured variables), the pragmatic approach is a two-way split:

1. **`Interpret.Helpers`** — Pure utility functions that do NOT depend on the `phiWithSchemeOmega` closure:
   - `containsBottomTy` (lines 459–467) — pure recursive function, no closure deps
   - `substSchemeNames` (lines 440–457) — uses only `substForTypes` from closure; refactor to take `IntMap.IntMap String` parameter
   
   However, most "helper" functions (like `reifyTypeArg`, `inferredOmegaInst`, `applyInferredArgs`, etc.) deeply depend on closure variables, making extraction require a large environment parameter.

2. **Better approach**: Split the `go` loop + `continueRaise` (lines 671–1161, ~490 lines) and the remaining non-go helpers into two modules, both parameterized by a shared `InterpretEnv` record.

### 5a. Create `src/MLF/Elab/Phi/Omega/Interpret/Env.hs`

**New module**: `MLF.Elab.Phi.Omega.Interpret.Env`

**Exports**: `InterpretEnv(..)`, `mkInterpretEnv`

**Define `InterpretEnv`** capturing all the shared closure variables:
```haskell
data InterpretEnv = InterpretEnv
    { ieCanonicalNode :: NodeId -> NodeId
    , ieLookupNodePV :: NodeId -> Maybe TyNode
    , ieLookupVarBound :: NodeId -> Maybe NodeId
    , ieLookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    , ieBindParents :: BindParents
    , ieConstraint :: Constraint
    , ieCopyMap :: IntMap.IntMap NodeId
    , ieMTrace :: Maybe EdgeTrace
    , ieMSchemeInfo :: Maybe SchemeInfo
    , ieTraceBinderSources :: IntSet.IntSet
    , ieTraceBinderReplayMap :: IntMap.IntMap NodeId
    , ieEdgeRoot :: NodeId
    , ieEdgeLeft :: NodeId
    , ieEdgeRight :: NodeId
    , ieDomainEnv :: DomainEnv  -- from mkOmegaDomainEnv
    , ieInteriorSet :: IntSet.IntSet
    , ieOrderRoot :: NodeId
    , ieOrderKeys :: IntMap.IntMap Order.OrderKey
    , ieSchemeBinderKeys :: IntSet.IntSet
    , ieSubstForTypes :: IntMap.IntMap String
    , ieDebugPhi :: forall a. String -> a -> a
    , ieNodes :: NodeMap TyNode
    , ieSchemeRootGenMap :: IntMap.IntMap GenNodeId
    , ieOrderKeysForBinders :: [NodeId] -> IntMap.IntMap Order.OrderKey
    , ieReifyBoundWithNamesAt :: IntMap.IntMap String -> NodeId -> Either ElabError ElabType
    , ieReifyTypeWithNamedSetNoFallbackAt :: IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
    , ieNamedSet :: IntSet.IntSet
    , ieSi :: SchemeInfo
    }
```

**Move** `mkInterpretEnv` — extract from the top of `phiWithSchemeOmega`'s where clause (lines 69–280) to build this env from `OmegaContext`, `IntSet.IntSet`, `SchemeInfo`.

### 5b. Create `src/MLF/Elab/Phi/Omega/Interpret/Helpers.hs`

**New module**: `MLF.Elab.Phi.Omega.Interpret.Helpers`

**Exports**: `reifyTypeArg`, `substSchemeNames`, `containsBottomTy`, `reifyBoundType`, `reifyTargetTypeForInst`, `inlineBaseBounds`, `inlineAliasBounds`, `inlineAliasBoundsAsBound`, `inferInstAppArgs`, `inferredOmegaInst`, `inferredArgMap`, `applyInferredArgs`, `applyInferredArgsWith`, `traceArgMap`, `inferredArgMapFromTarget`, `ApplyFun(..)`

**Move these definitions** from the `where` clause, re-parameterized on `InterpretEnv`:
- `ApplyFun` newtype (lines 58–59)
- `reifyTypeArg` (lines 390–438)
- `substSchemeNames` (lines 440–457)
- `containsBottomTy` (lines 459–467)
- `reifyBoundType` (lines 469–470)
- `reifyTargetTypeForInst` (lines 472–478)
- `inlineBaseBounds` (lines 480–484)
- `inlineAliasBounds` (lines 486–487)
- `inlineAliasBoundsAsBound` (lines 489–490)
- `inlineAliasBoundsWith` (lines 494–501)
- `inferInstAppArgs` (lines 503–506)
- `inferredOmegaInst` (lines 548–576)
- `traceArgMap` (lines 292–319)
- `inferredArgMapFromTarget` (lines 321–338)
- `preferInferredArg` (lines 340–345)
- `inferredArgMap` (lines 347–349)
- `applyInferredArgs` (lines 351–352)
- `applyInferredArgsWith` (lines 354–383)

Each function takes `InterpretEnv` as its first argument instead of capturing closure variables.

### 5c. Create `src/MLF/Elab/Phi/Omega/Interpret/OmegaLoop.hs`

**New module**: `MLF.Elab.Phi.Omega.Interpret.OmegaLoop`

**Exports**: `interpretOmegaOps`

**Move these definitions** (re-parameterized on `InterpretEnv`):
- `go` (lines 671–909) — the main omega interpretation loop
- `continueRaise` (lines 911–1161) — the continuation for OpRaise
- `idsForStartType` (lines 1163–1175)
- `parseBinderId` (lines 1177–1179)
- `binderNameFor` (lines 1181–1189)
- `atBinderWith` (lines 1191–1198)
- `isRigidNode` (lines 1202–1206)
- `binderIndex` (lines 1208–1215)
- `prefixBinderNames` (lines 1217–1223)
- `underContext` (line 1226)

Plus helpers used by `go`: `nodeExists` (lines 665–669).

### 5d. Rewrite `src/MLF/Elab/Phi/Omega/Interpret.hs` as facade

**Resulting facade** (~90 lines):
```haskell
module MLF.Elab.Phi.Omega.Interpret (
    phiWithSchemeOmega
) where

import MLF.Elab.Phi.Omega.Interpret.Env (InterpretEnv(..), mkInterpretEnv)
import MLF.Elab.Phi.Omega.Interpret.Helpers (inferredOmegaInst, applyInferredArgs)
import MLF.Elab.Phi.Omega.Interpret.OmegaLoop (interpretOmegaOps)
-- ... minimal other imports

phiWithSchemeOmega :: OmegaContext -> IntSet.IntSet -> SchemeInfo -> Int -> [InstanceOp] -> Either ElabError Instantiation
phiWithSchemeOmega ctx namedSet si introCount omegaOps =
    let env = mkInterpretEnv ctx namedSet si
    in phiWithScheme env introCount omegaOps
  where
    phiWithScheme env introCount' omegaOps' = do
        -- reorderBindersByPrec + applyIntros + interpretOmegaOps (~30 lines)
        ...
```

The `phiWithScheme`, `reorderBindersByPrec`, `desiredBinderOrder`, `reorderTo`, and `applyIntros` functions (lines 508–658, ~150 lines) **stay in the facade** since they orchestrate the overall flow. The `go` loop and helpers are delegated to submodules.

### 5e. Update `mlf2.cabal`

In the `other-modules` stanza, add:
```
MLF.Elab.Phi.Omega.Interpret.Env,
MLF.Elab.Phi.Omega.Interpret.Helpers,
MLF.Elab.Phi.Omega.Interpret.OmegaLoop,
```

### 5f. Create directory

```
mkdir -p src/MLF/Elab/Phi/Omega/Interpret/
```

### Verification

```bash
cabal build all 2>&1 | head -50
```

---

## Step 6: Final verification

```bash
cd /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-163
cabal build all && cabal test
```

### Acceptance criteria

| Criterion | Target |
|-----------|--------|
| `cabal build all && cabal test` | passes, 1288 examples, 0 failures |
| `MLF.Constraint.Normalize` facade | < 200 lines |
| `MLF.Reify.Type` facade | < 200 lines |
| `MLF.Elab.Run.ResultType.Fallback` facade | < 200 lines |
| `MLF.Constraint.Presolution.Plan` facade | < 200 lines |
| `MLF.Elab.Phi.Omega.Interpret` facade | < 200 lines |
| New submodules in `mlf2.cabal` | 12 new entries |
| No behavioral changes | identical test output |

### Line count targets (approximate)

| Module | Before | Facade after | New submodules |
|--------|--------|-------------|----------------|
| `Normalize` | 848 | ~80 | `Internal` (~60), `Graft` (~370), `Merge` (~370) |
| `Reify.Type` | 822 | ~160 | `Type.Core` (~670) |
| `Fallback` | 822 | ~90 | `Fallback.Core` (~650) |
| `Plan` | 821 | ~65 | `Plan.Env` (~70), `Plan.Generalize` (~530), `Plan.ReifyStep` (~120) |
| `Interpret` | 1226 | ~180 | `Interpret.Env` (~230), `Interpret.Helpers` (~280), `Interpret.OmegaLoop` (~500) |

---

## Implementation order recommendation

Execute splits in this order for minimal risk and incremental verification:

1. **Step 1** (Normalize) — cleanest separation, well-defined function boundaries
2. **Step 2** (Reify.Type) — clean two-way split, core vs. wrappers
3. **Step 3** (Fallback) — clean two-way split, entry points vs. core
4. **Step 4** (Plan) — three-way split with existing sub-module pattern
5. **Step 5** (Interpret) — hardest split due to closure variables; requires `InterpretEnv` refactor

Run `cabal build all && cabal test` after each step before proceeding.

---

## Important implementation notes

1. **`-Wall` compliance**: All new modules must have explicit export lists. Watch for unused imports after moving functions.

2. **Import clashes**: `MLF.Constraint.Types` is imported unqualified in many modules. New `Internal` modules that re-export `NormalizeState` must not clash with those names.

3. **`where` clause extraction**: When extracting from a `where` clause, any variable captured by closure must become an explicit parameter. Document each such parameter clearly.

4. **Module header style**: Follow existing pattern with `{- | Module ... Description ... -}` doc comments.

5. **No re-export widening**: The facade must export exactly the same names as before. Internal submodules should NOT be imported by any module other than their parent facade.
