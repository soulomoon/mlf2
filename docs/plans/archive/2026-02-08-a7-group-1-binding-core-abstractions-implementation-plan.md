# A7 Group 1: Binding Core Abstractions Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Consolidate duplicated binding/path/node/scope helper logic into shared modules and migrate all current call sites without behavior regressions.

**Architecture:** Introduce narrow, pure helper modules under `MLF.Binding` for path traversal, node-ref enumeration, scope-graph building, and bound-child collection. Migrate `Queries`, `Validation`, `Tree`, and binding-related presolution helpers to call the new helpers, then delete duplicated local implementations. Preserve external behavior by locking coverage with characterization tests before and after migration.

**Tech Stack:** Haskell (GHC2021/Haskell2010 modules), Cabal, Hspec/QuickCheck, `containers`, `mtl`.

---

## Skills & Constraints
- Use @haskell-pro for module/API design and total functions.
- Use @test-driven-development and keep red→green per task.
- Use @verification-before-completion before claiming done.
- Keep paper-faithful behavior unchanged; this group is abstraction-only.

### Task 1: Add Red Tests for Shared Binding Helper Modules

**Files:**
- Create: `test/BindingSharedAbstractionSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Write the failing test**

```haskell
module BindingSharedAbstractionSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
import MLF.Binding.Path (bindingPathToRootWithLookup, firstGenAncestorFromPath)
import MLF.Binding.NodeRefs (allNodeRefs, nodeRefExists)

spec :: Spec
spec = describe "Binding shared abstractions" $ do
  it "bindingPathToRootWithLookup follows lookup chains to root" $ do
    let bp = IntMap.fromList
          [ (nodeRefKey (TypeRef (NodeId 2)), (TypeRef (NodeId 1), BindFlex))
          , (nodeRefKey (TypeRef (NodeId 1)), (GenRef (GenNodeId 0), BindFlex))
          ]
    bindingPathToRootWithLookup (`IntMap.lookup` bp) (TypeRef (NodeId 2))
      `shouldBe` Right [TypeRef (NodeId 2), TypeRef (NodeId 1), GenRef (GenNodeId 0)]

  it "firstGenAncestorFromPath returns nearest strict gen ancestor" $ do
    let fakePath (TypeRef (NodeId 5)) = Right [TypeRef (NodeId 5), GenRef (GenNodeId 2), GenRef (GenNodeId 0)]
        fakePath _ = Right []
    firstGenAncestorFromPath fakePath (TypeRef (NodeId 5)) `shouldBe` Just (GenNodeId 2)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Binding shared abstractions'`
Expected: FAIL with compile errors like `Could not find module 'MLF.Binding.Path'`.

**Step 3: Write minimal implementation**

Create `src/MLF/Binding/Path.hs` and `src/MLF/Binding/NodeRefs.hs` with minimal compile-ready implementations:

```haskell
-- src/MLF/Binding/Path.hs
module MLF.Binding.Path (
  bindingPathToRootWithLookup,
  bindingPathToRoot,
  bindingPathToRootLocal,
  firstGenAncestorFromPath
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)
import MLF.Constraint.Types

bindingPathToRootWithLookup
  :: (Int -> Maybe (NodeRef, BindFlag))
  -> NodeRef
  -> Either BindingError [NodeRef]
bindingPathToRootWithLookup lookupParent start =
  let go seen path key =
        if IntSet.member key seen
          then Left (BindingCycleDetected (reverse path))
          else case lookupParent key of
            Nothing -> Right (reverse path)
            Just (p, _) -> go (IntSet.insert key seen) (p:path) (nodeRefKey p)
  in go IntSet.empty [start] (nodeRefKey start)

bindingPathToRoot :: Constraint -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRoot c = bindingPathToRootWithLookup (`IntMap.lookup` cBindParents c)

bindingPathToRootLocal :: BindParents -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRootLocal bp = bindingPathToRootWithLookup (`IntMap.lookup` bp)

firstGenAncestorFromPath
  :: (NodeRef -> Either BindingError [NodeRef])
  -> NodeRef
  -> Maybe GenNodeId
firstGenAncestorFromPath pathLookup start =
  case pathLookup start of
    Left _ -> Nothing
    Right path -> listToMaybe [ gid | GenRef gid <- drop 1 path ]
```

```haskell
-- src/MLF/Binding/NodeRefs.hs
module MLF.Binding.NodeRefs (allNodeRefs, nodeRefExists) where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Types

allNodeRefs :: Constraint -> [NodeRef]
allNodeRefs c =
  map (TypeRef . fst) (toListNode (cNodes c)) ++
  map (GenRef . GenNodeId) (IntMap.keys (getGenNodeMap (cGenNodes c)))

nodeRefExists :: Constraint -> NodeRef -> Bool
nodeRefExists c ref = case ref of
  TypeRef nid -> maybe False (const True) (lookupNodeIn (cNodes c) nid)
  GenRef gid -> IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
```

Also wire modules in `mlf2.cabal` and `test/Main.hs`.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Binding shared abstractions'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/BindingSharedAbstractionSpec.hs test/Main.hs mlf2.cabal src/MLF/Binding/Path.hs src/MLF/Binding/NodeRefs.hs
git commit -m "refactor(binding): add shared path and node-ref helper modules"
```

### Task 2: Add Red Tests for Scope-Graph and Bound-Children Helpers

**Files:**
- Modify: `test/BindingSharedAbstractionSpec.hs`
- Create: `src/MLF/Binding/ScopeGraph.hs`
- Create: `src/MLF/Binding/Children.hs`
- Modify: `mlf2.cabal`

**Step 1: Write the failing test**

```haskell
import MLF.Binding.ScopeGraph (buildTypeEdgesFrom, rootsForScope)

it "rootsForScope returns non-referenced nodes inside scope" $ do
  let edges = IntMap.fromList [(0, IntSet.fromList [1,2]), (1, IntSet.singleton 3)]
      scope = IntSet.fromList [0,1,2,3]
  rootsForScope id Just edges scope `shouldBe` IntSet.singleton 0
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='rootsForScope returns'`
Expected: FAIL with compile error: missing `MLF.Binding.ScopeGraph` (or missing symbol).

**Step 3: Write minimal implementation**

```haskell
-- src/MLF/Binding/ScopeGraph.hs
module MLF.Binding.ScopeGraph (
  buildTypeEdgesFrom,
  buildScopeNodesFromPaths,
  rootsForScope
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Control.Monad (foldM, when)
import MLF.Constraint.Types

buildTypeEdgesFrom :: (NodeId -> Int) -> NodeMap TyNode -> IntMap.IntMap IntSet
buildTypeEdgesFrom toKey nodes =
  foldl' addOne IntMap.empty (map snd (toListNode nodes))
  where
    addOne m node =
      let pk = toKey (tnId node)
          kids = IntSet.delete pk (IntSet.fromList [toKey k | k <- structuralChildrenWithBounds node])
      in if IntSet.null kids then m else IntMap.insertWith IntSet.union pk kids m

buildScopeNodesFromPaths
  :: (NodeRef -> Either BindingError [NodeRef])
  -> [Int]
  -> Either BindingError (IntMap.IntMap IntSet)
buildScopeNodesFromPaths lookupPath = foldM step IntMap.empty
  where
    step acc nidInt = do
      path <- lookupPath (TypeRef (NodeId nidInt))
      let gens = [ gid | GenRef gid <- path ]
      when (null gens) (Left (MissingBindParent (TypeRef (NodeId nidInt))))
      pure (foldl' (\m gid -> IntMap.insertWith IntSet.union (getGenNodeId gid) (IntSet.singleton nidInt) m) acc gens)

rootsForScope
  :: (Int -> Int)
  -> (Int -> Maybe Int)
  -> IntMap.IntMap IntSet
  -> IntSet
  -> IntSet
rootsForScope parentKey childKey edges scope =
  let referenced = IntSet.fromList
        [ cid
        | nid <- IntSet.toList scope
        , ck <- IntSet.toList (IntMap.findWithDefault IntSet.empty (parentKey nid) edges)
        , Just cid <- [childKey ck]
        , IntSet.member cid scope
        ]
  in IntSet.difference scope referenced
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Binding shared abstractions'`
Expected: PASS for the new tests.

**Step 5: Commit**

```bash
git add test/BindingSharedAbstractionSpec.hs src/MLF/Binding/ScopeGraph.hs src/MLF/Binding/Children.hs mlf2.cabal
git commit -m "refactor(binding): add shared scope graph and child-collection helpers"
```

### Task 3: Migrate Existing Call Sites and Remove Duplication

**Files:**
- Modify: `src/MLF/Binding/Queries.hs`
- Modify: `src/MLF/Binding/Validation.hs`
- Modify: `src/MLF/Binding/Tree.hs`
- Modify: `src/MLF/Binding/Canonicalization.hs`
- Modify: `src/MLF/Constraint/BindingUtil.hs`
- Modify: `src/MLF/Constraint/Presolution/Base.hs`

**Step 1: Write the failing test**

Add a migration regression in `test/BindingSpec.hs` that compares path/interior behavior across representative constraints before and after canonicalization.

```haskell
it "interiorOf equals interiorOfUnder id on valid constraints" $ property $ 
  (SmallBindingConstraint c0) ->
    forAll (elements (bindingRoots c0)) $ \root ->
      interiorOf c0 root == interiorOfUnder id c0 root
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='interiorOf equals interiorOfUnder id'`
Expected: FAIL if migration is incomplete or helper wiring is inconsistent.

**Step 3: Write minimal implementation**

Replace duplicated local definitions with imports:

```haskell
-- Queries.hs
import MLF.Binding.Path (bindingPathToRoot, bindingPathToRootLocal, bindingPathToRootWithLookup)
import MLF.Binding.NodeRefs (allNodeRefs, nodeRefExists)

-- Validation.hs
import MLF.Binding.Path (bindingPathToRoot, bindingPathToRootLocal, bindingPathToRootWithLookup, firstGenAncestorFromPath)
import MLF.Binding.NodeRefs (allNodeRefs, nodeRefExists)
import MLF.Binding.ScopeGraph (buildTypeEdgesFrom, buildScopeNodesFromPaths, rootsForScope)
import MLF.Binding.Children (collectBoundChildrenWithFlag)
```

Update `BindingUtil.firstGenAncestorFrom` to delegate to the shared path helper and keep `ElabError` conversion local.

**Step 4: Run tests to verify pass**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='MLF.Binding.Tree'`
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Binding shared abstractions'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Binding/Queries.hs src/MLF/Binding/Validation.hs src/MLF/Binding/Tree.hs src/MLF/Binding/Canonicalization.hs src/MLF/Constraint/BindingUtil.hs src/MLF/Constraint/Presolution/Base.hs test/BindingSpec.hs
git commit -m "refactor(binding): migrate duplicated path/scope/children logic to shared helpers"
```

### Task 4: Final Verification and Documentation Sync

**Files:**
- Modify: `TODO.md` (mark progress note under A7 if needed)
- Modify: `CHANGELOG.md`
- Modify: `implementation_notes.md`

**Step 1: Write the failing test**

No new behavioral tests. Add an explicit checklist entry in `implementation_notes.md` for removed duplication points.

**Step 2: Run verification gate**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 3: Write minimal documentation updates**

Add short notes:
- Which duplicated helpers were removed
- Which new modules are now canonical

**Step 4: Re-run verification**

Run: `cabal test --test-show-details=direct`
Expected: PASS.

**Step 5: Commit**

```bash
git add TODO.md CHANGELOG.md implementation_notes.md
git commit -m "docs(binding): document shared helper consolidation for A7"
```
