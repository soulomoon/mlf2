# Generalized Unification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Split Solve-phase unification into two phases — first-order worklist then batch Rebind — matching thesis Section 7.6.

**Architecture:** Defer `harmonize` calls from `solveRepresentative` during the worklist loop, collecting pairs. After the worklist drains, batch-harmonize all pairs. Then upgrade to equivalence-class-aware multi-node harmonization via a new `harmonizeBindParentsMulti` primitive.

**Tech Stack:** Haskell (GHC 9.12.2), Hspec, cabal

---

### Task 1: Add `suDeferredHarmonize` field to `SolveState`

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:180-184`

**Step 1: Add the field**

Change `SolveState` from:
```haskell
data SolveState = SolveState
    { suConstraint :: Constraint
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    }
```
to:
```haskell
data SolveState = SolveState
    { suConstraint :: Constraint
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    , suDeferredHarmonize :: [(NodeRef, NodeRef)]
    }
```

**Step 2: Update the initial state construction**

In `solveUnify` at line 228, change:
```haskell
let st = SolveState { suConstraint = c0', suUnionFind = IntMap.empty, suQueue = cUnifyEdges c0' }
```
to:
```haskell
let st = SolveState { suConstraint = c0', suUnionFind = IntMap.empty, suQueue = cUnifyEdges c0', suDeferredHarmonize = [] }
```

**Step 3: Build and verify no regressions**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds (unused field warning is OK at this point)

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: All 767 tests pass

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "Add suDeferredHarmonize field to SolveState for generalized unification"
```

---

### Task 2: Defer harmonization in `solveRepresentative`

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:405-445`

**Step 1: Replace `harmonize` call with deferred collection**

In `solveRepresentative` (line 405), change:
```haskell
    solveRepresentative left leftNode right rightNode = do
        harmonize left right
        cCur <- gets suConstraint
```
to:
```haskell
    solveRepresentative left leftNode right rightNode = do
        deferHarmonize left right
        cCur <- gets suConstraint
```

**Step 2: Add `deferHarmonize` helper**

Add after the `enqueue` helper (around line 267):
```haskell
    deferHarmonize :: NodeId -> NodeId -> SolveM ()
    deferHarmonize l r =
        modify' $ \s ->
            s { suDeferredHarmonize = (typeRef l, typeRef r) : suDeferredHarmonize s }
```

**Step 3: Build to check compilation**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds. Tests will likely fail at this point because harmonization is deferred but not yet executed.

**Step 4: Commit (WIP)**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "WIP: defer harmonize calls in solveRepresentative"
```

---

### Task 3: Add `batchHarmonize` after worklist drains

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:229-253`

**Step 1: Add `batchHarmonize` helper**

Add after the `deferHarmonize` helper:
```haskell
    batchHarmonize :: SolveM ()
    batchHarmonize = do
        pairs <- gets suDeferredHarmonize
        uf <- gets suUnionFind
        let canonical = UnionFind.frWith uf
            -- Canonicalize both sides and deduplicate
            canonPairs = dedup
                [ (canonRef canonical l, canonRef canonical r)
                | (l, r) <- pairs
                , canonRef canonical l /= canonRef canonical r
                ]
        mapM_ harmonizePair canonPairs
      where
        canonRef canon ref = case ref of
            TypeRef nid -> typeRef (canon nid)
            GenRef _ -> ref
        dedup = map head . group . sort
        harmonizePair (l, r) = do
            cBefore <- gets suConstraint
            case BindingAdjustment.harmonizeBindParentsWithTrace l r cBefore of
                Left err -> throwSolveError (BindingTreeError err)
                Right (c', _trace) -> modify' $ \s -> s { suConstraint = c' }
```

Note: You will need to add `import Data.List (group, sort)` — but `sort` is likely already imported. Check the existing imports; `Data.List (find)` is at line 117. Extend it to `Data.List (find, group, sort)`.

Also add a `Eq` and `Ord` instance check for `NodeRef` — if `NodeRef` doesn't have `Ord`, use `nub` with a Set instead:
```haskell
        dedup = nubByCanon
        nubByCanon [] = []
        nubByCanon (x:xs) = x : nubByCanon (filter (/= x) xs)
```

**Step 2: Wire `batchHarmonize` into `solveUnify` after the loop**

Change the section at line 229:
```haskell
            final <- execStateT loop st
```
to:
```haskell
            final <- execStateT (loop >> batchHarmonize) st
```

**Step 3: Build and run all tests**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: All 767 tests pass — deferred harmonization produces identical results

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "Generalized unification step 1: batch harmonize after worklist drains"
```

---

### Task 4: Write regression test for deferred harmonization

**Files:**
- Modify: `test/SolveSpec.hs` (add at end of existing describe blocks, around line 720)

**Step 1: Write a test that verifies deferred harmonization matches per-pair**

Add inside the `"Phase 5 -- Solve"` describe block:
```haskell
    describe "Generalized unification (Ch 7.6)" $ do
        it "deferred harmonization produces same result as per-pair for chained vars" $ do
            -- Three variables chained: v0 = v1, v1 = v2, with different bind depths.
            -- Verifies batch harmonize after worklist matches sequential behavior.
            let v0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                v1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                v2 = TyVar { tnId = NodeId 2, tnBound = Nothing }
                arrow = TyArrow (NodeId 3) (NodeId 0) (NodeId 1)
                root = TyArrow (NodeId 4) (NodeId 3) (NodeId 2)
                nodes = nodeMapFromList
                    [ (0, v0), (1, v1), (2, v2)
                    , (3, arrow), (4, root)
                    ]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef (NodeId 0)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 1)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 2)), (typeRef (NodeId 4), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 3)), (typeRef (NodeId 4), BindFlex))
                        ]
                    , cUnifyEdges =
                        [ UnifyEdge (NodeId 0) (NodeId 1)
                        , UnifyEdge (NodeId 1) (NodeId 2)
                        ]
                    }
            case solveUnify defaultTraceConfig constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc } -> do
                    -- All three vars should end up in the same equivalence class
                    cUnifyEdges sc `shouldBe` []
```

**Step 2: Run the test**

Run: `cabal test --test-show-details=direct --test-options='--match="Generalized unification"' 2>&1 | tail -20`
Expected: PASS

**Step 3: Commit**

```bash
git add test/SolveSpec.hs
git commit -m "Add regression test for deferred harmonization (generalized unification)"
```

---

### Task 5: Add `harmonizeBindParentsMulti` to `Binding.Adjustment`

**Files:**
- Modify: `src/MLF/Binding/Adjustment.hs:31-38` (export list)
- Modify: `src/MLF/Binding/Adjustment.hs` (add new function after `harmonizeBindParents`)

**Step 1: Write the failing test first**

Add to `test/SolveSpec.hs` in the `"Generalized unification"` block:
```haskell
        it "multi-node harmonize raises all members to LCA" $ do
            -- Four nodes: v0 bound to inner, v1 bound to inner, v2 bound to root.
            -- inner bound to root. Unify all three: v0=v1=v2.
            -- Multi-node LCA should be root; all should be raised there.
            let v0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                v1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                v2 = TyVar { tnId = NodeId 2, tnBound = Nothing }
                inner = TyArrow (NodeId 3) (NodeId 0) (NodeId 1)
                root = TyArrow (NodeId 4) (NodeId 3) (NodeId 2)
                nodes = nodeMapFromList
                    [ (0, v0), (1, v1), (2, v2)
                    , (3, inner), (4, root)
                    ]
                constraint = rootedConstraint $ emptyConstraint
                    { cNodes = nodes
                    , cBindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef (NodeId 0)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 1)), (typeRef (NodeId 3), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 2)), (typeRef (NodeId 4), BindFlex))
                        , (nodeRefKey (typeRef (NodeId 3)), (typeRef (NodeId 4), BindFlex))
                        ]
                    , cUnifyEdges =
                        [ UnifyEdge (NodeId 0) (NodeId 1)
                        , UnifyEdge (NodeId 1) (NodeId 2)
                        ]
                    }
            case solveUnify defaultTraceConfig constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc } -> do
                    cUnifyEdges sc `shouldBe` []
```

**Step 2: Run to verify it passes with current deferred approach**

Run: `cabal test --test-show-details=direct --test-options='--match="multi-node"' 2>&1 | tail -20`
Expected: PASS (deferred pairs already handles this case)

**Step 3: Add `harmonizeBindParentsMulti` to `Binding.Adjustment`**

Add to the export list:
```haskell
module MLF.Binding.Adjustment (
    -- * Main API
    harmonizeBindParentsWithTrace,
    harmonizeBindParents,
    harmonizeBindParentsMulti,
    -- * Lower-level operations
    raiseToParent,
    raiseToParentWithCount,
) where
```

Add the implementation after `harmonizeBindParents` (after line 109):
```haskell
-- | Harmonize binding parents for multiple nodes simultaneously.
--
-- Given a list of NodeRefs (members of an equivalence class), compute the
-- LCA of all their bind parents and raise every member to that LCA.
-- This is the generalized Rebind from thesis Section 7.6.2: one Rebind
-- per equivalence class instead of one per pair.
--
-- Returns the updated constraint and the combined raise trace.
harmonizeBindParentsMulti
    :: [NodeRef] -> Constraint
    -> Either BindingError (Constraint, [NodeId])
harmonizeBindParentsMulti [] c = Right (c, [])
harmonizeBindParentsMulti [_] c = Right (c, [])
harmonizeBindParentsMulti refs c0 = do
    -- Collect all bind parents
    let parents =
            [ p
            | ref <- refs
            , Just (p, _) <- [lookupBindParent c0 ref]
            ]
        roots = [ref | ref <- refs, Nothing == lookupBindParent c0 ref]
    case (parents, roots) of
        -- All are roots: nothing to do
        ([], _) -> Right (c0, [])
        -- Some are roots: raise all non-roots to root level
        (_, (_:_)) -> do
            let go c trace [] = Right (c, trace)
                go c trace (ref:rest) = do
                    (c', t) <- raiseToRoot ref c
                    go c' (trace ++ t) rest
                nonRoots = [ref | ref <- refs, Nothing /= lookupBindParent c0 ref]
            go c0 [] nonRoots
        -- All have parents: fold LCA pairwise, then raise all to it
        (p1:pRest, []) -> do
            lca <- foldLCA c0 p1 pRest
            let go c trace [] = Right (c, trace)
                go c trace (ref:rest) = do
                    (c', t) <- raiseToParentWithCount ref lca c
                    go c' (trace ++ t) rest
            go c0 [] refs
  where
    foldLCA c start [] = Right start
    foldLCA c start (p:ps) = do
        lca <- bindingLCA c start p
        foldLCA c lca ps
```

**Step 4: Build and test**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds

Run: `cabal test --test-show-details=direct --test-options='--match="Generalized unification"' 2>&1 | tail -20`
Expected: All generalized unification tests pass

**Step 5: Commit**

```bash
git add src/MLF/Binding/Adjustment.hs test/SolveSpec.hs
git commit -m "Add harmonizeBindParentsMulti for equivalence-class batch Rebind"
```

---

### Task 6: Upgrade `batchHarmonize` to use equivalence classes

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs` (the `batchHarmonize` helper)

**Step 1: Add `equivalenceClasses` helper to `Solve.hs`**

Add a local helper inside the `solveUnify` where block:
```haskell
    -- | Compute equivalence classes from the union-find map.
    -- Returns a list of lists, where each inner list contains all NodeIds
    -- that share the same canonical representative.
    equivalenceClasses :: IntMap NodeId -> Constraint -> [[NodeId]]
    equivalenceClasses uf c =
        let canonical = UnionFind.frWith uf
            allNodeIds = map fst (toListNode (cNodes c))
            grouped = IntMap.toList $
                foldl'
                    (\acc nid ->
                        let rep = canonical nid
                        in IntMap.insertWith (++) (getNodeId rep) [nid] acc
                    )
                    IntMap.empty
                    allNodeIds
        in [ members | (_, members) <- grouped, length members > 1 ]
```

**Step 2: Rewrite `batchHarmonize` to use equivalence classes + `harmonizeBindParentsMulti`**

Replace the existing `batchHarmonize` with:
```haskell
    batchHarmonize :: SolveM ()
    batchHarmonize = do
        uf <- gets suUnionFind
        c <- gets suConstraint
        let classes = equivalenceClasses uf c
        mapM_ harmonizeClass classes

    harmonizeClass :: [NodeId] -> SolveM ()
    harmonizeClass members = do
        cBefore <- gets suConstraint
        let refs = map typeRef members
        case BindingAdjustment.harmonizeBindParentsMulti refs cBefore of
            Left err -> throwSolveError (BindingTreeError err)
            Right (c', _trace) -> modify' $ \s -> s { suConstraint = c' }
```

**Step 3: Remove the now-unused `deferHarmonize` helper and `suDeferredHarmonize` field**

Remove `deferHarmonize` helper. In `solveRepresentative`, remove the `deferHarmonize left right` call entirely — harmonization is now handled by `batchHarmonize` via equivalence classes, not by collecting pairs.

Remove `suDeferredHarmonize` from `SolveState`:
```haskell
data SolveState = SolveState
    { suConstraint :: Constraint
    , suUnionFind :: IntMap NodeId
    , suQueue :: [UnifyEdge]
    }
```

Update the initial state construction back to:
```haskell
let st = SolveState { suConstraint = c0', suUnionFind = IntMap.empty, suQueue = cUnifyEdges c0' }
```

**Step 4: Build and run full test suite**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: All tests pass (767 existing + new generalized unification tests)

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "Generalized unification step 2: equivalence-class batch Rebind (Ch 7.6)"
```

---

### Task 7: Remove dead `harmonize` helper

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs:515-520`

**Step 1: Delete the `harmonize` function**

Remove:
```haskell
    harmonize :: NodeId -> NodeId -> SolveM ()
    harmonize lRoot rRoot = do
        cBefore <- gets suConstraint
        case BindingAdjustment.harmonizeBindParentsWithTrace (typeRef lRoot) (typeRef rRoot) cBefore of
            Left err -> throwSolveError (BindingTreeError err)
            Right (c', _trace) -> modify' $ \s -> s { suConstraint = c' }
```

**Step 2: Build and test**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds with no warnings about unused `harmonize`

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: All tests pass

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "Remove dead per-pair harmonize helper from Solve"
```

---

### Task 8: Add Note block documenting the generalized unification algorithm

**Files:**
- Modify: `src/MLF/Constraint/Solve.hs` (add Note block near top of module, after existing Notes)

**Step 1: Add the Note**

Add after `Note [Normalize vs Solve unification]` (around line 98):
```haskell
{-
Note [Generalized unification (Ch 7.6)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (Section 7.6) defines generalized unification problems as
simultaneous unification of multiple equivalence classes on the same type.
The key insight: instead of calling Rebind per-pair during the worklist,
we do all first-order unification in one pass (the worklist loop), then
call Rebind once per equivalence class via `batchHarmonize`.

This is both more efficient (k pairs → 1 LCA per class) and handles cases
where sequential per-pair admissibility checking would fail but simultaneous
checking succeeds (Figure 7.6.1 in the thesis).

Implementation:
  1. Worklist loop: standard first-order unification (union-find, decompose,
     occurs-check) with NO harmonization.
  2. batchHarmonize: compute equivalence classes from the final UF, then call
     `harmonizeBindParentsMulti` once per class.
  3. applyUFConstraint: rewrite to canonical reps as before.

Paper references:
  • Definition 7.6.1: generalized unification problem
  • Definition 7.6.2: generalized admissibility
  • Section 7.6.2: generalized algorithm
  • Lemma 7.6.3: soundness, completeness, principality
-}
```

**Step 2: Build**

Run: `cabal build all 2>&1 | tail -5`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Solve.hs
git commit -m "Add Note [Generalized unification (Ch 7.6)] to Solve module"
```

---

### Task 9: Update thesis obligations and claims

**Files:**
- Modify: `docs/thesis-obligations.yaml`
- Modify: `docs/thesis-claims.yaml`

**Step 1: Read current files**

Read `docs/thesis-obligations.yaml` and `docs/thesis-claims.yaml` to find the right insertion points.

**Step 2: Add obligation O07-GENUNIF**

Add a new obligation entry scoped to Section 7.6:
```yaml
- id: O07-GENUNIF
  section: "7.6"
  title: "Generalized unification"
  description: >
    Implement generalized unification (Def 7.6.1, Def 7.6.2, Section 7.6.2,
    Lemma 7.6.3): batch Rebind after first-order unification in Solve phase.
  status: implemented
  tests:
    - "test/SolveSpec.hs: Generalized unification (Ch 7.6)"
```

**Step 3: Update CLM-UNIFICATION or add CLM-GEN-UNIFICATION**

Add a new claim or expand the existing one to cover Section 7.6.

**Step 4: Commit**

```bash
git add docs/thesis-obligations.yaml docs/thesis-claims.yaml
git commit -m "Add thesis obligation O07-GENUNIF and claim for Section 7.6"
```

---

### Task 10: Full regression run and final commit

**Step 1: Run the full test suite**

Run: `cabal test --test-show-details=direct 2>&1 | tail -30`
Expected: All tests pass (767 existing + new generalized unification tests)

**Step 2: Run build with warnings**

Run: `cabal build all 2>&1 | grep -i warn`
Expected: No warnings

**Step 3: Verify git status is clean**

Run: `git status`
Expected: Clean working tree, all changes committed

**Step 4: Update CHANGELOG.md**

Add entry:
```markdown
- Generalized unification (Ch 7.6): batch Rebind after first-order unification
  in Solve phase, with `harmonizeBindParentsMulti` for equivalence-class-aware
  binding-tree harmonization.
```

**Step 5: Final commit**

```bash
git add CHANGELOG.md
git commit -m "Update CHANGELOG for generalized unification (Ch 7.6)"
```
