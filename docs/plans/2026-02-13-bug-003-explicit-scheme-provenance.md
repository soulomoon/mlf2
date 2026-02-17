# BUG-003 Fix: Explicit Scheme Provenance for Binder Enumeration

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix BUG-003 (BUG-2026-02-11-004) by making scheme provenance `(ownerGen, schemeRoot)` explicit throughout the presolution pipeline, eliminating the heuristic `implicitBindersM` fallback that causes binder identity mismatches with Phi translation.

**Architecture:** The thesis defines instantiation from a scheme node `s = hg·i` introduced by gen node `g`, with binder order from `<P` (leftmost-lowermost) in the binding tree. Currently, `instantiationBindersM` falls back to `implicitBindersM` when the body isn't a TyForall — this heuristic searches `allGenNodes` and returns different binder nodes than what `siSubst` (built by `buildBinderPlan`) contains. The fix threads explicit `GenNodeId` through the pipeline so binder enumeration always uses the owning gen node's scope, matching what `bindForallBindersFromSpec` selects during ForallIntro.

**Tech Stack:** Haskell, GHC, Cabal, Hspec

**Paper references:**
- `papers/these-finale-english.txt:9440` — instantiation from scheme at gen node
- `papers/these-finale-english.txt:10553` — expansion of scheme
- `papers/these-finale-english.txt:16893` — binder order `<P`
- `papers/these-finale-english.txt:10607,11589,17305` — degenerate/trivial scheme handling

**Reproducer:**
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V"'
```

---

### Task 1: Extract `findSchemeIntroducerM` to StateAccess

**Files:**
- Modify: `src/MLF/Constraint/Presolution/StateAccess.hs` (add export + function)
- Modify: `src/MLF/Constraint/Presolution/Copy.hs` (import from StateAccess, remove local def)

**Step 1: Add `findSchemeIntroducerM` to StateAccess.hs**

Add to the export list after `liftBindingError`:

```haskell
    -- * Scheme provenance
    findSchemeIntroducerM
```

Add at the bottom of the file:

```haskell
-- | Find the gen node that introduces a scheme rooted at the given body node.
--
-- Walks the binding-parent chain upward from the body root until it hits a
-- GenRef. This is the thesis-aligned scheme owner discovery: a scheme s is
-- introduced by the gen node g such that s ∈ schemes(g).
--
-- Fails with InternalError if no gen ancestor is found on the binding path.
findSchemeIntroducerM :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM GenNodeId
findSchemeIntroducerM canonical c0 root0 = do
    let root = canonical root0
    path <- liftBindingError $
        BindingCanonical.withQuotientBindParents "findSchemeIntroducerM" canonical c0 (typeRef root) $ \startC bindParents ->
            BindingPath.bindingPathToRootLocal bindParents startC
    case [gid | GenRef gid <- path] of
        (gid:_) -> pure gid
        [] ->
            throwError
                (InternalError ("findSchemeIntroducerM: scheme introducer not found for " ++ show root))
```

This requires importing `BindingCanonical` and `BindingPath` — check what Copy.hs uses. Actually, Copy.hs calls `bindingPathToRootUnderM` from Base.hs. Use the same approach:

```haskell
findSchemeIntroducerM :: (NodeId -> NodeId) -> Constraint -> NodeId -> PresolutionM GenNodeId
findSchemeIntroducerM canonical c0 root0 = do
    let root = canonical root0
    path <- Base.bindingPathToRootUnderM canonical c0 (typeRef root)
    case [gid | GenRef gid <- path] of
        (gid:_) -> pure gid
        [] ->
            throwError
                (InternalError ("findSchemeIntroducerM: scheme introducer not found for " ++ show root))
```

Import `bindingPathToRootUnderM` from `Base` (it's already exported).

**Step 2: Update Copy.hs to import from StateAccess**

Remove the local `findSchemeIntroducerM` definition (lines 57-66). Add import:

```haskell
import MLF.Constraint.Presolution.StateAccess (findSchemeIntroducerM, ...)
```

**Step 3: Build to verify**

Run: `cabal build all`
Expected: Clean build, no errors.

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/StateAccess.hs src/MLF/Constraint/Presolution/Copy.hs
git commit -m "refactor: extract findSchemeIntroducerM to StateAccess"
```

---

### Task 2: Add `eprSchemeOwnerGen` to EdgePlan and compute in planner

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs` (add field)
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs` (compute gen node)

**Step 1: Add field to EdgePlan**

In `Plan.hs`, add to `EdgePlanResolved`:

```haskell
data EdgePlan = EdgePlanResolved
    { eprEdge :: InstEdge
    , eprLeftTyExp :: ResolvedTyExp
    , eprRightNode :: TyNode
    , eprLeftCanonical :: NodeId
    , eprRightCanonical :: NodeId
    , eprAllowTrivial :: Bool
    , eprSuppressWeaken :: Bool
    , eprSchemeOwnerGen :: GenNodeId   -- NEW
    }
    deriving (Eq, Show)
```

Update `mkEmptyResolvedPlan` to accept and store it:

```haskell
mkEmptyResolvedPlan ::
    InstEdge ->
    ResolvedTyExp ->
    TyNode ->
    NodeId ->
    NodeId ->
    GenNodeId ->    -- NEW
    EdgePlan
mkEmptyResolvedPlan edge leftTyExp rightNode leftCan rightCan schemeGen =
    EdgePlanResolved
        { ...
        , eprSchemeOwnerGen = schemeGen
        }
```

Add import: `import MLF.Constraint.Types (GenNodeId)` (or from Graph).

**Step 2: Compute in planner**

In `Planner.hs`, add import:

```haskell
import MLF.Constraint.Presolution.StateAccess (findSchemeIntroducerM, getConstraintAndCanonical)
```

In `planEdge`, after resolving `leftTyExp`, compute the scheme owner:

```haskell
    schemeGen <- findSchemeIntroducerM canonical constraint0 (rteBodyId leftTyExp)

    pure EdgePlanResolved
        { ...
        , eprSchemeOwnerGen = schemeGen
        }
```

**Step 3: Fix any callers of `mkEmptyResolvedPlan`**

Search for `mkEmptyResolvedPlan` and update call sites to pass the new `GenNodeId` argument. (Check test files too.)

**Step 4: Build to verify**

Run: `cabal build all`
Expected: Clean build.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs
git commit -m "feat: add eprSchemeOwnerGen to EdgePlan, compute in planner"
```

---

### Task 3: Add `instantiationBindersFromGenM` in Base.hs

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs` (add new function, export it)

**Step 1: Add the explicit-provenance binder enumeration function**

Add to exports:

```haskell
    instantiationBindersFromGenM,
```

Add the function. This mirrors the binder selection logic in `bindForallBindersFromSpec` (ForallIntro.hs:108-169) so both paths agree on which nodes are binders:

```haskell
-- | Compute instantiation binders from explicit scheme provenance.
--
-- Given the owning gen node and the scheme body root, enumerate binders
-- using the binding tree scope of the gen node — matching the thesis
-- definition where binders come from the gen node's scope (s = hg·i).
--
-- This replaces the heuristic `implicitBindersM` with explicit provenance.
instantiationBindersFromGenM :: GenNodeId -> NodeId -> PresolutionM (NodeId, [NodeId])
instantiationBindersFromGenM gid bodyRoot0 = do
    (c0, canonical) <- getConstraintAndCanonical
    let bodyC = canonical bodyRoot0
        nodes = cNodes c0

    -- 1. Get flex children under the gen node's scope
    bindersUnderGen <- case Binding.boundFlexChildrenUnder canonical c0 (genRef gid) of
        Left err -> throwError (BindingTreeError err)
        Right bs -> pure bs

    -- 2. Compute reachability from the body root
    let reachable =
            Traversal.reachableFromUnderLenient
                canonical
                (lookupNodeIn nodes)
                bodyC

    -- 3. Filter to live TyVar nodes reachable from body
    let isLiveVar nid =
            case lookupNodeIn nodes nid of
                Just TyVar{} -> not (VarStore.isEliminatedVar c0 nid)
                _ -> False

        bindersReachable =
            [ canonical b
            | b <- bindersUnderGen
            , IntSet.member (getNodeId (canonical b)) reachable
            , isLiveVar (canonical b)
            ]

    -- 4. Deduplicate by canonical ID
    let bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId b, b)
                    | b <- bindersReachable
                    ]

    -- 5. Partition into freeLike (parent outside body subgraph) / other
    bp <- liftBindingError $ Binding.canonicalizeBindParentsUnder canonical c0
    let parentInfoOf nid = IntMap.lookup (nodeRefKey (typeRef nid)) bp

        isFreeLike nid =
            case parentInfoOf nid of
                Nothing -> True
                Just (p, flag) ->
                    flag == BindFlex && case p of
                        TypeRef pN -> not (IntSet.member (getNodeId pN) reachable)
                        GenRef _ -> True

        isFlexBound nid =
            case parentInfoOf nid of
                Nothing -> True
                Just (_p, flag) -> flag == BindFlex

        (freeLike0, other0) = partition isFreeLike bindersCanon

    -- 6. Sort by order keys (leftmost-lowermost, paper <P)
    let orderKeys = Order.orderKeysFromRootWith canonical nodes bodyC Nothing

    freeLike <- case Order.sortByOrderKey orderKeys freeLike0 of
        Left err -> throwError $ InternalError ("instantiationBindersFromGenM: order key error (freeLike): " ++ show err)
        Right sorted -> pure sorted
    other <- case Order.sortByOrderKey orderKeys (filter isFlexBound other0) of
        Left err -> throwError $ InternalError ("instantiationBindersFromGenM: order key error (other): " ++ show err)
        Right sorted -> pure sorted

    -- 7. Exclude wrapper body if it's a bound variable
    let bodyIsWrapper =
            case lookupNodeIn nodes bodyC of
                Just TyVar{} ->
                    case VarStore.lookupVarBound c0 bodyC of
                        Just _ -> True
                        Nothing -> False
                _ -> False
        candidates0 = freeLike ++ other
        candidates =
            if bodyIsWrapper
                then filter (/= bodyC) candidates0
                else candidates0

    pure (bodyC, candidates)
```

Ensure these imports are present in Base.hs:
- `qualified MLF.Constraint.Traversal as Traversal`
- `qualified MLF.Constraint.VarStore as VarStore`
- `qualified MLF.Util.Order as Order`
- `qualified MLF.Binding.Tree as Binding`
- `Data.List (partition)`

**Step 2: Build to verify**

Run: `cabal build all`
Expected: Clean build (function is defined but not yet called).

**Step 3: Commit**

```bash
git add src/MLF/Constraint/Presolution/Base.hs
git commit -m "feat: add instantiationBindersFromGenM with explicit scheme provenance"
```

---

### Task 4: Update `instantiationBindersM` to accept `GenNodeId`

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs` (change signature, replace fallback)

**Step 1: Change signature and replace `implicitBindersM` fallback**

Change the signature from:
```haskell
instantiationBindersM :: NodeId -> PresolutionM (NodeId, [NodeId])
```
to:
```haskell
instantiationBindersM :: GenNodeId -> NodeId -> PresolutionM (NodeId, [NodeId])
```

Replace the body. The TyForall path stays (it's already thesis-aligned). The non-TyForall path delegates to `instantiationBindersFromGenM`:

```haskell
instantiationBindersM :: GenNodeId -> NodeId -> PresolutionM (NodeId, [NodeId])
instantiationBindersM gid nid0 = do
    st <- get
    let c0 = psConstraint st
        uf0 = psUnionFind st
        canonical = UnionFind.frWith uf0
        nid = canonical nid0
        cache0 = psBinderCache st
        nodes = cNodes c0
        lookupNode = Types.lookupNode nid nodes
    case IntMap.lookup (getNodeId nid) cache0 of
        Just binders ->
            if null binders
                then pure (nid, binders)
                else do
                    let root =
                            case lookupNode of
                                Just TyForall{ tnBody = inner } -> canonical inner
                                _ -> nid
                    pure (root, binders)
        Nothing -> case lookupNode of
            Nothing -> throwError (NodeLookupFailed nid)
            Just node -> case node of
                TyForall { tnId = forallId, tnBody = inner } -> do
                    binders <- orderedBindersRawM forallId
                    if null binders
                        then instantiationBindersM gid inner
                        else do
                            modify' $ \st1 ->
                                let cache1 = psBinderCache st1
                                    cache2 = IntMap.insert (getNodeId nid) binders cache1
                                    cache3 = IntMap.insert (getNodeId inner) binders cache2
                                in st1 { psBinderCache = cache3 }
                            pure (inner, binders)
                _ -> do
                    -- Explicit provenance: use gen node scope instead of heuristic
                    (bodyRoot, binders) <- instantiationBindersFromGenM gid nid
                    when (not (null binders)) $
                        modify' $ \st1 ->
                            let cache1 = psBinderCache st1
                                cache2 = IntMap.insert (getNodeId nid) binders cache1
                                cache3 = IntMap.insert (getNodeId bodyRoot) binders cache2
                            in st1 { psBinderCache = cache3 }
                    pure (bodyRoot, binders)
```

**Step 2: Delete `implicitBindersM`**

Remove the entire `implicitBindersM` function (lines 591-709) and its local helper `reachableFromWithBounds`. Check if `reachableFromWithBounds` is used elsewhere — if not, delete it. Also remove the `<|>` import from `Control.Applicative` if no longer needed (check `traceInteriorRootRef` still uses it).

**Step 3: This will cause compile errors in all callers — that's expected**

Do NOT build yet. Proceed to Task 5 to update all callers.

**Step 4: Commit (with callers broken — optional, or combine with Task 5)**

Can defer commit to after Task 5 if preferred.

---

### Task 5: Update all callers of `instantiationBindersM`

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Expansion.hs`
- Modify: `src/MLF/Constraint/Presolution/Witness.hs`
- Modify: `src/MLF/Constraint/Presolution/Copy.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Solve.hs`
- Modify: `src/MLF/Constraint/Presolution/Materialization.hs`

This is the largest task. Thread `GenNodeId` through all call sites.

**Step 1: Update Expansion.hs — `decideMinimalExpansion`**

Change signature:
```haskell
decideMinimalExpansion :: GenNodeId -> Bool -> TyNode -> TyNode -> PresolutionM (Expansion, [(NodeId, NodeId)])
```

At line 200, pass `gid`:
```haskell
(bodyRoot, boundVars) <- instantiationBindersM gid bodyId
```

**Step 2: Update Expansion.hs — `applyExpansion`, `applyExpansionTraced`, `applyExpansionEdgeTraced`**

Add `GenNodeId` as first parameter to each:
```haskell
applyExpansion :: GenNodeId -> Expansion -> TyNode -> PresolutionM NodeId
applyExpansionTraced :: GenNodeId -> Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
applyExpansionEdgeTraced :: GenNodeId -> Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
```

In each `cata` algebra, capture `gid` in the closure. The `ExpInstantiateF` cases change from:
```haskell
(bodyRoot, boundVars) <- instantiationBindersM b
```
to:
```haskell
(bodyRoot, boundVars) <- instantiationBindersM gid b
```

All 6 call sites within these functions (lines 320, 330, 378, 390, 447, 475) get the same treatment.

**Step 3: Update Witness.hs — `witnessFromExpansion` and `binderArgsFromExpansion`**

Change signatures:
```haskell
witnessFromExpansion :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM [InstanceStep]
binderArgsFromExpansion :: GenNodeId -> TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
```

In `witnessAlg`, capture `gid` in the closure. The `ExpInstantiateF` case at line 105:
```haskell
(_bodyRoot, boundVars) <- instantiationBindersM gid b
```

In `binderArgsFromExpansion`, the local `instantiationBinders` helper:
```haskell
let instantiationBinders nid = do
        (_bodyRoot, binders) <- instantiationBindersM gid nid
        pure binders
```

**Step 4: Update EdgeProcessing/Witness.hs — `edgeWitnessPlan` and `buildEdgeTrace`**

Change signatures:
```haskell
edgeWitnessPlan :: GenNodeId -> Bool -> NodeId -> TyNode -> Expansion -> PresolutionM EdgeWitnessPlan
buildEdgeTrace :: GenNodeId -> EdgeId -> NodeId -> TyNode -> Expansion -> (CopyMap, InteriorSet, FrontierSet) -> PresolutionM EdgeTrace
```

In `edgeWitnessPlan` line 73:
```haskell
baseSteps0 <- witnessFromExpansion gid root leftRaw expn
```

In `buildEdgeTrace` line 120:
```haskell
bas <- binderArgsFromExpansion gid leftRaw expn
```

Also in `buildEdgeTrace`, replace `traceInteriorRootRef` call (line 127) with direct gen ref:
```haskell
let interiorRootRef = genRef gid
```

**Step 5: Update EdgeProcessing/Unify.hs — `runExpansionUnify`**

Change signature:
```haskell
runExpansionUnify :: GenNodeId -> EdgeId -> TyNode -> TyNode -> Expansion -> [InstanceOp] -> PresolutionM EdgeExpansionResult
```

At line 97:
```haskell
(resNodeId, (copyMap0, interior0, frontier0)) <- applyExpansionEdgeTraced gid expn leftRaw
```

At line 130:
```haskell
bas <- binderArgsFromExpansion gid leftRaw expn
```

**Step 6: Update EdgeProcessing/Interpreter.hs — `executeUnifiedExpansionPath`**

Extract `schemeGen` from the plan:
```haskell
let schemeGen = eprSchemeOwnerGen plan
```

Update all downstream calls:
- Line 77: `decideMinimalExpansion schemeGen (eprAllowTrivial plan) n1Raw n2`
- Line 93: `edgeWitnessPlan schemeGen (eprSuppressWeaken plan) n1Id n1Raw finalExp`
- Line 97: `runExpansionUnify schemeGen edgeId n1Raw n2 finalExp (ewpBaseOps witnessPlan)`
- Line 101: `buildEdgeTrace schemeGen edgeId n1Id n1Raw finalExp expTrace`

**Step 7: Update EdgeProcessing/Solve.hs — `unifyExpansionNode`**

In `unifyExpansionNode`, compute `gid` from the TyExp body:
```haskell
unifyExpansionNode expNode targetId = do
    (c0, canonical) <- getConstraintAndCanonical
    gid <- findSchemeIntroducerM canonical c0 (tnBody expNode)
    ...
```

Update calls:
- Line 120: `decideMinimalExpansion gid True expNode targetNode`
- Line 142: `applyExpansionEdgeTraced gid finalExp expNode`
- Line 155: `binderArgsFromExpansion gid expNode finalExp`

Import `findSchemeIntroducerM` from `StateAccess`.

**Step 8: Update Materialization.hs — `materializeExpansions`**

In the `forM exps` loop, compute `gid` per TyExp:
```haskell
fmap IntMap.fromList $ forM exps $ \expNode -> do
    let eid = tnId expNode
    expn <- getExpansion (tnExpVar expNode)
    (c0, canonical) <- getConstraintAndCanonical
    gid <- findSchemeIntroducerM canonical c0 (tnBody expNode)
    nid' <- case expn of
        ExpIdentity -> applyExpansion gid expn expNode
        _ ->
            let root = canonical eid
            in if root /= eid
                then pure root
                else applyExpansion gid expn expNode
    pure (getNodeId eid, nid')
```

Import `findSchemeIntroducerM` from `StateAccess`.

**Step 9: Update Copy.hs — `expansionCopySetsM`**

Already has `gid` at line 87. Change line 141:
```haskell
(_root, binders) <- instantiationBindersM gid bodyC
```

**Step 10: Build to verify**

Run: `cabal build all`
Expected: Clean build, no errors.

**Step 11: Commit**

```bash
git add src/MLF/Constraint/Presolution/Base.hs \
        src/MLF/Constraint/Presolution/Expansion.hs \
        src/MLF/Constraint/Presolution/Witness.hs \
        src/MLF/Constraint/Presolution/Copy.hs \
        src/MLF/Constraint/Presolution/Materialization.hs \
        src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs \
        src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs \
        src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs \
        src/MLF/Constraint/Presolution/EdgeProcessing/Solve.hs
git commit -m "feat: thread explicit GenNodeId through presolution pipeline, remove implicitBindersM"
```

---

### Task 6: Run tests and fix BUG-003

**Files:**
- Modify: `test/ElaborationSpec.hs` (flip BUG-003 expectations if needed)
- Possibly modify: `src/MLF/Constraint/Presolution/Base.hs` (tune binder enumeration)

**Step 1: Run BUG-003 tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V"'`
Expected: BUG-003-V1 and BUG-003-V2 should now pass (or show a different, more specific error if further tuning is needed).

**Step 2: Run BUG-004 regression tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004"'`
Expected: All BUG-004 variants still pass.

**Step 3: Run full test suite**

Run: `cabal test mlf2-test --test-show-details=direct`
Expected: 652+ examples, 0 failures.

**Step 4: If BUG-003 tests still fail, debug**

The most likely issue is that `instantiationBindersFromGenM` returns a different number of binders than `decideMinimalExpansion` expects (arity mismatch). Compare the binder count from the new function against what the expansion's `ExpInstantiate` args expect. The fix may require adjusting the binder count or the expansion decision.

**Step 5: Commit**

```bash
git add -A
git commit -m "fix: BUG-003 higher-arity bounded alias chains now elaborate correctly"
```

---

### Task 7: Update bug tracker and docs

**Files:**
- Modify: `Bugs.md` (mark BUG-2026-02-11-004 as resolved)
- Modify: `CHANGELOG.md` (add entry)
- Modify: `implementation_notes.md` (document the explicit provenance approach)

**Step 1: Update Bugs.md**

Move BUG-2026-02-11-004 to resolved section with:
- Resolution: explicit scheme provenance eliminates `implicitBindersM` heuristic
- Regression tests: `BUG-003-V1`, `BUG-003-V2` in `test/ElaborationSpec.hs`

**Step 2: Update CHANGELOG.md**

Add entry:
```
- Fix BUG-003: higher-arity bounded alias chains (∀a.∀(b≥a).∀(c≥b). ...) now elaborate correctly by threading explicit scheme provenance (ownerGen, schemeRoot) through the presolution pipeline, replacing the heuristic `implicitBindersM` fallback.
```

**Step 3: Update implementation_notes.md**

Document the explicit provenance approach and its thesis alignment.

**Step 4: Commit**

```bash
git add Bugs.md CHANGELOG.md implementation_notes.md
git commit -m "docs: update bug tracker, changelog, and implementation notes for BUG-003 closure"
```
