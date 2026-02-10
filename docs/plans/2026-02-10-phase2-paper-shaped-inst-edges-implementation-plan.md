# Phase 2 Paper-Shaped Instantiation Edges Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enforce a thesis-shaped invariant that every Phase-4 instantiation edge has a `TyExp` left endpoint, and make Phase 4 fail fast if this invariant is violated.

**Architecture:** Add a normalization pass in Phase 2 that rewrites every remaining instantiation edge into `TyExp ≤ τ` form (including type-error edges), preserving edge IDs and binding-tree ancestry. Extend `NormalizeState` with expansion-variable allocation so wrappers are fresh and collision-free. In Phase 4, replace permissive branching with a hard assertion that rejects any non-`TyExp` left edge.

**Tech Stack:** Haskell, Cabal, Hspec (`mlf2-test`), existing presolution/normalization modules.

---

### Task 1: Add failing tests for Phase-2 paper-shape invariant

**Files:**
- Modify: `test/NormalizeSpec.hs`
- Test: `test/NormalizeSpec.hs`

**Step 1: Write the failing tests**

Add these tests under `describe "Phase 2 — Normalization"`:

```haskell
it "wraps residual Var <= Var edges with a TyExp on the left" $ do
    let nodes = nodeMapFromList
            [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
            , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
            ]
        edge0 = InstEdge (EdgeId 7) (NodeId 0) (NodeId 1)
        c0 = emptyConstraint { cNodes = nodes, cInstEdges = [edge0] }
        c1 = normalize c0
    cInstEdges c1 `shouldSatisfy` ((== 1) . length)
    let [InstEdge eid l r] = cInstEdges c1
    eid `shouldBe` EdgeId 7
    r `shouldBe` NodeId 1
    case lookupNodeMaybe (cNodes c1) l of
        Just TyExp{ tnBody = b } -> b `shouldBe` NodeId 0
        other -> expectationFailure ("expected TyExp-left edge, got " ++ show other)

it "wraps residual type-error edges too (strict invariant)" $ do
    let nodes = nodeMapFromList
            [ (0, TyBase (NodeId 0) (BaseTy "Int"))
            , (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3))
            , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
            , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
            ]
        edge0 = InstEdge (EdgeId 9) (NodeId 0) (NodeId 1)
        c0 = emptyConstraint { cNodes = nodes, cInstEdges = [edge0] }
        c1 = normalize c0
    let [InstEdge _ l _] = cInstEdges c1
    case lookupNodeMaybe (cNodes c1) l of
        Just TyExp{} -> pure ()
        other -> expectationFailure ("expected TyExp-left error edge, got " ++ show other)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`

Expected: FAIL because normalization currently leaves non-`TyExp` left edges unchanged.

**Step 3: Write minimal implementation**

Do not implement full behavior yet; only add a placeholder note near `graftInstEdges` call site to mark where Phase-2 wrapping will run.

```haskell
-- TODO(paper-shape): enforce TyExp-left form for all remaining cInstEdges
```

**Step 4: Run test to verify it still fails for the right reason**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`

Expected: FAIL with assertion on non-`TyExp` left endpoints.

**Step 5: Commit**

```bash
git add test/NormalizeSpec.hs src/MLF/Constraint/Normalize.hs
git commit -m "test: add failing normalization tests for paper-shaped inst edges"
```

### Task 2: Add fresh expansion-var allocation to normalization state

**Files:**
- Modify: `src/MLF/Constraint/Normalize.hs`
- Test: `test/NormalizeSpec.hs`

**Step 1: Write the failing test**

Add a test asserting wrappers get distinct `ExpVarId`s when multiple residual edges are wrapped:

```haskell
it "allocates fresh ExpVarIds for synthesized TyExp wrappers" $ do
    let nodes = nodeMapFromList
            [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
            , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
            , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
            ]
        e0 = InstEdge (EdgeId 1) (NodeId 0) (NodeId 1)
        e1 = InstEdge (EdgeId 2) (NodeId 0) (NodeId 2)
        c1 = normalize (emptyConstraint { cNodes = nodes, cInstEdges = [e0, e1] })
        expVars =
            [ s
            | InstEdge _ l _ <- cInstEdges c1
            , Just TyExp { tnExpVar = s } <- [lookupNodeMaybe (cNodes c1) l]
            ]
    length expVars `shouldBe` 2
    length (IntSet.fromList (map getExpVarId expVars)) `shouldBe` 2
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match fresh ExpVarIds"`

Expected: FAIL (no synthesized wrappers yet).

**Step 3: Write minimal implementation**

In `NormalizeState`, add `nsNextExpVar`; initialize it from the maximum existing `TyExp` exp-var key.

```haskell
data NormalizeState = NormalizeState
    { nsNextNodeId :: !Int
    , nsNextExpVar :: !Int
    , nsUnionFind :: !(IntMap NodeId)
    , nsConstraint :: !Constraint
    }

freshExpVarIdNorm :: NormalizeM ExpVarId
freshExpVarIdNorm = do
    n <- gets nsNextExpVar
    modify' $ \s -> s { nsNextExpVar = n + 1 }
    pure (ExpVarId n)
```

Add helper to seed `nsNextExpVar`:

```haskell
maxExpVarKeyOrMinus1 :: Constraint -> Int
maxExpVarKeyOrMinus1 c =
    case [ getExpVarId s | (_, TyExp{ tnExpVar = s }) <- toListNode (cNodes c) ] of
        [] -> -1
        xs -> maximum xs
```

**Step 4: Run test to verify compile + interim behavior**

Run: `cabal build mlf2-test`

Expected: PASS build (feature test still failing until Task 3).

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Normalize.hs test/NormalizeSpec.hs
git commit -m "refactor: add normalization ExpVar allocator state"
```

### Task 3: Implement Phase-2 wrapper pass (all residual edges)

**Files:**
- Modify: `src/MLF/Constraint/Normalize.hs`
- Test: `test/NormalizeSpec.hs`

**Step 1: Write the failing test**

Add a binding-parent inheritance regression:

```haskell
it "inherits the left-node binding parent for synthesized TyExp wrappers" $ do
    let left = NodeId 0
        right = NodeId 1
        g0 = GenNodeId 0
        nodes = nodeMapFromList
            [ (0, TyVar { tnId = left, tnBound = Nothing })
            , (1, TyVar { tnId = right, tnBound = Nothing })
            ]
        bp = IntMap.fromList [(nodeRefKey (typeRef left), (genRef g0, BindFlex))]
        c0 = emptyConstraint
            { cNodes = nodes
            , cInstEdges = [InstEdge (EdgeId 0) left right]
            , cBindParents = bp
            }
        c1 = normalize c0
        [InstEdge _ wrappedLeft _] = cInstEdges c1
    IntMap.lookup (nodeRefKey (typeRef wrappedLeft)) (cBindParents c1)
        `shouldBe` Just (genRef g0, BindFlex)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match inherits the left-node binding parent"`

Expected: FAIL until wrapper insertion copies parent info.

**Step 3: Write minimal implementation**

Implement `enforcePaperShapedInstEdges` and call it in `normalizeLoop` after `applyUnionFindToConstraint`:

```haskell
normalizeLoop = do
    before <- gets nsConstraint
    ...
    applyUnionFindToConstraint
    enforcePaperShapedInstEdges
    after <- gets nsConstraint
    when (before /= after) normalizeLoop

enforcePaperShapedInstEdges :: NormalizeM ()
enforcePaperShapedInstEdges = do
    edges <- gets (cInstEdges . nsConstraint)
    rewritten <- mapM wrapLeft edges
    modify' $ \s ->
        let c0 = nsConstraint s
        in s { nsConstraint = c0 { cInstEdges = rewritten } }
```

`wrapLeft` behavior:
- Canonicalize `instLeft`/`instRight` via current union-find roots.
- If left root is already `TyExp`, keep it.
- Otherwise allocate `TyExp freshNid freshExpVar leftRoot`, insert it into `cNodes`, copy left parent binding (if present), and rewrite edge left to `freshNid`.
- Preserve `instEdgeId` unchanged.

**Step 4: Run tests to verify they pass**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match fresh ExpVarIds"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match inherits the left-node binding parent"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Normalize.hs test/NormalizeSpec.hs
git commit -m "feat: enforce TyExp-left inst-edge invariant in normalization"
```

### Task 4: Add failing test for Phase-4 fail-fast assertion

**Files:**
- Modify: `test/Presolution/EdgeTraceSpec.hs`
- Test: `test/Presolution/EdgeTraceSpec.hs`

**Step 1: Write the failing test**

Add a new example in `EdgeTraceSpec` that runs `processInstEdge` on a non-`TyExp` left edge and expects `InternalError`:

```haskell
it "fails fast when processInstEdge receives non-TyExp left edge" $ do
    let left = NodeId 0
        right = NodeId 1
        nodes = nodeMapFromList
            [ (0, TyVar { tnId = left, tnBound = Nothing })
            , (1, TyVar { tnId = right, tnBound = Nothing })
            ]
        c0 = rootedConstraint emptyConstraint { cNodes = nodes, cBindParents = inferBindParents nodes }
        st0 = PresolutionState c0 (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
        edge = InstEdge (EdgeId 0) left right
    case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
        Left (InternalError msg) -> msg `shouldContain` "expected TyExp-left"
        Left err -> expectationFailure ("expected InternalError, got: " ++ show err)
        Right _ -> expectationFailure "expected processInstEdge to fail"
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match fails fast when processInstEdge receives non-TyExp left edge"`

Expected: FAIL because current code accepts non-`TyExp` branch.

**Step 3: Write minimal implementation**

In `src/MLF/Constraint/Presolution/EdgeProcessing.hs`, replace permissive branch with fail-fast:

```haskell
case n1Raw of
    TyExp { tnExpVar = s } -> ...existing path...
    _ ->
        throwError
            (InternalError
                ( "processInstEdge: expected TyExp-left edge after normalize; edge="
                    ++ show edgeId
                    ++ " leftNode="
                    ++ nodeTag n1Raw
                )
            )
```

Keep witness/trace behavior unchanged for the valid `TyExp` path.

**Step 4: Run tests to verify it passes**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match fails fast when processInstEdge receives non-TyExp left edge"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgeTrace"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgeTraceSpec.hs
git commit -m "feat: assert TyExp-left invariant in phase-4 edge processing"
```

### Task 5: Remove dead non-TyExp edge path and tighten comments

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Test: `test/Presolution/EdgeTraceSpec.hs`

**Step 1: Write the failing test**

Add/adjust a test assertion that there is no fallback structural solve path in `processInstEdge` behavior (only `TyExp` path or error).

```haskell
it "does not perform fallback direct-mode solving in processInstEdge" $ do
    -- Reuse the non-TyExp setup; success path is forbidden.
    ...
```

**Step 2: Run test to verify it fails (if fallback remains)**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match direct-mode"`

Expected: FAIL until fallback logic is removed.

**Step 3: Write minimal implementation**

Delete now-unreachable helper branch usage from `processInstEdge`; keep auxiliary helpers only if still referenced by `unifyStructure` internals. Update module notes to state:
- Phase 2 guarantees `TyExp`-left edges.
- Phase 4 enforces this precondition with fail-fast `InternalError`.

**Step 4: Run tests to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match processInstEdge"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgeTraceSpec.hs
git commit -m "refactor: remove direct-mode branch from processInstEdge"
```

### Task 6: Document the invariant and record project progress

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`

**Step 1: Write the failing doc check (manual)**

Confirm docs still describe non-`TyExp` Phase-4 handling.

Run: `rg -n "Phase 4|TyExp|inst edge|direct" implementation_notes.md TODO.md`

Expected: stale or incomplete description of the new invariant.

**Step 2: Update documentation**

Add a concise note in `implementation_notes.md`:

```markdown
- Normalization now rewrites all residual instantiation edges to `TyExp <= τ`.
- Presolution `processInstEdge` treats non-`TyExp` left edges as invariant violations and fails fast.
```

Add a `CHANGELOG.md` entry summarizing:
- Phase-2 strict paper-shape wrapper pass
- Phase-4 fail-fast assertion
- regression tests added

Update `TODO.md` next steps:
- migrate edge-processing DSL to single `TyExp`-mode foundation

**Step 3: Run markdown sanity checks**

Run: `rg -n "TyExp <=|fail fast|paper-shape" implementation_notes.md CHANGELOG.md TODO.md`

Expected: updated lines present.

**Step 4: Commit docs**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md
git commit -m "docs: record paper-shaped inst-edge invariant and phase-4 assertion"
```

**Step 5: (Optional squash marker)**

If maintaining linear history for PR, mark this commit for squash with the feature branch before opening PR.

### Task 7: Full verification before completion

**Files:**
- Verify working tree only (no code changes expected)

**Step 1: Run targeted specs first**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match Phase 2 — Normalization"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match processInstEdge"`

Expected: PASS.

**Step 2: Run full validation command**

Run: `cabal build all && cabal test`

Expected: full build + full test suite PASS.

**Step 3: Check no accidental file drift**

Run: `git status --short`

Expected: only intended files changed.

**Step 4: Final commit (if any outstanding staged changes)**

```bash
git add -A
git commit -m "test: finalize paper-shaped inst-edge invariant rollout"
```

**Step 5: Prepare PR summary**

Include:
- problem statement (non-paper-shaped residual edges)
- architectural change (Phase-2 wrapping + Phase-4 assertion)
- test evidence (`NormalizeSpec`, `EdgeTraceSpec`, full suite)
- thesis-faithfulness rationale.
