# Gen-Fallback Removal: Verification & Closure Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Verify that DEV-GEN-FALLBACK-PRESENT is fully addressed, add a positive Q(g) test, close the deviation and spec tasks.

**Architecture:** The spec in `.kiro/specs/thesis-exact-gen-fallback-removal/` anticipated function names (`closestGenAncestor`, `immediateGen`, `bindersFromGen`) that don't exist in the codebase. Analysis shows the actual gen-ancestor fallback protection is already in place: `checkNoGenFallback` rejects fallback-dependent constraints, `parentRefForBinders` routes Q(g) through gen nodes, and `tryBound` in Context.hs is thesis-correct bound-descent (`∀(⩾ C)`), not gen-ancestor fallback.

**Tech Stack:** Haskell, Hspec, cabal

---

### Task 1: Run spec verification commands and confirm completion

**Files:**
- Read: `.kiro/specs/thesis-exact-gen-fallback-removal/tasks.md`

**Step 1: Run all verification commands from the spec**

```bash
# Task 1.1 verification — should return NO matches (functions don't exist)
rg -n "closestGenAncestor|bindersFromGen" src/MLF/Elab/Phi.hs src/MLF/Elab/Phi/Context.hs

# Task 1.2 verification — bindersBase is a local variable, not a fallback function
rg -n "closestGenAncestor|immediateGen" src/MLF/Elab/Reify.hs src/MLF/Reify/Core.hs

# Task 2.1 verification — GenRef routing exists
rg -n "interiorOfUnder|GenRef" src/MLF/Elab/Generalize.hs

# Task 3.2 verification — checkNoGenFallback is wired in
rg -n "checkNoGenFallback" src/MLF

# Task 4 verification — full test suite passes
cabal test --test-show-details=direct
```

Expected:
- Tasks 1.1, 1.2: No matches for the anticipated fallback functions
- Task 2.1: GenRef usage exists (for scheme scope, not fallback)
- Task 3.2: checkNoGenFallback found in Validation.hs, Tree.hs, Translate.hs
- Task 4: 765 examples, 0 failures

**Step 2: Document verification results**

Record which verification commands passed and what they confirm.

---

### Task 2: Add positive Q(g) gen-node translation test

**Files:**
- Modify: `test/ElaborationSpec.hs` (near line 2379, after the existing gen-fallback tests)

**Step 1: Write the positive Q(g) test**

Add a test that constructs a constraint where a gen node owns a scheme root with
direct flex children, calls `boundFlexChildren` on the `GenRef`, and verifies the
returned binders match Q(g) — the direct flex children of the gen node.

```haskell
it "Q(g) returns direct flex children of gen node" $ do
    let rootGen = GenNodeId 0
        schemeRoot = NodeId 100
        aN = NodeId 1
        bN = NodeId 2
        body = NodeId 101
        nodes = nodeMapFromList
            [ (getNodeId schemeRoot, TyForall schemeRoot body)
            , (getNodeId body, TyArrow body aN bN)
            , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
            , (getNodeId bN, TyVar { tnId = bN, tnBound = Nothing })
            ]
        bindParents = IntMap.fromList
            [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))
            , (nodeRefKey (typeRef aN), (typeRef schemeRoot, BindFlex))
            , (nodeRefKey (typeRef bN), (typeRef schemeRoot, BindFlex))
            ]
        constraint = emptyConstraint
            { cNodes = nodes
            , cBindParents = bindParents
            , cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
            }

    -- Q(g) should NOT include schemeRoot's children — only direct gen children
    genBinders <- requireRight (Binding.boundFlexChildren constraint (genRef rootGen))
    genBinders `shouldBe` [schemeRoot]

    -- Q(n) for schemeRoot should include its direct children
    forallBinders <- requireRight (Binding.boundFlexChildren constraint (typeRef schemeRoot))
    forallBinders `shouldBe` [aN, bN]

    -- checkNoGenFallback should pass (schemeRoot has direct binders)
    Binding.checkNoGenFallback constraint `shouldBe` Right ()
```

**Step 2: Run the test to verify it passes**

Run: `cabal test --test-show-details=direct 2>&1 | tail -5`
Expected: all tests pass (766 examples, 0 failures)

**Step 3: Commit**

```bash
git add test/ElaborationSpec.hs
git commit -m "Add positive Q(g) gen-node translation test"
```

---

### Task 3: Mark spec tasks as done and update deviation status

**Files:**
- Modify: `.kiro/specs/thesis-exact-gen-fallback-removal/tasks.md`
- Modify: `docs/thesis-deviations.yaml`

**Step 1: Update tasks.md — mark all tasks checked with completion notes**

Replace each `- [ ]` with `- [x]` and add completion notes explaining what
implements each task:

- Task 1.1: `tryBound` is thesis-correct bound-descent; no gen-ancestor fallback exists
- Task 1.2: `ModeTypeNoFallback` + `parentRefForBinders` handle direct binders; no `closestGenAncestor`/`immediateGen` exist
- Task 2.1: `parentRefForBinders` routes scheme roots to `GenRef`; `bindersForGen` in Selection.hs computes Q(g)
- Task 2.2: No additional entrypoint needed — routing is already explicit
- Task 3.1: `checkNoGenFallback` in `Binding/Validation.hs` (lines 186-220)
- Task 3.2: Wired into `requireValidBindingTree` in `Phi/Translate.hs` (line 439)
- Task 4.1: Context tests at ElaborationSpec.hs lines 2212-2351
- Task 4.2: Invariant test at line 2353; positive Q(g) test added in Task 2

**Step 2: Update thesis-deviations.yaml — upgrade DEV-GEN-FALLBACK-PRESENT**

Change the deviation entry:
- `status: mitigated` → `status: accepted`
- Update description to note that `checkNoGenFallback` rejects fallback-dependent
  constraints, `parentRefForBinders` routes Q(g) explicitly, and no gen-ancestor
  fallback code paths remain
- Update `rationale` to reference the validation guard and test evidence

**Step 3: Run full test suite**

Run: `cabal test --test-show-details=direct`
Expected: all tests pass

**Step 4: Commit**

```bash
git add .kiro/specs/thesis-exact-gen-fallback-removal/tasks.md docs/thesis-deviations.yaml
git commit -m "Close DEV-GEN-FALLBACK-PRESENT: verify and accept gen-fallback removal"
```

---

## Analysis Summary

The spec anticipated removing functions that don't exist. The actual protection is:

| Mechanism | Location | Purpose |
|-----------|----------|---------|
| `checkNoGenFallback` | `Binding/Validation.hs:186` | Rejects constraints needing gen-ancestor fallback |
| `requireValidBindingTree` | `Phi/Translate.hs:432` | Gates Φ translation on binding-tree + gen-fallback + scheme-closure checks |
| `parentRefForBinders` | `Reify/Core.hs:565` | Routes scheme roots to `GenRef` for Q(g) |
| `bindersForGen` | `Presolution/Plan/BinderPlan/Selection.hs:100` | Computes Q(g) from direct gen-node children |
| `boundFlexChildrenUnder` | `Binding/Tree.hs:194` | Returns direct flex children (no fallback) |
| `tryBound` (Context.hs) | `Phi/Context.hs:221` | Thesis-correct bound-descent `∀(⩾ C)`, NOT gen-ancestor fallback |
