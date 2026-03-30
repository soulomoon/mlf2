# Round 153 Plan — Fix OpRaise non-spine missing computation context for non-local bind-parent

- roadmap_item_id: item-3
- roadmap_id: 2026-03-29-03-non-local-proxy-phi-translation-and-reclassification
- roadmap_revision: rev-001
- branch: orchestrator/round-153-fix-opraise-non-spine-context
- worktree: orchestrator/worktrees/round-153

## Problem

In `src/MLF/Elab/Phi/Omega/Interpret.hs`, the OpRaise non-spine case (line 1026 onward) computes `mbCandidate` and `mbRootInst` to determine the instantiation context. When both are `Nothing`, it throws `PhiTranslatabilityError` at line 1147 ("OpRaise (non-spine): missing computation context").

After the item-2 fix (reifyInst TyMu), the pipeline gets past reification but hits this OpRaise site. The bind-parent guard at lines 1097–1099 only recognizes `TyForall{}` as a binder-like node. When the bind-parent is `TyMu`, the guard returns `False`, making `mbRootInst = Nothing`. Combined with an empty/shallow spine making `mbCandidate = Nothing`, the error fires.

## Fix Description

`TyMu` is a binder-like node (it introduces a recursive variable, analogous to `TyForall` introducing a polymorphic variable). The bind-parent guard should accept it.

## Steps

### Step 1: Add `TyMu{}` to the `mbRootInst` bind-parent guard

**File:** `orchestrator/worktrees/round-153/src/MLF/Elab/Phi/Omega/Interpret.hs`
**Location:** Lines 1097–1099

**Current code:**
```haskell
                                            || case lookupNodePV parentC of
                                                Just TyForall{} -> True
                                                _ -> False
```

**Change to:**
```haskell
                                            || case lookupNodePV parentC of
                                                Just TyForall{} -> True
                                                Just TyMu{} -> True
                                                _ -> False
```

**Rationale:** `TyMu` is already available in scope via the `MLF.Constraint.Types` import at line 30. No import changes needed — `TyMu` is defined in `MLF.Constraint.Types.Graph.NodeEdge` and re-exported through `MLF.Constraint.Types`.

**Implementation note:** Use `sed` to insert the new line:
```bash
sed -i '' '/Just TyForall{} -> True/a\
                                                Just TyMu{} -> True' \
  orchestrator/worktrees/round-153/src/MLF/Elab/Phi/Omega/Interpret.hs
```

### Step 2: Assess the `nodeTy0` guard at lines 1038–1042

**File:** Same file, lines 1038–1042

**Current code:**
```haskell
                        nodeTy0 <-
                            case lookupBindParent (typeRef nC) of
                                Just (TypeRef parent, _) ->
                                    case lookupNodePV (canonicalNode parent) of
                                        Just TyForall{} -> reifyTypeWithNamedSetNoFallbackAt substForTypes namedSet' nC
                                        _ -> reifyBoundType nC
                                _ -> reifyBoundType nC
```

**Assessment:** This is a secondary guard that decides how to reify `nodeTy0`. When the bind-parent is `TyForall`, it uses `reifyTypeWithNamedSetNoFallbackAt`; otherwise it falls back to `reifyBoundType`. For `TyMu` bind-parents, the fallback `reifyBoundType` should be acceptable since the critical failure path is in `mbRootInst`, not here. However, if tests show the reified type is incorrect after Step 1, this guard may also need `Just TyMu{} -> reifyTypeWithNamedSetNoFallbackAt substForTypes namedSet' nC`. **Do NOT change this in the initial fix** — only if post-fix test results indicate reification problems.

### Step 3: Verify build and existing tests pass

**Commands (in worktree):**
```bash
cd orchestrator/worktrees/round-153
cabal build all
cabal test
```

**Expected:** All 1175+ existing examples pass, 0 failures. The fix is strictly additive (new pattern match arm), so no existing behavior changes for non-TyMu paths.

### Step 4: Assess PipelineSpec:2336 expected-failure test

**File:** `orchestrator/worktrees/round-153/test/PipelineSpec.hs`, lines 2336–2348

**Current test:**
```haskell
it "hits elaboration blocker for non-local proxy wrapper despite open fallback at pipeline entrypoints" $ do
    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
        expr = ELet "g" (ELamAnn "x" recursiveAnn (EVar "x")) (EApp (EVar "g") (EVar "g"))
        pipelineRuns = [ ("unchecked", runPipelineElab ...), ("checked", runPipelineElabChecked ...) ]
    forM_ pipelineRuns $ \(label, result) ->
        expectStrictPipelineFailure (label ++ " non-local proxy wrapper") result
```

This test expects `expectStrictPipelineFailure` (asserts `PhiTranslatabilityError` or similar). After item-3, this test may:

- **(a) Still fail** — if there are additional downstream blockers beyond the `mbRootInst` guard. In this case, no change needed.
- **(b) Start succeeding** — if item-3 was the last blocker. In this case, the test will fail because it expects failure but gets success. **DO NOT change this test** — upgrading it is item-4's scope.

**Action:** Run the test in isolation to determine which case applies:
```bash
cd orchestrator/worktrees/round-153
cabal test --test-option='-m' --test-option='hits elaboration blocker for non-local proxy'
```

Document the result in the review. If case (b), note that the test now needs upgrading but that's item-4 work.

### Step 5: Verify ElaborationSpec non-spine OpRaise test still passes

**File:** `orchestrator/worktrees/round-153/test/ElaborationSpec.hs`, line 4498

The existing test "Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)" at line 4498 uses `TyForall` bind-parents in its handcrafted constraint. This test should be completely unaffected by the change (it doesn't exercise `TyMu`).

**Command:**
```bash
cd orchestrator/worktrees/round-153
cabal test --test-option='-m' --test-option='non-spine OpRaise'
```

### Step 6: Check for `-Wall` warnings

The new `Just TyMu{} -> True` line should not introduce any warnings since `TyMu` is already in scope and the pattern match is not exhaustive-altering (the `_ -> False` wildcard still catches remaining constructors).

**Command:**
```bash
cd orchestrator/worktrees/round-153
cabal build all 2>&1 | grep -i warning
```

## Files Modified

| File | Change |
|------|--------|
| `src/MLF/Elab/Phi/Omega/Interpret.hs` | Add `Just TyMu{} -> True` arm at line 1099 |

## Files NOT Modified (and why)

| File | Reason |
|------|--------|
| `test/PipelineSpec.hs:2336` | Expected-failure→success upgrade is item-4 scope |
| `test/ElaborationSpec.hs` | Existing non-spine test uses TyForall, unaffected |
| No imports in Interpret.hs | `TyMu` already available via `MLF.Constraint.Types` import (line 30) |

## Risk Assessment

- **Low risk:** Single new pattern match arm in an existing case expression. The wildcard `_ -> False` already handles all other constructors; inserting `Just TyMu{} -> True` before it is purely additive.
- **No behavioral change for non-TyMu paths:** All existing tests exercise `TyForall` or root-match paths, which are untouched.
- **Potential downstream issue:** If the pipeline test (PipelineSpec:2336) starts succeeding, it will fail the test suite because it asserts failure. The implementer should document this but NOT fix it (item-4 scope). If this blocks the test gate, the reviewer should assess whether temporarily marking it `pending` is acceptable or whether item-3 and item-4 should be combined.

## Verification Checklist

- [ ] `src/MLF/Elab/Phi/Omega/Interpret.hs` line ~1099: `Just TyMu{} -> True` present
- [ ] `cabal build all` succeeds with no new warnings
- [ ] `cabal test` passes (0 failures)
- [ ] PipelineSpec:2336 behavior documented (still-fails or now-succeeds)
- [ ] ElaborationSpec:4498 non-spine OpRaise test still passes
- [ ] No unrelated files changed
