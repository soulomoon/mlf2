# Round 152 Plan — Fix reifyInst TyMu without authoritative binder child

## Selected Item
- `item-2`: Fix reifyInst TyMu without authoritative binder child for non-local proxy
- Roadmap: `2026-03-29-03-non-local-proxy-phi-translation-and-reclassification` rev-001

## Problem Summary

In `src/MLF/Reify/Type.hs`, the `goFull` function handles `TyMu` nodes at
lines 352–372. For **local** TyMu nodes, `orderedFlexChildren` returns exactly
one binder child (the μ-variable TyVar, registered as a flex-child via
`attachUnder` during constraint generation — see
`src/MLF/Frontend/ConstraintGen/Translate.hs:675`). For **non-local proxy**
TyMu wrappers (where a recursive annotation crosses a let boundary and the
binding tree is not extended to cover the proxy), `orderedFlexChildren` returns
`[]`, causing a `PhiTranslatabilityError`.

## Implementation Steps

### Step 1: Fix the `[]` case in the TyMu branch of `goFull`

**File:** `src/MLF/Reify/Type.hs`
**Lines:** 352–372 (the `TyMu` guard inside `goFull`)
**Tool:** bash + sed (per AGENTS.md fourmolu constraint)

**Current code (lines 357–362):**
```haskell
              [] ->
                Left $
                  PhiTranslatabilityError
                    [ "reifyInst: missing authoritative instantiation translation for TyMu without binder child",
                      "expansion args=[]"
                    ]
```

**Replacement for the `[] ->` arm:**
```haskell
              [] -> do
                -- Non-local proxy TyMu: no binder child in binding tree.
                -- Synthesize binder from the TyMu node itself.  The TyMu
                -- node IS the recursive wrapper; its ID serves as the binder
                -- identity.  Any body-internal references to the μ-variable
                -- that were unified away will resolve through normal
                -- reification; cycle detection (inProgress) covers the case
                -- where the body transitively references this TyMu.
                let synthBinder = n
                    namedExtra' = IntSet.insert (getNodeId synthBinder) namedExtra
                (cache', bodyTy) <- vChild cache0 namedExtra' mode (canonical b)
                let t = TMu (varName synthBinder) bodyTy
                    cacheFinal = cacheInsertLocal mode key t cache' namedExtra
                pure (markDone cacheFinal, t)
```

**Rationale:**
- `n` is the canonical TyMu node (already in `inProgress` via `markStart`).
- `varName n` gives a consistent name (from the `nameForVar` callback).
- Inserting `getNodeId n` into `namedExtra` means if the body transitively
  references `n` through the `isNamedLocal` check (line 348), it resolves as
  `TVar (varName n)`, matching the TMu binder name.
- If the body references `n` through `cacheInProgress` cycle detection
  (line 228), it returns `varFor n` = `TVar (varName n)`, also consistent.
- The existing `[bndr]` path (line 356) remains unchanged.
- The existing `_ ->` multi-binder error path (lines 363–367) remains unchanged.

**What this preserves:**
- Local TyMu reification (1-binder path): untouched.
- Multi-binder error: untouched.
- All other `goFull` branches: untouched.

### Step 2: Add targeted unit test for TyMu 0-binder reification

**File:** `test/PipelineSpec.hs`
**Location:** After the existing TyCon reification test (approx. line 395), add
a new `it` block in the same `describe` group.

**Test code (conceptual):**
```haskell
    it "reifies TyMu without binder child (non-local proxy fallback)" $ do
      -- Construct a minimal constraint with a TyMu whose μ-variable has NO
      -- binding-tree entry as a flex-child of the TyMu.  This simulates
      -- the non-local proxy scenario.
      let muVarId  = NodeId 0   -- the μ-variable (TyVar)
          intId    = NodeId 1   -- base type Int
          arrowId  = NodeId 2   -- arrow: muVar -> Int
          muId     = NodeId 3   -- TyMu node (body = arrow)
          muVar    = TyVar  muVarId Nothing
          intNode  = TyBase intId (BaseTy "Int")
          arrowNd  = TyArrow arrowId muVarId intId
          muNode   = TyMu   muId arrowId
          nodes    = fromListNode
                       [ (muVarId, muVar)
                       , (intId,   intNode)
                       , (arrowId, arrowNd)
                       , (muId,    muNode)
                       ]
          -- NO bind-parent entry for muVarId under muId
          constraint = emptyConstraint { cNodes = nodes }
          solved = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (PresolutionViewBoundary.fromSolved solved) muId of
        Right ty -> do
          -- Should produce a TMu wrapping the body type
          case ty of
            TMu _ _ -> pure ()
            _       -> expectationFailure $
                         "Expected TMu, got: " ++ show ty
        Left err ->
          expectationFailure $
            "Non-local proxy TyMu reify should not error: " ++ show err
```

**Why this test is sufficient:**
- It exercises exactly the 0-binder TyMu path in `goFull` (lines 352–372).
- The `emptyConstraint` has no `cBindParents`, so `orderedFlexChildren` returns
  `[]` for the TyMu node — directly hitting the patched code path.
- It verifies the fix produces a `TMu` instead of `PhiTranslatabilityError`.
- It does not test the full elaboration pipeline (OpRaise is item-3's scope).

### Step 3: Assess impact on existing tests that assert the old error

Two existing tests assert `PhiTranslatabilityError` with the exact substring
`"reifyInst: missing authoritative instantiation translation"`:

1. **`test/PipelineSpec.hs:2303–2315`** — `expectStrictPipelineFailure` for
   `let g = (λ(x:μa.a→Int).x) in g g`.
   - `expectStrictPipelineFailure` accepts any pipeline failure containing
     `"PhiTranslatabilityError"` OR several other error tags (lines 95–101).
   - After the fix, the reifyInst TyMu path succeeds, but the pipeline may
     still fail at OpRaise (Site B, `src/MLF/Elab/Phi/Omega/Interpret.hs:1147`)
     with a *different* `PhiTranslatabilityError` ("OpRaise (non-spine): missing
     computation context").
   - **Expected outcome:** Test still passes — `expectStrictPipelineFailure`
     matches `PhiTranslatabilityError` regardless of which site.
   - **Action:** No change needed. Verify by running the test.

2. **`test/Research/P5ClearBoundarySpec.hs:91–117`** — Checks specific error
   snippets including `"reifyInst: missing authoritative instantiation
   translation"` and `"expansion args="`.
   - Expression: `nestedForallContrastExpr` (polymorphic mediation absorbs μ).
   - After the fix, if the TyMu 0-binder path now succeeds for this expression,
     the pipeline may either:
     (a) succeed entirely → the test's `Left err ->` branch is not taken →
         test fails at the `Right (_term, ty) -> expectationFailure ...` branch.
     (b) fail at a different site with different error snippets → the
         `shouldSatisfy isInfixOf` checks fail.
   - **Action:** Run the test suite first. If this test breaks:
     - If the error changes to OpRaise, update the expected snippets to match
       the new error.
     - If the pipeline now succeeds, convert the test to a success assertion
       (the correct non-recursive outcome was already validated at line 73).
     - Either way, the test description at line 91 (already reclassified in
       item-1) accurately describes this as a "downstream consequence."

### Step 4: Build and run full test suite

**Commands:**
```bash
cabal build all && cabal test
```

**Expected outcome:** 1175 examples, 0 failures.

If Step 3 identified a broken test in P5ClearBoundarySpec:91, apply the
adjustment described above and rerun.

### Step 5: Run targeted verification for non-local proxy tests

**Command:**
```bash
cabal test --test-option='-m' --test-option='non-local proxy'
```

This runs any tests matching "non-local proxy" — should include the new test
from Step 2 and the existing PipelineSpec:2303 test.

**Command:**
```bash
cabal test --test-option='-m' --test-option='TyMu without binder'
```

This runs the specific new test from Step 2.

## Files Modified

| File | Change |
|------|--------|
| `src/MLF/Reify/Type.hs` | Replace `PhiTranslatabilityError` in `[] ->` arm (lines 357–362) with synthesized-binder fallback |
| `test/PipelineSpec.hs` | Add unit test for TyMu 0-binder reification (after ~line 395) |
| `test/Research/P5ClearBoundarySpec.hs` | **Conditional** — adjust error expectations at line 91 if the fix changes the error path |

## Files NOT Modified

| File | Reason |
|------|--------|
| `src/MLF/Elab/Phi/Omega/Interpret.hs` | OpRaise fix is item-3's scope |
| `orchestrator/state.json` | Planner does not edit state |
| `orchestrator/roadmaps/*/roadmap.md` | Planner does not change roadmap |

## Risk Assessment

- **Low risk:** The fix is strictly additive — a new fallback arm inside an
  existing pattern match. The `[bndr]` and multi-binder paths are untouched.
- **Medium risk:** P5ClearBoundarySpec:91 may need error-expectation adjustment.
  This is expected and accounted for in Step 3.
- **No risk to non-recursive programs:** The fix only activates for TyMu nodes
  with 0 binder children. Non-recursive programs never produce TyMu nodes.

## Verification Contract Cross-Reference

Per `orchestrator/roadmaps/.../rev-001/verification.md` item-2 checks:

| Check | Plan coverage |
|-------|---------------|
| `reifyInst` handles 0-binder TyMu without `PhiTranslatabilityError` | Step 1 |
| Existing local TyMu reification (1-binder path) unchanged | Step 1 (preserves `[bndr]` arm) |
| New targeted test validates non-local TyMu reification | Step 2 |
| `cabal build all && cabal test` passes | Steps 4–5 |
| `cabal test --test-option='-m' --test-option='non-local proxy'` | Step 5 |

## Implementation Notes for Implementer

- **CRITICAL:** Use bash + sed/heredoc for `.hs` file edits. Do NOT use the
  Edit tool on Haskell files (fourmolu reformatting concern per AGENTS.md).
- The synthesized binder approach is safe because:
  - `varName n` uses the TyMu node's canonical ID, which is stable.
  - `namedExtra` insertion ensures body references to `n` resolve as named vars.
  - `inProgress` cycle detection provides a second safety net for recursive refs.
- The fix matches the pattern of the existing `[bndr]` path — same variables
  (`namedExtra'`, `cache'`, `bodyTy`, `t`, `cacheFinal`), same cache operations,
  same `markDone` wrapping — just with `synthBinder = n` instead of
  `canonical bndr`.
