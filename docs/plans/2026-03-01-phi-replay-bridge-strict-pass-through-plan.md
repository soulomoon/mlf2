# Phi Replay Bridge Strict Pass-Through Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove runtime fallback projection from `computeTraceBinderReplayBridge` so the replay map is validated and passed through unchanged — no synthesis, no repair.

**Architecture:** The bridge function currently implements a five-tier fallback in `projectReplayTarget` (source-name remapping → positional → default → raw domain → canonical domain). We replace this with: domain parity check, per-target codomain membership check, pass-through. Dead helper bindings are removed. One new test covers malformed codomain fail-fast.

**Tech Stack:** Haskell (GHC 9.12.2), Hspec, cabal

---

## Context for the Implementer

### What is the replay bridge?

`computeTraceBinderReplayBridge` in `src/MLF/Elab/Phi/Translate.hs:455-582` is a local helper inside `phiFromEdgeWitnessCore`. It takes an `EdgeTrace` (produced by presolution) and two `SchemeInfo` values (replay-domain and source-domain), validates the trace's `etBinderReplayMap`, and returns a triple `(traceBinderSourceSet, replayMap, replayMapDomain)` consumed by Omega.

### What are we removing?

Lines 537-574 contain `projectReplayTarget` and `projectOne` — a five-tier fallback that remaps each source key through name-matching, positional zipping, default target, and domain membership before passing to Omega. The design says: presolution already produces a correct, deterministic replay map. Runtime should not repair it.

### What stays?

- Domain parity validation (lines 519-535): `missingSources` / `extraSources` check.
- Per-target codomain membership validation (raw or canonical): ensures each replay target is a valid binder in the replay domain.
- Fail-fast `PhiInvariantError` on any violation.

### Key files

| File | Role |
|------|------|
| `src/MLF/Elab/Phi/Translate.hs:455-582` | Bridge function to modify |
| `test/ElaborationSpec.hs:1563-1610` | Existing domain-mismatch test |
| `src/MLF/Elab/Phi/Omega.hs:625-661` | Downstream consumer (read-only) |
| `src/MLF/Constraint/Presolution/Base.hs:148-177` | EdgeTrace contract (read-only) |

---

### Task 1: Write failing test for malformed replay codomain

**Files:**
- Modify: `test/ElaborationSpec.hs` (insert after line ~1610, after the existing domain-mismatch test)

**Step 1: Write the failing test**

Add this test inside the same `describe` block as the existing replay-map domain mismatch test (around line 1563). Insert after the closing of that test at line 1610:

```haskell
            it "fails fast when replay-map codomain target is outside replay binder domain" $ do
                let root = NodeId 100
                    binderA = NodeId 1
                    bogusTarget = NodeId 99
                    c = rootedConstraint emptyConstraint
                        { cNodes = nodeMapFromList
                                [ (getNodeId root, TyArrow root binderA binderA)
                                , (getNodeId binderA, TyVar { tnId = binderA, tnBound = Nothing })
                                , (getNodeId bogusTarget, TyBase bogusTarget (BaseTy "Bool"))
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef binderA), (genRef (GenNodeId 0), BindFlex))
                                ]
                        }
                    solved = mkSolved c IntMap.empty
                    scheme =
                        Elab.schemeFromType
                            (Elab.TForall "a" Nothing
                                (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
                    si = Elab.SchemeInfo
                        { Elab.siScheme = scheme
                        , Elab.siSubst = IntMap.fromList [(getNodeId binderA, "a")]
                        }
                    tr =
                        EdgeTrace
                            { etRoot = root
                            , etBinderArgs = [(binderA, binderA)]
                            , etInterior = fromListInterior [root, binderA, bogusTarget]
                            -- Domain is correct (binderA -> bogusTarget), but bogusTarget
                            -- is not in the replay binder domain (siSubst siReplay).
                            , etBinderReplayMap = IntMap.fromList [(getNodeId binderA, bogusTarget)]
                            , etCopyMap = mempty
                            }
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness [OpWeaken binderA]
                        }
                case Elab.phiFromEdgeWitnessWithTrace defaultTraceConfig generalizeAtWith solved Nothing (Just si) (Just tr) ew of
                    Left (Elab.PhiInvariantError msg) ->
                        msg `shouldSatisfy` ("replay-map target outside replay binder domain" `isInfixOf`)
                    Left err ->
                        expectationFailure ("Expected PhiInvariantError for codomain, got " ++ show err)
                    Right inst ->
                        expectationFailure ("Expected fail-fast codomain error, got " ++ Elab.pretty inst)
```

**Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct 2>&1 | grep -A 5 "codomain target"`
Expected: FAIL — the current bridge silently projects `bogusTarget` through the fallback tiers instead of rejecting it.

---

### Task 2: Replace `projectReplayTarget` with strict pass-through validation

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs:455-582`

**Step 1: Remove dead helper bindings**

In `computeTraceBinderReplayBridge`, inside the `Just tr ->` branch (lines 460-582), delete these bindings that only exist to support the fallback tiers:

```haskell
-- DELETE these bindings (lines 480-499):
                    sourceNameByKey =
                        IntMap.fromList (IntMap.toList (siSubst siSource))
                    replayKeyByName =
                        IntMap.foldlWithKey'
                            (\acc key name -> Map.insert name key acc)
                            Map.empty
                            (siSubst siReplay)
                    traceCopyMap = getCopyMapping (etCopyMap tr)
                    ib = mkIdentityBridge res (Just tr) traceCopyMap
                    replayKeysInTraceOrder =
                        sortOn (traceOrderRank ib) (IntMap.keys (siSubst siReplay))
                    positionalReplayMap =
                        IntMap.fromList
                            [ (sourceKey, NodeId replayKey)
                            | (sourceKey, replayKey) <- zip traceBinderSourceKeys replayKeysInTraceOrder
                            ]
                    defaultReplayTarget =
                        case replayKeysInTraceOrder of
                            replayKey : _ -> Just (NodeId replayKey)
                            [] -> Nothing
```

These are only consumed by `projectReplayTarget` which we are removing.

**Step 2: Replace `projectReplayTarget` + `projectOne` with strict validation**

Replace lines 537-582 (the `else` branch after domain parity check) with:

```haskell
                    else
                        let validateTarget sourceKey = do
                                replayTargetRaw <-
                                    case IntMap.lookup sourceKey replayMapRaw of
                                        Just t -> Right t
                                        Nothing ->
                                            Left $
                                                PhiInvariantError $
                                                    unlines
                                                        [ "trace binder replay-map missing source key after domain validation"
                                                        , "edge: " ++ show (ewEdgeId ew)
                                                        , "source key: " ++ show sourceKey
                                                        ]
                                let rawKey = getNodeId replayTargetRaw
                                unless
                                    (targetInReplayDomainRaw replayTargetRaw
                                        || targetInReplayDomainCanonical replayTargetRaw)
                                    (Left $
                                        PhiInvariantError $
                                            unlines
                                                [ "replay-map target outside replay binder domain"
                                                , "edge: " ++ show (ewEdgeId ew)
                                                , "source key: " ++ show sourceKey
                                                , "replay target: " ++ show replayTargetRaw
                                                , "target canonical key: " ++ show (canonicalNode replayTargetRaw)
                                                , "replay binder domain: " ++ show (IntSet.toList replayBinderDomainRaw)
                                                ])
                                pure (sourceKey, replayTargetRaw)
                        in case mapM validateTarget traceBinderSourceKeys of
                            Left err -> Left err
                            Right replayEntries ->
                                Right
                                    ( traceBinderSourceSet
                                    , IntMap.fromList replayEntries
                                    , replayMapDomain
                                    )
```

Key differences from old code:
- No `projectReplayTarget` — no name/positional/default fallback tiers.
- Each target is validated for domain membership (raw or canonical) and passed through unchanged.
- `unless` import may be needed — check if `Control.Monad (unless)` is already imported.

**Step 3: Check for newly-dead imports**

After removing the helper bindings, check if these imports are still needed:
- `Data.Map.Strict` (`Map.insert`, `Map.lookup`) — only if used elsewhere in the function
- `MLF.Elab.Phi.IdentityBridge` (`mkIdentityBridge`, `traceOrderRank`) — only if used elsewhere
- `Data.List (sortOn)` — only if used elsewhere

Remove any that are now dead. Build must stay warning-free (`-Wall`).

**Step 4: Build to verify compilation**

Run: `cabal build all 2>&1 | tail -20`
Expected: Clean build, no warnings.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/Translate.hs
git commit -m "Remove runtime fallback projection from replay bridge

Replace five-tier projectReplayTarget with strict validate-and-pass-through.
Bridge now validates domain parity and codomain membership, then returns
the replay map unchanged. Dead helper bindings removed."
```

---

### Task 3: Run new codomain test to verify it passes

**Files:**
- Test: `test/ElaborationSpec.hs`

**Step 1: Run the new codomain test**

Run: `cabal test --test-show-details=direct 2>&1 | grep -A 5 "codomain target"`
Expected: PASS — the strict bridge now rejects `bogusTarget` with `PhiInvariantError` containing "replay-map target outside replay binder domain".

**Step 2: Run the existing domain-mismatch test**

Run: `cabal test --test-show-details=direct 2>&1 | grep -A 5 "domain mismatch"`
Expected: PASS — domain parity validation is unchanged.

---

### Task 4: Run full test suite

**Files:**
- All test files

**Step 1: Run full suite**

Run: `cabal build all && cabal test --test-show-details=direct`
Expected: All tests pass (767+ tests). No warnings.

If any tests fail, investigate:
- Tests that relied on the fallback projection to silently remap a target will now fail fast. These are false negatives from the old behavior — update the test fixture's `etBinderReplayMap` to provide the correct target directly, or update the assertion to expect `PhiInvariantError`.
- Do NOT re-introduce fallback logic to fix failures.

**Step 2: Commit test**

```bash
git add test/ElaborationSpec.hs
git commit -m "Add replay-map codomain validation test for strict bridge"
```

---

### Task 5: Verify search invariant — no runtime projection in bridge

**Step 1: Grep for removed patterns**

Run: `grep -n 'sourceNameByKey\|replayKeyByName\|positionalReplayMap\|defaultReplayTarget\|replayKeysInTraceOrder' src/MLF/Elab/Phi/Translate.hs`
Expected: No matches. All five helper bindings are gone.

Run: `grep -n 'projectReplayTarget\|projectOne' src/MLF/Elab/Phi/Translate.hs`
Expected: No matches. The fallback projection functions are gone.

**Step 2: Grep for remaining bridge structure**

Run: `grep -n 'validateTarget\|targetInReplayDomain\|missingSources\|extraSources' src/MLF/Elab/Phi/Translate.hs`
Expected: Matches for the validation logic that remains.

---

### Task 6: Sync docs

**Files:**
- Modify: `CHANGELOG.md`
- Modify: `implementation_notes.md` (if it exists and has replay bridge content)

**Step 1: Add CHANGELOG entry**

Add under the current date section:

```markdown
- Phi replay bridge is now strict pass-through: runtime fallback projection removed,
  replay-map codomain validated against replay binder domain, fail-fast on violations.
```

**Step 2: Update implementation_notes.md if applicable**

If `implementation_notes.md` mentions the replay bridge or fallback projection, update it to reflect the new strict behavior. If it doesn't mention the bridge, skip this step.

**Step 3: Commit docs**

```bash
git add CHANGELOG.md implementation_notes.md
git commit -m "docs: sync replay bridge strict pass-through changes"
```

---

## Verification Checklist

- [ ] `grep -rn 'projectReplayTarget\|positionalReplayMap\|defaultReplayTarget\|sourceNameByKey\|replayKeyByName' src/MLF/Elab/Phi/Translate.hs` returns nothing
- [ ] `cabal build all` — clean, no warnings
- [ ] `cabal test --test-show-details=direct` — all green
- [ ] New test "fails fast when replay-map codomain target is outside replay binder domain" passes
- [ ] Existing test "fails fast when replay-map source domain mismatches trace binder sources" still passes
- [ ] CHANGELOG updated
