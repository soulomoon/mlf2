# Thesis-Exact Strict Replay Cutover Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.
> **Design Doc:** [`docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-design.md`](/Volumes/src/mlf4/docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-design.md)

**Goal:** Deliver one atomic strict cutover that makes presolution the sole owner of replay-target normalization and makes Phi/Omega hard-reject malformed replay targets (no runtime repair).

**Architecture:** Implement red-first tests, then replace mixed-domain replay-map synthesis with deterministic producer normalization from active source binders to replay binders, add explicit producer-side replay-domain validation, remove runtime target projection in `computeTraceBinderReplayBridge`, and restore strict `OpRaise` fail-fast for unresolved trace-source targets. Finish with docs + full gate verification in one green change.

**Tech Stack:** Haskell (`base`, `containers`), Cabal, Hspec, ripgrep, existing MLF presolution/elaboration pipeline.

---

### Task 1: Prepare isolated execution context (@using-git-worktrees)

**Files:**
- Create: `/Volumes/src/mlf4` (no source edits in this task)

**Step 1: Create dedicated worktree branch**

Run:

```bash
cd /Volumes/src/mlf4
git worktree add ../mlf4-strict-replay-cutover -b codex/strict-replay-cutover
```

Expected: new worktree at `../mlf4-strict-replay-cutover`.

**Step 2: Capture baseline targeted status before edits**

Run:

```bash
cd /Volumes/src/mlf4-strict-replay-cutover
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi|IdentityBridge|Witness|OpWeaken|OpRaise|MissingEdgeTrace|A6|BUG-002|BUG-003"'
```

Expected: baseline snapshot recorded for comparison after cutover.

### Task 2: Tighten existing strictness predicates first (red) (@test-driven-development)

**Files:**
- Modify: `/Volumes/src/mlf4/test/ThesisFixDirectionSpec.hs:15-31`
- Modify: `/Volumes/src/mlf4/test/ReduceSpec.hs:185-197`
- Modify: `/Volumes/src/mlf4/test/TypeCheckSpec.hs:188-200`

**Step 1: Replace broadened fail-fast predicate in ThesisFixDirectionSpec**

Use this exact shape:

```haskell
isStrictPhiFailFast :: String -> Bool
isStrictPhiFailFast msg =
    "OpWeaken: unresolved non-root binder target" `isInfixOf` msg
```

**Step 2: Replace broadened fail-fast predicate in ReduceSpec**

Use this exact shape:

```haskell
isStrictPhiFailFast :: String -> Bool
isStrictPhiFailFast msg =
    "OpWeaken: unresolved non-root binder target" `isInfixOf` msg
```

**Step 3: Replace broadened fail-fast predicate in TypeCheckSpec**

Use this exact shape:

```haskell
isStrictPhiFailFast :: String -> Bool
isStrictPhiFailFast msg =
    "OpWeaken: unresolved non-root binder target" `isInfixOf` msg
```

**Step 4: Run only tightened strictness tests and confirm red state**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "thesis target|dual annotated coercion consumers fail fast"'
```

Expected: at least one failure before implementation change (current runtime projection path still reachable).

**Step 5: Commit red tests**

```bash
git add /Volumes/src/mlf4/test/ThesisFixDirectionSpec.hs /Volumes/src/mlf4/test/ReduceSpec.hs /Volumes/src/mlf4/test/TypeCheckSpec.hs
git commit -m "test: tighten strict OpWeaken fail-fast predicate to unresolved-target class only"
```

### Task 3: Add new red regressions for producer contract + runtime hard reject (@test-driven-development)

**Files:**
- Modify: `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs` (add tests near replay-map validation block around `1628+`)
- Modify: `/Volumes/src/mlf4/test/ElaborationSpec.hs` (add test near replay-map validation block around `1563+`)

**Step 1: Add replay-codomain-in-replay-domain test in WitnessSpec**

Add a test that builds a rooted `TyForall` with known ordered binders and asserts normalized trace codomain is subset of replay binders:

```haskell
it "normalization maps replay codomain to replay binders of edge root only" $ do
    -- Build constraint with root forall and live TyVar binders.
    -- Run normalizeEdgeWitnessesM.
    -- Assert every etBinderReplayMap target is in orderedBinders(root) filtered to TyVar.
```

**Step 2: Add binderArgs/replayMap-domain equality + stale-source pruning test in WitnessSpec**

Add a test that includes stale source binders in input `etBinderArgs`, normalized ops that reference only a subset, and assert:

```haskell
let sourceKeys = IntSet.fromList [getNodeId b | (b, _) <- etBinderArgs tr']
    mapKeys = IntSet.fromList (IntMap.keys (etBinderReplayMap tr'))
sourceKeys `shouldBe` mapKeys
sourceKeys `shouldBe` IntSet.fromList [getNodeId expectedActiveSource]
```

**Step 3: Add malformed source-space replay target hard-fail test in ElaborationSpec**

Add a test with `etBinderReplayMap = IntMap.fromList [(sourceKey, sourceBinder)]` where `sourceBinder` is not in replay quantifier domain and assert:

```haskell
case Elab.phiFromEdgeWitnessWithTrace defaultTraceConfig generalizeAtWith solved Nothing (Just si) (Just tr) ew of
    Left (Elab.PhiInvariantError msg) ->
        msg `shouldSatisfy` ("trace binder replay-map target outside replay binder domain" `isInfixOf`)
    Left err -> expectationFailure ("Expected PhiInvariantError, got " ++ show err)
    Right inst -> expectationFailure ("Expected hard-fail, got " ++ Elab.pretty inst)
```

**Step 4: Run new regression subset to confirm red**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "replay-map validation|normalization maps replay codomain|stale source binders|malformed source-space replay target"'
```

Expected: FAIL before producer/runtime changes.

**Step 5: Commit red regressions**

```bash
git add /Volumes/src/mlf4/test/Presolution/WitnessSpec.hs /Volumes/src/mlf4/test/ElaborationSpec.hs
git commit -m "test: add red regressions for strict replay producer/runtime contracts"
```

### Task 4: Rebuild replay-map producer normalization in WitnessNorm (@haskell-pro @test-driven-development)

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs:75-372`

**Step 1: Replace mixed candidate replay assignment with deterministic source-entry normalization**

Implement:

```haskell
sourceEntriesInOrder :: [(NodeId, NodeId)]
sourceEntriesInOrder =
    reverse $
        snd $
            foldl'
                (\(seen, acc) (sourceBinder, arg) ->
                    let key = getNodeId sourceBinder
                    in if IntSet.member key seen
                        then (seen, acc)
                        else (IntSet.insert key seen, (sourceBinder, arg) : acc)
                )
                (IntSet.empty, [])
                binderArgs0
```

**Step 2: Compute target keys used from normalized ops (`opsNorm`)**

Implement:

```haskell
targetKeysUsed :: IntSet.IntSet
targetKeysUsed =
    IntSet.fromList
        [ getNodeId (canonical n)
        | op <- opsNorm
        , n <- case op of
            OpGraft _ t -> [t]
            OpWeaken t -> [t]
            OpRaise t -> [t]
            OpMerge a b -> [a, b]
            OpRaiseMerge a b -> [a, b]
        ]
```

**Step 3: Define active source domain and replay binder sequence**

Implement:

```haskell
activeSourceEntries =
    if IntSet.null targetKeysUsed
        then sourceEntriesInOrder
        else
            [ entry
            | entry@(sourceBinder, _arg) <- sourceEntriesInOrder
            , IntSet.member (getNodeId (canonical (rewriteNode sourceBinder))) targetKeysUsed
            ]

replayBinders =
    [ canonical b
    | b <- bindersOrdered
    , isExactTyVar (canonical b)
    ]
  where
    bindersOrdered =
        either (const []) id $
            Binding.orderedBinders canonical c0 (typeRef (canonical edgeRoot))
```

**Step 4: Deterministically assign active sources to first N replay binders and hard-fail when short**

Implement:

```haskell
let nActive = length activeSourceEntries
when (length replayBinders < nActive) $
    throwError $
        WitnessNormalizationError (EdgeId eid) $
            Witness.ReplayMapIncomplete (map fst (drop (length replayBinders) activeSourceEntries))

let replayPairs = zip activeSourceEntries (take nActive replayBinders)
    replayMapSourceFinal =
        IntMap.fromList
            [ (getNodeId sourceBinder, replayBinder)
            | ((sourceBinder, _), replayBinder) <- replayPairs
            ]
    activeBinderArgs = map fst (zip activeSourceEntries [1 .. nActive])
```

**Step 5: Update trace output contract fields atomically**

Set:

```haskell
trace' = fmap (\tr -> tr
    { etBinderArgs = activeBinderArgs
    , etBinderReplayMap = replayMapSourceFinal
    }) mbTrace
```

**Step 6: Run presolution witness subset**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 — Witness normalization|replay-map validation|normalization maps replay codomain|stale source binders"'
```

Expected: new producer tests move toward green; any remaining failures are validation/runtime follow-ups.

**Step 7: Commit producer normalization**

```bash
git add /Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs /Volumes/src/mlf4/test/Presolution/WitnessSpec.hs
git commit -m "fix: normalize replay-map from active source domain to replay binder domain"
```

### Task 5: Tighten producer validation in WitnessValidation and Driver (@haskell-pro)

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs:39-116`
- Modify: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs:154-189`

**Step 1: Add replay-domain membership validation in WitnessValidation**

Extend `OmegaNormalizeError` and checks:

```haskell
data OmegaNormalizeError
    = ...
    | ReplayMapTargetOutsideReplayDomain NodeId NodeId

replayBinderDomain =
    IntSet.fromList
        [ getNodeId b
        | b <- replayBindersForRoot
        ]

checkReplayTarget (sourceKey, replayTargetRaw) =
    let replayTarget = canon replayTargetRaw
    in if not (IntSet.member (getNodeId replayTarget) replayBinderDomain)
        then Left (ReplayMapTargetOutsideReplayDomain (NodeId sourceKey) replayTarget)
        else if isLiveTyVar replayTarget
            then Right ()
            else Left (ReplayMapNonTyVarTarget (NodeId sourceKey) replayTarget)
```

**Step 2: Mirror replay-domain codomain check in Driver with explicit InternalError**

Add in `computePresolution` loop:

```haskell
let replayBinders =
        IntSet.fromList [getNodeId b | b <- replayBindersForTrace tr]
forM_ (IntMap.toList (etBinderReplayMap tr)) $ \(sourceKey, replayTarget) ->
    when (IntSet.notMember (getNodeId (canonical replayTarget)) replayBinders) $
        Left $
            InternalError $
                unlines
                    [ "edge replay-map codomain target outside replay binder domain"
                    , "edge: " ++ show (EdgeId eid)
                    , "source key: " ++ show sourceKey
                    , "replay target: " ++ show replayTarget
                    ]
```

**Step 3: Run validation-focused subset**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "replay-map validation|Witness normalization invariants"'
```

Expected: PASS for replay domain/completeness/injectivity/TyVar contract checks.

**Step 4: Commit validation hardening**

```bash
git add /Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs /Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs /Volumes/src/mlf4/test/Presolution/WitnessSpec.hs
git commit -m "fix: enforce replay-domain membership at presolution validation boundaries"
```

### Task 6: Make Phi replay bridge validation-only pass-through (remove runtime repair) (@haskell-pro)

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs:455-582`

**Step 1: Remove runtime projection helpers and fallback branches**

Delete branches based on:
- source name mapping
- positional replay map
- default replay target

Delete logic rooted at current `projectReplayTarget`.

**Step 2: Derive replay binder domain from scheme quantifier identities first**

Implement:

```haskell
parseBinderId :: String -> Maybe NodeId
parseBinderId ('t':rest) = NodeId <$> readMaybe rest
parseBinderId _ = Nothing

replayBinderDomainRaw =
    let fromScheme =
            case siScheme siReplay of
                Forall binds _ ->
                    IntSet.fromList
                        [ getNodeId n
                        | (name, _mbBound) <- binds
                        , Just n <- [parseBinderId name]
                        ]
    in if IntSet.null fromScheme
        then IntSet.fromList (IntMap.keys (siSubst siReplay))
        else fromScheme
```

**Step 3: Validate every replay target is in replay domain (raw or canonical alias) and pass through unchanged**

Implement:

```haskell
projectOne sourceKey = do
    replayTargetRaw <- ...
    if targetInReplayDomainRaw replayTargetRaw || targetInReplayDomainCanonical replayTargetRaw
        then pure (sourceKey, replayTargetRaw)
        else Left (PhiInvariantError ...)
```

Return:

```haskell
Right (traceBinderSourceSet, IntMap.fromList replayEntries, replayMapDomain)
```

**Step 4: Run Phi bridge strictness tests**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "replay-map source domain mismatches|malformed source-space replay target|MissingEdgeTrace|OpWeaken on an alias target fails fast"'
```

Expected: PASS with hard-fail replay-domain validation and no runtime repair.

**Step 5: Commit bridge cutover**

```bash
git add /Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs /Volumes/src/mlf4/test/ElaborationSpec.hs
git commit -m "fix: remove runtime replay-target repair and enforce strict pass-through bridge"
```

### Task 7: Restore strict OpRaise fail-fast for unresolved trace-source targets (@haskell-pro)

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:764-783`

**Step 1: Reintroduce fail-fast branch only when unresolved target is trace-source**

Update:

```haskell
case nNonBottom <|> nExisting of
    [] | IntSet.member (getNodeId nSource) traceBinderSources ->
            Left $
                PhiInvariantError $
                    unlines
                        [ "trace/replay binder key-space mismatch (OpRaise unresolved trace-source target)"
                        , "op: OpRaise"
                        , "source target: " ++ show nSource
                        , "replay-map domain: " ++ show (IntMap.keys traceBinderReplayMap)
                        ]
       | otherwise ->
            go binderKeys namedSet' vs accum rest lookupBinder
```

**Step 2: Keep non-trace-source no-op behavior unchanged**

Do not change the non-trace-source `[] -> go ...` behavior.

**Step 3: Run focused OpRaise/bug matrix slice**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "OpRaise|A6|BUG-002|BUG-003|MissingEdgeTrace"'
```

Expected: PASS with strict trace-source fail-fast and no bug-matrix regression.

**Step 4: Commit Omega strictness restoration**

```bash
git add /Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs /Volumes/src/mlf4/test/ElaborationSpec.hs
git commit -m "fix: fail fast on unresolved trace-source OpRaise targets"
```

### Task 8: Update docs and tracker artifacts in same atomic change

**Files:**
- Modify: `/Volumes/src/mlf4/implementation_notes.md` (top replay-contract section)
- Modify: `/Volumes/src/mlf4/CHANGELOG.md` (`Unreleased` replay bullets)
- Modify: `/Volumes/src/mlf4/TODO.md` (Task 25 next-items and fallback wording)
- Modify: `/Volumes/src/mlf4/docs/thesis-deviations.yaml` (TA-004 wording, if needed)

**Step 1: Add explicit runtime no-repair policy and producer hard-contract note**

Add a dated note that runtime bridge validates and passes through only; source-space replay targets are hard errors.

**Step 2: Add changelog entry for atomic strict cutover**

Add one concise bullet covering:
- producer normalization to active-source/replay-domain contract,
- runtime repair removal,
- OpRaise unresolved trace-source strict fail-fast restoration,
- tests/docs updated.

**Step 3: Remove/close replay-fallback TODO language**

Edit Task 25 next actions so it no longer describes runtime replay fallback as an active behavior.

**Step 4: Update thesis deviation text only if semantics changed**

If TA-004 text still mentions deterministic runtime fallback behavior, update it to strict hard-reject policy.

**Step 5: Commit docs**

```bash
git add /Volumes/src/mlf4/implementation_notes.md /Volumes/src/mlf4/CHANGELOG.md /Volumes/src/mlf4/TODO.md /Volumes/src/mlf4/docs/thesis-deviations.yaml
git commit -m "docs: record strict replay hard-reject contract and atomic cutover"
```

### Task 9: Run invariant search and targeted verification (@verification-before-completion)

**Files:**
- No source edits expected

**Step 1: Run invariant grep command**

Run:

```bash
rg -n "sourceKeysForNodeWithClassFallback|fallbackBinderCandidates|replayFromAlias|fallbackHint|fallbackRaw|etBinderReplayHints" /Volumes/src/mlf4/src /Volumes/src/mlf4/test
```

Expected: no matches.

**Step 2: Run targeted strict slice command**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi|IdentityBridge|Witness|OpWeaken|OpRaise|MissingEdgeTrace|A6|BUG-002|BUG-003"'
```

Expected: PASS.

### Task 10: Full gate and final handoff checks (@verification-before-completion)

**Files:**
- No source edits expected

**Step 1: Run full build+test gate**

Run:

```bash
cabal build all && cabal test
```

Expected: PASS.

**Step 2: Capture final review-ready state**

Run:

```bash
git status --short
git log --oneline -n 12
```

Expected: clean working tree and clear commit trail for atomic cutover.

**Step 3: Final integration commit (only if uncommitted changes remain)**

```bash
git add -A
git commit -m "fix: atomic strict replay cutover (producer hard-contract + runtime hard-reject)"
```
