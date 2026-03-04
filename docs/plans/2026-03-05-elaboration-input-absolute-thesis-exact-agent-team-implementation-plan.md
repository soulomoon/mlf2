# Elaboration Input Absolute Thesis-Exact (Agent Team) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the `Elaboration input` mechanism more thesis-exact under strict all-path interpretation by removing residual solved/env indirection, removing scope-error swallowing in ga' selection, and retiring synthetic test-only trace shortcuts.

**Architecture:** Keep runtime behavior single-path and chi-native, but tighten internal contracts so Phi and scope resolution consume presolution-domain evidence directly and fail explicitly on invalid binding-state assumptions. Drive changes with RED->GREEN guardrails, then parallelize independent refactors across agent teams with strict file ownership.

**Tech Stack:** Haskell (GHC/Cabal), Hspec, existing MLF elaboration/presolution pipeline modules.

---

## Agent Team Topology

- Team A (`guards-red`): Add failing absolute-contract guards and protect against regression.
- Team B (`phi-env-chi-only`): Remove residual `Solved` handle surfaces from Phi environment/facade/translate helpers.
- Team C (`scope-strictness`): Remove ga' scope error swallowing and make failure explicit.
- Team D (`trace-fixture-hardening`): Remove synthetic auto-trace shortcut from test-only Phi helper and migrate affected tests.
- Team E (`verification-docs`): Integrate, run required gates, and update row/docs/ledger.

Wave order:
1. Wave 0: Team A only (intentionally RED baseline)
2. Wave 1: Teams B + C + D in parallel (disjoint file ownership)
3. Wave 2: Team E integration + verification + docs

## Task 1: Team A - Add RED guard for absolute row-1 contract

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Add failing guard test for residual non-thesis surfaces**

```haskell
it "elab-input absolute thesis-exact guard" $ do
  phiEnvSrc <- readFile "src/MLF/Elab/Phi/Env.hs"
  scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
  phiTestOnlySrc <- readFile "src/MLF/Elab/Phi/TestOnly.hs"
  isInfixOf "peResult :: Solved" phiEnvSrc `shouldBe` False
  isInfixOf "askResult ::" phiEnvSrc `shouldBe` False
  isInfixOf "Left _ -> ref" scopeSrc `shouldBe` False
  isInfixOf "phiFromEdgeWitnessAutoTrace" phiTestOnlySrc `shouldBe` False
```

**Step 2: Run guard to confirm RED baseline**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
Expected: FAIL (at least one assertion fails before Teams B/C/D)

**Step 3: Commit guard baseline**

```bash
git add test/PipelineSpec.hs
git commit -m "test: add RED guard for absolute elaboration-input thesis contract"
```

## Task 2: Team B - Remove solved-backed Phi environment surfaces

**Files:**
- Modify: `src/MLF/Elab/Phi/Env.hs`
- Modify: `src/MLF/Elab/Phi.hs`
- Modify: `src/MLF/Elab/Phi/Translate.hs`

**Step 1: Remove solved-handle fields and accessors from Phi environment**

```haskell
data PhiEnv = PhiEnv
  { peCanonical :: NodeId -> NodeId
  , peCopyMap :: IntMap.IntMap NodeId
  , peInvCopyMap :: IntMap.IntMap NodeId
  , peGaParents :: Maybe GaBindParents
  , peTrace :: Maybe EdgeTrace
  }
```

**Step 2: Remove solved-backed helper plumbing in facade/translate**

```haskell
-- remove exports/imports of askResult/remapSchemeInfoM
module MLF.Elab.Phi ( ... ) where
```

**Step 3: Ensure remap logic stays presolution-driven only**

```haskell
remapSchemeInfoByTrace :: PresolutionView -> EdgeTrace -> SchemeInfo -> SchemeInfo
```

**Step 4: Compile + targeted tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6|Phi|elab-input"'`
Expected: PASS for selected slice

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/Env.hs src/MLF/Elab/Phi.hs src/MLF/Elab/Phi/Translate.hs
git commit -m "refactor: remove solved-backed Phi environment surfaces"
```

## Task 3: Team C - Make ga' scope selection fail explicitly

**Files:**
- Modify: `src/MLF/Elab/Run/Scope.hs`
- Create: `test/ScopeSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Change scope preference helper to preserve binding errors**

```haskell
preferGenScope :: Constraint -> NodeRef -> Either BindingError NodeRef
```

**Step 2: Thread explicit error propagation into canonical scope resolution**

```haskell
resolveCanonicalScopeView ... = do
  scope0 <- bindingScopeRef constraint scopeRoot
  scopeBase <- preferGenScope constraint scope0
  pure (canonicalizeScopeRefView presolutionView redirects scopeBase)
```

**Step 3: Add focused spec for non-swallowing behavior**

```haskell
it "ga scope resolution does not swallow binding tree errors" $ do
  -- malformed bind-parent graph
  resolveCanonicalScopeView ... `shouldSatisfy` isLeft
```

**Step 4: Register spec and run focused tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ga scope"'`
Expected: PASS

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Scope.hs test/ScopeSpec.hs test/Main.hs mlf2.cabal
git commit -m "refactor: preserve ga scope binding errors in scope selection"
```

## Task 4: Team D - Retire synthetic auto-trace helper path

**Files:**
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Remove `phiFromEdgeWitnessAutoTrace` export + implementation**

```haskell
module MLF.Elab.Phi.TestOnly (
  phiFromEdgeWitnessNoTrace,
  phiFromEdgeWitness,
  ...
) where
```

**Step 2: Update tests to use explicit trace-backed fixtures**

```haskell
phiFromEdgeWitnessWithTrace ... (Just edgeTrace) ew
```

**Step 3: Keep strict no-trace fail-fast contract**

```haskell
phiFromEdgeWitnessNoTrace ... ew = Left (MissingEdgeTrace (ewEdgeId ew))
```

**Step 4: Run focused elaboration tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MissingEdgeTrace|Phase 6|Elaboration"'`
Expected: PASS

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/TestOnly.hs test/ElaborationSpec.hs
git commit -m "refactor: retire synthetic auto-trace test-only Phi helper"
```

## Task 5: Team E - Integrate teams and drive gates GREEN

**Files:**
- Integration only (no feature edits unless conflict resolution required)

**Step 1: Merge Team B/C/D changes on top of Team A RED guard commit**

Run: `git cherry-pick <teamB> <teamC> <teamD>`
Expected: clean apply or resolved conflicts

**Step 2: Re-run absolute guard (must turn GREEN)**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
Expected: PASS

**Step 3: Run required non-regression slices**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
Expected: PASS

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
Expected: PASS

**Step 4: Run full validation gate**

Run: `cabal build all && cabal test`
Expected: PASS (all examples green)

**Step 5: Commit integration**

```bash
git add -A
git commit -m "refactor: harden elaboration input toward absolute thesis-exact contract"
```

## Task 6: Team E - Docs and tracker closeout after green gates only

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `Bugs.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team/task_plan.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team/findings.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team/progress.md`

**Step 1: Update row wording and evidence references**

```markdown
| Elaboration input | ... | ... | ... | Yes (absolute strict all-path contract) |
```

**Step 2: Resolve or replace stale bug-tracker entry for row-1 absolute gap**

```markdown
### BUG-... Status: Resolved
```

**Step 3: Record gate evidence with exact command outputs**

Run: same gate commands from Task 5 and paste PASS counts in docs.

**Step 4: Commit docs closeout**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md Bugs.md TODO.md tasks/todo/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team/*
git commit -m "docs: close out elaboration-input absolute thesis-exact migration"
```

## Controller Playbook (Agent-Team Execution)

1. Spawn Team A and land RED guard commit first.
2. Spawn Teams B/C/D in parallel with strict file ownership and explicit "do not touch other teams' files" prompts.
3. Run controller integration and gates (Team E responsibilities).
4. Only after all gates are green, perform docs closeout and move task folder from `tasks/todo/...` to `tasks/archive/...`.

## Exit Criteria

- No solved-backed Phi env surface remains.
- ga' scope path no longer swallows binding-tree errors.
- No synthetic auto-trace test-only shortcut remains in Phi test helper surface.
- Absolute row-1 guard is green and included in regression cadence.
- `checked-authoritative`, `Dual-path verification`, and full gate are green.
- Table row + docs + bug tracker are synchronized with the implemented contract.
