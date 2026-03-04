# Elaboration Input Witness-Authoritative Thesis-Exact (Agent Team) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make row `Elaboration input` materially more thesis-exact by removing remaining elaboration-time fallback synthesis so edge translation is witness-authoritative over `χp` (Def. 15.3.12, §15.3.6).

**Architecture:** Keep the production path `χp`-native end-to-end, but retire fallback behavior that reconstructs schemes/instantiations from secondary heuristics (`SchemeFreeVars` fallback, coherence scoring, RHS type-derived fallback, and trace-derived inst synthesis). Preserve existing fail-fast contracts and verify behavior with RED->GREEN guards plus the existing checked-authoritative and dual-path slices.

**Tech Stack:** Haskell (GHC 9.12/Cabal), Hspec, MLF elaboration/presolution pipeline modules.

---

## Scope and Thesis Anchors

- Thesis references:
  - `papers/these-finale-english.txt` Def. 15.3.12 (translation of instantiation edge from chosen propagation witness)
  - `papers/these-finale-english.txt` §15.3.6 (elaboration from translatable presolution)
- Row target:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md` row `Elaboration input`
- Repo strict criterion remains in force:
  - `thesis exact includes test-only code paths`

## Re-audit Gaps This Plan Targets

1. `src/MLF/Elab/Elaborate.hs:122-131`
- `scopeRootFromBase` swallows binding-tree failure (`Left _ -> typeRef root`) instead of explicit failure.

2. `src/MLF/Elab/Elaborate.hs:143-187` and `:170-179`
- `generalizeAtWith` and `generalizeAtNode` still include fallback ladders (`SchemeFreeVars` fallback to `Nothing` ga and fallback reify) plus coherence scoring.

3. `src/MLF/Elab/Elaborate.hs:556-713`
- Let-elaboration still uses heuristic fallback selection (`fallbackChoiceFromApp` / `fallbackChoiceFromLam` / `fallbackChoiceFromVar`) derived from RHS typing/coherence.

4. `src/MLF/Elab/Elaborate.hs:866-953`
- `reifyInst` still has fallback branch that synthesizes `InstApp` from trace/expansion when `phi` returns `InstId`.

5. `src/MLF/Elab/Run/Pipeline.hs:148-163`
- Root generalization still falls back on `SchemeFreeVars` by retrying with `Nothing` ga and then reifying a scheme directly.

## Agent Team Topology

- Team A (`contracts-red`): Add RED guards for residual fallback surfaces.
- Team B (`pipeline-root-strict`): Remove root-scheme fallback ladder in pipeline.
- Team C (`strictness-regressions`): Add/adjust strict fail-fast regression tests and matcher wiring.
- Team D (`elaborate-witness-authoritative`): Retire fallback logic in `MLF.Elab.Elaborate` and keep witness-authoritative translation.
- Team E (`integration-docs`): Integrate commits, run required gates, update table/docs/ledgers.

Wave order:
1. Wave 0: Team A only (must fail RED)
2. Wave 1: Teams B + C + D in parallel (strict file ownership)
3. Wave 2: Team E integration + GREEN gates
4. Wave 3: Team E docs/ledger closeout (post-green only)

## Task 1: Team A - RED guards for witness-authoritative contract

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Add source guards that should fail on current baseline**

```haskell
it "elab-input witness-authoritative guard" $ do
  elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
  runSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
  forM_
    [ "generalizeNeedsFallback"
    , "generalizeAtWith Nothing scopeRoot targetNode"
    , "preferMoreCoherent"
    , "fallbackChoiceFromApp"
    , "instFromTrace <- case (allowFallbackFromTrace, phi, mExpansion, mSchemeInfo) of"
    ] $ \marker ->
      isInfixOf marker elabSrc `shouldBe` False
  isInfixOf "generalizeAtWithView Nothing rootScope rootTarget" runSrc `shouldBe` False
```

**Step 2: Run RED matcher**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative guard"'`
Expected: FAIL with non-empty evidence.

**Step 3: Commit**

```bash
git add test/PipelineSpec.hs
git commit -m "test: add RED guard for witness-authoritative elaboration input"
```

## Task 2: Team B - Remove root generalization fallback ladder

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Test: `test/PipelineSpec.hs`

**Step 1: Replace fallback ladder with strict generalize call**

```haskell
(rootScheme, rootSubst) <- fromElabError $
  generalizeAtWithView (Just bindParentsGa) rootScope rootTarget
```

- Remove `generalizeNeedsFallback` and the retry path (`Nothing` ga + `reifyType` fallback) in the root generalization block.

**Step 2: Preserve checked-authoritative output path**

- Keep `typeCheck termClosed` authoritative behavior unchanged.
- Do not alter result-type diagnostics path in this task.

**Step 3: Run focused checks**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs
git commit -m "refactor: remove root generalization fallback ladder in pipeline"
```

## Task 3: Team C - Add strict regression coverage (fail-fast semantics)

**Files:**
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/Main.hs` (only if a new spec module is introduced)
- Modify: `mlf2.cabal` (only if a new spec module is introduced)

**Step 1: Add strictness regression tests (no fallback synthesis contract)**

- Add focused tests that assert the elaboration-input path does not silently recover via legacy fallback synthesis when invariant prerequisites are violated.
- Prefer explicit fail-fast assertions (`shouldSatisfy isLeft`) around malformed edge/scope fixtures where prior fallback would mask errors.

**Step 2: Add source guard slice in ElaborationSpec (if needed)**

```haskell
it "elab-input witness-authoritative source guard (elaborate core)" $ do
  src <- readFile "src/MLF/Elab/Elaborate.hs"
  src `shouldSatisfy` (not . isInfixOf "fallbackChoiceFromApp")
  src `shouldSatisfy` (not . isInfixOf "fallbackChoiceFromLam")
  src `shouldSatisfy` (not . isInfixOf "fallbackChoiceFromVar")
```

**Step 3: Run focused suite**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative|MissingEdgeTrace|Phase 6"'`
Expected: PASS with non-empty evidence.

**Step 4: Commit**

```bash
git add test/ElaborationSpec.hs test/Main.hs mlf2.cabal
git commit -m "test: add strict fail-fast coverage for witness-authoritative elaboration input"
```

## Task 4: Team D - Retire fallback logic in Elaborate core

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Make scope-root resolution explicit-failure instead of swallow-fallback**

- Replace `scopeRootFromBase` fallback `Left _ -> typeRef root` with error-propagating flow.
- Thread `Either ElabError` through `scopeRootForNode` / `generalizeAtNode` where needed.

**Step 2: Remove generalization fallback/coherence ladder in Elaborate**

- Delete local `generalizeNeedsFallback` path in Elaborate.
- Delete `preferMoreCoherent` and `generalizeAtWith Nothing ...` retries.
- Use `ecGeneralizeAtWith` result directly for elaboration-node generalization.

**Step 3: Remove let-level heuristic scheme fallback chooser**

- Retire `fallbackChoiceFromApp` / `fallbackChoiceFromLam` / `fallbackChoiceFromVar` and related coherence-selection logic.
- Keep minimal normalization (`normalizeSchemeSubstPair`) only for canonicalized output shape, not for fallback-driven substitution.

**Step 4: Make edge instantiation witness-authoritative**

- Remove `instFromTrace` fallback branch in `reifyInst`.
- Return translated `phi` from `phiFromEdgeWitnessWithTrace` directly.
- Keep existing fail-fast behavior for missing/inconsistent trace/witness inputs.

**Step 5: Run focused slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative guard"'`
Expected: PASS.

**Step 6: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs
git commit -m "refactor: make elaboration input witness-authoritative and fallback-free"
```

## Task 5: Team E - Integrate and drive gates GREEN

**Files:**
- Integration changes only unless conflict resolution is required.

**Step 1: Cherry-pick Team B/C/D on top of Team A RED commit**

Run: `git cherry-pick <teamB> <teamC> <teamD>`
Expected: clean apply or resolved conflicts.

**Step 2: Verify row-specific GREEN guard**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input witness-authoritative guard"'`
Expected: PASS with non-empty evidence.

**Step 3: Re-run required non-regression slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
Expected: PASS.

**Step 4: Full gate**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 5: Commit integration**

```bash
git add -A
git commit -m "refactor: close elaboration-input witness-authoritative strictness gap"
```

## Task 6: Team E - Docs and ledger closeout (only after all gates pass)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `Bugs.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/task_plan.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/findings.md`
- Modify: `tasks/todo/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/progress.md`

**Step 1: Update row narrative with current evidence**

- If all strict guards and full gate pass: row may be promoted to `Yes` with current code references.
- If any residual fallback remains by design: keep row `No` and document residual rationale explicitly.

**Step 2: Update bug tracker**

- Add discovered bug entry if Wave 1 introduces regressions.
- Move to resolved section only after green full gate.

**Step 3: Record exact verification counts**

- Paste matcher counts and full-gate counts into docs.

**Step 4: Commit docs closeout**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md Bugs.md tasks/todo/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/*
git commit -m "docs: record witness-authoritative elaboration-input closeout evidence"
```

## Controller Execution Playbook

1. Enforce strict file ownership for each team; no cross-team edits in Wave 1.
2. Require Team A RED proof before accepting any implementation commit.
3. Dispatch Team B/C/D in parallel.
4. Team E integrates and runs gate sequence in exact order.
5. Docs/ledger changes only after all runtime gates and full gate are green.

## Exit Criteria

- No elaboration-input fallback synthesis markers remain in active runtime/tested code path:
  - no `SchemeFreeVars` recovery ladder in `Elaborate`/`Pipeline` elaboration input path,
  - no let-level fallback chooser,
  - no `reifyInst` fallback synthesis from trace/expansion when `phi` is `InstId`.
- `elab-input witness-authoritative guard` is green with non-empty evidence.
- Existing row-level safety slices remain green:
  - `elab-input absolute thesis-exact guard`
  - `checked-authoritative`
  - `Dual-path verification`
- Full gate `cabal build all && cabal test` is green.
- TMT row narrative and repo ledgers reflect the actual post-change contract.
