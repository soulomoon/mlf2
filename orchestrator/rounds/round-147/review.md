# Round 147 — Review: Phase 6 Fix Alias-Bound Resolution for Recursive Types

**Reviewer decision: APPROVED**

## Baseline Checks

### 1. Whitespace / conflict-marker check

```
$ git -C .../worktrees/round-147 diff --check codex/automatic-recursive-type-inference..HEAD
(no output — clean)
```

**PASS**

### 2. state.json valid JSON

```
$ python3 -m json.tool orchestrator/state.json >/dev/null
PASS: valid JSON
```

**PASS**

### 3. Roadmap bundle resolves

```
$ roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" \
  && test -f "$roadmap_dir/roadmap.md" \
  && test -f "$roadmap_dir/retry-subloop.md" \
  && test -f "$roadmap_dir/verification.md"
PASS: roadmap bundle resolves
```

**PASS**

### 4. Full build + test gate

```
$ cabal build all && cabal test
Build profile: -w ghc-9.12.2 -O1
...
1175 examples, 0 failures
Test suite mlf2-test: PASS
```

**PASS** — 1175 examples, 0 failures. Zero regressions.

## Item-2 Specific Checks

### 1. `bindingFor` in ReifyPlan.hs handles inter-binder TVar bounds

**PASS** — Line 322 adds `binderNameSet = Set.fromList (IntMap.elems subst)`.
Lines 628–643 update `mbBoundTyped`:
- `Just (TVar v) | v `Set.member` binderNameSet -> Right Nothing` (normalize to unbounded)
- `| otherwise -> Left $ ValidationFailed [...]` (preserve hard-stop for non-binder TVars)

This is the exact pattern from the plan. The implementer used `IntMap.elems subst` instead of the plan's suggested `[n | (n, _) <- bindings]`, which is semantically equivalent since `subst` maps node IDs to binder names.

### 2. μ/∀ interaction test no longer asserts "alias bounds survived"

**PASS** — `PipelineSpec.hs` changes replace `rendered `shouldSatisfy` isInfixOf "alias bounds survived scheme finalization"` with `rendered `shouldSatisfy` (not . isInfixOf "alias bounds survived scheme finalization")`. The test now verifies the old error is gone.

```
$ cabal test --test-option='-m' --test-option='interaction'
characterizes μ/∀ interaction as recursive-at-constraint level with current Phase-6 fail-closed behavior [✔]
1 example, 0 failures
```

### 3. Higher-order recursion test no longer asserts "alias bounds survived"

**PASS** — Same pattern as above. The test now verifies the alias-bounds error is gone.

```
$ cabal test --test-option='-m' --test-option='higher-order recursion'
characterizes higher-order recursion as recursive-at-constraint level with current Phase-6 fail-closed behavior [✔]
1 example, 0 failures
```

### 4. Test count stable

**PASS** — 1175 examples, 0 failures. Same count as baseline, no regressions.

### 5. Note block added

**PASS** — Lines 648–666 contain `{- Note [Inter-binder alias bounds in recursive types] -}` with explanation covering:
- Why TVar bounds arise for recursive types
- Why normalizing to unbounded is safe (over-approximation)
- Reference to thesis §5 (graphic constraints)
- Constraint graph artefact explanation

### 6. ElaborationSpec.hs change (additional, within scope)

**PASS** — `generalizeAt rejects alias bounds (no ∀(b ⩾ a))` renamed to `generalizeAt normalizes inter-binder alias bounds to unbounded (no ∀(b ⩾ a))`. Test now expects `Right` (success) instead of `Left` with "alias bounds survived". This is a direct consequence of the behavior change and is within scope.

## Plan Conformance

| Plan Step | Status | Notes |
|-----------|--------|-------|
| Step 1: `binderNameSet` guard + conditional normalization | **Conforms** | Uses `IntMap.elems subst` (equivalent to plan's `bindings` approach) |
| Step 2: Note block | **Conforms** | Present at lines 648–666 |
| Step 3: μ/∀ interaction test upgraded | **Conforms with adaptation** | Test verifies alias-bounds error is gone; tolerates downstream TCRollBodyMismatch (Phase 7) |
| Step 4: Higher-order recursion test upgraded | **Conforms with adaptation** | Test verifies alias-bounds error is gone; tolerates PhiTranslatabilityError (Phase 6) |
| Step 5: Build+test passes | **Conforms** | 1175 examples, 0 failures |
| Step 6: implementation-notes.md | **Conforms** | Present in commit, documents fix, test changes, and remaining downstream blockers |

### Test adaptation note

The plan suggested tests assert full pipeline success with `typeCheck term `shouldBe` Right ty`. The implementation instead characterizes the new failure modes:
- μ/∀ interaction: Phase 6 now passes, but Phase 7 hits `TCRollBodyMismatch` (separate recursive roll/unroll issue)
- Higher-order recursion: Phase 6 now passes the alias-bounds check, but hits `PhiTranslatabilityError` (separate Phi translation coverage gap)

This is a legitimate adaptation: the alias-bounds fix unblocks Phase 6, but downstream blockers (items 3/4 on the roadmap) still exist. The tests correctly verify that the **specific** Phase 6 alias-bounds error is gone while characterizing the new failure surface. The implementation-notes.md documents both remaining blockers clearly.

### Additional ElaborationSpec.hs change

The implementer also modified `test/ElaborationSpec.hs` to update a unit test that directly tested alias-bound rejection. This change is within scope — the behavior being tested changed, so the test expectation must change too. The test now expects success (unbounded normalization) instead of rejection.

## Evidence Summary

- **Commit**: `eb170ef` on branch `orchestrator/round-147-alias-bounds-fix`
- **Files changed**: 4 (ReifyPlan.hs, PipelineSpec.hs, ElaborationSpec.hs, implementation-notes.md)
- **Build**: Clean, zero warnings relevant to the change
- **Tests**: 1175 examples, 0 failures — no regressions
- **Specific tests**: Both μ/∀ interaction and higher-order recursion tests pass
- **Code quality**: Note block present, minimal diff, guard is correctly scoped

## Decision

**APPROVED**

The implementation correctly resolves the Phase 6 alias-bounds rejection for recursive types. The fix is minimal, well-guarded (only inter-binder TVars are normalized), properly documented with a Note block, and the test suite shows zero regressions. The test adaptations are reasonable given the documented downstream blockers.
