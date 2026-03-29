# Round 151 — Plan

## Item

**item-1**: Reclassify nested-forall μ absorption as known correct behavior

## Scope

Documentation-only. No production code changes. No test assertion changes.
Only test description strings and `implementation_notes.md` prose are modified.

---

## Steps

### Step 1: Rename test description at `P5ClearBoundarySpec.hs:73`

**File:** `test/Research/P5ClearBoundarySpec.hs`
**Line:** 73

**Old text:**
```haskell
        it "fails closed once the same wrapper crosses a nested forall boundary" $ do
```

**New text:**
```haskell
        it "correctly absorbs μ when polymorphic mediation crosses a nested forall boundary" $ do
```

**Rationale:** The current "fails closed" framing implies a deficiency. The test
asserts `containsMu fallbackTy == False`, which is correct: polymorphic mediation
via `id` legitimately absorbs the μ wrapper during constraint solving. The new
name describes the actual behavior — correct absorption, not failure.

### Step 2: Rename test description at `P5ClearBoundarySpec.hs:91`

**File:** `test/Research/P5ClearBoundarySpec.hs`
**Line:** 91

**Old text:**
```haskell
        it "hits the same authoritative instantiation-translation blocker on both current pipeline entrypoints once the wrapper crosses a nested forall boundary" $ do
```

**New text:**
```haskell
        it "reports PhiTranslatabilityError at pipeline entrypoints as a downstream consequence of correct non-recursive nested-forall outcome" $ do
```

**Rationale:** The current "blocker" framing implies this is an independent defect.
In reality, since μ is correctly absorbed (Step 1), the result type is
non-recursive, and the PhiTranslatabilityError is a downstream consequence of
that correct outcome — not a separate "blocker." The new name reflects this
causal chain.

### Step 3: Reclassify in `implementation_notes.md:25-27`

**File:** `implementation_notes.md`
**Lines:** 25–27

**Old text:**
```markdown
**Known remaining limitations:**
- Nested-forall-mediated recursive types: μ is absorbed during constraint solving through polymorphic mediation (e.g., `let rec f = id f`). In these cases, the result-type fallback correctly reports no μ-type because constraint solving legitimately removed the recursive wrapper through polymorphic generalization.
- Non-local proxy at pipeline entrypoints: the result-type fallback now correctly returns μ-types for non-local reconstruction, but the pipeline entrypoint still fails with `PhiTranslatabilityError` at the elaboration level. This is a separate elaboration-layer issue, not a result-type fallback defect.
```

**New text:**
```markdown
**Known correct behavior under polymorphic mediation:**
- Nested-forall-mediated recursive types: μ is absorbed during constraint solving through polymorphic mediation (e.g., `let rec f = id f`). In these cases, the result-type fallback correctly reports no μ-type because constraint solving legitimately removed the recursive wrapper through polymorphic generalization. This is not a limitation — it is the expected outcome when a polymorphic mediator generalizes away the recursive structure.

**Known remaining limitation:**
- Non-local proxy at pipeline entrypoints: the result-type fallback now correctly returns μ-types for non-local reconstruction, but the pipeline entrypoint still fails with `PhiTranslatabilityError` at the elaboration level. This is a separate elaboration-layer issue, not a result-type fallback defect.
```

**Rationale:** The nested-forall bullet is reclassified from a "limitation" to
"known correct behavior under polymorphic mediation." The non-local proxy bullet
remains under a (now singular) "Known remaining limitation" heading. The factual
content of both bullets is preserved; only the framing changes.

---

## Verification

### Baseline checks (from verification contract)

1. `git diff --check` — no whitespace or conflict-marker damage
2. `python3 -m json.tool orchestrator/state.json >/dev/null` — state.json valid
3. Roadmap bundle resolves:
   ```
   roadmap_dir="$(python3 -c "import json; print(json.load(open('orchestrator/state.json'))['roadmap_dir'])")" \
     && test -f "$roadmap_dir/roadmap.md" \
     && test -f "$roadmap_dir/retry-subloop.md" \
     && test -f "$roadmap_dir/verification.md"
   ```
4. `cabal build all && cabal test` — full repo gate passes

### Task-specific checks (item-1)

5. Verify `test/Research/P5ClearBoundarySpec.hs:73` description says
   "correctly absorbs μ when polymorphic mediation crosses a nested forall boundary"
6. Verify `test/Research/P5ClearBoundarySpec.hs:91` description says
   "reports PhiTranslatabilityError at pipeline entrypoints as a downstream
   consequence of correct non-recursive nested-forall outcome"
7. Verify `implementation_notes.md` has a "Known correct behavior under
   polymorphic mediation" section separate from "Known remaining limitation"
8. Verify test assertions are unchanged:
   - Line 75: `containsMu fallbackTy \`shouldBe\` False` (unchanged)
   - Lines 92–115: `expectedSnippets` and `expectStrictPipelineFailure`-style
     assertions (unchanged)
9. Verify no production code files are modified (`src/`, `src-public/`, `app/`)
10. `cabal test --test-option='-m' --test-option='P5 clear-boundary'` — the
    renamed tests still pass with the new descriptions

### Files modified (exhaustive list)

| File | Change type |
|------|-------------|
| `test/Research/P5ClearBoundarySpec.hs` | Test description strings only (lines 73, 91) |
| `implementation_notes.md` | Prose reclassification only (lines 25–27) |

No other files are modified by the implementer for this item.

---

## Explicit constraints

- **No code changes.** Only string literals in test descriptions and markdown prose.
- **Test assertions unchanged.** `containsMu == False` at line 75 remains correct.
  The `expectedSnippets` and error-checking logic at lines 92–115 remain identical.
- **No new tests.** Existing tests cover the behavior; only their descriptions change.
- **No cabal changes.** No modules added, removed, or renamed.
