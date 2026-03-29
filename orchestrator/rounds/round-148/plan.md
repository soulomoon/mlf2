# Round 148 — Plan: Item-3 ELet Recursive-Let (Fixpoint) Reduction Support

## Selected Item

- **Item id:** `item-3`
- **Title:** ELet: Add recursive-let (fixpoint) reduction support

## Roadmap Reference

- **`roadmap_id`:** `2026-03-29-02-iso-recursive-inference-gap-fixes`
- **`roadmap_revision`:** `rev-001`
- **`roadmap_dir`:** `orchestrator/roadmaps/2026-03-29-02-iso-recursive-inference-gap-fixes/rev-001`

## Scope & Constraints

The implementer works in worktree `orchestrator/worktrees/round-148/` on branch
`orchestrator/round-148-elet-fixpoint-reduction`. The implementer must **not**
directly modify `orchestrator/state.json` (controller-owned).

## Root Cause Analysis

### Symptom

`Reduce.hs:33-35` treats `ELet` as non-recursive: when `rhs` is a value,
it substitutes `rhs` for `v` in `body` only. For recursive definitions like
`let f = \x. f x in f`, after substitution the body becomes `\x. f x` where
`f` is now free — causing `TCUnboundVar "f"` during type checking.

### Current Code (lines 33-35)

```haskell
ELet v sch rhs body
    | not (isValue rhs) -> (\rhs' -> ELet v sch rhs' body) <$> step rhs
    | otherwise -> Just (substTermVar v rhs body)
```

The second branch does `substTermVar v rhs body` — substituting `rhs` for `v`
in `body` but NOT in `rhs` itself. Since `rhs` may contain `v` (recursive
binding), `v` remains free.

### Fix Strategy: Self-Substitution via Letrec Unfolding

When `v` occurs free in `rhs` (detected via `freeTermVars`), substitute
`ELet v sch rhs (EVar v)` for each occurrence of `v` in `body`. This is the
standard one-step unfolding for recursive let:

- `let f = \x. f x in f` → `let f = \x. f x in f` (the letrec itself)
- On the next step, the outer letrec reduces again (since `rhs` is a value
  and body is `EVar "f"`): → `\x. (let f = \x. f x in f) x`
- This is now a lambda (value) — `normalize` stops. ✓

For non-recursive lets (where `v` is NOT free in `rhs`), the behavior is
completely unchanged — the existing `substTermVar v rhs body` path runs.

### Termination

`normalize` calls `step` repeatedly until `step` returns `Nothing`. With
recursive lets, each unfolding produces a lambda wrapped around applications of
letrecs, growing the term. BUT: `isValue (ELam ...)` returns `True`, so once
the recursive body is a lambda, `step` on the outer term returns `Nothing` and
`normalize` terminates.

For `let f = \x. f x in f`:
1. `step` detects recursive let, substitutes → `ELet f sch (\x. f x) (EVar "f")`
   Wait — this IS the same term. We need the unfolding to produce `substTermVar v (ELet v sch rhs (EVar v)) body`.

Let me trace more carefully:
- Input: `ELet "f" sch (\x. f x) (EVar "f")`
- `rhs = \x. f x` (value), `"f" ∈ freeTermVars rhs` → recursive
- Substitute `ELet "f" sch (\x. f x) (EVar "f")` for `"f"` in `body = EVar "f"`
- Result: `ELet "f" sch (\x. f x) (EVar "f")` — SAME TERM! Infinite loop!

This approach loops. We need a different strategy.

**Correct approach: substitute `rhs` for `v` in BOTH `body` AND `rhs`.**

Actually the standard letrec reduction is:
`let rec f = rhs in body` → `body[f := rhs[f := let rec f = rhs in f]]`

But this is complex. A simpler approach that works for our case:

**Approach: Substitute `rhs` for `v` in `body`, where `rhs` has been made
self-referential by wrapping recursive occurrences.**

Actually, the simplest correct approach for a call-by-value language:

```
let f = V in body → body[f := V[f := let f = V in f]]
```

This substitutes `V` (with recursive occurrences replaced by the letrec
re-entry) into `body`. Each recursive call re-enters the letrec.

In code:
```haskell
-- For recursive let (v free in rhs):
let selfRef = ELet v sch rhs (EVar v)  -- re-entry point
    rhs' = substTermVar v selfRef rhs  -- rhs with recursive refs → re-entry
in substTermVar v rhs' body            -- body with v → expanded rhs
```

Trace for `let f = \x. f x in f`:
- `selfRef = ELet "f" sch (\x. f x) (EVar "f")`
- `rhs' = substTermVar "f" selfRef (\x. f x) = \x. (ELet "f" sch (\x. f x) (EVar "f")) x`
- `result = substTermVar "f" rhs' (EVar "f") = \x. (ELet "f" sch (\x. f x) (EVar "f")) x`

This is a lambda (value) — `normalize` stops immediately! But when this lambda
is applied to an argument, the letrec inside unfolds again. ✓

For `let f = \x. f (id x) in f` (μ/∀ interaction after Phase 6 fix):
Same pattern — produces a lambda value. ✓

Non-recursive lets: `v ∉ freeTermVars rhs` → existing path unchanged. ✓

## Steps

### Step 1: Add recursive-let reduction to `step`

**File:** `src/MLF/Elab/Reduce.hs`

**Location:** Lines 33-35

**Change:**

```haskell
-- BEFORE:
    ELet v sch rhs body
        | not (isValue rhs) -> (\rhs' -> ELet v sch rhs' body) <$> step rhs
        | otherwise -> Just (substTermVar v rhs body)

-- AFTER:
    ELet v sch rhs body
        | not (isValue rhs) -> (\rhs' -> ELet v sch rhs' body) <$> step rhs
        | v `Set.member` freeTermVars rhs ->
            -- Recursive let: unfold one step.
            -- See Note [Recursive let reduction]
            let selfRef = ELet v sch rhs (EVar v)
                rhs'    = substTermVar v selfRef rhs
            in Just (substTermVar v rhs' body)
        | otherwise -> Just (substTermVar v rhs body)
```

This adds a single guard that checks whether the binding is recursive (variable
free in its own RHS). If so, it performs the one-step letrec unfolding described
above. If not, the existing non-recursive path runs unchanged.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 2: Add Note block

**File:** `src/MLF/Elab/Reduce.hs`

**Location:** After the `step` function (after line 48)

**Change:** Add a GHC-style Note block:

```haskell
{- Note [Recursive let reduction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For recursive bindings (where the bound variable v appears free in rhs),
we cannot simply substitute rhs for v in body because rhs itself contains
v — that occurrence would become free.

Instead we perform a standard one-step letrec unfolding:

  let v = V in body  →  body[v := V[v := let v = V in v]]

where V is the evaluated rhs (a value). This replaces each occurrence of v
in rhs with a "re-entry point" (the original letrec applied to just v),
producing rhs'. Then rhs' is substituted for v in body.

For example:
  let f = \x. f x in f
  → (\x. (let f = \x. f x in f) x)

The result is a lambda (value), so normalize stops. When the lambda is later
applied, the inner letrec unfolds again — giving lazy recursive unfolding
without infinite expansion.

Non-recursive lets (v not free in rhs) use the original direct substitution
path and are completely unaffected. -}
```

### Step 3: Update `freeTermVars` visibility (if needed)

`freeTermVars` is already defined at line 101 in the same module. No changes
needed — it's locally available.

### Step 4: Upgrade the recursive ELet normalization test

**File:** `test/PipelineSpec.hs`

**Location:** Lines 1472-1478

**Current test:**
```haskell
it "normalize produces a value for simple self-recursive elaborated term" $ do
    let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
    case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
      Left err -> expectationFailure (renderPipelineError err)
      Right (term, _ty) -> do
        let nf = normalize term
        isValue nf `shouldBe` True
```

This test should now PASS (currently it likely doesn't because `normalize`
loops or produces a non-value). Verify it passes without changes. If it already
asserts `isValue nf == True`, the fix should make it pass.

### Step 5: Upgrade the type-preservation test

**File:** `test/PipelineSpec.hs`

**Location:** Lines 1480-1504

**Current test:** Tolerates `TCUnboundVar` as a known limitation (line 1496).

**Change:** Remove the `TCUnboundVar` tolerance — after the fix, recursive let
reduction should not leave unbound variables:

```haskell
-- BEFORE (line 1496):
                      Left (TCUnboundVar _) -> pure () -- expected for recursive let step

-- AFTER:
                      Left tcErr ->
                        expectationFailure
                          ( "Type preservation failed after step:\n"
                              ++ "  error: " ++ show tcErr)
```

Wait — we need to be careful. The type-preservation test checks that
`typeCheck(step(term))` matches the original type. After the recursive let
fix, `step` on a recursive let produces a more complex term. The type checker
needs to handle the expanded letrec. If `typeCheck` on the stepped term
produces `TCUnboundVar` for any reason, we should NOT tolerate it anymore.

BUT: the test has a complex structure (lines 1491-1504) with a `checkPreservation`
loop. Let me specify the exact change:

Remove the `Left (TCUnboundVar _) -> pure ()` line and fold it into the
general failure case. If this causes test failures, it means the type checker
cannot handle the stepped term — and that would be a separate issue to
investigate.

### Step 6: Full build and test suite gate

**Commands:**
```bash
cabal build all && cabal test
```

**Verification:** Exit code 0, zero test failures. The entire 1175+ test suite
must pass.

### Step 7: Write implementation-notes.md

Record a concise change summary at
`orchestrator/rounds/round-148/implementation-notes.md`.

## Deliverables

| Artifact | Path |
|----------|------|
| Recursive let reduction in `step` | `src/MLF/Elab/Reduce.hs` |
| Updated type-preservation test | `test/PipelineSpec.hs` |
| Implementation notes | `orchestrator/rounds/round-148/implementation-notes.md` |

## Risk Assessment

- **Low risk:** The recursive guard (`v `Set.member` freeTermVars rhs`) only
  activates for recursive bindings. Non-recursive lets are unaffected.

- **Low risk:** The one-step letrec unfolding is a well-known reduction
  semantics for recursive let in call-by-value languages. It terminates
  because each unfolding produces a value (the rhs is already a value by the
  `isValue rhs` guard, and the substitution wraps recursive refs in letrecs
  inside a value).

- **Medium risk:** The `freeTermVars` call adds O(|rhs|) cost to each
  recursive let step. For the recursive expressions in our test suite (small
  lambdas), this is negligible. For pathologically large recursive rhs terms
  it could be noticeable, but this is not a production concern.

- **Medium risk:** The type-preservation test upgrade (Step 5) may reveal that
  `typeCheck` cannot handle letrec-expanded terms. If so, the test change
  should be reverted to keep tolerating the limitation, and a comment updated.

## Non-Goals

- This plan does NOT add `ELetRec` as a separate constructor. The fix is purely
  in the reduction semantics — detecting recursive lets at reduction time.
- This plan does NOT modify the elaboration pipeline or constraint generation.
- This plan does NOT add fuel/depth limits to `normalize`. The reduction is
  guaranteed to terminate for lambda values.
