# Round 148 — Review

## Decision: APPROVED

## Baseline Checks

| Check | Command | Result |
|-------|---------|--------|
| Whitespace/conflict | `git diff --check` in worktree | ✅ PASS (exit 0, no output) |
| Valid JSON | `python3 -m json.tool orchestrator/state.json >/dev/null` | ✅ PASS (exit 0) |
| Roadmap bundle | `test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"` | ✅ PASS |
| Full build+test | `cabal build all && cabal test` in worktree | ✅ PASS — **1175 examples, 0 failures** |

## Item-3 Specific Checks

### 1. Recursive `ELet` reduction uses fixpoint unfolding

✅ PASS. `src/MLF/Elab/Reduce.hs` lines 35-40 add a guard checking
`v `Set.member` freeTermVars rhs` and perform standard one-step letrec
unfolding:

```haskell
let selfRef = ELet v sch rhs (EVar v)
    rhs'    = substTermVar v selfRef rhs
in Just (substTermVar v rhs' body)
```

This is the well-known `body[v := V[v := let v = V in v]]` reduction.

### 2. Recursive ELet test upgraded from `TCUnboundVar` tolerance to clean success

✅ PASS. The type-preservation test (`PipelineSpec.hs` ~line 1479) no longer
contains the `Left (TCUnboundVar _) -> pure ()` tolerance. The removed lines:

```haskell
-- Note: recursive ELet substitution leaves the bound variable
-- free in the RHS (step treats let as non-recursive), so
-- typeCheck on the post-step term may fail with TCUnboundVar.
-- This is a known limitation; we tolerate it here.
```

and

```haskell
Left (TCUnboundVar _) -> pure () -- expected for recursive let step
```

The now-unused `TypeCheckError(..)` import was also removed (clean diff).

**Targeted test:** `cabal test --test-option='-m' --test-option='type preservation'` → 1 example, 0 failures.

### 3. `step`/`normalize` produce no unbound variables for recursive lets

✅ PASS. The normalization test (`PipelineSpec.hs` line 1471) passes:
`normalize` on a recursive self-referential term produces a value
(`isValue nf == True`).

**Targeted test:** `cabal test --test-option='-m' --test-option='normalize produces a value for simple self-recursive'` → 1 example, 0 failures.

### 4. Non-recursive `ELet` behavior unchanged

✅ PASS. The `otherwise` branch at line 41 is identical to the original.
The non-recursive test passes.

**Targeted test:** `cabal test --test-option='-m' --test-option='step/normalize unchanged for non-recursive'` → 1 example, 0 failures.

## Diff Review

**Commit:** `f4b5875` on branch `orchestrator/round-148-elet-fixpoint-reduction`

**Files changed:** 2

| File | Lines added | Lines removed | Description |
|------|-------------|---------------|-------------|
| `src/MLF/Elab/Reduce.hs` | +31 | -0 | Recursive-let guard + Note block |
| `test/PipelineSpec.hs` | +0 | -6 | Remove `TCUnboundVar` tolerance + unused import |

**Plan step-by-step alignment:**

| Plan Step | Implemented | Notes |
|-----------|-------------|-------|
| Step 1: recursive-let guard in `step` | ✅ | Exact match to plan |
| Step 2: Note block | ✅ | Verbatim match |
| Step 3: `freeTermVars` visibility | ✅ | No change needed (already local) |
| Step 4: normalize test passes | ✅ | Test was already correct, fix makes it pass |
| Step 5: type-preservation upgrade | ✅ | `TCUnboundVar` tolerance removed |
| Step 6: full build+test | ✅ | 1175 examples, 0 failures |
| Step 7: implementation-notes.md | ✅ | Written with correct summary |

**No regressions:** Full suite passes (1175/1175).

**Indentation:** 4-space convention maintained throughout.

**No unrelated changes:** Diff is strictly targeted (+31/-6).

## Evidence Summary

- All 4 baseline checks pass.
- All 4 item-3 specific checks pass.
- Diff matches plan exactly across all 7 steps.
- No test regressions.
- No formatting noise.
- Commit `f4b5875` is clean and self-contained.
