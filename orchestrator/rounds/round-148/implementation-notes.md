# Round 148 — Implementation Notes

## Change Summary

Added recursive-let (fixpoint) reduction support to `MLF.Elab.Reduce.step`.

### Problem

`step` treated all `ELet` bindings as non-recursive: when the RHS was a value,
it substituted `rhs` for `v` in `body` only, leaving `v` free inside `rhs`.
For recursive definitions like `let f = \x. f x in f`, the result `\x. f x`
had an unbound `f`, causing `TCUnboundVar "f"` during type checking.

### Fix

Added a guard in `step` that detects recursive bindings (`v ∈ freeTermVars rhs`)
and performs standard one-step letrec unfolding:

```
let v = V in body  →  body[v := V[v := let v = V in v]]
```

Each recursive occurrence of `v` in `rhs` is replaced with a "re-entry point"
(the original letrec applied to just `v`), producing `rhs'`. Then `rhs'` is
substituted for `v` in `body`. Non-recursive lets are completely unaffected.

### Files Changed

| File | Change |
|------|--------|
| `src/MLF/Elab/Reduce.hs` | Added recursive-let guard + letrec unfolding in `step`; added `Note [Recursive let reduction]` |
| `test/PipelineSpec.hs` | Removed `TCUnboundVar` tolerance in type-preservation test (no longer needed); removed now-unused `TypeCheckError(..)` import |

### Verification

- `cabal build all` — zero warnings, zero errors
- `cabal test` — 1175 examples, 0 failures
- Type-preservation test now succeeds without the `TCUnboundVar` tolerance
- Diff: +31/-6 across 2 files (strictly targeted, no formatting noise)
- Commit: `f4b5875` on branch `orchestrator/round-148-elet-fixpoint-reduction`
