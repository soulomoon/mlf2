# Round 153 Implementation Notes

## Change

Added `Just TyMu{} -> True` to the `mbRootInst` bind-parent guard in
`src/MLF/Elab/Phi/Omega/Interpret.hs` (line 1099), treating `TyMu` nodes as
binder-like (analogous to `TyForall`) when deciding whether an OpRaise
non-spine context has a valid root instantiation.

## Diff

```haskell
-- Before (lines 1097-1099):
|| case lookupNodePV parentC of
    Just TyForall{} -> True
    _ -> False

-- After (lines 1097-1100):
|| case lookupNodePV parentC of
    Just TyForall{} -> True
    Just TyMu{} -> True
    _ -> False
```

## Build & Test Results

- `cabal build all`: **clean**, no new warnings
- `cabal test`: **1176 examples, 0 failures**

## Impact Assessment

### PipelineSpec "hits elaboration blocker" test (line ~2336)
**Still passes with `expectStrictPipelineFailure`** — case (a) from the plan.
The `mbRootInst` guard is no longer the blocker for the TyMu bind-parent path,
but there is at least one additional downstream blocker keeping the pipeline
failure behavior intact. No test change needed; upgrading this test is item-4
scope.

### ElaborationSpec "non-spine OpRaise" test (line ~4498)
**Passes unchanged** — this test uses `TyForall` bind-parents in its
handcrafted constraint and is completely unaffected by the change.

### nodeTy0 guard (lines 1038-1042)
Not modified per plan Step 2 guidance. The fallback `reifyBoundType` path
is used for TyMu bind-parents. No test failures indicate this is acceptable
for now.

## Files Modified

| File | Change |
|------|--------|
| `src/MLF/Elab/Phi/Omega/Interpret.hs` | +1 line: `Just TyMu{} -> True` at line 1099 |

## Files NOT Modified

| File | Reason |
|------|--------|
| `test/PipelineSpec.hs` | Expected-failure test still passes; upgrade is item-4 |
| `test/ElaborationSpec.hs` | Uses TyForall, unaffected |
| `src/MLF/Reify/Type.hs` | item-2 scope, not item-3 |
