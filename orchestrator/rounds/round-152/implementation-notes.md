# Round 152 Implementation Notes

## What Changed

### `src/MLF/Reify/Type.hs`

Restructured the TyMu branch of `goFull` (lines 352–375) from a
`binder <- case binders of ...` extraction pattern into a three-way
`case binders of` where each arm is self-contained:

1. **`[bndr]` arm (local TyMu):** Unchanged behavior. Uses the single
   flex-child binder from the binding tree.

2. **`[]` arm (non-local proxy TyMu, NEW):** Synthesizes a binder from
   the TyMu node itself (`synthBinder = n`). Inserts `getNodeId n` into
   `namedExtra` so body references resolve as `TVar (varName n)`.
   Returns `TMu (varName synthBinder) bodyTy` — same cache/markDone
   pattern as the `[bndr]` path.

3. **Multi-binder error arm:** Unchanged.

**Why restructure:** The plan's replacement code returns `(Cache, ElabType)`
from the `[]` arm, but the original code used `binder <- case binders of`
which expects `Either ElabError NodeId`. The `[]` arm can't both assign a
`NodeId` to `binder` and short-circuit with a full result. Restructuring
into three self-contained arms is the correct approach while preserving
identical behavior for the `[bndr]` and error paths.

### `test/PipelineSpec.hs`

Added unit test "reifies TyMu without binder child (non-local proxy
fallback)" after the existing TyCon reification tests. Constructs a
minimal constraint with a TyMu node (body = arrow: muVar → Int) where
the μ-variable has NO binding-tree entry as a flex-child. Verifies
`reifyType` produces `TMu` instead of `PhiTranslatabilityError`.

### `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`

Updated `expectCurrentArchitectureBlocker` error expectations. After the
fix, `sameLaneAliasFrameClearBoundaryExpr` no longer fails at the
reifyInst TyMu site (`PhiTranslatabilityError`). Instead the pipeline
reaches type-checking and reports `TCLetTypeMismatch`. The predicate now
accepts either error class.

**P5ClearBoundarySpec:91** was NOT affected — it still passes as-is.
**PipelineSpec:2303** was NOT affected — `expectStrictPipelineFailure`
already accepts any `PhiTranslatabilityError` regardless of site.

## Verification

- `cabal build all` — clean, no warnings
- `cabal test` — 1176 examples, 0 failures
- `cabal test --test-option='-m' --test-option='TyMu without binder'` — 1 example, 0 failures
