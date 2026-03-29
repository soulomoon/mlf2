# Round 146 — Plan: Item-1 Phase 4 Witness Normalization for TyMu

## Selected Item

- **Item id:** `item-1`
- **Title:** Phase 4: Extend witness normalization to handle TyMu nodes

## Roadmap Reference

- **`roadmap_id`:** `2026-03-29-02-iso-recursive-inference-gap-fixes`
- **`roadmap_revision`:** `rev-001`
- **`roadmap_dir`:** `orchestrator/roadmaps/2026-03-29-02-iso-recursive-inference-gap-fixes/rev-001`

## Scope & Constraints

The implementer works in worktree `orchestrator/worktrees/round-146/` on branch
`orchestrator/round-146-item1-witness-tymu`. The implementer must **not** directly
modify `orchestrator/state.json` (controller-owned).

## Root Cause Analysis

The nested recursive lets test (`PipelineSpec.hs:1350`) fails in Phase 4 with
`WitnessNormalizationError`. Three code paths in the witness normalization
pipeline lack TyMu awareness:

1. **Interior computation** — `WitnessNorm.hs:bindersOrdered` (lines 133–143)
   and `WitnessValidation.hs:replayBindersForRoot` (lines 123–133) look up the
   edge root node and only special-case `TyVar{ tnBound = Just bnd }`. When the
   root is a `TyMu` node, they fall through to the `_ -> direct` branch without
   following through to `tnBody`, yielding an empty/wrong binder list.

2. **Validation node checks** — `WitnessValidation.hs:requireGraftTarget`
   (line 208) pattern-matches `TyVar{ tnBound = Just bnd }` to check graft
   validity. A TyMu node falls through to the `_ -> Right ()` default, which
   silently permits any graft target. While this happens to not reject, it
   does not positively classify TyMu binder nodes, leaving them implicitly
   unrecognized.

3. **WitnessCanon.hs** — `stripExteriorOps` (line 32) filters ops by
   `inInterior`. If TyMu body sub-nodes aren't in the computed interior set
   (because the root-following logic doesn't unwrap TyMu), legitimate ops get
   stripped, producing an incorrect normalized witness that then fails
   validation.

The common fix pattern is: wherever the code special-cases `TyForall` or
`TyVar{ tnBound = Just }` by following `tnBody`, add a parallel case for
`TyMu{ tnBody = b }` that follows through to the body in the same way.

## Steps

### Step 1: Add TyMu case to `bindersOrdered` in WitnessNorm.hs

**File:** `src/MLF/Constraint/Presolution/WitnessNorm.hs`

**Location:** Lines 139–143 (the `case NodeAccess.lookupNode c0 rootC of` block
inside `bindersOrdered`)

**Change:** Add a `Just TyMu{ tnBody = b }` case that follows through to the
body, mirroring the `TyVar{ tnBound = Just bnd }` case:

```haskell
-- BEFORE:
in case NodeAccess.lookupNode c0 rootC of
    Just TyVar{ tnBound = Just bnd } ->
        let viaBound = orderedUnder bnd
        in if null direct then viaBound else direct
    _ -> direct

-- AFTER:
in case NodeAccess.lookupNode c0 rootC of
    Just TyVar{ tnBound = Just bnd } ->
        let viaBound = orderedUnder bnd
        in if null direct then viaBound else direct
    Just TyMu{ tnBody = b } ->
        let viaBody = orderedUnder b
        in if null direct then viaBody else direct
    _ -> direct
```

**Rationale:** TyMu, like TyForall, introduces a scope with a body. Binder
discovery must follow through to the body to find binders reachable from the
recursive structure.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 2: Add TyMu case to `replayBindersForRoot` in WitnessValidation.hs

**File:** `src/MLF/Constraint/Presolution/WitnessValidation.hs`

**Location:** Lines 129–133 (inside `replayBindersForRoot`)

**Change:** Add a `Just TyMu{ tnBody = b }` case:

```haskell
-- BEFORE:
in case NodeAccess.lookupNode (constraint env) rootC of
    Just TyVar{ tnBound = Just bnd } ->
        let viaBound = orderedUnder bnd
        in if null direct then viaBound else direct
    _ -> direct

-- AFTER:
in case NodeAccess.lookupNode (constraint env) rootC of
    Just TyVar{ tnBound = Just bnd } ->
        let viaBound = orderedUnder bnd
        in if null direct then viaBound else direct
    Just TyMu{ tnBody = b } ->
        let viaBody = orderedUnder b
        in if null direct then viaBody else direct
    _ -> direct
```

**Rationale:** Identical reasoning to Step 1 — the validation-side binder
discovery must also follow through TyMu bodies.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 3: Add TyMu to `requireGraftTarget` in WitnessValidation.hs

**File:** `src/MLF/Constraint/Presolution/WitnessValidation.hs`

**Location:** Lines 208–218 (inside `requireGraftTarget`)

**Change:** After the `TyVar{ tnBound = Just bnd }` case, add handling for
TyMu binder nodes. When a graft targets a node inside a TyMu body, the node
is a legitimate graft target if the TyMu's recursive binder variable has a
bottom bound (same logic as TyVar):

```haskell
-- AFTER the existing TyVar case, add:
Just TyMu{} -> Right ()
```

**Rationale:** TyMu nodes are always valid graft targets — they represent
recursive type positions, not ordinary structure with bound constraints that
could reject grafts. The `_ -> Right ()` fallthrough already handles this
case; this step makes the intent explicit so future readers don't have to
rely on the fallthrough. If analysis shows TyMu graft targets need
bound-checking, the explicit match provides the correct attachment point.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 4: Add TyMu to `abstractBoundShape` in WitnessNorm.hs

**File:** `src/MLF/Constraint/Presolution/WitnessNorm.hs`

**Location:** Lines 227–252 (inside `abstractBoundShape`)

**Change:** The inner `go` function already handles `TyMu` implicitly via the
`Just node -> let children = structuralChildren node` fallthrough (line 243),
since `structuralChildren TyMu{ tnBody = b } = [b]`. However, the outer entry
point (lines 248–252) only enters the recursion for `TyVar{ tnBound = Just bnd }`.
Add a `TyMu` entry case:

```haskell
-- BEFORE:
in case NodeAccess.lookupNode c0 (canonical nid) of
    Just TyVar{ tnBound = Just bnd } ->
        go IntSet.empty bnd
    _ ->
        False

-- AFTER:
in case NodeAccess.lookupNode c0 (canonical nid) of
    Just TyVar{ tnBound = Just bnd } ->
        go IntSet.empty bnd
    Just TyMu{ tnBody = b } ->
        go IntSet.empty b
    _ ->
        False
```

**Rationale:** `abstractBoundShape` determines whether an OpWeaken target has an
abstract bound shape, used for pruning finalized ops. TyMu nodes have bodies
that are structurally similar to TyForall/TyVar bounds — they contain
variables and type structure that should be walked. Without this, TyMu-rooted
binders may be incorrectly pruned from the final witness.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 5: Upgrade the test at PipelineSpec.hs:1350

**File:** `test/PipelineSpec.hs`

**Location:** Lines 1350–1371 (the `"characterizes nested recursive lets..."` test)

**Change:** Replace the current "expects Phase 4 failure" test with a full
pipeline success assertion. The test currently:
1. Calls `expectAlignedPipelinePastPhase3 expr`
2. Verifies the pipeline returns `WitnessNormalizationError` in Phase 4
3. Asserts the constraint contains `TyMu`

Replace with:
1. Keep the `TyMu` presence check (it validates the cycle detection is working)
2. Replace the `forM_ pipelineRuns` block that asserts Phase 4 failure with
   assertions that both unchecked and checked pipelines **succeed**
3. Assert the returned term type-checks

```haskell
-- Replace lines 1359-1371 with:
-- Both pipeline runs should succeed now that witness normalization handles TyMu
forM_ pipelineRuns $ \(label, result) ->
  case result of
    Left err ->
      expectationFailure
        (label ++ " failed: " ++ renderPipelineError err)
    Right (term, ty) ->
      typeCheck term `shouldBe` Right ty
```

Keep the `expectAlignedPipelinePastPhase3 expr` call (line 1356) and the
`constraintContainsTyMu` assertion (line 1358) — these validate prerequisites.

**Verification:**
```bash
cabal test --test-show-details=direct --test-option='-m' --test-option='nested recursive lets'
```

### Step 6: Full build and test suite gate

**Commands:**
```bash
cabal build all && cabal test
```

**Verification:** Exit code 0, zero test failures. The entire 1175+ test suite
must pass, confirming:
- The nested recursive lets test now succeeds end-to-end
- No regressions in non-recursive witness normalization
- All other test families remain stable

### Step 7: Iterative debugging (conditional)

If Step 6 reveals failures:

1. **Regressions in existing tests** — Investigate whether the new TyMu cases
   fire for non-recursive edges. The new cases should only activate when
   `NodeAccess.lookupNode` returns a `TyMu` constructor, which only exists
   after cycle detection in `Acyclicity.hs`. Non-recursive constraints will
   never have TyMu nodes. Add guards if needed.

2. **Different Phase 4 error for nested lets** — If the test still fails but
   with a different `OmegaNormalizeError` variant (e.g., `MergeDirectionInvalid`,
   `OpOutsideInterior` on different nodes), the interior set computation or
   order-key generation may also need TyMu awareness. Investigate:
   - `Order.orderKeysFromConstraintWith` — does it follow through TyMu bodies?
   - `Binding.interiorOfUnder` — does the binding tree correctly scope TyMu?
   - `edgeInteriorExact` — is the TyMu body included in the interior set?

3. **Phase 5/6 failures** — The test may pass Phase 4 but fail in a later phase.
   That is expected and acceptable for this round — later phases are addressed
   by items 2–4 in the roadmap. If the Phase 6 error is "alias bounds survived
   scheme finalization", that's the known item-2 issue. Update the test to assert
   the *new* failure point (Phase 5 or 6) instead of full success, but only as a
   last resort — the completion criteria require full pipeline success.

**Verification:** Re-run `cabal build all && cabal test` after each fix.

## Deliverables

| Artifact | Path |
|----------|------|
| WitnessNorm TyMu handling | `src/MLF/Constraint/Presolution/WitnessNorm.hs` |
| WitnessValidation TyMu handling | `src/MLF/Constraint/Presolution/WitnessValidation.hs` |
| Upgraded test | `test/PipelineSpec.hs` |

## Risk Assessment

- **Low risk:** Steps 1–4 add `TyMu` match arms alongside existing `TyVar`/`TyForall`
  arms. They follow established patterns and only activate for TyMu nodes, which
  only exist after automatic cycle detection. Non-recursive constraint paths are
  unchanged.
- **Medium risk:** The nested recursive lets expression may expose additional
  Phase 4 issues beyond the binder/interior computation (e.g., order-key gaps,
  merge direction on recursive edges). Step 7 covers iterative debugging.
- **Out of scope:** Phase 5/6 failures (items 2–5 on the roadmap) are not
  addressed here. If the test cannot fully pass due to a later-phase failure,
  document the new failure point and consult the reviewer.

## Non-Goals

- This plan does NOT modify `WitnessCanon.hs:stripExteriorOps` directly. The
  `inInterior` check uses the interior set computed by `edgeInteriorExact` in
  `WitnessNorm.hs`, which delegates to `Binding.interiorOfUnder`. If the binding
  tree correctly includes TyMu body nodes (which it should, since TyMu is
  introduced with proper binding structure by `Acyclicity.hs`), no changes to
  `stripExteriorOps` should be needed. If testing reveals otherwise, this
  becomes part of Step 7 debugging.
- This plan does NOT modify `Binding.Tree`, `Binding.Queries`, or
  `NodeAccess.hs`. These modules use `structuralChildren` which already
  handles `TyMu`. Changes there would only be needed if the binding tree
  itself miscomputes interior sets for TyMu — unlikely given that simple
  self-recursion already works.
