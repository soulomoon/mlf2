# Round 160 — Plan

## Scope

Add test coverage for six untested core modules:
`MLF.Reify.Type`, `MLF.Reify.Core`, `MLF.Reify.Named`, `MLF.Reify.TypeOps`,
`MLF.Util.Graph`, `MLF.Util.UnionFind`.

Each new spec file must exercise the module's core exported functions with at
least 3 meaningful examples per function. All new specs are wired into
`test/Main.hs` and `mlf2.cabal`.

---

## Step 1 — Create `test/Util/UnionFindSpec.hs`

**File**: `test/Util/UnionFindSpec.hs`

**Module**: `module Util.UnionFindSpec (spec) where`

**Exports to test** (from `MLF.Util.UnionFind`):
- `frWith`
- `findRootWithCompression`

**Functions to test and example descriptions**:

### `frWith` (at least 3 examples)
1. **Identity on unmapped node**: `frWith IntMap.empty (NodeId 5)` returns `NodeId 5`.
2. **Self-loop**: `frWith (IntMap.singleton 1 (NodeId 1)) (NodeId 1)` returns `NodeId 1`.
3. **Chain chase**: Build a parent map `{1→2, 2→3}` and verify `frWith map (NodeId 1)` returns `NodeId 3`.
4. **No path compression**: After `frWith`, the original map is unchanged (verify via a second lookup that still chains).

### `findRootWithCompression` (at least 3 examples)
1. **Identity on unmapped node**: Returns `(NodeId 5, unchanged map)`.
2. **Self-loop**: Returns `(NodeId 1, unchanged map)`.
3. **Chain with compression**: For `{1→2, 2→3}`, returns `(NodeId 3, map')` where `map'` has `1→3` (compressed).
4. **Multiple calls converge**: After compression from step 3, calling again for `NodeId 1` returns immediately.

**Imports**: `Test.Hspec`, `Data.IntMap.Strict`, `MLF.Util.UnionFind`, `MLF.Constraint.Types.Graph (NodeId(..))`.

**Verification**: `cabal build mlf2-test`

---

## Step 2 — Create `test/Util/GraphSpec.hs`

**File**: `test/Util/GraphSpec.hs`

**Module**: `module Util.GraphSpec (spec) where`

**Exports to test** (from `MLF.Util.Graph`):
- `topoSortBy`
- `reachableFrom`
- `reachableFromStop`

**Functions to test and example descriptions**:

### `topoSortBy` (at least 3 examples)
1. **Empty input**: `topoSortBy "test" compare (const []) []` returns `Right []`.
2. **Single node**: Returns `Right [n]`.
3. **Linear chain**: Nodes `[1,2,3]` with deps `1→2, 2→3` returns `Right [3,2,1]` (dependencies first).
4. **Diamond DAG**: Nodes `[1,2,3,4]` where 1 depends on 2 and 3, both depend on 4 → result starts with 4 and ends with 1.
5. **Cycle detection**: A cycle `1→2, 2→1` returns `Left (InstantiationError _)`.
6. **Tie-break comparator**: Two independent nodes are ordered by the `cmp` argument (verify with `flip compare`).

### `reachableFrom` (at least 3 examples)
1. **Isolated node**: No successors → result is `{start}`.
2. **Linear chain**: Start at root of `1→2→3` → result is `{1,2,3}`.
3. **DAG with shared children**: Diamond graph → all nodes reachable.

### `reachableFromStop` (at least 3 examples)
1. **No stop**: Same as `reachableFrom`.
2. **Stop at a node**: In chain `1→2→3`, stop at 2 → result is `{1}` (2 is included in result but not expanded — per code, `Cons nC acc` returns `acc` when `isStop`, so 2 is NOT in result set; verify carefully from source).
3. **Stop doesn't apply to start**: Start node is never stopped even if `shouldStop` returns `True` for it.

**Imports**: `Test.Hspec`, `Data.IntSet`, `MLF.Util.Graph`, `MLF.Util.ElabError (ElabError(..))`.

Use simple `Int`-keyed nodes with `id` as canonical, and explicit successor lists via a lookup map.

**Verification**: `cabal build mlf2-test`

---

## Step 3 — Create `test/Reify/TypeOpsSpec.hs`

**File**: `test/Reify/TypeOpsSpec.hs`

**Module**: `module Reify.TypeOpsSpec (spec) where`

**Exports to test** (from `MLF.Reify.TypeOps`):
Focus on pure `ElabType`-level functions that can be tested without building a
full constraint graph. The graph-dependent functions (`resolveBaseBound*`,
`resolveBoundBody*`, `inlineBaseBoundsType`, `inlineAliasBounds*`) require a
`Constraint` or `Solved` and are covered transitively by existing
`PipelineSpec`/`ElaborationSpec`; they are lower priority.

**Primary functions to test**:

### `splitForalls` (3+ examples)
1. **No foralls**: `splitForalls (TArrow (TVar "a") (TVar "b"))` → `([], TArrow …)`.
2. **One forall, no bound**: `splitForalls (TForall "a" Nothing (TVar "a"))` → `([("a", Nothing)], TVar "a")`.
3. **Nested foralls with bounds**: `splitForalls (TForall "a" (Just (TBase (BaseTy "int"))) (TForall "b" Nothing (TArrow (TVar "a") (TVar "b"))))` → `([("a", Just …), ("b", Nothing)], TArrow …)`.

### `stripForallsType` (3 examples)
1. **No foralls**: Returns the type unchanged.
2. **Single forall**: Returns the body.
3. **Nested foralls**: Returns the innermost body.

### `freeTypeVarsType` (3+ examples)
1. **Variable**: `freeTypeVarsType (TVar "a")` → `{"a"}`.
2. **Arrow**: `freeTypeVarsType (TArrow (TVar "a") (TVar "b"))` → `{"a","b"}`.
3. **Bound variable**: `freeTypeVarsType (TForall "a" Nothing (TVar "a"))` → `{}`.
4. **Mixed**: `freeTypeVarsType (TForall "a" Nothing (TArrow (TVar "a") (TVar "b")))` → `{"b"}`.

### `freeTypeVarsList` (3 examples)
Delegate to `freeTypeVarsType` in sorted order; test same cases as above.

### `freeTypeVarsFrom` (3 examples)
1. Subset of seed vars that appear free in the type.
2. Seed vars that are bound → empty.
3. Mixed free and bound.

### `substTypeCapture` (3+ examples)
1. **Simple var replacement**: `substTypeCapture "a" (TBase (BaseTy "int")) (TVar "a")` → `TBase …`.
2. **No-op for different var**: Substituting "a" leaves "b" untouched.
3. **Capture avoidance**: `substTypeCapture "a" (TVar "b") (TForall "b" Nothing (TVar "a"))` → the inner "b" is freshened.

### `substTypeSimple` (3 examples)
1. **Simple replacement**: Same as substTypeCapture case 1.
2. **No-op**: Same as substTypeCapture case 2.
3. **No capture avoidance**: Verify this does NOT rename binders (differs from `substTypeCapture`).

### `renameTypeVar` (3 examples)
1. Rename free var.
2. No-op for non-matching var.
3. Rename in nested structure.

### `alphaEqType` (3+ examples)
1. **Equal types**: `alphaEqType (TVar "a") (TVar "a")` → `True`.
2. **Different vars**: `TVar "a"` vs `TVar "b"` → `False`.
3. **Alpha-equivalent foralls**: `∀a.a` vs `∀b.b` → `True`.
4. **Non-alpha-equivalent**: `∀a.a` vs `∀a.b` → `False`.

### `matchType` (3+ examples)
1. **Concrete match**: Pattern `TVar "a"` with binder set `{"a"}` against `TBase int` → `Right {"a" → TBase int}`.
2. **Structure mismatch**: Pattern `TArrow _ _` against `TBase _` → `Left`.
3. **Multi-var pattern**: Pattern `TArrow (TVar "a") (TVar "b")` with `{"a","b"}` against `TArrow (TBase int) (TBase bool)` → `Right {"a" → TBase int, "b" → TBase bool}`.

### `freshTypeName` (3 examples)
1. Empty used set → `"u0"`.
2. `{"u0"}` → `"u1"`.
3. `{"u0","u1"}` → `"u2"`.

### `freshTypeNameFromCounter` (3 examples)
1. Counter 0, empty → `("u0", 1)`.
2. Counter 0, `{"u0"}` → `("u1", 2)`.
3. Counter 5, empty → `("u5", 6)`.

### `firstNonContractiveRecursiveType` (3 examples)
1. No `TMu` → `Nothing`.
2. Contractive `TMu "a" (TArrow (TVar "a") (TVar "a"))` → `Nothing`.
3. Non-contractive `TMu "a" (TVar "a")` → `Just (TMu "a" (TVar "a"))`.

### `parseNameId` (3 examples)
1. `parseNameId "t42"` → `Just 42`.
2. `parseNameId "abc"` → `Nothing`.
3. `parseNameId "t0"` → `Just 0`.

**Imports**: `Test.Hspec`, `Data.Set`, `Data.Map.Strict`, `MLF.Reify.TypeOps`,
`MLF.Types.Elab`, `MLF.Constraint.Types.Graph (BaseTy(..))`.

Note: `parseNameId` and `freshNameLike` are actually re-exported from
`MLF.Util.Names`; test them via `MLF.Reify.TypeOps` which re-exports them.

**Verification**: `cabal build mlf2-test`

---

## Step 4 — Create `test/Reify/NamedSpec.hs`

**File**: `test/Reify/NamedSpec.hs`

**Module**: `module Reify.NamedSpec (spec) where`

**Exports to test** (from `MLF.Reify.Named`):
- `namedNodes`
- `softenedBindParentsUnder`

These functions require a `PresolutionView` or `Constraint` with bind parents.
Use `SpecUtil.emptyConstraint`, `SpecUtil.nodeMapFromList`, and
`SpecUtil.rootedConstraint` to build minimal constraints. Construct a
`PresolutionView` using the `PresolutionView` constructor from
`MLF.Constraint.Presolution.View`.

### `namedNodes` (3+ examples)
1. **Empty constraint**: No type nodes → `Right IntSet.empty`.
2. **Single TyVar under gen root**: A TyVar whose bind-parent is a gen-ref → appears in result.
3. **TyArrow under gen root**: Not a TyVar → does NOT appear in result.
4. **TyVar not directly under gen root**: Nested TyVar under another node → does NOT appear.

### `softenedBindParentsUnder` (3+ examples)
1. **No weakened vars**: All bind flags unchanged.
2. **Weakened rigid var becomes flex**: A TyVar child that is rigid-bound AND in `cWeakenedVars` → becomes flex.
3. **Non-weakened rigid stays rigid**: A rigid-bound child NOT in `cWeakenedVars` → remains rigid.

Build minimal constraints with explicit `cBindParents`, `cWeakenedVars`, and node maps.

**Imports**: `Test.Hspec`, `Data.IntMap.Strict`, `Data.IntSet`,
`MLF.Reify.Named`, `MLF.Constraint.Presolution.View (PresolutionView(..))`,
`MLF.Constraint.Types`, `SpecUtil`.

**Verification**: `cabal build mlf2-test`

---

## Step 5 — Create `test/Reify/TypeSpec.hs`

**File**: `test/Reify/TypeSpec.hs`

**Module**: `module Reify.TypeSpec (spec) where`

**Exports to test** (from `MLF.Reify.Type`):
- `reifyType`
- `reifyTypeWithNames`
- `reifyTypeWithNamesNoFallback`
- `reifyTypeWithNamesNoFallbackOnConstraint`
- `reifyTypeWithNamedSet`
- `reifyTypeWithNamedSetNoFallback`
- `reifyWith`
- `reifyWithAs`
- `ReifyRoot(..)` (data type, not a function)
- `solvedFromView`
- `freeVars`

These all require a `Solved` or `PresolutionView` from a real pipeline run.
Use `SpecUtil.runToSolvedDefault` / `SpecUtil.runPipelineArtifactsDefault` to
obtain solved constraints from simple expressions, then call the reify
functions.

### `reifyType` (3+ examples)
1. **Identity**: Parse `"fun x -> x"`, reify the result type → should produce a
   `TForall` or `TArrow`.
2. **Base type**: Parse `"42"`, reify → `TBase (BaseTy "int")`.
3. **Let-polymorphism**: Parse `"let id = fun x -> x in id"`, reify → should succeed
   with a polymorphic type.

### `reifyWith` / `reifyWithAs` (3+ examples)
1. Test with `RootType` mode.
2. Test with `RootBound` mode.
3. Test with `RootTypeNoFallback` mode.

Use pipeline helpers to get `Solved` and `NodeId`, then call `reifyWith`
directly. The `reifyWithAs` variant adds a post-transform; test with `Right` identity.

### `solvedFromView` (3 examples)
1. Verify it produces a `Solved` from a `PresolutionView`.
2. Verify the canonical function of the result is consistent.
3. Verify nodes are preserved.

### `freeVars` (3 examples)
1. Closed type → empty set.
2. Open type with free var → non-empty set.
3. Partially closed type → only unbound vars.

Use `SpecUtil.runToSolvedDefault` to produce `Solved` values from string
expressions.

**Imports**: `Test.Hspec`, `Data.IntMap.Strict`, `Data.IntSet`,
`MLF.Reify.Type`, `MLF.Constraint.Solved`, `MLF.Types.Elab`,
`MLF.Util.ElabError`, `SpecUtil`.

**Verification**: `cabal build mlf2-test`

---

## Step 6 — Create `test/Reify/CoreSpec.hs`

**File**: `test/Reify/CoreSpec.hs`

**Module**: `module Reify.CoreSpec (spec) where`

**Exports to test** (from `MLF.Reify.Core`):
`reifyType`, `reifyTypeWithNames`, `reifyTypeWithNamesNoFallback`,
`reifyTypeWithNamesNoFallbackOnConstraint`, `reifyTypeWithNamedSet`,
`reifyTypeWithNamedSetNoFallback`, `reifyWithAs`, `reifyBoundWithNames`,
`reifyBoundWithNamesOnConstraint`, `reifyBoundWithNamesBound`,
`reifyBoundWithNamesOnConstraintBound`, `freeVars`, `namedNodes`.

`MLF.Reify.Core` is a thin re-export facade over `MLF.Reify.Type`,
`MLF.Reify.Bound`, and `MLF.Reify.Named`. The spec confirms re-export
consistency — each function delegates correctly.

### Per-function tests (3+ examples each for a representative subset)
Since Core delegates 1:1, test a representative subset that exercises each
delegate module:

1. **`reifyType`** (from `Type`): Same pipeline-based tests as Step 5 but called
   via `Core.reifyType`.
2. **`reifyBoundWithNames`** (from `Bound`): Reify a bound type for a polymorphic
   expression.
3. **`reifyBoundWithNamesBound`** (from `Bound`): Reify and get `BoundType`.
4. **`namedNodes`** (from `Named`): Confirm facade delegates correctly.
5. **`freeVars`** (from `Bound`): Free vars on a solved constraint.

Use `SpecUtil.runToSolvedDefault` / `SpecUtil.runPipelineArtifactsDefault`.

**Imports**: `Test.Hspec`, `Data.IntMap.Strict`, `Data.IntSet`,
`MLF.Reify.Core`, `MLF.Constraint.Solved`, `MLF.Types.Elab`,
`MLF.Util.ElabError`, `SpecUtil`.

**Verification**: `cabal build mlf2-test`

---

## Step 7 — Wire new specs into `test/Main.hs`

**File**: `test/Main.hs`

**Changes**:

Add 6 qualified imports (sorted into existing import block):

```haskell
import qualified Reify.CoreSpec
import qualified Reify.NamedSpec
import qualified Reify.TypeSpec
import qualified Reify.TypeOpsSpec
import qualified Util.GraphSpec
import qualified Util.UnionFindSpec
```

Add 6 `spec` calls at the end of the `hspec $ do` block (before the closing
of `main`), after `Constraint.SolvedSpec.spec`:

```haskell
        Reify.TypeSpec.spec
        Reify.CoreSpec.spec
        Reify.NamedSpec.spec
        Reify.TypeOpsSpec.spec
        Util.GraphSpec.spec
        Util.UnionFindSpec.spec
```

**Verification**: `cabal build mlf2-test`

---

## Step 8 — Wire new modules into `mlf2.cabal`

**File**: `mlf2.cabal`

**Changes**: Add the 6 new spec modules to the `test-suite mlf2-test` →
`other-modules` list. Insert alphabetically among existing entries:

```
                      Reify.CoreSpec,
                      Reify.NamedSpec,
                      Reify.TypeSpec,
                      Reify.TypeOpsSpec,
                      Util.GraphSpec,
                      Util.UnionFindSpec,
```

Insert after `PublicSurfaceSpec,` (for `Reify.*`) and after
`TranslatablePresolutionSpec,` (for `Util.*`), or simply append before
`SpecUtil,`. The exact position within the list is not critical as long as
each entry is present.

**Verification**: `cabal build mlf2-test`

---

## Step 9 — Full verification

Run the complete verification gate:

```bash
cabal clean && cabal build all 2>&1
cabal test --test-show-details=direct 2>&1
```

**Acceptance criteria**:
1. `cabal build all` exits 0 with no warnings.
2. `cabal test` exits 0 with 0 failures.
3. Test example count ≥ 1177 (must not decrease from baseline).
4. All 6 new spec files are listed in `cabal test` output.
5. Each new spec has ≥ 3 meaningful examples per exported function tested.
6. `./scripts/thesis-conformance-gate.sh` exits 0.

---

## Implementation Notes

- **Pattern to follow**: See `test/InertSpec.hs` and `test/CanonicalizerSpec.hs`
  for the module structure, import style, and `describe`/`it` nesting pattern.
- **Test helpers**: Use `SpecUtil.emptyConstraint`, `nodeMapFromList`,
  `rootedConstraint`, `runToSolvedDefault`, `runPipelineArtifactsDefault` for
  constraint/pipeline-based tests.
- **Pure functions first**: TypeOps, UnionFind, and Graph modules export many
  pure functions that can be tested directly with hand-constructed data — no
  pipeline needed.
- **Directory structure**: Spec files under `test/Reify/` and `test/Util/`
  follow the existing `test/Presolution/`, `test/Phi/`, and `test/Constraint/`
  subdirectory pattern.
- **Expose guard**: All target modules are already in `mlf2-internal`
  `other-modules` or `exposed-modules`; `mlf2-test` depends on `mlf2-internal`,
  so no library stanza changes needed.
