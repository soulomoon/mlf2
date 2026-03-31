# Round 162 — Expand property-based testing (QuickCheck)

## Branch

`orchestrator/round-162-quickcheck-properties`

## Overview

Add QuickCheck properties for four deliverables:
- (a) Reification round-trip well-formedness
- (b) Canonicalization idempotency
- (c) Binding tree parent-child invariant preservation through presolution
- (d) Unification symmetry

## Existing QuickCheck Infrastructure

- `QuickCheck >=2.14 && <2.18` already in `build-depends` for `mlf2-test`.
- `hspec` already in use; QuickCheck integrates via `Test.Hspec` + `Test.QuickCheck` (`property`, `forAll`).
- Existing patterns to follow:
  - `test/CanonicalizerSpec.hs`: `Arbitrary UnionFindMap`, `Arbitrary RedirectMap`, `property $` style.
  - `test/BindingSpec.hs`: `genValidBindingTree`, `genTreeBindingTree`, `Arbitrary SmallBindingConstraint`, `property $ forAll (choose ...) $ \n ->` style.
  - `test/GraphOpsSpec.hs`: `genFlexChain`, `genAllFlexTree`, `property $ forAll` style.
  - `test/TypeSoundnessSpec.hs`: `genClosedWellTypedElabTerm`, complex Gen-based generators.
  - `test/Presolution/WitnessSpec.hs`: `forAll genInstanceOps`, `forAll genNormalizeEnvParams`.

## File Plan

### New file: `test/Property/QuickCheckPropertySpec.hs`

Create a single new spec module that houses all four property groups. This keeps property tests discoverable and avoids polluting existing unit-test specs.

### Files to modify

1. `mlf2.cabal` — add `Property.QuickCheckPropertySpec` to `test-suite mlf2-test` → `other-modules`.
2. `test/Main.hs` — import and wire `Property.QuickCheckPropertySpec.spec`.

---

## Step 1: Create directory and spec file skeleton

**File**: `test/Property/QuickCheckPropertySpec.hs`

Create the module with these imports:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
module Property.QuickCheckPropertySpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

-- Types and functions under test
import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode, makeCanonicalizer)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren, DecomposeMismatch(..))
import MLF.Binding.Tree (checkBindingTree)
import MLF.Binding.Validation (checkBindingTree, checkBindingTreeUnder)
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, substTypeCapture, substTypeSimple)
import MLF.Types.Elab (Ty(..), ElabType, BoundType, BaseTy(..))
import SpecUtil (emptyConstraint, rootedConstraint, nodeMapFromList, bindParentsFromPairs)
```

The module exports exactly `spec :: Spec`.

---

## Step 2: Generators and Arbitrary instances

### 2a. `Arbitrary ElabType` / Generator `genElabType`

Write a sized generator `genElabType :: Int -> Gen ElabType` that produces well-formed `ElabType` values. Use these constructors:

- **`TVar`**: pick from `["a", "b", "c", "d", "e"]` via `elements`.
- **`TBase`**: pick from `[BaseTy "int", BaseTy "bool"]`.
- **`TBottom`**: leaf.
- **`TArrow`**: recursive with `genElabType (n `div` 2)` for dom/cod.
- **`TForall`**: binder name from `elements ["x","y","z"]`, optional bound from `genBoundType (n `div` 2)`, body from `genElabType (n `div` 2)`.
- **`TMu`**: binder name from `elements ["r","s"]`, body from `genElabType (n `div` 2)`.

At size 0, pick only leaves: `TVar`, `TBase`, `TBottom`.

Write `genBoundType :: Int -> Gen BoundType` analogously but without `TVar` (since `BoundType = Ty 'NoTopVar` cannot contain top-level `TVar`). Leaves: `TBase`, `TBottom`. Recursive: `TArrow`, `TForall`, `TMu`.

Wrap as:
```haskell
instance Arbitrary (Ty 'AllowVar) where
    arbitrary = sized genElabType
    shrink = shrinkElabType

shrinkElabType :: ElabType -> [ElabType]
-- shrink arrows to their children, foralls to body, etc.
```

### 2b. Generator `genCanonUnionFind` / `genCanonRedirects`

Reuse the existing pattern from `CanonicalizerSpec.hs`:
- `genUnionFindMap :: Gen (IntMap.IntMap NodeId)` — forest of 0..8 nodes where parent(k) ∈ [0..k].
- `genRedirectMap :: Gen (IntMap.IntMap NodeId)` — sparse map, each key may redirect to any node in domain.

These are already in `CanonicalizerSpec.hs`. Either import them (if made accessible) or duplicate the ~20 lines in the new module. **Preferred**: duplicate, to avoid changing existing module exports.

### 2c. Generator `genValidBindingConstraint`

Reuse the pattern from `BindingSpec.hs: genValidBindingTree :: Int -> Gen Constraint`. The implementer should copy or refactor the generator. The core idea:
- Build a chain of TyForall nodes: node 0 → node 1 → ... → node (n-1).
- Each child bound to its structural parent.
- Random BindFlex/BindRigid flags.
- Wrap with `rootedConstraint`.

### 2d. Generator `genTyNodePair`

For unification symmetry, generate pairs of `TyNode` values with the same head constructor:
```haskell
genTyNodePair :: Gen (TyNode, TyNode)
```
Cases:
- Both `TyArrow`: distinct `NodeId` children.
- Both `TyForall`: distinct `NodeId` body.
- Both `TyBase`: same or different `BaseTy`.
- Both `TyVar`: distinct `NodeId`.
- Both `TyBottom`.
- Both `TyMu`: distinct `NodeId` body.
- Mixed constructors (for mismatch symmetry).

---

## Step 3: Deliverable (a) — Reification round-trip well-formedness

### Property: `prop_freeVarsSubsetAfterSubst`

**Name in spec**: `"substTypeCapture preserves free-variable well-formedness"`

For any `ElabType` `ty`, variable name `v`, and replacement type `s`:
- `freeTypeVarsType (substTypeCapture v s ty)` ⊆ `(freeTypeVarsType ty \\ {v}) ∪ freeTypeVarsType s`

```haskell
it "substTypeCapture preserves free-variable well-formedness" $ property $
    forAll (sized genElabType) $ \ty ->
    forAll (elements ["a","b","c","d","e"]) $ \v ->
    forAll (sized genElabType) $ \s ->
        let result = substTypeCapture v s ty
            fvResult = freeTypeVarsType result
            expected = Set.union (Set.delete v (freeTypeVarsType ty)) (freeTypeVarsType s)
        in fvResult `Set.isSubsetOf` expected
```

### Property: `prop_alphaEqReflexive`

**Name in spec**: `"alphaEqType is reflexive"`

For any `ElabType` `ty`: `alphaEqType ty ty == True`.

```haskell
it "alphaEqType is reflexive" $ property $
    forAll (sized genElabType) $ \ty ->
        alphaEqType ty ty === True
```

### Property: `prop_alphaEqSymmetric`

**Name in spec**: `"alphaEqType is symmetric"`

For any two `ElabType` values `ty1`, `ty2`: `alphaEqType ty1 ty2 == alphaEqType ty2 ty1`.

```haskell
it "alphaEqType is symmetric" $ property $
    forAll (sized genElabType) $ \ty1 ->
    forAll (sized genElabType) $ \ty2 ->
        alphaEqType ty1 ty2 === alphaEqType ty2 ty1
```

### Property: `prop_substIdempotent`

**Name in spec**: `"substituting a variable not free in a type is identity"`

For any `ElabType` `ty` and replacement `s`, if `v` is not free in `ty`, then `substTypeCapture v s ty == ty`.

```haskell
it "substituting a variable not free in a type is identity" $ property $
    forAll (sized genElabType) $ \ty ->
    forAll (sized genElabType) $ \s ->
        let used = freeTypeVarsType ty
            fresh = head (filter (`Set.notMember` used) ["q0","q1","q2","q3","q4"])
        in substTypeCapture fresh s ty === ty
```

---

## Step 4: Deliverable (b) — Canonicalization idempotency

### Property: `prop_canonicalizationIdempotent`

**Name in spec**: `"canonicalization is idempotent (makeCanonicalizer)"`

This property already exists in `CanonicalizerSpec.hs`. Add a reinforced version with `withMaxSuccess 200`:

```haskell
it "canonicalization is idempotent (makeCanonicalizer)" $
    withMaxSuccess 200 $ property $
    forAll genUnionFindMap $ \uf ->
    forAll genRedirectMap $ \redirects ->
        let canon = makeCanonicalizer uf redirects
            nodes = nodeDomain uf redirects
        in all (\nid -> let c = canonicalizeNode canon nid
                        in canonicalizeNode canon c == c) nodes
```

### Property: `prop_canonicalizationStable`

**Name in spec**: `"canonicalization is stable (triple-apply)"`

For any UF + redirects: `canon (canon (canon n)) == canon n` for all nodes.

```haskell
it "canonicalization is stable (triple-apply)" $
    withMaxSuccess 200 $ property $
    forAll genUnionFindMap $ \uf ->
    forAll genRedirectMap $ \redirects ->
        let canon = makeCanonicalizer uf redirects
            nodes = nodeDomain uf redirects
        in all (\nid -> let c1 = canonicalizeNode canon nid
                            c2 = canonicalizeNode canon c1
                            c3 = canonicalizeNode canon c2
                        in c1 == c2 && c2 == c3) nodes
```

### Property: `prop_canonicalizationDeterministic`

**Name in spec**: `"canonicalization is deterministic"`

Two `Canonicalizer` instances built from the same UF + redirects produce the same result for all nodes.

```haskell
it "canonicalization is deterministic" $
    withMaxSuccess 200 $ property $
    forAll genUnionFindMap $ \uf ->
    forAll genRedirectMap $ \redirects ->
        let canon1 = makeCanonicalizer uf redirects
            canon2 = makeCanonicalizer uf redirects
            nodes = nodeDomain uf redirects
        in all (\nid -> canonicalizeNode canon1 nid == canonicalizeNode canon2 nid) nodes
```

---

## Step 5: Deliverable (c) — Binding tree invariant preservation

### Property: `prop_validBindingTreePassesCheck`

**Name in spec**: `"genValidBindingTree always produces valid binding trees"`

```haskell
it "genValidBindingTree always produces valid binding trees" $ property $
    forAll (choose (1, 20)) $ \n -> do
        c <- generate (genValidBindingConstraint n)
        case checkBindingTree c of
            Right () -> pure ()
            Left err -> expectationFailure (show err)
```

### Property: `prop_bindingParentChildConsistency`

**Name in spec**: `"every child in binding tree has a valid parent node"`

For a generated valid constraint: every key in `cBindParents` refers to a parent that exists as either a `TypeRef` in `cNodes` or a `GenRef` in `cGenNodes`.

```haskell
it "every child in binding tree has a valid parent node" $ property $
    forAll (choose (1, 20)) $ \n -> do
        c <- generate (genValidBindingConstraint n)
        let bp = cBindParents c
            nodeExists ref = case ref of
                TypeRef nid -> case lookupNodeIn (cNodes c) nid of
                    Just _ -> True
                    Nothing -> False
                GenRef gid -> IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
        forM_ (IntMap.elems bp) $ \(parentRef, _flag) ->
            parentRef `shouldSatisfy` nodeExists
```

### Property: `prop_bindingTreeNoCycles`

**Name in spec**: `"binding tree has no cycles"`

For a generated valid constraint: following `cBindParents` from any node always terminates at a root.

```haskell
it "binding tree has no cycles" $ property $
    forAll (choose (1, 20)) $ \n -> do
        c <- generate (genValidBindingConstraint n)
        let bp = cBindParents c
            terminates start = go IntSet.empty start
              where
                go visited key
                    | IntSet.member key visited = False  -- cycle!
                    | otherwise = case IntMap.lookup key bp of
                        Nothing -> True  -- root
                        Just (parent, _) -> go (IntSet.insert key visited) (nodeRefKey parent)
        forM_ (IntMap.keys bp) $ \childKey ->
            childKey `shouldSatisfy` terminates
```

### Property: `prop_rootedConstraintPreservesValidity`

**Name in spec**: `"rootedConstraint preserves binding tree validity"`

Wrapping a valid constraint with `rootedConstraint` still passes `checkBindingTree`.

```haskell
it "rootedConstraint preserves binding tree validity" $ property $
    forAll (choose (1, 15)) $ \n -> do
        c <- generate (genValidBindingConstraint n)
        case checkBindingTree c of
            Right () -> pure ()
            Left err -> expectationFailure ("rooted constraint invalid: " ++ show err)
```

---

## Step 6: Deliverable (d) — Unification symmetry

### Property: `prop_decomposeUnifySymmetric`

**Name in spec**: `"decomposeUnifyChildren is symmetric"`

For any pair of TyNode values: `decomposeUnifyChildren n1 n2` and `decomposeUnifyChildren n2 n1` produce structurally equivalent results (same `Right`/`Left`, and if `Right`, the same number of child edges with swapped sides).

```haskell
it "decomposeUnifyChildren is symmetric" $ property $
    forAll genTyNodePair $ \(n1, n2) ->
        case (decomposeUnifyChildren n1 n2, decomposeUnifyChildren n2 n1) of
            (Right edges1, Right edges2) ->
                length edges1 === length edges2
            (Left _, Left _) -> property True
            _ -> property False
```

### Property: `prop_decomposeUnifySameHeadSucceeds`

**Name in spec**: `"decomposeUnifyChildren succeeds for same-head nodes"`

For any pair of TyNode values with the same head constructor (both TyArrow, both TyBase with same tag, both TyForall, both TyBottom): `decomposeUnifyChildren` returns `Right`.

```haskell
it "decomposeUnifyChildren succeeds for same-head nodes" $ property $
    forAll genSameHeadTyNodePair $ \(n1, n2) ->
        case decomposeUnifyChildren n1 n2 of
            Right _ -> property True
            Left err -> counterexample (show err) (property False)
```

### Property: `prop_decomposeUnifyDifferentHeadFails`

**Name in spec**: `"decomposeUnifyChildren fails for different-head nodes"`

For any pair with different head constructors (e.g., TyArrow + TyBase): `decomposeUnifyChildren` returns `Left`.

```haskell
it "decomposeUnifyChildren fails for different-head nodes" $ property $
    forAll genDifferentHeadTyNodePair $ \(n1, n2) ->
        case decomposeUnifyChildren n1 n2 of
            Left _ -> property True
            Right edges -> counterexample ("unexpected success: " ++ show edges) (property False)
```

### Generator: `genSameHeadTyNodePair`

```haskell
genSameHeadTyNodePair :: Gen (TyNode, TyNode)
```

Pick a constructor, then generate two nodes of that kind with distinct NodeId children.

### Generator: `genDifferentHeadTyNodePair`

```haskell
genDifferentHeadTyNodePair :: Gen (TyNode, TyNode)
```

Pick two different constructors and generate one of each.

---

## Step 7: Wire into build

### 7a. `mlf2.cabal`

Add to `test-suite mlf2-test` → `other-modules`:
```
Property.QuickCheckPropertySpec,
```

Place it alphabetically near `Parity.FrozenArtifacts` / after `Presolution.*`.

### 7b. `test/Main.hs`

Add:
```haskell
import Property.QuickCheckPropertySpec qualified
```

And in the `hspec $ do` block:
```haskell
Property.QuickCheckPropertySpec.spec
```

Place it after `Reify.CoreSpec.spec` (at the end of the existing list).

---

## Step 8: Verification

Run these commands in order:

1. **Build**: `cabal build all`
   - Must exit 0, no warnings.

2. **Test**: `cabal test --test-show-details=direct`
   - Must exit 0, 0 failures.
   - Grep output for `QuickCheckProperty` to confirm the new spec ran.
   - Count examples: must be ≥ 1274 (baseline) + new properties.

3. **Thesis conformance**: `./scripts/thesis-conformance-gate.sh`
   - Must exit 0.

4. **Property case count**: Verify each `property` test ran at least 100 cases.
   - QuickCheck default is 100; `withMaxSuccess 200` means ≥ 200.
   - Check output lines like `+++ OK, passed 100 tests.` or `+++ OK, passed 200 tests.`

5. **Arbitrary / generator audit**: Confirm every property either:
   - Uses an `Arbitrary` instance (implicit `property $`), OR
   - Uses an explicit `forAll genFoo $ ...` generator.

---

## Summary of Exact Artifacts

| Artifact | Action |
|---|---|
| `test/Property/QuickCheckPropertySpec.hs` | **Create** — new module with all properties + generators |
| `mlf2.cabal` | **Modify** — add `Property.QuickCheckPropertySpec` to `other-modules` |
| `test/Main.hs` | **Modify** — import + wire `Property.QuickCheckPropertySpec.spec` |

### Property inventory (minimum 12 properties)

| # | Deliverable | Property name in spec | Min cases |
|---|---|---|---|
| 1 | (a) | `substTypeCapture preserves free-variable well-formedness` | 100 |
| 2 | (a) | `alphaEqType is reflexive` | 100 |
| 3 | (a) | `alphaEqType is symmetric` | 100 |
| 4 | (a) | `substituting a variable not free in a type is identity` | 100 |
| 5 | (b) | `canonicalization is idempotent (makeCanonicalizer)` | 200 |
| 6 | (b) | `canonicalization is stable (triple-apply)` | 200 |
| 7 | (b) | `canonicalization is deterministic` | 200 |
| 8 | (c) | `genValidBindingTree always produces valid binding trees` | 100 |
| 9 | (c) | `every child in binding tree has a valid parent node` | 100 |
| 10 | (c) | `binding tree has no cycles` | 100 |
| 11 | (c) | `rootedConstraint preserves binding tree validity` | 100 |
| 12 | (d) | `decomposeUnifyChildren is symmetric` | 100 |
| 13 | (d) | `decomposeUnifyChildren succeeds for same-head nodes` | 100 |
| 14 | (d) | `decomposeUnifyChildren fails for different-head nodes` | 100 |

### Generator / Arbitrary inventory

| Name | Target type | Location |
|---|---|---|
| `genElabType :: Int -> Gen ElabType` | `Ty 'AllowVar` | new spec |
| `genBoundType :: Int -> Gen BoundType` | `Ty 'NoTopVar` | new spec |
| `shrinkElabType :: ElabType -> [ElabType]` | `Ty 'AllowVar` | new spec |
| `instance Arbitrary (Ty 'AllowVar)` | — | new spec |
| `genUnionFindMap :: Gen (IntMap NodeId)` | union-find map | new spec (dup from CanonicalizerSpec) |
| `genRedirectMap :: Gen (IntMap NodeId)` | redirect map | new spec (dup from CanonicalizerSpec) |
| `nodeDomain :: IntMap NodeId -> IntMap NodeId -> [NodeId]` | domain helper | new spec (dup from CanonicalizerSpec) |
| `genValidBindingConstraint :: Int -> Gen Constraint` | `Constraint` | new spec (adapted from BindingSpec) |
| `genTyNodePair :: Gen (TyNode, TyNode)` | pair of nodes | new spec |
| `genSameHeadTyNodePair :: Gen (TyNode, TyNode)` | same-head pair | new spec |
| `genDifferentHeadTyNodePair :: Gen (TyNode, TyNode)` | diff-head pair | new spec |

---

## Retry-trigger avoidance checklist

- [ ] No trivial properties (all test real invariants: subset, reflexivity, symmetry, idempotency, cycle-freedom, structural well-formedness)
- [ ] Every property uses explicit `Arbitrary` or `Gen`
- [ ] Every property runs ≥ 100 cases (default 100; deliverable (b) uses 200)
- [ ] ElabType generator produces non-trivial types (arrows, foralls, mu, not just leaves)
- [ ] Shrinking is provided for ElabType to help debug failures
