# Round 166 — Item 7: Golden Test Expansion — Implementation Plan

## Summary

Add golden tests that snapshot (a) pretty-printed xMLF output for 5 canonical pipeline
examples and (b) constraint graph summary dumps for 3 examples. Golden files live under
`test/golden/`. A simple `GOLDEN_ACCEPT=1` workflow regenerates them. The new spec is
wired into `test/Main.hs` and `mlf2.cabal`.

---

## Key API surfaces (from codebase analysis)

### xMLF pretty-printing path

```
runPipelineElab :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
```

- `pretty :: ElabTerm -> String` — defined in `MLF.Elab.Types`, uses `prettyXmlfTerm . toXmlfTerm`
  for most constructors, custom rendering for `ELet`/`ERoll`/`EUnroll`.
- `pretty :: ElabType -> String` — defined in `MLF.Elab.Types`, uses `prettyXmlfType . toXmlfType`.
- Both imported via `MLF.Elab.Pipeline (Pretty(..))` which `MLF.Pipeline` re-exports.

### Constraint graph generation path

```
inferConstraintGraph :: PolySyms -> NormSurfaceExpr -> Either ConstraintError ConstraintResult
```

- `ConstraintResult` exposes `crConstraint :: Constraint`, `crRoot :: NodeId`.
- `Constraint` fields: `cNodes`, `cInstEdges`, `cUnifyEdges`, `cBindParents`, `cGenNodes`.
- All types derive `Show`. No existing summary function — plan defines one in the spec.

### Normalization

- `unsafeNormalizeExpr :: SurfaceExpr -> NormSurfaceExpr` (from `SpecUtil`).

---

## Step 1 — Create `test/GoldenSpec.hs`

**File**: `test/GoldenSpec.hs` (new)

### Module structure

```haskell
module GoldenSpec (spec) where

import Data.List (intercalate, sort)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import System.Directory (createDirectoryIfMissing)
import Test.Hspec

import MLF.Frontend.Syntax
import MLF.Pipeline
    ( ElabTerm, ElabType, Pretty(..), PolySyms
    , runPipelineElab, inferConstraintGraph
    , ConstraintResult(..)
    )
import MLF.Constraint.Types.Graph
    ( Constraint(..), NodeId(..), TyNode(..), InstEdge(..)
    , toListNode, getNodeId, BaseTy(..)
    )
import SpecUtil (unsafeNormalizeExpr)
```

### 5 canonical test expressions

Define at module level:

| Name | Expression | Description |
|------|-----------|-------------|
| `identityExpr` | `ELam "x" (EVar "x")` | `λx. x` — identity |
| `churchTrueExpr` | `ELam "t" (ELam "f" (EVar "t"))` | `λt. λf. t` — Church true |
| `polyLetExpr` | `ELet "id" (ELam "x" (EVar "x")) (ELet "a" (EApp (EVar "id") (ELit (LInt 1))) (EApp (EVar "id") (ELit (LBool True))))` | polymorphic let — id used at two types |
| `rank2AppExpr` | `EApp (ELam "f" (ELet "a" (EApp (EVar "f") (ELit (LInt 1))) (EApp (EVar "f") (ELit (LBool True))))) (ELam "x" (EVar "x"))` | rank-2 application — pass identity to a function requiring poly arg |
| `chooseExpr` | `ELam "x" (ELam "y" (EVar "x"))` | `λx. λy. x` — choose (always-first) |

### Golden comparison helper

```haskell
-- | Compare actual output against a golden file.
-- When GOLDEN_ACCEPT=1 is set, overwrite the golden file with actual output.
goldenTest :: FilePath -> String -> Expectation
goldenTest goldenPath actual = do
    accept <- lookupEnv "GOLDEN_ACCEPT"
    case accept of
        Just "1" -> do
            createDirectoryIfMissing True (takeDirectory goldenPath)
            writeFile goldenPath actual
        _ -> do
            expected <- readFile goldenPath
            -- Force full evaluation to avoid lazy IO handle issues
            length expected `seq` actual `shouldBe` expected

-- | Take the directory component of a file path (simple, no filepath dep needed
-- since filepath is already in build-depends).
takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (/= '/') . reverse
```

**Note**: `filepath` is already in the test `build-depends` (via `directory` dependency chain), but the plan uses a simple inline `takeDirectory` to avoid adding an import. Alternatively, `import System.FilePath (takeDirectory)` can be used since `filepath` is already a transitive dependency and is listed in `mlf2-test`'s `build-depends`.

Actually, looking at the cabal file again: `filepath` is listed in `build-depends` for `mlf2-test` (line 439). So `import System.FilePath (takeDirectory)` is preferred.

### Part (a): xMLF pretty-print golden tests (5 examples)

For each of the 5 expressions:
1. Normalize with `unsafeNormalizeExpr`
2. Run through `runPipelineElab Set.empty`
3. Produce a deterministic output string: `"term: " ++ pretty term ++ "\ntype: " ++ pretty ty ++ "\n"`
4. Compare against golden file

```haskell
spec :: Spec
spec = describe "Golden tests" $ do
    describe "xMLF pretty-print output" $ do
        goldenXmlfTest "identity"    identityExpr
        goldenXmlfTest "church-true" churchTrueExpr
        goldenXmlfTest "poly-let"    polyLetExpr
        goldenXmlfTest "rank2-app"   rank2AppExpr
        goldenXmlfTest "choose"      chooseExpr

    describe "Constraint graph summaries" $ do
        goldenConstraintTest "identity"  identityExpr
        goldenConstraintTest "poly-let"  polyLetExpr
        goldenConstraintTest "choose"    chooseExpr

goldenXmlfTest :: String -> SurfaceExpr -> Spec
goldenXmlfTest name expr = it ("golden xMLF output: " ++ name) $ do
    let normExpr = unsafeNormalizeExpr expr
    case runPipelineElab Set.empty normExpr of
        Left err -> expectationFailure ("Pipeline failed: " ++ show err)
        Right (term, ty) -> do
            let output = unlines
                    [ "-- xMLF golden output for: " ++ name
                    , "term: " ++ pretty term
                    , "type: " ++ pretty ty
                    ]
            goldenTest ("test/golden/xmlf-" ++ name ++ ".golden") output
```

### Part (b): Constraint graph summary golden tests (3 examples)

Define a deterministic constraint summary function:

```haskell
constraintSummary :: Constraint -> NodeId -> String
constraintSummary c root = unlines
    [ "root: " ++ show (getNodeId root)
    , "node-count: " ++ show (length nodeList)
    , "inst-edge-count: " ++ show (length (cInstEdges c))
    , "unify-edge-count: " ++ show (length (cUnifyEdges c))
    , "bind-parent-count: " ++ show (IntMap.size (cBindParents c))
    , "nodes:"
    , unlines (map showNode (sort (map (\(nid, n) -> (getNodeId nid, nodeTag n)) nodeList)))
    ]
  where
    nodeList = toListNode (cNodes c)
    nodeTag :: TyNode -> String
    nodeTag n = case n of
        TyVar{}    -> "TyVar"
        TyArrow{}  -> "TyArrow"
        TyForall{} -> "TyForall"
        TyBase _ (BaseTy b)  -> "TyBase(" ++ b ++ ")"
        TyExp{}    -> "TyExp"
        TyBottom{} -> "TyBottom"
        TyCon{}    -> "TyCon"
        TyMu{}     -> "TyMu"
    showNode (nid, tag) = "  " ++ show nid ++ ": " ++ tag

goldenConstraintTest :: String -> SurfaceExpr -> Spec
goldenConstraintTest name expr = it ("golden constraint summary: " ++ name) $ do
    let normExpr = unsafeNormalizeExpr expr
    case inferConstraintGraph Set.empty normExpr of
        Left err -> expectationFailure ("Constraint gen failed: " ++ show err)
        Right ConstraintResult{ crConstraint = c, crRoot = root } -> do
            let output = unlines
                    [ "-- Constraint graph summary for: " ++ name
                    , constraintSummary c root
                    ]
            goldenTest ("test/golden/constraint-" ++ name ++ ".golden") output
```

### Comments documenting the `--accept` workflow

Add at the top of the module (below module declaration):

```haskell
{- Note [Golden test workflow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These tests compare pipeline output against checked-in golden files under
test/golden/. To regenerate golden files after an intentional output change:

    GOLDEN_ACCEPT=1 cabal test

This overwrites each .golden file with the current output. Review the diff
with `git diff test/golden/` before committing.
-}
```

**Verification**: `cabal build all && cabal test` after wiring (Steps 2–4).

---

## Step 2 — Generate initial golden files

**Command** (run from worktree root):

```bash
GOLDEN_ACCEPT=1 cabal test --test-show-details=direct
```

This creates 8 golden files:

| Golden file path | Content |
|-----------------|---------|
| `test/golden/xmlf-identity.golden` | xMLF term + type for `λx. x` |
| `test/golden/xmlf-church-true.golden` | xMLF term + type for `λt. λf. t` |
| `test/golden/xmlf-poly-let.golden` | xMLF term + type for polymorphic let |
| `test/golden/xmlf-rank2-app.golden` | xMLF term + type for rank-2 application |
| `test/golden/xmlf-choose.golden` | xMLF term + type for `λx. λy. x` |
| `test/golden/constraint-identity.golden` | Constraint summary for `λx. x` |
| `test/golden/constraint-poly-let.golden` | Constraint summary for polymorphic let |
| `test/golden/constraint-choose.golden` | Constraint summary for `λx. λy. x` |

**Verification**: All 8 files exist and are non-empty. `git diff test/golden/` shows only new files.

---

## Step 3 — Wire into `mlf2.cabal`

**File**: `mlf2.cabal`

**Change**: Add `GoldenSpec` to the `other-modules` list of `test-suite mlf2-test`.

Insert after `SpecUtil,` (currently the last entry before the blank line):

```
                      GoldenSpec
```

The trailing comma on `SpecUtil,` already exists (line 420); add `GoldenSpec` without a trailing comma (it becomes the new last entry). Alternatively, add a comma after `SpecUtil,` and put `GoldenSpec` on the next line — match the existing pattern where each module has a trailing comma.

Concretely, change:

```
                      SpecUtil,
```

to:

```
                      SpecUtil,
                      GoldenSpec
```

**Verification**: `cabal build mlf2-test` succeeds.

---

## Step 4 — Wire into `test/Main.hs`

**File**: `test/Main.hs`

**Changes**:

1. Add import (alphabetical position, after `FrontendDesugarSpec qualified`):

```haskell
import GoldenSpec qualified
```

2. Add spec call inside the `hspec $ do` block, after `Reify.CoreSpec.spec` and before the end:

```haskell
    GoldenSpec.spec
```

Position suggestion: after `Property.QuickCheckPropertySpec.spec` (line 110), which is currently the last spec call. Add:

```haskell
    GoldenSpec.spec
```

**Verification**: `cabal test` runs golden tests and they pass.

---

## Step 5 — Full verification gate

```bash
cabal build all && cabal test
```

**Expected**: All tests pass (existing 1288 + 8 new golden tests). No warnings from `GoldenSpec`.

**Also verify**:
- `git status` shows new files:
  - `test/GoldenSpec.hs`
  - `test/golden/xmlf-identity.golden`
  - `test/golden/xmlf-church-true.golden`
  - `test/golden/xmlf-poly-let.golden`
  - `test/golden/xmlf-rank2-app.golden`
  - `test/golden/xmlf-choose.golden`
  - `test/golden/constraint-identity.golden`
  - `test/golden/constraint-poly-let.golden`
  - `test/golden/constraint-choose.golden`
- Modified files: `mlf2.cabal`, `test/Main.hs`
- No production code modified

---

## Implementation notes

### nodeTag pattern match completeness

The `nodeTag` helper in `constraintSummary` must cover all `TyNode` constructors.
Current constructors (from `MLF.Constraint.Types.Graph.NodeEdge`):
`TyVar`, `TyArrow`, `TyForall`, `TyBase`, `TyExp`, `TyBottom`, `TyCon`, `TyMu`.

If a constructor is missing, `-Wall` will catch it since the test suite inherits `warnings`.

### TyBase nodeTag

`TyBase` carries `NodeId` and `BaseTy` — use `TyBase _ (BaseTy b) -> "TyBase(" ++ b ++ ")"`.
Actual field pattern: `TyBase { tnId :: NodeId, tnBase :: BaseTy }` or a positional match.
Verify the exact constructor shape from `NodeEdge.hs` and adjust accordingly.

### Determinism

- Node list is sorted by `NodeId` (Int) before serialization → deterministic.
- `pretty` on `ElabTerm`/`ElabType` is deterministic (no fresh name generation involved).
- Constraint graph structure is deterministic for a given input expression.

### Imports needed from SpecUtil

Only `unsafeNormalizeExpr` is needed from `SpecUtil`. All other imports come from
`MLF.Pipeline` (public API) and `MLF.Constraint.Types.Graph` (internal, already exposed).

### No new dependencies

The plan uses only `System.Environment` (from `base`), `System.Directory` (already in
`build-depends`), and `System.FilePath` (already in `build-depends`). No new packages.

---

## Scope boundary

This plan:
- ✅ Creates golden tests for 5 xMLF pretty-print outputs
- ✅ Creates golden tests for 3 constraint graph summaries
- ✅ Stores golden files under `test/golden/`
- ✅ Documents `GOLDEN_ACCEPT=1` workflow
- ✅ Wires into test suite (`Main.hs` + `mlf2.cabal`)
- ❌ Does NOT modify production code
- ❌ Does NOT add new dependencies
- ❌ Does NOT change the roadmap
