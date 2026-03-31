# Round 167 — Plan: Public API Enrichment (item-8)

**Branch**: `orchestrator/round-167-public-api-enrichment`
**Base**: `codex/automatic-recursive-type-inference` @ `9ec1618`
**Baseline**: 1296 examples, 0 failures

---

## Inventory of what already exists

| Symbol | Location | Status |
|--------|----------|--------|
| `PipelineConfig(..)` | `MLF.Elab.PipelineConfig` → re-exported by `MLF.Pipeline` | ✅ exists |
| `defaultPipelineConfig` | same chain | ✅ exists |
| `TraceConfig(..)` / `defaultTraceConfig` | `MLF.Util.Trace` → re-exported | ✅ exists |
| `PipelineError(..)` (6 constructors) | `MLF.Elab.PipelineError` → re-exported | ✅ exists |
| `renderPipelineError :: PipelineError -> String` | same chain | ✅ exists |
| `runPipelineElabWithConfig` / `...CheckedWithConfig` | `MLF.Elab.Run` → re-exported | ✅ exists |
| `inferConstraintGraph` | `MLF.Pipeline` (delegates to `generateConstraints`) | ✅ exists |
| `ConstraintResult(..)` | `MLF.Pipeline` (re-export from `MLF.Frontend.ConstraintGen`) | ✅ exists |
| `Constraint` type (opaque) | NOT exported by any public module | ❌ missing |
| `NodeId`, `TyNode`, `InstEdge` | NOT in public modules | ❌ missing |
| `formatPipelineError :: PipelineError -> Text` | — | ❌ missing |
| Introspection helpers on `Constraint` | — | ❌ missing |

## Deliverables

1. **`formatPipelineError :: PipelineError -> Text`** in `MLF.Pipeline` — structured, multi-line, `Text`-based error formatting.
2. **`pipelineErrorPhase :: PipelineError -> Int`** in `MLF.Pipeline` — extract the phase number (1/3/4/5/6/7).
3. **`pipelineErrorPhaseName :: PipelineError -> String`** in `MLF.Pipeline` — human-readable phase name.
4. **Constraint graph introspection** in `MLF.API` — opaque `Constraint` export plus helper functions and key graph types.
5. **Haddock doc-comments** for every new export.
6. **Tests** — at least one per new function.

---

## Step 1 — Add `text` dependency to the public library and test suite

**File**: `mlf2.cabal`

### Changes

1. In the `library` stanza (line ~297), add `text` to `build-depends`:
   ```
   build-depends:    base ^>=4.21.0.0,
                     containers,
                     mtl,
                     text,
                     mlf2:mlf2-internal
   ```

2. In the `test-suite mlf2-test` stanza (line ~436), add `text` to `build-depends`:
   ```
   build-depends:
       base ^>=4.21.0.0,
       containers,
       directory,
       filepath,
       mtl,
       text,
       mlf2,
       ...
   ```

**Verification**: `cabal build all` (no test changes yet, just dependency check).

---

## Step 2 — Add `formatPipelineError`, `pipelineErrorPhase`, `pipelineErrorPhaseName` to `MLF.Pipeline`

**File**: `src-public/MLF/Pipeline.hs`

### New imports

```haskell
import Data.Text (Text)
import qualified Data.Text as T
```

### New export entries (add under `-- * Pipeline entrypoints` section)

```haskell
    , formatPipelineError
    , pipelineErrorPhase
    , pipelineErrorPhaseName
```

### New function definitions (append after the existing `inferConstraintGraph` definition)

```haskell
-- | Extract the numeric pipeline phase where the error occurred.
--
-- Phase mapping:
--
--   * 1 — Constraint generation
--   * 3 — Acyclicity check
--   * 4 — Presolution
--   * 5 — Solve (unification)
--   * 6 — Elaboration
--   * 7 — Type checking
pipelineErrorPhase :: PipelineError -> Int
pipelineErrorPhase err = case err of
    PipelineConstraintError{}  -> 1
    PipelineCycleError{}       -> 3
    PipelinePresolutionError{} -> 4
    PipelineSolveError{}       -> 5
    PipelineElabError{}        -> 6
    PipelineTypeCheckError{}   -> 7

-- | Human-readable name of the pipeline phase where the error occurred.
pipelineErrorPhaseName :: PipelineError -> String
pipelineErrorPhaseName err = case err of
    PipelineConstraintError{}  -> "constraint generation"
    PipelineCycleError{}       -> "acyclicity check"
    PipelinePresolutionError{} -> "presolution"
    PipelineSolveError{}       -> "solve"
    PipelineElabError{}        -> "elaboration"
    PipelineTypeCheckError{}   -> "type checking"

-- | Structured, multi-line 'Text' rendering of a 'PipelineError'.
--
-- Returns output in the format:
--
-- @
-- [Phase N] phase-name error:
--   \<detail from Show instance\>
-- @
--
-- Use 'renderPipelineError' for a single-line 'String' alternative.
formatPipelineError :: PipelineError -> Text
formatPipelineError err = T.pack $
    "[Phase " ++ show (pipelineErrorPhase err) ++ "] "
    ++ pipelineErrorPhaseName err ++ " error:\n  "
    ++ detail err
  where
    detail e = case e of
        PipelineConstraintError ce  -> show ce
        PipelineCycleError ce       -> show ce
        PipelinePresolutionError pe -> show pe
        PipelineSolveError se       -> show se
        PipelineElabError ee        -> show ee
        PipelineTypeCheckError te   -> show te
```

### Haddock requirements

- Each function has a Haddock `-- |` comment as shown above.
- `pipelineErrorPhase` documents the phase-number mapping.
- `formatPipelineError` shows the output format in a `@...@` code block and cross-references `renderPipelineError`.

**Verification**: `cabal build all`

---

## Step 3 — Add constraint graph introspection to `MLF.API`

**File**: `src-public/MLF/API.hs`

### New imports

```haskell
import MLF.Constraint.Types.Graph
    ( Constraint (..)
    , NodeId (..)
    , TyNode (..)
    , InstEdge (..)
    , UnifyEdge (..)
    , GenNode (..)
    , GenNodeId (..)
    , BindFlag (..)
    , BindParents
    , NodeMap
    , GenNodeMap
    , lookupNode
    , toListNode
    )
import qualified Data.IntMap.Strict as IntMap
```

### New export section

Add a new section to the export list:

```haskell
    -- * Constraint graph introspection
    , Constraint (..)
    , NodeId (..)
    , TyNode (..)
    , InstEdge (..)
    , UnifyEdge (..)
    , GenNode (..)
    , GenNodeId (..)
    , BindFlag (..)
    , BindParents
    , NodeMap
    , GenNodeMap
    , lookupNode
    , constraintNodeCount
    , constraintEdgeCount
```

### New function definitions

```haskell
-- | Number of type nodes in a constraint graph.
constraintNodeCount :: Constraint -> Int
constraintNodeCount = length . toListNode . cNodes

-- | Total number of edges (instantiation + unification) in a constraint graph.
constraintEdgeCount :: Constraint -> Int
constraintEdgeCount c = length (cInstEdges c) + length (cUnifyEdges c)
```

### Haddock requirements

- Module header updated to mention constraint graph introspection:
  > `MLF.API` is the frontend-oriented umbrella API for downstream callers that
  > want surface syntax, parsing/pretty-printing, normalization helpers, and
  > constraint graph introspection.
- Section header `-- * Constraint graph introspection` with doc comment on the section.
- Each new function gets a `-- |` Haddock comment.

**Verification**: `cabal build all`

---

## Step 4 — Add tests for all new exports

**File**: `test/PublicSurfaceSpec.hs`

### New imports

```haskell
import Data.Text (Text)
import qualified Data.Text as T
import qualified MLF.API as API
```

### New test cases

Add these inside the existing `describe "Public surface contracts"` block:

```haskell
    describe "MLF.Pipeline (error formatting)" $ do
        it "formatPipelineError produces structured Text output" $ do
            let err = Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x")
                formatted = Pipeline.formatPipelineError err
            T.unpack formatted `shouldContain` "[Phase 1]"
            T.unpack formatted `shouldContain` "constraint generation"
            T.unpack formatted `shouldContain` "UnknownVariable"

        it "pipelineErrorPhase returns correct phase numbers" $ do
            Pipeline.pipelineErrorPhase
                (Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x"))
                `shouldBe` 1
            Pipeline.pipelineErrorPhase
                (Pipeline.PipelineCycleError
                    (Pipeline.CycleError [] "test"))
                `shouldBe` 3

        it "pipelineErrorPhaseName returns human-readable names" $ do
            Pipeline.pipelineErrorPhaseName
                (Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x"))
                `shouldBe` "constraint generation"

    describe "MLF.API (constraint graph introspection)" $ do
        it "constraintNodeCount returns a positive count for a real graph" $ do
            expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
                expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
                    let c = API.crConstraint cr
                    API.constraintNodeCount c `shouldSatisfy` (> 0)

        it "constraintEdgeCount returns a non-negative count" $ do
            expectRight (parseNormEmlfExpr "let id = λ(x) x in id 1") $ \expr ->
                expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
                    let c = API.crConstraint cr
                    API.constraintEdgeCount c `shouldSatisfy` (>= 0)

        it "lookupNode retrieves the root node from a constraint graph" $ do
            expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
                expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
                    let c = API.crConstraint cr
                        rootId = API.crRoot cr
                    API.lookupNode rootId (API.cNodes c) `shouldSatisfy` \case
                        Nothing -> False
                        Just _  -> True
```

**Note on imports**: `ConstraintResult` fields (`crConstraint`, `crRoot`) are already exported by `Pipeline` via `ConstraintResult(..)`. But since we now re-export them from `API` too (via `Constraint(..)`), the test can use either module. Use `API.crConstraint` etc. through the `API` qualified import to specifically test the `MLF.API` surface. However `crConstraint`, `crRoot` are not fields of `Constraint` — they're fields of `ConstraintResult` which is exported by `MLF.Pipeline`. The test should get `ConstraintResult` from `Pipeline.inferConstraintGraph` and then use `API.constraintNodeCount` on the constraint inside it.

Actually, `ConstraintResult` is exported by `MLF.Pipeline`, not `MLF.API`. The introspection functions in `MLF.API` take `Constraint` values. The test should:
1. Get `ConstraintResult` from `Pipeline.inferConstraintGraph`
2. Extract `crConstraint` from the result
3. Pass it to `API.constraintNodeCount`

Since `ConstraintResult(..)` is exported by `MLF.Pipeline`, the test imports it from there.

### Haddock requirements

No Haddock needed in tests.

### Test module wiring

`PublicSurfaceSpec` is already wired into `mlf2.cabal` and `test/Main.hs` — no changes needed.

**Verification**: `cabal build all && cabal test`

---

## Step 5 — Add `CycleError(..)` to `MLF.Pipeline` exports

The test in Step 4 constructs a `PipelineCycleError (CycleError [] "test")`. This requires `CycleError(..)` to be importable. Currently `MLF.Pipeline` exports `PipelineError(..)` which includes `PipelineCycleError` (taking a `CycleError`), but `CycleError` itself is NOT in the export list.

**File**: `src-public/MLF/Pipeline.hs`

### Changes

1. Add import:
   ```haskell
   import MLF.Constraint.Acyclicity (CycleError(..))
   ```
2. Add to export list under `-- * Pipeline entrypoints`:
   ```haskell
       , CycleError (..)
   ```
3. Add Haddock: the type already has doc-comments in the source; re-export is sufficient.

**Verification**: `cabal build all`

---

## Step 6 — Final verification gate

```bash
cabal build all && cabal test
```

All 1296+ examples must pass with 0 failures. Build must be warning-free.

---

## Summary of files modified

| File | Change |
|------|--------|
| `mlf2.cabal` | Add `text` to `library` and `mlf2-test` `build-depends` |
| `src-public/MLF/Pipeline.hs` | Add `formatPipelineError`, `pipelineErrorPhase`, `pipelineErrorPhaseName`, `CycleError(..)` export |
| `src-public/MLF/API.hs` | Add constraint graph type re-exports + `constraintNodeCount`, `constraintEdgeCount` |
| `test/PublicSurfaceSpec.hs` | Add 6 new test cases covering all new exports |

## Scope boundaries

- **In scope**: New exports, Haddock, tests for the public API surface.
- **Out of scope**: Behavioral changes to pipeline internals, changes to `renderPipelineError`, new error constructors, changes to `PipelineConfig` fields (already sufficient per `TraceConfig`).
- **Design decision**: `formatPipelineError` lives in `src-public/MLF/Pipeline.hs` (not in `mlf2-internal`) to avoid adding `text` to the internal library's dependencies. This keeps the `text` dependency scoped to the public layer only.
- **Design decision**: `Constraint(..)` is exported with record fields so consumers can access `cNodes`, `cInstEdges`, etc. directly. The alternative (opaque + accessor functions) was rejected because `ConstraintResult(..)` already exposes the `Constraint` value and consumers need field access for meaningful introspection.
