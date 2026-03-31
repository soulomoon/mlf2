# Round 166 — Item 7: Golden Test Expansion — Review

## Decision: **APPROVED**

## Baseline gate results

| Gate | Command | Exit code | Result |
|------|---------|-----------|--------|
| Build | `cabal build all` | 0 | PASS — no warnings |
| Test | `cabal test` | 0 | PASS — 1296 examples, 0 failures |
| Thesis conformance | `./scripts/thesis-conformance-gate.sh` | 0 | PASS |

## No-regression check

- Pre-round baseline: 1288 tests
- Post-implementation: 1296 tests (+8 golden tests)
- ✅ 1296 ≥ 1288

## Cabal module list check

- `GoldenSpec` appears in `mlf2.cabal` `other-modules` at line 421: ✅
- `GoldenSpec` imported in `test/Main.hs` at line 20, called at line 112: ✅

## Roadmap identity check

`selection.md` metadata:
- `roadmap_id`: `2026-03-30-01-codebase-quality-and-coverage-improvements` ✅
- `roadmap_revision`: `rev-001` ✅
- `roadmap_item_id`: `item-7` ✅

## Task-specific checks

### 1. Golden files checked in under `test/golden/` (8 files: 5 xmlf + 3 constraint)

| File | Size | Status |
|------|------|--------|
| `test/golden/xmlf-identity.golden` | 96 bytes | ✅ |
| `test/golden/xmlf-church-true.golden` | 178 bytes | ✅ |
| `test/golden/xmlf-poly-let.golden` | 186 bytes | ✅ |
| `test/golden/xmlf-simple-app.golden` | 73 bytes | ✅ |
| `test/golden/xmlf-choose.golden` | 173 bytes | ✅ |
| `test/golden/constraint-identity.golden` | 168 bytes | ✅ |
| `test/golden/constraint-poly-let.golden` | 348 bytes | ✅ |
| `test/golden/constraint-choose.golden` | 201 bytes | ✅ |

All 8 files exist and are non-empty. ✅

### 2. `--accept` workflow documented

`{- Note [Golden test workflow] -}` block present at lines 3–12 of `test/GoldenSpec.hs`, documenting `GOLDEN_ACCEPT=1 cabal test` workflow. ✅

### 3. At least 5 canonical xMLF pipeline examples tested

5 xMLF golden tests: identity, church-true, poly-let, simple-app, choose. ✅

### 4. At least 3 constraint graph summary tests

3 constraint golden tests: identity, poly-let, choose. ✅

### 5. `GOLDEN_ACCEPT=1` mechanism

`goldenTest` helper at lines 76–86:
- Checks `lookupEnv "GOLDEN_ACCEPT"`
- On `Just "1"`: creates directory and writes actual output to golden file
- Otherwise: reads golden file, forces evaluation (`length expected \`seq\``), and compares with `shouldBe`
- Uses `System.FilePath.takeDirectory` (from `filepath` already in `build-depends`)

Mechanism is correct and complete. ✅

### 6. No production code modified

Files changed in diff:
- `mlf2.cabal` (test-suite `other-modules` only)
- `test/GoldenSpec.hs` (new)
- `test/Main.hs` (import + spec call)
- 8 golden files under `test/golden/`

No files under `src/`, `src-public/`, `src-research/`, or `app/` were modified. ✅

## Plan compliance

### Step-by-step comparison

| Plan step | Status | Notes |
|-----------|--------|-------|
| Step 1: Create `test/GoldenSpec.hs` | ✅ | Module structure, imports, helpers, test expressions all present |
| Step 2: Generate golden files | ✅ | 8 golden files generated and checked in |
| Step 3: Wire into `mlf2.cabal` | ✅ | `GoldenSpec` added to `other-modules` |
| Step 4: Wire into `test/Main.hs` | ✅ | Import and spec call added |
| Step 5: Full verification gate | ✅ | `cabal build all && cabal test` passes |

### Acceptable deviation: rank2-app → simple-app

The plan specified `rank2AppExpr` (passing identity to a function requiring polymorphic argument). This fails the pipeline with `TCArgumentMismatch` because lambda-bound parameters are inferred monomorphically without annotation.

The implementer replaced it with `simpleAppExpr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))` — identity applied to a literal. This is documented in `implementation-notes.md` with clear rationale. The substitution is **acceptable**: the expression still exercises the application path through the pipeline, and the plan's intent (5 canonical examples) is fulfilled.

### Constraint summary determinism

`constraintSummary` at lines 111–135:
- Extracts node list from `cNodes` via `toListNode`
- Maps each `(NodeId, TyNode)` to `(Int, String)` via `getNodeId` + `nodeTag`
- **Sorts** the mapped list by `(Int, String)` — deterministic since both components are `Ord`
- Pattern match on `nodeTag` is total over all 8 `TyNode` constructors (`TyVar`, `TyBottom`, `TyArrow`, `TyBase`, `TyCon`, `TyForall`, `TyExp`, `TyMu`) — `-Wall` would catch any missing constructor

Output is deterministic. ✅

## Summary

All three gates pass. All 8 golden files are checked in and non-empty. The `GOLDEN_ACCEPT=1` workflow is documented and correctly implemented. The plan's rank2-app → simple-app deviation is documented and reasonable. No production code was modified. Test count increased from 1288 to 1296.
