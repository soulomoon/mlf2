# Round 160 — Review

## Roadmap Identity

- Roadmap ID: `2026-03-30-01-codebase-quality-and-coverage-improvements`
- Roadmap Revision: `rev-001`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001`
- Roadmap Item: `item-1` — "Add test coverage for untested core modules (MLF.Reify.*, MLF.Util.*)"
- Branch: `orchestrator/round-160-test-coverage`
- Worktree: `orchestrator/worktrees/round-160`

---

## Baseline Checks

### 1. Build gate: `cabal build all`

```
$ cabal build all
Build profile: -w ghc-9.12.2 -O1
...
EXIT_CODE=0
```

**Result: PASS** — exit 0, no warnings.

### 2. Test gate: `cabal test --test-show-details=direct`

```
$ cabal test --test-show-details=direct
...
1273 examples, 0 failures
EXIT_CODE=0
```

**Result: PASS** — exit 0, 0 failures, 1273 examples.

### 3. Thesis conformance gate: `./scripts/thesis-conformance-gate.sh`

```
$ ./scripts/thesis-conformance-gate.sh
...
[thesis-gate] PASS: thesis conformance anchors are green
EXIT_CODE=0
```

**Result: PASS** — exit 0, all 107 obligations anchored.

### 4. No regressions: test count ≥ 1177

- Baseline: 1177
- Current: 1273
- Delta: +96 new examples

**Result: PASS** — count increased from 1177 to 1273.

### 5. Cabal module lists

All 6 new `.hs` test files are listed in `mlf2.cabal` → `test-suite mlf2-test` → `other-modules`:
- `Reify.CoreSpec`
- `Reify.NamedSpec`
- `Reify.TypeOpsSpec`
- `Reify.TypeSpec`
- `Util.GraphSpec`
- `Util.UnionFindSpec`

Additionally, 6 library modules were moved from `other-modules` to `exposed-modules` in `mlf2-internal` to allow direct test imports:
- `MLF.Util.ElabError`
- `MLF.Util.Graph`
- `MLF.Reify.TypeOps`
- `MLF.Reify.Named`
- `MLF.Reify.Type`
- `MLF.Reify.Bound`

**Result: PASS**

### 6. Roadmap identity recorded

Verified in this `review.md` and `review-record.json`.

**Result: PASS**

---

## Item-1 Specific Checks

### 1. New spec files wired into `test/Main.hs`

All 6 new imports and spec calls are present in the diff:
- `import Util.UnionFindSpec qualified`
- `import Util.GraphSpec qualified`
- `import Reify.TypeOpsSpec qualified`
- `import Reify.NamedSpec qualified`
- `import Reify.TypeSpec qualified`
- `import Reify.CoreSpec qualified`

Corresponding `*.spec` calls added at the end of the `hspec $ do` block.

**Result: PASS**

### 2. Each new spec has ≥ 3 meaningful examples per exported function

| File | Function | Examples | ≥ 3? |
|------|----------|----------|------|
| `UnionFindSpec.hs` | `frWith` | 4 | ✅ |
| `UnionFindSpec.hs` | `findRootWithCompression` | 4 | ✅ |
| `GraphSpec.hs` | `topoSortBy` | 6 | ✅ |
| `GraphSpec.hs` | `reachableFrom` | 3 | ✅ |
| `GraphSpec.hs` | `reachableFromStop` | 3 | ✅ |
| `TypeOpsSpec.hs` | `splitForalls` | 3 | ✅ |
| `TypeOpsSpec.hs` | `stripForallsType` | 3 | ✅ |
| `TypeOpsSpec.hs` | `freeTypeVarsType` | 4 | ✅ |
| `TypeOpsSpec.hs` | `freeTypeVarsList` | 3 | ✅ |
| `TypeOpsSpec.hs` | `freeTypeVarsFrom` | 3 | ✅ |
| `TypeOpsSpec.hs` | `substTypeCapture` | 3 | ✅ |
| `TypeOpsSpec.hs` | `substTypeSimple` | 3 | ✅ |
| `TypeOpsSpec.hs` | `renameTypeVar` | 3 | ✅ |
| `TypeOpsSpec.hs` | `alphaEqType` | 4 | ✅ |
| `TypeOpsSpec.hs` | `matchType` | 3 | ✅ |
| `TypeOpsSpec.hs` | `freshTypeName` | 3 | ✅ |
| `TypeOpsSpec.hs` | `freshTypeNameFromCounter` | 3 | ✅ |
| `TypeOpsSpec.hs` | `firstNonContractiveRecursiveType` | 3 | ✅ |
| `TypeOpsSpec.hs` | `parseNameId` | 3 | ✅ |
| `NamedSpec.hs` | `namedNodes` | 4 | ✅ |
| `NamedSpec.hs` | `softenedBindParentsUnder` | 3 | ✅ |
| `TypeSpec.hs` | `reifyType` | 4 | ✅ |
| `TypeSpec.hs` | `solvedFromView` | 3 | ✅ |
| `TypeSpec.hs` | `freeVars` | 3 | ✅ |
| `TypeSpec.hs` | `reifyWith` | 3 | ✅ |
| `TypeSpec.hs` | `reifyWithAs` | 3 | ✅ |
| `CoreSpec.hs` | `reifyType` (delegate) | 3 | ✅ |
| `CoreSpec.hs` | `freeVars` (delegate) | 3 | ✅ |
| `CoreSpec.hs` | `namedNodes` (delegate) | 3 | ✅ |

**Result: PASS** — all functions have ≥ 3 meaningful examples.

### 3. Count new test examples

96 new `it` blocks across 6 spec files:
- `UnionFindSpec.hs`: 8
- `GraphSpec.hs`: 12
- `TypeOpsSpec.hs`: 44
- `NamedSpec.hs`: 7
- `TypeSpec.hs`: 16
- `CoreSpec.hs`: 9

**Result: PASS**

---

## Step-by-Step Plan Comparison

| Plan Step | Requirement | Status | Evidence |
|-----------|-------------|--------|----------|
| Step 1 | `test/Util/UnionFindSpec.hs` with `frWith` + `findRootWithCompression` | ✅ | File created, 8 examples, tests pass |
| Step 2 | `test/Util/GraphSpec.hs` with `topoSortBy` + `reachableFrom` + `reachableFromStop` | ✅ | File created, 12 examples, tests pass |
| Step 3 | `test/Reify/TypeOpsSpec.hs` with 13 pure function tests | ✅ | File created, 14 functions tested, 44 examples |
| Step 4 | `test/Reify/NamedSpec.hs` with `namedNodes` + `softenedBindParentsUnder` | ✅ | File created, 7 examples, tests pass |
| Step 5 | `test/Reify/TypeSpec.hs` with `reifyType` + `solvedFromView` + `freeVars` | ✅ | File created, 16 examples, tests pass |
| Step 6 | `test/Reify/CoreSpec.hs` with re-export facade tests | ✅ | File created, 9 examples, tests pass |
| Step 7 | `test/Main.hs` updated with 6 imports + spec calls | ✅ | All 6 present in diff |
| Step 8 | `mlf2.cabal` updated with 6 new modules | ✅ | All 6 in `other-modules` |

---

## Additional Changes (outside plan)

1. **`test/Main.hs` style reformatting**: Import style changed from `import qualified X` to `import X qualified` (postpositive style). Indentation changed from 8-space to 4-space. All imports and spec calls are preserved; this is a cosmetic change.

2. **`mlf2.cabal` exposed-modules promotion**: 6 modules (`MLF.Util.ElabError`, `MLF.Util.Graph`, `MLF.Reify.TypeOps`, `MLF.Reify.Named`, `MLF.Reify.Type`, `MLF.Reify.Bound`) moved from `other-modules` to `exposed-modules` in `mlf2-internal`. This was necessary because `mlf2-test` can only import exposed modules; the plan assumed they were already exposed.

3. **`test/RepoGuardSpec.hs`**: Updated to include the 6 new spec modules in its guardrail module list. This is a correct maintenance change to keep the guardrail wired-modules check passing.

None of these additional changes introduce risk — they are mechanical requirements for the new tests to compile and run.

---

## Decision

**APPROVED**

All baseline checks pass. All plan steps are implemented. All item-1 specific checks pass. Test count increased from 1177 to 1273 (+96 examples). No regressions. The additional changes (style reformatting, module exposure promotion, guardrail updates) are mechanical necessities and do not introduce any behavioral risk.
