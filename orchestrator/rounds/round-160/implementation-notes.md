# Round 160 — Implementation Notes

## Summary

Added test coverage for 6 previously-untested core modules in `MLF.Reify.*` and `MLF.Util.*`.

## Changes

### New spec files (6)

| File | Module tested | Functions | Examples |
|------|--------------|-----------|----------|
| `test/Util/UnionFindSpec.hs` | `MLF.Util.UnionFind` | `frWith`, `findRootWithCompression` | 8 |
| `test/Util/GraphSpec.hs` | `MLF.Util.Graph` | `topoSortBy`, `reachableFrom`, `reachableFromStop` | 12 |
| `test/Reify/TypeOpsSpec.hs` | `MLF.Reify.TypeOps` | 13 pure functions (splitForalls, stripForallsType, freeTypeVarsType, freeTypeVarsList, freeTypeVarsFrom, substTypeCapture, substTypeSimple, renameTypeVar, alphaEqType, matchType, freshTypeName, freshTypeNameFromCounter, firstNonContractiveRecursiveType, parseNameId) | ~45 |
| `test/Reify/NamedSpec.hs` | `MLF.Reify.Named` | `namedNodes`, `softenedBindParentsUnder` | ~12 |
| `test/Reify/TypeSpec.hs` | `MLF.Reify.Type` | `reifyType`, `solvedFromView`, `freeVars` | ~12 |
| `test/Reify/CoreSpec.hs` | `MLF.Reify.Core` | Re-export facade consistency | ~7 |

### Modified files (3)

- `test/Main.hs` — Added imports and spec wiring for all 6 new modules
- `mlf2.cabal` — Added 6 modules to `test-suite mlf2-test` `other-modules`
- `test/RepoGuardSpec.hs` — Updated spec-module guardrail list to include new modules

## Verification

- `cabal build all`: exit 0, no warnings
- `cabal test`: 1273 examples, 0 failures (baseline was 1177, +96 new)
- `./scripts/thesis-conformance-gate.sh`: PASS
- Commit: `f1a51b7` on branch `orchestrator/round-160-test-coverage`
