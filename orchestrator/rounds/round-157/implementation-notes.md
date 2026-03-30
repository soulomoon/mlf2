# Round 157 — Implementation Notes

## Change Summary

**File changed:** `.github/workflows/thesis-conformance.yml` (1 file, +35 −3)

Replaced the single monolithic `thesis-conformance` job with two matrix-parameterized jobs:

1. **`build-and-test`** — `cabal build all` + `cabal test` (~2–3 min). Provides fast regression signal.
2. **`thesis-conformance`** — `cabal build all` + `./scripts/thesis-conformance-gate.sh`. Full thesis-anchor coverage. Depends on `build-and-test` via `needs:`.

Both jobs declare identical `strategy.matrix` blocks (`os: [ubuntu-latest]`, `ghc: ['9.12.2']`), making lane expansion a YAML-only edit.

## Verification

| Gate | Result |
|------|--------|
| YAML syntax (`python3 yaml.safe_load`) | ✅ Pass |
| `cabal build all` | ✅ Pass |
| `cabal test` (1176 examples) | ✅ Pass, 0 failures |
| `./scripts/thesis-conformance-gate.sh` | ✅ Pass |

## Known Limitations

- The `thesis-conformance` job re-runs `cabal build all` because GitHub Actions jobs run on independent runners with no shared filesystem. Acceptable for a 1×1 matrix.
- Absolute paths in `docs/thesis-obligations.yaml` will cause the thesis-conformance job to fail on CI runners (item-3 scope, not addressed here).
- No Cabal dependency caching added (future optimization).

## Commit

```
00d7655 Add bounded CI matrix: split into build-and-test and thesis-conformance jobs
```
