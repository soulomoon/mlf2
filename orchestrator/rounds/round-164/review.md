# Review: Round 164 — Item-5: Research Module Hygiene

**Reviewer**: orchestrator-reviewer
**Date**: 2026-03-31
**Commit**: `d2fdc4f`
**Branch**: `orchestrator/round-164-research-module-hygiene`
**Base**: `codex/automatic-recursive-type-inference`

## Decision: APPROVED

## Verification Results

### Baseline Gates

| Gate | Result | Detail |
|------|--------|--------|
| `cabal build all` | ✅ PASS | Exits 0, no warnings |
| `cabal test` | ✅ PASS | 1288 examples, 0 failures |
| Test count ≥ 1288 | ✅ PASS | Exactly 1288 |
| Roadmap identity | ✅ MATCH | `2026-03-30-01-codebase-quality-and-coverage-improvements` / `rev-001` |

### Item-5 Specific Gates

| Gate | Result | Detail |
|------|--------|--------|
| `cabal build mlf2-internal` (no Research in scope) | ✅ PASS | Up to date, no Research modules needed |
| `cabal build mlf2-research` (Research modules compile) | ✅ PASS | Up to date, all 10 modules build |
| No .hs source files modified | ✅ PASS | All 10 files are `similarity index 100%` renames; 4 promoted modules have zero content diff |

## Scope Review

### Files changed (12 total)

- **`mlf2.cabal`**: +38 −17 lines — new `mlf2-research` stanza, Research modules removed from `mlf2-internal`, 4 modules promoted from `other-modules` to `exposed-modules`, `mlf2:mlf2-research` added to executable and test-suite `build-depends`
- **`AGENTS.md`**: +1 line — new bullet documenting `mlf2-research` organization
- **10 .hs files**: Pure `rename` (100% similarity) from `src/MLF/Research/` → `src-research/MLF/Research/`

### No scope creep

- Only `mlf2.cabal` and `AGENTS.md` have content changes.
- No Haskell source content was modified.
- No unrelated modules touched.

## Design Notes

The implementer deviated from the plan in one beneficial way: instead of sharing `hs-source-dirs: src` (as the plan initially suggested), the files were physically moved to a new `src-research/` directory. This provides clean physical separation between the core library and research modules — a better outcome than the plan's original approach.

The 4 modules promoted from `other-modules` to `exposed-modules` in `mlf2-internal` (`Presolution.Base`, `Elab.Run.Annotation`, `Elab.Run.Generalize`, `Elab.Types`) are necessary because Research modules import them, and Cabal requires them to be exposed for cross-library dependency resolution. No content changes were needed.

## Conclusion

Clean, low-risk Cabal reorganization. All verification gates pass. No regressions, no scope creep, no source modifications. Approved for merge.
