# Round 163 — Task Selection

## Selected Item

**item-4**: Decompose large modules (>800 lines)

## Why Now

- Dependencies satisfied: item-1 ✅ and item-3 ✅ are both completed.
- Merge prerequisite met: item-3 completed in round-162.
- Items 1–3 established a QuickCheck property suite and type-safety foundation that provides a solid regression net — any accidental behavioral change from module splitting will be caught immediately by the 1288-example test baseline.
- This is a pure structural refactor (no behavioral changes), making it the lowest-risk item to run next and a clean predecessor for any items that may touch the same modules later.
- All five target modules exceed the 800-line threshold and are candidates for splitting into focused submodules with re-export facades.

## Roadmap Identity

| Field              | Value                                                                                              |
|--------------------|----------------------------------------------------------------------------------------------------|
| roadmap_id         | 2026-03-30-01-codebase-quality-and-coverage-improvements                                           |
| roadmap_revision   | rev-001                                                                                            |
| roadmap_dir        | orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001            |
| roadmap_item_id    | item-4                                                                                             |

## Test Baseline

- **1288 examples, 0 failures** (as of round-162 / commit `c98d1a8`)
- Branch: `codex/automatic-recursive-type-inference`

## Deliverable Summary

Split the top 5 non-research modules by line count:

| Module                                 | Current Lines |
|----------------------------------------|---------------|
| Elab.Phi.Omega.Interpret               | 1226          |
| Constraint.Normalize                   | 848           |
| Reify.Type                             | 822           |
| Elab.Run.ResultType.Fallback           | 822           |
| Constraint.Presolution.Plan            | 821           |

Each split must:
1. Preserve the original module as a re-export facade (target: <200 lines per facade).
2. Register new submodules in `mlf2.cabal` (`other-modules` / `exposed-modules` as appropriate).
3. Produce no behavioral changes.

## Verification Gate

```
cabal build all && cabal test
```

Pass criteria: identical test results (1288 examples, 0 failures), each split parent module <200 lines, new submodules listed in `mlf2.cabal`.
