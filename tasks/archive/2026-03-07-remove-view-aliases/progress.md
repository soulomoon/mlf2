# Progress Log

## 2026-03-07
- Initialized alias-removal follow-up task.
- Confirmed unsuffixed APIs are already `PresolutionView`-typed; remaining work is caller churn plus duplicate export/definition removal.
- Migrated all remaining callers to the unsuffixed runtime/reify names and removed the duplicate `...View` / `...FromView` definitions/exports.
- Added a source guard in `test/PipelineSpec.hs` that the duplicate alias names are retired from runtime and reify modules.
- Focused verification passes:
  - `ga scope` -> PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator` -> PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved` -> PASS (`1 example, 0 failures`)
  - `duplicate ...View aliases are retired from runtime and reify modules` -> PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard` -> PASS (`1 example, 0 failures`)
  - `checked-authoritative` -> PASS (`8 examples, 0 failures`)
  - `Dual-path verification` -> PASS (`4 examples, 0 failures`)
- Full verification: `cabal build all && cabal test` -> PASS (`970 examples, 0 failures`).
