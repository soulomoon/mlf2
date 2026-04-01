# GHC 9.14 Upgrade

## Goal

Update the project so the declared local and CI compiler lane is GHC 9.14,
including any dependency bounds, source fixes, and documentation updates
required for a clean build and test run on that compiler.

## Scope

- Update compiler pins and supporting toolchain references together.
- Prefer root-cause compatibility fixes over temporary compatibility shims.
- Keep thesis-faithfulness and runtime behavior unchanged unless the compiler
  upgrade forces a documented deviation.
- Update adjacent tracking surfaces when the upgrade meaningfully changes them.

## Phases

| Phase | Status | Notes |
| --- | --- | --- |
| Create task packet and survey current toolchain pins | completed | Surveyed repo pins plus the local `ghc 9.14.1` / `cabal 3.16.1.0` toolchain. |
| Upgrade compiler pins and repair compatibility issues | completed | Moved the declared lane to GHC 9.14.1, widened `base` to `4.22`, updated CI/docs, and replaced the external `recursion-schemes` dependency with a local `Data.Functor.Foldable` module because the current `indexed-traversable` release chain still blocks `base-4.22`. |
| Verify on the upgraded toolchain | completed | `cabal build all -w ghc-9.14.1`, `cabal test -w ghc-9.14.1`, and `./scripts/thesis-conformance-gate.sh` all pass. |
| Sync docs/task surfaces and finalize | completed | Updated `CHANGELOG.md`, `README.md`, `TODO.md`, orchestrator guidance, and task notes. |

## Decisions

- Assume the user wants a real upgrade, not a docs-only pin bump: the repo
  should build and test under GHC 9.14, and config/docs should reflect that.

## Errors Encountered

| Error | Attempt | Resolution |
| --- | --- | --- |
| `cabal build all -w ghc-9.14.1` failed at dependency resolution because `mlf2.cabal` bounded `base` to `^>=4.21.0.0`. | Widened all package component `base` bounds to `^>=4.22.0.0`. | Resolved the direct compiler/package mismatch and exposed the next transitive blocker. |
| The `recursion-schemes -> free -> semigroupoids -> indexed-traversable` chain could not solve under `base-4.22` because the available `indexed-traversable` release caps `base < 4.22`. | Verified the dependency path after `cabal update`, then removed the external `recursion-schemes` package dependency and supplied the small `Data.Functor.Foldable` surface the repo actually uses. | The build now resolves and compiles on GHC 9.14.1 without adding compatibility shims or waiting on upstream releases. |
| GHC 9.14 promoted incomplete record selector usage to `-Wincomplete-record-selectors` warnings in several presolution/elaboration/test helpers. | Replaced selector-based access on sum types with constructor-bound fields and small helper functions in the affected modules. | Restored a warning-free build under the upgraded compiler. |
