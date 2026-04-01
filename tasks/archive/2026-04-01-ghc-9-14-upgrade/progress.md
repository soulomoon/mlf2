# Progress

## 2026-04-01

- Reviewed `AGENTS.md` and the applicable process skills.
- Surveyed current GHC pins in `cabal.project`, `README.md`, and the GitHub
  Actions workflow.
- Created the active task packet under
  `tasks/todo/2026-04-01-ghc-9-14-upgrade/`.
- Confirmed the local toolchain already has `ghc 9.14.1` and `cabal 3.16.1.0`.
- Ran the first upgrade probe:
  - `cabal build all -w ghc-9.14.1` -> FAIL at dependency resolution because
    `mlf2.cabal` still bounds `base` to `^>=4.21.0.0`.
- Widened direct `base` bounds to `^>=4.22.0.0`, updated the compiler pin to
  `ghc-9.14.1`, and synced the CI/doc surfaces to the new lane.
- Re-ran dependency resolution after `cabal update` and confirmed the next
  blocker was upstream: `recursion-schemes` currently pulls a dependency chain
  ending at `indexed-traversable`, whose available release still caps
  `base < 4.22`.
- Replaced the external `recursion-schemes` dependency with a local
  `Data.Functor.Foldable` module that implements the subset of the API used by
  this codebase.
- Fixed `-Wincomplete-record-selectors` warnings exposed by GHC 9.14 in
  presolution, elaboration, and test helper modules.
- Final verification:
  - `cabal build all -w ghc-9.14.1` -> PASS
  - `cabal test -w ghc-9.14.1` -> PASS (`1302 examples, 0 failures`)
  - `./scripts/thesis-conformance-gate.sh` -> PASS
