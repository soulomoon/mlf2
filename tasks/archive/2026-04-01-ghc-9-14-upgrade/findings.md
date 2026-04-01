# Findings

## Initial Survey

- `cabal.project` currently pins `with-compiler: ghc-9.12.2` and includes a
  comment that GHC 9.14+ ships `base 4.22`, so the repo already anticipated a
  base-version change at this boundary.
- The local workspace toolchain already has `ghc 9.14.1` active, and `ghcup`
  reports `9.14.1` as the installed `latest` lane with `base-4.22.0.0`.
- `README.md` documents the supported matrix lane as `ubuntu-latest / GHC
  9.12.2`.
- `.github/workflows/thesis-conformance.yml` currently runs only GHC `9.12.2`
  in both jobs.

## First 9.14 Probe

- `cabal build all -w ghc-9.14.1` fails immediately at dependency resolution,
  before source compilation:
  - `mlf2` currently requires `base ^>=4.21.0.0`
  - GHC 9.14.1 provides `base-4.22.0.0`
- That means the first required change is widening the package's direct `base`
  bounds; the earlier `cabal.project` comment about transitive breakage has
  not yet been revalidated under the current package index.

## Task Assumption

- The upgrade scope is the narrow, execution-oriented path: move the declared
  compiler lane to GHC 9.14 and make the minimum honest compatibility fixes
  required for the project to build and test there.

## Transitive Dependency Root Cause

- After widening direct `base` bounds to `^>=4.22.0.0`, dependency resolution
  still failed through the chain
  `recursion-schemes -> free -> semigroupoids -> indexed-traversable`.
- Under the current package index, the available `indexed-traversable` release
  caps `base < 4.22`, so the blocker is upstream package availability rather
  than any direct use of advanced compiler internals in this repo.
- The codebase only uses a small subset of `recursion-schemes`
  (`Base`, `Recursive`, `Corecursive`, `cata`, `para`, `ana`, `hylo`, and
  `ListF`), so the clean root-cause fix is to provide that surface locally
  instead of carrying the full dependency stack.

## Source Compatibility Notes

- GHC 9.14 emits `-Wincomplete-record-selectors` warnings for record selectors
  used against sum types. The affected presolution, elaboration, and test
  helpers needed small source changes to bind fields through constructors or
  helper functions instead of partial selectors.
- No thesis-faithfulness deviation was introduced by the compiler upgrade:
  changes were limited to toolchain declarations, dependency plumbing, and
  warning-clean source adjustments.

## Final Verification

- `cabal build all -w ghc-9.14.1` passes.
- `cabal test -w ghc-9.14.1` passes with `1302 examples, 0 failures`.
- `./scripts/thesis-conformance-gate.sh` passes, including:
  - `[thesis-obligations] PASS: all obligations are mapped and green`
  - `[thesis-claims] PASS: all validations green`
  - `[thesis-gate] PASS: thesis conformance anchors are green`
