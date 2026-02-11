# Progress Log

## 2026-02-11
- Invoked `systematic-debugging` skill for BUG-2026-02-11-003.
- Gathered current bug tracker status and recent commit context.
- Initialized task planning files for this debugging session.
- Converted `BUG-004-V2` and `BUG-004-V4` from sentinel failures to strict success assertions in `test/ElaborationSpec.hs`.
- Reproduced:
  - `BUG-004-V2` now succeeds.
  - `BUG-004-V4` fails with `TCInstantiationError ... InstBot expects TBottom, got Int -> Int`.
- Used `cabal repl lib:mlf2-internal` to inspect pre-typecheck elaborated term and locate failing instantiation shape in V4.
- Implemented and iterated fixes:
  - `Finalize`: retain quantified names in `usedNames`.
  - `Omega`: scheme-arity-aware reorder identity requirement.
  - `Elaborate`: narrow closed bounded-identity collapse for desugared annotated-lambda params.
  - `TypeCheck`: permit equal-bound `InstBot` via `alphaEqType`.
- Recovery iteration:
  - Full gate initially regressed after broad Omega/TypeCheck changes.
  - Reverted broad Omega relaxations and revalidated targeted `make` + Φ tests.
  - Kept only the minimal combination that preserved both BUG-004 closure and legacy gates.
- Validation completed:
  - Targeted BUG checks and witness checks pass.
  - Full gate pass: `cabal build all && cabal test`.
