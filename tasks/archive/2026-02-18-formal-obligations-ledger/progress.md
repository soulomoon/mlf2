# Progress Log: Formal Obligations Ledger

## 2026-02-19
- Created task folder and planning artifacts under `tasks/todo/2026-02-18-formal-obligations-ledger/`.
- Confirmed baseline thesis conformance gate is green before changes.
- Added ledger assets:
  - `docs/thesis-obligations-ch14-15.yaml`
  - `docs/thesis-obligations-ch14-15.md` (generated)
  - `scripts/render-thesis-obligations-ledger.rb`
  - `scripts/check-thesis-obligations-ledger.sh`
- Integrated obligations stage in `scripts/thesis-conformance-gate.sh`.
- Added/updated O* rule anchors across TypeCheck/Reduce/Elaboration/Presolution specs.
- Fixed checker/test issues found by executable matcher sweep:
  - Ruby `tally` compatibility
  - renderer `--check` write side-effect
  - `BoundType` construction mismatch in `TypeCheckSpec`
  - non-interior translatability fixture setup
  - `InstUnder` anchor semantics
- Verification commands:
  - `./scripts/check-thesis-obligations-ledger.sh` (PASS)
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)
- Archived task folder to `tasks/archive/2026-02-18-formal-obligations-ledger/` after closure.
