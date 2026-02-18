# Findings: Thesis Conformance Gate Command/Profile

## 2026-02-18 initial findings
- Repository currently has no `.github/workflows/` directory; CI gate must be added from scratch.
- Existing thesis anchors are already present and stable by label:
  - Φ/Ω matrix rows via `R-...` labels (`test/Presolution/WitnessSpec.hs`, `test/Presolution/MergeEmissionSpec.hs`).
  - A6 parity regressions (`test/PipelineSpec.hs`, `test/TypeCheckSpec.hs`).
  - Strict bug regression `BUG-2026-02-17-002` (`test/PipelineSpec.hs`).
  - Representative theorem gate (`gate: \\y. let id = (\\x. x) in id y has type forall a. a -> a`) in `test/PipelineSpec.hs`.
- Hspec `--match` is substring-based in this suite invocation context; alternation-style patterns (`A|B`) produced `0 examples` and cannot be used for grouped matching.
- Measured focused counts (all green):
  - `--match "R-"` => `19 examples, 0 failures`.
  - `--match "A6 parity"` => `3 examples, 0 failures`.
  - `--match "BUG-2026-02-17-002"` => `1 example, 0 failures`.
  - `--match "Phase 3 atomic wrapping equivalence gates"` => `7 examples, 0 failures`.
  - `--match "has type forall a. a -> a"` => `1 example, 0 failures`.

## 2026-02-18 completion findings
- Canonical gate command now exists at `scripts/thesis-conformance-gate.sh`.
- Gate script includes stale-matcher protection by enforcing minimum matched-example counts per thesis anchor slice.
- CI enforcement now exists at `.github/workflows/thesis-conformance.yml` and runs:
  - `cabal build all`
  - `./scripts/thesis-conformance-gate.sh`
- Documentation/tracker sync completed across:
  - `README.md`
  - `TODO.md`
  - `implementation_notes.md`
  - `CHANGELOG.md`
