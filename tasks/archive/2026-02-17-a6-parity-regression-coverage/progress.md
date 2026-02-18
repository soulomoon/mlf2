# Progress Log: A6 (P2) Parity and Regression Coverage

## 2026-02-17
- Initialized task folder and planning files.
- Next: audit existing coverage in the four target spec modules.
- Audited target specs and identified A6 gaps:
  - Strong existing parity in `ElaborationSpec`.
  - Partial parity in `PipelineSpec` (missing dedicated bounded/coercion-heavy parity check with checked/typeCheck alignment).
  - Missing bounded/coercion-heavy parity regressions in `TypeCheckSpec` and `ReduceSpec`.
- Next: implement focused regression/parity tests in `PipelineSpec`, `TypeCheckSpec`, and `ReduceSpec` (plus optional bridging assertion in `ElaborationSpec` only if needed).
- Implemented A6 tests:
  - `test/PipelineSpec.hs`: added bounded alias + coercion-heavy parity case across unchecked/checked/typeCheck.
  - `test/TypeCheckSpec.hs`: added two parity regressions for elaborated bounded/coercion-heavy terms.
  - `test/ReduceSpec.hs`: added two normalization parity regressions for bounded/coercion-heavy elaborated terms.
- Removed an initial failing A6 Elaboration addition after audit confirmed coverage already existed there.
- Validation:
  - Ran `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6"'` (green).
  - Ran `cabal build all && cabal test` (green).
