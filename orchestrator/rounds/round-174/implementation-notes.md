## Summary

- Outcome: narrow success.
- Root cause: `preserveRetainedChildAliasBoundary` only treated identity-scheme
  binders as lawful intermediate steps in the retained-child alias chain. The
  frozen packet adds one more same-lane clear-boundary alias shell (`keep`)
  whose elaborated binder scheme is not identity-shaped, so the preservation
  hook stopped one frame too early and both authoritative entrypoints reached
  `PipelineTypeCheckError (TCLetTypeMismatch ...)`.
- Retry correction: attempt 1 set the alias-depth helper too high at the
  current `hold` boundary. Attempt 2 tightens that bound so the preservation
  hook admits exactly one additional same-lane alias shell (`keep`) before the
  final identity child and no deeper alias chain.
- Fix: keep the existing direct retained-child identity requirement for the
  final preserved child, but allow exactly one additional same-lane alias-frame
  shell with a non-identity scheme while still requiring the alias rhs itself
  to stay alias-frame-shaped. This keeps the change packet-bounded inside
  `src/MLF/Elab/TermClosure.hs` and lets the existing recursive-authority
  preservation logic fire at the `hold` boundary for the frozen double-alias
  packet only.
- Tests: reuse the focused pipeline regression in `test/PipelineSpec.hs` and
  the tightened research probe in
  `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`; both remain
  exact-packet-only and assert recursive success on both authoritative
  entrypoints.

## Commands

- Focused checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "double-alias clear-boundary packet"'`
- Full gate:
  - `cabal build all && cabal test`

## Outcome

- The focused packet regression and research probe both pass:
  `sameLaneDoubleAliasFrameClearBoundaryExpr` now preserves recursive output on
  both `runPipelineElab` and `runPipelineElabChecked`.
- The full repository gate passed after the bounded fix:
  `cabal build all && cabal test` finished with `1306 examples, 0 failures`.
