# Progress

## 2026-02-27
- Resumed from partial implementation with 8 failing tests clustered in:
  - non-root `OpWeaken` replay resolution,
  - `OpGraft` binder-spine miss (`BUG-002-V1`),
  - explicit-forall round-trip mismatch.
- Verified fallback-removal symbol invariants via ripgrep.
- Added replay-bridge fixes in `MLF.Elab.Phi.Translate`:
  - scheme-quantifier-first replay ID derivation,
  - in-domain replay target filtering,
  - deterministic fallback to scheme-domain replay IDs when source coverage exceeds replay binder count.
- Added strict replay-map consumption fixes in `MLF.Elab.Phi.Omega`:
  - preserve replay raw IDs (no eager canonicalization),
  - replay-map/source-alias-based spine recovery for non-root weaken and graft targeting,
  - removed runtime class-member fallback branch.
- Updated strict no-fallback expectations in `test/ElaborationSpec.hs` for two alias-class tests.
- Debugged A6 with a local repl harness (`/tmp/debug_a6.hs`) to inspect `etBinderReplayMap`, witness ops, and `phi traceBinderReplayMap`.
- Verification runs:
  - targeted A6 parity: PASS,
  - targeted `BUG-002-V1`: PASS,
  - targeted explicit-forall round-trip: PASS,
  - full `mlf2-test`: PASS (`883 examples, 0 failures`),
  - full gate `cabal build all && cabal test`: PASS.
