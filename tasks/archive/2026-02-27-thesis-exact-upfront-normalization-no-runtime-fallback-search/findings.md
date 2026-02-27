# Findings

## 2026-02-27
- `EdgeTrace.etBinderReplayMap` must be treated as required producer contract data; preserving source-domain provenance (`etBinderArgs`, `etCopyMap`, `etInterior`) while carrying replay targets separately keeps Phi/Omega deterministic.
- A6 failures showed replay-domain drift: scheme quantifier spine IDs (`[28,5]`) can diverge from `siSubst` key-space. Bridge construction must prioritize scheme-quantifier-derived replay IDs, not raw `siSubst` keys alone.
- Canonicalizing replay-map targets too early in Omega causes binder-spine misses; replay targets must remain in replay raw-ID space through binder index lookup.
- Duplicate/non-root `OpWeaken` scenarios can be resolved deterministically from replay-map source-alias projections (`IB.sourceKeysForNode` + replay-map lookup) without runtime class-member search.
- Remaining alias-class success tests in `ElaborationSpec` were legacy behavior checks and had to be switched to fail-fast expectations to match strict plan semantics.
- Final verification is green:
  - `cabal test mlf2-test --test-show-details=direct` => `883 examples, 0 failures`
  - `cabal build all && cabal test` => PASS.
