# Findings

## Current Observations

- `STMu` now lowers through `internalizeCoercionCopy` into an acyclic graph `TyMu` with a single structural body child and an explicit binder edge, so recursive surface annotations can cross Phase 1 without introducing cyclic graph structure.
- Reification/result-type reconstruction must treat `TyMu` as an explicit binder owner rather than a simple structural node; once threaded through `MLF.Reify.Type`, the checked-authoritative pipeline reconstructs elaborated `TMu` directly instead of unfolded surrogates.
- `TyMu` integration required broad but mechanical exhaustiveness updates across normalization, copy/rewrite/finalize passes, presolution planning, and debug/fallback helpers; constructor-matching support was sufficient, and no solver-wide recursive inference machinery was needed.
