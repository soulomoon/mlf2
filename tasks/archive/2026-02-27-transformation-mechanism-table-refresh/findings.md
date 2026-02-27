# Findings — 2026-02-27 Transformation Mechanism Table Refresh

## Key discoveries
- The transformation table header was stale (`d790ce9` / earlier timestamp) versus current `HEAD` (`17d12c5`).
- Core pipeline and presolution claims remain accurate: `fromPresolutionResult` + `ElabEnv` path and closure-before/after-edge ordering are still current.
- The largest drift was in Phi weaken-resolution wording: docs still referenced class-member fallback, but current code resolves via replay-map aliases and fails fast when unresolved.
- `IdentityBridge` now documents witness-domain-only source-key provenance (raw/canonical/copy/trace) with canonical-alias fallback in binder-index selection, without class-member expansion in Phi identity lookup.
