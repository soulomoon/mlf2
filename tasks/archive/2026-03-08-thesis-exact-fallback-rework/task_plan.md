# Task Plan — 2026-03-08 thesis-exact fallback rework

## Goal
Execute the approved thesis-exact fallback rework so the residual live fallback behaviors are removed, strict fail-fast regressions are covered by tests, and the implementation/docs reflect only the behavior that actually remains.

## Phases
- [completed] Context refresh and approved-plan read-in
- [completed] RED guard and semantic regression setup
- [completed] Thesis-exact implementation pass
- [completed] Full verification and docs sync

## Decisions
- Treat thesis exactness as the priority over compatibility recovery.
- Keep `checkNoGenFallback` and `NoFallback` entrypoints untouched.
- Preserve only witness/domain-owned instantiation authority in `reifyInst`: direct edge targets, trace binder args, and copied nodes from `etCopyMap`.
- Preserve exact-scheme annotation reuse only when the source scheme already matches the demanded annotation; do not reintroduce `targetArgs <|> expansionArgs` or chooser-style scheme replacement.
- Reclassify the fallback-dependent alias/nested-let family to strict fail-fast baselines when only expansion-derived recovery would keep them green.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Removing recursive scheme fallback naively broke let-poly / explicit-forall regressions | 1 | Routed the differing-scope branch through the already-computed structural scheme plan instead of recursive generalization. |
| Removing `reifyInst` secondary recovery initially broke alias/nested-let/A6/full-gate clusters | 1 | Traced the issue to wrong authoritative target selection; widened witness/domain search to direct target nodes and `etCopyMap` copied nodes without restoring expansion fallback. |
| Exact-scheme annotations started producing duplicate top-level `forall`s | 1 | Reused already-authoritative polymorphic sources directly in `AAnnF` instead of re-closing them. |
| Frozen parity baseline drifted after the strictness closeout | 1 | Regenerated `test/golden/legacy-replay-baseline-v1.json` after the full code/test story was true. |
