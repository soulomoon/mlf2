# Findings — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Plan

## Scope
- Execute the agent-team implementation plan end-to-end (code + tests + docs closeout).

## Baseline findings before Wave 0
- Legacy solved-typed elaboration/Phi surfaces are still present:
  - `src/MLF/Elab/Elaborate.hs`: `GeneralizeAtWithLegacy`, legacy `elaborate` / `elaborateWithGen` / `elaborateWithScope` entrypoints.
  - `src/MLF/Elab/Phi/Translate.hs`: `GeneralizeAtWithLegacy`, `phiFromEdgeWitnessNoTrace`, deprecated `phiFromEdgeWitness`.
  - `src/MLF/Elab/Phi.hs`: re-exports legacy no-trace/deprecated Phi helpers.
  - `src/MLF/Elab/Phi/TestOnly.hs`: `phiFromEdgeWitnessAutoTrace` still accepts solved-typed callback shape.
  - `test/ElaborationSpec.hs`: many callsites still pass solved-aware callbacks and solved argument into test-only Phi helpers.
- Existing guards cover some chi-first constraints but do not yet enforce strict symbol retirement criteria for this row closeout.

## Process/state findings
- Session catchup reported unsynced prior-session context; task files were reconciled before code edits.
- Working tree contains one pre-existing modified file:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md`
  - This edit will be preserved and incorporated during closeout.

## Wave 0 findings
- New strict guards correctly fail against current legacy surfaces:
  - `elab-input thesis-exact guard` currently fails in both Pipeline and Elaborate guard contexts.
- `checked-authoritative` slice remains green (8 examples, 0 failures), so the stricter RED guard does not regress checked-authoritative baseline behavior.

## Wave 1 findings
- Legacy solved-typed elaboration/Phi APIs were removed from production-facing modules:
  - `MLF.Elab.Elaborate`: removed `GeneralizeAtWithLegacy` and solved-typed `elaborate*` entrypoints.
  - `MLF.Elab.Pipeline`: removed `elaborate` re-export.
  - `MLF.Elab.Phi.Translate`: removed `GeneralizeAtWithLegacy`, `phiFromEdgeWitnessNoTrace`, deprecated `phiFromEdgeWitness`.
  - `MLF.Elab.Phi`: removed legacy no-trace/deprecated re-exports.
- `MLF.Elab.Phi.TestOnly` migrated callback contracts to chi-native shape while keeping strict missing-trace fail-fast behavior.

## Wave 2 findings
- `test/ElaborationSpec.hs` callsites were migrated to chi-native callback usage (`generalizeAtWithActive solved`).
- Removed test dependency on retired `Elab.elaborate` by asserting missing-trace fail-fast directly through `phiFromEdgeWitnessWithTrace` with absent trace entries.
- Required parity/guard slices are green post-migration:
  - `elab-input thesis-exact guard`: PASS
  - `checked-authoritative`: PASS
  - `Dual-path verification`: PASS

## Wave 3 Team E closeout findings
- Required closeout gates were rerun in this integrated workspace with temporary
  cache-home overrides to avoid sandbox cache-permission failures:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)
- Strict row criterion is now satisfied:
  - solved-typed elaboration/Phi compatibility entrypoints are retired from
    production modules;
  - test-only Phi callback contract is chi-native;
  - strict fail-fast no-trace invariant remains in place.
- Closeout docs were updated in the same wave (`TMT`, `implementation_notes`,
  `TODO`, `CHANGELOG`, and task tracking files), and the TMT
  `Elaboration input` row was flipped to `Thesis-exact = Yes` only after the
  four required gates passed.
