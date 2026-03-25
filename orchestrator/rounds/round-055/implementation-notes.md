# Round 055 Implementation Notes

## Summary

- Added the explicit `rootLocalSingleBase` proof in
  `src/MLF/Elab/Run/ResultType/Fallback.hs` from the selected `I1` ingredients:
  local binding, singleton `rootBoundCandidates`, no multi-inst, and no
  inst-arg multi-base.
- Routed `targetC` to `baseTarget` only for that selected local single-base
  lane, while preserving the inherited scheme-alias/base-like route and the
  existing retained-target families unchanged.
- Added one bounded local single-base success helper/example, one matched
  non-local fail-closed contrast, and one source guard in
  `test/PipelineSpec.hs`.

## Verification

- Red: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  failed before the production change on the new non-local single-base contrast
  and the missing `rootLocalSingleBase` source guard.
- Green: the same focused command passed after the `Fallback.hs` change.
- Baseline checks passed:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-055/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-055/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/roadmap.md`
  - required `test -f ...` artifact checks from `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-022/verification.md`
  - the single-lane anchor `rg -n 'rootLocalSingleBase|baseTarget|rootBoundCandidates|keepTargetFinal|targetC|rootLocalMultiInst|rootLocalInstArgMultiBase|rootLocalSchemeAliasBaseLike|boundVarTarget|schemeBodyTarget' src/MLF/Elab/Run/ResultType/Fallback.hs test/PipelineSpec.hs`
- Full gate: `cabal build all && cabal test` passed.

## Scope Guard

- New `I2` edits stayed within the `I2` file set:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
  - `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`
  - `orchestrator/rounds/round-055/implementation-notes.md`
- Left `orchestrator/rounds/round-055/state-snapshot.json`, the bug tracker, replay lanes, `InstBot`,
  `boundVarTarget` widening, `boundTarget` overlay work, `schemeBodyTarget`
  consolidation, and `ResultType.View` untouched.
- Pre-existing worktree entries still present after this pass:
  - `orchestrator/rounds/round-055/state-snapshot.json`
  - `orchestrator/rounds/round-055/plan.md`
  - `orchestrator/rounds/round-055/selection.md`
