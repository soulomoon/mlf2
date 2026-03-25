# Round 072 Implementation Notes

- Implemented the bounded `N5` slice in
  `src/MLF/Elab/Run/ResultType/Fallback.hs` by introducing the explicit
  `rootNonLocalSchemeAliasBaseLike` proof and routing only the selected
  non-local `targetC` arm through it. The frozen `baseTarget` computation
  stayed unchanged, and the accepted local empty-candidate / local continuity
  lanes stayed unchanged.
- Refreshed only the existing `ARI-C1 feasibility characterization (bounded prototype-only)`
  block in `test/PipelineSpec.hs`: kept
  `schemeAliasBaseLikeFallback False` as the selected non-local packet anchor,
  kept `schemeAliasBaseLikeFallback True` as the preserved local continuity
  contrast, and tightened the source guard to require the explicit non-local
  proof plus the dedicated `targetC` arm.
- TDD evidence:
  - first focused run after the source-guard refresh exposed a test-harness
    compile error in the new negative guard; fixed that test-only predicate and
    reran;
  - focused red rerun:
    `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
    -> fail (`20 examples, 1 failure`) because `Fallback.hs` did not yet define
    `rootNonLocalSchemeAliasBaseLike` or use it in the dedicated non-local
    `targetC` arm;
  - focused green rerun after the production edit:
    same command -> pass (`20 examples, 0 failures`).
- Required bounded verification:
  - `git diff --check` -> pass
  - `python3 -m json.tool orchestrator/rounds/round-072/state-snapshot.json >/dev/null` -> pass
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-072/state-snapshot.json`
    -> pass (`contract_version: 2`, `retry: null`)
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-005/roadmap.md`
    -> pass
  - required `test -f` predecessor-doc / mechanism-table / retry-subloop checks
    -> pass
  - `cabal build all && cabal test` -> pass (`1141 examples, 0 failures`).
- No blockers were encountered after the focused test-harness fix.
