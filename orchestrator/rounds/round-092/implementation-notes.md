# Round 092 Implementation Notes

## Change Summary

- Rewrote the canonical item-`4` ledger so it uses only the approved
  row-result vocabulary and carries rows `1` and `2` as accepted predecessor
  evidence.
- Strengthened the focused exact-pocket public-output freeze in
  `test/PipelineSpec.hs` so one spec now asserts
  `TForall "a" Nothing (TVar "a")` directly for both public pipeline
  entrypoints.
- Refreshed the round-local notes so they mirror the bounded item-`4`
  contract summary without touching controller-owned retry artifacts.

## Exact Item-4 Contract Summary

- This round repairs authoritative item-`4` evidence for the same frozen
  same-lane retained-child pocket only.
- The contract summary under repair remains `attempt-1` with `retry: null`.
- The frozen tuple remains unchanged:
  same-lane retained-child family, `boundVarTargetRoot`, one owner-local
  retained-child frame, and the route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` with
  clear-boundary-only status.
- The exact frozen packet remains:

  ```haskell
  ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
  ```

  where
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.
- Accepted continuity remains unchanged:
  item `1` froze the pocket and six-row ledger;
  item `2` localized the first earlier breakpoint to `Phase 6 (elaboration)`;
  and item `3` cleared that exact `Phase 6 (elaboration)` breakpoint.
- The helper-visible internal recursive fact for the same pocket remains a
  `TMu`-bearing reconstruction plus `containsMu True`.
- The authoritative public-output result for both `runPipelineElab` and
  `runPipelineElabChecked` remains
  `TForall "a" Nothing (TVar "a")`.
- The bounded item-`4` outcome token remains
  `admitted but not reconstruction-visible / blocker debt`.
- Item `5` remains later work only.

## Verification

- `git diff --check`
  - Result: passed.
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  - Result: passed.
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  - Result: passed; the live controller state remains on the v2 roadmap /
    retry contract with the resolved roadmap locator and the active retry
    object for `item-4` `attempt: 2`.
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  - Result: passed.
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  - Result: passed.
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - Result: passed.
- `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
  - Result: passed.
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
  - Result: passed.
- exact-pocket replay via `cabal repl mlf2-test`
  - Result: reproduced `computeResultTypeFallback` as
    `TArrow (TVar "t32") (TMu "t38" ...)`, `containsMu True`, and both public
    entrypoints as `Right (TForall "a" Nothing (TVar "a"))` on the exact
    frozen packet.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - Result: passed with `1 example, 0 failures`, with the focused spec
    asserting `TForall "a" Nothing (TVar "a")` directly for both
    `runPipelineElab` and `runPipelineElabChecked`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Result: passed with `22 examples, 0 failures`.
- `cabal build all && cabal test`
  - Result: passed with `1144 examples, 0 failures`.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Result: returned only `test/PipelineSpec.hs`.
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  - Result: returned only `test/PipelineSpec.hs`.
- `git diff --name-only -- orchestrator/state.json orchestrator/roadmap.md orchestrator/retry-subloop.md orchestrator/verification.md Bugs.md`
  - Result: reported only the pre-existing controller-owned drift on
    `orchestrator/state.json`; the preserved roadmap / retry / verification
    pointers and `Bugs.md` remained untouched by this round.
