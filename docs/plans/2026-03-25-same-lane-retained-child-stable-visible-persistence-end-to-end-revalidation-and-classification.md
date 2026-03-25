# Same-Lane Retained-Child Stable-Visible-Persistence End-To-End Revalidation And Classification

Date: 2026-03-25
Round: `round-092`
Roadmap item: `item-4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: end-to-end revalidation and bounded persistence classification
for the frozen same-lane retained-child pocket only
Artifact kind: canonical bounded item-4 revalidation / classification record

## Stage Contract Freeze

This artifact implements only roadmap item `4` for `attempt-1` with
`retry: null`.

The frozen tuple remains unchanged:

- family: same-lane retained-child;
- recursive-shape anchor: `boundVarTargetRoot`;
- owner / binder frame: one owner-local retained-child frame;
- route:
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`; and
- quantified-boundary status: clear-boundary only.

The exact frozen packet remains:

```haskell
ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
  (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
```

where
`recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`.

This artifact does not authorize:

- any `src/`, `src-public/`, `app/`, or `mlf2.cabal` repair;
- any widening into roadmap item `5`;
- any widening into the non-local alias-bound / base-like family;
- neighboring consumer routes;
- nested-`forall`, nested owner, or nested scheme-root positive success;
- replay / `InstBot` repair or `BUG-2026-03-16-001`;
- equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  a second interface, or fallback widening; or
- any reopened `non-cyclic-graph` revision argument.

This round may record exactly one lawful outcome token for this one pocket
only.

## Exact Item-4 Contract Summary

- This round repairs authoritative item-4 evidence for the same frozen
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
- The bounded item-4 outcome token remains
  `admitted but not reconstruction-visible / blocker debt`.
- Item `5` remains later work only.

## Accepted Continuity Revalidated

The accepted predecessor chain remains binding and unchanged:

- item `1` froze the exact pocket and six-row ledger in
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`;
- item `2` localized the first earlier breakpoint to `Phase 6 (elaboration)`
  in
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`;
- item `3` cleared that exact elaboration breakpoint in
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`;
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary remains live
  under
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`;
- the same-lane retained-child successor gate remains the only lawful live
  subject under
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`,
  and
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`; and
- `checkedAuthoritative` in
  `src/MLF/Elab/Run/Pipeline.hs:182-194` still makes the checked
  type-checker result authoritative even when
  `computeResultTypeFallback` reconstructs a different diagnostic type.

The exact same-lane route and clear-boundary filter also remain unchanged in
`src/MLF/Elab/Run/ResultType/Fallback.hs:558-780`, including
`boundVarTargetRoot`, `boundHasForallFrom`, the
`sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC` selection,
and the final `generalizeWithPlan` / scheme-to-type reconstruction step.

## Exact-Pocket Replay Evidence

Fresh exact-pocket replay was rerun through `cabal repl mlf2-test` for the
same frozen packet only. The replay reused the same exact helper-visible
same-lane local retained-child rewiring already frozen in
`test/PipelineSpec.hs:1495-1570`, then printed the internal fallback result
and both public pipeline output types.

Replay output:

```text
TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))
True
Right (TForall "a" Nothing (TVar "a"))
Right (TForall "a" Nothing (TVar "a"))
```

The first two lines are the exact helper-visible internal reconstruction read:

- `computeResultTypeFallback` on the exact same-lane retained-child route
  reconstructs a `TMu`-bearing type term; and
- `containsMu True` still holds for that exact helper-visible internal read.

The last two lines are the exact public-output reads:

- `runPipelineElab` now succeeds but returns the authoritative public type
  `TForall "a" Nothing (TVar "a")`; and
- `runPipelineElabChecked` returns that same authoritative public type.

To keep that public-output fact review-visible inside the test corpus,
`test/PipelineSpec.hs` now uses one focused exact-pocket spec that asserts the
returned type directly for both `runPipelineElab` and
`runPipelineElabChecked`, freezing the authoritative public-output shape as
`TForall "a" Nothing (TVar "a")`.

## End-To-End Ledger Revalidation

| Phase / surface | Live anchor(s) | Row result | Continuity note |
| --- | --- | --- | --- |
| Solver admission state | accepted item `1` + accepted item `2` frozen tuple; `Fallback.hs:558-735` | `satisfied on accepted predecessor evidence` | Accepted item `2` already fixed the same family, `boundVarTargetRoot`, one owner-local retained-child frame, the route `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, and clear-boundary-only status as the exact admitted pocket under review, and the current exact-pocket reruns do not contradict that accepted solver record. |
| Elaboration handoff / result state | accepted item `3`; `test/PipelineSpec.hs`; fresh exact-pocket replay | `satisfied on accepted predecessor evidence` | Accepted item `3` already cleared the exact `Phase 6 (elaboration)` breakpoint on both `runPipelineElab` and `runPipelineElabChecked`, and the current exact-pocket reruns still reach authoritative output on both entrypoints without reintroducing that earlier breakpoint. |
| Reification / reconstruction state | `Fallback.hs:736-780`; `test/PipelineSpec.hs`; fresh exact-pocket replay | `satisfied on current exact-pocket evidence` | The exact same-lane retained-child reconstruction path still reaches `generalizeWithPlan` and reconstructs a `TMu`-bearing type term for the same frozen route rather than switching families or relying on quantified crossing. |
| Internal output surface | `test/PipelineSpec.hs`; fresh exact-pocket replay | `satisfied on current exact-pocket evidence` | The exact helper-visible internal output still exposes recursive structure review-visibly: `computeResultTypeFallback` yields `TArrow (TVar "t32") (TMu "t38" ...)` and `containsMu True`. |
| Public output surface | `src/MLF/Elab/Run/Pipeline.hs:182-194`; `test/PipelineSpec.hs`; fresh exact-pocket replay | `first actual continuity breakpoint` | The authoritative public output omits the recursive structure and instead returns `TForall "a" Nothing (TVar "a")` on both entrypoints. Internal/public continuity therefore stops here for this pocket. |
| Reviewer-visible evidence trail | item `1` frozen tuple; the exact anchors above | `not credited after earlier breakpoint` | Reviewers can point to one frozen tuple through solver, elaboration, reification, and helper-visible internal output, but they cannot lawfully claim one stable visible recursive result across both internal and public surfaces because the authoritative public surface collapses to `forall identity` at the earlier public-output breakpoint. |

## One Authoritative Item-4 Outcome

Authoritative item-4 outcome token:
`admitted but not reconstruction-visible / blocker debt`

Why this is the strongest lawful read:

1. The exact pocket is still admitted and still preserves the same family,
   anchor, owner-local frame, route, and clear-boundary status.
2. The exact same-lane retained-child reconstruction/internal path still
   carries visible recursive structure (`TMu ...`, `containsMu True`) on the
   helper-visible internal surface.
3. The authoritative public output for the same exact packet is now frozen as
   `TForall "a" Nothing (TVar "a")`, so recursive structure is not visible on
   the public surface and item-5 / `P6` success remains unearned.
4. This is not `fail-closed rejection`: no forbidden quantified crossing,
   family drift, owner/frame drift, or neighboring-route substitution is
   needed to explain the result. The pocket survives part of the ledger, but
   public visibility still collapses into blocker debt.

This artifact therefore keeps the pocket below accepted `stable visible
persistence` without widening into item `5` or any repair round. Item `5`
remains later work only.

## Verification Record

- exact-pocket replay via `cabal repl mlf2-test`
  - Result: reproduced the helper-visible internal/output mismatch quoted
    above for the exact frozen packet only.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - Result: passed with `1 example, 0 failures`, with the focused spec
    asserting `TForall "a" Nothing (TVar "a")` directly for both
    `runPipelineElab` and `runPipelineElabChecked`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Result: passed with `22 examples, 0 failures` after freezing the
    exact-pocket public-output fact.
- `cabal build all && cabal test`
  - Result: passed with `1144 examples, 0 failures`.
