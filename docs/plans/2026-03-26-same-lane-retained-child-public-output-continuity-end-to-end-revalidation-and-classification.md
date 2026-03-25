# Same-Lane Retained-Child Public-Output Continuity End-To-End Revalidation And Classification

Date: 2026-03-26
Round: `round-097`
Roadmap item: `item-4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: refreshed end-to-end revalidation and bounded public-output
continuity classification for the frozen same-lane retained-child pocket only
Artifact kind: canonical bounded item-4 revalidation / classification record

## Stage Contract Freeze

This artifact implements only roadmap item `4` for `attempt-1` with
`retry: null`.

This round is docs-first and evidence-first. Its job is to rerun only the
exact frozen same-lane retained-child pocket after accepted item `3`
confirmed that the bounded `Pipeline.hs` / `TermClosure.hs` root-handoff
slice contains no alternate recursive whole-packet authoritative result, and
then record exactly one lawful item-4 outcome token for that same exact
pocket only.

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

- any `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal` change;
- any rewrite of `orchestrator/state.json`, roadmap files, retry files,
  verification files, or `Bugs.md`;
- any reopening of roadmap item `3`;
- any widening into roadmap item `5`;
- any widening into the non-local alias-bound family, neighboring consumer
  routes, nested-`forall`, replay / `InstBot`, equi-recursive reasoning,
  cyclic structural graphs, multi-SCC search, second-interface work, or
  fallback widening; or
- any reopened `non-cyclic-graph` decision inside this packet.

This round may record exactly one lawful item-4 outcome token for this one
exact pocket only. Item `5` remains later work only.

## Exact Item-4 Contract Summary

- This round revalidates authoritative item-4 evidence for the refreshed
  2026-03-26 public-output continuity family on the same exact frozen pocket
  only.
- Accepted continuity remains unchanged:
  item `1` froze the refreshed exact pocket and review ledger;
  item `2` fixed the unchanged public-output collapse anchor at
  `checkedAuthoritative`, with `termClosed` and `typeCheck termClosed` as
  the same-pocket dependencies; and
  item `3` confirmed that the bounded `Pipeline.hs` / `TermClosure.hs`
  root-handoff slice contains no alternate recursive whole-packet
  authoritative result for this pocket.
- The helper-visible internal recursive fact for the same pocket remains a
  `TMu`-bearing reconstruction plus `containsMu True`.
- The authoritative public-output result for both `runPipelineElab` and
  `runPipelineElabChecked` remains
  `TForall "a" Nothing (TVar "a")`.
- The bounded item-4 outcome token remains
  `admitted but not reconstruction-visible / blocker debt`.
- Item `5` remains later work only, and this round does not reopen
  `non-cyclic-graph`.

## Accepted Refreshed Continuity Revalidated

The accepted predecessor chain remains binding and unchanged:

- item `1` froze the refreshed exact pocket and review ledger in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`;
- item `2` fixed the unchanged exact public-output collapse anchor in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`;
- item `3` confirmed blocker debt within the unchanged current architecture
  in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`;
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary remains live
  under
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`;
- accepted strategic items `2`, `5`, `6`, and `7` remain bounded strategic
  context only under
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`,
  and
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`;
- the immediate predecessor item-4 and item-5 records remain
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-end-to-end-revalidation-and-classification.md`
  and
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-successor-decision-gate.md`;
  and
- accepted rounds `089` through `096` remain bounded predecessor evidence
  only. They do not authorize widening into a broader capability claim or an
  architecture decision inside this item-4 packet.

Fresh exact-pocket evidence in this round does not contradict that accepted
chain. It reproduces the same helper-visible/internal versus authoritative
public split and therefore preserves the refreshed blocker-debt posture.

## Exact-Pocket Replay Evidence

Fresh exact-pocket replay was rerun through `cabal repl mlf2-test` with
`:module + *PipelineSpec` for the same frozen packet only. The replay reused
the same exact helper-visible same-lane local retained-child rewiring
anchored in `test/PipelineSpec.hs:1495-1570`, then printed the internal
fallback result, `containsMu`, and both public pipeline output types.

Replay output:

```text
TArrow (TVar "t32") (TMu "t38" (TArrow (TVar "t38") (TBase (BaseTy {getBaseName = "Int"}))))
True
Right (TForall "a" Nothing (TVar "a"))
Right (TForall "a" Nothing (TVar "a"))
```

The first two lines are the exact helper-visible internal reconstruction
read:

- `computeResultTypeFallback` on the exact same-lane retained-child route
  still reconstructs a `TMu`-bearing type term; and
- `containsMu True` still holds for that exact helper-visible internal read.

The last two lines are the exact authoritative public-output reads:

- `runPipelineElab` still returns the authoritative public type
  `TForall "a" Nothing (TVar "a")`; and
- `runPipelineElabChecked` returns that same authoritative public type.

The fresh replay therefore matches the accepted refreshed item-1 / item-2 /
item-3 chain: recursive structure remains visible on the helper-visible
internal surface only, while both authoritative public entrypoints still
collapse to `forall identity`.

## End-To-End Ledger Revalidation

| Phase / surface | Live anchor(s) | Row result | Continuity note |
| --- | --- | --- | --- |
| Solver admission state | accepted refreshed item `1`; accepted refreshed item `2`; `Fallback.hs` same-lane route anchors | `satisfied on accepted predecessor evidence` | Accepted refreshed items `1` and `2` already froze the same family, `boundVarTargetRoot`, one owner-local retained-child frame, the route `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`, and clear-boundary-only status as the exact admitted pocket under review, and the fresh exact-pocket reruns do not contradict that accepted solver record. |
| Elaboration handoff / result state | accepted predecessor item `3`; fresh exact-pocket replay; focused exact-pocket Phase-6 test | `satisfied on accepted predecessor evidence` | Accepted predecessor item `3` already cleared the exact earlier elaboration breakpoint for this pocket, and the fresh exact-pocket reruns still reach authoritative output on both public entrypoints without reviving an earlier failure before the public-output surface. |
| Reification / reconstruction state | `test/PipelineSpec.hs:1495-1570`; fresh exact-pocket replay; accepted refreshed item `2` path audit | `satisfied on current exact-pocket evidence` | The same exact retained-child route still reaches the same-lane reconstruction path and yields a `TMu`-bearing helper-visible type term for the same frozen pocket rather than switching family, frame, route, or quantified-boundary status. |
| Internal output surface | `computeResultTypeFallback`; fresh exact-pocket replay | `satisfied on current exact-pocket evidence` | The exact helper-visible internal output still exposes recursive structure review-visibly: `computeResultTypeFallback` yields `TArrow (TVar "t32") (TMu "t38" ...)` and `containsMu True`. |
| Public output surface | `runPipelineElab`; `runPipelineElabChecked`; accepted refreshed item `2` unchanged-anchor audit; fresh exact-pocket replay | `first actual continuity breakpoint` | The authoritative public output still omits the recursive structure and instead returns `TForall "a" Nothing (TVar "a")` on both public entrypoints. Internal/public continuity therefore stops here for this pocket. |
| Reviewer-visible evidence trail | refreshed frozen tuple; fresh exact-pocket replay; focused exact-pocket tests | `not credited after earlier breakpoint` | Reviewers can point to one frozen tuple through solver, elaboration, reconstruction, and helper-visible internal output, but they cannot lawfully claim one stable visible recursive result across both internal and public surfaces because the authoritative public surface still collapses to `forall identity` at the earlier public-output breakpoint. |

## One Authoritative Item-4 Outcome

Authoritative item-4 outcome token:
`admitted but not reconstruction-visible / blocker debt`

Why this remains the strongest lawful read:

1. The exact pocket is still admitted and still preserves the same family,
   anchor, owner-local frame, route, and clear-boundary-only status.
2. The exact same-lane retained-child reconstruction/internal path still
   carries visible recursive structure (`TMu ...`, `containsMu True`) on the
   helper-visible internal surface.
3. The authoritative public output for the same exact packet still remains
   `TForall "a" Nothing (TVar "a")`, so recursive structure is not visible
   on the public surface.
4. Accepted refreshed item `3` confirmed that the bounded root-handoff slice
   contains no alternate recursive whole-packet authoritative result to
   expose locally, so the current evidence supports classification, not
   repair.
5. This is not `fail-closed rejection`: the same pocket still survives
   solver admission, elaboration, reconstruction, and helper-visible
   internal output without needing a family swap, route swap, quantified
   crossing, or replay-only rescue.

This artifact therefore preserves the refreshed exact-pocket outcome below
`stable visible persistence` without widening into item `5` or any repair
round. Item `5` remains later work only.

## Verification Record

- exact-pocket replay via `cabal repl mlf2-test` with `:module + *PipelineSpec`
  - Result: reproduced the helper-visible recursive reconstruction and the
    two-entrypoint `forall identity` public result quoted above for the same
    frozen packet only.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - Result: passed with `1 example, 0 failures`.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Result: passed with `22 examples, 0 failures`.

This round remains docs-only. The full `cabal build all && cabal test` gate
is lawfully out of scope unless the diff escapes the authorized docs /
orchestrator surface, which this artifact does not do.
