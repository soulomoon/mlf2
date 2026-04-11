# General Automatic Iso-Recursive Full-Inference Positive-Family Aggregate Classification Current-State Refresh

Date: 2026-04-11
Status: current-truth refresh that supersedes the April 5 positive-family
aggregate as the live repo read while leaving the older artifact intact as
historical predecessor evidence
Artifact kind: canonical docs-only current-state positive-family aggregate
classification record

## Why This Refresh Exists

The April 5 aggregate was honest on its own ledger, but it no longer matches
the current repository state.

Two changes matter:

1. The merged April 10 P5 closeout now keeps the selected same-wrapper
   nested-`forall` packet plus the clear-boundary chain through
   `sameLaneNonupleAliasFrameClearBoundaryExpr` recursive on both
   authoritative entrypoints.
2. The repository now has an explicit representative `P2` harness in
   `test/Research/P2RepresentativeSupportSpec.hs` that locks two different
   authoritative positive packets for the family:
   - the exact `C1` non-local alias-bound / base-like `Int` packet; and
   - a second route-pure non-local alias-bound / base-like `Bool` packet.

The April 5 ledger did not admit either the merged April 10 P5 closeout or a
direct two-packet non-local `P2` representative harness. This refresh
therefore treats the April 5 artifact as predecessor truth only, not as the
current classification surface.

## Current Evidence Ledger

The current admissible positive ledger is:

| Evidence row | Current sources | Current read carried into this refresh |
| --- | --- | --- |
| `C1 authoritative packet` | `test/Research/C1AuthoritativeSurfaceSpec.hs`; `test/Research/P2RepresentativeSupportSpec.hs`; `test/PipelineSpec.hs` | The exact non-local scheme-alias / base-like packet remains recursively visible on `runPipelineElab` and `runPipelineElabChecked` while the fallback helper surface stays bounded and non-recursive. |
| `second non-local base-like packet` | `test/Research/P2RepresentativeSupportSpec.hs`; `test/Research/C1AuthoritativeSurfaceSpec.hs`; `test/PipelineSpec.hs`; `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` | The `Bool` variant of the same non-local alias-bound / base-like packet remains recursively visible on both authoritative entrypoints, while the fallback surface stays bounded and visibly non-recursive. That gives `P2` a second admitted packet under the same non-local route story rather than a cross-route surrogate. |
| `selected same-wrapper retained-child packet` | `test/Research/P5ClearBoundarySpec.hs`; `test/PipelineSpec.hs`; `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` | The selected same-wrapper nested-`forall` packet remains recursively visible on both authoritative entrypoints, and the current route explanation is still the retained-child proof cluster centered on `sameLaneLocalRetainedChildTarget`, `recursiveTargetProofFor`, `generalizeScopeRoot`, and `targetC`. |
| `alias-through-nonuple same-lane chain` | `test/Research/P5ClearBoundarySpec.hs`; `test/PipelineSpec.hs`; `implementation_notes.md` | `sameLaneClearBoundaryExpr` through `sameLaneNonupleAliasFrameClearBoundaryExpr` remain recursive on both authoritative entrypoints under the current clear-boundary retained-child route / guard story. |
| `frontier honesty` | `implementation_notes.md`; `docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md` | `sameLaneDecupleAliasFrameClearBoundaryExpr` and deeper alias shells remain closed, so the broader-positive chain still has an honest bounded frontier rather than open-ended widening. |

## Classification Rubric

The same bucket meanings from the April 5 aggregate remain in force:

- `credible general support`
- `packet-specific folklore`
- `current-architecture blockers`

The relevant promotion rule is unchanged:
accepted authoritative success across more than one admitted route family, or
across more than one accepted packet under one frozen route / frontier story,
is enough to rise above `packet-specific folklore`.

## Family Summary Matrix

| Positive-family row | Evidence ledger row(s) used | Bucket | Strongest lawful current read |
| --- | --- | --- | --- |
| `P2 non-local-propagation` | `C1 authoritative packet`; `second non-local base-like packet` | `credible general support` | The current repo no longer has only one `P2` packet. `C1` still shows recursive evidence reaching the authoritative surfaces through the explicit non-local alias-bound / base-like arm. The `Bool` variant shows the same family pressure on a second admitted packet under that same non-local route story, and its fallback surface remains bounded and non-recursive. That is stronger than folklore because the family now has route-pure authoritative support without borrowing evidence from the retained-child lane. |
| `P3 retained-child-owner-sensitive` | `alias-through-nonuple same-lane chain`; `frontier honesty` | `credible general support` | The same owner-local retained-child route remains recursively visible across the accepted clear-boundary chain, and the merged frontier remains explicit and bounded. |
| `P4 binder-sensitive-placement` | `alias-through-nonuple same-lane chain`; `frontier honesty` | `credible general support` | The same clear-boundary chain continues to vary retained ancestry and binder-sensitive placement without losing recursive visibility, while the closed frontier keeps the family bounded and honest. |
| `P5 polymorphism-nested-forall` | `selected same-wrapper retained-child packet`; `alias-through-nonuple same-lane chain`; `frontier honesty` | `credible general support` | The selected same-wrapper nested-`forall` packet is now a real positive authoritative packet, not predecessor-only folklore. Together with the merged clear-boundary chain and preserved closed frontier, the current architecture now has bounded, review-visible family support for the broader-positive `P5` surface. |
| `P6 reconstruction-visible-output` | `C1 authoritative packet`; `second non-local base-like packet`; `selected same-wrapper retained-child packet`; `alias-through-nonuple same-lane chain`; `frontier honesty` | `credible general support` | The authoritative current surfaces now keep recursive structure review-visible across multiple admitted positive lanes, including two route-pure non-local packets and the retained-child broader-positive lane. The repo therefore has representative reconstruction-visible support inside the inherited current architecture. |

## Current Positive-Side Conclusion

The strongest honest positive-family aggregate read on current HEAD is:

- `P2 non-local-propagation` = `credible general support`
- `P3 retained-child-owner-sensitive` = `credible general support`
- `P4 binder-sensitive-placement` = `credible general support`
- `P5 polymorphism-nested-forall` = `credible general support`
- `P6 reconstruction-visible-output` = `credible general support`

This refresh matters because no positive row now remains at
`packet-specific folklore` or `current-architecture blockers`.

The inherited production boundary is unchanged:

- explicit-only recursive baseline;
- iso-recursive meaning;
- non-equi-recursive meaning;
- no fallback widening;
- no second interface; and
- no cyclic or multi-SCC widening.

This artifact does not itself publish the repo-level decision. It updates the
positive-side ledger so the current readiness decision can be made honestly on
current evidence rather than on the stale April 5 subset.
