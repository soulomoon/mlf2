# Draft Repo-Scope Decision Rerun After `rev-004`

Status: draft only. Task-local and non-authoritative. This file does not
replace the accepted March 26 global gate, does not change
`orchestrator/state.json`, and does not create a live successor revision.

## Question

If the accepted exact-pocket `rev-003` / `rev-004` same-lane repair is carried
forward into the repo-scope matrix honestly, what is the strongest current
repo-level strategic read?

## Decision Inputs Reused

- inherited baseline:
  explicit-only production support, iso-recursive meaning, non-equi-recursive
  semantics, non-cyclic graph encoding, and no fallback / second interface
- accepted capability contract and family matrix from March 25
- accepted March 25 architecture decision:
  `continue within the current architecture`
- accepted March 26 representative matrix and March 26 global
  `keep` vs `reopen` gate as historical evidence only
- accepted rev-003 / rev-004 exact-pocket settlement and validation for the
  same-lane `C2` / `C5` / `C7` pocket
- the refreshed task-local matrix draft in
  `representative_matrix_refresh_draft.md`

## Why The March 26 Reopen Gate Is Now Historically True But Strategically Stale

The March 26 reopen gate was honest on the evidence it consumed. Its decisive
aggregate premise was:

- zero `stable visible persistence` rows anywhere in the representative matrix;
- the exact `C7` output-continuity pocket still collapsed from helper-visible
  recursion to authoritative public `forall identity`; and
- the global positive-family ledger therefore left no surviving
  reconstruction-visible route inside the inherited acyclic model.

Those premises no longer hold on the post-`rev-004` read:

- the exact same-lane `C2` / `C5` / `C7` pocket now has accepted
  stable-visible continuity on both internal and public surfaces; and
- the old `C7` collapse is now historical evidence rather than the current
  exact-pocket read.

So the accepted March 26 reopen gate remains valid historical evidence, but it
should not be treated as the latest repo-scope read once the repaired
exact-pocket result is carried forward.

## Draft Outcome Evaluation

| Candidate posture | Support on the refreshed read | Blocking evidence | Draft status |
| --- | --- | --- | --- |
| `non-cyclic-graph = keep` | The inherited acyclic model now contains one exact same-lane pocket with stable visible persistence on current internal and public surfaces. | `P1` still lacks unannotated automatic success, `P2` still ends visibly non-recursive, and `P5` remains reject-side only. Representative repo-level settlement across `P1` through `P6` is still not credible. | `not selected` |
| `reopen the non-cyclic-graph revision question` | The old March 26 gate previously chose reopen when no representative row survived to stable visible persistence. | That decisive premise is now stale. The remaining blockers are `C1` and `P5`, but the current record does not yet prove that either one forces cyclic structure, multi-SCC search, a second interface, fallback behavior, or another explicit boundary change. | `not selected on the refreshed read` |
| `narrowed unresolved / continue within the current architecture` | The repaired same-lane pocket proves that stable visible persistence is possible inside the inherited acyclic model on at least one exact route. The remaining blocker set is now narrower and more specific: `P1`, `P2`, and `P5`, with `C1` and quantified-boundary handling as the main unresolved reads. | This posture still falls well short of a repo-level capability win and does not settle whether a later boundary revision will be needed. | `selected` |

## Draft Current Repo-Scope Read

Draft strongest current read:
`narrowed unresolved / continue within the current architecture`

Why this is the strongest honest draft read:

1. `keep` is still too strong.
   The refreshed matrix does not yet satisfy representative success across the
   positive family matrix.
2. The old global reopen logic is no longer decisive.
   The repaired exact same-lane pocket now demonstrates stable visible
   persistence inside the inherited acyclic model, so the repo no longer sits
   at the earlier zero-visibility global failure point.
3. The remaining blockers are now narrower than before.
   `C1` still blocks non-local visibility, and `C3` still blocks positive
   nested-`forall` / polymorphism coverage, but neither blocker yet proves
   that the inherited acyclic representation itself must be revised.
4. The March 25 strategic posture therefore becomes the strongest honest
   carry-forward posture again, but on a narrower blocker set than March 25
   had available.

## Draft Remaining Blocker Set

- `P1 local-recursive-shape`
  still lacks review-visible unannotated automatic success.
- `P2 non-local-propagation`
  is still blocked by `C1` ending visibly non-recursive on current
  authoritative surfaces.
- `P5 polymorphism-nested-forall`
  is still reject-side only under the quantified-boundary guard.

The same-lane `C2` / `C5` / `C7` exact pocket is no longer the main global
blocker on this draft read.

## Recommended Next Live Controller Step

If this draft is promoted into a real successor revision, the next live
controller step should be:

1. freeze one post-`rev-004` repo-scope successor boundary that keeps the
   March 25 / March 26 aggregate artifacts immutable as historical evidence;
2. publish one authoritative refreshed representative matrix that carries
   forward the repaired `C2` / `C5` / `C7` read plus the bounded `C1` / `P5`
   probes; and
3. choose one narrowed successor gate:
   either continue bounded on `C1` / `P5` inside the current architecture, or
   reopen a boundary only if new evidence shows those remaining families
   cannot be carried inside the inherited model.

That would convert the current repo-level question from
`does any route survive visibly at all?`
to
`can the remaining non-local and quantified-boundary families be carried
inside the inherited model without another boundary change?`
