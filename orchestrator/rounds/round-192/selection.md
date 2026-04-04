# Round 192 - Task Selection

**Selected item**: item-6
**Item title**: Run the fail-closed negative-family and termination-pressure campaign
**Roadmap identity**:
- roadmap_id: 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap
- roadmap_revision: rev-001
- roadmap_dir: orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001
- roadmap_item_id: item-6

## Why now

`item-6` is the lowest-numbered unfinished roadmap item, its dependencies
`item-1`, `item-3`, `item-4`, and `item-5` are done, and no live retry state
forces a same-round retry.

Accepted `round-191` closed `item-5` through the bounded positive-family
aggregate classification and explicitly kept `item-6`,
`non-cyclic-graph`, and repo-level readiness unresolved. The active roadmap
now makes the next unfinished work concrete: run the minimum bounded evidence
campaign for representative `N1`, `N2`, and `N6` cases on the item-4
authoritative surfaces, reusing the nested-`forall` contrast in
`test/Research/P5ClearBoundarySpec.hs` and the disagreement, ambiguity, and
widened-search pressure checks in `test/PipelineSpec.hs`, without reopening
the item-3 route-family or guard contract.

The next honest move is therefore `item-6`, not another `item-5` packet and
not the `item-7` repo-level readiness / architecture decision. This family
already froze the fail-closed route/guard boundaries in accepted `round-179`
and the authoritative evaluation surfaces plus corpus obligations in accepted
`round-180`. What is still missing is one bounded current-architecture read
of whether the representative negative-family rows actually remain fail-closed
or bounded on those live surfaces after the positive-family campaign.
Jumping to `item-7` now would skip the still-required `N1` / `N2` / `N6`
evidence gate that the active roadmap now names explicitly.

## Current baseline

- Base branch: `codex/automatic-recursive-type-inference` at commit `ae64788`
  (`Advance full-inference roadmap after round-191`)
- Active roadmap revision: `rev-001`, with items `1` through `5` done and
  items `6` and `7` pending
- Live retry state: none recorded for this family (`retry: null`)
- Accepted `round-179` fixed the fail-closed search contract to the named
  route arms `rootNonLocalSchemeAliasBaseLike` and
  `sameLaneLocalRetainedChildTarget` plus the retained-child guard cluster
  `boundHasForallFrom` / `keepTargetFinal` / `targetC`; ambiguous anchors,
  owners, or binder-side placements stay reject-side, and `N6` stays bounded
  without fallback, cyclic search, multi-SCC handling, or equi-recursive
  widening
- Accepted `round-180` fixed the authoritative current surfaces and corpus
  obligations: `runPipelineElab`, `runPipelineElabChecked`, the matching
  internal/public pipeline facades, `test/Research/P5ClearBoundarySpec.hs`,
  and `test/PipelineSpec.hs` already carry the review-visible negative /
  pressure rows that `item-6` must now consume
- Accepted `round-191` closed `item-5` through
  `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`,
  classifying `P2` as `packet-specific folklore`,
  `P3` / `P4` / `P6` as `credible general support`, and `P5` as
  `current-architecture blockers`; that accepted artifact explicitly leaves
  `N1`, `N2`, `N6`, `non-cyclic-graph`, and repo-level readiness unresolved
- `test/Research/P5ClearBoundarySpec.hs` currently keeps the clear-boundary
  control recursive while `nestedForallContrastExpr` stays non-recursive and
  surfaces `PhiTranslatabilityError` at the authoritative entrypoints, so the
  quantified-boundary `N2` pressure is already visible as reject-side context
- `test/PipelineSpec.hs` currently keeps the authoritative entrypoint
  alignment and the fail-closed boundary checks review-visible: the suite
  already carries explicit no-ranking / no-rescue source guards around
  `sameLaneLocalRetainedChildTarget`, `keepTargetFinal`, and `targetC`, plus
  multiple fail-closed local-vs-non-local contrast checks once the selected
  wrapper leaves the local `TypeRef` lane
- No accepted artifact in this family yet reruns those representative
  negative/pressure rows after `item-5`, ties them back to the accepted
  item-3 contract on the item-4 authoritative surfaces, and records one
  honest aggregate read of `N1`, `N2`, and `N6`
- `N3`, `N4`, and `N5` remain out of scope for this round unless a later
  accepted roadmap revision explicitly reopens them
- Repository status in the canonical round worktree before writing this
  selection was one pre-existing controller-owned modification:
  `M orchestrator/state.json`
- Controller-prepared round branch:
  `orchestrator/round-192-negative-family-campaign`
- Controller-prepared round worktree: `orchestrator/worktrees/round-192`

## Scope

- Keep `item-6` active, but bind this round to exactly one bounded
  current-architecture campaign: representative `N1`, `N2`, and `N6`
  evidence on the item-4 authoritative surfaces only
- Reuse the already frozen negative / pressure corpus only:
  `nestedForallContrastExpr` plus the clear-boundary control in
  `test/Research/P5ClearBoundarySpec.hs`, and the existing disagreement,
  ambiguity, and widened-search pressure checks in `test/PipelineSpec.hs`
- Keep the campaign grounded in the accepted item-3 contract only:
  `rootNonLocalSchemeAliasBaseLike`,
  `sameLaneLocalRetainedChildTarget`, and the retained-child guard cluster
  `boundHasForallFrom` / `keepTargetFinal` / `targetC`
- Allow only the minimum focused hardening or test/doc evidence changes
  needed to show whether those representative rows still fail closed or stay
  bounded on `runPipelineElab` and `runPipelineElabChecked`; if the current
  evidence already suffices, the round should stop at that honest aggregate
  read instead of inventing extra work
- End with one bounded `item-6` aggregate read only: whether representative
  `N1`, `N2`, and `N6` behavior remains fail-closed or bounded under the
  current architecture, without counting any ambiguous, unsound, or
  widened-search case as positive success
- Do not reopen the item-3 route-family or guard contract, do not add
  ranking / tie-breaking / heuristic candidate selection, do not widen into
  cyclic search, multi-SCC handling, equi-recursive reasoning, fallback
  behavior, or a second interface, and do not reopen `non-cyclic-graph`
- Do not promote reject-side nested-`forall` behavior into positive `P5`
  success, do not broaden into `N3`-`N5`, and do not jump ahead to the
  `item-7` repo-level readiness / architecture decision
