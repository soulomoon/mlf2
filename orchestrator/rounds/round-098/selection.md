# Round 098 Selection

Date: 2026-03-26
Round: `round-098`
Role: guider
Active subject: same-lane retained-child public-output continuity vs
`non-cyclic-graph` successor loop after accepted roadmap item `4`
Successor lane: roadmap item `5` only, deciding whether the accepted exact
same-lane retained-child blocker-debt result keeps the current architecture
credible or reopens `non-cyclic-graph` for this one bounded pressure point

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only
  (`git status --short --untracked-files=all` returned
  `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `5`: decide whether the accepted public-output result keeps the
current architecture credible or reopens `non-cyclic-graph`.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-098"`, `stage: "select-task"`, `current_task: null`,
`last_completed_round: "round-097"`, and `retry: null`. The authoritative
retry contract in
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
only overrides roadmap order when a live retry object is present. No retry
object is active, so the normal lowest-numbered-unfinished-item rule governs
this round.

The authoritative roadmap at
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
now marks items `1` through `4` done, with item `5` as the first unfinished
dependency-satisfied item. Item `5` depends only on items `2` and `4`, and
both are now accepted and complete. Item `5` is therefore the only lawful
next step.

The accepted predecessor chain makes the item-5 handoff exact:

- accepted `round-094` finalized item `1` and froze the refreshed exact
  same-lane retained-child pocket plus review ledger in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`:
  family `same-lane retained-child`, anchor `boundVarTargetRoot`, one
  owner-local retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact `ELet "k" ...` packet with
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`;
- accepted `round-095` finalized item `2` and fixed the unchanged exact
  authoritative-path audit in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`:
  helper-visible/internal reconstruction still carries
  `TMu ...` plus `containsMu True`, both authoritative public entrypoints
  still return `TForall "a" Nothing (TVar "a")`, and
  `checkedAuthoritative` remains the first exact owner-local break, with
  `termClosed` and `typeCheck termClosed` as the same-pocket dependencies;
- accepted `round-096` finalized item `3` and confirmed in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
  that the bounded `Pipeline.hs` / `TermClosure.hs` root-handoff slice does
  not contain an alternate recursive whole-packet authoritative result, so
  the exact public-output collapse remains blocker debt within the unchanged
  current architecture; and
- accepted `round-097` finalized item `4` and fixed the refreshed exact
  item-4 outcome in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`:
  helper-visible/internal recursive structure still shows
  `TMu ...` plus `containsMu True`, both authoritative public entrypoints
  still return `TForall "a" Nothing (TVar "a")`, the public output surface
  remains the first actual continuity breakpoint, and the exact-pocket
  token is now fixed at
  `admitted but not reconstruction-visible / blocker debt`.

That means the next missing step is no longer any replay, classification, or
repair slice. The next missing step is the bounded architecture-pressure
decision itself: consume only the accepted exact-pocket blocker-debt result
and decide whether that accepted evidence keeps the inherited current
architecture / `non-cyclic-graph` boundary credible for this one pressure
point, or whether it specifically forces reopening the `non-cyclic-graph`
revision question.

The inherited boundary and predecessor contracts remain unchanged while item
`5` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the loop to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. Accepted `N14`, accepted strategic items `2`, `5`, `6`,
and `7`, accepted rounds `089` through `093`, and accepted refreshed rounds
`094` through `097` remain bounded predecessor evidence only. They do not
authorize widening into the non-local alias-bound family, neighboring
consumer routes, nested-`forall` success, replay repair, cyclic structural
search, a second interface, fallback widening, or any broader architecture
claim beyond the one explicit item-5 decision.

`Bugs.md` still lists open `BUG-2026-03-16-001`, but that replay /
`InstBot` defect remains predecessor context only. It creates no retry
obligation here and does not authorize leaving roadmap order or widening
this exact public-output continuity gate.

The retry contract matters specially for item `5`: it is aggregate-only, so
review may reject and return the same round to `plan`, but `accepted + retry`
is not lawful for this roadmap item. That makes the round even more tightly
bounded to one final exact-pocket decision record.

## Round Scope Guard

- This round is limited to roadmap item `5` only.
- Consume only the refreshed frozen case carried by accepted items `1`
  through `4`: same-lane retained-child family, `boundVarTargetRoot`, one
  owner-local retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact frozen packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`.
- Consume exactly one accepted continuity split:
  helper-visible/internal recursive structure
  (`TMu ...`, `containsMu True`) versus authoritative public output
  `TForall "a" Nothing (TVar "a")`.
- Treat the accepted item-4 token
  `admitted but not reconstruction-visible / blocker debt`
  as fixed predecessor evidence only. Item `5` must decide what that exact
  accepted result means for this bounded architecture-pressure question,
  not rerun classification or reopen repair work.
- Record exactly one lawful exact-pocket item-5 outcome:
  `blocker debt remains within the current architecture`; or
  `reopen the non-cyclic-graph revision question`.
- Do not widen into the non-local alias-bound family, neighboring consumer
  routes, nested-`forall` success, replay-family repair, broad automatic
  recursive inference, equi-recursive reasoning, cyclic structural graphs,
  multi-SCC search, second-interface work, or fallback widening.
- Do not add a second successor lane or a broader architecture redesign.
  This round may only decide the one explicit
  current-architecture-vs-`non-cyclic-graph` question for this one exact
  pressure point.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact frozen same-lane retained-child pocket still preserves recursive
  structure on the helper-visible/internal path while both authoritative
  public entrypoints still return `TForall "a" Nothing (TVar "a")`;
- accepted `round-097` fixed the refreshed exact item-4 token as
  `admitted but not reconstruction-visible / blocker debt`, but item `5`
  has not yet recorded the bounded architecture-pressure meaning of that
  accepted result;
- `non-cyclic-graph = unknown` remains inherited architecture-pressure
  context until item `5` decides whether the accepted exact-pocket evidence
  is still compatible with the current architecture or specifically forces a
  reopen; and
- open `BUG-2026-03-16-001` remains predecessor replay context only, not
  authority to divert away from this same-pocket decision gate.
