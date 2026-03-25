# Round 097 Selection

Date: 2026-03-26
Round: `round-097`
Role: guider
Active subject: same-lane retained-child public-output continuity vs
`non-cyclic-graph` successor loop after accepted roadmap item `3`
Successor lane: roadmap item `4` only, revalidating the frozen same-lane
retained-child pocket end to end and classifying its public-output
continuity result

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

Roadmap item `4`: revalidate the frozen pocket end to end and classify its
public-output continuity result.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-097"`, `stage: "select-task"`, `current_task: null`,
`last_completed_round: "round-096"`, and `retry: null`. The authoritative
retry contract in
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
only overrides roadmap order when a live retry object is present. No retry
object is active, so the normal lowest-numbered-unfinished-item rule governs
this round.

The authoritative roadmap at
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
now marks items `1`, `2`, and `3` done, with item `4` as the first
unfinished dependency-satisfied item. Item `5` depends on item `4`, so item
`4` is the only lawful next step.

The accepted predecessor chain makes the item-4 handoff exact:

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
  that this bounded `Pipeline.hs` / `TermClosure.hs` root-handoff slice does
  not contain an alternate recursive whole-packet authoritative result:
  `rootScheme`, `typeCheck termClosed`, and the root-level fallback all stay
  at `TForall "a" Nothing (TVar "a")`, so the exact public-output collapse
  remains blocker debt within the unchanged current architecture.

That means the next missing step is no longer another exact-path audit and
not yet the bounded architecture-pressure decision. The next missing step is
the item-4 revalidation itself: rerun the exact frozen pocket across the
accepted ledger after the confirm-only item-3 result, then classify exactly
one lawful exact-pocket continuity outcome for that same frozen tuple.

The inherited boundary and predecessor contracts remain unchanged while item
`4` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the loop to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. Accepted `N14`, accepted strategic items `2`, `5`, `6`,
and `7`, accepted rounds `089` through `093`, and accepted refreshed rounds
`094` through `096` remain bounded predecessor evidence only. They do not
authorize widening into the non-local alias-bound family, neighboring
consumer routes, nested-`forall` success, replay repair, cyclic structural
search, a second interface, fallback widening, or a reopened
`non-cyclic-graph` decision during item `4`.

`Bugs.md` still lists open `BUG-2026-03-16-001`, but that replay /
`InstBot` defect remains predecessor context only. It creates no retry
obligation here and does not authorize leaving roadmap order or widening
this exact public-output continuity gate.

## Round Scope Guard

- This round is limited to roadmap item `4` only.
- Revalidate only the refreshed frozen case carried by accepted items `1`,
  `2`, and `3`: same-lane retained-child family, `boundVarTargetRoot`, one
  owner-local retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact frozen packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`.
- Consume exactly one accepted continuity split:
  helper-visible/internal recursive structure
  (`TMu ...`, `containsMu True`) versus authoritative public output
  `TForall "a" Nothing (TVar "a")`.
- Treat the accepted item-3 confirm-only result as predecessor evidence only:
  the bounded root-handoff slice did not repair the collapse, so item `4`
  must now classify the same exact pocket honestly rather than reopen that
  repair slice.
- Record exactly one lawful exact-pocket item-4 outcome:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`.
- If any later row still depends on route switching, quantified crossing,
  witness-only rescue, packet-history-only rescue, replay / `InstBot`
  reasoning, fallback-like recovery, or any new family / owner / anchor
  reinterpretation, classify that honestly as blocker debt or fail-closed
  rejection rather than widening the subject.
- Do not widen into the non-local alias-bound family, neighboring consumer
  routes, nested-`forall` success, replay-family repair, broad automatic
  recursive inference, equi-recursive reasoning, cyclic structural graphs,
  multi-SCC search, second-interface work, or fallback widening.
- Do not reopen `non-cyclic-graph` during item `4`; roadmap item `5` alone
  may consume the accepted item-4 classification and make that bounded
  architecture-pressure decision.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact frozen same-lane retained-child pocket still preserves recursive
  structure on the helper-visible/internal path while both authoritative
  public entrypoints still return `TForall "a" Nothing (TVar "a")`;
- accepted `round-096` confirmed that the authorized bounded root-handoff
  slice does not clear that collapse, so item `4` still lacks an accepted
  end-to-end classification for the same exact pocket after the confirm-only
  item-3 result;
- `non-cyclic-graph = unknown` remains architecture-pressure context only,
  not authority to reopen revision during item `4`; and
- open `BUG-2026-03-16-001` remains predecessor replay context only, not
  authority to divert away from this same-pocket revalidation gate.
