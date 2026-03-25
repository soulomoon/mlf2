# Round 095 Selection

Date: 2026-03-26
Round: `round-095`
Role: guider
Active subject: same-lane retained-child public-output continuity vs
`non-cyclic-graph` successor loop at its second bounded item
Successor lane: roadmap item `2` only, auditing the exact authoritative
public-output path for the same frozen pocket

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

Roadmap item `2`: audit the exact authoritative public-output path for the
frozen pocket.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-095"`, `stage: "select-task"`, `current_task: null`,
`last_completed_round: "round-094"`, and `retry: null`. The authoritative
retry contract in
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
only overrides roadmap order when a live retry object is present. No retry
object is active, so the normal lowest-numbered-unfinished-item rule governs
this round.

The authoritative roadmap at
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
now marks item `1` done and items `2` through `5` pending. Item `2` depends
only on item `1`, and accepted `round-094` already finalized that dependency.
Item `2` is therefore the first unfinished dependency-satisfied item and the
only lawful next step.

The accepted predecessor chain is already exact enough to narrow this round
to one public-path audit only:

- accepted rounds `089` through `093` froze the exact same-lane
  retained-child pocket, localized and cleared the earlier exact
  `Phase 6 (elaboration)` breakpoint, revalidated the same pocket end to end,
  and fixed the bounded successor posture
  `blocker debt remains within the current architecture`;
- accepted `round-094` froze the refreshed case and review ledger in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
  for exactly one pocket:
  same-lane retained-child family,
  `boundVarTargetRoot`,
  one owner-local retained-child frame,
  route `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact `ELet "k" ...` packet with
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`; and
- that accepted item-1 artifact also freezes the exact continuity split now
  under review:
  helper-visible/internal recursive structure remains `TMu ...` plus
  `containsMu True`,
  while both authoritative public entrypoints still return
  `TForall "a" Nothing (TVar "a")`.

That means the next missing step is not repair, revalidation, or another
architecture decision. The next missing step is to inspect only the exact
authoritative public-output path for the already-frozen pocket and localize
the first exact owner-local continuity-loss site between the accepted
helper-visible reconstruction path and the authoritative public-output path.

The inherited boundary docs remain unchanged while item `2` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the loop to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. Accepted `N14`, accepted strategic items `2`, `5`, `6`,
and `7`, plus accepted rounds `089` through `094`, remain bounded predecessor
evidence only. They do not authorize widening into the non-local alias-bound
family, neighboring consumer routes, nested-`forall` success, replay repair,
or a reopened `non-cyclic-graph` revision claim on this round.

`Bugs.md` still lists open `BUG-2026-03-16-001`, but that replay /
`InstBot` defect remains predecessor context only. It creates no retry
obligation here and does not authorize widening away from this exact
public-output continuity gate.

## Round Scope Guard

- This round is limited to roadmap item `2` only.
- Audit exactly one same-lane retained-child pocket:
  same-lane retained-child family, `boundVarTargetRoot`, one owner-local
  retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact frozen packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`.
- Audit exactly one continuity split:
  helper-visible/internal recursive structure
  (`TMu ...`, `containsMu True`) versus authoritative public output
  `TForall "a" Nothing (TVar "a")`.
- Localize only the first exact owner-local continuity-loss site on the
  authoritative public-output path, including `checkedAuthoritative` and any
  exact same-pocket anchors it depends on.
- Treat accepted rounds `089` through `094`, accepted `N14`, and accepted
  strategic items `2`, `5`, `6`, and `7` as bounded predecessor evidence
  only. Do not relitigate those accepted stages.
- Do not repair behavior yet.
- Do not widen into the non-local alias-bound family, neighboring consumer
  routes, nested-`forall` success, replay / `InstBot` repair, broad
  automatic-recursive-inference claims, or architecture revision.
- Do not reopen `non-cyclic-graph` on item `2`; this round audits the exact
  authoritative public-output path only.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact same frozen pocket still loses recursive structure at the
  authoritative public surface while helper-visible/internal reconstruction
  preserves it;
- accepted `round-094` fixed the exact case and review ledger, so the next
  lawful missing work is the item-2 public-path audit for that same pocket;
- `non-cyclic-graph = unknown` remains unresolved architecture-pressure
  context only, not an accepted reopen; and
- open `BUG-2026-03-16-001` remains predecessor replay context only.
