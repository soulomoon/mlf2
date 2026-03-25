# Round 096 Selection

Date: 2026-03-26
Round: `round-096`
Role: guider
Active subject: same-lane retained-child public-output continuity vs
`non-cyclic-graph` successor loop at its third bounded item
Successor lane: roadmap item `3` only, clearing or confirming the exact
authoritative public-output collapse within the current architecture for the
same frozen pocket

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

Roadmap item `3`: clear or confirm the exact authoritative public-output
collapse within the current architecture.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-096"`, `stage: "select-task"`, `current_task: null`,
`last_completed_round: "round-095"`, and `retry: null`. The authoritative
retry contract in
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/retry-subloop.md`
only overrides roadmap order when a live retry object is present. No retry
object is active, so the normal lowest-numbered-unfinished-item rule governs
this round.

The authoritative roadmap at
`orchestrator/roadmaps/2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap/rev-001/roadmap.md`
now marks items `1` and `2` done and items `3` through `5` pending. Item `3`
depends only on items `1` and `2`, and accepted `round-094` plus accepted
`round-095` already finalized those dependencies. Item `3` is therefore the
first unfinished dependency-satisfied item and the only lawful next step.

The accepted predecessor chain is now exact enough to narrow this round to
one minimum bounded implementation-or-proof gate only:

- accepted rounds `089` through `093` froze the exact same-lane
  retained-child pocket, localized and cleared the earlier exact
  `Phase 6 (elaboration)` breakpoint, revalidated the same pocket end to end,
  and fixed the bounded predecessor posture
  `blocker debt remains within the current architecture`;
- accepted `round-094` froze the refreshed case and review ledger in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
  for exactly one pocket:
  same-lane retained-child family,
  `boundVarTargetRoot`,
  one owner-local retained-child frame,
  route `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact `ELet "k" ...` packet with
  `recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))`;
- accepted `round-095` audited the exact authoritative public-output path in
  `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
  and fixed the unchanged exact collapse anchor:
  helper-visible/internal recursive structure still remains
  `TMu ...` plus `containsMu True`,
  both authoritative public entrypoints still return
  `TForall "a" Nothing (TVar "a")`, and
  `checkedAuthoritative` remains the first exact owner-local
  continuity-loss site, with `termClosed` and `typeCheck termClosed` as the
  same-pocket dependencies that feed that authoritative result.

That means the next missing step is not another pure audit, not end-to-end
revalidation yet, and not another architecture decision yet. The next missing
step is to consume only that exact unchanged-anchor audit and determine
whether the current architecture can clear the exact public-output collapse
for this same pocket with a minimum bounded docs/code/test slice, or whether
the accepted evidence for this same pocket can only confirm that the collapse
remains blocker debt inside the unchanged current architecture.

The inherited boundary docs remain unchanged while item `3` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the loop to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. Accepted `N14`, accepted strategic items `2`, `5`, `6`,
and `7`, plus accepted rounds `089` through `095`, remain bounded predecessor
evidence only. They do not authorize widening into the non-local alias-bound
family, neighboring consumer routes, nested-`forall` success, replay repair,
cyclic structural search, a second interface, fallback widening, or a
reopened `non-cyclic-graph` revision claim on this round.

`Bugs.md` still lists open `BUG-2026-03-16-001`, but that replay /
`InstBot` defect remains predecessor context only. It creates no retry
obligation here and does not authorize widening away from this exact
public-output continuity gate.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- Clear or confirm the exact public-output collapse for exactly one
  same-lane retained-child pocket:
  same-lane retained-child family, `boundVarTargetRoot`, one owner-local
  retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary-only status, and the exact frozen packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`.
- Consume exactly one continuity split:
  helper-visible/internal recursive structure
  (`TMu ...`, `containsMu True`) versus authoritative public output
  `TForall "a" Nothing (TVar "a")`.
- Consume exactly one accepted unchanged-anchor audit:
  `checkedAuthoritative` remains the first exact owner-local break, with
  `termClosed` and `typeCheck termClosed` as the same-pocket dependencies.
- Treat accepted rounds `089` through `095`, accepted `N14`, and accepted
  strategic items `2`, `5`, `6`, and `7` as bounded predecessor evidence
  only. Do not relitigate those accepted stages.
- Either land the minimum bounded docs/code/test slice needed so the exact
  frozen pocket preserves recursive structure on the authoritative public
  output surface without changing family, anchor, owner-local frame, route,
  or clear-boundary status, or record bounded proof that the collapse remains
  blocker debt under the unchanged current architecture.
- Do not widen into the non-local alias-bound family, neighboring consumer
  routes, nested-`forall` success, replay / `InstBot` repair, broad
  automatic-recursive-inference claims, equi-recursive reasoning, cyclic
  structural graphs, multi-SCC search, second-interface work, fallback
  widening, or architecture revision.
- Do not reopen `non-cyclic-graph` on item `3`; this round clears or
  confirms the exact authoritative public-output collapse within the current
  architecture only.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the exact same frozen pocket still loses recursive structure at the
  authoritative public surface while helper-visible/internal reconstruction
  preserves it;
- accepted `round-095` fixed the first exact owner-local collapse anchor as
  `checkedAuthoritative`, so the next lawful missing work is the item-3
  minimum bounded clear-or-confirm gate for that same pocket only;
- `non-cyclic-graph = unknown` remains unresolved architecture-pressure
  context only, not an accepted reopen; and
- open `BUG-2026-03-16-001` remains predecessor replay context only.
