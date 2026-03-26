# Round 100 Selection

Date: 2026-03-26
Round: `round-100`
Role: guider
Active subject: global `non-cyclic-graph` settlement and automatic
iso-recursive inference roadmap family after the accepted item-1 settlement
contract freeze in `round-099`

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-100` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `2`: produce the bounded production-surface settlement evidence
slice for `C1`, `C2`, and `C5`.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-100"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-099"`, and
`retry: null`. No live same-round retry object exists, so the retry contract
does not force a retry loop. The default guider rule therefore still applies:
select the lowest-numbered unfinished roadmap item.

The active roadmap bundle already records item `1` as done from accepted
`round-099`, and item `2` is now the lowest-numbered unfinished item. Item
`2` depends only on item `1`, which is satisfied by the canonical freeze in
`docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`.
Items `3` through `5` depend on item `2`, while items `6` through `8` remain
blocked until item `5` records `non-cyclic-graph = keep`. Selecting anything
later would skip roadmap order and violate the pre-item-5 settlement-evidence
boundary.

The accepted item-1 freeze also hands off directly to item `2`. The frozen
ledger keeps repo-level `non-cyclic-graph` settlement unresolved, preserves
`non-cyclic-graph = unknown` as the live architecture-pressure axis, and says
the next missing proof work is bounded production-surface evidence for the
unresolved propagation and placement families inside the inherited acyclic
model. The accepted representative campaign keeps the repo-level feasibility
read at `bounded subset only`, with `C1`, `C2`, `C5`, and `C7` all still
classified as
`admitted but not reconstruction-visible / blocker debt` and with zero
accepted `stable visible persistence` rows. That makes item `2` the right
next move: it narrows the next round to the settlement slice that can sharpen
`P2`, `P3`, and `P4` without pretending the repo has already cleared the
family matrix.

The selected slice is also the narrowest lawful continuation of the accepted
same-lane predecessor evidence. The refreshed exact-pocket chain for the
same-lane retained-child case still ends at
`admitted but not reconstruction-visible / blocker debt` inside the current
architecture, not at global success and not at
`reopen the non-cyclic-graph revision question`. That exact pocket remains
bounded predecessor evidence only. Item `2` is where the roadmap explicitly
widens from that one exact pocket into a bounded production-surface evidence
slice for `C1` non-local propagation, `C2` same-lane retained-child
continuity, and `C5` binder-sensitive / owner-sensitive placement.

The inherited baseline remains unchanged while item `2` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still keeps the explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary live. The accepted full-pipeline
contract still requires review-visible continuity on the existing solver ->
elaboration -> reconstruction -> internal/public output surfaces before any
positive `P6` read is lawful. Open `BUG-2026-03-16-001` in `Bugs.md` remains
predecessor replay context only; it does not create a retry obligation here
and does not authorize roadmap reordering, production implementation, cyclic
search, multi-SCC search, second interfaces, or fallback widening.

## Round Scope Guard

- This round is limited to roadmap item `2` only.
- The output must stay settlement-evidence-only and bounded to `C1`, `C2`,
  and `C5` on the existing production surfaces.
- The output must classify the selected slice honestly against the frozen
  ledger vocabulary:
  `stable visible persistence`,
  `admitted but not reconstruction-visible / blocker debt`, or
  `fail-closed rejection`.
- The output may consume accepted predecessor evidence, but it must preserve
  the truth that accepted rounds `round-094` through `round-098` remain exact
  pockets only and do not settle repo-level `non-cyclic-graph` by
  themselves.
- Do not widen into item `3` mechanism work, item `4` campaign work, item
  `5` global settlement, production implementation, hardening, cyclic search,
  multi-SCC search, second interfaces, fallback paths, or a broad capability
  claim.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- repo-level `non-cyclic-graph` settlement is still unresolved;
- the accepted matrix still has zero `stable visible persistence` rows;
- `C1`, `C2`, and `C5` remain blocker-debt pockets rather than accepted
  production-surface success; and
- `BUG-2026-03-16-001` remains open predecessor replay context only, not
  authority to skip the item-2 settlement slice.
