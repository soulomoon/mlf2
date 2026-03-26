# Round 103 Selection

Date: 2026-03-26
Round: `round-103`
Role: guider
Active subject: global `non-cyclic-graph` settlement and automatic
iso-recursive inference roadmap family after the accepted item-4
representative family-matrix settlement campaign in `round-102`

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-001`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  controller-owned machine-state edit only (`git status --short --branch`
  returned `## codex/round-103` and `M orchestrator/state.json`)

## Selected Roadmap Item

Roadmap item `5`: decide the global `non-cyclic-graph` settlement gate.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state for this packet at
`active_round_id: "round-103"`, `stage: "select-task"`,
`current_task: null`, `last_completed_round: "round-102"`, and
`retry: null`. No live same-round retry object exists, so the retry contract
does not force a retry loop. The guider rule therefore remains: select the
lowest-numbered unfinished roadmap item whose dependencies are satisfied.

The active roadmap bundle already records items `1` through `4` as done, and
item `5` is now the lowest-numbered unfinished item. Its dependencies are
fully satisfied by the accepted item-1 freeze in
`docs/plans/2026-03-26-global-non-cyclic-graph-settlement-contract-and-unresolved-family-evidence-ledger.md`,
the accepted item-2 and item-3 bounded settlement slices, and the accepted
item-4 representative campaign in
`docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`.
Items `6` through `8` remain blocked behind item `5`, and the roadmap plus
guider contract explicitly forbid production implementation before accepted
item `5` records `non-cyclic-graph = keep`.

Item `5` is also the exact aggregate-only gate that the accepted evidence now
hands off to. The item-1 freeze already fixed the lawful settlement bar:
`keep` is allowed only if accepted production-surface evidence makes
representative family-matrix settlement credible inside the inherited acyclic
model, while `reopen` is allowed only if accepted evidence shows that at
least one required positive family cannot be carried inside that model
without crossing the current architecture boundary. The accepted item-4 matrix
refresh is now the binding aggregate input to that gate: zero
`stable visible persistence` rows, five
`admitted but not reconstruction-visible / blocker debt` rows
(`P1-row`, `C1`, `C2`, `C5`, `C7`), three `fail-closed rejection` rows
(`C3`, `C4`, `C6`), `P5` still reject-side only, `P6` still below visible
persistence, and `N4` still pressure context only rather than an already
accepted reopen result. That is exactly the evidence posture item `5` must
consume, and item `4` expressly leaves item `5` as the next immediate
consumer.

The accepted predecessor chain also keeps this choice honest. The March 25
capability contract still fixes the representative positive families `P1`
through `P6` and required negative or bounded families `N1` through `N6` as
the repo-level claim bar. The March 25 architectural audit still records
`non-cyclic-graph = unknown`, not `keep` or `revise`. The March 25
full-pipeline contract still makes `stable visible persistence` the only
lawful positive `P6` success token. The accepted same-lane exact-pocket chain
from rounds `round-094` through `round-098` and its refreshed bounded
continuity artifacts remain predecessor evidence only: they strengthen one
pressure point, but they do not settle the repo-level gate by themselves.
After accepted `round-102`, the next lawful move is therefore the aggregate
settlement decision itself, not another bounded evidence slice and not item
`6` implementation work.

The inherited baseline remains unchanged while item `5` runs.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still keeps the explicit-only / iso-recursive / non-equi-recursive /
non-cyclic-graph / no-fallback boundary live. Open `BUG-2026-03-16-001` in
`Bugs.md` remains predecessor replay context only; it does not create a retry
obligation here and does not authorize roadmap reordering, cyclic search,
multi-SCC search, second interfaces, fallback widening, or production
implementation.

## Round Scope Guard

- This round is limited to roadmap item `5` only.
- The output must record exactly one aggregate outcome:
  `non-cyclic-graph = keep` or
  `reopen the non-cyclic-graph revision question`.
- The output must consume only the accepted item-1 freeze, accepted item-2
  and item-3 bounded slices, and the accepted item-4 representative matrix
  refresh.
- The output must stay aggregate-only: no new evidence reruns, no production
  implementation, no hardening, no capability claim, and no silent widening
  into cyclic search, multi-SCC search, second interfaces, or fallback
  paths.
- If the accepted outcome is `reopen the non-cyclic-graph revision question`,
  later guider work must keep the next move inside a same-family roadmap
  revision rather than silently continuing into item `6`.
- Do not edit `orchestrator/state.json` or production code in this round.

## Blockers

No live retry obligation is present.

Active bounded blockers that must remain bounded rather than widened work:

- the accepted matrix still has zero `stable visible persistence` rows;
- `P2`, `P3`, `P4`, and `P6` remain blocker-debt families, while `P5`
  remains reject-side only;
- `N4` remains architecture-pressure context only, not an already accepted
  reopen result; and
- item `6` implementation, item `7` hardening, and item `8` capability-claim
  work remain blocked until this aggregate gate records the lawful settlement
  outcome.
