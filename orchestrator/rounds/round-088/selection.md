# Round 088 Selection

Date: 2026-03-25
Round: `round-088`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `6`
Successor lane: roadmap item `7` only, running the aggregate architecture
decision and successor-choice gate from the accepted `bounded subset only`
coverage result

## Roadmap Provenance

- Roadmap ID: `2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap`
- Roadmap Revision: `rev-007`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-007`
- State Snapshot: `orchestrator/rounds/round-088/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 7: make the architecture decision and successor-plan choice from
the bounded-subset-only coverage result.

## Why This Item Should Run Now

`orchestrator/rounds/round-088/state-snapshot.json`
fixes the live controller state at `active_round_id: "round-088"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-088-item-7-architecture-decision"`, `active_round_dir:
"orchestrator/rounds/round-088"`, and `last_completed_round: "round-087"`.
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-007/retry-subloop.md`
only overrides roadmap order when a live retry object is present. `retry` is
currently `null`, so the normal lowest-numbered unfinished item rule still
governs.

`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-007/roadmap.md`
marks items `1` through `6` done and item `7` pending. Item `7` depends on
items `2` and `6`, both of which are already accepted and recorded as
complete, so it is now the next lawful successor and the only unfinished item
on the live roadmap.

Accepted `round-087` finalized item `6` in
`orchestrator/rounds/round-087/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"representative-coverage-and-feasibility-campaign-classified-as-bounded-subset-only"`.
The canonical artifact
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
fixes the aggregate evidence that item `7` must now consume: zero
`stable visible persistence` rows, blocker debt in both admitted families,
nested-`forall` remaining reject-side only, and the still-live item-2
`non-cyclic-graph = unknown` risk. That read is explicitly stronger than
"another bounded packet works" but weaker than either broad generality or an
architectural dead end, so the next missing step is the explicit fork
decision itself.

The accepted item-2 audit in
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
already narrowed the live strategic uncertainty to one unresolved
architecture-pressure point: `non-cyclic-graph = unknown`. The accepted item-6
coverage result did not resolve that risk, but it also did not collapse the
current architecture into a proved dead end. Item `7` therefore has a
concrete bounded job: turn the accepted `bounded subset only` read into one
explicit strategic outcome instead of letting the control plane drift.

`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
places the architecture fork immediately after the representative coverage
campaign. Running anything broader first would skip the roadmap's explicit
decision gate. Reopening mechanism design, search design, reconstruction
contracts, or implementation work here would also silently widen scope beyond
what the accepted roadmap currently authorizes.

The inherited baseline remains fixed.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. The accepted `N14` predecessor decision at
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves earlier packet wins as bounded evidence only. Item `7` must
therefore make a strategic decision from bounded evidence without treating
that evidence as if it already proves general automatic iso-recursive
inference.

`Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry
obligation or change roadmap order. Repository status in the active worktree
shows only controller-owned `M orchestrator/rounds/round-088/state-snapshot.json` drift. No live
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `7` only.
- Record exactly one explicit strategic outcome for the refreshed control
  plane: `continue within the current architecture`, `pursue targeted
  boundary revision`, or `stop`.
- Base the decision on the accepted item-2 audit and the accepted item-6
  `bounded subset only` matrix result, including zero `stable visible
  persistence` rows, blocker debt in both admitted families, nested-`forall`
  reject-side status, and the still-live `non-cyclic-graph = unknown` risk.
- If the decision is `continue`, name exactly one concrete bounded successor
  cycle and why it is the next lawful evidence gate.
- If the decision is `pursue targeted boundary revision`, name the first
  lawful revision gate and the boundary under review.
- If the decision is `stop`, preserve predecessor truth and state why broader
  work is not yet justified.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, bug tracker, or
  review/merge artifacts.
- Do not blur multiple outcomes together, silently authorize broad
  implementation, silently revise the inherited boundary, or reinterpret
  bounded predecessor evidence as a proof of general automatic
  iso-recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active strategic blockers that must remain blockers rather than widened work:

- the item-6 matrix contains zero `stable visible persistence` rows;
- both admitted families still carry `admitted but not
  reconstruction-visible / blocker debt` rather than accepted positive
  full-pipeline success;
- nested-`forall` pressure remains reject-side only; and
- the accepted item-2 `non-cyclic-graph = unknown` risk is still unresolved.
