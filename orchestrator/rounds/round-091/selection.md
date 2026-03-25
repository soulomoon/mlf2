# Round 091 Selection

Date: 2026-03-25
Round: `round-091`
Role: guider
Active subject: same-lane retained-child stable-visible-persistence
successor loop after accepted roadmap item `2`
Successor lane: roadmap item `3` only, clearing or confirming the exact
Phase 6 elaboration breakpoint for the frozen same-lane retained-child
pocket

## Roadmap Provenance

- Roadmap ID: `2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap`
- Roadmap Revision: `rev-003`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003`
- State Snapshot: `orchestrator/rounds/round-091/state-snapshot.json`
- Migration note: backfilled from the active round selection packet; live resumability continues in `orchestrator/state.json`.

## Selected Roadmap Item

Roadmap item `3`: clear or confirm the exact Phase 6 elaboration breakpoint
for the frozen same-lane retained-child pocket.

## Why This Item Should Run Now

`orchestrator/rounds/round-091/state-snapshot.json` fixes the selection-time controller state for this packet at
`active_round_id: "round-091"`, `stage: "select-task"`, `current_task: null`,
`retry: null`, `branch: "codex/round-091"`, `active_round_dir:
"orchestrator/rounds/round-091"`, and `last_completed_round: "round-090"`.
The live resumable controller state for this still-open round continues in `orchestrator/state.json` inside the active worktree.
`orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/retry-subloop.md` only overrides roadmap order when a live
retry object is present. `retry` is currently `null`, so the normal
lowest-numbered-unfinished-item rule still governs this round.

`orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/roadmap.md` marks items `1` and `2` done and item `3` as the
first unfinished item. Item `4` depends on item `3`, and item `5` depends on
item `4`. Item `3` is therefore the lowest-numbered unfinished
dependency-satisfied successor and the only lawful next round.

Accepted `round-090` finalized item `2` in
`orchestrator/rounds/round-090/review-record.json` with
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"same-lane-retained-child-first-breakpoint-localized-to-phase-6-elaboration"`.
The canonical breakpoint audit at
`docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`
now localizes the first exact-pocket continuity break for the frozen case:
the exact packet
`ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`
survives solver admission on the same-lane retained-child route, but both
`runPipelineElab` and `runPipelineElabChecked` fail in
`Phase 6 (elaboration)` with
`PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`.
That is now the exact bounded gap item `3` must address.

The roadmap already says what item `3` owns after that audit: either land the
minimum bounded slice needed so this exact frozen packet survives Phase 6
without changing family, anchor, owner-local frame, route, or
quantified-boundary status, or record bounded proof that this exact
elaboration gap remains blocker debt under the unchanged architecture. That
means the next missing step is no longer another audit and not yet end-to-end
classification or successor decision. The next missing step is to clear or
confirm this one exact elaboration break.

The accepted predecessor chain makes that ordering necessary.
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
and
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
still classify this same pocket as the strongest bounded candidate but only as
`admitted but not reconstruction-visible / blocker debt`.
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
selected exactly one successor gate inside the unchanged architecture:
determine whether this strongest same-lane retained-child pocket can cross
from blocker debt into accepted `stable visible persistence`. Item `4`
cannot honestly revalidate the pocket and item `5` cannot honestly make the
bounded successor decision until item `3` resolves whether the exact Phase 6
elaboration failure can be cleared at all.

The inherited boundary remains fixed.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. The accepted `N14` predecessor decision at
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves one exact same-lane retained-child packet as bounded
predecessor evidence only. It does not authorize widening into the
alias-bound family, neighboring routes, nested-`forall` success, general
automatic inference, or a reopened `non-cyclic-graph` revision argument.

`Bugs.md` still lists open `BUG-2026-03-16-001`, but that replay / `InstBot`
defect remains predecessor implementation context only and does not change
roadmap order for this successor loop. The migration only backfills roadmap provenance for this packet.
No live blocker or retry obligation forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- Work only on the frozen case from
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-case-and-review-ledger.md`
  and the exact breakpoint localized in
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-breakpoint-audit.md`:
  same-lane retained-child family, `boundVarTargetRoot`, one owner-local
  retained-child frame, route
  `sameLaneLocalRetainedChildTarget -> keepTargetFinal -> targetC`,
  clear-boundary status only, and the exact Phase 6 elaboration error above.
- Lawful item-3 outputs are limited to one of two bounded reads:
  either the minimum exact-pocket slice needed so this same packet survives
  Phase 6 elaboration under the unchanged family / anchor / frame / route /
  quantified-boundary contract, or bounded proof that this exact elaboration
  gap remains blocker debt under the unchanged architecture.
- Do not credit reification / reconstruction, internal output, public output,
  or reviewer-visible stable persistence unless the exact frozen packet first
  survives the item-2 Phase 6 elaboration breakpoint on the same route.
- Do not widen into the non-local alias-bound / base-like family,
  neighboring consumer routes, nested-`forall` or quantified-crossing
  success, general automatic recursive inference, or a reopened
  `non-cyclic-graph` revision question.
- Do not silently change the frozen tuple. Any family swap, anchor swap,
  owner / binder rewrite, route hop, quantified crossing, witness-only rescue,
  packet-history-only rescue, or fallback-like recovery is a blocker for this
  pocket rather than permission to widen scope.
- Do not edit `orchestrator/rounds/round-091/state-snapshot.json`, `orchestrator/roadmaps/2026-03-25-01-same-lane-retained-child-stable-visible-persistence-successor-orchestrator-roadmap/rev-003/roadmap.md`, `Bugs.md`,
  or any plan/review/merge/task-packet artifacts during this selection stage.

## Blockers

No live controller blocker or retry obligation is present.

Active bounded blockers that must remain blockers rather than widened work:

- the exact frozen packet currently fails in `Phase 6 (elaboration)` with
  `PhiTranslatabilityError ["reifyInst: missing authoritative instantiation translation for edge 3","expansion args=[NodeId {getNodeId = 31}]]`;
- later ledger rows remain `not credited after earlier breakpoint`, so the
  helper-visible `containsMu True` fact cannot be reused as proof of stable
  persistence until the exact elaboration break is cleared;
- nested-`forall` / quantified-crossing pressure remains reject-side only and
  is outside the clear-boundary frozen pocket;
- `non-cyclic-graph = unknown` remains architecture-pressure context only,
  not authority to reopen revision during item `3`; and
- open `BUG-2026-03-16-001` remains predecessor replay context only, not
  authority to divert away from this exact Phase 6 elaboration gate.
