# Round 082 Selection

Date: 2026-03-25
Round: `round-082`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted `N14 = continue-bounded`
Successor lane: roadmap item 1 only, defining the repo-level capability
contract and evaluation corpus for general automatic iso-recursive inference

## Selected Roadmap Item

Roadmap item 1: define the repo-level capability contract and evaluation
corpus for general automatic iso-recursive inference.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/orchestrator/state.json`
fixes the live controller state at `active_round_id: "round-082"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-082-item-1-capability-contract"`, `active_round_dir:
"orchestrator/rounds/round-082"`, and `last_completed_round: "round-081"`.
The same state file also records a prior non-observable guider launch under
`resume_error`, but the recorded recovery action is
`"relaunch_fresh_guider_same_stage"` and the machine state remains
`select-task` with `retry: null`. This is therefore still an ordinary
same-round selection recovery, not a live retry that overrides roadmap order.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/orchestrator/retry-subloop.md`
only forces same-round retry behavior when `retry` is populated, so the
normal lowest-numbered unfinished-item rule still governs this selection.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/orchestrator/roadmap.md`
marks items 1 through 7 pending, with item 1 dependency-free and every later
item depending on earlier unfinished work. Item 2 depends on item 1; item 3
depends on items 1 and 2; item 4 depends on items 1, 2, and 3; item 5
depends on items 1, 3, and 4; item 6 depends on items 2, 4, and 5; and item
7 depends on items 2 and 6. Item 1 is therefore the lowest-numbered
unfinished roadmap item and also the only lawful immediate successor under
the current dependency graph.

Accepted `round-081` finalized `N14` with `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, and `final_outcome: "continue-bounded"` in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/orchestrator/rounds/round-081/review-record.json`.
That accepted `N14` result preserved one exact same-lane retained-child
`boundVarTarget -> targetC` packet as bounded predecessor evidence only and
explicitly stated that no new successor lane was yet authorized or bound.
The refreshed strategic roadmap and live roadmap are the separate lawful
follow-up that now decide what should happen next. They also explicitly say
that early rounds in this refreshed control plane are expected to be
docs-first and must not silently widen into implementation, architectural
revision, or another packet-first cycle.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
sets the immediate strategic need: stop treating bounded packet progress as
if it already proved general capability, define the real target precisely
enough to test, and build the corpus that can distinguish broad capability
from another narrow success. Its milestone structure makes target clarity and
capability-corpus definition the first gate, and its honest-current-position
section states that the repo still has bounded verified packets rather than
general automatic inference. Item 1 in the live roadmap is the concrete,
bounded repo-local translation of that gate: one docs-first artifact must
define what "general automatic iso-recursive inference" means here, separate
bounded evidence from general capability, define the required positive and
negative corpus families, and state explicit success and failure criteria for
soundness, thesis-faithfulness, termination, and explainability.

The inherited baseline and accepted `N14` continuity make item 1 necessary
before any broader audit or design work. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, non-cyclic graph structure, and no
silent fallback widening. The accepted `N14` decision artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
preserves exactly one bounded packet as continuity only and explicitly rejects
treating it as completion of the long-horizon goal. Item 1 must therefore run
before item 2 can audit architecture against a defined target, before item 3
can generalize packets into reusable mechanisms, and before any search,
reconstruction, or coverage argument could be coherent.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-082/Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor context only and does not create a retry obligation or authorize
a different live subject. Repository status in the active worktree shows only
the controller-owned `M orchestrator/state.json` drift. No repository-state
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item 1 only.
- Keep the round docs-first and bounded: produce one artifact that defines
  the repo-level capability contract and evaluation corpus for general
  automatic iso-recursive inference; do not edit production code, tests,
  public surfaces, executables, Cabal, controller state, or review/merge
  artifacts.
- Cite the inherited baseline contract and accepted `N14` continuity
  explicitly, and distinguish bounded packet evidence from any claim of
  general capability.
- Define the required positive and negative corpus families, including
  success cases, fail-closed ambiguity cases, unsoundness guards,
  binder/ownership-sensitive cases, polymorphism interactions,
  reconstruction-visible cases, and termination-pressure cases.
- State explicit success and failure criteria around soundness,
  thesis-faithfulness, termination, and explainability.
- Keep architecture audit, mechanism-map generalization, search/ambiguity
  design, reconstruction contract, coverage campaign, and architecture
  decision work out of scope until their later roadmap items.
- Do not treat accepted bounded predecessor packets as proof that general
  automatic recursive inference already exists.
- Do not silently authorize boundary revision, implementation work, fallback
  widening, equi-recursive reasoning, cyclic graph encoding, or broader
  solver experiments.

## Blockers

No live controller blocker or retry obligation is present.

Active boundaries that must remain blockers rather than widened work:

- accepted `N14` supplies bounded predecessor continuity only; it does not
  authorize another packet-first cycle or broader inference claims by itself;
- the target for "general automatic iso-recursive inference" is not yet
  defined precisely enough to support architecture audit, search design,
  reconstruction claims, or coverage claims until item 1 completes; and
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it.
