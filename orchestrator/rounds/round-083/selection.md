# Round 083 Selection

Date: 2026-03-25
Round: `round-083`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `1`
Successor lane: roadmap item `2` only, auditing the inherited architectural
constraints against the accepted capability contract

## Selected Roadmap Item

Roadmap item 2: audit the current architectural constraints against the
capability contract.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/orchestrator/state.json`
fixes the live controller state at `active_round_id: "round-083"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-083-item-2-constraint-audit"`, `active_round_dir:
"orchestrator/rounds/round-083"`, and `last_completed_round: "round-082"`.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/orchestrator/retry-subloop.md`
only overrides roadmap order when `retry` is populated. `retry` is currently
`null`, so this is an ordinary selection step and the normal lowest-numbered
unfinished-item rule still governs.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/orchestrator/roadmap.md`
marks item `1` done and items `2` through `7` pending. Item `2` depends only
on item `1`, which was accepted in `round-082`; every later item depends on
item `2` directly or indirectly. Item `2` is therefore the lowest-numbered
unfinished roadmap item and the next lawful dependency-satisfied successor.

Accepted `round-082` finalized item `1` in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/orchestrator/rounds/round-082/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"repo-level-capability-contract-and-evaluation-corpus-defined"`. The
canonical artifact
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
now defines the repo-level target and the minimum family matrix
(`P1`-`P6`, `N1`-`N6`), but it explicitly does not classify whether the
inherited architecture can satisfy that contract. Item `2` is the first round
that can answer that question in a bounded way.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
identifies the architectural audit as the next strategic gate: re-evaluate
`iso-recursive`, `non-equi-recursive`, `non-cyclic-graph`, and `no-fallback`
as `keep`, `revise`, or `unknown`, and decide whether general inference still
looks plausible inside the current architecture. That is exactly what the live
roadmap translates into item `2`. Running item `3` or later before this audit
would skip the explicit plausibility gate and risk drifting from docs-first
strategic work into unjustified mechanism or search design.

The inherited baseline and predecessor continuity make this audit necessary.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, non-cyclic structural encoding, and no
silent fallback widening. The accepted `N14` decision artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves one exact same-lane retained-child `boundVarTarget -> targetC`
packet as bounded predecessor evidence only, not as proof of general
capability and not as authority to widen architecture. Item `2` must therefore
test the current constraints against the newly defined capability contract
without silently revising semantics, representation, interfaces, or fallback
behavior.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-083/Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry obligation
or change roadmap order. Repository status in the active worktree shows only
controller-owned `M orchestrator/state.json` drift. No repository-state
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `2` only.
- Keep the round docs-only and bounded: classify the inherited
  `iso-recursive`, `non-equi-recursive`, `non-cyclic-graph`, and
  `no-fallback` constraints as `keep`, `revise`, or `unknown` against the
  accepted item-1 capability contract and the `P1`-`P6` / `N1`-`N6` family
  matrix.
- State which positive or negative families each constraint appears to
  support, block, or leave unresolved, and whether general automatic
  iso-recursive inference still appears plausible inside the current
  architecture without silently revising semantics, representation,
  interfaces, or fallback behavior.
- Cite the inherited baseline contract, the accepted item-1 capability
  contract, and the accepted `N14` predecessor decision explicitly.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, or review/merge artifacts.
- Do not generalize packets into a mechanism map, design search/ambiguity or
  reconstruction policy, run coverage campaigns, or authorize architecture
  revision beyond the audit classifications themselves.
- Do not treat bounded predecessor packets as if they already prove general
  automatic recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active blockers that must remain blockers rather than widened work:

- accepted item `1` defines the target and corpus, but it does not yet prove
  that the inherited architecture fits that target;
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it; and
- open `BUG-2026-03-16-001` remains predecessor implementation context only,
  not authority to divert this round away from the strategic architectural
  audit.
