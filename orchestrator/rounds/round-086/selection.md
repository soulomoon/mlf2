# Round 086 Selection

Date: 2026-03-25
Round: `round-086`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `4`
Successor lane: roadmap item `5` only, defining the bounded full-pipeline
reconstruction and validation contract

## Selected Roadmap Item

Roadmap item 5: define the full-pipeline reconstruction and validation
contract.

## Why This Item Should Run Now

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/orchestrator/state.json`
fixes the live controller state at `active_round_id: "round-086"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-086-item-5-reconstruction-contract"`, `active_round_dir:
"orchestrator/rounds/round-086"`, and `last_completed_round: "round-085"`.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/orchestrator/retry-subloop.md`
only overrides roadmap order when a retry object is present. `retry` is
currently `null`, so the normal lowest-numbered unfinished item rule still
governs this selection.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/orchestrator/roadmap.md`
marks items `1` through `4` done and items `5` through `7` pending. Item `5`
depends on items `1`, `3`, and `4`, all of which are already accepted and
recorded as complete. Item `6` depends on item `5`, and item `7` depends on
item `6`, so no later pending item is yet dependency-satisfied. Item `5` is
therefore the next lawful successor.

Accepted `round-085` finalized item `4` in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/orchestrator/rounds/round-085/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"search-model-established-with-bounded-admissibility-ambiguity-and-termination-read"`.
The canonical artifact
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-search-model.md`
explicitly leaves `full-pipeline P6 reconstruction-visible-output proof`
as item `5` work. Item `4` established when recursive candidates may be
admitted; item `5` now owns the next missing debt of proving that an admitted
candidate survives solver state, elaboration, reification / reconstruction,
and internal/public output surfaces without hidden fallback or manual
reinterpretation.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
states that the next strategic gate after the search model is to prove
full-pipeline reconstruction. The live roadmap translates that milestone into
item `5`: define one bounded docs-only reconstruction and validation contract
before any representative coverage campaign or architecture decision. Running
item `6` or `7` first would skip the still-missing contract that says what
counts as stable recursive evidence across the pipeline and what must fail
closed when that evidence disappears, changes family, crosses quantified
structure, or survives only through fallback-like interpretation.

The inherited boundary remains fixed, so this is still a bounded docs-first
selection rather than an implementation round.
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. The accepted `N14` decision artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves predecessor packets as bounded evidence only, not as proof of
general capability and not as authority to widen architecture. Item `5` must
therefore define one evidence-preservation contract inside the inherited
boundary, or record blocker debt, without silently widening semantics,
representation, interfaces, or fallback behavior.

`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-086/Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry
obligation or change roadmap order. Repository status in the active worktree
shows only controller-owned `M orchestrator/state.json` drift. No live
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `5` only.
- Keep the round docs-only and bounded: define one full-pipeline
  reconstruction and validation contract for admitted recursive candidates
  inside the inherited architecture.
- Start from the accepted item-3 mechanism map and accepted item-4 search
  model, and state the concrete evidence trail that must persist across solver
  state, elaboration, reification / reconstruction, and internal/public output
  surfaces.
- Define fail-closed conditions for recursion that disappears, changes
  mechanism family, crosses quantified boundaries, shifts owner / binder
  interpretation, or only survives through manual interpretation or
  fallback-like reasoning.
- Explain how validation should distinguish lawful persisted recursion from
  packet-specific reading or silent output drift.
- Cite the inherited baseline contract, the accepted item-3 mechanism map,
  the accepted item-4 search model, the human strategic roadmap, and the
  accepted `N14` predecessor decision explicitly.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, verification rules, bug
  tracker, or review/merge artifacts.
- Do not run the representative coverage campaign, make the item-7
  architecture decision, widen the admitted search families, or treat bounded
  predecessor packets as if they already prove general automatic
  iso-recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active blockers that must remain blockers rather than widened work:

- full-pipeline reconstruction visibility is still undefined, so this round
  must write that contract before any coverage claim can be credible;
- positive `P5` nested-`forall` success remains unresolved blocker debt, so
  item `5` may define fail-closed validation around quantified crossings but
  may not upgrade negative-only evidence into positive polymorphism success;
  and
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it.
