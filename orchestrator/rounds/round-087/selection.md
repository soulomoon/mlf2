# Round 087 Selection

Date: 2026-03-25
Round: `round-087`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `5`
Successor lane: roadmap item `6` only, running the bounded representative
coverage and feasibility campaign

## Roadmap Provenance

- Roadmap ID: `2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap`
- Roadmap Revision: `rev-006`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-006`
- State Snapshot: `orchestrator/rounds/round-087/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 6: run a representative coverage and feasibility campaign.

## Why This Item Should Run Now

`orchestrator/rounds/round-087/state-snapshot.json`
fixes the live controller state at `active_round_id: "round-087"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-087-item-6-coverage-campaign"`, `active_round_dir:
"orchestrator/rounds/round-087"`, and `last_completed_round: "round-086"`.
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-006/retry-subloop.md`
only overrides roadmap order when `retry` is populated. `retry` is currently
`null`, so the normal lowest-numbered unfinished item rule still governs.

`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-006/roadmap.md`
marks items `1` through `5` done and items `6` and `7` pending. Item `6`
depends on items `2`, `4`, and `5`, all of which are already accepted and
recorded as complete. Item `7` depends on items `2` and `6`, so it is not
yet dependency-satisfied. Item `6` is therefore the next lawful successor.

Accepted `round-086` finalized item `5` in
`orchestrator/rounds/round-086/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"full-pipeline-reconstruction-and-validation-contract-defined"`. The
canonical artifact
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
explicitly leaves representative coverage across the broader family matrix as
item-`6` work. It also fixes the concrete classification vocabulary that item
`6` must now apply: `stable visible persistence`, `admitted but not
reconstruction-visible / blocker debt`, and `fail-closed rejection`.

That makes item `6` concrete rather than open-ended. The campaign must use
the accepted item-5 persistence tuple, phase-and-surface ledger, and outcome
vocabulary to classify representative capability-corpus families across:

- both admitted item-4 families;
- nested-`forall` reject-side pressure;
- ambiguity-reject pressure;
- termination-pressure cases;
- binder-sensitive cases; and
- reconstruction-heavy cases.

The output must then say whether the accumulated evidence supports broad
generality, a bounded subset only, or an architectural dead end. That is the
missing evidence gate before item `7`; it is not item `7` itself.

`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
states that the broad coverage campaign comes after full-pipeline
reconstruction and before the architecture fork decision. Running item `7`
first would skip the still-missing representative classification step that is
supposed to inform, but not pre-empt, the architecture choice.

The inherited boundary remains fixed, so this is still a bounded
evidence-classification round rather than a widening round.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. The accepted `N14` decision artifact at
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves predecessor packets as bounded evidence only, not as proof of
general capability and not as authority to widen the architecture. Item `6`
must therefore classify representative pressure honestly inside the inherited
boundary, without silently authorizing implementation, boundary revision, or
the item-7 architecture choice.

`Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry
obligation or change roadmap order. Repository status in the active worktree
shows only controller-owned `M orchestrator/rounds/round-087/state-snapshot.json` drift. No live
blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `6` only.
- Use the accepted item-5 persistence tuple, phase-and-surface ledger, and
  lawful outcome vocabulary to classify representative capability-corpus
  families.
- Cover both admitted item-4 families plus representative nested-`forall`
  reject-side, ambiguity-reject, termination-pressure, binder-sensitive, and
  reconstruction-heavy pressure.
- State which cases achieve `stable visible persistence`, which remain
  `admitted but not reconstruction-visible / blocker debt`, and which stay
  `fail-closed rejection`.
- End with the bounded feasibility read required by item `6`: whether the
  accumulated evidence supports broad generality, a bounded subset only, or
  an architectural dead end.
- Cite the inherited baseline contract, the strategic roadmap, the accepted
  item-5 reconstruction contract, and the accepted `N14` predecessor decision
  explicitly.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, bug tracker, or
  review/merge artifacts.
- Do not make the item-7 architecture decision, revise the inherited
  boundary, widen the admitted family vocabulary, or treat bounded predecessor
  packets as if they already prove general automatic iso-recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active blockers that must remain blockers rather than widened work:

- the non-local alias-bound / base-like family still carries accepted
  blocker debt because its visible output read remains non-recursive
  (`TBase (BaseTy "Int")` / `containsMu False`), so this round may classify
  that debt but may not silently promote it to positive recursive success;
- the same-lane retained-child family remains the strongest bounded positive
  candidate, but it still needs representative classification under the item-5
  contract before any broader feasibility claim is credible; and
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it.
