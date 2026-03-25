# Round 085 Selection

Date: 2026-03-25
Round: `round-085`
Role: guider
Active subject: refreshed strategic control plane for general automatic
iso-recursive inference after accepted roadmap item `3`
Successor lane: roadmap item `4` only, designing the bounded search,
ambiguity, and termination model for the inherited acyclic architecture

## Roadmap Provenance

- Roadmap ID: `2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap`
- Roadmap Revision: `rev-004`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004`
- State Snapshot: `orchestrator/rounds/round-085/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 4: design the search, ambiguity, and termination model for
general recursive inference.

## Why This Item Should Run Now

`orchestrator/rounds/round-085/state-snapshot.json`
fixes the live controller state at `active_round_id: "round-085"`, `stage:
"select-task"`, `current_task: null`, `retry: null`, `branch:
"codex/round-085-item-4-search-model"`, `active_round_dir:
"orchestrator/rounds/round-085"`, and `last_completed_round: "round-084"`.
`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/retry-subloop.md`
only overrides roadmap order when `retry` is populated. `retry` is currently
`null`, so this is a normal selection step and the lowest-numbered unfinished
item rule still governs.

`orchestrator/roadmaps/2026-03-25-00-general-automatic-iso-recursive-inference-strategic-orchestrator-roadmap/rev-004/roadmap.md`
marks items `1` through `3` done and items `4` through `7` pending. Item `4`
depends on items `1`, `2`, and `3`, all of which are already accepted and
recorded as complete. Every later pending item depends on item `4` directly
or indirectly. Item `4` is therefore the next lawful dependency-satisfied
successor.

Accepted `round-084` finalized item `3` in
`orchestrator/rounds/round-084/review-record.json`
with `attempt_verdict: "accepted"`, `stage_action: "finalize"`, and
`final_outcome:
"mechanism-map-established-with-bounded-p2-p5-pressure-read"`. The canonical
artifact
`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`
explicitly leaves three named debts for successor work: generalized search /
admissibility rules, a full reconstruction contract, and broader coverage.
Of those, item `4` owns the first debt exactly. Running item `5`, `6`, or `7`
before item `4` would skip the still-missing search discipline that the
accepted mechanism map says is required before reconstruction proof, coverage,
or an architecture decision can be justified.

`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
states that broad recursive inference is not credible until the repo can say
when recursive inference is considered, when it is forbidden, how competing
candidates are compared, when ambiguity must fail closed, and why search
terminates. The live roadmap translates that strategic milestone into item
`4`, and the accepted item-3 mechanism map already points at the exact
candidate-generation debt still carried by packet-specific guards
(`rootNonLocalSchemeAliasBaseLike`,
`sameLaneLocalRetainedChildTarget`, `boundHasForallFrom`, and
`not hasForall`).

The inherited boundary and predecessor continuity still make this a bounded
docs-first step rather than an implementation round.
`docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
still binds the repo to explicit-only recursive behavior, iso-recursive
meaning, non-equi-recursive semantics, structurally acyclic encoding, and no
fallback widening. The accepted `N14` decision artifact at
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
still preserves the earlier exact packet as bounded predecessor evidence only,
not as proof of general capability and not as authority to widen the
architecture. Item `4` must therefore generalize bounded packet evidence into
one search / admissibility / termination model inside the inherited boundary,
or record unresolved blockers, without silently widening semantics,
representation, interfaces, or fallback behavior.

`Bugs.md` still
lists open `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains
predecessor implementation context only and does not create a retry
obligation or change roadmap order. Repository status in the active worktree
shows only controller-owned `M orchestrator/rounds/round-085/state-snapshot.json` drift. No repository
state blocker forces a different selection.

## Round Scope Guard

- This round is limited to roadmap item `4` only.
- Keep the round docs-only and bounded: design one search / admissibility /
  ambiguity / termination model for the inherited acyclic architecture rather
  than proposing code changes or selecting another exact packet.
- Start from the accepted item-3 mechanism map and define how anchor-first
  recursive-shape discovery produces candidate targets before downstream
  consumer choice.
- State how owner / binder placement and target / consumer alignment constrain
  candidate admission across local versus non-local propagation.
- Define which situations must fail closed, including nested-`forall`,
  owner-crossing, neighboring-route, and competing-candidate cases where the
  accepted record does not justify heuristic guessing.
- Explain why the proposed search terminates without cyclic graphs,
  multi-SCC search, a second executable interface, or fallback behavior.
- Address the named item-3 packet-guard debt
  (`rootNonLocalSchemeAliasBaseLike`,
  `sameLaneLocalRetainedChildTarget`, `boundHasForallFrom`, and
  `not hasForall`) by either lifting it into general admissibility rules or
  recording it as an unresolved blocker.
- Cite the inherited baseline contract, the accepted item-3 mechanism map,
  the human strategic roadmap, and the accepted `N14` predecessor decision
  explicitly.
- Do not edit production code, tests, public surfaces, executables, Cabal,
  controller state, roadmap state, retry rules, or review/merge artifacts.
- Do not define the full reconstruction contract, run a representative
  coverage campaign, authorize architecture revision, or treat bounded
  predecessor packets as if they already prove general automatic
  iso-recursive inference.

## Blockers

No live controller blocker or retry obligation is present.

Active blockers that must remain blockers rather than widened work:

- the accepted item-3 mechanism map still leaves packet-specific search debt
  unresolved, so this round must define or fail-closed that search discipline
  rather than jumping ahead to reconstruction, coverage, or architecture
  decisions;
- positive `P5` recursive-polymorphism evidence is still absent, so this round
  may define admissibility boundaries around nested-`forall` pressure but may
  not upgrade negative-only evidence into a positive polymorphism success; and
- the inherited explicit-only / iso-recursive / non-equi-recursive /
  non-cyclic-graph / no-fallback boundary remains binding unless a later
  accepted roadmap item explicitly changes it.
