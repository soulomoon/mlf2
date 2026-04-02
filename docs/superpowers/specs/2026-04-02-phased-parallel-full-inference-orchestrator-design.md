# Phased-Parallel Full-Inference Orchestrator Design

Date: 2026-04-02

## Goal

Retune the live repo-local `orchestrator/` control plane so research toward
full automatic iso-recursive type inference can run through multiple parallel
agent teams where the work is naturally separable, without turning the control
plane into a merge-conflict factory or weakening the existing evidence
standards.

This is a control-plane redesign only. It should update the active roadmap
family and the shared orchestrator contract, then stop. It must not start
runtime rounds.

## Current State

The newly scaffolded live family
`2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
is still effectively serial:

- every roadmap item is marked `Parallel safe: no`;
- `orchestrator/state.json` keeps `max_parallel_rounds: 1`;
- the guider role says "Do not run parallel rounds";
- the planner role says to keep round plans serial unless the roadmap
  explicitly authorizes more.

That is too conservative for the actual research surface.

The active roadmap already separates the work into distinct semantic areas:

- a family freeze and success-bar item;
- a semantic mechanism map;
- a search / ambiguity / termination model;
- a reconstruction-visible readiness contract;
- a positive-family implementation campaign;
- a negative-family / termination-pressure campaign; and
- a final readiness or architecture decision.

Those areas are not equally parallelizable.

The docs-first middle of the roadmap naturally decomposes into three largely
independent strands after one serial freeze:

1. how recursive evidence propagates and where recursive structure belongs;
2. how recursive candidates are introduced, compared, and rejected; and
3. how recursive success becomes reviewable reconstructed output.

The later implementation/evidence campaigns are only partially parallelizable.
They should use planner-authored worker fan-out with explicit ownership, not
free-form concurrent rounds touching the same surfaces.

## User-Approved Constraints

- Optimize for real throughput, not for the appearance of maximal parallelism.
- Preserve the inherited semantic boundary:
  explicit-only production baseline,
  iso-recursive meaning,
  non-equi-recursive,
  non-cyclic-graph unless later explicitly revised,
  and no fallback or second interface.
- Keep predecessor roadmap families and accepted round history immutable.
- Do not start runtime rounds as part of this redesign.

## Decision

Adopt phased parallelism.

### Phase 1: Serial Freeze

Keep `item-1` serial.

Reason:
before any fan-out, the family needs one stable authority chain, one frozen
semantic matrix, one success bar, and one explicit statement of what later
parallel teams are allowed to touch.

### Phase 2: Parallel Research Wave

After `item-1`, run three parallel docs-first rounds:

- `item-2`: semantic mechanism map
- `item-3`: candidate generation / ambiguity / boundedness model
- `item-4`: reconstruction-visible readiness contract

These rounds may co-run because they can each write their own artifact and
reason about different semantic questions while sharing the same frozen
baseline from `item-1`.

### Phase 3: Integrated Positive-Family Campaign

Keep `item-5` as one roadmap item, but explicitly allow planner-authored
worker fan-out inside that round.

Reason:
the positive-family campaign needs one integrated result, but the actual work
can be split by bounded ownership slices once items `2` through `4` have made
the contracts stable enough.

The preferred worker partition is by semantic aspect, not by arbitrary file
ownership:

- propagation / placement slice
- polymorphism / nested-`forall` slice
- reconstruction-visible slice

The planner may choose a smaller partition if the repo evidence does not
justify all three workers.

### Phase 4: Integrated Negative-Family Campaign

Keep `item-6` as one roadmap item, also with optional planner-authored worker
fan-out.

This round should validate:

- ambiguity rejection (`N1`)
- soundness guards (`N2`)
- bounded termination pressure (`N6`)

Again, one integrated decision matters more than independent worker outputs,
so the roadmap item stays singular while the plan may fan out internally.

### Phase 5: Serial Decision

Keep `item-7` serial.

The final readiness or architecture decision must integrate the total evidence
surface and therefore should not be parallelized.

## Why Not Maximum Fan-Out

Do not parallelize everything.

That would be the wrong tradeoff here for three reasons:

1. the semantic contracts are still moving, so early code fan-out would create
   merge churn and contradictory local assumptions;
2. the repo still carries caution around shared build/test state, so parallel
   coding must be explicit and ownership-driven rather than casual; and
3. the final outputs that matter are integrated evidence reads, not isolated
   worker wins.

The best throughput shape is therefore:

- serial freeze;
- parallel semantic research;
- integrated bounded implementation/evidence campaigns with worker fan-out;
- serial closeout decision.

## Roadmap Revision

The current active family may be revised in place because its `rev-001` has
not yet been used by any runtime round.

The roadmap should be updated to this metadata shape:

### Item 1

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: none`

No semantic change to the item beyond explicitly freezing the later
parallelization rules and worker-plan requirement.

### Item 2

- `Parallel safe: yes`
- `Parallel group: research-wave-a`
- `Merge after: item-1`

This item owns one dedicated semantic-mechanism artifact only.

### Item 3

- `Parallel safe: yes`
- `Parallel group: research-wave-a`
- `Merge after: item-1`

This item owns one dedicated search / ambiguity / termination artifact only.

### Item 4

- `Parallel safe: yes`
- `Parallel group: research-wave-a`
- `Merge after: item-1`

This item owns one dedicated reconstruction / readiness artifact only.

### Item 5

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-2, item-3, item-4`

This item remains one integrated positive-family campaign item, but its
completion notes should explicitly authorize planner-authored worker fan-out
with disjoint ownership boundaries and one integrated review.

### Item 6

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-5`

This item remains one integrated negative-family / boundedness campaign item
with optional worker fan-out.

### Item 7

- `Parallel safe: no`
- `Parallel group: none`
- `Merge after: item-6`

No change beyond making the end-state integration explicit.

## Shared Contract Changes

The shared orchestrator contract should be refreshed because the user
explicitly wants agent-team parallelism, and the current role wording is
incompatible with that goal.

### `orchestrator/state.json`

Change:

- `max_parallel_rounds: 3`

Do not change:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- base branch
- active round state

This remains a setup-only redesign.

### `orchestrator/roles/guider.md`

Revise the guider from single-round selection to bounded multi-round dispatch.

The guider should:

- choose all ready items whose dependencies are satisfied and whose
  `Parallel safe` / `Parallel group` metadata allows co-scheduling;
- respect `max_parallel_rounds`;
- prefer the lowest-numbered ready items;
- avoid dispatching two rounds that would edit the same shared control-plane or
  product surfaces unless the roadmap item explicitly makes that safe; and
- continue to fail closed when retry state forces a same-round retry.

The guider should still refuse uncontrolled parallelism.

### `orchestrator/roles/planner.md`

Revise the planner so it may author `worker-plan.json` when the selected item
explicitly permits integrated worker fan-out.

The planner should:

- define worker ownership in terms of exact files or exact function/module
  slices;
- keep workers on disjoint write sets;
- specify focused verification per worker;
- require one integrated verification pass before review approval; and
- avoid assuming shared `dist-newstyle` safety.

### `orchestrator/roles/reviewer.md`

Revise the reviewer so parallel and worker-fanout rounds are judged on the
integrated result rather than on isolated worker slices.

The reviewer should:

- confirm `worker-plan.json` exists when required;
- verify worker ownership boundaries were respected;
- verify integrated diffs match the selected item;
- run the authoritative baseline and task-specific gates on the integrated
  result; and
- reject if two workers silently overlapped or if integration widened scope.

### `orchestrator/roles/implementer.md`

Revise the implementer wording to acknowledge bounded worker ownership when a
fan-out plan exists, while still forbidding scope creep and self-approval.

### `orchestrator/roles/merger.md`

Revise the merger so it preserves whether an item was executed as:

- one serial round;
- parallel item-level rounds; or
- one integrated fan-out round.

The merge notes should still summarize one item-level result honestly.

### `orchestrator/roles/recovery-investigator.md`

Add one parallel-specific recovery obligation:
diagnose whether failure came from true semantic blockers, bad worker
partitioning, or merge/integration mistakes between parallel slices.

## Verification Contract Changes

The active `verification.md` should keep the existing baseline gates and add
parallel-specific checks that are currently only implicit.

It should explicitly require reviewers to confirm:

- item-level parallel rounds touched disjoint artifact files;
- any worker-fanout round includes `worker-plan.json`;
- worker ownership boundaries were respected;
- focused worker checks ran where planned;
- one authoritative integrated verification pass ran before approval; and
- approval is based on the integrated round result, not on isolated worker
  outputs.

For coding waves, the contract should prefer:

- focused checks in worker branches/worktrees; and
- one integrated `cabal build all && cabal test` gate in the integrated round
  before approval.

That keeps parallelism useful without multiplying redundant full-suite runs.

## Retry Contract Changes

The active `retry-subloop.md` should add explicit retry reasons for:

- illegal co-scheduling of overlapping items;
- missing or invalid `worker-plan.json`;
- worker ownership overlap;
- integration drift from the selected roadmap item; and
- approval attempts based on partial worker evidence rather than the
  integrated result.

## Files To Update

The implementation phase after this spec should update:

- `orchestrator/state.json`
- the active roadmap bundle
  `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`
- the matching active
  `verification.md`
- the matching active
  `retry-subloop.md`
- `orchestrator/roles/guider.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roles/reviewer.md`
- `orchestrator/roles/implementer.md`
- `orchestrator/roles/merger.md`
- `orchestrator/roles/recovery-investigator.md`

The top-level pointer stubs should not need semantic changes beyond continuing
to match `state.json`.

## Non-Goals

- Do not widen the semantic boundary.
- Do not change production code for recursive inference in this redesign.
- Do not reopen completed predecessor roadmap families.
- Do not start runtime rounds.
- Do not claim repo-level readiness for full automatic iso-recursive
  inference as part of the control-plane refresh.

## Expected Outcome

After this redesign lands, the control plane will be able to do the useful
kind of parallelism for this problem:

- one shared freeze item;
- three concurrent semantic research teams;
- controlled integrated worker fan-out for the bounded code/evidence phases;
- one final integrated decision.

That is the highest-throughput shape that still keeps the evidence honest.
