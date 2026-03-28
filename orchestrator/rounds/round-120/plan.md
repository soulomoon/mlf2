# Round 120 Plan (`item-1` Bounded `C1` / `P2` Successor Freeze)

## Objective

Execute roadmap item `1` only:
freeze the bounded `C1` / `P2` successor authority, exact packet, success
bar, and writable slice.

This round is `attempt-1` with `retry: null`. The plan stays inside one
aggregate docs-only slice only:
publish one canonical freeze artifact for the exact admitted non-local `C1`
packet and the bounded current-architecture successor lane opened by accepted
`round-119`.

## Binding Boundary Carried Forward

The following accepted truth remains fixed and must be carried forward without
widening:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the live production baseline:
  explicit recursive annotations only, iso-recursive meaning only,
  `non-equi-recursive = keep`, inherited non-cyclic structure,
  `no-fallback = keep`, and one-interface-only.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  still defines `P2 non-local-propagation` as a required positive family.
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
  still defines the review-visible output bar for authoritative success.
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
  is the accepted refreshed matrix that keeps `C1` as bounded non-success on
  current authoritative surfaces.
- `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`
  is the direct authority that opens exactly one live successor lane:
  bounded `C1` / `P2` only.
- The settled same-lane `C2` / `C5` / `C7` pocket remains closed predecessor
  truth only. This round must not reopen it.
- `P5` remains reject-side context only. This round must not promote `P5`
  into a second live lane.
- No equi-recursive reasoning, cyclic structural graphs, multi-SCC search,
  second interfaces, fallback widening, production hardening, rollout, or
  repo-level readiness claim is authorized here.

## Authoritative Output Set

The implementer may write only the following round-owned outputs:

1. `docs/plans/2026-03-28-c1-p2-authoritative-surface-successor-authority-success-bar-and-writable-slice-freeze.md`
   - owner: aggregate synthesis only;
   - role: the only authoritative item-1 artifact.
2. `orchestrator/rounds/round-120/implementation-notes.md`
   - owner: aggregate synthesis only;
   - role: non-authoritative companion notes and verification log for the same
     item-1 artifact.

Single-writer rule:

- planner owns `orchestrator/rounds/round-120/plan.md` only;
- reviewer remains the only lawful writer for `review.md`,
  `reviews/attempt-<n>.md`, and `review-record.json`;
- this round does not authorize lane-local scratch files, worker plans, or
  any other canonical writable surface.

## Parallel-Lane Decision

This round is `aggregate-only` and `not lane-parallelizable`.

No parallel split is authorized. The round owns one authority freeze only, and
the artifact must present one coherent predecessor chain, one exact success
bar, and one exact writable slice. Parallel sidecars would add coordination
cost without creating independent evidence.

## Sequential Task List

### Task 1: Freeze the direct predecessor authority chain

- State explicitly that item `1` consumes:
  the March 14 baseline contract,
  the March 25 capability contract,
  the March 25 full-pipeline validation contract,
  the accepted March 27 refreshed matrix,
  and the accepted March 27 narrowed successor gate.
- State explicitly that the same-lane `C2` / `C5` / `C7` pocket remains
  settled predecessor truth only and is not reopened here.
- State explicitly that `P5` remains out of scope for this family unless a
  later accepted gate reopens it.

### Task 2: Freeze the exact live subject and current read

- Bind the live subject to the exact admitted non-local `C1` packet only.
- Name the exact route as `baseTarget -> baseC -> targetC`.
- Carry forward the accepted current read that:
  - the fallback surface remains `TBase (BaseTy "Int")` with `containsMu False`;
  - both authoritative public pipeline entrypoints currently return the
    non-`TMu` `forall`-identity shape; and
  - that current read is still blocker debt rather than success.
- Name the exact harness and predecessor evidence surfaces that currently
  expose this read, including the accepted March 27 matrix and the focused
  `test/Research/C1AuthoritativeSurfaceSpec.hs` harness.

### Task 3: Freeze the exact success bar for the next family round

- State that item `2` may claim exact-packet success only if the admitted
  `C1` packet preserves recursive structure on the current authoritative
  surfaces, not merely on helper-visible or fallback-only reads.
- State explicitly whether supporting fallback/internal evidence may be cited
  and how it relates to the authoritative public read.
- State explicitly that one exact-packet improvement is not yet the same as
  general `P2` family settlement or repo-level readiness.

### Task 4: Freeze the bounded writable slice and blocked changes

- Name the permitted implementation slice for item `2` precisely:
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  `test/PipelineSpec.hs`,
  `test/Main.hs`,
  `mlf2.cabal`,
  the round-owned artifacts, and the new canonical docs freeze.
- State explicitly that item `2` may narrow further than this slice, but may
  not widen beyond it without a later accepted change.
- State explicitly that cyclic search, multi-SCC search, second interfaces,
  fallback widening, unrelated family work, and reopening `P5` or the
  same-lane pocket remain blocked.

### Task 5: Make item `2` the next lawful move

- End the canonical freeze by stating that item `2` is the next lawful move.
- State that item `2` is the first code-bearing round in this family and must
  preserve the frozen exact subject, success bar, and writable slice.

## Command Plan

- read-only authority checks:
  `sed -n` and `rg -n` over the March 14, March 25, and March 27 accepted
  artifacts plus `test/Research/C1AuthoritativeSurfaceSpec.hs`
- docs-output verification:
  `git diff --check`
  `python3 -m json.tool orchestrator/state.json >/dev/null`
  `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
- full Cabal gate:
  skip for this round unless the diff escapes docs/orchestrator-only scope.

## Exit Criteria

This round is complete only when:

- the canonical freeze artifact exists and states the predecessor authority
  chain, exact `C1` packet, exact current read, exact success bar, and exact
  writable slice;
- the artifact keeps the same-lane `C2` / `C5` / `C7` pocket closed and keeps
  `P5` out of scope;
- `implementation-notes.md` records the concise docs-only change summary and
  verification log; and
- item `2` is left as the next lawful move with no silent widening.
