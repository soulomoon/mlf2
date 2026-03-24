# General Automatic Iso-Recursive Inference Strategic Roadmap

Date: 2026-03-25
Status: human-facing strategic roadmap
Scope: long-horizon roadmap beyond accepted `N14`

## Purpose

This document defines the strategic program required to move from the current
state of bounded accepted packets toward a credible claim of general automatic
iso-recursive inference.

It is intentionally not a repo-local round plan. It is a human roadmap for
deciding whether the goal is reachable, what evidence is still missing, and
when the current architecture must be revised instead of stretched further.

## Current State

The accepted record through `round-081` proves a narrower result than the full
goal:

- the repo can freeze, implement, reverify, and classify bounded recursive
  inference packets;
- the exact same-lane retained-child `boundVarTarget -> targetC` packet is now
  accepted bounded evidence; and
- the inherited baseline still stands: automatic recursive-type inference is
  unresolved and disabled outside explicit recursive annotations.

The current repo-local record therefore supports this statement:

> The project has credible bounded evidence, but it does not yet have general
> automatic iso-recursive inference.

## Strategic Goal

For this repo, the long-horizon target is:

> Synthesize recursive types for unannotated programs thesis-faithfully,
> within a solver/pipeline design that remains sound, reviewable, terminating,
> and explainable.

That target is stronger than "another bounded packet works." It requires a
general mechanism, a principled search story, broad coverage, and an explicit
architectural decision on which current constraints remain essential.

## Strategic Principles

1. Do not confuse packet progress with generality.
2. Prefer explicit architectural decisions over silent drift.
3. Keep thesis-faithfulness as a first-class acceptance criterion, not a
   post-hoc rationalization.
4. Treat soundness, termination, and reconstruction as equal partners to raw
   inference coverage.
5. If the current architecture cannot plausibly reach the goal, say so and
   revise it explicitly instead of continuing with unbounded local patchwork.

## Strategic Milestones

### Milestone 1 - Define the real capability target

State precisely what "general automatic iso-recursive inference" means in this
repo.

This definition should answer:

- which classes of unannotated programs must succeed;
- which cases must still fail closed;
- what inferred output must look like after elaboration/reconstruction;
- what guarantees are required around soundness, thesis-faithfulness,
  termination, and explainability; and
- whether "general" means a broad bounded subset or a default solver
  capability.

Exit gate:

- the team can state the target as a testable contract rather than an
  aspiration.

### Milestone 2 - Build the capability corpus

Create the positive and negative corpus that the eventual claim must satisfy.

The corpus should include:

- successful inference cases;
- must-fail ambiguity cases;
- must-fail unsoundness cases;
- cases that stress binder placement and ownership;
- cases that stress polymorphism interactions;
- cases that stress reconstruction and user-visible output; and
- cases that stress termination boundaries.

Exit gate:

- the project has a benchmark corpus that can distinguish bounded packet wins
  from genuine generality.

### Milestone 3 - Audit the current architectural constraints

Re-evaluate the current inherited boundaries:

- `iso-recursive`
- `non-equi-recursive`
- `non-cyclic-graph`
- `no-fallback`

For each constraint, classify it as one of:

- keep;
- revise; or
- unknown pending focused research.

Current strategic prior:

- `iso-recursive`: likely keep;
- `non-equi-recursive`: likely keep unless a concrete completeness failure
  proves otherwise;
- `non-cyclic-graph`: highest-risk constraint for generality;
- `no-fallback`: likely keep as a production principle, but it may need a more
  explicit research-mode search policy rather than pure packet-shaped
  fail-closed expansion.

Exit gate:

- a written architecture position states whether general inference appears
  reachable inside the current model or only after boundary revision.

### Milestone 4 - Generalize from packets to mechanisms

Stop organizing progress around the next convenient lane and instead identify
the reusable mechanism families required for broad inference.

Likely mechanism families:

- recursive-shape discovery;
- binder and owner placement;
- target selection and consumer alignment;
- local versus non-local propagation;
- interaction with polymorphism and instantiation;
- reconstruction of inferred recursion into elaborated output; and
- fail-closed handling of ambiguity and unsafe cases.

The strategic question is whether accepted packet history can be explained by a
small number of reusable mechanisms rather than by many unrelated local
exceptions.

Exit gate:

- at least one mechanism map explains multiple accepted packets and predicts
  new families.

### Milestone 5 - Design the search, ambiguity, and termination model

General recursive inference is not credible unless the project can explain:

- when recursive inference is considered;
- when recursive inference is forbidden;
- how multiple recursive candidates are compared;
- when ambiguity causes rejection instead of heuristic guessing; and
- why search still terminates.

This milestone is likely one of the hardest. Without it, broad inference turns
into ad hoc packet-specific behavior.

Exit gate:

- a principled search model exists and has explicit ambiguity and termination
  boundaries.

### Milestone 6 - Prove full-pipeline reconstruction

Recursive structure inferred inside one internal phase is not enough. The
project must show that inferred recursion survives:

- solver state;
- elaboration;
- reification or reconstruction;
- internal/public output surfaces; and
- reviewable evidence trails.

Exit gate:

- inferred recursive results are stable across the full pipeline and do not
  depend on silent fallback or packet-specific manual interpretation.

### Milestone 7 - Run a broad coverage campaign

Validate the mechanism and search model across representative families rather
than a single narrow chain.

Coverage should include:

- local and non-local cases;
- retained-child and alias-bound families;
- nested `forall` interactions;
- owner-sensitive and binder-sensitive cases;
- ambiguity cases that must fail;
- cases that threaten termination; and
- reconstruction-heavy cases.

Exit gate:

- success is broad enough that the word "general" becomes credible.

### Milestone 8 - Make the architecture fork decision

After the coverage campaign, choose one of three paths:

1. general inference is reachable within the current architecture;
2. general inference is reachable only after targeted architectural revision;
3. general inference is not currently justified.

If revision is required, likely candidates include:

- introducing a richer recursion representation while remaining
  thesis-faithful;
- revisiting the strict `non-cyclic-graph` model if it is the primary
  obstacle;
- revising how fail-closed search is expressed so that research exploration is
  not forced into endless packet-specific structure; or
- restructuring ownership/binding machinery for inferred recursion.

Exit gate:

- the project makes an explicit strategic choice instead of drifting into
  accidental architectural change.

### Milestone 9 - Decide product posture

Even a technically successful research result still needs a repo-level
delivery posture.

Possible postures:

- default-on production capability;
- explicit feature gate;
- research-only mode;
- or staged rollout from research gate to production default.

Exit gate:

- promotion policy is tied to evidence and operational confidence rather than
  momentum.

## Decision Gates

The program should be treated as a sequence of decision gates, not as an
open-ended implementation stream.

### Gate G1 - Target clarity

Can the project define "general automatic iso-recursive inference" precisely
enough to test?

If not, stop and finish the target definition before more implementation.

### Gate G2 - Architectural plausibility

Do the current boundaries plausibly permit the target?

If not, move to explicit architecture revision instead of continuing bounded
packet work.

### Gate G3 - Search credibility

Is there a principled ambiguity and termination story?

If not, broad inference is not ready.

### Gate G4 - Reconstruction credibility

Does inferred recursion survive the full pipeline coherently?

If not, the result is still internal prototype evidence, not general
capability.

### Gate G5 - Coverage credibility

Does the evidence span a representative family matrix rather than a few
selected lanes?

If not, keep the claim bounded.

## Principal Risks

### Risk 1 - Packet accumulation without mechanism unification

The project may continue to collect bounded wins without ever producing a
general mechanism. This would create motion without convergence.

### Risk 2 - The `non-cyclic-graph` boundary may be too restrictive

This may be the single most important architectural risk. If broad recursive
inference fundamentally wants a richer structural representation, then further
packet work inside the current graph model may have declining value.

### Risk 3 - Search becomes heuristic rather than principled

If general recursive inference depends on hidden ranking or lane-specific
guessing, the result will not meet the repo's fail-closed standard.

### Risk 4 - Reconstruction becomes the hidden bottleneck

It is possible for solver-side recursive inference to look promising while the
elaboration/reification story remains too brittle to expose it cleanly.

### Risk 5 - Thesis-faithfulness drifts under pressure

If the general goal starts forcing architectural revisions, the repo must not
silently abandon the paper-faithful commitments that justify the project.

## Success Criteria

The project should only claim general automatic iso-recursive inference when
all of the following are true:

- the positive corpus succeeds broadly;
- the negative corpus still fails closed;
- search behavior is bounded and explainable;
- inferred recursion survives the full pipeline;
- thesis-faithfulness is documented, including any explicit deviations; and
- any boundary changes are deliberate, reviewable, and justified.

## Stop Criteria

The program should explicitly stop or pause if any of the following becomes
clear:

- the target cannot be defined precisely enough to test;
- the current architecture is not plausible and no acceptable revision is yet
  available;
- the required search model is incompatible with the repo's fail-closed
  standards;
- reconstruction cannot be made coherent without unacceptable widening; or
- the required architectural changes would move the repo too far from its
  thesis-faithful aims.

## Honest Current Position

As of 2026-03-25, the honest project status is:

> The repo is closer to general automatic iso-recursive inference than it was
> before accepted `N11` through `N14`, but it still has bounded verified
> packets rather than a general automatic inference capability.

That is a meaningful advance, but it is not the final goal.

## Recommended Immediate Next Human-Level Deliverable

If the project wants to keep moving deliberately, the next highest-value human
document is:

> `General Automatic Iso-Recursive Inference Feasibility And Architecture Decision`

That document should answer:

- what exact capability the repo is aiming for;
- whether the current architecture can plausibly reach it;
- which constraint is most likely to require revision first; and
- what evidence would justify either staying the course or changing the
  architecture.
