# Staged Roadmap Design: From `ARI-C1` to Unannotated Single-SCC Iso-Recursive Inference

Date: 2026-03-14
Status: Proposed design

## Purpose

Define a new research-only roadmap that starts from the accepted bounded `ARI-C1` handoff and stages toward a final implementation-handoff target for **unannotated, single-SCC, iso-recursive type inference**.

This design does **not** authorize implementation. It defines the next roadmap only.

## Starting Point

The completed top-level orchestrator reached a bounded result:

- explicit-only recursive support is the current accepted baseline;
- `ARI-C1` is the accepted bounded candidate;
- `ARI-C1` is annotation-anchored, single-SCC, single recursive binder family;
- equi-recursive equality/unfolding is out of scope;
- cyclic structural graph encoding is out of scope; and
- fully unannotated recursive-type synthesis is still unresolved.

## Final Goal of the New Roadmap

The new roadmap should end at a **research-backed implementation handoff**, not at an implementation attempt.

That handoff target is:

- unannotated iso-recursive inference;
- limited to **single-SCC** cases only;
- still non-equi-recursive;
- still non-cyclic-graph; and
- still fail-closed if the bounded target cannot be justified without widening.

## Non-Goals

- No implementation milestone in this roadmap.
- No multi-SCC recursive inference target.
- No equi-recursive reasoning or implicit unfolding semantics.
- No cyclic `TyNode` or cyclic constraint-graph representation.
- No silent widening from bounded cases to broad recursive inference.
- No rewriting of predecessor orchestrator packet history.

## Chosen Roadmap Shape

Use a **conservative ladder** from `ARI-C1`, widening one boundary at a time instead of jumping directly to broad unannotated inference.

The roadmap uses:

- the accepted `ARI-C1` result as the baseline (`R0`);
- three bridge stages (`R1` to `R3`);
- one final handoff stage (`R4`).

## Fixed Boundary Model

These constraints remain mandatory for the entire roadmap:

- **Iso-recursive only**: recursive types stay explicit fixed-point constructors, not equi-recursive equalities.
- **Non-cyclic graph**: recursive structure must not require cyclic structural graph encoding.
- **Fail closed**: if a stage discovers it needs forbidden widening, the roadmap records `not-yet-go` instead of stretching scope.
- **No silent enablement**: research progress must not imply default-on implementation behavior.
- **Single-SCC endpoint only**: the roadmap stops at a handoff for unannotated single-SCC inference.

## Roadmap Milestones

### `R0` Existing Baseline

Accepted prior result:

- bounded `ARI-C1` implementation handoff for annotation-anchored recursive-shape handling.

This is the starting point, not a new milestone to execute.

### `R1` Gap Map from `ARI-C1` to Unannotated Single-SCC Inference

Deliverable:

- one design artifact that names the exact information currently supplied by explicit anchors;
- what information might be recovered locally without widening semantics; and
- which unannotated single-SCC cases remain blocked.

Purpose:

- make the missing delta explicit rather than treating unannotated inference as a vague extension of `ARI-C1`.

Exit condition:

- a reviewer can point to a finite, concrete gap list between accepted `ARI-C1` behavior and the target unannotated single-SCC behavior.

### `R2` Inference-Obligation Contract

Deliverable:

- one artifact that defines the exact obligations a bounded unannotated single-SCC inference design must satisfy.

Required focus:

- binder ownership and scope discipline;
- occurs-check and termination constraints;
- reconstruction and reification obligations;
- principality risk boundaries; and
- failure cases that must remain rejected.

Purpose:

- turn “maybe inferable” into explicit proof obligations.

Exit condition:

- the bounded target is specified tightly enough that an implementation attempt would know which invariants must hold and which failures are expected.

### `R3` Verifier-Facing Feasibility Contract

Deliverable:

- one artifact that defines the exact evidence a later bounded feasibility pass would need.

Required contents:

- positive example classes for unannotated single-SCC cases;
- negative example classes that must still fail;
- success evidence;
- no-go triggers; and
- immediate stop conditions.

Purpose:

- ensure any later experimentation is judgeable by explicit reviewer/verifier evidence rather than intuition.

Exit condition:

- a future spike or prototype could be accepted or rejected with no ambiguity about the criteria.

### `R4` Implementation-Handoff Spec

Deliverable:

- one final implementation-handoff document for bounded unannotated single-SCC iso-recursive inference.

Required contents:

- the exact target behavior;
- explicit exclusions;
- first-touch module boundaries;
- required tests and failure cases;
- stop triggers that force a return to research; and
- sequencing guidance for the eventual implementation planner.

Purpose:

- stop the roadmap at the point where implementation could begin safely without this roadmap itself becoming an implementation project.

Exit condition:

- an implementer can begin planning from a bounded, reviewed handoff instead of inventing the target themselves.

## Validation Model

Each stage must produce a reviewer-visible artifact with an explicit gate.

- `R1` gate: the gap map is concrete and finite.
- `R2` gate: the obligation contract is specific enough to define admissible and inadmissible designs.
- `R3` gate: feasibility can be judged by explicit success/no-go/stop criteria.
- `R4` gate: the handoff target is concrete enough for implementation planning.

## Fail-Closed Rules

The roadmap must stop and record `not-yet-go` if any stage discovers that progress requires:

- equi-recursive reasoning or implicit unfolding;
- cyclic graph representation;
- multi-SCC support;
- silent widening from bounded cases to broad automatic inference; or
- unverifiable claims about soundness, principality, or termination.

## Recommendation

Adopt this roadmap as a new, separate research track rooted in the completed `ARI-C1` handoff.

It is the smallest design that:

- moves meaningfully closer to true automatic recursive-type inference;
- stays honest about what the repo has and has not established;
- preserves the thesis-sensitive boundaries already accepted; and
- ends at a usable implementation handoff rather than prematurely forcing code.
