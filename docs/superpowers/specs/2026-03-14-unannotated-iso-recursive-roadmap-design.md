# Staged Roadmap Design: From `ARI-C1` to Unannotated Single-SCC Iso-Recursive Inference

Date: 2026-03-14
Status: Proposed design

## Purpose

Define a new research-only roadmap that starts from the accepted bounded `ARI-C1` handoff and stages toward a final implementation-handoff target for **unannotated, single-SCC, iso-recursive type inference**.

This design does **not** authorize a production implementation milestone. It defines the next roadmap only, while allowing a later bounded feasibility pass if a future roadmap item explicitly authorizes that research step under non-default, fail-closed conditions.

## Starting Point

The completed top-level orchestrator reached a bounded result:

- explicit-only recursive support is the current accepted baseline;
- `ARI-C1` is the accepted bounded candidate;
- `ARI-C1` is annotation-anchored, single-SCC, single recursive binder family;
- the accepted item-2 invariant audit remains authoritative for acyclicity, binding-tree discipline, occurs-check/termination, and reconstruction/reification/witness replay obligations;
- equi-recursive equality/unfolding is out of scope;
- cyclic structural graph encoding is out of scope; and
- fully unannotated recursive-type synthesis is still unresolved.

## Final Goal of the New Roadmap

The new roadmap should end at a **research-backed implementation handoff**, not at an implementation attempt.

That handoff target is:

- unannotated iso-recursive inference;
- limited to **single-SCC, single-binder-family** cases only, where the SCC is taken over the inference-dependency graph for candidate recursive binder obligations rather than the structural `TyNode`/constraint graph;
- still non-equi-recursive;
- still non-cyclic-graph; and
- still fail-closed if the bounded target cannot be justified without widening.

## Non-Goals

- No production implementation milestone in this roadmap.
- No multi-SCC recursive inference target.
- No equi-recursive reasoning or implicit unfolding semantics.
- No cyclic `TyNode` or cyclic constraint-graph representation.
- No silent widening from bounded cases to broad recursive inference.
- No rewriting of predecessor orchestrator packet history.

## Chosen Roadmap Shape

Use a **conservative ladder** from `ARI-C1`, widening one boundary at a time instead of jumping directly to broad unannotated inference.

The roadmap uses:

- the accepted `ARI-C1` result as the baseline (`R0`);
- three design bridge stages (`R1` to `R3`);
- one bounded feasibility gate (`R4`); and
- one final handoff stage (`R5`).

## Fixed Boundary Model

These constraints remain mandatory for the entire roadmap:

- **Iso-recursive only**: recursive types stay explicit fixed-point constructors, not equi-recursive equalities.
- **Non-cyclic graph**: recursive structure must not require cyclic structural graph encoding.
- **Single-SCC means obligation-level recursion, not structural graph cycles**: each node in the inference-dependency graph is a candidate recursive binder obligation owned by one recursive binder family, and there is an edge `A -> B` when discharging obligation `A` requires recursive-shape facts introduced by obligation `B`; the only allowed SCC is one component in that obligation graph, while the `TyNode`/constraint graph itself must remain acyclic.
- **Single binder family only**: the endpoint inherits the accepted `ARI-C1` shape restriction that all admitted obligations belong to one recursive binder family, with no cross-family SCC linking.
- **Fail closed**: if a stage discovers it needs forbidden widening, the roadmap records `not-yet-go` instead of stretching scope.
- **No silent enablement**: research progress must not imply default-on implementation behavior.
- **Single-SCC endpoint only**: the roadmap stops at a handoff for unannotated single-SCC, single-binder-family inference.

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

### `R2` Bounded Subset Selection and Admissibility Contract

Deliverable:

- one artifact that selects exactly one bounded unannotated single-SCC candidate subset; and
- one admissibility statement that records deferred and rejected alternatives.

Required focus:

- one candidate only, with a stable identifier;
- a precise statement of which unannotated cases are admitted by that candidate;
- an explicit statement that the candidate remains single-binder-family with no cross-family SCC linking unless a later roadmap explicitly widens that boundary;
- deferred alternatives that are still plausible later; and
- rejected alternatives that would already breach the fixed boundary model.

Purpose:

- prevent the roadmap from silently broadening from `ARI-C1` to a vague notion of “unannotated inference.”

Exit condition:

- all later milestones can refer to one chosen bounded subset rather than the whole unannotated design space.

### `R3` Inference-Obligation Contract

Deliverable:

- one artifact that defines the exact obligations the `R2` chosen subset must satisfy.

Required contents:

- acyclicity obligations for the non-cyclic structural graph boundary;
- binder ownership and scope discipline;
- occurs-check and termination constraints;
- reconstruction, reification, and witness replay obligations;
- principality risk boundaries; and
- failure cases that must remain rejected.

Purpose:

- turn the selected subset from “admitted candidate” into explicit proof obligations.

Exit condition:

- the selected subset is specified tightly enough that a feasibility pass would know which invariants must hold and which failures are expected.

### `R4` Bounded Feasibility Decision

Deliverable:

- one artifact that defines the exact evidence required to judge the `R2` chosen subset; and
- one explicit decision outcome, either `feasible-continue` or `not-yet-go`.

Required contents:

- positive example classes for the chosen unannotated single-SCC subset;
- negative example classes that must still fail;
- success evidence, including acyclicity preservation and witness/replay safety;
- no-go triggers;
- immediate stop conditions; and
- a reviewer-visible explanation for the final decision outcome.

Purpose:

- ensure the roadmap does not move from obligations straight to handoff without a bounded feasibility judgment, mirroring the accepted `selection -> feasibility -> handoff` chain used for `ARI-C1`.

Exit condition:

- the chosen subset is either explicitly cleared as `feasible-continue` under the fixed boundaries or explicitly stopped as `not-yet-go`.

### `R5` Implementation-Handoff Spec

Deliverable:

- one final implementation-handoff document for bounded unannotated single-SCC iso-recursive inference, but only if `R4` concluded `feasible-continue`.

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
- `R2` gate: exactly one bounded subset is selected and all alternatives are explicitly deferred or rejected.
- `R3` gate: the obligation contract is specific enough to define admissible and inadmissible designs for the chosen subset.
- `R4` gate: feasibility is judged with an explicit `feasible-continue` or `not-yet-go` result.
- `R5` gate: the handoff target is concrete enough for implementation planning.

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
- preserves the accepted `selection -> feasibility -> handoff` evidence chain instead of collapsing those gates into one step.
