# General Automatic Iso-Recursive Inference Strategic Orchestrator Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed post-`L2`
  automatic iso-recursive successor loop whose accepted execution record ended
  at `orchestrator/rounds/round-081`.
- Completed rounds `round-001` through `round-081` remain authoritative
  historical evidence. They are predecessor evidence for this refreshed
  control plane and must not be silently rewritten.
- The inherited baseline contract remains: explicit recursive annotations are
  supported, automatic recursive-type inference is unresolved and disabled, and
  the explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary remains binding unless an
  accepted roadmap decision explicitly changes it.
- The accepted `N14` decision artifact at
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the immediate predecessor result: one exact same-lane retained-child
  `boundVarTarget -> targetC` packet is accepted bounded evidence only, the
  long-horizon goal remains unresolved, and no successor lane is yet
  authorized or bound.
- The strategic human roadmap at
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-strategic-roadmap.md`
  is now the source document for this refreshed control plane.
- The new live goal is not "pick the next packet." The live goal is to decide
  whether general automatic iso-recursive inference is reachable, under which
  architecture, and what evidence or revisions are required before broader
  implementation can be justified.
- Early rounds in this refreshed control plane are expected to be docs-first.
  Production changes, broader inference experiments, or boundary revisions are
  not lawful until the corresponding strategic roadmap items explicitly make
  them so.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [pending] Define the repo-level capability contract and evaluation corpus for general automatic iso-recursive inference
   Depends on:
   Completion notes: produce one docs-first artifact that turns the strategic
   roadmap into a concrete capability contract for this repo. It must define
   what "general automatic iso-recursive inference" means here, distinguish
   bounded packet evidence from general capability, identify the required
   positive and negative corpus families, and state explicit success / failure
   criteria around soundness, thesis-faithfulness, termination, and
   explainability. This item must cite the inherited baseline plus accepted
   `N14` continuity and must leave code and solver behavior unchanged.

2. [pending] Audit the current architectural constraints against the capability contract
   Depends on: item 1
   Completion notes: classify the inherited `iso-recursive`,
   `non-equi-recursive`, `non-cyclic-graph`, and `no-fallback` constraints as
   `keep`, `revise`, or `unknown`, record which appear compatible with the
   defined capability target and which look like likely blockers, and state
   whether general inference still appears plausible inside the current
   architecture. This item remains docs-only unless the roadmap is explicitly
   amended later.

3. [pending] Generalize the accepted packet history into a reusable mechanism map
   Depends on: items 1, 2
   Completion notes: extract reusable mechanism families from accepted bounded
   evidence rather than selecting another narrow lane. The output should cover
   recursive-shape discovery, binder / owner placement, target / consumer
   alignment, local versus non-local propagation, interaction with polymorphism
   and instantiation, reconstruction obligations, and fail-closed ambiguity
   handling. The result should explain multiple accepted packets and make clear
   what still lacks a general mechanism.

4. [pending] Design the search, ambiguity, and termination model for general recursive inference
   Depends on: items 1, 2, 3
   Completion notes: define when recursive inference is considered, when it is
   forbidden, how competing recursive candidates are compared, when ambiguity
   must fail closed, and why the resulting search still terminates. This item
   may include bounded prototype planning if earlier accepted items make that
   lawful, but it must not silently widen beyond the accepted capability
   contract and architectural audit.

5. [pending] Define the full-pipeline reconstruction and validation contract
   Depends on: items 1, 3, 4
   Completion notes: state how inferred recursive structure must survive solver
   state, elaboration, reification / reconstruction, and user-visible output
   surfaces, and define the corresponding validation requirements. This item
   must make clear what evidence is needed before any solver-side success can
   count as a repo-level capability.

6. [pending] Run a representative coverage and feasibility campaign
   Depends on: items 2, 4, 5
   Completion notes: validate the chosen mechanism and search model across
   representative local / non-local, retained-child / alias-bound,
   nested-`forall`, binder-sensitive, ambiguity, termination, and
   reconstruction-heavy families. The output must say whether the accumulated
   evidence supports broad generality, a bounded subset only, or an
   architectural dead end.

7. [pending] Make the architecture decision and successor-plan choice
   Depends on: items 2, 6
   Completion notes: record exactly one strategic outcome for this refreshed
   control plane: continue within the current architecture, pursue targeted
   boundary revision, or stop. If continuing, make the next cycle concrete and
   bounded. If revising, name the first lawful revision gate and the
   architectural boundary under review. If stopping, preserve predecessor truth
   and state why broader work is not yet justified.
