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

1. [done] Define the repo-level capability contract and evaluation corpus for general automatic iso-recursive inference
   Depends on:
   Completion notes: accepted in `round-082` via
   `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`.
   The repo now has a docs-first capability contract that defines the
   repo-level target, separates bounded predecessor evidence from any general
   capability claim, names the minimum positive / negative family matrix
   (`P1`-`P6`, `N1`-`N6`), and records later success / no-claim gates around
   soundness, thesis-faithfulness, termination, and explainability. The
   inherited explicit-only / iso-recursive / non-equi-recursive /
   non-cyclic-graph / no-second-interface / no-fallback boundary remains
   unchanged, and no production behavior was widened.

2. [done] Audit the current architectural constraints against the capability contract
   Depends on: item 1
   Completion notes: accepted in `round-083` via
   `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`.
   The inherited four-constraint audit is now complete: `iso-recursive =
   keep`, `non-equi-recursive = keep`, `non-cyclic-graph = unknown`, and
   `no-fallback = keep`. The accepted item-2 read is still bounded and
   docs-only: the repo-level target remains unresolved because the
   `non-cyclic-graph` constraint is not yet shown either sufficient or
   blocking for representative `P2`-`P6` pressure, especially around
   non-local propagation, binder/owner sensitivity, nested polymorphism,
   reconstruction visibility, and bounded `N6` termination pressure. No
   architecture revision, semantic widening, interface addition, or fallback
   widening was authorized.

3. [done] Generalize the accepted packet history into a reusable mechanism map
   Depends on: items 1, 2
   Completion notes: accepted in `round-084` via
   `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`.
   The accepted mechanism map now explains two bounded predecessor chains
   (`N4`-`N7` `baseTarget -> baseC` and `N11`-`N14`
   `boundVarTarget -> targetC`) through one seven-family vocabulary covering
   recursive-shape discovery, binder / owner placement, target / consumer
   alignment, local versus non-local propagation, interaction with
   polymorphism and instantiation, reconstruction obligations, and
   fail-closed ambiguity / unsafe-case handling. The bounded acyclic-model
   read is still partial: `P2`-`P4` are only partially explained, `P5`
   remains negative-only / unresolved, and the accepted record still depends
   on named packet guards rather than a general search policy. No search
   model, full reconstruction contract, coverage campaign, or architecture
   decision was authorized.

4. [pending] Design the search, ambiguity, and termination model for general recursive inference
   Depends on: items 1, 2, 3
   Completion notes: starting from the accepted item-3 mechanism map, define
   one bounded search / admissibility / termination model for the inherited
   acyclic architecture. The output must state how anchor-first
   recursive-shape discovery produces candidate targets, how owner / binder
   placement and target / consumer alignment constrain local versus non-local
   propagation, when nested-`forall` / owner-crossing / competing-candidate
   situations must fail closed, and why the resulting search terminates
   without cyclic graphs, multi-SCC search, or fallback. It must address the
   named item-3 debt around packet-specific guards
   (`rootNonLocalSchemeAliasBaseLike`, `sameLaneLocalRetainedChildTarget`,
   `boundHasForallFrom`, `not hasForall`) by either lifting them into general
   admissibility rules or recording them as unresolved blockers. Stop short
   of the full reconstruction contract, the coverage campaign, or an
   architecture decision.

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
