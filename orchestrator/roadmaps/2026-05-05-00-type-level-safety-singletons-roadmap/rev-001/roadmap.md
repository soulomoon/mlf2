# Type-Level Safety With Singletons Roadmap

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`
Base branch: `master`
Created: 2026-05-05
Contract: `orchestrator-v2`

## Goal

Encode runtime invariants as compile-time types using GADTs, DataKinds, and
the `singletons` library. The codebase already uses GADTs and DataKinds, but
several important invariants still need a clearer compile-time boundary:

- mixed type-node and gen-node references are still stored in the binding-tree
  surface and converted at boundaries;
- `Constraint` phase progress must stay visible in function signatures instead
  of relying on documentary phase comments;
- `ForallSpec` must avoid redundant binder counts and partial binder indexing;
- witness construction must make well-formedness checks explicit at the point
  of construction.

This family keeps the refactor internal to the constraint, binding, presolution,
solve, and elaboration implementation surfaces. Public API changes are out of
scope unless a later semantic roadmap update explicitly approves them.

## Alignment Summary

- Preserve thesis faithfulness over compatibility with older internal helper
  shapes.
- Prefer narrow, named type-level seams over broad compatibility bridges.
- Keep `MLF.Backend.IR` and other public-facing boundaries out of this family
  unless a selected round proves a direct dependency.
- Use `singletons-th` only for phase singletons; simpler indexed types should
  remain plain GADTs/DataKinds unless the type-level computation needs
  singleton dispatch.
- Default to serial execution. These milestones touch shared type definitions
  and broad module signatures, so planner-selected worker fan-out requires
  explicit non-overlapping ownership and an integration plan.

## Outcome Boundaries

- `singletons-th` is the only intended new type-level dependency. Do not add
  `type-level-sets`, `first-class-families`, or similar libraries without a
  semantic roadmap update.
- Each behavior-changing round must run focused tests first, then the full gate
  required by `verification.md` before approval.
- The refactor is strictly internal by default. `src-public/MLF/API.hs` and
  `src-public/MLF/Pipeline.hs` are unchanged unless the selected direction
  explicitly requires public-surface work.
- Type-level machinery belongs near `src/MLF/Constraint/Types/` and
  `src/MLF/Binding/`; pipeline modules should consume indexed types rather
  than define unrelated type-level computation.
- Compile-time cost from `singletons-th` is acceptable. Runtime performance
  regressions are not.
- Migration shims must be private, owner-local, named for their backend or
  phase, and retired when their owning milestone closes.

## Global Sequencing Rules

- Default sequencing is serial: lower-numbered unfinished milestones should be
  selected first unless `roadmap-view.json` dependencies and current repo
  evidence justify a different dependency-ready direction.
- A round may reconcile current implementation evidence with roadmap intent,
  but it must not mark a milestone done unless the reviewer approves exact
  closeout selectors through `roadmap-view.json`.
- Status-only closeout may change status markers and compact completion
  pointers only. Any change to milestone meaning, direction meaning,
  sequencing, parallel lanes, verification meaning, or retry policy requires a
  semantic roadmap update.
- Planner-authored worker fan-out must be recorded in
  `round-plan-record.json`; do not encode worker scheduling only in prose.

## Parallel Lanes

- `lane-main`: default serial lane for all milestones in this family.
- `lane-docs`: documentation-only checks may run separately only when they do
  not modify shared type definitions, state, or active roadmap coordination.

## Milestones

### [pending] 1. NodeRef GADT And RefTag Boundary

- Milestone id: `milestone-1`
- Depends on: none
- Intent: make type-node and gen-node reference separation statically visible
  at typed boundaries, while preserving a deliberately named mixed-reference
  storage surface only where the binding tree truly needs one.
- Completion signal: runtime type/gen discrimination helpers are either
  deleted or isolated at explicitly named boundary conversion points; type-only
  operations accept typed references; binding-tree mixed storage has a
  documented owner; `cabal build all && cabal test` passes.
- Parallel lane: `lane-main`
- Coordination notes: this milestone controls the reference vocabulary used by
  later phase, binding, and witness work. The planner must inspect the current
  `NodeRef` / `NodeRefTag` shape before selecting a round because some pieces
  may already exist in the codebase.
#### Completion Pointers: milestone-1

none yet

#### Candidate Direction: Typed NodeRef Boundary Reconciliation

- Direction id: `direction-1a-noderefgadt-reftag-kind`
- Summary: reconcile the current `NodeRef` / `NodeRefTag` implementation with
  the roadmap's type-safe reference goal.
- Why it matters now: later milestones should not depend on an ambiguous
  reference-boundary story.
- Preconditions: current `NodeRef`, `NodeRefTag`, `SomeNodeRef`, and binding
  operations have been inspected at HEAD.
- Parallel hints: serial only; these definitions are shared broadly.
- Boundary notes: do not widen public API surfaces.
- Extraction notes: a lawful round may either finish migration toward a
  stronger GADT shape or document and enforce the accepted mixed-key seam if
  current architecture requires it.

### [pending] 2. Phase Kind And Singletons Foundation

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: add the singletons dependency and define the promoted phase kind used
  by the phase-indexed constraint graph.
- Completion signal: `singletons-th` is registered in `mlf2.cabal`, the
  `Phase` kind and singleton type are available from a dedicated module, a
  focused smoke test or equivalent compile-time evidence proves the singleton
  surface is usable, and `cabal build all && cabal test` passes.
- Parallel lane: `lane-main`
- Coordination notes: keep singleton boilerplate dedicated and do not spread
  Template Haskell across unrelated modules.
#### Completion Pointers: milestone-2

none yet

#### Candidate Direction: Singleton Phase Scaffold

- Direction id: `direction-2a-phase-singletons-foundation`
- Summary: verify and harden the `Phase` singleton foundation.
- Why it matters now: phase-indexed constraints rely on this shared kind.
- Preconditions: milestone 1 is either complete or the selected round proves
  that the phase work does not depend on unfinished NodeRef migration.
- Parallel hints: serial by default; can be docs/test-only if source is already
  implemented.
- Boundary notes: no additional type-level dependencies beyond `singletons-th`.
- Extraction notes: inspect current cabal, `Phase`, and test coverage before
  deciding whether this is implementation, test, or closeout work.

### [pending] 3. Phase-Indexed Constraint Type

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: make pipeline phase progress explicit in `Constraint` types and main
  phase entrypoints, so invalid phase ordering fails at compile time instead of
  depending on comments or raw graph views.
- Completion signal: `Constraint (p :: Phase)` is the graph type, main phase
  functions advance `Raw -> Normalized -> Acyclic -> Presolved`, solve consumes
  presolved constraints and returns the solved abstraction, broad graph casts
  are retired, any remaining phase erasure is private and owner-named, and the
  full gate passes.
- Parallel lane: `lane-main`
- Coordination notes: this is the highest-churn milestone. Split into small
  rounds if needed, but keep each round's selected direction and closeout
  selectors precise.
#### Completion Pointers: milestone-3

none yet

#### Candidate Direction: Constraint Phase Boundary Hardening

- Direction id: `direction-3a-phase-indexed-constraint`
- Summary: finish or validate the phase-indexed constraint graph and phase
  transition entrypoints.
- Why it matters now: later presolution and solve work should consume explicit
  phase types.
- Preconditions: `Phase` is available and the current `Constraint` type has
  been inspected.
- Parallel hints: serial; broad type signatures make concurrent edits risky.
- Boundary notes: owner-local transition helpers are acceptable; broad raw-view
  compatibility helpers are not.
- Extraction notes: use focused compile/build slices before the full gate.

### [pending] 4. ForallSpec Binder Safety

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: remove redundant binder counts and make binder-indexing operations
  total enough that quantifier-introduction and Phi translation cannot drift
  through off-by-one runtime assumptions.
- Completion signal: `ForallSpec` has no redundant binder-count field, binder
  count is derived from the bounds payload or a stronger length-indexed
  structure, partial binder-array indexing is absent from the selected
  translation path, and tests cover the replacement behavior.
- Parallel lane: `lane-main`
- Coordination notes: the planner must compare the current implementation to
  the original Vec-indexed proposal and select the smallest round that closes
  the real safety gap.
#### Completion Pointers: milestone-4

none yet

#### Candidate Direction: ForallSpec Shape And Indexing Audit

- Direction id: `direction-4a-forallspec-binder-safety`
- Summary: audit and close the remaining binder-count and partial-indexing
  risks around `ForallSpec`.
- Why it matters now: witness and elaboration correctness depends on binder
  arity staying aligned.
- Preconditions: phase-indexed constraint boundary is stable enough that
  presolution/elaboration signatures will not churn underneath the round.
- Parallel hints: serial unless split into read-only audit and later code work.
- Boundary notes: do not add a heavy type-level collection dependency without
  a semantic roadmap update.
- Extraction notes: if the current list-based design is accepted, the round
  must document why it satisfies the safety goal and add tests for the actual
  risk.

### [pending] 5. Witness Smart Constructors

- Milestone id: `milestone-5`
- Depends on: `milestone-1`
- Intent: enforce `EdgeWitness` and `InstanceWitness` well-formedness at
  construction time instead of relying on scattered elaboration-time checks.
- Completion signal: production witness construction goes through smart
  constructors, constructor visibility is intentionally controlled, validation
  covers the invariants required by consumers, redundant downstream checks are
  removed only when the constructor guarantee really subsumes them, and the
  full gate passes.
- Parallel lane: `lane-main`
- Coordination notes: this milestone may proceed after milestone 1 even if
  phase-indexed work is still pending, but planner selection must avoid
  conflicts with active presolution rewrites.
#### Completion Pointers: milestone-5

none yet

#### Candidate Direction: Witness Construction Invariant Closure

- Direction id: `direction-5a-witness-constructor-invariants`
- Summary: finish smart-constructor enforcement and validation for witness
  values.
- Why it matters now: witness consumers should not each rediscover the same
  malformed-value cases.
- Preconditions: current `EdgeWitness`, `InstanceWitness`, presolution witness
  construction, and test fixture construction have been inspected.
- Parallel hints: serial; tests and production constructors share exported
  surfaces.
- Boundary notes: do not weaken tests by replacing concrete malformed fixtures
  with smoke assertions.
- Extraction notes: a lawful round may split production constructor use from
  fixture migration if the blast radius is high.

### [pending] 6. Integration And Cleanup

- Milestone id: `milestone-6`
- Depends on: `milestone-3`, `milestone-4`, `milestone-5`
- Intent: remove migration shims, update durable guidance, and prove the
  type-level safety changes are stable.
- Completion signal: no broad migration shims remain, docs and `AGENTS.md`
  match the accepted implementation, focused compile-time or source-shape
  guards cover the important invariants, performance-sensitive checks are not
  measurably worse, and `cabal build all && cabal test` passes.
- Parallel lane: `lane-main`
- Coordination notes: do not use this as a dumping ground for unrelated
  cleanup. If late evidence changes future roadmap meaning, use semantic
  `update-roadmap`.
#### Completion Pointers: milestone-6

none yet

#### Candidate Direction: Contract And Cleanup Closeout

- Direction id: `direction-6a-integration-cleanup`
- Summary: align docs, tests, and migration-shim cleanup with the accepted
  type-level safety implementation.
- Why it matters now: the roadmap should close only when the implemented
  safety story, docs, and verification gates agree.
- Preconditions: milestones 3, 4, and 5 have reviewer-approved closeout or the
  selected round is explicitly a closeout audit.
- Parallel hints: documentation-only follow-ups can be isolated, but final
  closeout remains serial.
- Boundary notes: do not claim terminal completion from local source scans
  alone if required build/test evidence is missing.
- Extraction notes: include any remaining `AGENTS.md`, `docs/architecture.md`,
  `CHANGELOG.md`, and focused guard-test updates in a bounded closeout plan.
