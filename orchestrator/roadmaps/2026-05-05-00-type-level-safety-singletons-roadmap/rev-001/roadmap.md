# Type-Level Safety with Singletons Roadmap

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`
Base branch: `master`
Created: 2026-05-05

## Goal

Encode runtime invariants as compile-time types using GADTs, DataKinds,
and the `singletons` library. The codebase already uses GADTs (~25 modules)
and DataKinds (~10 modules) but has no `singletons` dependency and leaves
several critical invariants unchecked at compile time:

- `NodeRef` is a plain sum type discriminated at ~32 runtime sites
- `Constraint` has no phase tracking — field availability per pipeline
  stage is documentary only
- `ForallSpec` uses a runtime `Int` binder count — off-by-one errors
  surface as panics in `Elab/Phi/Translate.hs`
- `EdgeWitness` carries unchecked well-formedness invariants

This roadmap introduces `singletons` for the phase-indexed `Constraint`
(the killer app — runtime dispatch on a type-level phase value) and uses
plain GADTs + DataKinds for the simpler indexed types.

## Outcome Boundaries

- `singletons` / `singletons-base` / `singletons-th` are the only new
  dependencies. No `type-level-sets`, `first-class-families`, or other
  heavy type-level libraries.
- Each milestone must build and pass all 2524+ tests before the next
  milestone begins.
- The refactor is strictly internal — no public API changes to
  `src-public/MLF/API.hs` or `src-public/MLF/Pipeline.hs`.
- Type-level machinery stays in `src/MLF/Constraint/Types/` and
  `src/MLF/Binding/`. Pipeline modules consume indexed types but do not
  define type-level computation.
- The `singletons` boilerplate (generated via TH or handwritten) lives in
  dedicated `*.Singletons` modules, not scattered across existing files.
- Performance must not regress measurably. Compile-time cost of
  `singletons` TH is acceptable; runtime cost is not.

## Global Sequencing Rules

- Milestones are serial: each depends on the previous.
- Every milestone must pass `cabal build all && cabal test` before the
  next begins.
- The NodeRef GADT (milestone 1) is the foundation — all later milestones
  depend on the indexed ref type.
- The phase-indexed Constraint (milestones 2-3) is the largest change and
  may be split into sub-milestones if the diff exceeds ~500 lines.
- Milestone 5 (witness validation) does not use `singletons` but completes
  the type-safety picture and should ship with this family.

## Parallel Lanes

- `lane-main`: default serial lane for the full family.

## Milestones

### 1. NodeRef GADT and RefTag kind

- Milestone id: `milestone-1`
- Depends on: none
- Intent: replace the plain `NodeRef` sum type with a GADT indexed by a
  `RefTag` kind (`TypeTag` | `GenTag`). This eliminates the ~32 runtime
  `expectTypeRef` / pattern-match discriminators and makes functions that
  only accept type nodes statically enforced.
- Scope:
  - Define `RefTag` as a promoted data kind in
    `MLF.Constraint.Types.Graph.NodeEdge`.
  - Redefine `NodeRef (t :: RefTag)` as a GADT with `TypeRef :: NodeId ->
    NodeRef 'TypeTag` and `GenRef :: GenNodeId -> NodeRef 'GenTag`.
  - Split `BindParents` from a single `IntMap (NodeRef, BindFlag)` into
    separate typed maps or a heterogeneous container, since the current
    `nodeRefKey` scheme encodes both tags in one `IntMap`.
  - Update `IntMapUtils.hs` (~17 filter sites), `GraphOps.hs` (3
    `expectTypeRef` calls), `Reify/Type/Core.hs` (~12 match arms), and
    all other NodeRef consumers.
  - Delete `expectTypeRef` and `expectGenRef` — they become unnecessary.
  - Introduce `SomeNodeRef` existential for the ~5 sites that genuinely
    need to store both tags (e.g., `BindParents` keying).
- Completion signal: `expectTypeRef` is deleted, all NodeRef consumers
  are type-correct, `cabal build && cabal test` passes.
- Estimated diff: ~300 lines across ~15 files.

### 2. Add singletons dependency and Phase kind foundation

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: add `singletons-th` to `mlf2.cabal` and define the `Phase`
  kind with its singleton type. This is the scaffolding for the
  phase-indexed Constraint.
- Scope:
  - Add `singletons-th` (which transitively provides `singletons-base`
    and `singletons`) to `build-depends` in `mlf2.cabal`.
  - Create `MLF.Constraint.Types.Phase` module defining:
    - `data Phase = Raw | Normalized | Acyclic | Presolved | Solved`
    - `genSingletons [''Phase]` to generate `Sing`, `SingI`, `SingKind`
      instances
  - Create a smoke test that pattern-matches on `Sing ('Raw :: Phase)` to
    verify TH generation works.
- Completion signal: `cabal build && cabal test` passes with the new
  dependency; `SPhase` singleton type is usable in a test.
- Estimated diff: ~50 lines + cabal change.

### 3. Phase-indexed Constraint type

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: add a phantom type parameter to `Constraint` that tracks which
  pipeline stage it has reached. Define type families that compute field
  availability per phase. This is the main `singletons` win — functions
  can dispatch on the phase at runtime via `Sing p` while the type system
  enforces field access.
- Scope:
  - Add phantom parameter: `data Constraint (p :: Phase) = Constraint
    { ... }` in `MLF.Constraint.Types.Graph`.
  - Define type families for field gating (e.g., `type family
    HasInstEdges (p :: Phase) :: Bool`).
  - Introduce `withConstraintPhase :: Sing p -> Constraint p -> (forall
    q. Constraint q -> r) -> r` for safe phase transitions.
  - Update `ConstraintGen` to produce `Constraint 'Raw`.
  - Update `Normalize` to produce `Constraint 'Normalized`.
  - Update `Acyclicity` to produce `Constraint 'Acyclic`.
  - Update `Presolution` to produce `Constraint 'Presolved`.
  - Update `Solve` to produce `Constraint 'Solved`.
  - Each pipeline stage's type signature changes from `Constraint ->
    Constraint` to `Constraint p -> Constraint (Next p)`.
  - Pipeline orchestrator (`Elab/Run/Pipeline.hs`) threads the
    phase-indexed type through all stages.
  - Helper: `type family Next (p :: Phase) :: Phase` with closed type
    family mapping each phase to its successor.
- Completion signal: the pipeline type signatures are phase-indexed,
  `cabal build && cabal test` passes, impossible phase accesses are type
  errors.
- Estimated diff: ~400-600 lines across ~30 files. This is the largest
  milestone.
- Risk: high churn. Mitigation: use `unsafeCoerce`-based `castConstraint`
  as a migration shim, then remove it file-by-file.

### 4. Vec-indexed ForallSpec

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: replace `fsBinderCount :: Int` in `ForallSpec` with a
  length-indexed `Vec n BoundRef`, eliminating off-by-one panics in
  quantifier instantiation.
- Scope:
  - Import or define `Data.Vec` from `singletons-base` (or
    `vector-sized`, whichever is lighter).
  - Redefine `ForallSpec` as `data ForallSpec (n :: Nat) = ForallSpec {
    fsBounds :: Vec n BoundRef }`.
  - Update `forallSpecFromForall` in `Binding/Tree.hs` to produce a
    `ForallSpec n` with the length tracked at the type level.
  - Update `Elab/Phi/Translate.hs` (~15 indexing sites) to use
    safe `Vec` indexing instead of `!!` / `atMay`.
  - Update `Presolution/Expansion.hs` and `Presolution/Copy.hs` that
    consume `ForallSpec`.
- Completion signal: no `!!` or `atMay` calls on binder arrays remain in
  `Phi/Translate.hs`, `cabal build && cabal test` passes.
- Estimated diff: ~200 lines across ~8 files.

### 5. Witness smart constructors

- Milestone id: `milestone-5`
- Depends on: `milestone-1` (uses indexed NodeRef), does not require
  `singletons`
- Intent: enforce `EdgeWitness` well-formedness at construction time
  rather than assuming it at elaboration time. The invariants (ewRoot
  reachable from ewLeft, ops correspond to expansion) become checked
  preconditions.
- Scope:
  - Add `mkEdgeWitness :: ... -> Either WitnessError EdgeWitness` in
    `MLF.Constraint.Types.Witness` that validates all invariants.
  - Add `mkInstanceWitness :: [InstanceOp] -> Either WitnessError
    InstanceWitness` that validates op well-formedness.
  - Replace all direct `EdgeWitness { ... }` record construction with
    calls to `mkEdgeWitness`.
  - Add `WitnessError` type with descriptive constructors for each
    invariant violation.
  - Update `Presolution/Witness.hs` and `Presolution/WitnessNorm.hs`
    which construct witnesses.
  - Update `Elab/Phi/Translate.hs` to remove redundant runtime
    validation that the smart constructor now guarantees.
- Completion signal: all `EdgeWitness` construction goes through smart
  constructors, `cabal build && cabal test` passes.
- Estimated diff: ~150 lines across ~6 files.

### 6. Integration and cleanup

- Milestone id: `milestone-6`
- Depends on: `milestones 3, 4, 5`
- Intent: remove migration shims, update documentation, ensure the
  type-level changes are stable and well-tested.
- Scope:
  - Remove any `unsafeCoerce`-based `castConstraint` migration shims
    from milestone 3.
  - Update `docs/architecture.md` to document the phase-indexed
    Constraint and type-level invariants.
  - Add focused tests for phase-mismatch type errors (compile-time
    negative tests using `shouldNotTypecheck` or similar).
  - Verify no performance regression in `cabal test` runtime.
  - Update `AGENTS.md` if the type-level conventions warrant repo-wide
    guidance.
- Completion signal: no migration shims remain, docs are updated,
  `cabal build && cabal test` passes with no performance regression.
- Estimated diff: ~100 lines across ~5 files.
