# Architecture Deepening Roadmap

Roadmap family: `2026-05-16-00-architecture-deepening-roadmap`
Revision: `rev-001`
Base branch: `master`
Created: 2026-05-16
Contract: `orchestrator-v2`

## Goal

Scaffold one fresh orchestrator family for all six architecture candidates
selected after the read-only architecture pass at `68e8e732`:

- Primitive Inventory Module;
- Backend Callable-Shape Module;
- Native Parity Policy Module;
- CLI Emission Preparation;
- Prepared Generalization Artifact Depth;
- Snapshot Finalization Seam.

The family should deepen ownership boundaries, not broaden public facades. Each
round should move duplicated policy or caller knowledge into the owning Module
or produce a concrete no-change justification tied to the current repo
architecture, `CONTEXT.md`, and accepted ADRs.

## Alignment Summary

- Approved alignment decision: scaffold all six selected architecture
  candidates into one roadmap family.
- Already-decided, not new candidates: Legacy Surface Retirement is settled by
  `docs/adr/2026-05-14-legacy-surface-retirement.md`, and Backend Structural
  Recursive Data Matching is settled by
  `docs/adr/2026-05-14-backend-structural-recursive-data-matching.md`.
- Chosen strategy: start with lower-risk backend/runtime/test ownership
  cleanup, then move into CLI emission preparation, then the deeper
  elaboration and Snapshot Finalization seams.
- Success means each milestone leaves one clearer owner, consumer modules that
  depend on that owner instead of copying policy or inspecting raw fields,
  focused regression coverage for the moved responsibility, and
  `docs/architecture.md` updates when module ownership changes.
- Preserve the current public boundary decisions: xMLF remains the elaboration
  IR, `MLF.Backend.IR` remains the executable backend IR seam, and no second
  public backend IR or broad public lowering API is introduced.
- Use current domain language. Prefer **Prepared Generalization Artifact** and
  **Snapshot Finalization**. Do not use "snapshot materialization" for the
  canonical read-model and solved-handle construction seam.
- Preserve thesis faithfulness over compatibility convenience. Do not add
  aliases, broad raw-view bridges, or migration shims unless a selected
  roadmap direction proves they are paper-backed and temporary.
- Default to serial execution. The milestones touch shared compiler,
  interpreter, backend, CLI, and parity surfaces, so any planner fan-out must
  record disjoint ownership and integrated validation in
  `round-plan-record.json`.

## Outcome Boundaries

- Public API files under `src-public/` are out of scope unless a selected round
  explicitly proves a public-surface ownership gap.
- New public facades, duplicate IR layers, lazy-runtime/STG machinery, broad
  FFI lanes, permanent compatibility layers, and parser alias preservation are
  out of scope.
- Do not reopen Legacy Surface Retirement or Backend Structural Recursive Data
  Matching as candidate discovery work in this family. If a selected round
  touches either area, it must treat the accepted ADR as binding context.
- Ownership moves must stay close to existing module boundaries and should
  prefer existing repo helpers, types, and test-support seams over new
  abstraction layers.
- Tests that currently encode real behavior must be preserved or strengthened.
  Do not turn parity, backend, CLI, or elaboration coverage into smoke-only
  checks.
- Behavior-changing work must run focused validation first and then
  `cabal build all && cabal test` before approval.
- Docs-only or guidance-only rounds may use `git diff --check` plus manual
  evidence that the docs do not overclaim implementation state.
- Roadmap completion is reviewer-owned. Source scans or absence of obvious
  TODOs are not enough to mark a milestone done.

## Global Sequencing Rules

- Default sequencing is serial: lower-numbered unfinished milestones should be
  selected first unless current repo evidence and `roadmap-view.json`
  dependencies justify another dependency-ready direction.
- Milestone 1 establishes the primitive inventory owner before backend,
  native, and CLI emission policy cleanup depends on primitive names and
  support policy.
- Milestone 2 isolates callable-shape ownership before parity or CLI work
  relies on backend callable diagnostics.
- Milestone 3 moves native parity policy next to `ProgramMatrix` before CLI
  emission preparation refactors test surfaces that may consume that policy.
- Milestone 4 narrows CLI/file orchestration after backend/native policies have
  visible owners.
- Milestones 5 and 6 then move into the deeper elaboration and finalized
  snapshot seams.
- A round may start as an audit, but it must produce durable evidence: a
  selected owner, a concrete no-change justification, tests, docs, or a
  semantic roadmap update if the candidate proves mis-scoped.
- Status-only closeout may change status markers and compact completion
  pointers only. Any change to milestone meaning, direction meaning,
  sequencing, parallel lanes, verification meaning, or retry policy requires a
  semantic roadmap update.
- Planner-authored worker fan-out must be recorded in
  `round-plan-record.json`; do not encode worker scheduling only in prose.

## Parallel Lanes

- `lane-backend-runtime`: Primitive Inventory Module, Backend Callable-Shape
  Module, Native Parity Policy Module, and CLI Emission Preparation.
- `lane-elaboration`: Prepared Generalization Artifact Depth and Snapshot
  Finalization Seam.
- `lane-docs`: documentation-only ownership alignment that does not modify
  source, tests, roadmap coordination, or active round state.

The current controller state keeps `max_parallel_rounds` at `1`. These lanes
are ownership hints for future planner reasoning, not permission to run
concurrent rounds unless the controller state and selected plan explicitly
allow it.

## Milestones

### [done] 1. Primitive Inventory Module

- Milestone id: `milestone-1`
- Depends on: none
- Intent: create one private primitive-inventory Module that owns reserved
  runtime names, source/backend types, and native-emission policy while
  existing frontend, backend, lowering, CLI, docs, and tests become adapters.
- Completion signal: primitive names/types/support policy have one private
  owner or explicit owner map; repeated tables in `MLF.Frontend.Program`
  builtins, `MLF.Backend.IR`, `MLF.Backend.Convert`, and
  `MLF.Backend.LLVM.Lower` are deleted or adapter-local; focused tests assert
  inventory consistency; `docs/architecture.md` records the owner; required
  validation passes.
- Parallel lane: `lane-backend-runtime`
- Coordination notes: inspect the current anchors first:
  `src/MLF/Frontend/Program/Builtins.hs`,
  `src/MLF/Backend/IR.hs`, `src/MLF/Backend/Convert.hs`, and
  `src/MLF/Backend/LLVM/Lower.hs`.
#### Completion Pointers: milestone-1

- round-238 created `MLF.Primitive.Inventory`, migrated the selected frontend/backend adapters to that private owner, kept lowerer wrapper/runtime implementation details local, and passed focused primitive checks plus the full cabal gate.
- round-239 moved LLVM lowering primitive-name/support coverage to MLF.Primitive.Inventory, kept lowerer wrapper/runtime details local, and passed focused primitive checks plus cabal build all && cabal test.

#### Candidate Direction: Primitive Inventory Module

- Direction id: `direction-1a-primitive-inventory-module`
- Summary: move primitive reserved names, source/backend typing, and native
  emission support policy behind one private owner Module.
- Why it matters now: the current Interface is effectively "all tables must
  agree", which makes primitive changes high-risk and difficult to test
  directly.
- Preconditions: inspect frontend builtins, backend IR validation, backend
  conversion, LLVM lowering, docs, and primitive-related tests at HEAD.
- Parallel hints: serial by default; docs-only inventory can run in
  `lane-docs` only if it does not modify source or tests.
- Boundary notes: keep the owner private. Do not add a broad public primitive
  facade, `BackendPrim`, second executable IR, or broad FFI lane.
- Extraction notes: a lawful first round may create the owner and migrate one
  adapter family, then leave further adapter migration to later rounds.

### [pending] 2. Backend Callable-Shape Module

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: deepen one private backend Module that owns callable-shape facts so
  validation, conversion, and lowering stop classifying callable heads through
  separate heuristics.
- Completion signal: `BackendApp` versus `BackendClosureCall` classification
  is owned by one backend callable-shape Module or explicit owner map; backend
  IR validation, conversion, and LLVM lowering consume that owner; focused
  backend tests cover direct/closure diagnostics and lowering behavior;
  required validation passes.
- Parallel lane: `lane-backend-runtime`
- Coordination notes: keep `MLF.Backend.IR` as the executable seam. This
  milestone deepens callable-shape ownership; it does not create another
  public backend IR.
#### Completion Pointers: milestone-2

none yet

#### Candidate Direction: Backend Callable-Shape Module

- Direction id: `direction-2a-backend-callable-shape-module`
- Summary: centralize callable-shape facts used by `MLF.Backend.IR`,
  `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower`.
- Why it matters now: closure/direct-call diagnostics and lowering become
  brittle when each backend phase repeats head classification.
- Preconditions: inspect `backendCallableHead` and validation use in
  `MLF.Backend.IR`, conversion callable classification in
  `MLF.Backend.Convert`, lowering callable checks in
  `MLF.Backend.LLVM.Lower`, and callable tests at HEAD.
- Parallel hints: serial; shared backend classification changes can affect
  multiple compilation paths.
- Boundary notes: callable facts should stay internal to backend/runtime
  surfaces unless a selected round proves a public need.
- Extraction notes: prefer one representative callable family or diagnostic
  path for the first implementation round if the blast radius is high.

### [pending] 3. Native Parity Policy Module

- Milestone id: `milestone-3`
- Depends on: `milestone-1`, `milestone-2`
- Intent: move backend/native/object-code parity policy next to
  `ProgramMatrix` so interpreter, LLVM, object, and native expectations share
  one test Interface instead of re-encoding support with manual string lists.
- Completion signal: `ProgramMatrix`, `ProgramSpec`, `BackendLLVMSpec`, frozen
  parity helpers, and LLVM tool support consume one parity policy owner or
  explicit owner map; skip/support policy is visible and tested; manual string
  list drift is removed or guarded; required validation passes.
- Parallel lane: `lane-backend-runtime`
- Coordination notes: coverage changes must classify support by layer: source
  checking, interpreter/runtime, backend/native, object-code, and tool
  availability. Do not hide backend limitations behind test convenience.
#### Completion Pointers: milestone-3

none yet

#### Candidate Direction: Native Parity Policy Module

- Direction id: `direction-3a-native-parity-policy-module`
- Summary: place backend/native/object-code parity policy beside
  `ProgramMatrix` and make backend tests consume it.
- Why it matters now: `ProgramMatrix` owns runtime rows while backend tests
  still duplicate coverage selection, so green tests can miss the exact native
  path a row claims to support.
- Preconditions: inspect `test/Parity/ProgramMatrix.hs`,
  `test/ProgramSpec.hs`, `test/BackendLLVMSpec.hs`, frozen parity helpers,
  `test/LLVMToolSupport.hs`, and native/tool availability checks at HEAD.
- Parallel hints: serial by default; a pure inventory of current policy may
  run in `lane-docs` if it does not change coverage.
- Boundary notes: keep low-level test policy in test-support seams rather than
  widening production facades.
- Extraction notes: a lawful first round may make support/skip policy visible
  and add guard tests before migrating all backend rows.

### [pending] 4. CLI Emission Preparation

- Milestone id: `milestone-4`
- Depends on: `milestone-1`, `milestone-2`, `milestone-3`
- Intent: move backend-emission preparation into a backend-owned Adapter so
  `MLF.Program.CLI` remains command/file orchestration instead of also owning
  parsing, Prelude injection, checking, Prelude pruning, and backend render
  preparation policy.
- Completion signal: backend emission preparation has a backend-owned Adapter;
  CLI delegates semantic preparation to that Adapter; tests can verify
  preparation without file IO where appropriate; CLI still owns command
  orchestration and file handling; required validation passes.
- Parallel lane: `lane-backend-runtime`
- Coordination notes: inspect `src/MLF/Program/CLI.hs`, `app/Main.hs`, and
  `test/BackendLLVMSpec.hs` before selecting a slice. Keep file IO tests only
  where command/file behavior is the actual subject.
#### Completion Pointers: milestone-4

none yet

#### Candidate Direction: CLI Emission Preparation Adapter

- Direction id: `direction-4a-cli-emission-preparation`
- Summary: extract backend-emission semantic preparation from CLI orchestration
  into a backend-owned Adapter.
- Why it matters now: backend emission policy changes should not require tests
  to drive file IO or CLI command plumbing when the behavior is semantic
  preparation.
- Preconditions: inspect CLI source loading, Prelude retention pruning,
  checked-program conversion, backend rendering paths, `app/Main.hs`, and
  backend emission tests at HEAD.
- Parallel hints: serial; CLI, backend conversion, and tests share execution
  paths.
- Boundary notes: do not create a public lowering API solely for tests. Use a
  private backend Adapter and test-support seams where needed.
- Extraction notes: a lawful first round may extract a pure preparation helper
  and migrate one emission path or test surface.

### [pending] 5. Prepared Generalization Artifact Depth

- Milestone id: `milestone-5`
- Depends on: `milestone-4`
- Intent: deepen **Prepared Generalization Artifact** so callers consume
  capability Adapters for elaboration, root generalization, and result-type
  reconstruction instead of reaching into a raw prep bag with many exposed
  fields.
- Completion signal: `MLF.Elab.Run.Generalize.Prepare` exposes cohesive
  capability Adapters or owner APIs; `MLF.Elab.Run.Pipeline` and related
  consumers no longer need raw knowledge of redirect maps, copy maps,
  canonical maps, scope selection, and result-type reconstruction inputs;
  focused generalization/elaboration tests cover the selected behavior;
  required validation passes.
- Parallel lane: `lane-elaboration`
- Coordination notes: use the domain phrase **Prepared Generalization
  Artifact**. The artifact should remain the shared output of **Generalization
  Preparation** and the input consumed by elaboration, result-type
  reconstruction, and root-scheme generalization.
#### Completion Pointers: milestone-5

none yet

#### Candidate Direction: Prepared Generalization Artifact Depth

- Direction id: `direction-5a-prepared-generalization-artifact-depth`
- Summary: replace raw-field coupling around the prepared artifact with
  capability Adapters for the known consumers.
- Why it matters now: `Pipeline` should ask **Generalization Preparation** for
  a **Prepared Generalization Artifact** instead of understanding individual
  preparation maps and scope plumbing.
- Preconditions: inspect `MLF.Elab.Run.Generalize.Prepare`,
  `MLF.Elab.Run.Pipeline`, result-type reconstruction, root generalization,
  and focused generalization/elaboration tests at HEAD.
- Parallel hints: serial; elaboration signatures are shared and can churn
  across many callers.
- Boundary notes: use test-support seams for low-level fixture setup instead
  of widening production facades.
- Extraction notes: one lawful first round is an ownership audit plus one
  narrow capability Adapter migration.

### [pending] 6. Snapshot Finalization Seam

- Milestone id: `milestone-6`
- Depends on: `milestone-5`
- Intent: keep `MLF.Constraint.Finalize` as the production **Snapshot
  Finalization** seam while moving stepwise helpers and solved-to-view
  adapters behind internal or test-support Modules.
- Completion signal: `MLF.Constraint.Finalize` exposes the production
  construction seam for finalized `PresolutionView` artifacts and solved
  handles; stepwise helpers and solved-to-view adapter mechanics are internal
  or test-support only; `MLF.Constraint.Presolution.View` stays the read-model
  surface; focused presolution/reify/result-type tests cover the selected
  behavior; required validation passes.
- Parallel lane: `lane-elaboration`
- Coordination notes: this milestone is not Legacy Surface Retirement
  rediscovery. Treat the accepted ADR as binding: stale compatibility adapters
  should be deleted rather than moved to a new compatibility home unless a
  selected round proves a thesis-backed temporary seam.
#### Completion Pointers: milestone-6

none yet

#### Candidate Direction: Snapshot Finalization Seam

- Direction id: `direction-6a-snapshot-finalization-seam`
- Summary: tighten the production `MLF.Constraint.Finalize` Interface and move
  internal/testing mechanics behind owner-local seams.
- Why it matters now: finalization entrypoints, step helpers, and solved/view
  conversion sit close together, which makes canonical-map and snapshot
  changes harder to localize.
- Preconditions: inspect `MLF.Constraint.Finalize`,
  `MLF.Constraint.Presolution.View`, `MLF.Constraint.Solved.Internal`,
  solved/test-support modules, reify tests, and Snapshot Finalization guard
  tests at HEAD.
- Parallel hints: serial; this touches constraint finalization, presolution
  read-model access, solved internals, and tests.
- Boundary notes: keep remaining raw-view or solved/view bridges private,
  owner-local, and named for the concrete boundary they serve. Do not add broad
  production adapters.
- Extraction notes: prefer one production entrypoint or one test-support
  helper migration for the first implementation round if the full seam cleanup
  is too large.
