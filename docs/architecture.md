# Architecture / Repo Layout

This repository implements the MLF → xMLF pipeline described in `papers/these-finale-english.txt` (see also `papers/xmlf.txt`).
Goal: keep the implementation paper-faithful to the thesis and document any deviations; use `papers/xmlf.txt` only as supplementary detail when the thesis is silent.

## Public API (downstream users)

Downstream code should import:

- `MLF.API` — umbrella frontend module (surface syntax + eMLF / `.mlfp` parse/pretty + normalization helpers)
- `MLF.Pipeline` — canonical pipeline/runtime module (e.g. `inferConstraintGraph`, `runPipelineElab`, `runPipelineElabWithConfig`, `typeCheck`, `step`, `normalize`, `.mlfp` elaboration/checking/runtime)
- `MLF.XMLF` — explicit xMLF syntax, parser, and pretty-printing helpers

Public modules live under `src-public/` and the public Cabal library only exposes:

- `src-public/MLF/API.hs`
- `src-public/MLF/Pipeline.hs`
- `src-public/MLF/XMLF.hs`

Active implementation planning lives under `tasks/todo/YYYY-MM-DD-description/`.
Root-level `task_plan.md`, `findings.md`, and `progress.md` are historical
artifacts and are not part of the current task workflow.

## Repo layout

- `src/` builds the private implementation library `mlf2-internal`.
- `src-public/` contains the public entry modules `MLF.API`, `MLF.Pipeline`, and `MLF.XMLF`.
- `app/` contains the `mlf2` executable entrypoint.
- `test/` contains the Hspec suite, the manual test runner, frozen parity tooling/artifacts, and test-support parity owners such as `Parity.ProgramMatrix` plus `Parity.ProgramMatrix.NativePolicy`.
- `papers/` contains the thesis/reference texts used for paper-faithful implementation work.
Historical executable research harnesses have been retired from the active build. Their accepted evidence remains under `docs/plans/` and `orchestrator/rounds/`; do not reintroduce a research sublibrary or CLI entrypoint without a fresh reviewed roadmap.

## Internal implementation (package-private)

All implementation modules live under `src/` and are built as a private sublibrary:

- Cabal sublibrary: `library mlf2-internal` with `visibility: private`

The code is organized by domain (not by phase) under `src/MLF/`:

- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Frontend.Syntax.Program` / `MLF.Frontend.Parse.Program` / `MLF.Frontend.Pretty.Program` — canonical `.mlfp` syntax ownership under the main frontend boundary
- `MLF.Frontend.Program.Package` — private owner for `.mlfp` package identity, module identity, source-unit shape, trivial-package adapters, one-root filesystem discovery, explicit module graph/order validation, and package-to-program projections used while the checker still consumes the existing in-memory program artifact
- `MLF.Frontend.Program.Resolve` — assigns `.mlfp` symbol identities and produces the resolved semantic program artifact consumed by the checker
- `MLF.Frontend.Program.Check` — module/import/class/data environment assembly for `.mlfp`, including static validation that may fail before the eMLF pipeline
- `MLF.Frontend.Program.Elaborate` — lowers executable `.mlfp` bindings to surface eMLF `SurfaceExpr`
- `MLF.Frontend.Program.Finalize` — normalizes lowered surface eMLF, calls the internal detailed eMLF pipeline entrypoints with program-owned external binding modes, resolves `.mlfp` deferred obligations, and accepts rewritten terms only after the xMLF typecheck guard
- `MLF.Frontend.Program.Prelude` — built-in source-level `.mlfp` Prelude used by the CLI/file runner as an explicit import target
- `MLF.Frontend.Program.Run` — runtime entrypoint that evaluates pure checked `.mlfp` bindings through the existing xMLF runtime, executes checked `main : IO Unit` actions through the reserved IO primitive boundary, and renders recovered closed ADT values with source constructor syntax
- `MLF.Primitive.Inventory` — private owner for builtin type names/kinds, opaque builtin metadata, shared source/backend primitive signatures, and native support classification for lowerable reserved primitives. `MLF.Frontend.Program.Builtins`, `MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower` adapt this inventory; LLVM lowering still owns wrapper/runtime implementation details downstream.
- `MLF.Backend.CallableShape` — private owner for direct-vs-closure callable-head classification shared by `MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower`; it centralizes callable-head policy without creating a second executable backend IR surface
- `MLF.Backend.IR` — typed backend IR boundary for checked `.mlfp` programs, before LLVM lowering
- `MLF.Backend.Convert` — checked `.mlfp` program to typed backend IR conversion, including backend type conversion, explicit ADT construct/case recovery, and closure conversion where the checked xMLF shape is unambiguous
- `MLF.Backend.Emission.Prepare` — private adapter for backend-emission semantic preparation from a caller-provided source string: parsing with a display path, Prelude injection, checking, and backend-owned Prelude retention pruning before LLVM rendering
- `MLF.Backend.LLVM` — repo-local LLVM backend facade over a small typed LLVM AST, lowerer, and pretty-printer for the supported typed backend IR subset, with explicit diagnostics for unsupported backend nodes
- Structural recursive ADT matching currently remains adapter-local in `MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower`. The accepted target is one private backend matcher module named `MLF.Backend.StructuralRecursiveData`, but that module is not yet present in the current codebase.
- `MLF.Constraint.*` — constraint graph types + normalize + acyclicity + presolution + solve
- `MLF.Binding.*` — binding tree queries + executable χe ops + harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, plus xMLF typechecking/reduction)
- `MLF.Elab.TypeCheck` — the single typing judgment owner for elaborated `.mlfp` / xMLF terms
- `MLF.XMLF.*` — explicit xMLF syntax and related helpers
- `MLF.Reify.*` — graph-to-type reification and related type operations
- `MLF.Types.*` — elaborated/runtime term and type representations
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

The `.mlfp` package/module owner is `MLF.Frontend.Program.Package`. Current
one-file `.mlfp` inputs are represented as trivial package source units, then
projected into the existing in-memory `Program` artifact at the checker/resolver
edge. The package owner can discover `.mlfp` files under one explicit local
root, retain source paths, build a module-to-file graph, reject duplicate,
missing, or cyclic module imports, and project modules in dependency order so
imports are checked before importers. `Resolve` and `Check` still consume the
ordered in-memory package projection, `Finalize` consumes the assembled program
scope, and `Run` evaluates all checked module bindings together. There is no
package root/search path policy beyond that one explicit root, persisted
interface artifact, cache/build graph policy, stable `.mlfp` ABI, linker, or CLI
package mode today. Future separate compilation should be introduced only
through the package owner and an explicit compiler-owned interface artifact
carrying exported principal schemes, visible type/class/instance summaries, and
checked xMLF/runtime payloads rather than by peeking at source outside the
package projection.

No active executable or test component depends on historical research modules.

### `.mlfp` resolved-symbol boundary

`MLF.Frontend.Program.Resolve` owns semantic identity for `.mlfp` global names.
Parsed program syntax is `Program 'Parsed`; the resolver produces a
`ResolvedSemanticProgramArtifact` whose modules group `Program 'Resolved` syntax,
local semantic symbols, full visible scope, and exports as one checker input.
`ResolvedModule` keeps that semantic artifact plus diagnostic adapters such as
reference lists. Resolved syntax stores semantic symbols at global reference
sites, including value, constructor, type, class, method, import/export, and
source-type heads. Local term binders remain local names and resolved term
references distinguish `ResolvedLocalValue` from `ResolvedGlobalValue`.

A `SymbolIdentity` records namespace, defining module/name, and constructor or
method owner identity; `SymbolSpelling` records the source spelling that reached
that identity. `MLF.Frontend.Program.Check` consumes the semantic artifact, and
`MLF.Frontend.Program.Elaborate` compiles resolved expressions and patterns
through identity indexes beside its visible string maps. Surface-spelling maps
and reference-list adapters are for lookup, diagnostics, source rendering,
audits, and runtime-name construction;
semantic decisions compare stored identities or identity-aware source-type
shapes, not qualified strings. Deferred method finalization carries paired
display/identity type views so instance and evidence lookup stay semantic even
after eMLF type recovery.

## Key graph and witness types

- `Expr` (`MLF.Frontend.Syntax`) — surface eMLF terms
- `Constraint` (`MLF.Constraint.Types.Graph`) — constraint graph plus binding tree
- `TyNode` — graph nodes (`TyVar`, `TyArrow`, `TyForall`, `TyBase`, `TyCon`, `TyExp`, `TyMu`, `TyBottom`)
- `InstEdge` — instantiation edges (`<=`)
- `BindParents` — child-to-parent binding tree map with `BindFlag`
- `Expansion` — presolution recipes (identity, forall-intro, instantiation, composition)
- `EdgeWitness` — per-edge xMLF instantiation reconstruction metadata

## Type-level safety invariants

The codebase uses GADTs, DataKinds, and the `singletons` library to encode
runtime invariants as compile-time types:

- **Typed node-reference seam** (`MLF.Constraint.Types.Graph`): the current mixed
  binding-tree key is the unindexed `NodeRef` (`TypeRef NodeId | GenRef
  GenNodeId`). Newer type-safe helpers use the `NodeRefTag (t :: RefTag)` GADT
  with `TypeRefTag :: NodeId -> NodeRefTag 'TypeTag` and `GenRefTag ::
  GenNodeId -> NodeRefTag 'GenTag`, plus `SomeNodeRef` for existential mixed
  contexts. Code that needs compile-time separation should use `NodeRefTag`;
  legacy mixed maps still store `NodeRef`.

- **Phase-indexed `Constraint`** (`MLF.Constraint.Types.Graph`): `Constraint` is
  parameterized by a phantom `Phase` (`'Raw | 'Normalized | 'Acyclic | 'Presolved | 'Solved`).
  The dedicated singleton owner is `MLF.Constraint.Types.Phase.Singletons`,
  and `MLF.Constraint.Types.Phase` re-exports that singleton surface together
  with the `Next` transition family. The main phase entrypoints now make the
  phase progression explicit: normalization returns `Constraint 'Normalized`,
  acyclicity checking returns `Constraint 'Acyclic`, presolution returns
  `Constraint 'Presolved`, and solve consumes presolved constraints to produce
  the opaque `Solved` abstraction. Directional transition helpers in
  `MLF.Constraint.Types.Graph` encode those boundaries. The old generic
  graph-level phase escape hatches have been retired; any remaining phase
  erasure is owner-local and named for the backend that still stores raw
  constraints internally, such as solved construction, presolution in-progress
  state, research, or test support. Broad raw-view adapters are retired rather
  than preserved as public
  read-model surfaces.

- **`ForallSpec`** (`MLF.Constraint.Types.Witness`): `fsBinderCount` was removed;
  binder count is derived from `length fsBounds`. `mkForallSpec` validates
  non-empty bounds.

- **Witness construction boundary** (`MLF.Constraint.Types.Witness`):
  the default production surface exports abstract `EdgeWitness` /
  `InstanceWitness` types plus smart constructors and read-side accessors. Raw
  witness constructors live only behind
  `MLF.Constraint.Types.Witness.TestSupport` for deliberate test fixtures.
  `mkEdgeWitness` currently enforces the construction-time invariant the live
  production path can prove directly (non-negative intro counts). Finalized
  `InstanceWitness` values now require a normalization-owned validated-ops
  token before `mkInstanceWitness` can mint them, while
  `mkUncheckedInstanceWitness` stays on the explicit owner-local
  pre-normalization seam. Context-heavy Ω
  normalization and Φ translation checks that are not subsumed by that token
  remain downstream owners.

## Shared ownership notes

- Core graph node/edge/binding identifiers and types live in `MLF.Constraint.Types.Graph`; witness metadata lives in `MLF.Constraint.Types.Witness`; presolution-only state/types live in `MLF.Constraint.Types.Presolution`.
- Shared unification flow lives in `MLF.Constraint.Unify.Core`; shared structural decomposition lives in `MLF.Constraint.Unify.Decompose`.
- Prefer `MLF.Constraint.Canonicalizer` for redirect + union-find canonicalization instead of ad hoc chase helpers.
- Parser aliases, parser support for non-canonical legacy syntax, and stale frontend AST compatibility spellings are compatibility surfaces, not protected architecture. The old `MLF.Elab.Legacy` expansion-conversion module has been retired; the remaining expansion-argument translation helper is owned by annotation elaboration because that is the only live consumer. Frame cleanup that spans these surfaces as Legacy Surface Retirement; Snapshot Finalization is only the read-model/finalized-snapshot sub-slice. Retire reached compatibility paths consistently across frontend eMLF and explicit xMLF parsers unless the thesis requires the older spelling or adapter; see `docs/adr/2026-05-14-legacy-surface-retirement.md`.
- `MLF.Pipeline` exposes only the canonical public elaboration entrypoints (`runPipelineElab`, `runPipelineElabWithConfig`). Internal checker-authoritative aliases have also been retired; parity probes call the canonical entrypoint directly. The detailed unchecked elaboration path remains separate only because it exposes distinct `.mlfp` finalization behavior rather than an alias for the checked pipeline.
- The old public program re-export shim has been retired. `.mlfp` parsing and pretty-printing are owned by `MLF.API`; `.mlfp` checking and runtime are owned by `MLF.Pipeline`.
- Presolution state access should go through `MonadPresolution` plus `MLF.Constraint.Presolution.Ops` and `StateAccess`; edge processing is split across planner/interpreter passes with typed `EdgePlan`.
- Elaboration entrypoints bundle inputs as `ElabConfig`/`ElabEnv`, and tracing is explicit via `TraceConfig`.
- `MLF.Elab.Run.Generalize.Prepare` owns the elaboration-side Generalization
  Preparation step. Its normal API exposes the abstract
  `PreparedGeneralizationArtifact` plus owner operations for prepared
  annotation, elaboration inputs, root-scheme generalization, and result-type
  reconstruction, while keeping redirect/canonicalization, copy-node recovery,
  scope overrides, and the owner-local base-constraint projection on
  `GaBindParents.gaBaseConstraint` out of `MLF.Elab.Run.Pipeline`.
- `MLF.Elab.Run.ResultType.View` owns result-type reconstruction's query
  adapter over the prepared result-type input: bound overlays, no-fallback
  reification, base-target projection into `GaBindParents.gaBaseConstraint`,
  scope/target queries, and target generalization. `ResultType.Fallback.*`
  should select policy paths through this adapter rather than patching
  `PresolutionView` records or rebuilding base-map inputs locally.

## Typed backend IR and lowering boundary

The current checked-program backend path is:

1. `MLF.Frontend.Program.Resolve` assigns resolved symbol identities.
2. `MLF.Frontend.Program.Check` and `MLF.Frontend.Program.Finalize` accept the
   `.mlfp` program only after the existing eMLF/xMLF checker boundary and xMLF
   typecheck guard have succeeded.
3. `MLF.Backend.Convert` converts the resulting checked program artifacts into
   the typed backend IR in `MLF.Backend.IR`.
4. `MLF.Backend.LLVM` consumes that typed backend IR, lowers the supported
   subset into a repo-local LLVM AST, and pretty-prints deterministic LLVM
   `.ll` text.

`MLF.Backend.IR` is therefore the first backend-owned representation after a
`.mlfp` program has already passed the existing checker and xMLF typecheck
guard. `MLF.Backend.Convert` is the only conversion boundary from checked
program artifacts into that IR. It is not a second inference or typing
authority: if the checked artifact cannot be represented faithfully, conversion
must report an unsupported checked shape instead of inventing frontend
semantics or repairing types.

The current one-backend-IR contract is:

- xMLF remains the thesis-faithful typed elaboration IR.
- `MLF.Backend.IR` is the single executable eager backend IR in the current
  repo architecture.
- `MLF.Backend.Convert` is the only checked-program to backend-IR conversion
  boundary.
- Any ANF-like normalization, layout-only structure, or lowerability-only
  representation stays private to backend-owned lowering helpers rather than
  becoming a second executable IR, a public `LowerableBackend.IR`, or a second
  checked-program authority.

Within that single backend IR, `MLF.Backend.IR` owns the eager executable
representation consumed by the rest of the backend. The owned executable term
shapes are typed direct application, explicit closures and
`BackendClosureCall`, ADT construction and case analysis, lets, lambdas, type
abstraction/application, and recursive roll/unroll. The validation-visible
invariants for those executable shapes live at this boundary so conversion and
lowering share one executable contract.

That callable contract is explicit. `BackendApp` is the direct first-order
call node, so local direct aliases that remain first-order stay on this path.
`BackendClosureCall` is the indirect closure-call node, so closure-valued
aliases, captured closures, constructor-field projections, and case/let-
selected closure values stay on this explicit path instead of relying on
lowerer recovery.
`MLF.Backend.CallableShape` is the private owner for the shared callable-head
classifier that enforces this split; `MLF.Backend.IR` stays the executable IR
seam, while conversion and lowering consume the same owner instead of keeping
separate direct-vs-closure heuristics.

The ADT/case ownership split is explicit. Row-4 ADT/case ownership means
semantic constructor/case nodes stay in `MLF.Backend.IR`:
`BackendData`, `BackendConstructor`, `BackendConstruct`, and `BackendCase`
preserve constructor metadata, constructor use, and case alternatives only.
Runtime tags, field slots, closure-record storage for function-like fields,
and nullary tag-only representation stay private to LLVM/native lowering.
Checked-program conversion must not assign runtime tag values, field offsets,
boxing/storage policy, or layout-only witnesses.

Structural recursive ADT identity and payload-shape matching is also a private
backend-owned concern, but the current implementation has not yet centralized it.
Today the matching logic is still split across backend IR validation,
checked-program conversion, and LLVM lowering, with adapter-local helpers for
structural `BTMu`/nominal data comparisons. The accepted target is to move that
logic behind `MLF.Backend.StructuralRecursiveData`, where source-local recovery
and representation normalization remain conversion concerns and the matcher
returns derived match/mismatch evidence for the current adapter operation. Until
that module exists, do not treat it as live architecture; treat the ADR as the
implementation handoff and keep any cleanup aligned with
`docs/adr/2026-05-14-backend-structural-recursive-data-matching.md`.

The row-5 primitive/eager contract is explicit as well. The current primitive
surface is the inventory-owned reserved runtime-binding set in
`MLF.Primitive.Inventory`: `__mlfp_and` plus the IO primitive names classified
there for native support. Checked-program conversion and lowering keep those
primitives on the existing
`BackendVar`, `BackendApp`, and `BackendTyApp` surface, with no new `BackendPrim`,
no second executable IR, no public lowering API, and no broad FFI lane.
The shared builtin-type and primitive-signature inventory for this path lives
in `MLF.Primitive.Inventory`; `MLF.Frontend.Program.Builtins`,
`MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower` adapt
that owner, while `MLF.Backend.LLVM.Lower` still owns downstream runtime
wrapper bodies, C runtime symbol names, closure layout, and eager sequencing
implementation details.

The eager sequencing contract is reviewable at that same boundary:

- let RHS before body;
- case scrutinee before branch selection;
- direct/primitive call arguments in written order; and
- effect sequencing remains explicit through `__io_bind`.

Unsupported broader primitive or ordering-sensitive shapes stay on explicit
backend diagnostic paths instead of falling through to a fallback runtime
path.

The row-6 polymorphism/lowerability contract is explicit too:

- checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`.
- LLVM/native lowering owns only the specialization-based lowerable subset.
- Complete type applications may specialize privately inside the lowerer.
- Residual runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary.

LLVM/native lowering owns only downstream private lowering/runtime details for
that same `MLF.Backend.IR` program: closure-record layout and closure ABI
details, environment-record layout, layout-only lowering helpers, native
wrapper/runtime symbol emission, and executable rendering support. Those
concerns do not create a second executable IR, and they do not move executable
ownership out of `MLF.Backend.IR`.

Lazy STG-like machinery stays out of scope for the current backend boundary:
no thunks, no update frames, no CAF update semantics, no graph reduction, and
no implicit laziness rescue.

A later lower IR may be introduced only when all of the following hold:

- distinct backend-owned executable invariants that cannot live in
  `MLF.Backend.IR` or a private lowering helper;
- a dedicated validation/evidence owner for that new boundary; and
- a later accepted roadmap revision before any new durable or public surface
  is added.

The boundary invariants are:

- every backend expression node carries its result `BackendType`;
- module-level binding names are runtime names and must be globally unique in a
  `BackendProgram`;
- `backendProgramMain` must name one of those bindings;
- binding declarations must match the type carried by their expression body;
- variable references must resolve either to lexical binders introduced by
  lambda/let/case patterns or to globally unique program bindings, and the
  carried variable type must match that binding;
- `BackendApp` heads must stay on the direct-call path, while malformed direct
  calls on closure-valued heads fail with explicit backend callable
  diagnostics;
- lambda, application, let, type abstraction/application, recursive roll, and
  recursive unroll nodes satisfy local type equalities checked by
  `validateBackendProgram`;
- ADT construction and case analysis are explicit backend nodes checked against
  program constructor metadata for known constructors, constructor arity,
  constructor-local `forall` bounds, argument/result types, case scrutinee
  type, and alternative result type.
- closure construction and indirect closure calls are explicit backend nodes:
  closure entry names must be unique, capture and value-parameter binders must
  be locally unique, capture expressions must match their declared capture
  types, the closure body must match the declared function result after value
  parameters are applied, `BackendClosureCall` heads must remain closure
  values rather than direct callables, and `BackendClosureCall` arguments must
  match the function type carried by the closure expression.

This module intentionally lives in the private `mlf2-internal` library for now.
Conversion and lowering modules should depend on this IR rather than reaching
back into `MLF.Frontend.Program.*` internals for backend decisions.
`MLF.Backend.LLVM` preserves that boundary by validating the IR before
lowering, rendering the supported first-order subset plus explicit closure IR,
and producing explicit unsupported-node diagnostics for backend constructs that
do not yet have LLVM lowering. The LLVM backend is intentionally repo-local:
`Syntax` models the small LLVM surface used by mlf2, `Lower` maps backend IR
into that AST, and `Ppr` emits opaque-pointer LLVM IR text accepted by LLVM 15+
tools or LLVM 14-era tools run with `-opaque-pointers`.

The backend has two emission contracts. Both raw and native emission still
consume the same backend IR program. Raw emission keeps the checked `.mlfp`
`main` as an ordinary module-qualified LLVM function and is the stable IR
inspection surface. Native emission adds a C ABI `i32 @main()` wrapper around a
zero-argument checked `.mlfp` `main`, renders supported pure `Int`, `Bool`, and
first-order ADT results to stdout using the same value text as `ProgramSpec`,
prints one trailing newline, writes no stderr on success, and returns process
exit status `0`. Native emission declares libc `malloc`/`printf` and emits
backend-owned runtime definitions such as `__mlfp_and` when those names are not
program bindings. Unsupported result types fail before native-run assertions,
so the native process boundary does not invent source or IO semantics.
`docs/backend-native-pipeline.md` records the linked-executable test pipeline,
toolchain discovery, generated artifacts, runtime support, and row coverage
classification. `Parity.ProgramMatrix.NativePolicy` owns the test-support
ProgramSpec-to-LLVM native/object-code row policy consumed by
`BackendLLVMSpec`; it classifies interpreter-success rows by source checking,
interpreter/runtime expectation, backend LLVM assembly, object-code smoke,
native executable run, and required LLVM/native tools without widening
production backend APIs.

`MLF.Backend.Emission.Prepare` owns the shared semantic preparation step for
raw and native backend emission: parsing a provided source string with the
caller's display path, injecting the source-level Prelude, checking the
program, and pruning the checked Prelude module to the binding and data
dependency closure required by backend rendering. The CLI adapter in
`src/MLF/Program/CLI.hs` remains the file and command owner for `emit-backend`
and `emit-native`: it reads the requested file, delegates semantic preparation
to the backend adapter, renders through `MLF.Backend.LLVM`, and presents
user-facing errors. Runtime execution through `run-program` stays in that CLI
adapter and `MLF.Frontend.Program.Run`.

The explicit closure ABI is private to the backend IR-to-LLVM path. A closure
value is a heap pointer to a two-word record containing a code pointer and an
environment pointer or null. Non-empty environments are heap records with one
machine word per captured runtime value. Closure entry functions are private
LLVM functions named by the backend IR and take a hidden `ptr env` argument
before erased monomorphic value parameters. Direct first-order calls still use
the existing direct-call path; closure-valued aliases, captured closures, and
case/let-selected closure values must be represented with
`BackendClosureCall`.

Checked-program conversion now closure-converts ordinary monomorphic escaping
source lambdas, returned local function values, closure-valued let aliases,
partial applications that produce function values, indirect calls through
monomorphic function-valued constructor fields, indirect calls through those
closure values, aliases, or projected fields, and monomorphic recursive
higher-order top-level or closed local helper flows whose function-valued
arguments fit the explicit closure ABI. Direct first-order local calls remain
direct backend applications. Polymorphic runtime function values,
type-parameter-headed higher-order constructor fields, recursive local helpers
that capture ordinary lexical values, and final executable linking remain
future extension points. Those diagnostics do not weaken source inference,
checking, module visibility, or runtime semantics; they only describe the
current IR-to-LLVM lowering surface.

## `Solved` boundary and thesis-exact cleanup rule

`MLF.Constraint.Solved` should not be kept or removed for its own sake. The thesis-exact rule is:

- keep exactly the semantic boundary that the thesis needs;
- move out compatibility/convenience glue that the thesis does not require;
- remove `Solved` entirely only if its remaining semantics are represented elsewhere just as explicitly.

In the current codebase, `PresolutionView` is the primary read-only runtime/internal API, while `Solved` still carries some finalized-snapshot and original-vs-canonical semantics that are not yet pure glue. Pass-through read-model adapters are not protected boundaries; the old `MLF.Elab.Run.ChiQuery` facade has been retired in favor of direct `PresolutionView` access.

### Current classification of the `Solved` ecosystem

| Current home | Contents | Rule |
|---|---|---|
| Public `MLF.Constraint.Solved` facade | `Solved`, `fromSolveOutput`, `canonical`, `canonicalMap`, `originalConstraint`, `canonicalConstraint`, `validateCanonicalGraphStrict` | This is the current production surface. It exists to preserve replay-faithful solved construction, explicit original ↔ canonical correspondence, and strict post-finalization validation. Do not add convenience read queries back to this facade. |
| `MLF.Constraint.Finalize` | `presolutionViewFromSnapshot`, `finalizePresolutionViewFromSnapshot`, `validateCanonicalSnapshotStrict`, `finalizeSolvedFromSnapshot`, `finalizeSolvedForConstraint` | This is the production construction authority for Snapshot Finalization. It may use owner-local solved/finalize internals, but callers should ask `Finalize` for finalized views or solved handles instead of assembling them through compatibility adapters. Stepwise mechanics and solved-to-view fixtures are not part of this facade. |
| `MLF.Constraint.Finalize.Internal` | Owner-local snapshot sanitizing, canonicalization, solved bind-parent pruning, strict solved validation, and solved-to-view record construction used by `Finalize` and Finalize test support | Private Snapshot Finalization mechanics. Import is allowed only to `MLF.Constraint.Finalize` and `MLF.Constraint.Finalize.TestSupport`. |
| `MLF.Constraint.Solved.Internal` | Owner-local constructors and helpers such as `fromConstraintAndUf`, `fromPreRewriteState`, `rebuildWithConstraint`, `pruneBindParentsSolved`, and internal read/query utilities | Internal implementation detail for solved construction and finalization. Import is allowed only to the solved facade, solved test support, Snapshot Finalization internals, or modules under `MLF.Constraint.Solved.*`. |
| `MLF.Constraint.Finalize.TestSupport`, `MLF.Constraint.Solved.TestSupport`, and `test/SolvedFacadeTestUtil.hs` | Finalize fixture helpers such as solved-to-view construction and bind-parent pruning; low-level solved fixture constructor `mkTestSolved`; snapshot fixture construction; and audit-only original-domain helpers such as `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement` | Test and audit support only. These helpers must not widen the production `Finalize` or `Solved` facades. |
| Retired / guarded absent | `fromSolved`, `toRawPresolutionViewForLegacy`, reify-local `solvedFromView`, `Finalize.stepSolvedFromPresolutionView`, public `lookupNode`/`lookupBindParent`/`bindParents`/`lookupVarBound`/`genNodes`, public enumeration helpers, raw canonical container accessors, dead mutation hooks, and broad solved/view adapters | Keep absent. Current guard tests assert these do not reappear on the public facade or production adapter path. |

### Practical consequence

The previous table-driven facade cleanup is complete. The current rule is no
longer “move these things out now”; it is “keep the facade narrow and route new
work through the current owners”:

- use `PresolutionView` for read-only elaboration/reification access;
- use `MLF.Constraint.Finalize` for Snapshot Finalization construction;
- use `MLF.Constraint.Solved` only for solved-handle construction from solve
  output, original/canonical correspondence, and strict solved validation;
- keep low-level fixture construction behind `MLF.Constraint.Finalize.TestSupport`,
  `MLF.Constraint.Solved.TestSupport`, or test-local helpers.

The project goal is **not** “delete `Solved` no matter what”; it is “keep the
remaining solved boundary only while it carries thesis-relevant finalized-snapshot
semantics that are not represented elsewhere just as explicitly.”

## Fallback Policy

- Elaborative/runtime fallback ladders are now removed: the production path prefers explicit witness/scheme authority and surfaces structured errors when that authority is insufficient.
- The remaining `MLF.Elab.Run.ResultType.Fallback.*` modules are bounded
  result-type reconstruction policy logic over `ResultTypeView` queries and
  `EdgeTrace`, not a compatibility ladder back to legacy solved/view adapters or
  fallback-local `PresolutionView` record surgery.
- Planner scheme ownership for synthesized wrappers is body-root only; there is no wrapper-root recovery path.
- Instantiation inference keeps only structurally justified argument recovery.

## Witness Representation (Φ/Σ)

- `EdgeWitness.ewForallIntros` stores the number of quantifier introductions
  needed for the O phase, and `EdgeWitness.ewWitness` stores the Ω-only instance
  operations (`OpGraft`, `OpMerge`, `OpRaise`, `OpWeaken`, `OpRaiseMerge`).
- `EdgeTrace` is the per-instantiation-edge provenance record consumed by Φ. It
  tracks the expansion root, binder→argument pairs, exact interior `I(r)`,
  replay contract, replay-domain binder map, replay-domain binders, and copy-map
  provenance. Witness operation node IDs, `etBinderArgs`, `etInterior`, and
  `etCopyMap` keys live in one source-ID domain; canonical IDs are derived at
  lookup sites rather than globally rewriting trace provenance.

## Executable

- `app/Main.hs` builds the `mlf2` binary (demo runner).

## Tests

The test suite depends on both:

- `mlf2` (public library) and
- `mlf2:mlf2-internal` (private internal library)

This keeps the downstream surface small while still allowing specs to import internal modules.

## 2026-03-08 fallback-removal architecture note

The active elaboration path is now intentionally fail-fast around the old fallback seams:

- generalization does not retry through GA-disabled or raw reify ladders;
- let elaboration no longer chooses among RHS-/env-derived alternate schemes; the authoritative generalization result is the only live let scheme source;
- `MLF.Elab.Run.Generalize` no longer passes a recursive generalization callback into `applyGeneralizePlan`, and `MLF.Elab.Generalize` now uses the existing structural scheme plan instead of recursively generalizing another scope;
- planner scheme-owner resolution is body-root authoritative;
- instantiation inference is structural and prefix-based rather than catch-all heuristic;
- `reifyInst` is witness/domain-only apart from exact source-scheme reuse for already-authoritative annotations; the live authority set is `ewLeft`/`ewRight`, `etBinderArgs`, and copied witness-domain nodes from `etCopyMap`, and if only expansion-derived recovery would make an application/annotation succeed, the pipeline now fails fast.

This keeps the runtime path closer to the thesis boundary: if witness/scheme information is insufficient, the code now fails explicitly instead of silently switching to a weaker reconstruction mode.
