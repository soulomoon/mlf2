# Architecture / Repo Layout

This repository implements the MLF → xMLF pipeline described in `papers/these-finale-english.txt` (see also `papers/xmlf.txt`).
Goal: keep the implementation paper-faithful to the thesis and document any deviations; use `papers/xmlf.txt` only as supplementary detail when the thesis is silent.

## Public API (downstream users)

Downstream code should import:

- `MLF.API` — umbrella frontend module (surface syntax + eMLF / `.mlfp` parse/pretty + normalization helpers)
- `MLF.Pipeline` — canonical pipeline/runtime module (e.g. `inferConstraintGraph`, `runPipelineElab`, `runPipelineElabWithConfig`, `typeCheck`, `step`, `normalize`, `.mlfp` package discovery/checking/runtime)
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
- `test/` contains the Hspec suite, the manual test runner, frozen parity tooling/artifacts, static `.mlfp` file/root/search-path package fixtures under `test/programs/`, and test-support parity owners such as `Parity.ProgramMatrix` plus `Parity.ProgramMatrix.NativePolicy`.
- `papers/` contains the thesis/reference texts used for paper-faithful implementation work.
Historical executable research harnesses have been retired from the active build. Their accepted evidence remains under `docs/plans/` and `orchestrator/rounds/`; do not reintroduce a research sublibrary or CLI entrypoint without a fresh reviewed roadmap.

## Internal implementation (package-private)

All implementation modules live under `src/` and are built as a private sublibrary:

- Cabal sublibrary: `library mlf2-internal` with `visibility: private`

The code is organized by domain (not by phase) under `src/MLF/`:

- `MLF.Frontend.*` — surface syntax, desugaring, constraint generation
- `MLF.Frontend.TypeLevel` — internal owner for the richer pre-core type-level
  AST and normalization rules: kind-variable-bearing type forms,
  capture-avoiding `Λ` beta reduction, closed type-family ordered reduction,
  stuck-family rejection, cycle detection, and fuel diagnostics before erasure
  into the thesis-facing MLF core
- `MLF.Frontend.Syntax.Program` / `MLF.Frontend.Parse.Program` / `MLF.Frontend.Pretty.Program` — canonical `.mlfp` syntax ownership under the main frontend boundary, including closed type-family declaration parse/pretty syntax
- `MLF.Frontend.Program.TypeFamilies` — pre-resolution `.mlfp` type-family normalization and erasure, converting closed family declarations and uses into normalized family-free source types before the existing resolver/checker path
- `MLF.Frontend.Program.Package` — private owner for `.mlfp` package identity, module identity, source-unit shape, trivial-package adapters, explicit local roots/search paths, filesystem discovery, explicit module graph/order validation, and package-to-program projections used while the checker still consumes the existing in-memory program artifact
- `MLF.Frontend.Program.Resolve` — assigns `.mlfp` symbol identities and produces the resolved semantic program artifact consumed by the checker
- `MLF.Frontend.Program.Interface` — private owner for typed checked module interface artifacts, including checked exports, local data/class summaries, visible instances, source-path metadata, and direct package-module dependencies
- `MLF.Frontend.Program.BuildGraph` — private owner for deterministic package build graph and cache validation policy, using parsed source metadata plus typed interface summary metadata rather than file modification times, file sizes, hidden globals, or source-reparse fallbacks as the correctness mechanism
- `MLF.Frontend.Program.Check` — module/import/class/data environment assembly for `.mlfp`, including static validation that may fail before the eMLF pipeline
- `MLF.Frontend.Program.Checked` — abstract checked-program publication boundary; its constituent resolved/checking types already require semantic identities, while `Checked.Internal` remains owner/test support
- `MLF.Frontend.Program.Elaborate` — lowers executable `.mlfp` bindings to surface eMLF `SurfaceExpr`
- `MLF.Frontend.Program.Finalize` — normalizes lowered surface eMLF, calls the internal detailed eMLF pipeline entrypoints with program-owned external binding modes, resolves `.mlfp` deferred obligations, and accepts rewritten terms only after the xMLF typecheck guard
- `MLF.Frontend.Program.Prelude` — built-in source-level `.mlfp` Prelude used by the CLI package entrypoints as an explicit import target
- `MLF.Frontend.Program.Run` — runtime entrypoint that evaluates pure checked `.mlfp` bindings through the existing xMLF runtime, executes checked `main : IO Unit` actions through the reserved IO primitive boundary, and renders recovered closed ADT values with source constructor syntax
- `MLF.Primitive.Inventory` — private owner for builtin type names/kinds, opaque builtin metadata, shared source/backend primitive signatures, and native support classification for lowerable reserved primitives. `MLF.Frontend.Program.Builtins`, `MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower` adapt this inventory; LLVM lowering still owns wrapper/runtime implementation details downstream.
- `MLF.Platform.Contract` — private owner for typed platform substrate contract declarations and deterministic fingerprint-material rendering across ABI version, contract package identity, target triple, trusted substrate components, host toolchain identities, sysroot identity, system library identities, native codegen settings, linker mode, ambient-input policy, and loader-environment policy. It owns declaration/fingerprint-material substrate identity only, not lock validation, generated binding drift closure, native command records, native link records, native execution records, package-manager/linker completion, platform/proof closeout, or self-boot proof completion.
- `MLF.Platform.EnvironmentPolicy` — private owner for pure validation of explicit ambient-input and loader-environment snapshots against the typed policies declared by `MLF.Platform.Contract`, plus deterministic evidence and violation rendering. It owns policy validation over caller-provided observations only; real host capture, native link records, native execution records, proof-manifest emission, and proof closeout remain later platform/proof slices.
- `MLF.Platform.ToolchainIdentity` — private owner for pure validation of declared host toolchain identity from `MLF.Platform.Contract` against explicit observation snapshots, plus deterministic evidence and violation rendering for target triple, resolved tools, sysroot identity, system library identities, codegen settings, and linker mode. It owns validation over caller-provided declarations and observations only; real host toolchain discovery, checked package locks, native command records, native link records, native execution records, proof-manifest emission, and proof closeout remain later platform/proof slices.
- `MLF.Platform.PackageLock` — private owner for pure checked local package lock validation over explicit package/build metadata, required ABI version, and declared substrate fingerprint material. It reuses package/module identities from `MLF.Frontend.Program.Package` and source/interface metadata from `MLF.Frontend.Program.BuildGraph`; package root discovery, source hashing/regeneration, final lock-file parsing, package solving, generated binding drift closure, native command/link/execution records, proof-manifest emission, and proof closeout remain later platform/proof slices.
- `MLF.Platform.NativeLinkRecord` — private owner for pure canonical native link record validation over explicit link-step facts, deterministic evidence rendering, root-bounded stage-output checks for object inputs and linked output artifacts, and resolved linked-library identity diagnostics. It reuses `TargetTriple` and `ToolchainLinkerMode` from `MLF.Platform.Contract`; real linker invocation, host library/toolchain discovery, generated binding drift closure, native execution records, proof-manifest emission, proof-runner integration, and proof closeout remain later platform/proof slices.
- `MLF.Backend.CallableShape` — private owner for callable reference/head data and identity matching shared by `MLF.Backend.IR`, `MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower`; `MLF.Backend.IR` owns classification over executable expressions
- `MLF.Backend.IR` — typed backend IR boundary for checked `.mlfp` programs, before LLVM lowering
- `MLF.Backend.Convert` — checked `.mlfp` program to typed backend IR conversion, including backend type conversion, explicit ADT construct/case recovery, and closure conversion where the checked xMLF shape is unambiguous
- `MLF.Backend.Emission.Prepare` — private adapter for backend-emission semantic preparation from a caller-provided source string or located package before LLVM rendering
- `MLF.Backend.LLVM` — repo-local LLVM backend facade over a small typed LLVM AST, lowerer, and pretty-printer for the supported typed backend IR subset, with explicit diagnostics for unsupported backend nodes
- `MLF.Backend.StructuralRecursiveData` — private owner for structural recursive
  ADT identity and payload-shape matching shared by backend IR validation,
  checked-program conversion, and LLVM lowering; all semantic matching is
  identity-only, with display names retained for diagnostics
- `MLF.Constraint.*` — constraint graph types + normalize + acyclicity + presolution + solve
- `MLF.Binding.*` — binding tree queries + executable χe ops + harmonization
- `MLF.Witness.*` — ω execution helpers (base χe operations)
- `MLF.Elab.*` — elaboration to xMLF (Φ/Σ translation, reify/generalize, plus xMLF typechecking/reduction)
- `MLF.Elab.TypeCheck` — the single typing judgment owner for elaborated `.mlfp` / xMLF terms
- `MLF.XMLF.*` — explicit xMLF syntax and related helpers
- `MLF.Reify.*` — graph-to-type reification and related type operations
- `MLF.Types.*` — elaborated/runtime term and type representations
- `MLF.Util.*` — shared utilities (order keys, union-find, etc.)

The `.mlfp` package/module owner is `MLF.Frontend.Program.Package`. One-file
`.mlfp` inputs are represented as trivial package source units, while directory
inputs and explicit search-path roots are represented as ordered local package
roots. The builtin Prelude is prepended as its own canonical source unit, and
check/run paths retain that package boundary so the checker can reuse its
checked Prelude artifact; there is no flattening Prelude adapter. Cache
eligibility follows hidden builtin source provenance minted by the Prelude
owner, not the display path `<mlfp-prelude>`, and the cache key retains the
complete resolved syntax, identities, and spellings observed by checking. The
one-slot cache builds with a Prelude-owned descending identity supply, stores
the checked module with the extrema of every generated identity it contains,
and advances each client supply past those extrema on both cache hits and
misses. The package owner can discover `.mlfp` files under those roots/search
paths, retain source paths, reject duplicate modules across searched roots with
source-path context, build a module-to-file graph, reject missing or cyclic
module imports, and project modules in dependency order so imports are checked
before importers. `MLF.Frontend.Program.Interface` owns the private checked
module interface artifact that `Check` uses for prior-module import visibility
and package consistency validation. The artifact records
checked exports, local data/class summaries, visible instances, source paths,
and direct `PackageModuleId` dependencies; validation fails closed when graph
order, source paths, dependencies, or exported symbol ownership do not match the
checked package. The interface artifact is not a second typechecker authority:
it is extracted only from checked modules plus the package graph, and malformed
artifacts are rejected rather than repaired. `MLF.Frontend.Program.BuildGraph`
owns the private package build graph/cache policy on top of `Package` and
`Interface`: build nodes carry package module ids, source paths, direct imports,
deterministic parsed-source metadata, and direct dependency interface metadata
ordered by the package graph. Cache entries validate module id, source metadata,
direct dependency ids, dependency interface metadata, and the module's own
interface summary metadata against the current package build graph before reuse.
Stale source, stale dependency-interface, and malformed interface cases fail
closed with module/interface names instead of falling back to modification-time,
file-size, hidden-global, or source-reparse heuristics. `Finalize` consumes the
assembled program scope, and `Run` evaluates all checked module bindings
together. `MLF.Program.CLI` and the narrow public `MLF.Pipeline` package
functions route check/run/backend-entry preparation through this package owner.
There is no persisted interface file format, package manager, remote dependency
system, stable `.mlfp` ABI, linker, or separate compilation mode today. Future
separate compilation should be introduced only through the package, interface,
and build-graph owners and explicit compiler-owned artifacts that carry checked
xMLF/runtime payloads rather than by peeking at source outside the package
projection.
The fixture migration evidence for package substrate behavior is kept in
`test/programs/packages/` and `ProgramFixturePackageSpec`. The compiler
frontend seed package contract is kept in
`test/programs/compiler-seed/frontend-contract/` and
`ProgramCompilerSeedSpec`; it proves ordinary package discovery, module graph
order, source-path retention, check/run through the interpreter, and CLI
`check-program`/`run-program` output for the seed root without introducing a
second compiler-source loader. The same fixture owns the bounded
symbolic-input lexer seed: `SeedSource`, `SeedToken`, `SeedDiagnostic`, and
`SeedLexer` define source span labels, input symbols, tokens, diagnostics,
lexer results, and `.mlfp` evidence rendering for one accepted token path and
one rejected diagnostic path. `SeedAst` and `SeedParser` layer the bounded
parser seed on that token stream with one definition AST shape, parser
result/diagnostic values, and interpreter evidence for one accepted parse and
one rejected missing-equals parse. The seed package is currently lowerable by
the existing backend/native subset and the spec validates backend/native LLVM,
object-code generation, and linked native execution for this bounded
entrypoint. That evidence stays under the existing package, CLI, backend IR,
and LLVM owners; it is not a source-text lexer/parser, package manager, ABI,
linker, separate-compilation mode, arbitrary native compiler workload
guarantee, or self-hosting claim. `docs/mlfp-self-boot-readiness.md` records
the remaining self-boot gaps by layer.

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
audits, and runtime-name construction. The intended semantic rule is to compare
stored identities or identity-aware source-type shapes, not qualified strings.
Instance and evidence matching uses the shared identity-bearing
`ClassApplicationKey` and `EvidenceMethodKey` abstractions. These keys retain
`TypeView` head and binder identities; display projections are not evidence
lookup keys.

### Resolved xMLF identity target

The accepted target for checked xMLF is **Resolved xMLF Identity IR**; see
`docs/adr/2026-06-18-resolved-xmlf-identity-ir.md`.

Today `MLF.Types.Elab.XmlfTerm` stores `ResolvedVar` directly for executable
variable occurrences and lambda/let binders. `EVarNode` is the single variable
node; deferred terms are built from explicit `DeferredRef` values, with generated
refs required for accepted checked-program terms. `ELam`/`ELet` are the only
lambda/let term forms. Local-binder construction helpers are for tests and
fixture-like call sites. `CheckedBinding` carries a
`ResolvedVar`, and constructor bindings carry a
`ConstructorId` reference consumed by backend conversion. Normal checked-program
finalization rejects terms that still carry deferred
variable identity. Derived-instance synthesis, instance skeleton construction,
lowering, and constraint generation share the module-owned supply, so lexical
and deferred identities exist before xMLF is built. Graph-derived locals retain
their graph provenance through capture avoidance; there is no checked-term
stamping or graph-local freshening pass. Metadata-derived constructor bindings
are published with the quantified `ETyAbs` spine constructed from
`ConstructorInfo`, bypassing generic vacuous-forall stripping that would erase
phantom owner parameters. The opaque
unchecked finalization path runs the same `unresolvedXmlfTermVarRefs` rejection
before it can publish a `CheckedBinding`. `MLF.Types.Identity` owns common
`IdDetails` reference-name, local, constructor, and local-rename projections for
these consumers. Core elaboration emits resolved variable occurrences from
`EnvBinding`: external environment values carry `EnvId`, while lambda and let
binders carry `LocalId`, and lambda/let rewrite helpers refresh local occurrence
sidecar types when they rewrite binder types. Every `XmlfTerm` variable and binder
form carries `ResolvedVar` at the type level. The elaboration `Env` has one
authoritative `Map ResolvedTermIdentityKey EnvBinding`; runtime names are
payload projections, and the type-check environment is derived instead of
stored as a second synchronized index. `PreparedExternalBindings` likewise
owns alias-indexed `PreparedExternalBinding` records that pair each external
binding with its checked scheme. Constraint, elaboration, and type-check maps
are projections of those records; preferred/fallback union cannot cross-pair
independently merged binding and scheme maps.

Resolved Program expressions still pass through `compileResolvedExpr` into the
source-shaped `SurfaceExpr` boundary. Its variable, lambda, let, and annotated
lambda nodes carry a phase-indexed `TermReference` directly: parser input is
name-only by construction, while `MLF.Frontend.TermResolve` crosses once to a
resolved carrier that requires `IdDetails` plus a display/runtime spelling.
Normalization, desugaring, and constraint generation preserve that payload into
the exact `ALam`/`ALet` node. Finalization therefore does not
reconstruct binder meaning from a `Map String IdDetails`, a wrapper node, or a
runtime-name sidecar. Runtime-name projections remain boundary views.
Source `ELamAnn` nodes always desugar through the thesis κσ coercion. A
compiler-generated `EvidenceId` parameter instead desugars to the core-only
`EExactLamNode`; class metadata already owns its exact type, so routing it
through κσ would incorrectly introduce a fresh flexible codomain.
The source κσ graph uses direct rigid and flexible copies of the annotation;
Figure 8.2.3's Eq-Var case is not encoded as a synthetic bounded-`forall`
wrapper around the flexible copy.

Backend-emission preparation, runtime reachability, typechecking, reduction, and
backend conversion consume `ResolvedVar` / `IdDetails`. Constructor applications
carry `ConstructorId`; constructor bindings, including constructor-local
`forall` shapes, are finalized directly from `ConstructorInfo` metadata. Their
surface expression is not a fallback semantic authority.

`LoweredBindingIdentity` is a closed top-level/constructor/method sum. A local,
environment, or deferred `IdDetails` value cannot cross the module-binding
construction boundary. Type-binder substitutions are direct
`Map TypeBinderIdentity TypeView` values; display aliases are source-boundary
inputs, not substitution state. `typeViewFromSourceTypeInScope` constructs the
identity-bearing node tree first and then changes only its display projection,
so import qualification cannot temporarily erase a type-head identity.

`TypeView` owns one node-level tree whose head and binder nodes carry display
spelling, semantic identity payload, and lookup aliases. Stable-name source
types and lookup indexes are derived from those payloads; there is no cached
identity spelling or parallel identity-shaped tree that can diverge. Context
nodes retain constructor-scope identities that are not visible in a projected
result type. Construction from source-shaped syntax accepts one shape plus
identity aliases and rejects a missing or ambiguous payload explicitly.
Substitution, quantified specialization, and subtree projection preserve node
payloads and lexical binder contexts directly instead of projecting two
`SrcType`s and pairing their strings. `typeViewNodeView`, `typeViewToResolved`,
free-binder collection, matching, and backend conversion likewise traverse
payloads directly. Display-only updates are shape checked and preserve
payloads. `CheckedProgram` publication therefore does not run a late
identity-hydration or completeness repair traversal. Constructor-visible
rewriting, deferred constructor/case obligations, evidence keys, and
type-binder substitution retain those identities. The focused snapshot is
`docs/audit/identity-string-reference-audit.md`.

The target does not duplicate every checked module declaration inside every
term occurrence. `CheckedModule` remains the declaration owner for data,
classes, instances, and exports. The term layer carries executable identity
needed to typecheck, reduce, run, analyze dependencies, and convert to backend
IR without returning to source spelling or string-keyed constructor recovery.
Source spellings remain diagnostics/rendering data; `SymbolIdentity` remains
the semantic equality key.

## Key graph and witness types

- `Expr` (`MLF.Frontend.Syntax`) — surface eMLF terms
- `XmlfTerm` (`MLF.Types.Elab`) — checked xMLF term representation; executable
  occurrences and lambda/let binders store `ResolvedVar` directly
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
  token before `mkInstanceWitness` can mint them. `normalizeInstanceOpsFull`
  returns that opaque token directly instead of returning a raw operation list;
  the predicate-style validator cannot mint one. Edge normalization consumes
  its destination-presentation validation certificate when it restores frozen
  source identities, then proves the final operand domain and terminal
  root-`RaiseMerge` trace authority before sealing the source presentation.
  The raw sealing primitive remains private to the internal witness owner, and
  no `[InstanceOp] -> ValidatedInstanceOps` conversion is exported through a
  production façade. Meanwhile,
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
- Generalization root selection returns a tagged live/base root. Application of
  that plan immediately pairs the root with its owning `PresolutionView` or
  `GaBindParents.gaBaseConstraint`, and the no-fallback reifier consumes that
  pair. A bare `NodeId` is never used to guess between graph domains, because
  live and base graphs may reuse the same numeric key.
- `MLF.Elab.Run.ResultType.View` owns result-type reconstruction's query
  adapter over the prepared result-type input: bound overlays, no-fallback
  reification, base-target projection into `GaBindParents.gaBaseConstraint`,
  scope/target queries, and target generalization. `ResultType.Fallback.*`
  should select policy paths through this adapter rather than patching
  `PresolutionView` records or rebuilding base-map inputs locally.
- Annotation result reconstruction has two closed outcomes: apply an
  instantiation selected by exact target construction or frozen witness
  translation, or generalize the annotation target. It does not rewrite a Φ
  computation after selection. Composite annotation sources likewise must
  inhabit `ConstructedAnnotationSource`; only a direct semantic occurrence key
  can inhabit `WitnessAnnotationSource`.

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

Production lowering accepts `ProductionBackendProgram`, constructed only by
`mkProductionBackendProgram` after closed-program validation.
`MLF.Backend.Convert` returns this capability wrapper, and LLVM entrypoints do
not accept a raw `BackendProgram`. Its raw projection is confined to
`MLF.Backend.IR.Production.Internal` for the LLVM lowering owner. Backend IR
constructors themselves require identities for modules, declarations,
references, lexical binders, patterns, type heads, and type binders; compact
identity-erasing patterns are unidirectional match/render views. Test support
constructs complete fixture identities rather than entering a permissive raw
IR. `validateBackendProgram` therefore checks relationships and typing, not a
repairable missing-identity mode.

That callable contract is explicit. `BackendApp` is the direct first-order
call node, so local direct aliases that remain first-order stay on this path.
`BackendClosureCall` is the indirect closure-call node, so closure-valued
aliases, captured closures, constructor-field projections, and case/let-
selected closure values stay on this explicit path instead of relying on
lowerer recovery.
`MLF.Backend.IR` classifies callable heads directly from `BackendExpr`.
`MLF.Backend.CallableShape` owns only the shared callable reference/head data
and identity-matching rules, so conversion and lowering consume the same shape
without creating a second executable-expression view.

The ADT/case ownership split is explicit. Row-4 ADT/case ownership means
semantic constructor/case nodes stay in `MLF.Backend.IR`:
`BackendData`, `BackendConstructor`, `BackendConstruct`, and `BackendCase`
preserve constructor metadata, constructor use, and case alternatives only.
Runtime tags, field slots, closure-record storage for function-like fields,
and nullary tag-only representation stay private to LLVM/native lowering.
Checked-program conversion must not assign runtime tag values, field offsets,
boxing/storage policy, or layout-only witnesses.

Structural recursive ADT identity and payload-shape matching is centralized in
`MLF.Backend.StructuralRecursiveData`. `MLF.Backend.IR`,
`MLF.Backend.Convert`, and `MLF.Backend.LLVM.Lower` reuse that owner. Its
nominal and structural entrypoints compare carried data, constructor, type
binder, and recursive-owner identities. Structural names remain diagnostic and
canonical emitted spellings; they cannot supply a missing owner or override a
conflicting identity. Conversion normalizes representation shape only after the
owner is selected by identity.

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
- production modules, data declarations, constructors, data parameters,
  bindings, term references, lexical binders, closure references, patterns, and
  type references must carry semantic identity;
- module-level binding names remain runtime names and must be globally unique in
  a `BackendProgram`;
- `backendProgramMainIdentity` must designate one of those bindings; the
  companion `backendProgramMain` string is runtime/diagnostic metadata and is
  not the production lookup key;
- binding declarations must match the type carried by their expression body;
- variable references must resolve either to lexical binders introduced by
  lambda/let/case patterns or to globally unique program bindings, and the
  carried variable type must match that binding;
- `BackendApp` heads must stay on the direct-call path, while malformed direct
  calls on closure-valued heads fail with explicit backend callable
  diagnostics;
- lambda, application, let, type abstraction/application, recursive roll, and
  recursive unroll nodes satisfy local type equalities checked by
  `validateBackendProgram`; test fixtures construct and validate the same
  identity-complete IR;
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
`MLF.Backend.LLVM` preserves that boundary by running production identity
validation before lowering, rendering the supported first-order subset plus
explicit closure IR, and producing explicit unsupported-node diagnostics for
backend constructs that do not yet have LLVM lowering. The LLVM backend is
intentionally repo-local:
`Syntax` models the small LLVM surface used by mlf2, `Lower` maps backend IR
into that AST, and `Ppr` emits opaque-pointer LLVM IR text accepted by LLVM 15+
tools or LLVM 14-era tools run with `-opaque-pointers`.

The backend has two emission contracts. Both raw and native emission still
consume the same backend IR program. Raw emission keeps the checked `.mlfp`
`main` as an ordinary module-qualified LLVM function and is the current internal
IR inspection surface. Native emission adds a C ABI `i32 @main()` wrapper around a
zero-argument checked `.mlfp` `main`, renders supported pure `Int`, `Bool`, and
first-order ADT results to stdout using the same value text as `ProgramSpec`,
prints one trailing newline, writes no stderr on success, and returns process
exit status `0`. Native emission declares libc `malloc`/`printf` and emits
backend-owned runtime definitions such as `__mlfp_and` when those names are not
program bindings. Unsupported result types fail before native-run assertions,
so the native process boundary does not invent source or IO semantics.
`docs/backend-native-pipeline.md` records the linked-executable test pipeline,
toolchain discovery, generated artifacts, runtime support, and row coverage
classification. `Parity.ProgramMatrix.NativePolicy` owns the merged
test-support interpreter, LLVM, native, and object-code row policy consumed by
`BackendLLVMSpec`. Each row constructs one checked artifact, asserts its
interpreter result, and then reuses that artifact for backend LLVM assembly,
object-code smoke, and native execution without widening production backend
APIs.

`MLF.Backend.Emission.Prepare` owns the shared semantic preparation step for
raw and native backend emission: source-string callers parse with a display path
and become located trivial packages, while package callers pass an already
discovered located package. Both paths check the package and prune the checked
Prelude module to the binding and data dependency closure required by backend
rendering. The CLI adapter in `src/MLF/Program/CLI.hs` remains the command and
file-or-root owner for `check-program`, `run-program`, `emit-backend`, and
`emit-native`: it parses `--search-path`, loads file inputs as trivial packages
or directory inputs as local package roots, injects the source-level Prelude,
delegates semantic preparation or runtime execution, and presents user-facing
errors.

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

- Production presolution commits one `EdgeExecutionArtifacts` packet per edge.
  The packet owns the expansion, witness, Raise authority, non-source operation
  origins, raw construction certificate, and replay trace as one immutable
  proof artifact. `recordEdgeExecutionArtifacts` accepts an equal replay and
  rejects a write that changes any field; replay likewise rejects a packet that
  conflicts with the committed edge. The exported legacy `PresolutionState`
  fixture pattern remains a test-compatibility projection and may synthesize
  empty auxiliary fields, so it is not the production construction contract.
- `EdgeWitness.ewForallIntros` stores the number of quantifier introductions
  needed for the O phase, and `EdgeWitness.ewWitness` stores the Ω-only instance
  operations (`OpGraft`, `OpMerge`, `OpRaise`, `OpWeaken`, `OpRaiseMerge`).
- `EdgeTrace` is the per-instantiation-edge provenance record consumed by Φ. It
  tracks the expansion root, binder→argument pairs, exact interior `I(r)`,
  replay contract, replay-domain binder map, replay-domain binders, and copy-map
  provenance. Witness operation node IDs, `etBinderArgs`, `etInterior`, and
  `etCopyMap` keys live in one source-ID domain; canonical IDs are derived at
  lookup sites rather than globally rewriting trace provenance.
- Φ/Ω cannot consume a witness and trace independently. The opaque
  `PhiReplayCertificate` fetches and pairs them at the elaboration boundary
  through one producer-owned edge key, then checks that the witness's embedded
  edge identity agrees. It does not rederive packet association from `ewRoot`,
  `etRoot`, or `etResultRoot`: replay/finalization can leave those IDs in
  distinct source, destination, and construction presentations. Production Φ
  entry points and `OmegaContext` require this paired authority; only the
  test-support seam can express a missing trace.
- `RawExpansionConstruction` records the exact destination-domain parent edits
  made while constructing χₑ, together with argument and semantic-meta roles.
  Composed expansion steps union compatible certificates and reject a shared
  child with conflicting parents. Preparation projects this evidence through
  the final quotient into `ExpansionConstructionPlacements`; consumers then
  derive the candidate's current owner and binding flag from the authoritative
  binding tree. Cross-source copy edges retain Phase-4 alignment. If a
  construction child and parent collapse to one source representative, the
  administrative χₑ edge is discarded and terminal χₚ Raise/Weaken placement
  remains authoritative.
- Φ represents the thesis split directly. `QuantifierReordering` validates
  φ_R, `EdgeTranslation` validates T(e), and `OccurrenceComputation` retains
  their validated composition. Each component checks application and
  alpha-equivalent endpoints; the composition seam requires exact
  identity-bearing `ElabType` equality, so two alpha-equivalent forall trees
  with different binder identities cannot be silently joined.
- Every production Φ entry point requires the frozen pre-solve
  `GaBindParents` certificate. Ω classifies source `OpRaise` rigidity from
  `gaBaseConstraint`, never from the finalized replay representative. A later
  graph node may supply an operation type only when the source certificate
  proves that node did not yet exist. Provenance-free low-level fixtures mint
  an identity certificate only behind `MLF.Elab.Phi.TestSupport`; there is no
  production optional-Γ or `Phi.Env` state seam.
- `ElabReadModel` is the general read-only graph projection. Φ translation can
  consume it only after `buildPhiReadModel` has produced a `PhiReadModel`
  capability proving binding-tree integrity and the absence of Gen fallback;
  legal non-Φ reification is not rejected merely because an intermediate graph
  is not Φ-ready. Scheme closure is separately context-aware:
  `SchemeClosureAuthority` admits free binder identities only through an exact
  ambient, inherited Γ, or locally closed Γ capability. Generalization outer
  binders additionally pass through the opaque `FinalizeBinderPlan`, which
  pairs the planner order with its reified bounds once; finalization cannot
  manufacture a forall for a residual ref. Let publication and prepared-root
  closure revalidate the composed scheme at their authoritative boundaries.

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
- Identity-backed forall binders are authoritative evidence: a nested let that carries graph identities through the elaborated scheme remains a normal success case, not an expansion-derived fallback.

This keeps the runtime path closer to the thesis boundary: if witness/scheme information is insufficient, the code now fails explicitly instead of silently switching to a weaker reconstruction mode.
