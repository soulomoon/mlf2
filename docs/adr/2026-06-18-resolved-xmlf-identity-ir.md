# ADR: Make Checked xMLF Carry Resolved Identity

Date: 2026-06-18
Status: Accepted

## Context

The `.mlfp` resolver already assigns semantic `SymbolIdentity` values to
global source references. The checker, elaborator, and backend then lose part of
that provenance when executable terms become `XmlfTerm`, because older
name-only variable and binder views stored plain `String` names. Later phases
recover constructor, method, primitive, and ordinary binding meaning from side
maps such as `ValueInfo`, `DataInfo`, `CheckedModule`, and `ConvertContext`.

That recovery is workable but it makes checked xMLF less complete than the
front end has already proved. It also forces backend conversion to rediscover
constructor intent from term shape and string lookup, and it keeps
compiler-generated constructor bindings on the same surface pipeline path as
user definitions even when the constructor metadata already carries the needed
identity and type information.

GHC Core is a useful comparison point. Core expressions do not carry every
module artifact inside each term, but a variable occurrence is an `Id`, not a
string. The `Id` carries stable details such as data-constructor worker or
wrapper identity. The module still carries TyCons, binds, rules, and codegen
metadata beside the terms. The target here follows that split: checked xMLF
terms should carry resolved executable identity at variable occurrences, while
module-level declarations remain owned by checked module metadata.

## Decision

Adopt **Resolved xMLF Identity IR** as the target checked term architecture.

The final checked xMLF term layer must not represent global executable
references as plain strings. It should carry a resolved variable value:

```haskell
data ResolvedVar = ResolvedVar
  { resolvedVarName :: String,
    resolvedVarRuntimeName :: String,
    resolvedVarType :: ElabType,
    resolvedVarDetails :: IdDetails
  }

data IdDetails
  = LocalId LocalRef
  | EnvId EnvRef
  | TopLevelId SymbolIdentity
  | ConstructorId ConstructorRef
  | MethodId SymbolIdentity
  | PrimitiveId PrimitiveRef
  | DeferredId DeferredRef

data ConstructorRef = ConstructorRef
  { constructorRefSymbol :: SymbolIdentity,
    constructorRefRuntimeName :: String,
    constructorRefOwnerType :: SymbolIdentity,
    constructorRefOwnerRuntimeName :: String,
    constructorRefIndex :: Int,
    constructorRefForalls :: [(String, Maybe SrcType)],
    constructorRefArgs :: [SrcType],
    constructorRefResult :: SrcType
  }

data PrimitiveRef = PrimitiveRef
  { primitiveRefSymbol :: SymbolIdentity,
    primitiveRefRuntimeName :: String
  }
```

The exact field names may change during implementation, but the invariant may
not: a checked constructor occurrence must carry enough identity to decide that
it is a constructor without consulting a string-keyed value environment, and
must carry enough metadata to recover its owner, order, field types, result
type, and backend constructor identity without shape guessing.

`XmlfTerm` should move from name-only variable nodes and string binders toward resolved
occurrences and typed binders. Local binders may remain compact, but local
occurrences must identify the binder by a local reference rather than by an
unqualified spelling when the term has crossed into checked xMLF. Source
spelling remains diagnostic metadata, not executable identity.

The checked module remains the owner of declarations:

- `CheckedModule` still carries data, class, instance, and export metadata.
- Resolved term occurrences point at that metadata by semantic identity or
  stable constructor reference.
- Backend conversion may still build backend `DataMeta` and `ConstructorMeta`,
  but it must use resolved term details instead of reclassifying name-only variables
  through `ccConstructors`.

This is a single checked IR layer in the sense that executable identity is in
the checked term. It is not a requirement to duplicate every module declaration
inside every occurrence. Module declarations and checked terms together form the
checked program artifact; source scopes and string lookup maps are no longer
part of executable identity.

## Required Invariants

- No global executable occurrence in checked xMLF is a bare `String`.
- Constructor occurrences are distinguishable by `IdDetails` without inspecting
  spelling or matching structural lambda/roll shapes.
- Constructor binding terms, constructor applications, case alternatives, and
  backend conversion all agree on one `ConstructorRef` identity.
- Display names and source spellings are never used for semantic equality.
- `SymbolIdentity` remains the stable cross-module identity key.
- Local binders and local occurrences are alpha-renamable without changing
  global identity.
- Typechecking, reduction, free-variable queries, closure analysis, runtime
  dependency discovery, backend conversion, and backend emission preparation
  consume resolved references rather than reconstructing them from strings.

## Migration Shape

1. Introduce the resolved variable model beside the current term code, but make
   it the target type for checked-program finalization rather than a backend-only
   annotation.
2. Extend lower/finalize so user definitions and compiler-generated bindings
   create resolved occurrences at the checked xMLF boundary.
3. Give constructor bindings a metadata-derived checked path that constructs the
   same resolved constructor identity as ordinary constructor occurrences.
4. Move backend conversion to consume `ConstructorId` directly for constructor
   bindings and constructor applications. Structural constructor recognition
   remains only as a temporary compatibility adapter while old string terms are
   still accepted internally.
5. Move runtime dependency discovery, free-variable collection, closure and
   evidence argument analysis, and emission preparation to resolved variables.
6. Delete the string-only executable global path once all checked-program
   producers and consumers use resolved variables.

During migration, bridges must be owner-local and named as compatibility seams.
They should not become a second permanent IR.

## Implementation Status

As of 2026-07-11, the checked identity boundary is construction-complete:

- `LoweredBinding` carries `LoweredBindingIdentity`.
- `CheckedBinding` carries `ResolvedVar`.
- Constructor bindings carry `ConstructorId ConstructorRef`.
- Backend constructor-binding synthesis consumes that checked constructor
  identity.
- `MLF.Types.Identity` owns `IdDetails` reference-name, local, constructor, and
  local-rename projections, and `ResolvedVar` plus binding constructor-reference
  helpers delegate those projections through that identity layer.
- `XmlfTerm` and `XmlfTermF` variable, lambda, and let nodes store
  `ResolvedVar` directly instead of `Maybe ResolvedVar`; `EVarNode` is the
  single variable node, deferred terms are built from explicit `DeferredRef`
  values,
  `ELam`/`ELet` are the only lambda/let term forms, and
  `mkLocalLam`/`mkLocalLet` are local-binder construction helpers for tests and
  fixture-like call sites.
- Core elaboration emits `EVarNode` directly from `EnvBinding`; initial
  external environments use `EnvId`, lexical lambda/let binders use `LocalId`,
  and lambda/let rewrite helpers refresh local occurrence sidecar types when
  binder types are aligned to the target type.
- Derived-instance synthesis, instance skeleton construction, resolved lowering,
  and constraint generation allocate from one caller-owned identity supply
  before `XmlfTerm` is built. Graph-derived locals preserve their graph
  provenance through capture avoidance. Checked-program finalization preserves
  those identities, canonicalizes occurrence types from the identity-keyed
  environment, and rejects terms that still carry deferred variable identity.
  Metadata-derived constructor bindings publish the quantified `ETyAbs` spine
  they construct directly, so generic vacuous-forall stripping cannot erase a
  phantom owner parameter. Finalization does not stamp or freshen terms after
  elaboration.
- Backend-emission preparation, checked-program runtime reachability, and
  backend-conversion free-variable scans consume `ResolvedVar` / `IdDetails`
  before falling back to the string-runtime compatibility view.
- Backend conversion recursive-let lifting and capture-avoiding rewrite helpers
  now preserve resolved occurrence and lambda/let binder identities while
  generating helper bindings.
- Backend conversion builtin-type normalization and ordinary lambda/let
  emission now preserve resolved occurrence and binder identities while running
  temporary backend type inference and emitting backend binders.
- Backend conversion partial-application, closure-demand, handler-call, and
  structural lambda-shape probes read resolved local identity before falling
  back to runtime names.
- Backend conversion let-alias application-head unfolding compares resolved let
  binder identity before falling back to runtime names.
- Backend conversion recognizes resolved constructor application heads through
  `ConstructorId ConstructorRef` before falling back to string-runtime
  compatibility for unresolved terms.
- xMLF pretty/XMLF projection reads resolved local identity before the
  string-runtime compatibility view, so rendered checked terms do not expose
  stale local runtime spellings.
- Pipeline type-abstraction freshening preserves resolved occurrence and binder
  identities while renaming internal type variables.
- Pipeline authoritative-annotation selection recognizes resolved local
  identity-lambda terms before falling back to runtime-name equality.
- `MLF.Elab.TermClosure` preserves resolved occurrence and binder identities while
  substituting internal type names and aligning type-variable binders.
- xMLF reducer capture-avoidance counts resolved occurrence/binder identity
  types when freshening type binders.
- Checked-program deferred constructor/case/method rewrite helpers preserve
  resolved occurrence and binder identities.
- Checked-program deferred placeholder matching reads resolved occurrence
  identity before falling back to string-runtime compatibility names.
- Checked-program deferred local evidence finalization matches evidence methods
  by class identity, type identity, and method identity instead of evidence runtime
  names.
- Instance method resolution keeps an identity-indexed method map beside the
  source-name map, so deferred method dispatch selects the concrete instance
  method by `MethodInfo` identity.
- Module definition finalization collects checked layer results by definition
  `SymbolIdentity` instead of checked-binding runtime name.
- Module finalization read contexts are keyed by lowered binding identity rather
  than lowered binding runtime name.
- Module pipeline results are re-keyed by lowered binding identity immediately
  after the string-keyed pipeline boundary returns.
- Constructor bindings construct their quantified and term-lambda spine from
  metadata and publish it directly instead of running a constructor-specific
  result-shape repair or generic vacuous-forall stripping.
- Retained-child preservation analysis reads resolved let-binder and variable
  identity before falling back to the string-runtime compatibility view.
- Checked-program runtime aggregation now builds the executable main term with
  resolved binding identities, and Church-data decoding reads resolved handler
  identity before falling back to runtime names.
- Source-type finalization's vacuous-forall stripping counts resolved identity
  types; metadata-derived constructor bindings bypass that generic step so
  phantom quantified owner parameters remain represented by `ETyAbs`.
- Checked-program local `let` binder identities preserve the complete binding
  scheme instead of only the scheme body, so type-instantiated local
  occurrences keep enough checked-IR type information.
- xMLF typechecking rejects resolved local variable occurrences whose identity
  type is stale relative to the resolved binding identity.
- Backend conversion consumes the checked occurrence types directly; it no
  longer reconciles stale resolved-local annotations at its own boundary.
- Opaque unchecked checked-binding finalization uses the same construction-time
  lexical identities and rejects any remaining `DeferredId` before storage.
- Surface variable and binder nodes carry `TermReference` directly. Resolved
  nodes retain exact `IdDetails` through normalization, desugaring, constraint
  generation, and elaboration; finalization no longer reconstructs local binder
  meaning from runtime strings or an identity wrapper.
- `ClassApplicationKey` and `EvidenceMethodKey` retain identity-bearing
  `TypeView`s across elaboration, finalization, and runtime evidence lookup.
- `CheckedProgram` is abstract. `mkCheckedProgram` validates mention-sensitive
  `TypeView`, `ElabType`, and term identity completeness once at publication;
  runtime and backend consumers accept that capability without revalidation.
- Deferred constructor/case substitution stores `TypeView` values by
  `TypeBinderIdentity`, and structural constructor result binders derive their
  identity from the data owner plus `StructuralResultBinder` role.
- Backend conversion returns `ProductionBackendProgram`; LLVM entrypoints accept
  only that validated identity-complete capability. Its raw projection is
  owner-internal to LLVM lowering, while metadata-light tests use
  `BackendProgramFixture`.
- Every constructor binding, including constructor-local `forall` shapes, has a
  metadata-derived checked finalization path built from `ConstructorInfo`; the
  surface expression is no longer a fallback semantic authority.
- `EnvRef` and `PrimitiveRef` no longer use strings as semantic identity:
  `EnvRef` compares generated `UniqueIdentity` values, `PrimitiveRef` compares
  builtin `SymbolIdentity` values, and `DeferredId` is treated as an unresolved
  marker rather than a semantic identity.

Status update, 2026-07-11: `XmlfTerm` occurrences and binders store `ResolvedVar`
directly, and normal plus opaque checked-program finalization reject remaining
`DeferredId` references. `SurfaceExpr` variable and binder nodes now carry
metadata-light or resolved `TermReference` payloads directly. `TypeView` stores
one node-level identity-bearing tree and derives its source projections and alias
indexes. `CheckedProgram` construction owns the single recursive
`TypeView`/`ElabType` validation traversal, and backend reference policies share
one `ReferenceMode`. Production backend conversion publishes
`ProductionBackendProgram`, LLVM accepts only that capability, and
metadata-light backend values cross an explicit fixture capability. The shared
raw backend representation still admits fixture constructors, but production
cannot enter lowering through that path. The current focused snapshot is
`docs/audit/identity-string-reference-audit.md`.

## Performance Expectation

Resolved identity alone is not expected to make `.mlfp` checking much faster.
The speedup comes from the simplifications it enables:

- constructor bindings are finalized from metadata plus a small checked-term
  guard instead of running through the full surface pipeline;
- backend conversion can lower constructor applications directly from
  `ConstructorId`, avoiding string recovery and structural shape guessing;
- dependency and closure scans can compare resolved references instead of
  spelling-derived names.

The accepted performance claim for this design is not "resolved variables make
everything fast." The measurable target is that constructor-heavy packages
spend less time in `program.check.module.*.constructor-bindings` as the
metadata-derived constructor path grows beyond its initial nullary subset.

## Rejected Alternatives

- Keep name-only variables as the checked xMLF executable identity. This preserves
  the current recovery work and keeps constructor provenance outside the term.
- Add a backend-only constructor annotation. That helps lowering but does not
  make checked xMLF a complete executable IR.
- Add a dedicated `EConstruct` node immediately. The constructor-worker-as-var
  model is closer to the existing xMLF calculus and GHC Core. A dedicated node
  can be reconsidered if constructor applications need invariants that
  `ConstructorId` plus `EApp` cannot express.
- Copy complete `DataInfo` graphs into every variable occurrence. That makes
  stale duplicated metadata more likely and bloats ordinary terms. Occurrences
  should carry resolved identity and the constructor snapshot needed at the use
  site, while checked modules remain declaration owners.

## Consequences

- `MLF.Types.Elab` is no longer a string-based xMLF syntax layer for terms.
  If a paper-facing minimal xMLF text is needed, it should be a diagnostic
  pretty/erasure view from checked `XmlfTerm`, not a second parsed term IR.
- Tests that inspect `show checkedBindingTerm` by spelling will need to assert
  semantic identity or rendered diagnostics instead.
- Backend conversion should get smaller around constructor recovery as remaining
  deferred-name `XmlfTerm` producers disappear.
- The resolved-symbol resolver remains authoritative; finalization must not
  invent identities after checking.
- The implementation should prefer one resolved term representation over a
  long-lived pair of string and resolved xMLF terms.
