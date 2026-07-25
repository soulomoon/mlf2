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
  { resolvedVarType :: ElabType,
    resolvedVarDetails :: IdDetails
  }

data IdDetails
  = LocalId LocalRef
  | EvidenceId LocalRef
  | EnvId EnvRef
  | TopLevelId SymbolIdentity
  | ConstructorId ConstructorRef
  | MethodId SymbolIdentity
  | PrimitiveId PrimitiveRef
  | DeferredId DeferredRef

data ConstructorRef = ConstructorRef
  { constructorRefSymbol :: SymbolIdentity }

data PrimitiveRef = PrimitiveRef
  { primitiveRefSymbol :: SymbolIdentity }
```

The occurrence itself carries enough identity to decide that it is a
constructor without consulting a string-keyed value environment. The owning
checked `ConstructorInfo` and module metadata carry the constructor owner,
order, field types, result type, and backend layout. `ConstructorRef` is the
identity-bearing join key; duplicating that metadata on every occurrence would
create another independently stale representation.

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
   bindings and constructor applications. Any remaining structural recognition
   is identity-pinned, metadata-derived shape handling; it is not a name-based
   compatibility adapter.
5. Move runtime dependency discovery, free-variable collection, closure and
   evidence argument analysis, and emission preparation to resolved variables.
6. Delete the string-only executable global path once all checked-program
   producers and consumers use resolved variables.

During migration, bridges must be owner-local and named as compatibility seams.
They should not become a second permanent IR.

## Implementation Status

As of 2026-07-21, the checked identity boundary is construction-complete:

- `Expr` is indexed by raw or resolved term-reference phase. The one resolver
  transition allocates lexical identities and produces nodes that require
  `ResolvedTermReference IdDetails`; normalization and desugaring preserve it.
- `XmlfTerm` and `XmlfTermF` variable, lambda, and let nodes require
  `ResolvedVar`. Deferred terms use explicit `DeferredRef` values, and normal
  plus opaque finalization reject a remaining `DeferredId`. There is no late
  occurrence-annotation or binder-stamping pass.
- `LoweredBinding` requires one `LoweredBindingIdentity` and both source and
  expected `TypeView`s. `CheckedBinding` requires `ResolvedVar`, `TypeView`,
  `ElabType`, and `XmlfTerm`. Constructor bindings carry
  `ConstructorId ConstructorRef` and are built directly from
  `ConstructorInfo`, including their quantified spine. The lowered identity is
  a closed top-level/constructor/method sum; generic resolved locals and
  deferred references cannot be used as module-binding identities.
- `TypeView` is an abstract, identity-bearing node tree. Every semantic type
  head and binder node carries its payload; display/stable-name types and alias
  maps are projections. Construction consumes one source shape plus identity
  aliases and rejects missing or ambiguous payloads; no parallel identity-shaped
  source tree or cached identity spelling remains. Matching, substitution,
  specialization, subtree projection, free-binder collection, elaborated-type
  conversion, and backend conversion traverse payloads directly.
- Scope-visible type spelling is applied only after `TypeView` construction.
  Import aliases therefore update a display projection while the node payload
  remains authoritative. Legacy source-shaped compatibility receives explicit
  head aliases from the originating views, uses identity-aware type heads and
  alpha-equivalent binders, and does not parse stable-looking text as identity.
- Constraint, evidence, deferred-obligation, module-result, and environment
  keys use `SymbolIdentity`, `TypeBinderIdentity`, `ResolvedTermIdentityKey`, or
  `DeferredRef`. Ambiguous display aliases are lookup-boundary failures, not
  semantic tie breakers. Elaboration owns one identity-keyed binding table and
  derives its type-check view. Prepared external bindings pair the external
  binding and checked scheme in one record, then derive constraint,
  elaboration, and type-check views instead of merging or synchronizing
  parallel maps.
- Backend IR constructors require identities for declarations, references,
  lexical binders, patterns, type heads, and type binders. Identity-erasing
  pattern synonyms are match-only views, and test support constructs complete
  deterministic identities rather than entering a permissive fixture IR.
- Backend conversion publishes `ProductionBackendProgram`; LLVM accepts only
  that validated capability. Structural data recovery and LLVM semantic caches
  are identity-keyed, and generated function/closure parameters are complete
  identity/name/type records at construction. Backend validator maps and LLVM
  cache-key constructors store their identities directly, without generic
  one-constructor wrappers or ignored name arguments.
- `EnvRef` compares generated `UniqueIdentity`, `PrimitiveRef` compares builtin
  `SymbolIdentity`, and `DeferredId` remains an unresolved marker rather than a
  semantic identity.

There is no production name fallback after resolution. Runtime names, source
spellings, and stable-name projections remain only at parsing, diagnostics,
source-shaped type syntax, and emission boundaries. The current focused
inventory is `docs/audit/identity-string-reference-audit.md`.

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
- Backend conversion uses resolved constructor/evidence identity directly;
  deferred references remain an explicit unfinished-finalization state and are
  rejected before a checked binding is published.
- The resolved-symbol resolver remains authoritative; finalization must not
  invent identities after checking.
- The implementation should prefer one resolved term representation over a
  long-lived pair of string and resolved xMLF terms.
