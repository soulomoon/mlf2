# ADR: Make Checked xMLF Carry Resolved Identity

Date: 2026-06-18
Status: Accepted

## Context

The `.mlfp` resolver already assigns semantic `SymbolIdentity` values to
global source references. The checker, elaborator, and backend then lose part of
that provenance when executable terms become `ElabTerm`, because `EVar` and
binders store plain `String` names. Later phases recover constructor, method,
primitive, and ordinary binding meaning from side maps such as `ValueInfo`,
`DataInfo`, `CheckedModule`, and `ConvertContext`.

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
```

The exact field names may change during implementation, but the invariant may
not: a checked constructor occurrence must carry enough identity to decide that
it is a constructor without consulting a string-keyed value environment, and
must carry enough metadata to recover its owner, order, field types, result
type, and backend constructor identity without shape guessing.

`ElabTerm` should move from `EVar String` and string binders toward resolved
occurrences and typed binders. Local binders may remain compact, but local
occurrences must identify the binder by a local reference rather than by an
unqualified spelling when the term has crossed into checked xMLF. Source
spelling remains diagnostic metadata, not executable identity.

The checked module remains the owner of declarations:

- `CheckedModule` still carries data, class, instance, and export metadata.
- Resolved term occurrences point at that metadata by semantic identity or
  stable constructor reference.
- Backend conversion may still build backend `DataMeta` and `ConstructorMeta`,
  but it must use resolved term details instead of reclassifying `EVar String`
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

As of 2026-06-18, the first checked-binding slice is implemented:

- `LoweredBinding` carries `LoweredBindingIdentity`.
- `CheckedBinding` carries `ResolvedVar`.
- Constructor bindings carry `ConstructorId ConstructorRef`.
- Backend constructor-binding synthesis consumes that checked constructor
  identity.

This does not complete the ADR. `ElabTerm` occurrences and local binders remain
string-based during the migration, and constructor applications still need to
move to the same resolved identity path.

## Performance Expectation

Resolved identity alone is not expected to make `.mlfp` checking much faster.
The speedup comes from the simplifications it enables:

- constructor bindings can be finalized from metadata plus a small checked-term
  guard instead of always running through the full surface pipeline;
- backend conversion can lower constructor applications directly from
  `ConstructorId`, avoiding string recovery and structural shape guessing;
- dependency and closure scans can compare resolved references instead of
  spelling-derived names.

The accepted performance claim for this design is not "resolved variables make
everything fast." The measurable target is that constructor-heavy packages
spend less time in `program.check.module.*.constructor-bindings` once the
metadata-derived constructor path is implemented.

## Rejected Alternatives

- Keep `EVar String` as the checked xMLF executable identity. This preserves
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

- `MLF.Types.Elab` will no longer be the smallest string-based xMLF syntax once
  the migration reaches checked programs. If a paper-facing minimal xMLF syntax
  is still needed, it should be a separate parse/pretty surface or erasure view.
- Tests that inspect `show checkedBindingTerm` by spelling will need to assert
  semantic identity or rendered diagnostics instead.
- Backend conversion should get smaller around constructor recovery but will
  need an explicit bridge while old `ElabTerm` producers remain.
- The resolved-symbol resolver remains authoritative; finalization must not
  invent identities after checking.
- The implementation should prefer one resolved term representation over a
  long-lived pair of string and resolved xMLF terms.
