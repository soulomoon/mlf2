# `.mlfp` Resolved Symbol Identities

This note describes the enforced `.mlfp` resolve boundary. It replaces the
older audit-only model: global symbols are now represented in resolved syntax,
not only in a `resolvedModuleReferences` side table.

## Boundary

`MLF.Frontend.Syntax.Program` is phase-indexed:

- `Program 'Parsed` is parser output and may contain surface `String` names.
- `Program 'Resolved` is resolver output and stores `ResolvedSymbol` values at
  global reference sites.
- `SrcTy` has a resolved counterpart for `.mlfp` source types, so resolved type
  heads are symbols rather than `STBase`/`STCon` strings.
- Local term binders remain source names; resolved term references distinguish
  `ResolvedLocalValue` from `ResolvedGlobalValue`.

`ResolvedModule.resolvedModuleSyntax` is the syntax consumed by the checker and
elaborator. `resolvedModuleReferences` remains useful for audits, diagnostics,
and tests, but it is no longer the production carrier of resolved references.

## Identity Vs. Spelling

`ResolvedSymbol` separates semantic identity from source spelling:

- `SymbolIdentity` is the stable key: namespace, defining module/name, and an
  optional owning type/class identity for constructors and methods.
- `SymbolSpelling` records how this reference was written: local, unqualified
  import, qualified/aliased import, or builtin.

Two spellings such as `answer` and `C.answer` compare equal with
`sameResolvedSymbol` when they name the same declaration. Diagnostics and
rendering may use spelling; semantic checks must use identity.

## Checker Rules

The checker may keep visible maps keyed by surface spelling because source
lookup, diagnostics, and runtime rendering need those names. Once a name has
been resolved, semantic decisions compare identities:

- constructor result validation compares the resolved result type head identity
  against the enclosing data declaration identity, with arity checked
  separately;
- imported instance deduplication compares `(class identity, resolved head
  identity shape)`, so unqualified and aliased spellings do not create distinct
  semantic instances;
- elaboration compiles resolved expressions and patterns through
  `SymbolIdentity` indexes before lowering to eMLF surface syntax;
- deferred method finalization carries `TypeView`/`ConstraintInfo` metadata so
  method-local evidence and instance lookup use class/type identities rather
  than recovered display names;
- duplicate case branches and mixed qualified/unqualified references compare
  constructor/method/type/value identities, not display strings.

This keeps qualification as presentation and visibility data rather than a
semantic identity mechanism.
