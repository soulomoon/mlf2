# `.mlfp` Resolved Symbol Identity Audit

This note records the issue #26 audit and the minimal frontend/program boundary
model added in `MLF.Frontend.Program.Types`.

## Boundary model

`ResolvedSymbol` separates two facts that the current checker often stores in
the same string:

- `SymbolIdentity` is the stable semantic key. It contains a namespace, defining
  module, defining name, and optional owner identity for constructors and class
  methods.
- `SymbolSpelling` is reference-side surface data. It retains the source name,
  display name, and whether the spelling is local, unqualified import,
  qualified/aliased import, or builtin.

The model covers value, constructor, type, class, method, and module/import
identities. Constructors are owned by a type identity; methods are owned by a
class identity. Two visible spellings such as `answer` and `L.answer` therefore
compare equal through `sameResolvedSymbol` when they name the same declaration
from the same defining module.

This is a model-level contract only. It deliberately does not migrate checker
visibility behavior, parser grammar, or import/export semantics.

## Current qualified/unqualified branch sites

- `MLF.Frontend.Parse.Program`: `qualifiedUpperIdent` and
  `qualifiedLowerIdent` accept at most one uppercase module qualifier and then
  flatten the spelling into a `String`. The type parser reuses
  `qualifiedUpperIdent`, so source types carry qualified spellings as plain
  `STBase` or `STCon` names.
- `MLF.Frontend.Syntax.Program`: `ProgramSpanIndex` stores spans by visible
  strings for values, constructors, types, classes, import items, export items,
  modules, and import aliases. Diagnostics currently recover locations by
  visible spelling rather than semantic identity.
- `MLF.Frontend.Program.Check`: `buildImportScope` branches on `importAlias`
  and `importExposing`. Unaliased imports add exported names directly; aliased
  imports first call `qualifyModuleExports` and optionally add explicitly
  exposed unqualified names.
- `MLF.Frontend.Program.Check`: `qualifyModuleExports`, `qualifyInstance`, and
  `qualifyInstanceHeadOnly` manufacture qualified copies of exported values,
  data/type names, constructors, classes, methods, constraints, and source
  types by rewriting display strings and `SrcType` heads.
- `MLF.Frontend.Program.Check`: `unqualifiedName`, `classIdentity`,
  `methodClassIdentity`, `instanceClassIdentity`, `sameInstanceHead`, and
  duplicate/overlap checks already reconstruct semantic identity from module
  fields plus unqualified names.
- `MLF.Frontend.Program.Check`: instance visibility is split between
  `importedUnqualifiedClassIdentities`, `unqualifiedInstancesForImport`,
  `qualifiedInstancesForImport`, `instanceVisibleForUnqualifiedImport`, and
  `instanceVisibleForQualifiedImport`. These paths decide whether instances are
  visible via class imports, method-only imports, qualified aliases, and exposed
  data mentions.
- `MLF.Frontend.Program.Check`: export assembly in `buildExports`,
  `collectExportValue`, `collectExportType`, and `collectExportClass` preserves
  local exported values, abstract types, constructor-bearing types, and class
  methods with maps keyed by visible names.
- `MLF.Frontend.Program.Elaborate`: `canonicalSourceType`, `lowerTypeRaw`,
  `lookupConstructorInfoMaybe`, `constructorNameMatches`,
  `resolveInstanceInfoWithSubst`, and `resolveMethodInstanceInfoWithSubst`
  compare qualified and unqualified source type/class spellings by consulting
  checker-built scope maps and module/name pairs.
- `MLF.Frontend.Program.Finalize`: source type recovery and deferred obligation
  resolution revisit the same names through `recoverSourceType`,
  `matchDataInfoEncoding`, constructor ambiguity handling, and
  `resolveInstanceInfoWithSubst`.
- `MLF.Frontend.Program.Run`: runtime decoding uses `lookupDataInfosForType`,
  `lookupDataInfosByName`, `qualifiedDataName`, and `canonicalFieldType` to
  match qualified type names against source data declarations while rendering
  user-facing constructor names.

## Minimal next-step contract

Later resolver work should replace ad hoc string splitting at the program
boundary with `SymbolIdentity` lookups, while keeping `SymbolSpelling` available
for diagnostics and display. The checker can then stop encoding identity in
qualified copies of surface names, but that behavioral migration belongs to the
resolver and downstream-migration issues, not this audit/model issue.
