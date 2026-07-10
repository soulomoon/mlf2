# Identity/String Reference Audit

- **Created:** 2026-07-05
- **Last reviewed:** 2026-07-10
- **Status:** Implemented and verified in the 2026-07-10 working tree
- **Canonical decisions:**
  `docs/adr/2026-06-18-resolved-xmlf-identity-ir.md` and
  `docs/architecture.md`

This focused audit records post-resolution places where a string could once
decide semantic meaning even though identity metadata was available. The goal
is not to remove all strings. Source spellings, diagnostics, ABI names, runtime
symbols, parser keys, and generated labels remain strings at their owning
boundaries.

The enforced invariant is:

> Once a reference has crossed a resolved or checked boundary, semantic lookup,
> equality, substitution, rewriting, deduplication, and recovery use carried
> identity. Strings may be projected for display or emission, but cannot recover
> or override semantic meaning.

## Classification

- **ADDRESSED:** the production semantic path is identity-complete and
  fail-closed.
- **BOUNDARY:** the string is owned by parsing, display, diagnostics, ABI/runtime
  emission, or generated-code naming and does not decide semantic meaning.
- **REPRESENTATION DEBT:** production is fenced correctly, but a shared type
  still admits explicit metadata-light/test values.

## Current Status

| Area | Status | Current invariant |
| --- | --- | --- |
| Evidence keys and lookup | **ADDRESSED** | Class applications and evidence methods use identity-bearing `TypeView` keys. |
| Checked `TypeView` completeness | **ADDRESSED / REPRESENTATION DEBT** | Checked programs are mention-sensitively validated; `mkTypeView` remains available for explicit metadata-light construction. |
| Constructor visible-type rewriting | **ADDRESSED** | Rewriting selects the constructor owner by `SymbolIdentity`. |
| Resolved frontend binder transport | **ADDRESSED / BOUNDARY** | `EBinderIdentity` carries exact binder identity through the string-shaped surface pipeline. |
| Constraint/elaboration binding environments | **ADDRESSED / BOUNDARY** | Resolved references use `BindingKey`; graph-local binders use scope-derived `LocalRef` identity. |
| Opaque checked finalization | **ADDRESSED** | Every checked-binding path rejects remaining `DeferredId` references. |
| Deferred constructor/case obligations | **ADDRESSED** | Obligations retain `TypeView`s and substitutions are keyed by `TypeBinderIdentity`. |
| Constructor result abstraction/recovery | **ADDRESSED** | Structural result binders have an owner-derived identity and source recovery requires one unambiguous owner. |
| Type equality and matching | **ADDRESSED / REPRESENTATION DEBT** | Production defaults are identity-only; metadata-light comparison is explicit at its owning adapter. |
| Primitive and structural type lowering | **ADDRESSED / BOUNDARY** | Primitive binders and lowered display/identity projections retain exact head and binder identities. |
| Backend production references | **ADDRESSED / REPRESENTATION DEBT** | `ProductionBackendProgram` proves the identity-complete boundary; shared IR still supports explicit metadata-light fixtures. |
| Structural conversion | **ADDRESSED / BOUNDARY** | Production uses owner identity; structural-name recovery is confined to explicit metadata-light conversion. |
| LLVM semantic keys and provenance | **ADDRESSED / BOUNDARY** | Semantic caches and provenance use identities; emitted/generated LLVM names remain strings. |

## 1. Evidence Keys Preserve `TypeView` Identity

Status: **ADDRESSED**.

Owner:

- `src/MLF/Frontend/Program/Types.hs :: ClassApplicationKey`
- `src/MLF/Frontend/Program/Types.hs :: EvidenceMethodKey`
- `src/MLF/Frontend/Program/Types.hs :: constraintClassApplicationKey`
- `src/MLF/Frontend/Program/Types.hs :: evidenceMethodKey`

Elaboration, finalization, and runtime now share these keys. They retain the
normalized identity structure, head identities, and binder identities carried
by each `TypeView`; no production lookup projects arguments to `[SrcType]` and
then rebuilds metadata-light views.

This makes stale display text harmless and makes conflicting identity payloads
unequal. Recursive superclass cycle/deduplication checks obey the same key rule
as method lookup.

Resolved overloaded calls also distinguish identity-ground arguments from
still-polymorphic constraint variables. A failed exact substitution is reported
as ambiguous only for identity-complete ground views; a generic instance body
remains deferred instead of being rejected or recovered by a type spelling.

Focused coverage lives in `test/ResolvedSymbolSpec.hs`, including stale identity
spellings and conflicting carried payloads.

## 2. Checked `TypeView` Completeness Is Enforced

Status: **ADDRESSED / REPRESENTATION DEBT**.

Owner:

- `src/MLF/Frontend/Program/Types.hs :: TypeViewIdentityGap`
- `src/MLF/Frontend/Program/Types.hs :: typeViewIdentityGaps`
- `src/MLF/Frontend/Program/Check.hs :: validateCheckedProgramTypeViews`
- `src/MLF/Backend/Convert.hs :: convertCheckedProgram`

Completeness is mention-sensitive: every semantic type head and binder present
in a checked view must have identity evidence, while `STBottom` and other shapes
with no semantic reference require none. Canonical builtin heads are recognized
through their builtin identities.

The validator covers binding views, data and constructor views, class and
instance views, exports, evidence metadata, and deferred method/constructor/case
payloads. It runs when the checked artifact is produced and again before backend
conversion. Missing identity is a structured error, not an invitation to recover
by spelling.

`mkTypeView` remains useful for parser-side or explicit metadata-light fixtures;
that constructor availability is representation debt, not a production fallback.

Focused coverage is in `test/ProgramSpec.hs` and `test/BackendConvertSpec.hs`.

## 3. Constructor Rewriting Uses Owner Identity

Status: **ADDRESSED**.

Owner:

- `src/MLF/Frontend/Program/Elaborate.hs :: constructorVisibleTypeView`
- `src/MLF/Frontend/Program/Elaborate.hs :: rewriteConstructorOwnerHeadDisplays`
- `src/MLF/Frontend/Program/Check.hs :: rewriteOwnerTypeHeads`

Visible constructor-type rewriting no longer performs whole-tree replacement by
plain `SrcType` equality. It rewrites occurrences owned by the exact data
`SymbolIdentity`, preserving distinct same-spelled heads and their sidecars. The
display tree is projected only after semantic selection.

## 4. Resolved Binder Identity Crosses the Surface Boundary Directly

Status: **ADDRESSED / BOUNDARY**.

Owner:

- `src/MLF/Frontend/Syntax.hs :: EBinderIdentity`
- `src/MLF/Frontend/Program/Surface.hs :: surfaceBinderIdentity`
- `src/MLF/Frontend/Normalize.hs`
- `src/MLF/Frontend/Desugar.hs`
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- `src/MLF/Frontend/Program/Finalize.hs`

`SurfaceExpr` remains the source-shaped, string-bearing parser/elaboration
carrier. An internal `EBinderIdentity IdDetails` wrapper now transports the
resolved binder through normalization, desugaring, constraint generation, and
elaboration into the exact `ALam`/`ALet` node. Finalization therefore does not
reconstruct local binder meaning from `Map String IdDetails` or a runtime-name
sidecar.

Generated handler parameters use fresh `LocalRef`s. Source pattern variables are
introduced by branch-local identity-bearing lets. Deferred evidence matching uses
`resolvedVarSameIdentity`; strings remain only runtime/display projections.

Global references still cross the surface carrier as stable aliases, but entry
into and exit from that boundary are identity-indexed and fail closed. The
remaining string is a carrier spelling, not executable identity.

## 5. Opaque and Deferred Finalization Fail Closed

Status: **ADDRESSED**.

Owner:

- `src/MLF/Frontend/Program/Finalize.hs :: unresolvedXmlfTermVarRefs`
- `src/MLF/Frontend/Program/Finalize.hs :: finalizeOpaqueUncheckedBindingWithContext`
- `src/MLF/Frontend/Program/Types.hs :: TypeBinderSubst`
- `src/MLF/Frontend/Program/Types.hs :: DeferredProgramObligation`

Opaque unchecked finalization now runs the same unresolved-`DeferredId` rejection
as ordinary checked binding finalization.

Deferred constructor and case obligations retain identity-bearing occurrence and
source `TypeView`s. `TypeBinderSubst` stores replacement `TypeView`s by
`TypeBinderIdentity`; display-name maps are derived only for the explicit
string-shaped application boundary and ambiguous aliases are discarded rather
than selected arbitrarily. Quantified specialization removes substituted
foralls without dropping the remaining binder metadata.

## 6. Structural Result Binders and Source Recovery Have Owners

Status: **ADDRESSED**.

Owner:

- `src/MLF/Types/Identity.hs :: StructuralResultBinder`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Frontend/Program/Elaborate.hs :: dataStructuralResultBinderIdentity`
- `src/MLF/Frontend/Program/Finalize.hs :: recoverElabSourceType`

The constructor-result abstraction no longer relies on a `$name_result` prefix
as semantic identity. Its `TypeBinderIdentity` is derived from the owning data
`SymbolIdentity` plus the `StructuralResultBinder` role.

`recoverElabSourceType` accepts a recovered nominal source type only when exactly one
`DataInfo` owner matches the identity-bearing structural shape. Ambiguous or
conflicting candidates remain unrecovered; list order cannot choose an owner.

The former owner-unavailable `recoverSourceType` entrypoint was removed. The
only string-only recovery API is the explicitly named
`recoverSourceTypeMetadataLight` fixture adapter, so production callers cannot
silently select the fail-closed but identity-free mode.

## 7. Backend Production Boundary Is Identity-Complete

Status: **ADDRESSED / REPRESENTATION DEBT**.

Owner:

- `src/MLF/Backend/IR.hs :: ProductionBackendProgram`
- `src/MLF/Backend/IR.hs :: mkProductionBackendProgram`
- `src/MLF/Backend/Convert.hs :: convertCheckedProgram`
- `src/MLF/Backend/LLVM.hs`
- `src/MLF/Backend/StructuralRecursiveData.hs`

Checked conversion returns `ProductionBackendProgram`, not an unchecked
`BackendProgram`. Construction validates module/data/constructor/binding,
lexical term, closure, type-head, and type-binder identities. LLVM entrypoints
accept only that capability wrapper, so production lowering cannot accidentally
take a metadata-light program.

Structural recursive conversion uses data-owner identity, parameter identity,
and explicit nominal/structural boundary checks. Generated and lexical type
variables are not matched by shared spelling. Metadata-light structural-name
recovery remains an explicitly named adapter for fixtures and boundary input.

Shared Backend IR constructors still use `Maybe identity` and a name-keyed
substitution alternative for explicit metadata-light tests. Splitting those
representations can make invalid states unrepresentable later, but production
behavior is already fail-closed.

## 8. LLVM Uses Identity for Semantics

Status: **ADDRESSED / BOUNDARY**.

Owner:

- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Backend/LLVM/Lower/Types.hs`

Specialization, wrapper, closure-entry, local-function, constructor-field, and
semantic cache keys include their carried identities. Explicit type
substitutions use `TypeBinderIdentity`; same-spelled generated binders do not
match. Closure capture/parameter provenance is identity-bearing before equality,
deduplication, or lookup.

LLVM symbols, SSA names, block labels, runtime declarations, and generated
closure-entry spellings remain strings because they are emitted-code names. They
do not recover a source semantic reference.

## 9. Constraint and Elaboration Environments Are Identity-Keyed

Status: **ADDRESSED / BOUNDARY**.

Owner:

- `src/MLF/Frontend/ConstraintGen/Types.hs :: BindingKey`
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- `src/MLF/Elab/Elaborate/Algebra.hs`
- `src/MLF/Types/Identity.hs :: ScopedGraphLocalId`

Resolved surface references, binders, free-reference discovery, external-binding
caches, annotation lookup, alias following, and elaboration environments use a
`ResolvedBindingKey ResolvedTermIdentityKey`. The key survives through
`AResolvedVar` and is not reconstructed from the variable's runtime spelling.

Raw metadata-light expressions still use `MetadataLightBindingKey`, but that
constructor is confined to the explicit pre-resolved/test boundary. When raw
graph elaboration must synthesize lexical locals, `ScopedGraphLocalId` derives a
distinct `LocalRef` from the graph node and binder ordinal. Two same-spelled
locals therefore cannot alias through a shared generated name.

## 10. Production Type Equality Is Identity-Only

Status: **ADDRESSED / REPRESENTATION DEBT**.

Owner:

- `src/MLF/Reify/TypeOps.hs :: alphaEqType`
- `src/MLF/Reify/TypeOps.hs :: typeHeadMatches`
- `src/MLF/Reify/TypeOps.hs :: alphaEqTypeMetadataLight`
- `src/MLF/Elab/Generalize.hs :: shadowCompareTypes`

The production defaults compare free type binders and nominal heads by carried
identity. Two metadata-free heads with equal text are not production-equal.
Callers that intentionally compare graph-reified metadata-light types name that
choice explicitly through `alphaEqTypeMetadataLight` and
`typeHeadMatchesMetadataLight`; generalization's shadow comparator is one such
owner-local adapter. This keeps compatibility behavior visible instead of
silently weakening every type comparison.

## 11. Primitive and Lowered Types Retain Identity

Status: **ADDRESSED / BOUNDARY**.

Owner:

- `src/MLF/Primitive/Inventory.hs :: primitiveTypeToElabTypeFrom`
- `src/MLF/Frontend/Program/Builtins.hs :: builtinValueTypeBinderIdentities`
- `src/MLF/Frontend/Program/Elaborate.hs :: lowerTypeViewWithIdentities`
- `src/MLF/Frontend/Program/Finalize.hs :: lowerExternalTypeViews`
- `src/MLF/Backend/IR.hs :: primitiveTypeToBackendTypeFromWithHeadIdentities`

Primitive specifications remain a compact string-shaped inventory, but their
conversion allocates `TypeBinderIdentity` values for free, forall, and μ
binders, attaches builtin `SymbolIdentity` values to heads, and seeds fresh
identities after every supplied identity. Builtin `TypeView`s publish those
binder sidecars rather than treating a stable-looking spelling as proof.

Structural lowering maintains separate display and identity projections. Data
parameter substitutions are selected from resolved binder aliases, constructor
field lowering follows the exact transitive data-identity closure, and
view-provided head aliases are admitted only after resolving their carried
`SymbolIdentity` to an in-scope `DataInfo`. This preserves identities for hidden
constructor-field types and stale display names without creating a name
fallback. Backend conversion consumes the resulting complete `TypeView` and
fails closed if any mentioned head or binder still lacks identity.

## 12. Structure Simplification Review

The implementation review removed three avoidable parallel paths:

- resolved `TypeView` comparison hydrates known head and binder identities once
  in `ensureTypeViewCompatible`, rather than requiring each resolved-expression
  caller to add builtin/scope identities independently;
- evidence-method consumers share one resolved-variable requirement, so the
  missing-identity failure is owned in one place; and
- structural source recovery has no owner-unavailable production entrypoint.
  Production uses `recoverElabSourceType`; string-only fixtures must opt into
  `recoverSourceTypeMetadataLight` explicitly.

Two larger reductions remain representation changes rather than safe local
cleanup: replace the string-shaped `SurfaceExpr` carrier with a resolved carrier,
and split metadata-light Backend IR fixtures from identity-complete production
IR constructors. The checked and production gates already fence both seams, so
this audit does not add another compatibility abstraction around them.

## Expected String Boundaries

The following remain intentionally string-bearing:

- parser and resolver name lookup before identities exist;
- source spellings and aliases used for diagnostics;
- pretty-printer output;
- runtime/ABI symbol projection from an identity-bearing declaration;
- LLVM symbols, SSA names, and block labels; and
- explicit metadata-light test adapters.

A future audit should treat a string here as a defect only if a new consumer uses
it for post-resolution semantic selection, equality, recovery, or substitution.

## Verification Notes

Focused identity, Program finalization, constraint generation, elaboration,
pipeline, Backend IR, structural recursive data, and Backend conversion suites
pass in the 2026-07-10 working tree. In particular, Program source-type
finalization passes 197/197, Backend conversion passes 151/151, Reify TypeOps
passes 53/53, and the generalization shadow comparator passes 12/12.

The required completion gate passes without raising the default file-descriptor
limit:

```sh
cabal build all -j1
cabal test -j1
```

The full suite completed 3386 examples with 0 failures.
