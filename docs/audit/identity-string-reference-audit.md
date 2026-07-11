# Identity/String Reference Audit

- **Created:** 2026-07-05
- **Last reviewed:** 2026-07-11
- **Status:** Implemented and verified in the 2026-07-11 working tree
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
| Checked `TypeView` completeness | **ADDRESSED / REPRESENTATION DEBT** | One node-level tree owns spellings and identities; display/identity types and alias indexes are read-only projections, production mutation is structural, and metadata-light construction is test-local. |
| Constructor visible-type rewriting | **ADDRESSED** | Rewriting selects the constructor owner by `SymbolIdentity`. |
| Resolved frontend binder transport | **ADDRESSED / BOUNDARY** | `EVar`/`ELam`/`ELet`/`ELamAnn` nodes carry `MetadataLightTermReference` or `ResolvedTermReference IdDetails` directly. |
| Constraint/elaboration binding environments | **ADDRESSED / BOUNDARY** | Resolved references use `BindingKey`; derived-instance synthesis, lowering, and constraint generation share the caller-owned identity supply, and graph-derived locals retain graph provenance through capture avoidance. |
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
normalized identity structure carried by each `TypeView`; no production lookup
projects arguments to `[SrcType]` and then rebuilds metadata-light views.
`TypeView` stores one node-level tree. Each head or binder node owns its display
spelling, identity spelling, semantic payload, and relevant aliases; the public
display/identity projections and lookup maps are derived views rather than four
independently mutable fields. Context head/binder nodes retain identities needed
by projected constructor views even when the visible source type no longer
mentions them. Type substitution and quantified specialization walk node
identities directly, so same-spelled binders are never selected by map order.

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
- `src/MLF/Frontend/Program/Checked.hs :: mkCheckedProgram`
- `src/MLF/Frontend/Program/Checked/Internal.hs :: CheckedProgram`
- `src/MLF/Backend/Convert.hs :: convertCheckedProgram`

Completeness is mention-sensitive: every semantic type head and binder present
in a checked view must have identity evidence, while `STBottom` and other shapes
with no semantic reference require none. Canonical builtin heads are recognized
through their builtin identities.

One recursive constructor validates each binding's `TypeView` and `ElabType`
payloads, then data and constructor views, class and instance views, exports,
evidence metadata, and deferred method/constructor/case payloads. The public
`CheckedProgram` type is abstract and its read accessors are not record fields,
so production callers cannot bypass `mkCheckedProgram` with record update.
Runtime and backend conversion consume that certificate without repeating the
same traversal. `Checked.Internal` exists only for owner code and explicit
invalid test fixtures. Missing identity is a structured construction error, not
an invitation to recover by spelling.

The production module no longer exports the partial `mkTypeView` constructor or
a bidirectional record pattern. Explicit metadata-light fixtures use
`test/TypeViewTestSupport.hs`; the underlying shared `TypeView` can still carry
missing payloads before the checked completeness gate, which is the remaining
representation debt.

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

- `src/MLF/Frontend/Syntax.hs :: TermReference`
- `src/MLF/Frontend/Syntax.hs :: EVarNode / ELamNode / ELetNode / ELamAnnNode`
- `src/MLF/Frontend/Normalize.hs`
- `src/MLF/Frontend/Desugar.hs`
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- `src/MLF/Frontend/Program/Finalize.hs`

`SurfaceExpr` remains source-shaped, but variable and binder nodes now own a
`TermReference` directly. Parser input constructs `MetadataLightTermReference`;
resolved lowering constructs `ResolvedTermReference IdDetails displayName`.
Normalization and desugaring preserve that payload, constraint generation
derives `BindingKey` from it once, and finalization no longer branches through or
reconstructs an identity wrapper.

Generated handler parameters use fresh `LocalRef`s. Source pattern variables are
introduced by branch-local identity-bearing lets. Deferred evidence matching uses
`resolvedVarSameIdentity`; strings remain only runtime/display projections.

Global and local resolved references cross the carrier with their semantic
identity and a display/runtime spelling. The string is a projection, not
executable identity.

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
take a metadata-light program. The raw projection is owner-internal to LLVM
lowering (with an explicit test-support import); metadata-light inputs use the
separate `BackendProgramFixture` capability in `MLF.Backend.IR.Fixture`.

`validateBackendProgram` is now the identity-complete default. The permissive
core is explicitly named `validateBackendProgramMetadataLight`; tests reach it
through `BackendProgramFixture` and
`test/BackendIRTestSupport.hs :: validateBackendProgramFixture`.
`BackendValidationContext` stores one tagged key map/set per namespace; it no
longer keeps parallel name and identity collections or a mode flag.

Structural recursive conversion uses data-owner identity, parameter identity,
and explicit nominal/structural boundary checks. Generated and lexical type
variables are not matched by shared spelling. Metadata-light structural-name
recovery remains an explicitly named adapter for fixtures and boundary input.

Shared Backend IR constructors still use `Maybe identity` and type substitution
retains an explicit metadata-light name key for fixture types. Splitting those
representations can make invalid states unrepresentable later, but production
behavior is already fail-closed.

All reference matchers share `MLF.Types.Reference.ReferenceMode`; backend term,
type, closure-entry, callable, symbol, and `IdDetails` paths no longer maintain
parallel two-constructor mode enums or conversion functions. Callable-head
classification also pattern-matches `BackendExpr` directly instead of routing
through a one-instance typeclass and adapter view.

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

Structural lowering follows the identity-bearing semantic projection. When a
stale display head and its identity-bearing lowering produce different shapes,
the visible lowered projection is rebuilt from the identity shape instead of
publishing two incompatible trees. Data parameter substitutions are selected
from resolved binder identities, constructor field lowering follows the exact
transitive data-identity closure, and view-provided head aliases are admitted
only after resolving their carried `SymbolIdentity` to an in-scope `DataInfo`.
This preserves identities for hidden constructor-field types and stale display
names without creating a name fallback. Backend conversion consumes the
resulting complete `TypeView` and fails closed if any mentioned head or binder
still lacks identity.

## 12. Structure Simplification Review

The 2026-07-11 implementation pass removed the remaining avoidable parallel
paths identified by this audit:

- `TypeView` now stores one identity-bearing node tree; its two source-type
  projections and head/binder alias indexes are read-only, and explicit
  transforms replace a partial bidirectional record pattern;
- `TypeViewSubst` is keyed directly by `TypeBinderIdentity`, without a
  one-constructor key wrapper or conversion adapter;
- surface term nodes carry metadata-light or resolved references directly, so
  the wrapper constructor, the forwarding `Program.Surface` module, and every
  wrapper-specific traversal branch are gone;
- one shared `ReferenceMode` and matcher policy replaces the parallel symbol,
  `IdDetails`, backend type/term, closure-entry, callable, type-bound, and
  validation mode types;
- backend validation uses tagged reference keys rather than parallel name and
  identity maps/sets; production validation is the default, while the
  metadata-light path is test-local;
- `Backend.IR` classifies `BackendExpr` directly, with no one-instance callable
  typeclass or intermediate expression view; callable-head collapse takes
  `NonEmpty`, and unknown closure references use `Maybe` instead of a sentinel;
- structural head metadata is a direct `Map String SymbolIdentity`, without a
  one-field wrapper;
- checked identity validation is one recursive program traversal covering both
  `TypeView` and `ElabType` payloads; and
- trivial forwarding aliases across backend conversion, IR, checking,
  elaboration, and pipeline code were inlined and deleted.

The remaining representation debt is narrower: raw Backend IR constructors
still admit explicit metadata-light fixtures alongside the validated
`ProductionBackendProgram` path. The production capability gate already fences
that seam, so no compatibility layer was added.

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

Focused identity, Program finalization, Backend IR, and Backend conversion suites
pass in the 2026-07-11 working tree. In particular, resolved-symbol identity
coverage passes 65/65, Program source-type finalization passes 197/197, Backend
IR passes 123/123, and Backend conversion passes 151/151.

The required completion gate passes without raising the default file-descriptor
limit:

```sh
cabal build all && cabal test
```

The full suite completed 3386 examples with 0 failures.
