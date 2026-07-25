# Identity/String Reference Audit

- **Created:** 2026-07-05
- **Last reviewed:** 2026-07-25
- **Status:** Implemented in the current working tree
- **Canonical decisions:**
  `docs/adr/2026-06-18-resolved-xmlf-identity-ir.md` and
  `docs/architecture.md`

This audit covers places where a string could decide semantic meaning after a
reference had already been resolved. The target is not a string-free compiler.
Source spellings, parser lookup, diagnostics, runtime symbols, ABI names, LLVM
names, and generated labels remain strings at their owning boundaries.

The enforced invariant is:

> After resolution, semantic lookup, equality, substitution, rewriting,
> deduplication, recovery, and provenance use carried identity. A string may be
> projected for display or emission, but cannot create, recover, replace, or
> override semantic identity.

## Result

The post-resolution production path is identity-first and correct by
construction. There is no shared metadata-light semantic IR and no production
fallback that repairs missing identity from a spelling.

| Area | Current construction invariant |
| --- | --- |
| Surface expressions | `Expr` is indexed by `RawTermReferences` or `ResolvedTermReferences`; resolved nodes can only contain `ResolvedTermReference IdDetails name`. |
| Checked xMLF | Every executable occurrence and lambda/let binder in `XmlfTerm` contains `ResolvedVar`; no unresolved constructor alternative exists. |
| Source type views | Abstract `TypeView` stores one node tree whose semantic heads and binders carry `SymbolIdentity` or `TypeBinderIdentity`; missing payloads fail construction. |
| Constraints and evidence | `ConstraintInfo`, `ClassApplicationKey`, `EvidenceMethodKey`, and `EvidenceMethod` carry class, type, method, and executable identities directly. |
| Lowered and checked bindings | `LoweredBinding` requires source and expected `TypeView`s plus a closed `LoweredBindingIdentity` (`top-level`, `constructor`, or `method`); local, environment, and deferred identities cannot be constructed as module bindings. `CheckedBinding` requires `ResolvedVar`, `TypeView`, `ElabType`, and `XmlfTerm`. |
| Backend IR | Every constructible module, data, constructor, binding, term reference, lexical binder, pattern binder, type head, and type binder carries identity. Identity-erasing patterns are match-only views. |
| Structural data | Nominal/structural matching is pinned by owner identity and binder identity; a same-spelled wrong owner fails closed. |
| Compiler-exact binders | Packet preparation records a unique identity-bearing source-to-construction quotient. Gamma publication, bounded-binder adoption, body-consumer projection, and consumer specialization require that route plus the exact endpoint; missing, ambiguous, wrong-source, or position-only matches fail closed. The body term, checked source type, and lookup environment cross the quotient together. Structured operated endpoints retain free graph identities as dependencies rather than aliasing them to the consumer. The endpoint's declaration identity decides whether an already-source-aligned binder remains quantified or is inherited free, and packet placement distinguishes whole-packet matches from body-only matches that must retain the exact bound. |
| Edge proof packets | Production edge execution commits expansion, witness, Raise authority, non-source origins, raw construction roles, and trace in one `EdgeExecutionArtifacts` value. Equal replay is idempotent; a conflicting field is rejected. |
| Φ computations | Opaque `QuantifierReordering`, `EdgeTranslation`, and `OccurrenceComputation` values retain validated φ_R, T(e), and their composition. Component endpoints are alpha-checked, while the composition seam requires identity-bearing type equality. |
| LLVM lowering | Semantic environments and caches are identity-keyed. Function, wrapper, closure-entry, capture, and parameter records combine identity, name, and payload instead of storing parallel lists. |

## Construction boundaries

### Parser to resolved surface

`MLF.Frontend.Syntax.TermReference` has phase-indexed constructors:

- `RawTermReference` is available only before resolution;
- `ResolvedTermReference` requires `IdDetails`.

`MLF.Frontend.TermResolve.resolveTermReferences` allocates lexical identities
while crossing that boundary and rewrites every occurrence from the lexical
environment. Normalization, desugaring, and constraint generation preserve the
resolved carrier. `BindingKey` is therefore only a
`ResolvedTermIdentityKey`; it has no string-keyed semantic alternative.

### Resolved surface to checked xMLF

`MLF.Types.Elab.XmlfTerm` stores `ResolvedVar` in `EVarNode`, `ELam`, and
`ELet`. Deferred work uses explicit `DeferredRef` identities and normal plus
opaque finalization reject a remaining `DeferredId`. The retired late
`annotateResolvedTermVars` repair pass is not part of the pipeline.

`LoweredBinding` carries required source and expected `TypeView`s. Constraint
metadata is represented once by identity-bearing `ConstraintInfo`; evidence
methods require both their method `SymbolIdentity` and executable
`ResolvedVar`. Constructor forall identities and backend constructor metadata
are derived from the constructor's `TypeView`, rather than stored as parallel
string sidecars. Resolved annotations are projected from the carried `TypeView`
with stable identity spellings; display-name freshening can rename only the
display component and never allocates a replacement identity. Deferred case
results are constructed directly from their identity-bearing result view.

`LoweredBindingIdentity` is a closed sum. Its public constructors accept only a
top-level symbol, a constructor, or a method; the former generic
`ResolvedVar -> LoweredBindingIdentity` route was removed because it could
admit `LocalId`, `EnvId`, or `DeferredId` at a module-binding boundary.

### `TypeView`

`TypeView` is abstract. Its private node tree stores display spelling together
with the semantic payload at each head or binder. Public source-shaped display
and stable-name projections are derived views, not semantic authorities. It has
no cached identity spelling and no second identity-shaped source tree.

The source boundary uses `typeViewFromSourceType`: one source shape plus head
and binder identity aliases. It returns an explicit missing/ambiguous payload
error instead of accepting parallel display/identity trees and zipping them.
`typeViewWithIdentityAliases` may enrich lookup aliases, but cannot replace a
node payload.

`typeViewFromSourceTypeInScope` constructs that identity-bearing tree before
choosing an import-visible display spelling. Qualification such as `Core.Box`
to `C.Box` is therefore a payload-preserving display transform, not a string
rewrite followed by identity recovery. Source-shaped compatibility checks use
the same identity-aware type-head relation plus binder alpha-equivalence. When
a stable spelling crosses a legacy source-shaped adapter, the originating
`TypeView` supplies its identity alias map; the string-only builtin resolver
continues to reject stable-looking text without that payload.

Production consumers use `typeViewNodeView`, `typeViewToResolved`, and direct
structural traversal for matching, free-binder collection, elaborated-type
conversion, and backend conversion. These operations no longer project a
`SrcType`, recover identity from alias maps, or zip a string tree back together
with identity metadata. `freeTypeBinderIdentitiesTypeView` is total because an
identity-free semantic node cannot be constructed.

Substitution, specialization, and subtree projection also operate on the node
tree directly. They retain carried aliases and explicit lexical binder-context
nodes, and delete obsolete context while rebuilding from the surviving
payloads; they do not filter identity maps by projected string names.

`typeViewWithDisplay` may change display text while preserving the node payload
and shape. `TypeViewSubst` and `TypeBinderSubst` are both direct
`Map TypeBinderIdentity TypeView` views. The former string-alias sidecar and
string-shaped substitution adapter were deleted. Display alias maps exist only
for source-shaped lookup boundaries; ambiguous aliases are omitted.

### Backend IR and structural matching

Backend IR constructors require identity. Unidirectional patterns such as
`BackendVar`, `BTVar`, and `BackendConstructor` erase identity only while
matching or rendering; test support supplies deterministic identities when it
constructs compact fixtures. The deleted `MLF.Backend.IR.Fixture`,
`MLF.Types.Reference`, `ReferenceMode`, and permissive metadata-light validator
are not compatibility paths.

`validateBackendProgram` checks closed-program relationships and local typing;
`ProductionBackendProgram` records that validation before LLVM lowering. It is
not a repair capability and never fills an identity from a name.

Validation contexts store `Map SymbolIdentity` and `Map BackendLocalKey`
directly. The generic one-constructor reference-key wrapper was removed because
the owning record fields already separate namespaces. Backend type substitution
uses `TypeBinderIdentity` directly under its domain type alias.

`MLF.Backend.StructuralRecursiveData` and the corresponding IR/conversion
adapters match nominal heads, recursive self binders, constructor owners, and
type substitutions by carried identity. Error paths preserve the offending
identity instead of canonicalizing it from a same-spelled declaration.

### LLVM lowering

LLVM/runtime strings remain emitted names. Source semantic selection uses
`BackendBindingRef`, `LowerLocalKey`, constructor identity keys, type-binder
substitution keys, callable identities, and closure-entry identities.

`FunctionParam`, `ClosureCaptureSlot`, `FunctionForm`, `Wrapper`, and
`ClosureEntry` keep identity, display name, type, and value kind in combined
records. Generated parameters are constructed in one traversal from their
types. Returned-partial closure seeds store supplied `(BackendType,
LowerValueKind)` pairs, and identity assignment produces complete capture and
parameter records directly; no equal-length name/type/identity lists are later
zipped into semantic state.

LLVM expression/type cache keys place identities directly in their typed key
constructors. The old `identityRefKey identity name` helper ignored `name`; it
and its one-constructor wrapper were removed so the key type itself demonstrates
that display spelling cannot participate in equality.

### Elaboration environments

`MLF.Elab.Elaborate.Algebra.Env` owns one map keyed by
`ResolvedTermIdentityKey`. It no longer keeps a second runtime-name map or a
separately synchronized type-check environment; the type-check view is derived
from the authoritative bindings. Shadowing therefore cannot delete or replace
an outer semantic identity merely because a display spelling is reused.

`PreparedExternalBindings` stores one alias-indexed
`PreparedExternalBinding` record containing the external binding and its
checked scheme together. Restriction and preferred/fallback union therefore
cannot pair a binding identity from one source with a scheme from another. Its
constraint-generation bindings, elaboration bindings, and type-check
environment are pure derived views, rather than independently merged maps or
caches that every operation must update in lockstep.

Transparent let construction composes the RHS packet's completed Gamma into
the enclosing scheme by `TypeBinderIdentity` before freshening. The completed
identities and their bound-dependency closure remain protected through both
freshening boundaries; ambiguous routes and conflicting concrete bounds are
rejected during construction rather than reconciled after term creation.

## Intentional string boundaries

The remaining string-bearing boundaries are:

- parser and resolver lookup before identity exists;
- source spellings and aliases used to resolve input or render diagnostics;
- stable-name projections used to pass through legacy source-shaped type
  syntax, always accompanied by the carried payload;
- runtime and ABI symbol projection from an identity-bearing declaration;
- LLVM symbols, SSA names, block labels, and generated helper labels.

A future audit should report a string only when it selects post-resolution
semantic meaning, or when independent parallel fields can encode a mismatched
identity/name/payload relationship that a constructor could have prevented.

## Removed repair and compatibility surfaces

The implementation removed the following classes of invalid state:

- optional resolved identity on checked xMLF variable nodes;
- late term-reference annotation and deferred-identity stamping passes;
- metadata-light/backend fixture constructors in production modules;
- name-or-identity matching modes and name fallback in semantic matchers;
- duplicate constraint display/type sidecars;
- optional source/expected `TypeView`s on lowered bindings;
- constructor forall identity sidecars independent of the constructor view;
- parallel display/identity type projections and projected-name alias repair;
- string-alias sidecars on identity-keyed type substitutions;
- independently merged external-binding/scheme maps, parallel elaboration
  name/identity maps, and prepared type-check caches;
- generic one-constructor identity-key wrappers in backend validation and LLVM
  semantic cache keys;
- source-type projection plus alias-recovery backend conversion; and
- parallel LLVM function-parameter and returned-partial capture lists.

## Verification

Focused resolved-symbol, Program finalization, TypeCheck, Backend IR, Backend
conversion, and LLVM suites cover stale spellings, conflicting payloads,
same-spelled distinct binders, wrong structural owners, identity-preserving
error paths, generated closure parameters, stable builtin aliases, and aliased
import display projection. Compiler-exact packet tests additionally cover
missing and wrong source routes, bounded-endpoint mismatch, unrelated leading
foralls, endpoint-declared versus inherited source binders, an already-published
exact consumer argument, whole-packet versus body-only bound placement, and
identity-directed consumer specialization. Construction-Gamma tests also cover
checked source projection through exact construction routes, structured
operated dependencies that must not collapse into the consumer, and graph-node
delegation that requires independent source-sidecar authority.

`Phi.ComputationSpec` additionally covers component endpoint validation and
rejects an alpha-equivalent composition seam whose binder identities differ.
Presolution tests cover atomic packet conflict rejection, composed χₑ role
evidence, quotient-projected construction placement, collapsed same-source
placement, and retained cross-source placement. Pipeline and program tests
cover the exact paper `g g` term together with ambient-Γ, locally closed Γ,
and explicit positional-forall regressions. The required completion gate is:

```sh
cabal build all && cabal test
```

The final serialized completion result is recorded in `implementation_notes.md`
with the change that closes this audit. Until that fresh gate is recorded, the
working-tree implementation is complete but its completion evidence is pending.
