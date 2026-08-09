## 2026-08-09 - Systematic-test cleanup and parser-parity restoration

- Re-enabled `ProgramParserParitySpec` in the active Hspec harness.  Its
  dynamic `.mlfp` cases now run through one generated public-CLI package,
  sharded into bounded definition batches, instead of compiling a separate
  parser package for every fixture.  The full dynamic group passes all three
  checks, including recursive module-body sequencing and malformed-input
  rejection.
- Fixed the production defects revealed by that restored path.  Prepared
  multi-root artifacts retain per-root scope overrides and select that lexical
  authority before each root elaborates.  A local constructor above a deeper
  result owner is placed at its certified construction scope, while unrelated
  owners still fail closed.  Deferred fallback batching now preserves both the
  unconsumed eligible prefix and the untouched suffix.  Representative ambient
  lookup uses an exact node-key authority, when one is frozen, to recover the
  structural recursive declaration shared by repeated branches.  A finalized
  body-consumer certificate is projected through the frozen operated endpoint
  before ambient comparison, so deep monomorphic partial applications retain
  their declared codomain without a global type-shape preference.  An
  enclosing result is exact authority for a child application only when one
  application of the checked source scheme reaches it.  If a monomorphic
  nested application exposes only Church-encoding structural binders, the
  child is checked first and its owner-final construction supplies the
  immediate codomain; genuine non-structural source foralls still use the
  parent endpoint to choose their specialization.  Root `RaiseMerge`
  requirements that share a solved monomorphic result retain separate Gamma
  declarations whenever a distinct flexible route survives.  If no such
  route remains, equal-bound requirements with compatible exact occurrence
  evidence merge their edge IDs onto the primary declaration rather than
  manufacturing a binder identity from a rigid/base node.
- Fixed two construction-identity failures found by the restored gates.  A
  pending local consumer first proves that exactly one raw direct exterior is
  unbounded and only then applies the construction quotient, so a certified
  declaration alias cannot be counted as a second pending exterior.  A frozen
  generated O15 counterexample also showed a source forall moving from a
  returned higher-rank parameter into an enclosing outer binder spine.  The
  reconciler now consumes only the active packet's validated
  `SourceOwnerConsumerCompletion`, matches its exact consumer identity and
  completed bound, and rechecks the actual child term before replacing that
  source owner's stale provisional Gamma entry.  Ordinary conflicting ambient
  declarations still fail closed.
- Froze the subsequent generated seed `937635187` counterexample, where one
  source forall had several graph occurrences in a shared solved class and the
  class exposed more than one outward construction binder.  The relaxed route
  collector now retains the source identity instead of choosing a peer or
  failing before occurrence selection; the strict collector still rejects the
  same ambiguous class.  Scheme alignment restricts representative lookup to
  source occurrence keys owned by that SchemeInfo, and owner-final let
  publication propagates its exact alignment rename to the checked RHS and
  publication environment instead of changing only the scheme.  The frozen
  regression and seeds `937635187`, `195565654`, and `20260809` (100 generated
  programs each) pass.
- Froze generated seed `449181304` case 66, where an ignored let returned a
  source-polymorphic function beneath two value lambdas after exact Gamma
  completion had closed the result declaration's bound.  Publishing the open
  child directly with `InstAbstrRef` was invalid because an xMLF instantiation
  cannot cross the enclosing value arrow.  Exact returned-body construction
  now prefers the completed declaration bound, separates certified open
  value-lambda parameters from residual result binders, constructs a unique
  checked instantiation from the source codomain to that residual bound, and
  applies it at the first exact result beneath the value-lambda spine.  The
  rebuilt term and its result publication are both typechecked before the
  endpoint plan is accepted; ambiguous or inexact constructions fail closed.
  The new helper has a direct placement/typecheck regression, and seed
  `449181304` also passes 100 generated programs.
- Removed same-path integration repetitions and assertions that were true for
  every possible value, and strengthened surviving tests to observe a real
  edge, copy trace, or typecheck result.  The thesis ledger now classifies
  deterministic evidence as `fixed` with one execution and generated evidence
  as `quickcheck` with at least 100 successes.  O07 core decomposition and O11
  presolution structural unification have independent oracles; variable
  well-formedness, instantiation, and lambda/let environment obligations also
  exercise their own judgment boundaries.
- Added repository guards that reject disabled or focused Hspec combinators,
  reused thesis property functions, and duplicate normalized top-level
  property equations.  The ledger renderer and executable checker enforce the
  fixed/generated distinction and execute all 107 obligation anchors in one
  process.  Final validation passes all 302 fixed annotation cases, 126 Root
  RaiseMerge cases, 15 repository guards, 107 thesis-obligation anchors,
  `cabal build all -j1`, the complete serialized suite (4042 examples, zero
  failures), and `./scripts/thesis-conformance-gate.sh`.

## 2026-08-07 - Source-declaration construction and paper `g g` completion

- Fixed the mixed-source declaration cycle at binder-plan construction time.
  A frozen unbounded source declaration is authoritative only when a solved
  occurrence creates the required-Gamma back-edge and the operated bound uses
  that exact semantic identity. The plan records the declaration-before-Gamma
  ordering fact, and reification leaves its source-owned bound at `Bottom` for
  the later certified `Lambda(Gamma)`/`Hyp` computation instead of repairing a
  free variable after finalization.
- Made source declaration lookup lexical as well as semantic. Authority is
  indexed by `(TypeBinderIdentity, frozen base node)`, so legitimate copies of
  one source scheme remain distinct declarations. An occurrence can use the
  identity-only route only when the declaration is globally unique; with
  multiple copies, only exact frozen-base provenance can select an owner.
  Within one frozen declaration, the planner prefers the base or explicit
  base-to-solved route and refuses ambiguous live aliases rather than choosing
  the smallest `NodeId`.
- Completed exact source-root lambda boundaries before substituting remaining
  routed parameter occurrences. This constructs `a -> (a -> a)` as
  `N -> N`, where `N = forall a. a -> a`, instead of duplicating the completed
  sibling body into `N -> N -> N`. Independently frozen and inherited lambda
  endpoints now reconcile only through the explicit value-lambda binder-spine
  construction; eliminating a vacuous unbounded forall is one such checked
  construction, not a broad type-shape equivalence.
- The focused evidence includes 298 fixed annotation examples and twelve
  consecutive generated seeds (`20260819` through `20260830`, 1200 programs).
  The complete serialized suite passes 4043 examples with zero failures, and
  `./scripts/thesis-conformance-gate.sh` passes. The repaired runtime harness
  remains fast: higher-kinded method resolution is sub-second in the built
  test binary, and Prelude sharing is still checked at the package boundary.
  This is finite executable evidence for the compiler's supported paper
  `g g` constructions, not a universal or mechanized proof of eMLF.

## 2026-08-07 - Construction-encoded body-consumer owner progress

- Revalidated the runtime-test repair before continuing eMLF work. The merged
  higher-kinded interpreter/LLVM/native row takes 3.8242 seconds of Hspec time,
  plain lambda/application takes 0.4485 seconds, and bare overloaded-method
  rejection takes 0.0912 seconds. The package regressions prove that two
  independent runtime artifacts and interpreter/backend consumers share one
  builtin Prelude semantic build, while the importless higher-kinded artifact
  performs zero Prelude builds.
- Replaced `BodyConsumerBoundRefinementCertificate`'s independently writable
  owner-finalized Boolean with private pending and finalized constructors.
  Nine validated producers now use one pending smart constructor, and the
  owner-boundary function is the sole constructor of finalized certificates.
  Authority reclassification and lifecycle advancement therefore happen as
  one transition instead of record-updating two fields that could disagree.
- Added a repository guard for the opaque constructor boundary and the absence
  of the old Boolean field. Focused owner-final construction tests pass (34
  examples), as do all 40 tests whose names exercise the paper `g g` family.
  `cabal build -j1 all` and the complete serialized suite pass (4012 examples,
  0 failures); `./scripts/thesis-conformance-gate.sh` also passes in full.

## 2026-08-04 - Construction-directed endpoint completion and final audit

- Fixed the remaining generated bounded-identity counterexample before
  changing its construction path. The frozen test exercises
  `forall (a >= forall b. b -> b). a -> a` beneath a let and three nested
  applications. Its descendant certificate records the complete bounded
  declaration, while an intermediate application Gamma presents the exact
  body after opening the leading binder. The installer now closes that view
  only for a private `BodyConsumerOrdinaryOwnerEmission` whose owner, edge,
  exterior, incoming endpoint, operated endpoint, and construction endpoint
  all certify the same declaration. This is a pre-typecheck Gamma transition,
  not a final-type repair or same-shape fallback.
- Kept child and enclosing lambda endpoints separate. A locally certified body
  endpoint cannot discharge an incompatible inherited endpoint belonging to
  the enclosing lambda's outgoing `Hyp`; only the completed inherited endpoint
  is consulted there. Conversely, an owner-final child certificate remains the
  source for the enclosing body construction, so its already-applied M/N/Hyp
  computation is not replayed. This preserves both the paper K result bound
  and the nested paper `g g` topology.
- Made inherited ambient refinement advance the construction identity route at
  the same boundary that installs its completed declaration. A
  `BodyConsumerInheritedAmbient` certificate now replaces the provisional
  graph/semantic aliases with the exact ambient construction ref, composes
  incoming routes that ended at that placeholder, retires the exact inverse
  ambient-to-semantic rename, and rejects third-identity or duplicate-binding
  conflicts. Consequently constrained nullary evidence is constructed under
  its completed ambient bound instead of later rediscovering a stale
  `Hyp` at `Bottom`; no authority selector or post-typecheck repair is needed.
- Froze seed `457459717`'s 53rd generated program, where an applied annotated
  lambda returns three nested lambdas ending in a source-polymorphic identity.
  The retained-Gamma endpoint constructor already opened a duplicate
  same-identity, same-bound declaration at the immediate returned codomain;
  it now carries that exact certificate through every value-arrow codomain.
  This prevents an administrative packet from quantifying the same graph
  binder both outside and inside a deep returned-lambda spine while leaving
  arrow domains, different identities, and different bounds untouched.
- The runtime matrix keeps the located package/Prelude boundary through one
  interpreter/LLVM/native artifact. The formerly duplicated higher-kinded row
  fell from 42.66 seconds combined to 3.4772 seconds of Hspec time (3.55
  seconds wall for the full focused command). The shared-Prelude and
  importless-package cache regressions take 1.9530 and 0.1678 seconds,
  respectively; the method lowering itself was never the bottleneck.
- Focused validation passes all 235 fixed annotation examples, the paper K and
  `BUG-002-V3` regressions, the previously frozen `g g` owner chains, seeds
  `91774058`, `541689707`, `1070269036`, and `457459717`, and every seed from
  `1000` through `1020` (2500 generated programs across those 25 seed runs).
  The complete serialized suite passes all 3977 examples.
- This establishes the tested construction path for the compiler's supported
  paper `g g` forms. It remains executable implementation evidence, not a
  mechanized proof of every eMLF typing, normalization, or soundness theorem.

## 2026-08-02 - Canonical source-type conversion and lower-bound shadowing

- Found a lexical-scope reset in normalized annotation conversion. When a
  structural lower bound contained `STCon` or `STVarApp`, its argument helper
  re-entered conversion with an empty bound-name set. A nested `forall a`
  could therefore reuse the resolver identity carried by an enclosing
  `forall a`.
- `MLF.Elab.SourceType` now threads the lexical binder set through every
  constructor and variable-application argument, including structural lower
  bounds. The new regression first failed with both binders carrying
  `GeneratedTypeBinderIdentity 993200` and now proves that the inner binder is
  distinct while its body occurrences retain that inner identity.
- Removed the recursive converter copies from annotation elaboration, Algebra,
  and external-binding preparation. Those owners now delegate to
  `MLF.Elab.SourceType`; the external-binding seam passes its required free
  binder order into the owner and receives the exact allocated refs back.
  This deletes roughly four hundred lines of duplicated traversal and makes
  the lexical-scope rule single-owner rather than synchronization-by-review.
- Generated annotation evidence exposed two remaining identity-domain leaks.
  Root publication was consulting occurrence-local routes from nested source
  annotations, allowing a child-owned forall to become the root projection;
  it now carries only the filtered inherited/exact root routes. Separately,
  direct application Gamma refinement compared a source-identity operated
  endpoint with a provisional graph-identity bound. The refiner now routes
  both through the source-construction quotient before choosing the bound, so
  a checked identity argument publishes its complete annotated forall instead
  of retaining the open graph body and failing a later claim.
- Root exact endpoints now distinguish producer `Typ(a')` from an already
  checked `S'(operated)`. Generalization projects only the former through its
  packet; application construction rebuilds its requirements from the latter
  before binder selection and claim validation. This keeps the construction
  direction explicit rather than inferring it from two untagged types.
- Focused regressions pass for annotation allocation, constructor-bound
  shadowing, ordered external-binding allocation, supplied external identities,
  the paper's checked-IR `g g`, and bare overloaded-method rejection. The
  importless higher-kinded runtime row still constructs no Prelude and remains
  sub-second in the already-built test binary.

## 2026-08-01 - Constructed application endpoints and let ambient re-entry

- A Bottom-backed terminal application result is no longer accepted as exact
  merely because its graph identity is present in ambient Gamma. When the
  function packet owns that terminal result, the endpoint remains provisional
  until the checked function construction completes it. Parameter-side graph
  variables remain admissible, so this does not discard already-constructed
  ambient authority.
- A direct lambda's static body packet is now treated as a prospective result.
  After elaborating the function child, the application retains that result
  only when `constructExactInstantiation` proves that the owner-final checked
  function constructs the corresponding exact arrow. This prevents an
  enclosing application from specializing a packet that the function owner
  did not materialize.
- When a checked identity argument and a provisional expected result present
  different endpoints, the exact-instantiation construction decides the
  ordering. A positively checked principal argument endpoint is retained and
  the outgoing `EApp` construction performs any required specialization.
- Let publication now re-enters the exact ambient declarations recorded by the
  checked RHS owner before constructing and checking its published term. The
  enclosing let-Gamma wrapper performs the same certificate-driven re-entry
  before its second construction check. Thus the `InstAbstr` emitted for the
  paper's `g g` result is checked under the Gamma that constructed it; no later
  type-shape recovery or free-variable closure invents its bound.
- Added fixed regressions for the principal identity-argument endpoint and for
  owner-final ambient re-entry around a let-bound paper `g g`. The complete
  fixed annotation slice passes (`148 examples, 0 failures`), and seeds
  `1195910434`, `984941370`, `1`, `20260727`, `20260801`, `2147483646`,
  `618238226`, and `486053823` each pass all 100 generated O15 cases.
- The previously slow focused rows remain fast in the already-built binary:
  the higher-kinded interpreter/LLVM/native parity row takes 1.31 seconds, the
  ordinary lambda/application row 0.92 seconds, and bare overloaded-method
  rejection 0.16 seconds wall time. Serialized `cabal build all` and
  `cabal test` pass; the current suite contains 3883 examples.
- This completes the compiler's tested construction path for the paper's
  annotated `g g` term across the covered lambda, application, let, ambient
  Gamma, and owner-final publication families. It remains executable evidence
  for the supported language, not a mechanized proof of every eMLF theorem.

## 2026-07-27 - Identity-authoritative paper self-application construction

- Added `MLF.Elab.SourceType` as the sole normalized-source-annotation to
  `ElabType` conversion owner. It consumes resolved symbol and binder identity
  maps, advances the shared identity supply past every carried payload,
  allocates genuinely free source variables as flexible existentials, and
  rejects a missing source head or binder. Annotation elaboration no longer
  creates a rigid root to compensate for a failed name lookup.
- Compiler-exact construction routes now distinguish two operations. A strict
  merge rejects different source identities for one graph node within the same
  domain. Entering a nested exact annotation performs explicit lexical
  shadowing after each domain has independently proved its one-to-one route.
  This preserves first-class local polymorphism when solving reuses a graph
  node across nested source scopes without weakening same-layer conflict
  checks.
- Owner-final let publication now retains a free identity as ambient only when
  the exact publication environment owns it and either the construction
  certificate declares it or an enclosing authoritative term scheme proves
  its ambient use. A bounded construction-local declaration is substituted in
  the scheme while the term receives the corresponding explicit `Lambda`/`N`
  computation. Thus the published checked term and scheme are produced
  together instead of closing or specializing one after the other.
- Exact producer comparison checks constructor and type-application arity
  before pairwise recursion. Recursive-let free-term-variable analysis now
  agrees with the checker that the let binder is in scope in both RHS and
  body, and a redundant endpoint validation was removed because certified
  completion already entails it.
- Four fixed regressions cover source-owned application Gamma, a deep mixed
  annotation owner chain, paper `g g` beneath nested lambda owners, and bounded
  owner-final publication. Focused validation passes the 102-example fixed
  annotation slice, two independent 100-case O15 generated seeds, 372 pipeline
  cases, 154 backend-conversion cases, 90 program-diagnostic cases, 29 package
  owner/cache cases, 130 witness-translation cases, and all five xMLF golden
  files. The merged higher-kinded interpreter/LLVM/native row takes 1.37
  seconds and the bare overloaded-method rejection takes 0.095 seconds in the
  already-built test binary.
- This completes the tested paper `g g` construction path for the compiler's
  supported source language. It is executable implementation evidence, not a
  mechanized proof of every typing, normalization, or soundness theorem in
  eMLF.

## 2026-07-26 - Construction-owned annotation/replay authority

- Generalization preparation now calls `mkElaborationEdgeAuthority` while all
  canonical annotated roots are available. The constructor checks every
  lambda, application, annotation, let-scope, and unfold edge; rejects missing
  or orphan annotation types and duplicate annotation-edge ownership; and
  seals the occurrence-owned type map with the complete `EdgeArtifacts`
  aggregate.
- Each validated canonical root is exposed only as an opaque
  `AuthorizedElaborationRoot`. `elaborateWithEnvDetailed` requires that
  capability and obtains both the construction tree and its edge authority
  from it. A caller can no longer pass an arbitrary `AnnExpr` beside a
  separately valid environment.
- Removed the parallel annotation-type and edge-artifact fields from
  `PreparedGeneralizationArtifact`, `ElabEnv`, `AnnotationContext`, and
  `AlgebraContext`, plus the separate pre-elaboration validation calls.
  Generalization and elaboration project both views from the same sealed
  authority instead of reconnecting them by `EdgeId` after construction.
- This closes a forgeable Chapter 15 construction seam and strengthens the
  executable evidence for §15.3.5-§15.3.8. It does not discharge the remaining
  mechanized-proof deviations for all eMLF terms.
- Validation:
  - the construction-authority, lambda/application authority, prepared-root,
    and repository-boundary regressions pass;
  - the paper `g g` checked-IR/Phi slice passes (`21 examples, 0 failures`);
  - the previously slow `rejects bare overloaded method use` row passes in
    `0.0914s` Hspec time (`2.08s` wall time);
  - `scripts/check-thesis-claims.sh` passes (`21 claims, 5 documented
    deviations`); and
  - `cabal build all -j1 && cabal test -j1` passes.

## 2026-07-26 - Normalization-owned witness publication

- `normalizeEdgeWitnessesM` now returns an opaque
  `NormalizedEdgeArtifacts` value containing the complete consumer-facing
  edge aggregate. Both timed and pure presolution finalization retain this
  value and pass it to result construction; result construction no longer
  projects mutable `psEdgeExecutionArtifacts` and assumes normalization ran
  earlier.
- Witness normalization transforms each `EdgeExecutionArtifacts` packet in
  place as one value, then publishes the aggregate from exactly those packets.
  This removes the intermediate witness map, trace map, key rejoin, and the two
  theoretically unreachable reconstruction errors.
- `EdgeWitness.ewForallIntros`, expansion-derived introduction counts, and the
  Φ O-phase input use `Natural`. `mkEdgeWitness` is consequently total, matching
  the paper's interpretation of O as a count rather than a signed value with a
  later validation step.
- `MLF.Constraint.Presolution.Witness` is now implementation-only in Cabal.
  Low-level normalization fixtures use
  `MLF.Constraint.Presolution.TestSupport`, whose wrapper deliberately discards
  the production publication token.
- This closes an executable construction gap but does not mechanize thesis
  Lemma 11.5.3; that universal proof obligation remains recorded as
  `DEV-WITNESS-NORM-NO-PROOF`.

## 2026-07-26 - Sealed presolution result and application-site authority

- `MLF.Constraint.Presolution.Base` moved from the internal library's exposed
  modules to `other-modules`. The production
  `MLF.Constraint.Presolution` façade now exports `PresolutionResult`
  abstractly with read-only selectors; callers cannot construct a result with
  unrelated constraint, redirect, union-find, and edge-artifact state.
- Removed component-map `mkEdgeArtifacts`, empty/insert/filter mutation
  builders, and identity-set replacement from the Base export surface.
  `MLF.Constraint.Presolution.TestSupport` owns the legacy fixture join and
  rebuilds a complete `EdgeExecutionArtifacts` packet per key before entering
  the production aggregate constructor.
- `alignAnnInstantiationSites` now consumes `EdgeArtifacts`, not
  `IntMap EdgeWitness`. It selects prepared endpoints from one complete packet,
  preserves redirected endpoints only for explicitly certified identity
  edges, and reports `PhiInvariantError` for every other missing edge. Both
  initial multi-root preparation and later root canonicalization propagate the
  failure rather than treating missing evidence as identity.
- The new semantic regression proves the missing-packet rejection and the
  identity-edge success path; repository guards keep the owner module,
  constructor, and component builders hidden. The nine fixed annotation
  examples, including paper `g g`, and all eleven Phi-alignment examples pass.

## 2026-07-26 - Construction-closed presolution state

- `PresolutionState` no longer exports either its internal constructor or the
  legacy bidirectional pattern that rebuilt edge execution state from separate
  expansion, witness, and trace maps. Production initialization goes through
  `mkPresolutionState`, whose only edge input is an
  `IntMap EdgeExecutionArtifacts`; cache and version state is initialized by
  the owner.
- Pending-Weaken certification now traverses complete execution packets, and
  final replay validation selects each complete published `EdgeArtifact` by
  the required `EdgeId`. Neither path projects witness and trace maps and then
  reconnects them by integer key.
- Compact positional state fixtures and component projections remain available
  only from `MLF.Constraint.Presolution.TestSupport`. The test builder checks
  identical key sets and the witness's embedded `EdgeId`; witness-normalization
  fixtures share one smart helper rather than manually populating every
  mutable-state cache field.
- `RepoGuardSpec` prevents the legacy constructor/reconnection vocabulary from
  returning to the production owners. This closes another forgeable
  construction seam; it strengthens executable Chapter 15 evidence but is not
  a mechanized proof for all eMLF terms. Focused validation passes 83 witness
  normalization examples, 34 edge-interpreter examples, 60 O15 obligations,
  and all 9 fixed annotation cases; the final serial suite passes 3704
  examples.

## 2026-07-26 - Construction-closed Phase 4 edge packets

- `EdgeArtifacts` no longer exports a record constructor containing four
  independently mutable `IntMap`s. It owns an opaque `IntMap EdgeArtifact`;
  every `EdgeArtifact` contains the edge's expansion, normalized witness,
  frozen replay trace, and exact expansion-construction certificate.
- Production finalization projects `EdgeArtifacts` directly from the
  construction-closed `EdgeExecutionArtifacts` map; it no longer splits the
  producer packet into component maps and reassembles it afterward.
  `mkEdgeArtifacts` remains a checked test-support boundary and rejects unequal
  expansion/witness/trace/construction key sets plus a witness whose embedded
  `EdgeId` disagrees with the selected map key. Production
  `mapEdgeArtifacts` and owner-local Var-Let filtering preserve the invariant;
  test mutation helpers rebuild the complete aggregate, so no operation can
  create a partial packet.
- `PresolutionResult` stores only the aggregate; its compatibility projections
  derive expansion, witness, trace, construction, and identity views from that
  value. Generalization preparation consumes the aggregate directly and
  canonicalizes it as one value.
  `mkPhiReplayCertificate`, annotation authority validation, application edge
  computation, identity-topology recovery, identity-edge recognition, and
  root-`RaiseMerge` authority now use a single per-edge packet lookup. Driver
  completeness failure is one `MissingEdgeArtifacts` error. Tests that
  previously deleted only one component now assert construction failure;
  valid mutation fixtures replace or clone a complete packet.
- The previously slow higher-kinded runtime parity row remains one merged
  interpreter/LLVM/native test and completes in 1.160 seconds locally. The
  explicit two-artifact Prelude-cache regression completes in 1.941 seconds
  and confirms one semantic Prelude build. This preserves the located package
  provenance/cache repair while continuing the eMLF work.
- Validation passes Phase 6 (323 examples), Phi alignment (11), root
  `RaiseMerge` Gamma construction (12), all 107 thesis obligations at 100
  generated cases each, the thesis-claims checker, warning-free
  `cabal build all -j1`, and the full 3703-example serial suite.

## 2026-07-26 - Aggregate-owned Phi replay authority

- `mkPhiReplayCertificate` now accepts one `EdgeArtifacts` aggregate instead
  of independently supplied witness and trace maps. This preserves the
  producer's per-edge packet boundary through Phase 6 and removes the API state
  in which a caller could select a witness from one presolution and a trace
  from another.
- `AnnotationContext` reaches that aggregate through its construction-owned
  elaboration authority. Annotation, lambda/application/unfold authority
  construction, ordinary occurrence replay, and result-type replay all use the
  same opaque certificate; checked witness and trace projections come back
  from the certificate rather than parallel ad hoc validation.
- Focused validation passes Phase 6 (322 examples), Phi alignment (11
  examples, including annotated `g g`), the nine fixed annotation cases, and
  all 107 thesis-obligation properties at 100 generated cases each. The
  thesis-claims checker, warning-free `cabal build all -j1`, and the full
  3703-example serial suite also pass. This closes the cross-map composition
  seam; it does not turn executable evidence into a mechanized proof of
  Chapter 15.

## 2026-07-26 - Construction-closed Phi replay certificates

- Production Φ entry points no longer accept an `EdgeWitness` beside a
  `Maybe EdgeTrace`. They consume an opaque `PhiReplayCertificate`, so Ω
  interpretation cannot be entered without both artifacts.
- `mkPhiReplayCertificate` fetches both artifacts through one producer-owned
  edge key and validates the witness's embedded edge identity. It deliberately
  does not infer association from roots: presolution replay/finalization can
  leave `ewRoot`, `etRoot`, and `etResultRoot` in distinct construction,
  frozen-source, and destination presentations. Their common committed edge
  packet is the authority.
- `OmegaContext` and its internal domain environment now carry `EdgeTrace`
  directly. All no-trace branches and empty replay/copy/interior substitutes
  are removed from production; only `MLF.Elab.Phi.TestSupport` retains an
  optional trace to assert `MissingEdgeTrace`.
- Focused validation passed the 111-example Φ translation slice, 11 Φ
  alignment examples (including the paper's annotated `g g` construction), 21
  eMLF source-annotation examples, and all 107 thesis-obligation properties at
  100 generated cases each. The thesis-claims checker, warning-free
  `cabal build all -j1`, and the full 3703-example serial suite also pass. This
  closes a forgeable interpreter state but does not claim a mechanized proof of
  Chapter 15.

## 2026-07-26 - Construction-closed normalized witness certificates

- `normalizeInstanceOpsFull` no longer returns raw `[InstanceOp]`. Successful
  normalization returns opaque `ValidatedInstanceOps`, and the unconditional
  `validatedInstanceOpsAfterNormalization` façade has been removed. The only
  raw seal is owner-internal to `MLF.Constraint.Types.Witness.Internal`.
- `certifyNormalizedWitness` retains evidence for the exact
  destination-presentation operation sequence. `normalizeEdgeWitnessesM`
  consumes that evidence after restoring frozen source identities and can seal
  the final witness only after all operands are in the producer-approved
  source/replay domain and any root `RaiseMerge` is unique, terminal, and
  backed by the matching construction trace.
- O11 witness properties now build variable-size flexible binding trees.
  Independent expected sequences cover duplicate-Raise elimination,
  delayed-Weaken placement with unrelated operations, RaiseMerge coalescing,
  validation, and normalization idempotence. This closes the forgeable runtime
  construction seam; `DEV-WITNESS-NORM-NO-PROOF` remains because Lemma 11.5.3
  is not mechanized in a proof assistant.
- Validation passed the 83-example witness-normalization slice (including 800
  generated certification cases), all 107 thesis-obligation anchor executions,
  the thesis claims/deviations checker, warning-free `cabal build all -j1`, and
  the full 3702-example serial suite.

## 2026-07-26 - Structurally generated G(sigma) and unreachable-binder elimination

- `internalizeCoercionType` binds only roots owned by a coercion copy. A bare
  shared variable keeps its existing owner, so an annotation-local existential
  remains flexible while an ambient Gamma binder remains restricted under the
  enclosing definition instead of being captured by the annotation gen.
- Constraint generation now implements Figure 8.2.3's construction cases
  before allocation: an unused `forall` returns its translated body directly,
  and any body whose translated graph is rooted at its binder returns
  `G(sigma)`. This includes graph-normalized Eq-Var forms such as
  `forall (a >= Int). forall b. a`, not only a syntactic body `a`. The
  rule is deliberately not extended to `mu`: a recursive owner carries nominal
  structural identity even when its self variable is vacuous. Its lexical
  identity node is retained for source/checked-IR authority but bound directly
  at the coercion-copy gen, rather than below a `TyMu` owner that cannot
  structurally reach it. A `StructuralResultBinder` nested inside that nominal
  `STMu` is also retained by semantic identity. It is compiler-owned Church
  representation metadata in the repo's explicit recursive-type extension,
  not a source quantifier in Figure 8.2.3's restricted-type grammar. This keeps
  the empty-data shape `mu self. forall result. result` constructor-directed
  and prevents presolution from treating its collapsed bottom node as a locked
  evidence frontier. Unreachable ordinary `forall` nodes are never created,
  rather than being cleaned from an invalid binding tree afterward.
- `O08-SYN-TO-GRAPH` is no longer one fixed graph with randomized names. Its
  QuickCheck generator composes nested arrows, bases, constructors,
  variable-headed applications, structural bounds, `forall`, `mu`, bottom, and
  repeated free existentials. A separately implemented recursive oracle checks
  both copies against source structure, identity-bearing lexical binders,
  existential sharing, permissions, Eq-Var/vacuity, source authority, and
  binding-tree validity. Five additional deterministic seeds cover 500
  generated cases during focused validation; together with the original
  regression seed this covers 600 generated O08 cases. Final validation passed
  the 79-example Phase 1 slice, the 227-example source-type finalization slice,
  the thesis-claims checker, and the full 3702-example serial suite.

## 2026-07-26 - Construction-closed annotation and Phi authority

- Annotation source selection is now represented by
  `AnnotationSourceConstruction`. A direct semantic reference can select
  witness replay; every composite producer must complete its exact checked
  construction (or the independently checked annotated-lambda construction)
  and preserves the original construction error otherwise.
- All production Φ entry points require the frozen `GaBindParents`
  certificate. Ω reads `OpRaise` source rigidity from
  `gaBaseConstraint`; only an operation node proved absent from that source
  snapshot may use its later constructed graph node for type recovery. The old
  optional-Γ fixture behavior is confined to `MLF.Elab.Phi.TestSupport`, and
  the unused `MLF.Elab.Phi.Env` state abstraction is deleted.
- Annotation elaboration retains the selected construction computation instead
  of synthesizing a replacement after closing and rechecking the term.
  Result-type reconstruction likewise selects one closed outcome:
  `ApplyAnnotationInstantiation` or `GeneralizeAnnotationTarget`.
  `adjustAnnotationInst` and the closed-term recomputation path are retired.
- Focused validation covers exact-source selection, the 21-example eMLF source
  annotation slice, explicit forall-bound preservation, 128 witness
  translations, 11 Φ-alignment examples, and 60 O15 obligations. The final
  serial gate passes `cabal build all -j1` without warnings and all 3696 tests
  under `cabal test -j1`.

## 2026-07-26 - Destination-owned Sigma ordering

- Thesis Definitions 15.3.3 and 15.3.4 define `Typexp(a')` as `S'(sc)`, where
  `sc` is the root of the expansion at the edge destination. Φ now derives the
  Σ(g) leftmost-lowermost order from the construction-owned
  `EdgeTrace.etResultRoot`. The frozen source root `etRoot` remains the
  authority for interpreting Ω operations; the two roots no longer share one
  local value.
- The prior binding-LCA root switch was both unnecessary and semantically
  wrong for an edge whose source and destination expansion roots expose
  different binder sets. Missing `<P` keys no longer preserve the current
  quantifier position, and a cyclic forall-bound dependency no longer drops
  the dependency graph and sorts by position. Both conditions fail before a
  reorder computation is constructed.
- Focused regressions distinguish source and destination roots, require missing
  order-key rejection independent of the current spine order, reject cyclic
  bound dependencies, and retain producer quantifiers outside a strict replay
  subset. The complete 128-example `Witness translation` slice is green. The
  final serial gate passes `cabal build all -j1` and all 3695 tests under
  `cabal test -j1`.

## 2026-07-26 - Mixed annotation publication and bounded self-application

- The §12.3.2 source pseudo-type `exists beta. forall alpha.
  beta -> alpha -> alpha` now uses one construction rule at source-annotation
  boundaries: free existential refs are generalized against the enclosing
  Gamma before the explicit universal spine. This applies both to expression
  roots and to nested let RHS publication; a focused regression verifies that
  `beta` is instantiated by the let use and does not leak into the enclosing
  `Bool` result.
- Compiler-exact root closure consumes the packet's prepared
  source-result identity route. It can therefore reuse the source constructor's
  abstraction without equating an unrelated local Gamma binder by position or
  bound shape.
- Annotation child construction now has three explicit cases: lambdas enter
  the source scheme's binder scope, inferable producers at monomorphic source
  types receive that exact endpoint, and inferable producers at quantified
  source types synthesize before the annotation computation. This keeps
  direct `__io_bind ... : IO Unit` applications constrained without replacing
  the paper's canonical `omega[N] id` by a pointwise forall construction.
- Focused validation passes the thesis annotation evidence 9/9, Phase 7
  typechecking 127/127, the eMLF boundary matrix 39/39, source-binder
  projection 22/22, compiler-exact completion 5/5, and the paper term through
  LLVM/native emission. This completes the tested annotated `g g` construction
  chain; it is not a proof that every term in the full eMLF calculus or its
  metatheory is implemented. The final serial gate passes `cabal build all
  -j1` and all 3692 tests under `cabal test -j1`.

## 2026-07-25 - Annotation coercion construction audit

- Source annotation construction now returns a role-labelled coercion pair:
  the rigid domain is edge authority and the flexible codomain is the exported
  result. This removes positional tuple selection from the κ construction
  boundary.
- Annotation elaboration no longer rewrites an `InstInside (InstBot ...)`
  payload with the expected bound after witness translation. The existing
  preserving-coercion construction from the checked source type is sufficient,
  and the 123-test annotation slice remains green without that repair.
- `O08-SYN-TO-GRAPH` now exercises a mixed existential/universal annotation
  instead of only checking that an `Int` node exists. Its 100 QuickCheck cases
  require the existential node to be shared, universal binders to be copied,
  domain/codomain roles to be restricted/instantiable, source authority to
  remain on the codomain, and the binding tree to stay valid.

## 2026-07-25 - Construction-Gamma ownership and runtime-cache closeout

- Lambda-body projection now produces a checked source-to-construction
  certificate from the exact source-binder sidecar, construction aliases, and
  explicit binder renames. The body term, its checked source type, and its
  resolved lookup environment enter the same construction identity domain
  together before outgoing Phi replay; shape equality cannot authorize this
  transition.
- A structured operated endpoint keeps its free graph identities as
  dependencies of the flexible consumer. Occurrence routing no longer aliases
  those nodes to the consumer, declaration-bound projection applies only exact
  routes whose targets occur in the operated endpoint, and generalization
  protects the corresponding identities. This prevents construction of an
  illegal self-bound such as `c > Box c -> Bool` from the intended
  `c > Box f -> Bool`.
- Local Gamma construction may delegate a graph declaration node only when the
  displaced emitted binder has independent exact source-sidecar authority.
  Construction-time replay likewise accepts an occurrence route conflict only
  when the active construction alias proves the incoming identity.
- The frozen parity artifact was regenerated through `frozen-parity-gen` after
  `Constraint` gained explicit graft-result construction evidence. Its focused
  equality check passes against the current structure.
- Focused validation passes automatic mu coverage 92/92, Phase 6 elaboration
  317/317, the high-risk identity/eMLF campaign 62/62, and each paper
  self-application layer (principal inference, checked xMLF, and LLVM/native)
  independently. The formerly duplicated higher-kinded parity row completes
  in 0.95 seconds, bare-overloaded-method rejection in 0.16 seconds, and the
  shared Prelude cache regression in 1.89 seconds on the validation machine.
  These timings are test-harness evidence, not a claim that higher-kinded
  resolution itself was optimized.

## 2026-07-21 - Identity-directed compiler-exact construction and Prelude reuse

- Exact packet preparation now establishes one unique, identity-bearing
  quotient from each source annotation binder to its inferred graph binder.
  Church-normal-form matching is admitted only at this narrow construction
  boundary and succeeds only when every usable normal form yields the same
  substitution.
- Internal packet schemes stay in the construction identity domain.
  `psgGammaBoundScheme` is materialized before publication and applies the
  quotient in reverse to expose the outward source identities. Consumer-owned
  bounds specialize only a leading forall whose identity matches the routed
  construction binder; unrelated quantifiers retain their order.
- A bounded graph binder can adopt an exact source identity only after its
  graph node or representative resolves through the source-binder sidecar and
  the independently supplied exact endpoint agrees. Alpha or Church equality
  validates the resulting type but cannot authorize the identity replacement;
  missing and wrong-source routes fail closed. Ordinary bounded packets remain
  locally owned.
- An exact endpoint's own `forall` declarations remain locally quantified even
  when the source sidecar already carries the same generated identity. Only an
  already-projected binder that the endpoint uses free is inherited and
  reopened. Completed construction-packet projection carries this declaration
  ownership through both projection passes, so the paper's complete
  `sigma-id = forall a. a -> a` reaches `psgSchemeInfo`,
  `psgOperatedSchemeInfo`, and `psgGammaBoundScheme` unchanged.
- Subterm packet placement distinguishes a whole-packet match from a match of
  the packet body alone. A body-only match retains an exact source bound;
  copied forall spines are installed only for a whole-packet match or for a
  graph-local body that needs the copied spine for lexical closure.
- Transparent let construction composes the completed RHS Gamma by binder
  identity before freshening. It retains bound-dependency closure, protects
  completed identities through both freshening boundaries, and rejects a
  one-to-many source route or conflicting concrete bounds instead of repairing
  the finished term.
- The same exact construction provenance restores `BUG-2026-02-08-004` to its
  original thesis-green `Int` contract. The 2026-02-26 fail-fast expectation
  was a sentinel for missing replay authority, not a semantic requirement;
  unresolved non-root weakenings remain negative tests elsewhere.
- A vacuous source forall may be eliminated before preserving the exact
  bounded forall required by the annotation. Together these construction
  rules produce the paper Section 15.3.8 term through checked xMLF and both
  interpreter and LLVM/native execution. This is a completion claim for the
  annotated `g g` construction chain, not for the entire eMLF calculus or its
  metatheory.
- The runtime test matrix preserves the builtin Prelude as a located package
  source unit. Its one-slot cache validates the complete resolved structural
  key, builds with an owner-private descending supply, retains generated
  identity extrema with the checked module, and advances each client supply
  past the cached artifact. The production facade hides every test-only cache
  entrypoint.
- Focused validation passes source-binder projection 17/17, reduced
  self-application packets 13/13, the annotated self-application surface
  12/12, the eMLF boundary matrix 39/39, systematic bug variants 13/13, and
  package-owner/cache coverage 25/25. The merged interpreter/LLVM/native row
  for the paper term passes 1/1 in 0.97 seconds and the higher-kinded method row
  passes 1/1 in 0.77 seconds. Each row constructs one checked artifact, and
  neither source imports Prelude, so both now perform zero Prelude checks.

## 2026-07-15 - Root-domain reification and ML factory construction

- Kept `ReifyLiveRoot` / `ReifyBaseSchemeRoot` attached to the graph that owns
  the selected root through the actual no-fallback reifier. Base-root
  reification now requires `GaBindParents.gaBaseConstraint`; it does not infer
  a graph from a bare `NodeId` or retry through the live graph.
- Added an overlapping-key regression in which live and base graphs both own
  `NodeId 10` with different structural types. The base-tagged plan now reifies
  the base `Bool -> Bool` structure rather than the live `Int -> Int` node.
- Restored the paper/bug contract for the ordinary ML term
  `let make = \x.\y.x in let c1 = make (-4) in c1 True`: the canonical path
  constructs `c1 : forall a. a -> Int`, returns `Int`, and emits an xMLF term
  that Phase 7 typechecks. Removed duplicated fail-fast and witness-shape
  assertions that contradicted this semantic contract; genuine unresolved
  non-root `OpWeaken` rejection remains covered by `BUG-002-V4`.

## 2026-06-10 - Round 352 bounded module-body source-definition sequencing substrate

- Added exact two- and three-source-definition helper entrypoints in the
  shared parser-owned `ParserParityParser.mlfp` library. The helpers reuse the
  existing bounded source-definition row substrate, parse one
  `parseSourceDefinitionRows` row at a time, and accumulate rows through
  `appendProjectionValues`.
- Migrated the selected two-definition, three-definition, and imported
  three-definition module-body paths onto those helper entrypoints while
  preserving module-body dispatch, exact definition counts, definition-row
  order, post-import body parsing, current source-definition semicolon
  handling, spans, diagnostics, package source-layout evidence, and aggregate
  parser-parity outputs.
- Removed the migrated second/third source-definition continuation aliases
  instead of leaving compatibility wrappers, and added focused static Hspec
  coverage for helper presence, migrated call sites, and alias absence.
- Scope remains bounded compiler-frontend/parser ergonomics substrate only.
  This is not full parser parity, compiler-package implementation,
  platform/proof progress, native/backend completion, package-manager/linker
  work, or self-boot completion.

## 2026-06-09 - Round 351 bounded import-row sequencing substrate

- Added a narrow bounded import-row sequencing helper family in the shared
  parser-owned `ParserParityParser.mlfp` library. The helper parses one
  `parseImportProjectionRows` row at a time, appends later import rows through
  `appendProjectionValues`, and advances through explicit one- and
  three-import budget entry points before the existing post-import body
  continuation.
- Migrated the selected one-import and three-import module-body paths onto the
  helper while preserving `parseImportedBodyAfterImport`, module-body dispatch,
  import projection row order, post-import body parsing, spans, diagnostics,
  package source-layout evidence, and aggregate parser-parity outputs.
- Removed the migrated second/third import continuation aliases instead of
  leaving compatibility wrappers, and added focused static Hspec coverage for
  the helper surface, migrated call sites, and alias absence.
- Scope remains bounded compiler-frontend/parser ergonomics substrate only.
  This is not full parser parity, compiler-package implementation,
  platform/proof progress, native/backend completion, package-manager/linker
  work, or self-boot completion.

## 2026-06-09 - Round 350 bounded complete-program module-row sequencing substrate

- Added a narrow bounded complete-program module-row sequencing helper family
  in the shared parser-owned `ParserParityParser.mlfp` library. The helper
  returns accumulated `ValueProjectionRows` at end of input, otherwise parses
  one more `parseSharedProgramModule`, appends rows with `appendLine`, and
  advances through explicit remaining-module budget entry points.
- Migrated the selected two-, three-, and four-module complete-program tail
  paths onto the helper while preserving the existing four-module maximum and
  leaving any fifth module to fail through the existing parser reply
  end-of-input boundary.
- Removed the migrated second/third/fourth program-module continuation aliases
  instead of leaving compatibility wrappers, and added focused static Hspec
  coverage for the helper surface, migrated call sites, and alias absence.
- Scope remains bounded compiler-frontend/parser ergonomics substrate only.
  This is not full parser parity, compiler-package implementation,
  platform/proof progress, native/backend completion, package-manager/linker
  work, or self-boot completion.

## 2026-06-09 - Round 349 bounded source-definition row sequencing substrate

- Added a narrow bounded source-definition row sequencing helper family in the
  shared parser-owned `ParserParityParser.mlfp` library. The helper parses one
  `parseSourceDefinitionRows` row at a time, appends rows through
  `appendProjectionValues`, and advances through explicit remaining-count
  entry points for the selected fixed budgets.
- Migrated the selected four-, thirteen-, and sixteen-definition row paths onto
  the helper while preserving the existing exact-count parser entrypoint names
  and their current callers.
- Removed the migrated second/third/fourth and batch continuation aliases
  instead of leaving compatibility wrappers, and added focused static Hspec
  coverage for the helper surface, migrated call sites, and alias absence.
- Scope remains bounded compiler-frontend/parser ergonomics substrate only.
  This is not full parser parity, compiler-package implementation,
  platform/proof progress, native/backend completion, package-manager/linker
  work, or self-boot completion.

## 2026-06-07 - Round 339 compiler-seed lexer parser parity slice

- Added a parser-parity conformance fixture copied byte-for-byte from
  `test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp`.
- Added the committed canonical parser-program projection for the selected
  SeedLexer source and a thin `.mlfp` parser-parity root that exposes only the
  source path/text before calling the shared parser library.
- Extended the shared parser-owned source-text library only for the bounded
  SeedLexer needs: the lexer/reverse/string scan budgets, decimal line-number
  advancement through line 227, seven-argument application chains, and the
  nested parenthesized token-stream constructor application used in
  `lexAfterLiteral`.
- Extended `ProgramParserParitySpec` with direct shared-parser equality,
  generated aggregate positive coverage, source-copy equality, one malformed
  case-branch negative, and static shortcut guards.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, package resolver, driver,
  platform, proof, or self-boot progress.

## 2026-06-06 - Round 338 compiler-seed data-model parser parity slice

- Added parser-parity conformance fixtures copied byte-for-byte from the
  selected compiler-seed frontend data-model modules: `SeedSource`,
  `SeedToken`, `SeedDiagnostic`, and `SeedAst`.
- Added one committed canonical parser-program projection for the selected
  four-source package order and a thin `.mlfp` parser-parity root that exposes
  only source path/text pairs before calling the shared parser library.
- Extended the shared parser-owned source-text library only for bounded
  structural needs in the selected seed modules: four-source projection,
  larger token/line bounds for the copied source, export/data rows, repeated
  five-branch case definitions, and nested constructor applications.
- Extended `ProgramParserParitySpec` with direct shared-parser equality,
  aggregate positive registration, source-copy equality checks, one malformed
  selected case-branch negative, and static shortcut guards.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, package resolver, driver,
  platform, proof, or self-boot progress.

## 2026-06-06 - Round 337 parser parity package source-layout extension

- Added package-layout parser-parity conformance fixtures copied from the
  exact run-program conformance sources for `cross-module-let` and
  `search-path-package`.
- Added committed parser-program projection fixtures for
  `package-cross-module-let` and `package-search-path-import`, preserving the
  individual `Core.mlfp`, `Main.mlfp`, `SearchLib.mlfp`, and `Main.mlfp`
  source paths and module names in rendered rows.
- Added thin `.mlfp` package fixture roots that expose selected source-file
  path/text pairs before calling
  `renderParserParityPackageProjectionFromSourceTexts`.
- Extended `ProgramParserParitySpec` with direct shared-parser package
  assertions, generated aggregate positive sections, one dynamic package-layout
  malformed import-semicolon diagnostic, and shortcut/static guards for
  fixture-specific package parser/token/projection shortcuts.
- Extended the shared parser-owned source-text library only with a package
  renderer that parses each source through the existing source-file token,
  parser-state, projection-row, diagnostic, and dynamic negative-evidence
  paths before joining rows in explicit source order.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-06 - Round 336 parser parity exact authoritative unified source extension

- Added parser-parity conformance fixtures copied from the exact unified
  corpus files `authoritative-case-analysis.mlfp`,
  `authoritative-let-polymorphism.mlfp`,
  `authoritative-nullary-overloaded-method.mlfp`, and
  `authoritative-overloaded-method.mlfp`.
- Added committed parser-program projection fixtures for those exact
  authoritative unified corpus paths, including the importless
  let-polymorphism module with no import rows.
- Added thin `.mlfp` fixture roots that expose only `sourceFile` and
  `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- Extended `ProgramParserParitySpec` with direct shared-parser assertions,
  generated aggregate positive sections, one dynamic authoritative unified
  malformed let-polymorphism diagnostic, and shortcut/static guards for
  fixture-specific parser/token/projection shortcuts.
- The existing shared parser-owned source-text library already parsed these
  exact case, let/lambda/application, class/instance, and deriving surfaces
  through generic token, parser-state, projection-row, diagnostic, and dynamic
  negative-evidence paths, so no parser-library code change was needed.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-06 - Round 335 parser parity named recursive ADT source-module extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded named recursive-ADT corpus slice copied from
  `test/programs/recursive-adt/deriving-eq.mlfp`,
  `test/programs/recursive-adt/recursive-gadt.mlfp`, and
  `test/programs/recursive-adt/recursive-existential.mlfp`.
- Added committed parser-program projection fixtures for `deriving-eq`,
  `recursive-gadt`, and `recursive-existential`, preserving source module
  names `DerivingEq`, `RecursiveGadt`, and `RecursiveExistential` in rendered
  module rows.
- Added thin `.mlfp` fixture roots that expose only `sourceFile` and
  `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- Extended the shared source parser for the selected deriving, GADT, and
  existential recursive-ADT syntax families so they render projection rows
  from parsed source structure and dynamic module headers instead of
  `Main`-only static projection keys.
- Added public parser-parity coverage for a malformed named recursive-ADT case
  branch through `renderParserNegativeEvidenceFromSourceText`, with
  parser-owned `expected-case-branch-arrow@...` diagnostic evidence.
- Removed the retired static recursive-ADT fallback recognizer from the shared
  parser library so the selected named corpus modules and carried recursive
  ADT cases use the dynamic program parser path.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-05 - Round 334 parser parity complex recursive program extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded complex recursive program slice based on
  `test/programs/recursive-adt/complex-recursive-program.mlfp`, covering
  `ComplexRecursiveProgram` exports for `Eq`, `Nat(..)`, `Tree(..)`, `eq`,
  `mirror`, `leftDepth`, `rightDepth`, and `main`.
- Added the committed parser-program projection fixture for
  `complex-recursive-program`, plus a thin `.mlfp` fixture root that exposes
  only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for a malformed Tree traversal branch
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-case-branch-arrow@...` diagnostic evidence.
- Extended the shared source parser for the selected syntax family with the
  composed `Eq`/recursive `Nat deriving Eq`/recursive `Tree` declaration
  sequence, eight-item export lists, bounded nested parenthesized
  constructor/function applications, and line-number evidence through the
  fixture's canonical final span.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  whole-fixture recognition, pre-rendered `mirror`/`leftDepth`/`rightDepth`/
  `main` rows, exact helper/main expression shortcuts, and static negative
  evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-05 - Round 333 parser parity module-integrated recursive existential extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded module-integrated recursive existential slice covering
  `Core` exporting `Eq`, `Nat(..)`, `Expr(..)`, `SomeExpr(..)`, and `eq`,
  plus `User` importing that surface, defining `peel` and `peelSome`, and
  computing `eq (peelSome (SomeExpr (Step (DoneNat (Succ Zero)))))
  (Succ Zero)`.
- Added the committed parser-program projection fixture for
  `module-integrated-recursive-existential`, plus a thin `.mlfp` fixture root
  that exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed `SomeExpr` case-branch
  syntax through `renderParserNegativeEvidenceFromSourceText`, with
  parser-owned `expected-case-branch-arrow@...` diagnostic evidence.
- Extended the shared source parser for the selected syntax family with
  simple source-type applications such as `Expr a`, parenthesized plain lambda
  parameters, bounded nested parenthesized constructor/function applications,
  the `Eq`/derived-`Nat`/`Expr`/`SomeExpr` declaration sequence, and generic
  imported-definition rows for `peel`, `peelSome`, and `main`. Also extended
  the parser-library line-number helper only as far as the fixture's canonical
  final span requires.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  whole-fixture recognition, pre-rendered `peel`/`peelSome`/`main` rows, exact
  `peelSome`/`main` expression shortcuts, and static negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-05 - Round 332 parser parity abstract recursive ADT module-use extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded abstract recursive ADT module-use slice covering `Core`
  exporting `Nat` abstractly with `zero`, `succ`, `peel`, and `isZero`, plus
  `User` importing that five-item surface and computing
  `isZero (peel (succ zero))`.
- Added the committed parser-program projection fixture for
  `abstract-recursive-adt-module-use`, plus a thin `.mlfp` fixture root that
  exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed destructor-case syntax
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-case-branch-arrow@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered `zero`/`succ`/`peel`/`isZero`/`main` rows, whole-fixture
  recognition, imported-main expression shortcuts, and static
  abstract-recursive-ADT module-use negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-05 - Round 331 parser parity recursive ADT/typeclass integration extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded recursive ADT/typeclass integration slice covering
  `typeclass-integration`: an `Eq` class, recursive `Nat` declaration,
  explicit `Eq Nat` instance, nested `case left` / `case right` method body,
  `same` wrapper definition, and nested constructor applications in `main`.
- Added the committed parser-program projection fixture for
  `typeclass-integration`, plus a thin `.mlfp` fixture root that exposes only
  `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed nested case syntax through
  `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-case-branch-arrow@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered instance/method/definition rows, whole-fixture recognition, and
  static typeclass-integration negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-04 - Round 330 parser parity recursive-tree extension

- Extended the shared parser-owned parser-parity source-text parser library
  with bounded recursive tree slices covering `recursive-tree-first-order` and
  `recursive-tree-deriving`, including a `Tree` declaration with two-field
  `Branch : Tree -> Tree -> Tree`, two-argument constructor patterns,
  wildcard patterns, recursive `mirror`, nested constructor/application
  expressions, and `data Tree ... deriving Eq`.
- Added committed parser-program projection fixtures for both recursive-tree
  cases, plus thin `.mlfp` fixture roots that expose only `sourceFile` and
  `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- Registered both positive fixtures in the generated aggregate parser-parity
  public CLI driver and added public malformed recursive-tree case syntax
  coverage through `renderParserNegativeEvidenceFromSourceText`, with
  parser-owned `expected-case-branch-arrow@...` diagnostic evidence.
- The generalized two-argument constructor-pattern grammar now carries two
  older malformed case-pattern fixtures to the later token-derived branch-arrow
  location; their expected dynamic evidence spans were updated to match the
  shared parser's honest grammar path.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including recursive-tree fixture-specific token streams, success
  keys, pre-rendered `Tree`/`Branch`/`mirror`/`isBranch`/`main` rows,
  whole-fixture recognition, and static recursive-tree negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-04 - Round 329 parser parity recursive-list-tail extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded recursive list-tail slice covering `module RecursiveList
  export (Nat(..), List(..), tailOrNil, isNil, main)`, paired `Nat` and `List`
  data declarations, a two-field `Cons : Nat -> List -> List` constructor,
  two-argument list case patterns, and nested `isNil (tailOrNil (Cons Zero
  Nil))` application.
- Added the committed parser-program projection fixture for
  `recursive-list-tail`, plus a thin `.mlfp` fixture root that exposes only
  `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed recursive-list case syntax
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-case-branch-arrow@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered `tailOrNil`/`isNil`/`main` rows, whole-fixture recognition, and
  static recursive-list negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-04 - Round 328 parser parity recursive-ADT plain Nat extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded recursive-ADT plain Nat slice covering `module NatPlain export
  (Nat(..), isZero, peel, main)`, a recursive `Nat` data declaration,
  top-level `isZero` and `peel` annotated-lambda case expressions, and nested
  constructor/application expressions in `main`.
- Added the committed parser-program projection fixture for
  `recursive-adt-plain-nat`, plus a thin `.mlfp` fixture root that exposes only
  `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed recursive-ADT plain Nat
  case syntax through `renderParserNegativeEvidenceFromSourceText`, with
  parser-owned `expected-case-branch-arrow@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered `isZero`/`peel`/`main` rows, whole-fixture recognition, and
  static recursive-ADT plain Nat negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-03 - Round 327 parser parity authoritative cross-module let-polymorphism extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded cross-module let-polymorphism slice covering a one-definition
  `Core` module exporting `applyId`, `let id = λx x in id 1`, and a `User`
  module importing and referencing that value.
- Added the committed parser-program projection fixture for
  `authoritative-cross-module-let-polymorphism`, plus a thin `.mlfp` fixture
  root that exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed cross-module-let syntax
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-def-semicolon@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered `applyId`/`main` rows, whole-fixture recognition, and static
  cross-module-let negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-03 - Round 326 parser parity authoritative recursive-let extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded authoritative recursive-let slice covering `Nat` data
  declarations, typed local recursive lets, annotated-lambda RHS bodies that
  parse through case expressions, and outer case expressions over constructor
  applications.
- Added the committed parser-program projection fixture for
  `authoritative-recursive-let`, plus a thin `.mlfp` fixture root that exposes
  only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed recursive-let case syntax
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-case-branch-arrow@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys,
  pre-rendered `peel`/`main` rows, whole-fixture recognition, and static
  recursive-let negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-02 - Round 323 parser parity higher-order returned-function extension

- Extended the shared parser-owned parser-parity source-text parser library
  with a bounded higher-order returned-function slice covering annotated-lambda
  bodies and typed local-let bodies that return another annotated lambda,
  parenthesized function-valued callee application, and canonical projection
  rendering for the selected returned-function fixture.
- Added the committed parser-program projection fixture for
  `higher-order-returned-function`, plus a thin `.mlfp` fixture root that
  exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed returned-function syntax
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-expression-close-paren@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys, the
  `make` row shortcut, pre-rendered returned-function rows, and static
  returned-function negative evidence.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-02 - Round 322 parser parity higher-order local-function-flow extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with a bounded higher-order local-function-flow slice covering
  two-definition source modules, typed local let chains, annotated-lambda RHS
  expressions, nested let bodies without recursive parser binding cycles, and
  generic multi-digit integer tokens.
- Added the committed parser-program projection fixture for
  `higher-order-local-function-flow`, plus a thin `.mlfp` fixture root that
  exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed typed local let chains
  through `renderParserNegativeEvidenceFromSourceText`, with parser-owned
  `expected-let-in@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys, the `use`
  row shortcut, and pre-rendered local-flow rows.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-02 - Round 321 parser parity higher-order partial-application extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with a bounded higher-order partial-application slice covering
  nested plain lambda bodies, parenthesized expression atoms, function-valued
  source types, and identifier-boundary token recognition.
- Added the committed parser-program projection fixture for
  `higher-order-partial-application`, plus a thin `.mlfp` fixture root that
  exposes only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed parenthesized partial
  application through `renderParserNegativeEvidenceFromSourceText`, with the
  parser-owned `expected-expression-close-paren@...` diagnostic evidence.
- Extended shortcut/static guards for round-specific parser/token/projection
  shortcuts, including fixture-specific token streams, success keys, and
  pre-rendered `keepLeft`/`apply`/`main` rows.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-06-01 - Round 318 parser parity rejected-shape retry

- Repaired the shared parser-library complete-program path after review
  rejection. Module parsing now accepts a shared parsed identifier token and
  renders the module row from that parsed module name instead of branching on
  `Core`/`User`.
- Export/import projection lists now use shared parsed item accumulation in the
  active path. The rejected fixed families
  `parseNatSurfaceExportRows`, `parseClassSurfaceExportRows`,
  `parseThreeItemImportRows`, and `parseFourItemImportRows` are gone from the
  parser-library path.
- Lexer-owned tokens now carry parsed start/end positions and token text.
  Export/import row names, ordering, `(..)` spans, plain type/value
  classification, and import-module spans are derived from parsed tokens rather
  than fixed Core/User fixture-family strings. The import-module row span is
  derived from the module-name token start through the following parsed
  `exposing` token start.
- Complete-program parsing remains bounded to four modules because this
  `.mlfp` parser-library slice still has bounded non-recursive list handling,
  but each slot calls the same shared module parser and the bound is not tied
  to Core/User fixtures.
- Export/import projection accumulation uses generic bounded list-budget steps
  rather than self-recursive parser combinators after a recursive attempt hit
  the current presolution `OperationOnLockedNode` limitation. The bound is not
  tied to Nat/Eq/main or Core/User fixture families, and row names/order/spans
  still come from parsed tokens.
- Extended `ProgramParserParitySpec` rejected-shape guards for the fixed
  known-module, fixed export/import item, fixed span, and fixed row-family
  helper names.
- Base refresh on 2026-06-01: preserved the dirty retry state in
  `/tmp/mlf4-round318-refresh-20260601165747`, stashed the implementation
  edits, fast-forwarded `orchestrator/round-318-next-parser-parity-slice` from
  `3b38af28` to current `origin/master` at `1279adcd`, and replayed the round
  changes. The only content conflict was `test/ProgramParserParitySpec.hs`;
  the resolution keeps the refreshed single batch-driver setup and adds the
  round-318 multi-module and malformed-separator assertions to that batch
  fixture. `orchestrator/state.json` was already modified and controller-owned;
  it was excluded from the implementation patch replay and not content-edited.
- Evidence after refresh: focused multi-module parser parity matcher passed
  (`221.5416s`, 1 example), malformed import-exposing matcher passed
  (`221.8468s`, 1 example), static shortcut Hspec guard passed (`0.2928s`, 1
  example), the broader `MLF.Program parser parity` Hspec group passed
  (`214.7520s`, 9 examples), direct batch and direct `run-program` smokes
  passed for both new multi-module fixtures plus the carried import fixture,
  rejected-shape audits produced no matches,
  `git rev-list --left-right --count HEAD...origin/master` returned `0 0`, and
  `git diff --check` passed. Full `cabal test` was not rerun after these
  parser-only gates.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-05-25 - Round 318 parser parity multi-module export/import extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with the bounded complete-program slice for a `Core` module followed
  by a `User` module, covering selected abstract export/import items,
  recursive ADT constructor export/import items, and cross-module references in
  the carried surfaces.
- Added committed parser-program projection fixtures for
  `multi-module-abstract-export-import` and
  `multi-module-recursive-adt-export-import`, plus thin `.mlfp` fixture roots
  that expose only `sourceFile` and `sourceText` before calling
  `renderParserParityProjectionFromSourceText`.
- Extended `ProgramParserParitySpec` canonical projection rendering so every
  parsed module in a program is rendered in source order, and added public
  negative evidence for malformed import-exposing separators through
  `renderParserNegativeEvidenceFromSourceText`.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-05-25 - Parser parity batch driver

- Replaced the repeated public `.mlfp` parser-parity fixture executions with
  one generated aggregate driver under `dist-newstyle/parser-parity-batch`.
  The generated driver imports the shared parser library once through
  `--search-path`, embeds the carried positive fixture source texts, and
  renders the positive projections, negative diagnostic evidence, and retry
  evidence as labelled sections.
- Kept canonical-parser projection checks in Haskell for each positive fixture
  so committed oracles still compare against the current canonical parser
  independently of the generated `.mlfp` batch driver.
- Scope remains bounded parser parity only. This is a test harness performance
  refactor, not a full parser parity, resolver, checker, backend,
  compiler-package, driver, platform, proof, or self-boot claim.

## 2026-05-25 - Program frontend timing diagnostics

- Added `MLF_PROGRAM_TIMING=1` as an opt-in CLI timing flag. `check-program`
  and `run-program` now emit package-load and frontend-check stage timings to
  stderr while preserving normal stdout. `MLF_PROGRAM_TIMING_DETAIL=1` adds
  per-module and per-module-phase timings inside `program.check.modules`.
  `MLF_PROGRAM_TIMING_OPERATIONS=1` is the narrow profiling mode for
  per-constructor, per-instance-method, and per-definition timings.
- The parser-parity batch check measured the slowdown in
  `program.check.modules`: `254060.643ms` out of `real 254.18s`. Package
  loading, module graph/order, type-family normalization, resolver, and package
  interface validation were all millisecond-scale in that run.
- The detailed parser-parity batch check measured `program.check.modules` at
  `253027.244ms`, led by `ParserParityParser` at `160623.202ms`,
  `ParserParityParserCombinator` at `33107.746ms`, `ParserParityLexer` at
  `28116.792ms`, and `ParserParityAst` at `26240.458ms`.
- A `cross-module-let` calibration run showed the small-package `Prelude`
  checker cost was dominated by `instance-bindings` (`329.396ms` of
  `456.941ms`). The parser-parity batch was different: it was dominated by
  `def-bindings`, especially `ParserParityParser.def-bindings`
  (`155502.345ms`), `ParserParityParserCombinator.def-bindings`
  (`31699.633ms`), `ParserParityLexer.def-bindings` (`26351.485ms`), and
  `ParserParityAst.def-bindings` (`25614.077ms`).
- An operation-level `cross-module-let` run showed the current small-package
  hot spots are Prelude-owned operations: `Eq (List a).eq` (`213.304ms`),
  `Eq Nat.eq` (`64.126ms`), `Eq (Option a).eq` (`50.795ms`), then
  constructor bindings such as `Cons` (`24.587ms`) and `Succ` (`15.929ms`).
- Added a module-level finalization context that prepares the shared runtime
  external-binding environment once per checked module, while keeping
  binding-local deferred placeholders outside that cache to avoid name
  collisions. This shares normalized runtime bindings and authoritative scheme
  info across binding finalization calls, but it did not materially move the
  parser-parity batch: the generated batch still measured
  `program.check.modules` at `245423.268ms` (`real 262.28s`), with
  `ParserParityParser.def-bindings` still dominant at `153617.643ms`.

## 2026-05-24 - Round 317 parser parity qualified import/reference extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with the bounded qualified import/reference syntax slice: import
  aliases, alias-only imports, exposed classes/types/constructors/values and
  methods, plus qualified value, type, constructor, class, and method
  references.
- Added committed parser-program projection fixtures for
  `qualified-import-alias-references` and `qualified-import-alias-only`, plus
  thin `.mlfp` fixture roots that expose only `sourceFile` and `sourceText`
  before calling `renderParserParityProjectionFromSourceText`.
- Added public negative evidence for malformed import-alias syntax through
  `renderParserNegativeEvidenceFromSourceText`, keeping the rejection on the
  shared source-text lexer/parser path with an `expected-import-alias`
  parser-owned diagnostic.
- Scope remains bounded parser parity only. This is not full parser parity,
  resolver, checker, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-05-24 - Round 316 parser parity GADT/existential extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with the bounded recursive-ADT syntax slice: parameterized data
  heads, GADT-style constructor result heads, constructor-local Unicode
  `∀` binders, nested `Expr a` / `Expr Nat` constructor field and result
  source types, constructor applications, and related constructor/case
  patterns.
- Added committed parser-program projection fixtures for
  `gadt-result-constructor-spans` and `existential-constructor-forall`, plus
  thin `.mlfp` fixture roots that expose only `sourceFile` and `sourceText`
  before calling `renderParserParityProjectionFromSourceText`.
- Added public negative evidence for malformed constructor-local forall syntax
  through `renderParserNegativeEvidenceFromSourceText`, keeping the rejection
  on the shared source-text lexer/parser path with an
  `expected-constructor-forall-dot` parser-owned diagnostic.
- Scope remains bounded parser parity only. This is not full parser parity,
  checker, resolver, backend, compiler-package, driver, platform, proof, or
  self-boot progress.

## 2026-05-23 - Round 315 parser parity type-family/type-level extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with the bounded closed type-family and type-level syntax slice:
  optional module export lists, `type family` declarations, kind-variable
  result kinds, kinded and plain family parameters, constructor and variable
  type-level patterns, type-level lambdas/applications in family equations,
  and source type-family-style annotations.
- Added committed parser-program projection fixtures for
  `type-family-kind-lambda` and `type-family-apply-annotation`, plus thin
  `.mlfp` fixture roots that expose only `sourceFile` and `sourceText` before
  calling `renderParserParityProjectionFromSourceText`.
- Added public negative evidence for malformed type-family equation syntax
  through `renderParserNegativeEvidenceFromSourceText`, keeping the rejection
  on the shared source-text lexer/parser path.
- Scope remains bounded parser parity only. This is not full parser parity,
  type-family checker/reducer support, checker, backend, compiler-package,
  driver, platform, proof, or self-boot progress.

## 2026-05-23 - Round 314 parser parity higher-kinded constraint extension

- Extended the shared parser-owned parser-parity source-text lexer/parser
  library with the bounded higher-kinded class/data and constrained
  multi-parameter class syntax slice: kinded declaration parameters,
  source kind arrows, variable-headed type applications, constructor type
  applications, superclass prefixes, Unicode functional dependencies, and
  empty instance bodies.
- Added committed parser-program projection fixtures for
  `higher-kinded-class-data-params` and `multiparam-superclass-fundep`, plus
  thin `.mlfp` fixture roots that expose only `sourceFile` and `sourceText`
  before calling `renderParserParityProjectionFromSourceText`.
- Added public negative evidence for malformed functional-dependency syntax
  through `renderParserNegativeEvidenceFromSourceText`, keeping the rejection
  on the shared source-text lexer/parser path.
- Scope remains bounded parser parity only. This is not full parser parity,
  type-family parity, checker, backend, compiler-package, driver, platform,
  proof, or self-boot progress.

## 2026-05-22 - Round 313 parser parity typeclass/instance extension

- Extended the shared parser-owned parser-parity library with a bounded
  source-text declaration grammar slice for class declarations, method
  signatures, deriving clauses, instance declarations, and instance method
  definitions.
- Added committed parser-program projection fixtures for
  `typeclass-deriving-method` and `typeclass-instance-nullary-method`, plus
  thin `.mlfp` fixture roots that expose only `sourceFile` and `sourceText`
  before calling `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for a malformed instance method
  definition through `renderParserNegativeEvidenceFromSourceText`, keeping the
  negative evidence on the same shared source-text lexer/parser path.
- Scope remains bounded parser parity only. This is not full parser parity,
  checker, backend, compiler-package, driver, platform, proof, or self-boot
  progress.

## 2026-05-22 - Round 312 parser parity case-pattern extension

- Extended the shared parser-owned parser-parity library with a bounded
  source-text `case ... of` grammar slice for constructor application
  scrutinees, constructor patterns, wildcard patterns, nested constructor
  patterns, parenthesized pattern arguments, branch arrows, and branch
  separators.
- Added committed parser-program projection fixtures for
  `case-expression-constructor-patterns` and `case-expression-nested-patterns`,
  plus thin `.mlfp` fixture roots that expose only `sourceFile` and
  `sourceText` before calling `renderParserParityProjectionFromSourceText`.
- Added public parser-parity coverage for malformed case branch arrows through
  `renderParserNegativeEvidenceFromSourceText`, keeping negative evidence on
  the same shared source-text lexer/parser path.
- Scope remains bounded parser parity only. This is not full parser parity,
  checker, backend, compiler-package, driver, platform, proof, or self-boot
  progress.

## 2026-05-22 - Round 311 parser parity source-text front door

- Changed the carried parser-parity fixture roots to expose `sourceFile` and
  `sourceText` only, then call the shared parser-owned
  `renderParserParityProjectionFromSourceText` entrypoint through
  `--search-path`.
- Replaced the failed exact-source lexer/classifier WIP with a shared
  parser-library source scanner. The scanner advances a source cursor, skips
  trivia, recognizes the carried token set, builds tokens from observed source
  text, and avoids recursive top-level lexer bindings that the public
  `run-program` runtime rejects.
- Extended the shared parser path to consume the six carried positive
  parser-parity fixtures through parser-state combinators and EOF checks, then
  return the existing bounded projection keys. Negative evidence now enters via
  source text and reports parser-owned diagnostics for missing import
  semicolons, definition semicolons, `let ... in`, typed annotation types,
  constructor colons, and Bool-definition equals signs.
- Scope remains bounded parser parity only. This is not a full parser parity,
  checker, backend, driver, platform, proof, or self-boot claim.

## 2026-05-21 - Round 310 parser parity shared library consolidation

- Consolidated the carried parser-parity fixture packages for
  `basic-module-def-bool`, `import-exposing-def-bool`,
  `value-def-list-int-ref`, `let-lambda-application`,
  `typed-annotation-types`, and `data-declaration-constructor-spans` onto one
  shared parser-owned `.mlfp` library under
  `test/programs/compiler-parser-parity/parser-library/`.
- Fixture roots are now thin harnesses that provide source file identity and
  `ParserSourceInput` token/source-symbol streams, then call the shared
  `renderParserParityProjection` entrypoint through `--search-path`. Temporary
  negative-evidence packages likewise call shared parser diagnostics instead
  of copying fixture-local parser support.
- The shared lexer validates the parser-parity source-input stream and no
  longer recognizes complete fixture source text to return a success token
  stream. The shared parser keeps parser-state evidence in
  `ParserParityParserCombinator` and validates all carried positive and
  negative token streams through one `parseTokens` entrypoint.
- Scope remains parser parity only. This does not claim full parser parity,
  checker, resolver, backend, driver, platform, proof, generic Prelude parser
  APIs, or self-boot progress.

## 2026-05-21 - Round 309 parser parity data declaration tracer

- Added a bounded parser-owned `.mlfp` parity package for
  `module Main export (Nat(..), main) { data Nat = Zero : Nat | Succ : Nat -> Nat; def main : Nat = Succ Zero; }`
  under
  `test/programs/compiler-parser-parity/data-declaration-constructor-spans/`.
- Added the matching canonical parser fixture and committed projection under
  `test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/`,
  covering ordered export items, `ExportTypeWithConstructors`,
  `DeclData`, data-declaration spans, constructor spans, `Nat -> Nat`
  source-type rendering, and constructor application rendering.
- Extended `ProgramParserParitySpec` only inside test-owned projection and
  package evidence helpers: it now renders ordered export lists, data
  declarations, constructor declarations, and the selected constructor-value
  application, and checks malformed data-declaration syntax through the public
  `run-program` path.
- Scope remains parser-only. This does not claim full parser parity, checker,
  backend, driver, platform, compiler-package, proof, parser combinators, or
  self-boot progress.
- Evidence: focused RED data-declaration matcher failed before renderer and
  package support existed; focused GREEN data-declaration matcher and focused
  GREEN malformed-data-declaration matcher passed. Round-owned implementation
  notes under `orchestrator/rounds/round-309/implementation-notes.md` record
  the full focused, regression, full-suite, and thesis-gate validation.

## 2026-05-20 - Round 308 parser parity typed annotation tracer

- Added a bounded parser-owned `.mlfp` parity package for
  `import Prelude exposing (Int);` and
  `def main : Int = let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int);`
  under `test/programs/compiler-parser-parity/typed-annotation-types/`.
- Added the matching canonical parser fixture and committed projection under
  `test/conformance/mlfp/parser-parity/typed-annotation-types/`, covering
  module/export/import spans, the `Int` exposing-item span, typed let
  annotation rendering, annotated lambda parameter rendering, expression
  annotation rendering, and arrow/forall source-type rendering.
- Extended `ProgramParserParitySpec` only inside test-owned projection and
  package evidence helpers: it now renders `STVar`, `STArrow`, `STForall`,
  typed `ELet`, annotated parameters, and `EAnn`, and checks malformed
  annotation syntax through the public `run-program` path.
- Scope remains parser-only. This does not claim full parser parity, checker,
  backend, driver, platform, compiler-package, proof, parser combinators, or
  self-boot progress.
- Evidence: focused RED typed-annotation matcher failed before the
  fixture/package existed; focused GREEN typed-annotation matcher and focused
  GREEN malformed-annotation matcher passed. Round-owned implementation notes
  under `orchestrator/rounds/round-308/implementation-notes.md` record the
  full focused, regression, full-suite, and thesis-gate validation.

## 2026-05-20 - Round 307 parser parity let/lambda/application tracer

- Added a bounded parser-owned `.mlfp` parity package for
  `import Prelude exposing (Int);` and
  `def main : Int = let id = λx x in id 1;` under
  `test/programs/compiler-parser-parity/let-lambda-application/`.
- Added the matching canonical parser fixture and committed projection under
  `test/conformance/mlfp/parser-parity/let-lambda-application/`, covering
  module/export/import spans, the `Int` exposing-item span, the carried
  value-definition span, and a stable rendered expression shape for `let`,
  bare lambda, and left-associated application.
- Extended `ProgramParserParitySpec` only inside test-owned projection and
  package evidence helpers: it now renders `ELet`, `ELam`, and `EApp` shapes,
  and checks malformed let-expression sequencing through the public
  `run-program` path.
- Scope remains parser-only. This does not claim full parser parity, checker,
  backend, driver, platform, compiler-package, proof, parser combinators, or
  self-boot progress.
- Evidence: focused RED/GREEN let/lambda/application matcher, focused GREEN
  malformed-let matcher, and full parser-parity group covering rounds 304-307.

## 2026-05-20 - Round 306 parser parity value-definition-list tracer

- Added a bounded parser-owned `.mlfp` parity package for
  `import Prelude exposing (Int);`, `def two : Int = 2;`, and
  `def main : Int = two;` under
  `test/programs/compiler-parser-parity/value-def-list-int-ref/`.
- Added the matching canonical parser fixture and committed projection under
  `test/conformance/mlfp/parser-parity/value-def-list-int-ref/`, including
  spans for the module, export, import, `Int` exposing item, two value
  definitions, integer literal `2`, and lower-case reference `two`.
- Extended `ProgramParserParitySpec` only inside test-owned projection and
  package evidence helpers: it now renders value-definition lists and the two
  selected expression atoms, and checks malformed value-definition sequencing
  through the public `run-program` path.
- Scope remains parser-only. This does not claim full parser parity, checker,
  backend, driver, platform, compiler-package, proof, or self-boot progress.
- Evidence: focused RED/GREEN value-definition-list matcher, focused
  RED/GREEN negative sequencing matcher, full parser-parity group, direct
  package smokes for the new tracer plus round-304/round-305 tracers,
  `git diff --check`, `cabal build all && cabal test`, and
  `./scripts/thesis-conformance-gate.sh`.

## 2026-05-18 - Unicode string literal native tracer

- Added a public `.mlfp` tracer for `def main : String = "λ";` that checks,
  runs, emits raw LLVM, validates assembly/object code, emits native LLVM, and
  executes natively with the same quoted escape output as `run-program`.
- String globals are rendered as UTF-8 byte arrays in LLVM, with byte-accurate
  lengths and escaped non-printable bytes.
- Native String rendering now handles the selected two-byte UTF-8 scalar by
  decoding it to the existing decimal escape text; broader Unicode string
  library operations remain future broad-text-substrate work.

## 2026-05-03 - Backend IR executable-boundary family closed on the merged 710c92eb baseline

- The backend IR executable-boundary family now has rows 1 through 7 closed on the merged `710c92eb` baseline.
- The final backend contract remains one executable eager backend IR, eager runtime lowering only, no lazy STG machinery, no public `LowerableBackend.IR`, and no fallback/runtime-rescue widening.
- This round closes the evidence/guidance ledger without changing backend implementation behavior; it adds no new backend implementation feature.

## 2026-05-01 - Native LLVM parity coverage closeout

- `BackendLLVMSpec` now classifies every shared `ProgramSpec`-to-LLVM
  runtime-success row with one explicit coverage record: native-run checked or
  native-unsupported with a required diagnostic, plus object-code smoke where
  selected.
- The coverage guard requires advanced renderable rows from the typeclass,
  first-class polymorphism, and higher-order backend slices to remain on the
  native-run path, preventing supported parity rows from quietly staying
  assembly-only.
- Added `docs/backend-native-pipeline.md` to document the raw/native emission
  split, temporary `.ll`/object/executable artifacts, LLVM and linker discovery,
  backend-owned runtime support, result comparison, unsupported result shapes,
  and Hspec pending behavior when tools are absent.

## 2026-04-30 - Partial applications lower as closure values

- Checked-program conversion now packages underapplied monomorphic function
  calls as explicit `BackendClosure` values that capture supplied arguments and
  apply the remaining value parameters later. Saturated calls stay on the
  existing direct or closure-call paths.
- Higher-order partials keep function-typed supplied arguments in the packaged
  closure. When a callee parameter is later underapplied, direct function
  arguments are first wrapped as closure values so the generated partial
  closure does not mix raw function pointers with closure-record calls.
- Direct function wrappers created for closure-demanded arguments now capture
  local free variables from inline function expressions, so the generated
  closure entry does not reference names outside its closure environment.
- Closure-value argument demand is propagated through top-level aliases and
  local let-bound helpers. Non-variable partial callees beta-normalize immediate
  lambda heads and capture local free variables before their closure entry is
  emitted, so generated entries do not reference out-of-scope locals.
- Local function bindings used through underapplication are closure-converted
  so the packaged partial captures a closure pointer instead of referencing a
  local helper from a separate closure entry. Existing typeclass/evidence
  partial rows stay on the evidence-aware lowering path.
- Supplied polymorphic function values remain on the existing static
  specialization path instead of becoming partial-closure captures, because
  runtime closure environments do not store `forall` values directly.
- Supplied higher-rank function values follow the same boundary: only
  first-order function-pointer shapes are wrapped and captured in partial
  closures, matching the LLVM lowerer's stored-function representation.
- Ordinary call conversion treats hidden evidence arguments by their exact
  argument index, so evidence already supplied in a call spine does not suppress
  closure wrapping for a later closure-demanded function argument.
- Generated partial-closure capture and parameter names are freshened against
  visible source binders and globals before packaging, preventing source
  bindings from colliding with backend-generated closure slots.
- Closure-demand alias analysis looks through simple let-wrapped aliases such
  as `let f = use in f`, preserving supplied-argument offsets while propagating
  demanded closure-value parameters.
- LLVM parity coverage now includes top-level and local partial application
  rows, with `llvm-as`, object-code smoke, and native execution coverage when
  the local LLVM toolchain is available.

## 2026-04-30 - Closure-valued ADT fields

- Checked-program conversion now stores monomorphic function-valued constructor
  fields as `BackendClosure` values. Direct function references are
  eta-expanded under the known field type when they are not already closure
  values, while captured lambdas keep their closure environments.
- Backend IR validation and LLVM lowering now treat constructor pattern binders
  for arrow fields as closure-valued locals. Case-projected function fields are
  loaded as closure pointers and must be applied with `BackendClosureCall`.
- The shared ProgramSpec-to-LLVM parity matrix includes a runtime row that
  stores a captured function in an ADT, projects it with case analysis, applies
  it, and returns `41`.

## 2026-04-30 - Native execution for supported LLVM parity rows

- Extended `BackendLLVMSpec` so supported shared `ProgramSpec`-to-LLVM parity
  rows emit native-entrypoint LLVM, validate it, compile/link/run it through the
  native toolchain helper, and compare stdout/stderr/exit status against the
  same runtime expectations used by `ProgramSpec`.
- Native-unsupported parity rows are now named in one explicit map with expected
  diagnostic fragments. The remaining predicate rows that require rendering
  function values stay rejected before native execution instead of silently
  remaining assembly-only.

## 2026-04-30 - Nullary local evidence for parameterized result aliases

- Tightened nullary overloaded method finalization so placeholder type
  instantiations are replayed for concrete instance methods but not blindly
  applied to already-instantiated local evidence parameters. This keeps
  constrained aliases such as `DefaultBox a => Box a` usable at `Box Nat`
  without over-instantiating the local evidence rewrite.
- The `.mlfp` elaborator now preserves expected result annotations on bare
  constrained value uses, matching the existing application path so source
  aliases can instantiate their hidden evidence arguments from the surrounding
  expected type.
- Backend conversion now uses expected-result function context to infer
  nominal ADT instantiation arguments for retained type applications, avoiding
  finalizer-wide head recovery while keeping the constrained alias row in
  shared interpreter and LLVM parity coverage.
- Finalization now decides vacuous `forall` stripping from the checked type and
  the matching term type-abstraction spine together, so a binder retained by a
  term-level instantiation remains in both `checkedBindingType` and
  `checkedBindingTerm`.

## 2026-04-29 - Nullary overloaded method expected-type resolution

- `.mlfp` nullary overloaded methods / associated values now carry expected
  source-type evidence through deferred method finalization. A use such as
  `(mempty : Nat)` resolves by matching the method result against the expected
  type to recover the class argument, then reuses the existing coherent
  instance/evidence resolution path.
- Ambiguity remains fail-closed: bare nullary method uses without an explicit
  or propagated expected type still report `ProgramAmbiguousMethodUse`, and
  ordinary overloaded methods still require enough term arguments.

## 2026-04-29 - Native LLVM toolchain runner harness

- Extended `LLVMToolSupport` with native toolchain discovery for `llc` plus a
  C compiler/linker selected from `CC`, `cc`, `clang`, or `gcc`. The `CC`
  value may be a command line such as `ccache clang` or `xcrun clang`, with
  launcher arguments preserved after executable lookup. Missing pieces mark the
  relevant Hspec examples pending instead of failing unrelated backend tests.
- Added a temporary-build runner that lowers `.ll` to an object, links a native
  executable, runs it, captures stdout/stderr/exit status, and removes build
  products. The coverage is harness-only and does not define `.mlfp` result
  rendering semantics.

## 2026-04-29 - Final ProgramSpec-to-LLVM parity closure

- Removed the last temporary LLVM unsupported classification from the shared
  `ProgramSpec` runtime-success parity matrix. Every
  `programSpecToLLVMParityCases` row now expects LLVM emission and `llvm-as`
  validation instead of accepting an unsupported backend diagnostic.
- The LLVM lowerer now stores first-order function-valued constructor fields as
  explicit closure pointers. Checked-program conversion eta-expands direct
  function references when needed, captured constructor-field functions carry
  closure environments, and case-projected function fields remain
  `BackendClosureCall` callees.
- Representative object-code coverage now includes the former unsupported
  typed non-data constructor-field row, so the `llc -filetype=obj` smoke subset
  exercises stored function-field lowering in addition to the existing first
  order, ADT, import, and first-class-polymorphism representatives.

## 2026-04-29 - Higher-kinded and hidden-owner constructor LLVM parity

- `MLF.Backend.IR` now preserves checked variable-headed type applications as
  `BTVarApp` instead of rejecting all `STVarApp` fields at backend conversion.
  Substitution, alpha equality, constructor matching, validation, and LLVM
  lowering all understand applied type variables once they are resolved to
  concrete backend data heads.
- `MLF.Backend.Convert` now recovers backend constructor and case nodes from the
  structural constructor terms produced for higher-kinded and hidden-owner ADTs.
  Recovery is anchored to the constructor owner/result shape and the checked
  constructor metadata, then normalizes structural recursive owner types back to
  canonical backend data names. Unqualified structural owner binders are scoped
  to the current module only; imported and qualified owners must match their
  canonical module-qualified backend data names.
- LLVM parity now supports the issue-owned higher-kinded data-field rows and
  hidden-owner value-constructor import rows, and the `llc` smoke subset includes
  both hidden-owner and qualified-alias identity representatives.

## 2026-04-28 - Typeclass evidence reaches LLVM backend

- Function-valued Eq evidence now lowers as first-order LLVM function
  references or private wrappers instead of escaping as unsupported arrow
  values. Hidden class evidence parameters lower to opaque pointers and calls
  through evidence use indirect calls.
- Backend conversion preserves source data identity through evidence, case, and
  derived `Eq` paths so parameterized and qualified ADTs do not recover the
  wrong same-shaped data declaration during LLVM conversion.
- Recursive derived `Eq` helpers that capture only hidden evidence are lifted to
  backend helper bindings with explicit evidence/type parameters; ordinary
  lexical captures remain unsupported.

## 2026-04-28 - Shared Program-to-LLVM parity matrix

- Extracted the `.mlfp` interpreter-success surface from `ProgramSpec` into
  `Parity.ProgramMatrix`, including recursive-ADT fixtures, unified fixtures,
  `ExpectRunValue` rows from the eMLF surface and boundary matrices, and the
  standalone source-program runtime checks that were previously embedded in
  `ProgramSpec`.
- Added LLVM parity coverage in `BackendLLVMSpec` to emit LLVM for the shared
  runtime-success surface. Supported cases must produce LLVM accepted by
  `llvm-as`, while a small named subset also runs through
  `llc -filetype=obj -o /dev/null`.
- Temporary LLVM gaps are named in one explicit unsupported table and checked
  against a fixed count, so adding or removing interpreter-success coverage
  cannot silently change the backend parity surface.

## 2026-04-28 - Recursive let and recursive ADT LLVM parity

- `MLF.Backend.Convert` now promotes eligible closed recursive local functions
  to backend helper bindings before LLVM lowering. The source local name remains
  a non-recursive alias in the original body, while the helper owns the actual
  self-call path as an ordinary backend/global function.
- The promotion is intentionally narrow: only monomorphic first-order recursive
  local functions are accepted. Recursive local functions that capture lexical
  values still fail during backend conversion instead of relying on closure
  conversion that the current backend does not implement.
- Added a first-order recursive Tree fixture and broadened backend LLVM smoke
  coverage across recursive Nat, List, Tree, GADT-shaped, existential-shaped,
  and recursive-local-let programs. The LLVM tests assemble with `llvm-as` and
  run object-code smoke coverage with `llc` when those tools are available.

## 2026-04-28 - Real LLVM backend boundary

- Replaced the former backend inspection-text boundary with `MLF.Backend.LLVM`,
  a private repo-local LLVM backend made of `Syntax`, `Lower`, `Ppr`, and a
  facade exposing checked-program and backend-program rendering entrypoints.
- The backend still targets `MLF.Backend.IR` after checked `.mlfp` conversion.
  The LLVM facade validates that IR first, then lowers the reachable first-order
  binding closure to opaque-pointer LLVM IR with deterministic global/local
  names.
- Lowering supports first-order integer, boolean, string, ADT pointer, and
  recursive pointer representations; saturated direct calls; simple concrete
  polymorphic specializations; SSA-style lets; constructor allocation with
  zero-based tags; constructor case switches with field loads and join phis;
  and representation-preserving roll/unroll no-ops.
- The backend intentionally rejects partial applications, escaping functions,
  escaping lambdas, function-typed constructor fields, unsupported source/base
  types, non-ASCII string literals, and representation-changing roll/unroll
  nodes until closure conversion or a richer runtime representation exists.
- Backend validation tests now assemble emitted LLVM with `llvm-as` and smoke
  representative codegen with `llc` when LLVM tools are available on `PATH` or
  in a standard local LLVM installation. LLVM-dependent assertions are marked
  pending when the tools are absent, so `cabal test` can run in environments
  without LLVM while still exercising the checks wherever the tools are
  installed. The harness retries with `-opaque-pointers` for LLVM 14-era tools
  that require the explicit flag.
- The first-order LLVM parity slice now reuses the shared `.mlfp` program
  matrix through `ProgramMatrix`. `BackendLLVMSpec` lowers the core
  ProgramSpec cases for lambda/application, non-recursive let polymorphism,
  typed let annotations, term annotations, and the unified let-polymorphism,
  cross-module, and constructor/case fixtures. The sibling unsupported classes
  remain outside this slice: recursive lets, typeclass evidence,
  first-class-polymorphic values, higher-kinded/GADT-heavy shapes, partial
  application, and closure conversion.

## 2026-04-22 - Typed backend IR boundary

- Added `MLF.Backend.IR` as the private backend-owned representation after the
  existing `.mlfp` checker and xMLF typecheck guard. The module defines
  `BackendProgram`, modules, data/constructor metadata, typed expressions,
  backend types, case alternatives, and validation errors.
- The backend IR is intentionally not a second inference authority. Its
  validator only enforces local invariants expected from a checked conversion:
  globally unique runtime binding names, an existing `main` binding, expression
  result types on every node, binding/body type agreement, lexical/global
  variable reference resolution, and local type equalities for lambda,
  application, let, type abstraction/application, and recursive roll/unroll
  nodes.
- ADT construction and case analysis now have explicit backend IR nodes. LLVM
  lowering consumes this boundary rather than reaching back into source syntax
  or Church-encoded xMLF terms to rediscover backend control/data structure.
  Program validation checks those nodes against constructor metadata:
  constructor names are unique and known, construct arguments/results match the
  constructor declaration, and case alternatives match the scrutinee and case
  result types.

## 2026-04-20 - Constraint-aware typeclasses and qualified imports

- `.mlfp` constrained types are now source-level program semantics. The parser
  accepts `Eq a => ...` and `(Eq a, Eq b) => ...` on definitions, method
  signatures, and instances; lowering passes one hidden runtime method value per
  class constraint while raw eMLF syntax remains unchanged.
- Instance resolution now handles schema instances such as
  `Eq a => Eq (Option a)` and rejects overlapping heads by unification, not only
  exact equality. Evidence resolution fails closed through the existing
  no-matching-instance lane when a required dictionary cannot be built.
- `deriving Eq` generates constrained instances for parameterized ADTs. Recursive
  derived instances use a monomorphic local self function inside the generated
  method body so recursive fields do not re-infer the polymorphic schema through
  hidden evidence on every call.
- Parameterized constructor runtime bindings keep the Church representation but
  repair generated constructor spines so data-parameter type abstractions are
  instantiated before the eliminator result abstraction. This preserves the
  deferred constructor-use path while keeping recursive `List a` values
  executable.
- Qualified imports are `.mlfp`-owned names. `import M as A;` imports exported
  names qualified only, while `import M as A exposing (...)` also imports the
  selected names unqualified; hidden constructors stay hidden through qualified
  access.

## 2026-04-20 - `.mlfp` source-surface usability slice

- Added located `.mlfp` parser/checker/runner entrypoints while preserving the
  unlocated compatibility APIs. `ProgramDiagnostic` is now the user-facing
  diagnostic wrapper for program errors, with optional source spans, primary
  messages, and hints that are only emitted when mechanically justified.
- Pattern matching now supports nested constructor patterns, variable
  patterns, wildcards, and pattern annotations. Branches remain ordered:
  repeated top constructors are allowed when nested patterns distinguish them,
  while flat duplicates and branches after a catch-all are rejected as
  unreachable. Lowering still targets deferred case obligations and the
  existing Church eliminator representation.
- Added an explicit built-in `Prelude` module for the CLI/file runner and kept
  checker APIs free of implicit imports. Current prelude contents are source
  definitions only: `Nat(..)`, `Option(..)`, `List(..)` with `Nil` and `Cons`,
  `Eq`, `eq`, `and`, and `id`. User modules named `Prelude` conflict with the
  built-in runner prelude.
- Runtime rendering now recovers source ADT heads from lowered Church types and
  prints closed ADT values as source constructors, including parameterized
  payloads such as `Some (Succ Zero)`.
- The user-level contract is now documented in
  `docs/mlfp-language-reference.md`.

## 2026-04-19 - `.mlfp` deferred program obligations through eMLF

- `.mlfp` lowering now records a unified deferred program obligation map for
  overloaded methods, constructors, and case eliminators. Program finalization
  uses the internal detailed eMLF pipeline result, rewrites deferred obligations
  after inference, and reruns xMLF typechecking on rewritten terms.
- Every source constructor occurrence now lowers through a typed placeholder,
  including ordinary applications, bare nullary values, recursive indexed
  constructors, GADT-style constructors, and existential constructors.
  Deferred constructor metadata carries expected-type seeds, occurrence
  templates, constructor-local `forall` binders, and runtime instantiation
  order. Constructor definitions remain Church-encoded runtime bindings.
  Method lowering supports partial overloaded applications via eta expansion;
  bare overloaded methods still fail.
- Program finalization uses internal external binding modes so constructor and
  case placeholders can be inference-local when recursive or indexed evidence
  would be too early as a public external scheme. The unchecked detailed
  pipeline entrypoint is restricted to program obligations and generated
  constructor-forall bindings, and every fully rewritten binding is accepted
  only after the xMLF typecheck guard.
- Source-known overloaded method arguments can still provide expected-type
  guidance, but runtime instance-method selection is deferred until
  post-eMLF finalization. Explicit and derived recursive `Eq Nat` calls remain
  strict runtime coverage.
- Source-type recovery now handles parameterized ADT heads well enough for
  post-eMLF instance recovery such as `Box Nat`.
- The former `emlfPendingSuccessMatrix` rows now live in the strict eMLF
  boundary matrix and pass as runtime `true`: inferred case scrutinees,
  first-class polymorphic constructor and pattern-bound values, partial
  overloaded method application, and parameterized ADT instance recovery.
- Fresh focused verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF"'`
    -> `34 examples, 0 failures`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
    -> `67 examples, 0 failures`
  - `cabal build all && cabal test`
    -> `1616 examples, 0 failures`
  - `git diff --check`
    -> clean

## 2026-04-19 - `.mlfp` first-class polymorphism parity locked

- Added `test/programs/unified/first-class-polymorphism.mlfp` as the durable
  program example for passing a top-level polymorphic value as a first-class
  argument. The `.mlfp` application elaborator now preserves such an argument
  when the callee already expects the full polymorphic scheme and the
  uninstantiated application typechecks.
- Added `ProgramSpec` parity coverage for the current user-facing eMLF surface
  through `.mlfp`: lambda/application, let polymorphism, typed let annotation,
  term annotation, annotated rank-2 lambdas, and first-class polymorphic
  arguments. The lower-level `ElaborationSpec` eMLF regression remains inline
  as an engine check rather than as the program example.
- Promoted the `.mlfp` parity coverage into table-driven matrices. The supported
  surface matrix now includes a local first-class polymorphic argument row.
- Fresh focused verification:
  - `cabal run mlf2 -- run-program test/programs/unified/first-class-polymorphism.mlfp`
    -> `true`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program eMLF"'`
    -> `15 examples, 0 failures`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "first-class polymorphic parameter"'`
    -> `1 example, 0 failures`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
    -> `48 examples, 0 failures`
  - `cabal build all && cabal test`
    -> `1597 examples, 0 failures`

## 2026-04-19 - Complex recursive `.mlfp` corpus fixture

- Added `test/programs/recursive-adt/complex-recursive-program.mlfp` to the
  recursive-ADT corpus. The fixture composes multiple self-recursive `Tree`
  traversals (`mirror`, `leftDepth`, and `rightDepth`) over nested branching
  and checks the result through derived `Eq Nat`.
- `ProgramSpec` now roundtrips and executes ten recursive-ADT fixtures.
- Fresh verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
    -> `46 examples, 0 failures`
  - `cabal build all && cabal test`
    -> `1595 examples, 0 failures`

## 2026-04-19 - `.mlfp` unified path contract hardened

- Executable `.mlfp` bindings now have an explicit documented route:
  `Program.Check -> Program.Elaborate SurfaceExpr -> Program.Finalize ->
  runPipelineElabWithEnv -> xMLF typecheck -> normalize/run`.
- Constructor declarations are validated while building module data
  environments, so invalid result heads fail even when the constructor is
  unused by runtime definitions.
- `runPipelineElabChecked` / `runPipelineElabCheckedWithConfig` remain public
  compatibility aliases; the shared pipeline already returns typechecker-
  authoritative output.

## 2026-04-14 - `.mlfp` now reuses the existing MLF typecheck/runtime path

- `.mlfp` syntax ownership moved under the main frontend boundary via
  `MLF.Frontend.Syntax.Program`, `MLF.Frontend.Parse.Program`, and
  `MLF.Frontend.Pretty.Program`; the older `MLF.Frontend.Program.Syntax`,
  `Parse`, and `Pretty` modules are now thin forwarding seams.
- `.mlfp` expression elaboration now lives in `MLF.Frontend.Program.Elaborate`.
  `Program.Check` assembles module/import/class/data environments, then lowers
  executable expressions into ordinary surface eMLF `SurfaceExpr`s.  The
  downstream `Program.Finalize` handoff normalizes those terms and calls
  `runPipelineElabWithEnv`.
- `MLF.Elab.TypeCheck` owns the typing judgment for checked `.mlfp` terms, and
  `Program.Run` evaluates those checked bindings through the existing xMLF
  runtime instead of a separate `.mlfp` authority/runtime layer.
- Public truth now matches the code: `MLF.API` owns `.mlfp` parse/pretty,
  `MLF.Pipeline` owns `.mlfp` checking/runtime, and `MLF.Program` is a
  compatibility re-export instead of a separate surface.
- Added focused unified-path fixtures:
  - `test/programs/unified/authoritative-let-polymorphism.mlfp`
  - `test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp`
  - `test/programs/unified/authoritative-case-analysis.mlfp`
  - `test/programs/unified/authoritative-overloaded-method.mlfp`
  - `test/programs/unified/authoritative-recursive-let.mlfp`
- Fresh focused verification on current HEAD:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
    -> `27 examples, 0 failures`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`
    -> `24 examples, 0 failures`

## 2026-04-14 - Recursive-ADT programs now run through the main executable

- `app/Main.hs` now recognizes `run-program <file.mlfp>` and evaluates the
  file through `MLF.Program` instead of routing every invocation through the
  older prototype-only entrypoint.
- The runnable recursive-ADT corpus now includes three additional positive
  examples:
  - `test/programs/recursive-adt/recursive-list-tail.mlfp`
  - `test/programs/recursive-adt/recursive-tree-deriving.mlfp`
  - `test/programs/recursive-adt/abstract-module-use.mlfp`
- `ProgramSpec` now roundtrips and runs all nine recursive-ADT samples and
  covers the CLI helper path directly.
- Fresh verification on current HEAD:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
    -> `24 examples, 0 failures`
  - `cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp`
    -> `true`
  - `cabal build all && cabal test`
    -> `1565 examples, 0 failures`

## 2026-04-13 - Fully automatic unannotated iso-recursive inference is now implemented within the inherited boundary

- The repo can now honestly claim **fully automatic unannotated iso-recursive
  inference across its representative family matrix** inside the inherited
  explicit-only / iso-recursive / non-equi-recursive / no-fallback /
  no-second-interface production boundary.
- The decisive proof surface is now current-head, not the April 11
  stop-before-code read. Fresh focused evidence spans the contract's required
  positive and negative classes:
  - `URI-R2-C1 unannotated carrier` proves direct unannotated recursive-carrier
    discovery on both authoritative entrypoints.
  - `URI-R2-C1 reconstruction` proves the same inferred carrier survives
    reconstruction-visible output on authoritative surfaces.
  - `URI-R2-C1 uniqueness reject` plus the preserved ambiguity negatives keep
    multi-candidate and witnessless lanes fail-closed without heuristic
    ranking.
  - `URI-R2-C1 owner-sensitive non-local transparent mediation`,
    `URI-R2-C1 combined wrapper`, and the broader `item-4 edge cases` block
    lock the required non-local, owner-sensitive, binder-sensitive, and
    nested-`forall` unannotated families on authoritative surfaces.
- The April 11 fully-automatic-unannotated stop/Phase-4-closed docs under
  `docs/plans/2026-04-11-fully-automatic-unannotated-iso-recursive-inference-*.md`
  are now predecessor evidence only. Live repo truth is refreshed in
  `docs/plans/2026-04-13-fully-automatic-unannotated-iso-recursive-inference-completion-refresh.md`.

## 2026-04-11 - Representative family matrix now supports the repo-level general automatic iso-recursive claim

- The live current-state aggregate now promotes every positive row in the
  current readiness contract (`P2` through `P6`) above folklore or blocker
  status inside the inherited explicit-only boundary.
- `P2 non-local-propagation` is no longer packet-specific folklore: the new
  `test/Research/P2RepresentativeSupportSpec.hs` harness locks both the exact
  `C1` non-local alias-bound / base-like `Int` packet and a second
  route-pure non-local alias-bound / base-like `Bool` packet as recursive on
  `runPipelineElab` and `runPipelineElabChecked`, giving the family direct
  authoritative support across two admitted packets under the same non-local
  route story.
- That `P2` harness now also includes owner-sensitive non-local wrapper
  packets on the same route, not just the exact source packet. Durable current
  coverage now includes identity-consumer propagation, transparent eta
  mediation, let-aliased transparent eta mediation, and stacked transparent
  eta mediation around the same non-local recursive source for both `Int` and
  `Bool`. This keeps the route representative when ownership moves through
  local wrapper binds instead of only when the packet is returned directly.
- The result-type fallback now also makes its non-local singleton-base support
  explicit instead of inferring it from separate local/non-local booleans:
  `MLF.Elab.Run.ResultType.Fallback.Core` carries a named
  `BaseTargetAdmission` classifier for unique root-base, unique inst-arg-base,
  and scheme-alias/base-like packets, so the unique non-local single-base and
  inst-arg-only singleton-base families are now admitted current-truth rows
  rather than unresolved fail-closed folklore.
- `P5 polymorphism-nested-forall` no longer stops at the historical nonuple
  frontier in the live repo state. Durable current-head coverage now includes
  `sameLaneDecupleAliasFrameClearBoundaryExpr`, and the combined
  same-wrapper nested-`forall` plus owner-local alias packet
  `let id = \z.z in let k = id (\x:μα.α→Int. x) in let hold = k in let u = (\y
  -> y) hold in u` is now recursive on the fallback surface and on both
  authoritative pipeline entrypoints. The same combined family is also now
  durably covered through a decuple owner-local alias chain, and the next
  composed transparent eta-mediated variant
  `let id = \z.z in let wrap = \h.\z.h z in let k = id (\x:μα.α→Int/Bool. x) in let hold = wrap k in (\y -> y) hold`
  is now recursive on both authoritative pipeline entrypoints for both `Int`
  and `Bool` carriers. Both the direct transparent eta mediator and the
  let-aliased transparent eta mediator variant now also stay recursive
  through a decuple owner-local alias chain for both carrier families. The
  next stacked let-aliased transparent mediator family
  `let wrap1 = \h. let mid = h in \z. mid z in let wrap2 = \h. let mid = h in \z. mid z in let hold = wrap2 (wrap1 k) in ...`
  is now also recursive on both authoritative entrypoints for both `Int` and
  `Bool` carriers, and through a decuple owner-local alias chain for both
  carriers. The sibling let-aliased direct-wrapper lane
  `let wrap1 = \h. let mid = h in \z. mid z in let wrap2 = \h. let mid = h in \z. mid z in let hold = wrap2 k in ...`
  is now also stable for both carriers instead of poisoning `wrap2 k` back to
  a stale polymorphic shell, including through a decuple owner-local alias
  chain. The mixed direct/let-aliased stacked lane is also now durably covered
  through a decuple owner-local alias chain for both carriers. The term-level
  transparent-mediator scheme recovery now walks through alias-only `let`
  frames while collecting eta parameters, and it treats forward binder
  references or alpha-inequivalent generalized schemes as narrow recovery
  triggers so malformed binder order and stale wrapper shells no longer block
  the stacked alias-shaped lane. The older nonuple closeout record is
  historical predecessor evidence only. No broader upper frontier beyond the
  newly covered decuple alias-chain plus stacked transparent-eta and
  sibling-direct packets is claimed here yet.
- The mediated unannotated same-wrapper nested-`forall` lane now also stays
  recursive on both authoritative entrypoints: when let-generalization sees an
  identity-like wrapper around a self-recursive unannotated lambda and direct
  node reification no longer exposes the recursive carrier, elaboration now
  reconstructs `μa. a -> τ` from the lambda body result type and the detected
  `f x` self-application. This clears the previously failing
  `let id = \z.z in let f = id (\x -> let _ = f x in 0/true) in (\y -> y) f`
  pocket while preserving the fail-closed witnessless lane
  `let f = id f in f`.
- That unannotated mediated lane is now durably covered through the next
  combined transparent-mediator shapes as well. The already-admitted recovery
  path keeps
  `let id = \z.z in let wrap = \h.\z.h z in let f = id (\x -> let _ = f x in 0/true) in let hold = wrap f in (\y -> y) hold`
  recursive on both authoritative entrypoints, and the same remains true for
  the let-aliased transparent eta form
  `let wrap = \h. let k = h in \z. k z in ...`, plus stacked transparent
  mediators `wrap2 (wrap1 f)`. Current durable coverage now includes those
  direct `Int`/`Bool` packets and their decuple owner-local alias-chain
  variants in `PipelineSpec`, while still making no broader claim beyond the
  tested combined-wrapper family.
- The owner-sensitive non-local analogue of that unannotated transparent-
  mediator family is now also durably covered. Current `PipelineSpec`
  coverage now locks the packets that return the mediator result directly
  (`let hold = wrap f in hold`) for both `Int` and `Bool`, across direct
  transparent wrappers, let-aliased transparent wrappers, stacked transparent
  wrappers, and stacked let-aliased transparent wrappers, on both
  authoritative entrypoints. The same family is now also durably covered
  through a decuple owner-local alias chain for all four wrapper shapes on
  both carriers. No new production edit was required for this slice; the
  current implementation already admitted the route, and the repo now records
  it as durable current truth rather than REPL-only evidence.
- `MLF.Elab.Elaborate.Algebra` now also uses one shared structural recursive
  candidate selector for the live unannotated recovery families instead of
  helper-first branch order. Returned-helper and direct recursive-carrier
  proofs are collected into `StructuralRecursiveCandidateSelection`,
  alpha-equivalent candidates collapse to one proof, and distinct surviving
  candidates now keep the lane fail-closed on genuinely self-recursive RHSs
  instead of choosing the first structural story. That gate is explicitly
  restricted to `annContainsVar v rhsAnn`, so non-recursive alias-wrapper
  lets on the owner-sensitive `P2` route continue to preserve their original
  scheme path. Durable current-head negative coverage now includes the direct
  self-app plus returned-helper multi-cluster packet, which stays fail-closed
  on both authoritative entrypoints.
- The previously fail-closed returned-helper fixed-point lane is now also
  admitted on both authoritative entrypoints: when an unannotated recursive
  helper feeds its own self-application into an outer recursive call
  (`let f = \x -> let g = \y -> f (g y) in g in f`), let-generalization now
  prefers the inferred self-codomain carrier `μa. a -> a` over the
  bottom-collapsed alias placeholder, and the rolled RHS lambda is aligned
  against the unfolded `μ` body before the final `ERoll`. This clears the old
  Phase 7 `TCArgumentMismatch` pocket while preserving the witnessless
  fail-closed lane.
- The retained-child authoritative preservation rewrite now re-closes its
  adjusted term against the root scheme before final output. That preserves
  the settled alias-frame clear-boundary chain after the broader unannotated
  recursive rewrites, instead of dropping a leading unbounded `forall` on
  `runPipelineElab`.
- The authoritative replay gap for the combined nested-`forall` plus
  owner-local alias packet is now closed in `MLF.Elab.Elaborate.Algebra`.
  When `reifyInst` cannot recover an authoritative translation for a
  single-binder identity-like polymorphic wrapper, elaboration now directly
  instantiates that wrapper at the already-typed recursive argument carrier
  when the argument has a contractive recursive witness. This removes the old
  Phase 6 `PhiTranslatabilityError` without widening the fail-closed negative
  lanes.
- The same-lane retained-child fallback path is now cardinality-aware in the
  three live ambiguity shapes that were still positional on April 12: multiple
  sibling retained-child candidates, multiple recursive descendants inside one
  retained-child target, and mixed retained-child/base-target competition now
  keep the lane fail-closed instead of choosing a witness by traversal order
  or family priority. Direct child evidence remains the primary proof source;
  inst-root evidence still acts only as fallback when the child target has no
  direct recursive witness.
- The preserved non-local negatives are now better described as ambiguity
  rejects than as unresolved positives: the multi-inst and inst-arg multi-base
  contrast rows stay reject-side because the current admission model requires a
  unique structural base-target proof, not because the route is still
  unclassified.
- The owner-sensitive non-local unannotated `URI-R2-C1`
  transparent-mediation regression surface now also includes both mixed
  direct/let-aliased stacked wrapper orders, with and without decuple
  owner-local alias shells, on both carriers and both authoritative
  entrypoints. No production-code widening was required for this slice.
- The adjacent unannotated `URI-R2-C1` combined-wrapper surface now also
  includes stacked plain transparent mediators, stacked plain let-aliased
  mediators, and both mixed stacked orders on both authoritative entrypoints,
  with the mixed stacked orders additionally locked through the decuple
  owner-local alias chain. No production-code widening was required there
  either.
- The bounded same-wrapper nested-`forall` transparent-mediator surface now
  also includes the plain stacked transparent `Bool` row, both plain mixed
  stacked orders on both carriers, and the reverse mixed
  let-aliased/direct stacked decuple-alias-chain rows. No production-code
  widening was required for that slice either.
- The `P2` representative-support harness now also includes owner-sensitive
  non-local stacked let-aliased transparent mediators and both mixed stacked
  transparent-mediator orders on both carriers. No production-code widening
  was required for that slice either.
- That same `P2` harness now also includes the next representative
  combined-wrapper owner-sensitive non-local packets: transparent,
  let-aliased transparent, stacked let-aliased transparent, and mixed
  direct/let-aliased stacked transparent mediators on both carriers. No
  production-code widening was required for that slice either.
- `MLF.Elab.Run.ResultType.Util` now owns one shared `CandidateSelection`
  helper for the current recursive-candidate seams. `Fallback.Core` uses it
  for base-target / retained-child cardinality, and `Elaborate.Algebra` uses
  the same helper for the alpha-equivalent helper/direct structural recovery
  lane instead of carrying a second local selector. The private internal
  library now exposes that util seam for direct tests, and the retained-child
  exact-output research harness now locks the decuple alias-frame packet on
  both authoritative entrypoints.
- That retained-child exact-output research harness now also locks the
  same-wrapper nested-`forall` alias-frame and decuple alias-frame packets on
  both authoritative entrypoints, so those already-admitted retained-child
  packets now have explicit exact-output coverage instead of inheriting
  confidence only from broader parity harnesses.
- `MLF.Elab.TermClosure` now preserves direct clear-boundary retained-child
  packets, not just the alias-frame repair lane. When authoritative replay
  reaches the direct same-wrapper nested-`forall` transparent-mediator packet,
  retained-child preservation now returns the recursive RHS directly once the
  surrounding clear-boundary body and recursive witness are confirmed. The
  retained-child exact-output research harness now also locks both that direct
  packet and its decuple alias-shell companion on both authoritative
  entrypoints.
- That retained-child exact-output research harness now also locks the direct
  stacked transparent-mediator same-wrapper nested-`forall` packet and its
  decuple alias-shell companion on both authoritative entrypoints, so the next
  already-admitted stacked transparent family now has exact-output coverage
  instead of relying only on broader type-parity rows.
- `MLF.Elab.Run.Pipeline` now pre-closes retained-child authoritative
  candidates against the root scheme before replaying the preserved child.
  That keeps already-recursive direct packets from skipping the first closure
  pass just because the pre-preserve term happens to be type-closed, which in
  turn closes the direct stacked let-aliased same-wrapper nested-`forall`
  exact-output asymmetry without widening `Elaborate.Algebra`.
- That retained-child exact-output research harness now also locks the direct
  stacked let-aliased transparent-mediator same-wrapper nested-`forall`
  packet and its decuple alias-shell companion on both authoritative
  entrypoints, so the stacked let-aliased family now has the same exact-output
  coverage as the already-admitted direct stacked transparent family.
- That retained-child exact-output research harness now also locks both mixed
  direct/let-aliased stacked same-wrapper nested-`forall` packets and both
  decuple alias-shell companions on both authoritative entrypoints, so the
  mixed stacked transparent-mediator lane now has the same exact-output
  coverage as the already-admitted direct stacked transparent and stacked
  let-aliased families.
- Fresh focused verification on current HEAD is green:
  `Shared candidate selection` -> `3 examples, 0 failures`,
  `same-lane retained-child representative-gap probes` -> `44 examples, 0 failures`,
  `P2 representative-support harness` -> `24 examples, 0 failures`,
  `URI-R2-C1 combined wrapper` -> `24 examples, 0 failures`,
  `URI-R2-C1 owner-sensitive non-local transparent mediation` -> `24 examples, 0 failures`,
  `URI-R2-C1 ambiguity reject` -> `1 example, 0 failures`,
  `item-4 edge cases` -> `85 examples, 0 failures`,
  `same-wrapper nested-forall plus` -> `22 examples, 0 failures`,
  `sameLaneClearBoundaryExpr` -> `5 examples, 0 failures`,
  `ARI-C1 feasibility characterization` -> `74 examples, 0 failures`,
  `cabal build all && cabal test` -> `1539 examples, 0 failures`.
- The superseding current-state records are:
  - `docs/plans/2026-04-11-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification-current-state-refresh.md`
  - `docs/plans/2026-04-11-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision-current-state-refresh.md`
- Under that refreshed ledger, the repo can now honestly claim general
  automatic iso-recursive inference across its representative family matrix
  within the inherited explicit-only / iso-recursive / non-equi-recursive /
  no-fallback / no-second-interface production boundary.
- Verification on current HEAD:
  `./scripts/thesis-conformance-gate.sh` passed, and
  `cabal build all && cabal test` passed with `1420 examples, 0 failures`.

## 2026-04-10 - P5 broader-positive enactment family closed on the merged nonuple frontier

- The broader-positive enactment family now closes on merged `ea8db76`, where
  accepted `round-220` already recorded
  `./scripts/thesis-conformance-gate.sh` and
  `cabal build all && cabal test` with `1365 examples, 0 failures`.
- The enacted positive frontier is the selected same-wrapper nested-`forall`
  packet plus the explicit clear-boundary anchors from
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr` on both
  `runPipelineElab` and `runPipelineElabChecked`.
- `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth only, while
  `sameLaneDecupleAliasFrameClearBoundaryExpr`, deeper alias shells, `P2`,
  `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
  `N6 termination-pressure` stay closed.
- This closeout republishes already accepted evidence only, so no new
  thesis-deviation record is required and `docs/thesis-deviations.yaml`
  remains unchanged.

## 2026-03-29 - Automatic iso-recursive type inference implemented and tested

Automatic iso-recursive type inference is now implemented and tested end-to-end across all pipeline phases. An initial implementation in round-144 was followed by a gap-fix campaign (rounds 146-149) that addressed four specific robustness gaps: witness normalization for TyMu nodes, alias-bounds resolution for recursive types, ELet fixpoint reduction for recursive let-bindings, and result-type fallback opening for non-local recursive reconstruction.

The 5-step mechanism works as follows:

1. **Cycle detection** in `MLF.Constraint.Acyclicity`
   (`breakCyclesAndCheckAcyclicity`) detects cycles in the constraint graph
   and automatically introduces `TyMu` nodes to break them.
2. **Reification** in `MLF.Reify.Type` produces `TMu` types from `TyMu` graph
   nodes.
3. **Elaboration** emits `ERoll`/`EUnroll` coercions for recursive type
   boundaries.
4. **Phase 7 type checker** (`MLF.Elab.TypeCheck`) accepts recursive types
   including `TMu`, `ERoll`, and `EUnroll`.
5. **Phase 7 reducer** (`MLF.Elab.Reduce`) handles roll/unroll reduction steps.

This is an **extension beyond the core thesis**, which assumes acyclic
constraint graphs (Section 9.3). The extension is documented in
`docs/thesis-deviations.yaml` under `DEV-AUTO-ISO-RECURSIVE`.

Non-recursive programs remain unaffected — the cycle detection is a no-op when
no cycles exist, so all existing behavior is preserved identically.

**Current behavior under polymorphic mediation:**
- The mediated-preservation lane stays inside the existing boundary: recursive functions preserve visible `μ` through identity-like polymorphic mediators only when elaboration can already recover a contractive recursive-domain witness from the self reference. Cases without that witness (for example `let rec f = id f`) remain fail-closed instead of synthesizing a new recursive wrapper.
- The supported positive surface is intentionally narrow: the witness-backed `μ/∀` mediation case and the already-supported same-wrapper authoritative packets remain recursive, but broader mediated families are still out of scope until a new witness-backed positive regression is named and verified.

**Resolved gap (non-local proxy elaboration):**
- Non-local proxy `PhiTranslatabilityError` at pipeline entrypoints has been resolved. The `reifyInst` TyMu 0-binder fallback (round 152) and `OpRaise` non-spine bind-parent guard for μ-type nodes (round 153) fixed the two `PhiTranslatabilityError` crash sites. The pipeline now reaches presolution for non-local proxy wrappers. Survey of all 13 `ElaborationSpec` `PhiTranslatabilityError` assertion sites (round 154) confirmed none match the non-local proxy pattern — all are legitimate untranslatable cases.
- `let g = (λx:μα.α→Int. x) in g g` is classified as a **correct semantic error**: in an iso-recursive type system, the function expects `μα.α→Int` but receives a value of function type `μα.α→Int → μα.α→Int`. The current pipeline rejects this during presolution as `ExecError (UnmatchableTypes ... "rigid structural mismatch")`; this recursive proxy is not the thesis's valid annotated self-application construction. Separately, the single-use case `let g = (λx:μα.α→Int. x) in g` succeeds with the correct `μ→μ` arrow type, via two targeted fixes in `MLF.Elab.Elaborate.Algebra`: (1) lambda parameter type fallback from `generalizeAtNode` to `reifyNodeTypePreferringBound` when the former returns a bare `TVar` for a μ-annotated parameter, and (2) selective let-scheme override that replaces unquantified internal `TVar` codomains with the detected contractive μ-type while leaving fully-polymorphic schemes intact for downstream elaboration.

**Test evidence:** all 1177 examples pass, including focused integration tests in
`test/TypeSoundnessSpec.hs` and `test/PipelineSpec.hs` covering simple
self-recursion, nested recursive lets, recursive data patterns, polymorphic recursion,
μ/∀ interaction, higher-order recursion, non-local recursive result types, explicit-μ stability,
and non-local proxy elaboration boundary tests.

## 2026-03-25 - Repo-local orchestrator migrated to revisioned roadmap bundles

- The repo-local orchestrator now resolves its live control plane through
  `orchestrator/state.json` fields `roadmap_id`, `roadmap_revision`, and
  `roadmap_dir` instead of treating top-level `orchestrator/roadmap.md`,
  `orchestrator/retry-subloop.md`, and `orchestrator/verification.md` as
  authoritative.
- Historical roadmap epochs were materialized under
  `orchestrator/roadmaps/<roadmap_id>/rev-###/`, and completed round packets
  now point at stable roadmap bundle paths plus per-round
  `state-snapshot.json` files.
- Completed rounds keep their accepted substantive truth unchanged, but older
  packets now carry migration-added roadmap provenance and, where necessary,
  migration-generated `review-record.json` placeholders for rounds that
  predated the repo-local review-record schema.
- The currently open `round-091` successor loop stays resumable on roadmap
  item `3` under the same same-lane retained-child bounded subject, now using
  the revisioned-roadmap locator contract.

## 2026-03-25 - Strategic loop completed through round-088 and live control plane refreshed for the bounded persistence gate

- The strategic general automatic iso-recursive inference control plane is now
  complete through accepted rounds `round-082` through `round-088`. That
  accepted chain produced the repo-level capability contract, architectural
  constraint audit, mechanism map, search model, full-pipeline reconstruction
  contract, representative coverage campaign, and the item-7 architecture
  decision.
- The authoritative item-7 outcome is
  `continue within the current architecture`, and the selected successor is
  one bounded same-lane retained-child `stable visible persistence` gate
  inside the inherited acyclic model.
- The live orchestrator control plane is refreshed in place for that bounded
  gate. Its roadmap stages are: freeze the exact persistence case and ledger,
  audit the current continuity breakpoint, land the minimum bounded slice or
  proof, revalidate end to end, and record the bounded follow-on decision.
- Completed rounds `round-001` through `round-088` remain predecessor
  evidence, and the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary remains
  unchanged unless a later accepted round explicitly revises it.

## 2026-03-24 - `boundVarTarget` same-lane retained-child packet completed through bounded `N14`

- The live post-`L2` successor control plane is now complete through accepted
  rounds `round-078` through `round-081`, and `orchestrator/state.json` is
  back at idle `stage: "done"` with no pending roadmap item.
- `N11` froze exactly one new bounded target inside
  `MLF.Elab.Run.ResultType.Fallback`: the same-lane local `TypeRef`
  retained-child `boundVarTarget -> targetC` packet only, with
  `schemeBodyTarget` preserved as neighboring boundary context and the earlier
  `baseTarget` lane preserved as predecessor evidence only.
- `N12` landed the bounded implementation slice for that packet in
  `MLF.Elab.Run.ResultType.Fallback` and `test/PipelineSpec.hs` by keeping the
  `boundVarTarget` candidate search unchanged, introducing the explicit
  `sameLaneLocalRetainedChildTarget` proof, and routing only the selected
  retained-child `keepTargetFinal` / `targetC` consumer through that proof.
- `N13` reverified that exact packet under fresh focused and full-repo gates
  (`20 examples, 0 failures`; `1141 examples, 0 failures`) without widening
  scope, and `N14` finalized the bounded next-step token `continue-bounded`
  for the same verified packet.
- Accepted `N14 = continue-bounded` preserves the exact same-lane retained-child
  packet as bounded evidence only. It does not authorize or bind a successor
  lane or next bounded cycle; any future work must begin with a separate
  roadmap amendment/update and must still respect the inherited explicit-only /
  non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
  boundary.

## 2026-03-20 - Bounded `H` cycle runtime complete; successor `I1` bind queued

- The live top-level continue-bounded control plane has now executed the full `H1` through `H4` cycle on repaired `URI-R2-C1`.
- `H1` froze the remaining local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC` lane as the next bounded slice. `H2` then landed the exact bounded implementation in `MLF.Elab.Run.ResultType.Fallback` and `test/PipelineSpec.hs` by introducing the explicit local proof `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`, keeping `baseTarget` fail-closed outside the selected lane, and passing the full repo gate.
- `H3` reverified that exact `H2` lane under fresh read-only anchor checks, a focused rerun of `ARI-C1 feasibility characterization (bounded prototype-only)`, a fresh `cabal build all && cabal test` gate, and predecessor continuity checks.
- `H4` finalized the bounded next-step token `continue-bounded` for the already-reverified `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane. That result is recorded as permission for one more bounded cycle only, not as widening approval.
- The live roadmap now records `H1` through `H4` as done and appends one new pending successor item `I1`, a fresh exact-target bind under the same repaired `URI-R2-C1` subject and inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.

## 2026-03-20 - Live orchestrator refreshed for a bounded `H` cycle

- The completed `G1` through `G4` cycle is now predecessor evidence rather than live work. The live top-level control plane has been refreshed in place for one new bounded non-widening `H1` through `H4` cycle.
- The new approved scaffold source is `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`. The live roadmap now stages `H1` through `H4`: continue-bounded target bind, one bounded fail-closed implementation slice, bounded verification, and a new decision gate.
- The accepted `continue-bounded` outcome from `G4` is treated as permission for another bounded cycle only. It is not interpreted as widening approval.
- The next bounded target family is the still-unselected local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC` lane; it remains out of scope until an accepted `H1` bind freezes it explicitly.
- The live subject remains fixed to repaired `URI-R2-C1`, and the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains mandatory unless a later accepted roadmap update changes it explicitly.

## 2026-03-18 - Live orchestrator refreshed for a continue-bounded follow-on cycle

- The completed initial successor cycle (`U1` through `U6`, rounds `round-028` through `round-033`) is now predecessor evidence rather than live work. The live top-level control plane has been refreshed in place for one new bounded non-widening follow-on cycle.
- The new approved scaffold source is `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`. The live roadmap now stages `C1` through `C4`: continue-bounded target bind, one bounded fail-closed implementation slice, bounded verification, and a new decision gate.
- The accepted `continue-bounded` outcome from `U6` is treated as permission for another bounded cycle only. It is not interpreted as widening approval.
- The live subject remains fixed to repaired `URI-R2-C1`, and the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains mandatory unless a later accepted roadmap update changes it explicitly.

## 2026-03-18 - Unannotated successor `U1` through `U6` runtime complete with bounded `continue-bounded` outcome

- The live top-level successor roadmap is now complete through accepted rounds `round-028` through `round-033`. The controller finished the initial bounded `U1` through `U6` cycle and now rests at terminal `stage: "done"` with `last_completed_round: "round-033"`.
- `U1` through `U4` remained docs-only evidence gates and all finalized fail-closed on the repaired `URI-R2-C1` live subject: `U1` bound the inherited baseline, `U2` narrowed provenance-stable authority, `U3` refuted uniqueness/owner stability without heuristic ranking, and `U4` refuted constructor-directed / acyclic / terminating admissibility without equi-recursive or cyclic widening.
- `U5` landed exactly one bounded production slice in `MLF.Elab.Run.ResultType.Fallback`, with focused `PipelineSpec` coverage. The slice hardens the still-bound repaired `URI-R2-C1` path without authorizing broad unannotated recursive inference, and the full repo gate passed on the accepted round.
- `U6` consumed the authoritative `U1` through `U5` chain plus fresh full-gate verification and finalized the bounded next-step token `continue-bounded`. The cycle therefore ends with evidence for another explicit bounded cycle, not with widening approval and not with a blocked stop.

### 2026-03-11 recursive-types orchestration packet

- Added a repo-tracked, docs-only orchestration packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` for the recursive-types campaign: task memory, milestone mechanism table, orchestration prompt, and authoritative JSONL event log now live together in one task folder.
- The packet fixes role separation explicitly: the orchestrator stays dispatch-only, every planner/researcher/implementer/reviewer/QA/verifier/integrator action uses a fresh agent, retries stay within a round on a clean branch/worktree rooted at the same captured `master`, and only the integrator performs the final commit + `--no-ff` merge.
- Corrected the recursive-types roadmap overview so the milestone order now matches the detailed task breakdown (`M0` semantic freeze, `M1` explicit/core types, `M2` runtime/typechecker/reducer, `M3` public XMLF, `M4` contractiveness, `M5` source syntax, `M6` pipeline-acceptance spike, `M7` optional inference).
- Updated task-workflow guidance to acknowledge optional orchestration artifacts (`mechanism_table.md`, `orchestrator_prompt.md`, `orchestrator-log.jsonl`) for YES/NO-gated campaigns.

### 2026-03-10 warning-free closeout

- Restored a warning-free rebuild after the stabilization landing without changing behavior.
- Cleared the remaining live warning sites by removing redundant imports, renaming shadowed locals, filling the new `psPendingWeakenOwners` field in `PresolutionState` test fixtures, and replacing incomplete test-only list-pattern binds with explicit case analysis.

### 2026-03-10 post-split stabilization-and-landing loop closeout

- Treated the live tree as the already-split baseline and revalidated it with `cabal build all && cabal test` before doing any further stabilization work.
- Confirmed the warning-free cleanup loop needed no additional production edits beyond the already-green split tree.
- Added explicit thin-façade ownership guards for the split runtime façades:
  - `test/PipelineSpec.hs` now locks `MLF.Elab.Phi.Omega`, `MLF.Constraint.Presolution.EdgeUnify`, and `MLF.Elab.Elaborate` to stay thin child-module façades;
  - `test/RepoGuardSpec.hs` now locks `MLF.Reify.Core` and `MLF.Constraint.Solve` as thin façades as well.
- Added stabilization guards for the landed public/module topology:
  - `test/RepoGuardSpec.hs` now asserts `README.md` and `docs/architecture.md` agree on `MLF.API` / `MLF.Pipeline` / `MLF.XMLF` ownership;
  - `test/RepoGuardSpec.hs` now asserts the split child modules stay implementation-only Cabal entries rather than drifting onto the public surface.
- Ran the split-owner regression sweep in the fixed order `Omega` → `EdgeUnify` → `Reify.Core` → `Solve` → `Elaborate`, then reran the full gate; the landing loop completed without reopening the split design.

### 2026-03-10 public-boundary and presolution test-seam cleanup

- Removed the legacy public `MyLib` wrapper and documented the supported downstream surfaces as `MLF.API`, `MLF.Pipeline`, and `MLF.XMLF` only.
- Added explicit public-surface and repository-guard specs: public API contract coverage, test-harness wiring checks, `MyLib` absence checks, and a dual-import guard that rejects simultaneous `MLF.Constraint.Types` + `MLF.Constraint.Types.Graph` imports.
- Narrowed `MLF.Constraint.Presolution` back toward a runtime-focused facade by moving test-only state/copy/interior/building-block helpers behind the new `MLF.Constraint.Presolution.TestSupport` entrypoint; runtime-facing `computePresolution`, `PresolutionResult`, `PresolutionView`, `PresolutionPlanBuilder`, `PresolutionError`, and `EdgeTrace` remain on the main facade.
- Applied low-risk orchestration thinning without semantic changes: `MLF.Constraint.Presolution.Driver.rewriteConstraint` now delegates bind-parent rebuilding to the shared `MLF.Constraint.Presolution.Rewrite.rebuildBindParents`, and `MLF.Elab.Run.Pipeline` now uses explicit snapshot-preparation / redirect-then-canonicalize helpers to keep assembly order visible.

### 2026-03-10 public pipeline hard-cut cleanup

- `MLF.Pipeline` is now the canonical public owner of constraint generation, elaboration, execution, typechecking, and runtime-facing xMLF helpers.
- `MLF.API` is now frontend-only: surface syntax, parsing, pretty-printing, and normalization helpers remain, while pipeline/runtime exports moved off the umbrella surface.
- In-repo downstream callers (`app/Main.hs`, parser/strict-failure specs, and constraint-generation specs) now import execution/runtime behavior from `MLF.Pipeline` directly.

### 2026-03-10 remaining refactor-loop monolith splits

- Completed the low-risk prep loops before the large splits:
  - introduced `EdgeArtifacts` in presolution base plumbing and routed it through `dropTrivialSchemeEdges`, `ElabEnv`, `ResultTypeInputs`, and pipeline/result-type construction;
  - added explicit assembly helpers `TraceCopyArtifacts` / `prepareTraceCopyArtifacts`, `mkInitialPresolutionState`, and `tyExpNodeIds` so pipeline/driver preparation logic is named and localized.
- Split `MLF.Elab.Phi.Omega` into façade + child modules:
  - `MLF.Elab.Phi.Omega.Domain`
  - `MLF.Elab.Phi.Omega.Interpret`
  - `MLF.Elab.Phi.Omega.Normalize`
- Split `MLF.Constraint.Presolution.EdgeUnify` into façade + child modules:
  - `MLF.Constraint.Presolution.EdgeUnify.State`
  - `MLF.Constraint.Presolution.EdgeUnify.Omega`
  - `MLF.Constraint.Presolution.EdgeUnify.Unify`
- Split `MLF.Reify.Core` into façade + child modules:
  - `MLF.Reify.Cache`
  - `MLF.Reify.Named`
  - `MLF.Reify.Type`
  - `MLF.Reify.Bound`
- Split `MLF.Constraint.Solve` into façade + child modules:
  - `MLF.Constraint.Solve.Worklist`
  - `MLF.Constraint.Solve.Harmonize`
  - `MLF.Constraint.Solve.Finalize`
- Split `MLF.Elab.Elaborate` into façade + child modules:
  - `MLF.Elab.Elaborate.Algebra`
  - `MLF.Elab.Elaborate.Scope`
  - `MLF.Elab.Elaborate.Annotation`
- The façade modules remain the export owners; the new children own the internal implementation slices.

### 2026-03-09 presolution state-access single-style cleanup

- Retired the one-off `WithCanonicalT` reader layer from `MLF.Constraint.Presolution.StateAccess`; presolution canonical/constraint reads now use the shared direct `PresolutionM` helpers only.
- Rewrote `MLF.Constraint.Presolution.EdgeUnify.checkNodeLocked` to capture `(Constraint, canonical)` once via `getConstraintAndCanonical` and perform the same strict-ancestor binding walk without the reader wrapper.
- Added a focused source guard to keep the retired reader-layer API from reappearing and updated `Presolution.Base` notes so they describe only the direct state-access pattern.

### 2026-03-09 presolution internal export-surface cleanup

- Retired stale internal presolution re-export surfaces so `MLF.Constraint.Presolution.Driver` no longer re-exports `processInstEdge` and `MLF.Constraint.Presolution.EdgeProcessing` no longer re-exports solve-owned helper operations.
- Kept the public `MLF.Constraint.Presolution` boundary unchanged so the test/runtime-facing `processInstEdge` surface still comes from the actual `EdgeProcessing` owner module.
- Added a focused source guard to keep those stale internal export surfaces from reappearing while preserving the existing public Phase 4 boundary shape.

### 2026-03-09 delayed-weaken owner-boundary surface cleanup

- Retired the dead `flushPendingWeakens` flush-all entrypoint from `MLF.Constraint.Presolution.EdgeUnify` now that owner-boundary delayed-weaken scheduling is the only live presolution drain path.
- Kept `flushPendingWeakensAtOwnerBoundary` and the owner lookup helpers as the authoritative pending-weaken API surface; `EdgeProcessing` scheduling semantics stay unchanged.
- Tightened the row3 guard slices so they continue asserting owner-boundary scheduling markers and additionally forbid the legacy flush-all helper from reappearing in `EdgeUnify`.

### 2026-03-10 pending-weaken façade-boundary cleanup

- Retired the last remaining read-only delayed-weaken façade re-export: `pendingWeakenOwners` now stays owned by `MLF.Constraint.Presolution.EdgeUnify.Omega`, and the only live consumers (`Driver` and `EdgeProcessing`) import it directly.
- Kept the thesis-sensitive mechanics untouched: owner lookup remains in `StateAccess`, owner-boundary flushing stays in `Omega`, and queue stamping still lives in `EdgeUnify.State`.
- Added a focused `PresolutionFacadeSpec` source guard so `MLF.Constraint.Presolution.EdgeUnify` does not grow the `pendingWeakenOwners` façade surface back.
- Retired the dead `pendingWeakenOwnerForNode` / `pendingWeakenOwnerForEdge` alias wrappers so pending-weaken owner queries are now single-sourced directly in `MLF.Constraint.Presolution.StateAccess`, with `EdgeUnify` calling the authoritative helpers by name.
- `MLF.Constraint.Presolution.EdgeProcessing` now reads pending-unify edges and closure seed data through shared `MLF.Constraint.Presolution.StateAccess` helpers instead of peeking `PresolutionState` fields directly; the owner-boundary scheduling algorithm and diagnostics remain unchanged.
- Canonical scheme-root owner/root-set bookkeeping is now shared in `MLF.Elab.Run.Generalize.Common` via `canonicalSchemeRootOwners`; `ResultType.Fallback` and `Generalize.Phase4` consume the same mechanical construction while keeping their surrounding policy/reachability logic local.
- `MLF.Elab.Run.ChiQuery` remains the shared chi-first facade, but no longer carries the derived `chiCanonicalBindParents` convenience helper; the lone fallback caller now reads canonical bind parents directly from `chiCanonicalConstraint`.

### 2026-03-10 state-access dead-export cleanup

### 2026-03-10 chi-query dead-export cleanup

### 2026-03-10 binding-validation dead-export cleanup

### 2026-03-10 canonicalizer dead-export cleanup

### 2026-03-10 run-debug dead-export cleanup

### 2026-03-10 term-closure dead-export cleanup

### 2026-03-10 warning-free import cleanup

- Cleaned up the remaining redundant imports surfaced after the dead-export loop in `MLF.Elab.Phi.Omega.Interpret`, `MLF.Elab.Elaborate.Scope`, `MLF.Elab.Elaborate.Algebra`, `MLF.Elab.Elaborate.Annotation`, and `MLF.Elab.Run.ResultType`.
- The fixes were purely syntactic import removals; no behavior or owner boundaries changed.
- Verified the tree with `cabal build all --ghc-options='-fforce-recomp -Werror'` followed by `cabal test`.

- Retired the unused `closeTermWithSchemeSubst` helper from `MLF.Elab.TermClosure` after revalidating that all live callers use the stricter `closeTermWithSchemeSubstIfNeeded` path or the other still-exported term-closure helpers.
- Kept the Phase 6 term-closure behavior unchanged for live callers; this round removed only the unused eager wrapper.
- Added a focused `PipelineSpec` Phase 6 guard so the dead term-closure export does not reappear.

- Retired the unused `edgeOrigins` helper from `MLF.Elab.Run.Debug` after revalidating that the live result-type/generalize debug surface only uses `debugGaScope`, `debugGaScopeEnabled`, `debugWhenM`, and `debugWhenCondM`.
- Kept the ga-scope debug owner boundary otherwise unchanged; this round removed only the dead edge-origin introspection helper.
- Added a focused `PipelineSpec` guard so the dead debug export does not reappear.

- Retired the unused `canonicalizeRef` helper from `MLF.Constraint.Canonicalizer` after revalidating that all live callers use node-level canonicalization only.
- Kept the canonicalizer boundary focused on `canonicalizeNode`, construction helpers, and redirect-stability behavior; no runtime canonicalization semantics changed.
- Added a focused `CanonicalizerSpec` source guard so the retired ref helper does not reappear.

- Retired the stale `validateSingleGenRoot` export from `MLF.Binding.Validation` after confirming the helper is only used internally within that module.
- Kept the function local so the binding-tree checks still share one implementation without widening the `Binding.Validation` owner surface.
- Added a focused `BindingSpec` source guard so the helper does not reappear in the export list.

- Retired the unused `chiLookupBindParent` and `chiBindParents` passthroughs from `MLF.Elab.Run.ChiQuery` after confirming there were no live production or test call sites.
- Kept the active chi-query boundary focused on the lookups still used by `Elaborate`, `Scope`, `ResultType`, and `ResultType.View`.
- Added a focused `PipelineSpec` chi-first guard so the retired passthrough exports/signatures do not reappear.

- Retired the unused `instEdgeOwnerM` helper from `MLF.Constraint.Presolution.StateAccess` after confirming there were no live production or test call sites.
- Kept the authoritative owner lookup path unchanged: live delayed-weaken ownership still flows through `pendingWeakenOwnerM` / `pendingWeakenOwnerUnder`, and no scheduling or planner logic moved.
- Added a focused `PipelineSpec` source guard so the dead `instEdgeOwnerM` export/signature does not reappear in `StateAccess`.

### 2026-03-08 snapshot preparation single-owner cleanup

- Extracted the shared snapshot-preparation prelude for `PresolutionView` / finalize construction into `MLF.Constraint.Presolution.View` as `SnapshotPreparation`, `prepareSnapshotPreparation`, `prepareSnapshotPreparationFromParts`, and `buildPresolutionView`.
- `MLF.Constraint.Finalize` now reuses that shared preparation instead of maintaining a second UF-sanitization/live-node-filter copy or rebuilding a raw view just to override `pvCanonicalConstraint`.
- The raw-vs-finalized canonical-constraint split remains intentional: `fromPresolutionResult` still uses raw `rewriteConstraintWithUF`, while finalize paths still use repaired/finalized constraint construction.

### 2026-03-08 remaining fallback-removal closeout

- Removed the last GA→no-GA→reify retry ladders from the active elaboration/runtime entrypoints. `SchemeFreeVars` is now surfaced directly rather than retried through weaker generalization or raw reify routes.
- Removed the residual let-level chooser in `MLF.Elab.Elaborate`; let-bound schemes now come only from the authoritative `generalizeAtNode` result, with post-generalization normalization limited to shape-preserving simplification.
- Removed the recursive fallback callback from `MLF.Elab.Run.Generalize` and the recursive scheme fallback branch from `MLF.Elab.Generalize`; when scheme ownership differs, elaboration now uses the already-computed structural scheme plan rather than recursively generalizing another scope.
- Planner scheme-owner resolution is now body-root-only for synthesized wrappers; wrapper-root recovery is no longer representable in `MLF.Constraint.Presolution.EdgeProcessing.Planner`.
- `inferInstAppArgsFromScheme` now uses only structurally justified inference. The old generic fallback branch is gone; partial-application recovery comes from explicit arrow-prefix structure rather than catch-all matching.
- Empty-Ω witness translation now reconstructs instantiations from witness/domain-owned authority only: direct target nodes, trace binder args, and copied nodes from `etCopyMap`. `reifyInst` no longer falls back to `expansionArgs`, and expansion-only application/annotation sites now fail fast unless the source scheme already exactly matches the demanded annotation.
- Annotation closure now distinguishes between genuinely polymorphic annotation subjects and monomorphic/coercion-only subjects, so explicit-forall annotations preserve the intended binders without reintroducing the old compatibility ladders.
- Final scheme normalization/finalization now preserves explicit-forall bounds and rewrites residual rigid placeholder names into stable binders when the authoritative plan proves they are the remaining abstracted variables.
- Verification:
  - `cabal build all && cabal test` — PASS (`998 examples, 0 failures`)

# Implementation Notes

## 2026-03-17 - Live orchestrator successor track now targets bounded progress toward unannotated iso-recursive inference

- The top-level `orchestrator/` no longer points at the completed `URI-R2-C1` replay repair track as live work. That finished track is now predecessor evidence for a successor control plane that keeps making bounded progress toward unannotated iso-recursive solver/pipeline inference.
- The live successor design source is `docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`, and the initial roadmap now stages `U1` through `U6`: inherited-baseline bind, provenance authority clearance, uniqueness/owner-stability clearance, feasibility clearance, one bounded implementation slice, and a bounded next-widening decision gate.
- The live subject starts from repaired `URI-R2-C1`, not from broad automatic recursive inference. The inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary remains mandatory unless a later accepted roadmap update changes it explicitly.
- The successor control plane is intentionally dynamic but fail-closed: after accepted rounds, the guider may refine future pending items or append another bounded cycle, but completed-item truth and subject-boundary continuity must remain intact.

## 2026-03-17 - `URI-R2-C1` repair track completed with bounded `InstBot` replay repair

- The bounded replay repair track is now complete. The live successor roadmap `R1` through `R4` finished across `round-024` through `round-027`, and the live controller now rests at terminal `stage: "done"` with `last_completed_round: "round-027"`.
- `R2` landed the accepted production repair at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch): the locked `URI-R2-C1` replay lane no longer trips the old `InstBot expects ⊥` mismatch, while strict non-replay `InstBot` misuse still fails closed.
- `R3` revalidated that accepted repair on the exact `URI-R2-C1` / `uri-r2-c1-only-v1` lane without reopening broader replay work, and `R4` consumed the authoritative `R1` through `R3` record to finalize the bounded outcome `repair-accepted`.
- The accepted repair track stayed bounded throughout: no second executable interface, no compatibility fallback, and no broadened replay/regression campaign were introduced.

## 2026-03-17 - Live orchestrator successor track now targets the bounded replay repair lane

- The top-level `orchestrator/` no longer points at the completed `D1` through `D4` diagnostic roadmap as live work. That finished track is now explicit predecessor evidence for a bounded repair-track successor roadmap.
- The live successor design source is `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`, and the live roadmap now stages `R1` through `R4`: repair-boundary reproduction, bounded `InstBot` repair, locked replay-path verification, and a terminal repair decision gate.
- The repair scope stays locked to `URI-R2-C1`, `uri-r2-c1-only-v1`, and `witness-replay/applyInstantiation-instbot-precondition`.
- The v2 retry contract remains active, but its live retry-eligible stages are now `R1`, `R2`, and `R3`; `R4` is terminal and may not use accepted semantic retries.
- This scaffold change does not repair production behavior by itself. It prepares the repo-local control plane for a bounded implementation campaign at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).

## 2026-03-16 - `URI-R2-C1` replay root-cause successor track completed

- The live top-level `orchestrator/` successor roadmap for the authoritative `P2-W` replay mismatch is now complete. Rounds `round-020` through `round-023` are accepted historical evidence, not active live work.
- `D2` localized the bounded divergence to `witness-replay/applyInstantiation-instbot-precondition` and assigned the owner account to `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- `D3` established one bounded repair-supporting direction without scope widening: align the `InstBot` precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized boundary.
- `D4` finalized the successor-track aggregate result as `reopen-repair-track`.
- No production repair landed in this track. Any actual implementation change now requires a separate repair-track roadmap that treats `round-020` through `round-023` as predecessor evidence.

## 2026-03-16 — Live orchestrator successor track now targets the `P2` replay root cause

- The top-level `orchestrator/` no longer points at the completed prototype-evidence `P1` through `P4` roadmap as live work. That finished track is now explicit predecessor evidence for a new successor roadmap focused only on the authoritative `P2-W` replay mismatch.
- The live successor design source is `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`, and the live roadmap now stages `D1` through `D4`: replay reproduction, mismatch localization, bounded fixability probing, and a repair-track decision gate.
- The v2 retry contract remains active, but its live retry-eligible stages are now `D1`, `D2`, and `D3`; `D4` is terminal and may not use accepted semantic retries.
- This is still a control-plane-only change. No production solver/runtime behavior changed.

## 2026-03-16 — Orchestrator `contract_version: 2` retry subloop

- The live top-level `orchestrator/` now carries a forward-only retry amendment for future `URI-R2-C1` prototype-evidence rounds. `orchestrator/state.json` records `contract_version: 2` and an optional `retry` block that keeps same-round retries explicit instead of overloading the outer stage machine.
- Review authority is now two-dimensional for `P1` through `P3`: `attempt_verdict` says whether the current attempt counts as valid evidence, while `stage_action` says whether the stage finalizes now or loops back to `plan`. Only `accepted + finalize` becomes authoritative carry-forward for downstream stages.
- Retry history is now meant to stay immutable and auditable: `review.md` remains the latest live review, `reviews/attempt-<n>.md` stores per-attempt snapshots, `attempt-log.jsonl` is controller-owned machine state history, and `review-record.json` is written only when a stage becomes authoritative.
- `P4` remains an aggregate gate, not a semantic retry sink. It may still bounce on ordinary implementation rejection, but it must never emit `accepted + retry`.
- This is a control-plane-only change. Historical rounds `round-016` through `round-019` remain valid `contract_version: 1` evidence, and no production solver/runtime behavior changed.

## 2026-03-13 — M7 explicit-only acyclic graph `TyMu` path

- Phase 1 no longer rejects normalized recursive annotations outright. Instead,
  explicit surface `STMu` annotations lower into a first-class graph `TyMu`
  node whose single structural child is the recursive body, while the binding
  tree records the explicit binder relationship.
- The graph/runtime structural walkers that reason about explicit binders now
  treat `TyMu` like `TyForall`-style structure rather than as a cyclic back
  edge: traversal, copying, normalization, plan/root resolution, and
  acyclicity-sensitive ownership code all descend through the body child.
- Structural reconstruction now preserves the explicit recursive binder
  end-to-end. Constructor-matching unification decomposes `TyMu` by body only,
  and reification/result-type reconstruction rebuild elaborated `TMu` instead
  of falling back to an unfolded surrogate.
- This remains an explicit-only slice. Unannotated programs still do not
  synthesize recursive types, and no equi-recursive equality, cyclic term-DAG
  encoding, implicit unfolding, or solver-wide recursive unification was added.

## 2026-03-12 — M5 surface `μ` exposure with explicit Phase 1 boundary

- The eMLF frontend surface now admits recursive annotation syntax through
  `SrcTy`/`NormSrcType` via `STMu`, with parser acceptance for both `μa. τ` and
  `mu a. τ` and canonical pretty-printing back to `μ`.
- Normalization treats `STMu` as a structural wrapper: it recurses into the
  body, preserves the recursive binder, and still performs the existing
  alias-bound `forall` normalization inside recursive bodies and recursive
  structural bounds.
- M5 intentionally stops at the public/frontend boundary. `generateConstraints`
  and `generateConstraintsCore` now detect any normalized recursive annotation
  subtree and fail with `RecursiveAnnotationNotSupported` before Phase 1
  internalization, so `inferConstraintGraph` / `runPipelineElab` still reject
  recursive surface annotations until explicit M6 lowering work lands.

## 2026-03-09 — ResultTypeView validation single-sourced per computation

- The runtime result-type facade now builds and validates the base `ResultTypeView` once per computation, then threads that validated view through the annotation and fallback workers.
- `MLF.Elab.Run.ResultType.Ann` and `MLF.Elab.Run.ResultType.Fallback` no longer rebuild the base view on the normal runtime path; they consume the already-validated view and only derive overlay variants when overlay semantics are actually needed.
- This preserves the strict malformed-view fail-fast boundary at `buildResultTypeView` while removing redundant setup and keeping row-2/result-type behavior unchanged.

## 2026-03-09 — Presolution compatibility facade retired

- The public Phase 4 presolution entrypoint now imports its owner modules directly instead of routing shared helpers through an extra compatibility layer.
- This keeps the exported presolution/testing surface unchanged while shrinking the maintained module graph and making owner boundaries explicit.
- No thesis-facing behavior changed: presolution execution, replay-map validation, edge-unify test hooks, expansion helpers, and raw-with-raise unification still come from the same underlying owner modules as before.

### 2026-03-08 Task 46 narrowed witness-authoritative closeout
- Removed the last live `Elaborate` scope-root swallow: `scopeRootFromBase` now propagates base `bindingPathToRootLocal` failures instead of silently collapsing to `typeRef root`.
- Kept the successful no-gen-ancestor path unchanged (`Nothing -> typeRef root`), so only malformed base binding trees now fail fast.
- Added a focused `PipelineSpec` source guard, `elab-input witness-authoritative guard`, to prevent the old `Left _ -> typeRef root` fallback from returning in row-1 elaboration.
- Verification:
  - `elab-input witness-authoritative guard` — PASS (`1 example, 0 failures`)
  - `elab-input absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`9 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`1005 examples, 0 failures`)

### 2026-03-08 single-source `schemeBodyTarget` ownership
- Removed the duplicate local `schemeBodyTarget` definition from `MLF.Elab.Elaborate` and made `MLF.Elab.Run.Scope` the single owner of scheme-target selection helpers.
- Kept `schemeBodyTarget` as the thesis `S′`-style subterm target selector, but added `generalizeTargetNode` beside it for the `S`-style named-node generalization case after the richer helper duplicated quantifiers on nested-let / alias regressions.
- Added direct `ScopeSpec` coverage for named non-scheme-root, scheme-root, forall-body, canonical scheme-body alias, and generalization-target behavior, plus a `PipelineSpec` source guard that `Elaborate` no longer defines `schemeBodyTarget ::`.
- Verification:
  - `schemeBodyTarget` — PASS (`6 examples, 0 failures`)
  - `nested` — PASS (`27 examples, 0 failures`)
  - `BUG-002-V2` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`1004 examples, 0 failures`)

## Thesis Alignment (Phase A–E)

### 2026-03-08 retire final non-must-stay solved facade helper cluster
- Removed the remaining non-must-stay public `Solved` helpers (`lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents`) and replaced their owner-local use with direct constraint/canonical logic in `Reify.Core`, `Presolution.View`, and the solved-view parity tests.
- Added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting that final Reify/view helper cluster is absent from the `Solved` facade.
- This exhausts the actionable rows from the solved classification table: the public `Solved` facade now contains only replay-faithful construction, original↔canonical boundary primitives, and strict solved-graph validation.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `MLF.Constraint.Solved` — PASS (`51 examples, 0 failures`)
  - `migration guardrail: thesis-core boundary matches legacy outcome` — PASS (`1 example, 0 failures`)
  - `PresolutionView mirrors solved canonical/node/bound queries` — PASS (`1 example, 0 failures`)
  - `final reify/view helper cluster is absent from the Solved facade` — PASS (`1 example, 0 failures`)

### 2026-03-08 relocate `pruneBindParentsSolved` behind Finalize
- Removed `pruneBindParentsSolved` from the public `MLF.Constraint.Solved` facade and kept the implementation behind `MLF.Constraint.Finalize`, where its only live production owner already was.
- Updated the one test caller in `ElaborationSpec` to use `Finalize.stepPruneSolvedBindParents` and added a direct solved-facade guard asserting the prune helper no longer reappears there.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `MLF.Constraint.Solved` — PASS (`49 examples, 0 failures`)
  - `checked-authoritative does not adapt solved via prune helper at entry` — PASS (`1 example, 0 failures`)
  - `prune helper is absent from the Solved facade` — PASS (`1 example, 0 failures`)

### 2026-03-08 move solved test/audit helper bundle behind test utility
- Added `test/SolvedFacadeTestUtil.hs` as the test-only home for `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement`.
- Removed that helper bundle from the public `MLF.Constraint.Solved` facade and added a direct guard in `test/Constraint/SolvedSpec.hs` proving the facade no longer exposes it.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `MLF.Constraint.Solved` — PASS (`48 examples, 0 failures`)
  - `WitnessDomain` — PASS (`23 examples, 0 failures`)
  - `ga scope` — PASS (`2 examples, 0 failures`)
  - `test-only helper bundle is absent from the Solved facade` — PASS (`1 example, 0 failures`)

### 2026-03-08 retire dead raw canonical container accessors
- Removed `canonicalBindParents` and `canonicalGenNodes` from the `Solved` facade and internal implementation after confirming they had no live code callers.
- Added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting those raw canonical container accessors do not reappear on the `Solved` facade.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `MLF.Constraint.Solved` — PASS (`46 examples, 0 failures`)
  - `raw canonical container accessors are absent from the Solved facade` — PASS (`1 example, 0 failures`)

### 2026-03-08 relocate remaining shared `Solved` compatibility builders
- Split `MLF.Constraint.Solved` into a thin public facade plus non-exposed `MLF.Constraint.Solved.Internal`, preserving `Solved` opacity while moving the shared compatibility builders out of the public surface.
- Redirected `MLF.Constraint.Finalize` and `MLF.Reify.Core` to use the internal builder functions locally, and updated the public-facing solved tests to use `mkTestSolved` plus a direct facade-absence guard for `fromConstraintAndUf` / `rebuildWithConstraint`.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `MLF.Constraint.Solved` — PASS (`45 examples, 0 failures`)
  - `migration guardrail: thesis-core boundary matches legacy outcome` — PASS (`1 example, 0 failures`)
  - `GeneralizeEnv stores canonical maps, not solved handles` — PASS (`1 example, 0 failures`)

### 2026-03-08 narrow `geRes` to canonical map
- Replaced `GeneralizeEnv.geRes :: Solved` with `geCanonicalMap :: IntMap.IntMap NodeId` in the presolution planning context because the environment only used the solved handle to recover a sanitized canonical map for `presolutionViewFromSnapshot`.
- Removed `buildSolvedFromPresolutionView` from `MLF.Constraint.Presolution.Plan` and now derive the preserved canonical map directly with `stepSanitizeSnapshotUf constraint (pvCanonicalMap presolutionView)`.
- Added a direct migration guard in `test/PresolutionSpec.hs` asserting the planning layer stores canonical maps rather than solved handles.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `GeneralizeEnv stores canonical maps, not solved handles` — PASS (`1 example, 0 failures`)
  - `Phase 4 — Principal Presolution` — PASS (`161 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)

### 2026-03-08 retire dead `Solved` mutation hooks
- Removed `rebuildWithNodes`, `rebuildWithBindParents`, and `rebuildWithGenNodes` from `MLF.Constraint.Solved`; no live code callers remained, and the solved classification table had already marked them safe to retire from the production surface.
- Added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting those dead mutation hooks do not reappear on the `Solved` surface.
- Verification:
  - `cabal build all && cabal test` — PASS
  - `dead mutation hooks are absent from the Solved surface` — PASS (`1 example, 0 failures`)
  - `MLF.Constraint.Solved` — PASS (`44 examples, 0 failures`)

### 2026-03-08 solved ecosystem classification table closeout
- Expanded `docs/architecture.md` from a coarse `Solved` cleanup note into a full grouped 3-column classification covering the `Solved` surface plus adjacent solved-related seams.
- Recorded the authoritative evidence matrix in the 2026-03-08 solved classification audit notes, including exact classifications for every exported `Solved` symbol and the main view/finalize/reify/planner compatibility seams.
- Locked the thesis-exact cleanup rule to: keep replay-faithful construction, original↔canonical correspondence, and strict solved-graph validation explicit; relocate compat glue; retire dead/test-only surface from the production API.
- Verification:
  - static audit counts (`32` export entries, `13` direct `src/` importers, `12` direct `test/` importers, `6` named adjacent seams)
  - `MLF.Constraint.Solved` — PASS (`43 examples, 0 failures`)
  - `chi-first guard: runtime and reify modules no longer adapt Solved through fromSolved` — PASS (`1 example, 0 failures`)

### 2026-03-08 deduplicate low-risk helper pairs
- Moved `freshNameLike` into `MLF.Util.Names` and `mapBoundType` into `MLF.Elab.Types`, removing duplicate local helper definitions from their former call sites.
- This was a pure deduplication pass only; no behavior or API shape changed beyond the internal helper homes.
- Verification:
  - `freshNameLike is shared via MLF.Util.Names` — PASS (`1 example, 0 failures`)
  - `mapBoundType is shared via MLF.Elab.Types` — PASS (`1 example, 0 failures`)
  - `MLF.Frontend.Normalize` — PASS (`5 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`978 examples, 0 failures`)

### 2026-03-08 shared frontend/XMLF parser scaffolding
- Extracted shared lexer/literal helpers into `MLF.Parse.Common` and the common type-grammar core into `MLF.Parse.Type`.
- Rewired `MLF.Frontend.Parse` and `MLF.XMLF.Parse` to use that shared scaffolding while keeping their term/computation grammars local.
- Preserved the XMLF-specific forall-binder stopping rule by parameterizing binder-list parsing instead of forcing the frontend and XMLF parsers onto the same exact binder-sequence grammar.
- Added a source guard proving the duplicated lexer/type-helper block no longer lives in both parser modules.
- Verification:
  - `frontend and XMLF parsers share lexer/type scaffolding modules` — PASS (`1 example, 0 failures`)
  - `Frontend eMLF parser` — PASS (`30 examples, 0 failures`)
  - `xMLF parser` — PASS (`8 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`976 examples, 0 failures`)

### 2026-03-09 snapshot canonicalization prelude extraction
- Shared the snapshot-UF preparation seam between `MLF.Constraint.Presolution.View` and `MLF.Constraint.Finalize` by extracting one `SnapshotPreparation` path that sanitizes dead/self union-find entries once and derives the canonical map/query function from that sanitized UF.
- Kept the thesis-sensitive canonical-constraint split unchanged: `fromPresolutionResult` still uses raw `rewriteConstraintWithUF`, while finalize entrypoints still use `repairNonUpperParents` / `finalizeConstraintWithUF` for their canonical constraints.
- Removed the old finalize build-then-override shape so snapshot-driven finalize views now reuse the shared preparation directly instead of constructing a raw `PresolutionView` only to replace `pvCanonicalConstraint`.
- Verification:
  - `PresolutionView mirrors solved canonical/node/bound queries` — PASS (`1 example, 0 failures`)
  - `fromSolveOutput matches explicit pre-rewrite snapshot construction` — PASS (`1 example, 0 failures`)
  - `cabal test --test-show-details=direct` — PASS
  - `cabal build all && cabal test` — PASS

### 2026-03-08 canonicalization helper extraction
- Extracted the duplicated canonicalization helpers shared by `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` into `MLF.Constraint.Canonicalization.Shared`.
- Kept behavior unchanged by rewiring both consumer modules to the same implementation and preserving the existing solved/view semantic parity tests.
- Added a direct source guard ensuring those two modules do not each reintroduce local copies of the helper block.
- Verification:
  - `Canonicalization helper dedup guards` — PASS (`1 example, 0 failures`)
  - `PresolutionView mirrors solved canonical/node/bound queries` — PASS (`1 example, 0 failures`)
  - `Canonicalizer` — PASS (`5 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`975 examples, 0 failures`)

### 2026-03-08 guard-first surface `Expr` fold refactor
- Added direct row1 desugaring-contract tests in `test/FrontendDesugarSpec.hs` for annotated-term lowering, annotated-lambda lowering, nested structural recursion, and typed-let coercion-only behavior.
- Added recursion-schemes support only for `Expr 'Surface ty` in `MLF.Frontend.Syntax` and refactored `MLF.Frontend.Desugar.desugarSurface` to a local `cata`.
- `MLF.Frontend.Normalize` remains explicit and unchanged because binder/capture semantics still dominate there.
- Verification:
  - `MLF.Frontend.Desugar` — PASS (`4 examples, 0 failures`)
  - `desugars annotated lambda parameters via let` — PASS (`1 example, 0 failures`)
  - `ELet with EAnn RHS does not create explicit-scheme instantiation structure` — PASS (`1 example, 0 failures`)
  - `row1 closeout guard|checked-authoritative` — PASS (`2 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`974 examples, 0 failures`)

### 2026-03-07 remove final χp `...View` alias duplicates
- Removed the remaining duplicate `...View` / `...FromView` aliases from runtime and reify helpers; the unsuffixed `PresolutionView`-typed names are now the only canonical APIs.
- Updated runtime, result-type, elaboration, Phi, and test call sites to use the unsuffixed names only.
- Added a direct source guard that duplicate alias names are retired from runtime and reify modules.
- Verification:
  - `ga scope` — PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved` — PASS (`1 example, 0 failures`)
  - `duplicate ...View aliases are retired from runtime and reify modules` — PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`970 examples, 0 failures`)

### 2026-03-07 χp/view-native elaboration closeout
- Removed non-test/non-legacy `fromSolved` usage from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`.
- `PresolutionView` is now the primary internal/runtime API for scope resolution, bound/alias inlining, generalization helpers, result-type generalization, and the non-legacy reify surface.
- The planning/generalization reify context now carries `PresolutionView` snapshots directly; `fromSolved` remains only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
- Verification:
  - `chi-p global cleanup guard: runtime elaboration helpers no longer import fromSolved` — PASS (`1 example, 0 failures`)
  - `chi-p wrapper retirement guard: primary helper signatures are PresolutionView-native` — PASS (`1 example, 0 failures`)
  - `resolveCanonicalScope propagates binding tree cycle errors` — PASS (`1 example, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`969 examples, 0 failures`)

### 2026-03-07 finish χp/view-native elaboration cleanup
- Removed the remaining non-legacy `fromSolved` wrappers from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`.
- `PresolutionView` is now the primary internal/runtime API for elaboration scope helpers, bound/alias inlining, generalization builders, result-type fallback generalization, and reification helpers; `fromSolved` remains only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
- Added a direct source guard asserting runtime/reify modules no longer adapt `Solved` through `fromSolved`.
- Verification:
  - `ga scope` — PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved` — PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `ResultType|Phase 6 — Elaborate|chi-first gate stays green` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`969 examples, 0 failures`)

### 2026-03-07 retire library-side Φ test hooks
- Removed `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge` from the main library; no test-only Φ helper surface remains exposed from `mlf2-internal`.
- Moved the pure witness-domain ranking/de-dup logic into `test/Phi/WitnessDomainUtil.hs` and renamed the dedicated unit suite to `WitnessDomain`.
- `MLF.Elab.Phi.Omega` keeps the same direct replay-spine fail-fast runtime behavior, but now computes witness-domain diagnostic matches locally instead of importing a test-facing bridge module.
- Verification:
  - `WitnessDomain` — PASS (`23 examples, 0 failures`)
  - `Generalize shadow comparator` — PASS (`8 examples, 0 failures`)
  - `no-trace test entrypoint fails fast with MissingEdgeTrace` — PASS (`1 example, 0 failures`)
  - `elab-input thesis-exact guard` — PASS (`2 examples, 0 failures`)
  - `elab-input absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`966 examples, 0 failures`)

### 2026-03-07 Thesis-exact Φ identity cleanup
- Removed the stale compiled `MLF.Elab.Phi.Binder` module and retired its helper re-exports from `MLF.Elab.Phi`, so no compiled Phi surface still advertises the old canonical/base-key/copy-map reconciliation helpers.
- `MLF.Elab.Phi.Omega` remains on the accepted direct replay-spine fail-fast contract; `MLF.Elab.Phi.IdentityBridge` is now documented explicitly as a witness-domain utility/diagnostic/test surface rather than a runtime target-repair engine.
- Added a row9-11 facade cleanup source guard and a dedicated `OpGraft` missing-from-spine regression alongside the existing `OpWeaken` fail-fast coverage.
- Verification:
  - `row9-11 facade cleanup guard` — PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard` — PASS (`1 example, 0 failures`)
  - `OpWeaken on binder target missing from quantifier spine fails fast` — PASS (`1 example, 0 failures`)
  - `OpGraft on binder target missing from quantifier spine still fails fast even when IdentityBridge finds witness-domain matches` — PASS (`1 example, 0 failures`)
  - `IdentityBridge` — PASS (`24 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS (`966 examples, 0 failures`)

### 2026-03-07 TMT improving-loop rerun reclosed row2 and row8
- Row2 `Result-type context wiring` is back to `Yes`: the live pipeline now builds finalized clean/generalized `PresolutionView` artifacts directly from `Finalize.finalizePresolutionViewFromSnapshot`, `ResultType.View` validates from canonical constraint + canonical map, and `ChiQuery` no longer exposes solved-compat shims.
- Row8 `Translatability normalization` is back to `Yes`: `rigidifyTranslatablePresolutionM` now applies §15.2.8 all-inert `W`-normalization before and after rigidification, and the frozen parity oracle is refreshed to freeze the resulting solved artifacts.
- Verification:
  - `row2 absolute thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `row2 closeout guard` — PASS (`3 examples, 0 failures`)
  - `row8 thesis-exact guard` — PASS (`1 example, 0 failures`)
  - `Translatable presolution` — PASS (`10 examples, 0 failures`)
  - `O15-TRANS` — PASS (`5 examples, 0 failures`)
  - `O05-` — PASS (`3 examples, 0 failures`)
  - `Frozen parity artifact baseline` — PASS (`1 example, 0 failures`)
  - `checked-authoritative` — PASS (`8 examples, 0 failures`)
  - `Dual-path verification` — PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test` — PASS.
- This supersedes the temporary “row2 and row8 reopened” classification recorded below.

### 2026-03-07 TMT per-row fresh review reopened row2 and row8
- A new per-row audit reviewed every TMT row with one fresh reviewer agent per row against the live codebase and `papers/these-finale-english.txt`.
- The audit refreshed wording/evidence for rows 1, 3, 6, 12, and 13, and kept rows 4, 5, 7, 9, 10, 11, and 14 materially unchanged.
- It also reopened two thesis-faithfulness gaps:
  - row2 `Result-type context wiring`: the live path still seeds `PresolutionView` from `Solved` and validates through `ChiQuery.chiSolved`, so a hidden solved-compat adapter remains;
  - row8 `Translatability normalization`: live finalization enforces Definition 15.2.10 / Theorem 15.2.11 constructive translatability, but not §15.2.8’s stronger all-inert `W` normalization.
- This supersedes the blanket “all 14 mechanisms yes” closeout claim from the earlier 2026-03-07 verifier sweep section below.

### 2026-03-06 stale non-root `OpWeaken` pruning for BUG-2026-02-06-002
- Recovered `let-c1-apply-bool` without relaxing Ω strictness.
- `MLF.Constraint.Presolution.WitnessNorm` now prunes a non-root `OpWeaken`
  only after finalized source/replay binder domains are known, and only when
  the target is absent from those finalized domains and its bound skeleton is
  no longer fully abstract.
- This keeps the producer-side distinction between:
  - dead residue: top-level stale weakens whose target path has concretized
    leaves (for `let-c1-apply-bool`, the bound chain reaches `Int`);
  - live semantic weaken: under-lambda strict weakens whose target path remains
    fully abstract (`BUG-002-V4`).
- Updated regressions:
  - `test/PipelineSpec.hs`
    - `make let-c1-apply-bool path typechecks to Int`
    - `make let-c1-apply-bool prunes the stale non-root OpWeaken before Phi`
    - `BUG-002-V4 keeps the strict non-root OpWeaken when c1 stays abstract under lambda`
    - BUG-002 sentinel/strict-target matrix rows now assert the actual checked
      success outputs (`TBottom -> Int` for `make-app` / `let-c1-return`,
      `Int` for `let-c1-apply-bool`)
  - `test/ThesisFixDirectionSpec.hs`
    - checked + unchecked BUG-002 thesis target now both assert `Int`
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bottom-int arrow"'`: PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "let-c1-apply-bool"'`: PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'`: PASS
  - `cabal build all && cabal test`: PASS (`956 examples, 0 failures`)

### 2026-03-07 TMT full-sweep closeout sync
- The 2026-03-05 row6 `MAXIMUMRETRY` orchestrator run is now historical-only evidence.
- It was superseded first by the 2026-03-06 row6 replay-contract recovery and
  then by the fresh round-2 full verifier sweep archived under
  `/Volumes/src/mlf4/docs/plans/2026-03-06-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-2.md`.
- Historical result at the time: that fresh sweep re-evaluated all 14 TMT mechanisms from a green base and returned `YES` for every row, with `cabal build all && cabal test` passing (`959 examples, 0 failures`).
- Later on 2026-03-07, a stricter per-row thesis/code review reopened row2 and row8; treat the sweep as historical verifier evidence, not the final live classification.

### 2026-03-06 Task 48 row6 replay-contract recovery closeout
- Closed the post-orchestrator replay-contract recovery from a clean green base
  after the fresh round-1 `MAXIMUMRETRY` regression.
- `MLF.Constraint.Presolution.WitnessNorm` now treats no-replay projection as a
  source-domain contract:
  - wrapper-vs-semantic classification uses restored/source identities rather
    than rewritten canonical ids;
  - no-replay `OpWeaken` keeps the historical graft-target heuristic
    (`graftTargetCount`) in source space, so valid success paths
    (`\y. let id = (\x. x) in id y`, A6, annotation-heavy baselines) stay green;
  - strict no-replay remains available only when a surviving source-domain
    non-root `OpWeaken` remains (bug-002 path).
- Producer fail-fast is now narrowed and explicit:
  - residual no-replay replay-family rejection applies to single-target,
    source-interior rogue `OpGraft` shapes that cannot be projected away;
  - wrapper `OpRaise` under `GenRef` / missing type-tree binding is pruned
    before Phi, while type-tree-bound invalid raises still fail via
    `R-RAISE-INVALID-11`.
- Recovery verification evidence:
  - row6/no-replay witness obligations: PASS
  - `checked-authoritative`: PASS
  - `Dual-path verification`: PASS
  - `cabal build all && cabal test`: PASS (`954 examples, 0 failures`)

### 2026-03-05 Row6 orchestrated execution (historical blocked run; superseded)
- Historical record only: this blocked run was superseded by the 2026-03-06
  replay-contract recovery and the 2026-03-07 fresh round-2 full sweep.
- Executed
  `/Volumes/src/mlf4/docs/plans/archive/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`
  against task tracker
  `/Volumes/src/mlf4/docs/plans/archive/2026-03-05-orchestrated-execution-improving-loop-agent-prompt-codex-subagents-fresh-round-1.md`.
- Round-1 target was row6 (`Replay-map producer normalization`), with strict
  role-separated gates across six attempts.
- Terminal outcome for this run: attempt-limit exhaustion (`MAXIMUMRETRY`).
- Final attempt baseline regressed system health:
  - `cabal build all && cabal test` -> FAIL (`126 failures`), dominated by
    `WitnessNormalizationError ReplayMapIncomplete`.
  - Prior required sanity slices now unstable in full baseline:
    `Phase 4 thesis-exact unification closure`,
    `checked-authoritative`, `Dual-path verification`.
- Historical follow-up direction captured at the time:
  - treat replay contract redesign as cross-phase work (producer + consumer),
    then recover baseline green before re-attempting row6 closeout.

### 2026-03-05 Task 47 row3 strict owner-boundary scheduling closeout (agent-team execution)
- Closed the remaining row3 strict gap around owner-boundary delayed-weaken
  scheduling in presolution.
- Removed flush-all-owner boundary fallback shape from
  `MLF.Constraint.Presolution.EdgeProcessing` and kept strict boundary
  invariants (`closed owner` flush + residual-owner fail-fast checks).
- Added stable owner provenance for pending weakens:
  - `EdgeUnify` now stamps owner buckets at enqueue-time and carries them in
    presolution state (`psPendingWeakenOwners`) so boundary selection is not
    derived from mutable post-merge graph shape.
  - Edge-local omega weaken queueing uses edge-local meta identity and
    edge-owner context wiring from `runExpansionUnify`.
- Hardened diagnostics:
  - boundary/finalization violations now report pending owner buckets from both
    edge-loop and driver finalization checks.
- Added/updated strict guards:
  - `Pipeline (Phases 1-5) / row3 absolute thesis-exact guard` now asserts the
    flush-all-owner fallback pattern is absent.
  - `Phase 4 thesis-exact unification closure` remains green with owner-stamped
    boundary scheduling.
- Verification evidence:
  - `--match "row3 absolute thesis-exact guard"` -> PASS (`6 examples`)
  - `--match "Phase 4 thesis-exact unification closure"` -> PASS (`11 examples`)
  - `--match "Translatable presolution"` -> PASS (`8 examples`)
  - `--match "generalizes reused constructors via make const"` -> PASS (`1 example`)
  - `--match "BUG-002-V1"` -> PASS (`1 example`)
  - `--match "Frozen parity artifact baseline"` -> PASS (`1 example`)
  - `--match "checked-authoritative"` -> PASS (`8 examples`)
  - `--match "Dual-path verification"` -> PASS (`4 examples`)
  - `cabal build all && cabal test` -> PASS.

### 2026-03-05 Task 42 row2 absolute thesis-exact hardening (agent-team execution)
- Completed strict wave-based hardening for TMT row `Result-type context wiring`
  with ownership-enforced Team A-E execution.
- Retired row2-local solved-overlay/materialization surfaces:
  - removed `rtvSolved`, `rtvOriginalConstraint`, and `solveFromInputs` from
    `MLF.Elab.Run.ResultType.View`;
  - migrated `Ann`/`Fallback`/`Util` consumers from `View.rtvSolved` and
    solved-only scope/reify helpers to view-native `PresolutionView` paths.
- Preserved strict malformed-view fail-fast semantics at
  `buildResultTypeView` via canonical graph validation and `ValidationFailed`
  error propagation.
- Added and locked regression guard:
  - `Pipeline (Phases 1-5) / Integration Tests / row2 absolute thesis-exact guard`
    asserting row2 solved-overlay surfaces remain absent.
- Verification evidence (required order):
  - RED proof before Wave 1:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
      -> FAIL (`1 example, 1 failure`).
  - GREEN after integration:
    - `--match "row2 absolute thesis-exact guard"` -> PASS (`1 example`)
    - `--match "row2 closeout guard"` -> PASS (`3 examples`)
    - `--match "checked-authoritative"` -> PASS (`8 examples`)
    - `--match "Dual-path verification"` -> PASS (`4 examples`)
    - `cabal build all && cabal test` -> PASS (`935 examples, 0 failures`)

### 2026-03-05 Task 41 absolute strict all-path hardening (agent-team execution)
- Completed strict-wave hardening for row `Elaboration input` with explicit
  ownership splits and integration gates.
- Removed residual non-thesis surfaces targeted by the absolute guard:
  - `MLF.Elab.Phi.Env` no longer carries solved-backed `peResult` /
    `askResult` helper surface.
  - `MLF.Elab.Run.Scope` no longer keeps the redundant `preferGenScope`
    re-lookup helper; ga′ scope now flows directly from `bindingScopeRef` to
    `canonicalizeScopeRef`, and binding-tree errors continue to propagate.
  - `MLF.Elab.Phi.TestOnly` no longer exports/implements
    `phiFromEdgeWitnessAutoTrace`; no-trace helper remains strict fail-fast
    (`MissingEdgeTrace`).
- Added and locked regression guard:
  - `Pipeline (Phases 1-5) / Integration Tests / elab-input absolute thesis-exact guard`
    asserts absence of the three residual surfaces above.
- Verification evidence (required order):
  - RED proof before implementation:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
      -> FAIL (guard sees residual surface).
  - GREEN after integration:
    - `--match "elab-input absolute thesis-exact guard"` -> PASS (`1 example`)
    - `--match "checked-authoritative"` -> PASS (`8 examples`)
    - `--match "Dual-path verification"` -> PASS (`4 examples`)
    - `cabal build all && cabal test` -> PASS (`934 examples, 0 failures`)

### 2026-03-04 docs closeout: strict-policy elaboration-input alignment
- Strict table policy remains unchanged: thesis-exact classification includes
  test-only code paths.
- Retired solved-typed test-only Φ surfaces in `MLF.Elab.Phi.TestOnly`:
  `phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and
  `phiFromEdgeWitnessAutoTrace` now use chi-native `GeneralizeAtWith` callback
  shape (no `Solved`-typed helper signatures).
- No-trace test entrypoint remains strict fail-fast (`MissingEdgeTrace`), so
  the migration preserves the trace contract while removing solved-typed test
  surfaces.
- Row `Elaboration input` is now `Thesis-exact = Yes` under the strict policy,
  conditional on the existing closeout gates (`elab-input thesis-exact guard`,
  `checked-authoritative`, `Dual-path verification`, and full gate).

### 2026-03-04 Task 39 strict legacy-retirement closeout (Team E verification)
- Closed the strict elaboration-input criterion that includes test-only paths:
  - production elaboration/Phi modules no longer expose solved-typed
    compatibility entrypoints;
  - test-only Phi helpers use the chi-native callback shape
    (`GeneralizeAtWith` without a solved-typed callback argument);
  - fail-fast no-trace invariant is preserved (`MissingEdgeTrace`).
- Updated TMT row `Elaboration input` to `Thesis-exact = Yes` only after all
  required closeout gates passed in this workspace.
- Verification evidence (required gates, run with temporary
  `HOME=/tmp/codex-home` and `XDG_CACHE_HOME=/tmp/codex-cache` to avoid cache
  permission failures while keeping command semantics unchanged):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)

### 2026-03-04 Wave 4 docs closeout (Task 38 agent-team replan)
- Closed Team E docs/verifier handoff for
  the historical 2026-03-04 elab-input replan tracker (not retained as a standalone archive directory).
- Thesis contract references for this migration are now explicit in closeout
  docs:
  - `papers/these-finale-english.txt` Def. 15.3.12 (translation starts from
    translatable `χp` and chosen per-edge witnesses)
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5 (edge-witness
    translation pipeline into term elaboration)
- Runtime closure references recorded in TMT:
  - `runPipelineElabWith` threads `ecGeneralizeAtWith` and
    `eePresolutionView` into `elaborateWithEnv`
    (`src/MLF/Elab/Run/Pipeline.hs:112-141`);
  - `reifyInst` calls `phiFromEdgeWitnessWithTrace` with `presolutionView`
    (`src/MLF/Elab/Elaborate.hs:917-949`);
  - active Φ entry/core signatures are `PresolutionView`-based
    (`src/MLF/Elab/Phi/Translate.hs:284-317`).
- Recorded verification evidence from already-run Wave 3 gates:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)

### 2026-03-04 Task 35 elaboration-input thesis-exact closeout
- Elaboration input row is now closed as thesis-exact for active runtime flow:
  - active elaboration path no longer depends on `ChiQuery.chiSolved` materialization in `elaborateWithEnv`;
  - active Elaborate/Phi generalize callback shape uses `χp`-native inputs;
  - checked-authoritative behavior remains unchanged on representative slices.
- Verification evidence (Task 35 gates):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)
- Historical note (superseded by Task 39 strict closeout):
  - this task closed active-path boundaries; Task 39 subsequently retired the
    remaining solved-typed production compatibility surfaces and updated
    test-only callback contracts.

### 2026-03-04 Wave 3 Task 6 verifier closeout (row-2 adapter retirement evidence)
- Row-2 adapter retirement is closed in runtime boundaries:
  - `ResultTypeInputs` no longer exposes `rtcSolvedCompat`/`rtcSolveLike`.
  - `ElabConfig` no longer includes `ecSolved`.
  - Guard search confirms adapter symbols now appear only in row2 closeout
    tests, not in `src/` runtime modules.
- Verification evidence captured for closeout:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    - PASS (`3 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`929 examples, 0 failures`)
- Post-row2 priorities:
  1. Reduce `ResultType.View` solved-overlay scaffolding (`rtvSolved` +
     bound-overlay rebuild path) where equivalent `χp`-native queries exist.
  2. Continue simplifying compatibility-shaped helper signatures that still
     thread `Solved` through generalize/reify flows.
  3. Keep row2 closeout guard slices in regular regression cadence.

### 2026-03-03 Wave 3 Task 6 verifier closeout (row-1 chi-first boundary evidence)
- Row-1 runtime boundary shape is now explicit and stable:
  - `ElabEnv` carries `eePresolutionView`, GA parents, edge artifacts, and
    scope overrides (no `eeSolvedCompat` field).
  - `elaborateWithEnv` no longer performs entry-time
    `Solved.rebuildWithConstraint`; chi-first queries flow through `ChiQuery`.
  - Production pipeline remains checked-authoritative; result-type
    reconstruction remains diagnostic-only.
- Explicit adapters still present (row-2 follow-up surface):
  - `rtcSolvedCompat` + `rtcSolveLike` at the result-type boundary.
  - `ElabConfig.ecSolved` as an elaboration compatibility input for existing
    generalize/reify helper signatures.
- Verification evidence for this closeout:
  - Requested combined matcher
    `--match "row1 closeout guard|checked-authoritative|Dual-path verification"`
    selected `0 examples` (PASS, empty selection).
  - Required narrow fallback slices:
    - `--match "row1 closeout guard"`: PASS (`2 examples, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`7 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - Full gate: `cabal build all && cabal test` PASS.
- Ordered next steps (row-2):
  1. Move result-type bound-overlay/materialization to `χp`-native views.
  2. Remove `rtcSolveLike` from `ResultType.View` construction.
  3. Remove `rtcSolvedCompat` from `ResultTypeInputs` and pipeline wiring.
  4. Re-evaluate `ElabConfig.ecSolved` removal once row-2 adapters are gone.

### 2026-03-03 Task 31 chi-first elaboration/result-type internal cleanup (Tasks 1-6 complete)
- Completed the chi-first migration plan at
  `docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md`
  with wave gates green.
- Added shared chi-query facade for elaboration/result-type internals:
  - new `MLF.Elab.Run.ChiQuery` centralizes `PresolutionView` reads
    (canonical, node lookup, var-bound lookup, bind-parent lookup, and
    canonical-constraint access).
- Result-type internals now prefer `χp` reads:
  - `ResultType.View` routes runtime node/bound reads through `ChiQuery`,
    retaining solved compatibility only in `rtcSolveLike` and bound-overlay
    materialization needed by legacy helper signatures.
- Elaborate internals now prefer `χp` reads:
  - `ElabEnv` carries `eePresolutionView` plus runtime edge/scope artifacts
    (no `eeSolvedCompat` field);
  - `elaborateWithEnv` uses `ChiQuery` for canonicalization/boundary queries
    and avoids local solved-from-constraint materialization.
- Pipeline boundary integration:
  - added `mkResultTypeInputs` and wired pipeline result-type/elaboration
    setup through explicit compatibility inputs (`rtcSolvedCompat`,
    `ecSolved`) instead of ad hoc internal reconstruction.
- Guardrails and verification:
  - source-level chi-first guard tests and phase-gate matcher aliases were
    added in `PipelineSpec`/`ElaborationSpec`;
  - Gate A: `--match "chi-first guard"` PASS;
  - Gate B: `--match "ResultType|Phase 6 — Elaborate|chi-first"` PASS;
  - Task 6 closeout slice:
    `--match "Phase 6 — Elaborate|ResultType|Dual-path verification"` PASS;
  - Gate C: `cabal build all && cabal test` PASS (`923 examples, 0 failures`).

### 2026-03-03 Task 29 solved follow-up closure (Phases 1-6 complete)
- Completed solved-boundary follow-ups from `TODO.md` with end-to-end green validation (`cabal build all && cabal test`: `913 examples, 0 failures`).
- Consolidated solved-to-view projection at the presolution boundary:
  - shared adapter `MLF.Constraint.Presolution.View.fromSolved`
  - removed duplicated runtime adapters in elaboration/pipeline call sites.
- Replaced runtime construction dependencies on test-only naming:
  - added production-safe `Solved.fromConstraintAndUf`
  - migrated runtime planner/reify uses off `mkTestSolved` naming.
- Tightened solved/base mapping handling in presolution planning:
  - introduced explicit `SolvedToBaseResolution` classification (`mapped | same-domain | missing`)
  - routed scope/target fallback handling through typed resolution.
- Expanded and hardened guard coverage:
  - solved invariant checks (`validateOriginalCanonicalAgreement`, `canonicalizedBindParents`)
  - isolated O15 empty-sequence translation guard (`Trχ(ε)=ε` with `Σ(g)=ε`)
  - AAnn result-type primary vs fallback equivalence with populated GA mapping assertions.
- Closed review follow-ups:
  - removed tautological constructor parity test and replaced with semantic constructor invariants,
  - narrowed adapter guard wording to avoid overclaiming corpus scope.

### 2026-03-03 Task 30 solved compatibility-read reduction (Waves 0-4 complete)
- Reduced compatibility-oriented internal solved reads across generalize/result-type internals while preserving behavior:
  - removed unused context compatibility fields (`gcConstraintForReify`, `rbConstraintForReify`);
  - added lightweight context trace when `SolvedToBaseMissing` is hit for a node present in base-domain constraints.
- Generalize reify flow cleanup:
  - alias solved rebuild is now gated to non-OnConstraint branches;
  - explicit-bound helper reification now uses OnConstraint bound reads when structural-scheme path is authoritative.
- Result-type solved-read centralization:
  - added `MLF.Elab.Run.ResultType.View` as a read boundary;
  - confined `rtcSolveLike` usage to view construction;
  - refactored `ResultType`, `Ann`, and `Fallback` to consume the view interface.
- Fallback high-risk path replacement:
  - removed local `Solved.rebuildWithNodes` patching in fallback core;
  - introduced bound-overlay materialization at the view boundary while preserving target-selection and `bindParentsGaFinal` semantics.
- Regression coverage additions:
  - `generalizeWithPlan` GA->no-GA fallback ladder on `SchemeFreeVars` and double-`SchemeFreeVars` reify fallback;
  - integrated result-type fallback handling for `gaSolvedToBase` `same-domain` and `missing` roots.
- Validation:
  - focused carry-forward + new checks: pass;
  - full gate: `cabal build all && cabal test` => `917 examples, 0 failures`.

### 2026-03-03 Runtime thesis-exact elaboration-input strict checklist closeout
- Closed the remaining runtime row-1 boundary gaps tracked in
  `docs/plans/2026-03-02-runtime-thesis-exact-elab-input-implementation-plan.md`
  with a strict 5-item checklist.
- Runtime replay/mediation removal:
  - removed direct production `Solved.fromPreRewriteState` /
    `solveResultFromSnapshot` calls from `MLF.Elab.Run.Pipeline`;
  - deleted inline `setSolvedConstraint` replay helper path from
    `Pipeline.hs` and removed `MLF.Elab.Run.PipelineBoundary`.
  - introduced `MLF.Constraint.Finalize` as the shared runtime finalization
    boundary used by pipeline/runtime paths.
  - restored full snapshot-finalization semantics in the shared boundary by
    reusing `Solve.finalizeConstraintWithUF` (UF rewrite, eliminated-binder
    rewrite, UF substitution update, bind-parent pruning, strict validation).
- Result-type replay removal:
  - `rtcSolveLike` no longer calls replay reconstruction; it now materializes
    solved state from `PresolutionView` canonical data (`pvCanonicalConstraint`
    + `pvCanonicalMap`).
- Elaboration boundary wiring completion:
  - `ElabEnv` no longer carries solved compatibility state (`eeSolvedCompat`);
  - `elaborateWithEnv` no longer performs entry-time solved reconstruction
    (`Solved.rebuildWithConstraint`);
  - compatibility solved access remains explicit in `ElabConfig.ecSolved`.
- Added executable closeout tests (exact plan-matcher names):
  - `row1 boundary uses thesis-core elaboration input contract`
  - `elaborateWithEnv consumes thesis-core input`
  - `row1 boundary validates-only and does not mediate input`
  - `migration guardrail: thesis-core boundary matches legacy outcome`
  - `final row1 state uses single thesis-core boundary path`
  - `Dual-path verification`
- Migration guardrail alignment note:
  - thesis-core vs legacy canonical-map checks now compare on shared live-node
    domain, while preserving strict canonical-constraint and solved-query parity
    assertions; legacy eliminated-node-only canonical links are treated as
    historical metadata, not runtime-domain divergence.
- Validation:
  - targeted closeout slices above: PASS;
  - regression anchors: `Phase 6 — Elaborate` PASS, `Pipeline (Phases 1-5)` PASS, `Dual-path verification` PASS;
  - full gate: `cabal build all && cabal test` PASS.

### 2026-03-05 TMT row `Ordering of transformations` wave execution (Task 44)
- Implemented the row-ordering agent-team plan from
  `docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md`.
- Wave 0 RED guard:
  - Added `row3 ordering thesis-exact guard` checks in `PipelineSpec` and
    a semantic closure characterization in `UnificationClosureSpec`.
  - Confirmed RED baseline (`2 examples, 2 failures`) before refactors.
- Wave 1+2 core refactor:
  - `MLF.Constraint.Presolution.EdgeProcessing` now integrates delayed-weaken
    flushing within the edge-loop boundary machinery and preserves per-edge
    unify-closure fail-fast checks.
  - `MLF.Constraint.Presolution.EdgeUnify.flushPendingWeakens` was hardened for
    repeated invocation/no-op safety on stale targets.
  - `MLF.Constraint.Presolution.Driver` no longer performs global post-loop
    `flushPendingWeakens`; post-loop work is now explicit
    `runFinalizationStage`:
    - materialization,
    - rewrite/canonicalization,
    - rigidification for translatability,
    - witness normalization,
    with construction checkpoints for pending queues, TyExp coverage/removal,
    and witness/trace domain alignment.
- Regression handling:
  - Initial per-edge weaken flushing triggered
    `OperationOnLockedNode` regressions in reused-constructor paths
    (`generalizes reused constructors via make const`, `BUG-002-V1`) and frozen
    parity drift.
  - Edge-loop scheduling was adjusted so weaken queues are allowed intra-loop
    while preserving per-edge unify-closure boundaries; strict queue drain is
    enforced at the loop-final boundary.
- Verification:
  - `row3 ordering thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `Phase 4 thesis-exact unification closure`: PASS (`8 examples, 0 failures`)
  - `Translatable presolution`: PASS (`8 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - full gate: `cabal build all && cabal test` PASS
  - full-suite direct evidence: `cabal test mlf2-test --test-show-details=direct`
    PASS (`938 examples, 0 failures`)
- Thesis-exact classification note:
  - Row remains `No` in the TMT because weaken flushing is still loop-final
    rather than strictly per-edge after each propagation step.

### 2026-03-05 TMT row3 absolute ordering follow-up execution (Task 45)
- Executed the follow-up agent-team plan from
  `docs/plans/2026-03-05-tmt-row3-ordering-absolute-thesis-exact-agent-team-implementation-plan.md`
  with Wave 0..4 ownership boundaries and sequential gates.
- Wave 0 RED contracts:
  - added strict matcher `row3 absolute thesis-exact guard` in
    `PipelineSpec` and `Presolution.UnificationClosureSpec`;
  - confirmed RED baseline before implementation (`4 examples, 4 failures`).
- Wave 1 integration:
  - added pending-weaken ownership APIs (`PendingWeakenOwner`,
    owner-lookup helpers, owner-boundary flush API surface) in presolution
    base/state/edge-unify layers;
  - rewired `EdgeProcessing` loop to owner-boundary scheduling hooks:
    `scheduleWeakensByOwnerBoundary`,
    `flushPendingWeakensAtOwnerBoundary`,
    `assertNoPendingWeakensOutsideOwnerBoundary`;
  - removed loop-final-only fallback shape
    (`flushPendingWeakens` + `drainPendingUnifyClosureIfNeeded`).
- Wave 2/3 regression + fix:
  - verification exposed residual pending-weaken boundary failures in
    `Phase 4 thesis-exact unification closure`
    (`pending weakens` remained after edge-loop boundary);
  - root cause: planner-owner boundary key and pending-node owner buckets could
    diverge;
  - fix: boundary scheduler now flushes all currently pending owner buckets at
    each owner boundary and reasserts owner-bucket emptiness post-flush.
- Verification evidence (strict required stack):
  - `row3 absolute thesis-exact guard`: PASS (`4 examples, 0 failures`)
  - `Phase 4 thesis-exact unification closure`: PASS (`10 examples, 0 failures`)
  - `Translatable presolution`: PASS (`8 examples, 0 failures`)
  - `generalizes reused constructors via make const`: PASS (`1 example, 0 failures`)
  - `BUG-002-V1`: PASS (`1 example, 0 failures`)
  - `Frozen parity artifact baseline`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - final gate `cabal build all && cabal test`: PASS
    (`942 examples, 0 failures` from `mlf2-test` log summary)
- Classification note:
  - Row remains `Thesis-exact = No` under strict criterion.
  - Current boundary scheduler is thesis-shape aligned and no longer
    loop-final-only, but remains compatibility-conservative
    (flush-all-pending-owner-buckets at boundaries) rather than a fully proven
    per-edge owner-local weaken schedule.

### 2026-03-01 TMT3 Wave 3 docs closeout (all-aligned policy)
- Transformation Mechanism Table (`docs/notes/2026-02-27-transformation-mechanism-table.md`) is now fully all-aligned for the current branch: every row is `Aligned` and no row references active `DEV-TMT-*` IDs.
- `docs/thesis-deviations.yaml` now moves all `DEV-TMT-*` records out of active `deviations` and into `history.resolved`.
- Each resolved `DEV-TMT-*` history entry includes:
  - `resolution_date: 2026-03-01`
  - replacing commit metadata from TMT3 Wave 1/Wave 2 integration commits where relevant
  - regression test evidence anchors (Phi alignment, IdentityBridge, replay-map and pipeline closeout slices)
- Campaign closeout status: TMT3 is documentation-complete and ready for final integration handoff.

### 2026-03-01 Transformation-mechanism thesis-exact classification campaign
- Historical Wave 0-2 campaign work introduced explicit `DEV-TMT-*` tracking to
  classify remaining non-aligned rows and bind them to code/test evidence.
- Wave 3 completed the closeout: those IDs are now retired to
  `docs/thesis-deviations.yaml` `history.resolved`, and the table itself is
  fully `Aligned`.

### 2026-03-01 Single-solved elaboration input migration
- Elaboration input wiring now uses a single solved snapshot handle in `ElabEnv` (`eeSolved`) and in result-type context (`rtcSolved`).
- Split solved field names were removed from elaboration and result-type wiring (`eeResPhi`, `eeResReify`, `eeResGen`, `rtcSolvedForGen`, `rtcSolvedClean`).
- Checked-authoritative output policy is preserved; `runPipelineElab`/`runPipelineElabChecked` parity remains locked by targeted regression tests.
- Generalization-context differences remain explicit through `GaBindParents`, scope overrides, redirects, and plan-builder-driven generalization.
- For behavior stability in this migration, the authoritative single solved snapshot threaded by pipeline elaboration/result-type wiring is `solvedForGen`.

### Solved Semantics
- `Solved` is now a projection-first layer over equivalence classes.
- `originalConstraint` is the primary accessor for pre-solve data.
- `canonicalConstraint` is used only when post-solve canonical data is explicitly needed.
- Canonical chasing (`canonical`) reconciles aliases but is not the primary source of semantic inputs.

### Phi Translation
- Phi/Omega/IdentityBridge resolve binder identity from witness domain (EdgeTrace/EdgeWitness) first.
- Runtime class-member fallback search is removed from Phi/Omega binder resolution.
- `sourceKeysForNode` is strict witness-domain ranking (raw + copy/trace provenance only; no canonical/class-member expansion fallback).
- Non-root replay resolution is replay-map/source-alias deterministic and fail-fast on contract misses.

### 2026-03-06 TMT identity row re-audit

- Thesis §§15.3.1-15.3.6 carries elaboration identity directly through named nodes, computation contexts, and witness-derived computations (`ε`, `ϕR`, `T(e)`); it does not introduce a separate identity-reconciliation object.
- The accepted runtime path is now witness-domain exact: `Translate` validates trace/replay key-space contracts, while `Omega` uses direct replay/source targets with fail-fast behavior and no local source-candidate recovery helpers.
- `MLF.Elab.Phi.IdentityBridge` remains as a witness-domain utility/diagnostic module and test surface only; it is no longer authoritative for runtime target repair.
- The source-domain interior-membership exception is intentionally narrow: direct forward `etCopyMap` alias evidence is still accepted as witness-authoritative provenance, but reverse-copy/canonical candidate expansion is not used for runtime repair.

### 2026-02-27 Phi strict replay-map normalization (upfront, no runtime fallback search)

- `EdgeTrace` now carries required `etBinderReplayMap` metadata (source binder key -> replay binder node).
- Presolution normalization/validation enforces replay-map completeness, TyVar codomain, and injectivity contracts.
- 2026-02-27 strict runtime contract: producer normalization must emit an active-source/replay-domain replay map; runtime bridge logic validates and passes through only (no projection/repair).
- Phi `computeTraceBinderReplayBridge` aligns replay candidates with scheme quantifier IDs first, validates domain/targets, and fails fast on mismatches.
- 2026-03-01 strict pass-through follow-up: `computeTraceBinderReplayBridge` no longer carries projection-helper fallback paths (`projectReplayTarget`/`projectOne`); runtime bridge checks are domain parity + codomain membership only, then pass-through.
- Omega consumes replay-map targets in replay raw-ID space (no eager canonical rewrite), then resolves binder indices deterministically.
- Source-space replay targets are hard errors (strict fail-fast), not repairable runtime cases.
- No-trace Phi entrypoint is strict fail-fast (`MissingEdgeTrace`) for production parity.

### Pipeline Boundary
- Presolution owns all graph transformations.
- Elaboration path does not mutate Solved or the constraint graph.
- Pipeline setup (before elaboration) may use `rebuildWithConstraint` for canonicalization and `pruneBindParentsSolved` for cleanup.
- Generalize creates local Solved variants via `rebuildWithConstraint` for alias reification; these do not propagate to the pipeline's Solved handle.

### 2026-02-27 Thesis exactness cleanup (A-E)

- Phase A:
  - Added seeded closure API `runUnifyClosureWithSeed` and switched presolution closure drains to seed from `psUnionFind`.
  - Removed per-drain UF rewrite from presolution closure loop.
  - Added hard presolution edge-boundary assertions that reject pending unify edges before and after each inst-edge closure cycle.
- Phase B:
  - `computePresolution` now enforces producer-boundary artifact invariants with explicit errors:
    - `ResidualUnifyEdges`, `ResidualInstEdges`, `ResidualTyExpNodes`,
    - `MissingEdgeWitnesses`, `MissingEdgeTraces`.
  - Witness/trace completeness is checked against non-trivial input instantiation edge IDs (let-edge trivials excluded).
- Phase C:
  - Removed canonical-domain query exports from `MLF.Constraint.Solved`:
    - `canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`.
  - Migrated reify/result-type call sites to projection-first access patterns via:
    - `Solved.lookupNode`, `Solved.lookupVarBound`, `Solved.canonical`,
    - `Solved.originalConstraint`/`Solved.canonicalConstraint` as explicit domain selectors.
- Phase D:
  - `IdentityBridge.sourceKeysForNode` is now strict (no implicit class fallback).
  - Removed class-fallback APIs from IdentityBridge and runtime Phi/Omega binder lookup.
  - Translate/Omega now consume replay-map/source-alias contracts directly and fail fast when targets are unresolved.
- Phase E:
  - Removed transitional runtime entrypoint `runPipelineElabProjectionFirst`.
  - Kept dual-path validation only in test harness (`DualPathSpec`) by comparing native solved artifacts against legacy snapshot reconstruction.

### 2026-02-26 Legacy replay removal + frozen parity baseline

- Removed internal legacy fallback elaboration entrypoints:
  - `runPipelineElabViaLegacySolve` is no longer defined/exported in `MLF.Elab.Run.Pipeline`,
    `MLF.Elab.Run`, and `MLF.Elab.Pipeline`.
- Removed legacy fallback test harness helpers from `test/SpecUtil.hs`.
- Replaced live native-vs-legacy parity tests with a frozen artifact oracle:
  - deterministic artifact builder/renderer in `test/Parity/FrozenArtifacts.hs`,
  - checked-in baseline `test/golden/legacy-replay-baseline-v1.json`,
  - authoritative parity spec `test/FrozenParitySpec.hs`,
  - generator executable `frozen-parity-gen`,
  - regen script `scripts/update-frozen-parity-artifacts.sh` with two-pass deterministic check.
- Scope note:
  - low-level snapshot APIs (`solveUnifyWithSnapshot`, `fromSolveOutput`) remain available for snapshot-centric unit tests, but no longer drive production parity behavior.

### 2026-02-26 Thesis-exact unification ordering + regression hardening

- Presolution now enforces thesis `SolveConstraint` order in Phase 4:
  - drain initial pending unification closure before inst-edge traversal,
  - process edges in topological order,
  - drain closure after each edge when unification work is pending.
- Presolution now carries UF metadata explicitly (`prUnionFind`) while exposing `prConstraint` as the typed presolved graph; the temporary raw graph remains an internal Phase 4 state detail.
- Shared unification closure logic is centralized in `MLF.Constraint.Unify.Closure` and reused by both Solve and Presolution.
- `Solved.fromPresolutionResult` now uses replay-equivalent snapshot finalization (shared semantics with `fromSolveOutput`).
- Production decision update (2026-02-26): default elaboration pipeline now uses presolution-native solved construction directly (`fromPresolutionResult`) without dual-run legacy replay in the production path.
- Regression hardening:
  - presolution closure drain is now a no-op when `cUnifyEdges` is empty (avoids forcing closure over transient intermediate binding-tree shapes),
  - strengthened parity coverage with explicit legacy-vs-native solved and elaboration anchors.
- Verification snapshot:
  - `cabal test mlf2-test --offline` => `838 examples, 0 failures`.

### 2026-02-26 Milestone 5 gap-closure (OpWeaken alias recovery + IdentityBridge class-members)

- Closed a remaining Ω replay gap where `OpWeaken` could degrade to `ε` when the witness target resolved to a non-binder alias in canonical space.
- `MLF.Elab.Phi.Omega` now attempts binder recovery from `Solved.classMembers` when `OpWeaken` lands on a non-binder replay target; if a recoverable binder index exists in the current `VSpine`, Φ emits `InstElim` at that binder instead of skipping.
- `MLF.Elab.Phi.IdentityBridge.sourceKeysForNode` now includes solved equivalence-class members (`Solved.classMembers`) in source-key expansion, so binder identity can be recovered without relying only on trace/copy-map reverse links.
- Added regressions:
  - `test/ElaborationSpec.hs`: `OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`.
  - `test/Phi/IdentityBridgeSpec.hs`: `includes solved class members for canonical alias recovery`.
- Validation: `cabal build all && cabal test` (`824 examples, 0 failures`).

### 2026-02-26 IdentityBridge binder-identity disambiguation follow-up

- Discovered a remaining replay ambiguity after class-member expansion: when multiple scheme binders shared one solved class, `lookupBinderIndex` could give both binders the same spine index via class-expanded exact-key ties.
- Root cause: class-member keys were participating in exact-match ranking, so direct binder targets lost raw identity distinction and fell back to lowest spine index.
- Fix in `MLF.Elab.Phi.IdentityBridge`:
  - split key matching into exact (no class fallback) vs class fallback vs canonical alias fallback;
  - `lookupBinderIndex` now ranks exact raw/copy/trace identity keys first, then class fallback only when no exact keys exist, then canonical alias fallback.
- Added regression:
  - `test/Phi/IdentityBridgeSpec.hs`: `preserves raw binder identity before class-member fallback`.
- Guarded existing behavior:
  - revalidated `test/ElaborationSpec.hs` alias-target weaken regression (`OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`) to ensure class fallback still recovers alias targets.
- Validation: `cabal build all && cabal test` (`825 examples, 0 failures`).

### 2026-02-26 solvedConstraint migration phase 6 (`ebCanonicalConstraint` removal)

- Removed `ebCanonicalConstraint :: Constraint` from `MLF.Constraint.Solved.EquivBackend`.
- Replaced canonical-storage strategy with explicit canonical graph slices in backend state:
  - `ebCanonicalNodes`, `ebCanonicalInstEdges`, `ebCanonicalUnifyEdges`
  - `ebCanonicalBindParents`, `ebCanonicalPolySyms`, `ebCanonicalEliminatedVars`, `ebCanonicalWeakenedVars`
  - `ebCanonicalAnnEdges`, `ebCanonicalLetEdges`, `ebCanonicalGenNodes`
- Added internal helpers:
  - `canonicalConstraintFromBackend` (reconstruct full canonical `Constraint`)
  - `setCanonicalConstraint` (replace canonical slices from a `Constraint`)
- Updated constructors/rebuilders/mutation helpers to operate on canonical slices while keeping public API behavior unchanged (`canonicalConstraint`, `rebuildWithConstraint`, `patchNode`, `rebuildWithNodes`, etc.).
- Verification after refactor: `cabal build all && cabal test` (`822 examples, 0 failures`).

### 2026-02-26 solvedConstraint migration batch 2 (Reify/Fallback canonical-domain API)

- Removed direct `Solved.solvedConstraint` usage from:
  - `MLF.Reify.Core` (`reifyWith` bind-parent access)
  - `MLF.Elab.Run.ResultType.Fallback` (canonical bound lookup + scope-root post-check path)
- Added `Solved.canonicalizedBindParents :: Solved -> Either BindingError BindParents` so reification can keep canonical bind-parent normalization semantics without extracting raw canonical constraints.
- Added `bindingScopeRefCanonical :: Solved -> NodeId -> Either BindingError NodeRef` in `MLF.Elab.Run.Scope` for canonical-domain scope-root lookup over `Solved.canonicalBindParents`.
- `bindingScopeRefCanonical` now delegates through the primary `bindingScopeRef` owner path on `chiCanonicalConstraint`; `MLF.Elab.Run.Scope` no longer keeps a separate handwritten canonical bind-parent traversal, while `letScopeOverrides` still owns base-vs-solved scope divergence behavior.
- Fallback bound-resolution no longer depends on raw canonical `Constraint`; it now traverses canonical nodes/var-bounds via `Solved.lookupCanonicalNode` and `Solved.lookupCanonicalVarBound`.
- Verification after migration: `cabal build all && cabal test` (`822 examples, 0 failures`).

### 2026-02-24 Eliminate DEV-PHI-STANDALONE-GRAFT-EXTENSION (thesis-exact standalone graft)

- Retired DEV-PHI-STANDALONE-GRAFT-EXTENSION: deeper thesis analysis (Def. 15.3.4) reveals the standalone `OpGraft` handler (`atBinderKeep` + `InstInside(InstBot σ)`) IS thesis-exact — the deviation description was backwards. The paired `OpGraft+OpWeaken` handler producing `InstApp σ` is a sound optimization (equivalent by normalizeInst Rule 1).
- Paired handler retained: deleting it breaks Omega's incremental type-state when multiple graft+weaken pairs interact (binder elimination via `atBinder` changes type-state for subsequent ops; `atBinderKeep` preserves it).
- Added normalizeInst Rule 1b: collapses context-wrapped `InstSeq (InstUnder v (InstInside (InstBot t))) (InstUnder v InstElim)` → `InstUnder v (InstApp t)` for single-level non-front binders.
- Added Rule 1b test in ElaborationSpec.
- Deviation register 6 → 5 entries.
- Verification: 786 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-24 DEV-PHI-STANDALONE-GRAFT-EXTENSION investigation (deviation retained)

- Investigated eliminating DEV-PHI-STANDALONE-GRAFT-EXTENSION by reversing coalescing direction in `coalesceDelayedGraftWeakenWithEnv` (move graft forward instead of weaken backward).
- Finding: moving descendant ops before the graft changes Omega's type-state evolution — the graft applies `InstInside(InstBot argTy)` which descendant ops depend on. Reordering produces incorrect instantiations (proven by `TCArgumentMismatch` failures on `\y. let id = (\x. x) in id y` and other baselines).
- Conclusion: the standalone graft handler with `atBinderKeep` is semantically load-bearing. The deviation cannot be eliminated without redesigning Omega's incremental type-state translation.
- Added `StandaloneGraftRemaining` error constructor to `WitnessValidation.hs` and `assertNoStandaloneGrafts` validation function to `WitnessCanon.hs` (exported for targeted testing).
- Added characterization tests: "leaves graft standalone when middle ops touch protected set" and "rejects standalone graft with no matching weaken" in `WitnessSpec.hs`.
- Updated deviation description in `thesis-deviations.yaml` with root-cause analysis and additional test evidence matchers.
- Verification: 785 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-24 Eliminate DEV-PHI-WITNESS-WEAKEN-SUPPRESSION (thesis-exact witness emission)

- Witness emission now always emits `OpWeaken` for unbounded binders (thesis-exact Def. 15.3.4): removed `suppressWeaken` and `argIsGenBound` guards from `classify` in `Witness.hs`, simplified `witnessAlg` stepper signature (removed `Bool` parameter and suffix flag computation), deleted `argIsGenBound` helper.
- Removed annotation-edge blanket weaken stripping: deleted `dropWeakenOps` from `EdgeProcessing/Witness.hs`, removed `suppressWeaken` parameter from `edgeWitnessPlan`, removed `eprSuppressWeaken` field from `EdgePlanResolved` in `Plan.hs`, updated `Planner.hs` and `Interpreter.hs` call sites.
- Extended Omega translation to handle previously-suppressed weakens: added graceful skip in standalone `OpWeaken` case when binder is no longer in the identity list (already eliminated by prior operation).
- Retired `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION` from deviation register (7 → 6 entries).
- Verification: 782 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-23 Phi thesis-purity follow-up (deviation sync + Omega alignment)

- Registered three load-bearing Chapter 15.3 implementation choices in `docs/thesis-deviations.yaml` and linked them from `CLM-PHI-CORRECTNESS` in `docs/thesis-claims.yaml`:
  - `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION`
  - `DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION`
  - `DEV-PHI-STANDALONE-GRAFT-EXTENSION`
- Removed the `OpGraft; OpRaise; OpWeaken` `mergeIntoApp` peephole from the operation replay loop in `MLF.Elab.Phi.Omega` so operations are replayed de-fused.
- Moved binder-aware bottom rescue (`TBottom -> TVar binder`) into `reifyTypeArg`; call sites now pass binder context directly instead of post-reify rescue helpers.
- Updated bounded `OpGraft+OpWeaken` bound-match behavior:
  - Φ now emits literal thesis-shaped `InstApp boundTy` when graft arg matches explicit bound.
  - Internal replay-state evolution still uses elimination for type-state compatibility; mismatch path remains fail-fast.
- Added/adjusted Omega normalization for de-fused left-associated `OpGraft;OpRaise;OpWeaken` shapes so historical elaboration baselines (notably `\y. let id = (\x. x) in id y`) do not bottom-collapse.

### 2026-02-22 Defensible exactness (traceable evidence chains)

- Added machine-checked thesis claims registry (`docs/thesis-claims.yaml`, 21 claims across Ch. 7-15) and deviation register (`docs/thesis-deviations.yaml`, 5 deviations) with cross-link validation via `scripts/check-thesis-claims.sh`.
- Added `supports_claims` back-links to obligations ledger (`docs/thesis-obligations.yaml`) so every obligation references the claims it supports.
- Added three new test modules for thesis property coverage:
  - `test/TranslatablePresolutionSpec.hs` — Def. 15.2.10 translatable presolution (3 examples).
  - `test/PhiSoundnessSpec.hs` — Def. 15.3.4 Phi soundness (3 examples).
  - `test/ExpansionMinimalitySpec.hs` — Def. 10.1.1 expansion minimality (4 examples).
- Upgraded conformance gate (`scripts/thesis-conformance-gate.sh`) with claims checker and three new anchor matchers.
- Migrated `docs/paper-map.md` Known Deviations and Audit Checklist sections to reference machine-checked artifacts.
- Closed spec drift: all open `.kiro` spec tasks annotated with deferred notes and deviation cross-references.
- Upgraded claim statuses: CLM-TRANSLATABLE-PRESOLUTION, CLM-PHI-CORRECTNESS, CLM-EXPANSION-MINIMALITY from `partial` to `defended`.
- Verification: 781 examples, 0 failures; conformance gate green; claims checker green.

### 2026-02-20 Theorem-proxy scope upgrade (structurally rich generators + new proxies)

- Upgraded `test/TypeSoundnessSpec.hs` generator from flat `elements` pool to sized typed-by-construction:
  - `genTermAtType` builds terms top-down with a typing context (`TyCtx`), using `sized`/`frequency` to produce nested `ELam`, `EApp`, `ELet` at depth.
  - `genAtom` generates type-correct leaves (variables from context, literals at matching ground type, lambda fallback for arrow types).
  - Top-level `genClosedWellTypedElabTerm` occasionally wraps in vacuous `ETyAbs` for type-abstraction coverage.
- Added two new theorem proxies:
  - Multi-step preservation: `typeCheck t = Right tau => typeCheck (normalize t) = Right tau`.
  - Determinism: `step t == step t` (referential transparency guard for small-step).
- Existing proxies (1-step preservation, progress) retained and upgraded to 300 max-success with 30% coverage thresholds.
- Executable theorem-proxy scope:
  - **Progress**: well-typed closed term is value or steps (property-based, 300 samples).
  - **1-step preservation**: type preserved across single `step` (property-based, 300 samples).
  - **n-step preservation**: type preserved across full `normalize` (property-based, 300 samples).
  - **Determinism**: `step` is a pure function (property-based, 300 samples).
  - **Not mechanized**: full inductive proofs of progress/preservation/determinism remain non-mechanized; these are executable proxies only.

### 2026-02-19 Phase 7 theorem obligations executable proxies

- Added `/Volumes/src/mlf4/test/TypeSoundnessSpec.hs` with two property-style checks:
  - preservation proxy: if `typeCheck t = Right tau` and `step t = Just t'`, then `typeCheck t' = Right tau`.
  - progress proxy for closed terms: if `typeCheck t = Right tau` and term is closed, then `isValue t || isJust (step t)`.
- Scope is intentionally Phase 7 local (ElabTerm generator only), so failures isolate `MLF.Elab.TypeCheck`/`MLF.Elab.Reduce` behavior rather than upstream pipeline stages.
- Wired into test harness:
  - `/Volumes/src/mlf4/mlf2.cabal`
  - `/Volumes/src/mlf4/test/Main.hs`
- Added mandatory gate anchor:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh` now runs matcher `Phase 7 theorem obligations` (min `2` examples).
- This closes part of the remaining non-semantic formalization debt by making theorem proxies executable, while not claiming a mechanized proof.

### 2026-03-12 Phase 7 recursive runtime forms (M2 internal-only)

- Added internal-only elaborated runtime terms `ERoll ElabType ElabTerm` and `EUnroll ElabTerm` for iso-recursive runtime semantics.
- Typechecking now enforces the M2 runtime rules inside `MLF.Elab.TypeCheck`:
  - `ERoll (μa. τ) e : μa. τ` only when `e : τ[μa.τ / a]`.
  - `EUnroll e : τ[μa.τ / a]` only when `e : μa. τ`.
- Reduction now enforces call-by-value recursive runtime behavior inside `MLF.Elab.Reduce`:
  - `ERoll` is a value exactly when its payload is a value.
  - Reduction steps under `ERoll` / `EUnroll` until the payload is ready.
  - `EUnroll (ERoll (μa. τ) v) → v`.
- `MLF.Elab.Types` intentionally keeps these forms internal to M2: it does not lower or render `ERoll` / `EUnroll` as public/XMLF or pseudo-surface `roll[...]` / `unroll` syntax, and XMLF conversion rejects them if reached.
- Added focused Phase 7 recursive-runtime theorem-obligation proxies in `test/TypeSoundnessSpec.hs`:
  - positive correspondence: roll obligation ⇒ `ERoll` evidence, unroll obligation ⇒ `EUnroll` evidence, recursive-context obligation ⇒ recursive runtime-context evidence;
  - negative correspondence: the property fails when any of those obligation tags is present without the matching term/context evidence.

### 2026-03-12 Phase 7 contractiveness validation (M4 explicit-layer only)

- Added a shared internal recursive-type contractiveness check in `MLF.Reify.TypeOps` and enforced it centrally from `MLF.Elab.TypeCheck`.
- Kept the dedicated contractiveness `TypeCheckError` case internal to the elaboration surface; the public `MLF.Pipeline` facade continues to expose `TypeCheckError` abstractly rather than widening the downstream constructor set.
- Phase 7 now validates recursive types at every term-embedded type boundary that currently accepts elaborated types:
  - lambda annotations,
  - let schemes,
  - type-abstraction bounds,
  - instantiation arguments,
  - recursive runtime `ERoll` annotations.
- The v1 policy is intentionally conservative and thesis-aligned:
  - `TArrow` and `TCon` count as guards for recursive occurrences,
  - bare self-reference is rejected,
  - `forall` does not count as a guard,
  - shadowing binders still suppress the outer recursive variable.
- This keeps M4 as an explicit-layer containment step only. It does not add equi-recursive equality, cyclic graph support, parser/frontend recursive syntax, or inference-time recursive reasoning.
- Added focused regressions in `test/TypeCheckSpec.hs` for guarded acceptance plus non-contractive rejection across annotations, let schemes, bounds, instantiation arguments, and `ERoll`, and added an `ElaborationSpec` regression that locks the conservative `forall` policy.

### 2026-02-19 Formal obligations ledger (thesis Ch. 4–15) hard-fail enforcement

- Added canonical obligations ledger source:
  - `/Volumes/src/mlf4/docs/thesis-obligations.yaml`
- Added generated Markdown view:
  - `/Volumes/src/mlf4/docs/thesis-obligations.md`
- Added ledger tooling:
  - `/Volumes/src/mlf4/scripts/render-thesis-obligations-ledger.rb`
  - `/Volumes/src/mlf4/scripts/check-thesis-obligations-ledger.sh`
- Added mandatory gate stage:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh` now calls `check-thesis-obligations-ledger.sh` before legacy anchor slices.
- Scope/contract:
  - Covers Chapters 4–15 operational obligations (sections `4.2`–`4.4`, `5.2`–`5.3`, `7.3`, `8.2`, `9.4`, `10.1`–`10.4`, `11.2`–`11.6`, `12.1`–`12.4`, `14.2`–`14.3`, `15.2`–`15.3`).
  - Exact obligation inventory is fixed at `99` IDs (scope: Ch. 4–15 operational rules).
  - Checker hard-fails on count/id drift, missing/duplicate/unmapped obligations, non-anchored status, missing code/test anchors, markdown drift, zero-example matchers, or failing matched examples.
- Verification snapshot:
  - `./scripts/check-thesis-obligations-ledger.sh` (PASS)
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-19 Historical status cleanup

- This document is chronological; several 2026-02-16 entries capture intermediate debugging checkpoints.
- Any "red/open/in progress" status in those entries is historical and superseded by later closure entries and by `/Volumes/src/mlf4/Bugs.md` (Open: none).
- Current thesis-faithfulness status is:
  - Chapter 14/15 operational obligations are hard-enforced via the obligations ledger + conformance gate.
  - Semantic paper-faithfulness deltas tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` are closed.
  - Remaining debt is non-semantic (proof/formalization and assurance breadth).

### 2026-02-18 Thesis conformance gate command/profile

- Added canonical gate entrypoint:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`
- Gate behavior:
  - Runs thesis-anchor focused test slices with deterministic matcher strings.
  - Enforces minimum matched-example thresholds per slice so stale matcher strings cannot silently pass with `0 examples`.
  - Current thresholds:
    - `R-` matrix rows: min `15` examples
    - `A6 parity`: min `3`
    - `BUG-2026-02-17-002`: min `1`
    - `Phase 3 atomic wrapping equivalence gates`: min `7`
    - `has type forall a. a -> a`: min `1`
- CI enforcement:
  - Added `/Volumes/src/mlf4/.github/workflows/thesis-conformance.yml`.
  - CI job builds all targets (`cabal build all`) and then runs `./scripts/thesis-conformance-gate.sh`.
- Verification snapshot:
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-18 A5 (P3) totality/harness hardening closure

- Frontend coercion-copy failure typing:
  - Added `UnexpectedBareCoercionConst` to `MLF.Frontend.ConstraintGen.Types.ConstraintError`.
  - `MLF.Frontend.ConstraintGen.Translate.buildExprRaw` now rejects bare `ECoerceConst` with the typed constructor instead of stringly `InternalConstraintError`.
- STCon coercion-copy totalization:
  - Refactored constructor-argument internalization into `internalizeConArgs` (`NonEmpty` recursion) and removed in-branch `NE.head`/`NE.tail` + ad hoc accumulator plumbing from `STCon` handling.
  - Preserved existing sharing/rebinding semantics (`SharedEnv` threading and rigid child rebind behavior remain unchanged).
- Harness wiring hardening:
  - `test/Main.hs` now wires presolution via `PresolutionSpec.spec` only (single-source umbrella).
  - Added fail-fast wiring guard: `IORef` marker is set at presolution wiring and checked immediately after; test binary aborts if presolution umbrella wiring is removed.
- Regression coverage added/updated:
  - `test/ConstraintGenSpec.hs`:
    - `bare ECoerceConst rejects with typed UnexpectedBareCoercionConst (not InternalConstraintError string)`
    - `STCon coercion-copy failures surface as typed errors`
    - `nested STCon coercion-copy preserves binding-tree validity`
  - `test/PresolutionSpec.hs` + `test/Main.hs`: umbrella wiring consolidation and guard path.
- Verification snapshot:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "bare ECoerceConst rejects"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "STCon coercion-copy failures surface as typed errors"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 — Principal Presolution"'` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-17 BUG-2026-02-17-002 applied bounded/coercion A6 closure

- Root-cause chain (systematic-debugging):
  - `MLF.Elab.Elaborate` `ALetF` fallback shape checks only recognized raw `ALam`/`AApp`; annotated lambdas (`AAnn (ALam ...)`) skipped the lambda fallback path and retained mismatch-prone let scheme shaping.
  - `MLF.Elab.Elaborate` `AAppF` recovery upgraded `InstApp` only when the argument source was a named variable; literal arguments fell back to `InstElim`, bottomizing applications through unbounded binders.
- Implemented behavior:
  - `ALetF` now unwraps `AAnn` when classifying RHS shape (`rhsIsLam`/`rhsIsApp`).
  - Lambda fallback candidates now use `IntMap.empty` substitution and avoid extra RHS closure wrapping when a fallback scheme is selected.
  - `AAppF` `funInstRecovered` now permits non-variable arguments to drive `InstApp` recovery using checked argument type (still constrained by existing binder-shape guards).
- Result:
  - The applied A6 bounded/coercion variant now elaborates to `Int` in both unchecked and checked pipelines.
  - Regression sentinel was upgraded to strict success assertion in `test/PipelineSpec.hs`.
- Verification snapshot:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002"'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity"'` (PASS)
  - `cabal build all && cabal test` (PASS)

### 2026-02-17 A1 strict Ω normalization closure audit

- Audited A1 acceptance criteria against production witness normalization:
  - `MLF.Constraint.Presolution.WitnessCanon.normalizeInstanceOpsFull` rejects malformed merge direction as `MergeDirectionInvalid`.
  - `MLF.Constraint.Presolution.WitnessNorm.normalizeEdgeWitnessesM` surfaces normalization failures as `WitnessNormalizationError` without permissive fallback acceptance.
- Verification evidence:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match R-MERGE-NORM-09'` (PASS)
  - `cabal test mlf2-test --test-show-details=direct --test-options=\"--match=\\\"fails fast with MergeDirectionInvalid via presolution normalization\\\"\"` (PASS)
  - `cabal build all && cabal test` (PASS)
- Synced tracker closure in `TODO.md` (`A1 (P1)` entries now closed with dated AC status).

### 2026-02-17 A4 paper-faithfulness doc/spec sync

- Synced `.kiro/specs/paper-faithfulness-remaining-deltas/` to current state:
  - `requirements.md`: all semantic requirements in this spec are now marked present with evidence.
  - `design.md`: removed stale wording that described still-open Φ/witness semantic deltas; added explicit non-semantic remaining deltas only.
  - `tasks.md`: added closure note marking semantic plan complete and redirecting remaining backlog to `TODO.md`.
- Synced `TODO.md` A4 entries to done with dated closure note.
- Current residual non-thesis-exact scope is non-semantic:
  - proof/formalization debt,
  - full formal Phase-7 linkage to thesis proof obligations,
  - broader regression/docs/API cleanup backlog items tracked outside semantic bug tracking.

### 2026-02-17 BUG-2026-02-17-001 Φ keep-key + Graft/Raise/Weaken stabilization

- Root-cause cluster:
  - `MLF.Elab.Phi.Translate.computeTargetBinderKeys` retained replay keys when the target binder set was empty, suppressing `OpWeaken` elimination in edge traces that should discharge binders.
  - Ω translation emitted over-complex instantiations for unbounded same-binder triples `OpGraft -> OpRaise -> OpWeaken`, which drifted paper baselines (`id y`, annotation instantiation shapes).
  - Annotation handling needed a localized `InstId` fallback in `AAnnF` for non-variable annotation sources with explicit expected bounds.
- Implemented behavior:
  - `MLF.Elab.Phi.Translate`:
    - keep-keys are now strict intersection with target binders (no empty-target “keep everything” fallback).
  - `MLF.Elab.Phi.Omega`:
    - preserve spine Raise alias/eliminate behavior for empty intermediate contexts,
    - collapse unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples to direct `InstApp`,
    - retain conservative bound normalization for explicit bounds while avoiding destructive collapse for inferred unbounded variable cases.
  - `MLF.Elab.Elaborate` (`AAnnF`):
    - maintain strict generic `reifyInst` fallback policy (`Nothing -> False`),
    - add local non-variable annotation fallback from `InstId` to `InstInside (InstBot expectedBound)`.
- Verification snapshot:
  - PASS: `id y should have type`, `elaborates polymorphic instantiation`, `elaborates term annotations`, `term annotation can instantiate a polymorphic result`, `explicit forall annotation preserves foralls in bounds`.
  - PASS: `BUG-002-V` (seed `1593170056`), `BUG-003-V` (seed `1925916871`), `BUG-004` (seed `1593170056`), OpRaise source-domain interior guard.
  - `cabal build all` passes.
  - Intermediate checkpoint (superseded by closure section below): full `cabal test` then had 3 remaining failures in unrelated buckets (pipeline ann-redirect invariant + two Φ contract tests).

### 2026-02-17 BUG-2026-02-17-001 closure pass (remaining 3 buckets)

- Residual failures closed:
  - `MLF.Elab.Phi.Omega.resolveTraceBinderTarget` now enforces binder-domain fail-fast for trace-source operands when replay binder candidates are absent (`PhiInvariantError "trace/replay binder key-space mismatch"`), matching the strict Φ/Ω contract tests.
  - Non-spine `OpRaise` no longer rejects non-`⊥` bounds when a valid `C^m_n` context is available; Ω now executes the context-path intro/bot/alias translation directly in that case.
  - `PipelineSpec` canonicalization sentinel now asserts non-empty canonicalized scheme roots only when solve produced non-empty `union-find`; stale-node/root canonicalization checks remain strict.
- Verification snapshot:
  - PASS: `/Phase 6 — Elaborate (xMLF)/.../fails fast when OpWeaken targets a trace binder source with no replay binder mapping/`
  - PASS: `/Phase 6 — Elaborate (xMLF)/.../Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)/`
  - PASS: `/Pipeline (Phases 1-5)/applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently/`
  - PASS: `cabal build all && cabal test` (`678 examples, 0 failures`).

### 2026-02-16 BUG-2026-02-11-004/010 hybrid bridge follow-up (historical checkpoint; superseded)

- Extended edge trace metadata (`MLF.Constraint.Presolution.Base.EdgeTrace`) with:
  - `etBinderReplayHints :: IntMap NodeId`
  - Contract: source binder key -> replay-domain binder candidate (live TyVar only).
- Presolution now derives and persists replay hints during witness normalization:
  - `MLF.Constraint.Presolution.WitnessNorm` computes deterministic source/rewrite hint maps from canonicalized binder args + solved-node liveness.
  - normalized traces in `psEdgeTraces` now carry `etBinderReplayHints`.
- Witness validation now carries replay hints in normalization env (`binderReplayHints`) and rejects hinted operands that are not live TyVars (`HintedOperandNotLiveTyVar`), preventing silent replay through dead source keys when hints exist.
- Φ bridge construction now consumes hints + positional replay seeding:
  - `MLF.Elab.Phi.Translate.computeTraceBinderReplayBridge` now:
    - prefers hint candidates where valid,
    - adds positional source→replay seed from trace-order source binders and replay-subst keys,
    - keeps name-based/alias-based deterministic fallback.
  - Ω diagnostics include hint-domain payload in binder target mismatch errors.
- Verification snapshot (at that checkpoint):
  - PASS:
    - `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`
    - bounded-alias baseline (`b ⩾ a`) anchors
    - strict matrix guard: `make-app keeps codomain Int without bottom-domain collapse`
  - At that checkpoint (before 2026-02-17 closure), open:
    - `BUG-003-V1/V2` remain deterministic `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - At that checkpoint, full gate was red (`674 examples, 33 failures`) and BUG-2026-02-11-004 / BUG-2026-02-16-010 were open.

### 2026-02-16 BUG-2026-02-11-004 Φ/Ω source→replay binder bridge (historical checkpoint; superseded)

- Implemented the planned bridge at the Φ→Ω boundary:
  - `MLF.Elab.Phi.Translate` now computes, once per edge (after `siForOmega` finalization),:
    - `traceBinderSources :: IntSet` (deduped `etBinderArgs` binder keys, trace order)
    - `traceBinderReplayMap :: IntMap NodeId` (source binder key -> replay binder key)
  - mapping remains deterministic and name-driven:
    - scheme names come from `siScheme`,
    - replay keys come from `siSubst` with per-name key selection ranked by `traceOrderRank` (`IdentityBridge`),
    - pairing is `zip` of scheme binder names with trace binder sources.
- `OmegaContext` now carries the bridge contract explicitly:
  - `ocTraceBinderSources`
  - `ocTraceBinderReplayMap`
- `MLF.Elab.Phi.Omega` now resolves binder-target operands before execution for:
  - `OpGraft _ bv`
  - `OpWeaken bv`
  - `OpRaise n` (execution target only)
  - `OpMerge n m`
  - `OpRaiseMerge n m`
- Thesis-preserving split retained:
  - `OpRaise` translatability/interior checks continue to use raw source key (`nSource`) for `I(r)` checks.
  - replay execution paths use the resolved replay key.
- New fail-fast invariant:
  - If an Ω binder-target key is a trace binder source but has no replay-key mapping, Φ now returns `PhiInvariantError` with edge/op/raw-key/source-set/replay-domain/scheme-keys diagnostics.
  - This replaces prior silent drift into non-binder/bottomized behavior.
- Focused validation status (at that checkpoint):
  - PASS: new fail-fast regression (`OpWeaken` unmapped trace binder target).
  - PASS: source-domain interior alias regression (`OpRaise accepts source-domain interior membership ...`).
  - PASS: bounded alias baseline (`b ⩾ a`) non-regression anchors.
  - At that checkpoint, `BUG-003-V1/V2` remained in `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - At that checkpoint, full gate remained red in broader buckets (`cabal build all && cabal test`: `674 examples, 47 failures`).

### 2026-02-16 BUG-2026-02-16-007/008 `SchemeFreeVars` sentinel-drift closure

- Root cause:
  - BUG-003-V1/V2 hit plain `SchemeFreeVars (NodeId 27) ["__rigid24"]` in pipeline/result-type generalization paths.
  - Those paths retried only `BindingTreeError GenSchemeFreeVars`, so plain `SchemeFreeVars` escaped as a top-level `PipelineElabError` and masked the underlying strict-instantiation failure bucket.
- Fix:
  - `MLF.Elab.Run.Pipeline`: root generalization fallback now treats `SchemeFreeVars` and `BindingTreeError GenSchemeFreeVars` uniformly (`GA -> non-GA -> reifyType`).
  - `MLF.Elab.Run.ResultType.Util`: `generalizeWithPlan` now mirrors the same fallback policy.
  - `test/ElaborationSpec.hs`: BUG-003-V1/V2 sentinels now assert the stabilized strict-instantiation class (`InstBot expects TBottom`) instead of the transient `SchemeFreeVars` class.
- Result:
  - BUG-003 sentinel variants no longer fail with `SchemeFreeVars/__rigid24`.
  - Both variants are back in the shared bounded-alias failure bucket tracked by BUG-2026-02-11-004.
- Verification snapshot (2026-02-16):
  - PASS:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V1: triple bounded chain sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Systematic bug variants (2026-02-11 matrix)/BUG-003-V2: dual-alias sentinel reproduces known Phi invariant failure/" --seed 1481579064'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`

### 2026-02-16 BUG-2026-02-16-009 non-spine `OpRaise` context fallback

- Root cause:
  - In `MLF.Elab.Phi.Omega`, non-spine `OpRaise` adopted source targets through `etCopyMap` before context reconstruction.
  - For explicit-forall let-bound annotation baseline, source target had a valid `C^r_n` path while adopted target had none, causing `PhiTranslatabilityError "OpRaise (non-spine): missing computation context"`.
- Fix:
  - `OpRaise` now computes both adopted and source-domain raise/context targets.
  - Non-spine translation keeps adopted-target handling as primary.
  - If adopted-target non-spine context/root insertion cannot be constructed, Ω retries root-context insertion using the source-domain target.
- Result:
  - Restores explicit-forall round-trip baseline without regressing BUG-004/BUG-002 targeted anchors.
  - Preserves strict context behavior (`contextToNodeBound` still does not descend via forall-body fallback).
- Verification snapshot (2026-02-16):
  - PASS:
    - explicit-forall round-trip baseline
    - `BUG-004`
    - `BUG-002-V4`
    - strict target matrix
    - `contextToNodeBound does not descend through forall body fallback`
  - At that checkpoint, full gate remained red in separate buckets (`cabal build all && cabal test`: `672 examples, 4 failures`).

### 2026-02-16 BUG-2026-02-14-003 source-domain `I(r)` contract (surgical Omega/Translate)

- `MLF.Elab.Phi.Omega` now enforces `OpRaise` admissibility against trace-domain `I(r)` directly:
  - `etInterior` is consumed as-is (no canonical/copy-map remap in membership checks).
  - If an `OpRaise` source target is absent from `etInterior` but present only via a copy-map alias, Φ now raises a contract-level `PhiInvariantError` (identity-domain mismatch) instead of silently treating alias-domain membership as valid.
- `OpRaise` semantic execution now adopts the copied target when `etCopyMap` provides a source→copied mapping, while keeping the admissibility check in source-ID space. This preserves the source-domain contract and avoids over-specialization regressions on BUG-004 call-site annotation paths.
- `MLF.Elab.Phi.Translate` keeps trace semantics unchanged, but canonicalizes `etInterior` keys only for `namedSet` intersection because `namedSet0` is canonical-node keyed.
- New regressions:
  - `test/ElaborationSpec.hs`: `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`.
  - `test/PipelineSpec.hs`: `BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization`.
- Verification snapshot (2026-02-16):
  - Targeted anchors pass: `BUG-002-V4`, `BUG-2026-02-06-002 strict target matrix`, `BUG-004`, copy-map anchor, canonicalizer contract, and both new regressions.
  - At that checkpoint, full gate still reported unrelated buckets (`cabal build all && cabal test`: `672 examples, 9 failures`).

### 2026-02-16 BUG-2026-02-16-003 (`id id`) instantiation over-specialization fix

- Root cause (Phase 1 evidence):
  - In `MLF.Elab.Elaborate` (`AAppF`), `argInstFromFun` inferred an instantiation argument from function parameter type correctly, but then applied `inlineBoundVarsType` to the inferred argument list.
  - On `let id = \\x. x in id id`, this rewrote the inferred meta-var argument (`t18`) into a concrete arrow bound (`t14 -> t14`), over-specializing the argument-side `id` and triggering `TCArgumentMismatch`.
- Fix:
  - Keep inferred arguments unchanged in `argInstFromFun` (`instSeqApps args`), removing bound-variable inlining at this point.
  - No change to `inferInstAppArgs`, witness translation, or strict checker rules.
- Impact:
  - Restores let-polymorphic dual-instantiation behavior for the `id id` class while preserving previously fixed strict-target BUG-002 anchors.
- Verification snapshot (2026-02-16):
  - PASS:
    - `/Pipeline (Phases 1-5)/redirected let-use sites keep polymorphic schemes/`
    - `/Pipeline (Phases 1-5)/Checked-authoritative invariant/runPipelineElab type matches typeCheck(term) and checked pipeline type/`
    - `/Phase 6 — Elaborate (xMLF)/Polymorphism and Generalization/elaborates dual instantiation in application/`
    - `id id should have type`
    - `BUG-002-V2`, `BUG-002-V4`, strict target matrix, and `BUG-002-V4` OpRaise interior canonicalization gate
  - At that checkpoint, full gate remained red in separate buckets (`cabal build all && cabal test`: `672 examples, 5 failures`).

### 2026-02-17 BUG-2026-02-16-001/002 planner scheme-owner fallback (targeted closure)

- Context:
  - `EdgePlan` carries `eprSchemeOwnerGen`, resolved in planner.
  - planner classification tests for let/ann flags used synthesized wrappers (`ExpVarId < 0`) with sparse bind-parent maps.
- Root cause:
  - `MLF.Constraint.Presolution.EdgeProcessing.Planner.planEdge` resolved scheme owner strictly from TyExp body root.
  - For synthesized-wrapper topology, wrapper root can be in gen scope while body root path has no direct `GenRef`; strict body lookup threw `InternalError "scheme introducer not found ..."`.
- Implemented fix:
  - Added `resolveSchemeOwnerGen` in planner:
    - non-synth TyExp path remains strict body-root lookup (`findSchemeIntroducerM`),
    - synth-wrapper path does body-first lookup with wrapper-root fallback (`firstGenOnPath` + `bindingPathToRootUnderM`).
  - Strengthened `test/Presolution/EdgePlannerSpec.hs` repros to assert both flag threading and concrete scheme-owner resolution (`GenNodeId 0`).
- Verification snapshot (2026-02-17):
  - PASS `threads let-edge flag into allowTrivial` (seed `1481579064`)
  - PASS `threads ann-edge flag into suppressWeaken` (seed `1481579064`)
  - PASS `Edge plan types` matcher (`7 examples, 0 failures`)
  - PASS `Edge interpreter` matcher (`4 examples, 0 failures`)

### 2026-02-12 BUG-004-V2/V4 strict InstBot production fix (thesis-exact)

- Strict `InstBot` checker semantics are unchanged: `instBot` in `TypeCheck.hs` still requires the input type to be `TBottom`. This matches the paper's `⊥ ← τ` rule exactly.
- Only instantiation *production* was corrected in three places:
  1. `Omega.hs`: bare `InstBot argTy` was produced when `ty == TBottom || alphaEqType ty argTy`; tightened to `alphaEqType ty TBottom` so bare `InstBot` is only emitted when the input is actually `⊥`.
  2. `Elaborate.hs` ALamF: `generalizeAtNode` wraps monomorphic annotations in trivially bounded foralls (`∀(a:B).a`); these are now collapsed to the bound type `B` before use as lambda parameter types.
  3. `Elaborate.hs` AAppF: when an annotation has already updated a forall's bound from `⊥` to `τ`, the inferred argument instantiation is normalized to `InstElim` (which substitutes the bound without calling `instBot`) instead of `InstApp` (which would call `instBot` on the now-non-⊥ bound).
- The `InstInside(InstBot(t))` pattern (used by `instInsideFromArgsWithBounds` for unbounded binders) remains correct: `InstInside` enters the forall, then `InstBot` operates on the bound which IS `⊥`.
- Verification: `652 examples, 0 failures` including 3 new strict InstBot regression tests.

### 2026-02-11 EdgePlan cleanup (remove `EdgeStage`)

- `MLF.Constraint.Presolution.EdgeProcessing.Plan` now exposes a concrete resolved `EdgePlan` record.
  - Removed the single-constructor stage index (`EdgeStage`) and the `edgePlanStage` helper.
- `planEdge` and interpreter entrypoints now use `EdgePlan` directly (no phantom stage parameter).
- `EdgePlannerSpec` now checks concrete plan fields instead of a stage-tag assertion.
- Rationale: the stage index had no real transition boundary in production code (only `StageResolved`), so removing it tightens abstraction without semantic impact.
- Verification:
  - `cabal build mlf2-test` => pass.
  - `cabal build all && cabal test` => 631 examples, 0 failures.

### 2026-02-11 Phase 6 unified execution (wrapper-bridge removal)

- `MLF.Constraint.Presolution.EdgeProcessing.Interpreter` now runs one expansion-oriented execution function for all TyExp-left plans.
  - The prior separate synthesized-wrapper bridge function was removed.
- Wrapper semantics are preserved in the unified path:
  - synthesized wrappers still force `ExpIdentity` for their `ExpVarId`;
  - wrapper body/target instantiation pairs still use direct instantiation solving (`solveNonExpInstantiation`).
- Added characterization regression in `EdgeInterpreterSpec` for synthesized wrapper + forall target, asserting identity expansion assignment retention.
- Verification:
  - `Edge interpreter` matcher: 4 examples, 0 failures.
  - `Phase 3 atomic wrapping equivalence gates`: 7 examples, 0 failures.
  - Full gate remains green after bridge removal (`cabal build all && cabal test` => 631 examples, 0 failures).

### 2026-02-11 Phase 5 abstraction polish (type-level invariants + ID boundary)

- Resolved edge-plan payload now carries a refined `ResolvedTyExp` value; `eprMode`/`EdgePlanMode` were removed.
  - Effect: resolved plans encode TyExp-left shape directly instead of carrying a redundant runtime mode tag.
- Planner fail-fast is now structured, not stringly:
  - Added `ExpectedTyExpLeftInPlanner EdgeId TyNode` in `PresolutionError`.
  - Planner emits `PlanError (ExpectedTyExpLeftInPlanner edgeId leftNode)` for invariant violations.
- Synthesized wrapper `ExpVarId` allocation/checks are centralized in `MLF.Constraint.Types.SynthesizedExpVar`:
  - `initSynthExpVarSupply`, `takeSynthExpVar`, and `isSynthesizedExpVar`.
  - `Normalize` and interpreter now share this boundary instead of ad hoc negative-ID helpers.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types"'` => 7 examples, 0 failures.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge interpreter"'` => 3 examples, 0 failures.
  - Full gate remains green after polish changes.

### 2026-02-11 Phase 4 error-tag + regression-matrix completion

- Presolution phase boundaries now expose explicit error context:
  - `PlanError` wraps planner-surface failures (e.g. non-`TyExp` edge invariant).
  - `ExecError` wraps interpreter/runtime failures while preserving inner payloads.
- Added Phase 4 regression-matrix checks across presolution + pipeline suites:
  - expansion constructor coverage (identity / instantiate / forall-intro / compose),
  - identity trace-shape assertion,
  - compose witness-step shape assertion,
  - annotation-edge weaken suppression with preserved expansion assignments.
- One pre-existing occurs-check assertion was widened to accept wrapped errors (`PlanError`/`ExecError`) without changing semantic expectation.
- Verification: full suite is green after Phase 4 (`cabal build all && cabal test` => 630 examples, 0 failures).

## Summary of Changes

**Current status:** The pipeline records presolution witnesses and produces explicit generalization plans in `MLF.Constraint.Presolution.Plan`; elaboration applies these plans via `MLF.Elab.Generalize` without re-solving. Semantic paper-faithfulness deltas tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` are closed; remaining open work is non-semantic (proof/formalization and assurance breadth).

### 2026-02-11 Phase 3 wrapping equivalence recovery

- Normalization now stamps synthesized wrapper `TyExp` nodes with reserved negative `ExpVarId`s.
  - Rationale: preserve strict paper-shaped `TyExp <= τ` residual-edge invariant while retaining an unambiguous wrapper discriminator.
- Edge interpreter now dispatches synthesized-wrapper behavior by `ExpVarId < 0`, not TyExp body shape.
  - This prevents frontend TyExp edges from being misclassified as wrappers, restoring expansion-bearing semantics on real TyExp paths.
- Φ binder reorder now uses full order-key fallback when narrowed binder-key maps are incomplete.
  - Rationale: avoid false invariant failures (`PhiReorder: missing order key ...`) observed only under wrapped normalization shape, while keeping deterministic ordering via existing order-key comparison.
- Verification: Phase 3 equivalence gate suite (7/7) and full validation (`cabal build all && cabal test`, 626 examples) are green.

### 2026-02-08 A7 Group 1 binding-core shared-helper consolidation (docs sync)

- [x] Removed duplicated binding-path traversal helpers; canonical module is `MLF.Binding.Path` (`bindingPathToRootWithLookup`, `bindingPathToRoot`, `bindingPathToRootLocal`, `firstGenAncestorFromPath`).
- [x] Removed duplicated node-ref enumeration/existence helpers; canonical module is `MLF.Binding.NodeRefs` (`allNodeRefs`, `nodeRefExists`).
- [x] Removed duplicated scope-graph helper logic; canonical module is `MLF.Binding.ScopeGraph` (`buildTypeEdgesFrom`, `buildScopeNodesFromPaths`, `rootsForScope`).
- [x] Removed duplicated bound-child collection loops; canonical module is `MLF.Binding.Children` (`collectBoundChildrenWithFlag`, `collectBoundChildren`).
- Migration landing points:
  - `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, and `MLF.Binding.Canonicalization` now import the canonical helper modules.
  - `MLF.Constraint.BindingUtil.firstGenAncestorFrom` now delegates to `MLF.Binding.Path.firstGenAncestorFromPath`.
  - `MLF.Constraint.Presolution.Base.bindingPathToRootUnderM` now delegates to `MLF.Binding.Path.bindingPathToRootLocal` after quotient bind-parent canonicalization.
- Behavioral impact: none intended; this was an abstraction-only consolidation.

### 2026-02-09 H15 lambda-parameter source guard (implemented)

- Context:
  - After H13+H14, the `make` reproducer still failed in Phase 7 with a naming mismatch (`t23` vs `b`) even though let-scheme generalization was already correct (`forall a b. a -> b -> a`).
- Root cause:
  - In `MLF.Elab.Elaborate` (`ALam` case), unannotated lambdas could source parameter type reification from `resolvedLambdaParamNode lamNodeId` (copy-derived solved nodes) rather than lexical `paramNode`.
  - In the failing path this produced `ELam "y" (TVar "t23") ...`, while the let scheme stayed `... (TVar "b") ...`, causing `TCLetTypeMismatch`.
- Implemented fix:
  - Added `hasInformativeVarBound` and guarded param-source selection:
    - annotated-lambda desugaring keeps resolved-node behavior;
    - unannotated lambdas use resolved node only when its bound-chain reaches a non-`TyVar` bound (informative structural/base bound);
    - otherwise fall back to lexical `paramNode`.
  - This avoids solved-node-name leakage while preserving prior behavior for application typing paths that require resolved informative bounds.
- Regression coverage:
  - Added `PipelineSpec` test:
    - `does not leak solved-node names in make let mismatch`.
- Verification:
  - `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch"' --test-show-details=direct`
  - `cabal test mlf2-test --test-options='--match "runPipelineElab type matches typeCheck(term) and checked pipeline type"' --test-show-details=direct`
  - `cabal build all && cabal test`

### 2026-02-08 A7 group 2 dedup checklist

- [x] Frontend translate scope/parent wiring now routes through local helpers (`withScopedBuild`, `attachUnder`, `rebindScopeRoot`) across let/coercion/forall-internalization paths.
- [x] Elab run annotation node rewriting now routes through shared `mapAnnNodes`, reused by `applyRedirectsToAnn`, `canonicalizeAnn`, and debug edge-origin traversal.
- Result: duplicated control-flow wiring was collapsed into shared local helpers without changing behavior.

### 2026-02-18 A7 non-binding dedup closure (test harness)

- Added shared pipeline-stage helpers in `test/SpecUtil.hs`:
  - `runConstraintDefault`
  - `runToPresolutionWithAnnDefault`
  - `runPipelineArtifactsDefault` (`PipelineArtifacts` record for normalized constraint + presolution + solved + annotation + root).
- Migrated remaining non-binding harness duplication to shared helpers:
  - `test/PipelineSpec.hs`: replaced local pipeline setup chains and removed local `runPipelineWithPresolution`.
  - `test/ElaborationSpec.hs`: removed local `unsafeNormalize`/`generateConstraintsDefault`; moved binding-coverage + Φ-soundness setup to `runPipelineArtifactsDefault` and `runToPresolutionWithAnnDefault`.
  - `test/ConstraintGenSpec.hs`: default graph inference now reuses shared `unsafeNormalizeExpr`.
- Behavioral impact: none intended; this is consolidation-only refactoring to keep the solve chain single-sourced for A7 acceptance criteria.

### 2026-02-06 strict checked-authoritative follow-up

- `runPipelineElab` now uses checked type authority end-to-end while keeping reconstruction paths for diagnostics only.
- Top-level closure now falls back to explicit free-variable closure when root generalization yields no binders but the elaborated term is still type-open.
- Shared closure (`MLF.Elab.TermClosure`) now freshens scheme binders against existing `ETyAbs` names and rewrites free type-variable occurrences in term types/instantiations to avoid capture/regressions.
- Annotation elaboration aligns `InstInside (InstBot ...)` with the generalized annotation-bound head when available, reducing bound-erasure in explicit-forall annotation paths.
- Regression expectations in `test/ElaborationSpec.hs` were updated for checked-authoritative term/type shapes (top-level `ETyAbs` wrappers, `Bool`-authoritative result, and closed `∀a. a -> a` fallback for `\\y. let id = ... in id y`).
- Historical note: bounded aliasing requiring thesis Merge/RaiseMerge witness translation was still unresolved at this checkpoint.
- Root-cause clarification at that time: the gap was not pipeline order (desugaring before presolution remained correct), but alias-bound information being erased on a coercion path before edge-local RaiseMerge gating.
- This gap is now resolved by the 2026-02-08 staged-normalization + structural-gating implementation (see `BUG-2026-02-06-003` in `Bugs.md`).

### 2026-02-07 syntax frontend + canonical pretty migration

- Added eMLF parser/pretty modules:
  - `src/MLF/Frontend/Parse.hs`
  - `src/MLF/Frontend/Pretty.hs`
- Added paper-faithful xMLF syntax/parser/pretty modules:
  - `src/MLF/XMLF/Syntax.hs`
  - `src/MLF/XMLF/Parse.hs`
  - `src/MLF/XMLF/Pretty.hs`
- Added public xMLF API module: `src-public/MLF/XMLF.hs`.
- Extended `MLF.API` with explicit eMLF parse/pretty entry points (`parseRawEmlfExpr`, `parseRawEmlfType`, `parseNormEmlfExpr`, `parseNormEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`) and parse error rendering helpers.
- Added canonical syntax spec document: `docs/syntax.md` (legacy output, canonical target grammar, migration deltas, normalization rules, and implementation extensions).
- Migrated `MLF.Elab.Types` pretty-printing to syntax-driven rendering through `MLF.XMLF.Pretty`/`MLF.XMLF.Syntax` conversion helpers:
  - canonical xMLF computation forms are now printed (`ε`, `⊲σ`, `α⊳`, explicit `∀(⩾ ϕ)`/`∀(α ⩾) ϕ`, and derived `InstApp` as `∀(⩾ ⊲σ); N`);
  - unbounded binders are printed with explicit bottom bounds (`⩾ ⊥`);
  - term/type binder syntax now follows canonical parenthesized forms (`λ(x : σ)`, `Λ(α ⩾ σ)`).
- Added parser/pretty coverage tests:
  - `test/FrontendParseSpec.hs`
  - `test/FrontendPrettySpec.hs`
  - `test/XMLFParseSpec.hs`
  - `test/XMLFPrettySpec.hs`
- Updated existing elaboration pretty-output expectations in `test/ElaborationSpec.hs` to canonical syntax forms.

### 2026-02-08 solved-order shadow cutover semantics

- Generalize now treats solved-order as the solved-authoritative output order for reification/quantifier emission.
- After the 5/5 green gate, runtime fallback in `MLF.Elab.Generalize` no longer reifies or compares base-path shadow output.
- Solved-order output is authoritative in runtime generalization fallback (no runtime base-shadow compare).
- Shadow comparator helpers (`shadowCompareTypes`, `selectSolvedOrderWithShadow`) remain available for focused unit tests/debugging.

### 2026-02-08 staged frontend normalization + structural RaiseMerge gating (implemented)

- Implemented staged frontend boundaries:
  - Frontend types are now one indexed family: `SrcTy (n :: SrcNorm) (v :: SrcTopVar)`.
  - Backward-compatible aliases remain: `SrcType`, `NormSrcType`, `StructBound`, `RawSrcType`.
  - Forall bounds use `SrcBound n`; normalized bounds unwrap to `StructBound` via `unNormBound`.
- Implemented explicit normalization boundary:
  - `MLF.Frontend.Normalize` provides `normalizeType`/`normalizeExpr` with capture-avoiding alias inlining and explicit typed errors (`SelfBoundVariable`, `NonStructuralBoundInStructContext`) instead of runtime crashes.
  - Parser API has explicit raw and normalized entrypoints only (`parseRaw*`, `parseNorm*`); legacy compatibility aliases were removed for clean-break alignment.
- Implemented normalized-only compiler contracts:
  - `desugarSurface`, `generateConstraints`, and pipeline graph/elaboration entrypoints accept normalized expressions only.
- Implemented structural RaiseMerge gating:
  - `shouldRecordRaiseMerge` now uses only live canonical bound queries, binding-tree ancestry, edge-interior membership, same-root exclusion, and elimination state.
  - Precomputed binder-bound snapshots (`eusBinderBounds`) were removed from edge-unify state.
- Bounded aliasing baseline is restored end-to-end:
  - `runPipelineElab` and `runPipelineElabChecked` now both elaborate the bounded aliasing baseline to a type alpha-equivalent to `∀a. a -> a -> a`.
  - Regression test anchor: `test/ElaborationSpec.hs` case `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`.
- Tracking:
  - Ralph task: historical PRD path not retained in the current task tree
  - Related bug: `BUG-2026-02-06-003` (resolved in `Bugs.md`)

### 2026-02-08 strict SrcTy indexed model + staged pretty (implemented)

- Consolidated split frontend type declarations into one indexed AST in `MLF.Frontend.Syntax`:
  - `SrcNorm = RawN | NormN`
  - `SrcTopVar = TopVarAllowed | TopVarDisallowed`
  - `SrcTy` constructors (`STVar`, `STArrow`, `STBase`, `STCon`, `STForall`, `STBottom`) shared across raw/normalized paths.
- Added `SrcBound` wrappers and helpers (`mkSrcBound`, `mkNormBound`, `unNormBound`) so normalized forall bounds remain structurally rooted by type.
- Parser/normalizer/constraintgen internals now consume alias-aware wrappers instead of separate concrete `NST*`/`SB*` node declarations.
- Pretty printing is now staged/generic:
  - `prettyEmlfType :: SrcTy n v -> String`
  - `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`
  while preserving canonical output syntax.
- Regression anchors:
  - `test/ElaborationSpec.hs` — `SrcTy indexed aliases compile shape`
  - `test/FrontendParseSpec.hs` — `parses raw forall binder and keeps raw alias type`
  - `test/FrontendPrettySpec.hs` — `pretty-prints normalized staged types`
  - `test/ConstraintGenSpec.hs` — `internalizes normalized forall bounds using indexed StructBound alias`

### 2026-03-07 thesis-exact recursion-refactor verifier sweep

- Fresh verifier sweep outcome: rows 1–4 and 6 in `docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md` are thesis exact against the live codebase and the thesis references named there; rows 5, 7, and 8 remain open evidence/guardrail gaps.
- Row 1 `Surface Preprocessing Exactness`:
  - `MLF.Frontend.Normalize` remains the alias-bound normalization boundary; capture-avoiding substitution and alpha-renaming keep it binder-sensitive, so this module stays explicit rather than a blind recursion-schemes target.
  - `MLF.Frontend.Desugar.desugarSurface` is the tree-only preprocessing rewrite boundary.
  - `MLF.Frontend.ConstraintGen.Translate` remains the semantic boundary where annotations become explicit coercion/graphic constraints; typed lets stay coercion-only sugar (`ELet x (EAnn rhs σ) body`), not declared-scheme syntax.
- Row 2 `Leftmost-Lowermost Quantifier Ordering`:
  - `sigmaReorder` plus the O15 reorder guard cases are the authoritative freeze points for `§15.2.4` / `§15.3.4` binder-order semantics.
- Row 3 `Let-Scope Translation Discipline`:
  - The live parser/frontend/constraint path preserves the revised let-scope translation from `§15.2.6`; typed-let syntax remains sugar for an annotated RHS and does not reintroduce declared-scheme behavior.
- Row 4 `Translatable Presolution Boundary`:
  - Validation/rigidification is an explicit graph/presolution guardrail, not a recursion-schemes target.
- Row 5 `Typing Environment Construction`:
  - Scope and environment helpers stay under fail-fast binding-tree error propagation, and row5 now has direct production-path anchors for `Definition 15.3.6` / `Property 15.3.7` in `docs/thesis-obligations.yaml` plus live-path elaboration regressions in `test/ElaborationSpec.hs`.
- Row 6 `Computation Context Construction`:
  - `Phi.Context` / `Phi.Omega` remain explicit context-search logic; future cleanup must preserve context-find/reject behavior and binder/order-sensitive insertion points.
- Row 7 `Binder-Safe Tree Recursion Coverage` per-traversal audit:
  - Exhaustive active-campaign traversal inventory:
    - `src/MLF/Frontend/Desugar.hs` — `safe fold`: pure tree rewrite over normalized surface syntax; candidate for `cata`-style cleanup only.
    - `src/MLF/Frontend/Normalize.hs` — `keep explicit`: alias inlining uses capture-avoiding substitution and alpha-renaming, so binder/capture semantics remain the primary concern.
    - `src/MLF/Elab/TermClosure.hs` — `already recursion-schemes-backed`: retain as the positive reference example for genuinely tree-shaped elaboration helpers.
    - `src/MLF/Elab/Reduce.hs` — `already recursion-schemes-backed`: keep as a second positive reference example for tree-shaped term/type substitution.
    - `src/MLF/Elab/Elaborate.hs` — `keep explicit`: environment threading and subterm-specific elaboration rules are semantic control flow, not a blanket fold target.
    - `src/MLF/Constraint/Presolution/WitnessCanon.hs` — `graph-boundary`: localized folds are acceptable internally, but witness normalization remains graph-sensitive overall.
    - `src/MLF/Constraint/Presolution/Driver.hs` — `graph-boundary`: presolution scheduling/finalization is not a recursion-schemes simplification target.
    - `src/MLF/Constraint/Presolution/Validation.hs` — `graph-boundary`: translatability validation/rigidification is an explicit thesis guardrail.
    - `src/MLF/Reify/Core.hs` — `graph-boundary`: reification follows graph-aware naming/boundary rules and is outside broad tree-fold refactors.
  - Inventory rule: future recursion-refactor work may touch only entries classified `safe fold` or `already recursion-schemes-backed` unless a new verifier-owned thesis audit explicitly reclassifies a module.
- Row 8 `Graph-Phase Explicitness Guardrail`:
  - Explicit negative guardrail: broad recursion-schemes rewrites are a non-goal for graph-sensitive phases unless a later row-specific verifier pass says otherwise.
  - Guardrail-owned modules for this campaign are: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/Validation.hs`, `src/MLF/Constraint/Presolution/WitnessCanon.hs`, `src/MLF/Reify/Core.hs`, and their graph-aware companions.
  - Allowed local changes in those modules are limited to thesis-preserving helper extraction, documentation, or tests; proposals to replace the explicit graph algorithms themselves require new row-specific thesis evidence first.

### 2026-02-08 Phase 6 crash hardening (BUG-2026-02-06-001)

- Before the solved-order cutover, `MLF.Elab.Generalize.reifyWithGaBase` validated `solvedToBasePref` targets before any base-constraint reification.
- After the cutover gate passed, runtime elaboration no longer depends on `reifyWithGaBase`; fallback now reifies from solved-order roots/substitutions.
- The nested let + annotated-lambda reproducer remains covered by `test/ElaborationSpec.hs` and no longer crashes in Phase 6.
- The follow-up Phase 7 mismatch path (`BUG-2026-02-08-004`) is now resolved (2026-02-10) with thesis-green checked/unchecked `Int` behavior.

## Module Structure (Post-Refactor)

The codebase has been refactored for improved navigation and paper-faithfulness auditing:

### Graph Types (`MLF.Constraint.Types.Graph`)

The monolithic `Graph` module has been split into focused submodules:

| Submodule | Contents |
|-----------|----------|
| `Graph.NodeEdge` | Core node and edge definitions (`NodeId`, `TyNode`, `InstEdge`, `UnifyEdge`, etc.) |
| `Graph.Binding` | Binding-related types (`BindFlag`, `BindParents`, `BindingError`) |
| `Graph.Accessors` | Accessor utilities (`maxNodeIdKeyOr0`) |

`MLF.Constraint.Types.Graph` re-exports all submodules as a facade.

### Presolution (`MLF.Constraint.Presolution`)

Presolution modules now use shared state-access helpers:

| Module | Purpose |
|--------|---------|
| `StateAccess` / `Ops` | Shared `MonadPresolution` accessors (`getConstraint`, `modifyConstraint`, `liftBindingError`) |
| `EdgeProcessing` | Edge-local logic with explicit `EdgeCtx` |
| `EdgeProcessing.Witness` | Witness construction helpers |
| `EdgeProcessing.Unify` | Edge-local unification |

### Unification (`MLF.Constraint.Unify`)

Shared unification core for consistent behavior across phases:

| Module | Purpose |
|--------|---------|
| `Unify.Core` | Policy-driven unification with `UnifyStrategy` |
| `Unify.Decompose` | Structural decomposition helpers |

### Elaboration (`MLF.Elab`)

Elaboration now uses structured config records:

| Record | Purpose |
|--------|---------|
| `ElabConfig` | Static configuration (debug flags, etc.) |
| `ElabEnv` | Per-elaboration environment (naming, etc.) |

Legacy code is isolated in `MLF.Elab.Legacy` (e.g., `expansionToInst`).

### Documentation

- `docs/paper-map.md` — Paper-to-code mapping for auditing
- `docs/phase-notes.md` — Phase invariants and test references

### 1. src/MLF/Constraint/Presolution/Driver.hs (+ EdgeUnify/Witness)
- **`unifyStructure` / `unifyStructureEdge`**: Recursively unify structural children (TyArrow, TyForall, plus TyVar bounds) so `Arrow A B ~ Arrow C D` propagates `A~C` and `B~D` (Driver for global merges; EdgeUnify for edge-local χe execution).
- **`processInstEdge`**:
  - Uses `unifyStructure`/`unifyStructureEdge` instead of raw `unifyAcyclic`.
  - Eagerly materializes non-Identity expansions (`applyExpansionEdgeTraced`), binds the expansion root like the target, and unifies the expansion result with the target (plus the original TyExp wrapper).
  - Guards against `Identity` expansion cycles by skipping `TyExp ~ Target` unification when expansion is `Identity` (relying on `decideMinimalExpansion` unifications instead).
- **Per-edge instance witnesses (`Φ` input) + traces**:
  - Presolution records `EdgeWitness` + `EdgeTrace` per instantiation edge (`psEdgeWitnesses` / `psEdgeTraces`, surfaced as `prEdgeWitnesses` / `prEdgeTraces`).
  - Witnesses combine expansion-derived steps (`witnessFromExpansion`) with edge-local unification ops from `EdgeUnify` (Raise/Merge/Weaken).
  - `ExpForall` yields `StepIntro` entries (xMLF quantifier-introduction `O`) in `ewSteps`, not Ω ops; `ExpInstantiate` yields per-binder Ω ops (`OpGraft`/`OpWeaken`/`OpMerge`).
  - Witness steps are normalized in `normalizeEdgeWitnessesM` via `normalizeInstanceStepsFull` (coalesces Raise+Merge into RaiseMerge, enforces “Weaken-last” ordering, avoids double elimination).
  - `ExpInstantiate` witness/application logic skips “vacuous” `TyForall` wrappers (quantifier levels with no binders) so `Φ` construction doesn’t fail on nested/structural ∀ nodes.
  - `ExpInstantiate` witnesses avoid invalid grafts under non-⊥ bounds: if a binder has an instance bound that is another in-scope variable (e.g. `b ⩾ a`), presolution emits `OpMerge(b, a)` rather than `OpGraft` (paper Fig. 10 “alias + eliminate”).
    - Current behavior: RaiseMerge recording uses live structural graph facts (`shouldRecordRaiseMerge`) rather than alias-metadata survivability; this closed `BUG-2026-02-06-003`.
  - When an expansion includes a later `ExpForall`, `ExpInstantiate` witnesses suppress `OpWeaken` so binder metas stay flexible until the new quantifier is introduced (avoids empty Q(n) and lost ∀ in bounded-aliasing cases).
  - Edge-local unification can record `OpRaiseMerge(b, m)` when unification forces a **bounded** binder’s instantiation meta to unify with a `TyVar` bound **above the instantiation-edge root** in the binding tree (recorded as `OpRaise` + `OpMerge`, then normalized to `OpRaiseMerge`), matching the paper’s “escape to bound-above node” shape.
    - Implemented behavior: this emission path is no longer gated by edge-local `binderBounds`; it queries live canonical bounds and structural ancestry/interior predicates directly.
- **Scope tracking (paper `Raise` as graph transformation)**:
  - TyVar/TyVar unions harmonize binding parents by executing the paper `Raise(n)` graph operation as a binding-edge rewrite on `Constraint.cBindParents` (`MLF.Binding.Adjustment` / `MLF.Binding.GraphOps`).
  - During instantiation-edge solving (χe), the same per-step raises are also recorded as `OpRaise` in the edge witness Ω (`unifyAcyclicRawWithRaiseTracePrefer` → `unifyAcyclicEdge` / `unifyAcyclicEdgeNoMerge`), aligning with `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4 / Fig. 10).
  - Variable bounds and eliminations are stored in `Constraint.cVarBounds` / `Constraint.cEliminatedVars` (`MLF.Constraint.VarStore`) and are looked up by canonical `NodeId`, so they stay consistent as binding edges and UF representatives change.
- **`materializeExpansions`**: Avoids duplicating fresh nodes by reusing the already-unified expansion result for non-Identity expansions; Identity expansions still rewrite `TyExp` wrappers to their bodies.
- **`rewriteConstraint`**: Ensures Identity `TyExp` wrappers are erased even when they are not the Union-Find root (redirecting the whole UF class to the wrapper’s body). This fixes over-generalization bugs in paper-alignment baselines like `let id = (\x. x) in id id` and `\y. let id = (\x. x) in id y`.

### 2. src/MLF/Constraint/Normalize.hs
- **Module split**: `MLF.Constraint.Normalize` is now a thin façade over `MLF.Constraint.Normalize.Internal`, `MLF.Constraint.Normalize.Graft`, and `MLF.Constraint.Normalize.Merge`; the split preserves the existing public exports and behavior while keeping the parent module under 200 lines.
- **Module split**: `MLF.Reify.Type` is now a thin façade over `MLF.Reify.Type.Core`; the core reification algorithm (`reifyWith`, `reifyWithAs`, `ReifyRoot`) lives in `Type.Core`, while public wrapper functions (`reifyType`, `reifyTypeWithNames*`, `solvedFromView`, `freeVars`) remain in the façade.
- **Module split**: `MLF.Elab.Run.ResultType.Fallback` is now a thin façade over `MLF.Elab.Run.ResultType.Fallback.Core`; the bulk fallback computation (`computeResultTypeFallbackCore`) lives in `Fallback.Core`, while entry points (`computeResultTypeFallback`, `computeResultTypeFallbackWithView`) remain in the façade.
- **Module split**: `MLF.Constraint.Presolution.Plan` is now a thin façade over `MLF.Constraint.Presolution.Plan.Env`, `MLF.Constraint.Presolution.Plan.Generalize`, and `MLF.Constraint.Presolution.Plan.ReifyStep`; environment construction, generalization planning, and reification planning are separated into focused submodules, while `buildGeneralizePlans` remains in the façade.
- **Module split**: `MLF.Elab.Phi.Omega.Interpret` is now a pure re-export façade over `MLF.Elab.Phi.Omega.Interpret.Internal`; the full `phiWithSchemeOmega` implementation lives in the Internal submodule.
- **`applyUnionFindToConstraint`**: Enhanced to perform "grafting". When a `TyVar` node is unified with a structural node (e.g., `TyBase`), the `TyVar` node in the graph is destructively updated to become a copy of that structure. This ensures that external references to the variable (like the expression root) see the inferred structure.
- **Binding-edge Raise harmonization**: Var-var merging harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.

### 3. src/MLF/Constraint/Solve.hs
- **Binding-edge Raise harmonization**: Phase 5 harmonizes `Constraint.cBindParents` (paper `Raise(n)`) before unioning, keeping scope stable regardless of UF representative choice.
- **Elimination rewrite**: `solveUnify` now rewrites eliminated binders into their bounds (or explicit `TyBottom` nodes), removes them from the graph, and clears `cEliminatedVars` before elaboration.
  - The solve-time union-find map is extended with the elimination substitution so witness ops that mention eliminated ids still canonicalize to live nodes.

### 4. src/MLF/Elab/Generalize.hs + src/MLF/Elab/Generalize/* + src/MLF/Elab/Elaborate.hs + src/MLF/Elab/Run.hs (reexported via `MLF.Elab.Pipeline`)
- **Generalize is now an orchestrator**:
  - Phase-oriented logic moved into focused modules: `Generalize/Plan`, `SchemeRoots`, `BinderPlan`, `Ordering`, `ReifyPlan`, `Normalize`, and `Helpers`.
  - The top-level `generalizeAt`/`generalizeAtWith` functions now read as a linear pipeline of plan → binders → ordering → reify → normalize, with local helpers split by concern.
- **`generalizeAt`**:
  - Optimized to handle structural `TyForall` nodes (avoiding double quantification).
  - Returns the `subst` (renaming map) alongside the scheme.
- **Scope follows the solved graph**:
  - Binder discovery is binding-tree driven (`Constraint.cBindParents`): `TyForall` scopes use the body as the ≺ root, while non-Forall scopes use binding-parent paths to the nearest gen ancestor.
  - Presolution rewrite reconstructs binding parents and reattaches unparented nodes to the root gen node, keeping expansion/copy roots in-scope for generalization.
  - `generalizeAt` + `reifyTypeWithNamesNoFallback` rely solely on binding-tree enumeration (no free-variable fallback).
  - Rigid binding edges are treated as inline bounds, and bounds are included in reachability when ordering binders.
  - Elaboration no longer consults `cEliminatedVars`; eliminated binders are already rewritten out of the graph. Vacuous `TyForall` wrappers (no binders) are elided during reification.
- **`substInTerm` / `substInType`**: Implemented in `MLF.Elab.Elaborate` to apply the renaming map from `generalizeAt` to the elaborated term body. This ensures that terms use the same variable names as their type schemes (e.g., `Λa. λx:a. x` instead of `Λa. λx:t0. x`).
  - 2026-02-20: `deriveLambdaBinderSubst` now preserves alternate node-key aliases for the same binder name when lambda arity matches unbounded scheme arity, so elaborated RHS-coercion lets do not lose the `tN -> binder` rewrite needed for step/typeCheck stability.
- **`elaborate`**: Applies substitution to the RHS of let-bindings.
- **Witness translation (`Φ`) + quantifier reordering (`Σ`)**:
  - Elaboration reifies instantiations from recorded per-edge witnesses (`prEdgeWitnesses`) via `phiFromEdgeWitnessWithTrace` (rather than `expansionToInst`), using `EdgeTrace` for copy maps/interiors. Production elaboration requires trace; no-trace entry points are test/debug-only.
  - `Φ` consumes interleaved `ewSteps` (`StepIntro` for `O`, `StepOmega` for Ω); `OpGraft`+`OpWeaken` maps to `InstApp` (⟨τ⟩), `OpGraft` alone maps to an `InstBot` inside the binder, and `OpMerge`/`OpRaise`/`OpRaiseMerge` map to the paper’s alias/raise instantiations (Fig. 10).
- `phiFromEdgeWitnessWithTrace` targets binders using `InstUnder` contexts (`C{·}`) and prefixes Ω-translation with the ≺-based reordering ϕR/Σ(g) when `Typ` vs `Typexp` disagree (thesis Def. 15.3.4); missing non-spine contexts are errors, and normalized ω ops that violate translatability (e.g. `OpRaise` outside `I(r)`, non-transitive-flex `OpRaise` targets, non-binder targets, rigid-only-on-non-operated-endpoint for Merge/RaiseMerge) are rejected rather than silently skipped. Rigid identity behavior follows the literal thesis condition on operated node `n` for Raise/Merge/RaiseMerge.
  - Implemented explicit quantifier reordering instantiations (`sigmaReorder`) using adjacent swaps per `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.4).
  - Implemented `applyInstantiation` (in `MLF.Elab.Inst`, reexported via `MLF.Elab.Pipeline`) to check/apply xMLF instantiations to xMLF types (see `papers/these-finale-english.txt`; `papers/xmlf.txt` Fig. 3), used by tests to validate that `Φ(e)` transforms the source type into the target type.
- **`expansionToInst`**: Kept as a legacy/debug conversion from `Expansion` to `Instantiation` (no longer the main path for elaboration, and no longer re-exported via `MLF.Elab.Pipeline`).
- **`runPipelineElab`**: Generalizes the top-level result using the nearest gen ancestor of the expression root (root gen node for top-level), keeps reconstruction checks for diagnostics, and reports the type-checker result as the authoritative pipeline type.

## Testing
- **`test/ElaborationSpec.hs`**: Updated expectations to reflect correct polymorphic behavior and variable naming. Added integration tests for polymorphic instantiation.
- **Witness translation tests**: Added focused tests for `Σ(g)` reordering and for `Φ` soundness (`applyInstantiation source Φ(e) == target` for representative instantiation edges).
- **`test/PresolutionSpec.hs`**: Verified that instantiation edges merge nodes correctly.
- **`test/TypeCheckSpec.hs` + `test/ReduceSpec.hs`**: Cover xMLF type-checking and reduction/instantiation semantics.

Note: `test/ElaborationSpec.hs` also contains **paper-alignment baseline tests** that serve as regression coverage while we continue aligning witnesses toward `papers/these-finale-english.txt` (see also `papers/xmlf.txt`, especially around Merge/RaiseMerge and aliasing behavior).

## `papers/these-finale-english.txt` study: thesis ↔ repo mapping (with `papers/xmlf.txt` cross-reference)

This repo’s design is primarily informed by:

- `papers/these-finale-english.txt` (thesis) for **xMLF**’s explicit types/instantiations/terms and the **elaboration** story; see `papers/xmlf.txt` for supplemental xMLF presentation details and figure numbering.
- The earlier “graphic constraints” papers (ICFP’08 / TLDI’07) for the **solver pipeline** that produces presolutions.

### Paper anchors (from `papers/these-finale-english.txt`; `papers/xmlf.txt` figure numbers for reference)

- **Fig. 1–4**: xMLF grammar, instantiation judgments, instantiation-as-a-function on types, and xMLF term typing rules.
- **§3.1–§3.5 + Fig. 7/9/10**: elaboration from (graphical) eMLF presolutions to xMLF:
  - `/)(g) = Λ(Q(g))` (insert type abstractions for flexible bindings at a level)
  - `Φ(e)` (compute instantiation witnesses from solved instantiation edges)
  - `S/Q/T` (map presolution nodes to xMLF types)
  - `Σ(g)` (quantifier reordering when the expansion’s quantifier order differs)

### Mapping: paper notation → repo types/functions

| Paper | Meaning | Repo |
|------:|---------|------|
| `b` | eMLF surface term | `src/MLF/Frontend/Syntax.hs` (`Expr` + indexed `SrcTy` aliases) |
| `χ` | constraint graph | `src/MLF/Constraint/Types/Graph.hs` (`Constraint`) |
| `n` | type node in the graph | `NodeId` + `TyNode` in `Constraint.cNodes` |
| `g` | binding-tree node (generalization site) | `GenNodeId`/`GenNode` + `Constraint.cBindParents` |
| `≤` edge | instantiation constraint | `InstEdge` (`Constraint.cInstEdges`) |
| `=` edge | unification constraint | `UnifyEdge` (`Constraint.cUnifyEdges`) |
| `s·τ` | expansion node / expansion variable | `TyExp{ tnExpVar :: ExpVarId }` + `Expansion` recipes in `Presolution` |
| `χp` | (principal) presolution | `MLF.Constraint.Presolution.PresolutionResult` (plus `prEdgeExpansions`) |
| `τ` | xMLF type | `src/MLF/Elab/Types.hs` (`ElabType`) |
| `φ` | xMLF instantiation witness | `src/MLF/Elab/Types.hs` (`Instantiation`) |
| `a` | xMLF term | `src/MLF/Elab/Types.hs` (`ElabTerm`) |

### Mapping: solver + elaboration phases → modules

| Phase | Role (paper) | Repo entry point |
|------:|--------------|------------------|
| 1 | Constraint generation | `MLF.Frontend.ConstraintGen.generateConstraints` |
| 2 | Local simplification (grafting/merging) | `MLF.Constraint.Normalize.normalize` |
| 3 | Acyclicity / dependency ordering | `MLF.Constraint.Acyclicity.checkAcyclicity` |
| 4 | Presolution (minimal expansions) | `MLF.Constraint.Presolution.computePresolution` |
| 5 | Global unification | `MLF.Constraint.Solve.solveUnify` |
| 6 | Elaborate to xMLF | `MLF.Elab.Pipeline.elaborate` / `MLF.Elab.Pipeline.runPipelineElab` |

### Alignment notes / known gaps vs `papers/these-finale-english.txt` (see `papers/xmlf.txt` §3 for numbering)
- **Witness translation (`Φ`)**: `papers/these-finale-english.txt` translates *normalized instance-operation witnesses* into xMLF instantiations (see `papers/xmlf.txt` Fig. 10). This repo records a per-edge `EdgeWitness` during presolution and translates it to an xMLF `Instantiation` via `MLF.Elab.Pipeline.phiFromEdgeWitnessWithTrace` in production paths (`phiFromEdgeWitnessNoTrace` remains test/debug-only).
  - Quantifier-introduction (`O`) is not part of Ω in the thesis (see `papers/xmlf.txt`); the repo records these steps as `StepIntro` entries in `EdgeWitness.ewSteps` (from `ExpForall`) and translates them interleaved with Ω segments when constructing Φ(e).
  - Ω ops emitted today include `OpGraft`+`OpWeaken`, `OpMerge` (bounded aliasing like `b ⩾ a`, plus unification-induced aliasing during instantiation-edge solving), `OpRaise` (paper-general binding-edge raising on arbitrary interior nodes), and `OpRaiseMerge` for bounded-binder “escape” patterns. χe execution is paper-shaped for binding-tree ops: Raise/Weaken are executable binding-edge rewrites, and `EdgeTrace.etInterior` records the exact paper interior `I(r)` for filtering.
    - Bounded-aliasing caveat (`BUG-2026-02-06-003`) is resolved: RaiseMerge gating now uses structural live-graph predicates, and bounded aliasing elaborates to the thesis-aligned baseline in both checked and unchecked pipelines.
  - Φ requires a representable translation context; missing contexts and other non-translatable cases are hard failures. Rigid identity handling is literal for Raise/Merge/RaiseMerge on operated node `n`; rigid only on the non-operated endpoint is rejected as non-translatable.
- **Trace root/interior coherence**: `EdgeTrace` root/interior refresh and normalization share a single root-selection helper (`traceInteriorRootRef`) so `etRoot`, `etInterior`, and witness normalization all use the same interpretation of `r`/`I(r)`.
- **Witness merge-direction strictness**: Ω normalization rejects malformed merge direction (`MergeDirectionInvalid`) in all normalization entrypoints (helper + production); there is no permissive merge-direction fallback.
- **`OpRaise` translatability strictness**: non-rigid `OpRaise` now requires the operated node to be transitively flexibly bound to expansion root `r`; otherwise normalization fails fast with `NotTransitivelyFlexBound` (with direct validator and presolution-path regressions).
- **Fig. 15.3.4 witness matrix closure (2026-02-10)**: witness normalization/emission now has an explicit 15-row closure contract encoded as row-labeled tests (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`, with matrix gate green via `cabal test mlf2-test --test-show-details=direct --test-options='--match R-'` and full gate green via `cabal build all && cabal test`.
- **Context search strictness**: `contextToNodeBound` follows thesis context grammar (under-quantifier / inside-bound) and does not use non-thesis fallback descent through `TyForall` body.
- **Quantifier reification (binding-tree based)**: `Q(n)`/reification quantifies flexibly bound `TyVar` binders using binding-parent edges (bounds included in reachability), so bounds and contexts remain representable in Φ and generalization.
- **Quantifier reordering (`Σ(g)` / `ϕR`)**: implemented via `MLF.Elab.Sigma` / `MLF.Elab.Pipeline.sigmaReorder` (adjacent swaps per `papers/these-finale-english.txt` Def. 15.3.4 / Fig. 15.3.5; see `papers/xmlf.txt` §3.4). Φ translation (`phiFromEdgeWitnessWithTrace` → `phiWithSchemeOmega`) prefixes Ω-translation with this reordering whenever `Typ(a′)` and `Typexp(a′)` disagree in binder order — even when Ω contains no Raise steps — while still targeting binders for Ω using `InstUnder` instantiation contexts (paper’s `C{·}`). The computation is deterministic and fail-fast: missing <P order keys or bound-dependency cycles produce `InstantiationError` messages prefixed `PhiReorder:` rather than silently returning `InstId`.
- **Application elaboration shape**: now matches Fig. 7 — constraint generation emits instantiation edges for both function and argument, and elaboration wraps each side with `ETyInst` when non-identity.
- **Constraint representation differences**: the thesis's graphical presentation (see also `papers/xmlf.txt`) uses a term-dag plus a binding tree with flexible/rigid edges and node classes (inert/instantiable/restricted/locked). The repo mirrors the same split (`Constraint.cNodes` + `Constraint.cBindParents` with `BindFlex`/`BindRigid`); some paper machinery remains simplified (e.g. witness normalization/ordering is implemented but not yet backed by formal proofs).
- **xMLF Phase 7**: the repo includes type-checking and reduction for xMLF terms/instantiations (`MLF.Elab.TypeCheck`, `MLF.Elab.Reduce`) and uses them in tests, but still lacks a fully formalized/verified connection to the thesis presentation (e.g., proof obligations and full evaluation-context coverage).

## Kiro spec planning
- Paper-faithfulness deltas are captured in `.kiro/specs/paper-faithfulness-remaining-deltas/`, including evidence pointers to the thesis and code, plus a concrete implementation plan.

## 2026-02-10 BUG-2026-02-06-002 staged closure notes

- `MLF.Elab.Phi.Omega` now treats delayed binder-local `OpGraft ... OpWeaken` pairs as a single binder application path when no intervening op touches that binder, and rescues binder-arg `TBottom` reification to binder TVar naming when available.
- `MLF.Elab.Elaborate` let elaboration now computes an env-aware RHS type (`typeCheckWithEnv`) and uses a guarded fallback scheme only when the generalized scheme and RHS-derived generalized scheme are not alpha-equivalent.
- `MLF.Elab.Elaborate` application elaboration extends non-polymorphic-arg repair to `InstApp TForall{}` fun-instantiation payloads, reifying argument type from the argument annotation node.
- Current test evidence:
  - `BUG-2026-02-06-002 strict target matrix`: green (`4/4`).
  - `BUG-2026-02-06-002 thesis target`: green (checked + unchecked).
  - focused guards (make-const generalization, redirected let-use polymorphism, H15 non-leak): green.
  - sentinel matrix has been graduated to strict assertions (no pending cases under `BUG-2026-02-06-002`).


## 2026-02-10 BUG-2026-02-06-002 final closure notes

- Witness normalization now enforces thesis-shape upstream for graft/weaken interactions:
  - canonical ambiguous mapping rejects multiple canonical graft args for one weakened binder,
  - delayed graft/weaken pairs are coalesced safely before Ω translation.
- Ω translation is local again:
  - standalone `OpGraft` no longer performs delayed non-local weaken scan,
  - binder `TBottom` rescue is scoped to adjacent `OpGraft+OpWeaken` only.
- Scheme simplification preserves named structured bounds (`simplifySchemeBindings` blocks structured-bound inline for named binders), preventing Phase 6 dependency/bound erasure regressions.
- ALet fallback now has two scoped branches:
  - existing app/unbounded/Int-codomain path,
  - lambda replacement path with env-aware RHS typing and `subst = IntMap.empty` when replacing the scheme.
- Verification:
  - `BUG-2026-02-06-002 strict target matrix`: PASS (`4/4`)
  - full gate: `cabal build all && cabal test` => PASS (`604 examples, 0 failures`)

## 2026-02-10 BUG-2026-02-08-004 thesis-green closure notes

- Dedicated sentinel in `test/PipelineSpec.hs` was flipped from rejection-shape guarding to thesis-expected success (`Int`) for both `runPipelineElab` and `runPipelineElabChecked`.
- Root-cause seam was in `MLF.Elab.Elaborate` application elaboration:
  - witness-derived `InstApp` could survive onto a function term whose elaborated type was already monomorphic arrow, yielding invalid `InstElim` during type checking;
  - polymorphic-argument repair previously only inferred args from syntactic `ELam`, missing equivalent typed-arrow cases after function-side instantiation.
- Fix in `AApp`:
  - guard `InstApp` by `typeCheckWithEnv` of the function term (`InstApp` kept only for `TForall{}`);
  - extend arg-instantiation inference to variable arguments when the (possibly instantiated) function term typechecks to `TArrow paramTy _`.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'` => PASS (`1 example, 0 failures`).
  - `cabal build all && cabal test` => PASS (`604 examples, 0 failures`).

## 2026-02-10 delayed-weakening diagnostics alignment

- Condition (5) (`delayed weakenings`, thesis Definition 11.5.2 in `papers/these-finale-english.txt`) is now surfaced explicitly in witness validation errors:
  - `OmegaNormalizeError` adds `DelayedWeakenViolation weakenedBinder offendingNode`.
- Previous behavior reused `OpUnderRigid` for this case, which conflated two independent failure modes:
  - rigid-path interior failures, and
  - delayed-weaken ordering failures.
- The explicit constructor keeps normalization failure reporting paper-faithful and improves targeted regression assertions in `test/Presolution/WitnessSpec.hs`.

## 2026-02-11 BUG-2026-02-11-003 closure notes

- BUG-004 nested annotation variants (`V2`, `V4`) are now strict-success regressions (`Int`) in both unchecked and checked pipelines.
- V2 closure aligns scheme/finalization identity ownership with Φ reorder requirements:
  - `MLF.Constraint.Presolution.Plan.Finalize` now includes quantified binder names in `usedNames`, preserving binder identity through scheme finalization.
  - `MLF.Elab.Phi.Omega` reorder identity checks now require identity only for scheme-owned quantifier positions.
- Removed non-thesis compatibility paths:
  - `MLF.Elab.Elaborate.reifyInst` no longer synthesizes fallback instantiation sequences from expansion traces when `phi == InstId`; elaboration uses `phiFromEdgeWitnessWithTrace` only.
  - `MLF.Elab.TypeCheck` / `MLF.Elab.Inst` are strict-only for `InstBot` (`InstBotMode`/mode APIs removed).
- Producer-side annotation/elaboration shaping is now explicit:
  - Desugared `ELamAnn` parameter recovery uses coercion-domain form matching only (`∀(v ⩾ b). v` → `b`) instead of broad bounded-identity collapse.
  - In `AApp`, inferred `InstApp τ` is normalized to `InstElim` when the argument term is already `∀(⩾ τ) ...`, avoiding strict-instantiation failure on bounded-forall terms.
- Guardrail from debugging iteration:
  - broad Omega relaxations (empty-binder-key short-circuit, weaken keep-all on empty keep-set, graft skip outside keep-set) were reverted after they regressed legacy make/Φ suites.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "strict"'` => `15 examples, 0 failures`.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2"'` => `2 examples, 0 failures`.
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V4"'` => `2 examples, 0 failures`.
  - `cabal build all && cabal test` => pass.

## 2026-02-16 BUG-003 normalization-side deterministic graft+weaken contract (historical checkpoint; superseded by 2026-02-17 closure)

- Implemented an annotation-edge-only pre-normalization pass in `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs`:
  - scope: Ω-segment local (`StepIntro` boundaries preserved),
  - trigger: ambiguous multi-graft/no-weaken shape,
  - action: synthesize exactly one deterministic `OpGraft+OpWeaken` pair for the replay binder.
- Added explicit fail-fast surface for synthesis dead ends:
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs` now includes `DeterministicGraftWeakenSynthesisFailed NodeId [NodeId]`.
- Deterministic chooser provenance:
  - source ordering from `etBinderArgs`,
  - source->replay mapping from normalized replay hints (`etBinderReplayHints` bridge path),
  - arg selection from trace args rewritten into normalization space.
- Added targeted regressions in `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`:
  - synthesis success on annotation-edge ambiguous shape,
  - fail-fast on missing live candidate args,
  - non-annotation guard (no synthesis).
- Verification outcomes for this pass:
  - targeted synthesis tests: green,
  - strict anchors + BUG-010 matrix reproducer: green,
  - at that checkpoint, `BUG-003-V1/V2` were red (stricter replay key-space mismatch bucket on synthesized `OpGraft+OpWeaken` targeting source key `6`),
  - at that checkpoint, full gate remained red (`677 examples, 33 failures`) in that workspace.

## 2026-02-16 BUG-003 replay-bridge follow-up (historical checkpoint; superseded by 2026-02-17 closure)

- Applied a focused replay-bridge candidate expansion in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`:
  - `computeTraceBinderReplayBridge` now seeds alias candidates from source binders that share the same replay-hint class (`etBinderReplayHints`) before final replay-map selection.
  - This closes the synthesized-key under-coverage case where BUG-003 edge-0 source key `6` had no replay-map entry despite sharing hint provenance with mapped sources.
- Applied a strict Ω bounded-branch correction in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`:
  - `OpGraft+OpWeaken(bound-match)` now emits binder elimination (`InstElim`) instead of bounded `InstApp`, avoiding the `InstBot expects ⊥` invariant violation for non-`⊥` bounds.
  - This keeps strict `InstBot` behavior unchanged while aligning bounded graft+weaken semantics with elimination.
- Verification (at that checkpoint):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
    - now returns to the baseline strict bucket: `PipelineTypeCheckError (TCLetTypeMismatch ...)` (no replay key-space mismatch, no `InstBot` invariant crash).
  - PASS:
    - `--match "fails fast when OpWeaken targets a trace binder source with no replay binder mapping"`
    - `--match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"`
    - `--match "does not require Merge for bounded aliasing (b ⩾ a)"`
    - `--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"`
    - `--match "make-app keeps codomain Int without bottom-domain collapse"`
    - synthesis regressions in `test/Presolution/WitnessSpec.hs` (3/3)
  - At that checkpoint, full gate remained red: `cabal build all && cabal test` => `677 examples, 33 failures`.
- Root-cause evidence from traced BUG-003 edge-0 replay:
  - replay map now includes source key `6` (`traceBinderReplayMap=[(0,4),(1,8),(2,38),(4,4),(6,4)]`),
  - edge-0 Φ becomes `InstElim` (instead of failing earlier on key-space mismatch),
  - At that checkpoint, elaborated RHS remained bottomized (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`), so BUG-003 strict-success closure remained open in the original semantic bucket.

## 2026-02-16 BUG-003 baseline trace: why bounds are bottomized before Φ

- Additional edge-local tracing isolated the first irreversible drift before Φ translation:
  - during edge `0` presolution execution (`runExpansionUnify`), extra χe ops include:
    - `OpRaise 2 ; OpMerge 2 2`
    - `OpRaise 0 ; OpMerge 0 0`
  - at the same point, copied binder metas become self-bound and eliminated:
    - `35 = TyVar { tnBound = Just 35 }`
    - `37 = TyVar { tnBound = Just 37 }`.
- Copy provenance confirms source->meta mapping for these binders:
  - edge-0 `etCopyMap` includes `(0 -> 35)` and `(2 -> 37)`.
- Presolution/solve state chain:
  - `prConstraint` already contains:
    - edge-0 expansion rewritten as `ExpInstantiate [30,35,32,33,34]` (arg `31 -> 35`),
    - `cEliminatedVars = {35}`,
    - bound-arrow nodes referencing `35` (for example nodes `7`, `10`).
  - `solveUnify` rewrites eliminated self-bound `35` to `TyBottom 46`, yielding bottomized bounds in those arrows (`7 dom=46`, `10 dom/cod=46`).
- Consequence:
  - edge-0 scheme bounds are semantically bottomized in presolution+solve transitions, so Φ replay starts from an already-bottomized graph for BUG-003.

## 2026-02-17 BUG-003 thesis-exact closure: edge self-merge/self-bound guards

- Implemented surgical presolution guards in `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`:
  - RaiseMerge emission now skips same-UF-class endpoints before recording/writing (`repRoot == extRoot` => no-op).
  - Edge-local bound writes now skip canonical same-root writes in `setVarBoundM` (`findRoot nid == findRoot bnd` => no-op), preventing `n -> n` self-bound artifacts.
- Added focused regression in `/Volumes/src/mlf4/test/ElaborationSpec.hs`:
  - `BUG-003-PRES: edge-0 presolution does not leave self-bound binder metas`.
  - Test inspects edge-0 trace (`etBinderArgs` + `etCopyMap`) and asserts no surviving binder-meta in `prConstraint` is `TyVar { tnBound = Just self }`.
- Verification (sequential, targeted):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-PRES"'` -> PASS
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'` -> PASS (`2 examples, 0 failures`)
  - strict anchors -> PASS:
    - `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`
    - `does not require Merge for bounded aliasing (b ⩾ a)`
    - `bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines`
- Outcome:
  - BUG-003 V1/V2 now elaborate to the thesis-expected type (`∀a. a -> a -> a -> a`).
  - Presolution no longer leaves edge-0 self-bound binder metas for the BUG-003 shape.

## 2026-02-17 BUG-002 replay-key contract completion

- Replay-key normalization contract is now enforced across all three layers involved in Φ replay:
  - presolution trace/hint restoration (`MLF.Constraint.Presolution.WitnessNorm`),
  - bridge construction (`MLF.Elab.Phi.Translate`),
  - Ω binder target lookup (`MLF.Elab.Phi.Omega`).
- Bridge resolution now prefers replay-binder-domain keys derived from replay scheme metadata and rejects non-binder drift for binder-target ops.
- Reify-time scheme-bound normalization (`MLF.Constraint.Presolution.Plan.ReifyPlan`) now rewrites binder self-references inside bounds to `⊥` before bound admission:
  - removes illegal self-bound forms (`∀(a ⩾ a)`),
  - preserves structural information for bounded shapes (for example `b -> a` becomes `⊥ -> a` for binder `b`),
  - keeps strict alias-bound rejection (`∀(b ⩾ a)`) intact.
- Deterministic BUG-002 matrix (`BUG-002-V1..V4`, seed `1593170056`) is green in this workspace after this pass.

## 2026-02-26 OpWeaken no-op fallback removal (Fig. 15.3.4 / §15.3.5)

- Removed both non-root `OpWeaken` no-op fallback exits in `MLF.Elab.Phi.Omega`:
  - non-binder alias target with no recoverable binder-in-spine,
  - binder target that cannot be located in current `vSpineIds`.
- Non-root `OpWeaken` now has a strict invariant:
  - resolve replay binder and emit thesis-shaped `InstElim`, or
  - fail fast with `PhiTranslatabilityError` (no silent identity/no-op path).
- Error payloads are normalized across both former fallback sites and now include:
  - op/replay/canonical targets,
  - solved class members considered,
  - recoverable binders,
  - current spine ids,
  - replay/hint map domains.
- Added focused regressions in `test/ElaborationSpec.hs` for both former fallback branches and retained alias-recovery success coverage.
- Rebaselined legacy fallback-dependent pipeline regressions to assert strict fail-fast behavior rather than permissive success.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpWeaken"'` (green),
  - `cabal build all && cabal test` (green),
  - `cabal test --test-show-details=direct` (green).

## 2026-03-02 Wave 2 solved-indirection closeout (Tasks 7-9)

- Presolution planner boundary is now view-first:
  - `PresolutionPlanBuilder` closure migrated from `Solved -> ...` to `PresolutionView -> ...`.
  - `MLF.Constraint.Presolution.Plan.buildGeneralizePlans` now consumes `PresolutionView` canonical data directly.
  - `MLF.Elab.Run.Generalize.generalizeAtWithBuilder` adapts runtime solved handles into `PresolutionView` before invoking the plan builder.
- `MLF.Constraint.Presolution.View.fromPresolutionResult` now accepts any `PresolutionSnapshot`:
  - signature generalized to `PresolutionSnapshot a => a -> PresolutionView`,
  - this removes the `Presolution.Base <-> Presolution.View` cycle pressure introduced by the builder-signature migration.
- `MLF.Constraint.Solved` was reduced by removing `fromPresolutionResult` (production-only builder surface):
  - no production call sites depended on it,
  - compatibility/test paths continue through `fromSolveOutput`, `fromPreRewriteState`, and `mkTestSolved`.
- Planner solved-compat reconstruction note:
  - first attempt (`Solved.fromPreRewriteState` from view snapshot) regressed paper baseline tests with `InvalidBindingTree ... node ... not in constraint`,
  - final approach uses `Solved.mkTestSolved` over `pvCanonicalConstraint` plus a live-node sanitized canonical map, restoring baseline behavior.
- Hygiene guard scope note (Task 9):
  - enforce no direct `MLF.Constraint.Solved` imports in elaboration entrypoint/public modules (`MLF.Elab.Run`, `MLF.Elab.Pipeline`, `MLF.API`, `MLF.Pipeline`),
  - internal elaboration modules still keep compatibility solved reads where removal would require broader architectural changes.

## 2026-03-07 row9-11 direct-target Ω closeout

- Ω no longer defines the local source-candidate recovery helpers (`sourceCandidates`, `pickExistingSource`, `adoptOpNode`, `graftArgFor`).
- `resolveTraceBinderTarget` remains the only target-selection bridge from witness-domain source ids to replay ids for ω operations.
- `OpRaise` now fails fast when no direct replay/source target exists, including the formerly silent non-trace no-op case.
- Source-domain interior membership keeps one bounded exception: direct forward `etCopyMap` alias evidence may justify interior membership, but reverse-copy/canonical candidate expansion no longer participates in runtime target recovery.
- `IdentityBridge` remains in the codebase as a witness-domain utility/test surface; runtime Ω binder selection is direct and no longer driven by local candidate ranking helpers.
## 2026-03-08 — Final live fallback removal closeout

- Generalization now uses a single strict thesis-shaped path at elaboration/runtime/result-type entrypoints; `SchemeFreeVars` and related closure problems surface directly instead of retrying through weaker no-GA/reify ladders.
- Planner owner resolution for synthesized wrappers now derives ownership from the wrapped body only; missing body-root ownership fails explicitly.
- `inferInstAppArgsFromScheme` keeps only structurally justified inference.
- `Phi` / annotation elaboration no longer uses the old trace/copy-map fallback search helpers. Instead it relies on explicit witness/trace/expansion authority, plus narrow term-closure alignment for already-introduced type abstractions.
- Result-type bound-overlay queries are now single-sourced through `MLF.Elab.Run.ResultType.View`; `ResultType.Fallback` no longer rebuilds a local overlayed `PresolutionView`, and the row-2 guard forbids that duplicate path from returning.
- `MLF.Elab.Run.ResultType.View` is now narrowed to overlay-aware queries only; plain result-type context reads come directly from `ResultTypeInputs`/`ChiQuery`, and the row-2 guard forbids the retired pass-through accessor scaffolding.
- The dead `rtvSchemeBodyTarget` pass-through is retired too, so `ResultType.View` now matches that documented overlay-aware boundary exactly and leaves `schemeBodyTarget` ownership in `MLF.Elab.Run.Scope`.
- `generalizeTargetNode` and `schemeBodyTarget` now share one owner-local target-unwrapping core in `MLF.Elab.Run.Scope`, while preserving the documented `S` vs `S'` policy split: `generalizeTargetNode` still follows named aliases for the named node's own bound/body, and `schemeBodyTarget` still keeps non-scheme-root named aliases at the named node for subterm translation.
- Result-type scheme-root detection and `rootForTypeAnn` / `rootForTypePreAnn` peeling are now single-sourced in `MLF.Elab.Run.ResultType.Util`; both `ResultType` and `ResultType.Fallback` call the shared helper, while the canonical-vs-pre-canonical split remains preserved exactly.
- `bindingScopeRefCanonical` now reuses `bindingScopeRef` on the canonical constraint instead of maintaining a second bind-parent walker, while `letScopeOverrides` continues to own base-vs-solved scope divergence semantics.
- Presolution edge witness/trace assembly is now single-sourced under `MLF.Constraint.Presolution.Witness`; the thin `EdgeProcessing.Witness` wrapper and its one-off `EdgeWitnessPlan` boundary were retired without changing witness ingredients, TyExp-body root selection, or Phase-2 op integration.
- Witness/trace canonicalization is now single-sourced under `MLF.Constraint.Presolution.Rewrite`; elaboration/runtime reuses that owner contract directly, while the elaboration-local `canonicalizeExpansion` helper remains separate because its `ExpForall` behavior still differs.
- Result-type annotated recursion is now single-sourced through the `ResultType` facade/`Ann` owner path; `ResultType.Fallback` no longer carries the local `computeResultTypeFromAnnLocal` workaround for nested `AAnn` cases.

## 2026-04-13 — Initial recursive ADT program surface

- Added the first public recursive-ADT program layer, which was later absorbed into the unified `MLF.API` / `MLF.Pipeline` ownership path on 2026-04-14.
- Introduced the initial program modules (`MLF.Frontend.Program.Syntax`, `Parse`, `Pretty`, `Check`, and `Run`) for module-oriented programs with `data`, `case`, typeclasses, instances, and `deriving Eq` over the initial recursive-ADT corpus.
- Added the Phase-0 syntax/corpus freeze at `docs/plans/2026-04-13-recursive-adt-syntax-freeze.md` and executable corpus programs under `test/programs/recursive-adt/` covering plain recursive ADTs, GADT-style constructor refinement, existentials, deriving, typeclass integration, module/export behavior, and an integrated cross-module example.
- Wired the initial surface into the public library and regression coverage.

## 2026-04-19 — `.mlfp` ProgramSpec regression repair

- Repaired the recursive-ADT and unified `.mlfp` ProgramSpec regressions while preserving the shared old eMLF/typecheck route. No new `.mlfp` syntax, direct Program-to-elaborated-term fallback, permissive `EUnroll`, or broad `TypeCheck` weakening was introduced.
- The Phase 7 binder-hygiene fix is producer-side: `freshenTypeAbsAgainstEnv` now reserves type variables already present in lambda parameter types, let schemes, and nested type-abstraction bounds before descending, matching the environment shape enforced by `TypeCheck`.
- Recursive-ADT repair stayed local to the producer/conversion seams: raw variable bounds are normalized before bound conversion, recursive existential constructor handlers keep ownership of their Church result binder, and constructor result naming remains canonical only where it is not capture-prone.
- Final closure cleanup prunes vacuous leading type abstractions from non-recursive xMLF output; the `church-true` and `choose` goldens now record the non-vacuous `forall a. forall b. a -> b -> a` shape.
- Evidence: fail-fast `mlf2-test` passed with 1582 examples, 0 failures; `MLF.Program execution corpus` passed with 9 examples, 0 failures; `MLF.Program` passed with 34 examples, 0 failures; direct `.mlfp` probes returned `true` for `authoritative-overloaded-method.mlfp` and `1` for `authoritative-case-analysis.mlfp`; final serial gates passed with `cabal test`, `cabal build all`, and `cabal test` again.

## 2026-05-20 — Round 305 parser parity import exposing tracer

- Added the bounded parser-owned `.mlfp` parser parity package `test/programs/compiler-parser-parity/import-exposing-def-bool/` for a single import declaration before `def main : Bool = true;`.
- Added the canonical projection fixture `test/conformance/mlfp/parser-parity/import-exposing-def-bool/expected/parser-program.txt`, covering module, export, import module, import exposed `Bool`, and carried def source spans.
- Extended `test/ProgramParserParitySpec.hs` with the public behavior test for import-exposing source-span parity and an import-specific malformed-syntax negative through the public `run-program` path.
- Scope remains parser-only: no Prelude or production facade widening, and no checker, backend, driver, platform, compiler-package, proof, parser-complete, or self-boot completion claim.
- Evidence: focused RED/GREEN import parity matcher, focused RED/GREEN import negative matcher, full parser-parity group, direct new and round-304 package smokes, `git diff --check`, `cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh`.
## 2026-07-11 - Identity-first construction and reference simplification

- `Expr` now distinguishes raw and resolved references at the type level.
  `resolveTermReferences` allocates lexical `LocalRef`s while crossing the
  parser boundary; every resolved variable and binder carries
  `ResolvedTermReference IdDetails`. `XmlfTerm` likewise requires `ResolvedVar`
  on every executable occurrence and lambda/let binder. The former late
  deferred-stamping, graph-local freshening, and occurrence-annotation repair
  passes were removed.
- `TypeView` is one abstract identity-bearing node tree. Head and binder nodes
  own display spelling, semantic payload, and aliases; stable spellings are
  derived from payloads rather than cached. Construction consumes one source
  shape plus identity aliases and reports missing or ambiguous payloads, so no
  parallel display/identity trees can drift. Matching, free-binder collection,
  elaborated-type conversion, backend conversion, substitution,
  specialization, and subtree projection traverse node payloads directly.
  Checked publication therefore needs no identity-completeness repair
  traversal.
- `LoweredBinding`, `CheckedBinding`, constraints, evidence methods, deferred
  obligations, constructor metadata, and binder plans store combined
  identity-bearing records rather than parallel string/type/identity sidecars.
  Constructor bindings are produced directly from `ConstructorInfo`, including
  constructor-local `forall` binders.
- `LoweredBindingIdentity` is now a closed top-level/constructor/method sum;
  the generic resolved-variable constructor that admitted local, environment,
  and deferred identities was removed.
- `Elab` environments now store one identity-keyed binding table and derive
  the type-check environment. `PreparedExternalBindings` stores one
  `PreparedExternalBinding` per alias, pairing the external binding and checked
  scheme before any restriction or union; constraint, elaboration, and
  type-check views are derived, eliminating both cache synchronization and
  cross-pairing between independently merged maps.
- Scoped source-type conversion constructs the identity-bearing `TypeView`
  before applying an import-visible display spelling. This removes the former
  `Core.Box -> C.Box` string rewrite followed by identity lookup. Source adapter
  comparisons share identity-aware type-head and binder-alpha equivalence and
  receive head aliases from the originating binding views. The string-only
  builtin resolver still rejects stable-looking spellings without a payload.
- `TypeBinderSubst` is a direct `Map TypeBinderIdentity TypeView`; its display
  alias set and metadata-light string substitution adapter were removed.
- Backend validator maps use `SymbolIdentity`/`BackendLocalKey` directly, and
  LLVM semantic AST keys store payload identities directly. Generic
  one-constructor reference wrappers and helpers that accepted but ignored
  display names were removed.
- Backend IR constructors require identities for every semantic declaration,
  reference, lexical binder, pattern, type head, and type binder. Test helpers
  allocate deterministic fixture identities; the permissive fixture IR,
  `ReferenceMode`, and name-fallback matchers were deleted. Backend validation
  now checks closed-program relationships and typing only.
- Structural recursive matching preserves the carried owner/self identities,
  and its evidence records carry data and constructor identity while treating
  names as diagnostics. A same-spelled wrong owner fails without canonicalizing
  the offending identity.
- LLVM lowering stores complete `FunctionParam` and `ClosureCaptureSlot`
  records. Generated wrappers, aliases, callable forms, and returned-partial
  closures allocate identity/name/type/kind together, avoiding equal-length
  parallel lists and truncating `zipWith3` construction.
- `ProductionBackendProgram` remains the validated lowering capability, with
  its raw projection confined to the LLVM owner. The detailed inventory and
  intentional string boundaries are recorded in
  `docs/audit/identity-string-reference-audit.md`.
- Verification evidence is updated after the final `cabal build all && cabal
  test` completion gate.

## 2026-07-12 - eMLF coercion construction and principal self-application

- Annotation lowering now constructs the thesis coercion boundary directly:
  its domain is a rigid copy of the annotated scheme and its codomain is a
  distinct flexible copy. Figure 8.2.3's Eq-Var case is represented by that
  copied graph itself; lowering does not add a synthetic
  `forall (beta >= sigma). beta` owner around the codomain. Source `forall`
  nodes remain explicit because they belong to `sigma`, not because the
  coercion manufactures another quantifier.
- Source `forall` types retain explicit graph structure through constraint
  generation. Presolution applies the paper's Eq-Var equivalence
  `forall (a >= tau). a = tau` while normalizing, so exact alias bounds become
  canonical before generalization rather than surviving as repair work for
  elaboration.
- Phi replay reconstructs binder identities only from producer-owned replay
  domains. Explicit annotations reuse an existing polymorphic source scheme
  only when the source and annotation types are alpha-equivalent; lambda and
  occurrence sidecars are refreshed together when an annotation changes a
  binder type.
- Generalization no longer imports a base-constraint bound for a solved binder
  that is both live-unbounded and not explicitly bounded. This constructs
  `forall a. a -> a` directly and prevents the spurious
  `forall (a >= bottom -> bottom). a -> a` form formerly corrected by a later
  identity-wrapper override.
- Internal graph variables are classified from free references only. Bound
  graph identities in a valid scheme are no longer mistaken for unresolved
  state. For an unannotated identity lambda applied to a contractive recursive
  value, AApp uses the checked argument type when constructing the binder and
  its occurrences, rather than waiting for Phase 7 to expose two incompatible
  closed mu representations.
- Regression coverage now checks the direct annotation, explicit coercion
  desugaring, and polymorphic `apply` presentations of
  `lambda (g : forall a. a -> a). g g`. All infer the principal flexible result
  `forall (beta >= sigma-id). sigma-id -> beta` and the emitted xMLF term passes
  `typeCheck`. The nested `let f = lambda x. x; let g = f; g g` regression now
  asserts the exact `forall a. a -> a` identity shape instead of mere pipeline
  success.
- Focused evidence: constraint generation passes 60 examples and Phase 6
  passes 252 examples, including the nine recursive same-lane alias frames.

## 2026-07-13 - Paper self-application program boundary and focused test latency

- The non-runtime eMLF surface and boundary matrices now check their
  self-contained programs directly. Only runtime matrix rows prepend the
  built-in Prelude. This keeps static diagnostics on the code path they are
  intended to cover and reduces the isolated `rejects bare overloaded method
  use` case from 21.8 seconds to 1.17 seconds without bypassing
  `checkProgram`.
- Added a `.mlfp` regression for the paper term
  `lambda (g : forall a. a -> a). g g` with its principal declared type
  `forall (beta >= sigma-id). sigma-id -> beta`. The expression-level
  regression also checks that elaboration constructs an outer flexible type
  abstraction whose bound and lambda parameter are both `sigma-id`, rather
  than accepting only an alpha-equivalent final type.
- Focused validation passes the three paper self-application cases and the
  three thesis-exact coercion-construction cases; `cabal build all` is
  warning-free and `git diff --check` passes.
- The full `cabal test` gate is not green in the current identity-refactor
  worktree: it completed 3332 examples with 163 failures. The paper
  self-application and bare-overloaded-method cases pass in that run; the
  remaining failures cluster in the wider identity-refactor's
  recursive-carrier, checked identity-finalization, returned-closure backend,
  deferred-evidence, and frozen-baseline coverage. This run is diagnostic
  evidence, not a completion claim for the wider worktree.

## 2026-07-14 - Typed Prelude cache boundary and construction-time eMLF closure

- `ProgramSourceUnit`, `LocatedProgramSourceUnit`, and package graph nodes now
  carry a hidden ordinary/builtin source origin. Only the Prelude owner smart
  constructors can create builtin provenance; an ordinary unit whose path is
  `<mlfp-prelude>` remains ordinary and cannot activate the cache.
- Cache eligibility additionally requires a module named `Prelude` with no
  imports, making the cached checker's empty prior-interface assumption an
  explicit construction invariant. Modified Prelude syntax is available only
  through a narrow test-support seam.
- The one-slot checked Prelude cache validates a spelling-sensitive structural
  snapshot of the complete `ResolvedSemanticModule`. It uses one owner-private
  descending identity supply for both ordinary and timed entrypoints and stores
  the `CheckedModule` together with its generated-identity extrema. Each client
  generator advances past those extrema on hit or miss. Changed syntax or
  binder spelling therefore misses without making the cached result depend on
  timing mode, batch configuration, or call order.
- Runtime tests retain `ProgramPackage`/`LocatedProgramPackage` ownership all
  the way into checking instead of flattening through `withPrelude`. In the
  focused post-change measurements, `rejects bare overloaded method use` takes
  0.07 seconds inside the already-built test binary, while the merged
  interpreter/LLVM/native row for the higher-kinded method over a parameterized
  data constructor takes 0.77 seconds versus 42.66 seconds across the former
  duplicate lanes. Runtime artifacts add Prelude only for an explicit Prelude
  import; this row therefore performs zero Prelude checks. Imported rows retain
  the provenance-bearing package boundary and share the process cache.
- Local method lowering specializes the complete constrained evidence head
  before applying method-local evidence. Nullary uses derive the substitution
  from the expected result; non-nullary calls infer it once from their call
  arguments and share it with constraint evidence construction. Direct
  producer annotations preserve the evidence binding's own forall identities,
  so the function occurrence constructs `InstApp` before the evidence
  application instead of leaking the method-local binder into root Gamma.
  Only method-local quantified binders may be substituted; enclosing class
  arguments are rigid, and duplicate matching local evidence is rejected as
  ambiguous.
- Scheme-aware term closure treats the scheme as the construction contract for
  an explicit type-abstraction spine. It aligns an existing prefix, creates
  only the missing suffix (including bounded binders), and permits temporary
  typecheck failure only when the term still contains an unresolved deferred
  reference. Deferred case handlers use this single construction path and then
  pass a strict environment-aware type/alpha-equivalence gate.
- The paper's Section 15.3.8 term is checked at the program boundary as the
  exact xMLF construction `Λ(β ≥ σ-id). λ(g : σ-id). (g[σ-id] g)[β]`, not only
  by comparing its final type. The valid paper term remains distinct from the
  deliberately rejected non-local recursive-type proxy described above.
- Every `EvidenceInfo` class head is now a rigid local assumption throughout
  resolved lowering, deferred finalization, and interpreter dispatch. Thus
  local `Eq a` cannot discharge `Eq Bool`; flexible substitution remains only
  in global instance-head selection and method-local quantified binders.
  Focused regressions cover zero-method prerequisites, constrained nullary
  methods, and the valid fallback to an exact global instance.

## 2026-07-15 - Paper bounded-instantiation backend parity

- Binding Raise permission now follows the paper's node colours exactly:
  a node whose own edge is rigid is restricted/orange and may be raised while
  retaining that flag; only a flex-bound node beneath a strict rigid ancestor
  is locked/red. Lower-bound scope repair keeps externally referenced
  restricted variables in its frontier instead of treating rigidity as local
  ownership. Edge-local witness filtering classifies nodes through the same
  `nodeKind` taxonomy over its canonical binding snapshot, rather than a
  second path walker that could mistake a nested restricted node for locked.
- Presolution validates lower-bound reachability against the prospective
  union-find quotient before publishing a merge. Binding-tree repair and the
  UF link are committed together only after that validation succeeds, so a
  merge cannot temporarily publish a bound whose newly reachable frontier is
  locked.
- Backend conversion now interprets application-like xMLF computations against
  the forall spine they eliminate. In particular, `N` (`InstElim`) uses the
  leading binder's explicit bound; only an unbounded binder supplies bottom.
  Sequential applications substitute each chosen argument before interpreting
  the next binder, so dependent bounds retain their identity-bearing context.
- LLVM lowering preserves the order of interleaved type and term applications
  such as `(f[T] x)[U] y`. When the inner call is statically inlined, the later
  application is pushed into its result expression. Caller-owned continuation
  references are copied into a local callee's lexical environment by identity,
  while the rest of the caller environment remains out of scope.
- The paper program `omega[N] id`, followed by specialization at `Bool`, is now
  a shared interpreter/LLVM/native parity row. Its focused parity run checks
  source checking, interpreter output, backend validation, LLVM assembly,
  object generation, native linking, and native output `true`.

## 2026-07-19 - Construction-owned application and finalization invariants

- Root elaboration now uses a typed construction plan: exact roots carry their
  complete prepared generalization, while ordinary roots carry only the
  requirement-owned construction scope and recompute their final scheme from
  the authoritative elaborated result. This prevents an outer root annotation
  from silently dropping application-owned `RaiseMerge` requirements.
- Application elaboration constructs its local and root `Gamma` environments
  from the validated argument endpoint. A polymorphic argument therefore uses
  the endpoint that owns the accepted constraint, rather than first building a
  source-forall term and repairing its type after elaboration.
- Deferred case obligations carry their `LoweredBindingIdentity` at first
  construction. Finalization validates that identity against the owning
  binding; the former optional owner and late attachment paths are gone.
- Exact-result preparation is represented by a closed sum whose constructors
  encode before/after completion ownership. Invalid combinations of packet
  identity, completion identity, and stage cannot be assembled independently.
- `InstApp` reduction now mirrors type-level instantiation: when an argument is
  alpha-equivalent to an explicit non-bottom bound, the term reducer eliminates
  that binder directly. The recursive Nat runtime regression therefore keeps
  its real bound-matching application instead of relying on a synthetic
  vacuous/bottom shape.

## 2026-07-22 - Identity-authoritative scheme closure

- Generalization finalization consumes an opaque binder capability built from
  the planner-ordered identities and their one-to-one reified bounds. Residual
  free identities are rejected with `SchemeFreeVars`; they are no longer
  synthesized into fresh outer foralls after reification.
- Scheme closure is checked by exact `TypeBinderIdentity`. Intermediate
  generalization may remain open only under explicit inherited or locally
  constructed Γ authority, while let publication accepts only identities in
  its installed ambient type scope.
- Prepared root closure, including the no-local and owner-certified paths, is
  closed outright after construction and again after source-identity
  projection. A certificate cannot authorize an ambient identity that the
  planner did not bind at the root.

## 2026-07-23 - Provenance-closed application endpoints and shared runtime artifacts

- Application elaboration may restore an opened graph topology to a checked
  occurrence's complete source `forall` only through a positive identity
  certificate: the checked occurrence must have that exact closed scheme, the
  projected topology must equal its opened body, every opened free reference
  must be one of the scheme's declared identities, and every restored
  declaration must actually occur. Wrong-identity and vacuous-forall cases
  fail closed. This constructs the paper's reduced self-application spine
  `g[σ-id] g` directly while preserving the explicit Hyp/elimination route for
  direct IO primitives.
- External bindings install their own lexical type-binder identity map while
  constraint generation internalizes each scheme. The map is binding-local
  and left-biased over inherited identities, so same-spelled binders in sibling
  schemes cannot cross-pair. Strict checked-source replay then has the exact
  `checked identity <- source node -> replay node` route needed for Prelude
  Functor and Applicative methods; it never reconstructs that route from
  spelling or quantifier position.
- Construction-Gamma projection distinguishes direct source ownership from
  expansion-only aliases and locally constructed closure identities. Both
  application and ordinary-root paths consume the same explicit five-authority
  interface, preventing an expanded alias from being published as if it were
  a source declaration. Deferred constructor and method heads likewise consume
  ordered `InstApp` spines and reject conflicts or excess applications before
  final term construction.
- Omega classifies `OpRaise` against the frozen source binding parent carried
  by `GaBindParents`, not the finalized replay representative whose rigidity
  may be the result of that operation. Paired regressions cover source-flex/
  final-rigid execution and source-rigid/final-flex skipping.
- Runtime parity artifacts now retain `LocatedProgramPackage` provenance,
  share one checked artifact across interpreter/LLVM/native execution, and
  prepare backend state lazily. A focused higher-kinded class-method row fell
  from 42.66 seconds across duplicate lanes to about 1.45 seconds for the
  merged row. A no-Prelude row performs zero Prelude builds, while two
  independent Prelude clients share one semantic Prelude build.

## 2026-08-06 - Exact returned-owner bounds and final eMLF construction gate

- Root closure now applies combined construction authorities in their actual
  order. An application certificate projects the Gamma emitted by the
  application; the returned result's `OwnerFinalConstruction` then projects
  the exact bound payload for a uniquely routed local binder before root/local
  ownership is partitioned. The planner still owns binder identity, order, and
  scheme body. Missing or duplicate construction-spine matches fail at that
  boundary.
- This replaces the rejected alternative of rewriting the checked lambda body
  to the root planner's more specialized shape. The owner-emitted bound is the
  principal bound already validated by the checked xMLF lambda; forcing the
  specialized root shape would require a function-arrow instantiation that
  xMLF does not provide.
- The seed-2147483646 case is frozen as `completes a returned polymorphic
  parameter before publishing its application owner`. Seed `2040442873` now
  also freezes cases 16, 44, and 91: an exact paper-`g g` body packet beneath
  an applied outer lambda; a ground application result completed before outer
  publication; and a paper-`g g` lambda returned through an application and
  transparent let. The last two are constructed downward: an independent
  transparent-let pass authorizes the exact result identity without replacing
  the graph-selected scheme, and administrative lambda completion follows the
  exact returned-result chain to one checked lambda owner. Seed `1435051581`
  case 89 is frozen as `completes a returned higher-rank application result
  before enclosing lambda Gamma`. A structured exact producer now treats every
  prepared-packet mention as positive nested-identity ownership evidence, but
  accepts completion authority only from the packet selected by that exact
  edge and only when all matching completed views equal the closed presolution
  projection. Open projections remain at their lexical owner. The matching
  body-consumer path preserves a specialized endpoint only when the private
  route's construction-operated declaration is the certificate's completed
  bound and exact xMLF instantiation of its operated declaration reaches that
  endpoint. Seed `1120133952` case 100 is frozen as `keeps a multi-use
  polymorphic let lexical through returned lambda owners`. The failure came
  from using the softened planning colours for provisional root reification
  while final reification inlined a reachable rigid variable from the original
  colours and thereby exposed an unplanned binder identity. `BinderPlanInput`
  now carries those unsoftened colours explicitly, and binder closure selects
  the dependencies exposed by each planned rigid inline before final
  reification. This is construction-time closure, not a final free-variable
  repair. The complete fixed annotation group passes 268/268, and 37 pinned
  generated seeds each pass 100 elaboration, xMLF typecheck, and erasure cases
  (3700 programs total).
- The slow-test repair remains effective in the final tree: the already-built
  test binary runs `rejects bare overloaded method use` in 0.16 seconds wall
  time and the merged interpreter/LLVM/native higher-kinded parity row in 0.75
  seconds wall time. `cabal build -j1 all`, the serialized 4011-example
  `cabal test -j1`, and `./scripts/thesis-conformance-gate.sh` all pass. These
  gates establish the supported compiler constructions, including the paper's
  Section 15.3.8 `g g` form; they are executable evidence, not a mechanized
  proof of every eMLF metatheorem.
