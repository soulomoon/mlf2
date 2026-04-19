# Revision history for mlf2

## Unreleased

### Changed
- Added the next `.mlfp` typeclass/module ergonomics slice. Program types now
  parse and carry class constraints on definitions, methods, and instance
  declarations; constrained values lower to hidden method evidence, schema
  instances such as `Eq a => Eq (Option a)` resolve recursively, and overlapping
  instance heads are rejected by unification. `deriving Eq` now generates
  constrained instances for parameterized ADTs, including recursive `List a`,
  and reports the first field type with missing Eq evidence. Imports now support
  qualified aliases such as `import Core as C;` and selected alias imports such
  as `import Core as C exposing (eq);`, with qualified names available in
  expressions, patterns, types, constraints, classes, constructors, and methods.
  The built-in Prelude now exports `Nat(..)`, `Option(..)`, `List(..)`, `Eq`,
  `eq`, `and`, and `id`. Review hardening now rejects local instances that
  overlap imported schemas, validates pattern annotations against matched and
  catch-all-only scrutinee types, rejects branches after constructor-local
  catch-all patterns, preserves method-level constraints when eager and
  deferred typeclass evidence is materialized, avoids duplicate unqualified
  instance matches for aliased imports whose instance heads do not qualify, and
  avoids decoding non-data `main` values or typed non-data constructor fields
  through the global ADT fallback.
  Validation:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
  (`97 examples, 0 failures`), `MLF.Program eMLF`
  (`58 examples, 0 failures`), and `cabal build all && cabal test`
  (`1646 examples, 0 failures`).
- Made `.mlfp` more usable as a source-language surface. The program parser
  now has located entrypoints and the checker/runner expose
  `ProgramDiagnostic` with file/line/column rendering and mechanically
  justified hints for high-value failures. Pattern matching now supports
  ordered nested constructor patterns, variables, wildcards, and pattern
  annotations, with reachability-aware rejection of duplicate or unreachable
  branches. The CLI prepends an explicit-import built-in `Prelude`, and closed
  ADT runtime values render with source constructor syntax instead of raw
  Church terms. Added `docs/mlfp-language-reference.md` and linked it from the
  README. Focused validation:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
  (`76 examples, 0 failures`), `MLF.Program eMLF`
  (`39 examples, 0 failures`), `Public surface` (`24 examples, 0 failures`),
  `cabal build all && cabal test` (`1625 examples, 0 failures`), and
  `git diff --check`.
- Fixed the thesis-conformance CI gate so colored Hspec summaries and Ruby
  3.2 YAML dates from GitHub Actions are parsed before enforcing matcher
  coverage. Validation:
  `cabal build all && cabal test` (`1609 examples, 0 failures, 7 pending`) and
  `./scripts/thesis-conformance-gate.sh`.
- Added `.mlfp` first-class-polymorphism parity at
  `test/programs/unified/first-class-polymorphism.mlfp`. `.mlfp` application
  elaboration now preserves a top-level polymorphic argument when the callee
  expects the full polymorphic scheme, and `ProgramSpec` now covers the
  user-facing eMLF surface through `.mlfp`. The parity rows are now table-driven,
  with a boundary matrix that locks deferred program-obligation behavior through
  eMLF inference: overloaded dispatch, inferred case scrutinees, first-class
  polymorphic constructor and pattern-bound values, partial overloaded method
  application, parameterized ADT instance recovery, and the existing negative
  guards for bare methods, missing instances, and duplicate instances. Deferred
  obligations lower to typed placeholders and resolve after eMLF elaboration,
  with rewritten terms typechecked before acceptance; ordinary ADT constructor
  applications, bare nullary constructor values, GADT-style indexed
  constructors, and existential constructors now use the deferred path while
  Church constructor definitions remain the runtime representation, and
  explicit/derived recursive `Eq Nat` coverage is restored. Constructor
  obligations carry expected-type seeds, constructor-local `forall` evidence,
  and runtime instantiation order, with ambiguous constructor evidence failing
  as `ProgramAmbiguousConstructorUse`. The former pending-success matrix has
  been folded into strict positive coverage.
  Validated with `MLF.Program eMLF`
  (`34 examples, 0 failures`),
  `MLF.Program` (`67 examples, 0 failures`), and
  `cabal build all && cabal test` (`1616 examples, 0 failures`).
- Hardened the unified `.mlfp` eMLF/xMLF contract. Executable bindings are now
  documented and guarded as `SurfaceExpr -> runPipelineElabWithEnv -> xMLF
  typecheck -> normalize/run`, `runPipelineElabChecked` is documented as a
  compatibility alias for the checker-authoritative shared pipeline, and
  constructor declarations now fail early when their result type is not headed
  by the owning data type with the required arity.
- Repaired the `.mlfp` ProgramSpec regression cluster while keeping `.mlfp`
  on the shared old eMLF/typecheck route: no new surface forms, no direct
  `.mlfp -> ElabTerm` fallback, no permissive `EUnroll`, and no broad
  `TypeCheck` weakening. The recursive-ADT corpus, CLI/helper diagnostics,
  module-ordering case, and unified `.mlfp` integration checks are green.
  Validation: `MLF.Program execution corpus` (`10 examples, 0 failures`),
  `MLF.Program` (`46 examples, 0 failures`), direct public probes returning
  `true` for `authoritative-overloaded-method.mlfp` and `1` for
  `authoritative-case-analysis.mlfp`, plus `cabal build all && cabal test`
  (`1595 examples, 0 failures`).
- Added a new public recursive-ADT program surface via `MLF.Program`. The repo
  now parses, pretty-prints, checks, and evaluates module-oriented recursive
  algebraic data type programs with `data` declarations, constructor-pattern
  `case`, recursive GADT-style constructor result types, existential
  constructors via `forall`, single-parameter typeclasses / instances, and the
  initial `deriving Eq` lane. Added the Phase-0 syntax/corpus freeze
  (`docs/plans/2026-04-13-recursive-adt-syntax-freeze.md`), executable corpus
  fixtures under `test/programs/recursive-adt/`, and `ProgramSpec` regression
  coverage for parse/pretty roundtrips, execution, diagnostics, and a
  representative recursive-Nat baseline.
- Wired the `mlf2` executable to run recursive-ADT program files directly via
  `cabal run mlf2 -- run-program <file.mlfp>`, and expanded the sample corpus
  with `recursive-list-tail.mlfp`, `recursive-tree-deriving.mlfp`, and
  `abstract-module-use.mlfp`, followed by
  `complex-recursive-program.mlfp` for composed recursive tree traversals.
  `ProgramSpec` now roundtrips/runs all ten sample programs and verifies the
  CLI helper path. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program"'`
  (`46 examples, 0 failures`),
  `cabal run mlf2 -- run-program test/programs/recursive-adt/plain-recursive-nat.mlfp`
  (`true`), and `cabal build all && cabal test`
  (`1595 examples, 0 failures`).
- The repo can now honestly claim fully automatic unannotated iso-recursive
  inference across its representative family matrix inside the inherited
  explicit-only / iso-recursive / non-equi-recursive / no-fallback /
  no-second-interface boundary. Added
  `docs/plans/2026-04-13-fully-automatic-unannotated-iso-recursive-inference-completion-refresh.md`,
  marked the April 11 stop/Phase-4-closed fully-automatic-unannotated plan
  docs as predecessor evidence only, aligned `implementation_notes.md`, and
  renamed the stale higher-order characterization in `PipelineSpec` so current
  repo truth no longer says the stronger unannotated lane is still closed.
  Validated with focused matcher reruns for `URI-R2-C1 unannotated carrier`
  (`2 examples, 0 failures`), `URI-R2-C1 uniqueness reject`
  (`1 example, 0 failures`), `URI-R2-C1 reconstruction`
  (`4 examples, 0 failures`), the preserved unannotated contrast
  (`1 example, 0 failures`),
  `URI-R2-C1 owner-sensitive non-local transparent mediation`
  (`24 examples, 0 failures`),
  `URI-R2-C1 combined wrapper`
  (`24 examples, 0 failures`),
  `item-4 edge cases`
  (`85 examples, 0 failures`), plus `git diff --check`,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test`.
- Extended the retained-child exact-output research harness across the next
  same-wrapper nested-`forall` mixed stacked transparent-mediator family
  without widening production logic. `SameLaneRetainedChildRepresentativeGapSpec`
  now locks both mixed direct/let-aliased orders and both decuple alias-shell
  companions on both authoritative entrypoints, so the retained-child
  exact-output surface now covers the mixed stacked lane in addition to the
  already-locked direct stacked transparent and stacked let-aliased families.
  Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`44 examples, 0 failures`) and `cabal build all && cabal test`
  (`1539 examples, 0 failures`).
- Closed the retained-child exact-output asymmetry for the same-wrapper
  nested-`forall` stacked let-aliased transparent-mediator lane without
  widening elaboration heuristics. `MLF.Elab.Run.Pipeline` now pre-closes
  retained-child authoritative candidates against the root scheme before the
  existing preserved-child replay step, so already-recursive direct packets no
  longer lose the first closure pass merely because their pre-preserve term is
  type-closed. `SameLaneRetainedChildRepresentativeGapSpec` now also locks
  both the direct stacked let-aliased packet and its decuple alias-shell
  companion on both authoritative entrypoints. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`36 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-wrapper nested-forall plus"'`
  (`22 examples, 0 failures`), and `cabal build all && cabal test`
  (`1531 examples, 0 failures`).
- Extended the retained-child exact-output research harness across the next
  same-wrapper nested-`forall` stacked transparent-mediator family without
  widening production logic. `SameLaneRetainedChildRepresentativeGapSpec` now
  locks both the direct stacked transparent-mediator packet and its decuple
  alias-shell companion on both authoritative entrypoints, so that broader
  retained-child parity no longer depends on the type-only `PipelineSpec`
  rows for those two already-admitted packets. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`32 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-wrapper nested-forall plus"'`
  (`22 examples, 0 failures`), and `cabal build all && cabal test`
  (`1527 examples, 0 failures`).
- Preserved direct clear-boundary retained-child authoritative output instead
  of repairing only the alias-frame variants. `MLF.Elab.TermClosure` now
  recognizes the direct same-wrapper nested-`forall` transparent-mediator
  packet as a retained-child clear-boundary repair site, so authoritative
  replay keeps the full recursive binder shape when the RHS already has a
  recursive witness. `SameLaneRetainedChildRepresentativeGapSpec` now locks
  both the direct transparent-mediator packet and its decuple alias-shell
  companion on both authoritative entrypoints. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`28 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-wrapper nested-forall plus"'`
  (`22 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`85 examples, 0 failures`), and `cabal build all && cabal test`
  (`1523 examples, 0 failures`).
- Extended the retained-child exact-output research harness to cover the
  same-wrapper nested-`forall` alias-frame family in addition to the direct
  alias-frame chain. `SameLaneRetainedChildRepresentativeGapSpec` now locks
  the single alias-frame and decuple alias-frame same-wrapper nested-`forall`
  packets on both authoritative entrypoints, so exact retained-child public
  output no longer relies on implicit parity for those two already-admitted
  packets. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`24 examples, 0 failures`).
- Implemented the next plan-aligned shared recursive-candidate selection seam
  instead of keeping duplicate local selectors. `MLF.Elab.Run.ResultType.Util`
  now owns a reusable `CandidateSelection` helper with both plain `Eq` and
  custom-equality selection, `MLF.Elab.Run.ResultType.Fallback.Core` now uses
  that shared selector for base-target / retained-child cardinality handling,
  and `MLF.Elab.Elaborate.Algebra` now uses the same helper for the
  alpha-equivalent helper/direct structural recovery lane. The private
  internal library now exposes `ResultType.Util` so the shared selection law
  is testable directly, and the retained-child exact-output research harness
  now locks `sameLaneDecupleAliasFrameClearBoundaryExpr` on both authoritative
  entrypoints. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "Shared candidate selection"'`
  (`3 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child representative-gap probes"'`
  (`20 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`85 examples, 0 failures`), and `cabal build all && cabal test`
  (`1515 examples, 0 failures`).
- Extended the bounded same-wrapper nested-`forall` transparent-mediator
  frontier without widening production logic. `PipelineSpec` now locks the
  previously probe-only plain stacked transparent `Bool` row, both plain mixed
  stacked wrapper orders on `Int`/`Bool`, and the reverse mixed
  let-aliased/direct stacked decuple-alias-chain rows. That closes the
  remaining asymmetry in the current same-wrapper stacked transparent-eta
  family, so the broader authoritative parity surface no longer depends on
  REPL-only evidence for those packets. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-wrapper nested-forall plus"'`
  (`22 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
  (`74 examples, 0 failures`), and `cabal build all && cabal test`
  (`1496 examples, 0 failures`).
- Turned the next owner-sensitive non-local unannotated `URI-R2-C1`
  transparent-mediator family from REPL-only evidence into durable
  `PipelineSpec` coverage. Current-head tests now lock the packets that return
  `hold = wrap f` directly on both authoritative entrypoints for both `Int`
  and `Bool`, across direct transparent wrappers, let-aliased transparent
  wrappers, stacked transparent wrappers, stacked let-aliased transparent
  wrappers, mixed direct/let-aliased stacked wrappers, and mixed
  let-aliased/direct stacked wrappers. The same family is now also locked
  through a decuple owner-local alias chain for all six wrapper shapes on both
  carriers. No production-code change was needed in this slice; the current
  `Algebra` and pipeline behavior already admitted these packets, so the change
  records that broader non-local unannotated route as a reviewable regression
  surface instead of leaving it implicit. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 owner-sensitive non-local transparent mediation"'`
  (`24 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`73 examples, 0 failures`), and `cabal build all && cabal test`
  (`1477 examples, 0 failures`).
- Narrowed unannotated recursive let recovery around a shared structural
  candidate selector in `MLF.Elab.Elaborate.Algebra` instead of branch-order
  precedence. Let-generalization now combines returned-helper and direct
  recursive-carrier candidates through
  `StructuralRecursiveCandidateSelection`, de-duplicates alpha-equivalent
  proofs, and fails closed when distinct structural candidates survive on the
  same self-recursive RHS. The selector is explicitly gated to true
  self-recursive lets (`annContainsVar v rhsAnn`), so non-recursive alias
  wrapper bindings on the owner-sensitive `P2` route keep their earlier
  scheme flow instead of being reclassified by accident. Added a focused
  `PipelineSpec` ambiguity regression for the direct-self-app plus
  returned-helper multi-cluster packet, which now stays fail-closed on both
  authoritative entrypoints. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 ambiguity reject"'`
  (`1 example, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "P2 representative-support harness"'`
  (`10 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`49 examples, 0 failures`), and `cabal build all && cabal test`
  (`1453 examples, 0 failures`).
- Expanded the durable `P2` non-local representative-support harness beyond
  the exact `C1` packet and the route-pure `Bool` variant. The repo now locks
  owner-sensitive non-local wrapper packets on both authoritative
  entrypoints for `Int` and `Bool`: direct identity-consumer propagation,
  transparent eta mediators, let-aliased transparent eta mediators, stacked
  transparent eta mediators, stacked let-aliased transparent eta mediators,
  both mixed stacked transparent-mediator orders, and the next representative
  combined-wrapper packets around the same non-local recursive source. Current
  representative support now also locks the transparent, let-aliased
  transparent, stacked let-aliased transparent, and mixed
  direct/let-aliased stacked transparent-mediator combined-wrapper shapes on
  both carriers and both authoritative entrypoints. This turns that broader
  `P2` route from probe-only evidence into a wider reviewable regression
  surface. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "P2 representative-support harness"'`
  (`24 examples, 0 failures`) and `cabal build all && cabal test`
  (`1502 examples, 0 failures`).
- Extended the unannotated `URI-R2-C1` mediator frontier with durable
  combined-wrapper coverage instead of another packet-local heuristic. The
  existing recovery path already admits the next owner-sensitive packets where
  an identity-consumer wrapper is followed by a transparent eta mediator, a
  let-aliased transparent eta mediator, stacked transparent eta mediators,
  stacked let-aliased transparent eta mediators, and both mixed stacked
  wrapper orders. `PipelineSpec` now locks those direct `Int`/`Bool` packets
  on both authoritative entrypoints, plus the decuple owner-local alias-chain
  variants for the single-wrapper, same-order stacked, and mixed stacked
  shapes, so the broader unannotated lane no longer relies on REPL-only
  evidence for those combined packets. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 combined wrapper"'`
  (`24 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`85 examples, 0 failures`), and `cabal build all && cabal test`
  (`1489 examples, 0 failures`).
- Closed the next authoritative parity gap in the broader same-wrapper
  mediation frontier: stacked let-aliased transparent eta mediators now
  recover the same structural wrapper scheme as their direct transparent
  counterparts instead of keeping a malformed generalized binder order that
  died in Phase 7 with `TCTypeAbsVarInScope`. The term-level transparent
  mediator recovery now looks through alias-only `let mid = h` frames when it
  collects eta parameters, checks the structural recovery candidate against
  the pre-let environment instead of the malformed self-binding, and treats
  forward binder references in the generalized scheme as a recovery trigger in
  the same narrow lane. This makes the combined
  `same-wrapper nested-forall + stacked let-aliased transparent eta mediator`
  family stay recursive on both authoritative pipeline entrypoints for both
  `Int` and `Bool` carriers, and also through a decuple owner-local alias
  chain for both carriers. The sibling let-aliased direct-wrapper lane is now
  also locked as non-poisoning for both carriers, so a second aliased
  mediator in scope no longer forces `wrap2 k` back onto a stale polymorphic
  shell, including through a decuple owner-local alias chain. The mixed
  direct/let-aliased stacked lane is now also locked through a decuple
  owner-local alias chain for both carriers. Added ten focused
  `PipelineSpec` regressions for the direct, sibling-direct, mixed-stack, and
  decuple-chain `Int`/`Bool` packets. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "transparent eta mediator"'`
  (`18 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'`
  (`5 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
  (`67 examples, 0 failures`), and `cabal build all && cabal test`
  (`1420 examples, 0 failures`).
- Closed the next authoritative parity gap in the broader same-wrapper
  mediation frontier: `MLF.Elab.Elaborate.Algebra` now treats transparent
  eta-mediated applications with a recursive variable argument as alias-shaped
  at the binding boundary, so `let hold = wrap k` inherits `k`'s recovered
  recursive scheme/substitution instead of keeping the mediator app's stale
  polymorphic shell. The app elaborator also skips replay-only instantiation
  work when the transparent mediator will be bypassed to the already-typed
  recursive argument. This makes the combined
  `same-wrapper nested-forall + transparent eta mediator` family stay
  recursive on both authoritative pipeline entrypoints for both `Int` and
  `Bool` carriers instead of failing first in Phase 6 with
  `PhiTranslatabilityError` and then in Phase 7 with `TCArgumentMismatch`.
  The same family now also stays recursive through a decuple owner-local alias
  chain for both the direct transparent eta mediator and the let-aliased
  transparent eta mediator variant, across both `Int` and `Bool` carriers.
  Added focused `PipelineSpec` regressions for both carrier variants plus the
  four deeper decuple alias-chain packets.
  Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-wrapper nested-forall plus transparent eta mediator"'`
  (`6 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
  (`55 examples, 0 failures`), and `cabal build all && cabal test`
  (`1408 examples, 0 failures`).
- Closed the authoritative parity gap for the combined same-wrapper
  nested-`forall` plus owner-local alias packet: `MLF.Elab.Elaborate.Algebra`
  now recovers direct instantiation for single-binder identity-like
  polymorphic wrappers when authoritative replay translation is missing but
  the application argument already carries a contractive recursive witness.
  This makes
  `let id = \z.z in let k = id (\x:μα.α→Int. x) in let hold = k in let u = (\y -> y) hold in u`
  stay recursive on both authoritative pipeline entrypoints instead of
  failing in Phase 6 with `PhiTranslatabilityError`, while the fallback
  surface was already recursive. Added new `PipelineSpec` and
  `Research.P5ClearBoundarySpec` regressions for that combined packet, and
  locked `sameLaneDecupleAliasFrameClearBoundaryExpr` as an admitted
  broader-positive clear-boundary row on both fallback and authoritative
  surfaces. The same combined nested-`forall` family is now also durably
  covered through a decuple owner-local alias chain, not just a single
  alias-frame packet. Validated with
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
  (`28 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
  (`49 examples, 0 failures`), and `cabal build all && cabal test`
  (`1402 examples, 0 failures`).
- Refactored result-type fallback base-target selection around an explicit
  admission model: `MLF.Elab.Run.ResultType.Fallback.Core` now classifies
  unique root-base, unique inst-arg-base, and scheme-alias/base-like packets
  through a named `BaseTargetAdmission` proof instead of inferring them from
  separate local/non-local singleton booleans. This locks the newly admitted
  unique non-local single-base and inst-arg-only singleton-base families into
  the current truth while preserving the non-local multi-inst and inst-arg
  multi-base contrasts as ambiguity rejects. The retained-child same-lane
  fallback lane now also rejects sibling-candidate, intra-target, and mixed
  retained-child/base-target ambiguity explicitly instead of resolving them by
  traversal order or family priority, while keeping inst-root evidence as
  fallback only when the child target has no direct recursive witness. Added
  test-first `PipelineSpec` source-guard coverage for the classifier contract,
  runtime retained-child ambiguity regressions, the new mixed-family
  ambiguity regression, and synced the live unannotated-family planning
  artifacts to the new classification. Validated
  with `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization"'`
  (`46 examples, 0 failures`),
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "item-4 edge cases"'`
  (`24 examples, 0 failures`), and `cabal build all && cabal test`
  (`1393 examples, 0 failures`).
- Extended the automatic iso-recursive inference surface through mediated
  unannotated same-wrapper nested-`forall` lanes: let-scheme elaboration now
  reconstructs a contractive recursive carrier from the lambda body result
  when an identity-like wrapper hides the recursive witness at the mediated
  node, so `let id = \z.z in let f = id (\x -> let _ = f x in 0/true) in (\y
  -> y) f` now succeeds on both authoritative pipeline entrypoints while the
  witnessless `let f = id f in f` lane stays fail-closed. Added focused
  `PipelineSpec` coverage for direct, alias-reconstruction, and nested-`forall`
  `Bool` packets plus the new mediated `Int`/`Bool` positives.
- Broadened unannotated recursive let inference for returned helper fixed
  points: when let-generalization sees a nested self-application used as the
  argument to an outer recursive call, elaboration now prefers the inferred
  self-codomain carrier over the bottom-collapsed alias placeholder and
  aligns rolled lambdas against the unfolded `μ` body. This admits
  `let f = \x -> let g = \y -> f (g y) in g in f` as
  `(μa. a -> a) -> (μa. a -> a)` on both authoritative pipeline entrypoints
  instead of failing at Phase 7. The authoritative retained-child
  preservation path now also re-closes preserved terms against the root
  scheme, which keeps the alias-frame clear-boundary family stable after the
  broader unannotated rewrites. Added a positive `PipelineSpec` regression
  for the returned-helper fixed-point lane. Validated with
  `cabal build all && cabal test` (`1392 examples, 0 failures`).
- Refreshed the representative-family readiness ledger so the repo can now
  honestly claim general automatic iso-recursive inference inside its
  inherited explicit-only production boundary: added a direct
  `P2` representative-support harness covering both the exact `C1` non-local
  `Int` packet and a second route-pure non-local `Bool` packet on
  `runPipelineElab` / `runPipelineElabChecked`, published superseding
  current-state positive-family and repo-level readiness artifacts under
  `docs/plans/2026-04-11-*`, and synced `implementation_notes.md` / `TODO.md`
  to the refreshed current ledger. Validated with
  `./scripts/thesis-conformance-gate.sh` and
  `cabal build all && cabal test` (`1371 examples, 0 failures`).
- Closed the P5 broader-positive enactment family on merged `ea8db76`: the
  enactment contract froze in `round-206`, milestone-3 reached the merged
  nonuple frontier in `round-220`, and the new canonical closeout records the
  selected same-wrapper nested-`forall` packet plus
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr` as the current
  broader-positive frontier on both authoritative entrypoints while keeping
  `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only,
  `sameLaneDecupleAliasFrameClearBoundaryExpr` and deeper alias shells
  closed, and the repo-facing notes synced without adding a new
  thesis-deviation entry.
- Updated the supported compiler lane from GHC 9.12.2 / `base-4.21` to GHC
  9.14.1 / `base-4.22` across Cabal metadata, the local `cabal.project` pin,
  the GitHub Actions matrix, README guidance, TODO tracking, and current
  orchestrator implementer guidance.
- Documentation hygiene: audited all 47 `{- Note [...] -}` blocks across `src/` for stale references after round-163 module splits, added design-rationale Notes to 6 new submodules that lacked them (`Normalize.Internal`, `Reify.Type.Core`, `Plan.Env`, `Plan.Generalize`, `Plan.ReifyStep`, `Interpret.Internal`), and documented the remaining 4 round-163 module splits in `implementation_notes.md`.
- Documentation follow-up: added `{- Note [Scheme finalization] -}` to `Plan.Finalize` and `{- Note [Generalization planning context] -}` to `Plan.Context`, completing design-rationale coverage for all large `Constraint.Presolution.Plan` submodules.
- Completed CI test-matrix and failure-repair campaign (rounds 156–159): froze a bounded GitHub Actions matrix (ubuntu-latest / GHC 9.12.2, Windows excluded while thesis-conformance gates remain Unix-only), repaired the thesis-conformance baseline by fixing 18 stale code-anchor paths and converting 262 absolute paths to repo-relative, split the workflow into two matrix-parameterized jobs (`build-and-test` and `thesis-conformance`), and documented the CI matrix scope, runner boundaries, and authoritative verification commands in README.md, TODO.md, and CHANGELOG.md. Validated with `cabal build all && cabal test` (1176+ examples, 0 failures) and `./scripts/thesis-conformance-gate.sh` (PASS).
- Implemented and hardened automatic iso-recursive type inference end-to-end: the constraint solver detects cycles and automatically introduces `TyMu` nodes, reification produces `TMu` types, elaboration emits `ERoll`/`EUnroll` coercions, and Phase 7 type-checks and reduces recursive types including roll/unroll steps. An initial implementation in round-144 was followed by a gap-fix campaign (rounds 146-149) that addressed four specific robustness gaps: witness normalization for TyMu nodes, alias-bounds resolution for recursive types, ELet fixpoint reduction for recursive let-bindings, and result-type fallback opening for non-local recursive reconstruction. Documented known remaining limitations in `implementation_notes.md`. This is an extension beyond the core thesis (which assumes acyclic constraint graphs), documented in `docs/thesis-deviations.yaml` as `DEV-AUTO-ISO-RECURSIVE`. Validated with 1176 examples, 0 failures.
- Resolved non-local proxy `PhiTranslatabilityError` at pipeline entrypoints: `reifyInst` TyMu 0-binder fallback (round 152) and `OpRaise` bind-parent μ-guard (round 153) clear the two crash sites so that non-local proxy wrappers now reach type checking. Survey of `ElaborationSpec` `PhiTranslatabilityError` sites (round 154) confirmed all are legitimate untranslatable cases. Validated with 1176 examples, 0 failures.
- Completed the bounded same-lane retained-child public-output continuity vs
  `non-cyclic-graph` successor loop through accepted rounds `round-094`
  through `round-098`, ending with the exact-pocket outcome
  `blocker debt remains within the current architecture`; then scaffolded a
  fresh repo-level settlement-and-implementation control plane under
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
  so the same roadmap family can continue directly into production
  implementation once the global settlement gate keeps
  `non-cyclic-graph`.
- Completed the bounded same-lane retained-child stable-visible-persistence
  successor loop through accepted rounds `round-089` through `round-093`,
  clearing the exact Phase 6 elaboration breakpoint, freezing the exact
  authoritative public-output collapse to `TForall "a" Nothing (TVar "a")`,
  classifying the frozen pocket as
  `admitted but not reconstruction-visible / blocker debt`, and recording the
  bounded successor decision `blocker debt remains within the current
  architecture`; then scaffolded a fresh successor control plane under
  `2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap`
  for the next exact-pocket gate.
- Migrated the repo-local orchestrator to revisioned roadmap bundles under
  `orchestrator/roadmaps/<roadmap_id>/rev-###/`: live controller state now
  resolves the authoritative roadmap through `orchestrator/state.json`
  `roadmap_id` / `roadmap_revision` / `roadmap_dir`, completed rounds now
  point at stable roadmap bundles plus per-round `state-snapshot.json`
  files, early pre-review-record rounds now carry migration-generated
  roadmap-provenance `review-record.json` placeholders, and the open
  `round-091` same-lane retained-child loop remains resumable on item `3`
  under the new locator contract.
- Completed the strategic general automatic iso-recursive inference control
  plane through accepted rounds `round-082` through `round-088`, selecting
  `continue within the current architecture` as the authoritative item-7
  outcome; then refreshed the live top-level `orchestrator/` in place for one
  bounded same-lane retained-child `stable visible persistence` successor
  gate, preserving rounds `001` through `088` as predecessor evidence and
  keeping the v2 retry contract active.
- Completed the remaining post-`L2` `boundVarTarget` successor lane through
  accepted rounds `round-078` through `round-081`: `N11` froze the exact
  same-lane local `TypeRef` retained-child `boundVarTarget -> targetC`
  packet, `N12` landed the bounded `sameLaneLocalRetainedChildTarget` proof
  slice in `MLF.Elab.Run.ResultType.Fallback` plus focused
  `PipelineSpec` coverage, `N13` reverified that exact packet with fresh
  focused/full-gate evidence (`20` / `1141` examples, `0 failures`), and
  `N14` finalized the bounded next-step token `continue-bounded`; at that
  stage the live `orchestrator/roadmap.md` had no pending successor item, so
  any further work required a separate roadmap amendment/update.
- Accepted `round-077` `N10` as the reopened-loop safety and acceptance
  contract for the selected `boundVarTarget` planning subject: the live
  successor control plane now records the authoritative outcome
  `boundVarTarget-safety-acceptance-contract-established`, preserves the
  earlier `baseTarget` selection, earlier `N3` contract, exact accepted
  non-local `baseTarget -> baseC` packet, and repaired-queue retained-child
  packet as predecessor evidence only without widening the inherited
  explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary, and updates the live roadmap so
  `N11` is the next bounded docs-only exact-target-bind item.
- Accepted `round-076` `N9` as the thesis-backed next live-subject selection
  inside the accepted `N8 = reopen-planning-only-successor-lane`: the live
  successor control plane now records the authoritative outcome
  `boundVarTarget-planning-subject-selected`, preserves the earlier
  `baseTarget` selection plus the exact accepted non-local
  `baseTarget -> baseC` packet as predecessor evidence only without widening
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary, and updates the live roadmap so
  `N10` is the next bounded docs-only safety/acceptance-contract item.
- Accepted `round-075` `N8` as the separate post-`N7` roadmap-amendment
  authority gate for the exact accepted non-local `baseTarget -> baseC`
  evidence chain: the live successor control plane now records the
  authoritative outcome `reopen-planning-only-successor-lane`, preserves that
  exact packet as predecessor evidence only without widening the inherited
  explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary, and updates the live roadmap so
  `N9` became the next bounded docs-only next live-subject selection item.
- Accepted `round-074` `N7` as the bounded closure / next-cycle decision gate
  for the exact accepted non-local `baseTarget -> baseC` evidence chain: the
  live successor control plane now records the authoritative reopened-loop
  result `continue-bounded`, preserves that exact packet as the one current
  bounded verified slice without widening the inherited explicit-only /
  non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
  boundary, and updates the live roadmap so a separate post-`N7` roadmap
  amendment / update (`N8`) is required before any new bounded cycle can
  begin.
- Accepted `round-073` `N6` as the bounded verification/evidence
  consolidation gate for the exact accepted `N5` non-local
  `baseTarget -> baseC` proof slice: the live successor control plane now
  records read-only `MLF.Elab.Run.ResultType.Fallback` / `PipelineSpec`
  anchors, fresh focused `ARI-C1` and full repo gate evidence (`20` / `1141`
  examples, `0 failures`), preserves `N3` / `N4` / `N5` continuity without
  widening, and updates the live roadmap so `N7` is the next bounded
  decision/closure item.
- Accepted `round-072` `N5` as the bounded implementation slice for the exact
  `N4`-frozen non-local `baseTarget -> baseC` packet: the live successor
  control plane now records the explicit
  `rootNonLocalSchemeAliasBaseLike` proof and dedicated same-lane `targetC`
  arm in `MLF.Elab.Run.ResultType.Fallback`, keeps the accepted local lanes
  unchanged, refreshes the focused `PipelineSpec` behavior/source guards, and
  updates the live roadmap so `N6` is the next bounded verification/evidence
  item.
- Accepted `round-071` `N4` as the exact bounded target bind for the selected
  preserved generic scheme-alias / base-like `baseTarget` subject: the live
  successor control plane now freezes the non-local generic scheme-root
  alias-bound / base-like `baseTarget -> baseC` packet in
  `MLF.Elab.Run.ResultType.Fallback` as the only in-scope target, together
  with its same-lane generic `targetC` use and corresponding
  `schemeAliasBaseLikeFallback False` regression anchor, while updating the
  live roadmap so `N5` is the next bounded slice stage.
- Accepted `round-070` `N3` as the reopened-loop safety and acceptance
  contract for the selected preserved generic scheme-alias / base-like
  `baseTarget` planning subject: the live successor control plane now freezes
  verifier-checkable invariant axes and no-go conditions for alias-bound
  ownership, inverse-translation-safe bound inlining, and binding-flag
  reconstruction, while updating the live roadmap so `N4` is the next bounded
  exact-target bind item.
- Accepted `round-069` `N2` as the thesis-backed next live-subject selection
  inside the accepted `N1 = reopen-planning-only` lane: the live successor
  control plane now fixes the preserved generic scheme-alias / base-like
  `baseTarget` route as the only selected docs-first alias-bound /
  bound-inlining / binding-structure planning subject, keeps repaired
  `URI-R2-C1` closed as predecessor evidence, and updates the live roadmap so
  `N3` is the next bounded safety-contract item.
- Accepted `round-068` `N1` as a docs-only post-`L2` roadmap-amendment
  authority gate: the live successor control plane now records
  `reopen-planning-only`, keeps the repaired `URI-R2-C1` queue closed as
  predecessor evidence, preserves the inherited explicit-only /
  non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
  boundary, makes the preserved generic scheme-alias / base-like `baseTarget`
  route admissible for later `N2` selection only, and updates the live
  roadmap so `N2` is the next bounded planning item.
- Closed the repaired `URI-R2-C1` continue-bounded queue through accepted
  `L1` fail-closed and accepted `L2 = stop-blocked`, then scaffolded a fresh
  post-`L2` next-loop packet under
  `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/`; the new
  mechanism table / orchestrator prompt / JSONL log treat any further progress
  toward automatic iso-recursive inference as blocked behind a separate roadmap
  amendment plus a fresh selection.
- Completed the bounded `K1` bind round in accepted `round-062`: the live control plane now freezes the next bounded `K2` implementation slice to the repaired local empty-candidate / no-inst-arg scheme-alias/base-like `baseTarget -> baseC` lane and its same-lane `targetC` use in `MLF.Elab.Run.ResultType.Fallback`, with future ownership limited to `Fallback.hs` and `PipelineSpec.hs`; the live roadmap now marks `K1` done and carries one new pending successor implementation item `K2`.
- Completed the bounded `J` cycle through accepted rounds `round-058` through `round-061`: `J1` bound the local inst-arg-only singleton-base lane, `J2` landed that bounded `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC` slice in `MLF.Elab.Run.ResultType.Fallback` with focused `PipelineSpec` coverage and a passing full repo gate, `J3` reverified that exact lane under fresh focused/full-gate evidence, and `J4` finalized the bounded next-step token `continue-bounded`; the live roadmap now marks `J1` through `J4` done and carries one new pending successor bind item `K1`.
- Completed the bounded `H` cycle through accepted rounds `round-050` through `round-053`: `H1` bound the remaining local-binding `instArgRootMultiBase` lane, `H2` landed the bounded `rootLocalInstArgMultiBase` / `targetC -> rootFinal` slice in `MLF.Elab.Run.ResultType.Fallback` with focused `PipelineSpec` coverage and a passing full repo gate, `H3` reverified that exact lane under fresh focused/full-gate evidence, and `H4` finalized the bounded next-step token `continue-bounded`; the live roadmap now marks `H1` through `H4` done and carries one new pending successor bind item `I1`.
- Cleaned the completed `G`-cycle control-plane residue, archived the finished continue-bounded orchestrator-run task packet, restored missing historical `round-047` `selection.md` / `plan.md` artifacts in the main checkout, and refreshed the live top-level `orchestrator/` for one new bounded `H1` through `H4` cycle rooted in accepted `G4 = continue-bounded`, with the next bounded target family fixed to the remaining local-binding `instArgRootMultiBase` lane.
- Refreshed the live top-level `orchestrator/` after the accepted initial successor cycle: the control plane now targets one new bounded non-widening follow-on cycle (`C1` through `C4`) rooted in the accepted `continue-bounded` result, preserves rounds `001` through `033` as historical evidence, and resets `orchestrator/state.json` to idle `select-task` with `last_completed_round: "round-033"`.
- Completed the live unannotated iso-recursive successor orchestration through accepted rounds `round-028` through `round-033`: `U1` bound the inherited baseline, `U2` narrowed provenance-stable authority, `U3` refuted uniqueness/owner stability without heuristic ranking, `U4` refuted constructor-directed / acyclic / terminating admissibility, `U5` landed one bounded `ResultType.Fallback` hardening slice with focused `PipelineSpec` coverage and a passing full repo gate, and terminal `U6` finalized the bounded next-step token `continue-bounded`; `orchestrator/state.json` now rests at `stage: "done"` with `last_completed_round: "round-033"`.
- Refreshed the live top-level `orchestrator/` again after the accepted `URI-R2-C1` replay repair track: the control plane now targets a bounded successor campaign toward unannotated iso-recursive solver/pipeline inference, starts from repaired `URI-R2-C1`, preserves rounds `001` through `027` as historical evidence, and resets `orchestrator/state.json` to idle `select-task` for successor round `round-028`.
- Completed the live `URI-R2-C1` replay repair-track successor orchestration through accepted rounds `round-024` through `round-027`: `R1` finalized the localized reproduction, `R2` finalized the bounded `InstBot` repair on authoritative attempt `3`, `R3` finalized locked replay-path verification, and terminal `R4` finalized the bounded outcome `repair-accepted`; the live roadmap now marks all `R1` through `R4` items `done`.
- Refreshed the live top-level `orchestrator/` again after the accepted `reopen-repair-track`: the control plane now targets a bounded `URI-R2-C1` replay repair track (`R1` through `R4`) rooted in `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`, while preserving rounds `001` through `023` as historical evidence and resetting `orchestrator/state.json` to idle `select-task`.
- Completed the live `URI-R2-C1` replay root-cause successor orchestration through accepted rounds `round-020` through `round-023`: `D1`, `D2`, and `D3` all finalized as `pass`, `D4` finalized as `reopen-repair-track`, `orchestrator/roadmap.md` now marks all successor items `done`, and `orchestrator/state.json` is back at terminal `stage: "done"` with `last_completed_round: "round-023"`.
- Refreshed the live top-level `orchestrator/` again after the completed prototype-evidence hard-stop: the control plane now targets a bounded `URI-R2-C1` `P2` replay root-cause successor track (`D1` through `D4`) rooted in `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`, while preserving rounds `001` through `019` as historical evidence.
- Amended the live `URI-R2-C1` top-level `orchestrator/` to `contract_version: 2`: future `P1` through `P3` rounds now use an internal retry subloop with explicit `attempt_verdict`/`stage_action` review outcomes, repo-local retry-state and artifact rules, and immutable per-attempt review history, while historical rounds `round-016` through `round-019` remain preserved as v1 evidence.
- Refreshed the top-level `orchestrator/` again after the accepted prototype-free `remain-stop`: the live control plane now targets the bounded `URI-R2-C1` prototype-evidence track (`P1` through `P4`) rooted in `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`, preserves rounds `001` through `015` as historical evidence, and resets `orchestrator/state.json` to idle `select-task` so `run-orchestrator-loop` can start the new campaign.
- Refreshed the top-level `orchestrator/` again after the accepted `URI-R2-C1` `research-stop`: the live control plane now targets a bounded prototype-free re-entry evidence track (`RE1` through `RE5`) rooted in `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`, while preserving rounds `001` through `010` as historical evidence and resetting `orchestrator/state.json` to idle `select-task`.
- Completed the unannotated iso-recursive successor orchestrator runtime through accepted rounds `round-006` through `round-010`: the live control plane now records all roadmap items as done, the successor track terminates at a bounded `R5` `research-stop` decision for subset `URI-R2-C1`, and `orchestrator/state.json` is back at idle `stage: "done"` with `last_completed_round: "round-010"`.
- Refreshed the top-level `orchestrator/` into a successor control plane for the approved unannotated iso-recursive research track: the live roadmap now starts at the `R1` gap-map stage, `orchestrator/state.json` is reset to `select-task` while preserving historical `round-005` continuity, and the verification/role prompts now inherit the prior automatic-recursive-inference evidence chain instead of the completed roadmap.
- Added a top-level `orchestrator/` successor control plane for research-first automatic recursive-type inference on branch `codex/automatic-recursive-type-inference`: the new roadmap, verification contract, role prompts, and state file inherit the completed recursive-types packet as predecessor evidence, and `.worktrees/` is now gitignored for future round worktrees.
- Added M5 eMLF recursive-annotation surface exposure without pipeline lowering: `SrcTy` now includes `STMu`, the frontend parser/pretty-printer/normalizer accept and emit canonical `μa. τ` surface syntax (`mu a. τ` remains accepted on input), and Phase 1 now rejects normalized recursive annotations explicitly via `RecursiveAnnotationNotSupported` until later M6 lowering work lands.
- Added explicit M4 contractiveness validation for elaborated recursive types in Phase 7: `typeCheck` now rejects non-contractive `μ` types across term annotations, let schemes, type-abstraction bounds, instantiation arguments, and `ERoll`, while keeping the v1 policy conservative (`forall` does not count as a guard, arrows/constructors do).
- Added explicit public xMLF recursive-term surface forms `XRoll`/`XUnroll`, including canonical `roll[τ] e` / `unroll e` parser+pretty support, roundtrip coverage, rejection coverage, and internal XMLF display bridging for recursive runtime terms.
- Added the recursive-types design artifacts and staged implementation roadmap under `tasks/todo/2026-03-11-recursive-types-design/` and `docs/plans/2026-03-11-recursive-types-roadmap.md`.
- Added a docs-only recursive-types orchestration packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`, including the milestone mechanism table, orchestrator prompt, and authoritative JSONL event log; also corrected the roadmap overview so it consistently lists milestones `M0` through `M7`.
- Updated the recursive-types orchestration packet so verifier-backed architectural gaps can route into a same-milestone design-resolution lane instead of forcing immediate terminal failure; the current `M7` next step is now a thesis-backed `TyMu` design slice rather than a stop.
- Refreshed `roadmap.md` to match the current codebase layout (2026-03-11): documented the live public API split (`MLF.API` / `MLF.Pipeline` / `MLF.XMLF`), replaced stale deviation references with the `docs/thesis-*` audit surfaces, and expanded the module map to reflect the current solver/elaboration ownership boundaries.
- Restored a warning-free forced rebuild after the dead-export loop (2026-03-10): removed the remaining redundant imports in `MLF.Elab.Phi.Omega.Interpret`, `MLF.Elab.Elaborate.Scope`, `MLF.Elab.Elaborate.Algebra`, `MLF.Elab.Elaborate.Annotation`, and `MLF.Elab.Run.ResultType`, then re-verified with `cabal build all --ghc-options=' -fforce-recomp -Werror'` and `cabal test`.
- Restored a warning-free forced rebuild after the stabilization landing by clearing the remaining local warning sites in presolution helpers and test fixtures.
- Completed the post-split stabilization-and-landing pass for the live tree: the split runtime façades now have explicit thin-façade guards, the public-topology docs are source-guarded, the split child modules are Cabal-guarded as implementation-only, and the ordered owner sweeps plus final full gate are green.
- Made `MLF.Pipeline` the canonical public execution/runtime API and trimmed `MLF.API` back to frontend syntax/parse/pretty/normalization helpers.
- Completed the remaining refactor loop for the major internal monoliths:
  - `MLF.Elab.Phi.Omega` now delegates to `Omega.Domain`, `Omega.Interpret`, and `Omega.Normalize`;
  - `MLF.Constraint.Presolution.EdgeUnify` now delegates to `EdgeUnify.State`, `EdgeUnify.Omega`, and `EdgeUnify.Unify`;
  - `MLF.Reify.Core` now delegates to `Reify.Cache`, `Reify.Named`, `Reify.Type`, and `Reify.Bound`;
  - `MLF.Constraint.Solve` now delegates to `Solve.Worklist`, `Solve.Harmonize`, and `Solve.Finalize`;
  - `MLF.Elab.Elaborate` now delegates to `Elaborate.Algebra`, `Elaborate.Scope`, and `Elaborate.Annotation`.
- Added explicit prep structures/helpers used by the split campaign: `EdgeArtifacts`, `TraceCopyArtifacts`, `prepareTraceCopyArtifacts`, `mkInitialPresolutionState`, and `tyExpNodeIds`.
- Removed the legacy public `MyLib` wrapper and documented `MLF.API`, `MLF.Pipeline`, and `MLF.XMLF` as the supported downstream surfaces.
- Added public-surface / repository guard coverage: `PublicSurfaceSpec`, `RepoGuardSpec`, and `PresolutionFacadeSpec` now protect public API behavior, test-harness wiring, `MyLib` absence, the `Constraint.Types`/`Constraint.Types.Graph` ownership boundary, and the narrowed presolution facade.
- Moved presolution test-only helpers behind `MLF.Constraint.Presolution.TestSupport`, keeping `MLF.Constraint.Presolution` focused on runtime/internal Phase 4 entrypoints and runtime trace data.
- Thinned low-risk orchestration seams without changing behavior: `MLF.Constraint.Presolution.Driver` now reuses `Rewrite.rebuildBindParents`, and `MLF.Elab.Run.Pipeline` now uses explicit snapshot-preparation and redirect-then-canonicalize helpers.
- Moved `MLF.Constraint.Presolution.EdgeProcessing` pending-unify / closure-seed state reads behind `MLF.Constraint.Presolution.StateAccess` (2026-03-09), without changing owner-boundary scheduling or diagnostics.
- Single-sourced canonical scheme-root owner/root-set bookkeeping between `ResultType.Fallback` and `Generalize.Phase4` (2026-03-09); only the shared root→owner/root-set construction moved, not result-type or generalization policy logic.
- Retired the dead derived `chiCanonicalBindParents` helper from `MLF.Elab.Run.ChiQuery` (2026-03-09); the lone fallback caller now reads canonical bind parents directly from `chiCanonicalConstraint`.
- Single-sourced witness/trace canonicalization under `MLF.Constraint.Presolution.Rewrite` (2026-03-09): elaboration/runtime now reuses the same witness/trace canonicalizer contract directly, while the elaboration-local `canonicalizeExpansion` helper intentionally remains separate because its semantics still differ.
- Retired the dead `pendingWeakenOwnerForNode` / `pendingWeakenOwnerForEdge` alias wrappers from `MLF.Constraint.Presolution.EdgeUnify`, keeping pending-weaken owner queries single-sourced in `StateAccess`.
- Narrowed the delayed-weaken diagnostic boundary further (2026-03-10): `pendingWeakenOwners` now stays owned by `MLF.Constraint.Presolution.EdgeUnify.Omega`, while `Driver` and `EdgeProcessing` import it directly instead of through the `EdgeUnify` façade; added a focused façade guard to keep the re-export retired.
- Retired the dead `instEdgeOwnerM` state-access export (2026-03-10): `MLF.Constraint.Presolution.StateAccess` no longer exposes or defines the unused instantiation-edge owner helper, and a focused `PipelineSpec` guard keeps that export absent.
- Narrowed the shared chi-query surface again (2026-03-10): `MLF.Elab.Run.ChiQuery` no longer exports the dead `chiLookupBindParent` / `chiBindParents` passthroughs, and a focused `PipelineSpec` guard keeps those query aliases retired.
- Narrowed `MLF.Binding.Validation` to its still-used helper surface (2026-03-10): `validateSingleGenRoot` is now local-only, and a focused `BindingSpec` guard keeps that stale export retired.
- Retired the dead `canonicalizeRef` canonicalizer helper (2026-03-10): `MLF.Constraint.Canonicalizer` now exports only the live node-level canonicalization surface, and a focused `CanonicalizerSpec` guard keeps the dead ref helper retired.
- Narrowed `MLF.Elab.Run.Debug` to its live ga-scope tracing surface (2026-03-10): the unused `edgeOrigins` export is gone, and a focused `PipelineSpec` guard keeps that dead debug helper retired.
- Narrowed `MLF.Elab.TermClosure` to its live closure surface (2026-03-10): the dead `closeTermWithSchemeSubst` export is gone, and a focused `PipelineSpec` Phase 6 guard keeps that helper retired.
- Retired the dead `rtvSchemeBodyTarget` wrapper from `MLF.Elab.Run.ResultType.View` (2026-03-09): the result-type view boundary now matches its overlay-aware query surface exactly, while `schemeBodyTarget` remains owned by `MLF.Elab.Run.Scope`.
- Simplified `bindingScopeRefCanonical` to reuse the primary `bindingScopeRef` owner path on the canonical constraint (2026-03-09), removing the bespoke canonical bind-parent walker while leaving `letScopeOverrides` scope-divergence semantics unchanged.
- Single-sourced result-type root peeling (2026-03-09): `ResultType` and `ResultType.Fallback` now share one internal helper for scheme-root detection plus `rootForTypeAnn` / `rootForTypePreAnn` peeling, while preserving the canonical-vs-pre-canonical split and leaving broader result-type dispatch unchanged.
- Retired the redundant `preferGenScope` re-lookup from `MLF.Elab.Run.Scope` (2026-03-09): ga′ scope resolution now flows directly from `bindingScopeRef` to `canonicalizeScopeRef`, with the existing scope guards/tests updated around the surviving owner path.
- Re-greened the baseline solved-snapshot test seams (2026-03-09): test/frozen-parity snapshot solved reconstruction now replays through the strict `SolveSnapshot` → `Solved.fromSolveOutput` seam, and `frozen-parity-gen` now wires `SolvedFacadeTestUtil` explicitly.
- Single-sourced the shared target-unwrapping core in `MLF.Elab.Run.Scope` (2026-03-09): `generalizeTargetNode` and `schemeBodyTarget` now share one structural unwrapping helper while preserving the documented `S` vs `S'` selector split.
- Built and validated the base `ResultTypeView` once per runtime result-type computation, threading that validated view through the annotated and fallback workers instead of rebuilding it in each submodule (2026-03-09).
- Retired the one-off `WithCanonicalT` presolution reader layer so `MLF.Constraint.Presolution.EdgeUnify.checkNodeLocked` now uses the shared direct `PresolutionM` state-access helpers, with a focused source guard keeping the reader API retired (2026-03-09).
- Retired stale internal presolution re-export surfaces so `MLF.Constraint.Presolution.Driver` and `MLF.Constraint.Presolution.EdgeProcessing` now expose only the helpers they own, while the public Phase 4 boundary still exports `processInstEdge` (2026-03-09).
- Retired the dead flush-all delayed-weaken entrypoint from `MLF.Constraint.Presolution.EdgeUnify`, leaving owner-boundary flushing as the only supported drain surface in the live presolution path (2026-03-09).
- Retired the thin presolution compatibility facade so the public Phase 4 entrypoint now imports its owner modules directly while preserving the exported testing/runtime surface (2026-03-09).
- Shared the snapshot canonicalization preparation prelude between `MLF.Constraint.Presolution.View` and `MLF.Constraint.Finalize` without changing thesis-facing semantics (2026-03-09):
  - extracted one shared preparation path for dead/self UF sanitization plus canonical map/query derivation from the sanitized UF;
  - kept the intentional canonical-constraint split intact, with `fromPresolutionResult` still doing raw snapshot rewrite and finalize entrypoints still using repaired/finalized canonical-constraint construction;
  - removed the finalize build-then-override `PresolutionView` construction shape while preserving the existing parity/finalization behavior;
  - verified `PresolutionView mirrors solved canonical/node/bound queries` (`1 example, 0 failures`), `fromSolveOutput matches explicit pre-rewrite snapshot construction` (`1 example, 0 failures`), `cabal test --test-show-details=direct`, and `cabal build all && cabal test`.
- Closed the stale legacy-syntax stretch TODO and refreshed syntax wording (2026-03-08):
  - confirmed xMLF pretty emitters and elaboration display paths already emit canonical syntax via `MLF.XMLF.Pretty` / `MLF.Elab.Types`;
  - updated `docs/syntax.md` to describe legacy spellings as parser-only transition syntax instead of an active migration state.
- Closed the narrowed Task 46 elaboration-input strictness follow-up (2026-03-08):
  - made `MLF.Elab.Elaborate.scopeRootFromBase` propagate base binding-path failures instead of falling back to `typeRef root`;
  - added `PipelineSpec` guard coverage for the retired `Left _ -> typeRef root` fallback;
  - verified `elab-input witness-authoritative guard` (`1 example, 0 failures`), `elab-input absolute thesis-exact guard` (`1 example, 0 failures`), `checked-authoritative` (`9 examples, 0 failures`), `Dual-path verification` (`4 examples, 0 failures`), and `cabal build all && cabal test` (`1005 examples, 0 failures`).
- Single-sourced `schemeBodyTarget` ownership in `MLF.Elab.Run.Scope` (2026-03-08):
  - removed the duplicate local `schemeBodyTarget` helper from `MLF.Elab.Elaborate`;
  - kept `schemeBodyTarget` as the thesis `S′`-style subterm target selector and added `generalizeTargetNode` alongside it for the `S`-style named-node generalization case needed by nested-let / alias elaboration;
  - added direct `ScopeSpec` coverage plus a `PipelineSpec` source guard proving `Elaborate` no longer redefines `schemeBodyTarget ::`;
  - verified `schemeBodyTarget` (`6 examples, 0 failures`), `nested` (`27 examples, 0 failures`), `BUG-002-V2` (`1 example, 0 failures`), and `cabal build all && cabal test` (`1004 examples, 0 failures`).
- Swept live docs for stale workflow terminology beyond skill names/paths (2026-03-08):
  - replaced platform-specific execution-note wording with the current generic `@executing-plans` phrasing across live plan docs;
  - renamed older parallel-work headings/titles in live plans to the current `Parallel Work` terminology where those docs still describe current execution guidance.
- Swept live non-archival docs for stale skill references (2026-03-08):
  - confirmed the only live stale skill link was a retired team-orchestration skill reference in `docs/plans/2026-03-03-chi-p-query-first-elab-resulttype-agent-team-implementation-plan.md`;
  - replaced it with the current `dispatching-parallel-agents` + `tmux` execution-skills pairing and left archival history untouched.
- Refreshed the repository guidance entry points (2026-03-08):
  - reorganized `AGENTS.md` to make maintenance expectations, instruction precedence, workspace-safety rules, and task-planning norms explicit;
  - replaced the stale parallel-skill reference in current guidance with the active parallel-agent skills;
  - expanded `tasks/readme` to match the required task-folder structure.
- Completed the thesis-exact fallback rework closeout (2026-03-08):
  - removed the residual let-level chooser in `MLF.Elab.Elaborate`, the recursive callback from `MLF.Elab.Run.Generalize`, and the recursive scheme fallback from `MLF.Elab.Generalize`;
  - made `reifyInst` fail-fast unless witness/domain-owned authority is sufficient (`ewLeft`/`ewRight`, `etBinderArgs`, `etCopyMap` copied nodes), while preserving exact-annotation identity reuse for already-authoritative explicit-forall subjects;
  - updated the remaining fallback-dependent sentinels/corpora (`BUG-2026-02-06-002`, `BUG-2026-02-08-004`, nested-let alignment, dual annotated coercion consumers) to assert strict fail-fast behavior where only expansion-derived recovery had previously kept them green;
  - regenerated `test/golden/legacy-replay-baseline-v1.json` to the new strict semantics;
  - verified `cabal build all && cabal test` (`998 examples, 0 failures`).
- Removed the remaining live thesis-inexact fallback families from elaboration/planner/instantiation (2026-03-08):
  - removed the GA→no-GA→reify retry ladders from `MLF.Elab.Elaborate`, `MLF.Elab.Run.Pipeline`, and `MLF.Elab.Run.ResultType.Util`, and tightened `MLF.Elab.Generalize` so `SchemeFreeVars` now surfaces directly instead of retrying weaker routes;
  - removed synthesized wrapper owner fallback from `MLF.Constraint.Presolution.EdgeProcessing.Planner` and removed the generic fallback branch from `MLF.Elab.Run.Instantiation.inferInstAppArgsFromScheme`;
  - replaced empty-Ω compatibility recovery with authoritative witness/scheme-driven instantiation reconstruction in `MLF.Elab.Phi.Omega`, `MLF.Elab.Phi.Translate`, and `MLF.Elab.Elaborate`, while updating annotation closure/finalization to preserve the thesis-facing explicit-forall and bounded-coercion cases;
  - updated the fallback-removal guards/regressions in `PipelineSpec`, `GeneralizeSpec`, `EdgePlannerSpec`, `Phi.AlignmentSpec`, and the frozen parity baseline;
  - verified `cabal build all && cabal test` (`992 examples, 0 failures`).
- removed the remaining live GA/no-GA/reify ladders, planner wrapper-root fallback, and generic instantiation-inference fallback;
- tightened `Phi`/annotation elaboration to use explicit witness/trace/expansion authority, repaired the strict thesis-facing regressions that surfaced (BUG-003/BUG-004, explicit-forall annotation paths, let-polymorphic `id id`), and refreshed the stale sentinel/frozen-parity expectations to the new strict behavior;
- verified the campaign with focused guard slices and `cabal build all && cabal test` (`992 examples, 0 failures`).
- Retired the final non-must-stay solved facade helper cluster (2026-03-08):
  - removed `lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, and `canonicalizedBindParents` from the public `MLF.Constraint.Solved` facade;
  - replaced their owner-local use with direct constraint/canonical logic in `Reify.Core`, `MLF.Constraint.Presolution.View`, and the solved-view parity tests;
  - verified `cabal build all && cabal test`, `MLF.Constraint.Solved` (`51 examples, 0 failures`), `migration guardrail: thesis-core boundary matches legacy outcome` (`1 example, 0 failures`), `PresolutionView mirrors solved canonical/node/bound queries` (`1 example, 0 failures`), and `final reify/view helper cluster is absent from the Solved facade` (`1 example, 0 failures`).
- Relocated `pruneBindParentsSolved` behind Finalize (2026-03-08):
  - removed `pruneBindParentsSolved` from the public `MLF.Constraint.Solved` facade and kept the implementation owner-local to `MLF.Constraint.Finalize` / `MLF.Constraint.Solved.Internal`;
  - updated the one test caller to use `Finalize.stepPruneSolvedBindParents` and added a direct facade-absence guard in `Constraint.SolvedSpec`;
  - verified `cabal build all && cabal test`, `MLF.Constraint.Solved` (`49 examples, 0 failures`), `checked-authoritative does not adapt solved via prune helper at entry` (`1 example, 0 failures`), and `prune helper is absent from the Solved facade` (`1 example, 0 failures`).
- Moved the remaining solved test/audit helper bundle behind a test utility (2026-03-08):
  - added `test/SolvedFacadeTestUtil.hs` as the test-only home for `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement`;
  - removed that helper bundle from the public `MLF.Constraint.Solved` facade and added a direct solved-facade guard in `test/Constraint/SolvedSpec.hs`;
  - verified `cabal build all && cabal test`, `MLF.Constraint.Solved` (`48 examples, 0 failures`), `WitnessDomain` (`23 examples, 0 failures`), `ga scope` (`2 examples, 0 failures`), and `test-only helper bundle is absent from the Solved facade` (`1 example, 0 failures`).
- Retired dead raw canonical container accessors (2026-03-08):
  - removed `canonicalBindParents` and `canonicalGenNodes` from the `Solved` facade and internal implementation after confirming they had no live code callers;
  - added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting those raw canonical container accessors do not reappear on the facade;
  - verified `cabal build all && cabal test`, `MLF.Constraint.Solved` (`46 examples, 0 failures`), and `raw canonical container accessors are absent from the Solved facade` (`1 example, 0 failures`).
- Relocated remaining shared `Solved` compatibility builders (2026-03-08):
  - split `MLF.Constraint.Solved` into a thin facade plus non-exposed `MLF.Constraint.Solved.Internal` to preserve opacity while moving `fromConstraintAndUf` and `rebuildWithConstraint` off the public surface;
  - redirected `MLF.Constraint.Finalize` and `MLF.Reify.Core` to use the internal builders locally and updated the public solved tests to assert the facade no longer exposes those compat builders;
  - verified `cabal build all && cabal test`, `MLF.Constraint.Solved` (`45 examples, 0 failures`), `migration guardrail: thesis-core boundary matches legacy outcome` (`1 example, 0 failures`), and `GeneralizeEnv stores canonical maps, not solved handles` (`1 example, 0 failures`).
- Narrowed `geRes` to a canonical map (2026-03-08):
  - replaced `GeneralizeEnv.geRes :: Solved` with `geCanonicalMap :: IntMap.IntMap NodeId` in the presolution planning context;
  - removed `buildSolvedFromPresolutionView` from `MLF.Constraint.Presolution.Plan` and now preserve the sanitized canonical map directly from `PresolutionView` data;
  - added a direct migration guard in `test/PresolutionSpec.hs` asserting the planning layer stores canonical maps, not solved handles;
  - verified `cabal build all && cabal test`, `GeneralizeEnv stores canonical maps, not solved handles` (`1 example, 0 failures`), `Phase 4 — Principal Presolution` (`161 examples, 0 failures`), and `Generalize shadow comparator` (`8 examples, 0 failures`).
- Retired dead `Solved` mutation hooks (2026-03-08):
  - removed `rebuildWithNodes`, `rebuildWithBindParents`, and `rebuildWithGenNodes` from `MLF.Constraint.Solved` after confirming they had no live code callers;
  - added a direct migration guard in `test/Constraint/SolvedSpec.hs` preventing those hooks from reappearing on the `Solved` surface;
  - verified `cabal build all && cabal test`, `dead mutation hooks are absent from the Solved surface` (`1 example, 0 failures`), and `MLF.Constraint.Solved` (`44 examples, 0 failures`).
- Completed solved ecosystem classification table closeout (2026-03-08):
  - expanded `docs/architecture.md` from a coarse `Solved` cleanup note into a full grouped 3-column classification of the `Solved` surface and adjacent solved-related seams;
  - recorded the authoritative evidence matrix in the 2026-03-08 solved classification audit notes, covering every exported `Solved` symbol plus the main view/finalize/reify/planner compatibility seams;
  - locked the thesis-exact cleanup rule to preserve replay-faithful construction, original↔canonical correspondence, and strict solved-graph validation while relocating compat glue and retiring dead/test-only production surface;
  - verified static audit counts (`32` export entries, `13` direct `src/` importers, `12` direct `test/` importers, `6` named adjacent seams), `MLF.Constraint.Solved` (`43 examples, 0 failures`), and `chi-first guard: runtime and reify modules no longer adapt Solved through fromSolved` (`1 example, 0 failures`).
- Deduplicated low-risk helper pairs (2026-03-08):
  - moved `freshNameLike` into `MLF.Util.Names` and removed the duplicate local copies from `MLF.Frontend.Normalize` and `MLF.Reify.TypeOps`;
  - moved `mapBoundType` into `MLF.Elab.Types` and removed the duplicate local `mapBound` helpers from `MLF.Elab.Run.TypeOps` and `MLF.Constraint.Presolution.Plan.Finalize`;
  - added focused source guards for both helper homes;
  - verified `freshNameLike is shared via MLF.Util.Names` (`1 example, 0 failures`), `mapBoundType is shared via MLF.Elab.Types` (`1 example, 0 failures`), `MLF.Frontend.Normalize` (`5 examples, 0 failures`), `Generalize shadow comparator` (`8 examples, 0 failures`), and `cabal build all && cabal test` (`978 examples, 0 failures`).
- Shared frontend/XMLF parser scaffolding (2026-03-08):
  - extracted common lexer/literal helpers into `MLF.Parse.Common` and the shared type-grammar core into `MLF.Parse.Type`;
  - rewired `MLF.Frontend.Parse` and `MLF.XMLF.Parse` to use the shared scaffolding while keeping term/computation grammar entrypoints local;
  - preserved the XMLF-specific forall-binder stopping rule via a parser-config hook rather than forcing one binder-list grammar across both parsers;
  - added a source guard proving the duplicated lexer/type-helper block no longer lives in both parser modules;
  - verified `frontend and XMLF parsers share lexer/type scaffolding modules` (`1 example, 0 failures`), `Frontend eMLF parser` (`30 examples, 0 failures`), `xMLF parser` (`8 examples, 0 failures`), and `cabal build all && cabal test` (`976 examples, 0 failures`).
- Extracted shared canonicalization helpers (2026-03-08):
  - moved the duplicated canonical-map chase helpers (`buildCanonicalMap`, `chaseUfCanonical`, `equivCanonical`, `nodeIdKey`) into `MLF.Constraint.Canonicalization.Shared`;
  - rewired `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` to use that single implementation;
  - added a focused source guard to prevent the helper block from reappearing in both modules;
  - verified `Canonicalization helper dedup guards` (`1 example, 0 failures`), `PresolutionView mirrors solved canonical/node/bound queries` (`1 example, 0 failures`), `Canonicalizer` (`5 examples, 0 failures`), and `cabal build all && cabal test` (`975 examples, 0 failures`).
- Guard-first surface `Expr` fold refactor (2026-03-08):
  - added direct row1 desugaring-contract coverage in `test/FrontendDesugarSpec.hs` for annotated terms, annotated lambdas, nested recursion, and typed-let coercion-only lowering;
  - added manual recursion-schemes support for `Expr 'Surface ty` in `MLF.Frontend.Syntax` and refactored `MLF.Frontend.Desugar.desugarSurface` to a local `cata` while preserving exact coercion insertion and let placement;
  - kept `MLF.Frontend.Normalize` explicit and unchanged as the binder/capture-sensitive boundary;
  - verified `MLF.Frontend.Desugar` (`4 examples, 0 failures`), `desugars annotated lambda parameters via let` (`1 example, 0 failures`), `ELet with EAnn RHS does not create explicit-scheme instantiation structure` (`1 example, 0 failures`), `row1 closeout guard|checked-authoritative` (`2 examples, 0 failures`), and `cabal build all && cabal test` (`974 examples, 0 failures`).
- Removed final χp `...View` alias duplicates (2026-03-07):
  - collapsed the remaining duplicate `...View` / `...FromView` aliases in runtime and reify helpers so only the unsuffixed `PresolutionView`-typed APIs remain;
  - updated runtime, result-type, elaboration, Phi, and test call sites to use the canonical unsuffixed names;
  - added a source guard asserting the duplicate alias names are retired from runtime and reify modules;
  - verified `ga scope` (`2 examples, 0 failures`), `Generalize shadow comparator` (`8 examples, 0 failures`), `runtime and reify modules no longer adapt Solved through fromSolved` (`1 example, 0 failures`), `duplicate ...View aliases are retired from runtime and reify modules` (`1 example, 0 failures`), `row2 absolute thesis-exact guard` (`1 example, 0 failures`), `checked-authoritative` (`8 examples, 0 failures`), `Dual-path verification` (`4 examples, 0 failures`), and `cabal build all && cabal test` (`970 examples, 0 failures`).
- χp/view-native elaboration closeout (2026-03-07):
  - removed non-test/non-legacy `fromSolved` usage from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`;
  - promoted `PresolutionView` to the primary internal/runtime API for scope resolution, bound/alias inlining, generalization helpers, result-type generalization, and the non-legacy reify surface;
  - updated the planning/generalization reify context to carry `PresolutionView` snapshots directly and confined `fromSolved` to `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests;
  - added source guards for the cleanup and revalidated `chi-p global cleanup guard`, `chi-p wrapper retirement guard`, `resolveCanonicalScope propagates binding tree cycle errors`, `Generalize shadow comparator`, `row2 absolute thesis-exact guard`, and `cabal build all && cabal test` (`969 examples, 0 failures`).
- Finished χp/view-native elaboration cleanup (2026-03-07):
  - removed the remaining non-legacy `fromSolved` wrappers from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`;
  - made `PresolutionView` the primary internal/runtime API for scope lookup, type inlining, generalization, result-type fallback generalization, and reification helpers, while keeping `fromSolved` only in the presolution boundary, `MLF.Elab.Legacy`, and tests;
  - updated internal/test callers to pass explicit `PresolutionView`s and added a source guard that runtime/reify modules no longer adapt `Solved` through `fromSolved`;
  - verified `ga scope` (`2 examples, 0 failures`), `Generalize shadow comparator` (`8 examples, 0 failures`), `runtime and reify modules no longer adapt Solved through fromSolved` (`1 example, 0 failures`), `row2 absolute thesis-exact guard` (`1 example, 0 failures`), `ResultType|Phase 6 — Elaborate|chi-first gate stays green` (`1 example, 0 failures`), `checked-authoritative` (`8 examples, 0 failures`), `Dual-path verification` (`4 examples, 0 failures`), and `cabal build all && cabal test` (`969 examples, 0 failures`).
- Retired library-side Φ test hooks (2026-03-07):
  - removed `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge` from `mlf2-internal`;
  - moved pure witness-domain ranking helpers into `test/Phi/WitnessDomainUtil.hs` and renamed the dedicated unit suite to `WitnessDomain`;
  - switched `GeneralizeSpec` to real production `MLF.Elab.Generalize` imports, rewrote the two `normalizeInst` checks to behavior-level assertions, and kept production `MLF.Elab.Phi.Omega` on the same direct replay-spine fail-fast path with local diagnostics only;
  - verified `WitnessDomain` (`23 examples, 0 failures`), `Generalize shadow comparator` (`8 examples, 0 failures`), `no-trace test entrypoint fails fast with MissingEdgeTrace` (`1 example, 0 failures`), `elab-input thesis-exact guard` (`2 examples, 0 failures`), `elab-input absolute thesis-exact guard` (`1 example, 0 failures`), `row9-11 direct-target guard` (`1 example, 0 failures`), and `cabal build all && cabal test` (`966 examples, 0 failures`).
- Thesis-exact Phi identity cleanup (2026-03-07):
  - removed the stale compiled `MLF.Elab.Phi.Binder` module and retired its helper re-exports from `MLF.Elab.Phi`;
  - kept runtime `MLF.Elab.Phi.Omega` on the accepted direct replay-spine fail-fast contract, while tightening `MLF.Elab.Phi.IdentityBridge` notes/tests to describe it as a witness-domain utility/test surface rather than a runtime repair engine;
  - added a row9-11 facade source guard plus a dedicated `OpGraft` missing-from-spine regression alongside the existing `OpWeaken` fail-fast coverage;
  - verified `row9-11 facade cleanup guard`, `row9-11 direct-target guard`, `OpWeaken on binder target missing from quantifier spine fails fast`, `OpGraft on binder target missing from quantifier spine still fails fast even when IdentityBridge finds witness-domain matches`, `IdentityBridge`, and `cabal build all && cabal test` (`966 examples, 0 failures`).
- Thesis-exact recursion-refactor verifier sweep (2026-03-07):
  - ran the fresh verifier-owned sweep over the new 8-row recursion-refactor mechanism table;
  - closed all 8 rows to `YES` against the live thesis/code/test evidence;
  - added direct row5 production-path `Γ_{a′}` anchors/tests for the lambda-side and let-side environment rules;
  - completed the row7 exhaustive traversal inventory and the row8 explicit graph-phase non-goal guardrail, and resolved the related campaign-faithfulness gaps in `Bugs.md`.
- Docs thesis-exact recursion-refactor campaign scaffolding (2026-03-07):
  - added a new thesis-exact recursion-refactor mechanism table with 8 fixed rows spanning frontend preprocessing, elaboration ordering/scope, tree-recursion targets, and graph-phase guardrails;
  - added a matching improving-loop prompt with a simplified role model where `Planner` owns thesis/code research and evidence reconciliation;
  - added a JSONL orchestrator log template plus companion design/implementation-plan docs and synced `TODO.md` to track the campaign.
- TMT fresh round-2 closeout (2026-03-07):
  - row2 `Result-type context wiring` now uses finalized snapshot-native `PresolutionView`s in the live pipeline/result-type path and no longer exposes the row2 solved-compat shim in `ChiQuery`;
  - row8 `Translatability normalization` now performs §15.2.8 all-inert `W`-normalization on the live presolution path;
  - added dedicated row8 coverage, strengthened the row2 absolute guard, and refreshed `test/golden/legacy-replay-baseline-v1.json` to freeze the new thesis-exact artifacts;
  - revalidated `row2 absolute thesis-exact guard`, `row2 closeout guard`, `row8 thesis-exact guard`, `Translatable presolution`, `O15-TRANS*`, `O05-*`, `Frozen parity artifact baseline`, `checked-authoritative`, `Dual-path verification`, and `cabal build all && cabal test`.
- TMT per-row fresh review audit (2026-03-07):
  - reviewed all 14 Transformation Mechanism Table rows with one fresh agent per row against the newest codebase and the thesis;
  - refreshed row wording/evidence for rows 1, 3, 6, 12, and 13;
  - reclassified row2 and row8 back to `No` after finding a hidden solved-compat adapter in the live result-type path and a missing live all-inert `W` normalization step;
  - recorded the reopened gaps in `Bugs.md` and updated live tracker/docs accordingly.
- Docs TMT closeout tracker sync (2026-03-07):
  - updated the live Transformation Mechanism Table note, `TODO.md`, and `implementation_notes.md` so the row6 `MAXIMUMRETRY` run is explicitly historical rather than current next work;
  - recorded the fresh round-2 all-`YES` verification sweep as the authoritative post-closeout state for the TMT campaign;
  - archived directly related completed task folders still living under `tasks/todo/`.
- Round 2 TMT rows 9-11 direct-target closeout (2026-03-07):
  - removed Ω source-candidate reconciliation helpers from `MLF.Elab.Phi.Omega`, so `OpRaise` no longer recovers alternative source nodes after the replay bridge;
  - made unresolved non-trace `OpRaise` targets fail fast instead of degrading to `ε`, while preserving source-domain interior membership only for direct forward `etCopyMap` alias evidence;
  - added direct regression coverage for the new `OpRaise` fail-fast path and a guard that `Omega` no longer defines the old source-candidate helper layer;
  - verified gates:
    - `source-space identity replay target` (`1 example, 0 failures`)
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target` (`1 example, 0 failures`)
    - `OpRaise fails fast when a trace-source target resolves to no existing replay node` (`1 example, 0 failures`)
    - `OpRaise fails fast when a non-trace target resolves to no existing replay node` (`1 example, 0 failures`)
    - `row9-11 direct-target guard` (`1 example, 0 failures`)
    - `IdentityBridge` (`23 examples, 0 failures`)
    - `checked-authoritative` (`8 examples, 0 failures`)
    - `Dual-path verification` (`4 examples, 0 failures`)
    - `cabal build all && cabal test` PASS (`959 examples, 0 failures`).
- Docs orchestrator log format rationalization (2026-03-06):
  - chose `orchestrator-log.jsonl` as the single authoritative orchestrator execution log;
  - moved human-readable run summaries to `findings.md` / `progress.md` rather than a second canonical markdown log;
  - updated the live round-2 plan, active prompt templates, reusable goal-loop skill references, and scaffold script to emit/use JSONL event logs consistently.
- Docs orchestrator round-2 prompt workflow hardening (2026-03-06):
  - added standalone round-2 improving-loop prompt and companion plan docs
    with mandatory pre-planner researcher handoff;
  - raised the per-round implementation attempt cap from 6 to 10;
  - hardened planner execution flow with evidence reconciliation,
    no-progress detection, scope expansion, blocked mode, failed-attempt
    accept-or-revert hygiene, standalone row-14 YES/NO mapping, and
    explicit Verifier-owned transformation-table row refreshes before
    each `YES`/`NO` gate;
  - aligned the generic goal-loop prompt, reusable prompt template, and
    scaffold defaults with the same round-2 subagent workflow and
    `fresh-round-2` event-log naming.
- Task 48 row6 replay-contract recovery closeout (2026-03-06):
  - rewrote presolution no-replay projection in
    `MLF.Constraint.Presolution.WitnessNorm` to classify wrapper vs semantic
    replay ops in source-domain identity space rather than rewritten/canonical
    space;
  - preserved baseline no-replay weakening behavior (`graftTargetCount` /
    source-domain graft-target projection) while keeping strict no-replay
    fail-fast on the intended bug-002 path;
  - narrowed rogue no-replay replay-family rejection to residual single-target
    source-interior grafts and pruned wrapper `OpRaise` artifacts before Phi
    while keeping type-tree-bound invalid raises fail-fast in normalization;
  - verified recovery gates:
    - no-replay witness obligations (`6 examples, 0 failures`)
    - `checked-authoritative` and `Dual-path verification` targeted slices
    - bug-002 strict fail-fast, `\y. let id = (\x. x) in id y`, BUG-2026-02-08-004,
      A6 parity, BUG-2026-02-17-002, explicit forall round-trip
    - `cabal build all && cabal test` PASS (`954 examples, 0 failures`).
- Task 47 row3 owner-boundary scheduler follow-up (2026-03-05):
  - removed the explicit flush-all-owner boundary fallback from
    `MLF.Constraint.Presolution.EdgeProcessing` and switched to strict
    closed-owner boundary flushing;
  - stamped pending-weaken owner provenance at enqueue time in
    `MLF.Constraint.Presolution.EdgeUnify` so boundary ownership remains stable
    across later graph rewrites/merges;
  - hardened boundary/finalization diagnostics to report pending owner buckets
    in both edge-loop and Driver finalization invariant failures;
  - strengthened row3 absolute guard slices to assert absence of the
    flush-all-owner fallback pattern;
  - verified gates:
    - `row3 absolute thesis-exact guard` (`6 examples, 0 failures`)
    - `Phase 4 thesis-exact unification closure` (`11 examples, 0 failures`)
    - `Translatable presolution` (`8 examples, 0 failures`)
    - `generalizes reused constructors via make const` (`1 example, 0 failures`)
    - `BUG-002-V1` (`1 example, 0 failures`)
    - `Frozen parity artifact baseline` (`1 example, 0 failures`)
    - `checked-authoritative` (`8 examples, 0 failures`)
    - `Dual-path verification` (`4 examples, 0 failures`)
    - `cabal build all && cabal test` PASS.
- Task 45 TMT row3 absolute ordering follow-up execution (2026-03-05):
  - added strict RED->GREEN guard slice `row3 absolute thesis-exact guard`;
  - introduced owner-aware pending-weaken API surfaces in presolution
    base/state/edge-unify layers (`PendingWeakenOwner`, owner lookup helpers,
    owner-boundary flush API);
  - rewired `MLF.Constraint.Presolution.EdgeProcessing` to owner-boundary
    scheduling markers and removed loop-final-only fallback shape;
  - fixed Wave-3 boundary regression where pending-weaken owner buckets could
    survive boundary checks (`BUG-2026-03-05-002`) by flushing all pending
    owner buckets at each owner boundary and asserting post-boundary emptiness;
  - verified strict gate stack:
    - `row3 absolute thesis-exact guard` (`4 examples, 0 failures`)
    - `Phase 4 thesis-exact unification closure` (`10 examples, 0 failures`)
    - `Translatable presolution` (`8 examples, 0 failures`)
    - `generalizes reused constructors via make const` (`1 example, 0 failures`)
    - `BUG-002-V1` (`1 example, 0 failures`)
    - `Frozen parity artifact baseline` (`1 example, 0 failures`)
    - `checked-authoritative` (`8 examples, 0 failures`)
    - `Dual-path verification` (`4 examples, 0 failures`)
    - `cabal build all && cabal test` PASS (`942 examples, 0 failures` in test log summary).
- Task 44 TMT row `Ordering of transformations` thesis-exact wave integration (2026-03-05):
  - added RED->GREEN row guards:
    - `row3 ordering thesis-exact guard`,
    - `Phase 4 thesis-exact unification closure` characterization for
      `OpWeaken`/edge-interior consistency;
  - moved presolution post-loop work to explicit Driver finalization stage
    (`materialize -> rewrite/canonicalize -> rigidify -> witness normalize`)
    and removed Driver-global `flushPendingWeakens`;
  - integrated edge-loop weaken-flush boundary handling in
    `MLF.Constraint.Presolution.EdgeProcessing` with strict queue drain at the
    loop-final boundary and preserved per-edge unify-closure fail-fast checks;
  - hardened `EdgeUnify.flushPendingWeakens` for idempotent boundary use on
    stale/locked targets without changing witness-recording semantics;
  - resolved regressions (`make const`, `BUG-002-V1`, frozen parity) discovered
    during full-gate verification;
  - verification evidence:
    - RED baseline `row3 ordering thesis-exact guard`: FAIL (`2 examples, 2 failures`),
    - GREEN gates:
      - `row3 ordering thesis-exact guard` (`2 examples, 0 failures`),
      - `Phase 4 thesis-exact unification closure` (`8 examples, 0 failures`),
      - `Translatable presolution` (`8 examples, 0 failures`),
      - `checked-authoritative` (`8 examples, 0 failures`),
      - `Dual-path verification` (`4 examples, 0 failures`),
    - full gate `cabal build all && cabal test`: PASS,
    - full direct suite evidence: `938 examples, 0 failures`.
- Task 42 TMT row2 result-type context wiring absolute hardening (2026-03-05):
  - added RED->GREEN guard `row2 absolute thesis-exact guard`;
  - removed ResultType-local solved-overlay/materialization surfaces from
    `MLF.Elab.Run.ResultType.View` (`rtvSolved`, `rtvOriginalConstraint`,
    `solveFromInputs`);
  - migrated row2 consumers (`Ann`, `Fallback`, `Util`) to
    `PresolutionView`/view-helper query paths and removed direct
    `View.rtvSolved` dependencies;
  - reconciled integration wiring/imports across ResultType/Pipeline boundary
    modules with no algorithmic behavior changes;
  - verified required gates in strict order:
    RED baseline (`1 example, 1 failure`), GREEN gates
    `row2 absolute thesis-exact guard` (`1 example`),
    `row2 closeout guard` (`3 examples`),
    `checked-authoritative` (`8 examples`),
    `Dual-path verification` (`4 examples`), and full gate
    `cabal build all && cabal test` (`935 examples, 0 failures`).
- Task 41 elaboration-input absolute strict all-path hardening (2026-03-05):
  - added RED->GREEN guard `elab-input absolute thesis-exact guard` and
    removed residual surfaces across owned modules:
    `MLF.Elab.Phi.Env` solved-backed env accessors,
    ga-scope error swallowing in `MLF.Elab.Run.Scope`,
    and synthetic `phiFromEdgeWitnessAutoTrace` in
    `MLF.Elab.Phi.TestOnly`;
  - added `test/ScopeSpec.hs` and wired it into test suite registration to
    assert ga-scope binding errors are propagated explicitly;
  - verified required gates in strict order:
    `absolute guard` (`1 example`), `checked-authoritative` (`8 examples`),
    `Dual-path verification` (`4 examples`), full gate
    `cabal build all && cabal test` (`934 examples, 0 failures`).
- Docs closeout for strict elaboration-input classification (2026-03-04):
  finalized TMT row `Elaboration input` as `Thesis-exact = Yes` under the
  strict policy that includes test-only paths, with migration evidence that
  `MLF.Elab.Phi.TestOnly` helper signatures are no longer solved-typed and the
  no-trace entrypoint still fails fast with `MissingEdgeTrace`.
- Elaboration-input absolute thesis-exact re-audit (2026-03-04): rechecked
  Def. 15.3.12 / Fig. 15.3.5 / §15.3.6 against current code and downgraded TMT
  row `Elaboration input` to `Thesis-exact = No` under the table's strict
  criterion (includes test-only paths), because `MLF.Elab.Phi.TestOnly`
  still exposes `Solved`-typed helper signatures (`phiFromEdgeWitnessNoTrace`,
  alias `phiFromEdgeWitness`, and `phiFromEdgeWitnessAutoTrace`).
- Task 39 elaboration-input strict legacy-retirement closeout (2026-03-04):
  retired solved-typed elaboration/Phi compatibility APIs from production
  modules, migrated test-only Phi callback contracts to chi-native shape while
  preserving strict `MissingEdgeTrace` fail-fast behavior, and revalidated
  closeout gates (`elab-input thesis-exact guard` `2 examples`,
  `checked-authoritative` `8 examples`, `Dual-path verification` `4 examples`,
  `cabal build all && cabal test` `931 examples, 0 failures`).
- Wave 4 docs closeout for Task 38 elaboration-input thesis-exact replan
  (2026-03-04): set TMT row `Elaboration input` to `Thesis-exact = Yes` with
  current `χp`-native code/guard references, synced implementation/TODO/task
  tracker docs, and archived the replan folder with Wave 3 gate evidence
  (`2`/`8`/`4` examples; full suite `931 examples, 0 failures`).
- Task 35 elaboration-input thesis-exact closeout (2026-03-04):
  - closed the remaining elaboration-input row gap by removing active
    elaboration-path dependence on internal `chiSolved` materialization and
    validating chi-native active generalize callback wiring;
  - recorded closeout gate evidence:
    `elab-input thesis-exact guard` (`2 examples`),
    `checked-authoritative` (`8 examples`),
    `Dual-path verification` (`4 examples`),
    and `cabal build all && cabal test` (`931 examples, 0 failures`);
  - updated TMT row status and implementation/task notes to mark
    elaboration-input migration done.
- Wave 3 Task 6 row2 adapter retirement docs/verifier closeout (2026-03-04):
  - documented that runtime boundaries no longer expose
    `rtcSolvedCompat`/`rtcSolveLike`/`ecSolved` adapters;
  - captured final closeout verification evidence:
    `row2 closeout guard` (`3 examples`), `checked-authoritative`
    (`8 examples`), `Dual-path verification` (`4 examples`), and
    `cabal build all && cabal test` (`929 examples, 0 failures`);
  - reordered TODO priorities to post-row2 follow-ups (result-type
    solved-overlay simplification and compatibility-signature cleanup).
- Wave 3 Task 6 docs/verifier closeout (2026-03-03):
  - recorded row-1 runtime boundary shape as shipped (`ElabEnv` has no
    `eeSolvedCompat`; `elaborateWithEnv` has no entry-time
    `Solved.rebuildWithConstraint`);
  - captured closeout verification evidence after matcher fallback:
    `row1 closeout guard` (`2 examples`), `checked-authoritative`
    (`7 examples`), `Dual-path verification` (`4 examples`), and final
    `cabal build all && cabal test` PASS;
  - documented explicit row-2 follow-up ordering to retire
    `rtcSolvedCompat`/`rtcSolveLike` before removing remaining elaboration
    compatibility wiring.
- Chi-first elaboration/result-type internal cleanup (2026-03-03):
  - added shared presolution query facade `MLF.Elab.Run.ChiQuery` and wired
    elaboration/result-type internals to query `χp` through that facade;
  - migrated result-type internals to chi-first reads (`ResultType.View`)
    while confining solved compatibility to explicit boundary adapters
    (`rtcSolvedCompat` / `rtcSolveLike`);
  - migrated `elaborateWithEnv` to chi-first canonical/bound access and
    removed ad hoc solved reconstruction in elaboration internals, retaining
    only a narrowed compatibility adapter input (`eeSolvedCompat`);
  - integrated explicit boundary construction via `mkResultTypeInputs` in
    pipeline wiring and added gate-name regression checks for
    `chi-first`/`ResultType`/`Phase 6 — Elaborate`/`Dual-path verification`.
- Runtime replay/mediation removal follow-up (2026-03-03):
  - fixed shared replay-free finalization semantics by introducing
    `Solve.finalizeConstraintWithUF` and routing `MLF.Constraint.Finalize`
    through full snapshot-finalization steps (UF rewrite, eliminated-binder
    rewrite/substitution, bind-parent pruning, strict validation);
  - resolved post-cutover Phase 6 regressions (`PhiInvariantError` /
    `PhiTranslatabilityError`) caused by missing eliminated-binder finalization
    in the new runtime boundary;
  - aligned migration guardrail canonical-map assertions to shared live-node
    domain parity while keeping strict canonical-constraint and solved-query
    parity checks.
- Runtime thesis-exact elaboration-input closeout (2026-03-03):
  - removed direct replay mediation calls from row-1 `Pipeline.hs` boundary wiring (`Solved.fromPreRewriteState`, `solveResultFromSnapshot`, `setSolvedConstraint`) and centralized snapshot finalization in shared runtime constraint finalization helpers;
  - removed replay reconstruction from result-type context materialization (`rtcSolveLike`) by sourcing solved state from `PresolutionView` canonical data;
  - completed boundary wiring by removing `ecSolved` from `ElabConfig` and deriving internal solved access from `ElabEnv.eePresolutionView`;
  - added strict closeout regression slices with exact plan matchers:
    `row1 boundary uses thesis-core elaboration input contract`,
    `elaborateWithEnv consumes thesis-core input`,
    `row1 boundary validates-only and does not mediate input`,
    `migration guardrail: thesis-core boundary matches legacy outcome`,
    `final row1 state uses single thesis-core boundary path`,
    `Dual-path verification`.
- Task 30 solved-compat read reduction (2026-03-03):
  - removed compatibility-only generalize/context fields (`gcConstraintForReify`, `rbConstraintForReify`) and added light `SolvedToBaseMissing` base-domain invariant tracing in plan-context resolution;
  - tightened generalize reification flow by gating alias solved rebuild to non-OnConstraint paths and routing explicit-bound helper reification through OnConstraint bound reads when structural-scheme reify is authoritative;
  - introduced `MLF.Elab.Run.ResultType.View` and refactored result-type internals (`ResultType.hs`, `Ann.hs`, `Fallback.hs`) to centralize solved reads and confine `rtcSolveLike` usage to the view-construction boundary;
  - replaced fallback-core local `Solved.rebuildWithNodes` patching with a bound-overlay view path and preserved parity semantics in target selection and base-constraint mutation;
  - added regressions for GA fallback ladder and result-type fallback mapping branches (`same-domain`, `missing`), and revalidated focused matrix + full gate (`cabal build all && cabal test` => `917 examples, 0 failures`).
- Task 29 Phase 1 solved-view adapter consolidation (2026-03-03):
  - added shared `fromSolved :: Solved -> PresolutionView` at `MLF.Constraint.Presolution.View`;
  - removed duplicated runtime solved→presolution adapter builders from `MLF.Elab.Elaborate`, `MLF.Elab.Phi.Translate`, `MLF.Elab.Run.Generalize`, and `MLF.Elab.Run.Pipeline`;
  - added semantic adapter-equivalence coverage in `PipelineSpec` to keep solved-query behavior unchanged across the shared adapter path.
- Task 29 Phase 4 solved-boundary guard expansion (2026-03-03):
  - added direct solved invariant guards for `validateOriginalCanonicalAgreement` and `canonicalizedBindParents` in `Constraint.SolvedSpec`;
  - added isolated empty-sequence Φ guard (`Trχ(ε)=ε` with `Σ(g)=ε`) in `ElaborationSpec` to decouple Ω-empty coverage from reorder-only behavior;
  - added replay-relevant annotation result-type guard comparing primary annotation computation vs fallback facade in `ElaborationSpec`;
  - moved solved/base resolution test imports to `MLF.Constraint.Presolution.Plan.Context` and removed temporary top-level `MLF.Constraint.Presolution` re-exports for `SolvedToBaseResolution`/`resolveGaSolvedToBase`.
- Task 29 Phase 4 review-fix follow-up (2026-03-03):
  - replaced tautological `fromConstraintAndUf` constructor parity check with direct semantic invariants over canonicalization/original-domain preservation in `Constraint.SolvedSpec`;
  - strengthened AAnn result-type guard setup with populated GA solved/base mappings and explicit mapping assertions before primary-vs-fallback equivalence;
  - narrowed adapter-parity test wording in `PipelineSpec` to "selected solved queries on representative corpus" to avoid overclaiming coverage.
- Solved-module audit closeout (2026-03-03):
  - completed a production-vs-compat/test boundary inventory for `MLF.Constraint.Solved` consumers;
  - documented seven production call chains that still depend on solved-boundary assumptions;
  - mapped thesis-claims/obligations touchpoints and added prioritized follow-up actions/guard-test gaps in task artifacts.
- Wave 2 solved-indirection closeout:
  - migrated `PresolutionPlanBuilder` closure boundary from `Solved` to `PresolutionView` and updated planner/generalize call chain accordingly;
  - generalized `Presolution.View.fromPresolutionResult` to `PresolutionSnapshot a => a -> PresolutionView` to avoid base/view cycle pressure;
  - removed production-only `Solved.fromPresolutionResult` surface (compat/test paths now use remaining solved constructors/query APIs);
  - added source-level guards for (1) plan-builder closure type, (2) solved builder removal, and (3) elaboration entrypoint solved-import hygiene.
- TMT3 Wave 3 docs closeout: Transformation Mechanism Table is now fully all-aligned (all rows `Aligned`), and all `DEV-TMT-*` entries were moved from active `deviations` to `history.resolved` in `docs/thesis-deviations.yaml` with `resolution_date: 2026-03-01`, replacing-commit metadata (Wave 1/Wave 2), and regression evidence.
- Refactored elaboration/pipeline/result-type wiring to a single solved input handle (`eeSolved`, `rtcSolved`) while preserving checked-authoritative output behavior.
- Added regression locks for single-solved migration guards and checked/unchecked pipeline parity slices.
- Aligned elaboration pipeline with thesis model (§10.3-10.4, §12.1.3, §15.3.5-15.3.6)
- Solved is now projection-first; Phi resolves from witness domain before canonical fallback
- Eliminated patchNode mutation from elaboration path (Fallback.hs)
- Enforced read-only boundary for elaboration path on Solved
- Removed deprecated `solvedConstraint` alias
- Thesis A-E cleanup (2026-02-27):
  - added seeded unification-closure API (`runUnifyClosureWithSeed`) and switched presolution closure drains to seeded UF processing;
  - added explicit presolution artifact contract errors (`ResidualUnifyEdges`, `ResidualInstEdges`, `ResidualTyExpNodes`, `MissingEdgeWitnesses`, `MissingEdgeTraces`);
  - removed canonical-domain `Solved` query exports (`canonicalNodes`, `allCanonicalNodes`, `lookupCanonicalNode`, `lookupCanonicalVarBound`) after call-site migration;
  - made `IdentityBridge.sourceKeysForNode` strict witness-domain-only and removed runtime class-fallback identity recovery from Phi/Omega paths;
  - removed runtime `runPipelineElabProjectionFirst`; retained dual-path parity as test-only harness in `DualPathSpec`.
- Thesis-exact Phi replay normalization (2026-02-27):
  - replay metadata contract is now strict: `EdgeTrace.etBinderReplayMap` is required and validated at presolution boundaries;
  - witness normalization/validation enforces replay-map completeness + TyVar codomain + injectivity;
  - Phi bridge aligns replay targets to scheme quantifier IDs and fails fast on replay-domain violations;
  - Omega consumes replay-map/source-alias targets deterministically (no runtime class-member fallback search);
  - no-trace Phi path is strict fail-fast (`MissingEdgeTrace`), and legacy alias-recovery success tests were migrated to fail-fast expectations.
- Atomic strict replay cutover (2026-02-27): producer normalization now enforces the active-source/replay-domain contract; runtime replay-target repair is removed; unresolved trace-source `OpRaise` targets now fail fast; tests/docs were updated in the same change.
- Presolution driver replay-map codomain validation is now unconditional: empty replay binder domains no longer bypass outside-domain hard-reject checks.
- Phi replay bridge strict pass-through follow-up (2026-03-01): removed the remaining runtime projection helper path in `computeTraceBinderReplayBridge`; runtime now only validates replay-map domain/codomain invariants and forwards replay targets unchanged.

* Removed the internal legacy replay fallback path from elaboration (`runPipelineElabViaLegacySolve`) and cleaned legacy fallback helpers from the test harness.
* Replaced live native-vs-legacy parity checks with frozen baseline artifacts:
  - new deterministic artifact module `test/Parity/FrozenArtifacts.hs`,
  - authoritative baseline test `test/FrozenParitySpec.hs`,
  - generator executable `frozen-parity-gen`,
  - regeneration script `scripts/update-frozen-parity-artifacts.sh`,
  - checked-in baseline `test/golden/legacy-replay-baseline-v1.json`.
* Thesis-exact unification ordering update: presolution now enforces initial + per-edge unification closure ordering and carries UF metadata separately (`prUnionFind`) while preserving raw presolution graph (`prConstraint`) for translation; shared closure semantics are centralized in `MLF.Constraint.Unify.Closure`.
* Completed 2026-02-26 native cutover cleanup: production `runPipelineElab` now uses presolution-native solved construction (`fromPresolutionResult`) as the sole production path (temporary dual-run legacy parity guard removed after green-window validation); presolution closure drain continues to skip `runUnifyClosure` when no unify edges are pending, preventing false intermediate-state binding-tree failures.
* Added/updated regression coverage:
  - `O15-ELAB-LET` production baseline guard,
  - `O10-EXP-DECIDE` presolution closure guard,
  - `uses presolution-native solved artifacts`,
  - `Frozen parity artifact baseline`.
* Strict OpWeaken replay closure: removed non-root `OpWeaken` no-op fallbacks in `MLF.Elab.Phi.Omega`; non-root weaken now either resolves to thesis-shaped `InstElim` or fails fast with structured `PhiTranslatabilityError` diagnostics (targets/canonical/class-members/ids/replay domains). Added focused fallback-removal regressions and updated legacy fallback-dependent pipeline baselines to assert strict fail-fast behavior.
* IdentityBridge binder disambiguation: `lookupBinderIndex` now preserves raw binder identity when multiple scheme binders share a solved equivalence class by ranking non-class exact matches before class-member fallback; added regression `preserves raw binder identity before class-member fallback` and retained alias-target `OpWeaken` recovery behavior.
* Milestone 5 gap closure: Ω `OpWeaken` now recovers binder targets from `Solved.classMembers` when witness targets resolve to non-binder aliases (emits thesis-shape `InstElim` instead of `ε` skip); `IdentityBridge.sourceKeysForNode` now includes solved equivalence-class members for canonical-alias binder recovery; added regressions in `ElaborationSpec` and `Phi.IdentityBridgeSpec`.
* Solved API migration follow-up: added `validateCanonicalGraphStrict`, `allCanonicalNodes`, `canonicalizedBindParents`, and explicit canonical accessor `canonicalConstraint` to `MLF.Constraint.Solved`; added canonical-solved scope helper `bindingScopeRefCanonical` in `MLF.Elab.Run.Scope`; migrated `MLF.Elab.Run.Pipeline`, `MLF.Reify.Core`, `MLF.Elab.Run.ResultType.Fallback`, `MLF.Constraint.Presolution.Plan`, and `MLF.Elab.Run.Generalize` off direct `Solved.solvedConstraint` usage.
* Solved backend Phase 6 cleanup: removed `ebCanonicalConstraint` from `MLF.Constraint.Solved.EquivBackend`; backend now stores explicit canonical graph slices and reconstructs canonical `Constraint` views through internal helpers, preserving existing public API behavior.
* Eliminated DEV-PHI-STANDALONE-GRAFT-EXTENSION: standalone `OpGraft` handler (`atBinderKeep` + `InstInside(InstBot σ)`) recognized as thesis-exact (Def. 15.3.4); paired `OpGraft+OpWeaken` handler retained as sound optimization (equivalent by normalizeInst Rule 1); added normalizeInst Rule 1b for context-wrapped `InstUnder` collapse; deviation register 6 → 5 entries.
* DEV-PHI-STANDALONE-GRAFT-EXTENSION investigation: added `StandaloneGraftRemaining` error constructor and `assertNoStandaloneGrafts` validation function to `WitnessCanon.hs`; added characterization tests documenting that standalone grafts are load-bearing when descendant ops separate graft+weaken pairs (Omega's type-state evolution requires graft before descendant ops); updated deviation description with root-cause analysis; deviation retained as `implementation-choice`.
* Eliminated DEV-PHI-WITNESS-WEAKEN-SUPPRESSION: witness emission now always emits OpWeaken for unbounded binders (thesis-exact Def. 15.3.4); removed `suppressWeaken` and `argIsGenBound` guards from `Witness.hs`, removed `dropWeakenOps` from annotation edge processing, extended Omega translation to handle previously-suppressed weakens; deviation register 7 → 6 entries.
* Phi thesis-purity follow-up (Ch. 15.3): registered three load-bearing implementation-choice deviations (`DEV-PHI-WITNESS-WEAKEN-SUPPRESSION`, `DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION`, `DEV-PHI-STANDALONE-GRAFT-EXTENSION`) and linked them to `CLM-PHI-CORRECTNESS`; removed `mergeIntoApp` from Omega replay; moved binder-bottom rescue into `reifyTypeArg`; switched bounded `OpGraft+OpWeaken` bound-match emission to `InstApp boundTy` (literal thesis shape) while preserving replay-state compatibility; added normalization coverage for de-fused left-associated `OpGraft;OpRaise;OpWeaken` shapes to prevent historical `a -> ⊥` regression drift.
* Separate O from Ω in Phi translation (Def. 15.3.4): hoisted quantifier introductions (O) into a dedicated prefix phase before witness replay (Ω) in `phiWithSchemeOmega`, replacing the interleaved `goSteps` loop with `partitionSteps`/`applyIntros` + direct `go`; removed `InstanceStep` type and `ewSteps` field, replacing with `ewForallIntros :: Int` on `EdgeWitness`; removed `normalizeInstanceStepsFull`/`normalizeInstanceStepsWith`, `canonicalizeStep`, `partitionSteps`, `forallIntroSuffixCount`, and `dropWeakenSteps` (renamed to `dropWeakenOps`); `witnessFromExpansion` now returns `(Int, [InstanceOp])` and `integratePhase2Steps` operates on the same pair; retired DEV-STEPINTRO-NOT-OMEGA from deviation register (5 → 4 entries).
* Stratified binding tree for explicit foralls: enforced thesis-exact stratification (Section 9.2) by binding explicit-forall children under their scheme gen node instead of the forall binder's type node; removed now-dead interior widening code (DEV-INTERIOR-WIDENING) from `Copy.hs`; retired DEV-INTERIOR-WIDENING from deviation register (6 → 5 entries).
* Deviation register cleanup: retired 3 deviations (DEV-GEN-FALLBACK-PRESENT as thesis-correct with defense-in-depth note at `checkNoGenFallback`, DEV-LET-SCOPE-THREADING-PARTIAL as fully implemented, DEV-P2-SPEC-STUBS-DEFERRED as orphan); reclassified DEV-INTERIOR-WIDENING to `defense-in-depth` / `semantic-neutral` after confirming widening is non-load-bearing (fires 22 times, 0 test failures on removal); reclassified 3 representation choices (STEPINTRO-NOT-OMEGA, TYEXP-OCCURRENCE-REPR, CONSTRAINT-REPR-SIMPLIFIED) to `implementation-choice`; reclassified 2 proof gaps (WITNESS-NORM-NO-PROOF, PHASE7-NO-FORMAL-LINK) to `proof-gap`; register 9 → 6 entries with precise status classifications.
* Generalized unification (Ch 7.6): batch Rebind after first-order unification in Solve phase, with `harmonizeBindParentsMulti` for equivalence-class-aware binding-tree harmonization.
* Defensible exactness: added machine-checked thesis claims registry (`docs/thesis-claims.yaml`, 21 claims) and deviation register (`docs/thesis-deviations.yaml`, 9 deviations) with cross-link validation script (`scripts/check-thesis-claims.sh`); added `supports_claims` back-links to obligations ledger; added three new test modules (`TranslatablePresolutionSpec`, `PhiSoundnessSpec`, `ExpansionMinimalitySpec`) covering Def. 15.2.10, Def. 15.3.4, and Def. 10.1.1; upgraded conformance gate with claims checker and new anchors; migrated paper-map to reference machine-checked artifacts; closed spec drift with deferred notes and deviation cross-references; upgraded three claims from `partial` to `defended`; verification: 765 examples, 0 failures.
* BUG-2026-02-20-001: fixed elaboration-side lambda binder substitution key aliasing so RHS-coercion annotated-let identities no longer leak unresolved `t0` names after `step`; added `PipelineSpec` regression `BUG-2026-02-20-001`.
* Phase 7 theorem obligations: added `/Volumes/src/mlf4/test/TypeSoundnessSpec.hs` with executable property-style preservation/progress proxies over closed well-typed `ElabTerm` values, wired it into `mlf2-test` (`/Volumes/src/mlf4/mlf2.cabal`, `/Volumes/src/mlf4/test/Main.hs`), and enforced it in the thesis gate via `--match "Phase 7 theorem obligations"` in `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`.
* Formal obligations ledger: added canonical Ch. 4–15 rule-to-code-to-test inventory (`docs/thesis-obligations.yaml`), generated Markdown view (`docs/thesis-obligations.md`), renderer/checker scripts (`scripts/render-thesis-obligations-ledger.rb`, `scripts/check-thesis-obligations-ledger.sh`), and mandatory hard-fail gate integration in `scripts/thesis-conformance-gate.sh` (exact 99-ID coverage, anchor executability, and markdown drift enforcement).
* Thesis conformance gate: added canonical thesis-anchor command `scripts/thesis-conformance-gate.sh` (Φ/Ω `R-` matrix rows, A6 parity, BUG-2026-02-17-002 strict success, phase-3 equivalence gates, representative theorem baseline) with minimum matched-example thresholds, and added required CI workflow `.github/workflows/thesis-conformance.yml` to enforce `cabal build all` + gate command on push/PR.
* A7 (P2) non-binding dedup closure: added shared test harness pipeline-stage helpers in `test/SpecUtil.hs` (`runConstraintDefault`, `runToPresolutionWithAnnDefault`, `runPipelineArtifactsDefault`) and migrated `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` off remaining local normalize/solve-chain wrappers, preserving behavior while centralizing helper logic.
* A5 (P3) totality/harness hardening: replaced bare-coercion stringly internal failure with typed `UnexpectedBareCoercionConst`, totalized STCon coercion-copy argument traversal via `internalizeConArgs` (`NonEmpty` recursion), consolidated presolution harness wiring through `PresolutionSpec`, and added fail-fast harness guard in `test/Main.hs` so omitted umbrella wiring aborts test execution.
* BUG-2026-02-17-002 closure: fixed applied bounded/coercion A6 mismatch by aligning annotated-lambda let fallback and application instantiation recovery in `MLF.Elab.Elaborate`; converted the `PipelineSpec` sentinel into a strict success assertion (`Int`) and moved the bug to resolved in `Bugs.md`.
* A6 (P2) parity/regression coverage: expanded bounded/coercion-heavy checked-vs-unchecked parity tests in `test/PipelineSpec.hs`, `test/TypeCheckSpec.hs`, and `test/ReduceSpec.hs`, including `typeCheck` agreement and normalization-preservation checks for elaborated terms.
* A3 (P2) API cleanup: quarantined legacy elaboration conversion by removing `expansionToInst` from `MLF.Elab.Pipeline` exports; legacy helper remains in `MLF.Elab.Legacy`, with public client surfaces unchanged (`MLF.API`, `MLF.Pipeline`).
* A1 closure audit: verified strict Ω merge-direction behavior end-to-end (`normalizeInstanceOpsFull` + `normalizeEdgeWitnessesM`) and closed the A1 tracker entries with targeted regression evidence (`R-MERGE-NORM-09`, presolution fail-fast matcher) plus full gate pass (`cabal build all && cabal test`).
* BUG-2026-02-17-001 closure: finished Φ/Ω residual contract fixes by (1) enforcing `resolveTraceBinderTarget` fail-fast invariant errors for trace-source binder ops without replay binder candidates, (2) restoring non-spine `OpRaise` context-path translation for non-`⊥` bounds when `C^m_n` exists, and (3) tightening the `PipelineSpec` canonicalization sentinel to avoid hard-coded non-empty union-find assumptions; full gate is green (`cabal build all && cabal test`, `678 examples, 0 failures`).
* BUG-2026-02-16-010 (partial closure): normalized replay-key contracts end-to-end across presolution traces/hints (`etBinderReplayHints` restore/usage), Φ bridge construction, and Ω binder target lookup; fixed deterministic BUG-002 matrix replay drift (`BUG-002-V1..V4` with seed `1593170056`) and added reify-time self-bound normalization (`a` in its own bound -> `⊥`) to keep scheme bounds well-formed without weakening alias-bound rejection.
* BUG-2026-02-16-001/002: fixed planner-time scheme-owner lookup crash on synthesized-wrapper edge-classification fixtures by making `planEdge` resolve scheme introducers robustly (body-root first, wrapper-root fallback for synthesized `ExpVarId`s only); strengthened `EdgePlannerSpec` let/ann regressions to assert both flag threading and resolved `eprSchemeOwnerGen`.
* BUG-2026-02-11-004: fixed BUG-003 bounded-alias closure by guarding edge-local self-class RaiseMerge emission and skipping same-root bound writes in `MLF.Constraint.Presolution.EdgeUnify`; added `BUG-003-PRES` regression to assert edge-0 presolution leaves no self-bound binder metas, and revalidated `BUG-003-V*` + strict anchors green.
* BUG-2026-02-11-004/BUG-2026-02-16-010 (partial, historical intermediate): added presolution replay-hint metadata (`EdgeTrace.etBinderReplayHints`) and normalization-time hint derivation/validation, plus positional replay seeding in `computeTraceBinderReplayBridge`; strict matrix `make-app` bridge guard is green again.
* BUG-2026-02-11-004 (partial, historical intermediate): added a deterministic Φ→Ω binder bridge (`ocTraceBinderSources` + `ocTraceBinderReplayMap`) and fail-fast replay-contract checks for binder-target ops (`OpGraft` target, `OpWeaken`, `OpRaise` execution target, `OpMerge`, `OpRaiseMerge`); missing source→replay mappings now raise `PhiInvariantError` with edge/op/key-space diagnostics, with new focused regression coverage in `test/ElaborationSpec.hs`.
* BUG-2026-02-16-007/008: fixed BUG-003 sentinel drift from `SchemeFreeVars (__rigid24)` by aligning pipeline/result-type generalization fallback with elaboration fallback (`SchemeFreeVars` + `BindingTreeError GenSchemeFreeVars` both retry through non-GA and final `reifyType` fallback); BUG-003 sentinels now track the stabilized strict-instantiation bucket (`InstBot expects TBottom`) instead of transient scope-closure noise.
* BUG-2026-02-16-009: fixed explicit-forall let-bound round-trip failure by adding a non-spine `OpRaise` source-target fallback in `MLF.Elab.Phi.Omega`; Ω still prefers copy-map adopted targets, but when adopted-target context/root insertion is unavailable it retries root insertion with the source-domain raise target, eliminating `OpRaise (non-spine): missing computation context` while keeping BUG-004/BUG-002 anchors green.
* BUG-2026-02-16-003/004/005/006: fixed `id id` let-polymorphism regression by stopping `AAppF.argInstFromFun` from inlining inferred meta-var instantiation arguments (`inlineBoundVarsType`); this preserves the solver-selected instantiation variable and prevents argument-side over-specialization (`TCArgumentMismatch`) while restoring redirected let-use, checked-authoritative replay, dual-instantiation elaboration, and paper baseline `let id = (\\x. x) in id id`.
* BUG-2026-02-11-003 (2026-02-12 update): corrected instantiation production for BUG-004-V2/V4 — tightened Omega.hs bare `InstBot` to require `TBottom` input, collapsed trivially bounded foralls in ALamF, and normalized argument instantiation to `InstElim`/`InstId` when annotation already updated the bound; strict `InstBot` checker semantics are unchanged; added 3 strict InstBot regression tests (`652 examples, 0 failures`).
* BUG-2026-02-11-003: completed thesis-exact closure for BUG-004 nested-annotation variants by enforcing strict-only `InstBot` semantics (compat mode removed), deleting non-thesis reify fallback instantiation synthesis, normalizing bounded inferred argument instantiation (`InstApp τ` → `InstElim` when argument is already `∀(⩾ τ)`), and replacing bounded-identity lambda-annotation recovery with explicit coercion-domain form matching; `BUG-004-V2/V4` (including strict-shadow checks) are green in checked and unchecked pipelines.
* Tests/bug-tracking: added systematic variant matrix coverage in `test/ElaborationSpec.hs` (`BUG-002-V*`, `BUG-003-V*`, `BUG-004-V*`) and `test/Presolution/WitnessSpec.hs` (`US-010-V*`); confirmed new regressions on extended factory/bounded-alias/annotated-call-site paths and opened `BUG-2026-02-11-002`..`004` in `Bugs.md` with sentinel assertions and repro commands.
* Tests/bug-tracking: hardened `Phase 3 atomic wrapping equivalence gates` by removing permissive mismatch fallback and enforcing explicit `forall a. a -> a` identity-shape assertions; revalidated targeted bug suites plus full gate (`633 examples, 0 failures`) and synced `BUG-2026-02-06-002`/`BUG-2026-02-08-004` to resolved in `Bugs.md`.
* Cleanup: removed the single-constructor `EdgeStage` phantom index and `edgePlanStage`; `EdgePlan` is now a concrete resolved record with simplified planner/interpreter signatures, with no behavior change.
* Phase 6 bridge-removal polish: removed the dedicated synthesized-wrapper interpreter bridge function in favor of a unified TyExp expansion execution path, added wrapper-identity characterization coverage, and re-validated Phase-3 equivalence gates plus full suite (`631 examples, 0 failures`).
* Phase 5 abstraction polish: removed runtime edge-plan mode tagging in favor of refined `ResolvedTyExp` payloads, replaced planner TyExp-left invariant strings with structured `ExpectedTyExpLeftInPlanner` errors, centralized synthesized-wrapper `ExpVarId` allocation/checks in `MLF.Constraint.Types.SynthesizedExpVar`, and kept gate verification green.
* Phase 4 typed-two-pass closeout: introduced phase-tagged presolution errors (`PlanError`/`ExecError`), added regression-matrix coverage across Expansion/EdgeTrace/Witness/Pipeline specs (including annotation-edge weaken suppression), and verified full gate green (`cabal build all && cabal test` => `630 examples, 0 failures`).
* Phase 3 typed-two-pass rollout: restored wrapping equivalence by reserving negative `ExpVarId`s for synthesized wrappers, dispatching wrapper semantics via `ExpVarId < 0`, and adding a Phi order-key fallback to avoid false `PhiReorder` invariant failures under wrapped edge shape; gate suite + full test matrix are green (`626 examples, 0 failures`).
* Witness normalization/translatability: enforced Fig. 15.3.4 transitive-flex guard for non-rigid `OpRaise` in `WitnessValidation`; added dedicated direct + presolution-path regressions, and updated merge-emission coverage to assert fast failure on non-translatable escaped bounded-binder raises.
* Witness matrix closure: completed Fig. 15.3.4 15-row normalization/emission matrix (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) with explicit row-labeled tests across `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs`; matrix gate (`--match R-`) and full gate (`cabal build all && cabal test`) are green.
* BUG-2026-02-06-002: completed thesis-exact upstream graft/weaken closure (witness canonicalization + Ω localization + named-bound simplification guard + let-lambda fallback harmonization); strict bug matrix and full gate are green (`cabal build all && cabal test` => `604 examples, 0 failures`).
* BUG-2026-02-08-004: fixed nested let + annotated-lambda checked-authoritative path in `MLF.Elab.Elaborate` by guarding `InstApp` against non-∀ function terms and by extending polymorphic-argument instantiation inference to typed post-instantiation function arrows; dedicated `PipelineSpec` sentinel now asserts thesis-green `Int` for unchecked + checked pipeline.
* Witness normalization: condition-(5) delayed-weaken ordering now reports a dedicated `DelayedWeakenViolation` instead of overloading `OpUnderRigid`; added focused witness-spec regressions for delayed-weaken violation and delayed graft/weaken coalescing behavior.
* BUG-2026-02-06-002: retained C18/C21/C21.1 elaboration-path fixes (`Phi.Omega` delayed graft/weaken handling + `Elaborate` let-scheme/app-inst repair), graduated the 4-case sentinel matrix to strict assertions, and brought the full `BUG-2026-02-06-002` matcher to green (`10 examples, 0 failures`).
* Elaboration/typecheck: fixed H15 lambda-parameter source selection in `MLF.Elab.Elaborate` so unannotated nested lambdas no longer leak solved-node names (e.g. `t23`) into let-RHS types; added `PipelineSpec` regression `does not leak solved-node names in make let mismatch`.
* Planning/docs: selected Option 1 (upstream witness-shape correction) for `BUG-2026-02-06-002`, added design/execution plans under `docs/plans/2026-02-09-*witness-shape-correction*`, and kept the 4-case bug sentinel matrix explicitly pending until strict closure tests are green.
* Binding/docs: documented A7 Group 1 shared-helper consolidation; duplicated binding path/node-ref/scope/children helpers are canonicalized in `MLF.Binding.Path`, `MLF.Binding.NodeRefs`, `MLF.Binding.ScopeGraph`, and `MLF.Binding.Children`, with migrations in `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, `MLF.Binding.Canonicalization`, `MLF.Constraint.BindingUtil`, and `MLF.Constraint.Presolution.Base`.
* Tests: finalized A7 shared harness dedup by centralizing shared pipeline test helpers in `test/SpecUtil.hs` and removing duplicate helper paths across `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` without behavior changes.
* Tests/docs: `PipelineSpec` rewrite regression now exercises both `applyRedirectsToAnn` and `canonicalizeAnn` so every node occurrence (including `ALet` scheme roots) is rewritten, and `docs/plans/2026-02-08-a7-group-2-frontend-elab-abstractions-implementation-plan.md` now lists a single Hspec filter that hits Pipeline/Phase 1/Phase 6 examples.
* Frontend syntax/pretty: consolidated raw and normalized frontend type syntax into indexed `SrcTy` (`SrcNorm`, `SrcTopVar`, `SrcBound`) with compatibility aliases (`SrcType`, `NormSrcType`, `StructBound`), and generalized pretty entry points to staged types (`prettyEmlfType :: SrcTy n v -> String`, `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`).
* Frontend/elab abstractions: deduplicated frontend scope-wiring in `ConstraintGen.Translate` via local helpers (`withScopedBuild`, `attachUnder`, `rebindScopeRoot`) and centralized AnnExpr node traversal in `MLF.Elab.Run.Annotation.mapAnnNodes` (reused by redirect/canonicalization/debug origin utilities).
* Frontend/pipeline: introduced staged raw vs normalized frontend types and parser entrypoints (`parseRaw*`/`parseNorm*`), and made desugaring/constraint generation/pipeline entrypoints normalized-input only.
* Frontend/normalization: removed the reachable `normalizeBound` runtime crash path by reporting `NonStructuralBoundInStructContext`, and completed the parser clean break by removing legacy `parseEmlf*` compatibility aliases.
* Presolution/elaboration: RaiseMerge gating now uses live structural graph state (no binder-bound snapshots), preserving witness normalization/translatability invariants while restoring bounded aliasing baseline elaboration (`∀a. a -> a -> a`) in both checked and unchecked pipelines.
* Presolution: witness normalization is strict for malformed merge direction (`MergeDirectionInvalid`) across helper and production paths; permissive merge-direction fallback was removed entirely.
* Elaboration/generalization: fixed a Phase 6 `MissingNode` crash by guarding base-constraint reification against stale `solvedToBasePref` targets in `reifyWithGaBase`; when the base node is absent, elaboration now falls back to solved-order reification.
* Tests: added `BUG-2026-02-06-001` regression coverage to ensure nested let + annotated-lambda application no longer fails in Phase 6.
* Pipeline: `runPipelineElab` now reports the checked type (`typeCheck term`) as authoritative; reconstructed result-type paths are retained for diagnostics.
* Generalize: solved-order is runtime-authoritative in fallback reification; runtime base-path shadow reify/compare was removed after the 5/5 gate pass.
* Generalize shadow comparator: solved/base mismatch still hard-fails with `ValidationFailed` plus context payload in focused comparator helpers/tests.
* Pipeline: when root generalization yields no binders but the elaborated term remains type-open, top-level closure now quantifies checked free type variables before final type checking.
* Elaboration: shared closure logic now alpha-freshens wrapper binders against nested `ETyAbs` binders and rewrites free type-variable occurrences in term-level types/instantiations to avoid capture.
* Annotation elaboration: annotation-bound alignment now prefers generalized annotation bounds when shaping `InstInside (InstBot ...)` updates.
* Tests: strict checked-authoritative baselines were updated (including top-level `ETyAbs` wrappers and authoritative `Bool` result cases), with bounded aliasing Merge/RaiseMerge still tracked as a known expected-failure gap.
* Frontend: added eMLF parser + pretty-printer (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`) and public API entry points (`parseRawEmlfExpr`, `parseRawEmlfType`, `parseNormEmlfExpr`, `parseNormEmlfType`, `prettyEmlfExpr`, `prettyEmlfType`).
* xMLF syntax: added dedicated parser/pretty modules (`MLF.XMLF.Syntax`, `MLF.XMLF.Parse`, `MLF.XMLF.Pretty`) and new public module `MLF.XMLF`.
* Pretty migration: `MLF.Elab.Types.Pretty` now renders through canonical xMLF syntax printers (Unicode-first, canonical computation forms such as `ε`, `⊲σ`, `α⊳`), with tests updated accordingly.
* Docs/tests: added `docs/syntax.md` as syntax source of truth and added parser/pretty coverage specs (`FrontendParseSpec`, `FrontendPrettySpec`, `XMLFParseSpec`, `XMLFPrettySpec`).

## 0.2.0.0 -- 2026-02-02

* Breaking: pipeline entry points now return `PipelineError` and provide config-aware variants (`runPipelineElabWithConfig`, `runPipelineElabCheckedWithConfig`).
* Breaking: tracing is fully explicit (no global trace config); `PipelineConfig`/`TraceConfig` are part of the public API.
* Breaking: constraint types are split into `MLF.Constraint.Types.Graph`, `MLF.Constraint.Types.Witness`, and `MLF.Constraint.Types.Presolution` (imports updated accordingly).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
- single-sourced the result-type bound-overlay `PresolutionView` query path through `MLF.Elab.Run.ResultType.View` and removed the duplicate fallback-local overlay rebuild.
- narrowed `MLF.Elab.Run.ResultType.View` to overlay-aware queries only and removed the remaining pass-through accessor scaffolding from result-type runtime call sites.
- retire the thin `EdgeProcessing.Witness` wrapper so per-edge witness/trace assembly is single-sourced under `MLF.Constraint.Presolution.Witness`;
- single-sourced annotated result-type recursion through the `ResultType` facade and retired the fallback-local `computeResultTypeFromAnnLocal` workaround.
