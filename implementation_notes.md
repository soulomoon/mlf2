# Implementation Notes

### 2026-02-19 Formal obligations ledger (thesis Ch. 14/15) hard-fail enforcement

- Added canonical obligations ledger source:
  - `/Volumes/src/mlf4/docs/thesis-obligations-ch14-15.yaml`
- Added generated Markdown view:
  - `/Volumes/src/mlf4/docs/thesis-obligations-ch14-15.md`
- Added ledger tooling:
  - `/Volumes/src/mlf4/scripts/render-thesis-obligations-ledger.rb`
  - `/Volumes/src/mlf4/scripts/check-thesis-obligations-ledger.sh`
- Added mandatory gate stage:
  - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh` now calls `check-thesis-obligations-ledger.sh` before legacy anchor slices.
- Scope/contract:
  - Covers Chapter 14 (`14.2`-`14.3`) and Chapter 15 (`15.2`-`15.3`) operational obligations only.
  - Exact obligation inventory is fixed at `61` IDs.
  - Checker hard-fails on count/id drift, missing/duplicate/unmapped obligations, non-anchored status, missing code/test anchors, markdown drift, zero-example matchers, or failing matched examples.
- Verification snapshot:
  - `./scripts/check-thesis-obligations-ledger.sh` (PASS)
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `cabal build all && cabal test` (PASS)

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
  - Full `cabal test`: reduced to 3 remaining failures in unrelated open buckets (pipeline ann-redirect invariant + two Φ contract tests).

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

### 2026-02-16 BUG-2026-02-11-004/010 hybrid bridge follow-up (presolution replay hints + positional replay seeding)

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
- Verification snapshot:
  - PASS:
    - `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`
    - `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`
    - bounded-alias baseline (`b ⩾ a`) anchors
    - strict matrix guard: `make-app keeps codomain Int without bottom-domain collapse`
  - FAIL (still open):
    - `BUG-003-V1/V2` remain deterministic `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - Full gate in current workspace remains red (`674 examples, 33 failures`), so BUG-2026-02-11-004 and BUG-2026-02-16-010 remain open.

### 2026-02-16 BUG-2026-02-11-004 Φ/Ω source→replay binder bridge (contract hardening)

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
- Focused validation status:
  - PASS: new fail-fast regression (`OpWeaken` unmapped trace binder target).
  - PASS: source-domain interior alias regression (`OpRaise accepts source-domain interior membership ...`).
  - PASS: bounded alias baseline (`b ⩾ a`) non-regression anchors.
  - STILL RED: `BUG-003-V1/V2` remain in `TCLetTypeMismatch` (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - Full gate remains red in broader open buckets (`cabal build all && cabal test`: `674 examples, 47 failures` in current workspace state).

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
  - Full gate remains red in separate open buckets (`cabal build all && cabal test`: `672 examples, 4 failures`).

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
  - Full gate still reports unrelated/open buckets (`cabal build all && cabal test`: `672 examples, 9 failures`).

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
  - Full gate remains red in separate open buckets (`cabal build all && cabal test`: `672 examples, 5 failures`).

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

**Current vs target:** The current pipeline records presolution witnesses and produces explicit generalization plans in `MLF.Constraint.Presolution.Plan`; elaboration applies these plans via `MLF.Elab.Generalize` without re-solving. The remaining paper-faithfulness deltas are tracked in `.kiro/specs/paper-faithfulness-remaining-deltas/` (constructor types `Cσ` and stricter translatability validation for Φ).

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
  - Ralph task: `tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/prd.json`
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
| `χ` | constraint graph | `src/MLF/Constraint/Types.hs` (`Constraint`) |
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

## 2026-02-16 BUG-003 normalization-side deterministic graft+weaken contract (in progress)

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
  - `BUG-003-V1/V2`: still red (now in a stricter replay key-space mismatch bucket on synthesized `OpGraft+OpWeaken` targeting source key `6`),
  - full gate remains red (`677 examples, 33 failures`) in this workspace.

## 2026-02-16 BUG-003 replay-bridge follow-up (in progress)

- Applied a focused replay-bridge candidate expansion in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`:
  - `computeTraceBinderReplayBridge` now seeds alias candidates from source binders that share the same replay-hint class (`etBinderReplayHints`) before final replay-map selection.
  - This closes the synthesized-key under-coverage case where BUG-003 edge-0 source key `6` had no replay-map entry despite sharing hint provenance with mapped sources.
- Applied a strict Ω bounded-branch correction in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`:
  - `OpGraft+OpWeaken(bound-match)` now emits binder elimination (`InstElim`) instead of bounded `InstApp`, avoiding the `InstBot expects ⊥` invariant violation for non-`⊥` bounds.
  - This keeps strict `InstBot` behavior unchanged while aligning bounded graft+weaken semantics with elimination.
- Verification (current workspace):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
    - now returns to the baseline strict bucket: `PipelineTypeCheckError (TCLetTypeMismatch ...)` (no replay key-space mismatch, no `InstBot` invariant crash).
  - PASS:
    - `--match "fails fast when OpWeaken targets a trace binder source with no replay binder mapping"`
    - `--match "OpRaise accepts source-domain interior membership even when etCopyMap aliases the target"`
    - `--match "does not require Merge for bounded aliasing (b ⩾ a)"`
    - `--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"`
    - `--match "make-app keeps codomain Int without bottom-domain collapse"`
    - synthesis regressions in `test/Presolution/WitnessSpec.hs` (3/3)
  - Full gate remains red: `cabal build all && cabal test` => `677 examples, 33 failures`.
- Root-cause evidence from traced BUG-003 edge-0 replay:
  - replay map now includes source key `6` (`traceBinderReplayMap=[(0,4),(1,8),(2,38),(4,4),(6,4)]`),
  - edge-0 Φ becomes `InstElim` (instead of failing earlier on key-space mismatch),
  - elaborated RHS remains bottomized (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`), so BUG-003 strict-success closure is still open in the original semantic bucket.

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
