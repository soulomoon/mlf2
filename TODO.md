# MLF Type Inference & Elaboration — TODO

See [roadmap.md](roadmap.md) for the full algorithm description and paper references (especially `papers/these-finale-english.txt`; see also `papers/xmlf.txt`).

---

## Task 17 BUG-2026-02-17-001 Φ keep-key + Graft/Raise/Weaken drift — 2026-02-17

- Root cause (deterministic seed `529747475`):
  - `computeTargetBinderKeys` retained replay keys even when target binders were empty.
  - Unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples in Ω expanded into over-complex instantiations that bottomized identity-shaped baselines.
  - Annotation baselines also needed localized fallback when `AAnnF` sees `inst == InstId` with an expected bound for non-variable annotation sources.
- Implemented fixes:
  - `MLF.Elab.Phi.Translate`: strict keep-key intersection with actual target binders.
  - `MLF.Elab.Phi.Omega`:
    - preserve spine Raise alias/eliminate behavior in empty-context cases,
    - collapse unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples to direct `InstApp`,
    - refine spine Raise bound handling for inferred-variable cases.
  - `MLF.Elab.Elaborate` (`AAnnF`):
    - keep generic non-var fallback disabled in `reifyInst`,
    - add non-variable annotation-local fallback from `InstId` to `InstInside (InstBot expectedBound)`.
- Verification (green):
  - `id y should have type`, `elaborates polymorphic instantiation`, `elaborates term annotations`,
    `term annotation can instantiate a polymorphic result`, `explicit forall annotation preserves foralls in bounds`.
  - `BUG-002-V` (`seed 1593170056`), `BUG-003-V` (`seed 1925916871`), `BUG-004` (`seed 1593170056`), OpRaise interior guard repro.
  - `cabal build all` passes.
- Full-gate closure:
  - residual failures were closed with:
    - `MLF.Elab.Phi.Omega.resolveTraceBinderTarget` fail-fast invariant for trace-source binder ops with no replay binder candidate,
    - non-spine `OpRaise` context-path translation for non-`⊥` bounds when `C^m_n` is available,
    - `PipelineSpec` canonicalization assertion tightened to require non-empty rewrites only when solve `union-find` is non-empty.
  - verification: `cabal build all && cabal test` passes (`678 examples, 0 failures`).

## Task 16 BUG-2026-02-11-004 regression reopen (bounded-alias strict-success) — 2026-02-17

- Targeted closure landed:
  - `MLF.Elab.Phi.Translate`: trace-free `computeTargetBinderKeys` now returns empty keep-key set (`mTrace = Nothing`), restoring StepIntro + weaken interleaving behavior.
  - `MLF.Elab.Elaborate`: removed variable-annotation conversion `InstInside (InstBot t) -> InstApp t` in `AAnnF`; this was producing `InstApp TBottom` for BUG-003 use sites.
- Current targeted matrix (seed `1925916871`) is green:
  - `BUG-003-V` (V1/V2) => PASS
  - `BUG-003-PRES` => PASS
  - `interleaves StepIntro with Omega ops in Φ translation` => PASS
  - `BUG-004` (V1..V4) => PASS
- Follow-up closure:
  - full gate is green (`cabal build all && cabal test`), and BUG tracker entries are closed in `/Volumes/src/mlf4/Bugs.md`.

## Task 15 BUG-2026-02-16-001/002 planner scheme-introducer crash closure — 2026-02-17

- Root cause:
  - `planEdge` resolved `eprSchemeOwnerGen` strictly from TyExp body root.
  - For synthesized-wrapper fixtures (`ExpVarId < 0`) with sparse bind-parent topology, body root had no direct `GenRef` ancestor while wrapper root did, causing:
    - `InternalError "scheme introducer not found for NodeId {getNodeId = 0}"`.
- Implemented fix:
  - `MLF.Constraint.Presolution.EdgeProcessing.Planner` now uses `resolveSchemeOwnerGen`:
    - non-synth TyExp path remains strict (`findSchemeIntroducerM` on body root),
    - synthesized-wrapper path falls back from body-root lookup to wrapper-root lookup.
  - Added helper `firstGenOnPath` (via `bindingPathToRootUnderM`).
  - `test/Presolution/EdgePlannerSpec.hs` let/ann bug repros now also assert `eprSchemeOwnerGen == GenNodeId 0`.
- Verification (green):
  - `--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064`
  - `--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064`
  - `--match "Edge plan types" --seed 1481579064`
  - `--match "Edge interpreter" --seed 1481579064`
- Tracker sync:
  - moved `BUG-2026-02-16-001` and `BUG-2026-02-16-002` to **Resolved** in `/Volumes/src/mlf4/Bugs.md`.

## Task 14 BUG-2026-02-16-010 bridge-domain regression follow-up — 2026-02-16

- Bridge hardening + replay-hint/positional replay seeding landed for Φ→Ω binder-target dispatch.
- Progress update:
  - strict matrix `make-app keeps codomain Int without bottom-domain collapse` is green again.
  - full gate still shows remaining replay-domain under-coverage in other phase-6 matrix paths (`BUG-002-V2`, `BUG-004-V2`, etc.).
- Immediate follow-up priorities:
  - characterize replay-map under-coverage cases (`traceBinderSources` includes keys that are semantically valid targets but absent from replay-map domain);
  - refine bridge-map construction/eligibility so fail-fast only fires on true contract violations;
  - keep BUG-003 bridge contract test and source-domain `OpRaise` interior alias regression green while restoring remaining affected anchors.
- Verification target:
  - green targeted matrix for BUG-002/BUG-004 + strict target matrix;
  - then rerun full gate: `cabal build all && cabal test`.
- 2026-02-17 investigation update (systematic-debugging, BUG-002):
  - deterministic slice remains red (`5 examples, 4 failures`) with buckets: `BUG-002-V1` spine mismatch, `BUG-002-V2/V3` replay key-space mismatch, and `PipelineSpec BUG-002-V4` interior guard regression.
  - root mismatch is cross-domain: presolution trace/hint identity space vs replay substitution key-space used by Ω binder lookup.
  - four minimal bridge-map hypotheses were tested and reverted; next step should be an architecture-level replay-domain normalization pass before additional local fixes.
- 2026-02-17 completion update:
  - replay-key normalization contract landed across trace/hint restoration, bridge construction, and Ω binder lookup.
  - deterministic `BUG-002-V` slice is now green with seed `1593170056` (`5 examples, 0 failures`).
  - follow-up bound normalization in `ReifyPlan` now rewrites binder self-references in bounds to `⊥`, removing residual V2 alias-finalization drift while keeping strict alias-bound rejection.
  - residual cross-link remains: `BUG-004-V2` is still red in this workspace (`TCArgumentMismatch`), so BUG-2026-02-16-010 stays open as a narrowed follow-up.

## Task 13 BUG-2026-02-16-007/008 sentinel drift closure — 2026-02-16

- Implemented generalization fallback alignment for plain `SchemeFreeVars`:
  - `MLF.Elab.Run.Pipeline` root generalization now retries `SchemeFreeVars` the same as `BindingTreeError GenSchemeFreeVars` and falls back to direct reification.
  - `MLF.Elab.Run.ResultType.Util.generalizeWithPlan` now uses the same fallback policy.
- Updated BUG-003-V1/V2 sentinels in `test/ElaborationSpec.hs` to track the stabilized strict-instantiation failure class (`InstBot expects TBottom`) rather than transient `SchemeFreeVars (__rigid24)`.
- Targeted verification (green):
  - exact BUG-003-V1 repro command (seed `1481579064`)
  - exact BUG-003-V2 repro command (seed `1481579064`)
  - `--match "BUG-003-V"` matrix slice (`2 examples, 0 failures`)
- Tracker sync:
  - `BUG-2026-02-16-007` and `BUG-2026-02-16-008` moved to resolved in `/Volumes/src/mlf4/Bugs.md`.
  - `BUG-2026-02-11-004` remains open as the underlying bounded-alias thesis-faithfulness gap.

## Task 11 Source-domain `I(r)` closure — 2026-02-16 (BUG-2026-02-14-003)

- Implemented surgical Ω/Φ domain contract fix:
  - `MLF.Elab.Phi.Omega`: `OpRaise` admissibility checks now use source-domain `etInterior` directly.
  - Added invariant diagnostic for alias-only membership (`source target ∉ I(r)` but copy-map alias ∈ `I(r)`).
  - `OpRaise` semantic execution now adopts source->copied node (`etCopyMap`) before canonicalization.
  - `MLF.Elab.Phi.Translate`: canonicalize `etInterior` only for `namedSet` intersection.
- Added regressions:
  - `ElaborationSpec`: source-domain `OpRaise` + copy-map alias no longer fails `outside I(r)`.
  - `PipelineSpec`: BUG-002-V4 OpRaise targets remain members of `etInterior` after witness/trace canonicalization.
- Targeted verification (all green):
  - `BUG-002-V4`
  - `BUG-2026-02-06-002 strict target matrix`
  - `BUG-004`
  - `tracks instantiation copy maps for named binders`
  - `witness/trace/expansion canonicalization`
- Full gate:
  - `cabal build all && cabal test` => `672 examples, 9 failures` (open buckets outside this surgical scope).

## Task 12 Non-spine OpRaise context fallback — 2026-02-16 (BUG-2026-02-16-009)

- Implemented targeted Φ/Ω non-spine context fix:
  - `MLF.Elab.Phi.Omega`: `OpRaise` now tracks both adopted and source-domain raise targets.
  - When adopted-target non-spine context/root insertion is unavailable, Ω retries root-context insertion using the source-domain raise target.
  - Adopted-target path remains primary to preserve previously fixed BUG-004 call-site behavior.
- Verification (green):
  - `/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Explicit forall annotation edge cases/explicit forall annotation round-trips on let-bound variables/`
  - `BUG-004`
  - `BUG-002-V4`
  - `BUG-2026-02-06-002 strict target matrix`
  - `contextToNodeBound does not descend through forall body fallback`
- Full gate:
  - `cabal build all && cabal test` => `672 examples, 4 failures` (historical snapshot before BUG-007/008 closure; current open trackers center on `BUG-2026-02-16-001/002` and umbrella `BUG-2026-02-11-004`).

## Task 10 Variant Matrix Scan — 2026-02-11 (new bug variants triage)

- Added systematic variant coverage:
  - `test/ElaborationSpec.hs` (`BUG-002-V1..V4`, `BUG-003-V1..V2`, `BUG-004-V1..V4`)
  - `test/Presolution/WitnessSpec.hs` (`US-010-V1..V2`)
- Targeted validation:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Systematic bug variants (2026-02-11 matrix)"'` (`10 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "US-010-V"'` (`2 examples, 0 failures`)
- Outcome:
  - Open regressions currently tracked:
    - `BUG-2026-02-11-002` (extended BUG-002 factory variants)
    - `BUG-2026-02-11-004` (BUG-003 higher-arity bounded-alias variants)
  - `BUG-2026-02-11-003` is resolved thesis-exact: `BUG-004-V2`/`V4` are strict success assertions (`Int`) in both checked and unchecked pipelines, with compat InstBot and reify fallback paths removed.
  - Remaining variant tests for open bugs stay sentinel-guarded until strict expected-success closure.

## Task 9 Verification Gate — 2026-02-11 (Phase-3 gate hardening + tracker sync)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`633 examples, 0 failures`)
- Closure summary:
  - Hardened `Phase 3 atomic wrapping equivalence gates` in `test/PipelineSpec.hs`:
    - removed permissive checked mismatch fallback for `make` path
    - enforced explicit `forall a. a -> a` identity-arrow shape for `\y. let id = (\x. x) in id y`
  - Revalidated targeted suites:
    - `--match "Phase 3 atomic wrapping equivalence gates"` (`7 examples, 0 failures`)
    - `--match "BUG-2026-02-06-002"` (`10 examples, 0 failures`)
    - `--match "BUG-2026-02-08-004"` (`1 example, 0 failures`)
  - Synced bug tracker status:
    - `BUG-2026-02-06-002` and `BUG-2026-02-08-004` marked **Resolved** in `/Volumes/src/mlf4/Bugs.md`
    - open bug list currently empty

## Task 7 Verification Gate — 2026-02-10 (BUG-2026-02-06-002 closure)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`604 examples, 0 failures`)
- Closure summary:
  - Upstream witness-shape correction finalized in `MLF.Constraint.Presolution.WitnessCanon` (ambiguous graft/weaken rejection + delayed pair coalescing).
  - Ω translation made local in `MLF.Elab.Phi.Omega` (no delayed look-ahead; adjacent graft/weaken rescue only).
  - Named structured-bound preservation corrected in `MLF.Constraint.Presolution.Plan.Normalize`.
  - ALet fallback harmonized in `MLF.Elab.Elaborate` (lam replacement schemes use `IntMap.empty` substitution).
- Follow-up:
  - `BUG-2026-02-06-002` moves to resolved tracking (`/Volumes/src/mlf4/Bugs.md`).

## Task 8 Verification Gate — 2026-02-10 (BUG-2026-02-08-004 closure)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`604 examples, 0 failures`)
- Closure summary:
  - Dedicated `BUG-2026-02-08-004` sentinel in `test/PipelineSpec.hs` now asserts thesis-expected success (`Int`) in both unchecked and checked pipelines.
  - `MLF.Elab.Elaborate` `AApp` now suppresses witness-derived `InstApp` when the elaborated function term is not `∀`-typed, preventing invalid `InstElim` chains on monomorphic arrows.
  - Polymorphic argument-instantiation inference now also runs for variable arguments when the (possibly instantiated) function term typechecks to an arrow, preserving κσ intent for annotated lambda parameters.
- Follow-up:
  - `BUG-2026-02-08-004` moved to **Resolved** in `/Volumes/src/mlf4/Bugs.md`.

## Task 7 Verification Gate — 2026-02-08

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (post-fix 5/5 consecutive green after `cb5e51d`)
- Historical context (pre-fix, before `cb5e51d`):
  - Baseline full verification: **FAIL** (`506 examples, 4 failures`; Phase 6 `shadow reify mismatch`)
  - Consecutive tracking:
    - `1/5` — **FAIL** (2026-02-08T07:42:32Z, 2s): `ValidationFailed ["shadow reify mismatch", ... "solved=t14 -> t14", "base=a -> a"]`
    - `2/5` — **FAIL** (2026-02-08T07:42:42Z, 3s): same failure signature
    - `3/5` — **FAIL** (2026-02-08T07:42:51Z, 3s): same failure signature
    - `4/5` — **FAIL** (2026-02-08T07:43:03Z, 3s): same failure signature
    - `5/5` — **FAIL** (2026-02-08T07:43:13Z, 3s): same failure signature
- Post-fix revalidation (exact timestamps, local timezone `+0800`):
  - `1/5` — **PASS** (start `2026-02-08T16:17:43+0800`, end `2026-02-08T16:17:46+0800`)
  - `2/5` — **PASS** (start `2026-02-08T16:17:46+0800`, end `2026-02-08T16:17:49+0800`)
  - `3/5` — **PASS** (start `2026-02-08T16:17:49+0800`, end `2026-02-08T16:17:52+0800`)
  - `4/5` — **PASS** (start `2026-02-08T16:17:52+0800`, end `2026-02-08T16:17:55+0800`)
  - `5/5` — **PASS** (start `2026-02-08T16:17:55+0800`, end `2026-02-08T16:17:58+0800`)
- Follow-up:
  - `BUG-2026-02-08-001` stays in `Bugs.md` under **Resolved**.
  - Runtime base-shadow cutover completed: `MLF.Elab.Generalize` fallback no longer reifies or compares base-path output at runtime; solved-order output is authoritative.
  - `BUG-2026-02-06-002` remains open: current debugging indicates binder-representative filtering in generalization can drop required factory binders (`make` path), causing scheme specialization drift or `SchemeFreeVars`.

## Task 7 Verification Gate — 2026-02-09 (H15)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (H15 guard + regression test)
- Scope:
  - Added targeted regression `PipelineSpec` case for solved-name leakage (`t23`) in the `make` let-mismatch path.
  - Implemented guarded ALam parameter-source selection in `MLF.Elab.Elaborate` (`hasInformativeVarBound`) to avoid copy-node name leakage while preserving informative resolved-node paths.
- Follow-up:
  - `BUG-2026-02-06-002` remains open for the broader polymorphic-factory behavior (`TBottom -> Int` vs expected `b -> a`) after H13/H14/H15.

## Task 7 Priority Plan — 2026-02-09 (H16 continuation)

- Selected strategy: **Option 1 — Upstream witness-shape correction**.
- Design doc: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-design.md`
- Implementation plan: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-implementation-plan.md`
- Sentinel policy while bug remains open: keep `BUG-2026-02-06-002 sentinel matrix` as pending; drive fix with strict RED matrix + witness/Φ regressions, then graduate sentinels to strict assertions at closure.


---

## Phase 1 — Constraint Generation ✅

- [x] Graphic type nodes (`TyVar`, `TyArrow`, `TyBase`, `TyForall`, `TyExp`)
- [x] Binding nodes (`TyForall`) + binding edges (`Constraint.cBindParents`)
- [x] Expansion nodes (`TyExp`) for let-bindings
- [x] Instantiation edges (`InstEdge`)
- [x] Constraint container (`Constraint`)
- [x] `inferConstraintGraph :: Expr -> Either TypeError ConstraintResult`
- [x] Literals produce `TyBase` nodes
- [x] Lambda parameters bound at surrounding binder
- [x] Applications emit instantiation edges
- [x] Let-bindings introduce child binders (`TyForall`)
- [x] Expansion variables shared across multiple uses of same binding
- [x] Variable shadowing / lexical scoping
- [x] Unknown variable error reporting
- [ ] `A5 (P3)` Totalize STCon coercion-copy path and remove remaining partial failure branch.

**Tests:** 23 examples, all passing (`cabal test`)

---

## Phase 2 — Normalize / Local Transformations ✅

- [x] Simplify trivial instantiation/unification edges (T ≤ T, T = T)
- [x] `normalize :: Constraint -> Constraint` with fixed-point iteration
- [x] `dropReflexiveInstEdges`, `dropReflexiveUnifyEdges` helpers
- [x] Grafting: copy structure onto variables when demanded by `InstEdge`
- [x] Merging: process `UnifyEdge`s via union-find
- [x] `graftInstEdges`, `mergeUnifyEdges` helpers
- [x] `NormalizeState` with fresh node allocation and union-find

**Tests:** 16 examples, all passing

---

## Phase 3 — Acyclicity Check ✅

- [x] Build instantiation dependency graph
- [x] Topological sort of `InstEdge` list
- [x] Cycle detection (DFS)
- [x] `isAcyclic :: Constraint -> Bool`
- [x] `checkAcyclicity :: Constraint -> Either CycleError AcyclicityResult`
- [x] `collectReachableNodes` for dependency analysis
- [x] `AcyclicityResult` with sorted edges and dependency graph

**Tests:** 41 examples (was 27 new, total 92), all passing

---

## Phase 4 — Principal Presolution ✅

- [x] Topological processing of `InstEdge`s using `AcyclicityResult`
- [x] Minimal expansion lattice implemented: `ExpIdentity`, `ExpInstantiate`, `ExpForall`, `ExpCompose`
- [x] `decideMinimalExpansion` covers forall↔forall (re-gen), forall→structure (instantiate), structure→forall (wrap), structure→structure (identity+unify)
- [x] `applyExpansion`/`instantiateScheme` to realize expansions and graft fresh nodes
- [x] Incremental unification inside presolution loop
- [x] Tests: `test/PresolutionSpec.hs` covers identity, instantiate, forall-intro, and compose (instantiate→forall)
- [ ] `A1 (P1)` Strict Ω normalization only (remove permissive fallback path in production).
- [x] `A6 (P2)` Add thesis-anchored witness normalization/translatability regression fixtures.
  - 2026-02-10: added strict transitive-flex `OpRaise` regression fixtures in `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs` (direct validator + `normalizeEdgeWitnessesM` path).
  - 2026-02-10: completed Fig. 15.3.4 15-row witness matrix closure (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) with row-labeled tests and green matrix/full gates.

## Phase 5 — Unification Solver ✅

- [x] `Solve` entrypoint `solveUnify :: Constraint -> Either SolveError SolveResult`
- [x] Robust union-find: reuse canonical NodeId representatives and path compression
- [x] Structural unification cases: Var=Var, Var=Structure, Arrow=Arrow, Base=Base, Forall=Forall
- [x] Occurs check on DAG
- [x] Error reporting
- [x] Tests: success cases and failure cases

---

## Phase 6 — Elaboration to xMLF (New Foundation) ⏳

Based on `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §3.

- [x] **Define xMLF AST** (`src/MLF/Types/Elab.hs`, re-exported via `MLF.Elab.Types`)
    - [x] Types `τ` (including `∀(α ≥ τ)` and `⊥`)
    - [x] Instantiations `φ` (Witnesses: `!α`, `N`, `O`, `Inside`, `Under`, `Comp`)
    - [x] Terms `a` (including `Λ` and `a φ`)
- [x] **Implement Elaboration Logic** (`src/MLF/Elab/Elaborate.hs`)
    - [x] `elaborate` + `runPipelineElab`
    - [x] Generate instantiation witnesses `φ` from per-edge `EdgeWitness` (Φ)
    - [x] Insert `Λ` abstractions based on presolution plans
    - [x] Insert explicit type annotations on lambda arguments
- [x] **Tests**
    - [x] Elaboration basics (id, const) + let-polymorphism
    - [x] Φ/Σ unit tests + instantiation-soundness checks
- [x] `A2 (P1)` Align pipeline-reported result type with checked type as authoritative.
- [ ] `A3 (P2)` Remove legacy helper from public elaboration API surface.
- [ ] `A6 (P2)` Add checked-vs-unchecked elaboration parity tests (incl. US-004-style paths).

---

## Phase 7 — xMLF Execution & Verification ⏳

Based on `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §1 & §2.

- [x] **Type Checker** (`src/MLF/Elab/TypeCheck.hs`)
    - [x] Implement `Γ ⊢ a : τ` rules
    - [x] Verify elaborated terms are well-typed
- [x] **Evaluator** (`src/MLF/Elab/Reduce.hs`)
    - [x] Implement small-step reduction `a ⟶ a'`
    - [x] Implement instantiation reduction rules (e.g., `(Λ...) N ⟶ ...`)
- [ ] `A6 (P2)` Add regression cases ensuring typecheck confirms elaboration parity for bounded/coercion-heavy terms.

---

## Paper-faithfulness deltas (tracked)

- [x] Add constructor types `Cσ` to the xMLF type AST (Fig. 14.2.1).
- [x] Integrate quantifier reordering ϕR when `Typ` vs `Typexp` differ (Def. 15.3.4).
- [x] Enforce translatable-presolution invariants for Φ (explicit `PhiTranslatabilityError` / `PhiInvariantError`; no silent non-spine `OpRaise` fallback).
- [x] Confirm Ω normalization emits Fig. 15.3.4 operations for current coverage; document the remaining US-004 κσ deviation in `test/ElaborationSpec.hs`.
  - 2026-02-10: Fig. 15.3.4 witness matrix closure gate is green (`cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`), covering all 15 row IDs.
- [ ] `A4 (P2)` Refresh paper-faithfulness docs to reflect implemented strict Φ/Σ behavior and list only unresolved deltas.

See `.kiro/specs/paper-faithfulness-remaining-deltas/` for the audit and plan.

---

## Audit Backlog — 2026-02-06

- [ ] `A1 (P1)` Enforce strict Ω normalization in production witness path (no permissive fallback on merge-direction errors).
AC: Presolution witness normalization fails fast on malformed merge direction; no fallback acceptance in production path.
Files: `src/MLF/Constraint/Presolution/WitnessCanon.hs`, `src/MLF/Constraint/Presolution/WitnessNorm.hs`, `test/Presolution/WitnessSpec.hs`

- [x] `A2 (P1)` Make checked type authoritative for pipeline results.
AC: `runPipelineElab` and checked pipeline type outcomes are aligned for known divergence cases (including US-004-style scenarios).
Files: `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/ElaborationSpec.hs`

- [ ] `A3 (P2)` Quarantine legacy elaboration helpers from public API surface.
AC: Legacy conversion helper is not exposed from public pipeline exports used by downstream clients.
Files: `src/MLF/Elab/Pipeline.hs`, `src/MLF/Elab/Legacy.hs`, `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`

- [ ] `A4 (P2)` Sync paper-faithfulness docs/specs with current strict Φ/Σ behavior and remaining true deltas only.
AC: `.kiro` paper-faithfulness status lines reflect current implementation; remaining open items are explicit and non-contradictory.
Files: `.kiro/specs/paper-faithfulness-remaining-deltas/requirements.md`, `.kiro/specs/paper-faithfulness-remaining-deltas/tasks.md`, `implementation_notes.md`

- [ ] `A5 (P3)` Remove remaining totality/harness footguns.
AC: No partial `error` in frontend STCon path; test-suite wiring cannot silently omit presolution umbrella spec.
Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `mlf2.cabal`, `test/Main.hs`

- [ ] `A6 (P2)` Expand thesis-anchored regression matrix for translatability, bounded coercions, and checked-vs-unchecked parity.
AC: Added targeted tests for strict translatability invariants and elaboration/type-check parity; references to thesis anchors included in spec names/comments.
Files: `test/ElaborationSpec.hs`, `test/Presolution/WitnessSpec.hs`, `test/PipelineSpec.hs`, `test/TypeCheckSpec.hs`, `test/ReduceSpec.hs`

- [ ] `A7 (P2)` Consolidate duplicated binding/scope/pipeline helper logic into shared abstractions.
AC: Binding path/children/scope-graph helpers are single-sourced; ConstraintGen scope+binder wiring uses shared combinators; repeated test pipeline harness steps (`unsafeNormalize`, `firstShow`, solve chain) are centralized in shared test utilities.
Files: `src/MLF/Binding/Queries.hs`, `src/MLF/Binding/Validation.hs`, `src/MLF/Binding/Tree.hs`, `src/MLF/Binding/Canonicalization.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run/Annotation.hs`, `src/MLF/Elab/Run/Debug.hs`, `test/SpecUtil.hs`, `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`, `test/ConstraintGenSpec.hs`
Progress (2026-02-08, Group 1): duplicated binding-core helpers are now single-sourced in `MLF.Binding.Path`, `MLF.Binding.NodeRefs`, `MLF.Binding.ScopeGraph`, and `MLF.Binding.Children`; migration landed in `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, `MLF.Binding.Canonicalization`, `MLF.Constraint.BindingUtil`, and `MLF.Constraint.Presolution.Base`. Remaining A7 work is the non-binding portions in the AC (`ConstraintGen`/pipeline test-harness consolidation).

---

## Stretch Goals / Future Work

- [x] Pretty-printer for xMLF terms
- [x] Parser + pretty-printer for eMLF surface syntax (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`)
- [x] Parser + pretty-printer for paper-faithful xMLF syntax (`MLF.XMLF.Parse`, `MLF.XMLF.Pretty`)
- [x] Canonical syntax docs (`docs/syntax.md`) and parser/pretty test coverage
- [ ] Push toward removing the Legacy syntax (keep parser compatibility transition-only; remove legacy pretty forms from internal/debug paths)
- [ ] Visualization of constraint graph (Graphviz / DOT)
- [ ] REPL that prints the inferred type and the elaborated xMLF term

## Active master refactor plan — 2026-02-10

- [x] Execute the master 6-phase typed two-pass edge DSL plan:
  - [x] Phase 1 + Phase 2 landed (typed planner/interpreter two-pass core).
  - [x] Phase 3 Task 7 + Task 8 (wrapping equivalence) are green.
  - [x] Phase 3 Task 9 + Task 10 landed (planner fail-fast + legacy-direct removal).
  - [x] Phase 4 Task 11-14 landed (phase-tagged errors, regression matrix, docs, verification gate).
  - [x] Phase 5 landed (plan type refinement, structured planner invariant errors, synthesized `ExpVarId` boundary module).
  - [x] Phase 6 landed (single unified expansion execution path; synthesized-wrapper bridge function removed).
  - [x] Post-phase cleanup landed (removed single-constructor `EdgeStage` phantom index from `EdgePlan`).
  - [ ] Next: run finishing-branch handoff + integration decision (merge/PR/cleanup).
  - Plan: `docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`
  - Task folder: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/`
  - Scope highlights:
    - Enforce Phase-2 paper-shaped residual instantiation edges (`TyExp <= τ`).
    - Add Phase-4 fail-fast assertion on non-`TyExp` left inst edges.
    - Introduce typed two-pass planner/interpreter for edge processing.
    - Refine plan payload to encode TyExp-left edges directly.
    - Eliminate wrapper-specific interpreter bridge execution while preserving wrapper identity semantics.
    - Add matrix regressions and full verification (`cabal build all && cabal test`).

## Active bug closures

- [x] `BUG-2026-02-06-002`: graduate `BUG-2026-02-06-002 sentinel matrix` from `pendingWith` to strict assertions now that strict/thesis target matrix is green under retained C18/C21/C21.1 behavior.
