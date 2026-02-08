# MLF Type Inference & Elaboration — TODO

See [roadmap.md](roadmap.md) for the full algorithm description and paper references (especially `papers/these-finale-english.txt`; see also `papers/xmlf.txt`).

---

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
- [ ] `A6 (P2)` Add thesis-anchored witness normalization/translatability regression fixtures.

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

---

## Stretch Goals / Future Work

- [x] Pretty-printer for xMLF terms
- [x] Parser + pretty-printer for eMLF surface syntax (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`)
- [x] Parser + pretty-printer for paper-faithful xMLF syntax (`MLF.XMLF.Parse`, `MLF.XMLF.Pretty`)
- [x] Canonical syntax docs (`docs/syntax.md`) and parser/pretty test coverage
- [ ] Push toward removing the Legacy syntax (keep parser compatibility transition-only; remove legacy pretty forms from internal/debug paths)
- [ ] Visualization of constraint graph (Graphviz / DOT)
- [ ] REPL that prints the inferred type and the elaborated xMLF term
