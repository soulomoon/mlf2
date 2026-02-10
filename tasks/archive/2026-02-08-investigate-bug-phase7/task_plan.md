# Task Plan: Investigate Phase 7 TCLetTypeMismatch for `make`

## Goal
Trace why `make`'s let-scheme specializes to `... -> Int` while its elaborated RHS remains polymorphic, document likely root causes and fix approaches, and recommend a path forward.

## Current Phase
Phase 7 (execution pending)

## Phases

### Phase 1: Requirements & Discovery
- [x] Understand user intent
- [x] Identify constraints and requirements
- [x] Document findings in findings.md
- **Status:** complete

### Phase 2: Planning & Structure
- [x] Define technical approach
- [x] Create project structure if needed
- [x] Document decisions with rationale
- **Status:** complete

### Phase 3: Implementation
- [x] Execute the plan step by step
- [x] Write code to files before executing
- [x] Test incrementally
- **Status:** complete

### Phase 4: Testing & Verification
- [x] Verify all requirements met
- [x] Document test results in progress.md
- [x] Fix any issues found
- **Status:** complete

### Phase 5: Delivery
- [x] Review all output files
- [x] Ensure deliverables are complete
- [x] Deliver to user
- **Status:** in_progress

## Key Questions
1. Where does let-scheme for `make` acquire a `... -> Int` instantiation while elaborated RHS remains polymorphic?
2. How do GammaPlan/ReifyPlan interplay with generalization/elaboration to enforce scheme vs polymorphic types?

## Decisions Made
| Decision | Rationale |
|----------|-----------|
| Use strict systematic-debugging flow (reproduce → compare patterns → single-hypothesis tests) before any durable fix | Prevents guesswork and isolates causes in a multi-phase pipeline |
| Treat code edits as temporary hypothesis probes and revert unless they improve bug behavior | Keeps workspace stable and avoids compounded regressions |
| Use internal pipeline dumps (`solvedClean` vs `solvedForGen`, `gaSolvedToBase`, trace logs) as primary evidence | The failure emerges from cross-module data-flow, not a single local function |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Catchup script missing ${CLAUDE_PLUGIN_ROOT}/scripts/session-catchup.py | 1 | Directory absent; noted and continuing |
| `cabal exec runghc` intermittently hid package module `MLF.API` | 1 | Switched reproducible runs to `cabal repl lib:mlf2` + `:l script` |
| Internal dump scripts initially failed due hidden modules / missing imports | 1 | Executed via `cabal repl lib:mlf2-internal` and fixed imports |
| Legacy /tmp/repro_bug_current.hs uses removed API helpers (unsafeNormalize, prettyType) | 1 | Replaced with new reproducer using normalizeExpr + runPipelineElabCheckedWithConfig |
| Initial H16 matrix script used polymorphic `Expr s ty` where `normalizeExpr` expects `SurfaceExpr` | 1 | Updated script signatures to `SurfaceExpr` |
| H16.2 temporary `Omega.hs` edit failed build (`find` not in scope) | 1 | Restored `find` import and rebuilt |
| H16.2 binder-arg matcher failed build (predicate typed as `NodeId -> Bool`) | 1 | Corrected matcher to pattern-match tuple `(binder,arg)` |
| H16.2 auxiliary dump script imported hidden internal modules from `lib:mlf2` | 1 | Dropped that script and continued with trace-based evidence via existing repro harness |

## Notes
- Update phase status as you progress: pending → in_progress → complete
- Re-read this plan before major decisions to keep the goal top of mind
- Log ALL errors so we do not repeat the same action

## 2026-02-09 Plan Extension: Upstream Witness-Shape Correction

### Phase 6: Planning for H16 continuation
- [x] Capture selected fix direction from brainstorming
- [x] Produce design document for chosen direction
- [x] Produce executable implementation plan with RED→GREEN gates
- **Status:** complete

### Phase 7: Execute upstream witness-shape correction
- [ ] Run Task 1–Task 8 from implementation plan
- [ ] Keep sentinel policy (pending) until strict matrix is green
- [ ] Graduate sentinels to strict assertions at closure
- **Status:** pending

### Added Decisions
| Decision | Rationale |
|----------|-----------|
| Use Option 1 (upstream witness-shape correction) as primary H16 direction | Local Ω probes (H16.1–H16.4) were insufficient/regressive; evidence points to presolution witness-shape contract |
| Keep existing sentinel matrix pending during active fix development | Prevents buggy shapes from appearing as “passing behavior tests” while still preserving reproducible bug documentation |
| Add separate strict RED matrix for implementation-driving checks | Enables TDD progression without losing pending sentinel policy until final closure |

### Plan Artifacts
- Design: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-design.md`
- Implementation plan: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-implementation-plan.md`

### Phase 7 task-by-task execution log
- [x] Task 1: Add strict RED matrix while keeping sentinel matrix pending
- [x] Task 2: Add witness-shape normalization regression tests (RED)
- [x] Task 3: Add Φ translation regression tests
- [x] Task 4: Implement canonical graft/weaken alignment in `WitnessCanon`
- [ ] Task 5: Thread alignment through `WitnessNorm` environment/wiring
- [ ] Task 6: Keep Ω strict/minimal (compatibility only if needed)
- [ ] Task 7: Graduate pending sentinels to strict passing assertions
- [ ] Task 8: Full verification + docs/changelog/bug tracker closure
- [ ] Task 5: Thread alignment through `WitnessNorm` environment/wiring
  - Investigation pass completed with two reversible probes (rewritten-id retention; unbounded-arg merge substitution); both were reverted due non-improving/regressive outcomes.
  - Requires a new hypothesis before marking complete.

### Phase 7 Execution — Direction matrix implementation plan (2026-02-08) batch 1
- **Status:** complete (Tasks 1-3)

- Completed:
  - Task 1: Added thesis-target RED spec and wired it into `mlf2-test`.
  - Task 2: Added reusable diagnostics + direction runner scripts.
  - Task 3: Added direction matrix template with baseline failure payload.

- Verification summary:
  - `BUG-2026-02-06-002 thesis target` is RED (`2/2` failures, expected baseline).
  - Direction runner baseline produced reproducible artifacts under `tmp/direction-matrix/`.
  - `trace.out` includes required markers: `generalizeAt:`, `elaborate let: scheme=`, `Phase`, `TCLetTypeMismatch`.

- Next:
  - Await review, then proceed to direction experiment tasks (`D1` onward) using one-direction isolation policy.

### Additional execution note (2026-02-10)
- Direction runner test filtering was refined after observing a `0 examples` outcome with combined regex matching.
- Current runner executes explicit per-check matches to keep direction evidence non-empty and comparable.

### Phase 7 Execution — Direction matrix implementation plan (2026-02-08) batch 2
- **Status:** complete (Tasks 4-6)

- Completed:
  - Task 4: D1 experiment executed, evaluated, and reverted (FAIL).
  - Task 5: D2 experiment executed, evaluated, and reverted (FAIL).
  - Task 6: D3 experiment executed, evaluated, and reverted (FAIL).

- Verification summary:
  - For D1/D2/D3, strict thesis target remained RED (`2/2` failures).
  - Focused nearby regressions stayed PASS in all three directions.
  - Matrix notes now include completed rows + result sections for D1-D3.

- Additional implementation note:
  - Direction runner switched to `cabal repl lib:mlf2` execution to avoid intermittent hidden-package failures under `runghc`.

- Next:
  - Await review, then continue with batch 3 (Tasks 7-9 / D4-D6).

### Phase 7 Execution — Upstream witness-shape plan resume (Tasks 5-7)
- **Status:** blocked at Task 7 gate

- Task 5:
  - Attempted a minimal `WitnessNorm` canonical binder-arg alignment patch.
  - Outcome: no strict matrix improvement; reverted.

- Task 6:
  - Verified Ω/Φ strict suites + H15 guard are green with no retained Ω behavior change.
  - A temporary Ω probe was tested and reverted due non-improvement.

- Task 7:
  - Not executed (no sentinel graduation), because strict matrix remains red (`4/4` failing).

- Blocking condition:
  - `BUG-2026-02-06-002 strict target matrix` must turn green before sentinel graduation.
  - Current evidence suggests residual failure includes at least one non-edge-witness path (`make-only`).

### Phase 7 Execution — Direction matrix batch 3 (D4-D6) (2026-02-10)
- **Status:** complete (all directions evaluated and rolled back)

- Completed:
  - D4 let scope/target routing probe (`schemeRootId -> trivialRoot`) in `src/MLF/Elab/Elaborate.hs`.
  - D5 let closure/substitution boundary probe (always `closeTermWithSchemeSubst`) in `src/MLF/Elab/Elaborate.hs`.
  - D6 solve-state canonicalization probe (`eeResPhi/eeResReify = solvedForGen`) in `src/MLF/Elab/Run/Pipeline.hs`.

- Verification summary:
  - D4: strict matrix stayed red (`4/4`) and introduced Phase 6 `PhiInvariantError`; focused regression `redirected let-use sites keep polymorphic schemes` failed.
  - D5: strict matrix unchanged (`4/4` baseline failures); focused regressions stayed green (no strict-target movement).
  - D6: strict matrix stayed red (`4/4`) with Phase 6 `PhiTranslatabilityError` shift; focused regression `redirected let-use sites keep polymorphic schemes` failed.

- Error log / recovery:
  - D4 regression (`PhiInvariantError`) detected under focused gates; patch reverted immediately.
  - D6 regression (`PhiTranslatabilityError` + `PhiInvariantError`) detected under focused gates; patch reverted immediately.
  - D5 had no effect on hard gate 1; reverted to avoid dead-end drift.

- Next:
  - All single-direction hypotheses D1-D6 are now exhausted as failing under hard gates.
  - Move to minimal coupled experiment (cross-direction) targeting two independent residual surfaces:
    1. make-only scheme alias/body mismatch (`a -> c` vs `a -> b -> a`), and
    2. `OpGraft/OpRaise` codomain lock-in to `TBottom` in application path.

### Phase 7 Execution — Upstream plan continuation (post D1-D6) (2026-02-10)
- **Status:** in progress (partial improvement)

- Implemented:
  - Temporary-to-retained probe in `src/MLF/Constraint/Presolution/Plan/Normalize.hs`:
    - allow structured alias inlining in `simplifySchemeBindings` even when binders are named.

- Gate results:
  - `BUG-2026-02-06-002 strict target matrix`: improved from `4/4` FAIL to `3/4` FAIL.
    - `make-only elaborates as polymorphic factory` now PASS.
    - remaining failures: `make-app`, `let-c1-return`, `let-c1-apply-bool`.
  - Focused regressions:
    - `generalizes reused constructors via make const`: PASS.
    - `redirected let-use sites keep polymorphic schemes`: PASS.
    - `does not leak solved-node names in make let mismatch`: PASS.
    - Witness suites (`Witness translation (Φ/Σ)` and `graft-weaken canonical alignment`): PASS.

- Decision:
  - Keep this patch (improves behavior without observed regressions on sentinel/focused suites).

- Next:
  - Continue on remaining codomain `TBottom` lock-in (application path) while preserving new `make-only` win.

### Phase 7 Execution — Remaining 3-shape investigation (2026-02-10)
- **Status:** in progress (C1 retained; C2 probes reverted)

- Attempted temporary probes on remaining application-path failures:
  1. `Omega.reifyTypeArg` binder-name fallback for grafted TVar arguments tied to binder/copy identity.
  2. `Omega.OpRaise(non-spine)` root-inst preference over candidate context insertion.

- Verification outcome for both probes:
  - strict matrix stayed `4 examples, 3 failures` (no further improvement beyond C1).
  - focused regressions remained PASS.

- Recovery:
  - reverted both Ω probes to avoid keeping non-improving behavior changes.

- Current retained delta:
  - only `src/MLF/Constraint/Presolution/Plan/Normalize.hs` (C1 structured alias inlining).

- Next:
  - continue with thesis-faithful application-path fix targeting persistent `TBottom` codomain lock-in.

### Phase 7 Execution — C3 target-binder fallback probe (2026-02-10)
- **Status:** attempted and reverted (regressed focused suite)

- Probe:
  - `src/MLF/Elab/Phi/Translate.hs`
  - when `keep-keys` from target binders was empty, derived a fallback keep-set from `OpGraft/OpWeaken` + non-concrete graft-arg types.

- Outcome:
  - strict matrix remained `4 examples, 3 failures`.
  - failure shape changed (`TCExpectedArrow` in make-app / let-c1-return), indicating altered elimination behavior without net improvement.
  - focused regression `redirected let-use sites keep polymorphic schemes` failed.

- Recovery:
  - reverted `Translate.hs` probe immediately.
  - re-ran gates to confirm restoration of baseline+C1 state.

- Next:
  - keep only retained C1 patch and continue with a different application-path strategy.

### Phase 7 Execution — C4 probe status (2026-02-10)
- **Status:** complete (reverted)

- Completed:
  - Tested a minimal `InstInside` probe in `src/MLF/Elab/Inst.hs` preserving `TVar` bounds during `InstInside` bound reconstruction.
  - Ran strict + focused + sentinel gates.

- Outcome:
  - No strict target improvement beyond retained C1 (`strict matrix` remained `4 examples, 3 failures`).
  - Focused regressions stayed PASS; sentinel matrix remained `4 pending`.

- Action:
  - Reverted the probe; continue with next reversible hypothesis.

### Phase 7 Execution — C5/C6 follow-up status (2026-02-10)
- **Status:** complete (all reverted)

- Completed probes:
  - C5: `WitnessCanon` temporary no-reorder path.
  - C6: `Omega.graftArgFor` temporary `etBinderArgs`-first path.
  - Additional temporary `Phi.Translate` alias-subst probe.

- Result:
  - None improved strict target beyond retained C1; all were reverted.

- New evidence:
  - Edge diagnostics confirm failing graft arg node is unbounded `TyVar` in solved constraint, but reifies as `TBottom` in Φ path.
  - Next probe should focus on reification/naming (not witness pairing structure).
- Follow-up probe C8 completed (reverted): expanded `traceArgMap` binder-name lookup via copy-map fwd/rev paths.
  - Effect: improved diagnostics (`inferredMap` populated) but no strict-target movement; reverted.
- executing-plans batch checkpoint (Tasks 5–7, 2026-02-10):
  - Task 5: no retained patch this batch (investigation/probe staging only).
  - Task 6: Ω/Φ + witness explicit suites green.
  - Task 7: still blocked by strict/thesis failures; sentinel matrix stays pending.
- Additional Task 5/6 probe C9 completed (reverted):
  - `reifyTypeArg` rescue of unbounded `TBottom` to binder TVar showed no strict-target improvement.

## 2026-02-10 — executing-plans continuation checkpoint (C10)

- Completed a temporary C10 probe in `src/MLF/Elab/Phi/Translate.hs` to test copy-map alias remap strengthening.
- Verified gates per plan discipline:
  - strict matrix: unchanged (`4 examples, 3 failures`)
  - focused guards: pass
  - sentinel matrix: `4 pending` (policy preserved)
  - full BUG matcher: unchanged (`10 examples, 5 failures, 4 pending`)
- Decision: C10 provides diagnostics only; no hard-gate improvement.
- Rollback executed: `src/MLF/Elab/Phi/Translate.hs` reverted.
- Status: continue Task 5 hypothesis loop, targeting pre-translation Φ/Ω specialization flow.

## 2026-02-10 — executing-plans continuation checkpoint (C11/C12)

- C11 diagnostic pass completed (temporary `applyInst` tracing in Ω; reverted):
  - established that failing edge-0 collapse is caused at final `OpWeaken` elimination (`∀u1 ... -> ... u1 ...` to `... ⊥ ...`) while keep-set is empty.
  - `OpRaise` did not contribute effective transformations on this failing path.

- C12 behavioral probe completed (temporary reachable-type keep-key fallback in `Translate`; reverted):
  - changed error family but did not improve strict gate.
  - introduced focused regressions (`redirected let-use ...`, H15 guard in that probe run).

- Current retained baseline after reverts:
  - strict matrix: `4 examples, 3 failures`.
  - focused regressions: PASS.
  - sentinel matrix: `4 pending`.

- Next direction:
  - continue Task 5 with stricter/narrower keep/elimination control tied to failing edge-local operation flow, avoiding global keep-key broadening.

## 2026-02-10 — executing-plans continuation checkpoint (C13)

- C13 probe completed (temporary Ω weaken guard for rigid-unbounded alias pattern; reverted).
- Outcome:
  - strict target unchanged in count; one shape shifted to `TCExpectedArrow`.
  - focused regressions failed (let-use polymorphism + H15 non-leak guard).
- Decision:
  - reject C13 and keep baseline.
- Current retained state unchanged:
  - strict matrix `4 examples, 3 failures`;
  - focused guards PASS;
  - sentinel matrix `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C14)

- C14 probe completed (temporary `Translate` keep-key root switch `ewRight -> ewRoot`; reverted).
- Outcome:
  - strict matrix unchanged (`4 examples, 3 failures`).
  - focused guards remained PASS.
  - sentinel matrix remained `4 pending`.
- Decision:
  - reject C14 as non-improving.
- Current retained state unchanged.

## 2026-02-10 — executing-plans continuation checkpoint (C15)

- C15 probe completed (temporary `Translate` named-set augmentation with `etBinderArgs` arg nodes; reverted).
- Outcome:
  - strict matrix unchanged (`4 examples, 3 failures`).
  - focused regression introduced (`redirected let-use sites keep polymorphic schemes`).
- Decision:
  - reject C15 as non-improving + regressing.
- Current retained state unchanged:
  - strict `3` failures,
  - focused guards PASS,
  - sentinel `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C16)

- C16 probe completed (temporary narrow `OpWeaken` singleton-unbounded preserve guard; reverted).
- Outcome:
  - strict matrix unchanged in count (`3` failures), with one failure-family shift.
  - focused regressions failed (let-use polymorphism + H15 guard).
- Decision:
  - reject C16.
- Current retained state unchanged:
  - strict `3` failures,
  - focused guards PASS,
  - sentinel `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C17)

- C17 probe completed on upstream witness path (`WitnessCanon`, temporary prune of unbounded-var weaken).
- Outcome:
  - strict matrix unchanged in count (`3` failures), error family shifted.
  - focused regression: H15 non-leak guard failed.
- Decision:
  - reject C17 and keep baseline.
- Current retained state unchanged:
  - strict `3` failures,
  - focused guards PASS,
  - sentinel `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C18)

- C18 probe completed in `src/MLF/Elab/Phi/Omega.hs` (retained for now):
  - delayed binder-matching `OpWeaken` handling in `OpGraft` path,
  - `TBottom -> binder TVar` rescue in `reifyTypeArg` for known binder args.
- Outcome:
  - strict matrix improved from `3` to `2` failures:
    - PASS: `make-only`, `make-app`
    - FAIL: `let-c1-return`, `let-c1-apply-bool`
  - focused guards PASS,
  - sentinel matrix unchanged at `4 pending`,
  - full BUG matcher improved to `10 examples, 4 failures, 4 pending`.
- Decision:
  - keep C18 as current best candidate while investigating residual let-scheme mismatch.

## 2026-02-10 — executing-plans continuation checkpoint (C19)

- C19 probe completed (temporary `ALet` fallback in `src/MLF/Elab/Elaborate.hs`):
  - attempted scheme recovery from elaborated RHS type when generalized let scheme mismatched.
- Outcome:
  - regressed strict matrix from C18 state (`2` failures) back to `3` failures,
  - `make-app` failed with `TCInstantiationError InstElim ... expects forall`.
- Decision:
  - reject C19 and revert fallback patch.
- Current retained state after rollback:
  - strict matrix `2` failures,
  - focused guards PASS,
  - sentinel matrix `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C20)

- C20 probe completed in two temporary variants (both reverted):
  - C20a: ALet fallback using local `typeCheck rhs'` mismatch detection.
  - C20b: ALet fallback using env-aware RHS checking (`typeCheckWithEnv`) to activate fallback on target paths.

- Outcome:
  - C20a: no hard-gate movement (`strict` remained `2` failures).
  - C20b: strict improved to `1` failure, but focused H15 guard regressed (`does not leak solved-node names ...` failed with changed error family).

- Decision:
  - reject C20 (focused-regression policy).
  - rollback to retained C18 baseline.

- Current retained state:
  - strict matrix `2` failures,
  - focused guards PASS,
  - sentinel matrix `4 pending`.

## 2026-02-10 — executing-plans continuation checkpoint (C21/C21.1)

- C21 implemented and retained in `src/MLF/Elab/Elaborate.hs`:
  - ALet env-aware fallback scheme derivation (`typeCheckWithEnv`) with alpha-equivalence gate.
  - fallback keeps `subst0` and only swaps scheme when truly different.

- C21.1 implemented and retained in `src/MLF/Elab/Elaborate.hs`:
  - AApp fun-instantiation repair for non-polymorphic args when `funInst = InstApp TForall{}`.

- Combined outcome with retained C18:
  - `BUG-2026-02-06-002 strict target matrix`: PASS (`4/4`).
  - focused guards: PASS.
  - `BUG-2026-02-06-002` matcher: PASS for strict + thesis-target; sentinel block remains `4 pending` by policy.

- Current retained state:
  - strict matrix `0` failures,
  - focused guards PASS,
  - sentinel matrix `4 pending`.

## 2026-02-10 — step-1/2/3 execution checkpoint

- Step 1 (`sentinel` graduation): complete.
- Step 2 (`BUG` matcher rerun): complete and green (`10/10`, no pending).
- Step 3 (`cabal build all && cabal test`): executed; build succeeds but test suite still has 5 failing non-target regressions (WitnessSpec + ElaborationSpec set).
- Decision:
  - keep sentinel graduation and retained C18/C21/C21.1 bug-target fixes.
  - next workstream should address full-suite blockers before closing task.

## 2026-02-10 — C21.2 narrowing checkpoint

- Narrowed ALet fallback eligibility (app RHS + unbounded scheme + Int codomain) to keep bug-target behavior focused.
- BUG-2026-02-06-002 matcher remains green after narrowing.
- Full-suite still reports 5 non-target failures; proceed with separate remediation track.

## 2026-02-10 — closure checkpoint (H16 upstream + elaboration harmonization)

- Phase 7 execution: **complete**.
- Implemented and retained:
  - upstream witness normalization hardening in `WitnessCanon`:
    - canonical ambiguous graft/weaken rejection,
    - delayed `OpGraft ... OpWeaken` coalescing with binder/descendant safety.
  - Ω translation localization in `Phi.Omega`:
    - removed non-local delayed-weaken look-ahead,
    - scoped `TBottom -> TVar` rescue to adjacent `OpGraft+OpWeaken` path only.
  - scheme finalization normalization guard:
    - prevent structural-bound inlining for named binders in `simplifySchemeBindings`.
  - elaboration let fallback harmonization:
    - app fallback keeps prior behavior,
    - lambda fallback uses env-aware RHS recheck with `subst = IntMap.empty` for replacement schemes.
- Final verification:
  - `BUG-2026-02-06-002 strict target matrix`: PASS (`4/4`).
  - previously failing Phase 6 bounded/generalize/forall-bound cases: PASS.
  - `cabal build all && cabal test`: PASS (`601 examples, 0 failures`).
- Close-out decision:
  - task is complete and ready to archive.
