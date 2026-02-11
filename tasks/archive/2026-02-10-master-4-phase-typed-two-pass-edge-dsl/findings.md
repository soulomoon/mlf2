# Findings — Master 4-Phase Typed Two-Pass Edge DSL

## Key Discoveries

1. **Current presolution accepts non-`TyExp` left edges.**
   - `processInstEdge` currently branches on `TyExp` vs non-`TyExp`, allowing a fallback direct structural solve path.

2. **Phase-2 normalization already classifies residual edges but does not force paper shape.**
   - Non-graftable and type-error edges can remain as non-`TyExp` left in `cInstEdges`.

3. **Paper-shaped explicit expansion representation is already the codebase's intended model.**
   - `TyExp` notes and presolution comments describe explicit expansion-node handling as the primary representation.

4. **Edge-processing decomposition modules already exist and are reusable.**
   - Existing `EdgeProcessing.Unify` / `EdgeProcessing.Witness` are suitable interpreter primitives for a two-pass plan architecture.

5. **BLOCKER: Wrapping all residual inst edges with TyExp breaks 7 downstream tests.**
   - Synthesized TyExp wrappers route edges through the TyExp branch in `processInstEdge`, which uses `decideMinimalExpansion` + `unifyStructure` instead of `solveNonExpInstantiation`.
   - `solveNonExpInstantiation` has special binding-permission and scheme-root logic that `unifyStructure` does not replicate.
   - New TyExp wrapper nodes introduce binding-tree entries that elaboration (`PhiReorder`) doesn't expect: `"PhiReorder: missing order key for binders [NodeId {getNodeId = 29},NodeId {getNodeId = 30}]"`.
   - New `ExpVarId`s create expansion variable entries that elaboration was not designed to handle.
   - Affected tests: 7 pipeline/elaboration tests (Pipeline Phases 1-5, BUG-2026-02-06-002, Phase 6 Elaborate).

6. **Phase 3 Task 7 gate tests are now explicit and pass on pre-wrapping baseline.**
   - Added an explicit gate suite in `PipelineSpec`: make-let-mismatch no-name-leak, sentinel matrix strict target, checked-authoritative invariant, thesis target unchecked/checked Int, and `\y. let id = (\x.x) in id y` shape gate.
   - Verified the new gate block passes on the clean Phase 2 baseline.

7. **BLOCKER (current): Task 8 implementation still diverges from baseline semantics.**
   - After adding post-loop `TyExp` wrapping in normalization and routing synthesized wrappers through `solveNonExpInstantiation`, the 7 gate tests fail.
   - Failure signatures:
     - `Phase 6 (elaboration): SchemeFreeVars (NodeId {getNodeId = 9}) ["t9"]`
     - `Phase 7 (type checking): TCTypeAbsVarInScope "t4"`
   - Comparative instrumentation on BUG-2026-02-06-002:
     - Baseline `0c2ab7a`: presolution emits non-identity expansions and non-empty witness steps on key edges.
     - Current Task 8 branch: all recorded edge expansions collapse to `ExpIdentity` with empty witness steps.
   - This indicates semantic drift in the expansion-bearing path, not just for wrapped legacy edges.

8. **Task 9 fail-fast invariant is now enforced in planner.**
   - `planEdge` now rejects non-`TyExp` left edges with `InternalError` containing `expected TyExp-left`.
   - Planner tests were updated to assert fail-fast on non-`TyExp` inputs and to use `TyExp`-left edges for let/ann flag threading assertions.
   - Interpreter spec no longer assumes planner allows legacy-direct non-`TyExp` entry.

9. **Regression after Task 9 is green.**
   - Targeted planner checks pass (including the new fail-fast test).
   - Full verification passes: `cabal build all && cabal test` => 626 examples, 0 failures.

## Thesis Alignment Notes

- Enforcing `TyExp-left` for all Phase-4 instantiation edges strengthens paper-shape fidelity by making expansion variables explicit for every residual instantiation relation handled in presolution.
- Adding Phase-4 fail-fast converts an implicit assumption into an explicit invariant, reducing silent divergence risk.
- Current Task 8 blocker shows that paper-shape wrapping must preserve non-identity expansion behavior on existing polymorphic edges; otherwise elaboration violates scheme-freeness invariants.

## Open Questions

- Whether to keep phase-tagged errors as wrappers (`PlanError`/`ExecError`) or richer payload records for tooling/debug output.
- Whether to expose planner/interpreter modules publicly (currently planned as internal-only modules).
- **NEW: How to reconcile TyExp wrapping with the non-TyExp presolution branch.** Options:
  1. Port `solveNonExpInstantiation` logic into the TyExp branch so both paths are equivalent.
  2. Only wrap edges whose body is TyForall/TyExp (already presolution-ready), leaving Var≤Var edges unwrapped.
  3. Make the fail-fast assertion (Task 3) conditional: only assert TyExp-left for edges that the planner can handle, deferring full coverage to Phase 3.
  4. Implement Tasks 2+3+7 together as an atomic change (wrapping + fail-fast + planner/interpreter migration).


### BLOCKER RESOLVED (2026-02-11): Phase 3 equivalence recovered

- Compared failing wrapped run against known-good Phase 2 baseline (`0c2ab7a`) on BUG-2026-02-06-002 using an internal debug worktree.
- Identified two concrete divergence points:
  1. Wrapper-path detection by TyExp body shape (`TyForall` check) incorrectly treated frontend TyExp edges as synthesized wrappers.
  2. Wrapped runs exposed a Phi binder-ordering edge case where the narrowed key map could miss copied scheme binders.
- Implemented equivalence-preserving fixes:
  - `Normalize`: synthesized wrapper ExpVars now allocate from a reserved negative-ID space.
  - `Interpreter`: synthesized-wrapper dispatch now keys off `ExpVarId < 0`; real TyExp edges always use expansion semantics.
  - `Phi.Omega`: binder reorder uses full order-key map as fallback when narrowed keys are incomplete (prevents false invariant failure while preserving deterministic ordering).
- Outcome:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 atomic wrapping equivalence gates"'` => 7 examples, 0 failures.
  - `cabal build all && cabal test` => 626 examples, 0 failures.
- Phase 3 Task 8 is unblocked and complete; remaining Phase 3 work is Task 9 (planner fail-fast) and Task 10 (legacy-direct removal).

### Phase 3 Task 10 complete (2026-02-11): legacy-direct execution path removed

- Removed dead runtime split now that normalization + planner enforce TyExp-left edges:
  - `Interpreter.executeEdgePlan` now always uses expansion execution and hard-errors on non-`TyExp` plan input.
  - `Plan.EdgePlanMode` is reduced to one constructor (`ExpansionMode`).
  - `EdgeProcessing` no longer re-exports `solveNonExpInstantiation`.
- Kept synthesized-wrapper semantics intact for wrapped residual edges:
  - Wrapper detection remains explicit (`ExpVarId < 0`) and still routes those edges through `solveNonExpInstantiation body target` inside the expansion interpreter path.
- Regression result remains green after dead-path removal:
  - `cabal build all && cabal test` => 626 examples, 0 failures.


### Phase 4 complete (2026-02-11): error tags + matrix + verification

- Presolution error model now carries phase context:
  - `PlanError PresolutionError` for planner-surface failures.
  - `ExecError PresolutionError` for interpreter/runtime failures.
- Planner fail-fast now surfaces with explicit phase tag on invariant violations (`expected TyExp-left`).
- Interpreter execution is wrapped with Exec tagging so runtime failures are phase-attributed without changing underlying root-cause payload.
- Regression matrix coverage added across requested suites:
  - `ExpansionSpec`: constructor coverage regression for identity / instantiate / forall-intro / compose.
  - `EdgeTraceSpec`: identity-path trace shape (`ExpIdentity`, empty binder-arg trace).
  - `WitnessSpec`: compose expansion remains aligned with interleaved StepIntro + Omega steps.
  - `PipelineSpec`: annotation edges keep expansion assignments while suppressing `OpWeaken` in witness steps.
- End-to-end verification after Phase 4 is green:
  - `cabal build all && cabal test` => 630 examples, 0 failures.


### Phase 5 closeout (2026-02-11): abstraction contracts tightened

- `EdgePlan` no longer carries `eprMode`; resolved plans now encode TyExp-left shape directly via `ResolvedTyExp`.
  - This removes a now-redundant runtime discriminator and makes the resolved-plan invariant explicit in the type payload.
- Planner fail-fast now reports a structured invariant constructor (`ExpectedTyExpLeftInPlanner`) wrapped by `PlanError`.
  - This replaces stringly `InternalError` matching in tests and makes invariant failures machine-checkable.
- Synthesized wrapper `ExpVarId` logic is now encapsulated in `MLF.Constraint.Types.SynthesizedExpVar`.
  - `Normalize` allocates IDs via `SynthExpVarSupply` + `takeSynthExpVar`.
  - Interpreter checks wrapper IDs via `isSynthesizedExpVar` instead of local ad hoc predicates.
- Targeted and full verification stayed green after the polish changes.

### Phase 6 closeout (2026-02-11): unified execution without wrapper bridge

- The wrapper-specific interpreter bridge was removable without changing observed behavior by moving wrapper handling into the single expansion-oriented execution function.
- A dedicated regression (`EdgeInterpreterSpec`) now locks wrapper behavior where regressions were most likely:
  - synthesized wrapper with forall target still records `ExpIdentity` for the wrapper `ExpVarId`.
- Phase-3 equivalence gates remained green after bridge removal, confirming no regression on the known blocker family.
- Practical design conclusion:
  - multiple interpreter execution families are not needed for the gMLF acyclic solving path;
  - one execution family is sufficient, with wrapper cases handled as localized input-shape handling inside that family.

### Post-phase cleanup (2026-02-11): `EdgeStage` removal

- The `EdgeStage` index had only one runtime-reachable state (`StageResolved`), so it did not enforce additional invariants.
- Thesis alignment requires ordered edge execution (propagate then unify), but does not require a staged plan kind for edge records.
- `EdgePlan` is now a concrete resolved plan record, which reduces API surface and test ceremony without changing edge semantics.
- Verification remained green after the simplification:
  - `cabal build all && cabal test` => 631 examples, 0 failures.
