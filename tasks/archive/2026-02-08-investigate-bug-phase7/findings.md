# Findings & Decisions

## Requirements
- Diagnose Phase 7 `TCLetTypeMismatch` failure mode where `make`'s let-scheme specializes to `... -> Int` but RHS elaboration stays polymorphic
- Report likely root causes, enumerate 2-3 fix candidates with tradeoffs, list exact file/function touch points, and recommend one approach
- Focus on `MLF.Elab.Elaborate`, `MLF.Elab.Generalize`, `MLF.Elab.Run.Scope`, and presolution plan modules (`GammaPlan`, `ReifyPlan`)

## Research Findings
- `Bugs.md` currently tracks `BUG-2026-02-06-002` as a Phase 6 Φ translation failure using `make`, with the Phase 7 `TCLetTypeMismatch` regression described as `BUG-2026-02-08-004`; both point at `MLF.Elab.Elaborate` and presolution plan modules (GammaPlan/ReifyPlan).
- `docs/notes/bug-2026-02-06-003-trace-report-2026-02-07.md` demonstrates how missing alias-bound metadata in presolution/Φ leads to Poly→⊥ mismatches, emphasizing the need to keep GammaPlan/ReifyPlan and let-scope reification aligned.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
|          |           |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| Catchup script path missing | Noted; proceeding without it |

## Resources
-

## Visual/Browser Findings
-

---
*Update this file after every 2 view/browser/search operations*
*This prevents visual information from being lost*

## 2026-02-08 BUG-2026-02-06-002 Reproduction Update
- Exact reproducer from `Bugs.md` now fails at **Phase 7**, not Phase 6.
- Repro command:
  - `cabal exec runghc /tmp/repro-bug-2026-02-06-002.hs`
- Stable observed error:
  - `Phase 7 (type checking): TCLetTypeMismatch (TForall "a" Nothing (TForall "b" Nothing (TArrow (TVar "a") (TArrow (TVar "t23") (TVar "a"))))) (TForall "a" Nothing (TForall "b" Nothing (TArrow (TVar "a") (TArrow (TVar "b") (TBase Int)))))`
- Interpretation:
  - Pipeline reaches elaboration successfully; mismatch appears between let annotation (more polymorphic body) and elaborated RHS body (specialized to `Int` in codomain).
  - Existing `BUG-2026-02-06-002` tracker entry is stale on phase/error details and overlaps with Phase-7 issue family.

## Pattern Analysis (Systematic Debugging Phase 2)
- Comparison matrix (`/tmp/repro-make-matrix.hs`):
  - `let make = \x.\y.x in ...` variants consistently fail (Phase 7 `TCLetTypeMismatch`, and one `SchemeFreeVars` in Phase 6 depending on body shape).
  - `let id = \x.x in id id` succeeds.
- This isolates the regression to polymorphic let-generalization/elaboration interactions for higher-order constant functions, not a global pipeline failure.
- Trace evidence (`/tmp/repro-bug-2026-02-06-002-trace.out`):
  - Let-scheme for `make` becomes `∀a b. a -> b -> Int` during elaboration generalization.
  - Elaborated RHS remains `λx:a. λy:t23. x` (type `a -> t23 -> a`).
  - Phase 7 mismatch is therefore genuine and deterministic, not a type-checker false positive.

## Data-Flow Evidence (Generalization Mapping)
- Internal dump (`/tmp/repro-constraint-dump.out`) shows `constraintForGeneralization` produces suspicious solved→base collisions:
  - `s2b[0] = 5`, `s2b[1] = 5` (distinct solved binders collapsed to same base key).
- `instCopyMapFull` contains entries that look non-copy / ownership-overriding:
  - `fromList [..., (0 -> 5), (1 -> 5), ...]`
- These entries are consumed as left-biased `copyOverrides` in `Phase2` node mapping, potentially overriding valid solved→base associations for lambda parameters.
- This aligns with observed let-scheme corruption (`make` specialized to `... -> Int`) while RHS remains polymorphic (`a -> t23 -> a`).

## Hypothesis Tests (Phase 3)
- H1 (tested): change `Phase2` copy-override precedence (`solvedToBase1` union order) to preserve existing solved→base mapping.
  - Outcome: no user-visible change for `BUG-2026-02-06-002`.
- H2 (tested): generalize lets at scheme root (`NodeId 5`) instead of `schemeBodyTarget` (`NodeId 4`).
  - Outcome: regressed broadly (`TCTypeAbsBoundMentionsVar`, `PhiInvariantError`); reverted.
  - Additional evidence: direct generalization differs strongly by target:
    - target `5` → `Right (Forall ... TVar "c", ...)`
    - target `4` → `Right (Forall [a,b] (a -> b -> Int), ...)`
- H3 (tested): always prefer base TyVar in `Phase1.restoreSchemeNodes` (`preferBaseVar`).
  - Outcome: shifts failure to `SchemeFreeVars` (`t25`) and reveals free-var pressure; reverted.
- H4 (tested): change final aligned solved→base precedence in `Finalize` to preserve `solvedToBaseAligned0` before `copyOverrides`.
  - Outcome: mapping collisions improved (`s2b[0]=0`, `s2b[1]=1`), but end-user bug still reproduces; reverted.

## Current Root-Cause Direction
- Primary corruption point appears in binder selection within `MLF.Constraint.Presolution.Plan.BinderPlan.Build`:
  - Candidate `NodeId 25` (reachable/in-gamma) is filtered out by representative gating tied to `solvedToBasePref` + `baseGammaRep`.
  - The representative picked for that base key is not ultimately retained for the same scope, which leaves either:
    - over-specialized scheme (`... -> Int`) when fallback representatives survive, or
    - `SchemeFreeVars` when restoration prefers base-polymorphic vars.
- This explains why symptoms oscillate between Phase 7 `TCLetTypeMismatch` and Phase 6 `SchemeFreeVars` under small mapping-policy tweaks.
- H5 (tested): relax representative suppression gate in `BinderPlan.Build` to keep reachable binders.
  - Outcome: no behavior change on reproducer (`TCLetTypeMismatch` unchanged).
- H6 (tested): run generalization on `constraintForGen` with empty union-find (in `Run.Pipeline`).
  - Outcome: no behavior change on reproducer (`TCLetTypeMismatch` unchanged); reverted.

## 2026-02-09 Phase 2 Continuation (Pattern Isolation)
- Fresh repro still deterministic in both unchecked and checked pipelines:
  - `Phase 7 (type checking): TCLetTypeMismatch ...`
  - Trace files: `/tmp/bug002_trace_fresh.out`, `/tmp/bug002_checked_trace_fresh.out`.

- Pattern comparison:
  - `k1 = \\x.\\y.x` and `k2 = \\x.\\y.y` both fail when let-generalized/apply-reused.
  - `id-id` still succeeds.
  - This confirms the issue is not specific to one lambda body; it is triggered by this let-generalization path.

- Key differential evidence (target root vs scheme body target):
  - For `make` let root (`schemeRoot=5`):
    - `generalize(target=5)` yields alias-style polymorphic wrapper.
    - `generalize(target=schemeBodyTarget(5)=4)` yields `forall a b. a -> b -> Int` (buggy specialization).
    - Source: `/tmp/repro_generalize_target_fresh.out`.
  - For `id` let root (`schemeRoot=2`):
    - `generalize(target=2)` gives wrapper-style alias form.
    - `generalize(target=schemeBodyTarget(2)=1)` gives expected `forall a. a -> a`.
    - Source: `/tmp/repro_generalize_target_id_fresh.out`.

- Stage-by-stage graph comparison (`base -> presolution -> solved -> constraintForGen`) for `make`:
  - Base graph keeps `make` body unconstrained:
    - `node4 = a -> node3`, `node3 bound -> node2 (b -> a)`.
  - Presolution introduces copied/app-instantiated structure:
    - `node3 bound -> node27`, `node27 bound -> node30`, `node30 cod -> node25`, `node25 bound -> Int`.
  - Solve canonicalization removes aliases (`node3`, `node27`) and rewires:
    - `node4 cod -> node30`.
  - Sources: `/tmp/dump_node_stages_make.out`, `/tmp/dump_parents_make.out`.

- Exact point where specialization becomes the emitted let scheme:
  - In first `make` generalization call, binder diagnostics still show polymorphic shape:
    - `boundTy0 = a -> b -> t25`.
  - Immediately after reification, `ty0Raw` becomes:
    - `a -> b -> Int`.
  - Source: `/tmp/bug002_fulltrace.out` lines near `boundSelfAlias` then `ty0Raw`.

- Isolation inside generalization transformation:
  - `generalizeAt` on `solvedClean` at target `4` returns:
    - `Left (SchemeFreeVars ... [\"t26\"])`.
  - Same call on `solvedForGen` returns:
    - `Right (forall a b. a -> b -> Int, ...)`.
  - Source: `/tmp/compare_generalize_solved_vs_forgen.out`.

- Concrete reparenting site in code:
  - `constraintForGeneralization` finalization force-reparents instantiation-copy nodes using `instCopyMap`:
    - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs:95`
    - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs:105`
  - Observed effect on this repro:
    - `node25` parent changes from `GenNodeId 2` (`solved`) to `GenNodeId 1` (`constraintForGen`).
    - `node12` parent changes from `GenNodeId 4` to `GenNodeId 1`.
    - `instCopyMapFull` includes `(25 -> 0)`, driving that reassociation.
    - Source: `/tmp/repro_constraint_dump_fresh.out`.

## Updated Root-Cause Direction
- The specialization seen in `make` is not created in `Finalize.normalizeScheme`; `fiTyRaw` is already specialized before that stage.
- The decisive corruption appears in `constraintForGeneralization` parent/ownership rewiring:
  - copy-derived nodes (`25`, `30`, and related) are moved into the `make` scheme scope (`GenNodeId 1`),
  - then `schemeBodyTarget` + reification sees the copied `Int`-specialized path as part of `make`’s scheme body,
  - yielding `forall a b. a -> b -> Int` instead of `forall a b. a -> b -> a`.

## 2026-02-09 Phase 3 Hypothesis Test (single-line Finalize patch)
- Hypothesis H7:
  - In `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs` line ~95 reassociation fold, preserving existing parent (`insertWith keepOld`) might stop copy-node parent overwrite into root gen scope.
- Minimal change tested:
  - `IntMap.insert ...` -> `IntMap.insertWith keepOld ...` in `bindParentsFinalAligned''` fold.
- Verification:
  - Reproducer unchanged (still Phase 7 `TCLetTypeMismatch`) in both unchecked and checked:
    - `/tmp/repro_bug_current.hs` via `cabal repl lib:mlf2`.
  - Parent ownership unchanged for key nodes in `constraintForGen`:
    - `node25`, `node12`, `node30` still under `GenNodeId 1`.
    - `/tmp/repro_constraint_dump_after_patch.out`.
- Conclusion:
  - This specific overwrite-preservation tweak does not affect the failing path.
  - Likely reason: target copy-node bindings are inserted earlier/upstream and this fold is not the effective overwrite point for this repro.
- Action:
  - Reverted H7 patch (no source delta retained).

## 2026-02-09 Phase 3 Hypothesis Test (skip reassociation when non-root parent exists)
- Hypothesis H8:
  - In the same `Finalize.hs` reassociation fold, skip root-gen reassignment if the copied node already has a non-root parent in the aligned map.
- Minimal change tested:
  - Added `hasNonRootParent` check from `acc` and guarded reassociation with `not hasNonRootParent`.
- Verification:
  - Reproducer unchanged (still Phase 7 mismatch in unchecked + checked).
  - Parent ownership unchanged for target copy nodes:
    - `node12`, `node25`, `node30` still parented by `GenNodeId 1` in `constraintForGen`.
  - Evidence file: `/tmp/repro_constraint_dump_h8.out`.
- Conclusion:
  - This gating did not trigger in the failing path (or triggered too late to matter); no effect on output.
- Action:
  - Reverted H8 patch (no source delta retained).

## 2026-02-09 Phase 3 Hypothesis Test (Phase4 instCopy owner lock)
- Hypothesis H9:
  - `Phase4.overrideSchemeInteriorParentsWith`/`bindParentsFinal` still cross-reassign copy nodes before Finalize.
  - Add strict guard: for `instCopyNodes`, do not reassign when current owner is a non-root different gen.
- Minimal change tested:
  - Temporary guards in:
    - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Phase4.hs` (`overrideSchemeInteriorParentsWith`, `bindParentsFinal`)
- Verification:
  - Reproducer unchanged (unchecked + checked still `TCLetTypeMismatch`):
    - `/tmp/repro_bug_h9_phase4.out`
  - Ownership changed only partially:
    - `gen node[25]` moved to `GenNodeId 4`,
    - `gen node[12]` and `gen node[30]` remained `GenNodeId 1`.
    - `/tmp/repro_constraint_dump_h9_phase4.out`
  - Binding trace still shows specialized let body:
    - `generalizeAt: ty0Raw=TArrow (TVar "a") (TArrow (TVar "b") (TBase Int))`
    - `/tmp/bug002_bindtrace_h9_patch.out`
- Conclusion:
  - Reparenting `node25` is not sufficient; specialization path persists via `node30`/`node12` and solved shape (`node4 cod -> node30`).
- Action:
  - Reverted H9 patch (no retained code change).

## 2026-02-09 Phase 3 Hypothesis Test (base-only scheme-root ownership seed)
- Hypothesis H10:
  - Copy/synthesized roots (e.g. `12`) entering `schemeRootOwnersBase` may contaminate ownership; seed owners from base roots only.
- Minimal change tested:
  - One-line filter in `Phase4.hs`:
    - `schemeRootOwnersBase` membership changed from `schemeRootsAllSet` to `schemeRootsBaseSet`.
- Verification:
  - Reproducer unchanged (same `TCLetTypeMismatch`):
    - `/tmp/repro_bug_h10_phase4.out`
  - Key parent assignments unchanged for failing path (`12/25/30` still under gen1 in `solvedForGen`):
    - `/tmp/repro_constraint_dump_h10_phase4.out`
  - Change introduced `-Wunused-local-binds` warning (`schemeRootsAllSet`) and provided no behavioral benefit.
- Conclusion:
  - Base-only seeding at this point does not isolate the corruption.
- Action:
  - Reverted H10 patch immediately; baseline source restored.

## 2026-02-09 Phase 3 Hypothesis Test (H11 local scheme-body alias reify in Generalize.hs)
- Hypothesis H11:
  - In the local scheme-body alias path, `ty0Raw` is reified from solved copy shape (`node4 -> node30 -> Int`) instead of alias-preserving binder context.
  - Reify `ty0Raw` via binder-local substitution (`rpSubstForBound`) when a single local scheme-body binder owns `typeRootC`.
- Minimal change tested:
  - Temporary patch in `/Volumes/src/mlf4/src/MLF/Elab/Generalize.hs` near `ty0Raw <- reifySchemeType`:
    - detected single local scheme-body alias binder
    - reified type root with `substForBound` for that binder.
- Verification:
  - Reproducer unchanged in unchecked + checked:
    - `/tmp/repro_bug_h11.out`
  - Binding trace still shows `make` scheme body specialization:
    - `generalizeAt: ty0Raw=TArrow (TVar "a") (TArrow (TVar "b") (TBase Int))`
    - `/tmp/bug002_bindtrace_h11.out`
- Conclusion:
  - Reifying with binder-local substitution at this stage does not alter the corrupted `make` scheme path.
  - Corruption likely precedes this final scheme-type selection and remains tied to upstream ownership/mapping of copied nodes.
- Action:
  - Reverted H11 patch; baseline source restored.

## 2026-02-09 Phase 3 Hypothesis Test (H12: remove nestedSchemeInteriorSet filter in substAliasesFromBaseLocal)
- Hypothesis H12:
  - In `ReifyPlan.hs` `substAliasesFromBaseLocal` (line ~143), the `nestedSchemeInteriorSet` filter drops alias mappings like `25 -> name` for local scheme-body binders whose base representative is a known local binder.
  - Removing this filter should restore the alias mapping and prevent the reifier from following the solved graph to `Int`.
- Minimal change tested:
  - Removed `not (IntSet.member solvedKey rpiNestedSchemeInteriorSet)` guard from `substAliasesFromBaseLocal` list comprehension in `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`.
- Verification:
  - **Error shifted** from Phase 7 `TCLetTypeMismatch` to Phase 6 `PhiInvariantError "PhiReorder: missing binder identity at positions [0]"`.
  - `ty0Raw` changed from `TArrow (TVar "a") (TArrow (TVar "b") (TBase Int))` to `TArrow (TVar "a") (TArrow (TVar "b") (TVar "c"))` — **Int specialization eliminated**.
  - Subst map now includes `(25,"c")` — node 25 regained its alias mapping.
  - But `25 -> "c"` is incorrect: node 25 should map to `"a"` (first parameter) not `"c"` (scheme body alias = binder 5).
  - Binder 5 ("c") bound became self-referential: `a -> b -> c` (mentions itself), causing Phi translation failure.
  - `boundMentionsSelfAlias` returned `False` because node 25 is in `nestedSchemeInteriorSet` and the self-alias check skips such nodes (Alias.hs line 37).
- Root-cause chain exposed:
  1. `solvedToBasePref[25]` maps to base node 5 or 6 (rep 5, name "c") instead of base node 1 (rep 1, name "b") or base node 0 (rep 0, name "a").
  2. `baseGammaPick` shows base key 1 → solved keys [26, 25], but `solvedToBasePref` disagrees.
  3. The `nestedSchemeInteriorSet` filter was masking this incorrect `solvedToBasePref` mapping by preventing node 25 from being substituted at all.
- Conclusion:
  - H12 is **partially confirmed**: removing the filter eliminates the `Int` specialization, proving the substitution path is correct.
  - But the underlying `solvedToBasePref` mapping for node 25 is wrong, producing `"c"` instead of `"a"`.
  - Two compounding issues: (a) incorrect `solvedToBasePref[25]` base preference, (b) `nestedSchemeInteriorSet` filter masking the incorrect mapping.
- Action:
  - Reverted H12 patch; baseline source restored.
- Next direction (H13):
  - Investigate why `solvedToBasePref[25]` maps to base node 5/6 instead of base node 1 in the GammaPlan construction.
  - The `gbiSolvedToBase` mapping in `GammaPlan.hs` is the likely source of the incorrect base preference for copy-derived nodes.

## 2026-02-10 H13: Fix solvedToBasePref mapping in GammaPlan (CONFIRMED)

- Root cause: Two positional zips in GammaPlan.hs (`qAlignSolvedToBaseLocal` and `alignSolvedToBase`→`alignPrefer`) incorrectly pair copy-derived node 25 with wrong base nodes because copy-derived nodes inflate the solved gamma set.
- Chain: `solvedToBase[25] = NodeId 0` (correct, from copyOverrides) → `qAlign[25] = NodeId 1` (incorrect, positional zip) → `alignPrefer[25] = NodeId 5` (incorrect, another positional zip, THE override) → `solvedToBasePref[25] = NodeId 5` (from alignPrefer, highest precedence).
- Fix (retained in GammaPlan.hs):
  1. `alignPrefer` filter: reject entries where `solvedToBase` disagrees AND the `solvedToBase` target is a gamma binder different from the key.
  2. `solvedToBasePrefLocal` precedence: reordered to `identityGammaScoped > preferGamma > solvedToBase > qAlign > identityGamma`.
- Result: `solvedToBasePref[25] = NodeId 0` (correct). 584/584 tests pass.

## 2026-02-10 H14: Targeted nestedSchemeInteriorSet bypass (CONFIRMED)

- Problem: All H12 variants (removing `nestedSchemeInteriorSet` filter entirely) caused 4 id-id regressions (`OpGraft: binder not found in quantifier spine`).
- Diagnostic trace showed:
  - `make` case: bypass adds `(25, "a", 0, 0)` — node 25 is copy of outer gamma binder 0, reachable from type root.
  - `id-id` case: bypass adds `(17, "a", 0, 0)` and `(12, "a", 1, 1)` — copy-derived nodes NOT reachable from type root.
- Key insight: type-root reachability distinguishes nodes that appear in the scheme body type (need alias) from internal instantiation nodes (must stay blocked).
- Fix (retained in ReifyPlan.hs):
  - In `substAliasesFromBaseLocal`, bypass `nestedSchemeInteriorSet` filter when:
    1. Base key is NOT in `nestedSchemeInteriorSet` (copy-derived from outer scope), AND
    2. Solved key IS reachable from type root (`rpiReachableFromWithBounds rpiTypeRoot`).
- Result: 584/584 tests pass. Make scheme type now correct `forall a b. a -> b -> a`.

## Combined H13+H14 Status

- **Int specialization eliminated**: `ty0Raw` is now `a -> b -> a` (was `a -> b -> Int`).
- **Remaining error**: `TCLetTypeMismatch` with `t23` vs `b` naming mismatch:
  - Annotation: `forall a b. a -> t23 -> a`
  - Elaborated: `forall a b. a -> b -> a`
- This is a separate issue: the let-scheme annotation uses internal name `t23` for the second binder instead of `b`. The scheme type itself is correct.
- Next direction: investigate why the annotation path produces `t23` instead of `b` for the second binder.

## 2026-02-09 Phase 3 Hypothesis Test (H15: why let annotation gets `t23` instead of `b`)

- Goal:
  - Determine why Phase 7 compares `forall a b. a -> t23 -> a` (RHS term) against `forall a b. a -> b -> a` (let scheme) after H13+H14.

- Reproduction setup:
  - Created `/tmp/repro_bug_h15.hs` using current API (`normalizeExpr` + `runPipelineElabCheckedWithConfig`) with `tcBinding/tcGeneralize/tcElab` traces.
  - Captured full stdout+stderr traces in `/tmp/repro_bug_h15_full.out`.

- Key evidence:
  1. `generalizeAt` computes the `make` scheme correctly with binder names `a,b`:
     - `generalizeAt: ty0Raw=TArrow (TVar "a") (TArrow (TVar "b") (TVar "a"))`
     - let scheme trace: `Forall [("a",Nothing),("b",Nothing)] ...`
     - subst: `fromList [(0,"a"),(1,"b"),(25,"a"),(26,"b")]`
  2. Elaborated RHS term still carries `t23` on lambda parameter `y`:
     - `pipeline elaborated term=... ELam "y" (TVar "t23") ...`
  3. Phase 7 then fails at `ELet` type equality:
     - `TCLetTypeMismatch ... (TArrow (TVar "a") (TArrow (TVar "t23") (TVar "a"))) ... (TArrow (TVar "a") (TArrow (TVar "b") (TVar "a")))`

- Root-cause chain:
  1. In `ALam` elaboration, parameter source is chosen as
     - `paramSource = fromMaybe paramNode (resolvedLambdaParamNode lamNodeId)`
     - (see `src/MLF/Elab/Elaborate.hs`, around line 302)
  2. For this repro, `resolvedLambdaParamNode lamNodeId` resolves to copied var node `23` (not original binder node `1`).
  3. Reifying from node `23` yields `TVar "t23"` for lambda `y` type.
  4. Later closure/substitution only renames `TVar "tN"` when `N` exists in substitution map (`substInTy` + `parseNameId`):
     - map for `make` contains `0,1,25,26`, but **not 23**.
     - therefore `t23` survives into RHS type.
  5. `ELet` typecheck compares RHS type with scheme type and fails (`TypeCheck.hs` let branch).

- Single-hypothesis probe (temporary):
  - Changed `paramSource` to `paramNode` (ignoring `resolvedLambdaParamNode`) in `Elaborate.hs`.
  - Outcome:
    - `ELam "y" (TVar "b")` observed (the `t23` symptom disappears).
    - Phase 7 error shifts to downstream mismatch at `c1` (`TArrow TBottom Int` vs `TArrow b a`), i.e. different issue surfaces.
  - Interpretation:
    - Confirms H15 cause for the `t23` naming mismatch.
    - Probe was reverted immediately (no retained source change).

- Conclusion:
  - H15 confirmed: `t23` is introduced by ALam param-source resolution to copied node 23, then preserved because closure substitution map does not include key 23.
  - This is a naming/source-selection mismatch between ALam param reification and let-scheme substitution domain, not a scheme-generalization error.

- Candidate fix direction (next step):
  - Restrict `resolvedLambdaParamNode` usage to annotated-lambda path only (or guard it so resolved node must be name-mappable under current let-scheme substitution domain).

## 2026-02-09 Phase 4 Implementation (H15 guard in ALam param-source selection)

- TDD flow:
  1. **RED**: added targeted regression in `test/PipelineSpec.hs`:
     - `does not leak solved-node names in make let mismatch`
     - asserts Phase-7 mismatch (if present) does not contain `t23`.
  2. **GREEN (attempt 1)**: unannotated lambdas forced to `paramNode`.
     - fixed `t23` leak but regressed unrelated application typing (`(\x.x) (-4)` produced `TCArgumentMismatch (TVar "t0") Int`).
  3. **GREEN (final)**: introduced guarded resolved-node use for unannotated lambdas via `hasInformativeVarBound`:
     - if resolved param's bound-chain reaches a non-`TyVar` node, use resolved node;
     - otherwise keep lexical `paramNode`.

- Retained code changes:
  - `src/MLF/Elab/Elaborate.hs`
    - added helper: `hasInformativeVarBound :: NodeId -> Bool`
    - updated `ALam` `paramSource` selection:
      - annotated-lambda path unchanged (`desugaredAnnLambdaInfo` still uses resolved param fallback);
      - unannotated path now uses resolved node only when informative, else lexical binder node.
  - `test/PipelineSpec.hs`
    - added H15 regression spec: `does not leak solved-node names in make let mismatch`.

- Post-fix behavior:
  - `make` trace no longer leaks `t23`; elaborated term shows `ELam "y" (TVar "b") ...`.
  - Remaining failure in reproducer is now broader bug shape (`TBottom -> Int` vs expected `b -> a`), not the naming leak.

- Verification evidence:
  - Targeted RED check (before fix): new test fails with rendered error containing `t23`.
  - Targeted GREEN checks:
    - `--match "does not leak solved-node names in make let mismatch"` passes.
    - `--match "runPipelineElab type matches typeCheck(term) and checked pipeline type"` passes (80/80).
  - Full gate:
    - `cabal build all && cabal test` passes (`585 examples, 0 failures`).

## 2026-02-09 H16 investigation start (remaining `TBottom`/arrow mismatch)

- Objective:
  - Investigate the remaining mismatch after H15 where `make`-family paths fail with `TBottom` in arrow domain (`⊥ -> Int`) instead of expected polymorphic arrow (`b -> a`).

- Reproduction matrix (`/tmp/repro_h16_matrix.hs`):
  - `make-only`:
    - `TCLetTypeMismatch` with nested forall shape drift.
  - `make-app` (`let make = ... in make (-4)`):
    - succeeds with type **`⊥ -> Int`**.
  - `let-c1-return`:
    - `TCLetTypeMismatch` (`forall a. ⊥ -> Int` vs `forall a. a`).
  - `let-c1-apply-bool` (canonical bug path):
    - `TCLetTypeMismatch` (`forall a b. ⊥ -> Int` vs `forall a b. b -> a`).

- Key isolation result:
  - The `TBottom` collapse occurs **before** introducing `let c1`; it is already visible in `make-app` result type (`⊥ -> Int`).
  - Therefore H16 is rooted in function instantiation/elaboration for `make (-4)`, not specifically in let-annotation comparison.

- Focused trace (`/tmp/repro_h16_make_app_trace.out`):
  - Let scheme for `make` is still generalized correctly:
    - `elaborate let: scheme=Forall [("a",Nothing),("b",Nothing)] a -> b -> a`.
  - During edge-0 function instantiation, Φ includes nested `InstBot` applications:
    - `InstInside (InstBot TBottom)` (twice), and later
+    - `InstInside (InstBot (TVar "t14"))` in sequence with `InstInside (InstBot Int)`.
  - Final checked type for `make-app` is `⊥ -> Int`.

- Comparison baseline (`/tmp/repro_h16_id_app_trace.out`):
  - Simple identity app `((\x.x) (-4))` elaborates to `Int` and does **not** show the problematic `InstBot` chain.

- Probable hot path for H16:
  - `MLF.Elab.Phi.Omega.reifyTypeArg` and OpGraft handling in Φ→inst translation (`InstBot` emission around graft/under/inside sequences), especially where free variable args become `TVar "tN"` and feed into bot-instantiation paths.

## H16 first hypothesis (next implementation candidate)

- Hypothesis H16.1:
  - In `MLF.Elab.Phi.Omega.reifyTypeArg`, when an argument reifies to an unconstrained fresh `TVar "tN"` (e.g. `t14`) and is used in an `OpGraft`/`OpGraft+OpWeaken` `InstBot` path, we should prefer binder-aligned naming/substitution (or avoid bot-instantiating that free TVar) to prevent domain collapse to `TBottom` in the resulting function type.

- Why this is plausible from evidence:
  - `make-app` trace shows `reifyTypeArg` returns `TVar "t14"` for arg node 14, and Φ then emits `InstInside (InstBot (TVar "t14"))` in the function-instantiation sequence.
  - Final type becomes `⊥ -> Int`, indicating one polymorphic parameter is being effectively eliminated through bot-instantiation path.
  - Working baseline (`(\x.x) (-4)`) does not produce this `InstBot (TVar "tN")` sequence.

- Minimal-test patch direction (H16.1 probe):
  - Add a narrow guard in `reifyTypeArg`/OpGraft translation to avoid introducing `InstBot` with unconstrained free TVar arguments in this path, favoring binder-aligned type from inferred maps when available.
  - Keep change local to OpGraft/OpGraft+OpWeaken path to minimize risk.

## 2026-02-09 H16.1 temporary probe (reifyTypeArg / InstBot TVar path)

- Goal:
  - Test whether preventing/retargeting `InstBot (TVar "tN")` argument flow in the OpGraft path fixes the `⊥ -> Int` collapse.

- Step 0 (test hardening before probe):
  - Added matrix sentinel tests in `test/PipelineSpec.hs` (`BUG-2026-02-06-002 sentinel matrix`):
    1. `make-only` mismatch sentinel
    2. `make-app` sentinel type `⊥ -> Int`
    3. `let-c1-return` mismatch with `TBottom`
    4. `let-c1-apply-bool` mismatch with `TBottom` and no `t23`
  - Verified: all 4 sentinel tests pass.

- Probe A (temporary):
  - File: `src/MLF/Elab/Phi/Omega.hs` (`reifyTypeArg`).
  - Change: introduced `chosenTy2` fallback to use `inferredSingleton` when `mbBinder=Just _` and `chosenTy1` is `TVar _`.
  - Result: **no behavioral change**.
    - `make-app` still inferred `Type: ⊥ -> Int`.

- Probe B (temporary refinement):
  - File: `src/MLF/Elab/Phi/Omega.hs` (`reifyTypeArg`).
  - Change: introduced `chosenTy3` to force binder-name alignment (`TVar binderName`) for `(mbBinder, TVar _)`.
  - Trace confirmed local effect:
    - `arg=14`: `chosenTy3=TVar "b"` (was `TVar "t14"`/`TVar "c"`).
  - Result: **still no behavioral change**.
    - `make-app` remained `Type: ⊥ -> Int`.
    - matrix outcomes unchanged.

- Conclusion:
  - H16.1 hypothesis rejected: this local `reifyTypeArg` TVar handling is not the root cause of bottom-domain collapse.
  - All temporary `Omega.hs` probe edits were reverted.
  - Sentinel tests remain in suite as regression guardrails and continue to pass.

- Next likely direction (H16.2 candidate):
  - Inspect OpGraft/OpGraft+OpWeaken translation decisions that emit `InstInside (InstBot TBottom)` before/around binder elimination, and how those interact with `syncIdsAcrossInstantiation`/binder-spine identity mapping.

## 2026-02-10 H16.2 probe (OpGraft/OpWeaken `InstBot`/`InstElim` path)

- Scope:
  - Started H16.2 after confirming findings-listed regressions were already present in `test/PipelineSpec.hs`:
    - H15 leak guard (`does not leak solved-node names in make let mismatch`)
    - 4-case sentinel matrix (`BUG-2026-02-06-002 sentinel matrix`).
  - Re-validated those targeted specs before probe.

- Instrumentation focus:
  - Added temporary trace instrumentation in `MLF.Elab.Phi.Omega` to log:
    - operation classification (`OpGraft`, `OpWeaken`),
    - `ids` spine before/after application,
    - instantiation payloads at each `applyInst`,
    - binder→arg lookup decisions for `OpWeaken`.

- Confirmed baseline collapse mechanism:
  - For edge 0 (`make-app`), Ω executes `OpGraft(13,16)`, `OpGraft(14,17)`, then `OpWeaken(16)`, `OpWeaken(17)`.
  - Baseline translation weakens with `InstElim` twice, producing:
    - after second weaken: `TArrow Int (TArrow TBottom Int)`
    - final checked type: `⊥ -> Int`.
  - Evidence:
    - `/tmp/repro_h16_make_app_trace_h162.out`
    - `/tmp/repro_h16_make_app_trace_h162_probe2.out`

- H16.2 hypothesis test (temporary):
  - Probe: when `OpWeaken` targets an unbounded/⊥-bounded binder, and we can recover the binder's graft argument, use `InstApp argTy` instead of raw `InstElim`.
  - Initial failure mode:
    - binder→arg recovery failed (`mbGraftArg=Nothing`) because trace binder ids are original (`0`,`1`) while Ω runs over copied ids (`16`,`17`).
  - First refinement:
    - matched via GA/base keys; this was incorrect for binder 17 because both trace binders mapped to same base key (`5`) in this context.
    - symptom: binder 17 picked arg 13 instead of arg 14.
  - Second refinement:
    - matched binder→arg through `copyMap`/inverse-copy correspondence (original↔copy identity), which correctly resolved:
      - binder 16 → arg 13
      - binder 17 → arg 14
    - evidence:
      - `/tmp/repro_h16_make_app_trace_h162_probe6.out`

- Observed effect under refined probe:
  - `make-app` no longer collapsed to `⊥ -> Int`; it became polymorphic-domain arrow (`... b -> Int`) rather than `⊥`.
  - `let-c1-return` mismatch changed from bottom-vs-var to var-vs-var (`b` vs expected `a`).
  - Canonical `let-c1-apply-bool` mismatch still present (still a Phase 7 mismatch family).
  - Matrix under probe:
    - `/tmp/repro_h16_matrix_h162_probe7.out`
  - Sentinel tests expectedly diverged from baseline (2 of 4 failed), so probe is not accepted as-is.

- Conclusion (H16.2 status):
  - **Partially confirmed**: the second-binder `⊥` collapse is linked to elimination behavior in `OpWeaken` after grafting, and binder/arg identity alignment matters.
  - **Not sufficient**: replacing weaken-elim with graft-app alone does not restore expected `b -> a`; codomain remains specialized to `Int`, and extra quantification shape appears.
  - Therefore, root cause is multi-part:
    1. weaken/elimination policy for grafted unbounded binders, and
    2. earlier/parallel specialization path (before final weaken), likely in the same edge’s OpGraft/OpRaise sequence.

- Action taken:
  - Reverted all temporary H16.2 `Omega.hs` edits after evidence collection.
  - Baseline sentinel + H15 regression tests pass again.

## 2026-02-10 H16.3 probe (OpWeaken identity-on-TVar-graft, temporary)

- Hypothesis:
  - If `OpWeaken` is immediately eliminating a binder whose graft argument reifies to unconstrained `TVar`, forcing `InstElim` may be the source of premature domain collapse (`⊥`).
  - Temporary idea: in that narrow case, treat weaken as identity (skip elimination) to preserve polymorphism and avoid synthetic bottoming.

- Probe implementation (temporary, reverted):
  - In `MLF.Elab.Phi.Omega` `OpWeaken` branch:
    - looked up binder's graft arg from trace via copy-map aligned binder matching,
    - when binder bound is `Nothing`/`Just TBottom` and arg reifies to `TVar _`, skipped elimination for that weaken.

- Observed behavior:
  - `make-app` and `let-c1-return` no longer matched previous sentinel failure shape; they became `TCExpectedArrow` failures (higher-rank/forall arrow expectation mismatch).
  - canonical `let-c1-apply-bool` mismatch remained unresolved.
  - Matrix output under probe:
    - `/tmp/repro_h16_matrix_h163_probe1.out`

- Sentinel impact:
  - `BUG-2026-02-06-002 sentinel matrix` failed 2/4 under probe:
    - `make-app currently collapses to bottom-int arrow sentinel`
    - `let-c1-return still reports bottom-vs-var mismatch sentinel`
  - Therefore probe does **not** satisfy “improves behavior without regressing sentinel suite”.

- Conclusion:
  - H16.3 probe rejected.
  - Skipping weaken-elimination for TVar grafts introduces type-shape regressions (`TCExpectedArrow`) and is not a valid direction.

- Action taken:
  - Reverted all H16.3 edits in `src/MLF/Elab/Phi/Omega.hs`.
  - Revalidated baseline targeted tests (sentinel matrix + H15 leak guard) are green.

## 2026-02-10 H16.4 probe (pre-weaken OpGraft/OpRaise specialization, stricter)

- Goal:
  - Keep `OpWeaken` semantics unchanged; probe only pre-weaken path (OpGraft/OpRaise neighborhood) to target remaining codomain `Int` lock-in.

- Temporary probe applied:
  - File: `src/MLF/Elab/Phi/Omega.hs`
  - Scope: binder-path `OpGraft` branch only (`InstInside (InstBot argTy)` generation).
  - Changes:
    - switched arg source to `graftArgFor arg bv` for this branch,
    - added temporary trace capture of `hasPendingWeaken` / `argTy` in `OpGraft(pre-weaken)`.
  - `OpWeaken` branch left untouched.

- Results:
  - Focused `make-app` trace remained unchanged at outcome level:
    - final type still `⊥ -> Int`.
  - Matrix remained unchanged across 4 cases (same sentinel shapes as baseline).
  - Sentinel suite remained green (4/4).

- Interpretation:
  - This stricter pre-weaken `OpGraft` adjustment does **not** affect the codomain `Int` lock-in or the `⊥` domain collapse.
  - The remaining issue is likely outside this local arg-source tweak in binder-path `OpGraft`.

- Action:
  - Reverted all temporary H16.4 `Omega.hs` edits.
  - Revalidated baseline targeted tests after revert.

## 2026-02-10 Test policy update: known buggy-shape sentinels marked pending

- Decision:
  - Converted the `BUG-2026-02-06-002 sentinel matrix` examples to explicit `pendingWith` markers.
- Rationale:
  - These examples document known-bug behavior and should not appear as “passing behavior tests” while H16 remains unresolved.
- Validation:
  - Targeted sentinel run now reports `4 pending` (not passing assertions).

## 2026-02-09 Planning outcome: selected Option 1 (upstream witness-shape correction)

- User-selected strategy:
  - **Option 1 — Upstream witness-shape correction (Recommended)**.

- Why this direction:
  - H16.1–H16.4 local probes in `MLF.Elab.Phi.Omega` either:
    - did not change the `⊥ -> Int` lock-in, or
    - shifted failure shape/regressed sentinel behavior.
  - This indicates the durable correction point is likely upstream in witness-shape construction/normalization, not an Ω-local weaken heuristic.

- Operational test policy retained:
  - `BUG-2026-02-06-002 sentinel matrix` remains `pendingWith` while bug is open.
  - A separate strict RED matrix is planned to drive implementation to closure without treating known-bug shapes as passing tests.

- New planning artifacts:
  - Design doc:
    - `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-design.md`
  - Execution plan:
    - `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-implementation-plan.md`

- Planned technical focus (H16 continuation):
  1. Strengthen graft/weaken canonical alignment in witness normalization (`WitnessCanon`/`WitnessNorm`).
  2. Keep Ω translation strict and minimal.
  3. Convert pending buggy-shape sentinels to strict passing assertions only when fix is proven and regression gates stay green.

## 2026-02-09 Phase 7 Task 1 result: strict RED matrix now locked

- Added executable bug-target block in `test/PipelineSpec.hs`:
  - `describe "BUG-2026-02-06-002 strict target matrix"`

- Observed RED behavior (current baseline):
  1. `make-only elaborates as polymorphic factory` fails with `TCLetTypeMismatch`.
  2. `make-app keeps codomain Int without bottom-domain collapse` fails due `TBottom` domain.
  3. `let-c1-return keeps second binder polymorphic` fails with `TCLetTypeMismatch` (`TArrow TBottom Int` artifact).
  4. `let-c1-apply-bool typechecks to Int` fails with `TCLetTypeMismatch` (polymorphism lost).

- Sentinel policy check remains intact:
  - Existing `BUG-2026-02-06-002 sentinel matrix` remains `4 pending` by design.

- Interpretation:
  - The strict matrix now provides concrete closure targets for Option 1 implementation without conflating known-bug sentinels with passing behavior tests.

## 2026-02-09 Phase 7 Task 2 result: witness-shape RED signal established

- Added focused witness normalization tests in `test/Presolution/WitnessSpec.hs`:
  - canonical alignment case (passes)
  - ambiguous canonical mapping rejection case (currently fails)
  - graft/weaken idempotence case (passes)

- New RED evidence:
  - `normalizeInstanceOpsFull` currently accepts an ambiguous canonicalized sequence where two distinct copied binders collapse to the same canonical binder with different graft args:
    - observed output: `Right [OpGraft 3 2, OpWeaken 2, OpGraft 1 2, OpWeaken 2]`
  - This supports the upstream fix direction: normalization currently lacks a guard against ambiguous graft/weaken canonical mapping.

- Practical note:
  - Hspec `--match` with alternation string (`a|b|c`) selected 0 examples in this harness; exact suite name matching works for targeted runs.

## 2026-02-09 Phase 7 Task 3 result: Φ regressions added and stable

- Added Φ translation coverage in `test/ElaborationSpec.hs` for two H16-related concerns:
  1. ambiguous repeated graft/weaken operations on the same non-front binder are rejected,
  2. non-front binder targeting remains stable even when preceded by a root graft.

- Current behavior under new tests:
  - ambiguity case currently rejects (passes expectation of failure path),
  - root-graft + non-front-target case yields expected `Int -> Bool` shape (passes).

- Interpretation:
  - The immediate RED gap remains upstream in witness normalization (Task 2 failing case), while existing Φ translation behavior for these synthetic non-front scenarios is presently stable.

## 2026-02-09 Phase 7 Task 4 result: upstream ambiguity guard implemented

- Implemented in `WitnessCanon.normalizeInstanceOpsFull`:
  - new `checkGraftWeakenAmbiguity` pass after canonicalization, before merge-direction/reorder validation.

- Rule introduced:
  - if a weakened binder key has more than one distinct canonical graft argument in the same op segment, normalization now fails with `AmbiguousGraftWeaken`.

- Evidence:
  - Task 2 failing witness regression now passes:
    - `graft-weaken canonical alignment` suite: `3/3` green.

- Scope/result:
  - This addresses one upstream malformed-shape class.
  - It does **not** yet fix the main pipeline strict matrix (`BUG-2026-02-06-002 strict target matrix` remains `4/4` failing), so additional upstream alignment (Task 5+) is required.

## 2026-02-09 Task 5 investigation findings (wiring hypotheses rejected)

- Diagnostic witness dump for `make-app` confirms problematic shape is still present upstream:
  - edge-0 `ewSteps` include dual grafts + dual weakens with intervening raises.
  - binder/arg trace pairing is present but does not prevent final `TBottom`-family mismatch.

- Probe A (keep rewritten ids in normalized witnesses) was rejected:
  - caused `OpWeaken targets non-binder node` translatability failures,
  - did not improve strict target outcomes.

- Probe B (convert unbounded-binder+unbounded-arg from graft/weaken to merge) was rejected:
  - triggered presolution `mkOmegaExecEnv` missing-copy internal errors,
  - unstable and non-viable for retention.

- Practical conclusion:
  - The retained upstream improvement is the ambiguity guard (`AmbiguousGraftWeaken`), but the main BUG-2026-02-06-002 failure path still needs a different correction point than the two Task 5 wiring probes above.

## 2026-02-10 — Direction matrix baseline findings (Tasks 1-3)

- New strict thesis-target spec (`BUG-2026-02-06-002 thesis target`) is now RED by construction:
  - unchecked: fails with `Phase 7 (type checking): TCLetTypeMismatch ... TArrow TBottom Int ...`
  - checked: same mismatch family
- The mismatch shape confirms the remaining codomain `Int` lock-in + `TBottom` domain collapse remains unresolved in current codepath.
- The new diagnostics harness confirms required trace observability is available for direction work:
  - `generalizeAt:` lines present
  - `elaborate let: scheme=` lines present
  - `Phase` + `TCLetTypeMismatch` summaries present
- Matrix control artifacts now exist for one-direction-at-a-time experiments:
  - test gate: `test/ThesisFixDirectionSpec.hs`
  - runner: `scripts/run-bug-2026-02-06-002-direction.sh`
  - notes template: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

- Harness quality refinement: a single combined `--match` regex under Hspec produced `0 examples`; explicit per-test `--match` invocations are now used in the direction runner to guarantee non-empty focused regression checks.

## 2026-02-10 — Direction matrix experiments D1-D3 findings

- D1 (binder representative filtering) did not move the strict target:
  - thesis target remained `2/2` failures (`TCLetTypeMismatch` family).
  - nearby focused regressions stayed green.

- D2 (type-root fallback gating) did not move the strict target:
  - gating `targetBoundUnderOtherGen` by gamma membership produced no thesis-target improvement.

- D3 (solved/base mapping + provenance precedence) changed internals but not outcome:
  - mapping/provenance deltas were visible in trace (`baseGammaSet` expansion, extra `t25` bound dependency),
  - final let scheme still specialized codomain to `Int`, and thesis target remained red.

- Cross-direction conclusion (D1-D3):
  - None of the first three isolated directions satisfy hard gate 1.
  - Next experiments should proceed to D4-D6 routing/closure/canonicalization directions.

## 2026-02-10 — Resume of upstream witness-shape plan (Tasks 5-7)

- A Task-5 wiring hypothesis in `WitnessNorm` (canonical binder-arg alignment + early ambiguous-map rejection in rewritten space) preserved witness tests but did not improve `BUG-2026-02-06-002 strict target matrix`.
- A Task-6 Ω strictness probe (`graftArgFor = arg`) was non-improving on strict bug targets and was reverted.
- Current Ω/Φ translation suites and H15 non-leak regression remain green.
- Sentinel graduation remains blocked:
  - strict bug matrix is still `4/4` failing,
  - sentinel matrix remains `4 pending` by policy.
- Additional diagnosis: `make-only` strict failure reproduces with no presolution edge witness for that shape, suggesting remaining root-cause extends beyond edge-witness normalization alone.

## 2026-02-10 — Direction matrix D4-D6 findings

- D4 (`Elaborate` let target reroute to `trivialRoot`) is not viable:
  - It changes failure family from Phase 7 mismatch into Phase 6 `PhiInvariantError`.
  - It breaks the nearby sentinel `redirected let-use sites keep polymorphic schemes`.
  - This indicates current let-target routing participates in Φ binder identity invariants and cannot be switched naively.

- D5 (always close RHS with `closeTermWithSchemeSubst`) is behaviorally neutral for the hard bug:
  - Strict bug matrix remains `4/4` red with baseline mismatch shapes.
  - Nearby focused regressions stay green.
  - Conclusion: closure-trigger policy is not the dominant cause of BUG-2026-02-06-002.

- D6 (`eeResPhi/eeResReify = solvedForGen`) is not viable:
  - It produces Phase 6 `PhiTranslatabilityError` (`OpRaise (non-spine): missing computation context`) across strict bug shapes.
  - It regresses `redirected let-use sites keep polymorphic schemes` with `PhiInvariantError`.
  - This confirms mixed-state split (`solvedClean` for Φ/reify, `solvedForGen` for generalization) is currently load-bearing.

- Consolidated post-D6 diagnosis:
  - Remaining failure is likely a coupled issue with two distinct surfaces:
    1. top-level `make-only` scheme body aliasing (`a -> c`) versus RHS inferred type (`a -> b -> a`), and
    2. application-path Φ instantiation that emits `InstBot TBottom` in codomain-sensitive positions.

## 2026-02-10 — Coupled follow-up finding (C1): structured alias inlining helps make-only

- A focused change in `MLF.Constraint.Presolution.Plan.Normalize.simplifySchemeBindings` to inline structured alias bounds (not only var/base alias cases) partially resolves BUG-2026-02-06-002.
- Observable effect:
  - strict matrix improved from 4 failing shapes to 3 failing shapes.
  - specifically, `make-only` now passes.
- Interpretation:
  - The `make-only` failure surface is tied to scheme-shape normalization/finalization (alias/body shape mismatch), not to edge-witness translation.
- Remaining failure surface is now concentrated on application paths (`make-app`/`let-c1-*`) with codomain `Int` + `TBottom` domain collapse, likely still in Φ/instantiation reconstruction.

## 2026-02-10 — C2 Ω probes after C1 (non-improving, reverted)

- Probe C2.1 (`Omega.reifyTypeArg` binder fallback):
  - mapped unresolved graft arg TVars to binder names when arg/binder are copy/bound-related.
  - result: instantiation text changed (e.g., `InstBot (TVar "t14")` -> `InstBot (TVar "b")`) but strict matrix remained `3 failures`.

- Probe C2.2 (`Omega.OpRaise(non-spine)` root-first preference):
  - preferred root-inst branch before candidate insertion (`InstIntro + InstBot`) when available.
  - result: no strict-matrix improvement; remaining failures unchanged.

- Conclusion:
  - neither Ω-local adjustment removed the residual `TBottom` lock-in for `make-app`/`let-c1-*`.
  - keep C1 normalization win; continue searching upstream/downstream interaction causing application-path collapse.

## 2026-02-10 — C3 target-binder fallback probe (reverted)

- Hypothesis:
  - empty `targetBinderKeys` in Φ translation was over-eliminating binders in application paths.
- Patch tested:
  - fallback keep-set in `Phi.Translate` based on `OpGraft/OpWeaken` and whether graft args reified to non-concrete types.
- Result:
  - no hard-gate improvement beyond C1 (`strict matrix` still `3 failures`).
  - introduced a focused regression (`redirected let-use sites keep polymorphic schemes`) and changed failures to `TCExpectedArrow` in two shapes.
- Conclusion:
  - this keep-key fallback heuristic is not thesis-safe in current form; reverted.

## 2026-02-10 — C4 finding: `InstInside` TVar-bound retention is non-improving

- Hypothesis:
  - residual application-path collapse might come from `InstInside` dropping `TVar` bounds to `Nothing`, which then feeds `InstElim` defaulting to `TBottom`.

- Probe:
  - in `MLF.Elab.Inst.instInsideFn`, retained `TVar` bounds (instead of dropping to `Nothing`) by allowing `elabToBound` conversion for the `TVar` case.

- Result:
  - strict bug matrix remained unchanged at `3` failures (post-C1 baseline):
    - `make-app`, `let-c1-return`, `let-c1-apply-bool` still fail.
  - focused regressions stayed green, sentinel policy stayed `4 pending`.

- Conclusion:
  - this is not the controlling collapse point for BUG-2026-02-06-002.
  - patch was reverted; continue with another minimal hypothesis targeting the remaining OpGraft/OpRaise/OpWeaken interaction.

## 2026-02-10 — C5/C6 findings + reification-root insight

- C5 (`WitnessCanon`: skip weaken reordering) and C6 (`Omega.graftArgFor`: prefer `etBinderArgs`) were both non-improving and reverted.
- A further temporary `Phi.Translate` alias-subst probe was also non-improving and reverted.

- New high-value diagnostic finding:
  - In the failing `let-c1-apply-bool` path, the problematic second graft arg node (`NodeId 23`) is an **unbounded `TyVar`** in solved constraint (`tnBound = Nothing`).
  - Yet Φ translation still reifies that arg as `TBottom` (`reifyTypeArg ... ty=TBottom`).

- Interpretation:
  - The remaining collapse is not due to missing binder/arg pairing in presolution witnesses (`ewSteps` and `etBinderArgs` are aligned),
  - but due to downstream reification/naming behavior in Φ translation (no-fallback naming path causing unbounded TyVars to collapse to `TBottom` under current context).

- Practical direction update:
  - Next hypothesis should target a thesis-safe naming/reification path that preserves unbounded graft-arg variables during Φ translation without leaking solved-node names (`H15` guard).

- Additional C8 insight (2026-02-10):
  - fixing trace binder-name lookup in `traceArgMap` fills inferred map entries (`a -> Int`, `b -> TBottom`) for the failing edge,
  - but does not change strict outcomes because the second arg still concretely reifies to `TBottom`.
  - This narrows the remaining issue further to how unbounded TyVars are reified under current no-fallback naming context (arg becomes `TBottom` rather than a reusable variable/name).

- executing-plans batch gate note (2026-02-10):
  - plan regex `OpGraft|OpWeaken|OpRaise|scheme-aware Φ` matched `0 examples` in this harness; explicit suite-name matches are required for reliable verification.
  - explicit Ω/Φ + witness suites remain green, while BUG-2026-02-06-002 strict/thesis targets remain red.

- C9 finding (2026-02-10):
  - even when forcing unbounded graft-arg `TBottom` reification to binder variable (`TVar "b"`) in `reifyTypeArg`, strict bug shapes remain unchanged.
  - This indicates the remaining lock-in is not solely due to that local `TBottom` token; earlier/later instantiation structure (not just arg naming) still drives the mismatch.

### Post-C1 probe C10 finding (2026-02-10)

- Hypothesis tested:
  - unresolved codomain lock-in might come from `SchemeInfo` substitution remapping gaps between binder keys and argument keys in trace copy-map space.
- Probe:
  - expanded `Phi.Translate` remapping (`remapSchemeInfoM` + `remapSchemeInfo`) to synthesize binder-name aliases for canonicalized argument nodes using direct/copy/reverse-copy lookup and `etBinderArgs`.
- Result:
  - no strict-gate movement (`BUG-2026-02-06-002 strict target matrix` stayed `3` failures after C1).
  - no focused regression and sentinel policy remained intact (`4 pending`).
- Interpretation:
  - copy-space alias propagation in `Translate` naming/remap is not the dominant blocker for remaining BUG-2026-02-06-002 shapes.
  - remaining root cause likely sits earlier in Φ/Ω operation semantics (pre-translation), especially around `OpGraft`/`OpRaise` specialization and downstream codomain `TBottom` lock-in.
- Action:
  - reverted the C10 probe.

### Post-C1 C11/C12 findings (2026-02-10)

- High-confidence local mechanism for remaining failure:
  - On failing edge-0, the codomain collapse to `TBottom` is introduced at the **final `OpWeaken`** elimination step, not at `OpRaise`.
  - Operationally: `∀u1. Int -> u1 -> Int` is weakened to `Int -> ⊥ -> Int` when `u1` is eliminated while still unbounded and not marked keep.

- New structural diagnosis:
  - `keepBinderKeys` being empty on this path is likely the proximate trigger for the residual bug.
  - However, naive broadening of keep-key selection from reachable target type nodes over-preserves polymorphism and regresses nearby let-use behavior.

- Consequence for next probe design:
  - any fix must be **narrow** and operation-aware (likely edge-local / step-local), preserving exactly the binder needed for application polymorphism without globally suppressing elimination semantics.

### Post-C1 C13 finding (2026-02-10)

- A narrow-looking weaken guard in Ω (`preserveByRigidAlias`) is still semantically over-broad in practice:
  - it changes failure shape for BUG strict cases but does not produce a strict pass,
  - and it regresses nearby let-use polymorphism/H15 guards.
- Implication:
  - direct local weakening suppression is not sufficient; final fix likely needs a more principled edge/phase contract (e.g., precise keep-set derivation tied to expected target arity/polymorphism, not structural alias heuristics alone).

### Post-C1 C14 finding (2026-02-10)

- Switching keep-key target-root from `ewRight` to `ewRoot` alone is behaviorally neutral for BUG-2026-02-06-002 strict gates.
- Interpretation:
  - root-anchor mismatch is not the dominant cause of residual codomain `TBottom` lock-in.
  - remaining issue is likely in edge-local elimination policy (which binders may be safely weakened) rather than simple root choice for keep-key candidate enumeration.

### Post-C1 C15 finding (2026-02-10)

- Simply adding trace binder-arg nodes to Φ reification `namedSet` is too permissive.
- It does not improve strict BUG-2026-02-06-002 targets and regresses nearby let-use polymorphism behavior.
- Interpretation:
  - residual issue is not a plain “arg nodes were hidden from naming” bug;
  - fix likely requires a tighter contract between operation translation (`OpGraft/OpWeaken`) and per-edge keep/elimination policy, not global named-set broadening.

### Post-C1 C16 finding (2026-02-10)

- Narrow singleton-case weaken suppression (`OpWeaken`) is still too invasive for adjacent behavior.
- Even with strict structural guards, local suppression of elimination in Ω causes focused regressions without strict-target improvement.
- Interpretation:
  - residual fix likely cannot be encoded as ad hoc weaken-skip policy; needs a principled upstream contract that yields correct keep/eliminate decisions before Ω translation.

### Post-C1 C17 finding (2026-02-10)

- Upstream witness normalization pruning of unbounded-var weaken steps is still too coarse.
- Even on the upstream path (not Ω interpreter), it changes failure family (`TCExpectedArrow`) without strict-target improvement and regresses the H15 non-leak focused guard.
- Interpretation:
  - the residual bug is not solved by a simple “drop weaken on unbounded var graft” rule; required condition likely depends on richer edge-local typing context than witness-op shape alone.

### Post-C1 C18 finding (2026-02-10)

- A delayed `OpWeaken` for the same binder after `OpGraft` is a real remaining collapse source in BUG-2026-02-06-002 paths.
- Treating that delayed pair as binder application (`InstApp`) plus consuming the delayed weaken improves strict outcomes:
  - `make-app` moves from FAIL to PASS,
  - strict target matrix improves from `3` to `2` failures,
  - focused/H15 guards remain green,
  - sentinel policy remains intact (`4 pending`).
- Remaining failures are now concentrated in let-binding scheme mismatch forms:
  - `let-c1-return`: `forall a. t16 -> Int` vs `forall a. a`
  - `let-c1-apply-bool`: `forall a b. b -> Int` vs `forall a b. b -> a`

### Post-C1 C19 finding (2026-02-10, reverted)

- A local elaboration fallback that derives let schemes from elaborated RHS type when scheme mismatch is detected is non-viable.
- It regresses the improved C18 state by reintroducing `make-app` failure as `TCInstantiationError InstElim ... expects forall`.
- Interpretation:
  - the residual issue is not safely fixable via ALet-local scheme fallback; root cause remains upstream in how let scheme roots/witness-shape interact under redirects/canonicalization.

### Post-C1 C20 finding (2026-02-10, reverted)

- An ALet-local fallback path can partially reduce strict BUG-2026-02-06-002 failures, but it is not stable under focused guard policy.
- Variant with env-aware RHS checking reduced strict matrix to a single failing shape (`let-c1-apply-bool`), indicating useful signal about residual let-scheme mismatch.
- However, the same variant changes the H15-focused mismatch behavior for the make-let guard (`TCArgumentMismatch`), which is treated as a regression.
- Conclusion:
  - ALet-local fallback remains too behavioral for acceptance.
  - keep C18 as retained best state; continue upstream/Φ-path investigation for the final codomain/argument lock-in without changing H15 guard behavior.

### Post-C1 C21/C21.1 finding (2026-02-10, retained)

- Let-scheme mismatch can be corrected in a thesis-compatible way by:
  1) using env-aware RHS typing (`typeCheckWithEnv`) to derive fallback let schemes only when generalized scheme is not alpha-equivalent (C21), and
  2) keeping app-side monomorphic argument repair for `InstApp TForall{}` at `AApp` sites (C21.1).
- Combined with C18, this clears strict BUG-2026-02-06-002 and thesis-target checks while preserving focused guards.
- Key practical outcome:
  - strict matrix and thesis-target examples now pass,
  - sentinel matrix intentionally remains pending (`4 pending`) until formal sentinel graduation step.

### 2026-02-10 step-1/2/3 batch finding

- Sentinel graduation is complete: `BUG-2026-02-06-002` pending tests are now strict assertions and pass.
- Bug-target evidence is now fully green:
  - strict target matrix green,
  - thesis-target checks green,
  - full BUG matcher green (`10 examples, 0 failures`).
- Full-project verification remains blocked by 5 broader regressions outside this bug-target matrix (current failing modules: `test/Presolution/WitnessSpec.hs`, `test/ElaborationSpec.hs`).

### Post-C1 C21.2 finding (2026-02-10)

- Narrowing fallback to app + unbounded-scheme + Int-codomain preserves BUG-2026-02-06-002 green status, but does not resolve broader pre-existing/full-suite blockers.
- Conclusion: BUG-2026-02-06-002 matrix is now strict-green; remaining work should shift to non-target regressions surfaced by full-gate verification.

### 2026-02-10 closure finding — final root-cause split and stable fix set

- The remaining full-suite blockers were split into two independent classes:
  1. witness-shape/Ω locality mismatch (`OpGraft` delayed-weaken reliance), and
  2. over-aggressive scheme simplification for named structured bounds.
- Stable closure required fixing both layers:
  - **Upstream witness normalization** must deliver adjacent/safe graft-weaken shape; Ω should remain local and deterministic.
  - **Scheme normalization** must preserve explicit/named bound structure (no forced structured-bound inline for named binders).
- After these were in place, a final let-lambda scheme mismatch was resolved by using a lambda fallback branch that resets substitution (`IntMap.empty`) when replacing scheme from env-aware RHS typing.
- Net effect: bug matrix stays green while all non-target regressions are cleared.

### 2026-02-10 subagent-driven follow-up finding — thesis delayed-weakening clarity

- Condition (5) validation for delayed weakenings now has an explicit error surface:
  - `DelayedWeakenViolation weakenedBinder offendingNode`.
- Why this matters:
  - previous `OpUnderRigid` overloading conflated rigid-path failures with delayed-weaken ordering failures;
  - explicit constructor makes witness diagnostics and test intent match thesis Definition 11.5.2 semantics.
- Added/updated regression evidence:
  - `flags delayed-weakening violations when later ops touch strict descendants`.
  - condition-5 tests now assert `DelayedWeakenViolation parent child`.
- Additional quality note:
  - removed redundant `foldl'` import in `WitnessCanon` to keep warning-free build status.
