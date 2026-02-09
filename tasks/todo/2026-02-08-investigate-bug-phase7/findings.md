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
