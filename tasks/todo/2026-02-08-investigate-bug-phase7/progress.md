# Progress Log

## Session: 2026-02-08

### Phase 1: Requirements & Discovery
- **Status:** complete
- **Started:** 2026-02-08
- Actions taken:
  - Recorded user request and constraints
  - Set up planning/todo folder with phase structure
  - Documented goals/findings for the Phase 7 `make` failure
- Files created/modified:
  - tasks/todo/2026-02-08-investigate-bug-phase7/task_plan.md (created/updated)
  - tasks/todo/2026-02-08-investigate-bug-phase7/findings.md (updated)
  - tasks/todo/2026-02-08-investigate-bug-phase7/progress.md (updated)

### Phase 2: Planning & Structure
- **Status:** in_progress
- Actions taken:
  - Mapped the generalization pipeline (`MLF.Elab.Elaborate` → `generalizeAtNode` → `applyGeneralizePlan`) and exposed the `TargetPlan`/`TypeRootPlan` heuristics that reify schemes.
- Files created/modified:
  - tasks/todo/2026-02-08-investigate-bug-phase7/progress.md (updated)

## Test Results
| Test | Input | Expected | Actual | Status |
|------|-------|----------|--------|--------|
|      |       |          |        |        |

## Error Log
| Timestamp | Error | Attempt | Resolution |
|-----------|-------|---------|------------|
| 2026-02-08 | session-catchup.py path missing | 1 | Noted and proceeding |

## 5-Question Reboot Check
| Question | Answer |
|----------|--------|
| Where am I? | Phase 2 |
| Where am I going? | Planning → Implementation → Testing → Delivery |
| What's the goal? | Trace why `make` let-scheme specializes while RHS stays polymorphic and recommend fix path |
| What have I learned? | Need to inspect elaboration/generalization flow around `make` detail |
| What have I done? | Established planning files and captured requirements |

---
*Update after completing each phase or encountering errors*

### Phase 1: Root Cause Investigation (Systematic Debugging)
- **Status:** in_progress
- Actions taken:
  - Reproduced `BUG-2026-02-06-002` from `Bugs.md` via a standalone runghc script.
  - Confirmed failure is currently Phase 7 `TCLetTypeMismatch`, not Phase 6 `PhiTranslatabilityError`.
  - Captured concrete mismatch payload for let-annotation vs RHS elaborated type.
- Commands run:
  - `cabal exec runghc /tmp/repro-bug-2026-02-06-002.hs`

### Phase 2: Pattern Analysis
- **Status:** in_progress
- Actions taken:
  - Built a behavior matrix across related expressions (make-only, make-apply, make-c1, make-ignore-c1, id-id).
  - Confirmed failures cluster around `make`-style constant function let-polymorphism.
  - Captured elaboration trace showing explicit divergence between let scheme and RHS term type.
- Commands run:
  - `cabal exec runghc /tmp/repro-make-matrix.hs`
  - `cabal exec runghc /tmp/repro-bug-2026-02-06-002-trace.hs > /tmp/repro-bug-2026-02-06-002-trace.out 2>&1`

### Phase 2: Pattern Analysis (continued)
- Actions taken:
  - Dumped `solvedClean` vs `solvedForGen` nodes and `gaSolvedToBase`/`gaBaseToSolved` mappings in internal REPL context.
  - Confirmed suspicious mapping collisions and non-copy copy-map entries (`0->5`, `1->5`).
- Commands run:
  - `cabal repl lib:mlf2-internal` + `/tmp/repro-constraint-dump.hs`
  - `cabal repl lib:mlf2-internal` + `/tmp/dump-edge-traces.hs`

### Phase 3: Hypothesis and Minimal Tests
- **Status:** in_progress
- Actions taken:
  - Ran four isolated hypothesis tests against generalization/copy-mapping behavior.
  - Reverted all experimental source edits after each unsuccessful or regressive outcome.
  - Confirmed no net source changes remain outside task docs.
- Hypothesis test log:
  - H1: `Phase2` solved→base union precedence tweak (no observable fix).
  - H2: let-generalization at scheme root (regressed to `TCTypeAbsBoundMentionsVar` / `PhiInvariantError`; reverted).
  - H3: `preferBaseVar` always base-preferred (shifted to `SchemeFreeVars`; reverted).
  - H4: `Finalize` solved→base precedence tweak (improved mapping diagnostics, bug persisted; reverted).
- Commands run:
  - `cabal repl lib:mlf2` + `/tmp/repro-bug-2026-02-06-002.hs`
  - `cabal repl lib:mlf2` + `/tmp/repro-make-matrix.hs`
  - `cabal repl lib:mlf2` + `/tmp/repro-bug-2026-02-06-002-trace.hs`
  - `cabal repl lib:mlf2-internal` + `/tmp/repro-constraint-dump.hs`
  - `cabal repl lib:mlf2-internal` + `/tmp/repro-generalize-target.hs`

### Workspace State
- Source files: reverted to baseline (no functional code changes retained).
- Task documentation: updated with evidence, hypothesis outcomes, and root-cause direction.

### Phase 3: Hypothesis and Minimal Tests (continued)
- Actions taken:
  - Verified representative-gating behavior by correlating binder-filter traces with solved→base mappings.
  - Confirmed `NodeId 25` can satisfy primary filter predicates yet still be dropped before `bindersFiltered`, consistent with `baseGammaRep` representative gating.
  - Revalidated baseline reproducer after reverting all source experiments.
- Commands run:
  - `cabal repl lib:mlf2` + `/tmp/repro-bug-2026-02-06-002-trace.hs`
  - `cabal repl lib:mlf2-internal` + `/tmp/repro-constraint-dump.hs`
- Result:
  - Root-cause direction narrowed to binder representative selection/retention rather than Φ translation logic.
- Additional hypothesis probes (reverted):
  - H5: representative-gate relaxation in `BinderPlan.Build`.
  - H6: `solvedForGen` with fresh/empty union-find.
- Both retained the same user-visible failure for `BUG-2026-02-06-002`.

## Session: 2026-02-09

### Phase 2: Pattern Analysis (systematic-debugging continuation)
- **Status:** complete
- Actions taken:
  - Re-ran fresh checked+unchecked traces for `BUG-2026-02-06-002`.
  - Ran additional pattern matrix (`k1`/`k2`/`id`) to confirm failure clustering.
  - Compared direct `generalizeAt` outputs for scheme root vs `schemeBodyTarget` on both `make` and `id`.
  - Dumped node+bound+parent state across stages (`base`, `presolution`, `solved`, `constraintForGen`) for `make`.
  - Isolated delta between `solvedClean` and `solvedForGen` by calling `generalizeAt` on each:
    - `solvedClean` at target `4` -> `SchemeFreeVars`
    - `solvedForGen` at target `4` -> specialized `a -> b -> Int`.
- Commands run:
  - `cabal repl lib:mlf2 -v0` + `/tmp/repro-bug-2026-02-06-002-trace.hs`
  - `cabal repl lib:mlf2 -v0` + `/tmp/repro-bug-002-checked-trace.hs`
  - `cabal repl lib:mlf2 -v0` + `/tmp/repro-k-matrix.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/repro-generalize-target.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/repro-generalize-target-id.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/dump_node_stages_make.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/dump_node_stages_id.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/dump_parents_make.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/compare_generalize_solved_vs_forgen.hs`
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/repro-constraint-dump.hs`

### Phase 2 Outcome
- Isolated corruption site to generalization preprocessing/finalization reparenting path:
  - copy-derived nodes are reassigned into `make` scope during `constraintForGeneralization` alignment,
  - after which reification of `schemeBodyTarget` observes the `Int`-specialized path.
- Candidate touchpoint for Phase 3 hypothesis tests:
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs` (`bindParentsFinalAligned''` copy-parent reassociation block).

### Phase 3: Single-hypothesis patch at Finalize.hs line ~95
- **Status:** complete (hypothesis rejected)
- Hypothesis H7:
  - Replacing reassociation `IntMap.insert` with `IntMap.insertWith keepOld` in `bindParentsFinalAligned''` may preserve prior parent ownership and prevent copy-node leakage into `make` scope.
- Change applied (temporary):
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs`
    - one-line change in reassociation fold.
- Verification run:
  - RED baseline: `cabal repl lib:mlf2 -v0` + `/tmp/repro_bug_current.hs` -> Phase 7 mismatch (unchecked + checked).
  - GREEN check after patch: same command -> unchanged mismatch.
  - Data-flow check after patch:
    - `cabal repl lib:mlf2-internal -v0` + `/tmp/repro-constraint-dump.hs`
    - `node25/node12/node30` parents in `constraintForGen` remained `GenNodeId 1`.
- Outcome:
  - No behavior change and no ownership-map change on target nodes.
  - Patch reverted immediately; no source code change retained from H7.

### Phase 3: Next single-hypothesis patch at same Finalize block
- **Status:** complete (hypothesis rejected)
- Hypothesis H8:
  - Skip reassociation when copied node already has a non-root parent in the aligned map (instead of overwrite-policy-only).
- Change applied (temporary):
  - `/Volumes/src/mlf4/src/MLF/Elab/Run/Generalize/Finalize.hs`
    - added local `hasNonRootParent` check in the `bindParentsFinalAligned''` fold guard.
- Verification run:
  - `cabal repl lib:mlf2 -v0` + `/tmp/repro_bug_current.hs` -> unchanged Phase 7 mismatch (unchecked + checked).
  - `cabal repl lib:mlf2-internal -v0` + `/tmp/repro-constraint-dump.hs` -> unchanged parent ownership (`node12/node25/node30` still under `GenNodeId 1`).
- Outcome:
  - No behavior/dataflow change on target path.
  - Patch reverted immediately; no retained source change from H8.

### Phase 3: Hypothesis H9 (Phase4 instCopy owner lock)
- **Status:** complete (hypothesis rejected)
- Actions taken:
  - Added temporary strict-owner guards for `instCopyNodes` in:
    - `overrideSchemeInteriorParentsWith`
    - `bindParentsFinal`
  - Also captured binding trace with `tcBinding=True` to verify where specialization remains.
- Commands run:
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 > /tmp/repro_bug_h9_phase4.out ...`
    - `:l /tmp/repro_bug_current.hs`
    - `main`
  - `cat <<'EOF' | cabal repl lib:mlf2-internal -v0 > /tmp/repro_constraint_dump_h9_phase4.out ...`
    - `:l /tmp/repro-constraint-dump.hs`
    - `main`
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 > /tmp/bug002_bindtrace_h9_patch.out ...`
    - `:l /tmp/repro-bug-002-bindtrace.hs`
    - `main`
- Result:
  - User-visible failure unchanged: Phase 7 `TCLetTypeMismatch` in checked + unchecked.
  - Ownership shifted only partially (`node25` -> `GenNodeId 4`), but `node12`/`node30` still in `GenNodeId 1`.
  - `ty0Raw` for `make` remained specialized to `a -> b -> Int`.
- Action:
  - Reverted H9 source edits.

### Phase 3: Hypothesis H10 (base-only schemeRootOwnersBase seed)
- **Status:** complete (hypothesis rejected)
- Actions taken:
  - One-line test in `Phase4` to seed `schemeRootOwnersBase` from `schemeRootsBaseSet` instead of `schemeRootsAllSet`.
- Commands run:
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 > /tmp/repro_bug_h10_phase4.out ...`
    - `:l /tmp/repro_bug_current.hs`
    - `main`
  - `cat <<'EOF' | cabal repl lib:mlf2-internal -v0 > /tmp/repro_constraint_dump_h10_phase4.out ...`
    - `:l /tmp/repro-constraint-dump.hs`
    - `main`
- Result:
  - Same Phase 7 mismatch persists.
  - No meaningful ownership improvement on `12/25/30`.
  - Introduced `-Wunused-local-binds` warning due unused `schemeRootsAllSet`.
- Action:
  - Reverted H10 immediately; no source-code deltas retained.

### Phase 3: Hypothesis H11 (local scheme-body alias reify at `Generalize.hs` line ~635)
- **Status:** complete (hypothesis rejected)
- Actions taken:
  - Added temporary H11 patch in `src/MLF/Elab/Generalize.hs`:
    - destructured `rpSubstForBound`
    - when a single local scheme-body alias binder matched `typeRootC`, reified `ty0Raw` with binder-local substitution.
- Commands run:
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 > /tmp/repro_bug_h11.out ...`
    - `:l /tmp/repro_bug_current.hs`
    - `main`
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 > /tmp/bug002_bindtrace_h11.out ...`
    - `:l /tmp/repro-bug-002-bindtrace.hs`
    - `main`
- Result:
  - Reproducer remained unchanged: unchecked+checked still `Phase 7 TCLetTypeMismatch`.
  - Trace for `make` still shows specialized scheme body reification:
    - `generalizeAt: ty0Raw=TArrow (TVar "a") (TArrow (TVar "b") (TBase Int))`
  - No user-visible improvement; hypothesis rejected.
- Action:
  - Reverted H11 patch immediately; no retained source-code delta.

### Phase 3: Hypothesis H12 (remove nestedSchemeInteriorSet filter in substAliasesFromBaseLocal)
- **Status:** complete (partially confirmed — error shifted, not fixed)
- Actions taken:
  - Removed `nestedSchemeInteriorSet` filter from `substAliasesFromBaseLocal` in `ReifyPlan.hs` line ~143.
- Commands run:
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 ...` + `/tmp/repro_bug_current.hs`
  - `cat <<'EOF' | cabal repl lib:mlf2 -v0 ...` + `/tmp/repro-bug-002-bindtrace.hs`
- Result:
  - Error shifted from Phase 7 `TCLetTypeMismatch` to Phase 6 `PhiInvariantError "PhiReorder: missing binder identity at positions [0]"`.
  - `ty0Raw` changed: `a -> b -> Int` → `a -> b -> c` (Int specialization eliminated).
  - Subst gained `(25,"c")` but mapping is incorrect — should be `"a"` not `"c"`.
  - `solvedToBasePref[25]` maps to base node 5/6 (rep 5, name "c") instead of base node 1 (rep 1, name "b") or 0 (rep 0, name "a").
  - Self-referential bound on binder 5 ("c"): `a -> b -> c` causes Phi failure.
- Action:
  - Reverted H12 patch; baseline source restored.

### Workspace State
- Source files: reverted to baseline (no functional code changes retained).
- Task documentation: updated with H12 evidence and next-direction pointer toward `solvedToBasePref` mapping in GammaPlan.

## Session: 2026-02-10

### Phase 3: Hypothesis H13 (fix solvedToBasePref[25] mapping in GammaPlan)
- **Status:** complete (confirmed — regression-free)
- Root cause identified:
  - Two positional zips (`qAlignSolvedToBaseLocal` and `alignSolvedToBase`→`alignPrefer`) incorrectly pair copy-derived node 25 with wrong base nodes because copy-derived nodes inflate the solved gamma set.
  - `alignPrefer[25] = NodeId 5` (incorrect) overrides `solvedToBase[25] = NodeId 0` (correct from copyOverrides) at highest precedence.
- Changes applied (retained):
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/Target/GammaPlan.hs`:
    1. `alignPrefer` filter: reject entries where `solvedToBase` disagrees AND the `solvedToBase` target is a gamma binder different from the key.
    2. `solvedToBasePrefLocal` precedence: reordered to `identityGammaScoped > preferGamma > solvedToBase > qAlign > identityGamma`.
- Verification:
  - Full test suite: **584/584 pass** (zero regressions).
  - Make reproducer: still shows Phase 7 `TCLetTypeMismatch` with `a -> b -> Int` (because H12/H14 not yet applied).
  - `solvedToBasePref[25]` now correctly maps to `NodeId 0`.

### Phase 3: Hypothesis H14 (targeted nestedSchemeInteriorSet bypass using type-root reachability)
- **Status:** complete (confirmed — regression-free)
- Problem: All previous H12 variants (removing/bypassing `nestedSchemeInteriorSet` filter) caused 4 id-id test regressions (`OpGraft: binder not found in quantifier spine`).
- Key insight: In `make`, node 25 is reachable from the type root (it appears in the return type `a -> b -> a`). In `id-id`, copy-derived nodes 12/17 are NOT reachable from the type root (they're internal instantiation nodes).
- Change applied (retained):
  - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`:
    - In `substAliasesFromBaseLocal`, bypass the `nestedSchemeInteriorSet` filter when:
      1. The base key is NOT in `nestedSchemeInteriorSet` (copy-derived from outer scope), AND
      2. The solved key IS reachable from the type root (`rpiReachableFromWithBounds rpiTypeRoot`).
- Verification:
  - Full test suite: **584/584 pass** (zero regressions).
  - Make reproducer: scheme type now correct `forall a b. a -> b -> a` (Int specialization eliminated!).
  - Remaining error: `TCLetTypeMismatch` with `t23` vs `b` naming mismatch in annotation:
    - Annotation: `forall a b. a -> t23 -> a`
    - Elaborated: `forall a b. a -> b -> a`
  - This is a separate naming issue in the let-scheme annotation path, not the scheme type itself.

### Combined H13+H14 Summary
- **Two retained source changes** fix the core Int specialization bug:
  1. GammaPlan.hs: correct `solvedToBasePref` mapping for copy-derived nodes
  2. ReifyPlan.hs: allow copy-derived nodes to get alias names when reachable from type root
- **Remaining work**: `t23` vs `b` naming mismatch in the let-scheme annotation (separate issue).

### Workspace State
- Source files: H13 (GammaPlan.hs) + H14 (ReifyPlan.hs) changes retained.
- Task documentation: updated with H13+H14 results.
