# Findings: BUG-2026-02-11-004 Bounded-Alias InstBot/Phi Gap

## Baseline
- Open bug entry: `/Volumes/src/mlf4/Bugs.md` (`BUG-2026-02-11-004`).
- Expected: checked/unchecked pipelines elaborate `BUG-003-V1/V2` to `∀a. a -> a -> a -> a`.
- Actual: strict `InstBot` invariant failure bucket.

## Active Questions
1. Where does non-`TBottom` input to `InstBot` originate in the BUG-003 path?
2. Is this a presolution witness generation issue, Phi translation/remap issue, or Omega execution/context issue?
3. Which minimal boundary contract change restores thesis behavior without weakening strict checks?

## 2026-02-15 Reproduction Evidence
- Seeded sentinel slice is green (`2 examples, 0 failures`) because tests assert the known failure bucket.
- Direct pipeline execution of BUG-003-V1 and BUG-003-V2 reproduces the live defect in both pipelines:
  - `TCInstantiationError (InstBot TBottom) ... "InstBot expects TBottom, got t44 -> t44 -> t44 -> t44"`.
- The failure is deterministic and shared across both variant shapes, so root cause is likely in common instantiation construction (Phi/Omega/elaboration), not variant-specific parsing or normalization.

## 2026-02-15 Deep Trace Findings
- `BUG-003-V1/V2` are still reproducible as deterministic strict failures in both pipelines.
- Edge-0 is the critical divergence point:
  - Ω witness is non-empty (`OpGraft/OpRaise` sequence), but Φ currently collapses to `InstId` for this edge.
  - `reifyInst` then falls back to `ExpInstantiate` arg nodes with no direct scheme source (`mSchemeInfo = Nothing`), producing a mixed `InstInside (InstBot ...)` chain that later violates strict `InstBot` semantics.
- Presolution/canonicalization evidence indicates identity-domain complexity on edge-0:
  - witness target node is `1`, while trace binder args and copy-map involve aliases `35/23` and additional binders `4/6`.
  - expansion arity (`5` args) does not align with the generalized binder spine expected at the annotation site.
- Failed minimal patches show symptoms can be shifted but not resolved in isolation:
  - fallback gating removes Phase-7 strict crash but leaves bottom-polluted let type mismatch.
  - binder-aware graft arg recovery surfaces a stricter Phase-6 bound mismatch.
- Current root-cause hypothesis (architectural):
  - Higher-arity bounded-alias edge-0 is crossing mismatched identity contracts between presolution witness/trace emission and Φ binder-bound consumption.
  - Local patches in only one layer (fallback or Ω arg choice) are insufficient; fix likely needs coordinated contract alignment across witness arg provenance + Φ binder matching.

## 2026-02-16 Contract Alignment Findings
- After preserving source-domain witness/trace provenance, raw edge-0 Ω steps carry source keys with binder target `0`:
  - `OpGraft 30 0 ; OpGraft 0 0 ; OpGraft 32 0 ; OpRaise 0 ; OpRaise 12`.
- Without source-ID adoption in Ω, this regresses to `MissingNode (NodeId 0)`.
- Adding source-ID adoption for op targets/args (ranked by `IdentityBridge`) removes `MissingNode`, but does not by itself close BUG-003.
- `Translate` was previously computing `targetBinderKeys` before deriving fallback scheme info (`siForOmega`) when `mSchemeInfo` is missing, which left binder key sets effectively stale/empty in edge-0 no-scheme paths.
  - Refactoring to derive `siForOmega` first fixes this ordering issue.
- Edge-0 now derives source-key substitutions `[(0,"a"), (1,"b")]` from remap/hydration, confirming source-domain remap is active.
- Remaining blocker is semantic, not plumbing:
  - With strict fallback guard (no raw `ExpInstantiate` fallback for no-scheme source), edge-0 currently replays to `phi = InstId`.
  - Let RHS remains bottomized (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`), causing deterministic `TCLetTypeMismatch` against expected `∀a. a -> a -> a -> a`.
- Interpretation:
  - Source-ID contract boundary is partially restored (no missing-node drift), but Ω raise replay still collapses in this higher-arity bounded-alias path (rigid handling + context routing), so BUG-003 closure is still open.

## 2026-02-16 Systematic-Debugging Refresh (Phase 1/2 + Hypothesis #4)
- Deterministic failure is re-confirmed with strict-success sentinels:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1481579064'`
  - both V1/V2 fail with the same `PipelineTypeCheckError (TCLetTypeMismatch ...)` and bottomized RHS `∀a. ⊥ -> t1 -> ⊥ -> ⊥`.
- Trace-driven Phase-1 evidence (`runPipelineElabWithConfig` + `tcGeneralize=True`) confirms the edge-0 collapse path:
  - `phi steps edge=0` still includes non-empty Ω operations.
  - both edge-0 raises in BUG-003 (`OpRaise 0`, `OpRaise 12`) hit rigid-skip after source adoption (`nAdopt=8` / `nAdopt=12`), producing `reifyInst phi edge=0 phi=InstId`.
  - elaborated let RHS is already bottomized before Phase 7 (`ETyAbs ... ELam "x" TBottom ...`), so typecheck mismatch is downstream symptom.
- Phase-2 comparison against the passing bounded-alias baseline (`b ⩾ a`) shows:
  - passing path also includes a rigid-skipped raise, so rigid skip alone is not sufficient to explain BUG-003;
  - key divergence is edge-0 op shape/targets:
    - passing: `OpGraft 23 0 ; OpGraft 23 0 ; OpRaise 8` → non-id phi (`InstInside (InstBot TBottom)` chain).
    - failing: `OpGraft 30 0 ; OpGraft 0 0 ; OpGraft 32 0 ; OpRaise 0 ; OpRaise 12` → `phi=InstId` under current routing.
- IdentityBridge probe for failing edge-0 (`mkIdentityBridge` with canonicalized trace):
  - `sourceKeysForNode 0 = [0,35,46]`
  - trace-binder order keys are `[0,1,2,4,6]`
  - this confirms source key `0` has no direct bridge candidate to binder key `1`; current raise adoption is therefore sensitive to bottom alias resolution.
- Hypothesis #4 (minimal): disable `OpRaise` bottom-to-root remap (`trace binder source + bottom => orderRoot`).
  - Result: causal change confirmed. Edge-0 no longer collapses to `InstId`; first raise becomes active (`nAdopt=46`, `parent=BindFlex`, `binderIndex=Just 0`) and `phi edge=0` becomes non-noop (`InstSeq InstIntro ...`).
  - But BUG-003 still fails (`TCLetTypeMismatch`) with a new bottomized quantifier shape:
    - RHS becomes `∀a. ∀(u0 ⩾ ⊥ -> ⊥). ∀t1. ⊥ -> t1 -> ⊥ -> ⊥`.
  - Interpretation: bottom-to-root remap contributes to collapse, but removing it is not sufficient. Remaining root cause includes graft/raise interaction in edge-0 source-target routing and bound reconstruction.

## 2026-02-16 Plan-Level Root-Cause Evidence (Presolution → Generalize mismatch)
- Direct pass/fail plan extraction (using `PresolutionPlanBuilder` + `applyGeneralizePlan`) shows edge-0 scheme generation is already divergent before Φ replay:
  - PASS edge-0 (`b ⩾ a`):
    - trace binders: `{0,1,3}`
    - edge-0 scheme/subst: `Forall [("a",Nothing),("b",Just ...)] TVar "b"`, `subst={0->"a",5->"b"}`
    - witness steps target binder `0`, which is in-scheme.
  - FAIL edge-0 (`BUG-003-V1`):
    - trace binders: `{0,1,2,4,6}`
    - edge-0 scheme/subst: `Forall [("a",Just ⊥->⊥),("b",Just ⊥->⊥->a)] TVar "b"`, `subst={4->"a",8->"b",38->"a"}`
    - witness steps still target binder `0` (`OpGraft ... 0`, `OpRaise 0`), which is not in-scheme.
- Key structural discrepancy:
  - In FAIL edge-0, trace binder/arg IDs contain source-domain keys not present in solved nodes:
    - `binder 0` does not exist in solved nodes (`lookupNodeIn` miss), canonical remains `0`.
    - `binder 2` also missing; `arg 31` missing.
  - In PASS, one missing binder key (`1`) canonicalizes to existing key `0`, so replay remains coherent enough.
- Interpretation:
  - The active source-ID contract preserves trace provenance keys that can be “ghost” relative to solved-node space.
  - For BUG-003 edge-0, ghost key `0` is consumed as an operation target but absent from the generated scheme key set; Ω adoption then falls back through bottom aliases (`sourceKeysForNode 0 = [0,35,46]`), which drives raise/graft replay toward no-op or bottomized paths.
  - This is a cross-boundary mismatch: presolution trace provenance key-space vs generalize/Φ binder key-space.

## 2026-02-16 Bridge-hardening implementation findings
- Landed the planned source→replay bridge contract:
  - `MLF.Elab.Phi.Translate` computes:
    - `traceBinderSources` (deduped trace binder sources)
    - `traceBinderReplayMap` (source binder key -> replay binder key)
  - `MLF.Elab.Phi.Omega` consumes both via `OmegaContext` and resolves binder-target operands in `OpGraft` target/`OpWeaken`/`OpRaise` execution target/`OpMerge`/`OpRaiseMerge`.
- Added focused regression:
  - `ElaborationSpec`: `fails fast when OpWeaken targets a trace binder source with no replay binder mapping`.
  - Confirmed red->green TDD cycle for this case.
- Preserved thesis split for `OpRaise`:
  - source-domain key remains authoritative for `I(r)` translatability checks;
  - resolved replay key is used for execution path.
- `BUG-003-V1/V2` status after bridge landing:
  - still failing with the same bottomized let mismatch (`∀a. ⊥ -> t1 -> ⊥ -> ⊥` vs expected `∀a. a -> a -> a -> a`).
  - bridge contract hardening did not close the bounded-alias semantic replay gap.
- Additional discovery during full gate:
  - multiple pipeline anchors now fail with `PhiInvariantError "trace/replay binder key-space mismatch"` in non-BUG-003 paths (e.g. `make` strict matrix).
  - tracked as new open bug `BUG-2026-02-16-010`.

## 2026-02-16 Replay-hint + positional-seeding follow-up findings
- Added presolution replay hints (`etBinderReplayHints`) and fed them into Φ bridge construction; Ω mismatch diagnostics now include hint domain.
- Added positional replay seeding in `computeTraceBinderReplayBridge` (trace-order source binders zipped with replay-subst keys) in addition to hint/name/alias candidates.
- This fixed the previous BUG-010 strict-matrix reproducer:
  - `/Pipeline (Phases 1-5)/BUG-2026-02-06-002 strict target matrix/make-app keeps codomain Int without bottom-domain collapse/` is now green.
- BUG-003 remains unchanged in failure bucket:
  - both V1/V2 still fail as `TCLetTypeMismatch` with bottomized RHS `∀a. ⊥ -> t1 -> ⊥ -> ⊥`.
  - trace evidence still shows edge-0 `phi = InstId` after replay mapping (`0 -> 4`) because all three grafts are no-op under bounded replay binder handling and both raises are rigid-skipped.
- Hypothesis results (reverted):
  - bypassing rigid-skip for traced raises changed shape (extra bottomized quantifier) but did not close BUG-003.
  - forcing bounded-graft replay to emit concrete instantiation violated strict InstBot invariant (`InstBot expects ⊥`) and was reverted.

## 2026-02-16 Continued hypothesis cycle (post-hybrid)
- Re-validated the same strict-success blocker after replay-hint landing:
  - edge-0 still routes source binder `0` to replay binder `4` (rigid-bounded), so `OpGraft` stays no-op and `OpRaise 0` rigid-skips under baseline contract.
- Compared edge-0 witness semantics by toggling annotation-edge weaken suppression:
  - keeping weakens on annotation edges exposed `WitnessNormalizationError (AmbiguousGraftWeaken (NodeId 35) [30,32,35])`.
  - this indicates BUG-003 edge-0 has multiple graft candidates for one weakened replay binder when weakens are preserved.
- Deterministic disambiguation experiment for ambiguous graft/weaken removed that normalization error but did not close BUG-003; failure returned to baseline `TCLetTypeMismatch`.
- Additional Ω raise-skip probes confirmed prior conclusion:
  - traced-only rigid-skip bypass changes shape but remains red.
  - global rigid-skip bypass fails earlier with `OpRaise target outside I(r)` on `OpRaise 12`.
- Outcome of this cycle:
  - no strict-success closure yet.
  - blocker remains a thesis-sensitive replay contract issue at edge-0 where source-domain witness intent is not reconstructed as a non-id Φ replay effect in the all-rigid replay slice.

## 2026-02-16 deterministic graft+weaken normalization pass findings
- Implemented annotation-edge deterministic synthesis in `WitnessNorm`:
  - segment-local (`StepIntro` boundaries preserved),
  - trigger on ambiguous multi-graft/no-weaken shape,
  - explicit synthesized `OpGraft+OpWeaken` output.
- Added explicit fail-fast constructor:
  - `DeterministicGraftWeakenSynthesisFailed NodeId [NodeId]` in `OmegaNormalizeError`.
- New targeted regressions in `test/Presolution/WitnessSpec.hs` are green:
  - annotation-edge synthesis success (deterministic arg choice),
  - fail-fast when no live candidate arg exists,
  - non-annotation guard (no synthesis).
- Matrix impact:
  - strict anchors and BUG-010 reproducer remain green,
  - BUG-003 does not close; failure class shifted to earlier Φ invariant mismatch:
    - `trace/replay binder key-space mismatch` on synthesized `OpGraft+OpWeaken` with raw target `NodeId 6`.
- Interpretation:
  - deterministic synthesis is active and test-covered,
  - but replay-domain coverage for synthesized binder target selection is still incomplete for BUG-003 edge-0 (`trace binder sources {0,1,2,4,6}` vs replay-map domain `{0,1,2,4}` in this pass).

## 2026-02-16 replay-bridge continuation findings
- Root-cause confirmation for the new mismatch bucket:
  - BUG-003 edge-0 synthesized `OpGraft+OpWeaken` targets source key `6`,
  - replay bridge originally built no mapping for `6`,
  - even though `6` shared replay-hint provenance with mapped source binders.
- `computeTraceBinderReplayBridge` under-coverage cause:
  - candidate seeding used only per-source direct hint/name/alias candidates,
  - it did not seed from other source binders in the same replay-hint class.
- Minimal bridge fix outcome:
  - adding hint-class peer candidate seeding maps source key `6` (`traceBinderReplayMap` now includes `(6 -> 4)` in BUG-003 traces),
  - removes the early `trace/replay binder key-space mismatch` failure.
- Downstream bounded-branch failure (new bucket) was semantic:
  - `OpGraft+OpWeaken(bound-match)` emitted `InstApp` under non-`⊥` bound,
  - `applyInstantiation` rejects this shape (`InstBot expects ⊥` on bound checking),
  - replacing bound-match with binder elimination (`InstElim`) restores strict behavior and removes that invariant crash.
- Net BUG-003 status after both focused fixes:
  - returns to the original strict mismatch bucket (`TCLetTypeMismatch` with bottomized RHS),
  - i.e. regression from synthesis/mapping is removed, but baseline BUG-003 closure remains open.

## 2026-02-16 baseline BUG-003 semantic gap trace (pre-Φ bottomization path)
- Direct edge-loop trace (`processInstEdge` one-by-one, BUG-003-V1 shape) shows the first irreversible drift at edge `0`:
  - new self-bound vars appear immediately after edge `0`:
    - `TyVar 35 bound=Just 35`
    - `TyVar 37 bound=Just 37`
  - both are already in `cEliminatedVars` at this point.
- Edge-0 expansion/witness evidence at that same point:
  - expansion before rewrite: `ExpInstantiate [30,31,32,33,34]`.
  - extra ops emitted by `runExpansionUnify` include self-merge pairs:
    - `OpRaise 2 ; OpMerge 2 2`
    - `OpRaise 0 ; OpMerge 0 0`
  - this is the signature of `RaiseMerge` emission in `EdgeUnify` (raise+merge pairing), not base Ω `ExpInstantiate` ops.
- Copy-map/meta evidence links source binders to self-bound metas:
  - edge-0 `etCopyMap` includes `(0 -> 35)`, `(2 -> 37)`.
  - after edge-0:
    - source binders `0` and `2` are eliminated,
    - metas `35` and `37` are self-bound and eliminated.
- Causal interpretation:
  - `EdgeUnify` records source-domain self-merge operations (`OpMerge 0 0`, `OpMerge 2 2`),
  - then `setVarBoundM` canonicalizes through UF roots, so those source-key updates land on meta roots (`35`, `37`) as `bound=self`,
  - creating eliminated self-bound vars before any Φ replay occurs.
- Presolution→solve chain confirms bottomization is already baked in pre-Φ:
  - `prConstraint`:
    - edge-0 expansion has already rewritten arg `31 -> 35`: `ExpInstantiate [30,35,32,33,34]`,
    - `cEliminatedVars = {35}`,
    - `35 = TyVar { tnBound = Just 35 }`,
    - scheme-bound arrows already reference `35` (for example nodes `7`, `10`).
  - `solveUnify`:
    - eliminated self-bound `35` is rewritten to bottom (`TyBottom 46`),
    - bound arrows become bottomized (`7: dom 46`, `10: dom/cod 46`).
- Conclusion:
  - edge-0 scheme bounds are bottomized in presolution/solve state transitions (edge execution + eliminated-binder rewrite), before Φ translation starts.

## 2026-02-17 implementation findings (self-merge/self-bound guard)
- RaiseMerge-only guard result:
  - Guarding RaiseMerge emission against same-UF-class endpoints in `EdgeUnify` is sufficient to make `BUG-003-V1/V2` pass again (`∀a. a -> a -> a -> a`), while keeping strict anchors green.
  - However, it is not by itself sufficient to prevent all pre-solve self-bound metas (`BUG-003-PRES` still observed `35 -> 35` after the first pass).
- Remaining self-bound write path:
  - Residual `root(n) == root(m)` bound writes can still occur through edge-local bound-write plumbing (not only explicit RaiseMerge emission).
  - These writes are semantically no-op but leave self-bound artifacts in `prConstraint`.
- Final closure move:
  - Added an edge-local adapter guard in `EdgeUnify.setVarBoundM` to skip same-root writes (`findRoot nid == findRoot bnd`).
  - This removes self-bound artifacts at source while preserving source-domain op payload IDs and existing witness/Φ contracts.
- Net outcome:
  - `BUG-003-PRES` green (no self-bound binder metas left in edge-0 `prConstraint`),
  - `BUG-003-V1/V2` green,
  - selected strict anchors (4) remain green.

## 2026-02-17 regression reopen findings (current workspace)
- `BUG-003` strict-success is red again and deterministic across both historical and current full-gate seeds:
  - `--match "BUG-003-V" --seed 1481579064` -> 2/2 FAIL.
  - `--match "BUG-003-V" --seed 1925916871` -> 2/2 FAIL.
- Failure class has shifted from earlier invariant buckets to direct bottomized elaboration mismatch:
  - expected: `∀a. a -> a -> a -> a`
  - actual: `⊥ -> ⊥ -> ⊥ -> ⊥` (both V1/V2).
- `BUG-003-PRES` still passes, so the earlier self-bound-meta root cause remains fixed:
  - `--match "BUG-003-PRES" --seed 1925916871` -> PASS.
- Additional Phase-6 drift accompanies BUG-003 regression in current workspace:
  - `interleaves StepIntro with Omega ops in Φ translation` currently fails (`"O"` vs expected `"O; ∀(u0 ⩾) N"`).
- Clean worktree A/B evidence:
  - In detached clean `HEAD` (`487db7d`), `interleaves StepIntro ...` passes.
  - Therefore, at least this Φ-step drift is introduced by current workspace-local uncommitted edits, not baseline `HEAD`.
- Scale of suspected drift surface (workspace-local):
  - very large uncommitted deltas in core replay/Φ path:
    - `src/MLF/Elab/Phi/Omega.hs`
    - `src/MLF/Elab/Phi/Translate.hs`
    - `src/MLF/Constraint/Presolution/WitnessNorm.hs`
    - plus coupled edits in `ReifyPlan`, `Rewrite`, `Elaborate`, `EdgeUnify`.
- Coupling evidence from transplant experiment:
  - Copying any one of current `WitnessNorm` / `Translate` / `Omega` into clean `HEAD` fails to compile (missing new fields/constructors such as `etBinderReplayHints`, replay-hint records, and related witness-validation constructors).
  - Interpretation: the regression is in a coupled local API+behavior patch stack, not an isolated one-file change.

## 2026-02-17 closure findings (BUG-003 regression fixed again)
- Companion Φ drift root cause:
  - `computeTargetBinderKeys` in `MLF.Elab.Phi.Translate` changed trace-free behavior and retained binder keys when `mTrace = Nothing`.
  - This suppressed `OpWeaken` in the StepIntro sentinel, producing `"O"` instead of `"O; ∀(u0 ⩾) N"`.
  - Fix: return `IntSet.empty` for trace-free paths; StepIntro sentinel returns to green.
- BUG-003 V1/V2 root cause:
  - In `MLF.Elab.Elaborate` (`AAnnF` branch), variable annotations converted `InstInside (InstBot t)` to `InstApp t`.
  - For BUG-003 this yielded `ETyInst (EVar "c") (InstApp TBottom)`, directly bottomizing the elaborated result.
  - Evidence from probe (`cabal repl mlf2-test`): both checked/unchecked pipelines showed `InstApp TBottom` at the use-site.
  - Fix: remove the variable-only `InstInside(InstBot) -> InstApp` conversion; keep expected-bound adjustment intact.
- Validation after fix:
  - `BUG-003-V1/V2` pass (`∀a. a -> a -> a -> a`),
  - `BUG-003-PRES` remains pass,
  - `BUG-004-V1..V4` remain pass,
  - StepIntro interleave sentinel passes with the Φ keep-key fix.
