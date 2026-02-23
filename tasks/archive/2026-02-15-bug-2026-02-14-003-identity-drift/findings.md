# Findings: BUG-2026-02-14-003 Identity Drift (Resume)

## Baseline from prior session
- Failing anchor: `/Pipeline (Phases 1-5)/BUG-2026-02-06-002 strict target matrix/let-c1-return keeps second binder polymorphic/`.
- Failure mode: `PhiTranslatabilityError ["OpGraft+OpWeaken targets non-binder node", target 8]`.
- Similar class observed on `BUG-002-V4` with `target 11`.
- Raw presolution witness targets were binder-like ids (e.g., `9`, `0`), but canonicalized elaboration path consumed drifted targets (`8`, `11`).

## Active questions
1. Which transform introduces target drift: witness canon, translate remap, or scheme/copy-map lookup?
2. What exact binder identity contract should hold between `etBinderArgs` and `ewWitness` targets?
3. Can one deterministic mapping layer restore identity without weakening strict checks?

## 2026-02-15 Baseline confirmation
- Reconfirmed `let-c1-return` failure with strict Phi rejection at canonical target `NodeId 8`.
- Reconfirmed `BUG-004` suite remains green (`4 examples, 0 failures`).

## 2026-02-15 Root-cause trace (expanded)
- Reproduced strict matrix failures beyond a single anchor:
  - `make-app`, `let-c1-return`, and `let-c1-apply-bool` all fail with `OpGraft+OpWeaken targets non-binder node`.
- Presolution raw vs canonical evidence remains consistent:
  - raw witness target (`NodeId 9` / `0`) is binder-like at emission time.
  - canonicalized witness/trace target (`NodeId 8` / `11`) is consumed by Phi and rejected as non-binder.
- Solved-graph probe for `let-c1-return`:
  - `NodeId 9` canonicalizes to `NodeId 8`.
  - `NodeId 8` is `TyBase Int` with rigid parent in solved constraint, i.e. not a binder.
- Phi-side trace for failing edge:
  - `reifyInst` sees witness+trace present for edge 0.
  - `phi steps ... OpGraft ... target 8` and strict rejection triggers before fallback.
- `gaSolvedToBase` probe confirms solved/base drift exists (e.g. `8 -> 9`), but existing membership logic still cannot reconcile edge ops with active binder identities.

## Failed hypotheses in this session
1. Ignore `mSchemeInfo` from source env and force Phi to derive scheme info from candidates.
   - Result: same non-binder failure.
2. Preserve witness ops through `canonicalizeWitness` (no op-node canonicalization).
   - Result: Translate remap still canonicalized to failing target.
3. Preserve step IDs in `Translate` (`remapNode = id`).
   - Result: failure moved from target `8` to raw target `9` with canonical `8`; still rejected.
4. Prune non-`TyVar` graft/weaken pairs in `WitnessNorm`.
   - Result: no effect on failing path because this pass runs before final solve collapse to non-`TyVar`.
5. Solve-aware skip for non-binder graft/weaken in Phi.
   - Result: removes immediate Phi error but causes new downstream Phase 6/7 regressions (`ValidationFailed alias bounds survived` / `InstElim expects forall`), so not acceptable.

## 2026-02-15 (resume continuation)

- Implemented source-ID-preserving canonicalization boundary:
  - `MLF.Elab.Run.Util.canonicalizeWitness` now preserves `ewSteps`/`ewWitness` op targets.
  - `MLF.Elab.Run.Util.canonicalizeTrace` now preserves `etBinderArgs`/`etCopyMap`.
  - Canonicalizer regressions pass (`witness/trace/expansion canonicalization`).

- Implemented source-domain membership plumbing in Phi:
  - `MLF.Elab.Phi.Translate` now keeps witness steps in source IDs and remaps `SchemeInfo.siSubst` via `remapSchemeInfoByTrace`.
  - `MLF.Elab.Phi.Omega` now resolves binder identity via source candidates (`sourceKeysForNode`) derived from trace/copy aliases.

- Current anchor state after these changes:
  - `BUG-2026-02-06-002 strict target matrix`: still `2 failures`:
    - `let-c1-return`: `ValidationFailed ["alias bounds survived scheme finalization: [\"a\"]"]`
    - `let-c1-apply-bool`: `TCArgumentMismatch Int Bool`
  - `BUG-002-V4`: still failing (`TCArgumentMismatch (TVar "t2") Bool`).
  - Guardrails remain green:
    - `BUG-004`: pass (`4 examples, 0 failures`)
    - `tracks instantiation copy maps for named binders`: pass
    - `witness/trace/expansion canonicalization`: pass

- Concrete artifact evidence for failing let-c1 path:
  - Presolution edge-0 trace includes source binder args:
    - `etBinderArgs = [(NodeId 9, NodeId 17), (NodeId 3, NodeId 18)]`
  - Witness edge-0 steps:
    - `OpGraft 17 9 ; OpWeaken 9`
  - Phi currently emits `InstApp Int` for this edge in debug runs, and downstream behavior remains over-specialized in BUG-002/003 anchors.

- Additional negative probe:
  - Temporarily disabled app-RHS scheme realignment in `MLF.Elab.Elaborate` (`appAligned` forced to `sch0Final`) to test whether let-app fallback collapsing caused the remaining failures.
  - Result: no change on failing anchors; probe reverted.

## 2026-02-16 continuation

- A stable local configuration was found where these anchors all pass together:
  - `BUG-002-V4`
  - `BUG-2026-02-06-002 strict target matrix`
  - `BUG-004`
  - `tracks instantiation copy maps for named binders`
  - `witness/trace/expansion canonicalization`
- Full-suite remains red (`653 examples, 19 failures`) with failures spread across broader Phase 1-6 areas not limited to the narrow BUG-003 anchor set.
- The highest-risk regressions correlate with aggressive let/app fallback reshaping in `MLF.Elab.Elaborate`; preserving strict Phi rules is not sufficient without broader elaboration stabilization.

## 2026-02-16 — New Findings (Systematic Debugging)

### Finding: Missing instantiation binders were caused by structural-only reachability in `instantiationBindersFromGenM`

- Location: `src/MLF/Constraint/Presolution/Base.hs` (`instantiationBindersFromGenM`).
- Symptom chain on `master` pre-patch:
  1. `edge0` in BUG-002-V4 produced `ExpIdentity` + empty `etBinderArgs` + empty witness.
  2. Copy-map anchor failed (`predicate failed on []`).
  3. BUG-002-V4/BUG-004/strict matrix diverged broadly.
- Mechanism:
  - Reachability from wrapper body root used `reachableFromUnderLenient` (structural children only).
  - For wrapper roots (`TyVar` with `tnBound`), structural traversal excludes the bound path; candidate binders become empty.
- Validation:
  - Switching reachability to bounds-aware traversal and bounds-aware order keys restored non-empty `ExpInstantiate`/trace/witness for `edge0` and fixed the copy-map anchor.

### Finding: After binder restoration, remaining failure is a Φ interior-domain mismatch

- New failing shape after the minimal patch:
  - `BUG-002-V4` fails with `OpRaise target outside I(r)`.
  - Error detail includes source op target `NodeId 1` while active interior set is `[5,11,30,31,35]`.
- Interpretation:
  - This is no longer a missing-binder/empty-witness failure.
  - It is the source-vs-canonical interior-membership reconciliation issue in Φ (identity-domain contract), consistent with BUG-2026-02-14-003 scope.

## 2026-02-16 — Surgical Omega closure findings

- Test-first confirmation:
  - Added `ElaborationSpec` regression `OpRaise accepts source-domain interior membership even when etCopyMap aliases the target`.
  - RED baseline reproduced the exact bug:
    - `PhiTranslatabilityError ["OpRaise target outside I(r)", ..., "interiorSet=[30,100]"]`.
- Root-cause confirmation in code:
  - `Omega` was remapping `etInterior` keys through canonical/copy aliases before `OpRaise` membership checks.
  - This can rewrite valid source targets out of the admissible set.
- Final fix shape:
  - `OpRaise` admissibility now checks source-domain membership directly against `etInterior`.
  - Added explicit invariant path for identity-domain mismatch:
    - source target missing from `etInterior` but copy-map alias present in `etInterior` => `PhiInvariantError`.
  - Kept strict outside-`I(r)` rejection for real misses (no alias hit).
  - For semantic execution only, `OpRaise` now adopts source→copied node from `etCopyMap` before canonicalization.
    - This was required to keep BUG-004 (`V2`) behavior correct once source-domain admissibility stopped rejecting the operation.
- Translate-side alignment:
  - `etInterior` is canonicalized only for `namedSet` intersection, preserving trace semantics elsewhere.
- Verification:
  - New regressions pass (`ElaborationSpec` + `PipelineSpec`).
  - Required anchors pass:
    - `BUG-002-V4`
    - `BUG-2026-02-06-002 strict target matrix`
    - `BUG-004`
    - `tracks instantiation copy maps for named binders`
    - `witness/trace/expansion canonicalization`
  - Full gate still fails in separate buckets:
    - `cabal build all && cabal test` => `672 examples, 9 failures`.
    - These are tracked as independent buckets and not folded into BUG-2026-02-14-003.

## 2026-02-16 — BUG-2026-02-16-003 cluster root cause and closure

- Reproduced BUG-2026-02-16-003 with deterministic seed and confirmed the same failure class also hit BUG-2026-02-16-004/005/006 (`TCArgumentMismatch` on `id id`).
- Added trace instrumentation to `AAppF` in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs` and confirmed:
  - `funInst` side was correct (`InstApp (TArrow (TVar "t18") (TVar "t18"))`).
  - `argInstFromFun` was overriding the raw witness-derived argument instantiation.
  - The override path used `inlineBoundVarsType`, which rewrote inferred meta-var `t18` into `TArrow (TVar "t14") (TVar "t14")`, causing over-instantiation and Phase 7 mismatch.
- Minimal fix validated:
  - Removed `inlineBoundVarsType` application from both `argInstFromFun` branches.
  - Kept inference itself unchanged (`inferInstAppArgs` still drives `argInstFromFun`).
- Post-fix targeted verification:
  - BUG-2026-02-16-003 repro: pass.
  - BUG-2026-02-16-004 checked-authoritative replay: pass.
  - BUG-2026-02-16-005 dual-instantiation spec: pass.
  - BUG-2026-02-16-006 paper baseline (`id id should have type`): pass.
  - Existing strict-target anchors (`BUG-002-V2`, `BUG-002-V4`, strict target matrix, OpRaise interior canonicalization gate): all pass.
