# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-014`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap lineage, pointer, and preserved-history consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle.
   - Confirm `selection.md` records matching
     `roadmap_id`, `roadmap_revision`, `roadmap_dir`,
     `milestone_id`, `direction_id`, and `extracted_item_id`.
   - Confirm final `review-record.json` records the same lineage fields.
   - Confirm `roadmap_item_id` is absent unless a compatibility mirror is
     explicitly required for a legacy reader.
   - Confirm the parent-workspace pointer stubs
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     match the active `roadmap_id`, `roadmap_revision`, and `roadmap_dir`.
   - Confirm the canonical round-worktree pointer stubs
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     also match the active `roadmap_id`, `roadmap_revision`, and
     `roadmap_dir`.
   - Confirm prior roadmap families and revisions remain unchanged.
   - Confirm `round-208`, `round-209`, and `round-210` artifacts remain
     immutable predecessor evidence if the round cites them.
   - Confirm the same `round-211` branch/worktree remains the live baseline
     and that its current diff in
     `Annotation.hs`, `Legacy.hs`, `Algebra.hs`,
     `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, and
     `test/Research/P5ClearBoundarySpec.hs`
     is preserved rather than discarded on a fresh round.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
4. **Thesis conformance gate**
   - Run `./scripts/thesis-conformance-gate.sh`.
5. **Broader-positive boundary discipline**
   - Confirm the round stays inside the selected milestone/direction scope and
     does not silently widen into cyclic search, multi-SCC behavior,
     equi-recursive reasoning, fallback rescue, or a second interface.
   - Confirm the retained-child clear-boundary lane remains predecessor truth
     rather than being silently upgraded into whole-frontier closure.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed unless the active milestone
     explicitly and honestly reclassifies them.
   - For milestone-2 rounds under `rev-014`, confirm the continuation stays
     limited to the preserved `round-211` baseline plus the same admitted
     broader authoritative application / let-polymorphism handoff and its
     helper-local scaffold in `Algebra.hs`.
6. **Authoritative-entrypoint discipline**
   - When a round claims broader-positive support, confirm the evidence is
     visible on both `runPipelineElab` and `runPipelineElabChecked`.

## Milestone-2 Task-Specific Checks

- Verify the diff stays inside the milestone-1 writable slice as superseded by
  `rev-014`:
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`.
- Verify the continuing round preserves the protected wins from the current
  `round-211` baseline:
  the selected packet on both authoritative entrypoints,
  checked-authoritative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`,
  `g g`,
  the now-green A6 and nested-let fail-fast rows,
  representative let-polymorphism / parity rows,
  the thesis gate, and
  the full repo gate.
- Verify the admitted `rev-014` continuation matches the blocker proof:
  the writable repair target is still the broader authoritative application /
  let-polymorphism handoff in `Algebra.hs`, and `rev-014` now admits the
  current nested helper-local scaffold under
  `funInstRecovered`,
  `argInstFromFun`,
  `fApp`,
  `scheme`,
  `rhsAbs0`, and
  `rhsAbs`
  instead of requiring another flatten-only cleanup.
- If the round touches `src/MLF/Elab/Elaborate/Algebra.hs`, verify those edits
  stay limited to the existing `AAppF` instantiation-recovery locals and
  `ALetF` scheme/handoff locals around
  `funInstByFunType`,
  `funInst'`,
  `normalizeFunInst`,
  `funInstNorm`,
  `funInstRecovered`,
  `fAppForArgInference`,
  `argInstFromFun`,
  `argInst'`,
  `argInstFinal`,
  `fApp`,
  `aApp`,
  `schemeBase`,
  `scheme`,
  `subst0`,
  `subst`,
  `schemeInfo`,
  `env'`,
  `rhsAbs0`,
  `rhsAbs`,
  `bodyElab`, and
  `rhsFinal`,
  and do not grow beyond the admitted helper-local scaffold:
  `containsMuType`,
  `containsMuBound`,
  `isIdentityLikeSchemeType`,
  `shouldInlineParamTy`,
  `shouldInferArgInst`,
  `isInternalTyVar`,
  `isIdentityLambdaBody`,
  `muAnnotationTy`, and
  the local `schemeTy` / strip-candidate staging used by
  `rhsAbs0` / `rhsAbs`.
- Verify `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`, and the round-owned test diffs remain the
  preserved `round-211` carry-forward baseline unless a later accepted
  roadmap revision explicitly reopens them.
- Verify the green gate cluster is explicitly rechecked:
  `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken`,
  `pipeline fails fast for nested-let when only expansion-derived instantiation remains`,
  `full pipeline fails fast post-boundary-enforcement for: nested-let`,
  `runtime snapshot rebuild stays stable across representative corpus`,
  `redirected let-use sites keep polymorphic schemes`,
  `elaborates dual instantiation in application`,
  `let id = (\x. x) in id id should have type ∀a. a -> a`,
  Phi/alignment let-polymorphism rows,
  thesis-alignment let-polymorphism rows,
  frozen parity baseline,
  `BUG-2026-02-06-001`,
  `BUG-002-V2`,
  `BUG-004-V1`,
  `BUG-004-V4`,
  `BUG-2026-02-17-002`, and
  `g g`.
- Verify direct let-polymorphism success does not regress into
  `arrow vs forall`, `arrow vs arrow-of-arrow`, or another equally
  non-landable mismatch.
- Verify the nested-let probes do not become false success `forall a. a -> a`.
- Verify
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  remain untouched unless a later accepted roadmap revision explicitly
  authorizes more.
- Verify both parent-workspace and canonical round-worktree live pointer stubs
  still match the active `rev-014` bundle.
- Verify `cabal build all && cabal test` passed.
- Verify `./scripts/thesis-conformance-gate.sh` passed.

## Approval Criteria

Approval requires all applicable baseline and milestone-2 checks to pass, the
review evidence to match the observed diff and command output, and the result
to stay inside the enacted `rev-014` boundary without silently widening
semantics, interfaces, or architecture. Review must not fail solely because
the admitted helper-local scaffold remains present; fail only when that
scaffold grows beyond the admitted set or the behavioral evidence regresses.
