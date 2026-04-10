# Verification Contract

Roadmap family: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
Revision: `rev-011`

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
   - Confirm `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` match the active
     `roadmap_id`, `roadmap_revision`, and `roadmap_dir`.
   - Confirm prior roadmap families and revisions remain unchanged.
   - Confirm `round-208`, `round-209`, and `round-210` artifacts remain
     immutable predecessor evidence if the round cites them.
   - For `rev-011` milestone-2 continuation, confirm the same `round-211`
     branch/worktree remains the live baseline when cited and that its current
     diff in `Annotation.hs`, `Legacy.hs`, `Algebra.hs`,
     `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, and
     `test/Research/P5ClearBoundarySpec.hs` is preserved rather than discarded
     on a fresh round.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
4. **Thesis conformance gate**
   - For `rev-011` milestone-2 continuation, run
     `./scripts/thesis-conformance-gate.sh` because the preserved blocker proof
     still includes the A6 `TCExpectedArrow` cluster.
5. **Broader-positive boundary discipline**
   - Confirm the round stays inside the selected milestone/direction scope and
     does not silently widen into cyclic search, multi-SCC behavior,
     equi-recursive reasoning, fallback rescue, or a second interface.
   - Confirm the retained-child clear-boundary lane remains predecessor truth
     rather than being silently upgraded into whole-frontier closure.
   - Confirm `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` remain closed unless the active milestone
     explicitly and honestly reclassifies them.
   - For milestone-2 rounds under `rev-011`, confirm the continuation stays
     limited to the preserved `round-211` baseline plus the newly admitted
     downstream application-consumer / immediate authoritative-companion seam.
6. **Authoritative-entrypoint discipline**
   - When a round claims broader-positive support, confirm the evidence is
     visible on both `runPipelineElab` and `runPipelineElabChecked`.
7. **Worker-plan integrity when fan-out is used**
   - If a round uses planner-authored worker fan-out, confirm
     `worker-plan.json` exists, worker ownership boundaries were respected, and
     approval is based on the integrated round result.

## Task-Specific Checks

Reviewers must also run checks specific to the selected milestone and
direction.

- **milestone-1**
  - Verify the artifact consumes accepted `round-203`, `round-204`, and
    `round-205` honestly and does not rewrite those predecessor artifacts.
  - Verify the artifact names the exact broader positive frontier, the expected
    behavior shift, the authoritative success surfaces, and the representative
    corpus.
  - Verify the artifact freezes the writable slice concretely enough for later
    code-bearing rounds and keeps excluded families and guardrails closed.
  - Verify the round stays docs/control-plane-only.
- **milestone-2**
  - Verify the diff stays inside the milestone-1 writable slice as superseded
    by `rev-011`:
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `src/MLF/Elab/Elaborate/Algebra.hs`,
    `test/ElaborationSpec.hs`,
    `test/PipelineSpec.hs`, and
    `test/Research/P5ClearBoundarySpec.hs`.
  - Verify the continuing round preserves the already-cleared Phase 6
    authoritative translation, the downstream
    `PhiReorder: missing binder identity` progress, the selected packet on both
    authoritative entrypoints, checked-authoritative parity, the classic
    let-polymorphism / explicit-`forall` positives,
    `BUG-2026-02-06-002`, and the retained-child exact packet from the current
    `round-211` baseline.
  - Verify the admitted `rev-011` continuation matches the blocker proof:
    direct `TermClosure.hs` repair and the bounded post-closure `ALetF`
    continuation are already exhausted; the writable repair target is now the
    downstream `ALamF` / `AAppF` consumer recovery surface in `Algebra.hs`,
    plus only the immediate authoritative witness refinement companion in
    `Annotation.hs` / `Legacy.hs` when reviewer evidence proves it necessary.
  - If the round touches `src/MLF/Elab/Elaborate/Algebra.hs`, verify those
    edits stay limited to the exact downstream consumer locals and immediate
    helper reads around
    `collapseTrivialBoundAlias`,
    `singleAppInstArg`,
    `recoverSingleAppArg`,
    `identityLikeMuArgInst`,
    and the precise `ALamF` / `AAppF` branches that now stop at
    `TCExpectedArrow`; they must not relitigate direct closure or the settled
    `ALetF` post-closure handoff as if those seams were still open.
  - If the round touches `src/MLF/Elab/Elaborate/Annotation.hs`, verify those
    edits stay inside `reifyInst` authoritative refinement helpers
    `authoritativeTargetType`,
    `inferAuthoritativeInstArgs`,
    `reifyTraceBinderInstArgs`,
    `instNeedsAuthoritativeRefinement`, and
    `instSeqApps`, and do not reopen unrelated annotation translation logic.
  - If the round touches `src/MLF/Elab/Legacy.hs`, verify those edits stay
    inside `expInstantiateArgsToInstNoFallback` / `instAppsFromTypes` as a
    mechanical companion to the admitted `Annotation.hs` witness refinement,
    not as a broader witness-translation redesign.
  - Verify the authoritative packet is still covered honestly on both
    entrypoints in `test/PipelineSpec.hs` and
    `test/Research/P5ClearBoundarySpec.hs`, and that
    `test/ElaborationSpec.hs` still proves the old Phase 6 and post-annotation
    seams remain cleared.
  - Verify the remaining fail-fast cluster is explicitly rechecked:
    - `dual annotated coercion consumers fail fast on unresolved non-root OpWeaken`
    - `pipeline fails fast for nested-let when only expansion-derived instantiation remains`
    - `full pipeline fails fast post-boundary-enforcement for: nested-let`
    - `BUG-2026-02-17-002`
    - the non-local proxy wrapper `g g`
  - Verify the nested-let probes do not become false success
    `forall a. a -> a`.
  - Verify
    `src/MLF/Elab/TermClosure.hs`,
    `src/MLF/Elab/Run/Pipeline.hs`,
    `src/MLF/Elab/Pipeline.hs`,
    `src-public/MLF/Pipeline.hs`,
    `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
    `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
    remain untouched unless a later accepted roadmap revision explicitly
    authorizes more.
  - Verify `cabal build all && cabal test` passed.
  - Verify `./scripts/thesis-conformance-gate.sh` passed.
  - Verify no fallback rescue, second interface, cyclic widening,
    equi-recursive reinterpretation, or negative-family reclassification is
    smuggled in.
- **milestone-3**
  - Verify the representative broader positive frontier named by milestone-1
    now passes honestly on both `runPipelineElab` and `runPipelineElabChecked`.
  - Verify the relevant research/pipeline regression surfaces were updated or
    replayed honestly.
  - Verify public/internal parity is real and does not depend on helper-only
    success or compatibility shims.
  - Verify preserved `P2` and representative negative-family rows remain
    closed.
- **milestone-4**
  - Verify the closeout artifact records the enacted behavior and evidence
    surface honestly.
  - Verify repo-facing notes and thesis-deviation records were updated when the
    accepted evidence requires them.
  - Verify `cabal build all && cabal test` passed for any code/test-bearing
    round and `./scripts/thesis-conformance-gate.sh` passed when required by
    the active milestone.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected milestone passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected milestone.
5. Any preserved-history or pointer-refresh requirement has been satisfied.
6. Any `pending-merge` refresh or re-review requirement has been satisfied.
7. The round result stays within the enacted broader-positive boundary the
   active milestone authorizes and does not silently widen semantics,
   interfaces, or architecture.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap",
  "roadmap_revision": "rev-011",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-011",
  "milestone_id": "<milestone-id>",
  "direction_id": "<direction-id>",
  "extracted_item_id": "<extracted-item-id>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
