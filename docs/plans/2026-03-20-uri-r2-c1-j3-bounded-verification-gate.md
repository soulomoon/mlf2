# `J3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-060`
Roadmap item: `J3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bounded verification and evidence consolidation gate

## Stage Contract Freeze

This artifact executes only roadmap item `J3` for `attempt-1`.

`J3` re-verifies exactly one already-accepted `J2` lane and nothing broader:
the local-binding inst-arg-only singleton-base
`rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
path in `src/MLF/Elab/Run/ResultType/Fallback.hs`, together with its focused
`ARI-C1 feasibility characterization (bounded prototype-only)` evidence in
`test/PipelineSpec.hs`.

This round is docs-only. It does not reopen implementation and it does not
preempt `J4`. It does not authorize:

- any production, test, public API, executable, or Cabal edit;
- any edit to `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/rounds/round-060/selection.md`, or
  `orchestrator/rounds/round-060/plan.md`;
- any reopening of accepted `I1` / `I2` / `I3` / `I4`, `J1`, or `J2`;
- any reopening of `rootLocalSingleBase`, the preserved scheme-alias/base-like
  `baseTarget` route, `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
  `keepTargetFinal`, or `boundVarTarget` as new live targets;
- any replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundTarget`,
  `schemeBodyTarget`, or `ResultType.View` work; or
- any widening beyond repaired `URI-R2-C1`, including non-local widening,
  multi-SCC widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, second-interface
  work, or compatibility / convenience fallback paths.

Accepted `U2 = authority-narrowed`, `U3 = uniqueness-owner-stable-refuted`,
and `U4 = constructor-acyclic-termination-refuted` remain binding negative
findings, not widening clearance.

## Accepted `I4` / `J1` / `J2` Continuity

Authoritative review-record recheck confirmed the governing predecessor chain:

- `round-057` / `I4` -> `attempt = 2`, `attempt_verdict = accepted`,
  `stage_action = finalize`, `status = authoritative`,
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
- `round-058` / `J1` -> `attempt = 1`, `attempt_verdict = accepted`,
  `stage_action = finalize`, `status = authoritative`,
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
- `round-059` / `J2` -> `attempt = 1`, `attempt_verdict = accepted`,
  `stage_action = finalize`, `status = authoritative`,
  `artifact_path = docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`

Accepted carry-forward facts re-read for this gate:

1. `I4` finalized `continue-bounded`, so successor work must remain one more
   bounded non-widening step inside repaired `URI-R2-C1`.
2. `J1` froze exactly one future `J2` slice: the local-binding inst-arg-only
   singleton-base `baseTarget -> baseC` lane plus its same-lane `targetC` use
   in `Fallback.hs`, with future ownership limited to `Fallback.hs` and
   `PipelineSpec.hs`.
3. `J2` landed exactly that slice, introducing
   `rootLocalInstArgSingleBase = rootBindingIsLocalType && IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1 && not rootHasMultiInst && not instArgRootMultiBase`
   and the dedicated same-lane `targetC` arm, then passed both the focused
   rerun and the full repo gate.
4. The completed `rootLocalSingleBase` lane, the preserved scheme-alias /
   base-like `baseTarget` route, `rootLocalMultiInst`,
   `rootLocalInstArgMultiBase`, retained-target / `keepTargetFinal`,
   replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
   `boundTarget`, `schemeBodyTarget`, `ResultType.View`, and all non-local
   widening remain inherited context only and are not reopened as `J3`
   targets.

Inherited boundary continuity was also rechecked through the approved
continue-bounded cycle docs and predecessor decision docs:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  still fixes the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph baseline.
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  still preserves fail-closed non-handoff discipline.
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  still records repaired `URI-R2-C1` as the controlling repaired lane.
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  still records `continue-bounded`, not widening approval.
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  and
  `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  still describe one bounded non-widening cycle at a time.

`/Volumes/src/mlf4/Bugs.md` was re-read as continuity context only. Its
`## Open` section is empty and it currently lists only resolved bugs, so it
does not authorize a detour or widening in `J3`.

## Read-Only Anchor Evidence

### `src/MLF/Elab/Run/ResultType/Fallback.hs`

Read-only anchor inspection confirms the accepted `J2` lane is still present
and still bounded:

- `Fallback.hs:382-387` keeps the selected inst-arg-only singleton-base
  `baseTarget -> baseC` branch confined to the exact local conditions
  `IntSet.null rootBaseBounds`, `not instArgRootMultiBase`,
  `not rootHasMultiInst`, `IntSet.size instArgBaseBounds == 1`, and
  membership of `baseC` in `instArgBaseBounds`.
- `Fallback.hs:531-536` still defines the selected local proof
  `rootLocalInstArgSingleBase` as
  `rootBindingIsLocalType && IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1 && not rootHasMultiInst && not instArgRootMultiBase`.
- `Fallback.hs:541-545` still keeps the adjacent completed
  `rootLocalSingleBase` proof as inherited context only.
- `Fallback.hs:685-691` still keeps `keepTargetFinal` confined to the retained
  local families
  `rootLocalMultiInst || rootLocalInstArgMultiBase || rootLocalSchemeAliasBaseLike || maybe False (const True) boundVarTarget`,
  proving the selected `J3` lane did not widen into retained-target logic.
- `Fallback.hs:692-700` still orders `targetC` with:
  `rootLocalSingleBase` first at `694-695`,
  the selected `rootLocalInstArgSingleBase` arm at `696-697`, and the
  preserved scheme-alias / base-like `baseTarget` route at `698-700`.

Together these anchors show the live source still matches the accepted `J2`
lane only, without reopening replay, retained-child, non-local, or broader
trigger-family logic.

### `test/PipelineSpec.hs`

Read-only anchor inspection confirms the focused bounded evidence remains
aligned to that same lane only:

- `PipelineSpec.hs:1348-1398` still defines the dedicated
  `localInstArgSingleBaseFallback` helper for the selected local
  inst-arg-only singleton-base lane.
- `PipelineSpec.hs:1399-1425` keeps the adjacent completed
  `localSingleBaseFallback` helper as inherited context only.
- `PipelineSpec.hs:1629-1631` still carries the accepted positive local
  example
  `keeps local inst-arg-only singleton-base fallback on the local TypeRef lane`.
- `PipelineSpec.hs:1633-1641` still carries the matched non-local fail-closed
  contrast
  `keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane`.
- `PipelineSpec.hs:1660-1712` still carries the source-guard assertions naming
  `rootLocalSingleBase`, `rootLocalInstArgSingleBase`, the dedicated
  `targetC` arm, preserved `keepTargetFinal`, and the inherited retained
  families.
- `PipelineSpec.hs:1714-1724` still begins the inherited non-local pipeline
  entrypoint fail-closed check.

These test anchors still prove the live evidence packet is bound to the exact
accepted `J2` local lane and still preserves fail-closed behavior when the
same wrapper leaves the local `TypeRef` lane.

## Verification Runs And Outcomes

Commands were executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-060`

### Baseline Checks

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `16:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - `160:25. [done] ... J1 ...`
  - `164:26. [done] ... J2 ...`
  - `168:27. [pending] ... J3 ...`
  - `172:28. [pending] ... J4 ...`

### Required File-Presence And Boundary Continuity Checks

- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  -> pass
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass
- `test -f /Volumes/src/mlf4/Bugs.md` -> pass

### Review-Record Continuity Checks

- `python3 -m json.tool orchestrator/rounds/round-057/review-record.json >/dev/null`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-058/review-record.json >/dev/null`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-059/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over the three review records -> pass:
  - `PASS round-057 stage_id=I4 attempt=2 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  - `PASS round-058 stage_id=J1 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  - `PASS round-059 stage_id=J2 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`

### Focused Rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - targeted block `ARI-C1 feasibility characterization (bounded prototype-only)`
    reran successfully;
  - `19 examples, 0 failures`;
  - both the selected positive local example and the matched non-local
    fail-closed contrast passed in the fresh rerun.

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - build completed under GHC `9.12.2`;
  - `Finished in 1.9704 seconds`;
  - `1140 examples, 0 failures`;
  - `Test suite mlf2-test: PASS`.

## Diff Discipline

Pre-write diff check results, before authoring this canonical `J3` artifact:

- `git status --short --untracked-files=all` -> pass for bounded pre-existing
  controller / guider state only:
  - `M orchestrator/state.json`
  - `?? orchestrator/rounds/round-060/plan.md`
  - `?? orchestrator/rounds/round-060/selection.md`
- `git diff --name-only` -> `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass (no output)

Post-write docs-only diff-discipline checks for this artifact are recorded
below after the canonical file itself existed:

- `git status --short --untracked-files=all` -> pass for docs-only implement-stage
  output plus the same pre-existing controller / planner prep:
  - `M orchestrator/state.json` (pre-existing controller state; not edited here)
  - `?? orchestrator/rounds/round-060/selection.md` (pre-existing guider-owned
    round artifact)
  - `?? orchestrator/rounds/round-060/plan.md` (pre-existing planner-owned
    round artifact)
  - `?? docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
    (the sole implement-stage authored artifact for this round)
- `git diff --name-only` -> `orchestrator/state.json`
  - confirms there is still no tracked code/test/public-API/executable/Cabal
    diff from this round; the new `J3` artifact is untracked docs-only output
    and is therefore surfaced via `git status`, not `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass (no output)
  - confirms no non-doc / non-orchestrator tracked diff exists in the
    worktree.
- final freshness rerun after the last artifact edit:
  - `git diff --check` -> pass (no output)
  - `git status --short --untracked-files=all` stayed identical to the
    post-write snapshot above
  - `git diff --name-only` stayed `orchestrator/state.json`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
    stayed empty

## Result

The exact accepted `J2`
`rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
lane remains stable under the current read-only anchors, the fresh focused
`ARI-C1` rerun, the fresh full repo gate, predecessor continuity rechecks,
and inherited boundary continuity.

No blocker was found during the required verification sequence. `J3` therefore
hands reviewer-visible bounded evidence forward only; it does not itself make
the `J4` decision and it does not authorize any widening or repair work.
