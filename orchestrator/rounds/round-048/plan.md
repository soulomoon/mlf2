# Round 048 Plan (`G3` Bounded Verification Gate)

## Objective

Execute only roadmap item `G3` and produce one accepted docs/evidence artifact
at:
`docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`.

This is the initial `G3` plan for `attempt-1` with `retry: null`. The round
must stay docs-only and reverify only the accepted `G2` local-binding
`rootLocalMultiInst` / `targetC -> rootFinal` lane under repaired
`URI-R2-C1`.

`G3` must record current bounded evidence from exactly four surfaces:

1. read-only anchor checks in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`,
   proving the accepted local `rootLocalMultiInst` gate and the matched
   fail-closed non-local contrast are still the live bounded lane;
2. a fresh rerun of the focused
   `ARI-C1 feasibility characterization (bounded prototype-only)` block in
   `test/PipelineSpec.hs`;
3. a fresh full repo gate via `cabal build all && cabal test`;
4. predecessor continuity checks anchored to the accepted `G1` / `G2` chain and
   the inherited boundary documents.

`attempt-1` remains docs-only. No production, test, public API, executable,
Cabal, roadmap, bug-tracker, or controller-state edits are planned or
authorized in this round. If any verification step fails, capture the blocker
in the canonical `G3` artifact and optional round notes; do not patch
`Fallback.hs`, `PipelineSpec.hs`, or any other code/test file inside this
verification gate.

## Locked Round Context

- Round id: `round-048`
- Roadmap item: `G3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Active worktree:
  `.worktrees/round-048`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no second interface / no fallback widening`
- Stage mode: docs-only bounded verification/evidence consolidation only

Accepted carry-forward facts that must remain binding throughout `G3`:

- `F3` remains the verification model for this cycle: read-only anchor checks,
  a fresh focused bounded-block rerun, a fresh full repo gate, predecessor
  continuity rechecks, and docs-only diff discipline.
- `F4` authoritative `attempt-1` finalized `continue-bounded`, so the next
  lawful action after the accepted `F3` record was one more bounded
  non-widening cycle rather than replay reopen or broader widening.
- `G1` authoritative `round-046` `attempt-2` froze exactly one future `G2`
  target: the local-binding `rootHasMultiInst` `keepTargetFinal` / `targetC`
  lane in `Fallback.hs`, with future ownership limited to `Fallback.hs` and
  `PipelineSpec.hs`, while leaving `instArgRootMultiBase` explicitly
  unselected.
- `G2` authoritative `round-047` `attempt-1` landed only that bounded slice,
  introduced `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst`,
  routed `keepTargetFinal` and `targetC -> rootFinal` through that one local
  multi-inst lane, added exactly one local `TypeRef` success example plus one
  matched non-local fail-closed contrast in the focused `ARI-C1` block, and
  passed the accepted full gate.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. It does not
  authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or any widening work in
  this round.

Current round control files already exist under
`orchestrator/rounds/round-048/`.
Respect those existing edits. Do not revert or "clean up" unrelated work while
collecting `G3` evidence.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-048/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
- `orchestrator/rounds/round-046/review-record.json`
- `orchestrator/rounds/round-047/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable verification artifact:

1. `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
   - canonical `G3` verification/evidence record.

Optional bounded note file:

1. `orchestrator/rounds/round-048/implementation-notes.md`
   - optional command transcript / blocker capture file if long outputs need a
     bounded home.

Read-only evidence anchors:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
4. `orchestrator/rounds/round-046/review-record.json`
5. `orchestrator/rounds/round-047/review-record.json`
6. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `G3` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-048/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/roadmap.md`
- `/Volumes/src/mlf4/Bugs.md`
- `orchestrator/rounds/round-048/selection.md`
- `orchestrator/rounds/round-048/plan.md`
- reviewer-owned history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-047/`

No edit to `Fallback.hs` or `PipelineSpec.hs` is authorized during `G3`
`attempt-1`. If the accepted `G2` slice fails reverification, capture the
blocker with evidence and stop; do not repair the slice inside this docs-only
gate.

## Sequential Tasks

### Task 1 - Freeze the `G3` `attempt-1` contract as a docs-only verification gate

- In the canonical `G3` artifact, state explicitly:
  - `Round: round-048`
  - `Roadmap item: G3`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility / convenience / default-path fallback or widening.
- State that `G3` verifies only the accepted `G2` local-binding
  `rootLocalMultiInst` / `targetC -> rootFinal` lane.
- State that `G3` does not reopen `G1` selection, does not reopen `G2`
  implementation, does not preempt `G4`, and does not authorize production or
  test edits during this attempt.
- State that any verification failure is a blocker to record, not permission to
  patch `Fallback.hs`, `PipelineSpec.hs`, or any other production/test file
  during this attempt.

### Task 2 - Reconstruct the accepted `G1` / `G2` evidence chain without widening it

- Use
  `orchestrator/rounds/round-047/review-record.json`
  as the authoritative acceptance proof that `G2` finalized as
  `attempt=1`, `attempt_verdict=accepted`, `stage_action=finalize`,
  `status=authoritative`, with the canonical artifact path
  `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`.
- Use
  `orchestrator/rounds/round-046/review-record.json`
  as the authoritative proof that `G1` froze exactly the one local-binding
  `rootHasMultiInst` lane and left `instArgRootMultiBase` unselected.
- Carry forward the exact bounded `G2` slice recorded in
  `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`:
  - `rootLocalMultiInst` is the reviewer-auditable proof for the selected
    local-binding lane;
  - `keepTargetFinal` retains the final target only on that local lane;
  - `targetC` selects `rootFinal` only for that local lane;
  - the focused `ARI-C1` block contains exactly one local positive example and
    one matched non-local fail-closed contrast for the selected lane.
- State explicitly that the accepted `G2` exclusions remain binding in `G3`:
  - `instArgRootMultiBase` remains unselected and out of scope;
  - `boundVarTarget` widening remains out of scope;
  - non-local widening remains out of scope;
  - no replay reopen;
  - no `MLF.Elab.Inst`;
  - no `InstBot`;
  - no equi-recursive reasoning;
  - no cyclic structural encoding;
  - no second executable interface; and
  - no compatibility / convenience / fallback-path widening.
- Treat `/Volumes/src/mlf4/Bugs.md` only as continuity context. Do not
  reinterpret replay-path bug history as current repair authority.
- State explicitly that `G3` answers only one bounded question:
  whether the accepted `G2` local `rootLocalMultiInst` /
  `targetC -> rootFinal` lane still looks stable under read-only anchor checks,
  the focused `ARI-C1` rerun, the fresh full repo gate, and predecessor
  continuity rechecks.

### Task 3 - Collect read-only evidence from the bounded code/test anchors

- Inspect the existing selected-lane logic in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `rootLocalMultiInst`;
  - `keepTargetFinal`, including the required `rootBindingIsLocalType` gate and
    the unchanged out-of-scope trigger families
    `instArgRootMultiBase`, `rootLocalSchemeAliasBaseLike`, and
    `boundVarTarget`;
  - the final `targetC` selection branch that chooses `rootFinal` only when
    `rootLocalMultiInst` or `rootLocalSchemeAliasBaseLike` holds and otherwise
    falls back to the inherited fail-closed branches;
  - the fact that `boundVarTarget` machinery still exists as inherited context,
    but the selected `G2` proof and examples do not widen through it.
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing the accepted bounded block still
  includes:
  - the retained-child same-lane baseline examples inherited from `E2` / `E3`;
  - the local scheme-alias/base-like local-`TypeRef` success example and its
    matched fail-closed contrast from `F2` / `F3`;
  - the local multi-inst local-`TypeRef` success example;
  - the matched non-local multi-inst fail-closed contrast;
  - the local-binding gate source guard that names `rootLocalMultiInst`;
  - the inherited bounded controls that stay fail-closed outside the selected
    local lane.
- Reconfirm from
  `orchestrator/rounds/round-046/review-record.json`
  and
  `orchestrator/rounds/round-047/review-record.json`
  that `G1` and `G2` finalized as authoritative accepted attempts with the
  expected artifact paths and no retry still pending.

Recommended evidence commands for this step:

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '520,695p'`
- `nl -ba test/PipelineSpec.hs | sed -n '1365,1468p'`
- `python3 -m json.tool orchestrator/rounds/round-046/review-record.json`
- `python3 -m json.tool orchestrator/rounds/round-047/review-record.json`

### Task 4 - Re-run the bounded verification suite required for `G3`

- Run the baseline checks required by
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-048/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-048/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-015/retry-subloop.md`
- Run the focused bounded test block exactly:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Run the mandatory fresh full repo gate exactly:
  - `cabal build all && cabal test`

Although `G3` is docs-only, the fresh full repo gate is still mandatory for
this stage because the roadmap and selection require current verification
evidence for the already-accepted `G2` slice, not merely a restatement of
`round-047`.

### Task 5 - Re-run predecessor continuity checks and docs-only diff checks

- Reconfirm that completed predecessor rounds `round-001` through `round-047`,
  the inherited automatic-recursive boundary docs, the replay-repair evidence,
  the completed initial successor cycle, and the recursive-types packet remain
  continuity context only and were not rewritten.
- Reconfirm that `Fallback.hs` and `PipelineSpec.hs` served only as read-only
  anchors for this round and did not receive `G3` code/test edits.
- Reconfirm the accepted `G1` / `G2` bounded lane remains the exact same local
  `rootLocalMultiInst` / `targetC -> rootFinal` lane and that
  `instArgRootMultiBase`, `boundVarTarget` widening, and non-local widening
  remain outside the selected scope.
- Reconfirm the round diff stays docs-only, limited to the canonical `G3`
  artifact and optional round notes plus reviewer/controller-owned outputs.
- If any continuity or diff-boundary check fails, record the blocker in the
  canonical `G3` artifact and stop; do not widen scope or patch code.

## Non-Goals

- No change to the accepted `G1` bind itself.
- No change to the accepted `G2` implementation slice except as inherited
  evidence under reverification.
- No selection or implementation of `instArgRootMultiBase` in this cycle.
- No `boundVarTarget` widening.
- No non-local binding widening.
- No edits to `src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API
  changes, executable changes, roadmap mutation, `Bugs.md` edits, or
  `mlf2.cabal` changes.
- No widening into equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, heuristic
  owner selection, compatibility shims, or convenience fallbacks.

## Reviewer Checks For This Round

1. `plan.md` keeps `G3` bounded to the accepted `G2` local
   `rootLocalMultiInst` / `targetC -> rootFinal` lane and names
   `attempt-1` with `retry: null`.
2. The canonical `G3` artifact captures read-only anchor evidence from
   `Fallback.hs` and `PipelineSpec.hs` showing the selected local lane, the
   matched non-local fail-closed contrast, and the unchanged out-of-scope
   trigger families.
3. The focused
   `ARI-C1 feasibility characterization (bounded prototype-only)` rerun passes
   and still covers the retained-child baseline, the scheme-alias/base-like
   lane, the local multi-inst success example, the matched non-local contrast,
   and the local-binding gate source guard.
4. The fresh `cabal build all && cabal test` gate passes, and predecessor
   continuity across the accepted `G1` / `G2` chain and inherited boundary
   docs remains intact.
5. The round stays docs-only, leaves `Fallback.hs` / `PipelineSpec.hs`
   read-only, and preserves the exclusions on `instArgRootMultiBase`,
   `boundVarTarget` widening, non-local widening, replay reopen,
   `MLF.Elab.Inst` / `InstBot`, equi-recursive reasoning, cyclic structural
   encoding, a second interface, and fallback/convenience widening.
