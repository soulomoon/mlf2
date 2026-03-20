# Round 053 Plan (`H4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `H4` and produce one accepted aggregate decision
artifact at
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`.

This round is the initial `H4` plan with `retry: null`, not a delta retry
plan. It is aggregate-only and docs-only. It must record exactly one bounded
next-step result token over the already reverified `H3` evidence chain for the
accepted repaired `URI-R2-C1` local-binding
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state,
or bug-tracker edits are planned or authorized in `attempt-1`. This round does
not reopen `H1` selection, `H2` implementation, or `H3` verification.

## Locked Round Context

- Round id: `round-053`
- Roadmap item: `H4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: aggregate-only, docs-only next-cycle decision gate

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `H4` and must not appear in the plan,
artifact, reviewer handoff, or final accepted review record.

Accepted predecessor facts that remain binding throughout `H4`:

- `H1` finalized in `round-050` as the authoritative bind that froze exactly
  one remaining local-binding `instArgRootMultiBase`
  `keepTargetFinal` / `targetC` slice under repaired `URI-R2-C1`, while
  leaving `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
  `boundVarTarget` as inherited context only.
- `H2` finalized in `round-051` as the authoritative implementation of only
  that bounded lane, with the reviewer-auditable proof
  `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`,
  with `targetC -> rootFinal` admitted only for that local lane, with one
  local success example plus one matched non-local fail-closed contrast, and
  with `baseTarget` rejection preserved outside the selected lane.
- `H3` finalized in `round-052` as authoritative `attempt-1`, with
  `attempt_verdict = accepted`, `stage_action = finalize`, and canonical
  artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`.
- The accepted `H3` artifact already reverified the exact bounded `H2` lane
  via read-only `Fallback.hs` / `PipelineSpec.hs` anchors, a fresh focused
  `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a
  fresh full repo `cabal build all && cabal test` gate, predecessor
  continuity, the matched non-local fail-closed contrast, and preserved
  `baseTarget` rejection outside the selected lane.
- `orchestrator/rounds/round-052/review-record.json` already records the
  accepted `H3` checks map as all-pass for `H3-CONTRACT`,
  `H3-H2-CONTINUITY`, `H3-FALLBACK-ANCHOR`, `H3-PIPELINE-ANCHOR`,
  `H3-FOCUSED-RERUN`, `H3-FULL-GATE`, `H3-CANONICAL-ARTIFACT`,
  `H3-DIFF-BOUNDARY`, and `H3-CONTINUITY`.
- `U2` remains `authority-narrowed`, `U3` remains
  `uniqueness-owner-stable-refuted`, and `U4` remains
  `constructor-acyclic-termination-refuted`; none of those accepted negative
  findings may be reinterpreted as widening clearance here.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `Open`
  section is empty, and the replay-path `BUG-2026-03-16-001` remains resolved,
  so bug history does not authorize replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `boundVarTarget` widening, non-local widening, or broader
  recursive widening in this round.

Current repository state is already non-pristine. Respect existing work and do
not revert or clean up unrelated changes while preparing the `H4` decision
artifact.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/roles/planner.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-053/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-052/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
   - create the canonical `H4` aggregate decision record.

Read-only decision anchors:

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
2. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
3. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
4. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-052/review-record.json`
5. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `H4` `attempt-1`:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/src/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/src-public/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/app/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/test/`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/mlf2.cabal`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/roadmap.md`
- `/Volumes/src/mlf4/Bugs.md`
- prior artifacts and review history under
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-001/`
  through
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-052/`
- any docs outside the canonical `H4` artifact path

## Sequential Tasks

### Task 1 - Freeze the `H4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `H4` artifact, state explicitly:
  - `Round: round-053`
  - `Roadmap item: H4`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility, convenience, or default-path widening.
- State that `H4` is aggregate-only and docs-only.
- State that `H4` records exactly one bounded next-step result token only.
- State that `H4` does not rerun `H1` / `H2` / `H3`, does not amend the
  roadmap, and does not itself widen the live subject or inherited boundary.
- State that any contradiction discovered in the accepted `H3` evidence chain
  is a blocker to record through `stop-blocked`, not permission to patch code,
  add tests, reopen replay, or update `Bugs.md` during this attempt.
- State explicitly that reviewer outcomes for this stage are limited to
  `accepted + finalize` or `rejected + retry`, and that `accepted + retry` is
  forbidden for `H4`.

### Task 2 - Reconstruct the accepted `H1` / `H2` / `H3` evidence chain without reopening prior stages

- Carry forward only accepted authoritative evidence from the `H3` chain:
  - the selected live subject remains repaired `URI-R2-C1`;
  - the exact bounded lane remains the accepted `H2` local-binding
    `rootLocalInstArgMultiBase` / `targetC -> rootFinal` slice;
  - the accepted read-only anchors remain `Fallback.hs` and
    `PipelineSpec.hs`;
  - `baseTarget` rejection remains preserved outside the selected lane;
  - `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and `boundVarTarget`
    remain inherited context only; and
  - non-local widening, replay reopen, `MLF.Elab.Inst`, and `InstBot` remain
    out of scope.
- Use
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/rounds/round-052/review-record.json`
  as the authoritative proof that `H3` finalized as `stage_id = "H3"`,
  `attempt = 1`, `attempt_verdict = "accepted"`, `stage_action = "finalize"`,
  `status = "authoritative"`, with artifact path
  `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`.
- Carry forward the accepted `H3` checks map as the bounded decision baseline:
  `H3-CONTRACT`, `H3-H2-CONTINUITY`, `H3-FALLBACK-ANCHOR`,
  `H3-PIPELINE-ANCHOR`, `H3-FOCUSED-RERUN`, `H3-FULL-GATE`,
  `H3-CANONICAL-ARTIFACT`, `H3-DIFF-BOUNDARY`, and `H3-CONTINUITY` all
  remained `pass`.
- Treat `/Volumes/src/mlf4/Bugs.md` only as continuity context. Do not
  reinterpret resolved replay-path bug history as current-round repair
  authority.
- State explicitly that `H4` answers one bounded question only:
  given the accepted `H3` evidence chain and current docs-level continuity,
  what single lawful next-step result token applies to the repaired
  `URI-R2-C1` local-binding
  `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, the required
     `round-052` review record is non-authoritative or contradictory,
     docs-level continuity checks fail, or the accepted `H3` verification can
     no longer be treated as the current bounded verification baseline for the
     same repaired `URI-R2-C1` local `rootLocalInstArgMultiBase` lane,
     including the matched non-local fail-closed contrast and preserved
     `baseTarget` rejection outside that lane.
   - if this rule matches, record the exact blocker and do not patch around it.

2. `widen-approved`
   - choose this only if already-accepted evidence in the current repository
     proves that widening is now lawful without rewriting accepted history.
   - at minimum that would require an accepted artifact or roadmap amendment
     that explicitly changes the live subject or inherited boundary and clears
     the still-binding `U2` / `U3` / `U4` negative findings as widening
     blockers for this slice.
   - absent such already-accepted evidence, `widen-approved` is not lawful.

3. `continue-bounded`
   - choose this if the accepted `H3` evidence chain remains authoritative, the
     accepted `H3` verification still stands as the current bounded
     verification baseline, no new blocker is found, and no already-accepted
     widening authority exists.
   - this result must preserve repaired `URI-R2-C1`, the accepted
     `Fallback.hs` / `PipelineSpec.hs` ownership anchor, the matched non-local
     fail-closed contrast, preserved `baseTarget` rejection outside the
     selected lane, and the explicit-only / non-equi-recursive /
     non-cyclic-graph / no-second-interface / no-fallback boundary.
   - if selected, the decision rationale must frame any future work as a
     separate bounded cycle or explicit stop decision after a new accepted
     roadmap update, not as authorization to widen this round.

The canonical `H4` artifact must record exactly one of those three tokens and
must explain why the selected token is lawful and why the other two tokens are
not.

### Task 4 - Run the docs-only verification and continuity checks required for `H4`

Run the baseline docs/state checks required by
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/orchestrator/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
- `test -f docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
- `python3 -m json.tool orchestrator/rounds/round-052/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `stage_id == "H3"`
  - `attempt == 1`
  - `attempt_verdict == "accepted"`
  - `stage_action == "finalize"`
  - `status == "authoritative"`
  - `artifact_path == "docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md"`
  - the `checks` map still includes all required `H3-*` pass keys named in
    Task 2.
- `rg -n 'rootLocalInstArgMultiBase|targetC -> rootFinal|baseTarget|rootHasMultiInst|rootLocalSchemeAliasBaseLike|boundVarTarget|non-local|Fallback\\.hs|PipelineSpec\\.hs' docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  to confirm the accepted bounded lane, preserved `baseTarget` rejection, and
  inherited exclusions remain the ones being aggregated.
- a short `python3` or `awk` continuity check over `/Volumes/src/mlf4/Bugs.md`
  proving the `Open` section is still empty and
  `BUG-2026-03-16-001` remains resolved continuity context only.

Reconfirm the round stays docs-only:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun the focused `ARI-C1` block or `cabal build all && cabal test` in
`H4` `attempt-1`. The accepted `H3` artifact already supplies the fresh bounded
verification baseline for this exact lane, and `H4` authorizes no production or
test edits. If an unexpected non-doc/code-path diff appears, stop and record
the blocker rather than widening the round.

### Task 5 - Author the canonical `H4` aggregate decision artifact

Write
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-053/docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
as the canonical stage result.

The artifact must include:

- stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
  `Retry state`, `Live subject`, `Artifact kind`);
- a short contract section explaining that `H4` is aggregate-only and
  docs-only;
- a carry-forward section summarizing the authoritative `H1` / `H2` / `H3`
  chain without reopening it;
- a decision section that records exactly one token:
  `continue-bounded`, `widen-approved`, or `stop-blocked`;
- a rationale section proving why the chosen token is lawful under the closed
  decision rules and why the other two tokens are not;
- a continuity section preserving the accepted `H3`-bounded lane,
  `Fallback.hs` / `PipelineSpec.hs` ownership, the matched non-local
  fail-closed contrast, preserved `baseTarget` rejection outside the selected
  lane, and the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph boundary; and
- a non-authorization section stating that this round does not itself reopen
  `H1` / `H2` / `H3`, replay, `MLF.Elab.Inst` / `InstBot`,
  `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
  non-local widening, or any broader recursive-inference work.

If the closed-rule decision resolves to `continue-bounded`, the artifact should
state that any future bounded cycle would require a new accepted roadmap update
and may not treat the accepted `H3` local
`rootLocalInstArgMultiBase` verification as already clearing replay reopen,
`MLF.Elab.Inst` / `InstBot`, `boundVarTarget`, non-local widening, or any
other trigger-family widening. If the closed-rule decision resolves to
`stop-blocked` or `widen-approved`, the artifact must record the exact accepted
basis for that result and keep it bounded to the already-reverified local lane.

## Non-Goals

- No change to the accepted `H1` bind itself.
- No change to the accepted `H2` implementation or `H3` verification.
- No selection or implementation of a new bounded slice in this round.
- No edits to `src/MLF/Elab/Run/ResultType/Fallback.hs`.
- No edits to `test/PipelineSpec.hs`.
- No edits to `src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API
  changes, executable changes, roadmap mutation, `Bugs.md` edits, or
  `mlf2.cabal` changes.
- No widening into `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`,
  `boundVarTarget`, non-local widening, equi-recursive reasoning, implicit
  unfolding, cyclic structural graph encoding, multi-SCC support, cross-family
  search, heuristic owner selection, compatibility shims, or convenience
  fallbacks.

## Reviewer Checks For This Round

1. `plan.md` keeps `H4` bounded to the accepted `H3`
   `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane and preserves
   `baseTarget` rejection outside the selected lane.
2. The canonical `H4` artifact records exactly one result token and explains
   why the selected token is lawful under the accepted `H3` evidence chain.
3. `widen-approved` is rejected unless an already-accepted roadmap or boundary
   amendment exists; `stop-blocked` is rejected unless a real contradiction or
   missing authority is found; `continue-bounded` is rejected unless the
   accepted `H3` chain still stands as authoritative.
4. The round remains docs-only and bounded to the accepted `H3` lane, with no
   reopening of `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`,
   `boundVarTarget`, replay, non-local widening, or any other excluded family.
5. The docs/state continuity checks pass, including authoritative
   `round-052` review-record continuity and bug-tracker continuity context.
6. Reviewer outcome space excludes `accepted + retry` for `H4`.
