# Round 049 Plan (`G4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `G4` and produce one accepted aggregate decision
artifact at
`docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`.

This round is the initial `G4` plan with `retry: null`, not a delta retry plan.
It is aggregate-only and docs-only. It must record exactly one bounded next-step
result token over the already reverified `G3` evidence chain for the accepted
repaired `URI-R2-C1` local-binding `rootLocalMultiInst` /
`targetC -> rootFinal` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state,
or bug-tracker edits are planned or authorized in `attempt-1`. This round does
not reopen `G1` selection, `G2` implementation, or `G3` verification.

## Locked Round Context

- Round id: `round-049`
- Roadmap item: `G4`
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

`accepted + retry` is forbidden for `G4` and must not appear in the plan,
artifact, reviewer handoff, or final accepted review record.

Accepted predecessor facts that remain binding throughout `G4`:

- `G1` finalized in `round-046` as the authoritative bind that froze exactly
  one remaining local-binding `rootHasMultiInst` `keepTargetFinal` / `targetC`
  slice under repaired `URI-R2-C1`, while leaving `instArgRootMultiBase`
  explicitly unselected.
- `G2` finalized in `round-047` as the authoritative implementation of only
  that bounded lane, with the reviewer-auditable proof
  `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst`, with
  `targetC -> rootFinal` admitted only for that local lane, with one local
  success example plus one matched non-local fail-closed contrast, and with
  `instArgRootMultiBase`, `boundVarTarget`, and non-local widening still out of
  scope.
- `G3` finalized in `round-048` as authoritative `attempt-1`, with
  `attempt_verdict = accepted`, `stage_action = finalize`, and canonical
  artifact path
  `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`.
- The accepted `G3` artifact already reverified the exact bounded `G2` lane via
  read-only `Fallback.hs` / `PipelineSpec.hs` anchors, a fresh focused
  `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a fresh
  full repo `cabal build all && cabal test` gate, and predecessor continuity.
- `U2` remains `authority-narrowed`, `U3` remains
  `uniqueness-owner-stable-refuted`, and `U4` remains
  `constructor-acyclic-termination-refuted`; none of those accepted negative
  findings may be reinterpreted as widening clearance here.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only. The resolved
  replay-path bug does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening, `instArgRootMultiBase`, or broader recursive
  widening in this round.

Current repository state is already non-pristine. Respect existing work and do
not revert or clean up unrelated changes while preparing the `G4` decision
artifact.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-049/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/retry-subloop.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
- `orchestrator/rounds/round-048/review-record.json`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`
   - create the canonical `G4` aggregate decision record.

Optional bounded round note file:

1. `orchestrator/rounds/round-049/implementation-notes.md`
   - optional reviewer-facing note file if long docs-only continuity output
     needs a bounded home.

Read-only decision anchors:

1. `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
2. `orchestrator/rounds/round-048/review-record.json`
3. `/Volumes/src/mlf4/Bugs.md`

Files that must remain untouched by `G4` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-049/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/roadmap.md`
- `/Volumes/src/mlf4/Bugs.md`
- prior artifacts and review history under
  `orchestrator/rounds/round-001/`
  through `orchestrator/rounds/round-048/`
- any docs outside the canonical `G4` artifact path and the optional round note
  file

## Sequential Tasks

### Task 1 - Freeze the `G4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `G4` artifact, state explicitly:
  - `Round: round-049`
  - `Roadmap item: G4`
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
- State that `G4` is aggregate-only and docs-only.
- State that `G4` records exactly one bounded next-step result token only.
- State that `G4` does not rerun `G1` / `G2` / `G3`, does not amend the
  roadmap, and does not itself widen the live subject or boundary.
- State that any contradiction discovered in the accepted `G3` evidence chain
  is a blocker to record through `stop-blocked`, not a license to patch code,
  add tests, reopen replay, or update `Bugs.md` during this attempt.
- State explicitly that reviewer outcomes for this stage are limited to
  `accepted + finalize` or `rejected + retry`, and that `accepted + retry` is
  forbidden for `G4`.

### Task 2 - Reconstruct the accepted `G3` evidence chain without reopening prior stages

- Carry forward only accepted authoritative evidence from the `G3` chain:
  - the selected live subject remains repaired `URI-R2-C1`;
  - the exact bounded lane remains the accepted `G2` local-binding
    `rootLocalMultiInst` / `targetC -> rootFinal` slice;
  - the accepted read-only anchors remain `Fallback.hs` and `PipelineSpec.hs`;
  - `instArgRootMultiBase` remains explicitly unselected and out of scope;
  - `boundVarTarget` widening remains out of scope; and
  - non-local widening remains out of scope.
- Use
  `orchestrator/rounds/round-048/review-record.json`
  as the authoritative proof that `G3` finalized as `stage_id = "G3"`,
  `attempt = 1`, `attempt_verdict = "accepted"`, `stage_action = "finalize"`,
  `status = "authoritative"`, with artifact path
  `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`.
- Carry forward the accepted `G3` checks map as the bounded decision baseline:
  `G3-CONTRACT`, `G3-G1-G2-AUTHORITY`, `G3-ANCHORS`, `G3-FOCUSED-RERUN`,
  `G3-FULL-GATE`, `G3-DIFF-BOUNDARY`, and `G3-CONTINUITY` all remained `pass`.
- Treat `/Volumes/src/mlf4/Bugs.md` only as continuity context. Do not
  reinterpret replay-path bug history as current-round repair authority.
- State explicitly that `G4` answers one bounded question only:
  given the accepted `G3` evidence chain and current docs-level continuity,
  what single lawful next-step result token applies to the repaired
  `URI-R2-C1` local-binding `rootLocalMultiInst` / `targetC -> rootFinal`
  lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, the required
     `round-048` review record is non-authoritative or contradictory, docs-level
     continuity checks fail, or the accepted `G3` verification can no longer be
     treated as the current bounded verification baseline for the same repaired
     `URI-R2-C1` local `rootLocalMultiInst` lane.
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
   - choose this if the accepted `G3` evidence chain remains authoritative, the
     accepted `G3` verification still stands as the current bounded verification
     baseline, no new blocker is found, and no already-accepted widening
     authority exists.
   - this result must preserve repaired `URI-R2-C1`, the inherited
     `Fallback.hs` / `PipelineSpec.hs` ownership anchor, and the explicit-only /
     non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
     boundary.
   - if selected, the decision rationale must keep `instArgRootMultiBase`
     explicitly unselected in the current accepted lane and frame any future
     work as a separate bounded cycle rather than as authorization to widen this
     round.

The canonical `G4` artifact must record exactly one of those three tokens and
must explain why the selected token is lawful and why the other two tokens are
not.

### Task 4 - Run the docs-only verification and continuity checks required for `G4`

Run the baseline docs/state checks required by
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-049/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-049/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-016/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `test -f docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
- `python3 -m json.tool orchestrator/rounds/round-048/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `stage_id == "G3"`
  - `attempt == 1`
  - `attempt_verdict == "accepted"`
  - `stage_action == "finalize"`
  - `status == "authoritative"`
  - `artifact_path == "docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md"`
  - the `checks` map still includes all required `G3-*` pass keys named in
    Task 2.
- `rg -n 'rootLocalMultiInst|targetC -> rootFinal|instArgRootMultiBase|boundVarTarget|non-local' docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
  to confirm the accepted bounded lane and exclusions remain the ones being
  aggregated.

Reconfirm the round stays docs-only:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun the focused `ARI-C1` block or `cabal build all && cabal test` in
`G4` `attempt-1`. The accepted `G3` artifact already supplies the fresh bounded
verification baseline for this exact lane, and `G4` authorizes no production or
test edits. If an unexpected non-doc/code-path diff appears, stop and record the
blocker rather than widening the round.

### Task 5 - Author the canonical `G4` aggregate decision artifact

Write
`docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`
as the canonical stage result.

The artifact must include:

- stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
  `Retry state`, `Live subject`, `Artifact kind`);
- a short contract section explaining that `G4` is aggregate-only and docs-only;
- a carry-forward section summarizing the authoritative `G1` / `G2` / `G3`
  chain without reopening it;
- a decision section that records exactly one token:
  `continue-bounded`, `widen-approved`, or `stop-blocked`;
- a rationale section proving why the chosen token is lawful under the closed
  decision rules and why the other two tokens are not;
- a continuity section preserving the accepted `G3`-bounded lane,
  `Fallback.hs` / `PipelineSpec.hs` ownership, and inherited explicit-only /
  non-equi-recursive / non-cyclic-graph boundary; and
- a non-authorization section stating that this round does not itself reopen
  `G1` / `G2` / `G3`, replay, `MLF.Elab.Inst` / `InstBot`,
  `instArgRootMultiBase`, `boundVarTarget`, non-local widening, or any broader
  recursive-inference work.

If the closed-rule decision resolves to `continue-bounded`, the artifact should
state that any future bounded cycle would require a new accepted roadmap update
and may not treat the remaining unselected `instArgRootMultiBase` family as
already cleared. If the closed-rule decision resolves to `stop-blocked` or
`widen-approved`, the artifact must record the exact accepted basis for that
result and keep it bounded to the already-reverified local lane.

## Non-Goals

- No change to the accepted `G1` bind itself.
- No change to the accepted `G2` implementation or `G3` verification.
- No selection or implementation of `instArgRootMultiBase` in this round.
- No edits to `src/MLF/Elab/Inst.hs`.
- No replay reopen, `InstBot` work, prototype/research entrypoints, public API
  changes, executable changes, roadmap mutation, `Bugs.md` edits, or
  `mlf2.cabal` changes.
- No widening into equi-recursive reasoning, implicit unfolding, cyclic
  structural graph encoding, multi-SCC support, cross-family search, heuristic
  owner selection, compatibility shims, or convenience fallbacks.

## Reviewer Checks For This Round

1. `plan.md` keeps `G4` bounded to the accepted `G3`
   `rootLocalMultiInst` / `targetC -> rootFinal` lane.
2. The canonical `G4` artifact records exactly one result token and explains
   why the selected token is lawful under the accepted `G3` evidence chain.
3. `widen-approved` is rejected unless an already-accepted roadmap or boundary
   amendment exists; `stop-blocked` is rejected unless a real contradiction or
   missing authority is found; `continue-bounded` is rejected unless the
   accepted `G3` chain still stands as authoritative.
4. The round remains docs-only and bounded to the accepted `G3` lane, with no
   reopening of `instArgRootMultiBase`, `boundVarTarget`, replay, non-local
   widening, or any other excluded family.
5. The docs/state continuity checks pass and no unexpected non-doc/code diff is
   introduced.
