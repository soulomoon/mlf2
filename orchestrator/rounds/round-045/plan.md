# Round 045 Plan (`F4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `F4` and produce one accepted aggregate decision artifact
at
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md`.

This round is an initial plan with `retry: null`, not a delta retry plan. It is
aggregate-only and docs-only. It must freeze exactly one bounded next-step result
token over the already reverified `F3` evidence chain for the accepted repaired
`URI-R2-C1` local-binding scheme-alias/base-like `rootLocalSchemeAliasBaseLike` /
`targetC -> rootFinal` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state, or
bug-tracker edits are planned or authorized in `attempt-1`. This round does not
reopen `F1` selection, `F2` implementation, or `F3` verification.

## Locked Round Context

- Round id: `round-045`
- Roadmap item: `F4`
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

`accepted + retry` is forbidden for `F4` and must not appear in the plan,
artifact, reviewer handoff, or final accepted review record.

Accepted predecessor facts that remain binding throughout `F4`:

- `F1` finalized in `round-042` as the authoritative bind that froze exactly one
  adjacent local-binding scheme-alias/base-like `keepTargetFinal` / `targetC`
  slice under repaired `URI-R2-C1`.
- `F2` finalized in `round-043` as the authoritative implementation of only that
  bounded lane, with the local proof
  `rootLocalSchemeAliasBaseLike = rootBindingIsLocalType && rootIsSchemeAlias && rootBoundIsBaseLike`,
  with `targetC -> rootFinal` admitted only for that local lane, with
  `boundVarTarget` absent for the selected slice, and with
  `rootHasMultiInst` / `instArgRootMultiBase` unchanged and out of scope.
- `F3` finalized in `round-044` as authoritative `attempt-1`, with
  `attempt_verdict = accepted`, `stage_action = finalize`, and canonical artifact
  path
  `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`.
- The accepted `F3` artifact already reverified the exact bounded `F2` lane via
  read-only `Fallback.hs` / `PipelineSpec.hs` anchors, the focused
  `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, a fresh
  full repo `cabal build all && cabal test` gate, and predecessor continuity.
- `U2` remains `authority-narrowed`, `U3` remains
  `uniqueness-owner-stable-refuted`, and `U4` remains
  `constructor-acyclic-termination-refuted`; none of those accepted negative
  findings may be reinterpreted as widening clearance here.
- `BUG-2026-03-16-001` remains replay-lane continuity context only and does not
  authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, or broader recursive
  widening in this round.

Current repository state is already non-pristine. Respect existing work and do not
revert or clean up unrelated changes while preparing the `F4` decision artifact.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-045/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/planner.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-044/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md`
   - create the canonical `F4` aggregate decision record.

Optional bounded round note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-045/implementation-notes.md`
   - optional reviewer-facing note file if long docs-only continuity output needs a
     bounded home.

Read-only decision anchors:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
2. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-044/review-record.json`
3. `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`

Files that must remain untouched by `F4` `attempt-1`:

- `/Users/ares/.codex/worktrees/d432/mlf4/src/`
- `/Users/ares/.codex/worktrees/d432/mlf4/src-public/`
- `/Users/ares/.codex/worktrees/d432/mlf4/app/`
- `/Users/ares/.codex/worktrees/d432/mlf4/test/`
- `/Users/ares/.codex/worktrees/d432/mlf4/mlf2.cabal`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- prior artifacts and review history under
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/` through
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-044/`
- any docs outside the canonical `F4` artifact path and the optional round note
  file

## Sequential Tasks

### Task 1 - Freeze the `F4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `F4` artifact, state explicitly:
  - `Round: round-045`
  - `Roadmap item: F4`
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
- State that `F4` is aggregate-only and docs-only.
- State that `F4` records exactly one bounded next-step result token only.
- State that `F4` does not rerun `F1` / `F2` / `F3`, does not amend the roadmap,
  and does not itself widen the live subject or boundary.
- State that any contradiction discovered in the accepted `F3` evidence chain is a
  blocker to record through `stop-blocked`, not a license to patch code, add
  tests, reopen replay, or update `Bugs.md` during this attempt.
- State explicitly that reviewer outcomes for this stage are limited to
  `accepted + finalize` or `rejected + retry`, and that `accepted + retry` is
  forbidden for `F4`.

### Task 2 - Reconstruct the accepted `F3` evidence chain without reopening prior stages

- Carry forward only accepted authoritative evidence from the `F3` chain:
  - the selected live subject remains repaired `URI-R2-C1`;
  - the exact bounded lane remains the accepted `F2` local-binding
    scheme-alias/base-like `rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal`
    slice;
  - the accepted read-only anchors remain `Fallback.hs` and `PipelineSpec.hs`;
  - `boundVarTarget` remains absent as authority for the selected slice; and
  - `rootHasMultiInst` / `instArgRootMultiBase` remain unchanged and out of scope.
- Use
  `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-044/review-record.json`
  as the authoritative proof that `F3` finalized as `stage_id = "F3"`,
  `attempt = 1`, `attempt_verdict = "accepted"`, `stage_action = "finalize"`,
  `status = "authoritative"`, with artifact path
  `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`.
- Carry forward the accepted `F3` checks map as the bounded decision baseline:
  `F3-CONTRACT`, `F3-DOCS-ONLY`, `F3-F2-CARRY-FORWARD`, `F3-FALLBACK-ANCHOR`,
  `F3-BOUNDVARTARGET-ABSENT`, `F3-OUT-OF-SCOPE-TRIGGERS`, `F3-FOCUSED-RERUN`,
  `F3-FULL-GATE`, `F3-CANONICAL-ARTIFACT`, and `F3-CONTINUITY` all remained
  `pass`.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` only as continuity
  context. Do not reinterpret `BUG-2026-03-16-001` as current-round repair
  authority.
- State explicitly that `F4` answers one bounded question only:
  given the accepted `F3` evidence chain and current docs-level continuity, what
  single lawful next-step result token applies to the repaired `URI-R2-C1`
  local-binding scheme-alias/base-like lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, the required
     `round-044` review record is non-authoritative or contradictory, docs-level
     continuity checks fail, or the accepted `F3` verification can no longer be
     treated as the current bounded verification baseline for the same repaired
     `URI-R2-C1` local scheme-alias/base-like lane.
   - if this rule matches, record the exact blocker and do not patch around it.

2. `widen-approved`
   - choose this only if already-accepted evidence in the current repository proves
     that widening is now lawful without rewriting accepted history.
   - at minimum that would require an accepted artifact or roadmap amendment that
     explicitly changes the live subject or inherited boundary and clears the still
     binding `U2` / `U3` / `U4` negative findings as widening blockers for this
     slice.
   - absent such already-accepted evidence, `widen-approved` is not lawful.

3. `continue-bounded`
   - choose this if the accepted `F3` evidence chain remains authoritative, the
     accepted `F3` verification still stands as the current bounded verification
     baseline, no new blocker is found, and no already-accepted widening authority
     exists.
   - this result must preserve repaired `URI-R2-C1`, the inherited
     `Fallback.hs` / `PipelineSpec.hs` ownership anchor, and the explicit-only /
     non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
     boundary.

The canonical `F4` artifact must record exactly one of those three tokens and must
explain why the selected token is lawful and why the other two tokens are not.

### Task 4 - Run the docs-only verification and continuity checks required for `F4`

Run the baseline docs/state checks required by
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
- `python3 -m json.tool orchestrator/rounds/round-044/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `stage_id == "F3"`
  - `attempt == 1`
  - `attempt_verdict == "accepted"`
  - `stage_action == "finalize"`
  - `status == "authoritative"`
  - `artifact_path == "docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md"`
  - the `checks` map still includes all required `F3-*` pass keys named in
    Task 2.
- `rg -n 'rootLocalSchemeAliasBaseLike|targetC -> rootFinal|boundVarTarget|rootHasMultiInst|instArgRootMultiBase' docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  to confirm the accepted bounded lane and exclusions remain the ones being
  aggregated.

Reconfirm the round stays docs-only:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun the focused `ARI-C1` block or `cabal build all && cabal test` in
`F4` `attempt-1`. The accepted `F3` artifact already supplies the fresh bounded
verification baseline for this exact lane, and `F4` authorizes no production or
test edits. If an unexpected non-doc/code-path diff appears, stop and record the
blocker rather than widening the round.

### Task 5 - Author the canonical `F4` aggregate decision artifact

Write
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md`
as the canonical stage result.

Required sections:

- stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
  `Retry state`, `Live subject`, `Artifact kind`);
- stage contract freeze;
- accepted `F3` evidence chain carried forward without reopening `F1` / `F2` /
  `F3`;
- authoritative review-record continuity for `round-044`;
- docs-only verification / continuity check results;
- one explicit result-token section naming exactly one of
  `continue-bounded`, `widen-approved`, or `stop-blocked`;
- an explanation for why the other two tokens are not lawful;
- a full-gate skip note stating that `F4` is docs-only and relies on accepted `F3`
  as the current bounded verification baseline;
- blockers section (`None` if no blocker); and
- non-authorization statement preserving the current subject/boundary and banning:
  replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening for this
  slice, `rootHasMultiInst` / `instArgRootMultiBase` widening, equi-recursive
  reasoning, cyclic structural encoding, multi-SCC widening, cross-family
  widening, a second executable interface, compatibility shims, and
  convenience/default-path widening.

### Task 6 - Hand reviewer the exact acceptance checks for this round

The artifact set should let review confirm all of the following:

1. `plan.md` explicitly names `attempt-1`, aggregate-only docs-only scope,
   `retry: null`, and the stage-outcome rule
   `accepted + finalize | rejected + retry`, with `accepted + retry` forbidden.
2. The only planned writable deliverable is the canonical `F4` artifact path,
   with optional round notes only; no production, test, roadmap, controller-state,
   or bug-tracker edits are authorized.
3. The final `F4` artifact records exactly one lawful result token from the closed
   set `continue-bounded`, `widen-approved`, `stop-blocked`.
4. The recorded result token is grounded in the accepted `F3` evidence chain and
   `round-044/review-record.json`, not in reopened `F1` / `F2` / `F3` work.
5. The accepted `F3` checks remain the decision baseline, including
   `boundVarTarget` absence for the selected slice and unchanged out-of-scope
   `rootHasMultiInst` / `instArgRootMultiBase` trigger families.
6. The round remains inside repaired `URI-R2-C1` and preserves the inherited
   explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface /
   no-fallback boundary.
7. The round does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
   `boundVarTarget` widening for this slice, or any convenience/default widening.
8. Docs-only diff evidence and the full-gate skip note justify why no new code-path
   verification rerun is lawful or required for this aggregate-only decision gate.
