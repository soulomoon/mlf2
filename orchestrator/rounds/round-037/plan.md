# Round 037 Plan (`C4` Next-Cycle Decision Gate)

## Objective

Execute only roadmap item `C4` and produce one accepted aggregate decision artifact
at
`docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`.

This round is aggregate-only and docs-only. It must consolidate the accepted
`C1` / `C2` / `C3` evidence chain into exactly one bounded next-step result for the
already-verified repaired `URI-R2-C1` local-binding-only fail-closed lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

No production, test, public API, executable, Cabal, roadmap, controller-state, or
bug-tracker edits are planned or authorized in `attempt-1`. This round does not
reopen `C1` selection, `C2` implementation, or `C3` verification.

## Locked Round Context

- Round id: `round-037`
- Roadmap item: `C4`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: aggregate-only, docs-only decision gate

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `C4` and must not appear in the plan, artifact,
or reviewer handoff.

Accepted predecessor facts that remain binding throughout `C4`:

- `C1` bound the cycle to repaired `URI-R2-C1`, preserved the inherited boundary,
  kept accepted `U2` / `U3` / `U4` negative findings binding, and froze exactly one
  bounded `C2` target: the local-binding-only `rootBindingIsLocalType`
  fail-closed retention slice.
- `C2` finalized in `round-035` as authoritative `attempt-2`, with
  `attempt_verdict=accepted`, `stage_action=finalize`, and focused same-case
  fail-closed evidence in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  and `test/PipelineSpec.hs`.
- `C3` finalized in `round-036` as authoritative `attempt-1`, with
  `attempt_verdict=accepted`, `stage_action=finalize`, and fresh bounded
  verification showing that the accepted `C2` slice stayed stable under the focused
  `ARI-C1 feasibility characterization (bounded prototype-only)` block, the fresh
  full repo gate, and predecessor continuity checks.
- `U6` finalized `continue-bounded`, not `widen-approved`, so this follow-on cycle
  remains non-widening unless an accepted later decision explicitly changes it.
- `BUG-2026-03-16-001` remains replay-lane continuity context only and does not
  authorize replay reopen, `MLF.Elab.Inst` work, or broader recursive-inference
  widening in this round.

Current repository state is already non-pristine. Respect existing work and do not
revert or clean up unrelated changes while preparing the `C4` decision artifact.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-037/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/retry-subloop.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
- `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `orchestrator/rounds/round-035/review-record.json`
- `orchestrator/rounds/round-036/review-record.json`
- `Bugs.md`

## Files Expected In Scope

Primary writable artifact:

1. `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
   - create the canonical `C4` aggregate decision record.

Optional bounded note file:

1. `orchestrator/rounds/round-037/implementation-notes.md`
   - optional command transcript / note file if the implementer needs a place for
     long continuity output.

Read-only decision anchors:

1. `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
2. `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
3. `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
4. `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
5. `orchestrator/rounds/round-035/review-record.json`
6. `orchestrator/rounds/round-036/review-record.json`
7. `Bugs.md`

Files that must remain untouched by `C4` `attempt-1`:

- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-037/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/roadmap.md`
- `Bugs.md`
- reviewer-owned history under prior round directories

## Sequential Tasks

### Task 1 - Freeze the `C4` `attempt-1` contract as an aggregate-only decision gate

- In the canonical `C4` artifact, state explicitly:
  - `Round: round-037`
  - `Roadmap item: C4`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding.
- State that `C4` records exactly one bounded result token only.
- State that `C4` does not perform new implementation, does not rerun the roadmap,
  and does not itself widen the live subject or boundary.
- State that any contradiction discovered in the accepted evidence chain is a blocker
  to record through `stop-blocked`, not a license to patch code, add tests, or amend
  the roadmap during this attempt.

### Task 2 - Reconstruct the accepted decision evidence chain without widening it

- Carry forward only accepted authoritative evidence:
  - `C1` froze the subject, boundary, and exact bounded `C2` target.
  - `C2` landed the local-binding-only `rootBindingIsLocalType` fail-closed slice.
  - `C3` verified that same slice under fresh bounded verification.
  - `U6` established the predecessor closed-rule semantics for choosing among
    `continue-bounded`, `widen-approved`, and `stop-blocked`.
- Use
  `orchestrator/rounds/round-035/review-record.json`
  and
  `orchestrator/rounds/round-036/review-record.json`
  as the authoritative acceptance proof that `C2` and `C3` finalized and remain
  authoritative.
- Treat `Bugs.md` only as continuity context.
  Do not reinterpret `BUG-2026-03-16-001` as current-round repair authority.
- State explicitly that `C4` answers one bounded question only:
  given the accepted `C1` / `C2` / `C3` chain and current docs-level continuity,
  what is the single lawful next-step result token for the repaired `URI-R2-C1`
  local-binding-only fail-closed lane?

### Task 3 - Apply a closed-rule decision sequence so exactly one token is recorded

Use the following order and stop at the first matching rule:

1. `stop-blocked`
   - choose this only if a required accepted artifact is missing, a required review
     record is non-authoritative or contradictory, docs-level continuity checks fail,
     or the accepted `C3` verification can no longer be treated as the current
     bounded verification baseline.
   - if this rule matches, record the exact blocker and do not patch around it.

2. `widen-approved`
   - choose this only if already-accepted evidence in the current repository proves
     that widening is now lawful without rewriting accepted history.
   - at minimum that would require an accepted artifact or roadmap amendment that
     explicitly changes the live subject/boundary and clears the previously binding
     `U2` / `U3` / `U4` negative findings as widening blockers.
   - absent such already-accepted evidence, `widen-approved` is not lawful.

3. `continue-bounded`
   - choose this if the `C1` / `C2` / `C3` evidence chain remains authoritative,
     the accepted `C3` verification still stands as the current bounded verification
     baseline, no new blocker is found, and no already-accepted widening authority
     exists.
   - this result must preserve repaired `URI-R2-C1` and the inherited explicit-only /
     non-equi-recursive / non-cyclic-graph boundary.

The canonical `C4` artifact must explain why the selected token is lawful and why the
other two tokens are not.

### Task 4 - Run the docs-only verification and continuity checks required for `C4`

Run the baseline docs/state checks required by
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/verification.md`:

- `git diff --check`
- `python3 -m json.tool orchestrator/rounds/round-037/state-snapshot.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-037/state-snapshot.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/retry-subloop.md`

Reconfirm authoritative continuity with at least these checks:

- `python3 -m json.tool orchestrator/rounds/round-035/review-record.json >/dev/null`
- `python3 -m json.tool orchestrator/rounds/round-036/review-record.json >/dev/null`
- a short `python3` assertion script proving:
  - `round-035` is `stage_id == "C2"`, `attempt_verdict == "accepted"`,
    `stage_action == "finalize"`, `status == "authoritative"`,
    `authoritative_attempt == 2`;
  - `round-036` is `stage_id == "C3"`, `attempt_verdict == "accepted"`,
    `stage_action == "finalize"`, `status == "authoritative"`,
    `authoritative_attempt == 1`;
  - both artifact paths match the accepted `C2` / `C3` canonical docs.

Reconfirm the round stays docs-only:

- `git status --short --untracked-files=all`
- `git diff --name-only`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`

Do not rerun `cabal build all && cabal test` in `C4` `attempt-1` unless an
unexpected non-doc/code-path edit appears first. The required current bounded
verification has already been supplied by accepted `C3`, and `C4` is only
aggregating that accepted verification into one decision token.

### Task 5 - Author the canonical `C4` aggregate decision artifact

Write
`docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
as the canonical stage result.

Required sections:

- stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
  `Retry state`, `Live subject`, `Artifact kind`);
- stage contract freeze;
- accepted evidence chain carried forward from `C1`, `C2`, `C3`, plus the
  predecessor `U6` closed-rule semantics;
- authoritative review-record continuity for `round-035` and `round-036`;
- docs-only verification / continuity check results;
- one explicit result-token section naming exactly one of
  `continue-bounded`, `widen-approved`, or `stop-blocked`;
- an explanation for why the other two tokens are not lawful;
- a full-gate skip note stating that `C4` is docs-only and relies on accepted `C3`
  as the current bounded verification baseline;
- blockers section (`None` if no blocker);
- non-authorization statement preserving the current subject/boundary and banning
  replay reopen, `MLF.Elab.Inst` work, code/test edits, roadmap edits, equi-recursive
  reasoning, cyclic structural graph encoding, multi-SCC widening, cross-family
  widening, second interfaces, compatibility shims, or convenience/default-path
  widening.

### Task 6 - Hand reviewer the exact acceptance checks for this round

The artifact set should let review confirm all of the following:

1. `plan.md` explicitly names `attempt-1`, aggregate-only docs-only scope, and the
   fact that `accepted + retry` is forbidden for `C4`.
2. The canonical `C4` artifact records exactly one result token and explains why the
   other two outcomes are not lawful under the accepted evidence.
3. The decision chain is limited to accepted `C1`, `C2`, `C3`, `U6`, the two
   authoritative review records, and continuity-only `Bugs.md` context.
4. The round did not reopen `C1` selection, `C2` implementation, or `C3`
   verification and did not widen beyond repaired `URI-R2-C1`.
5. The round stayed docs-only, and the artifact includes an explicit skip note for
   not rerunning the full Cabal gate because accepted `C3` already supplies the
   current bounded verification baseline.
6. No code, test, roadmap, controller-state, or bug-tracker edit was required to
   produce the `C4` decision token.

## Non-Goals

- No edits to `src/MLF/Elab/Inst.hs`
- No replay reopen or `InstBot` repair work
- No production or test changes
- No public API, executable, or `mlf2.cabal` changes
- No roadmap mutation, controller-state mutation, or `Bugs.md` edit
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as already
  cleared
- No equi-recursive reasoning, implicit unfolding, cyclic structural graph encoding,
  multi-SCC widening, cross-family widening, second interface, compatibility shim,
  or convenience/default-path widening
