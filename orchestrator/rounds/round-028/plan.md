# Round 028 Plan (`U1` Inherited Baseline And Repaired-Subject Bind)

## Objective

Execute only roadmap item `U1` and produce one accepted bind artifact at:
`/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`.

This round must restate the inherited automatic-recursive baseline, bind the live subject to repaired `URI-R2-C1`, and make hard stop triggers reviewer-visible before any `U2` through `U6` work can proceed.

## Locked Round Context

- Round id: `round-028`
- Stage: `plan` for roadmap item `U1`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Active subject (fixed): repaired `URI-R2-C1`
- Mandatory inherited boundary (fixed): `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: bounded docs/evidence bind only (no implementation slice)

Inherited predecessor facts that must remain unchanged:

- Automatic recursive inference is still unresolved/disabled as inherited baseline.
- `URI-R2-C1` replay repair track finalized as `repair-accepted`.
- Successor rounds must remain fail-closed and bounded until later accepted evidence explicitly authorizes widening.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-028/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` (continuity-only reference)

## Files Expected In Scope

Primary writable file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md`

Optional bounded note file:

1. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-028/implementation-notes.md` (only if needed for reviewer clarity)

Files that must remain untouched in this round:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-028/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- production code and test surfaces under `/Users/ares/.codex/worktrees/d432/mlf4/src/`, `/Users/ares/.codex/worktrees/d432/mlf4/src-public/`, `/Users/ares/.codex/worktrees/d432/mlf4/app/`, `/Users/ares/.codex/worktrees/d432/mlf4/test/`, `/Users/ares/.codex/worktrees/d432/mlf4/mlf2.cabal`
- prior round artifacts under `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-001/` through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-027/`

## Sequential Tasks

### Task 1 - Freeze `U1` attempt-1 contract and boundary

- In the `U1` artifact, state explicitly that this is `attempt-1` with `retry: null`.
- Reassert fixed live subject: repaired `URI-R2-C1`.
- Reassert fixed inherited boundary: explicit-only semantics, no equi-recursive reasoning, no cyclic structural graph encoding.
- State that `U1` is a bind stage only and does not clear implementation authority by itself.

### Task 2 - Consolidate inherited baseline truth from predecessor evidence

- Re-read and summarize the inherited baseline contract from:
  - `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- Carry forward only bounded continuity facts needed for `U1`:
  - explicit recursive path is implemented/accepted;
  - broad automatic recursive inference remains unresolved and disabled;
  - repaired `URI-R2-C1` is the only currently bound successor subject.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` as continuity context only; do not perform bug-triage changes in this stage.

### Task 3 - Author the canonical `U1` bind artifact

- Write `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-u1-unannotated-baseline-bind.md` with reviewer-auditable sections that include:
  - stage metadata (`Round`, `Roadmap item`, `Attempt`, `Retry state`);
  - inherited baseline restatement (automatic inference still unresolved/disabled);
  - explicit live-subject bind to repaired `URI-R2-C1`;
  - reviewer-visible hard stop trigger list that still forbids broad automatic inference.
- Hard stop triggers must explicitly include:
  - no equi-recursive equality or implicit unfolding success path;
  - no cyclic structural graph encoding;
  - no silent widening to multi-SCC, cross-family, or broad search-wide automatic inference;
  - no second executable interface, compatibility shim, or convenience fallback/default-on widening path;
  - no preemption of `U2` through `U6`.
- Keep the artifact as `U1` bind-only evidence; do not include `U2` authority analysis, uniqueness verdicting, feasibility verdicting, or implementation planning.

### Task 4 - Run and record bounded verification for `U1`

- Run baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` applicable to this docs/orchestrator slice:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-unannotated-iso-recursive-successor-roadmap-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f orchestrator/retry-subloop.md`
- Add `U1`-specific verification notes in the artifact showing:
  - the live subject is explicitly bound to repaired `URI-R2-C1`;
  - the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary is preserved;
  - hard stop triggers are present and concrete.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless this round unexpectedly edits code-bearing surfaces;
  - if docs-only scope holds, record the exact skip note in round artifacts.

### Task 5 - Prepare reviewer handoff for retry-aware `U1` semantics

- Ensure `U1` round output gives reviewer enough evidence to emit one legal retry-contract combination from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer records include:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if this stage later retries, prior attempt artifacts remain unchanged and new evidence is additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No broad automatic recursive inference design/implementation.
- No equi-recursive semantics, implicit unfolding, or cyclic graph authorization.
- No `U2` through `U6` pre-work, verdicting, or artifact substitution.
- No edits to `orchestrator/state.json`, `orchestrator/roadmap.md`, `Bugs.md`, or prior-round history.
- No production code/test edits and no second executable interface/fallback path.

## Reviewer Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` still apply.

Round-specific checks:

1. `plan.md` and produced `U1` artifact explicitly name `attempt-1` and `retry: null`.
2. The artifact binds the live subject explicitly to repaired `URI-R2-C1` and does not widen the subject.
3. The artifact restates inherited baseline truth accurately: explicit recursive path accepted; broad automatic recursive inference unresolved/disabled.
4. The hard stop trigger list is explicit, concrete, and preserves the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
5. The round remains `U1`-only and does not preempt `U2` through `U6`.
6. No second interface, compatibility fallback, or default-on widening path is introduced.
7. Verification evidence includes baseline commands and a clear full-gate skip justification when the round remains docs-only.
