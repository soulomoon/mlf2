# Round 026 Plan (`R3` Locked Replay-Path Verification)

## Objective

Execute only roadmap item `R3` for the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` lane and produce one reviewer-auditable verification artifact showing that the accepted bounded `R2` repair now carries the authoritative replay path successfully without reopening repair work.

This round verifies the already-accepted `applyInstantiation` / `InstBot` repair boundary. It does not authorize a second repair surface, a second executable interface, a compatibility fallback, or any broadened replay/regression campaign.

## Locked Round Context

- Round id: `round-026`
- Stage: `plan` for roadmap item `R3`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Subject boundary: `URI-R2-C1`
- Scenario boundary: `uri-r2-c1-only-v1`
- Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
- Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Controlling bug: `BUG-2026-03-16-001`

Inherited authoritative audit that must remain unchanged:

- `P1 = pass`
- `P2 = semantic-negative`
- `D1 = pass`
- `D2 = pass`
- `D3 = pass`
- `D4 = reopen-repair-track`
- `R1 = pass`
- `R2 = pass`

Roadmap ordering must remain unchanged: `R1` and `R2` stay authoritative evidence, `R3` is the only active stage, and `R4` remains pending.

## Authoritative Inputs To Preserve

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-026/selection.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-025/review-record.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md` (`BUG-2026-03-16-001`)
- predecessor evidence under `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-016/` through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-025/`

## Files Expected In Scope

Primary writable files:

1. `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`
2. `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-026/implementation-notes.md` only if one bounded reviewer handoff note is needed

Verification evidence to inspect but keep read-only unless a tiny verification-only assertion is strictly required:

1. `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`
2. `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/TypeCheck.hs`
3. `/Users/ares/.codex/worktrees/d432/mlf4/test/ElaborationSpec.hs`
4. `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`

Files that must remain untouched in this round:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`
- prior round artifacts, including `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-025/reviews/attempt-3.md` and `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-025/review-record.json`

## Sequential Tasks

### Task 1 - Reconfirm the accepted `R2` carry-forward and freeze the verification boundary

- Re-state inside the `R3` artifact that `round-025` attempt `3` is the authoritative `R2 = pass` result and that `R3` inherits, rather than reworks, that accepted repair.
- Record the exact fixed lane:
  - subject `URI-R2-C1`
  - scenario `uri-r2-c1-only-v1`
  - repair boundary `witness-replay/applyInstantiation-instbot-precondition`
  - owner boundary `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Record the fresh-attempt semantics explicitly: `attempt-1`, `retry: null`.
- State the fail-closed rule for this round: if the locked replay path no longer succeeds, capture the failure as verification evidence and stop; do not widen into a new repair round from inside `R3`.

### Task 2 - Re-run the locked replay-path checks on the already-accepted owner lane

- Re-execute the focused owner-lane tests that together prove the accepted repair stays both successful and bounded:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'`
- Treat the first command as the authoritative `R3` replay-success proof for the locked scenario.
- Treat the remaining three commands as the required boundedness proof that the old `InstBot expects ⊥` failure is gone only on the accepted replay lane and still fails closed outside it.
- If any of these commands fails, record the exact failure in the artifact and round notes without editing production code in this stage.

### Task 3 - Verify that no prohibited widening or second interface was introduced

- Re-run the bounded continuity checks that demonstrate `R2` did not quietly expand into a broader replay campaign:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "allows attempt-2 reruns and records live replay widening as bounded non-pass"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D1 attempt-1 via the root-cause tuple and records continuity drift against the historical replay boundary"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D2 attempt-1 via the root-cause tuple and reports continuity-blocked localization on live reruns"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D3 attempt-1 via the root-cause tuple and reports bounded-negative continuity drift on live reruns"'`
- Inspect the current tree to prove the owner boundary remains localized and no alternate interface appeared:
  - verify current evidence still centers on `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`, adjacent plumbing in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/TypeCheck.hs`, and the focused verification coverage in `/Users/ares/.codex/worktrees/d432/mlf4/test/ElaborationSpec.hs` plus `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`
  - verify there is no new diff under `app/`, `src-public/`, `src/MLF/Research/`, or `mlf2.cabal`
- Do not add a new verification executable, new replay entrypoint, or a second production interface just to make `R3` easier to demonstrate.

### Task 4 - Emit the canonical `R3` verification artifact

- Write `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`.
- The artifact must record:
  - `Attempt: 1`
  - the inherited authoritative chain `P1` through `R2`
  - the exact locked lane `URI-R2-C1` / `uri-r2-c1-only-v1`
  - the fixed repair and owner boundaries
  - the exact commands run for replay success, boundedness, continuity, and full-gate verification
  - the observed result that the authoritative replay path now succeeds and the old `InstBot expects ⊥, got: t9 -> t9` mismatch no longer occurs on that lane
  - the observed result that non-replay `InstBot` calls still fail closed
  - the observed result that no second interface, compatibility fallback, or broadened replay campaign was introduced
  - the exact files inspected and any files changed
  - a handoff statement that only `R4` may make the final `repair-accepted` / `repair-blocked` decision

### Task 5 - Re-run the full verification gate and prepare reviewer handoff

- Run the baseline checks required by `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md`:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/state.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
  - `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
  - `test -f orchestrator/retry-subloop.md`
- Run `cabal build all && cabal test` even if the round changes only docs, because `R3` is itself the locked replay verification stage and must actively re-prove the accepted repair on the full repository gate.
- Ensure reviewer evidence can confirm:
  1. `R3` stayed inside the locked `URI-R2-C1` / `uri-r2-c1-only-v1` lane.
  2. The accepted owner boundary stayed localized to `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
  3. The authoritative replay path succeeds and the old mismatch no longer occurs on that lane.
  4. Strict non-replay `InstBot` failures still hold, proving no fallback widening.
  5. No second executable interface, compatibility shim, or broadened replay campaign was introduced.
  6. `R4` remains pending and undecided.

## Non-Goals

- No new production repair in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`
- No change to `R1`, `R2`, or `R4`
- No roadmap edits or controller-state edits
- No rewrite of predecessor evidence, prior retry history, or `Bugs.md`
- No new executable interface, no compatibility fallback, and no widened replay/regression campaign
- No final `repair-accepted` / `repair-blocked` decision inside `R3`

## Reviewer Checks

Baseline checks from `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md` still apply.

Round-specific checks:

1. `plan.md` and the resulting `R3` artifact name `attempt-1` explicitly and preserve the fresh-attempt `retry: null` semantics.
2. The round remains fixed to `URI-R2-C1`, `uri-r2-c1-only-v1`, `witness-replay/applyInstantiation-instbot-precondition`, and `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
3. The recorded replay-success evidence uses the existing authoritative replay-path test and shows the old mismatch no longer occurs on the locked lane.
4. The recorded boundedness evidence shows strict non-replay `InstBot` failures still hold.
5. The continuity evidence shows the accepted `R2` repair did not reopen a broader replay campaign and preserved inherited authoritative audit.
6. The round does not introduce or rely on a second executable interface, compatibility fallback, or broader replay verification surface.
7. The `R3` artifact hands off only to `R4`; it does not preempt the final repair decision.
