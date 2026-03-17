# `R3` Locked Replay-Path Verification For `URI-R2-C1`

Date: 2026-03-17
Roadmap item: `R3`
Stage: `implement`
Attempt: 1
Retry state: `null`
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Controlling bug: `BUG-2026-03-16-001`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)

## Inherited Authoritative Audit

- `P1 = pass` via `orchestrator/rounds/round-016/review-record.json`
- `P2 = semantic-negative` via `orchestrator/rounds/round-017/review-record.json`
- `D1 = pass` via `orchestrator/rounds/round-020/review-record.json`
- `D2 = pass` via `orchestrator/rounds/round-021/review-record.json`
- `D3 = pass` via `orchestrator/rounds/round-022/review-record.json`
- `D4 = reopen-repair-track` via `orchestrator/rounds/round-023/review-record.json`
- `R1 = pass` via `orchestrator/rounds/round-024/review-record.json`
- `R2 = pass` via `orchestrator/rounds/round-025/review-record.json` (`attempt = 3`, authoritative)

`R3` inherits the accepted bounded `R2` repair. This round does not rework production logic. It verifies only that the already-accepted owner-lane repair still succeeds on the locked replay path, still fails closed outside that lane, and has not widened into a second interface or broader replay campaign.

## Fail-Closed Rule

If the locked replay path had failed during this round, `R3` would have recorded that failure as verification evidence and stopped. This stage does not authorize new repair work, owner widening, or any second executable path.

## Replay-Success And Boundedness Checks

Focused owner-lane commands rerun on the accepted `R2` state:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'`

Observed result:

- the authoritative replay-path check passed (`1 example, 0 failures`);
- the replayed type remained alpha-equivalent to the locked no-fallback shape, so the old localized mismatch `InstBot expects ⊥, got: t9 -> t9` no longer occurs on the accepted replay lane;
- the three strict non-replay `InstBot` rejection checks all passed (`1 example, 0 failures` each), so explicit non-bottom bounds still fail closed outside the accepted replay path.

## Continuity And No-Widening Checks

Required continuity commands rerun for the inherited predecessor contract:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "allows attempt-2 reruns and records live replay widening as bounded non-pass"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D1 attempt-1 via the root-cause tuple and records continuity drift against the historical replay boundary"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D2 attempt-1 via the root-cause tuple and reports continuity-blocked localization on live reruns"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D3 attempt-1 via the root-cause tuple and reports bounded-negative continuity drift on live reruns"'`

Observed result:

- all four continuity checks passed (`1 example, 0 failures` each);
- the inherited prototype/root-cause continuity expectations remain explicit evidence only, not live repair scope;
- `R3` therefore stayed inside `URI-R2-C1` / `uri-r2-c1-only-v1` and did not reopen a broader replay campaign.

## Owner-Boundary Inspection

Read-only inspection centered on:

- `src/MLF/Elab/Inst.hs`
- `src/MLF/Elab/TypeCheck.hs`
- `test/ElaborationSpec.hs`
- `test/Research/UriR2C1PrototypeP1Spec.hs`

The accepted owner lane still centers on `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), with only adjacent shared-evaluator plumbing in `MLF.Elab.TypeCheck` and focused verification coverage in the two test modules above.

No second executable interface or alternate production path was introduced for `R3`.

## Files Changed

- `docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md`
- `orchestrator/rounds/round-026/implementation-notes.md`

No production or test files changed in this round.

## Baseline And Full-Gate Verification

Commands run:

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
- `test -f orchestrator/retry-subloop.md`
- `git diff --stat -- src/MLF/Elab/Inst.hs src/MLF/Elab/TypeCheck.hs test/ElaborationSpec.hs test/Research/UriR2C1PrototypeP1Spec.hs app src-public src/MLF/Research mlf2.cabal`
- `cabal build all && cabal test`

Observed result:

- baseline checks passed;
- diff inspection stayed empty for `app/`, `src-public/`, `src/MLF/Research/`, and `mlf2.cabal`, so `R3` did not add a second interface or broaden the repair surface;
- the full repository gate passed.

## Handoff

`R3` records bounded verification evidence only. `R4` remains the only stage allowed to make the final `repair-accepted` / `repair-blocked` decision.
