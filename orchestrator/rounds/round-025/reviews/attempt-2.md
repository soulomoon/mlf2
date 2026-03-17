# Round `round-025` Attempt `2` Review (`R2`)

- Baseline checks:
  - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 diff --check` (pass)
  - `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null` (pass)
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json` (pass; live controller state still uses `contract_version: 2` and an active `retry` block for `R2` attempt `2`)
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md` (pass; ordered status list remains present for `R1` through `R4`)
  - `test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md && test -f /Users/ares/.codex/worktrees/d432/mlf4/docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md && test -f /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md` (pass)
  - `cabal build all && cabal test` (fail; `1124 examples, 4 failures`, all in `test/Research/UriR2C1PrototypeP1Spec.hs`)
    - `test/Research/UriR2C1PrototypeP1Spec.hs:208:59` expected `Just "partial-replay"`, got `Just "replay-domain-widening"`
    - `test/Research/UriR2C1PrototypeP1Spec.hs:374:41` expected `"pass"`, got `"semantic-negative"`
    - `test/Research/UriR2C1PrototypeP1Spec.hs:503:41` expected `"pass"`, got `"semantic-negative"`
    - `test/Research/UriR2C1PrototypeP1Spec.hs:614:41` expected `"pass"`, got `"semantic-negative"`
  - Continuity guards:
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 diff --name-only -- orchestrator/rounds/round-0{01..09} orchestrator/rounds/round-1{0..9} orchestrator/rounds/round-2{0..4} tasks/todo/2026-03-11-recursive-types-orchestration Bugs.md` (pass; no output)
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 ls-files --others --exclude-standard -- orchestrator/rounds/round-0{01..09} orchestrator/rounds/round-1{0..9} orchestrator/rounds/round-2{0..4} tasks/todo/2026-03-11-recursive-types-orchestration Bugs.md` (pass; no output)
    - `wc -l /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/attempt-log.jsonl` (pass; still `1`, so the controller-owned attempt log remains unchanged before this review)
    - `rg -n 'Attempt verdict|Stage action|Retry reason|Fix hypothesis' /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/reviews/attempt-1.md` (pass; prior review snapshot is still present and remains the immutable attempt-1 history)

- Task-specific checks:
  - Diff remains inside the bounded `R2` slice:
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 diff --name-only` => `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `test/ElaborationSpec.hs`
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 ls-files --others --exclude-standard -- docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md orchestrator/rounds/round-025` => only the `R2` stage artifact and current round packet are untracked
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 diff --name-only -- app src-public src/MLF/Research mlf2.cabal test/Research/UriR2C1PrototypeP1Spec.hs` (pass; no output, so there is no second executable interface, public-surface drift, replay-search widening, or direct edit to the still-failing prototype-entrypoint test)
  - Retry-delta alignment against the attempt-2 plan:
    - `rg -n 'matchType|allowReplayBoundMatch|instElimEnv' /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/Inst.hs` shows the rejected attempt-1 `matchType` escape hatch is gone; the new repair uses replay-context threading plus `allowReplayBoundMatch`
    - `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/Inst.hs | sed -n '32,159p'` shows the new environment threading is confined to `evalInstantiationWith`, and the non-bottom `InstBot` acceptance remains localized to `applyInstantiation` lines `137-149`
    - `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/TypeCheck.hs | sed -n '114,139p'` shows the adjacent `TypeCheck` edit is plumbing-only (`instElimEnv = \_v _replacement env' -> env'`), so checked semantics do not inherit the replay relaxation
  - Focused `R2` checks:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'` (pass; `1 example, 0 failures`)
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'` (pass; `1 example, 0 failures`)
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane"'` (pass; `1 example, 0 failures`)
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'` (pass; `1 example, 0 failures`)
  - Locked-lane evidence:
    - `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/test/ElaborationSpec.hs | sed -n '445,499p;1520,1554p'` shows the new type-variable-renaming comparator plus the three focused `URI-R2-C1` / misuse assertions
    - `sed -n '1,220p' /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` keeps the fixed subject/scenario/owner boundary, records the rejected attempt-1 approach, and reports the same full-gate failure set rather than claiming completion
  - Inherited-evidence continuity verified:
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-016/review-record.json` keeps `P1 = pass`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-017/review-record.json` keeps `P2 = semantic-negative`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-020/review-record.json` keeps `D1 = pass`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-021/review-record.json` keeps `D2 = pass`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-022/review-record.json` keeps `D3 = pass`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-023/review-record.json` keeps `D4 = reopen-repair-track`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-024/review-record.json` keeps `R1 = pass`
    - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/Bugs.md` still tracks the same controlling defect `BUG-2026-03-16-001` at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
  - Same inherited full-gate blocker, not a new widened diff:
    - the current full-gate failure set matches the four failures already recorded in `reviews/attempt-1.md`
    - `git -C /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025 diff --name-only -- test/Research/UriR2C1PrototypeP1Spec.hs` returns no output, so the still-red prototype-entrypoint failures are not introduced by this attempt's tracked diff
  - Reviewer judgment:
    - the attempt satisfies the retry delta plan's bounded-owner repair requirement and closes the attempt-1 free-variable misuse hole without introducing fallback behavior or a second interface
    - the attempt does not satisfy the repo-local finalization contract because the mandatory full `cabal build all && cabal test` baseline still fails

- Implemented stage result:
- `The attempt lands a bounded replay-context repair in /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/Inst.hs and all focused R2 checks pass, but the stage remains gate-blocked because the mandatory full cabal build all && cabal test baseline is still red on the same four inherited test/Research/UriR2C1PrototypeP1Spec.hs expectations.`

- Attempt verdict:
- `accepted`

- Stage action:
- `retry`

- Retry reason:
- `The localized applyInstantiation / InstBot repair now appears acceptable for URI-R2-C1 / uri-r2-c1-only-v1, but the repo-local contract still forbids finalizing R2 while cabal build all && cabal test fails on the unchanged four prototype-entrypoint expectations (P2, D1, D2, D3).`

- Fix hypothesis:
- `Keep the bounded InstBot repair unchanged and use the next retry to clear, quarantine, or contract-scope the four inherited UriR2C1PrototypeP1Spec expectation failures without widening beyond the current repair-track control plane.`

- Decision summary:
- Accept attempt 2 as valid bounded evidence, but do not make it authoritative carry-forward yet.
- The attempt closes the attempt-1 widening defect and stays within the localized owner boundary, yet the mandatory repo gate still blocks `accepted + finalize`.

- Evidence summary:
- Stage artifact: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
- Review snapshot: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/reviews/attempt-2.md`
- Key implementation evidence: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/Inst.hs`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/src/MLF/Elab/TypeCheck.hs`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/test/ElaborationSpec.hs`
- Key inherited authority: `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-016/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-017/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-020/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-021/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-022/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-023/review-record.json`, `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-024/review-record.json`
