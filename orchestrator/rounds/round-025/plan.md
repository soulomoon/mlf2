# Round 025 Plan Delta (`R2` Retry Attempt 3)

## Retry Context

- Round: `round-025`
- Roadmap item: `R2`
- Stage: `plan`
- Active attempt: `attempt-3`
- Retry mode: active under `contract_version: 2`
- Fixed subject/scenario: `URI-R2-C1` / `uri-r2-c1-only-v1`
- Fixed repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
- Fixed owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Controlling bug: `BUG-2026-03-16-001`

Inherited authoritative audit remains unchanged:

- `P1 = pass`
- `P2 = semantic-negative`
- `D1 = pass`
- `D2 = pass`
- `D3 = pass`
- `D4 = reopen-repair-track`
- `R1 = pass`

Stage ordering remains unchanged: `R1` authoritative evidence, `R2` active retry, `R3` pending, `R4` pending.

## Recorded Retry Cause

Attempt 2 was accepted as bounded `applyInstantiation` / `InstBot` repair evidence, but `R2` still cannot finalize because `cabal build all && cabal test` fails on four inherited expectations in `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`:

1. `P2-W` now reports `replay-domain-widening` instead of the historical `partial-replay`.
2. `D1` now reports `semantic-negative` instead of `pass`.
3. `D2` now reports `semantic-negative` instead of `pass`.
4. `D3` now reports `semantic-negative` instead of `pass`.

Recorded fix hypothesis to execute without widening scope:

1. Keep the bounded `InstBot` repair unchanged.
2. Clear, quarantine, or contract-scope the four inherited prototype-expectation failures.
3. Do not widen beyond the current repair-track control plane or rewrite predecessor evidence.

## File Scope For Attempt 3

Primary files that may change:

- `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Research/URI/R2/C1/Prototype/P2.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Research/URI/R2/C1/Prototype/D1.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Research/URI/R2/C1/Prototype/D2.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Research/URI/R2/C1/Prototype/D3.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Research/URI/R2/C1/Prototype/Artifact.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`

Files that stay read-only unless a tiny adjacent contract adjustment is proven necessary:

- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/TypeCheck.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/test/ElaborationSpec.hs`

Historical evidence that must not be rewritten:

- `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-016` through `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-024`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/reviews/attempt-1.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/reviews/attempt-2.md`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-025/orchestrator/rounds/round-025/attempt-log.jsonl`
- `/Users/ares/.codex/worktrees/d432/mlf4/Bugs.md`

## Delta Tasks Only

### Task 1 - Classify the four failures before changing behavior

- Reproduce only the four failing examples in `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`.
- Compare each failure against the current prototype contracts in `P2.hs`, `D1.hs`, `D2.hs`, `D3.hs`, and `Artifact.hs`.
- Decide which of the three bounded outcomes is actually correct:
  - `clear`: the tests are stale and should be updated to the already-intended post-repair contract;
  - `contract-scope`: the tests should keep predecessor facts historical and stop asserting live re-execution results that the bounded `R2` repair intentionally changes;
  - `quarantine`: the tests must remain in-tree but be made explicitly non-gating for the `R2` repair lane.
- Do not touch `applyInstantiation` while this classification remains unresolved.

### Task 2 - Prefer contract-scoping over production drift

- If the observed outputs are the direct consequence of the accepted bounded `InstBot` repair, keep `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs` unchanged and solve the blocker at the prototype-contract layer.
- First preference: update `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs` so the four assertions reflect the intended bounded relationship between:
  - immutable predecessor authority (`P2 = semantic-negative`, `D1 = pass`, `D2 = pass`, `D3 = pass` as recorded in historical artifacts), and
  - current live replay behavior after the localized `R2` repair.
- Only if the prototype modules themselves are internally inconsistent with that boundary may the retry edit the adjacent research files `P2.hs`, `D1.hs`, `D2.hs`, `D3.hs`, or `Artifact.hs`.
- Any such adjacent adjustment must stay inside the research/prototype layer, must not add a new entrypoint or fallback, and must not rewrite how earlier authoritative artifacts are stored.

### Task 3 - Quarantine only if contract-scoping cannot express the boundary cleanly

- If a clean contract-scoped assertion is impossible without reopening predecessor semantics, use the smallest explicit quarantine available inside `/Users/ares/.codex/worktrees/d432/mlf4/test/Research/UriR2C1PrototypeP1Spec.hs`.
- The quarantine must be narrow to these four inherited examples only, documented in-code as an `R2` repair-track gate exception, and must not suppress unrelated research or production checks.
- Do not hide the reason in review text only; the test surface itself must make the quarantine legible and intentional.

### Task 4 - Preserve historical evidence while refreshing the `R2` artifact

- Update `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` for `Attempt: 3`.
- Record:
  - that the bounded `InstBot` repair from attempt 2 was retained unchanged unless a tiny adjacent prototype-contract fix proved necessary;
  - which of `clear`, `contract-scope`, or `quarantine` was chosen for the four inherited prototype failures;
  - the exact files changed;
  - why predecessor rounds and review artifacts remained read-only.

### Task 5 - Revalidate both the repair lane and the gate

- Rerun the focused `R2` checks from attempt 2 first to prove the localized `applyInstantiation` repair still holds.
- Then rerun the previously failing `UriR2C1PrototypeP1Spec` examples, followed by `cabal build all && cabal test`.
- If the full gate still fails, record whether the remaining blocker is:
  - another inherited research-contract expectation, or
  - evidence that the attempted scoping/quarantine is incomplete.
- Do not claim `R2` completion unless the full gate passes.

## Non-Goals That Still Apply

- No replanning of `R1`, `R3`, or `R4`
- No roadmap edits
- No new executable interface
- No compatibility fallback or convenience widening
- No widened replay-semantics rewrite
- No rewriting of predecessor round artifacts or prior retry history

## Reviewer Focus For Attempt 3

1. The bounded `applyInstantiation` / `InstBot` repair remains unchanged unless a tiny adjacent prototype-contract fix is explicitly justified.
2. The four `UriR2C1PrototypeP1Spec` failures are resolved by bounded contract work, not by widening the repair lane.
3. Historical predecessor evidence remains immutable and is consumed only as authority, not regenerated.
4. Any quarantine is explicit, narrow, and limited to the inherited prototype blocker.
5. The final review records both the focused `R2` checks and the full-gate result exactly.
