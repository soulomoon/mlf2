# `R4` Repair Decision Gate For `URI-R2-C1`

Date: 2026-03-17
Roadmap item: `R4`
Stage: `implement`
Attempt: 1
Retry state: `null`
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Controlling bug: `BUG-2026-03-16-001`
Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
Artifact kind: aggregate-only terminal decision gate

## Terminal Contract Freeze

This round is aggregate-only and terminal under `orchestrator/retry-subloop.md`.
It decides the bounded repair-track outcome only.

It may not:

- reopen `R1`, `R2`, or `R3` as live implementation work;
- authorize new production edits;
- add a second executable interface;
- introduce a compatibility fallback or convenience shim; or
- widen beyond `URI-R2-C1`, `uri-r2-c1-only-v1`, `witness-replay/applyInstantiation-instbot-precondition`, and `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).

## Inherited Authoritative Audit

- `P1 = pass`
- `P2 = semantic-negative`
- `D1 = pass`
- `D2 = pass`
- `D3 = pass`
- `D4 = reopen-repair-track`
- `R1 = pass`
- `R2 = pass`
- `R3 = pass`

Authoritative `R1` through `R3` carry-forward records consumed by this gate:

- `R1`: `orchestrator/rounds/round-024/review-record.json` -> `attempt = 1`, `accepted + finalize`, `authoritative_result = pass`
- `R2`: `orchestrator/rounds/round-025/review-record.json` -> `attempt = 3`, `accepted + finalize`, `authoritative_result = pass`, `attempts_run = 3`, `max_attempts = 100`
- `R3`: `orchestrator/rounds/round-026/review-record.json` -> `attempt = 1`, `accepted + finalize`, `authoritative_result = pass`

Bounded facts carried forward from the accepted stage artifacts:

- `R1` reproduced the localized owner-boundary failure at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), including the old mismatch `InstBot expects ⊥, got: t9 -> t9`, without widening scope.
- `R2` finalized one bounded paper-faithful owner-lane repair at `applyInstantiation` / `InstBot`, within retry budget, without adding a second interface or fallback behavior.
- `R3` verified that the locked replay lane now succeeds for `URI-R2-C1` / `uri-r2-c1-only-v1`, the old mismatch no longer occurs on that replay path, strict non-replay `InstBot` failures still hold, and the round did not reopen a broader replay campaign.

Non-authoritative retry history remains background evidence only and is not reopened here as live work.

## Current Bounded Verification

Commands run in `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-027`:

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json` -> pass (`2:  "contract_version": 2,`; `13:  "retry": null`)
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass (`1. [done] R1`; `2. [done] R2`; `3. [done] R3`; `4. [pending] R4`)
- `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md` -> pass
- `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` -> pass
- `test -f orchestrator/retry-subloop.md` -> pass
- `python3 -m json.tool orchestrator/rounds/round-024/review-record.json >/dev/null` -> pass
- `python3 -m json.tool orchestrator/rounds/round-025/review-record.json >/dev/null` -> pass
- `python3 -m json.tool orchestrator/rounds/round-026/review-record.json >/dev/null` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md` -> pass
- `rg -n '"stage_id": "R1"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"' orchestrator/rounds/round-024/review-record.json` -> pass (`R1`, `accepted`, `finalize`, `pass`)
- `rg -n '"stage_id": "R2"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"|"attempts_run": 3|"max_attempts": 100' orchestrator/rounds/round-025/review-record.json` -> pass (`R2`, `accepted`, `finalize`, `pass`, `attempts_run = 3`, `max_attempts = 100`)
- `rg -n '"stage_id": "R3"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"authoritative_result": "pass"' orchestrator/rounds/round-026/review-record.json` -> pass (`R3`, `accepted`, `finalize`, `pass`)

Full Cabal gate skip note: The full Cabal gate was inherited from accepted `R3` and is out of scope for `R4`.

## Closed Decision Rule

Choose `repair-accepted` only if all of the following remain true together:

1. `R1` remains authoritative `pass` with localized reproduction continuity at the locked boundary.
2. `R2` remains authoritative `pass` on the bounded `applyInstantiation` / `InstBot` repair, finalized within retry budget, with no second interface or fallback widening.
3. `R3` remains authoritative `pass` proving locked replay success, removal of the old mismatch on the replay lane, preserved non-replay fail-closed behavior, and no broader replay widening.
4. The current bounded verification above passes without a missing artifact, inconsistent authoritative status, or round-local scope violation.

Otherwise choose `repair-blocked`.

Fail closed rules for this gate:

- if any prerequisite artifact is missing, record `repair-blocked`;
- if any authoritative record is not `accepted + finalize`, record `repair-blocked`;
- if any required verification token is absent or any bounded check fails, record `repair-blocked`;
- `repair-blocked` is a valid final bounded outcome for `R4`; it is not itself a reviewer rejection.

## Final Outcome

Final outcome token: `repair-accepted`

Decision summary:

- all authoritative `R1` through `R3` records remain `accepted + finalize` passes;
- the bounded `R2` repair remained within the locked owner lane and finalized within retry budget;
- `R3` already established replay-lane success and preserved strict non-replay `InstBot` fail-closed behavior;
- the current `R4` bounded verification found no missing artifact, inconsistent authoritative status, or scope violation.

## Files Changed By This Round

- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
- `orchestrator/rounds/round-027/implementation-notes.md`

## Reviewer Handoff

Reviewer-facing terminal semantics for `R4`:

- a correct `R4` attempt may finalize only as `accepted + finalize` with final outcome `repair-accepted` or `repair-blocked`;
- if review finds an aggregation defect or terminal-contract defect, the only legal retry path is `rejected + retry` back to `plan` in the same round;
- `accepted + retry` is forbidden for `R4`.
