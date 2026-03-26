# Findings

## 2026-03-26

- The current shared `run-orchestrator-loop` skill already says delegated
  stage failure should use `recovery-investigator`, but it does not state
  strongly enough that every non-terminal delegated-stage stop must attempt
  it before the controller can stop.
- The recent `round-099` failure is a useful pressure scenario: the guider
  stage produced no `selection.md`, and the stop rationale turned on whether
  `recovery-investigator` had been lawfully attempted or proven
  deterministically unavailable.
- The most important documents to update are:
  - `SKILL.md`
  - `references/resume-rules.md`
  - `references/delegation-boundaries.md`
  - `references/recovery-investigator.md`
- The `state-machine.md` and `worktree-merge-rules.md` references do not
  need behavioral changes for this issue.
- The final wording now makes three things explicit:
  - delegated-stage failure is not a lawful stop just because the first role
    launch returned unusable output;
  - `recovery-investigator` is the default first recovery action for
    non-terminal delegated-stage stops; and
  - direct blockage is lawful only after an attempted
    `recovery-investigator` fails to yield a qualifying path, or after a
    deterministic no-launch reason is recorded.
