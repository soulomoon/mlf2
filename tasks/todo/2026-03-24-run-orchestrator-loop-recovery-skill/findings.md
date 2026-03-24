# Findings

- The shared `run-orchestrator-loop` skill has no explicit recovery model
  between “delegated stage completed” and “terminal blockage.”
- The live `round-078` failure was incidental delegation non-observability, not
  a substantive guider outcome: the controller opened the round, but no
  `selection.md` appeared.
- The user-approved direction is broad controller recovery discretion with a
  dedicated `recovery-investigator` real subagent.
- The redesign must remain in the shared skill only; repo-local orchestrator
  contracts should remain unchanged.
- The controller still must not impersonate guider/planner/implementer/
  reviewer/merger by writing their substantive artifacts directly.
- The first spec-review pass exposed two concrete planning gaps that needed to
  be fixed in the spec: where the shared `recovery-investigator` role lives,
  and how blockage is recorded when no qualifying real subagent can be launched
  for recovery investigation itself.
- The implementation surface is small and explicit: `SKILL.md`,
  `references/delegation-boundaries.md`, `references/resume-rules.md`, plus a
  new `references/recovery-investigator.md`.
- Grep- and Python-assertion-based red/green checks are sufficient for this
  documentation-only implementation; no Cabal or repo-code verification is
  implicated by the approved scope.
- The first plan-review pass exposed three concrete implementation gaps:
  under-specified `recovery-investigator` inputs/outputs, missing mechanism
  switching and exhaustion semantics in `SKILL.md`, and incomplete verification
  of the full same-round / same-branch / same-worktree / same-stage resume
  invariant.
- The second plan-review pass exposed two more spec-alignment gaps:
  the plan needed to preserve `current_task` and retry attempt during recovery,
  and it needed an explicit observability re-check before the controller may
  leave recovery.
- The third plan-review pass found one remaining boundary gap: the plan had to
  make `recovery-investigator.md` explicitly forbid guider/planner/
  implementer/reviewer/merger substantive work, including acting as the stage
  reviewer.
- Executing the plan in an isolated worktree was necessary because the source
  checkout already had unrelated edits; the clean implementation branch is
  `codex/run-orchestrator-loop-recovery`.
- Task-level reviews found three real documentation-contract ambiguities that
  the initial implementation still left behind:
  the `SKILL.md` overview stop condition was too broad, the delegation-boundary
  doc still implied the controller might diagnose recovery failures directly,
  and the direct-blockage rule was not phrased consistently across all
  references.
- The final whole-implementation review found two additional shared-skill risks
  that were not covered by the plan’s original verification bundle:
  some no-investigator failure paths still did not explicitly record the precise
  blockage before telling the user, and `recovery-investigator.md` still needed
  explicit bans on controller-owned writes and repair actions.
- After the final fixes, the four-file shared-skill surface is aligned on the
  critical recovery semantics:
  direct blockage requires that no qualifying `recovery-investigator` can launch
  through any available delegation mechanism, the controller records precise
  blockage in `orchestrator/state.json` before informing the user, and the
  `recovery-investigator` is diagnosis/recommendation-only rather than a
  fallback worker.
- The implementation in `/Users/ares/.config/superpowers/worktrees/orchestratorpattern/run-orchestrator-loop-recovery`
  stays within the approved four-file scope and passes the plan’s final
  grep/assertion checks plus `git diff --check`.
- The follow-up implementation at head `3ddeb19646a9c7bcbf8314e881606bb6da6c9d32`
  resolves the two previously reported issues:
  `SKILL.md`, `delegation-boundaries.md`, and `resume-rules.md` now require
  recording precise blockage in `orchestrator/state.json` before informing the
  user when no qualifying `recovery-investigator` can launch, and
  `recovery-investigator.md` now states that the role is diagnosis/
  recommendation-only and explicitly forbids controller-owned writes or repair
  actions.
- Re-running the final plan assertion bundle and `git diff --check` against the
  new head passed cleanly, and no new scope or cross-file consistency issues
  were found in the approved four-file review surface.
