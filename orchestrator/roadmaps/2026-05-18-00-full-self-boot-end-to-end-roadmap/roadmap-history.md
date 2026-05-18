# Full Self-Boot End-to-End Roadmap History

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`

## rev-003

- Semantic update from round-259: future planner, implementer, and reviewer
  coordination now names the exact `tdd` skill path
  `/Users/ares/.agents/skills/tdd/SKILL.md` wherever behavior-changing
  implementation rounds depend on the TDD discipline.
- The update preserves rev-002 TDD semantics: behavior-changing
  implementation rounds still proceed through vertical RED -> GREEN ->
  refactor cycles with one public-interface behavior test first; pure
  docs-only, control-plane-only, review-only, semantic roadmap-update, and
  status-only closeout rounds remain exempt.
- The update carries forward the round-259 status-only closeout: milestone-2
  remains `in-progress` with the first conformance-corpus tracer completion
  pointer, and no milestone status, dependency, direction, stage order, or
  implementation scope changes.

## rev-002

- Semantic update from round-258: future behavior-changing implementation
  rounds must use the `tdd` skill and proceed through vertical RED -> GREEN ->
  refactor cycles. Pure docs-only, control-plane-only, review-only, semantic
  roadmap-update, and status-only closeout rounds are exempt from TDD.
- round-258 (`dcb12bdc`): completed milestone-1 as a docs-only readiness audit
  and alignment pass. The review approved the current-evidence and
  anti-self-hosting guardrails without claiming behavior implementation.
