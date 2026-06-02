# Full Self-Boot End-to-End Roadmap History

Roadmap family: `2026-05-18-00-full-self-boot-end-to-end-roadmap`

## rev-005

- Semantic update from round-324: future checker, checker-parity,
  compiler-source-package, driver, and conformance-validation rounds should
  prefer one aggregate public program run with labelled per-case evidence when
  cases can share checker/program setup context.
- Multiple checker-facing invocations remain lawful when semantic isolation,
  diagnostic boundaries, failure independence, stage-owned output isolation, or
  the public interface under test requires separate runs.
- The update preserves milestone ordering, proof oracle meaning,
  trusted-substrate boundaries, public-interface tests, committed expected
  outputs, and self-boot completion criteria. It adds planning/review
  coordination only, not checker implementation or optimization scope.

## rev-004

- Activated the integrated Broad String Library completion contract and marked
  milestone 3 complete after the whole-library round proved the native-capable
  text substrate.
- Carried parser-parity progress from rounds 304-308 into milestone 4 as
  fixture-scoped tracers for basic modules, imports, value definitions,
  let/lambda/application expressions, and typed annotations.
- Operator-directed correction on 2026-05-21: after observing that milestone 4
  had drifted into one parser package per fixture, the next normal milestone-4
  implementation selection after round-309 recovery/closeout must be
  `item-310-parser-library-consolidation`. Future parser rounds must grow one
  shared parser-owned `.mlfp` parser-combinator library with explicit parser
  monad sequencing and route fixtures through it rather than adding another
  exact-source fixture parser.

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
