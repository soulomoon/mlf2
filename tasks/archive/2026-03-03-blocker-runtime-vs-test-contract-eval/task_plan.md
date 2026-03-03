# Task Plan: Blocker Runtime vs Test Contract Evaluation

## Metadata
- Date: 2026-03-03
- Owner: codex-main
- Scope: independent triage of migration guardrail blocker

## Goal
Determine whether the current blocker (canonical-map mismatch in migration guardrail) should be fixed in runtime code or in test contract, using thesis-faithful semantics as the decision criterion.

## Phases
1. [completed] Gather evidence from failing/guard tests and runtime canonical-map construction
2. [completed] Compare observed behavior to thesis-faithful semantic invariants and migration guard intent
3. [completed] Recommend fix location (runtime vs test) and specify exact assertion updates if test contract should change

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `cabal test` run in parallel with another same-target run failed with transient log unlink error (`removeLink ... mlf2-test.log`) | 1 | Re-ran sequentially; no code changes required and results reproduced consistently. |

## Outcome
- Decision: canonical-map blocker should be fixed in test contract, not by forcing runtime to preserve eliminated-node-only legacy canonical-map metadata.
- Rationale: thesis-core presolution view canonical-map is live-domain by construction; legacy replay map includes eliminated-node-only keys outside live-domain semantic queries.
- Implemented assertion update:
  - compare canonical maps on shared live-node domain,
  - keep strict canonical-constraint equality and solved-query parity checks.
- Follow-up runtime issue uncovered during verification (Phase 6 regressions) was fixed separately by restoring full snapshot-finalization semantics in shared runtime finalization (`Solve.finalizeConstraintWithUF`).
