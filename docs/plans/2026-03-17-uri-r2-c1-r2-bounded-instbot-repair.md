# `R2` Bounded `InstBot` Repair For `URI-R2-C1`

Date: 2026-03-17
Roadmap item: `R2`
Stage: `implement`
Attempt: 3
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

## Attempt 2 Repair Retained Unchanged

Attempt 3 kept the accepted bounded `InstBot` repair from attempt 2 byte-for-byte in the
owner lane:

- `evalInstantiationWith` keeps the replay-context threading that lets later `InstBot`
  checks reuse substitutions already chosen on the same replay path.
- `applyInstantiation` still accepts a non-bottom `InstBot` only when replay-context
  substitution changes the argument and makes it alpha-equivalent to the current explicit
  bound.
- The adjacent plumbing-only edits in `src/MLF/Elab/TypeCheck.hs` and the focused
  regression checks in `test/ElaborationSpec.hs` were left unchanged.

No production or research-module code changed in attempt 3.

## Attempt 3 Contract Scope

Attempt 3 chose `contract-scope` for the inherited prototype blockers.

The blocker was not a new production mismatch. It was that
`test/Research/UriR2C1PrototypeP1Spec.hs` still treated live post-repair reruns as if they
had to reproduce the earlier prototype-era diagnostics verbatim. After the bounded `R2`
repair:

- live `P2-W` remains `semantic-negative`, but now reports
  `replay-domain-widening` with
  `witness replay produced t9 -> t9 but reification preserved t5 -> t5`;
- live `D1`, `D2`, and `D3` reruns become `semantic-negative` because they are checking
  continuity against immutable predecessor expectations (`partial-replay`, `D1 = pass`,
  `D2 = pass`, `D3 = pass`) that remain historical authority rather than current replay
  outputs.

Attempt 3 therefore updated only the prototype spec to make that boundary explicit:

- the four inherited examples now assert the live post-repair outputs;
- the same examples also keep the predecessor contract visible by checking seeded
  authoritative review-record fields or the continuity messages that still name the
  historical `partial-replay` / `pass` targets;
- predecessor rounds, prior retry reviews, and `attempt-log.jsonl` remained read-only.

## Files Changed

- `test/Research/UriR2C1PrototypeP1Spec.hs`
- `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
- `orchestrator/rounds/round-025/implementation-notes.md`

## Validation

Focused `R2` owner-lane checks:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'` -> pass (`1 example, 0 failures`)

Previously failing prototype checks:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "allows attempt-2 reruns and records live replay widening as bounded non-pass"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D1 attempt-1 via the root-cause tuple and records continuity drift against the historical replay boundary"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D2 attempt-1 via the root-cause tuple and reports continuity-blocked localization on live reruns"'` -> pass (`1 example, 0 failures`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D3 attempt-1 via the root-cause tuple and reports bounded-negative continuity drift on live reruns"'` -> pass (`1 example, 0 failures`)

Full required gate:

- `cabal build all && cabal test` -> pass (`1124 examples, 0 failures`)
