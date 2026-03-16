# `R1` Repair-Boundary Reproduction Contract For `URI-R2-C1`

Date: 2026-03-17
Roadmap item: `R1`
Stage: `implement`
Attempt: 1
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Controlling bug: `BUG-2026-03-16-001`
Artifact kind: implementation-facing reproduction contract

## Inherited Authoritative Inputs

- `P1 = pass` via `orchestrator/rounds/round-016/review-record.json`
- `P2 = semantic-negative` via `orchestrator/rounds/round-017/review-record.json`
- `D1 = pass` via `orchestrator/rounds/round-020/review-record.json`
- `D2 = pass` via `orchestrator/rounds/round-021/review-record.json`
- `D3 = pass` via `orchestrator/rounds/round-022/review-record.json`
- `D4 = reopen-repair-track` via `orchestrator/rounds/round-023/review-record.json`
- `BUG-2026-03-16-001` in `Bugs.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `orchestrator/rounds/round-024/selection.md`
- `orchestrator/rounds/round-024/plan.md`

## Locked Boundaries

- Subject stays `URI-R2-C1` only.
- Scenario stays `uri-r2-c1-only-v1` only.
- Divergence boundary stays `witness-replay/applyInstantiation-instbot-precondition`.
- Owner boundary stays `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- This round adds no production repair, no second executable interface, no compatibility fallback, and no widened replay search.

## Implementation-Facing Reproducer

Reviewer-auditable reproducer:

- Test: `Phase 6 — Elaborate (xMLF) / xMLF instantiation semantics (applyInstantiation) / URI-R2-C1 witness replay reproduces BUG-2026-03-16-001 at applyInstantiation InstBot`
- File: `test/ElaborationSpec.hs`
- Helper: `uriR2C1ReplayFixture`

The helper reconstructs the locked scenario directly through existing production code:

1. Start from the bounded fixture expression `(\x -> x) : forall a. a -> a`.
2. Run the live constraint/presolution/result-type path to obtain the generalized scheme and canonical annotation edge.
3. Reify the no-fallback type for that edge, yielding the accepted replay-lane shape `t5 -> t5`.
4. Replay the live witness through `phiFromEdgeWitnessWithTrace`, yielding the authoritative instantiation `∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N)`.
5. Apply that instantiation with `MLF.Elab.Inst.applyInstantiation`.

## Observed Failure Signature

The bounded reproducer currently observes:

- `schemeToType` at the owner-facing call site: `∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b`
- no-fallback replay-lane type: `t5 -> t5`
- witness replay instantiation: `∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N)`
- owner-boundary failure: `InstBot expects ⊥, got: t9 -> t9`

This proves the localized failure belongs to `applyInstantiation` after witness replay has already succeeded in producing a concrete instantiation and after no-fallback reification has already established the accepted replay-lane shape.

## Stage Result

`R1` is reproduced in implementation-facing terms at the locked owner boundary. The bounded handoff for later work is:

- `R2` may attempt one bounded production repair at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch) only.
- No broader replay rewrite, fallback path, alternate executable interface, or scope widening is authorized by this artifact.

## Validation

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay reproduces BUG-2026-03-16-001 at applyInstantiation InstBot"'`
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `cabal build all && cabal test`
