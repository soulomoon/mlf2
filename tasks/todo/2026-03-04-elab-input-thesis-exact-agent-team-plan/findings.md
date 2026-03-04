# Findings — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Plan

## Scope
- Write an implementation plan (no code changes) for making TMT row `Elaboration input` thesis-exact.

## Current-state findings
- TMT row is currently `Thesis-exact = No` because strict criterion includes test-only paths and solved-typed legacy/test-debug elaboration/Phi surfaces remain.
- Existing plan docs for this topic exist, but this iteration needs a focused closeout plan against current blockers.

## Concrete blockers encoded in plan
1. Remove solved-typed compatibility aliases/entrypoints from `MLF.Elab.Elaborate`.
2. Remove solved-typed compatibility aliases/entrypoints from `MLF.Elab.Phi.Translate` and `MLF.Elab.Phi`.
3. Remove solved-typed callback shape from `MLF.Elab.Phi.TestOnly` helper contracts.
4. Migrate `test/ElaborationSpec.hs` callsites to chi-native callback contracts.
5. Flip TMT row to `Yes` only after guard/parity/full-suite gates pass.

## Plan artifact
- `docs/plans/2026-03-04-elab-input-thesis-exact-legacy-retirement-agent-team-implementation-plan.md`
