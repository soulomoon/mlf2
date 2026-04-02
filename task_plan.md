# Task Plan

## Summary
Goal: Review `docs/superpowers/specs/2026-04-02-scaffold-orchestrator-loop-dual-mode-update-design.md` and write an implementation plan for updating `scaffold-orchestrator-loop` so it supports both bootstrap and terminal-control-plane next-family setup without starting runtime rounds.

## Phases
1. Review the design note, current scaffold skill docs, and the live repo-local orchestrator contract. - complete
2. Identify the actual implementation write-surface, state/pointer assumptions, and verification approach. - complete
3. Write the implementation plan under `docs/superpowers/plans/` and refresh persistent planning notes for this task. - complete

## Errors Encountered
| Error | Attempt | Resolution |
|------|---------|------------|
| Shallow asset listing hid nested scaffold files | 1 | Re-ran `find` without the depth limit and inspected the full asset tree before drafting the plan |
| Tried to read non-existent top-level pointer stub templates from the skill assets | 1 | Confirmed the current scaffold assets do not ship pointer stubs and treated them as optional compatibility files in the plan |
| First plan draft missed the `retry` schema drift | 1 | Re-checked the scaffold asset and state-schema files, then expanded the saved plan to include state-contract alignment work |
