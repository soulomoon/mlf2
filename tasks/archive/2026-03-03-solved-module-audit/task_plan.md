# Task Plan: Solved Module Audit

## Goal
Perform solved-module audit for production-vs-compat/test boundaries and thesis-faithfulness touchpoints.

## Phases
- [x] Phase 1: Inventory solved-related modules and classify production vs compat/test usage. (status: completed)
- [x] Phase 2: Trace call chains where production code still depends on solved-boundary assumptions. (status: completed)
- [x] Phase 3: Map thesis-faithfulness touchpoints and identify mismatch/deviation candidates. (status: completed)
- [x] Phase 4: Prioritize migration actions, risks, and guard-test requirements. (status: completed)
- [x] Phase 5: Produce audit summary with recommended ownership split and verification commands. (status: completed)
- [x] Phase 6: Final validation pass and task handoff packaging. (status: completed)

## Decisions
- Audit execution used an agent team split by independent domains: inventory/classification, call-chain tracing, and thesis-touchpoint mapping.
- `src-public/` and `app/` were treated as hard boundary checks; no solved-import violations were found.
- Recommendations prioritize behavior-preserving boundary cleanup (adapter dedup, runtime `mkTestSolved` removal, mapping/fallback tightening) ahead of larger architectural extraction.
- This task is documentation/audit scoped; no runtime code behavior changes are included in this iteration.

## Verification Commands
- `cabal build all && cabal test`

## Handoff Notes
- Primary audit deliverable: `tasks/archive/2026-03-03-solved-module-audit/findings.md`
- Rolling priorities updated in root `TODO.md`.
- Task folder moved to `tasks/archive/2026-03-03-solved-module-audit/` on 2026-03-03.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| None | 0 | N/A |
