# Task Plan — 2026-03-04 Elaboration Input Absolute Thesis-Exact Audit

## Goal
Re-audit the Transformation Mechanism Table row `Elaboration input` against `papers/these-finale-english.txt` and current code, then update the row and related docs so the thesis-exact classification is evidence-based and current.

## Scope
- Target doc: `docs/notes/2026-02-27-transformation-mechanism-table.md` (row `Elaboration input` only unless supporting metadata requires refresh).
- Sources: thesis text + active code paths in `src/MLF/Elab/*`, `src/MLF/Elab/Run/*`, and relevant tests.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Gather current doc + thesis evidence | complete | Located row and extracted Def. 15.3.12 + §15.3.6/Fig. 15.3.5 thesis contract. |
| 2. Audit code paths | complete | Verified production `χp` input path and test-only helper signatures. |
| 3. Update docs | complete | Patched TMT row + source revision; synced TODO/CHANGELOG/implementation notes and bug tracker. |
| 4. Verify and summarize | complete | Verified edited docs + evidence refs; prepared strict absolute verdict (docs-only update, no code tests rerun). |

## Error Log
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
