# Task Plan: Update Elaboration Input Row (Thesis vs Codebase)

## Goal
Update the `Elaboration input` row in the `Transformation Mechanism Table (Thesis vs Codebase)` to accurately reflect both thesis intent and current implementation.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Locate target table and current row text | complete | Target row confirmed in `docs/notes/2026-02-27-transformation-mechanism-table.md`. |
| 2. Audit thesis source for elaboration input definition | complete | Re-audited Def. 15.3.12 and §15.3.6 in `papers/these-finale-english.txt:14087-14097`, `:14112-14117`. |
| 3. Audit codebase elaboration pipeline input shape | complete | Verified active `χp` flow and remaining solved-typed test-only helper surfaces. |
| 4. Update table row text with aligned statement | complete | Reworded row with refreshed thesis/code references; retained strict classification. |
| 5. Verify diff and summarize | complete | Checked single-row diff content and reference ranges. |

## Decisions
- Kept `Thesis-exact = No` for `Elaboration input` because strict criterion includes test-only paths and `MLF.Elab.Phi.TestOnly` still exports solved-typed helper signatures.
- Updated row wording to emphasize active runtime `χp`-native flow while explicitly documenting the remaining solved-typed compatibility surface.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None yet | - | - |
