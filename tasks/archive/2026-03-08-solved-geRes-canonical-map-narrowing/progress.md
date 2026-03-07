# Progress Log

## 2026-03-08
- Started the cleanup task to replace `GeneralizeEnv.geRes :: Solved` with the canonical map it actually uses.
- Replaced `GeneralizeEnv.geRes` with `geCanonicalMap`, removed `buildSolvedFromPresolutionView`, updated the one explicit test fixture, and added a presolution migration guard for the narrowing.
- Synced the cleanup into `docs/architecture.md`, `implementation_notes.md`, `CHANGELOG.md`, and `TODO.md` after the code/tests were green.
