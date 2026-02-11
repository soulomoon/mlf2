# Findings

## 2026-02-11
- `Bugs.md` initially had no open bugs; BUG-002/003/004 baseline repros were marked resolved.
- Variant placement:
  - Expression-level variants added to `test/ElaborationSpec.hs` in `describe "Systematic bug variants (2026-02-11 matrix)"`.
  - Witness-level variants added to `test/Presolution/WitnessSpec.hs` in `describe "Witness normalization invariants (US-010 regression)"`.
- Outcomes from variant execution:
  - Passing strict-success variants: `BUG-004-V1`, `BUG-004-V3`, `US-010-V1`, `US-010-V2` (guarded rejection path).
  - Newly discovered failing families (both unchecked and checked pipelines):
    - `BUG-002-V1..V4` (`PhiInvariantError` / `PhiTranslatabilityError` around non-binder graft/weaken paths)
    - `BUG-004-V2`, `BUG-004-V4` (`PhiReorder` identity gap and `InstBot` mismatch)
    - `BUG-003-V1..V2` (`OpGraft(non-binder)` `InstBot` invariant failures on higher-arity bounded aliasing)
- Bug tracker/doc sync completed:
  - Added open bugs `BUG-2026-02-11-002`, `BUG-2026-02-11-003`, `BUG-2026-02-11-004` to `/Volumes/src/mlf4/Bugs.md`.
  - Updated `/Volumes/src/mlf4/CHANGELOG.md` and `/Volumes/src/mlf4/TODO.md` with Task 10 and regression triage notes.
