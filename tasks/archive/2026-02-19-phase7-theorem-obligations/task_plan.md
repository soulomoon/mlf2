# Task Plan: Phase 7 theorem obligations

## Goal
Implement executable preservation/progress proxy properties in TypeSoundnessSpec and wire them into the thesis gate.

## Phases
- [completed] Phase 1: Implement test module + wiring
- [completed] Phase 2: Gate/doc/tracker sync
- [completed] Phase 3: Verify and archive

## Decisions
- Use ElabTerm-only generator for Phase 7 unit-level properties.
- Add gate anchor without removing existing theorem baseline matcher.

## Errors Encountered
- Initial gate run failed due QuickCheck coverage threshold mismatch:
  - observed value coverage ~33.086% with requirement 35%.
  - resolution: added additional value terms (`idBool`, `ETyAbs "b" Nothing boolLit`) in generator to satisfy coverage target.
