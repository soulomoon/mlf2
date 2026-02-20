# Findings

- Phase 7 APIs needed by the new properties are already exported from MLF.Elab.Pipeline: typeCheck, step, isValue.
- Existing theorem baseline gate matcher remains in place and should be kept.
- Coverage-based properties need generator balance:
  - with the initial term set, value coverage stayed below 35% and failed `checkCoverage`.
  - adding extra value constructors fixed gate stability without weakening assertions.
