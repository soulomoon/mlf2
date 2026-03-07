# Findings

- Pending `geRes` usage audit.
- `GeneralizeEnv.geRes` was only read once, to feed `Solved.canonicalMap` into `presolutionViewFromSnapshot`; the environment did not otherwise depend on a full solved handle.
- `buildSolvedFromPresolutionView` existed solely to create that now-unnecessary `Solved` value, so the cleanup can preserve semantics by storing the sanitized canonical map directly via `stepSanitizeSnapshotUf constraint (pvCanonicalMap presolutionView)`.
- One explicit `GeneralizeEnv` fixture in `test/ElaborationSpec.hs` needed the new field, and `test/PresolutionSpec.hs` is the natural home for a direct source guard against reintroducing `geRes :: Solved`.
