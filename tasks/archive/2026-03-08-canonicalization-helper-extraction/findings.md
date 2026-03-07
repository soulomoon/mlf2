# Findings

- `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` still duplicate `buildCanonicalMap`, `chaseUfCanonical`, `equivCanonical`, and `nodeIdKey` almost verbatim.
- Existing behavior/semantic coverage already exists through `Constraint.SolvedSpec`, `CanonicalizerSpec`, and the `PresolutionView mirrors solved...` tests.
- The refactor should land in one internal utility home rather than extending the public boundary surface.
- `MLF.Constraint.Canonicalizer` was not the right home because its exported story is redirect/UF canonicalization as a first-class `Canonicalizer`, whereas the duplicated helpers are low-level solved/view reconstruction utilities.
