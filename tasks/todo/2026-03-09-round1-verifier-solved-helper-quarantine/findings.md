# Findings — Round 1 verifier

## Key Discoveries
- Pending.
- Repo-wide search currently shows `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement` are referenced from tests only, via `test/SolvedFacadeTestUtil.hs` and test specs.
- Initial thesis grep found many references to solved constraints/edges, but nothing suggesting these helper names or a paper-level concept for this post-solve inspection bundle.
- `src/MLF/Constraint/Solved/Internal.hs` still exports and defines the six helper functions verbatim, while `test/SolvedFacadeTestUtil.hs` provides the same test-facing surface using the public solved API.
- The test utility implementation is independent of `MLF.Constraint.Solved.Internal`; it rebuilds the helpers via `MLF.Constraint.Solved`, `MLF.Constraint.Finalize`, and `MLF.Constraint.NodeAccess`.
- `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` all describe Task 67 as having moved the remaining solved test/audit helper bundle behind `test/SolvedFacadeTestUtil.hs`; the current internal duplicate surface means those docs are only partially true.
- Current non-test imports of `MLF.Constraint.Solved.Internal` are limited to `MLF.Constraint.Finalize` and `MLF.Reify.Core`; these imports need checking for actual helper-name usage.
- Exact repo grep still finds the six helper names under both `test/SolvedFacadeTestUtil.hs` and `src/MLF/Constraint/Solved/Internal.hs`, so the quarantine is incomplete in the current tree.
- Current non-test code imports `MLF.Constraint.Solved.Internal` only for production builders/pruning (`fromConstraintAndUf`, `rebuildWithConstraint`, `pruneBindParentsSolved`), not for the six test-only helper names.
- Narrow validation confirms the current public-facade guard passes (`test-only helper bundle is absent from the Solved facade`: `1 example, 0 failures`) and the full solved spec passes (`MLF.Constraint.Solved`: `51 examples, 0 failures`), so today’s tests do not catch the internal duplicate bundle.
- Thesis evidence: `papers/these-finale-english.txt` frames a presolution as retaining all nodes of the original constraint, but is silent on repository-specific solved-view helper APIs; the duplicate helper bundle is therefore an implementation/testing convenience, not a thesis algorithm requirement.
