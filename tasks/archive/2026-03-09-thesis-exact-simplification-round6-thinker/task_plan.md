# Task Plan

## Goal
Propose exactly one bounded simplification still needed in the current repository, preserving thesis-exact behavior.

## Phases
- [complete] Initialize research notes
- [complete] Re-check repo state and guidance surfaces
- [complete] Inspect code for bounded simplification candidates
- [complete] Select one thesis-safe proposal
- [complete] Return schema-only result

## Decisions
- Follow `papers/these-finale-english.txt` as the source of truth; consult `papers/xmlf.txt` only if the thesis is silent.
- Avoid already accepted round themes: solved-helper quarantine removal, `MLF.Reify.Core` `solvedFromView` removal, `WithCanonicalT` retirement, `rtvSchemeBodyTarget` retirement, and edge witness/trace canonicalization.
- Respect unrelated working-tree edits already present in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`.
- Re-check live code and docs rather than reusing an older thinker result unchanged.
- Select retirement of redundant `MLF.Elab.Run.Scope.preferGenScope` as the round-6 proposal because the live code comment already classifies it as duplicate work, the helper has one production caller, and removing it would tighten the ga′ implementation to the single thesis-aligned lookup already performed by `bindingScopeRef`.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
