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
- Avoid already accepted themes from prior rounds and any idea rejected in this round.
- Respect unrelated working-tree edits already present in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`.
- Choose retirement of redundant `MLF.Elab.Run.Scope.preferGenScope` as the round-5 simplification candidate because the live code comment already classifies it as a duplicate pass over `bindingScopeRef`, it has a single production caller, and removing it would tighten the ga′ implementation to the thesis-facing path already described in the same note.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Shell command failed with `zsh: unmatched '` while batching file reads. | 1 | Re-ran the command with safer double-quoted shell literals. |
| Search command failed with `zsh: unmatched '` while looking for scope-related markers. | 1 | Re-ran the search using double-quoted regex text. |
