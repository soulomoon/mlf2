# Round 3 Verifier Plan: pending-weaken owner query relocation

## Goal
Decide whether moving pending-weaken owner query helpers out of `MLF.Constraint.Presolution.EdgeUnify` is still needed now, using thesis-first verification plus current repository state.

## Phases
- [in_progress] Gather current docs, backlog, and repository context
- [pending] Inspect code ownership/export surface and actual call sites
- [pending] Cross-check thesis constraints on weaken/raise/merge behavior
- [pending] Synthesize verdict with thesis-safety assessment

## Decisions
- Treat this as a verification-only task unless stale guidance is discovered.
- Use `papers/these-finale-english.txt` as primary source and `papers/xmlf.txt` only if the thesis is silent.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| None yet | 0 | n/a |

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `cabal test ... --match "row3 absolute thesis-exact guard|presolution internal export surface guard|row3 ordering thesis-exact guard|Phase 4 thesis-exact unification closure"` failed during test-suite build because `Solved.fromPreRewriteState` is no longer exported but `test/Parity/FrozenArtifacts.hs` and `test/SpecUtil.hs` still import it. | 1 | Treat as unrelated workspace/build break for this verifier pass; do not modify it. Continue with source/thesis inspection evidence. |
