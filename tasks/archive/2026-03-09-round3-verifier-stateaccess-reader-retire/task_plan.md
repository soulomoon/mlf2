# Round 3 Verifier Plan: retire `WithCanonicalT` reader layer

## Goal
Decide whether retiring the `WithCanonicalT` reader layer from `MLF.Constraint.Presolution.StateAccess` is still needed now, using thesis-first verification plus current repository state.

## Phases
- [completed] Gather current docs, backlog, and repository context
- [completed] Inspect code ownership/export surface and actual call sites
- [completed] Cross-check thesis constraints on rigid/locked-node behavior
- [completed] Synthesize verdict with boundedness and thesis-safety assessment

## Decisions
- Treat this as a verification-only task unless stale guidance is discovered.
- Use `papers/these-finale-english.txt` as primary source and `papers/xmlf.txt` only if the thesis is silent.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| `cabal test ... --match "row3 absolute thesis-exact guard"` failed during test-suite build because `Solved.fromPreRewriteState` is no longer exported but `test/Parity/FrozenArtifacts.hs` and `test/SpecUtil.hs` still import it. | 1 | Treat as unrelated workspace/build break for this verifier pass; do not modify it. Continue with source/thesis inspection evidence. |
