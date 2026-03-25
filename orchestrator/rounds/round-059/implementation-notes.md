# Round `round-059` Implementation Notes

## Summary

This implementer attempt executed only roadmap item `J2` for repaired
`URI-R2-C1`.

`Fallback.hs` now defines the bounded
`rootLocalInstArgSingleBase` proof and consumes it through a dedicated
same-lane `targetC` arm. `PipelineSpec.hs` adds the matching
inst-arg-only singleton-base helper, local positive example, non-local
fail-closed contrast, and source-guard coverage inside the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block.

## Scope Preserved

- kept the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unchanged;
- preserved completed `rootLocalSingleBase`, preserved scheme-alias/base-like
  `baseTarget`, `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
  retained-target behavior, non-local fail-closed behavior, and replay paths;
- did not edit `orchestrator/rounds/round-059/state-snapshot.json` or `/Volumes/src/mlf4/Bugs.md`; and
- left the pre-existing round controller files untouched.

## Verification Posture

- the focused `ARI-C1` block is green with the bounded `J2` helper/example and
  source-guard additions;
- `git diff --check` passes on the finished four-file diff; and
- `cabal build all && cabal test` passes (`1140` examples, `0` failures).
