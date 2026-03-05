# Findings

- Current characterization in test/PipelineSpec.hs shows edge 0 preserving `InstanceWitness [OpWeaken (NodeId 3)]` under `ReplayContractStrict` with empty replay artifacts.
- `c1` generalizes monomorphically, so the surviving non-root weaken has no live quantifier spine to act on.
- Thesis-exact fix is producer-side dead-op elimination after final replay/scheme domains are known, not Omega fallback.
- The first pruning rule based only on finalized source/replay binder domains was too broad: it also removed the strict non-root `OpWeaken` in `BUG-002-V4`.
- The useful discriminator is the post-bound-shape of the weaken target:
  - `let-c1-apply-bool` reaches a concretized bound skeleton (`Int` leaf reachable from the target bound chain), so the surviving weaken is stale residue.
  - `BUG-002-V4` remains fully abstract under lambda, so the same-shaped strict non-root weaken is still semantically live and must be preserved.
- Final `WitnessNorm` rule:
  - keep non-root `OpWeaken` if it is root-local, still in finalized source/replay binder domains, or its bound skeleton is still fully abstract;
  - otherwise prune it before Phi.
- After the refined rule:
  - `let-c1-apply-bool` elaborates to `Int` in checked and unchecked pipelines;
  - `BUG-002-V4` still preserves the strict non-root `OpWeaken`;
  - full suite passes (`956 examples, 0 failures`).
