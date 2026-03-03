# Findings

## 2026-03-03
- Reproduced blocker: `migration guardrail: thesis-core boundary matches legacy outcome` failed on canonical-map mismatch (`legacy` populated vs thesis-core empty).
- Artifact comparison on representative corpus showed:
  - thesis-core `PresolutionView` canonical map is live-domain only;
  - legacy replay canonical map entries were eliminated-node-only (keys not present in live pre-rewrite constraint node set);
  - canonical constraints matched despite map-shape mismatch.
- Decision criterion (thesis-faithful semantics):
  - semantic parity should be evaluated on live-node query domain and canonical-constraint/query behavior, not on eliminated-node bookkeeping artifacts.
- Resolution chosen:
  - test contract update in `test/PipelineSpec.hs` to project canonical-map comparison onto shared live-node domain;
  - retained strict canonical-constraint and solved-query parity assertions.
- Additional runtime compatibility finding:
  - replay-free finalization initially missed eliminated-binder rewrite/substitution and bind-parent pruning steps, causing Phase 6 regressions.
  - fixed by introducing shared `Solve.finalizeConstraintWithUF` and routing `MLF.Constraint.Finalize` through it.
