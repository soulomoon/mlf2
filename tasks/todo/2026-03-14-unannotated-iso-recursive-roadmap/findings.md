# Findings

## 2026-03-14

- The completed successor orchestrator explicitly did not deliver full automatic recursive-type inference.
- The accepted result is bounded `ARI-C1`: annotation-anchored recursive-shape handling only, with explicit exclusions for fully unannotated synthesis, equi-recursive equality, and cyclic graph encoding.
- The user wants a new staged roadmap that moves from `ARI-C1` toward true automatic recursive-type inference.
- The user wants the first milestone after `ARI-C1` to remain research-only: design the path toward fully unannotated inference before any implementation milestone.
- The roadmap should terminate at a research-backed implementation handoff for true unannotated iso-recursive inference, not at an implementation attempt.
- The preferred roadmap shape is the conservative ladder: widen one boundary at a time from `ARI-C1` rather than jumping directly to broad unannotated inference.
- The user wants three bridge stages between `ARI-C1` and the final unannotated-inference handoff target.
- The final handoff target should be limited to `single-SCC` unannotated iso-recursive inference; `multi-SCC` remains out of scope for this roadmap.
- The user approved the proposed roadmap skeleton: `R1` gap map, `R2` inference obligations, `R3` verifier-facing feasibility contract, `R4` implementation-handoff spec.
- The user accepted the fixed boundary model: iso-recursive only, non-cyclic-graph, fail-closed, no silent widening, and single-SCC only.
- The user accepted the milestone breakdown as decision-artifact stages rather than code stages.
- The written spec was saved to `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`.
