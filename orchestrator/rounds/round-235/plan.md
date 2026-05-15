### Selected Extraction
- Milestone: Witness Smart Constructors
- Milestone id: milestone-5
- Direction id: direction-5a-witness-constructor-invariants
- Extracted item id: item-5a-production-constructor-boundary-freeze
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Freeze the approved production `EdgeWitness` and `InstanceWitness` construction boundary so well-formed witness values are built through validating smart-constructor seams, while preserving the intentionally malformed negative-test surface through an explicit test-only path.

### Approach
Milestone 5 is dependency-ready because it depends only on milestone 1, which is already done, and milestone 6 still depends on milestone 5. Current HEAD also shows a bounded split point that matches the roadmap's extraction notes: production witness creation is already concentrated in a few presolution-owned sites (`MLF.Constraint.Presolution.Witness.buildEdgeWitness` and normalized witness reassembly in `MLF.Constraint.Presolution.WitnessNorm`), while tests still construct many raw malformed witness fixtures directly to prove downstream fail-fast behavior. The default export surface therefore overstates the current guarantee: `docs/architecture.md` claims witness smart constructors validate well-formedness at construction time, but `mkInstanceWitness` is still a plain wrapper and `mkEdgeWitness` only rejects negative intro counts. Round-235 should treat that as a production-boundary implementation slice, not a full milestone closeout: make the production construction seam authoritative, keep malformed fixtures available through an explicit test-only seam, and defer broad fixture migration plus downstream-check deletion unless the new constructor guarantee demonstrably subsumes those later checks.

### Steps
1. Audit `MLF.Constraint.Types.Witness` against its real consumers and narrow the default production construction surface to the approved smart-constructor/accessor seam, so production modules stop depending on open raw constructors while still keeping the existing read-side access they need.
2. Introduce or refine an explicit raw witness test seam for negative fixtures, then repoint the affected witness-domain and elaboration tests to that seam instead of relying on the default production export surface for malformed `EdgeWitness` / `InstanceWitness` literals.
3. Route every production witness creation or reassembly path through the approved constructor boundary, especially `MLF.Constraint.Presolution.Witness.buildEdgeWitness` and the normalized witness update in `MLF.Constraint.Presolution.WitnessNorm`, and lift only the invariants that those construction sites can actually guarantee at build time.
4. Keep `MLF.Constraint.Presolution.WitnessValidation` and elaboration/Phi fail-fast checks for any malformed cases that are still reachable outside the new guarantee. Remove a downstream validation only when the constructor boundary now proves the exact same invariant for the exact same input lane.
5. Add focused regression coverage for the selected boundary: source/export guards that prevent raw production construction from reappearing, preservation coverage that proves live presolution witness assembly still succeeds through the approved seam, and negative coverage showing malformed fixture paths remain explicit and fail closed rather than being weakened into smoke tests.
6. Update durable guidance only where the accepted boundary changes materially, then run focused witness/elaboration checks, diff hygiene, and the full `cabal build all && cabal test` gate required for behavior-changing milestone-5 work.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 3 — Witness normalization"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`
- Manual source check that the default `MLF.Constraint.Types.Witness` surface no longer permits raw production witness construction, that production witness creation/reassembly flows through the approved seam, and that malformed witness regressions still exist through an explicit test-only path
- `cabal build all && cabal test`
