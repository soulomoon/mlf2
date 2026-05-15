### Selected Extraction
- Milestone: Witness Smart Constructors
- Milestone id: milestone-5
- Direction id: direction-5a-witness-constructor-invariants
- Extracted item id: item-5a-instance-witness-validation-seam
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Make finalized production `InstanceWitness` values pass through a validation-owned construction seam, so consumer-ready witness ops carry the invariants already proven during normalization instead of being wrapped by a thin list constructor.

### Approach
Round-235 already froze the raw constructor boundary for `EdgeWitness` and `InstanceWitness`, but current HEAD still leaves `mkInstanceWitness = InstanceWitness` while `MLF.Constraint.Presolution.WitnessNorm` separately proves the replay-contract and normalized-op invariants consumers depend on. The next bounded serial slice is therefore to separate unchecked pre-normalization op accumulation from finalized production `InstanceWitness` construction, and tie the production constructor to the existing normalization proof lane. This advances milestone-5's remaining validation criterion without pulling in milestone-6 cleanup, broad fixture migration, or broad `Driver` / `Phi` validation removal. Downstream checks should move only if the new constructor proves the exact same invariant for the exact same finalized lane.

### Steps
1. Audit the live `InstanceWitness` construction lanes in `MLF.Constraint.Types.Witness`, `MLF.Constraint.Presolution.Witness`, and `MLF.Constraint.Presolution.WitnessNorm`, and split unchecked pre-normalization op accumulation from consumer-ready witness construction so the default smart-constructor seam no longer treats arbitrary `[InstanceOp]` as a finished production witness.
2. Define the production `mkInstanceWitness` contract around the invariants already established at normalization time, and route the post-normalization reassembly path through that contract instead of a thin list wrapper.
3. Keep any pre-normalization or negative-fixture raw construction on explicit owner-local or test-only seams, and update the touched presolution builders so unchecked witness ops do not leak back onto the default production surface.
4. Add focused regression coverage for the selected seam: a source guard that the default witness module still exposes only the approved constructor/accessor surface, a representative strict-replay normalization case, and a representative no-replay projection case showing finalized production witnesses are rebuilt only after the validation lane succeeds.
5. Audit one concrete downstream validation site closest to the finalized production lane and remove it only if the new `mkInstanceWitness` guarantee now proves the same invariant on the same post-normalization inputs; otherwise keep the downstream guard and document the still-live owner boundary accurately.
6. Run focused witness normalization and elaboration checks, then diff hygiene, then the full `cabal build all && cabal test` gate required for behavior-changing milestone-5 work.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalizeInstanceOpsFull produces validated witnesses when it succeeds"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization derives strict replay lane from edge semantics and maps codomain to edge-root replay binders"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "normalization prunes no-replay non-root raise wrappers before Phi"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Φ translation soundness"'`
- Manual source check that unchecked `[InstanceOp]` construction stays owner-local or test-only, `mkInstanceWitness` is no longer a thin list wrapper on the default production path, and any removed downstream check is exactly matched by the constructor guarantee
- `cabal build all && cabal test`
