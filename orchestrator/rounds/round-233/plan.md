### Selected Extraction
- Milestone: ForallSpec Binder Safety
- Milestone id: milestone-4
- Direction id: direction-4a-forallspec-binder-safety
- Extracted item id: item-4a-phi-spine-index-totalization
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Close the remaining milestone-4 safety gap by totalizing the selected Phi binder-spine indexing path while keeping the accepted `ForallSpec` list shape and `fsBounds`-derived binder count unchanged.

### Approach
Current HEAD already satisfies part of milestone-4: `ForallSpec` no longer stores a redundant binder-count field, and binder count is derived from `fsBounds` via `forallSpecBinderCount`. The binding/presolution side also remaps binder references through lookup-based logic rather than raw array indexing. The live gap is in the selected Phi path: `MLF.Elab.Phi.VSpine` still exposes partial index accessors, and `MLF.Elab.Phi.Omega.Interpret.Internal` still reads binder names, bounds, and ids through `!!`-based indexing during reorder and raise handling. This round should treat that as implementation work, not closeout: keep the current list-based `ForallSpec` representation, replace the partial binder-spine reads with explicit checked access, and add regression coverage for binder-alignment failures without broadening into milestone-5 witness-constructor work.

### Steps
1. Introduce a total binder-spine read boundary in `MLF.Elab.Phi.VSpine` (or an equally local helper seam) so binder name, bound, and id lookups used by production Phi translation report explicit `ElabError`/`PhiInvariantError` instead of relying on partial `!!`.
2. Update `MLF.Elab.Phi.Omega.Interpret.Internal` to use the checked spine-access path in the selected reorder, binder-name, and raise logic, preserving current semantics when indices are valid and failing explicitly when the witness/domain spine is inconsistent.
3. Retire direct partial spine indexing from the touched production call sites so future binder-path edits cannot silently reintroduce the same `!!`-based risk in the selected translation path.
4. Add focused regression coverage in `test/ElaborationSpec.hs` and/or `test/Phi/WitnessDomainSpec.hs` for binder-spine mismatch and out-of-range cases, plus a preservation case showing the valid binder-alignment behavior still succeeds after the totalization.
5. Update only the touched durable guidance if the accepted local contract changes materially; do not claim milestone-4 closeout from this slice alone, and do not introduce a heavier type-level collection dependency or public-surface change.
6. Run focused Phi/Witness-domain validation first, then diff hygiene, then the full `cabal build all && cabal test` gate required for behavior-changing milestone-4 work.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- Focused Hspec coverage for the touched Phi path, including `WitnessDomain`, binder-targeted `OpWeaken` / `OpGraft` / `OpRaise`, and any new binder-spine mismatch cases added in `test/ElaborationSpec.hs` or `test/Phi/WitnessDomainSpec.hs`
- Manual source check that production `MLF.Elab.Phi.*` no longer uses partial binder-array indexing in the selected path and that `ForallSpec` binder count still derives from `fsBounds`
- `cabal build all && cabal test`
