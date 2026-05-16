### Selected Extraction
- Milestone: Prepared Generalization Artifact Depth
- Milestone id: `milestone-5`
- Direction id: `direction-5a-prepared-generalization-artifact-depth`
- Extracted item id: `item-243-prepared-generalization-capability-adapters`
- Roadmap id: `2026-05-16-00-architecture-deepening-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-16-00-architecture-deepening-roadmap/rev-001`

### Goal
Make `MLF.Elab.Run.Generalize.Prepare` the owner-facing API for the normal pipeline's **Prepared Generalization Artifact** consumption. After this round, `MLF.Elab.Run.Pipeline` should ask the prepared artifact for elaboration inputs, root-scheme generalization, and result-type reconstruction instead of reading raw `pga*` fields for presolution views, bind-parent projections, redirects, canonicalization, edge artifacts, scope overrides, or result-type inputs.

### Approach
Current HEAD evidence:

- `src/MLF/Elab/Run/Generalize/Prepare.hs` exports `PreparedGeneralizationArtifact(..)` and fields including `pgaPresolutionView`, `pgaBindParentsGa`, `pgaGeneralizeAt`, `pgaResultTypeInputs`, `pgaEdgeArtifacts`, `pgaScopeOverrides`, `pgaAnnotated`, `pgaAnnNodeCanonical`, `pgaCanonical`, `pgaPlanBuilder`, and `pgaRedirects`.
- `src/MLF/Elab/Run/Pipeline.hs` destructures those fields immediately after `prepareGeneralizationArtifact`, then rebuilds elaboration config/env, resolves root scope through `resolveCanonicalScope`, computes the root target through `schemeBodyTarget`, and reaches into `pgaResultTypeInputs` for result-type diagnostics.
- `test/PipelineSpec.hs` and `test/RepoGuardSpec.hs` currently guard the older assembly boundary by looking for raw `pga*` fields, so tests must be updated to guard the new capability boundary instead of preserving raw-field coupling.
- `docs/architecture.md` already names `MLF.Elab.Run.Generalize.Prepare` as the owner of Generalization Preparation and says the Prepared Generalization Artifact is consumed by elaboration, root-scheme generalization, and result-type reconstruction. Update that wording only if the implemented API names or ownership details make the current text stale.

Implement this as one serial slice. Do not use worker fan-out: the signatures are shared across the elaboration pipeline and the current controller has `max_parallel_rounds = 1`.

Keep the production surface narrow. Do not add any `src-public/` API. If a low-level assertion still needs raw artifact internals, add a named test-support seam instead of exporting extra production fields solely for tests.

### Steps
1. In `src/MLF/Elab/Run/Generalize/Prepare.hs`, replace the caller-facing export of `PreparedGeneralizationArtifact(..)` with an abstract `PreparedGeneralizationArtifact` plus cohesive owner APIs. The concrete API names can vary if the implementation needs better local naming, but the capabilities must be explicit and cover:
   - `preparedAnnotated` or equivalent: return the canonical prepared annotation used by elaboration.
   - `preparedElaborationConfig` and `preparedElaborationEnv` or equivalent: build the `ElabConfig 'Presolved` and `ElabEnv 'Presolved` inputs for `elaborateWithEnv`, including canonicalizing `crAnnSourceTypes` keys with the artifact-owned annotation canonicalizer.
   - `stripPreparedWitnesslessAuthoritativeAnn` or equivalent: strip witnessless authoritative wrappers using artifact-owned edge witnesses rather than exposing `pgaEdgeArtifacts`.
   - `generalizePreparedRoot` or equivalent: given the final canonical/prepared authoritative root annotations, resolve the canonical root scope and target internally, then call the artifact-owned root generalization function.
   - `computePreparedResultType` or equivalent: run result-type reconstruction through the artifact-owned `ResultTypeInputs`, preserving the current direct-annotation and fallback behavior.
2. Keep the existing internal record fields private inside `Prepare`. If hiding the constructor requires test-only low-level visibility for existing copy-provenance assertions, add a narrow `MLF.Elab.Run.Generalize.Prepare.TestSupport` module and register it in `mlf2.cabal`; do not expose raw internals through the normal production import path just to satisfy tests.
3. Update `src/MLF/Elab/Run/Pipeline.hs` to consume only the new owner APIs from `Prepare` after `prepareGeneralizationArtifact`. Remove direct use of `pgaPresolutionView`, `pgaBindParentsGa`, `pgaGeneralizeAt`, `pgaResultTypeInputs`, `pgaEdgeArtifacts`, `pgaScopeOverrides`, `pgaAnnotated`, `pgaAnnNodeCanonical`, and `pgaRedirects` from the pipeline. Remove imports that become implementation details of `Prepare`, especially `GaBindParents(..)`, `EdgeArtifacts(..)`, `resolveCanonicalScope`, `schemeBodyTarget`, and direct result-type computation imports when they are no longer needed.
4. Preserve pipeline behavior: `authoritativeRootAnn` can remain pipeline-owned because it follows the elaborated term shape, but any logic needing artifact-owned witnesses, redirects, canonicalization, bind-parent projection, or result-type input assembly should move behind the new `Prepare` APIs.
5. Update focused tests:
   - Adjust the existing "assembly helper guard" in `test/PipelineSpec.hs` and the matching `test/RepoGuardSpec.hs` guard so they assert the new owner API boundary: `Pipeline` imports the abstract artifact/API functions, does not import `PreparedGeneralizationArtifact(..)`, does not call raw `pga*` accessors, and still does not call `mkResultTypeInputs`.
   - Adapt the "prepared generalization artifact drives redirecting instantiation behavior" test to exercise the new owner APIs for prepared annotation, root generalization, and result-type reconstruction. Preserve meaningful copy-provenance coverage through a test-support seam if the current direct raw-field assertions are still needed.
   - Keep existing elaboration/generalization behavior assertions real; do not replace them with smoke-only source scans.
6. Update `docs/architecture.md` only if the final API names or ownership text make the existing Prepared Generalization Artifact paragraph inaccurate. Keep the domain phrase **Prepared Generalization Artifact** and do not use "snapshot materialization" for this milestone.
7. If any new module is added under `src/`, register it in `mlf2.cabal`. If any new spec module is added, register it in both `mlf2.cabal` and `test/Main.hs`. Prefer updating existing `PipelineSpec`/`RepoGuardSpec` over adding new spec modules unless the test becomes unwieldy.

### Verification
Run focused checks first, then the full behavior-changing gate:

1. `git diff --check`
2. `cabal test mlf2-test --test-options '--match "assembly helper guard"'`
3. `cabal test mlf2-test --test-options '--match "prepared generalization artifact drives redirecting instantiation behavior"'`
4. `cabal test mlf2-test --test-options '--match "Pipeline \\\\(Phases 1-5\\\\)|Dual-path verification|chi-first integration keeps boundary wiring explicit"'`
5. `cabal test mlf2-test --test-options '--match "RepoGuard"'` if the guard edits land there and the matcher is accepted by Hspec; otherwise run the full test suite.
6. `cabal build all && cabal test`

Reviewer should additionally inspect that no `src-public/` files changed, `Pipeline` has no raw `pga[A-Z]` coupling, `Prepare` owns the result-type/root-generalization capability calls, and any test-support module is clearly named as test support rather than a normal production facade.

### Round Plan Record
Also written beside this file as `selection-record.json` and `round-plan-record.json`.
