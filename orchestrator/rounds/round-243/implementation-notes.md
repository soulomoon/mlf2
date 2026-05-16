### Changes Made
- `src/MLF/Elab/Run/Generalize/Prepare.hs`: narrowed the normal owner-facing module to an abstract `PreparedGeneralizationArtifact` plus prepared annotation, elaboration input, root generalization, witness stripping, and result-type reconstruction APIs.
- `src/MLF/Elab/Run/Generalize/Prepare/Internal.hs`: moved raw artifact fields and assembly mechanics behind the owner API, and added the capability functions consumed by the pipeline.
- `src/MLF/Elab/Run/Generalize/Prepare/TestSupport.hs`: added a narrow test-support view for copy-provenance assertions that still need raw prepared-artifact evidence.
- `src/MLF/Elab/Run/Pipeline.hs`: switched normal pipeline consumption to the `Prepare` owner APIs and removed direct `pga*` field, scope-resolution, edge-artifact, and result-type input coupling.
- `mlf2.cabal`: registered the new internal implementation module and test-support seam.
- `test/PipelineSpec.hs`: updated boundary guards and the Prepared Generalization Artifact behavior test to exercise the new owner APIs while retaining copy-provenance assertions through test support.
- `test/RepoGuardSpec.hs`: updated repository guardrails to require the abstract owner API and keep raw prepared-artifact fields out of `Pipeline`.
- `docs/architecture.md`: updated the Prepared Generalization Artifact ownership note to describe the abstract owner operations.

### Tests
- `test/PipelineSpec.hs`: verifies the assembly helper guard, the pipeline boundary guard, and redirecting-instantiation artifact behavior through the new owner APIs.
- `test/RepoGuardSpec.hs`: verifies type-level safety guidance stays synchronized with the audited owner-local seams.

Validation run:
- `cabal test -j1 mlf2-test --test-options '--match "assembly helper guard"'` passed, 1 example.
- `cabal test -j1 mlf2-test --test-options '--match "prepared generalization artifact drives redirecting instantiation behavior"'` passed, 1 example.
- `cabal test -j1 mlf2-test --test-options '--match "Pipeline \\\\(Phases 1-5\\\\)|Dual-path verification|chi-first integration keeps boundary wiring explicit"'` selected 0 examples, so I reran with the stable matcher below.
- `cabal test -j1 mlf2-test --test-options '--match "chi-first integration keeps boundary wiring explicit"'` passed, 1 example.
- `cabal test -j1 mlf2-test --test-options '--match "RepoGuard"'` selected 0 examples, so I reran with the changed repo-guard example below.
- `cabal test -j1 mlf2-test --test-options '--match "type-level safety guidance stays synchronized"'` passed, 1 example.
- `git diff --check` passed.
- `cabal build all && cabal test` passed, 2583 examples and 0 failures.

### Notes
No `src-public/` files changed. A parallel Cabal test compile hit a transient `PipelineSpec.o.tmp` rename failure; rerunning the focused checks with `-j1` produced deterministic passing results. The full required gate was run with the plan's exact `cabal build all && cabal test` command and passed.
