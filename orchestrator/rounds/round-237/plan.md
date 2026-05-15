### Selected Extraction
- Milestone: Integration And Cleanup
- Milestone id: milestone-6
- Direction id: direction-6a-integration-cleanup
- Extracted item id: item-6a-closeout-audit-and-guard-alignment
- Roadmap id: 2026-05-05-00-type-level-safety-singletons-roadmap
- Roadmap revision: rev-001
- Roadmap dir: orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001

### Goal
Close the type-level safety roadmap with a bounded integration pass: prove the accepted milestone-1 through milestone-5 invariants are still the live implementation story, retire any truly remaining broad migration shim in that audited surface if it is locally redundant, and otherwise finish milestone-6 through durable guidance, guard coverage, and full-gate evidence instead of inventing cleanup.

### Approach
Milestone-6 is now dependency-ready because milestones 3, 4, and 5 are all `done` in the active bundle, and round-236 already carries reviewer-approved closeout evidence for the final witness-constructor slice. Current HEAD already documents most of the accepted type-level safety contract in `AGENTS.md` and `docs/architecture.md`, and the existing guard suite already protects several phase and witness boundaries. The bounded closeout work is therefore not “misc cleanup”; it is a terminal audit of the exact integration surfaces this roadmap changed.

The audit should treat only explicitly transitional or compatibility-shaped seams as cleanup candidates. The clearest current candidate is the transitional `PreparedGeneralizationArtifact.pgaAcyclicBaseConstraint` bridge in `MLF.Elab.Run.Generalize.Prepare`. Accepted owner-local seams such as mixed `NodeRef` storage, directional `to*Constraint` rephasers, and the pre-normalization `mkUncheckedInstanceWitness` seam are not cleanup targets unless the source audit proves they have widened beyond their documented owners. If the audit shows that closing the remaining gap would change milestone meaning, accepted boundaries, or verification meaning, stop and record a semantic roadmap-update blocker instead of broadening the round.

### Steps
1. Audit the live milestone-6 integration surface in `src/MLF/Constraint/Types/Graph*`, `src/MLF/Constraint/Types/Phase*`, `src/MLF/Constraint/Types/Witness*`, `src/MLF/Constraint/Presolution/Witness*`, `src/MLF/Elab/Run/Generalize/Prepare.hs`, `AGENTS.md`, `docs/architecture.md`, `CHANGELOG.md`, and the existing guard specs to classify each remaining seam as either an accepted owner-local boundary or a true broad migration shim.
2. If that audit proves an explicitly transitional or broad shim in the selected surface is locally redundant, remove that one shim and update only the directly dependent code/tests/docs needed to keep the accepted type-level safety story coherent. If the audit finds no such code cleanup, keep production code unchanged and carry the round with evidence-only closeout work instead of inventing churn.
3. Align durable guidance to the audited outcome by updating `AGENTS.md`, `docs/architecture.md`, and `CHANGELOG.md`, and update an ADR only if the audit proves a live architectural decision is currently undocumented. The wording must describe the accepted implementation exactly, including any intentionally retained owner-local seams, and must not overclaim milestone completion evidence.
4. Tighten focused guard coverage in `test/RepoGuardSpec.hs`, `test/PipelineSpec.hs`, `test/PresolutionFacadeSpec.hs`, or another narrower touched guard spec so milestone-6 becomes mechanically reviewable: the accepted type-level safety markers stay synchronized across guidance, any retained seam stays explicitly owner-local and narrow, and any retired shim stays absent.
5. Run the focused guard/build checks, `git diff --check`, and the full `cabal build all && cabal test` gate. Confirm the round leaves no tracked generated build dirt, including `runtime/mlfp_io/target/release/libmlfp_io.d`, before handing off to review.

### Verification
- `git diff --check`
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "assembly helper guard: generalization preparation owns shared artifact assembly"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution witness assembly guard"'`
- Manual source audit that any remaining type-level safety seam is either explicitly owner-local and documented, or else is retired in this round without changing roadmap meaning
- Manual docs audit that `AGENTS.md`, `docs/architecture.md`, and `CHANGELOG.md` describe the same accepted type-level safety contract
- Manual post-gate check that no tracked generated runtime depfile dirt remains in the round payload
- `cabal build all && cabal test`
