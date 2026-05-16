# Verification Contract

Roadmap family: `2026-05-16-00-architecture-deepening-roadmap`
Revision: `rev-001`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks that match its touched scope.

1. **Build and test gate**
   - For behavior-changing work, run `cabal build all && cabal test` before
     approval.
   - While iterating, run the narrowest focused test slice that covers the
     touched behavior before expanding to the full gate.
   - Backend/runtime/CLI rounds should consider focused coverage under
     `test/BackendIRSpec.hs`, `test/BackendConvertSpec.hs`,
     `test/BackendLLVMSpec.hs`, `test/ProgramSpec.hs`,
     `test/FrozenParitySpec.hs`, `test/GoldenSpec.hs`,
     `test/LLVMToolSupport.hs`, and the program fixtures under
     `test/programs/`.
   - Elaboration and Snapshot Finalization rounds should consider focused
     coverage under `test/GeneralizeSpec.hs`, `test/ElaborationSpec.hs`,
     `test/PipelineSpec.hs`, `test/PresolutionFacadeSpec.hs`,
     `test/PresolutionSpec.hs`, `test/Constraint/SolvedSpec.hs`,
     `test/Reify/CoreSpec.hs`, `test/Reify/NamedSpec.hs`,
     `test/Reify/TypeSpec.hs`, and Snapshot Finalization guard tests.

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, dead code, duplicated ownership policy, or
     stale compatibility helpers are left behind.
   - Verify new modules are registered in `mlf2.cabal` and new spec modules
     are registered in both `mlf2.cabal` and `test/Main.hs`.

3. **Scope discipline**
   - Confirm the round stays inside the selected milestone and direction from
     `selection-record.json`.
   - Confirm production public surfaces under `src-public/` are unchanged
     unless the selected direction explicitly requires public-surface work.
   - Confirm `docs/architecture.md` is updated when ownership or module
     responsibility changes.
   - Confirm test-support seams are used instead of widening production
     facades solely for tests.

4. **Lineage and closeout**
   - Confirm `selection-record.json`, `round-plan-record.json`, and any
     approving `review-record.json` name the active `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `review-record.json` validates against
     `orchestrator/round-finalization-schema.md`.
   - If closeout is status-only, confirm every selector resolves through
     `roadmap-view.json` anchors and does not change future coordination.
   - If future coordination changes, require semantic-update mode instead of
     status-only closeout.

## Alignment Checks

- The round must consolidate ownership or produce a concrete no-change
  justification; it must not merely rename helpers while leaving duplicated
  policy or raw-field coupling in place.
- The round must preserve thesis faithfulness over compatibility convenience.
- The round must not reopen Legacy Surface Retirement or Backend Structural
  Recursive Data Matching as candidate discovery work. If touched, accepted
  ADRs are binding context.
- The round must not add aliases, broad raw-view bridges, migration shims,
  duplicate public backend IR layers, or broad public lowering APIs unless the
  selected direction explicitly authorizes a temporary owner-local seam.
- Backend/runtime rounds must keep `MLF.Backend.IR` as the executable backend
  IR seam.
- Test coverage must remain real assertions. Do not replace behavior, parity,
  CLI, backend, or elaboration coverage with smoke-only checks.
- Worker fan-out, if used, must be justified by non-overlapping ownership in
  `round-plan-record.json` and reviewed only after integrated verification.

## Task-Specific Checks

- **milestone-1 (Primitive Inventory Module)**
  - Verify reserved runtime names, source/backend types, and native-emission
    policy have one private owner or explicit owner map.
  - Verify frontend builtins, backend IR validation, backend conversion, LLVM
    lowering, docs, and tests no longer keep unguarded competing primitive
    tables.
  - Verify focused tests assert inventory consistency and any changed support
    policy.

- **milestone-2 (Backend Callable-Shape Module)**
  - Verify callable-shape facts are owned by one private backend Module or
    explicit owner map.
  - Verify backend IR validation, conversion, and LLVM lowering consume that
    owner instead of repeating separate callable-head heuristics.
  - Verify direct-call, closure-call, alias, and diagnostic behavior covered by
    the round remains tested.

- **milestone-3 (Native Parity Policy Module)**
  - Verify backend/native/object-code parity policy lives beside
    `ProgramMatrix` or behind an explicit test-support owner.
  - Verify `ProgramSpec`, `BackendLLVMSpec`, frozen parity helpers, and LLVM
    tool support consume that owner instead of maintaining manual string-list
    policy drift.
  - Verify support and skip policy is classified by source checking,
    interpreter/runtime, backend/native, object-code, and tool availability.
  - Verify coverage changes preserve or strengthen real assertions.

- **milestone-4 (CLI Emission Preparation)**
  - Verify backend-emission semantic preparation is owned by a backend Adapter
    rather than `MLF.Program.CLI`.
  - Verify CLI still owns command selection, file IO, and user-facing command
    orchestration.
  - Verify preparation behavior can be tested without file IO when file IO is
    not the behavior under test.
  - Verify no public lowering API is added solely for tests.

- **milestone-5 (Prepared Generalization Artifact Depth)**
  - Verify **Prepared Generalization Artifact** exposes capability Adapters or
    owner APIs for elaboration, root generalization, and result-type
    reconstruction.
  - Verify `Pipeline` and related consumers do not inspect raw preparation
    fields for redirect maps, copy maps, canonical maps, scope selection, or
    result-type reconstruction data when an owner API should provide them.
  - Verify focused elaboration/generalization tests cover the actual ownership
    move or no-change justification.

- **milestone-6 (Snapshot Finalization Seam)**
  - Verify `MLF.Constraint.Finalize` is the production **Snapshot
    Finalization** construction seam for finalized `PresolutionView` artifacts
    and solved handles.
  - Verify stepwise helpers and solved-to-view adapter mechanics are internal
    or test-support only.
  - Verify `MLF.Constraint.Presolution.View` remains the read-model surface
    and does not become a broad raw-view compatibility home.
  - Verify any remaining raw-view or solved/view bridge is private,
    owner-local, and named for its concrete boundary.

## Manual Checks

- For audit-only rounds, manually confirm the audit names exact files,
  duplicated policies or exposed raw-field coupling, and the selected owner or
  no-change reason.
- For docs-only rounds, manually check that wording does not claim behavior
  beyond the evidence produced by source inspection, tests, or accepted
  reviewer records.
- For parity and support-policy rounds, manually classify behavior by layer:
  source checking, interpreter/runtime, backend/native, object-code, and tool
  availability.
- For Snapshot Finalization rounds, manually check terminology against
  `CONTEXT.md` so "snapshot materialization" is not used for canonical
  read-model and solved-handle construction.
- For roadmap closeout, manually compare `roadmap.md` and
  `roadmap-view.json` to confirm milestone statuses, dependencies, direction
  ids, and anchors agree.

## Roadmap Overrides

- Retry behavior for this family should stay same-round by default while the
  selected plan remains valid and the retry stays within the selected
  milestone, direction, branch, and worktree.
- After three rejected attempts for the same round, require recovery review
  before another same-mechanism retry.
- If a retry would change ownership strategy, milestone meaning, sequencing,
  accepted public boundary, verification requirements, support-layer policy, or
  ADR interpretation, classify it as `semantic-update-required` instead of
  continuing as status-only closeout.
