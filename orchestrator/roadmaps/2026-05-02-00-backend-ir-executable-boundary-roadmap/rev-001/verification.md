# Verification Contract

Roadmap family: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
Revision: `rev-001`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap lineage and fresh-family consistency**
   - After controller activation, confirm `orchestrator/state.json` resolves
     this roadmap bundle.
   - Confirm `selection.md` records matching
     `roadmap_id`, `roadmap_revision`, `roadmap_dir`,
     `milestone_id`, `direction_id`, and `extracted_item_id`.
   - Confirm final `review-record.json` records the same lineage fields.
   - Confirm the parent-workspace and canonical round-worktree pointer stubs
     for `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     match this bundle after activation.
   - Confirm the completed `rev-027` roadmap family remains unchanged and is
     treated as predecessor completion, not as live debt reopened inside this
     family.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Scope discipline**
   - Confirm the round stays inside the selected milestone/direction scope.
   - Confirm no second public backend IR, public `LowerableBackend.IR`, or
     comparable duplicate executable IR surface was introduced unless a later
     accepted roadmap revision explicitly authorizes it.
   - Confirm no lazy STG machinery was introduced: no thunks, update frames,
     CAF update semantics, graph reduction, or implicit laziness rescue.
   - Confirm xMLF remains the typed elaboration IR and `MLF.Backend.IR`
     remains the first backend-owned executable representation.
   - Confirm the family stays serial; no repo-local parallel rounds are
     introduced.
4. **Evidence and test gate**
   - Docs-only rounds must still provide reviewable evidence in the touched
     docs/notes/tests and keep cited backend behavior honest.
   - If the round touches `src/`, `src-public/`, `test/`, `app/`, or
     `mlf2.cabal`, run the narrowest relevant backend test slice first, then
     run `cabal build all && cabal test` before approval.
   - Relevant focused backend slices include the affected modules in
     `test/BackendIRSpec.hs`,
     `test/BackendConvertSpec.hs`, and
     `test/BackendLLVMSpec.hs`.
   - If the round changes native executable claims or LLVM/native emission
     behavior, verify the affected `BackendLLVMSpec` native rows and keep
     `docs/backend-native-pipeline.md` synchronized with the observed behavior.
5. **Mechanism-table discipline**
   - The mechanism order in
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     must remain fixed.
   - Gate values in the table are exactly `YES` or `NO`.
   - A row may flip from `NO` to `YES` only with passing verification evidence
     that the reviewer can cite directly.
   - The verifier must refresh the table before and after a round when the
     round claims mechanism progress.
   - If round artifacts update the external orchestrator log, confirm any JSONL
     records remain compatible with
     `docs/plans/2026-05-02-backend-ir-executable-boundary-orchestrator-log-template.jsonl`.
6. **Guidance synchronization**
   - When the backend contract changes, keep the relevant durable surfaces
     synchronized:
     `docs/architecture.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `docs/backend-native-pipeline.md`, and the mechanism table when their
     claims are affected.
   - Update `AGENTS.md` only if the accepted change introduces a durable
     repo-wide workflow or policy rule. Do not move backend-architecture detail
     into `AGENTS.md` merely for redundancy.

## Task-Specific Checks

- **milestone-1**
  - Verify the accepted evidence states explicitly that xMLF remains the typed
    elaboration IR and `MLF.Backend.IR` is the single executable eager backend
    IR.
  - Verify the contract names the criteria that would justify any future lower
    IR instead of leaving that boundary to implicit discussion.
  - Verify no duplicate public backend IR surface was added while closing this
    row.
- **milestone-2**
  - Verify the eager-runtime contract is explicit about what belongs in
    `Backend.IR`, what belongs in LLVM lowering/native emission, and what lazy
    runtime machinery is intentionally out of scope.
  - Verify backend notes/docs do not smuggle in thunks, update frames, CAF
    update semantics, or graph reduction as unstated assumptions.
  - Verify `docs/backend-native-pipeline.md` stays consistent with the accepted
    eager runtime boundary.
- **milestone-3**
  - Verify direct first-order calls, local callable aliases, and indirect
    closure calls are distinguished clearly either by IR shape or by
    validation/diagnostics that make confusion hard.
  - Verify the evidence covers both success and rejection paths for callable
    shapes, including `BackendClosureCalledWithBackendApp`-style confused-call
    failures when relevant.
  - Verify the touched tests remain anchored in the backend-spec surfaces
    rather than informal examples only.
- **milestone-4**
  - Verify semantic ADT/case ownership remains explicit in `Backend.IR`.
  - Verify the accepted evidence identifies where tag values, field layout,
    boxing, and nullary-constructor strategy are owned.
  - Verify lowerer-owned layout policy is documented or locked by focused tests
    rather than remaining an unstated implementation accident.
- **milestone-5**
  - Verify primitive-operation support and eager evaluation-order assumptions
    are explicit enough for LLVM lowering and native emission.
  - Verify the round does not rely on hidden lowerer-only sequencing behavior
    without publishing the contract.
  - Verify unsupported primitive or ordering-sensitive shapes still fail with
    explicit backend diagnostics when they remain out of scope.
- **milestone-6**
  - Verify the accepted evidence states which polymorphic nodes may remain in
    `Backend.IR` and which shapes must be erased, specialized, or rejected
    before LLVM emission.
  - Verify the LLVM/native path does not silently accept unsupported
    polymorphic executables.
  - Verify focused tests cover both supported lowerable shapes and named
    unsupported diagnostics where lowerability remains intentionally closed.
- **milestone-7**
  - Verify every mechanism-table row reflects the accepted evidence honestly.
  - Verify docs, module notes, and backend tests cited by the family agree on
    the final contract.
  - Verify `AGENTS.md` changed only if the family earned a durable repo-wide
    workflow/policy update.

## Approval Criteria

Approval requires all applicable baseline and task-specific checks to pass, the
review evidence to match the observed diff and command output, and the result
to stay inside the one-backend-IR / eager-runtime / no-lazy-STG boundary
without silently widening architecture, public surface, or runtime semantics.
