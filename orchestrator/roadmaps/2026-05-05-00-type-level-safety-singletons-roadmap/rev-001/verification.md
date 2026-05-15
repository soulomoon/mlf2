# Verification Contract

Roadmap family: `2026-05-05-00-type-level-safety-singletons-roadmap`
Revision: `rev-001`
Contract: `orchestrator-v2`

## Baseline Checks

Every round must satisfy the checks that match its touched scope.

1. **Build and test gate**
   - For behavior-changing work, run `cabal build all && cabal test` before
     approval.
   - While iterating, run the narrowest focused test slice that covers the
     touched behavior before expanding to the full gate.
   - If the round touches `src/MLF/Constraint/Types/`, consider focused
     coverage under `test/BindingSpec.hs`, `test/GraphOpsSpec.hs`,
     `test/ElaborationSpec.hs`, and any narrower spec named by the plan.

2. **Diff hygiene**
   - Run `git diff --check`.
   - Verify no orphaned imports, dead code, or stale compatibility helpers are
     left behind.
   - Verify new modules are registered in `mlf2.cabal` and new spec modules
     are registered in both `mlf2.cabal` and `test/Main.hs`.

3. **Scope discipline**
   - Confirm the round stays inside the selected milestone and direction from
     `selection-record.json`.
   - Confirm no new type-level dependency is added outside the selected
     milestone's boundary.
   - Confirm public API files under `src-public/` are unchanged unless the
     selected direction explicitly requires public-surface work.

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

- The round must preserve thesis faithfulness over compatibility with older
  internal helper shapes.
- The round must not add compatibility aliases, broad raw-view bridges, or
  public facade widening unless the selected direction explicitly authorizes
  that move.
- The round must keep type-level machinery near the owning constraint or
  binding modules instead of scattering unrelated type computation across the
  pipeline.
- The round must not claim roadmap completion from source scans alone; reviewer
  approval needs the applicable focused and full validation evidence.
- Worker fan-out, if used, must be justified by non-overlapping ownership in
  `round-plan-record.json` and reviewed only after integrated verification.

## Task-Specific Checks

- **milestone-1 (NodeRef GADT and RefTag boundary)**
  - Verify type-only operations accept typed references or pass through one
    explicitly named boundary conversion point.
  - Verify `expectTypeRef` / `expectGenRef` style discrimination is deleted or
    replaced with a documented owner-local boundary conversion.
  - Verify mixed binding-tree storage remains intentional and documented if it
    is not fully split.
  - Verify no new public API surface is introduced for internal reference
    migration.

- **milestone-2 (phase kind and singletons foundation)**
  - Verify `singletons-th` is in `build-depends` in `mlf2.cabal`.
  - Verify the `Phase` kind and generated singleton type compile.
  - Verify singleton boilerplate is isolated in the owning phase module or a
    dedicated singleton module.

- **milestone-3 (phase-indexed Constraint)**
  - Verify `Constraint` has a phantom `Phase` parameter.
  - Verify normalization, acyclicity, presolution, and solve entrypoints expose
    phase progress in their type signatures.
  - Verify broad graph-level phase escape hatches are absent.
  - Verify any remaining phase erasure is private, named, and owner-local.

- **milestone-4 (ForallSpec binder safety)**
  - Verify `ForallSpec` has no redundant binder-count field.
  - Verify binder count is derived from `fsBounds` or a stronger
    length-indexed structure.
  - Verify no partial binder-array indexing remains in the selected
    translation path.
  - Verify tests cover the actual binder-alignment risk being closed.

- **milestone-5 (witness smart constructors)**
  - Verify production `EdgeWitness` and `InstanceWitness` construction goes
    through the approved smart-constructor boundary.
  - Verify the constructor boundary validates the invariants required by
    consumers.
  - Verify direct test fixture construction is either migrated or explicitly
    justified as a test-only seam.
  - Verify downstream runtime validation is removed only when the constructor
    guarantee actually subsumes it.

- **milestone-6 (integration and cleanup)**
  - Verify no broad migration shims remain.
  - Verify `AGENTS.md`, `docs/architecture.md`, and relevant ADRs match the
    accepted implementation.
  - Verify focused guard tests or compile-time checks cover the intended
    invariants.
  - Verify no material runtime or test-time performance regression is recorded.

## Manual Checks

- For docs-only rounds, manually check that wording does not claim behavior
  beyond the evidence produced by code inspection, tests, or accepted reviewer
  records.
- For type-level refactors, manually inspect GHC error-driven fixes for hidden
  compatibility fallbacks, partial pattern matches, or broad casts that pass
  tests but weaken the invariant.
- For roadmap closeout, manually compare `roadmap.md` and `roadmap-view.json`
  to confirm milestone statuses and direction ids agree.

## Roadmap Overrides

- Retry behavior for this family should stay same-round by default while the
  selected plan is still valid. Type-level refactors often require small GHC
  error-driven revisions, but the retry must stay within the selected
  milestone, direction, branch, and worktree.
- After three rejected attempts for the same round, require recovery review
  before another same-mechanism retry.
- If a retry would change milestone meaning, sequencing, accepted type-level
  boundary, or verification requirements, classify it as
  `semantic-update-required` instead of continuing as status-only closeout.
