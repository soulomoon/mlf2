# Verification Contract

Roadmap family: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
Revision: `rev-001`

## Baseline Checks

Every round must satisfy all baseline checks that match its touched scope.

1. **Roadmap identity, pointer, and preserved-history consistency**
   - Confirm `orchestrator/state.json` resolves the active roadmap bundle.
   - Confirm `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` point at the same `roadmap_id`,
     `roadmap_revision`, and `roadmap_dir`.
   - Confirm `selection.md` and `review-record.json` record matching roadmap
     identity fields when they are present.
   - Confirm prior roadmap families and revisions remain unchanged.
2. **Diff hygiene**
   - Run `git diff --check` on the round diff.
3. **Roadmap metadata integrity**
   - Confirm the active `roadmap.md` still includes `Item id:`, `Depends on:`,
     `Parallel safe:`, `Parallel group:`, and `Merge after:` for every item.
4. **Build and test gate for production/test changes**
   - If the round touches `src/`, `src-public/`, `app/`, `test/`, or
     `mlf2.cabal`, run `cabal build all && cabal test`.
5. **Thesis conformance gate for thesis-facing changes**
   - If the round touches `docs/thesis-*`, `docs/thesis-deviations.yaml`,
     `scripts/thesis-*`, or any other thesis-conformance source, run
     `./scripts/thesis-conformance-gate.sh`.
6. **Worker-plan integrity when fan-out is used**
   - If the round uses planner-authored worker fan-out, confirm
     `worker-plan.json` exists, worker ownership boundaries were respected,
     and approval is based on the integrated round result.

## Task-Specific Checks

Reviewers must also run checks specific to the selected roadmap item.

- **item-1**
  - Verify the predecessor authority chain cites the baseline contract, March
    25 capability contract, March 25 architectural audit, and the accepted
    April packet-settlement / handoff chain explicitly.
  - Verify the still-live positive and negative semantic obligations are
    frozen concretely and honestly.
  - Verify the family success bar and first actionable deliverable are
    explicit.
  - Verify the writable slice is explicit and fail-closed.
- **item-2**
  - Verify the mechanism map distinguishes settled predecessor fragments from
    still-missing general rules.
  - Verify the artifact covers recursive-shape discovery, non-local
    propagation, owner/binder-sensitive placement, polymorphism, and
    reconstruction visibility explicitly.
  - Verify the artifact does not upgrade packet evidence into a repo-level
    readiness claim.
- **item-3**
  - Verify candidate generation, rejection, and boundedness rules are explicit
    and fail closed.
  - Verify no fallback, cyclic search, multi-SCC search, or equi-recursive
    widening is smuggled in.
  - Verify ambiguity and unsoundness cases remain rejection territory unless a
    later accepted revision changes the boundary.
- **item-4**
  - Verify the authoritative repo entrypoints and output surfaces are named
    explicitly.
  - Verify the readiness contract requires reviewable reconstructed or
    elaborated output rather than solver-only success.
  - Verify representative positive and negative corpus obligations are stated
    concretely.
- **item-5**
  - Verify the diff stays inside the item-authorized writable surfaces for the
    selected bounded slice.
  - Verify representative `P2`-`P6` evidence is honest about exact coverage,
    with focused checks updated or replayed as needed.
  - Verify `cabal build all && cabal test` passed if code/test files changed.
  - Verify the aggregate positive-family read distinguishes general support,
    packet-specific folklore, and current-architecture blockers explicitly.
- **item-6**
  - Verify representative `N1`, `N2`, and `N6` cases either fail closed or
    stay bounded with evidence.
  - Verify no ambiguous or unsafe case is counted as positive success.
  - Verify focused checks and `cabal build all && cabal test` ran when
    production/test files changed.
- **item-7**
  - Verify the decision records exactly one explicit end-state outcome.
  - Verify any readiness claim, continued bounded handoff, or boundary-revision
    candidate is scoped exactly to the accumulated evidence.
  - Verify the artifact does not smuggle in a broader readiness or boundary
    claim than the family actually earned.

## Approval Criteria

Approval requires all of the following:

1. Every applicable baseline check passes.
2. Every task-specific check for the selected item passes.
3. Reviewer evidence in `review.md` matches the observed diff and command
   output.
4. No unresolved blocking issue remains for the selected item.
5. Any `pending-merge` refresh or re-review requirement has been satisfied.
6. The round result stays within the scope authorized by the active roadmap
   item and does not silently widen semantics, interfaces, or architecture.

## Reviewer Record Format

Each review must include:

- **Commands run**: exact shell commands with exit codes
- **Pass/fail result**: per-check result with output summary
- **Evidence summary**: key observations from the diff and verification output
- **Decision**: explicit **APPROVED** or **REJECTED: <reason>**

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001",
  "roadmap_item_id": "<item-N>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
