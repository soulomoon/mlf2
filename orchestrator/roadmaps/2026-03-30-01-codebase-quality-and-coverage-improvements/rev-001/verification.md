# Verification Contract

Roadmap family: `2026-03-30-01-codebase-quality-and-coverage-improvements`
Revision: `rev-001`

## Baseline Checks

Every round must pass all of the following before approval:

1. **Build gate**: `cabal build all` exits 0 with no warnings (`-Wall` is enabled).
2. **Test gate**: `cabal test` exits 0 with 0 failures.
3. **Thesis conformance gate**: `./scripts/thesis-conformance-gate.sh` exits 0.
4. **No regressions**: Test example count must not decrease from the pre-round baseline (currently 1177).
5. **Cabal module lists**: Any new `.hs` files must be listed in the appropriate `mlf2.cabal` stanza (`other-modules` or `exposed-modules`).
6. **Roadmap identity**: `selection.md` and `review-record.json` must record `roadmap_id`, `roadmap_revision`, `roadmap_dir`, and `roadmap_item_id` matching the active roadmap bundle in `state.json`.

## Task-Specific Checks

Reviewers must add checks specific to the current round. Examples by item:

- **item-1 (test coverage)**: Verify new spec files are wired into `test/Main.hs`. Verify each new spec has at least 3 meaningful examples per exported function tested. Count new test examples in output.
- **item-2 (InstBot fix)**: Verify the regression test exercises the previously-failing replay path. Verify `Bugs.md` is updated with resolution details and regression test paths.
- **item-3 (QuickCheck)**: Verify each property test has an `Arbitrary` instance or explicit generator. Verify property tests run at least 100 cases each. Count property tests in output.
- **item-4 (module decomposition)**: Verify each split parent module is a re-export facade under 200 lines. Verify no downstream imports broke (build gate covers this). Verify new submodules are in `mlf2.cabal`.
- **item-5 (research hygiene)**: Verify `mlf2-internal` builds without research modules in scope. Verify research modules still compile.
- **item-6 (parameter bundling)**: Verify targeted functions have reduced arity (<=4 non-self parameters). Verify record types follow `ElabConfig`/`ElabEnv` naming pattern.
- **item-7 (golden tests)**: Verify golden files are checked in under `test/golden/`. Verify `--accept` workflow is documented.
- **item-8 (public API)**: Verify new exports have Haddock doc-comments. Verify at least one test per new exported function.
- **item-9 (Note audit)**: Grep for function names mentioned in `{- Note -}` blocks and confirm no dangling references.

## Approval Criteria

Approval requires ALL of the following:

1. All baseline checks pass.
2. All task-specific checks for the current round pass.
3. Reviewer records evidence in `review.md` with exact commands run and output summaries.
4. No unresolved blocking issue remains.
5. Round stayed within the scope of the selected roadmap item.
6. Any `pending-merge` refresh or re-review requirement has been satisfied.

## Reviewer Record Format

Each review must include:

- **Commands run**: Exact shell commands with exit codes.
- **Pass/fail result**: Per-check pass or fail with output summary.
- **Evidence summary**: Key observations from diff and test output.
- **Decision**: Explicit **APPROVED** or **REJECTED: <specific reason and required changes>**.

When the round finalizes, also write `review-record.json`:

```json
{
  "roadmap_id": "2026-03-30-01-codebase-quality-and-coverage-improvements",
  "roadmap_revision": "rev-001",
  "roadmap_dir": "orchestrator/roadmaps/2026-03-30-01-codebase-quality-and-coverage-improvements/rev-001",
  "roadmap_item_id": "<item-N>",
  "decision": "approved",
  "evidence_summary": "<brief>"
}
```
