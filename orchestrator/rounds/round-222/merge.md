# Merge Preparation (`round-222` / `milestone-1` / `direction-1a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `milestone_id`: `milestone-1`
- `direction_id`: `direction-1a-freeze-one-backend-ir-contract`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Freeze one-backend-IR contract and guard against public lower-IR leaks`

## Squash Summary

- Merge the approved `milestone-1` / `direction-1a-freeze-one-backend-ir-contract`
  slice for the active backend-IR executable-boundary roadmap.
- The canonical repo-facing payload is the synchronized one-backend-IR contract
  freeze across `docs/architecture.md`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  and `test/RepoGuardSpec.hs`.
- The approved outcome keeps xMLF as the thesis-faithful typed elaboration IR,
  keeps `MLF.Backend.IR` as the single executable eager backend IR, keeps
  checked-program conversion terminating at `MLF.Backend.IR`, keeps lowering-only
  normalization private, flips only mechanism-table row 1 to `YES`, and adds
  the focused repository guard against public `MLF.Backend.*` /
  `LowerableBackend.*` exposure.
- No controller-state or roadmap-pointer edit belongs to the squash substance.
  The tracked `orchestrator/state.json`, `orchestrator/roadmap.md`,
  `orchestrator/verification.md`, and `orchestrator/retry-subloop.md` changes
  remain controller-owned bookkeeping outside the merge payload.

## Evidence

- `git diff --check master` passed, and the approved repo-facing diff stays
  bounded to the seven planner-authorized files above.
- The focused repository guard passed:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/one-backend-IR contract stays explicit and no public lower IR leaks/"'`
  reported `1 example, 0 failures`.
- The mechanism-table gate passed with row 1 = `YES` and rows 2 through 7
  remaining `NO`.
- The full required gate passed after the test change:
  `cabal build all && cabal test` reported `2339 examples, 0 failures`.
- Review evidence confirms the diff adds contract text and guard coverage only;
  it does not introduce a second executable backend IR, a public
  `LowerableBackend` surface, lazy STG machinery, cyclic search, or multi-SCC
  handling.

## Predecessor Continuity

- The inherited March baseline remains unchanged and authoritative:
  explicit-only recursive behavior stays live, recursive meaning stays
  iso-recursive, and the non-equi-recursive / non-cyclic-graph / no-fallback
  boundary remains fixed.
- The March 25 capability contract and full-pipeline reconstruction contract
  still treat general automatic iso-recursive inference as unresolved
  repo-level capability work with no second interface, no fallback widening,
  and no cyclic or multi-SCC search authorization.
- The March 25 architecture decision still selects
  `continue within the current architecture`, and the March 26 same-lane
  retained-child gate still records
  `blocker debt remains within the current architecture`.
- This round preserves that predecessor chain as bounded background evidence
  only. It does not reopen the `non-cyclic-graph` revision question, promote a
  general automatic-recursive-inference claim, or relitigate the prior
  settlement / decision artifacts.

## Review Confirmation

- `orchestrator/rounds/round-222/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot,
  `orchestrator/rounds/round-222/reviews/attempt-1.md`, matches the same
  `accepted + finalize` outcome.
- `orchestrator/rounds/round-222/review-record.json` matches the same lineage
  and records `merge_readiness: satisfied` plus `decision: approved`.

## Merge Readiness

- Merge readiness: confirmed for the approved bounded payload, provided the
  squash stays limited to the approved repo-facing files and round-local notes
  for `round-222`, and continues to exclude controller-owned bookkeeping.
- Base branch freshness: satisfied. `HEAD`, `master`, and
  `git merge-base HEAD master` all resolve to
  `2677b6496d0879a3c14b76c1300206076bd68bff`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `round-222` is ready for squash merge.

## Follow-Up Notes

- Preserve the lineage fields above unchanged in post-merge controller
  bookkeeping.
- Keep later summaries honest about scope: this round settles only the
  milestone-1 one-backend-IR contract freeze and row-1 mechanism-table gate.
  The next live blocker remains milestone-2 / row-2 eager-runtime work.
- Do not describe this merge as authorizing a second backend IR, public
  lowering surface, lazy runtime machinery, or any reopening of the inherited
  automatic-iso-recursive predecessor decisions.
