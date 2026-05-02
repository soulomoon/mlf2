# Merge Preparation (`round-223` / `milestone-2` / `direction-2a`)

## Lineage

- `roadmap_id`: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`
- `milestone_id`: `milestone-2`
- `direction_id`: `direction-2a-pin-eager-runtime-contract`
- `extracted_item_id`: `null` (`absent` in round wording)
- `base_branch`: `master`

## Squash Commit Title

`Pin eager-runtime lowering contract and keep lazy STG machinery out of scope`

## Squash Summary

- Merge the approved `milestone-2` /
  `direction-2a-pin-eager-runtime-contract` slice for the active backend-IR
  executable-boundary roadmap.
- The canonical repo-facing payload is the synchronized row-2 contract across
  `docs/architecture.md`, `docs/backend-native-pipeline.md`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`, and `test/RepoGuardSpec.hs`.
- The approved outcome keeps `MLF.Backend.IR` as the eager executable
  backend boundary, keeps checked-program conversion publishing that same
  structure directly, limits LLVM/native lowering to downstream private
  lowering/runtime details for that IR, flips only mechanism-table row 2 to
  `YES`, and makes the shared lazy-STG exclusions explicit
  (`no thunks`, `no update frames`, `no CAF update semantics`,
  `no graph reduction`, `no implicit laziness rescue`).
- No controller-state or roadmap-pointer edit belongs to the squash
  substance. The tracked `orchestrator/state.json`,
  `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
  `orchestrator/retry-subloop.md` changes remain controller-owned
  bookkeeping outside the merge payload.

## Evidence

- `git diff --check master` passed.
- The approved repo-facing diff remains bounded to the seven planner-authorized
  files above and totals `135 insertions` with `6 deletions`.
- The focused repository guard passed:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope/"'`
  reported `1 example, 0 failures`.
- The focused backend slices passed:
  `MLF.Backend.IR` reported `1 example, 0 failures`, and
  `MLF.Backend.LLVM/native process entrypoint` reported
  `7 examples, 0 failures`.
- The mechanism-table gate passed with `row1=YES`, `row2=YES`, and rows `3`
  through `7` remaining `NO`.
- The full required gate passed after the `src/` and `test/` edits:
  `cabal build all && cabal test` reported `2340 examples, 0 failures`.
- Review evidence confirms this round adds contract text and repository-guard
  coverage only. It does not introduce a second executable backend IR, a
  public lowering surface, lazy STG machinery, cyclic search, or multi-SCC
  handling.

## Predecessor Continuity

- The inherited March baseline remains unchanged and authoritative:
  explicit-only recursive behavior stays live, recursive meaning stays
  iso-recursive, and the non-equi-recursive / non-cyclic-graph /
  no-fallback boundary remains fixed.
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
  general automatic-recursive-inference capability claim, or relitigate the
  prior settlement / decision artifacts.

## Review Confirmation

- `orchestrator/rounds/round-223/review.md` records
  `Implemented stage result: accepted`,
  `Attempt verdict: accepted`, and
  `Stage action: finalize`.
- The latest review snapshot is
  `orchestrator/rounds/round-223/reviews/attempt-1.md`; it records the same
  lawful `accepted + finalize` outcome, and `review.md` preserves that result
  in expanded controller-visible form.
- `orchestrator/rounds/round-223/review-record.json` matches the same lineage
  and records `decision: approved`, `merge_readiness: satisfied`, and
  `controller_merge_hygiene: satisfied`.

## Merge Readiness

- Merge readiness: confirmed for the approved bounded payload, provided the
  squash stays limited to the seven repo-facing files above plus the
  round-local notes for `round-223`, and continues to exclude
  controller-owned bookkeeping.
- Base branch freshness: satisfied. `HEAD`, `master`, and
  `git merge-base HEAD master` all resolve to
  `5365d9752c3d2c426369d0b709de48224d741bb4`, and
  `git rev-list --left-right --count master...HEAD` reports `0 0`.
- `round-223` is ready for squash merge.

## Follow-Up Notes

- Preserve the lineage fields above unchanged in post-merge controller
  bookkeeping.
- Keep later summaries honest about scope: this round settles only the
  milestone-2 eager-runtime boundary and row-2 mechanism-table gate. The next
  live blocker remains milestone-3 /
  `direction-3a-clarify-direct-vs-closure-callable-shapes`.
- Do not describe this merge as authorizing a second backend IR, a public
  lowering surface, lazy runtime machinery, or any reopening of the inherited
  automatic-iso-recursive predecessor decisions.
