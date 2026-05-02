# Round 225 Review

Date: 2026-05-02
Round: `round-225`
Milestone: `milestone-4`
Direction: `direction-4a-freeze-adt-layout-ownership`
Extracted item: `absent`
Base branch: `master`
Branch: `orchestrator/round-225-freeze-adt-layout-ownership`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Findings

- None. No blocker issues were found in the round diff or verification runs.

## Commands Run

All commands below were run against the canonical round worktree at
`/Volumes/src/mlf4/orchestrator/worktrees/round-225` unless the command names
another path directly.

1. `sed -n '1,220p' /Users/ares/src/orchestratorpattern/skills/run-orchestrator-loop/SKILL.md`
2. `sed -n '1,220p' /Users/ares/.codex/skills/haskell-pro/SKILL.md`
3. `find /Volumes/src/mlf4 -name AGENTS.md -print`
4. `sed -n '1,240p' /Volumes/src/mlf4/AGENTS.md`
5. `sed -n '1,240p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/AGENTS.md`
6. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/roles/reviewer.md`
7. `sed -n '1,240p' /Volumes/src/mlf4/orchestrator/state.json`
8. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
9. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/verification.md`
10. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/retry-subloop.md`
11. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/selection.md`
12. `sed -n '1,380p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/plan.md`
13. `sed -n '1,260p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/rounds/round-225/implementation-notes.md`
14. `sed -n '1,260p' /Volumes/src/mlf4/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
15. `sed -n '1,220p' /Volumes/src/mlf4/orchestrator/rounds/round-222/review-record.json && printf '\n---\n' && sed -n '1,220p' /Volumes/src/mlf4/orchestrator/rounds/round-223/review-record.json && printf '\n---\n' && sed -n '1,240p' /Volumes/src/mlf4/orchestrator/rounds/round-224/review-record.json`
16. `sed -n '1,80p' /Volumes/src/mlf4/orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/verification.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/retry-subloop.md`
17. `sed -n '1,80p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/verification.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/worktrees/round-225/orchestrator/retry-subloop.md`
18. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 rev-parse --abbrev-ref HEAD && printf '\n' && git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 rev-parse HEAD master && printf '\n' && git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 merge-base HEAD master`
19. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --name-only master...HEAD && printf '\n---WORKTREE---\n' && git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --name-only`
20. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --stat master...HEAD && printf '\n---WORKTREE---\n' && git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --stat`
21. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --check`
22. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff -- docs/architecture.md docs/backend-native-pipeline.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
23. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff -- src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs`
24. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff -- test/BackendLLVMSpec.hs test/RepoGuardSpec.hs`
25. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff -- orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/state.json`
26. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 diff --name-only -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
27. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 status --short`
28. `python3 - <<'PY' ... compared state lineage fields to round-225 selection.md ... PY`
29. `git -C /Volumes/src/mlf4/orchestrator/worktrees/round-225 ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-225 | sort`
30. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen/"'`
31. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/accepts ADT construction and case analysis through constructor metadata/"'`
32. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/recovers explicit backend constructors and cases from checked ADT paths/"'`
33. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers Nat construction and case analysis to heap tags and switch/"'`
34. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/loads only constructor fields used by a case branch/"'`
35. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers source closure-valued constructor fields through the explicit closure ABI/"'`
36. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers nullary and recursive-list constructors through case/"'`
37. `table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md ... awk/test assertions for rows 1-7 ...`
38. `cabal build all && cabal test`
39. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/architecture.md | sed -n '158,182p'`
40. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/backend-native-pipeline.md | sed -n '24,42p'`
41. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/IR.hs | sed -n '52,72p'`
42. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/Convert.hs | sed -n '18,34p'`
43. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/src/MLF/Backend/LLVM/Lower.hs | sed -n '55,90p;165,186p;5044,5294p'`
44. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/test/BackendLLVMSpec.hs | sed -n '420,548p;1064,1078p'`
45. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/test/RepoGuardSpec.hs | sed -n '252,280p;610,646p'`
46. `nl -ba /Volumes/src/mlf4/orchestrator/worktrees/round-225/docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md | sed -n '8,18p'`

## Baseline Checks

1. `Roadmap lineage and fresh-family consistency`: `PASS`
   - `orchestrator/state.json` resolves the live round to
     `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
     `roadmap_revision = rev-001`,
     `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`,
     `active_round_id = round-225`,
     `milestone_id = milestone-4`, and
     `direction_id = direction-4a-freeze-adt-layout-ownership`.
   - The state/selection comparison script reported
     `{"missing": [], "absent_ok": true}`, so the canonical
     `selection.md` matches the live lineage fields and the absent
     extracted-item state.
   - Parent-workspace and canonical-worktree pointer stubs for
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     all point at the same `rev-001` roadmap bundle.
   - `git ... diff --name-only -- .../rev-027` returned no output, so the
     completed predecessor roadmap family remains unchanged and is still
     bounded predecessor evidence only.
   - This review and `review-record.json` record the same lineage fields.

2. `Diff hygiene`: `PASS`
   - `git diff --check` returned no output.
   - `git rev-parse HEAD master` and `git merge-base HEAD master` all resolved
     to `2c1661b3a21fea0065c6f68c57bac2d5b2585f23`, and
     `git diff --name-only master...HEAD` returned no output, so the reviewed
     round diff is the live uncommitted canonical worktree diff against the
     `master` baseline rather than a committed branch delta.
   - `git diff --name-only` shows twelve tracked file changes:
     eight implementation-owned payload files plus the four expected
     controller-owned pointer/state files.

3. `Scope discipline`: `PASS`
   - The implementation-owned payload stays limited to the eight planner- and
     user-authorized files:
     `docs/architecture.md`,
     `docs/backend-native-pipeline.md`,
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `test/BackendLLVMSpec.hs`, and
     `test/RepoGuardSpec.hs`.
   - Controller-owned dirtiness remains limited to
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`,
     `orchestrator/retry-subloop.md`,
     `orchestrator/state.json`,
     the active untracked roadmap bundle files, and the round-owned artifact
     directory (`git status --short` and `git ls-files --others ...`).
   - The synchronized contract surfaces all keep semantic ADT/case ownership at
     the backend IR boundary:
     `docs/architecture.md` lines 166-173,
     `src/MLF/Backend/IR.hs` lines 59-65, and
     `src/MLF/Backend/Convert.hs` lines 23-29
     all state that `BackendData`, `BackendConstructor`,
     `BackendConstruct`, and `BackendCase` own semantic metadata/use/alternative
     structure only.
   - Layout policy remains lowerer-owned and private:
     `docs/backend-native-pipeline.md` lines 30-36 and
    `src/MLF/Backend/LLVM/Lower.hs` lines 58-79, 168-180, 5088-5090,
    5104-5105, 5151-5152, and 5288-5291
     freeze declaration-order zero-based tags, tag slot `0`, fields after the
     tag word, closure-record storage for function-like constructor fields, and
     nullary tag-only heap objects as lowerer policy rather than a second IR.
   - No row-5/6/7 capability claim, lazy STG machinery, fallback runtime path,
     public backend surface, or second lowering surface was introduced.
     `docs/backend-native-pipeline.md` lines 24-36 and
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 12-15 keep row 4 private and rows 5-7 at `NO`.
   - The family remains serial; `orchestrator/state.json` still records
     `max_parallel_rounds = 1` and only `round-225` is active.

4. `Evidence and test gate`: `PASS`
   - The focused repository guard passed:
     `1 example, 0 failures`.
   - The focused backend IR baseline proof passed:
     `accepts ADT construction and case analysis through constructor metadata`
     with `1 example, 0 failures`.
   - The focused backend conversion baseline proof passed:
     `recovers explicit backend constructors and cases from checked ADT paths`
     with `1 example, 0 failures`.
   - The focused LLVM row-4 slices all passed with `1 example, 0 failures`
     each:
     `lowers Nat construction and case analysis to heap tags and switch`,
     `loads only constructor fields used by a case branch`,
     `lowers source closure-valued constructor fields through the explicit closure ABI`, and
     `lowers nullary and recursive-list constructors through case`.
   - Because `src/` and `test/` changed, the full repo gate was required and
     rerun. `cabal build all && cabal test` passed with
     `2341 examples, 0 failures` in `329.0688 seconds`.

5. `Mechanism-table discipline`: `PASS`
   - The awk/test gate over
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     passed.
   - The table keeps the fixed row order and uses only `YES` / `NO`.
   - Rows 1-4 are `YES`, while rows 5-7 remain `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).
   - The diff flips only row 4; rows 1-3 remain `YES`, and no later row moved.

6. `Guidance synchronization`: `PASS`
   - `docs/architecture.md`,
     `docs/backend-native-pipeline.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`, and
     `src/MLF/Backend/LLVM/Lower.hs`
     now agree on the same row-4 semantic-versus-layout ownership split.
   - `test/RepoGuardSpec.hs` adds the exact required guard name and checks the
     exact row-4 marker sets across the five contract surfaces
     (`test/RepoGuardSpec.hs` lines 260-274 and 613-639).
   - `AGENTS.md` is unchanged, which is correct because this round adds no
     durable repo-wide workflow or policy rule.

## Plan Conformance

1. `Task 1: Publish the semantic-versus-layout ownership boundary`: `PASS`
   - `docs/architecture.md` makes the semantic-only ownership of
     `BackendData`, `BackendConstructor`, `BackendConstruct`, and `BackendCase`
     explicit, while excluding runtime tags, field slots, closure-field
     storage, and nullary representation from the IR boundary
     (`docs/architecture.md` lines 166-173).
   - `src/MLF/Backend/IR.hs` publishes the same row-4 boundary in the module
     note and states that the IR carries no tag numbers, field offsets,
     nullary layout witnesses, or layout-only forms
     (`src/MLF/Backend/IR.hs` lines 59-65).
   - `src/MLF/Backend/Convert.hs` states that checked-program conversion emits
     semantic constructor/case nodes only and must not assign runtime layout
     policy
     (`src/MLF/Backend/Convert.hs` lines 23-29).
   - `src/MLF/Backend/LLVM/Lower.hs` makes the private lowerer-owned layout
     policy explicit and centralizes the fixed byte/offset helpers used by
     construction, case-tag loads, and field loads/stores
     (`src/MLF/Backend/LLVM/Lower.hs` lines 58-79, 168-180, 5088-5090,
     5104-5105, 5151-5152, and 5288-5291).
   - `docs/backend-native-pipeline.md` turns raw/native LLVM inspection into a
     reviewable evidence surface for the private lowerer policy without
     creating a second IR or public lowering interface
     (`docs/backend-native-pipeline.md` lines 30-36).

2. `Task 2: Lock row-4 behavior with focused LLVM and guard evidence`: `PASS`
   - `test/BackendLLVMSpec.hs` now freezes declaration-order tags and switch
     targets, tag-skipping field offsets, explicit closure ABI storage for
     function-valued constructor fields, and nullary tag-only representation in
     the exact localized tests required by the plan
     (`test/BackendLLVMSpec.hs` lines 424-440, 492-500, 530-548, and
     1066-1078).
   - `test/RepoGuardSpec.hs` adds the exact guard
     `ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen`
     over the five required contract surfaces
     (`test/RepoGuardSpec.hs` lines 260-274).
   - No new primitive/evaluation-order claim, polymorphism-lowerability claim,
     fallback path, or alternative runtime layout was added in the touched
     tests or docs.

3. `Task 3: Refresh mechanism-table row 4 only`: `PASS`
   - Row 4 now records the accepted semantic/layout split, cites the
     synchronized docs/module notes plus focused `BackendLLVMSpec` and
     `RepoGuardSpec` evidence, and points the next live blocker at milestone 5
     / row 5
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     line 12).
   - Rows 1-3 remain `YES`, and rows 5-7 remain `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).

## Milestone-4 Checks

1. `Semantic ADT/case ownership remains explicit in Backend.IR`: `PASS`
   - The docs and module notes now agree that ADT/case structure lives in
     `MLF.Backend.IR`, not in a lowerer-only recovery convention
     (`docs/architecture.md` lines 166-173;
     `src/MLF/Backend/IR.hs` lines 59-65;
     `src/MLF/Backend/Convert.hs` lines 23-29).

2. `Accepted evidence identifies where tags, field layout, boxing, and nullary strategy are owned`: `PASS`
   - The private lowerer-owned layout policy is explicit in
     `docs/backend-native-pipeline.md` lines 30-36 and
     `src/MLF/Backend/LLVM/Lower.hs` lines 58-79.
   - The helper and lowering call sites prove the implementation follows that
     same ownership split mechanically
     (`src/MLF/Backend/LLVM/Lower.hs` lines 168-180, 5088-5090, 5104-5105,
     5151-5152, and 5288-5291).

3. `Lowerer-owned layout policy is documented or locked by focused tests`: `PASS`
   - The row-4 repo guard and focused LLVM slices both passed, and the touched
     assertions directly freeze the declaration-order tag policy, tag-skipping
     field offsets, closure-field storage path, and nullary tag-only
     representation
     (`test/BackendLLVMSpec.hs` lines 424-440, 492-500, 530-548, and
     1066-1078;
     `test/RepoGuardSpec.hs` lines 260-274 and 613-639).

## Controller-Owned Merge Hygiene

- `git status --short` shows the expected twelve tracked modifications plus the
  untracked active roadmap bundle and round-owned artifact directory.
- The four tracked controller-owned files are
  `orchestrator/roadmap.md`,
  `orchestrator/verification.md`,
  `orchestrator/retry-subloop.md`, and
  `orchestrator/state.json`.
- The active untracked roadmap bundle files and the round-owned artifact files
  are controller/ledger state, not implementation payload.
- Merge readiness is therefore evaluated only against the eight authorized
  repo-facing payload files.

## Decision

- Verdict: `approved`
- Retry contract result: `accepted + finalize`
- Merge readiness: `satisfied`
- Payload files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`.
- Residual risks:
  milestone-5 through milestone-7 remain intentionally open and rows 5-7 stay
  `NO` in the mechanism table;
  the accepted result freezes the current lowerer-private layout policy rather
  than evaluating alternate layouts, so any later representation redesign will
  need new evidence and a new accepted round.

The round satisfies the `milestone-4` verification contract and is approved as
`accepted + finalize`.
