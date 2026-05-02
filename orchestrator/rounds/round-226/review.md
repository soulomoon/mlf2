# Round 226 Review

Date: 2026-05-02
Round: `round-226`
Milestone: `milestone-5`
Direction: `direction-5a-lock-primitive-and-evaluation-order-contract`
Extracted item: `absent`
Base branch: `master`
Branch: `orchestrator/round-226-lock-primitive-evaluation-order-contract`

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
`/Volumes/src/mlf4/orchestrator/worktrees/round-226` unless the command names
another path directly.

1. `pwd`
2. `rg --files -g 'AGENTS.md' -g 'SKILL.md'`
3. `sed -n '1,220p' /Users/ares/src/orchestratorpattern/skills/run-orchestrator-loop/SKILL.md`
4. `sed -n '1,220p' AGENTS.md`
5. `sed -n '1,260p' orchestrator/state.json`
6. `sed -n '1,260p' orchestrator/roles/reviewer.md`
7. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
8. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/verification.md`
9. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/retry-subloop.md`
10. `sed -n '1,260p' orchestrator/rounds/round-226/selection.md`
11. `sed -n '1,260p' orchestrator/rounds/round-226/plan.md`
12. `sed -n '1,260p' orchestrator/rounds/round-226/implementation-notes.md`
13. `sed -n '1,260p' docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
14. `git status --short`
15. `git rev-parse --abbrev-ref HEAD && git rev-parse master && git rev-parse HEAD`
16. `git merge-base HEAD master`
17. `git diff --check master...HEAD`
18. `git diff --name-only master...HEAD`
19. `git diff --stat master...HEAD`
20. `sed -n '1,80p' orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,80p' orchestrator/verification.md && printf '\n---\n' && sed -n '1,80p' orchestrator/retry-subloop.md`
21. `sed -n '1,80p' /Volumes/src/mlf4/orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/verification.md && printf '\n---\n' && sed -n '1,80p' /Volumes/src/mlf4/orchestrator/retry-subloop.md`
22. `git diff --name-only`
23. `git diff --stat`
24. `git diff -- docs/architecture.md docs/backend-native-pipeline.md src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md test/BackendLLVMSpec.hs test/RepoGuardSpec.hs`
25. `git diff -- orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-226`
26. `rg -n "milestone-5|direction-5a|round-225|round-224|round-223|round-222|rev-027" orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
27. `sed -n '330,470p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
28. `sed -n '1,220p' orchestrator/rounds/round-225/review-record.json`
29. `sed -n '1,220p' orchestrator/rounds/round-224/review-record.json && printf '\n---\n' && sed -n '1,220p' orchestrator/rounds/round-223/review-record.json && printf '\n---\n' && sed -n '1,220p' orchestrator/rounds/round-222/review-record.json`
30. `nl -ba docs/architecture.md | sed -n '166,200p'`
31. `nl -ba docs/backend-native-pipeline.md | sed -n '30,60p'`
32. `nl -ba src/MLF/Backend/IR.hs | sed -n '56,78p'`
33. `nl -ba src/MLF/Backend/Convert.hs | sed -n '21,38p'`
34. `nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '74,110p;1838,1898p;5158,5228p'`
35. `nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '3598,3765p'`
36. `nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '4100,4188p;4548,4610p'`
37. `nl -ba test/BackendLLVMSpec.hs | sed -n '68,98p;1560,1594p'`
38. `rg -n "preserves referenced Prelude bindings and lowers runtime primitive calls" test/BackendLLVMSpec.hs`
39. `nl -ba test/BackendLLVMSpec.hs | sed -n '156,170p'`
40. `nl -ba test/RepoGuardSpec.hs | sed -n '268,292p;652,678p'`
41. `nl -ba docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md | sed -n '8,18p'`
42. `rg -n "zipWithM|traverse .*arg|__io_bind|lowerCall|lowerGlobalCall|lowerExpr .*BackendLet|bindLet" src/MLF/Backend/LLVM/Lower.hs`
43. `python3 - <<'PY' ... compare state lineage fields to round-226 selection.md ... PY`
44. `git diff --name-only -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
45. `git ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-226 | sort`
46. `table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md ... awk rows 9-15 plus python gate assertions ...`
47. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/IO backend contract/"'`
48. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/preserves referenced Prelude bindings and lowers runtime primitive calls/"'`
49. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary/"'`
50. `/usr/bin/time -p sh -c 'cabal build all && cabal test'`
51. `git diff --check`
52. `rg -n "BackendPrim|LowerableBackend|fallback runtime|broad FFI|public lowering|thunks|update frames|CAF update semantics|graph reduction|implicit laziness rescue|lazy STG" docs/architecture.md docs/backend-native-pipeline.md src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs`
53. `sed -n '1,220p' /Users/ares/.codex/skills/haskell-pro/SKILL.md`

## Baseline Checks

1. `Roadmap lineage and fresh-family consistency`: `PASS`
   - `orchestrator/state.json` resolves the live round to
     `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
     `roadmap_revision = rev-001`,
     `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`,
     `active_round_id = round-226`,
     `milestone_id = milestone-5`, and
     `direction_id = direction-5a-lock-primitive-and-evaluation-order-contract`.
   - The state/selection comparison script reported
     `{'missing': [], 'absent_ok': True}`, so canonical `selection.md`
     matches the live lineage fields and the absent extracted-item state.
   - Parent-workspace and canonical-worktree pointer stubs for
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     all point at the same `rev-001` roadmap bundle.
   - `git diff --name-only -- .../rev-027` returned no output, so the
     completed predecessor roadmap family remains unchanged and is still
     bounded predecessor evidence only.
   - This review and `review-record.json` record the same roadmap lineage
     fields.

2. `Diff hygiene`: `PASS`
   - `git diff --check` returned no output.
   - `git rev-parse HEAD`, `git rev-parse master`, and `git merge-base HEAD master`
     all resolved to `5adb3702eb0d4e5230aa0a778f417075a20a6333`.
   - `git diff --name-only master...HEAD` and `git diff --stat master...HEAD`
     returned no output, so the review target is the live uncommitted
     canonical worktree diff against the `master` baseline rather than a
     committed branch-ahead delta.
   - `git diff --name-only` shows twelve tracked file changes:
     eight implementation-owned payload files plus the four expected
     controller-owned pointer/state files.

3. `Scope discipline`: `PASS`
   - The implementation-owned payload stays limited to the eight planned and
     user-authorized repo-facing files:
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
   - The synchronized contract-owner surfaces agree that row 5 is the closed
     reserved runtime-binding set
     `__mlfp_and`, `__io_pure`, `__io_bind`, and `__io_putStrLn`, still
     carried through `BackendVar`, `BackendApp`, and `BackendTyApp`
     (`docs/architecture.md:175-180`,
     `docs/backend-native-pipeline.md:38-43`,
     `src/MLF/Backend/IR.hs:66-68`,
     `src/MLF/Backend/Convert.hs:30-31`,
     `src/MLF/Backend/LLVM/Lower.hs:81-90`).
   - The same surfaces keep the inherited boundary closed against
     `BackendPrim`, a second executable IR, a public lowering surface or API,
     fallback runtime rescue, lazy STG machinery, and broad FFI expansion
     (`docs/architecture.md:179-202`,
     `docs/backend-native-pipeline.md:26-53`,
     `src/MLF/Backend/IR.hs:30-32,67-70`,
     `src/MLF/Backend/Convert.hs:21-22,31-35`,
     `src/MLF/Backend/LLVM/Lower.hs:18-23,77-94`).
   - The family remains serial; `orchestrator/state.json` still records
     `max_parallel_rounds = 1` and only `round-226` is active.

4. `Evidence and test gate`: `PASS`
   - The focused LLVM IO-contract slice passed with `7 examples, 0 failures`.
     That slice includes the new native-run row
     `executes nested __io_bind / __io_putStrLn actions in written order through the native IO runtime`.
   - The focused primitive-lowering slice
     `preserves referenced Prelude bindings and lowers runtime primitive calls`
     passed with `1 example, 0 failures`.
   - The exact repository guard
     `primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary`
     passed with `1 example, 0 failures`.
   - Because `src/` and `test/` changed, the full repo gate was required and
     rerun. `/usr/bin/time -p sh -c 'cabal build all && cabal test'` passed
     with `2343 examples, 0 failures`; the suite reported
     `Finished in 316.8670 seconds`, and the outer wall clock reported
     `real 319.42`.

5. `Mechanism-table discipline`: `PASS`
   - The awk plus Python gate over
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     passed with
     `{'rows': 7, 'gates': ['YES', 'YES', 'YES', 'YES', 'YES', 'NO', 'NO']}`.
   - The table keeps the fixed row order and uses only `YES` / `NO`.
   - Rows 1-4 remain `YES`, row 5 flips to `YES`, and rows 6-7 remain `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:9-15`).
   - Row 5 alone changed, and its next action now points to milestone 6 / row 6.

6. `Guidance synchronization`: `PASS`
   - `docs/architecture.md`,
     `docs/backend-native-pipeline.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`, and
     `src/MLF/Backend/LLVM/Lower.hs`
     now agree on the same row-5 primitive/eager contract.
   - `test/RepoGuardSpec.hs` adds the exact required guard name and checks the
     synchronized row-5 markers across those five surfaces
     (`test/RepoGuardSpec.hs:276-290,657-669`).
   - `AGENTS.md` is unchanged, which is correct because this round adds no
     durable repo-wide workflow or policy rule.

## Plan Conformance

1. `Task 1: Publish the closed primitive surface and eager sequencing contract`: `PASS`
   - `docs/architecture.md` makes the row-5 primitive surface, eager order,
     and no-fallback/no-widening boundaries explicit
     (`docs/architecture.md:175-191`).
   - `src/MLF/Backend/IR.hs` publishes the same row-5 boundary in the module
     note: primitive set, existing term forms, eager order, and explicit
     diagnostic boundary
     (`src/MLF/Backend/IR.hs:66-70`).
   - `src/MLF/Backend/Convert.hs` states that checked-program conversion keeps
     the current primitive representation and emitted eager structure rather
     than inventing a lowerer-private primitive form or fallback lane
     (`src/MLF/Backend/Convert.hs:30-35`).
   - `src/MLF/Backend/LLVM/Lower.hs` adds a dedicated note that ties the
     published order contract to concrete lowering sites:
     `bindLet` before the body,
     `lowerHeapCase` before tag dispatch,
     `zipWithM` for direct calls, and
     `traverse` for primitive/global primitive calls
     (`src/MLF/Backend/LLVM/Lower.hs:81-94,3609-3612,4165,4571-4580,4599`).
   - `docs/backend-native-pipeline.md` synchronizes the same primitive/eager
     contract for backend/native inspection and keeps native execution out of
     fallback or implicit laziness rescue
     (`docs/backend-native-pipeline.md:38-53`).

2. `Task 2: Lock row-5 behavior with focused LLVM/native and guard evidence`: `PASS`
   - `test/BackendLLVMSpec.hs` keeps the primitive recognition rows local to
     the existing IO contract section and adds the required native-run ordering
     proof with stdout `first\nsecond\nthird\n`
     (`test/BackendLLVMSpec.hs:70-94,1580-1588`).
   - The primitive-lowering evidence row
     `preserves referenced Prelude bindings and lowers runtime primitive calls`
     remains present and checks runtime primitive linkage explicitly
     (`test/BackendLLVMSpec.hs:163-170`).
   - `test/RepoGuardSpec.hs` adds the exact guard
     `primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary`
     over the five required contract surfaces
     (`test/RepoGuardSpec.hs:276-290,657-669`).
   - No new primitive executor, fallback runtime path, public lowering API, or
     broad FFI surface was introduced.

3. `Task 3: Refresh mechanism-table row 5 only`: `PASS`
   - Row 5 now records the accepted closed primitive surface and eager
     sequencing contract, cites the synchronized docs/module notes plus
     focused `BackendLLVMSpec` and `RepoGuardSpec` evidence, and points the
     next live blocker at milestone 6 / row 6
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:13`).
   - Rows 1-4 remain `YES`, and rows 6-7 remain `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:9-15`).

## Milestone-5 Checks

1. `Primitive-operation support and eager evaluation-order assumptions are explicit enough for LLVM lowering and native emission`: `PASS`
   - The row-5 contract is explicit in the durable docs/module notes
     (`docs/architecture.md:175-191`,
     `docs/backend-native-pipeline.md:38-53`,
     `src/MLF/Backend/IR.hs:66-70`,
     `src/MLF/Backend/Convert.hs:30-32`,
     `src/MLF/Backend/LLVM/Lower.hs:81-94`).
   - Focused native evidence passed, including the new written-order
     `__io_bind` / `__io_putStrLn` execution proof with output
     `first\nsecond\nthird\n`
     (`test/BackendLLVMSpec.hs:86-94,1580-1588`).

2. `The round does not rely on hidden lowerer-only sequencing behavior without publishing the contract`: `PASS`
   - The new lowerer note names the specific sequencing sites instead of
     leaving order implicit:
     `bindLet`,
     `lowerHeapCase`,
     `zipWithM`,
     and `traverse`
     (`src/MLF/Backend/LLVM/Lower.hs:88-90`).
   - The corresponding implementation sites still match that note exactly
     (`src/MLF/Backend/LLVM/Lower.hs:3610-3612,5163-5168,4165,4571-4580,4599`).

3. `Unsupported primitive or ordering-sensitive shapes still fail with explicit backend diagnostics when they remain out of scope`: `PASS`
   - The synchronized row-5 contract surfaces keep unsupported broader shapes
     on explicit diagnostic paths instead of a fallback runtime lane
     (`docs/architecture.md:189-191`,
     `src/MLF/Backend/IR.hs:69-70`,
     `src/MLF/Backend/LLVM/Lower.hs:93-94`).
   - Lowering still rejects unsupported call heads and unknown global names
     explicitly through `BackendLLVMUnsupportedCall` and
     `BackendLLVMUnknownFunction`
     (`src/MLF/Backend/LLVM/Lower.hs:4141-4144,4582-4583`).

## Decision

The round satisfies the `milestone-5` verification contract and is approved as
`accepted + finalize`.
