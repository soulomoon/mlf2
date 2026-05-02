# Round 223 Review

Date: 2026-05-02
Round: `round-223`
Milestone: `milestone-2`
Direction: `direction-2a-pin-eager-runtime-contract`
Extracted item: `absent`
Base branch: `master`
Branch: `orchestrator/round-223-pin-eager-runtime-contract`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Commands Run

All commands below were run against the canonical round worktree at
`/Volumes/src/mlf4/orchestrator/worktrees/round-223` unless the command names
another path directly.

1. `sed -n '1,220p' /Users/ares/src/orchestratorpattern/skills/run-orchestrator-loop/SKILL.md`
2. `pwd`
3. `rg --files -g 'AGENTS.md' -g 'state.json' -g 'reviewer.md' -g 'selection.md' -g 'plan.md' -g 'implementation-notes.md' -g 'verification.md' -g 'retry-subloop.md'`
4. `sed -n '1,220p' AGENTS.md`
5. `sed -n '1,260p' orchestrator/roles/reviewer.md`
6. `cat orchestrator/state.json`
7. `sed -n '1,240p' orchestrator/rounds/round-223/selection.md`
8. `sed -n '1,260p' orchestrator/rounds/round-223/plan.md`
9. `sed -n '1,260p' orchestrator/rounds/round-223/implementation-notes.md`
10. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/verification.md`
11. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/retry-subloop.md`
12. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
13. `sed -n '1,260p' docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
14. `git status --short`
15. `git branch --show-current && git rev-parse HEAD master && git merge-base HEAD master`
16. `git diff --stat master`
17. `git diff --name-status master`
18. `sed -n '1,80p' orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,80p' orchestrator/verification.md && printf '\n---\n' && sed -n '1,80p' orchestrator/retry-subloop.md`
19. `git diff --unified=3 master -- docs/architecture.md docs/backend-native-pipeline.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
20. `git diff --unified=3 master -- src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs`
21. `git diff --unified=3 master -- test/RepoGuardSpec.hs`
22. `git diff --unified=3 master -- orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
23. `git diff --check master`
24. `table=docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md ...`
25. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope/"'`
26. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'`
27. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/native process entrypoint/"'`
28. `cabal build all && cabal test`
29. `git rev-parse HEAD master && git merge-base HEAD master && git rev-list --left-right --count master...HEAD`
30. `git diff --name-only master -- orchestrator`
31. `git ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-223 | sort`
32. `git diff --name-only master -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
33. `python3 - <<'PY' ... compared state lineage fields to round-223 selection.md ... PY`
34. `python3 - <<'PY' ... checked rounds 001-098 for missing round dirs / review artifacts ... PY`
35. `git diff --name-only master -- docs/architecture.md docs/backend-native-pipeline.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md src/MLF/Backend/Convert.hs src/MLF/Backend/IR.hs src/MLF/Backend/LLVM/Lower.hs test/RepoGuardSpec.hs orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
36. `git diff --name-only master -- src-public app mlf2.cabal README.md AGENTS.md Bugs.md docs/mlfp-language-reference.md src/MLF/Backend/LLVM.hs test/BackendIRSpec.hs test/BackendConvertSpec.hs test/BackendLLVMSpec.hs test/Main.hs`
37. `nl -ba docs/architecture.md | sed -n '145,175p'`
38. `nl -ba src/MLF/Backend/IR.hs | sed -n '15,45p'`
39. `nl -ba src/MLF/Backend/Convert.hs | sed -n '7,30p'`
40. `nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '8,55p'`
41. `nl -ba docs/backend-native-pipeline.md | sed -n '18,48p'`
42. `nl -ba docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md | sed -n '7,18p'`
43. `nl -ba test/RepoGuardSpec.hs | sed -n '227,560p'`
44. `nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md | sed -n '84,200p'`
45. `nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md | sed -n '88,110p'`
46. `nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md | sed -n '72,88p'`
47. `nl -ba docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md | sed -n '68,130p'`

## Baseline Checks

1. `Roadmap lineage and fresh-family consistency`: `PASS`
   - `orchestrator/state.json` resolves
     `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
     `roadmap_revision = rev-001`,
     `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`,
     `active_round_id = round-223`,
     `milestone_id = milestone-2`, and
     `direction_id = direction-2a-pin-eager-runtime-contract`.
   - The state/selection comparison script reported all lineage fields present
     in `orchestrator/rounds/round-223/selection.md`, including the absent
     extracted-item state.
   - `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` all point at the same `rev-001` bundle.
   - `git diff --name-only master -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
     returned no output, so the completed `rev-027` family remains unchanged.
   - The historical continuity inventory over completed rounds `round-001`
     through `round-098` returned
     `{'checked_rounds': 98, 'missing_count': 0, 'missing_sample': []}`.
   - The inherited predecessor evidence remains bounded and unchanged:
     the baseline contract still keeps the explicit-only / iso-recursive /
     non-equi-recursive / non-cyclic-graph / no-fallback boundary live,
     strategic item `2` still classifies `non-cyclic-graph = unknown`,
     strategic item `6` still treats the representative matrix as bounded
     blocker debt plus fail-closed coverage rather than general success, and
     strategic item `7` still selects `continue within the current
     architecture`
     (`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
     lines 88-91 and 163-199;
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
     lines 90-98;
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
     lines 73-81).
   - The refreshed accepted `round-094` through `round-098` chain remains the
     same bounded predecessor evidence only. The item-5 gate still records
     `blocker debt remains within the current architecture`, not a reopened
     `non-cyclic-graph` revision
     (`docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
     lines 68-129).

2. `Diff hygiene`: `PASS`
   - `git diff --check master` returned no output.
   - The tracked diff against `master` is limited to eleven files:
     seven implementation-owned repo-facing files plus the expected controller
     pointers/state files. No whitespace or patch-shape issues appeared.

3. `Scope discipline`: `PASS`
   - `git diff --name-only master -- docs/architecture.md docs/backend-native-pipeline.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md src/MLF/Backend/Convert.hs src/MLF/Backend/IR.hs src/MLF/Backend/LLVM/Lower.hs test/RepoGuardSpec.hs orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
     returned exactly the expected seven implementation-owned files plus the
     expected controller-owned pointer/state files.
   - `git diff --name-only master -- src-public app mlf2.cabal README.md AGENTS.md Bugs.md docs/mlfp-language-reference.md src/MLF/Backend/LLVM.hs test/BackendIRSpec.hs test/BackendConvertSpec.hs test/BackendLLVMSpec.hs test/Main.hs`
     returned no output, so the round stayed inside the planner-authored
     writable slice and did not widen the public surface.
   - The synchronized row-2 contract is explicit in all required surfaces:
     `MLF.Backend.IR` owns the eager executable boundary
     (`docs/architecture.md` lines 151-168;
     `src/MLF/Backend/IR.hs` lines 21-35);
     checked-program conversion publishes that eager executable structure and
     rejects lazy-runtime or lowerer-private normalization detours
     (`src/MLF/Backend/Convert.hs` lines 9-18);
     LLVM/native lowering owns only downstream private closure ABI, layout,
     wrapper/runtime symbol emission, and executable rendering support
     (`src/MLF/Backend/LLVM/Lower.hs` lines 10-19 and 44-52); and
     the native pipeline still starts from the same backend IR and excludes
     thunks, update frames, CAF update semantics, graph reduction, and
     implicit laziness rescue
     (`docs/backend-native-pipeline.md` lines 24-45).
   - The diff adds only documentation/module-note/test-guard contract text.
     It does not introduce lazy STG machinery, cyclic search, multi-SCC
     handling, a second interface, or a second executable backend IR.
   - `orchestrator/state.json` keeps the family serial:
     `max_parallel_rounds = 1` and `active_rounds` contains only `round-223`.

4. `Evidence and test gate`: `PASS`
   - The focused row-2 repository guard slice passed:
     `1 example, 0 failures`.
   - The focused backend slices passed:
     `MLF.Backend.IR` `1 example, 0 failures`, and
     `MLF.Backend.LLVM/native process entrypoint` `7 examples, 0 failures`.
   - Because `src/` and `test/` changed, the full repo gate was required and
     was rerun. `cabal build all && cabal test` passed with
     `2340 examples, 0 failures` in `324.6440 seconds`.

5. `Mechanism-table discipline`: `PASS`
   - The mechanism-table gate script reported:
     `row1=YES`,
     `row2=YES`,
     `Direct calls, closure values, and callable shapes=NO`,
     `ADT/case semantics versus layout=NO`,
     `Primitive operations and eager evaluation order=NO`,
     `Polymorphism erasure and lowerability=NO`, and
     `Validation, evidence, and guidance synchronization=NO`.
   - The table keeps the fixed order and flips only row 2 in this round
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).
   - No external orchestrator log files changed.

6. `Guidance synchronization`: `PASS`
   - `docs/architecture.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `docs/backend-native-pipeline.md`,
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
     and `test/RepoGuardSpec.hs` now agree on the same eager-runtime ownership
     split and the same lazy-STG exclusions.
   - `AGENTS.md` is unchanged, which is correct because this round lands no
     durable repo-wide workflow or policy change.

## Plan Conformance

1. `Task 1: Publish the eager-runtime ownership split in the backend contract surfaces`: `PASS`
   - `docs/architecture.md`, `src/MLF/Backend/IR.hs`, and
     `src/MLF/Backend/Convert.hs` now make the same ownership split explicit:
     `MLF.Backend.IR` owns the eager executable term shapes and their
     validation-visible invariants, while conversion must publish that shape
     directly and reject lazy-runtime or lowerer-private repair paths
     (`docs/architecture.md` lines 151-168;
     `src/MLF/Backend/IR.hs` lines 21-35;
     `src/MLF/Backend/Convert.hs` lines 9-18).

2. `Task 2: Publish the LLVM/native-only ownership boundary and keep lazy machinery excluded`: `PASS`
   - `src/MLF/Backend/LLVM/Lower.hs` now states that raw and native lowering
     both start from the same `MLF.Backend.IR` program and that closure ABI,
     layout-only helpers, wrapper/runtime symbol emission, and executable
     rendering support stay private downstream details rather than a second IR
     or lazy runtime
     (`src/MLF/Backend/LLVM/Lower.hs` lines 10-19 and 44-52).
   - `docs/backend-native-pipeline.md` now describes `emit-backend` as raw
     inspection/lowering output and `emit-native` as the same eager IR plus
     private native-entrypoint/runtime support only, with explicit lazy-STG
     exclusions
     (`docs/backend-native-pipeline.md` lines 24-45).

3. `Task 3: Refresh the mechanism table for row 2 only`: `PASS`
   - Row 2 now records the accepted eager-runtime contract, cites the
     synchronized evidence surfaces plus the focused guard, flips to `YES`, and
     points the next action at milestone-3 / row-3 callable-shape work.
   - Row 1 remains `YES`, and rows 3 through 7 remain `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).

4. `Task 4: Add one focused repository guard for the eager-runtime split and lazy-STG exclusions`: `PASS`
   - `test/RepoGuardSpec.hs` adds the exact planned `Repository guardrails`
     example
     `eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope`
     and checks the five required files
     (`test/RepoGuardSpec.hs` lines 230-244).
   - The new marker sets enforce the approved wording markers for
     `docs/architecture.md`, `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`, `src/MLF/Backend/LLVM/Lower.hs`, and
     `docs/backend-native-pipeline.md`, plus the shared no-thunks /
     no-update-frames / no-CAF-update-semantics / no-graph-reduction /
     no-implicit-laziness-rescue exclusions
     (`test/RepoGuardSpec.hs` lines 493-549).

## Milestone-2 Checks

1. `Eager-runtime contract is explicit about IR ownership versus lowering/native ownership`: `PASS`
   - The architecture, IR, convert, lower, and native-pipeline surfaces all
     state the same split:
     `MLF.Backend.IR` owns the eager executable representation;
     `MLF.Backend.Convert` publishes that structure from checked programs; and
     LLVM/native lowering owns only downstream private closure ABI, layout, and
     native runtime support for that same IR.

2. `No lazy STG assumptions are smuggled in`: `PASS`
   - Every changed surface explicitly excludes thunks, update frames, CAF
     update semantics, graph reduction, and implicit laziness rescue
     (`docs/architecture.md` lines 166-168;
     `src/MLF/Backend/IR.hs` lines 27-31;
     `src/MLF/Backend/Convert.hs` lines 16-18;
     `src/MLF/Backend/LLVM/Lower.hs` lines 17-19;
     `docs/backend-native-pipeline.md` lines 25-28).

3. `docs/backend-native-pipeline.md stays consistent with the accepted eager-runtime boundary`: `PASS`
   - The document now says both emission modes consume the same backend IR and
     that native-mode additions are limited to private entrypoint/runtime
     support only, matching the lowering note and mechanism table
     (`docs/backend-native-pipeline.md` lines 24-45;
     `src/MLF/Backend/LLVM/Lower.hs` lines 44-52;
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     line 10).

## Controller-Owned Merge Hygiene

1. `Round diff shape relative to master`: `PASS`
   - `HEAD`, `master`, and `git merge-base HEAD master` all resolve to
     `5365d9752c3d2c426369d0b709de48224d741bb4`.
   - `git rev-list --left-right --count master...HEAD` reported `0 0`, so the
     live round is an uncommitted worktree diff on top of current `master`,
     exactly as expected for review on the accepted round-222 baseline.

2. `Controller-owned files are limited to expected active-roadmap/round lineage artifacts`: `PASS`
   - Tracked controller-owned modifications are limited to:
     `orchestrator/state.json`,
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`.
   - Pre-review untracked controller-owned files were limited to:
     the active `rev-001` roadmap bundle
     (`roadmap.md`, `verification.md`, `retry-subloop.md`, `selection.md`)
     and the current round lineage files
     (`orchestrator/rounds/round-223/selection.md`,
     `plan.md`,
     `implementation-notes.md`).
   - This review adds only the expected same-round review artifacts:
     `review.md`,
     `review-record.json`, and
     `reviews/attempt-1.md`.
   - No unrelated controller-owned history, prior active bundles, or sibling
     round artifacts changed.

3. `Merge hygiene assessment`: `PASS`
   - The controller-owned files in this worktree are acceptable for this
     round's merge hygiene because they are exactly the expected active-roadmap
     activation and active-round lineage artifacts for the fresh
     `2026-05-02-00-backend-ir-executable-boundary-roadmap` family.
   - The implementation-owned repo-facing payload remains separable and
     bounded to the seven planner-authorized files.

## Decision

`round-223` satisfies the `milestone-2` plan and the active `rev-001`
verification contract. The implementation-owned diff is confined to the
authorized row-2 contract surfaces, the focused row-2 repository guard and
both backend slices passed, row 2 of the mechanism table is the only new row
flipped to `YES`, and the full repo gate passed fresh from the canonical
worktree.

The round also preserves the inherited automatic-iso-recursive predecessor
boundary as bounded background evidence only. Nothing in this diff reopens the
non-cyclic-graph revision question, introduces lazy STG machinery, widens the
public backend surface, or departs from the serial backend-IR roadmap family.

Approved: `accepted + finalize`.
