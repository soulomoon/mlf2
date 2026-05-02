# Round 224 Review

Date: 2026-05-02
Round: `round-224`
Milestone: `milestone-3`
Direction: `direction-3a-clarify-direct-vs-closure-callable-shapes`
Extracted item: `absent`
Base branch: `master`
Branch: `orchestrator/round-224-clarify-callable-shapes`

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
`/Volumes/src/mlf4/orchestrator/worktrees/round-224` unless the command names
another path directly.

1. `sed -n '1,220p' /Users/ares/src/orchestratorpattern/skills/run-orchestrator-loop/SKILL.md`
2. `find .. -name AGENTS.md -print`
3. `sed -n '1,240p' AGENTS.md`
4. `sed -n '1,260p' orchestrator/roles/reviewer.md`
5. `git status --short && echo '---' && git branch --show-current && echo '---' && git rev-parse --abbrev-ref master && echo '---' && git merge-base HEAD master`
6. `sed -n '1,220p' orchestrator/state.json`
7. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md`
8. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/verification.md`
9. `sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/retry-subloop.md`
10. `sed -n '1,220p' orchestrator/rounds/round-224/selection.md`
11. `sed -n '1,260p' orchestrator/rounds/round-224/plan.md`
12. `sed -n '1,260p' orchestrator/rounds/round-224/implementation-notes.md`
13. `sed -n '1,260p' docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
14. `git diff --name-status master --`
15. `git diff --stat master --`
16. `git diff --check master --`
17. `sed -n '1,120p' orchestrator/roadmap.md && printf '\n---\n' && sed -n '1,120p' orchestrator/verification.md && printf '\n---\n' && sed -n '1,120p' orchestrator/retry-subloop.md`
18. `rg -n "BackendCallableBindingKind|BackendCallableHead|backendCallableHead|BackendClosureCalledWithBackendApp|BackendAppCalledWithClosureHead|BackendClosureCallUsedWithDirectHead|backendExprIsExplicitClosure|pushCallIntoExpression|applyEvidenceWrapperArgs" src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs test/BackendIRSpec.hs test/BackendConvertSpec.hs test/BackendLLVMSpec.hs test/RepoGuardSpec.hs docs/architecture.md docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
19. `git diff --unified=3 master -- docs/architecture.md src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs test/BackendIRSpec.hs test/BackendConvertSpec.hs test/BackendLLVMSpec.hs test/RepoGuardSpec.hs docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
20. `git diff --unified=3 master -- orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
21. `python3 - <<'PY' ... compared state lineage fields to round-224 selection.md ... PY`
22. `python3 - <<'PY' ... checked round-001 through round-098 directory continuity ... PY`
23. `git diff --name-only master -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
24. `nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md | sed -n '84,200p' && printf '\n---\n' && nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md | sed -n '88,110p' && printf '\n---\n' && nl -ba docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md | sed -n '72,88p' && printf '\n---\n' && nl -ba docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md | sed -n '68,130p'`
25. `for r in 094 095 096 097 098; do echo "== round-$r =="; test -f "orchestrator/rounds/round-$r/review-record.json" && jq '{round_id, decision, implemented_stage_result, attempt_verdict, stage_action}' "orchestrator/rounds/round-$r/review-record.json"; done`
26. `rg -n "reopen the non-cyclic-graph revision question|blocker debt remains within the current architecture|continue within the current architecture|non-cyclic-graph = unknown" docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
27. `git diff --name-only master -- orchestrator`
28. `git ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-224 | sort`
29. `git rev-parse HEAD master && echo '---' && git merge-base HEAD master && echo '---' && git rev-list --left-right --count master...HEAD`
30. `git diff --name-only master -- docs/backend-native-pipeline.md src-public app mlf2.cabal README.md AGENTS.md Bugs.md src/MLF/Backend/LLVM.hs src/MLF/Backend/LLVM/Syntax.hs src/MLF/Backend/LLVM/Ppr.hs test/Main.hs`
31. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous/"'`
32. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/validates explicit closure construction and indirect closure calls/"'`
33. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.IR/rejects malformed closure IR/"'`
34. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/keeps direct first-order local calls on the direct application path/"'`
35. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.Convert/classifies function-valued case pattern fields as closure locals/"'`
36. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers local function aliases without requiring closure conversion/"'`
37. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers case-selected closure callees through the explicit closure ABI/"'`
38. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/lowers let-selected closure callees through the explicit closure ABI/"'`
39. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/rejects BackendApp heads that select closure values through let or case/"'`
40. `python3 - <<'PY' ... parsed docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md and asserted row gates ... PY`
41. `cabal build all && cabal test`
42. `nl -ba docs/architecture.md | sed -n '152,210p'`
43. `nl -ba src/MLF/Backend/IR.hs | sed -n '40,66p' && printf '\n---\n' && nl -ba src/MLF/Backend/IR.hs | sed -n '268,348p' && printf '\n---\n' && nl -ba src/MLF/Backend/IR.hs | sed -n '788,825p' && printf '\n---\n' && nl -ba src/MLF/Backend/IR.hs | sed -n '890,1005p' && printf '\n---\n' && nl -ba src/MLF/Backend/IR.hs | sed -n '1187,1205p'`
44. `nl -ba src/MLF/Backend/Convert.hs | sed -n '9,25p' && printf '\n---\n' && nl -ba src/MLF/Backend/Convert.hs | sed -n '2770,2815p' && printf '\n---\n' && nl -ba src/MLF/Backend/Convert.hs | sed -n '2884,2900p' && printf '\n---\n' && nl -ba src/MLF/Backend/Convert.hs | sed -n '3848,3918p'`
45. `nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '42,58p' && printf '\n---\n' && nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '3210,3238p' && printf '\n---\n' && nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '3657,3676p' && printf '\n---\n' && nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '4078,4096p'`
46. `nl -ba test/BackendIRSpec.hs | sed -n '169,240p' && printf '\n---\n' && nl -ba test/BackendIRSpec.hs | sed -n '494,510p' && printf '\n---\n' && nl -ba test/BackendConvertSpec.hs | sed -n '792,860p' && printf '\n---\n' && nl -ba test/BackendLLVMSpec.hs | sed -n '360,370p' && printf '\n---\n' && nl -ba test/BackendLLVMSpec.hs | sed -n '1070,1090p' && printf '\n---\n' && nl -ba test/BackendLLVMSpec.hs | sed -n '1338,1346p' && printf '\n---\n' && nl -ba test/RepoGuardSpec.hs | sed -n '243,300p' && printf '\n---\n' && nl -ba test/RepoGuardSpec.hs | sed -n '562,600p'`
47. `nl -ba docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md | sed -n '7,16p'`

## Baseline Checks

1. `Roadmap lineage and fresh-family consistency`: `PASS`
   - `orchestrator/state.json` resolves
     `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
     `roadmap_revision = rev-001`,
     `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`,
     `active_round_id = round-224`,
     `milestone_id = milestone-3`, and
     `direction_id = direction-3a-clarify-direct-vs-closure-callable-shapes`.
   - The state/selection comparison script reported `{"missing": [], "absent_ok": true}`, so `selection.md` matches the live lineage, including the absent extracted-item state.
   - `orchestrator/roadmap.md`, `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md` all point at the same `rev-001` bundle.
   - `git diff --name-only master -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027`
     returned no output, so the completed `rev-027` roadmap family remains unchanged.
   - The round-directory continuity script reported
     `{"checked_rounds": 98, "missing_round_dirs": 0, "missing_sample": []}`.
   - Accepted `round-094` through `round-098` remain bounded predecessor evidence only: the spot checks still show `attempt_verdict = accepted` and `stage_action = finalize`, while the controlling predecessor docs still keep
     `non-cyclic-graph = unknown`,
     `continue within the current architecture`, and
     `blocker debt remains within the current architecture`, with
     `reopen the non-cyclic-graph revision question` still `not selected`
     (`docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
     lines 88-91 and 163-166;
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
     lines 75-81; and
     `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
     lines 69-70 and 86-87).
   - This review and `review-record.json` record the same lineage fields.

2. `Diff hygiene`: `PASS`
   - `git diff --check master --` returned no output.
   - `git diff --name-status master --` shows a tracked diff confined to
     thirteen files: nine implementation-owned repo-facing files plus the four expected controller-owned pointer/state files.

3. `Scope discipline`: `PASS`
   - The repo-facing implementation diff is limited to:
     `docs/architecture.md`,
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `test/BackendConvertSpec.hs`,
     `test/BackendIRSpec.hs`,
     `test/BackendLLVMSpec.hs`, and
     `test/RepoGuardSpec.hs`.
   - `git diff --name-only master -- docs/backend-native-pipeline.md src-public app mlf2.cabal README.md AGENTS.md Bugs.md src/MLF/Backend/LLVM.hs src/MLF/Backend/LLVM/Syntax.hs src/MLF/Backend/LLVM/Ppr.hs test/Main.hs`
     returned no output, so the round did not touch the planner-forbidden files or widen the public surface.
   - The durable contract remains inside the one-backend-IR / eager-runtime / no-lazy-STG boundary:
     `docs/architecture.md` keeps `MLF.Backend.IR` as the executable boundary and states the callable split explicitly
     (`docs/architecture.md` lines 152-175 and 195-210);
     `MLF.Backend.IR` still owns the validator-visible boundary and rejects confused direct-vs-closure heads explicitly
     (`src/MLF/Backend/IR.hs` lines 50-62, 273-274, 788-803, and 1187-1199);
     `MLF.Backend.LLVM.Lower` keeps lowering private and downstream
     (`src/MLF/Backend/LLVM/Lower.hs` lines 42-56).
   - `orchestrator/state.json` keeps the family serial:
     `max_parallel_rounds = 1`, and `active_rounds` contains only `round-224`.

4. `Evidence and test gate`: `PASS`
   - The focused callable-shape repo guard passed:
     `1 example, 0 failures`.
   - The focused backend IR slices passed:
     `1 example, 0 failures` for
     `validates explicit closure construction and indirect closure calls`, and
     `1 example, 0 failures` for
     `rejects malformed closure IR`.
   - The focused backend conversion slices passed:
     `1 example, 0 failures` for
     `keeps direct first-order local calls on the direct application path`, and
     `1 example, 0 failures` for
     `classifies function-valued case pattern fields as closure locals`.
   - The focused backend LLVM slices passed:
     `1 example, 0 failures` for
     `lowers local function aliases without requiring closure conversion`,
     `lowers case-selected closure callees through the explicit closure ABI`,
     `lowers let-selected closure callees through the explicit closure ABI`, and
     `rejects BackendApp heads that select closure values through let or case`.
   - Because `src/` and `test/` changed, the full repo gate was required and rerun. `cabal build all && cabal test` passed with
     `2340 examples, 0 failures` in `315.9484 seconds`.

5. `Mechanism-table discipline`: `PASS`
   - The parser gate over
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     asserted the exact row order and gate vector:
     row 1 `YES`,
     row 2 `YES`,
     row 3 `YES`,
     rows 4-7 `NO`.
   - The table still uses only `YES` / `NO`, keeps the fixed order, and now flips only row 3 to `YES`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).
   - No external orchestrator log files changed.

6. `Guidance synchronization`: `PASS`
   - `docs/architecture.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
     `test/BackendIRSpec.hs`,
     `test/BackendConvertSpec.hs`,
     `test/BackendLLVMSpec.hs`, and
     `test/RepoGuardSpec.hs`
     now agree on the same callable-shape contract.
   - `docs/backend-native-pipeline.md` is unchanged, which is correct for this row because the round contract explicitly disallowed edits there.
   - `AGENTS.md` is unchanged, which is correct because this round adds no durable repo-wide workflow or policy rule.

## Plan Conformance

1. `Task 1: Publish one explicit callable-shape contract at the backend IR boundary`: `PASS`
   - `docs/architecture.md` now states that `BackendApp` is the direct first-order call node, while `BackendClosureCall` is the indirect closure path for closure-valued aliases, captures, constructor-field projections, and case/let-selected closure values
     (`docs/architecture.md` lines 159-164 and 195-210).
   - `src/MLF/Backend/IR.hs` publishes the same callable split in the module note and adds the backend-owned shared classifier surface
     `BackendCallableBindingKind`,
     `BackendCallableHead`, and
     `backendCallableHead`
     (`src/MLF/Backend/IR.hs` lines 50-62, 273-274, 331-340, and 890-983).
   - Validation now reports the violated callable invariant directly:
     `BackendClosureCalledWithBackendApp` for malformed direct calls on closure heads, and
     `BackendDirectCalledWithBackendClosureCall` for malformed closure calls on direct heads
     (`src/MLF/Backend/IR.hs` lines 788-803 and 1187-1199).

2. `Task 2: Make conversion and lowering obey that shared callable contract`: `PASS`
   - `src/MLF/Backend/Convert.hs` now states the direct-vs-closure emission rule in its module note and routes application emission through the shared callable classifier
     (`src/MLF/Backend/Convert.hs` lines 15-22 and 2884-2899).
   - Direct first-order calls still emit `BackendApp`, while closure-valued heads emit `BackendClosureCall`
     (`src/MLF/Backend/Convert.hs` lines 2770-2815).
   - Case-shaped function results now use the same callable decision surface so closure-valued case results stay on `BackendClosureCall`, while direct results stay on `BackendApp`
     (`src/MLF/Backend/Convert.hs` lines 3853-3916).
   - `src/MLF/Backend/LLVM/Lower.hs` now states that lowering consumes the same callable contract rather than legalizing malformed `BackendApp` heads after let/case peeling
     (`src/MLF/Backend/LLVM/Lower.hs` lines 46-56).
   - Lowering consumes `backendCallableHead` in the evidence-wrapper path and in call lowering, and explicitly rejects closure-headed `BackendApp` forms instead of repairing them silently
     (`src/MLF/Backend/LLVM/Lower.hs` lines 3213-3235, 3657-3667, and 4081-4090).

3. `Task 3: Refresh focused callable-shape evidence and replace convention-based LLVM acceptance`: `PASS`
   - `test/BackendIRSpec.hs` covers explicit closure-call success and named rejection paths, including case-selected closure-call success and both confused-call diagnostics
     (`test/BackendIRSpec.hs` lines 169-239 and 494-509).
   - `test/BackendConvertSpec.hs` covers both direct local-call success and case-field closure classification, and now asserts that the converted case-field path does not contain a case-headed `BackendApp`
     (`test/BackendConvertSpec.hs` lines 792-801 and 846-854).
   - `test/BackendLLVMSpec.hs` keeps positive direct-call and explicit closure-call lowering coverage while replacing malformed case/let-headed `BackendApp` acceptance with rejection coverage
     (`test/BackendLLVMSpec.hs` lines 365-370, 1071-1086, and 1340-1344).
   - `test/RepoGuardSpec.hs` adds the exact planner-required guard name and marker sets spanning the four synchronized callable-contract files
     (`test/RepoGuardSpec.hs` lines 246-258 and 565-595).

4. `Task 4: Refresh mechanism-table row 3 only`: `PASS`
   - The mechanism table now records row 3 as accepted evidence-backed `YES`, cites the synchronized callable-contract surfaces plus the focused tests, and points the next action at milestone-4 / row-4 work
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
     lines 9-15).
   - Rows 1 and 2 remain `YES`, and rows 4 through 7 remain `NO`.
   - No other mechanism row changed in the tracked diff.

## Milestone-3 Checks

1. `Direct first-order calls, local callable aliases, and indirect closure calls are distinguished clearly`: `PASS`
   - The backend contract now states the split explicitly in the durable guidance and in `MLF.Backend.IR`
     (`docs/architecture.md` lines 159-164 and 195-210;
     `src/MLF/Backend/IR.hs` lines 50-62).
   - Direct local calls remain on the direct path in both conversion and lowering
     (`test/BackendConvertSpec.hs` lines 846-854;
     `test/BackendLLVMSpec.hs` lines 365-370).
   - Closure-valued aliases and case/let-selected closure values remain on the explicit closure-call path
     (`test/BackendIRSpec.hs` lines 206-239;
     `test/BackendConvertSpec.hs` lines 792-801;
     `test/BackendLLVMSpec.hs` lines 1071-1086).

2. `Evidence covers both success and rejection paths for callable shapes`: `PASS`
   - Success coverage exists for direct local calls, explicit closure calls, case-selected closure calls, and let-selected closure calls
     (`test/BackendIRSpec.hs` lines 226-239;
     `test/BackendConvertSpec.hs` lines 846-854;
     `test/BackendLLVMSpec.hs` lines 365-370 and 1071-1086).
   - Rejection coverage exists for malformed direct-call and closure-call heads with named diagnostics
     (`test/BackendIRSpec.hs` lines 496-509;
     `test/BackendLLVMSpec.hs` lines 1340-1344).

3. `Touched tests stay anchored in backend-spec surfaces`: `PASS`
   - The new and changed evidence lives in
     `BackendIRSpec`,
     `BackendConvertSpec`,
     `BackendLLVMSpec`, and
     `RepoGuardSpec`,
     all of which exercise the backend-owned contract surfaces directly rather than informal examples only.

## Controller-Owned Merge Hygiene

1. `Round diff shape relative to master`: `PASS`
   - `git rev-parse HEAD master` returned the same commit for both refs:
     `006eb569ced40de734ec2fcecf76b40f91fc7f74`.
   - `git merge-base HEAD master` returned the same commit, and
     `git rev-list --left-right --count master...HEAD` returned `0 0`.
   - This round therefore exists as an uncommitted canonical-worktree diff on top of merged `round-223`, not as extra branch commits ahead of `master`.

2. `Controller-owned files stayed outside the merge payload`: `PASS`
   - `git diff --name-only master -- orchestrator` returned only:
     `orchestrator/retry-subloop.md`,
     `orchestrator/roadmap.md`,
     `orchestrator/state.json`, and
     `orchestrator/verification.md`.
   - `git ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-224 | sort`
     showed only the active roadmap bundle files plus the round-owned lineage files prior to this review writeout.
   - The merge payload is therefore the nine implementation-owned repo-facing files only; controller pointer/state files, roadmap bundle files, and round artifacts remain controller-owned bookkeeping and review evidence.

3. `Merge payload readiness`: `PASS`
   - Merge payload files:
     `docs/architecture.md`,
     `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
     `src/MLF/Backend/Convert.hs`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/LLVM/Lower.hs`,
     `test/BackendConvertSpec.hs`,
     `test/BackendIRSpec.hs`,
     `test/BackendLLVMSpec.hs`, and
     `test/RepoGuardSpec.hs`.
   - Residual risks are non-blocking and already reflected honestly in the mechanism table:
     row 4 through row 7 remain `NO`, and the callable classifier intentionally rejects ambiguous callable heads instead of widening the callable contract beyond row 3.

## Decision

The round satisfies the `milestone-3` verification contract and the planner-authored row-3 callable-shape contract. It is approved as `accepted + finalize`.
