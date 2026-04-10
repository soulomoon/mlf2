# Round 217 Review

Date: 2026-04-10
Round: `round-217`
Milestone: `milestone-3`
Direction: `direction-3a-expand-the-broader-positive-representative-corpus`
Extracted item: `promote-same-lane-sextuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`
Base branch: `codex/automatic-recursive-type-inference`
Branch: `orchestrator/round-217-promote-p5-sextuple-alias-clear-boundary-anchor`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: none
- Fix hypothesis: not needed

## Commands Run

All commands below were run against the canonical round worktree at
`/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217`
unless the command itself names a different path.

1. `sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/AGENTS.md`
2. `sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roles/reviewer.md`
3. `python3 - <<'PY'
import json, pathlib
p = pathlib.Path('/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json')
print(json.dumps(json.loads(p.read_text()), indent=2))
PY`
4. `sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-020/verification.md`
5. `sed -n '1,240p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/rounds/round-217/selection.md`
6. `sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/rounds/round-217/plan.md`
7. `sed -n '1,260p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/rounds/round-217/implementation-notes.md`
8. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 status --short`
9. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff --name-only`
10. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 rev-parse HEAD && git -C /Users/ares/.codex/worktrees/d432/mlf4 rev-parse codex/automatic-recursive-type-inference && git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 merge-base HEAD codex/automatic-recursive-type-inference && git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 rev-list --left-right --count codex/automatic-recursive-type-inference...HEAD`
11. `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null && python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/state.json >/dev/null`
12. `rg -n 'roadmap_id|roadmap_revision|roadmap_dir|milestone_id|direction_id|extracted_item_id|roadmap_item_id' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/state.json /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/rounds/round-217/selection.md`
13. `sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md && sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/verification.md && sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md && sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/roadmap.md && sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/verification.md && sed -n '1,120p' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/orchestrator/retry-subloop.md`
14. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff -- test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs`
15. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff -- orchestrator/state.json orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
16. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff --name-only 21fddba470a78569c20ffee7fb98ff5011053ff5 -- test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs src/MLF/Elab/Elaborate/Annotation.hs src/MLF/Elab/Elaborate/Algebra.hs src/MLF/Elab/Legacy.hs`
17. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff --name-only 21fddba470a78569c20ffee7fb98ff5011053ff5 -- src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
18. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff --name-only -- orchestrator/roadmaps orchestrator/rounds/round-208 orchestrator/rounds/round-209 orchestrator/rounds/round-210 orchestrator/rounds/round-211 orchestrator/rounds/round-212 orchestrator/rounds/round-213 orchestrator/rounds/round-214 orchestrator/rounds/round-215 orchestrator/rounds/round-216`
19. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217 diff --check 21fddba470a78569c20ffee7fb98ff5011053ff5 --`
20. `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/Research/P5ClearBoundarySpec.hs | sed -n '70,380p' && nl -ba /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/PipelineSpec.hs | sed -n '2260,2375p' && nl -ba /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/ElaborationSpec.hs | sed -n '2140,2285p'`
21. `rg -n 'sameLaneSextupleAliasFrameClearBoundaryExpr|sameLaneSeptupleAliasFrameClearBoundaryExpr|sameLaneOctupleAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/Research/P5ClearBoundarySpec.hs /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/PipelineSpec.hs /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-217/test/ElaborationSpec.hs`
22. Focused verification:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuintupleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "selected same-wrapper nested-forall"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "let id ="'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "pipeline fails fast for nested-let when only expansion-derived instantiation remains"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "full pipeline fails fast post-boundary-enforcement for: nested-let"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Thesis alignment invariants"'`
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
23. `./scripts/thesis-conformance-gate.sh`
24. `cabal build all && cabal test`

## Baseline Checks

1. `Roadmap lineage, pointer, and preserved-history consistency`: `PASS`
   - `orchestrator/state.json`, the canonical round-worktree
     `orchestrator/state.json`, and `selection.md` all agree on
     `roadmap_id = 2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`,
     `roadmap_revision = rev-020`,
     `roadmap_dir = orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-020`,
     `milestone_id = milestone-3`,
     `direction_id = direction-3a-expand-the-broader-positive-representative-corpus`,
     and extracted item
     `promote-same-lane-sextuple-alias-clear-boundary-packet-to-next-explicit-milestone-3-representative-corpus-anchor`.
   - `roadmap_item_id` is absent from state, selection, and this review record.
   - Parent-workspace and canonical round-worktree pointer stubs all point at
     `rev-020`.
   - `git diff --name-only -- orchestrator/roadmaps orchestrator/rounds/round-208 ... orchestrator/rounds/round-216`
     returned empty, so prior roadmap bundles and predecessor artifacts remain
     unchanged.
   - The round plan and implementation notes treat merged `round-211` through
     merged `round-216` as settled baseline truth, not as live scope.

2. `Diff hygiene`: `PASS`
   - `git diff --check 21fddba470a78569c20ffee7fb98ff5011053ff5 --`
     returned cleanly.

3. `Build and test gate for production/test changes`: `PASS`
   - `cabal build all && cabal test` passed with `1356 examples, 0 failures`.

4. `Thesis conformance gate`: `PASS`
   - `./scripts/thesis-conformance-gate.sh` ended with
     `[thesis-gate] PASS: thesis conformance anchors are green`.

5. `Broader-positive boundary discipline`: `PASS`
   - The implementation-owned diff is limited to
     `test/Research/P5ClearBoundarySpec.hs`,
     `test/PipelineSpec.hs`, and
     `test/ElaborationSpec.hs`.
   - No production fallback files were touched:
     `src/MLF/Elab/Elaborate/Annotation.hs`,
     `src/MLF/Elab/Elaborate/Algebra.hs`, and
     `src/MLF/Elab/Legacy.hs` all stayed unchanged.
   - Closed continuity anchors stayed untouched:
     `src/MLF/Elab/TermClosure.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`,
     `src-public/MLF/Pipeline.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
   - Research adds the sextuple fallback anchor, the explicit dual-entrypoint
     packet row, and the packet/fallback helpers without widening beyond the
     selected extraction
     (`test/Research/P5ClearBoundarySpec.hs:92-94`,
     `test/Research/P5ClearBoundarySpec.hs:184-196`,
     `test/Research/P5ClearBoundarySpec.hs:257-359`).
   - Pipeline sharpens the pre-existing sextuple row into the live milestone-3
     promotion check while keeping septuple/octuple/nonuple rows as
     continuity-only evidence
     (`test/PipelineSpec.hs:2294-2375`).
   - Elaboration adds one exact-edge authoritative-instantiation guard for the
     sextuple packet and does not widen into generic alias-depth helpers
     (`test/ElaborationSpec.hs:2155-2266`).
   - `rg -n 'sameLaneSextuple...|sameLaneSeptuple...|sameLaneOctuple...|sameLaneNonuple...'`
     shows septuple/deeper names only in `test/PipelineSpec.hs`, confirming
     research and elaboration stay free of deeper publication.

6. `Authoritative-entrypoint discipline`: `PASS`
   - The new research packet row exercises both `runPipelineElab` and
     `runPipelineElabChecked`
     (`test/Research/P5ClearBoundarySpec.hs:184-196`).
   - The pipeline packet row exercises the same two authoritative entrypoints
     with the promoted sextuple packet
     (`test/PipelineSpec.hs:2294-2334`).
   - The elaboration guard locks the exact-edge authoritative-instantiation
     witness to `ExpInstantiate [NodeId 49]` and
     `InstSeq (InstApp (TVar "t50")) (InstApp (TVar "t56"))`, then re-runs
     `Elab.runPipelineElab` directly
     (`test/ElaborationSpec.hs:2227-2266`).

## Milestone-3 Checks

1. `Diff stays inside the preserved writable slice`: `PASS`
   - Implementation-owned edits stay within the three admitted test files.
   - The additional live diff in
     `orchestrator/state.json`,
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     is controller-owned bookkeeping and is outside implementation scope.

2. `Merged 21fddba baseline wins remain green`: `PASS`
   - `sameLaneSextupleAliasFrameClearBoundaryExpr`: `7 examples, 0 failures`
   - `sameLaneQuintupleAliasFrameClearBoundaryExpr`: `7 examples, 0 failures`
   - `sameLaneQuadrupleAliasFrameClearBoundaryExpr`: `7 examples, 0 failures`
   - `sameLaneTripleAliasFrameClearBoundaryExpr`: `7 examples, 0 failures`
   - `sameLaneDoubleAliasFrameClearBoundaryExpr`: `6 examples, 0 failures`
   - `sameLaneClearBoundaryExpr`: `5 examples, 0 failures`
   - `sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints`: `1 example, 0 failures`
   - `selected same-wrapper nested-forall`: `3 examples, 0 failures`
   - `sameLaneSeptupleAliasFrameClearBoundaryExpr`: `4 examples, 0 failures`
   - `checked-authoritative keeps representative corpus parity`: `4 examples, 0 failures`
   - `fail-closed once it leaves the local TypeRef lane`: `4 examples, 0 failures`
   - `BUG-2026-02-06-002`: `10 examples, 0 failures`
   - `BUG-2026-02-17-002`: `1 example, 0 failures`
   - `non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)`: `1 example, 0 failures`
   - `let id =`: `4 examples, 0 failures`
   - `pipeline fails fast for nested-let when only expansion-derived instantiation remains`: `1 example, 0 failures`
   - `full pipeline fails fast post-boundary-enforcement for: nested-let`: `1 example, 0 failures`
   - `Phi alignment`: `7 examples, 0 failures`
   - `Thesis alignment invariants`: `21 examples, 0 failures`
   - `Frozen parity artifact baseline`: `1 example, 0 failures`
   - `./scripts/thesis-conformance-gate.sh`: `PASS`
   - `cabal build all && cabal test`: `1356 examples, 0 failures`

3. `sameLaneSextupleAliasFrameClearBoundaryExpr is now explicit on research, pipeline, and elaboration surfaces`: `PASS`
   - Research publishes both the `containsMu` fallback anchor and the explicit
     dual-entrypoint packet row for sextuple alias depth.
   - Pipeline promotes the existing sextuple row into the live milestone-3
     representative packet after the merged quintuple anchor.
   - Elaboration publishes the matching exact-edge authoritative-instantiation
     guard for the sextuple packet.

4. `Merged first five anchors, predecessor truth, and deeper-boundary discipline remain honest`: `PASS`
   - Research ordering remains:
     first anchor,
     predecessor truth,
     merged double anchor,
     merged triple anchor,
     merged quadruple anchor,
     merged quintuple anchor,
     new sextuple packet,
     then preserved selected same-wrapper nested-`forall`
     (`test/Research/P5ClearBoundarySpec.hs:72-98`,
     `test/Research/P5ClearBoundarySpec.hs:114-198`).
   - Pipeline still carries septuple/octuple/nonuple alias rows only as
     continuity evidence outside the reviewed extraction
     (`test/PipelineSpec.hs:2336-2566`).
   - No research or elaboration tests promote deeper shells.

5. `Direct success remains real on both authoritative entrypoints and on the authoritative-instantiation guard`: `PASS`
   - The promoted packet succeeds directly on `runPipelineElab` and
     `runPipelineElabChecked`.
   - The elaboration guard asserts the concrete edge-expansion / `phi` pair
     and then confirms direct pipeline success.
   - No helper-only compatibility shim or second interface was introduced.

## Observed Mismatch

- `git rev-list --left-right --count codex/automatic-recursive-type-inference...HEAD`
  returned `0 0`, and both `HEAD` and the base branch resolve to
  `21fddba470a78569c20ffee7fb98ff5011053ff5`. The round implementation
  therefore lives as an uncommitted canonical worktree diff on top of the
  fresh merged baseline rather than as a committed branch delta. Reviewing the
  working-tree diff against `21fddba` is the correct authoritative surface and
  is not a blocker.

## Decision

**APPROVED: the canonical round worktree satisfies the active `rev-020`
baseline and milestone-3 checks. The implementation-owned diff stays within
`test/Research/P5ClearBoundarySpec.hs`, `test/PipelineSpec.hs`, and
`test/ElaborationSpec.hs`; `sameLaneSextupleAliasFrameClearBoundaryExpr` is
now explicit on the research, pipeline, and elaboration surfaces; the merged
first five anchors, predecessor alias-frame truth, selected same-wrapper
nested-`forall` packet, checked-authoritative parity, fail-closed contrasts,
and the septuple/deeper continuity boundary remain intact;
`./scripts/thesis-conformance-gate.sh` passed; and `cabal build all && cabal
test` passed with `1356 examples, 0 failures`. Write `review-record.json` as
the authoritative `accepted + finalize` record for `round-217`.**
