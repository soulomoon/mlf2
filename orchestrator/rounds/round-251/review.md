### Checks Run
- Command: `pwd`
  Result: pass; reviewer retry ran from `/Volumes/src/mlf4/orchestrator/worktrees/round-251`.
- Command: `git branch --show-current`
  Result: pass; canonical worktree is on `orchestrator/round-251-full-type-level-handoff`.
- Command: `git status --short --branch`
  Result: pass; broad round implementation diff is present, with the three handoff files still present as untracked files.
- Command: `sed -n '1,240p' AGENTS.md`
  Result: pass; repository guidance reloaded for the assigned worktree.
- Command: `sed -n '1,260p' orchestrator/roles/reviewer.md`
  Result: pass; reviewer role contract reloaded.
- Command: `sed -n '1,220p' orchestrator/role-contract.md`
  Result: pass; shared role ownership and artifact boundaries reloaded.
- Command: `sed -n '1,220p' orchestrator/project-contract.md`
  Result: pass; repo-wide invariants reloaded.
- Command: `sed -n '1,260p' orchestrator/active-roadmap-bundle.md`
  Result: pass; active roadmap bundle and status-only closeout rules reloaded.
- Command: `sed -n '1,260p' orchestrator/round-finalization-schema.md`
  Result: pass; `review-record-v3` schema reloaded.
- Command: `sed -n '1,260p' orchestrator/roadmap-update-schema.md`
  Result: pass; semantic roadmap update criteria reloaded.
- Command: `sed -n '1,260p' orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-002/verification.md`
  Result: pass; active verification contract reloaded.
- Command: `jq '{schema_version,roadmap_id,roadmap_revision,roadmap_dir,milestones:[.milestones[] | select(.milestone_id=="milestone-7")],directions:[.directions[] | select(.direction_id=="direction-7a-full-type-level-handoff-merge")],anchors:{"milestone-7-status":.anchors["milestone-7-status"],"milestone-7-completion":.anchors["milestone-7-completion"]}}' orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-002/roadmap-view.json`
  Result: pass; milestone-7 is `pending`, direction `direction-7a-full-type-level-handoff-merge` is selected, and closeout anchors `milestone-7-status` and `milestone-7-completion` resolve.
- Command: `sed -n '1,220p' orchestrator/rounds/round-251/selection-record.json`
  Result: pass; lineage names round-251, milestone-7, direction 7a, extracted item `round-251-full-type-level-handoff`, roadmap rev-002.
- Command: `sed -n '1,260p' orchestrator/rounds/round-251/round-plan-record.json`
  Result: pass; round plan record matches the selected lineage and `worker_mode` is `none`.
- Command: `sed -n '1,260p' orchestrator/rounds/round-251/plan.md`
  Result: pass; round plan reloaded.
- Command: `sed -n '1,260p' orchestrator/rounds/round-251/implementation-notes.md`
  Result: pass; retry notes record a docs-only fix to `docs/mlfp-language-reference.md` and no implementation-code change.
- Command: `sed -n '1,220p' orchestrator/rounds/round-251/review.md`
  Result: pass; prior rejected review reloaded as historical context before replacement.
- Command: `nl -ba docs/mlfp-language-reference.md | sed -n '420,490p'`
  Result: pass; the previously rejected section now separates `run-program`, `emit-backend`, and `emit-native`, lists supported native IO wrapper paths, and leaves `Applicative IO.ap` / `__io_ap` fail-closed for native emission.
- Command: `git diff -- docs/mlfp-language-reference.md | sed -n '1,320p'`
  Result: pass; retry diff removes stale blanket backend IO rejection wording and adds the corrected support-layer contract.
- Command: `sh -c '! rg -n '\''backend contract is fail-closed|rejects checked IO entrypoints|does not lower .*__io_pure|does not lower .*__io_bind|does not lower .*__io_putStrLn|backend IO is unsupported|structured unsupported diagnostic|emit-backend.*does not lower'\'' docs/mlfp-language-reference.md'`
  Result: pass; no stale backend/native IO rejection wording remains in the language reference.
- Command: `rg -n 'layer-specific rather than a blanket IO rejection|emit-backend.*lowers checked programs|inventory-classified native IO primitives|emit-native.*same backend IR|C ABI `main` wrapper|__io_map|__io_ap.*not native-lowerable|Functor IO\.map|Applicative IO\.ap' docs/mlfp-language-reference.md`
  Result: pass; positive markers for supported wrapper paths and the remaining `__io_ap` boundary are present.
- Command: `git diff --check`
  Result: pass; no whitespace errors.
- Command: `rg -n '^(<<<<<<<|=======|>>>>>>>)' AGENTS.md CHANGELOG.md README.md docs src src-public test mlf2.cabal runtime scripts`
  Result: pass; no conflict markers found. The command returned no matches.
- Command: `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass; generated depfile diff is empty.
- Command: `git status --short runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass; generated depfile has no tracked or untracked status.
- Command: `rg -n 'MLF\.Frontend\.(Program\.TypeFamilies|TypeLevel)|FrontendTypeLevelSpec' mlf2.cabal test/Main.hs`
  Result: pass; `mlf2.cabal` registers the new source/test modules and `test/Main.hs` imports and runs `FrontendTypeLevelSpec.spec`.
- Command: `jq -e '.schema_version == "review-record-v3" and .round_id == "round-251" and .decision == "approved" and .roadmap_closeout.mode == "status-only" and (.roadmap_closeout.status_changes | length) == 1 and (.roadmap_closeout.completion_pointers | length) == 1 and (.roadmap_closeout.history_entries | length) == 0 and .roadmap_closeout.semantic_update_required_reason == null' orchestrator/rounds/round-251/review-record.json`
  Result: pass; approval record uses `review-record-v3` and status-only closeout shape.
- Command: `jq -e --slurpfile rr orchestrator/rounds/round-251/review-record.json '.roadmap_id == $rr[0].roadmap_id and .roadmap_revision == $rr[0].roadmap_revision and .roadmap_dir == $rr[0].roadmap_dir and any(.milestones[]; .milestone_id == $rr[0].milestone_id and .status == $rr[0].roadmap_closeout.status_changes[0].expected_current_status) and (.anchors[$rr[0].roadmap_closeout.status_changes[0].roadmap_view_anchor] != null) and (.anchors[$rr[0].roadmap_closeout.completion_pointers[0].anchor_id] != null)' orchestrator/roadmaps/2026-05-17-00-mlfp-package-substrate-roadmap/rev-002/roadmap-view.json`
  Result: pass; review record lineage matches the active roadmap view, and every status-only selector resolves.
- Historical prior-review command: `test -f src/MLF/Frontend/Program/TypeFamilies.hs && test -f src/MLF/Frontend/TypeLevel.hs && test -f test/FrontendTypeLevelSpec.hs && printf 'present\n'`
  Result: pass; all three handoff-untracked files were present in the canonical worktree.
- Historical prior-review command: `git -C /Users/ares/.codex/worktrees/b648/mlf4 rev-parse --short HEAD && git -C /Users/ares/.codex/worktrees/b648/mlf4 branch --show-current && git -C /Users/ares/.codex/worktrees/b648/mlf4 status --short --branch`
  Result: pass; preserved handoff checkout was attached to `codex/full-type-level-mlfp` at `ca8f8e02`.
- Historical prior-review command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Frontend.TypeLevel"'`
  Result: pass; 12 examples, 0 failures.
- Historical prior-review command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program parse/pretty"'`
  Result: pass; 45 examples, 0 failures.
- Historical prior-review command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program diagnostics"'`
  Result: pass; 49 examples, 0 failures.
- Historical prior-review command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Program shared runtime-success parity surface"'`
  Result: pass; 123 examples, 0 failures.
- Historical prior-review command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Backend.LLVM"'`
  Result: pass; 312 examples, 0 failures.
- Historical prior-review command: `cabal build all`
  Result: pass.
- Historical prior-review command: `cabal test`
  Result: pass; 2560 examples, 0 failures.
- Historical prior-review command: `./scripts/thesis-conformance-gate.sh`
  Result: pass; thesis obligations, claims, and conformance anchors were green.
- Historical prior-review cleanup: `git restore -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: pass; only validation-created generated depfile churn was restored, and the retry depfile checks above confirm it is absent.

The full Cabal and thesis gates were not rerun in this retry. Reviewer judgment does not require rerunning them because the prior integrated review already ran the required behavior gates successfully on the reconciled implementation, the only rejected issue was stale public documentation, and the retry changed only `docs/mlfp-language-reference.md` plus `implementation-notes.md`.

### Plan Compliance
- Step 1: met. Canonical worktree and branch match the plan; implementation notes record the starting status.
- Step 2: met. Handoff and rev-002 verification files were loaded during the integrated review; the retry reloaded active roadmap and verification contracts.
- Step 3: met. The original handoff checkout state is recorded and was rechecked in the prior integrated review.
- Step 4: met. The handoff checkout is preserved on `codex/full-type-level-mlfp`.
- Step 5: met. The three handoff files are present in the canonical worktree.
- Step 6: met. The final diff covers the planned frontend, checker, backend, docs, Cabal, and test areas.
- Step 7: met. The transplant is present in the canonical round-251 worktree and package-substrate behavior remains covered by the package/fixture tests.
- Step 8: met with staging caveat. The three files are present and registered, but remain untracked until the controller/finalizer stages them.
- Step 9: met. Parser/pretty tests cover Unicode type lambdas, closed type families, kind annotations, multi-parameter classes, superclasses, and fundeps.
- Step 10: met. Focused tests cover closed-family scoping, RHS variable scope, simultaneous substitution, delayed lambda-body normalization, and stuck/cycle/fuel diagnostics.
- Step 11: met. Program checking/runtime parity covers multi-parameter classes, fundeps, superclasses, and package-owned visibility.
- Step 12: met. Backend/LLVM tests cover supported variable-headed type application paths and fail-closed unsupported paths.
- Step 13: met. Native IO tests cover `Functor IO.map`; `Applicative IO.ap` remains intentionally unsupported where function-valued IO results are not renderable.
- Step 14: met after retry. `docs/mlfp-language-reference.md` now matches the reconciled implementation: `emit-backend` can lower inventory-classified native IO wrapper paths, `emit-native` supplies the C ABI entrypoint, and `__io_ap` remains the explicit native-lowering boundary.
- Step 15: met. Generated depfile churn in `runtime/mlfp_io/target/release/libmlfp_io.d` is absent from the current diff.
- Step 16: met. No hard blocker requiring widened implementation scope was observed.
- Step 17: met. Implementation notes state the preserved branch, reconciled diff, validation results, retry fix, and implementer-side publication state.

### Decision
**APPROVED**

### Evidence
The rejected public-contract mismatch is fixed. The language reference now classifies support by layer: checked `IO` remains opaque until execution, `run-program` interprets the reserved IO primitive boundary where renderable, `emit-backend` emits private native IO wrapper declarations without a process entrypoint, and `emit-native` adds the C ABI `main` wrapper for `main : IO ...`. The same section explicitly lists the covered native wrapper paths and keeps `Applicative IO.ap` / `__io_ap` fail-closed for native emission.

The retry checks passed: stale wording scan found no old blanket backend IO rejection claims, positive-marker scan found the supported wrapper and fail-closed boundary wording, `git diff --check` passed, conflict-marker scan found no markers, and `runtime/mlfp_io/target/release/libmlfp_io.d` has no diff. Cabal/thesis gates remain applicable from the prior integrated review because the retry did not change implementation code.

Closeout classification: `status-only`. The approved result completes milestone-7 without changing future roadmap coordination, milestone meaning, sequencing, parallel lanes, extraction scope, verification meaning, or retry policy. The controller can mark `milestone-7` from `pending` to `done` through `milestone-7-status` and add a compact completion pointer through `milestone-7-completion`.
