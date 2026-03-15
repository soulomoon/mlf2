# MLF Type Inference & Elaboration — TODO

See [roadmap.md](roadmap.md) for the full algorithm description and paper references (especially `papers/these-finale-english.txt`; see also `papers/xmlf.txt`).

---

## Task 91 `URI-R2-C1` P2 replay root-cause orchestrator run (completed 2026-03-16)

- Completed:
  - executed the live top-level successor roadmap through `round-020` to `round-023` under `contract_version: 2`, with all substantive stage work delegated under the repo-local orchestrator contract;
  - landed `D1`, `D2`, and `D3` as authoritative `pass` results, then finalized `D4` with the terminal outcome `reopen-repair-track`;
  - returned `orchestrator/state.json` to terminal `stage: "done"` with `last_completed_round: "round-023"` so the finished diagnostic track is now historical evidence rather than live work.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Start a separate bounded repair-track roadmap before making any implementation change for `URI-R2-C1`; the completed `D1` through `D4` diagnostic roadmap is no longer live work.
  2. Keep any repair scope locked to `URI-R2-C1`, `uri-r2-c1-only-v1`, and the localized boundary `witness-replay/applyInstantiation-instbot-precondition`.
  3. Preserve `round-020` through `round-023` as predecessor evidence for the repair track instead of reopening the diagnostic rounds.

## Task 90 `URI-R2-C1` P2 replay root-cause orchestrator scaffold (completed 2026-03-16)

- Completed:
  - refreshed the live top-level `orchestrator/` so it now succeeds the finished prototype-evidence `P1` through `P4` campaign while preserving `orchestrator/rounds/round-001` through `round-019` as historical evidence;
  - wrote the approved successor design source at `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`, defining a bounded `D1` through `D4` diagnosis ladder rooted in the authoritative `P2-W` `partial-replay` mismatch;
  - updated the live roadmap, verification contract, retry-subloop doc, and role prompts so the next lawful run targets replay-failure reproduction, localization, bounded fixability probing, and a final `reopen-repair-track | remain-stop` decision.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md && test -f orchestrator/retry-subloop.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Run `round-020` on the new live roadmap, starting with `D1` replay-failure reproduction under `contract_version: 2`.
  2. Keep the investigation bounded to the authoritative `P1` subject token, the accepted `P2` replay mismatch, and the exact scenario `uri-r2-c1-only-v1`.
  3. Do not open any repair implementation track unless `D4` explicitly reaches `reopen-repair-track`.

## Task 89 `URI-R2-C1` orchestrator retry-subloop contract (completed 2026-03-16)

- Completed:
  - amended the live top-level `orchestrator/` from the single-shot prototype-evidence contract to a forward-only `contract_version: 2` retry model for future `P1` through `P3` rounds;
  - added the approved retry-subloop amendment spec at `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` plus the repo-local operational contract at `orchestrator/retry-subloop.md`;
  - updated `orchestrator/state.json`, `orchestrator/verification.md`, `orchestrator/roadmap.md`, and the live role prompts so review now distinguishes `attempt_verdict` from `stage_action`, preserves immutable per-attempt review history, and finalizes authority only on `accepted + finalize`.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `git diff --check`: PASS
  - `rg -n 'contract_version|retry-subloop|accepted \+ retry|accepted \+ finalize|rejected \+ retry' orchestrator docs/superpowers/specs AGENTS.md`: PASS
- Rolling priorities (next):
  1. If `URI-R2-C1` is revisited, add a new bounded roadmap item or successor roadmap before restarting the loop; the original `P1` through `P4` roadmap remains complete.
  2. Use the new retry-subloop contract only for future rounds; preserve `round-016` through `round-019` as immutable `contract_version: 1` evidence.
  3. If the real target is reopening `URI-R2-C1`, start with a bounded root-cause task on the `P2` replay mismatch rather than fabricating more terminal rounds.

## Task 88 `URI-R2-C1` prototype evidence orchestrator scaffold (completed 2026-03-15)

- Completed:
  - refreshed the live top-level `orchestrator/` so it now succeeds the finished prototype-free `RE1` through `RE5` campaign while preserving `orchestrator/rounds/round-001` through `round-015` as historical evidence;
  - reset `orchestrator/state.json` to `stage: "select-task"` on branch `codex/automatic-recursive-type-inference` while keeping `last_completed_round: "round-015"` so future prototype-evidence rounds can continue without colliding with historical directories;
  - replaced the live roadmap, verification contract, and role prompts with the approved `URI-R2-C1` prototype-evidence track rooted in `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`, with `P1` subject discovery as the next concrete item.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Use the refreshed top-level `orchestrator/` to land `P1`, the bounded subject-discovery prototype for `URI-R2-C1`, before provenance or safety stages.
  2. Keep prototype work isolated behind the shared research entrypoint `uri-r2-c1-prototype-entrypoint-v1` and the exact scenario `uri-r2-c1-only-v1`.
  3. Preserve the accepted prototype-free `RE4`/`RE5` stop results plus rounds `001` through `015` as historical evidence rather than live work.

## Task 87 `URI-R2-C1` re-entry successor orchestrator scaffold (completed 2026-03-14)

- Completed:
  - wrote the approved next-track design source at `docs/superpowers/specs/2026-03-14-uri-r2-c1-reentry-roadmap-design.md`, defining a bounded re-entry evidence ladder `RE1` through `RE5` rather than another implementation-handoff track;
  - refreshed the live top-level `orchestrator/` so it now succeeds the completed `research-stop` track while preserving rounds `001` through `010` as historical evidence;
  - replaced the live roadmap, verification contract, and role prompts with the new `URI-R2-C1` re-entry evidence track and reset `orchestrator/state.json` to idle `select-task` with `last_completed_round: "round-010"` so future runtime rounds can continue without colliding with historical directories.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Use the refreshed top-level `orchestrator/` to land `RE1`, the provenance-authority evidence contract for `URI-R3-O4`, before uniqueness or positive-evidence work.
  2. Keep the subject fixed to `URI-R2-C1` and preserve the single-SCC, single-binder-family, non-equi-recursive, non-cyclic-graph boundary unless a future roadmap explicitly changes it.
  3. Treat the re-entry track as prototype-free; if a future revisit appears to need prototype evidence, stop and amend the roadmap separately rather than admitting it implicitly.

## Task 86 Unannotated iso-recursive successor orchestrator runtime (completed 2026-03-14)

- Completed:
  - ran the refreshed top-level `orchestrator/` end-to-end through accepted successor rounds `round-006` through `round-010`, each with delegated guider/planner/implementer/reviewer/merger stages plus controller squash-merge bookkeeping;
  - landed the successor-track research artifacts:
    - `R1` gap map,
    - `R2` bounded subset selection (`URI-R2-C1`),
    - `R3` inference-obligation contract,
    - `R4` bounded feasibility decision (`not-yet-go`),
    - `R5` explicit bounded `research-stop` decision;
  - completed the successor roadmap with all items marked `done` and returned `orchestrator/state.json` to idle `stage: "done"` with `last_completed_round: "round-010"`.
- Verification:
  - each accepted round carried reviewer-recorded docs-only baseline checks (`git diff --check`, `python3 -m json.tool orchestrator/state.json`, roadmap marker parse, continuity evidence) and explicitly skipped `cabal build all && cabal test` only because no round touched `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`;
  - final roadmap/result state is recorded in `orchestrator/roadmap.md`, `orchestrator/state.json`, and `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`.
- Rolling priorities (next):
  1. Treat the accepted `research-stop` for `URI-R2-C1` as the current boundary; do not reopen implementation-handoff work without new bounded evidence that addresses the recorded `R4`/`R5` blockers.
  2. Preserve successor rounds `round-006` through `round-010` as the authoritative execution record for this track.
  3. If a future effort revisits unannotated inference, start from the recorded `R5` re-entry requirements rather than from `ARI-C1` directly.

## Task 85 Unannotated iso-recursive successor orchestrator scaffold (completed 2026-03-14)

- Completed:
  - refreshed the live top-level `orchestrator/` so it now succeeds the completed automatic-recursive-inference rounds while preserving `orchestrator/rounds/round-001` through `round-005` as historical evidence;
  - reset `orchestrator/state.json` to `stage: "select-task"` on branch `codex/automatic-recursive-type-inference` while keeping `last_completed_round: "round-005"` so future successor rounds can continue without colliding with historical round directories;
  - replaced the live roadmap, verification contract, and role prompts with the approved unannotated iso-recursive successor track rooted in `docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`, with `R1` gap mapping as the next concrete item.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Use the refreshed top-level `orchestrator/` to land the `R1` gap map before any candidate-selection or feasibility work.
  2. Keep successor rounds research-first and bounded to single-SCC, single-binder-family, non-equi-recursive, and non-cyclic-graph rules until the roadmap itself explicitly changes.
  3. Preserve completed rounds `001` through `005` plus the recursive-types predecessor packet as inherited evidence, not live work.

## Task 84 Automatic recursive-type inference orchestrator scaffold (completed 2026-03-14)

- Completed:
  - scaffolded a new top-level `orchestrator/` successor control plane from the repo-local template, including a takeover-specific roadmap, state file, verification contract, role prompts, and round artifact directory;
  - anchored the new control plane on branch `codex/automatic-recursive-type-inference`, explicitly inherited the completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`, and defined the new target as research-first automatic recursive-type inference beyond the current explicit-only boundary;
  - updated `.gitignore`, `AGENTS.md`, and `tasks/readme` so the repo-level orchestrator/worktree workflow is documented alongside the existing task-folder conventions.
- Verification:
  - `python3 -m json.tool orchestrator/state.json`: PASS
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`: PASS
  - `git diff --check`: PASS
- Rolling priorities (next):
  1. Use the new top-level `orchestrator/` to land the inherited-baseline and acceptance-contract slice before any solver-facing spike work.
  2. Treat `tasks/todo/2026-03-11-recursive-types-orchestration/` as immutable predecessor evidence unless a future round explicitly updates its human-facing summaries.
  3. Keep early rounds research-first: no silent widening from explicit-only recursive-type support into automatic inference until the roadmap explicitly authorizes a bounded spike.

## Task 83 Recursive-types orchestration packet (completed 2026-03-11)

- Completed:
  - created `tasks/todo/2026-03-11-recursive-types-orchestration/` with `task_plan.md`, `findings.md`, `progress.md`, `mechanism_table.md`, `orchestrator_prompt.md`, and `orchestrator-log.jsonl`;
  - corrected the recursive-types roadmap overview so the source-of-truth milestone order is consistently `M0`..`M7`;
  - updated `AGENTS.md` and `tasks/readme` so orchestration campaigns explicitly document their optional mechanism/prompt/log artifacts.
- Verification:
  - `python3` JSONL parse of `tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator-log.jsonl`: PASS
  - `python3` milestone/vocabulary consistency check across `docs/plans/2026-03-11-recursive-types-roadmap.md`, `tasks/todo/2026-03-11-recursive-types-orchestration/mechanism_table.md`, and `tasks/todo/2026-03-11-recursive-types-orchestration/orchestrator_prompt.md`: PASS
- Rolling priorities (next):
  1. Resume the packet from `M7` with a fresh authority audit on current `master`, then let the planner choose the smallest thesis-backed design-resolution slice.
  2. Keep each round scoped to the smallest slice in the lowest unfinished milestone, including design-only slices when architecture is the real blocker.
  3. Only terminal-fail `M7` if verifier plus authority evidence says there is neither a safe implementation slice nor a safe design-resolution slice.

## Task 82 Restore warning-free rebuild after dead-export loop (completed 2026-03-10)

- Completed:
  - removed the remaining redundant imports in `MLF.Elab.Phi.Omega.Interpret`, `MLF.Elab.Elaborate.Scope`, `MLF.Elab.Elaborate.Algebra`, `MLF.Elab.Elaborate.Annotation`, and `MLF.Elab.Run.ResultType`;
  - confirmed the tree now rebuilds cleanly under `-Werror`.
- Verification:
  - `cabal build all --ghc-options='-fforce-recomp -Werror'`: PASS
  - `cabal test`: PASS
- Rolling priorities (next):
  1. Keep the tree warning-free as future refactors land.
  2. Prefer deleting redundant imports over suppressing warnings.
  3. Revisit only if a later compiler/toolchain change introduces new warnings.

## Task 81 Retire dead `MLF.Elab.TermClosure.closeTermWithSchemeSubst` export (completed 2026-03-10)

- Completed:
  - removed the unused `closeTermWithSchemeSubst` export and definition from `MLF.Elab.TermClosure`;
  - added a focused `PipelineSpec` Phase 6 guard proving the dead term-closure helper stays absent.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "TermClosure no longer exposes closeTermWithSchemeSubst"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6"'`: PASS (`192 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Dead-export loop is complete; keep the new guards green during later refactors.
  2. Reopen this area only if a future sweep finds another genuinely dead internal export.
  3. Preserve the archived loop packet as the authoritative execution record.

## Task 80 Retire dead `MLF.Elab.Run.Debug.edgeOrigins` export (completed 2026-03-10)

- Completed:
  - removed the unused `edgeOrigins` export and definition from `MLF.Elab.Run.Debug`;
  - added a focused `PipelineSpec` ga-scope guard proving the dead debug helper stays absent.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Debug no longer exposes edgeOrigins"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`: PASS (`114 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Revalidate `closeTermWithSchemeSubst` immediately before deciding whether to retire it.
  2. Keep the new ga-scope debug guard green during later cleanup.
  3. Close the loop after the final medium-confidence row resolves.

## Task 79 Retire dead `MLF.Constraint.Canonicalizer.canonicalizeRef` (completed 2026-03-10)

- Completed:
  - removed the unused `canonicalizeRef` export and definition from `MLF.Constraint.Canonicalizer`;
  - added a focused `CanonicalizerSpec` guard proving the dead helper stays retired.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "canonicalizeRef stays retired"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Canonicalizer"'`: PASS (`6 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Revalidate `edgeOrigins` immediately before deciding whether to retire it.
  2. Keep the new canonicalizer guard green during later cleanup.
  3. Finish the remaining medium-confidence rows one by one.

## Task 78 Retire stale `Binding.Validation.validateSingleGenRoot` export (completed 2026-03-10)

- Completed:
  - removed `validateSingleGenRoot` from the `MLF.Binding.Validation` export list while keeping the helper local;
  - added a focused `BindingSpec` guard proving the stale export stays absent.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Validation no longer exports validateSingleGenRoot"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Binding.Tree"'`: PASS (`52 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Revalidate `canonicalizeRef` immediately before deciding whether to retire it.
  2. Keep the new binding guard green during later cleanup.
  3. Continue resolving the medium-confidence export rows one by one.

## Task 77 Retire dead `ChiQuery` bind-parent passthrough exports (completed 2026-03-10)

- Completed:
  - removed the unused `chiLookupBindParent` and `chiBindParents` exports/definitions from `MLF.Elab.Run.ChiQuery`;
  - added a focused `PipelineSpec` chi-first guard proving those retired query aliases stay absent.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ChiQuery no longer exposes chiLookupBindParent or chiBindParents"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first"'`: PASS (`14 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Continue the dead-export loop with `Binding.Validation.validateSingleGenRoot`.
  2. Keep the chi-first guard slice green during later cleanup.
  3. Revalidate medium-confidence dead exports immediately before mutating them.

## Task 76 Retire dead `instEdgeOwnerM` state-access export (completed 2026-03-10)

- Completed:
  - removed the unused `instEdgeOwnerM` export and definition from `MLF.Constraint.Presolution.StateAccess`;
  - added a focused `PipelineSpec` guard proving the retired state-access export/signature stay absent.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "retired instEdgeOwnerM export stays absent"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution state access guard"'`: PASS (`3 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`: PASS (`12 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Keep the state-access / façade guards green during future presolution cleanup.
  2. Reopen this area only if another genuinely unused helper remains after call-site rechecks.
  3. Prefer broader cleanups only when they still have a clear thesis-facing payoff.

## Task 75 Retire `pendingWeakenOwners` from the `EdgeUnify` façade (completed 2026-03-10)

- Completed:
  - removed the remaining `pendingWeakenOwners` re-export from `MLF.Constraint.Presolution.EdgeUnify`;
  - updated `MLF.Constraint.Presolution.Driver` and `MLF.Constraint.Presolution.EdgeProcessing` to import the helper directly from `MLF.Constraint.Presolution.EdgeUnify.Omega`;
  - added a focused `PresolutionFacadeSpec` source guard proving the helper now bypasses the façade.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "pendingWeakenOwners bypasses the EdgeUnify façade"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`: PASS (`4 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`: PASS (`7 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`: PASS (`12 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Optionally trim the apparently dead `instEdgeOwnerM` export if a similarly bounded pass confirms there are still no live call sites.
  2. Keep `pendingWeakenOwners` owned by `MLF.Constraint.Presolution.EdgeUnify.Omega`; do not regrow the `EdgeUnify` façade surface.
  3. If future cleanup in this seam widens beyond import ownership, rerun a broader verification gate.

## Task 74 Backlog reset and next-step triage (completed 2026-03-10)

- Completed:
  - audited the live `tasks/todo/` folders against `CHANGELOG.md`, `implementation_notes.md`, `TODO.md`, `Bugs.md`, and the current source guards;
  - confirmed the old `Solved.fromPreRewriteState` verifier blocker is stale by rerunning the focused `ga scope` slice successfully on the current tree;
  - closed the stale 2026-03-09 verifier/thinker folders whose targets were already landed or superseded by current repository state;
  - narrowed the remaining bounded cleanup seam to `pendingWeakenOwners` ownership on the `MLF.Constraint.Presolution.EdgeUnify` façade.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ga scope"'`: PASS (`2 examples, 0 failures`)
  - tracker audit: `Bugs.md` open section is empty; landed guards still exist for `WithCanonicalT`, `rtvSchemeBodyTarget`, and `preferGenScope`
- Rolling priorities (next):
  1. Decide whether `pendingWeakenOwners` should remain re-exported by `MLF.Constraint.Presolution.EdgeUnify` or move to direct `EdgeUnify.Omega` imports in `Driver` / `EdgeProcessing`.
  2. If that pass stays narrow, consider trimming the apparently dead `instEdgeOwnerM` export in `MLF.Constraint.Presolution.StateAccess` as the adjacent cleanup.
  3. Keep `tasks/todo/` limited to genuinely active work; archive one-off verifier/thinker research once its target is landed or superseded.

## Task 73 Restore warning-free build after stabilization landing (completed 2026-03-10)

- Completed:
  - removed the redundant `EdgeArtifacts` import and shadowed local names in presolution expansion/edge-processing helpers;
  - initialized the new `psPendingWeakenOwners` field across the remaining `PresolutionState` test fixtures in `Presolution.WitnessSpec`;
  - fixed the residual test-hygiene warnings in `Presolution.InstantiateSpec`, `NormalizeSpec`, and `InertSpec`.
- Verification:
  - `cabal build all --ghc-options=-fforce-recomp`: PASS (zero warnings)
  - `cabal test`: PASS
- Rolling priorities (next):
  1. Keep the tree warning-free during future refactors by rerunning a forced rebuild when state-record or façade splits change.
  2. Prefer small local warning fixes over suppressions.
  3. Revisit only if a future compiler/toolchain update introduces new warnings.

## Task 72 Stabilize and land the post-split refactor loop (completed 2026-03-10)

- Completed:
  - froze the live split-tree baseline from the current `master` workspace and re-verified the existing split state with `cabal build all && cabal test`;
  - confirmed Loop 1 required no source cleanup beyond the existing warning-free state (`cabal build all` stayed green with no new warnings);
  - added explicit Loop 2/3/4 stabilization guards in `test/PipelineSpec.hs` and `test/RepoGuardSpec.hs` covering thin split façades, current public-topology docs, and Cabal internal-child ownership for the split modules;
  - ran the ordered Loop 5 owner sweeps for `Omega`, `EdgeUnify`, `Reify.Core`, `Solve`, and `Elaborate`, then reran the full gate;
  - synced the task packet/docs and archived the completed run.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Repository guardrails"'`: PASS (`7 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Loop 2 split-facade guard: runtime facades stay thin and child-owned"'`: PASS (`1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Presolution facade"'`: PASS (`3 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Public surface contracts"'`: PASS (`8 examples, 0 failures`)
  - ordered owner sweeps via the built `mlf2-test` binary: PASS for `Phi alignment`, `WitnessDomain`, `Pipeline (Phases 1-5)`, `Phase 4 — OpRaise for interior nodes`, `Phase 2 — Merge/RaiseMerge emission`, `EdgeTrace`, `Phase 3 — Witness normalization`, `Phase 4 thesis-exact unification closure`, `Phase 6 — Elaborate (xMLF)`, `ga scope`, `Generalize shadow comparator`, `MLF.Constraint.Solved`, `Phase 5 -- Solve`, `Phase 7 typecheck`, and `Phase 7 theorem obligations`
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Keep the new split-façade / public-topology / Cabal-ownership guards green during future cleanup.
  2. Reopen this area only if a real thesis-faithfulness bug or ownership regression appears.
  3. Treat the archived task packet as the source of truth for the landed stabilization loop.

## Task 71 Restore green baseline after solved-facade test/import drift (completed 2026-03-09)

- Completed:
  - replaced the remaining test/frozen-parity snapshot callers of `Solved.fromPreRewriteState` with `SolvedFacadeTestUtil.solvedFromSnapshot`;
  - updated `SolvedFacadeTestUtil.solvedFromSnapshot` to replay snapshots through `SolveSnapshot` / `solveResultFromSnapshot` / `Solved.fromSolveOutput`, matching the strict checked path without reopening the public facade;
  - wired `SolvedFacadeTestUtil` into `exe:frozen-parity-gen` and merged the repair on `master` as `165c6dff8f85a7ca2a8a2a4c1627ce6fe9f405eb` (branch commit `658717fcfaa25c063c3e24440ae879ad719ca93e`).
- Verification:
  - `rg -n 'Solved\.fromPreRewriteState|\bfromPreRewriteState\b' test src src-public -g '!src/MLF/Constraint/Solved/Internal.hs'`: PASS
  - `cabal build mlf2-test`: PASS
  - `cabal test mlf2-test --test-show-details=direct`: PASS (`1010 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Resume the simplification orchestrator from the repaired `master` baseline.
  2. Keep the solved-snapshot replay seam green during future solved-facade and parity cleanup.
  3. Revisit only if a thesis-backed boundary change requires a different exposed test seam.

## Task 70 Remove remaining live fallback mechanisms for thesis-exactness (completed 2026-03-08)

- Completed:
  - removed the live GA→no-GA→reify ladders from elaboration/runtime/result-type generalization;
  - removed the planner synthesized-wrapper body-root→wrapper-root owner fallback and the generic `inferInstAppArgsFromScheme` fallback branch;
  - removed the residual let chooser, recursive generalization callback, and recursive scheme fallback;
  - made `reifyInst` witness/domain-only apart from exact source-scheme reuse for already-authoritative annotations;
  - updated the remaining fallback-dependent sentinels/corpora (`BUG-2026-02-06-002`, `BUG-2026-02-08-004`, nested-let alignment, dual annotated coercion consumers) to strict fail-fast expectations and regenerated frozen parity.
- Verification:
  - `cabal build all && cabal test`: PASS
  - final full gate: `998 examples, 0 failures`
  - `Phi alignment`: PASS (`7 examples, 0 failures`)
  - `Paper alignment baselines`: PASS (`25 examples, 0 failures`)
  - `Phase 4 — Principal Presolution`: PASS (`164 examples, 0 failures`)
  - `Instantiation inference strictness`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Keep the fallback-removal guards green during future elaboration/runtime cleanup.
  2. Prefer witness-/scheme-authoritative fail-fast behavior over new compatibility recovery paths.
  3. Revisit only if a thesis citation demonstrates a removed fallback was semantically required.

## Task 69 Retire final non-must-stay solved facade helper cluster (completed 2026-03-08)

- Completed:
  - removed `lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, and `canonicalizedBindParents` from the public `MLF.Constraint.Solved` facade;
  - replaced their owner-local use with direct constraint/canonical logic in `Reify.Core`, `MLF.Constraint.Presolution.View`, and the solved-view parity tests.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `MLF.Constraint.Solved`: PASS (`51 examples, 0 failures`)
  - `migration guardrail: thesis-core boundary matches legacy outcome`: PASS (`1 example, 0 failures`)
  - `PresolutionView mirrors solved canonical/node/bound queries`: PASS (`1 example, 0 failures`)
  - `final reify/view helper cluster is absent from the Solved facade`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Keep the public `Solved` facade restricted to the thesis-relevant core.
  2. Only revisit `Solved` again if the must-stay core itself can be replaced by an equally explicit boundary.
  3. Keep the full solved-facade guard stack green during future cleanup.

## Task 68 Relocate `pruneBindParentsSolved` behind Finalize (completed 2026-03-08)

- Completed:
  - removed `pruneBindParentsSolved` from the public `MLF.Constraint.Solved` facade;
  - kept the implementation owner-local behind `MLF.Constraint.Finalize` / `MLF.Constraint.Solved.Internal` and updated the one test caller to use `Finalize.stepPruneSolvedBindParents`.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `MLF.Constraint.Solved`: PASS (`49 examples, 0 failures`)
  - `checked-authoritative does not adapt solved via prune helper at entry`: PASS (`1 example, 0 failures`)
  - `prune helper is absent from the Solved facade`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Relocate the remaining production read-query helpers (`lookupNode`, `lookupBindParent`, `bindParents`, `lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents`) into stable owner-local or non-facade homes.
  2. Keep the solved-facade guard stack green during future cleanup.
  3. Preserve explicit replay-faithful and original↔canonical semantics while shrinking the facade further.

## Task 67 Move solved test/audit helper bundle behind test utility (completed 2026-03-08)

- Completed:
  - added `test/SolvedFacadeTestUtil.hs` as the test-only home for `mkTestSolved`, `classMembers`, `originalNode`, `originalBindParent`, `wasOriginalBinder`, and `validateOriginalCanonicalAgreement`;
  - removed that helper bundle from the public `MLF.Constraint.Solved` facade and added a direct solved-facade guard in `test/Constraint/SolvedSpec.hs`.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `MLF.Constraint.Solved`: PASS (`48 examples, 0 failures`)
  - `WitnessDomain`: PASS (`23 examples, 0 failures`)
  - `ga scope`: PASS (`2 examples, 0 failures`)
  - `test-only helper bundle is absent from the Solved facade`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Move `pruneBindParentsSolved` into `MLF.Constraint.Finalize`, where its only live owner already is.
  2. Then relocate the remaining production read-query helpers (`lookupNode`, `lookupBindParent`, `bindParents`, `lookupVarBound`, `genNodes`, `weakenedVars`, `isEliminatedVar`, `canonicalizedBindParents`) into their local owner modules or stable non-facade homes.
  3. Keep the new solved-facade guard stack green during future cleanup.

## Task 66 Retire dead raw canonical container accessors (completed 2026-03-08)

- Completed:
  - removed `canonicalBindParents` and `canonicalGenNodes` from the `Solved` facade and internal implementation;
  - added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting those raw canonical container accessors are absent from the facade.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `MLF.Constraint.Solved`: PASS (`46 examples, 0 failures`)
  - `raw canonical container accessors are absent from the Solved facade`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Retire the remaining test/audit-only enumeration helpers from the public `Solved` facade (`allNodes`, `instEdges`) by moving their test usage to direct constraint inspection.
  2. Continue shrinking the public facade by moving test-only helpers off it before touching the production-read query cluster.
  3. Keep the new raw-container absence guard green during future cleanup.

## Task 65 Relocate remaining shared `Solved` compatibility builders (completed 2026-03-08)

- Completed:
  - split `MLF.Constraint.Solved` into a thin facade plus non-exposed `MLF.Constraint.Solved.Internal`;
  - moved `fromConstraintAndUf` and `rebuildWithConstraint` off the public `Solved` surface and into local owner-module usage in `MLF.Constraint.Finalize` and `MLF.Reify.Core`;
  - updated public solved tests to use `mkTestSolved` and added a direct facade-absence guard for the relocated compat builders.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `MLF.Constraint.Solved`: PASS (`45 examples, 0 failures`)
  - `migration guardrail: thesis-core boundary matches legacy outcome`: PASS (`1 example, 0 failures`)
  - `GeneralizeEnv stores canonical maps, not solved handles`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Keep `fromSolved` and `solvedFromView` local to their owner modules and continue shrinking those compatibility seams only when their callers disappear.
  2. Continue narrowing the public `Solved` facade only where replay-faithful and original↔canonical semantics remain explicit.
  3. Keep the new solved-facade absence guard green during future cleanup.

## Task 64 Narrow `geRes` to canonical map (completed 2026-03-08)

- Completed:
  - replaced `GeneralizeEnv.geRes :: Solved` with `geCanonicalMap :: IntMap.IntMap NodeId` in the presolution planning context;
  - removed `buildSolvedFromPresolutionView` from `MLF.Constraint.Presolution.Plan` and preserved the sanitized canonical map directly from `PresolutionView` data;
  - added a direct migration guard in `test/PresolutionSpec.hs` asserting the planning layer stores canonical maps rather than solved handles.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `GeneralizeEnv stores canonical maps, not solved handles`: PASS (`1 example, 0 failures`)
  - `Phase 4 — Principal Presolution`: PASS (`161 examples, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
- Rolling priorities (next):
  1. Relocate the remaining compatibility builders (`fromConstraintAndUf`, `rebuildWithConstraint`, `fromSolved`, `solvedFromView`) into their local owner modules.
  2. Keep the new presolution migration guard green during future `Solved` cleanup.
  3. Continue narrowing the solved boundary only where the original↔canonical and replay-faithful semantics remain explicit.

## Task 63 Retire dead `Solved` mutation hooks (completed 2026-03-08)

- Completed:
  - removed `rebuildWithNodes`, `rebuildWithBindParents`, and `rebuildWithGenNodes` from `MLF.Constraint.Solved`;
  - added a direct migration guard in `test/Constraint/SolvedSpec.hs` asserting those hooks are absent from the `Solved` surface.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `dead mutation hooks are absent from the Solved surface`: PASS (`1 example, 0 failures`)
  - `MLF.Constraint.Solved`: PASS (`44 examples, 0 failures`)
- Rolling priorities (next):
  1. Narrow `geRes :: Solved` to the `canonicalMap` it actually uses.
  2. Relocate compatibility builders (`fromConstraintAndUf`, `rebuildWithConstraint`, `fromSolved`, `solvedFromView`) into their local owner modules.
  3. Keep the new dead-hook migration guard green during future `Solved` cleanup.

## Task 62 Solved ecosystem classification table closeout (completed 2026-03-08)

- Completed:
  - expanded `docs/architecture.md` from a coarse `Solved` cleanup note into a full grouped 3-column classification of the `Solved` surface and adjacent solved-related seams;
  - recorded the authoritative evidence matrix in the 2026-03-08 solved classification audit notes, covering every exported `Solved` symbol plus the main view/finalize/reify/planner compatibility seams;
  - locked the thesis-exact cleanup rule to preserve replay-faithful construction, original↔canonical correspondence, and strict solved-graph validation while relocating compat glue and retiring dead/test-only production surface.
- Verification:
  - static audit counts: PASS (`32` export entries, `13` direct `src/` importers, `12` direct `test/` importers, `6` named adjacent seams)
  - `MLF.Constraint.Solved`: PASS (`43 examples, 0 failures`)
  - `chi-first guard: runtime and reify modules no longer adapt Solved through fromSolved`: PASS (`1 example, 0 failures`)
- Rolling priorities (next):
  1. Use the new classification table as the sole architectural reference for future `Solved` cleanup decisions.
  2. Keep `docs/architecture.md` and the evidence matrix in sync when solved/view/finalize/reify call sites change.
  3. Treat narrowing `geRes`, relocating compat builders, and removing dead mutation hooks as separate follow-up changes with targeted verification.

## Task 61 Deduplicate low-risk helper pairs (completed 2026-03-08)

- Completed:
  - extracted `freshNameLike` into `MLF.Util.Names` and `mapBoundType` into `MLF.Elab.Types`;
  - removed the duplicate local helper definitions from `MLF.Frontend.Normalize`, `MLF.Reify.TypeOps`, `MLF.Elab.Run.TypeOps`, and `MLF.Constraint.Presolution.Plan.Finalize`;
  - added direct source guards for the shared helper homes.
- Verification:
  - `freshNameLike is shared via MLF.Util.Names`: PASS (`1 example, 0 failures`)
  - `mapBoundType is shared via MLF.Elab.Types`: PASS (`1 example, 0 failures`)
  - `MLF.Frontend.Normalize`: PASS (`5 examples, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`978 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep `MLF.Util.Names` and `MLF.Elab.Types` as the single helper homes for those utility functions.
  2. Keep the new source guards green when touching name generation or bound mapping logic.
  3. Continue architectural cleanup only where semantic audits say it is safe.

## Task 60 Shared frontend/XMLF parser scaffolding (completed 2026-03-08)

- Completed:
  - extracted shared lexer/literal helpers into `MLF.Parse.Common` and shared type-parser scaffolding into `MLF.Parse.Type`;
  - rewired `MLF.Frontend.Parse` and `MLF.XMLF.Parse` to consume that common core while keeping term/computation grammar entrypoints separate;
  - added a direct source guard proving the duplicated lexer/type-helper block no longer lives in both parser modules.
- Verification:
  - `frontend and XMLF parsers share lexer/type scaffolding modules`: PASS (`1 example, 0 failures`)
  - `Frontend eMLF parser`: PASS (`30 examples, 0 failures`)
  - `xMLF parser`: PASS (`8 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`976 examples, 0 failures`)
- Rolling priorities (next):
  1. Continue Task 58 queue item 3 (`schemeBodyTarget` audit/consolidation) only as a separate semantics-checked refactor.
  2. Keep the new parser scaffolding dedup guard green when touching either parser.
  3. Preserve the XMLF-specific binder-list stopping rule unless new tests justify changing it.

## Task 59 Canonicalization helper extraction (completed 2026-03-08)

- Completed:
  - extracted the duplicated canonicalization helpers (`buildCanonicalMap`, `chaseUfCanonical`, `equivCanonical`, `nodeIdKey`) into `MLF.Constraint.Canonicalization.Shared`;
  - rewired `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` to share that single implementation;
  - added a direct source guard preventing both modules from reintroducing local copies of the helper block.
- Verification:
  - `Canonicalization helper dedup guards`: PASS (`1 example, 0 failures`)
  - `PresolutionView mirrors solved canonical/node/bound queries`: PASS (`1 example, 0 failures`)
  - `Canonicalizer`: PASS (`5 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`975 examples, 0 failures`)
- Rolling priorities (next):
  1. Continue Task 58 queue item 2 (shared frontend/XMLF parser scaffolding) only as a separate follow-up.
  2. Keep the new canonicalization dedup guard green when touching solved/view reconstruction.
  3. Preserve thesis-faithful canonicalization semantics while extracting any future shared helper blocks.

## Task 58 Haskell dedup refactor queue (completed 2026-03-08)

- Goal:
  - record the next Haskell-oriented deduplication/refactor candidates in strict one-by-one order, so follow-up work stays focused and easy to verify.
- Queue (do in order):
  1. Extract canonicalization helpers shared by `MLF.Constraint.Solved` and `MLF.Constraint.Presolution.View` (`buildCanonicalMap`, `chaseUfCanonical`, `equivCanonical`, `nodeIdKey`) into one internal utility module. (completed 2026-03-08)
  2. Commonize shared lexer/type-parser scaffolding between `MLF.Frontend.Parse` and `MLF.XMLF.Parse`, while preserving grammar-specific entrypoints and avoiding forced unification of the full term grammars. (completed 2026-03-08)
  3. Audit and consolidate `schemeBodyTarget` between `MLF.Elab.Elaborate` and `MLF.Elab.Run.Scope`, but only if the more complete run-scope semantics can be adopted without changing elaboration behavior. (completed 2026-03-08)
  4. Extract the low-risk pure helper duplicates (`mapBound`, `freshNameLike`) into shared internal helpers after the higher-value refactors are done. (completed 2026-03-08 via Task 61)
- Completed:
  - removed the duplicate local `schemeBodyTarget` from `MLF.Elab.Elaborate` and made `MLF.Elab.Run.Scope` the single owner of scheme-target selection helpers;
  - kept `schemeBodyTarget` as the richer thesis `S′`-style subterm target selector and added `generalizeTargetNode` there for the `S`-style named-node generalization case that nested-let / alias elaboration still needs;
  - added direct `ScopeSpec` coverage for named non-scheme-root, scheme-root, forall-body, canonical scheme-body alias, and generalization-target behavior, plus a `PipelineSpec` source guard that `Elaborate` no longer defines `schemeBodyTarget ::`.
- Verification:
  - `schemeBodyTarget` — PASS (`6 examples, 0 failures`)
  - `nested` — PASS (`27 examples, 0 failures`)
  - `BUG-002-V2` — PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test` — PASS (`1004 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep `MLF.Elab.Run.Scope` as the single owner of scheme-target selection helpers.
  2. Preserve the thesis `S` vs `S′` distinction when touching named-node generalization or subterm translation.
  3. Keep the new scope/source guards green during future elaboration cleanup.
- Constraints:
  - do not batch multiple queue items into one change;
  - keep thesis-faithfulness guards green after each item;
  - treat parser commonization and `schemeBodyTarget` consolidation as behavior-sensitive refactors that require targeted tests before code movement.

## Task 57 Guard-first surface `Expr` fold refactor (completed 2026-03-08)

- Completed:
  - added direct row1 desugaring-contract coverage in `test/FrontendDesugarSpec.hs`;
  - added manual recursion-schemes support for `Expr 'Surface ty` and refactored `MLF.Frontend.Desugar.desugarSurface` to use `cata` only on the surface AST;
  - kept `MLF.Frontend.Normalize` explicit and unchanged as the binder/capture-sensitive boundary.
- Verification:
  - `MLF.Frontend.Desugar`: PASS (`4 examples, 0 failures`)
  - `desugars annotated lambda parameters via let`: PASS (`1 example, 0 failures`)
  - `ELet with EAnn RHS does not create explicit-scheme instantiation structure`: PASS (`1 example, 0 failures`)
  - `row1 closeout guard|checked-authoritative`: PASS (`2 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`974 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep the new direct row1 desugaring guards green during future frontend cleanup.
  2. Keep `MLF.Frontend.Normalize` explicit unless a fresh binder/capture audit widens scope.
  3. Continue the frontend preprocessing roadmap only inside the row1 audit boundary unless a guard exposes a let-scope mismatch.

## Task 56 Remove final χp `...View` alias duplicates (completed 2026-03-07)

- Completed:
  - removed the remaining duplicate `...View` / `...FromView` aliases from runtime and reify helpers now that the unsuffixed APIs are already `PresolutionView`-typed;
  - updated runtime, result-type, elaboration, Phi, and test call sites to use the canonical unsuffixed names only;
  - added a direct source guard that the duplicate alias names are retired from runtime and reify modules.
- Verification:
  - `ga scope`: PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved`: PASS (`1 example, 0 failures`)
  - `duplicate ...View aliases are retired from runtime and reify modules`: PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`970 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep the runtime/reify helper surface on one canonical `PresolutionView`-typed name per operation.
  2. Keep the row2/chi-first guard stack green during future elaboration cleanup.
  3. Continue Task 52’s frontend preprocessing guard-first work before any recursion-schemes refactor.

## Task 55 χp/view-native elaboration closeout (completed 2026-03-07)

- Completed:
  - removed non-test/non-legacy `fromSolved` usage from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`;
  - promoted `PresolutionView` to the primary internal/runtime API across the elaboration/runtime helper surface while keeping `fromSolved` only in `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests;
  - updated tests/internal facades to pass explicit `PresolutionView`s and added source guards for the wrapper retirement.
- Verification:
  - `chi-p global cleanup guard: runtime elaboration helpers no longer import fromSolved`: PASS (`1 example, 0 failures`)
  - `chi-p wrapper retirement guard: primary helper signatures are PresolutionView-native`: PASS (`1 example, 0 failures`)
  - `resolveCanonicalScope propagates binding tree cycle errors`: PASS (`1 example, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
  - `row2 absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test`: PASS (`969 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep `fromSolved` confined to `MLF.Constraint.Presolution.View`, `MLF.Elab.Legacy`, and tests.
  2. Keep the row2/chi-first source guards green during future runtime/reify cleanup.
  3. Continue Task 52’s frontend preprocessing guard-first work before any recursion-schemes refactor.

## Task 55 Finish χp/view-native elaboration cleanup (completed 2026-03-07)

- Completed:
  - removed the remaining non-legacy `fromSolved` wrappers from `MLF.Elab.Run.Scope`, `MLF.Elab.Run.TypeOps`, `MLF.Elab.Run.Generalize`, `MLF.Elab.Run.ResultType.Util`, and `MLF.Reify.Core`;
  - made `PresolutionView` the primary internal/runtime API across those elaboration/reify helpers while confining `fromSolved` to the presolution boundary, `MLF.Elab.Legacy`, and tests;
  - updated internal facades/tests to pass explicit `PresolutionView`s and added a direct source guard that runtime/reify modules no longer adapt `Solved` through `fromSolved`.
- Verification:
  - `ga scope`: PASS (`2 examples, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
  - `runtime and reify modules no longer adapt Solved through fromSolved`: PASS (`1 example, 0 failures`)
  - `row2 absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `ResultType|Phase 6 — Elaborate|chi-first gate stays green`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`969 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep `fromSolved` confined to the presolution boundary, legacy path, and tests.
  2. Keep the row2/chi-first guard stack green during future elaboration/reify cleanup.
  3. Continue Task 52’s frontend preprocessing guard-first work before any recursion-schemes refactor.

## Task 54 Retire library-side Φ test hooks (completed 2026-03-07)

- Completed:
  - removed `MLF.Elab.Phi.TestOnly` and `MLF.Elab.Phi.IdentityBridge` from `mlf2-internal`;
  - moved pure witness-domain ranking helpers into the test suite (`test/Phi/WitnessDomainUtil.hs` + `test/Phi/WitnessDomainSpec.hs`) and switched `GeneralizeSpec` to direct production imports;
  - kept production `Ω` on the direct replay-spine fail-fast contract while replacing the old `IdentityBridge` dependency with a tiny local diagnostic helper.
- Verification:
  - `WitnessDomain`: PASS (`23 examples, 0 failures`)
  - `Generalize shadow comparator`: PASS (`8 examples, 0 failures`)
  - `no-trace test entrypoint fails fast with MissingEdgeTrace`: PASS (`1 example, 0 failures`)
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `elab-input absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard`: PASS (`1 example, 0 failures`)
  - `cabal build all && cabal test`: PASS (`966 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep `mlf2-internal` free of new test-only helper modules.
  2. Keep the row9-11 direct-target guards green during future `Ω` cleanup.
  3. Continue Task 52’s frontend preprocessing guard-first work before any recursion-schemes refactor.

## Task 53 Thesis-exact Φ identity cleanup (completed 2026-03-07)

- Completed:
  - removed the stale compiled `MLF.Elab.Phi.Binder` helper surface and retired its re-exports from `MLF.Elab.Phi`;
  - kept runtime `Ω` on the direct replay-spine fail-fast contract, while tightening `IdentityBridge` notes/tests so it is explicitly a witness-domain utility/test surface rather than a runtime repair engine;
  - added a dedicated row9-11 facade source guard and an `OpGraft` missing-from-spine regression next to the existing `OpWeaken` fail-fast coverage.
- Verification:
  - `row9-11 facade cleanup guard`: PASS (`1 example, 0 failures`)
  - `row9-11 direct-target guard`: PASS (`1 example, 0 failures`)
  - `OpWeaken on binder target missing from quantifier spine fails fast`: PASS (`1 example, 0 failures`)
  - `OpGraft on binder target missing from quantifier spine still fails fast even when IdentityBridge finds witness-domain matches`: PASS (`1 example, 0 failures`)
  - `IdentityBridge`: PASS (`24 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`966 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep the row9-11 facade/direct-target guards green during future Phi/Omega cleanup.
  2. Continue Task 52’s frontend preprocessing guard-first work before any recursion-schemes refactor.
  3. Keep `IdentityBridge` in the utility/test lane unless a fresh verifier-owned thesis audit widens scope.

## Task 51 Thesis-exact recursion-refactor goal loop (completed 2026-03-07)

- Completed:
  - studied the thesis and the live Haskell codebase specifically for thesis-safe recursion-schemes and simplification work;
  - authored a new 8-row mechanism table that mixes positive tree-refactor targets with explicit graph-phase guardrails;
  - added a simplified improving-loop prompt whose `Planner` owns thesis/code research and evidence reconciliation;
  - added a matching JSONL event-log template plus design/implementation-plan docs for the campaign;
  - ran the fresh verifier sweep requested by the campaign and closed all 8 rows to `YES` against the live code/test/thesis evidence;
  - added direct row5 production-path `Γ_{a′}` anchors/tests for the lambda-side and let-side environment rules;
  - completed the row7 exhaustive traversal inventory and the row8 explicit graph-phase non-goal guardrail, so the recursion-refactor mechanism table is now fully green.
- Rolling priorities (next):
  1. Keep the 8-row recursion-refactor guard stack green under future frontend/elaboration/presolution cleanup.
  2. Restrict future recursion-schemes work to the row7 inventory and require row-specific verifier evidence before widening scope.
  3. Preserve the row5 production-path environment anchors and the row8 graph-phase non-goal guardrail through future changes.

## Task 52 Surface preprocessing exactness round-1 planner (completed 2026-03-08 via Task 57)

- Completed:
  - reconciled thesis `§12.3.2.2` / `§15.3.8`, supplementary `xmlf` desugaring notes, and the live frontend code/tests for alias-bound normalization plus coercion-based annotation lowering;
  - added the direct preprocessing-contract guards originally queued here across normalization/desugaring traversals (`ELamAnn`, `EAnn`, typed-let sugar, alias-bound normalization before Phase 1);
  - completed the bounded tree-only follow-up in Task 57 while keeping `MLF.Frontend.Normalize` explicit as the binder/capture-sensitive boundary.
- Rolling priorities (next):
  1. Keep the direct row1 frontend guards green during future frontend cleanup.
  2. Keep `MLF.Frontend.Normalize` explicit unless a fresh binder/capture audit widens scope.
  3. If a new guard fails because of scope placement rather than preprocessing shape, explicitly expand to `Let-Scope Translation Discipline` instead of patching across rows silently.

## Task 50 TMT fresh round-2 closeout (completed 2026-03-07)

- Completed:
  - closed row2 `Result-type context wiring` by retiring the live solved-compat adapter path from `Pipeline` / `ResultType.View` / `ChiQuery` and extending the absolute row2 source guard;
  - closed row8 `Translatability normalization` by putting §15.2.8 all-inert `W`-normalization on the live presolution path and adding a dedicated row8 guard;
  - refreshed `test/golden/legacy-replay-baseline-v1.json` so frozen parity now captures the new thesis-exact solved artifacts;
  - revalidated the closeout guard stack (`row1`, `row2`, `row3`, `row8`, `checked-authoritative`, `Dual-path verification`, `Frozen parity artifact baseline`, full gate).
- Verification:
  - `row2 absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `row2 closeout guard`: PASS (`3 examples, 0 failures`)
  - `row8 thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `Translatable presolution`: PASS (`10 examples, 0 failures`)
  - `O15-TRANS`: PASS (`5 examples, 0 failures`)
  - `O05-`: PASS (`3 examples, 0 failures`)
  - `Frozen parity artifact baseline`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Keep the row1/2/3/8 guards, frozen parity oracle, `checked-authoritative`, `Dual-path verification`, and full gate mandatory for future thesis-alignment changes.
  2. Reconcile the stale `MLF.Elab.Phi.Binder` cleanup bug against the now-closed row10 table entry, since the helper surface is compiled but not used on the active runtime path.
  3. Keep replay/witness and Ω direct-target guard stacks green when future presolution changes land.

## Task 49 TMT per-row fresh review (completed 2026-03-07)

- Completed:
  - Reviewed all 14 Transformation Mechanism Table rows with one fresh agent per row against the newest codebase and `papers/these-finale-english.txt`.
  - Refreshed row wording/evidence for rows 1, 3, 6, 12, and 13.
  - Reopened two thesis-faithfulness gaps:
    - row2 `Result-type context wiring` is not fully thesis-exact while the live path still seeds `PresolutionView` from `Solved` and validates through `ChiQuery.chiSolved`;
    - row8 `Translatability normalization` is not fully thesis-exact while §15.2.8 all-inert `W` normalization is not on the live production path.
  - Recorded the reopened gaps in `Bugs.md` and updated the live TMT note accordingly.
- Rolling priorities (next):
  1. Remove the hidden solved-compat adapter from the live row2 result-type path.
  2. Decide whether to implement live all-inert `W` normalization for row8 or keep that row explicitly scoped to Definition 15.2.10 / Theorem 15.2.11 only.
  3. Keep the fresh-review-validated guard suites and full gate mandatory while fixing reopened rows.

## Task 48 Row6 replay-contract recovery after orchestrated max-retry (completed 2026-03-06)

- Closed:
  - Recovered row6 replay-contract handling from the post-orchestrator red
    baseline in isolated worktree
    `/Users/ares/.config/superpowers/worktrees/mlf4/row6-replay-contract-recovery-20260306`.
  - `WitnessNorm` now evaluates no-replay wrapper-vs-semantic behavior in
    source-domain identity space, preserving baseline success paths while
    keeping strict no-replay fail-fast on bug-002-style residual weakens.
  - Rogue no-replay replay-family behavior is narrowed to producer-boundary
    rejection of residual single-target source-interior grafts; wrapper
    `OpRaise` artifacts are pruned before Phi and true invalid type-tree raises
    still fail during normalization.
  - Follow-up stale-witness shaping landed in the same recovery worktree:
    `WitnessNorm` now prunes only dead non-root `OpWeaken` residue after
    finalized source/replay binder domains are known, restoring
    `let-c1-apply-bool` to `Int` without relaxing strict Ω behavior for live
    non-root weakens (`BUG-002-V4` remains preserved under lambda).
- Verification:
  - no-replay witness obligations: PASS
  - `checked-authoritative`: PASS
  - `Dual-path verification`: PASS
  - `cabal build all && cabal test`: PASS (`956 examples, 0 failures`)
- Follow-up status:
  - Historical note: the fresh round-2 goal-table/orchestrator sweep had reported all 14 mechanisms `YES`.
  - Superseded on 2026-03-07 by the stricter per-row fresh review: row2 and row8 are reopened thesis-faithfulness gaps, while row6 remains closed.
- Rolling priorities (next):
  1. Keep source-domain replay-contract behavior covered when future
     presolution changes touch copy-map/canonicalization logic.
  2. Fix reopened row2 and row8 gaps before claiming end-to-end TMT closure again.
  3. Keep the closed-row thesis-exact guard suites and `cabal build all &&
     cabal test` mandatory for future thesis-alignment changes.

## Task 40 Elaboration-input strict-policy docs closeout (completed 2026-03-04)

- Closed:
  - TMT row `Elaboration input` is now `Thesis-exact = Yes` under the strict
    criterion (`thesis exact includes test-only code paths`).
  - Test-only Φ helper surfaces no longer carry solved-typed signatures.
  - No-trace helper remains strict fail-fast (`MissingEdgeTrace`).
- Verification (required for strict reclassification):
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)
- Rolling priorities (next):
  1. Task 43: simplify per-edge propagation transform toward uniform
     expansion+unification handling (retire synthesized-wrapper special-case
     path where thesis-equivalent behavior can be preserved).
  2. Task 36: trim non-active compatibility aliases in generalize/reify
     internals while preserving checked-authoritative behavior.
  3. Keep closeout guard slices plus full-gate verification mandatory for each
     follow-up reduction.

## Task 41 Elaboration-input absolute thesis-exact hardening (completed 2026-03-05)

- Completed:
  - Added RED->GREEN guard `elab-input absolute thesis-exact guard`.
  - Removed residual solved-backed Phi env surface in `MLF.Elab.Phi.Env`
    (`peResult`/`askResult` removed).
  - Removed ga' scope error swallowing in `MLF.Elab.Run.Scope`
    (`Left _ -> ref` fallback removed; errors now propagate).
  - Retired synthetic auto-trace test helper path by removing
    `phiFromEdgeWitnessAutoTrace` from `MLF.Elab.Phi.TestOnly` and migrating
    affected tests; no-trace path remains strict `MissingEdgeTrace` fail-fast.
  - Added focused `ScopeSpec` coverage and registered it in test harness/cabal.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team-implementation-plan.md`
  - Historical task tracker folder for this 2026-03-05 work was not retained as a standalone archive directory.
- Verification:
  - RED baseline before Wave 1:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input absolute thesis-exact guard"'`
      -> FAIL (expected RED).
  - GREEN gates after integration:
    - `--match "elab-input absolute thesis-exact guard"`: PASS (`1 example, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`8 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
    - `cabal build all && cabal test`: PASS (`934 examples, 0 failures`)

## Task 42 TMT row-2 absolute thesis-exact hardening (completed 2026-03-05)

- Completed:
  - Added RED->GREEN source guard `row2 absolute thesis-exact guard`.
  - Removed residual ResultType-local solved-overlay surfaces in
    `MLF.Elab.Run.ResultType.View` (`rtvSolved`, `rtvOriginalConstraint`,
    `solveFromInputs`).
  - Migrated row2 consumers (`Ann`, `Fallback`, `Util`) to
    `PresolutionView`/view-native scope and reify/generalize helpers.
  - Kept strict malformed-view fail-fast behavior at
    `buildResultTypeView` boundary.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team-implementation-plan.md`
  - Historical task tracker folder for this 2026-03-05 work was not retained as a standalone archive directory.
- Verification:
  - RED baseline before Wave 1:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
      -> FAIL (`1 example, 1 failure`).
  - GREEN gates after integration:
    - `--match "row2 absolute thesis-exact guard"`: PASS (`1 example, 0 failures`)
    - `--match "row2 closeout guard"`: PASS (`3 examples, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`8 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`935 examples, 0 failures`)

## Task 44 TMT row `Ordering of transformations` thesis-exact agent-team execution (completed 2026-03-05)

- Completed:
  - Added RED->GREEN row3 ordering guard coverage and Phase-4 semantic
    characterization for edge-local `OpWeaken` interior consistency.
  - Removed Driver-global post-loop `flushPendingWeakens` and extracted explicit
    post-loop finalization stage (`materialize -> rewrite/canonicalize ->
    rigidify -> witness normalize`) with construction checkpoints.
  - Integrated edge-loop weaken-flush scheduling in `EdgeProcessing` and
    preserved per-edge unify-closure fail-fast boundaries.
  - Resolved integration regressions discovered during full-gate runs:
    `generalizes reused constructors via make const`, `BUG-002-V1`,
    and frozen parity baseline mismatch.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md`
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-tmt-ordering-of-transformations-thesis-exact-agent-team-plan/`
- Verification:
  - RED baseline:
    - `--match "row3 ordering thesis-exact guard"`: FAIL (`2 examples, 2 failures`)
  - GREEN gates:
    - `--match "row3 ordering thesis-exact guard"`: PASS (`2 examples, 0 failures`)
    - `--match "Phase 4 thesis-exact unification closure"`: PASS (`8 examples, 0 failures`)
    - `--match "Translatable presolution"`: PASS (`8 examples, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`8 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - Final gate:
    - `cabal build all && cabal test`: PASS
    - `cabal test mlf2-test --test-show-details=direct`: PASS (`938 examples, 0 failures`)
- Status:
  - Row remains `Thesis-exact = No` pending a strict per-edge (not loop-final)
    weaken-flush schedule that preserves current regression/parity guarantees.

## Task 45 TMT row3 absolute ordering follow-up execution (completed 2026-03-05)

- Completed:
  - Added strict RED->GREEN row3 matcher
    `row3 absolute thesis-exact guard`.
  - Introduced owner-aware pending-weaken API surfaces and rewired
    `EdgeProcessing` to owner-boundary scheduling markers.
  - Removed loop-final-only fallback shape from edge-loop scheduling path.
  - Resolved Wave-3 boundary regression (`BUG-2026-03-05-002`) where pending
    weaken owner buckets could remain after boundary checks.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row3-ordering-absolute-thesis-exact-agent-team-implementation-plan.md`
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-row3-ordering-absolute-thesis-exact-agent-team-replan/`
- Verification:
  - RED baseline before Wave 1:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
      -> FAIL (`4 examples, 4 failures`).
  - GREEN gate stack:
  - `row3 absolute thesis-exact guard`
  - `Phase 4 thesis-exact unification closure`
  - `Translatable presolution`
  - `generalizes reused constructors via make const`
  - `BUG-002-V1`
  - `Frozen parity artifact baseline`
  - `checked-authoritative`
  - `Dual-path verification`
  - `cabal build all && cabal test`
  - Gate counts:
    - `row3 absolute thesis-exact guard`: PASS (`4 examples, 0 failures`)
    - `Phase 4 thesis-exact unification closure`: PASS (`10 examples, 0 failures`)
    - `Translatable presolution`: PASS (`8 examples, 0 failures`)
    - `generalizes reused constructors via make const`: PASS (`1 example, 0 failures`)
    - `BUG-002-V1`: PASS (`1 example, 0 failures`)
    - `Frozen parity artifact baseline`: PASS (`1 example, 0 failures`)
    - `checked-authoritative`: PASS (`8 examples, 0 failures`)
    - `Dual-path verification`: PASS (`4 examples, 0 failures`)
    - Final gate: `cabal build all && cabal test` PASS
      (`942 examples, 0 failures` from `mlf2-test` log summary).
- Status:
  - Row remains `Thesis-exact = No` under strict criterion; ordering is now
    owner-boundary scheduled (not loop-final-only), but still uses
    compatibility-conservative boundary flushing.

## Task 46 Elaboration-input witness-authoritative strictness plan (completed 2026-03-08)

- Completed:
  - removed the residual Elaborate scope-root fallback swallowing by making `scopeRootFromBase` propagate base binding-path failures instead of collapsing them to `typeRef root`;
  - added the focused source guard `elab-input witness-authoritative guard` so future row-1 cleanup cannot silently reintroduce `Left _ -> typeRef root` in `MLF.Elab.Elaborate`;
  - closed the narrowed follow-up after Task 70, which had already retired the broader fallback ladders originally tracked here.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-05-elaboration-input-witness-authoritative-agent-team-implementation-plan.md`
  - `/Volumes/src/mlf4/tasks/archive/2026-03-05-elaboration-input-witness-authoritative-agent-team-plan/`
- Verification:
  - `elab-input witness-authoritative guard`: PASS (`1 example, 0 failures`)
  - `elab-input absolute thesis-exact guard`: PASS (`1 example, 0 failures`)
  - `checked-authoritative`: PASS (`9 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`1005 examples, 0 failures`)
- Rolling priorities (next):
  1. Keep the new row-1 witness-authoritative guard green during future elaboration cleanup.
  2. Preserve fail-fast handling for malformed base binding trees unless a thesis citation requires a recovery path.
  3. Treat any future elaboration-input simplification as guard-first work anchored to the existing row-1 verification stack.

## Task 47 TMT row3 strict owner-boundary scheduling closeout (completed 2026-03-05)

- Completed:
  - Removed the old flush-all-owner fallback shape from
    `MLF.Constraint.Presolution.EdgeProcessing`.
  - Made pending-weaken owner provenance stable by stamping owner buckets at
    enqueue time in `MLF.Constraint.Presolution.EdgeUnify` and preserving that
    provenance through boundary flush selection.
  - Kept strict post-boundary/finalization fail-fast checks and improved
    diagnostics to include pending owner buckets.
  - Strengthened row3 absolute guard slices to detect flush-all-owner fallback
    regression.
- Verification:
  - `--match "row3 absolute thesis-exact guard"`: PASS (`6 examples, 0 failures`)
  - `--match "Phase 4 thesis-exact unification closure"`: PASS (`11 examples, 0 failures`)
  - `--match "Translatable presolution"`: PASS (`8 examples, 0 failures`)
  - `--match "generalizes reused constructors via make const"`: PASS (`1 example, 0 failures`)
  - `--match "BUG-002-V1"`: PASS (`1 example, 0 failures`)
  - `--match "Frozen parity artifact baseline"`: PASS (`1 example, 0 failures`)
  - `--match "checked-authoritative"`: PASS (`8 examples, 0 failures`)
  - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS
- Rolling priorities (next):
  1. Row 4 (`Per-edge propagation transform`): retire synthesized-wrapper
     special-case and enforce uniform expansion+unification semantics.
  2. Keep row1/row2/row3 strict guard suites mandatory while reducing later
     TMT non-thesis scaffolding.

## Task 32 TMT row-1 chi-first elab/generalize closeout (completed 2026-03-03)

- Completed:
  - `ElabEnv` no longer carries `eeSolvedCompat`.
  - `elaborateWithEnv` no longer performs entry-time
    `Solved.rebuildWithConstraint`.
  - Runtime production path stays single-path and checked-authoritative.
- Verification:
  - Requested closeout matcher:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 closeout guard|checked-authoritative|Dual-path verification"'`
    - PASS with `0 examples, 0 failures` (empty selection).
  - Required narrow fallback matchers:
    - `--match "row1 closeout guard"`: PASS (`2 examples, 0 failures`)
    - `--match "checked-authoritative"`: PASS (`7 examples, 0 failures`)
    - `--match "Dual-path verification"`: PASS (`4 examples, 0 failures`)
  - Final gate:
    - `cabal build all && cabal test`: PASS.
- Status:
  - Row-1 boundary closeout is complete.
  - Row-2 adapter retirement (`rtcSolvedCompat` / `rtcSolveLike`) remains.

## Task 33 TMT row-2 result-type adapter retirement (completed 2026-03-04)

- Completed:
  - `ResultTypeInputs` no longer exposes `rtcSolvedCompat`.
  - `MLF.Elab.Run.ResultType.Types` no longer exposes `rtcSolveLike`.
  - `ElabConfig` no longer includes `ecSolved`.
  - Runtime production path remains single-path and checked-authoritative.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-row2-resulttype-adapter-retirement-agent-team-implementation-plan.md`
  - Historical task tracker folder for this 2026-03-04 work is no longer present in the live todo-task tree.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
    - PASS (`3 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`929 examples, 0 failures`)
- Status:
  - Row-2 adapter retirement is closed.

## Task 34 Post-row2 runtime simplification priorities (completed via Task 42)

- Closed:
  - Priority (1) solved-overlay materialization reduction is completed in
    Task 42 (`ResultType.View` no longer exposes/materializes row2-local
    solved overlays).
  - Priority (2) compatibility-heavy row2 generalize/reify signatures were
    reduced by migrating consumer call chains to view-native helpers in
    Task 42.
  - Priority (3) guard slices remain active and are now reinforced with the
    new `row2 absolute thesis-exact guard`.

## Task 35 TMT elaboration-input thesis-exact closeout (completed 2026-03-04)

- Completed:
  - Added thesis-exact elaboration-input source guards and drove RED->GREEN
    migration over Wave 0..3.
  - Active elaboration input path now satisfies closeout guards:
    - no active-path `ChiQuery.chiSolved` dependency in `elaborateWithEnv`;
    - active Elaborate/Phi callback aliases avoid solved-typed
      generalize-at input requirements.
  - Checked-authoritative behavior remained unchanged on representative slices.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team-implementation-plan.md`
  - Historical task tracker folder for this 2026-03-04 work is no longer present in the live todo-task tree.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)

## Task 37 Team E verifier docs closeout for Task 5 (completed 2026-03-04)

- Completed:
  - Updated `docs/notes/2026-02-27-transformation-mechanism-table.md`
    so row `Elaboration input` is `Thesis-exact = Yes` with current
    code/guard references.
  - Synced `implementation_notes.md`, `CHANGELOG.md`, and task tracker files
    under
    the historical 2026-03-04 remediation tracker (no longer present in the live todo-task tree)
    for final Wave 3 verifier closeout.
- Verification evidence recorded (already-run gates):
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS

## Task 38 Elaboration-input thesis-exact replan with agent teams (completed 2026-03-04)

- Completed outcomes by wave:
  - Wave 0 (`guards`): tightened source guards to assert active `χp`-native
    elaboration input wiring.
  - Wave 1 (`phi-core`): active Φ trace entry/core signatures now use
    `GeneralizeAtWith` + `PresolutionView` in production paths.
  - Wave 2 (`callsites`): active `reifyInst`/pipeline call chain passes
    `presolutionView` directly into `phiFromEdgeWitnessWithTrace`.
  - Wave 3 (`verification`): required gates are green:
    - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
    - `checked-authoritative`: PASS (`8 examples, 0 failures`)
    - `Dual-path verification`: PASS (`4 examples, 0 failures`)
    - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)
  - Wave 4 (`docs-closeout`): TMT/implementation/changelog/task tracker synced
    and task folder archived.
- Plan + archive:
  - `/Volumes/src/mlf4/docs/plans/2026-03-04-elaboration-input-thesis-exact-agent-team-replan.md`
  - Historical task tracker folder for this 2026-03-04 replan was not retained as a standalone archive directory.
- Next work:
  1. Task 34 follow-on is complete: `ResultType.View` is now narrowed to overlay-aware queries only; keep the row-2 guard green if future result-type cleanup lands.
  2. Task 36: trim remaining non-active compatibility aliases in
     generalize/reify internals while keeping closeout guard slices mandatory.

## Task 39 Elaboration-input strict legacy-retirement closeout (completed 2026-03-04)

- Completed:
  - Retired solved-typed elaboration/Phi compatibility APIs from production
    modules for this row closeout.
  - Migrated test-only Phi callback surfaces to chi-native shape and preserved
    strict no-trace fail-fast behavior.
  - Updated TMT row `Elaboration input` to `Thesis-exact = Yes` only after
    required closeout gates passed in the integrated workspace.
- Plan/tracker:
  - `/Volumes/src/mlf4/docs/plans/2026-03-04-elab-input-thesis-exact-legacy-retirement-agent-team-implementation-plan.md`
  - Historical task tracker folder for this 2026-03-04 closeout was not retained as a standalone archive directory.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
    - PASS (`2 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
    - PASS (`8 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
    - PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`
    - PASS (`931 examples, 0 failures`)

## Task 36 Post-task35 compatibility cleanup (closed as stale 2026-03-08)

- Closed:
  - the named compatibility aliases from the original plan are already gone from production (`GeneralizeAtWithCompat`, `rtcSolvedCompat`, `eeSolvedCompat`);
  - later completed tasks absorbed the remaining live elaboration/result-type cleanup, so there is no standalone compatibility-alias work item left here;
  - remaining `fromSolved` usage is boundary-/legacy-only and should be tracked separately if it ever needs further reduction.

## Task 31 χp-first elaboration/result-type internal cleanup (completed 2026-03-03)

- Completed:
  - Added shared facade `MLF.Elab.Run.ChiQuery` for chi-first runtime queries.
  - Migrated `MLF.Elab.Elaborate` and `MLF.Elab.Run.ResultType.*` internals to
    prefer `PresolutionView` (`χp`) reads over internal solved reconstruction.
  - Kept solved compatibility only at explicit boundary adapters:
    `eeSolvedCompat` in `ElabEnv` and `rtcSolvedCompat`/`rtcSolveLike` in
    result-type inputs.
  - Wired pipeline boundary setup through `mkResultTypeInputs`.
  - Added/updated regression guardrails for:
    - `chi-first guard`
    - `ResultType|Phase 6 — Elaborate|chi-first`
    - `Phase 6 — Elaborate|ResultType|Dual-path verification`
- Verification:
  - Gate A: `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first guard"'` -> PASS
  - Gate B: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ResultType|Phase 6 — Elaborate|chi-first"'` -> PASS
  - Gate C: `cabal build all && cabal test` -> PASS (`923 examples, 0 failures`)
- Archive:
  - Historical task records for this 2026-03-03 work were not retained as a standalone archive directory.

## Task 30 Reduce internal solved compatibility reads (completed 2026-03-03)

- Completed:
  - Removed compatibility-only context fields (`gcConstraintForReify`, `rbConstraintForReify`) and added context-side invariant tracing for unexpected base-domain `SolvedToBaseMissing`.
  - Gated alias solved rebuild in `MLF.Elab.Generalize` and moved explicit-bound helper reification to OnConstraint path where authoritative.
  - Added `MLF.Elab.Run.ResultType.View` and centralized result-type solved reads; confined `rtcSolveLike` usage to the view-construction boundary.
  - Replaced fallback-core local `rebuildWithNodes` path with view-bound overlay materialization and preserved target-selection/base-mutation semantics.
  - Added regressions for:
    - `generalizeWithPlan` fallback ladder (`SchemeFreeVars` GA->no-GA and double fallback to `reifyType`)
    - result-type fallback `gaSolvedToBase` same-domain/missing integrated branches.
- Verification:
  - Focused matrix checks: PASS (carry-forward + 4 new tests).
  - `cabal build all && cabal test` -> `917 examples, 0 failures`.
- Archive:
  - Historical task records for this 2026-03-03 work were not retained as a standalone archive directory.

## Task 29 Solved module audit follow-up execution (completed 2026-03-03)

- Completed:
  - Shared `Solved -> PresolutionView` adapter (`MLF.Constraint.Presolution.View.fromSolved`) and removed duplicated runtime adapter code.
  - Runtime constructor migration to `Solved.fromConstraintAndUf` for planner/reify paths.
  - Explicit typed solved/base fallback handling via `SolvedToBaseResolution`.
  - Added solved-boundary guards for canonical/original invariants, isolated O15 `Trχ(ε)=ε`, and AAnn result-type primary/fallback equivalence with populated GA mappings.
  - Completed review-fix follow-up for test quality and claim precision; full validation remained green.
- Verification:
  - `cabal build all && cabal test` -> `913 examples, 0 failures`.
- Archive:
  - Historical task records for this 2026-03-03 work were not retained as a standalone archive directory.

## Task 28 Eliminate solved indirection via presolution view (completed 2026-03-02)

- Current status:
  - Wave 0/1/2 migration work is complete in this worktree.
  - `PresolutionPlanBuilder` now closes over `PresolutionView` (not `Solved`).
  - `MLF.Constraint.Solved` no longer exposes the production-only `fromPresolutionResult` builder.
  - Hygiene guard added for elaboration entrypoint modules to prevent direct solved imports.
  - Detailed plan and session tracking:
    - `docs/plans/2026-03-02-eliminate-solved-indirection-agent-team-implementation-plan.md`
    - historical task tracker folder not retained as a standalone archive directory
- Next:
  - Optional follow-up: continue reducing internal elaboration compatibility reads of `Solved` (result-type/generalize internals) once architecture allows full replacement without regressing paper baseline slices.

## Task 28 TMT identity row re-audit (closed as stale 2026-03-08)

- Status update (2026-03-08):
  - later row9-11 cleanup work removed the stale compiled `MLF.Elab.Phi.Binder` surface and confined `IdentityBridge` to the witness-domain utility/test lane;
  - the Transformation Mechanism Table row `Identity reconciliation mechanism` is now `Yes`, so this re-audit follow-up no longer represents live work.
- Completed:
  - Re-audited thesis elaboration identity handling against `papers/these-finale-english.txt` §15.3.1-§15.3.6 and the current Phi/Omega implementation.
  - Confirmed the live `IdentityBridge`/Ω path is witness-domain exact (`raw`/`copy`/`trace` only; no class-member fallback).
  - Identified the remaining row-level blocker: production Φ identity handling is still split across runtime reconciliation helpers (`Translate` replay bridge + `IdentityBridge` in `Omega`), and `MLF.Elab.Phi.Binder` still exposes canonical/base-key/copy-map reconciliation.
  - Updated Transformation Mechanism Table row `Identity reconciliation mechanism` to keep the row at `No` for absolute whole-codebase exactness, with the concrete blocker recorded.
- Next:
  - Collapse the active reconciliation object model in Φ (`Translate`/`IdentityBridge`/`Omega`) and retire or rewrite `MLF.Elab.Phi.Binder` so no compiled Phi path preserves a separate non-thesis identity-reconciliation mechanism.

## Task 27 Transformation Mechanism Table thesis-exact classification campaign (completed 2026-03-01)

- Completed:
  - Reclassified every row in `docs/notes/2026-02-27-transformation-mechanism-table.md`
    to `Aligned`.
  - Moved all `DEV-TMT-*` entries from active `deviations` to
    `history.resolved` in `docs/thesis-deviations.yaml`.
  - Added closeout metadata for each `DEV-TMT-*` record:
    - `resolution_date: 2026-03-01`
    - replacing commit references from TMT3 Wave 1 / Wave 2 where relevant
    - regression evidence anchors (`Phi alignment`, `IdentityBridge`,
      `replay-map`, and pipeline closeout slices).
  - Synced implementation/changelog/task docs for final integration readiness.
  - Baseline + targeted campaign slices are green (`cabal build all`, `cabal test`,
    targeted Phi/Presolution/Pipeline slices).
- Status:
  - Transformation-mechanism campaign is closed with all rows aligned on this
    branch.
  - No active `DEV-TMT-*` implementation-choice entries remain.

## Task 26 Phi replay bridge strict pass-through follow-up (completed 2026-03-01)

- Completed:
  - Removed remaining projection-era helper naming from `computeTraceBinderReplayBridge` (`projectOne` -> `validateTarget`) to keep bridge invariant explicit: validate + pass-through only.
  - Added/kept replay codomain fail-fast regression coverage in `ElaborationSpec` and aligned related strict error assertions to the runtime invariant message.
  - Synced docs (`CHANGELOG.md`, `implementation_notes.md`) with strict pass-through follow-up behavior.
  - Verification green: `cabal build all && cabal test` (`894 examples, 0 failures`).
- Status:
  - Runtime replay projection/repair paths are absent from bridge code; replay map domain/codomain contract is enforced fail-fast.

## Task 25 Thesis-Exact Phi Upfront Replay Normalization (completed 2026-02-27)

- Completed:
  - Switched fully to strict `etBinderReplayMap` producer/consumer contract for Phi replay.
  - Presolution replay-map normalization now enforces fail-fast completeness/TyVar-target/injectivity checks.
  - Translate/Omega replay bridge now aligns to scheme quantifier replay IDs and consumes replay targets deterministically.
  - Removed runtime class-member fallback search from non-root `OpWeaken`/`OpGraft` binder recovery paths.
  - `phiFromEdgeWitnessNoTrace` is strict fail-fast (`MissingEdgeTrace`), and no-trace alias-recovery expectations were migrated to fail-fast tests.
  - Verification green: `cabal test mlf2-test --test-show-details=direct` (`883 examples, 0 failures`) and `cabal build all && cabal test`.
- Status:
  - Runtime replay fallback is not active behavior; bridge handling is validate + pass-through only, and source-space replay targets are hard errors.

## Task 24 Thesis-Exact Unification Ordering + Presolution-Centric Solved Path (completed 2026-02-26)

- Completed:
  - Phase-4 thesis-order closure draining is enforced in presolution (initial + per-edge).
  - Presolution now carries UF metadata (`prUnionFind`) separately from raw translatable graph (`prConstraint`).
  - Shared unification closure engine extracted and reused by Solve/Presolution.
  - Closure engine now supports seeded UF drains (`runUnifyClosureWithSeed`) and presolution enforces no-pending-unify edge boundaries.
  - Presolution producer contract now fails fast on residual queues/TyExp and missing non-trivial witness/trace coverage.
  - Aligned native solved construction to replay-finalized semantics (`Solved.fromPresolutionResult` uses snapshot replay finalization).
  - Switched production pipeline to presolution-native solved path and removed temporary dual-run legacy parity guard after green-window validation.
  - Removed transitional runtime projection-first entrypoint; kept parity checks in test-only dual-path harness.
  - Removed canonical-domain Solved query exports after migrating call sites.
  - Tightened Phi identity resolution to strict witness-domain-first defaults with explicit class-fallback API/telemetry.
  - Expanded legacy/native parity regressions (including bounded/coercion-heavy anchors).
  - Full suite validation: `cabal test mlf2-test --offline` => `838 examples, 0 failures`.
- Next:
  - Done: removed explicit legacy fallback entrypoint and migrated parity checks to frozen baseline artifacts.
  - Optional follow-up: retire snapshot-only compatibility surfaces once no unit tests depend on `solveUnifyWithSnapshot`/`fromSolveOutput`.

## Task 23 Milestone 5 gap closure (Tasks 14-16) — completed 2026-02-26

- Completed:
  - Ω `OpWeaken` now recovers binder elimination targets via solved equivalence classes instead of skipping to `ε` on alias targets.
  - IdentityBridge source-key expansion now includes `Solved.classMembers`, enabling canonical-alias binder recovery.
  - IdentityBridge binder-index ranking now preserves raw binder identity before class-member fallback when multiple binders share one solved class.
  - Verified `Solved.unionFind` escape hatch remains removed and has no call sites in `src/` or `test/`.
  - Deviation/claims docs audited for Task 17 closure:
    - `DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP` is absent from `docs/thesis-deviations.yaml`.
    - `docs/thesis-claims.yaml` remains synchronized (`CLM-PHI-CORRECTNESS` has `deviations: []`).
    - `./scripts/check-thesis-claims.sh` passes (`21 claims, 4 deviations`).
  - Added regressions:
    - `OpWeaken on an alias target recovers binder via equivalence class and emits InstElim`
    - `includes solved class members for canonical alias recovery`
    - `preserves raw binder identity before class-member fallback`
    - `uses class-member fallback when target has no exact binder key`
  - Full gate: `cabal build all && cabal test` (`827 examples, 0 failures`).

## Task 22 Defensible Exactness — completed 2026-02-22

- Scope:
  - Move from "tested" to "defensible exactness": every thesis claim has a traceable evidence chain (thesis clause -> claim -> code path -> test -> gate) continuously enforced by CI.
- Implemented:
  - Machine-checked thesis claims registry (`docs/thesis-claims.yaml`, 21 claims across Ch. 7-15).
  - Deviation register (`docs/thesis-deviations.yaml`, 5 deviations) with cross-link validation.
  - Claims checker script (`scripts/check-thesis-claims.sh`) enforcing schema, cross-links, code paths, and orphan detection.
  - Obligations ledger `supports_claims` back-links.
  - Three new test modules: `TranslatablePresolutionSpec` (Def. 15.2.10), `PhiSoundnessSpec` (Def. 15.3.4), `ExpansionMinimalitySpec` (Def. 10.1.1).
  - Conformance gate upgraded with claims checker and three new anchor matchers.
  - Paper-map migrated to reference machine-checked artifacts.
  - Spec drift closed: all open `.kiro` spec tasks annotated with deferred notes and deviation cross-references.
  - Three claims upgraded from `partial` to `defended`.
- Verification:
  - `cabal build all && cabal test` (781 examples, 0 failures)
  - `./scripts/thesis-conformance-gate.sh` (PASS)
  - `./scripts/check-thesis-claims.sh` (PASS)

## Task 21 Phase 7 theorem obligations executable proxies — completed 2026-02-19

- Scope:
  - Address formalization debt by making Phase 7 theorem obligations executable via property-style proxies.
- Implemented:
  - Added `/Volumes/src/mlf4/test/TypeSoundnessSpec.hs` with:
    - preservation proxy property:
      - if `typeCheck t = Right tau` and `step t = Just t'`, then `typeCheck t' = Right tau`
    - progress proxy property for closed terms:
      - if `typeCheck t = Right tau` and `t` is closed, then `isValue t || isJust (step t)`
  - Wired `TypeSoundnessSpec` into:
    - `/Volumes/src/mlf4/mlf2.cabal` (`test-suite mlf2-test` `other-modules`)
    - `/Volumes/src/mlf4/test/Main.hs`
  - Added thesis gate anchor:
    - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`
    - matcher `--match "Phase 7 theorem obligations"` with minimum `2` matched examples.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 7 theorem obligations"'`
  - `./scripts/thesis-conformance-gate.sh`
  - `cabal build all && cabal test`

## Task 20 Formal Obligations Ledger (Chapters 14/15) — completed 2026-02-19

- Scope:
  - Build and enforce a canonical thesis rule-to-code-to-test ledger for Chapters 4–15 operational obligations (originally scoped to Ch. 14/15, expanded to cover sections `4.2`–`15.3`).
- Implemented:
  - Added canonical YAML ledger:
    - `/Volumes/src/mlf4/docs/thesis-obligations.yaml`
  - Added generated Markdown ledger:
    - `/Volumes/src/mlf4/docs/thesis-obligations.md`
  - Added renderer/checker scripts:
    - `/Volumes/src/mlf4/scripts/render-thesis-obligations-ledger.rb`
    - `/Volumes/src/mlf4/scripts/check-thesis-obligations-ledger.sh`
  - Wired checker as a required stage in:
    - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`
  - Added/updated stable `O*` anchor matchers across:
    - `/Volumes/src/mlf4/test/TypeCheckSpec.hs`
    - `/Volumes/src/mlf4/test/ReduceSpec.hs`
    - `/Volumes/src/mlf4/test/ElaborationSpec.hs`
    - `/Volumes/src/mlf4/test/Presolution/EnforcementSpec.hs`
    - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs`
    - `/Volumes/src/mlf4/test/Presolution/MergeEmissionSpec.hs`
- Closure criteria status:
  - [x] 99 obligations present and uniquely mapped (scope expanded from Ch. 14/15 to Ch. 4–15).
  - [x] All obligations marked `status=anchored`.
  - [x] Executable matcher anchors required and passing.
  - [x] Markdown drift checked from YAML source.
  - [x] Thesis conformance gate includes obligations stage.
  - [x] Full verification green (`cabal build all && cabal test`).

## Task 19 Thesis Conformance Gate Command/Profile — completed 2026-02-18

- Scope:
  - Add one canonical thesis-anchor gate command and a required CI profile that continuously enforces paper anchors.
- Implemented:
  - Added gate command:
    - `/Volumes/src/mlf4/scripts/thesis-conformance-gate.sh`
  - Added required CI workflow:
    - `/Volumes/src/mlf4/.github/workflows/thesis-conformance.yml`
  - Gate anchors enforced by focused matcher runs with minimum example-count checks:
    - Φ/Ω translatability matrix rows (`--match "R-"`, min 15)
    - A6 parity regressions (`--match "A6 parity"`, min 3)
    - strict A6 bug closure (`--match "BUG-2026-02-17-002"`, min 1)
    - representative theorem baseline (`--match "has type forall a. a -> a"`, min 1)
    - phase-3 equivalence guard (`--match "Phase 3 atomic wrapping equivalence gates"`, min 7)
- Verification:
  - `./scripts/thesis-conformance-gate.sh`
  - `cabal build all && cabal test`

## Task 18 BUG-2026-02-17-002 A6 applied bounded/coercion expected-pass gap — completed 2026-02-17

- Scope:
  - Close the applied bounded/coercion-heavy A6 variant so it typechecks to thesis-expected `Int`.
- Current status:
  - Resolved in `/Volumes/src/mlf4/Bugs.md` (`BUG-2026-02-17-002`, resolved 2026-02-17).
  - Regression now asserts strict success in `/Volumes/src/mlf4/test/PipelineSpec.hs`:
    - `BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines`.
- Verification (green):
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity"'`
  - `cabal build all && cabal test`

## Task 17 BUG-2026-02-17-001 Φ keep-key + Graft/Raise/Weaken drift — completed 2026-02-17

- Root cause (deterministic seed `529747475`):
  - `computeTargetBinderKeys` retained replay keys even when target binders were empty.
  - Unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples in Ω expanded into over-complex instantiations that bottomized identity-shaped baselines.
  - Annotation baselines also needed localized fallback when `AAnnF` sees `inst == InstId` with an expected bound for non-variable annotation sources.
- Implemented fixes:
  - `MLF.Elab.Phi.Translate`: strict keep-key intersection with actual target binders.
  - `MLF.Elab.Phi.Omega`:
    - preserve spine Raise alias/eliminate behavior in empty-context cases,
    - collapse unbounded same-binder `OpGraft -> OpRaise -> OpWeaken` triples to direct `InstApp`,
    - refine spine Raise bound handling for inferred-variable cases.
  - `MLF.Elab.Elaborate` (`AAnnF`):
    - keep generic non-var fallback disabled in `reifyInst`,
    - add non-variable annotation-local fallback from `InstId` to `InstInside (InstBot expectedBound)`.
- Verification (green):
  - `id y should have type`, `elaborates polymorphic instantiation`, `elaborates term annotations`,
    `term annotation can instantiate a polymorphic result`, `explicit forall annotation preserves foralls in bounds`.
  - `BUG-002-V` (`seed 1593170056`), `BUG-003-V` (`seed 1925916871`), `BUG-004` (`seed 1593170056`), OpRaise interior guard repro.
  - `cabal build all` passes.
- Full-gate closure:
  - residual failures were closed with:
    - `MLF.Elab.Phi.Omega.resolveTraceBinderTarget` fail-fast invariant for trace-source binder ops with no replay binder candidate,
    - non-spine `OpRaise` context-path translation for non-`⊥` bounds when `C^m_n` is available,
    - `PipelineSpec` canonicalization assertion tightened to require non-empty rewrites only when solve `union-find` is non-empty.
  - verification: `cabal build all && cabal test` passes (`678 examples, 0 failures`).

## Task 16 BUG-2026-02-11-004 regression reopen (bounded-alias strict-success) — completed 2026-02-17

- Targeted closure landed:
  - `MLF.Elab.Phi.Translate`: trace-free `computeTargetBinderKeys` now returns empty keep-key set (`mTrace = Nothing`), restoring StepIntro + weaken interleaving behavior.
  - `MLF.Elab.Elaborate`: removed variable-annotation conversion `InstInside (InstBot t) -> InstApp t` in `AAnnF`; this was producing `InstApp TBottom` for BUG-003 use sites.
- Current targeted matrix (seed `1925916871`) is green:
  - `BUG-003-V` (V1/V2) => PASS
  - `BUG-003-PRES` => PASS
  - `interleaves StepIntro with Omega ops in Φ translation` => PASS
  - `BUG-004` (V1..V4) => PASS
- Follow-up closure:
  - full gate is green (`cabal build all && cabal test`), and BUG tracker entries are closed in `/Volumes/src/mlf4/Bugs.md`.

## Task 15 BUG-2026-02-16-001/002 planner scheme-introducer crash closure — completed 2026-02-17

- Root cause:
  - `planEdge` resolved `eprSchemeOwnerGen` strictly from TyExp body root.
  - For synthesized-wrapper fixtures (`ExpVarId < 0`) with sparse bind-parent topology, body root had no direct `GenRef` ancestor while wrapper root did, causing:
    - `InternalError "scheme introducer not found for NodeId {getNodeId = 0}"`.
- Implemented fix:
  - `MLF.Constraint.Presolution.EdgeProcessing.Planner` now uses `resolveSchemeOwnerGen`:
    - non-synth TyExp path remains strict (`findSchemeIntroducerM` on body root),
    - synthesized-wrapper path falls back from body-root lookup to wrapper-root lookup.
  - Added helper `firstGenOnPath` (via `bindingPathToRootUnderM`).
  - `test/Presolution/EdgePlannerSpec.hs` let/ann bug repros now also assert `eprSchemeOwnerGen == GenNodeId 0`.
- Verification (green):
  - `--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064`
  - `--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064`
  - `--match "Edge plan types" --seed 1481579064`
  - `--match "Edge interpreter" --seed 1481579064`
- Tracker sync:
  - moved `BUG-2026-02-16-001` and `BUG-2026-02-16-002` to **Resolved** in `/Volumes/src/mlf4/Bugs.md`.

## Task 14 BUG-2026-02-16-010 bridge-domain regression follow-up — closed as stale 2026-03-08

- Status update (2026-03-08):
  - the narrowed BUG-004/BUG-002 follow-ups called out here were closed by later 2026-02-17 bug-fix tasks (`Task 16`, `Task 17`, `Task 18`), so this section is now historical rather than actionable.
- Bridge hardening + replay-hint/positional replay seeding landed for Φ→Ω binder-target dispatch.
- Progress update:
  - strict matrix `make-app keeps codomain Int without bottom-domain collapse` is green again.
  - full gate still shows remaining replay-domain under-coverage in other phase-6 matrix paths (`BUG-002-V2`, `BUG-004-V2`, etc.).
- Immediate follow-up priorities:
  - characterize replay-map under-coverage cases (`traceBinderSources` includes keys that are semantically valid targets but absent from replay-map domain);
  - refine bridge-map construction/eligibility so fail-fast only fires on true contract violations;
  - keep BUG-003 bridge contract test and source-domain `OpRaise` interior alias regression green while restoring remaining affected anchors.
- Verification target:
  - green targeted matrix for BUG-002/BUG-004 + strict target matrix;
  - then rerun full gate: `cabal build all && cabal test`.
- 2026-02-17 investigation update (systematic-debugging, BUG-002):
  - deterministic slice remains red (`5 examples, 4 failures`) with buckets: `BUG-002-V1` spine mismatch, `BUG-002-V2/V3` replay key-space mismatch, and `PipelineSpec BUG-002-V4` interior guard regression.
  - root mismatch is cross-domain: presolution trace/hint identity space vs replay substitution key-space used by Ω binder lookup.
  - four minimal bridge-map hypotheses were tested and reverted; next step should be an architecture-level replay-domain normalization pass before additional local fixes.
- 2026-02-17 completion update:
  - replay-key normalization contract landed across trace/hint restoration, bridge construction, and Ω binder lookup.
  - deterministic `BUG-002-V` slice is now green with seed `1593170056` (`5 examples, 0 failures`).
  - follow-up bound normalization in `ReifyPlan` now rewrites binder self-references in bounds to `⊥`, removing residual V2 alias-finalization drift while keeping strict alias-bound rejection.
  - residual cross-link remains: `BUG-004-V2` is still red in this workspace (`TCArgumentMismatch`), so BUG-2026-02-16-010 stays open as a narrowed follow-up.

## Task 13 BUG-2026-02-16-007/008 sentinel drift closure — completed 2026-02-16

- Implemented generalization fallback alignment for plain `SchemeFreeVars`:
  - `MLF.Elab.Run.Pipeline` root generalization now retries `SchemeFreeVars` the same as `BindingTreeError GenSchemeFreeVars` and falls back to direct reification.
  - `MLF.Elab.Run.ResultType.Util.generalizeWithPlan` now uses the same fallback policy.
- Updated BUG-003-V1/V2 sentinels in `test/ElaborationSpec.hs` to track the stabilized strict-instantiation failure class (`InstBot expects TBottom`) rather than transient `SchemeFreeVars (__rigid24)`.
- Targeted verification (green):
  - exact BUG-003-V1 repro command (seed `1481579064`)
  - exact BUG-003-V2 repro command (seed `1481579064`)
  - `--match "BUG-003-V"` matrix slice (`2 examples, 0 failures`)
- Tracker sync:
  - `BUG-2026-02-16-007` and `BUG-2026-02-16-008` moved to resolved in `/Volumes/src/mlf4/Bugs.md`.
  - `BUG-2026-02-11-004` remains open as the underlying bounded-alias thesis-faithfulness gap.

## Task 11 Source-domain `I(r)` closure — completed 2026-02-16 (BUG-2026-02-14-003)

- Implemented surgical Ω/Φ domain contract fix:
  - `MLF.Elab.Phi.Omega`: `OpRaise` admissibility checks now use source-domain `etInterior` directly.
  - Added invariant diagnostic for alias-only membership (`source target ∉ I(r)` but copy-map alias ∈ `I(r)`).
  - `OpRaise` semantic execution now adopts source->copied node (`etCopyMap`) before canonicalization.
  - `MLF.Elab.Phi.Translate`: canonicalize `etInterior` only for `namedSet` intersection.
- Added regressions:
  - `ElaborationSpec`: source-domain `OpRaise` + copy-map alias no longer fails `outside I(r)`.
  - `PipelineSpec`: BUG-002-V4 OpRaise targets remain members of `etInterior` after witness/trace canonicalization.
- Targeted verification (all green):
  - `BUG-002-V4`
  - `BUG-2026-02-06-002 strict target matrix`
  - `BUG-004`
  - `tracks instantiation copy maps for named binders`
  - `witness/trace/expansion canonicalization`
- Full gate:
  - `cabal build all && cabal test` => `672 examples, 9 failures` (open buckets outside this surgical scope).

## Task 12 Non-spine OpRaise context fallback — completed 2026-02-16 (BUG-2026-02-16-009)

- Implemented targeted Φ/Ω non-spine context fix:
  - `MLF.Elab.Phi.Omega`: `OpRaise` now tracks both adopted and source-domain raise targets.
  - When adopted-target non-spine context/root insertion is unavailable, Ω retries root-context insertion using the source-domain raise target.
  - Adopted-target path remains primary to preserve previously fixed BUG-004 call-site behavior.
- Verification (green):
  - `/Phase 6 — Elaborate (xMLF)/Paper alignment baselines/Explicit forall annotation edge cases/explicit forall annotation round-trips on let-bound variables/`
  - `BUG-004`
  - `BUG-002-V4`
  - `BUG-2026-02-06-002 strict target matrix`
  - `contextToNodeBound does not descend through forall body fallback`
- Full gate:
  - `cabal build all && cabal test` => `672 examples, 4 failures` (historical snapshot before BUG-007/008 closure; current open trackers center on `BUG-2026-02-16-001/002` and umbrella `BUG-2026-02-11-004`).

## Task 10 Variant Matrix Scan — completed 2026-02-11 (new bug variants triage)

- Added systematic variant coverage:
  - `test/ElaborationSpec.hs` (`BUG-002-V1..V4`, `BUG-003-V1..V2`, `BUG-004-V1..V4`)
  - `test/Presolution/WitnessSpec.hs` (`US-010-V1..V2`)
- Targeted validation:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Systematic bug variants (2026-02-11 matrix)"'` (`10 examples, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "US-010-V"'` (`2 examples, 0 failures`)
- Outcome:
  - Open regressions currently tracked:
    - `BUG-2026-02-11-002` (extended BUG-002 factory variants)
    - `BUG-2026-02-11-004` (BUG-003 higher-arity bounded-alias variants)
  - `BUG-2026-02-11-003` is resolved thesis-exact: `BUG-004-V2`/`V4` are strict success assertions (`Int`) in both checked and unchecked pipelines, with compat InstBot and reify fallback paths removed.
  - Remaining variant tests for open bugs stay sentinel-guarded until strict expected-success closure.

## Task 9 Verification Gate — completed 2026-02-11 (Phase-3 gate hardening + tracker sync)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`633 examples, 0 failures`)
- Closure summary:
  - Hardened `Phase 3 atomic wrapping equivalence gates` in `test/PipelineSpec.hs`:
    - removed permissive checked mismatch fallback for `make` path
    - enforced explicit `forall a. a -> a` identity-arrow shape for `\y. let id = (\x. x) in id y`
  - Revalidated targeted suites:
    - `--match "Phase 3 atomic wrapping equivalence gates"` (`7 examples, 0 failures`)
    - `--match "BUG-2026-02-06-002"` (`10 examples, 0 failures`)
    - `--match "BUG-2026-02-08-004"` (`1 example, 0 failures`)
  - Synced bug tracker status:
    - `BUG-2026-02-06-002` and `BUG-2026-02-08-004` marked **Resolved** in `/Volumes/src/mlf4/Bugs.md`
    - open bug list currently empty

## Task 7 Verification Gate — completed 2026-02-10 (BUG-2026-02-06-002 closure)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`604 examples, 0 failures`)
- Closure summary:
  - Upstream witness-shape correction finalized in `MLF.Constraint.Presolution.WitnessCanon` (ambiguous graft/weaken rejection + delayed pair coalescing).
  - Ω translation made local in `MLF.Elab.Phi.Omega` (no delayed look-ahead; adjacent graft/weaken rescue only).
  - Named structured-bound preservation corrected in `MLF.Constraint.Presolution.Plan.Normalize`.
  - ALet fallback harmonized in `MLF.Elab.Elaborate` (lam replacement schemes use `IntMap.empty` substitution).
- Follow-up:
  - `BUG-2026-02-06-002` moves to resolved tracking (`/Volumes/src/mlf4/Bugs.md`).

## Task 8 Verification Gate — completed 2026-02-10 (BUG-2026-02-08-004 closure)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (`604 examples, 0 failures`)
- Closure summary:
  - Dedicated `BUG-2026-02-08-004` sentinel in `test/PipelineSpec.hs` now asserts thesis-expected success (`Int`) in both unchecked and checked pipelines.
  - `MLF.Elab.Elaborate` `AApp` now suppresses witness-derived `InstApp` when the elaborated function term is not `∀`-typed, preventing invalid `InstElim` chains on monomorphic arrows.
  - Polymorphic argument-instantiation inference now also runs for variable arguments when the (possibly instantiated) function term typechecks to an arrow, preserving κσ intent for annotated lambda parameters.
- Follow-up:
  - `BUG-2026-02-08-004` moved to **Resolved** in `/Volumes/src/mlf4/Bugs.md`.

## Task 7 Verification Gate — completed 2026-02-08

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (post-fix 5/5 consecutive green after `cb5e51d`)
- Historical context (pre-fix, before `cb5e51d`):
  - Baseline full verification: **FAIL** (`506 examples, 4 failures`; Phase 6 `shadow reify mismatch`)
  - Consecutive tracking:
    - `1/5` — **FAIL** (2026-02-08T07:42:32Z, 2s): `ValidationFailed ["shadow reify mismatch", ... "solved=t14 -> t14", "base=a -> a"]`
    - `2/5` — **FAIL** (2026-02-08T07:42:42Z, 3s): same failure signature
    - `3/5` — **FAIL** (2026-02-08T07:42:51Z, 3s): same failure signature
    - `4/5` — **FAIL** (2026-02-08T07:43:03Z, 3s): same failure signature
    - `5/5` — **FAIL** (2026-02-08T07:43:13Z, 3s): same failure signature
- Post-fix revalidation (exact timestamps, local timezone `+0800`):
  - `1/5` — **PASS** (start `2026-02-08T16:17:43+0800`, end `2026-02-08T16:17:46+0800`)
  - `2/5` — **PASS** (start `2026-02-08T16:17:46+0800`, end `2026-02-08T16:17:49+0800`)
  - `3/5` — **PASS** (start `2026-02-08T16:17:49+0800`, end `2026-02-08T16:17:52+0800`)
  - `4/5` — **PASS** (start `2026-02-08T16:17:52+0800`, end `2026-02-08T16:17:55+0800`)
  - `5/5` — **PASS** (start `2026-02-08T16:17:55+0800`, end `2026-02-08T16:17:58+0800`)
- Follow-up:
  - `BUG-2026-02-08-001` stays in `Bugs.md` under **Resolved**.
  - Runtime base-shadow cutover completed: `MLF.Elab.Generalize` fallback no longer reifies or compares base-path output at runtime; solved-order output is authoritative.
  - `BUG-2026-02-06-002` remains open: current debugging indicates binder-representative filtering in generalization can drop required factory binders (`make` path), causing scheme specialization drift or `SchemeFreeVars`.

## Task 7 Verification Gate — completed 2026-02-09 (H15)

- Command: `cabal build all && cabal test`
- Gate status: **Passed** (H15 guard + regression test)
- Scope:
  - Added targeted regression `PipelineSpec` case for solved-name leakage (`t23`) in the `make` let-mismatch path.
  - Implemented guarded ALam parameter-source selection in `MLF.Elab.Elaborate` (`hasInformativeVarBound`) to avoid copy-node name leakage while preserving informative resolved-node paths.
- Follow-up:
  - `BUG-2026-02-06-002` remains open for the broader polymorphic-factory behavior (`TBottom -> Int` vs expected `b -> a`) after H13/H14/H15.

## Task 7 Priority Plan — completed 2026-02-09 (H16 continuation)

- Selected strategy: **Option 1 — Upstream witness-shape correction**.
- Design doc: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-design.md`
- Implementation plan: `docs/plans/2026-02-09-bug-2026-02-06-002-upstream-witness-shape-correction-implementation-plan.md`
- Sentinel policy while bug remains open: keep `BUG-2026-02-06-002 sentinel matrix` as pending; drive fix with strict RED matrix + witness/Φ regressions, then graduate sentinels to strict assertions at closure.


---

## Phase 1 — Constraint Generation ✅

- [x] Graphic type nodes (`TyVar`, `TyArrow`, `TyBase`, `TyForall`, `TyExp`)
- [x] Binding nodes (`TyForall`) + binding edges (`Constraint.cBindParents`)
- [x] Expansion nodes (`TyExp`) for let-bindings
- [x] Instantiation edges (`InstEdge`)
- [x] Constraint container (`Constraint`)
- [x] `inferConstraintGraph :: Expr -> Either TypeError ConstraintResult`
- [x] Literals produce `TyBase` nodes
- [x] Lambda parameters bound at surrounding binder
- [x] Applications emit instantiation edges
- [x] Let-bindings introduce child binders (`TyForall`)
- [x] Expansion variables shared across multiple uses of same binding
- [x] Variable shadowing / lexical scoping
- [x] Unknown variable error reporting
- [x] `A5 (P3)` Totalize STCon coercion-copy path and remove remaining partial failure branch.
  - 2026-02-18: refactored STCon coercion-copy argument traversal to total `internalizeConArgs` (`NonEmpty`-structured recursion), replaced bare-coercion stringly internal failure with typed `UnexpectedBareCoercionConst`, and added regression coverage (`bare ECoerceConst rejects ...`, `STCon coercion-copy failures surface as typed errors`, `nested STCon coercion-copy preserves binding-tree validity`).

**Tests:** 23 examples, all passing (`cabal test`)

---

## Phase 2 — Normalize / Local Transformations ✅

- [x] Simplify trivial instantiation/unification edges (T ≤ T, T = T)
- [x] `normalize :: Constraint -> Constraint` with fixed-point iteration
- [x] `dropReflexiveInstEdges`, `dropReflexiveUnifyEdges` helpers
- [x] Grafting: copy structure onto variables when demanded by `InstEdge`
- [x] Merging: process `UnifyEdge`s via union-find
- [x] `graftInstEdges`, `mergeUnifyEdges` helpers
- [x] `NormalizeState` with fresh node allocation and union-find

**Tests:** 16 examples, all passing

---

## Phase 3 — Acyclicity Check ✅

- [x] Build instantiation dependency graph
- [x] Topological sort of `InstEdge` list
- [x] Cycle detection (DFS)
- [x] `isAcyclic :: Constraint -> Bool`
- [x] `checkAcyclicity :: Constraint -> Either CycleError AcyclicityResult`
- [x] `collectReachableNodes` for dependency analysis
- [x] `AcyclicityResult` with sorted edges and dependency graph

**Tests:** 41 examples (was 27 new, total 92), all passing

---

## Phase 4 — Principal Presolution ✅

- [x] Topological processing of `InstEdge`s using `AcyclicityResult`
- [x] Minimal expansion lattice implemented: `ExpIdentity`, `ExpInstantiate`, `ExpForall`, `ExpCompose`
- [x] `decideMinimalExpansion` covers forall↔forall (re-gen), forall→structure (instantiate), structure→forall (wrap), structure→structure (identity+unify)
- [x] `applyExpansion`/`instantiateScheme` to realize expansions and graft fresh nodes
- [x] Incremental unification inside presolution loop
- [x] Tests: `test/PresolutionSpec.hs` covers identity, instantiate, forall-intro, and compose (instantiate→forall)
- [x] `A1 (P1)` Strict Ω normalization only (remove permissive fallback path in production).
  - 2026-02-17: closure audit confirmed strict production fail-fast for malformed merge direction (`MergeDirectionInvalid`) with targeted + full gate evidence (`--match R-MERGE-NORM-09`, `--match "fails fast with MergeDirectionInvalid via presolution normalization"`, `cabal build all && cabal test`).
- [x] `A6 (P2)` Add thesis-anchored witness normalization/translatability regression fixtures.
  - 2026-02-10: added strict transitive-flex `OpRaise` regression fixtures in `test/Presolution/WitnessSpec.hs` and `test/Presolution/MergeEmissionSpec.hs` (direct validator + `normalizeEdgeWitnessesM` path).
  - 2026-02-10: completed Fig. 15.3.4 15-row witness matrix closure (`R-GRAFT-VALID-01`..`R-RAISEMERGE-NORM-15`) with row-labeled tests and green matrix/full gates.

## Phase 5 — Unification Solver ✅

- [x] `Solve` entrypoint `solveUnify :: Constraint -> Either SolveError SolveResult`
- [x] Robust union-find: reuse canonical NodeId representatives and path compression
- [x] Structural unification cases: Var=Var, Var=Structure, Arrow=Arrow, Base=Base, Forall=Forall
- [x] Occurs check on DAG
- [x] Error reporting
- [x] Tests: success cases and failure cases

---

## Phase 6 — Elaboration to xMLF (New Foundation) ⏳

Based on `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §3.

- [x] **Define xMLF AST** (`src/MLF/Types/Elab.hs`, re-exported via `MLF.Elab.Types`)
    - [x] Types `τ` (including `∀(α ≥ τ)` and `⊥`)
    - [x] Instantiations `φ` (Witnesses: `!α`, `N`, `O`, `Inside`, `Under`, `Comp`)
    - [x] Terms `a` (including `Λ` and `a φ`)
- [x] **Implement Elaboration Logic** (`src/MLF/Elab/Elaborate.hs`)
    - [x] `elaborate` + `runPipelineElab`
    - [x] Generate instantiation witnesses `φ` from per-edge `EdgeWitness` (Φ)
    - [x] Insert `Λ` abstractions based on presolution plans
    - [x] Insert explicit type annotations on lambda arguments
- [x] **Tests**
    - [x] Elaboration basics (id, const) + let-polymorphism
    - [x] Φ/Σ unit tests + instantiation-soundness checks
- [x] `A2 (P1)` Align pipeline-reported result type with checked type as authoritative.
- [x] `A3 (P2)` Remove legacy helper from public elaboration API surface.
- [x] `A6 (P2)` Add checked-vs-unchecked elaboration parity tests (incl. US-004-style paths).

---

## Phase 7 — xMLF Execution & Verification ⏳

Based on `papers/these-finale-english.txt`; see also `papers/xmlf.txt` §1 & §2.

- [x] **Type Checker** (`src/MLF/Elab/TypeCheck.hs`)
    - [x] Implement `Γ ⊢ a : τ` rules
    - [x] Verify elaborated terms are well-typed
- [x] **Evaluator** (`src/MLF/Elab/Reduce.hs`)
    - [x] Implement small-step reduction `a ⟶ a'`
    - [x] Implement instantiation reduction rules (e.g., `(Λ...) N ⟶ ...`)
- [x] `A6 (P2)` Add regression cases ensuring typecheck confirms elaboration parity for bounded/coercion-heavy terms.

---

## Paper-faithfulness deltas (tracked)

- [x] Add constructor types `Cσ` to the xMLF type AST (Fig. 14.2.1).
- [x] Integrate quantifier reordering ϕR when `Typ` vs `Typexp` differ (Def. 15.3.4).
- [x] Enforce translatable-presolution invariants for Φ (explicit `PhiTranslatabilityError` / `PhiInvariantError`; no silent non-spine `OpRaise` fallback).
- [x] Confirm Ω normalization emits Fig. 15.3.4 operations for current coverage; document the remaining US-004 κσ deviation in `test/ElaborationSpec.hs`.
  - 2026-02-10: Fig. 15.3.4 witness matrix closure gate is green (`cabal test mlf2-test --test-show-details=direct --test-options='--match R-'`), covering all 15 row IDs.
- [x] `A4 (P2)` Refresh paper-faithfulness docs to reflect implemented strict Φ/Σ behavior and list only unresolved deltas.
  - 2026-02-17: synced `.kiro/specs/paper-faithfulness-remaining-deltas/{requirements,design,tasks}.md` with current implementation state (semantic deltas closed; remaining items are non-semantic assurance/docs debt).

See `.kiro/specs/paper-faithfulness-remaining-deltas/` for the audit and plan.

---

## Audit Backlog — 2026-02-06

- [x] `A1 (P1)` Enforce strict Ω normalization in production witness path (no permissive fallback on merge-direction errors).
AC status (2026-02-17): Confirmed. `normalizeInstanceOpsFull` rejects malformed merge direction with `MergeDirectionInvalid`; `normalizeEdgeWitnessesM` propagates as `WitnessNormalizationError` (no fallback acceptance). Targeted and full verification gates are green.
AC: Presolution witness normalization fails fast on malformed merge direction; no fallback acceptance in production path.
Files: `src/MLF/Constraint/Presolution/WitnessCanon.hs`, `src/MLF/Constraint/Presolution/WitnessNorm.hs`, `test/Presolution/WitnessSpec.hs`

- [x] `A2 (P1)` Make checked type authoritative for pipeline results.
AC: `runPipelineElab` and checked pipeline type outcomes are aligned for known divergence cases (including US-004-style scenarios).
Files: `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/ElaborationSpec.hs`

- [x] `A3 (P2)` Quarantine legacy elaboration helpers from public API surface.
AC status (2026-02-17): `expansionToInst` remains isolated in `MLF.Elab.Legacy`; `MLF.Elab.Pipeline` no longer re-exports it, and downstream public entry modules (`MLF.API`, `MLF.Pipeline`) do not expose legacy conversion helpers.
AC: Legacy conversion helper is not exposed from public pipeline exports used by downstream clients.
Files: `src/MLF/Elab/Pipeline.hs`, `src/MLF/Elab/Legacy.hs`, `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`

- [x] `A4 (P2)` Sync paper-faithfulness docs/specs with current strict Φ/Σ behavior and remaining true deltas only.
AC status (2026-02-17): `.kiro` paper-faithfulness status lines now reflect closed semantic deltas; remaining true deltas are explicitly scoped to formalization/assurance and broader non-semantic backlog.
AC: `.kiro` paper-faithfulness status lines reflect current implementation; remaining open items are explicit and non-contradictory.
Files: `.kiro/specs/paper-faithfulness-remaining-deltas/requirements.md`, `.kiro/specs/paper-faithfulness-remaining-deltas/tasks.md`, `implementation_notes.md`

- [x] `A5 (P3)` Remove remaining totality/harness footguns.
AC status (2026-02-18): Confirmed. Frontend coercion-copy no longer uses stringly internal error for bare coercion constants (`UnexpectedBareCoercionConst`), STCon coercion-copy traversal is totalized via `internalizeConArgs`, and the test harness now wires presolution only through `PresolutionSpec` with an explicit fail-fast wiring guard in `test/Main.hs` (`die` if umbrella marker not set).
AC: No partial `error` in frontend STCon path; test-suite wiring cannot silently omit presolution umbrella spec.
Files: `src/MLF/Frontend/ConstraintGen/Types.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Frontend/ConstraintGen.hs`, `test/ConstraintGenSpec.hs`, `test/PresolutionSpec.hs`, `test/Main.hs`

- [x] `A6 (P2)` Expand thesis-anchored regression matrix for translatability, bounded coercions, and checked-vs-unchecked parity.
AC status (2026-02-17): Added bounded/coercion-heavy parity regressions in `PipelineSpec`, `TypeCheckSpec`, and `ReduceSpec`, with existing `ElaborationSpec` matrix coverage retained; verified by focused A6 matcher and full gate (`cabal build all && cabal test`).
AC: Added targeted tests for strict translatability invariants and elaboration/type-check parity; references to thesis anchors included in spec names/comments.
Files: `test/ElaborationSpec.hs`, `test/Presolution/WitnessSpec.hs`, `test/PipelineSpec.hs`, `test/TypeCheckSpec.hs`, `test/ReduceSpec.hs`

- [x] `A7 (P2)` Consolidate duplicated binding/scope/pipeline helper logic into shared abstractions.
AC status (2026-02-18): Confirmed. Non-binding dedup is now single-sourced in test harness utilities (`runConstraintDefault`, `runToPresolutionWithAnnDefault`, `runPipelineArtifactsDefault`, `runToSolvedDefault` in `test/SpecUtil.hs`), with `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` migrated off local normalize/solve-chain wrappers; frontend scope/binder wiring remains shared via `ConstraintGen.Translate` combinators (`withScopedBuild`, `withScopedRebind`, `attachUnder`, `rebindScopeRoot`).
AC: Binding path/children/scope-graph helpers are single-sourced; ConstraintGen scope+binder wiring uses shared combinators; repeated test pipeline harness steps (`unsafeNormalize`, `firstShow`, solve chain) are centralized in shared test utilities.
Files: `src/MLF/Binding/Queries.hs`, `src/MLF/Binding/Validation.hs`, `src/MLF/Binding/Tree.hs`, `src/MLF/Binding/Canonicalization.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run/Annotation.hs`, `src/MLF/Elab/Run/Debug.hs`, `test/SpecUtil.hs`, `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`, `test/ConstraintGenSpec.hs`
Progress (2026-02-08, Group 1): duplicated binding-core helpers are now single-sourced in `MLF.Binding.Path`, `MLF.Binding.NodeRefs`, `MLF.Binding.ScopeGraph`, and `MLF.Binding.Children`; migration landed in `MLF.Binding.Queries`, `MLF.Binding.Validation`, `MLF.Binding.Tree`, `MLF.Binding.Canonicalization`, `MLF.Constraint.BindingUtil`, and `MLF.Constraint.Presolution.Base`. Remaining A7 work is the non-binding portions in the AC (`ConstraintGen`/pipeline test-harness consolidation).

---

## Stretch Goals / Future Work

- [x] Pretty-printer for xMLF terms
- [x] Parser + pretty-printer for eMLF surface syntax (`MLF.Frontend.Parse`, `MLF.Frontend.Pretty`)
- [x] Parser + pretty-printer for paper-faithful xMLF syntax (`MLF.XMLF.Parse`, `MLF.XMLF.Pretty`)
- [x] Canonical syntax docs (`docs/syntax.md`) and parser/pretty test coverage
- [x] Push toward removing the Legacy syntax (keep parser compatibility transition-only; remove legacy pretty forms from internal/debug paths)
  - Status (2026-03-08): closed as stale; canonical xMLF pretty output already ships via `MLF.XMLF.Pretty` / `MLF.Elab.Types`, and legacy spellings remain parser-accepted transition syntax only.
- [ ] Visualization of constraint graph (Graphviz / DOT)
- [ ] REPL that prints the inferred type and the elaborated xMLF term

## Active master refactor plan — 2026-02-10

- [x] Execute the master 6-phase typed two-pass edge DSL plan:
  - [x] Phase 1 + Phase 2 landed (typed planner/interpreter two-pass core).
  - [x] Phase 3 Task 7 + Task 8 (wrapping equivalence) are green.
  - [x] Phase 3 Task 9 + Task 10 landed (planner fail-fast + legacy-direct removal).
  - [x] Phase 4 Task 11-14 landed (phase-tagged errors, regression matrix, docs, verification gate).
  - [x] Phase 5 landed (plan type refinement, structured planner invariant errors, synthesized `ExpVarId` boundary module).
  - [x] Phase 6 landed (single unified expansion execution path; synthesized-wrapper bridge function removed).
  - [x] Post-phase cleanup landed (removed single-constructor `EdgeStage` phantom index from `EdgePlan`).
  - [x] Next: finishing-branch handoff + integration decision completed through the later `master` landings / cleanup follow-up.
  - Plan: `docs/plans/2026-02-10-master-4-phase-typed-two-pass-edge-dsl-implementation-plan.md`
  - Historical task folder for this campaign is no longer present in the live todo-task tree.
  - Scope highlights:
    - Enforce Phase-2 paper-shaped residual instantiation edges (`TyExp <= τ`).
    - Add Phase-4 fail-fast assertion on non-`TyExp` left inst edges.
    - Introduce typed two-pass planner/interpreter for edge processing.
    - Refine plan payload to encode TyExp-left edges directly.
    - Eliminate wrapper-specific interpreter bridge execution while preserving wrapper identity semantics.
    - Add matrix regressions and full verification (`cabal build all && cabal test`).

## Active bug closures

- [x] `BUG-2026-02-06-002`: under the final thesis-exact fallback rework, the old `let-c1-apply-bool` compatibility success path is now explicitly classified as fallback-dependent and asserted as structured fail-fast when only expansion-derived recovery remains.
- [x] `BUG-2026-02-08-004`: under the same strict policy, the nested let + annotated-lambda family is now documented as deliberate fail-fast rather than a compatibility success anchor.
