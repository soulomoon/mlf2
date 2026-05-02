# Round 227 Review

Date: 2026-05-03
Round: `round-227`
Milestone: `milestone-6`
Direction: `direction-6a-freeze-polymorphism-lowerability-contract`
Extracted item: `absent`
Base branch: `master`
Branch: `orchestrator/round-227-freeze-polymorphism-lowerability-contract`

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
`/Volumes/src/mlf4/orchestrator/worktrees/round-227` unless the command names
another path directly.

1. `pwd && printf '\n--- AGENTS ---\n' && sed -n '1,220p' AGENTS.md && printf '\n--- reviewer skill? role ---\n' && sed -n '1,260p' orchestrator/roles/reviewer.md`
2. `sed -n '1,240p' /Users/ares/src/orchestratorpattern/skills/run-orchestrator-loop/SKILL.md`
3. `sed -n '1,220p' /Users/ares/.codex/skills/haskell-pro/SKILL.md`
4. `rg --files -g 'AGENTS.md'`
5. `sed -n '1,240p' orchestrator/state.json && printf '\n--- roadmap ---\n' && sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/roadmap.md && printf '\n--- verification ---\n' && sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/verification.md && printf '\n--- retry ---\n' && sed -n '1,260p' orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001/retry-subloop.md`
6. `sed -n '1,220p' orchestrator/rounds/round-227/selection.md && printf '\n--- plan ---\n' && sed -n '1,260p' orchestrator/rounds/round-227/plan.md && printf '\n--- implementation-notes ---\n' && sed -n '1,260p' orchestrator/rounds/round-227/implementation-notes.md && printf '\n--- mechanism table ---\n' && sed -n '1,260p' docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`
7. `git status --short && printf '\n--- branch ---\n' && git branch --show-current && printf '\n--- merge-base ---\n' && git merge-base HEAD master && printf '\n--- diff stat ---\n' && git diff --stat master...HEAD && printf '\n--- name-only ---\n' && git diff --name-only master...HEAD`
8. `printf '--- worktree pointers ---\n' && sed -n '1,80p' orchestrator/roadmap.md && printf '\n--- verification pointer ---\n' && sed -n '1,80p' orchestrator/verification.md && printf '\n--- retry pointer ---\n' && sed -n '1,80p' orchestrator/retry-subloop.md && printf '\n--- parent pointers ---\n' && cd /Volumes/src/mlf4 && printf '\nparent roadmap:\n' && sed -n '1,80p' orchestrator/roadmap.md && printf '\nparent verification:\n' && sed -n '1,80p' orchestrator/verification.md && printf '\nparent retry:\n' && sed -n '1,80p' orchestrator/retry-subloop.md`
9. `git diff --check master...HEAD`
10. `git diff --name-only master && printf '\n--- stat ---\n' && git diff --stat master`
11. `git diff master -- docs/architecture.md docs/backend-native-pipeline.md src/MLF/Backend/IR.hs src/MLF/Backend/Convert.hs src/MLF/Backend/LLVM/Lower.hs docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md test/BackendLLVMSpec.hs test/RepoGuardSpec.hs`
12. `git diff master -- orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/state.json orchestrator/rounds/round-227 orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap`
13. `sed -n '80,150p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- around lowerLocalFunctionValue ---\n' && sed -n '3720,3905p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- around nativeRenderableKind/main path ---\n' && rg -n "polymorphic main binding|nativeRenderableKind|lowerBackendProgramNative|partial type application|escaping type abstraction|unspecialized polymorphic binding" src/MLF/Backend/LLVM/Lower.hs`
14. `sed -n '372,445p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- around lowerFunction ---\n' && sed -n '3275,3335p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- around lowerExpr tyabs ---\n' && sed -n '3605,3655p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- around resolveTypeArguments ---\n' && sed -n '4925,4975p' src/MLF/Backend/LLVM/Lower.hs && printf '\n--- nativeRenderableKind ---\n' && sed -n '671,735p' src/MLF/Backend/LLVM/Lower.hs`
15. `sed -n '160,270p' test/BackendLLVMSpec.hs`
16. `rg -n "describe \"polymorphism lowerability contract\"|supports top-level complete type application|supports local closure-sensitive specialization|supports first-class polymorphic lowering|qualifies closure entries emitted from type specializations|rejects polymorphic main bindings|rejects unspecialized polymorphic bindings|rejects escaping type abstractions|rejects partial type application|polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary|polymorphismLowerabilityMarkers" test/BackendLLVMSpec.hs test/RepoGuardSpec.hs`
17. `git diff --name-only master -- orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-027 && printf '\n--- public surface diff ---\n' && git diff --name-only master -- src-public mlf2.cabal src/MLF/Backend/LLVM.hs src/MLF/Backend/LLVM/Syntax.hs src/MLF/Backend/LLVM/Ppr.hs README.md Bugs.md CHANGELOG.md AGENTS.md`
18. `nl -ba docs/architecture.md | sed -n '188,205p' && printf '\n--- backend-native-pipeline ---\n' && nl -ba docs/backend-native-pipeline.md | sed -n '50,62p' && printf '\n--- Backend IR note ---\n' && nl -ba src/MLF/Backend/IR.hs | sed -n '66,84p' && printf '\n--- Convert note ---\n' && nl -ba src/MLF/Backend/Convert.hs | sed -n '28,40p' && printf '\n--- Lower note/helper ---\n' && nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '93,115p' && printf '\n' && nl -ba src/MLF/Backend/LLVM/Lower.hs | sed -n '3748,3925p'`
19. `nl -ba test/BackendLLVMSpec.hs | sed -n '182,245p' && printf '\n--- RepoGuard lines ---\n' && nl -ba test/RepoGuardSpec.hs | sed -n '289,307p' && printf '\n' && nl -ba test/RepoGuardSpec.hs | sed -n '688,695p' && printf '\n--- Mechanism table row 6/7 ---\n' && nl -ba docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md | sed -n '10,16p'`
20. `git diff --check master`
21. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/MLF.Backend.LLVM/polymorphism lowerability contract/"'`
22. `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Repository guardrails/polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary/"'`
23. `git ls-files --others --exclude-standard orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap orchestrator/rounds/round-227 | sort`
24. `python3 - <<'PY' ... compare state lineage fields to round-227 selection.md ... PY`
25. `python3 - <<'PY' ... assert mechanism table rows/gates/next-action for row 6 ... PY`
26. `cabal build all && cabal test`
27. `git rev-parse HEAD && git rev-parse master && git merge-base HEAD master && git branch --show-current`

## Baseline Checks

1. `Roadmap lineage and fresh-family consistency`: `PASS`
   - `orchestrator/state.json` resolves the live round to
     `roadmap_id = 2026-05-02-00-backend-ir-executable-boundary-roadmap`,
     `roadmap_revision = rev-001`,
     `roadmap_dir = orchestrator/roadmaps/2026-05-02-00-backend-ir-executable-boundary-roadmap/rev-001`,
     `active_round_id = round-227`,
     `milestone_id = milestone-6`, and
     `direction_id = direction-6a-freeze-polymorphism-lowerability-contract`.
   - The state/selection comparison script reported
     `{'missing': [], 'absent_ok': True}`, so canonical `selection.md`
     matches the live lineage fields and the absent extracted-item state.
   - Parent-workspace and canonical-worktree pointer stubs for
     `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and
     `orchestrator/retry-subloop.md`
     all point at the same `rev-001` roadmap bundle.
   - `git diff --name-only master -- .../rev-027` returned no output, so the
     completed predecessor roadmap family remains unchanged and is still
     bounded predecessor evidence only.
   - This review and `review-record.json` record the same roadmap lineage
     fields.

2. `Diff hygiene`: `PASS`
   - `git diff --check master` returned no output.
   - `git rev-parse HEAD`, `git rev-parse master`, and `git merge-base HEAD master`
     all resolved to `b4e239c5d6fafd55ed56ed81154ec4e3faf79ac8`.
   - `git diff --name-only master...HEAD` and `git diff --stat master...HEAD`
     returned no output, so the review target is the live uncommitted
     canonical worktree diff against the `master` baseline rather than a
     committed branch-ahead delta.
   - `git diff --name-only master` shows twelve tracked file changes:
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
   - The synchronized contract-owner surfaces agree that checked `Backend.IR`
     may retain `BackendTyAbs` / `BackendTyApp`, conversion may preserve them,
     LLVM/native lowering owns only the specialization-based lowerable subset,
     complete type applications may specialize privately, and residual runtime
     polymorphism stays on explicit diagnostics rather than widening the
     boundary
     (`docs/architecture.md:193-198`,
     `docs/backend-native-pipeline.md:55-59`,
     `src/MLF/Backend/IR.hs:71-83`,
     `src/MLF/Backend/Convert.hs:33-40`,
     `src/MLF/Backend/LLVM/Lower.hs:96-113`).
   - The lowerer hardening stays within the authorized row-6 boundary:
     `lowerBackendProgramCore` rejects polymorphic `main`,
     `nativeRenderableKind` rejects non-renderable polymorphic results,
     `lowerFunction` rejects unspecialized polymorphic function emission,
     `lowerExpr` rejects escaping `BackendTyAbs`,
     `resolveTypeArguments` rejects partial type application, and the new
     `residualZeroArityPolymorphism` helper only narrows residual unspecialized
     zero-arity polymorphic value escape paths
     (`src/MLF/Backend/LLVM/Lower.hs:387-423,671-679,3311-3313,3636-3637,3751-3758,3871-3923,4963`).
   - `git diff --name-only master -- src-public mlf2.cabal src/MLF/Backend/LLVM.hs src/MLF/Backend/LLVM/Syntax.hs src/MLF/Backend/LLVM/Ppr.hs README.md Bugs.md CHANGELOG.md AGENTS.md`
     returned no output, so no public surface widening, second IR, row-7
     closeout payload, or unrelated repo-facing files were added.
   - No lazy STG machinery, fallback lowering/runtime path, public lowering
     API, runtime polymorphism implementation, or second backend IR was
     introduced.
   - The family remains serial; `orchestrator/state.json` still records
     `max_parallel_rounds = 1` and only `round-227` is active.

4. `Evidence and test gate`: `PASS`
   - The focused LLVM row-6 slice passed:
     `8 examples, 0 failures`.
   - The exact repository guard
     `polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary`
     passed on serial rerun with `1 example, 0 failures`.
   - One earlier reviewer-side parallel attempt to launch the repo guard beside
     the focused LLVM slice hit a `dist-newstyle` package-cache lock race. That
     was a shared-build invocation issue in the review process, not a round
     failure; rerunning the guard serially per the plan’s shared-build
     discipline passed immediately.
   - Because `src/` and `test/` changed, the full repo gate was required and
     rerun. `cabal build all && cabal test` passed with
     `2352 examples, 0 failures`; the suite reported
     `Finished in 331.5107 seconds`.

5. `Mechanism-table discipline`: `PASS`
   - The mechanism-table script passed with
     `{'rows': 7, 'gates': ['YES', 'YES', 'YES', 'YES', 'YES', 'YES', 'NO']}`.
   - The table keeps the fixed row order and uses only `YES` / `NO`.
   - Rows 1-5 remain `YES`, row 6 flips to `YES`, and row 7 remains `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:10-15`).
   - Row 6 alone changed, and its next action now points to milestone 7 / row 7.

6. `Guidance synchronization`: `PASS`
   - `docs/architecture.md`,
     `docs/backend-native-pipeline.md`,
     `src/MLF/Backend/IR.hs`,
     `src/MLF/Backend/Convert.hs`, and
     `src/MLF/Backend/LLVM/Lower.hs`
     now agree on the same row-6 specialization-versus-rejection contract.
   - `test/RepoGuardSpec.hs` adds the exact required guard name and checks the
     synchronized row-6 markers across those five surfaces
     (`test/RepoGuardSpec.hs:292-306,688-693`).
   - `AGENTS.md` is unchanged, which is correct because this round adds no
     durable repo-wide workflow or policy rule.

## Plan Conformance

1. `Task 1: Publish the row-6 lowerability contract across the durable backend surfaces`: `PASS`
   - `docs/architecture.md` states explicitly that checked `Backend.IR` may
     carry `BackendTyAbs` / `BackendTyApp`, while LLVM/native lowering owns
     only the specialization-based lowerable subset and residual runtime
     polymorphism remains unsupported
     (`docs/architecture.md:193-198`).
   - `src/MLF/Backend/IR.hs` publishes the same row-6 boundary in the module
     note and keeps unsupported polymorphic shapes on the lowering-side
     rejection path rather than weakening checked IR
     (`src/MLF/Backend/IR.hs:71-83`).
   - `src/MLF/Backend/Convert.hs` states that checked-program conversion
     preserves representable polymorphic structure instead of erasing it to
     satisfy LLVM
     (`src/MLF/Backend/Convert.hs:33-40`).
   - `src/MLF/Backend/LLVM/Lower.hs` adds the dedicated row-6 note and ties it
     to the actual enforcement points:
     `lowerTyApp`,
     `lowerGlobalValue`,
     `lowerExpr`,
     `resolveTypeArguments`,
     `lowerFunction`,
     `lowerBackendProgramNative`, and
     `nativeRenderableKind`
     (`src/MLF/Backend/LLVM/Lower.hs:96-113,387-423,671-679,3311-3313,3636-3637,3871-3923,4963`).
   - `docs/backend-native-pipeline.md` makes the raw/native emission boundary
     explicit: emitted LLVM/native code is promised only for the specialized
     lowerable subset, and unsupported polymorphic executables fail before
     emission
     (`docs/backend-native-pipeline.md:55-59`).

2. `Task 2: Lock row-6 success and rejection behavior with focused LLVM and guard evidence`: `PASS`
   - `test/BackendLLVMSpec.hs` adds the required localized
     `describe "polymorphism lowerability contract"` section with the planned
     supported and rejection cases in one reviewable packet
     (`test/BackendLLVMSpec.hs:185-242`).
   - The supported rows cover:
     top-level complete type application,
     local/closure-sensitive specialization,
     first-class polymorphic lowering on the existing static lane, and
     qualified closure-entry specialization
     (`test/BackendLLVMSpec.hs:186-220`).
   - The rejection rows cover:
     polymorphic `main` binding,
     unspecialized polymorphic binding,
     escaping type abstraction / escaping polymorphic binding, and
     partial type application
     (`test/BackendLLVMSpec.hs:222-242`).
   - `test/RepoGuardSpec.hs` adds the exact required guard over the five
     contract surfaces, with the synchronized marker set kept local to
     `polymorphismLowerabilityMarkers`
     (`test/RepoGuardSpec.hs:292-306,688-693`).
   - No runtime polymorphism, fallback lowering, lazy rescue, public lowering
     API, second IR, or row-7 closeout behavior was introduced.

3. `Task 3: Refresh mechanism-table row 6 only`: `PASS`
   - Row 6 now records the accepted specialization-versus-rejection contract,
     cites the synchronized docs/module notes plus focused
     `BackendLLVMSpec` and `RepoGuardSpec` evidence, and points the next live
     blocker at milestone 7 / row 7
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:14`).
   - Rows 1-5 remain `YES`, and row 7 remains `NO`
     (`docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md:10-15`).

## Milestone-6 Checks

1. `Accepted evidence states which polymorphic nodes may remain in Backend.IR and which shapes must be erased, specialized, or rejected before LLVM emission`: `PASS`
   - The five synchronized contract surfaces now agree that
     `BackendTyAbs` / `BackendTyApp` may remain in checked `Backend.IR`,
     conversion may preserve them, and LLVM/native lowering owns only the
     specialization-based executable subset
     (`docs/architecture.md:193-198`,
     `docs/backend-native-pipeline.md:55-59`,
     `src/MLF/Backend/IR.hs:71-83`,
     `src/MLF/Backend/Convert.hs:33-40`,
     `src/MLF/Backend/LLVM/Lower.hs:96-113`).

2. `LLVM/native lowering does not silently accept unsupported polymorphic executables`: `PASS`
   - Unsupported residual polymorphism stays on explicit diagnostics:
     `polymorphic main binding`,
     `unspecialized polymorphic binding`,
     `escaping type abstraction`,
     `escaping polymorphic binding`, and
     `partial type application`
     are all owned by concrete lowerer checks rather than fallback behavior
     (`src/MLF/Backend/LLVM/Lower.hs:387-423,671-679,3311-3313,3636-3637,3751-3758,3871-3923,4963`).
   - The round does not add runtime polymorphism, a fallback execution lane, a
     second IR, or a public lowering surface.

3. `Focused tests cover supported lowerable shapes and named unsupported diagnostics where lowerability remains intentionally closed`: `PASS`
   - The focused LLVM slice passed with `8 examples, 0 failures` and covers the
     supported specialization paths plus the named rejection diagnostics in the
     exact row-6 contract section
     (`test/BackendLLVMSpec.hs:185-242`).
   - The exact repository guard passed with `1 example, 0 failures` and keeps
     the five contract surfaces synchronized
     (`test/RepoGuardSpec.hs:292-306,688-693`).

## Decision

- Verdict: `accepted + finalize`
- Merge readiness: `satisfied`
- Merge payload files:
  `docs/architecture.md`,
  `docs/backend-native-pipeline.md`,
  `src/MLF/Backend/IR.hs`,
  `src/MLF/Backend/Convert.hs`,
  `src/MLF/Backend/LLVM/Lower.hs`,
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`,
  `test/BackendLLVMSpec.hs`, and
  `test/RepoGuardSpec.hs`
- Residual risks:
  milestone 7 remains intentionally open and row 7 stays `NO` in the
  mechanism table;
  runtime polymorphism remains intentionally unsupported, so any broader
  lowering/public-surface/runtime-polymorphism claim will require a later
  accepted round or roadmap revision rather than piggybacking on this row-6
  closure.
