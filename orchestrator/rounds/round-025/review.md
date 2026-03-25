# Round `round-025` Attempt `3` Review (`R2`)

- Baseline checks:
  - `git -C .worktrees/round-025 diff --check` (pass)
  - `python3 -m json.tool orchestrator/rounds/round-025/state-snapshot.json >/dev/null` (pass)
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-025/state-snapshot.json` (pass; controller state remains on `contract_version: 2` with an active `retry` block for `R2` attempt `3`)
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/roadmap.md` (pass; ordered roadmap items for `R1` through `R4` remain parseable)
  - `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md && test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md && test -f orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/retry-subloop.md` (pass)
  - `cabal build all && cabal test` (pass; `1124 examples, 0 failures`)
  - Continuity guards:
  - `git -C .worktrees/round-025 diff --name-only -- orchestrator/rounds/round-{001..024} tasks/todo/2026-03-11-recursive-types-orchestration Bugs.md` (pass; no output)
  - `git -C .worktrees/round-025 ls-files --others --exclude-standard -- orchestrator/rounds/round-{001..024} tasks/todo/2026-03-11-recursive-types-orchestration Bugs.md` (pass; no output)
  - `wc -l orchestrator/rounds/round-025/attempt-log.jsonl && rg -n 'Attempt verdict|Stage action|Retry reason|Fix hypothesis' orchestrator/rounds/round-025/reviews/attempt-{1,2}.md` (pass; controller-owned attempt log is still exactly `2` lines before this review and earlier retry snapshots remain present with the required retry fields)

- Task-specific checks:
  - Diff remains bounded to the accepted `R2` lane:
  - `git -C .worktrees/round-025 diff --name-only` => `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `test/ElaborationSpec.hs`, `test/Research/UriR2C1PrototypeP1Spec.hs`
  - `git -C .worktrees/round-025 diff --name-only -- app src-public src/MLF/Research mlf2.cabal` (pass; no output, so there is no second executable interface, no public-surface drift, no replay-search widening into research modules, and no compatibility fallback surface)
  - Retry-delta alignment against `plan.md` and the approved repair-track design:
  - `nl -ba src/MLF/Elab/Inst.hs | sed -n '24,180p'` shows the bounded replay-context repair still lives only inside `MLF.Elab.Inst.evalInstantiationWith` / `applyInstantiation`, with `allowReplayBoundMatch` confined to the `InstBot` owner branch
  - `nl -ba src/MLF/Elab/TypeCheck.hs | sed -n '112,145p'` shows the adjacent `TypeCheck` change remains plumbing-only (`instElimEnv = \_v _replacement env' -> env'`), so checked semantics did not widen into a second repair path
  - `nl -ba test/Research/UriR2C1PrototypeP1Spec.hs | sed -n '186,225p;359,428p;488,556p;599,669p'` shows the attempt-3 contract-scope update is limited to the four inherited prototype blockers and explicitly preserves predecessor authority by still asserting historical `round-020` / `round-021` review-record fields while allowing live post-repair reruns to report bounded negative continuity drift
  - `sed -n '1,260p' docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` confirms the artifact records `Attempt: 3`, keeps the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` / `applyInstantiation` (`InstBot`) boundary, and documents `contract-scope` rather than any broadened repair or replay campaign
  - Focused owner-boundary checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot (TVar _)) still rejects explicit non-bottom bounds outside the replay lane"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'` (pass; `1 example, 0 failures`)
  - Focused inherited-blocker checks:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "allows attempt-2 reruns and records live replay widening as bounded non-pass"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D1 attempt-1 via the root-cause tuple and records continuity drift against the historical replay boundary"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D2 attempt-1 via the root-cause tuple and reports continuity-blocked localization on live reruns"'` (pass; `1 example, 0 failures`)
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "runs D3 attempt-1 via the root-cause tuple and reports bounded-negative continuity drift on live reruns"'` (pass; `1 example, 0 failures`)
  - Locked-lane evidence:
  - `nl -ba test/ElaborationSpec.hs | sed -n '442,505p;1517,1558p'` shows the alpha-renaming comparator plus the three strict non-replay misuse regressions and the locked `URI-R2-C1` replay success assertion
  - Inherited-authority continuity verified by read-only evidence:
  - `orchestrator/rounds/round-016/review-record.json` keeps `P1 = pass`
  - `orchestrator/rounds/round-017/review-record.json` keeps `P2 = semantic-negative`
  - `orchestrator/rounds/round-020/review-record.json` keeps `D1 = pass`
  - `orchestrator/rounds/round-021/review-record.json` keeps `D2 = pass`
  - `orchestrator/rounds/round-022/review-record.json` keeps `D3 = pass`
  - `orchestrator/rounds/round-023/review-record.json` keeps `D4 = reopen-repair-track`
  - `orchestrator/rounds/round-024/review-record.json` keeps `R1 = pass`
  - `tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md` remains immutable predecessor evidence
  - `Bugs.md` still records the same controlling defect `BUG-2026-03-16-001` at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
  - Reviewer judgment:
  - the current round satisfies the retry delta plan: the accepted attempt-2 owner-boundary repair stays intact, the attempt-3 change is limited to contract-scoping the four inherited prototype reruns, and the mandatory repo gate is now green
  - no forbidden widening into broader replay search, automatic inference, second-interface behavior, compatibility fallback, or production-path drift appears in the diff

- Implemented stage result:
- `pass`
- Attempt 3 preserves the bounded replay-context repair at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), contract-scopes the four inherited `test/Research/UriR2C1PrototypeP1Spec.hs` blockers to distinguish live post-repair reruns from immutable predecessor authority, and clears the mandatory full `cabal build all && cabal test` gate without widening beyond `URI-R2-C1` / `uri-r2-c1-only-v1`.

- Attempt verdict:
- `accepted`

- Stage action:
- `finalize`

- Retry reason:
- `none`

- Fix hypothesis:
- `none`

- Decision summary:
- Finalize `R2` on attempt 3.
- The round now satisfies both the bounded-owner repair requirement and the repo-local gate requirement, so this attempt is authoritative carry-forward for roadmap item `R2`.

- Evidence summary:
- Stage artifact: `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md`
- Review snapshot: `orchestrator/rounds/round-025/reviews/attempt-3.md`
- Authoritative review record: `orchestrator/rounds/round-025/review-record.json`
- Key implementation evidence: `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `test/ElaborationSpec.hs`, `test/Research/UriR2C1PrototypeP1Spec.hs`
- Key inherited authority: `orchestrator/rounds/round-016/review-record.json`, `orchestrator/rounds/round-017/review-record.json`, `orchestrator/rounds/round-020/review-record.json`, `orchestrator/rounds/round-021/review-record.json`, `orchestrator/rounds/round-022/review-record.json`, `orchestrator/rounds/round-023/review-record.json`, `orchestrator/rounds/round-024/review-record.json`
