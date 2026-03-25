# Round `round-025` Attempt `1` Review

- Baseline checks:
  - `git -C .worktrees/round-025 diff --check` => pass
  - `python3 -m json.tool orchestrator/rounds/round-025/state-snapshot.json >/dev/null` => pass
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-025/state-snapshot.json` => pass (`contract_version: 2`, `retry: null`)
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/roadmap.md` => pass
  - `test -f docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md && test -f docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md && test -f orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-002/retry-subloop.md` => pass
  - `cabal build all && cabal test` => fail; `test/Research/UriR2C1PrototypeP1Spec.hs` still reports 4 failures (`P2`, `D1`, `D2`, `D3` status expectations)

- Task-specific checks:
  - Scope bound check: `git -C .worktrees/round-025 status --short` and `git -C .worktrees/round-025 diff --stat` show production/test edits only in `src/MLF/Elab/Inst.hs`, `test/ElaborationSpec.hs`, plus the new `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` artifact and current-round review files.
  - Predecessor continuity check: `git -C .worktrees/round-025 diff --name-only -- orchestrator/rounds/round-0{01..09} orchestrator/rounds/round-1{0..9} orchestrator/rounds/round-2{0..4} tasks/todo/2026-03-11-recursive-types-orchestration Bugs.md` => empty; inherited rounds `001` through `024`, the accepted `D4 = reopen-repair-track` result, the recursive-types packet, and `Bugs.md` remain unchanged.
  - Locked replay checks:
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "URI-R2-C1 witness replay stays alpha-equivalent to the locked no-fallback shape"'` => pass
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "InstInside(InstBot) still rejects explicit non-bottom bounds without replay variables"'` => pass
    - `cabal test mlf2-test --test-show-details=direct --test-options='--match "fails InstBot when argument equals non-bottom input type"'` => pass
  - No-second-interface / no-drift check: the diff does not touch `app/`, `src/MLF/Research/`, controller files, or public entrypoints.
  - Blocking bounded-owner check:
    - `src/MLF/Elab/Inst.hs:127-140` accepts any `InstBot` argument with free variables whenever `matchType` can unify it with the current non-bottom bound.
    - REPL probe:
      - `cabal repl mlf2-test <<'EOF'`
      - `:m + MLF.Elab.Pipeline`
      - `let bound = TArrow (TVar "u") (TVar "u")`
      - `let ty = TForall "a" (Just bound) (TVar "a")`
      - `let inst = InstInside (InstBot (TVar "x"))`
      - `print (applyInstantiation ty inst)`
      - `:quit`
      - `EOF`
    - Observed result: `Right (TForall "a" (Just (TArrow (TVar "u") (TVar "u"))) (TVar "a"))`
    - This is broader than the approved `URI-R2-C1` replay lane: a plain free-variable `InstBot` misuse now succeeds against an arbitrary explicit non-bottom bound, even though the round artifact claims strict nearby misuse still fails.

- Implemented stage result:
  - `R2` attempt 1 makes the locked `URI-R2-C1` replay fixture succeed and records the bounded-repair artifact, but the production relaxation in `MLF.Elab.Inst.applyInstantiation` widens `InstBot` acceptance beyond the localized replay-only owner boundary.

- Attempt verdict:
  - `rejected`

- Stage action:
  - `retry`

- Retry reason:
  - `applyInstantiation` now accepts non-replay `InstBot` calls whenever the argument contains free variables that can unify with the current explicit bound, so the repair no longer stays confined to `URI-R2-C1` / `uri-r2-c1-only-v1`; the required full `cabal build all && cabal test` gate is also still red.

- Fix hypothesis:
  - Replace the free-variable `matchType` escape hatch with a narrower replay-lane predicate tied to the already-established bound context for the current `InstBot` owner path, add a regression proving `InstInside (InstBot (TVar "x"))` on explicit non-bottom bounds still fails outside the locked replay case, and rerun the full gate before reasserting `R2` completion.

- Decision summary:
  - Reject attempt 1 and return `R2` to retry. The replay-specific success is real, but the owner-boundary change is too permissive and the mandatory repo gate remains failing.

- Evidence summary:
  - Bounded replay test passed.
  - Existing nearby strictness tests passed but missed the free-variable misuse hole.
  - Direct REPL evidence demonstrates the widened acceptance on a non-replay explicit bound.
  - Earlier rounds, the accepted diagnostic reopen result, the recursive-types packet, and `Bugs.md` remain immutable.
