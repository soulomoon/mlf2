# Round 181 Review

Round: `round-181`
Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
Item: `item-5`

## Retry Outcome

- Implemented stage result: the round removes the packet-local `C1` shortcut
  from `src/MLF/Elab/Run/Pipeline.hs`, leaves the selected non-local
  scheme-alias / base-like packet recursive on `runPipelineElab` and
  `runPipelineElabChecked`, adds a focused `PipelineSpec` guard for shortcut
  absence, and records the bounded packet result in
  `orchestrator/rounds/round-181/implementation-notes.md`.
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `none`

## Commands Run

- `git branch --show-current` (exit `0`)
- `python3 -m json.tool orchestrator/state.json >/dev/null` (exit `0`)
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"` (exit `0`)
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"` (exit `0`)
- `python3 - <<'PY' ... ROUND181_POINTER_IDENTITY_OK ... PY` (exit `0`)
- `git diff --name-only codex/automatic-recursive-type-inference -- orchestrator/roadmaps orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md` (exit `0`)
- `git diff --check` (exit `0`)
- `python3 - <<'PY' ... ROUND181_C1_WRITABLE_SLICE_OK ... PY` (exit `0`)
- `python3 - <<'PY' ... C1_SHORTCUT_REMOVED ... PY` (exit `0`)
- `rg -n 'preserveC1AuthoritativeRecursiveAlias|isBlockedC1AliasScheme' src test orchestrator/rounds/round-181` (exit `0`)
- `git merge-base HEAD codex/automatic-recursive-type-inference && git rev-parse --short HEAD && git rev-parse --short codex/automatic-recursive-type-inference` (exit `0`)
- `python3 - <<'PY' ... print(worker_mode=none, stage=review, roadmap_item_id=item-5) ... PY` (exit `0`)
- `test ! -f orchestrator/rounds/round-181/worker-plan.json` (exit `0`)
- `git status --short` (exit `0`)
- `nl -ba orchestrator/rounds/round-181/selection.md | sed -n '70,140p'` (exit `0`)
- `nl -ba orchestrator/rounds/round-181/plan.md | sed -n '60,230p'` (exit `0`)
- `git diff -- src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs` (exit `0`)
- `git show 98af0ff -- src/MLF/Elab/Run/Pipeline.hs` (exit `0`)
- `nl -ba src/MLF/Elab/Run/Pipeline.hs | sed -n '180,230p'` (exit `0`)
- `nl -ba test/PipelineSpec.hs | sed -n '2208,2236p'` (exit `0`)
- `nl -ba test/Research/C1AuthoritativeSurfaceSpec.hs | sed -n '60,92p'` (exit `0`)
- `nl -ba orchestrator/rounds/round-181/implementation-notes.md` (exit `0`)
- `rg -n 'rootNonLocalSchemeAliasBaseLike|baseTarget|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs` (exit `0`)
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback/Core.hs | sed -n '470,490p;640,655p'` (exit `0`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'` (exit `0`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet"'` (exit `0`)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'` (exit `0`)
- `cabal build all && cabal test` (exit `0`)
- `python3 -m json.tool orchestrator/rounds/round-181/review-record.json >/dev/null` (exit `0`)
- `python3 - <<'PY' ... ROUND181_REVIEW_RECORD_IDENTITY_OK ... PY` (exit `0`)
- `ls -R orchestrator/rounds/round-181` (exit `0`)

## Pass/Fail By Contract

- Baseline 1, roadmap identity, pointer consistency, and preserved history:
  **PASS**. `orchestrator/state.json`, the live pointer stubs,
  `selection.md`, and `review-record.json` agree on
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`.
  `git diff --name-only ... -- orchestrator/roadmaps orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md`
  returned no paths, so the active roadmap bundle and pointer stubs stayed
  unchanged. The tracked `orchestrator/state.json` delta is the
  controller-owned review transition already called out in `selection.md`.
- Baseline 2, diff hygiene: **PASS**. `git diff --check` exited `0`.
- Baseline 3, roadmap metadata integrity: **PASS**. The active `roadmap.md`
  still records `Item id:`, `Depends on:`, `Parallel safe:`,
  `Parallel group:`, and `Merge after:` for all seven roadmap items.
- Baseline 4, build/test gate for production/test changes: **PASS**. The round
  touches `src/` and `test/`, and the required full gate
  `cabal build all && cabal test` passed. The suite finished with
  `1307 examples, 0 failures`.
- Baseline 5, thesis conformance gate: **NOT APPLICABLE**. No thesis-facing
  files changed.
- Baseline 6, worker-plan integrity: **NOT APPLICABLE**. `worker_mode=none`
  and `orchestrator/rounds/round-181/worker-plan.json` is absent.
- Item-5, writable slice compliance: **PASS**. The round-owned source/test
  delta is limited to `src/MLF/Elab/Run/Pipeline.hs`,
  `test/PipelineSpec.hs`, and
  `orchestrator/rounds/round-181/implementation-notes.md`, which matches the
  authorized write scope from `plan.md` lines `73`-`106`. No out-of-scope
  production, test, Cabal, roadmap, or docs path is touched.
- Item-5, focused representative evidence and shortcut removal:
  **PASS**. `test/Research/C1AuthoritativeSurfaceSpec.hs` still shows the
  exact `C1` packet on a non-recursive fallback surface and recursive
  authoritative surfaces, while `test/PipelineSpec.hs` adds a focused guard
  that fails if `preserveC1AuthoritativeRecursiveAlias` or
  `isBlockedC1AliasScheme` returns to `Run/Pipeline`. The focused C1 reruns
  all passed.
- Item-5, absence of the removed packet-local shortcut in `Run/Pipeline`:
  **PASS**. The diff deletes the helper block introduced by commit `98af0ff`,
  `src/MLF/Elab/Run/Pipeline.hs` now leaves `termClosed` equal to the
  retained-child-preserved term at lines `191`-`196`, and the token-absence
  script plus repository search show the deleted helper names survive only in
  round notes/plan text and in the regression guard.
- Item-5, full verification evidence is present and credible: **PASS**. The
  reviewer reran the three focused `C1` commands and then
  `cabal build all && cabal test`; all exited `0`. The targeted runs covered
  the exact `C1` harness, the exact selected non-local packet assertions, and
  the enclosing bounded-prototype describe block before the full suite.
- Item-5, bounded result and positive-family read stay honest: **PASS**.
  `implementation-notes.md` records only the exact `C1` packet result: the
  fallback surface stays on the `baseTarget -> baseC` non-recursive read,
  the authoritative entrypoints stay recursive after shortcut removal, and no
  broader `P2`-`P6` closure or repo-level readiness claim is made.

## Plan Conformance

- Step 1, root-cause localization: **PASS**. Commit `98af0ff` is the source of
  the packet-local helper, the current diff removes only that helper from
  `Run/Pipeline`, and the unchanged fallback route still names
  `rootNonLocalSchemeAliasBaseLike` and selects `baseTarget` for that
  non-local arm in `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
- Step 2, focused regression coverage first: **PASS**. The new
  `PipelineSpec` example at lines `2228`-`2231` is the exact bounded guard the
  plan required, and the matching `C1` authoritative-surface tests still pass.
- Step 3, smallest lawful production correction: **PASS**. The only production
  edit is in `src/MLF/Elab/Run/Pipeline.hs`; no new route family, no
  candidate-selection change, and no `TermClosure` or fallback-core change was
  introduced.
- Step 4, bounded notes only: **PASS**. `implementation-notes.md` records the
  shortcut removal and surviving exact-packet result without widening into
  same-lane, `P5`, aggregate `P2`, or repo-level claims.
- Step 5, focused and full verification gates: **PASS**. The reviewer reran
  the focused `C1` commands, rechecked shortcut absence and writable-slice
  scope, and reran `cabal build all && cabal test`.

## Evidence Summary

- `HEAD` and `codex/automatic-recursive-type-inference` both resolve to
  `664bef7`, so the round diff against the base branch is the working-tree
  delta only: the controller-owned `orchestrator/state.json` transition,
  the `Run/Pipeline` helper removal, the focused `PipelineSpec` addition, and
  the round-local notes/review artifacts.
- `src/MLF/Elab/Run/Pipeline.hs` now stops after
  `preserveRetainedChildAuthoritativeResult`; the packet-local `C1` helper and
  its supporting imports are gone. No replacement shortcut was added anywhere
  in the diff.
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` still contains the existing
  admitted non-local route arm `rootNonLocalSchemeAliasBaseLike` at lines
  `481`-`484`, and `targetC` still routes that arm through `baseTarget` at
  lines `644`-`653`.
- Focused reruns are consistent with the bounded packet claim:
  `C1 authoritative-surface harness` passed with fallback
  `TBase Int` / `containsMu False` and authoritative recursive output on both
  entrypoints, and the `PipelineSpec` selected-packet rerun passed the
  fallback-lane assertion, the authoritative recursion assertion, and the new
  shortcut-absence guard together.
- Full regression coverage is green. `cabal build all && cabal test` passed
  with `1307 examples, 0 failures`.
- Residual risk is low and non-blocking: the new guard is token-based rather
  than mechanism-based, so a differently named future packet-local rescue
  could evade that one assertion. The current round still removes the only
  known shortcut and keeps the exact packet green under the full suite.

## Decision

APPROVED: the round stays inside the authorized item-5 `C1` slice, removes the
packet-local `Run/Pipeline` shortcut, keeps the exact selected non-local packet
recursive on the authoritative entrypoints with focused and full verification
green, and records the bounded result without widening the family claim.
