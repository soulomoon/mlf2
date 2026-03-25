# Round `round-035` Attempt `1` Review (`C2`)

- Baseline checks:
  - `git diff --check` -> pass (no output).
  - `python3 -m json.tool orchestrator/rounds/round-035/state-snapshot.json >/dev/null` -> pass.
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-035/state-snapshot.json` -> pass (`2:  "contract_version": 2,`, `13:  "retry": null`).
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-002/roadmap.md` -> pass (ordered `C1` through `C4` list intact, `C2` still pending pre-merge).
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` -> pass.
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md` -> pass.
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md` -> pass.
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass.
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` -> pass.
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-002/retry-subloop.md` -> pass.
  - Continuity presence check via `python3` -> pass (`round-001..round-033 directories: True`; predecessor recursive-types packet present; replay-repair rounds `round-024` through `round-027` present; inherited boundary/repair docs present).
  - Authoritative predecessor record recheck via `python3` over `round-028` through `round-033` -> pass (each review record remains `accepted + finalize` with non-`none` artifact existence confirmed).
  - Full Cabal gate `cabal build all && cabal test` -> pass (`1127 examples, 0 failures`; `Test suite mlf2-test: PASS`).

- Task-specific checks:
  - `C2-CONTRACT` -> pass: `rg -n 'Attempt: \`attempt-1\`|Retry state: \`null\`|Live subject: repaired \`URI-R2-C1\`|explicit-only recursive baseline|non-equi-recursive semantics|non-cyclic structural graph encoding|authority-narrowed|uniqueness-owner-stable-refuted|constructor-acyclic-termination-refuted|rootBindingIsLocalType' docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md src/MLF/Elab/Run/ResultType/Fallback.hs` confirms `attempt-1`, `retry: null`, repaired `URI-R2-C1`, the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary, carried-forward `U2` / `U3` / `U4` constraints, and that `rootBindingIsLocalType` is the bound retention signal under review.
  - `C2-BOUNDED-DIFF` -> pass: `git status --short --untracked-files=all` shows only `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/PipelineSpec.hs`, the canonical `C2` artifact, and round notes; `git diff --name-only` lists only `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`; `git diff --name-only -- . ':(exclude)src/MLF/Elab/Run/ResultType/Fallback.hs' ':(exclude)test/PipelineSpec.hs'` returned no output; `git status --short --untracked-files=all | rg -n 'src/MLF/Elab/Inst|src/MLF/Research|src-public/|app/|mlf2\.cabal|orchestrator/state\.json|orchestrator/roadmap\.md|Bugs\.md'` returned no output.
  - `C2-LOCAL-GATE` -> pass: `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '478,715p'` shows `targetPresolutionView` switching on `rootBindingIsLocalType`, `keepTargetFinal` gated by `rootBindingIsLocalType`, and the non-local branch falling back to `rootFinal` instead of retaining the recursive-looking target, so wrapper/proxy retention remains fail-closed outside the local `TypeRef` case.
  - `C2-POSITIVE-COVERAGE` -> pass: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'` passed with `6 examples, 0 failures`, and `nl -ba test/PipelineSpec.hs | sed -n '1118,1135p'` shows the local-binding positive control exercising both `runPipelineElab` and `runPipelineElabChecked`.
  - `C2-NEGATIVE-COVERAGE` -> fail: `nl -ba test/PipelineSpec.hs | sed -n '1143,1169p'` shows the new non-local proxy case at lines `1143` through `1151` only calling `computeResultTypeFallback`, while the checked/unchecked entrypoint assertions at lines `1158` through `1169` still target the older out-of-scope unannotated proxy case instead of the same `g g` proxy-wrapper expression. A reproducible ad-hoc check,
    `printf '%s\n' ':m + MLF.Pipeline MLF.Frontend.Syntax SpecUtil Data.Set' 'let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))' 'let expr = ELet "g" (ELamAnn "x" recursiveAnn (EVar "x")) (EApp (EVar "g") (EVar "g"))' 'print (runPipelineElab Data.Set.empty (unsafeNormalizeExpr expr))' 'print (runPipelineElabChecked Data.Set.empty (unsafeNormalizeExpr expr))' ':quit' | cabal repl mlf2-test --repl-options='-ignore-dot-ghci'`,
    produced `Left (PipelineElabError (PhiTranslatabilityError ...))` for both entrypoints, so the behavior is currently fail-closed, but reviewer check 6 and Task 3 require that specific bounded proxy case to be covered in `PipelineSpec` across both unchecked and checked entrypoints. The submitted round does not encode that evidence.
  - `C2-CONTINUITY` -> pass: the continuity scripts confirm completed rounds `001` through `033`, inherited boundary docs, replay-repair rounds, the predecessor recursive-types packet, and accepted `U1` through `U6` records all remain present and untouched.
  - `C2-FULL-GATE` -> pass: fresh `cabal build all && cabal test` succeeded in the round worktree with `mlf2-test` reporting `1127 examples, 0 failures`.

- Implemented stage result: `fail`
- Attempt verdict: `rejected`
- Stage action: `retry`
- Retry reason: `c2-non-local-proxy-entrypoint-coverage-missing`
- Fix hypothesis: `Add focused Hspec coverage in test/PipelineSpec.hs for the same non-local proxy wrapper case (`let g = (\\x : mu a. a -> Int. x) in g g`) across both runPipelineElab and runPipelineElabChecked, keep the diff inside the existing bounded slice, and update the C2 artifact to record that entrypoint evidence.`
- Decision summary:
  - The production change stays inside the frozen `Fallback.hs` seam, uses `rootBindingIsLocalType` as the local-only retained-target gate, keeps non-local wrapper/proxy retention fail-closed, and avoids `MLF.Elab.Inst`, replay, prototype, API, executable, build-surface, roadmap, state, and bug-tracker edits.
  - The targeted `C2` block passes and the full repo gate passes, so there is no current build or runtime regression blocking the bounded lane itself.
  - The blocking issue is that the submitted regression evidence does not satisfy the round contract: the specific non-local proxy wrapper case is only asserted through direct `computeResultTypeFallback` reconstruction, not through both unchecked and checked pipeline entrypoints as required by Task 3 and reviewer check 6.
  - Under the retry-subloop contract, that missing bounded entrypoint coverage requires `rejected + retry`.
- Evidence summary:
  - All baseline checks and the mandatory full repo gate passed, and continuity with predecessor rounds/documents remains intact.
  - Diff inspection confirms the round stayed inside `src/MLF/Elab/Run/ResultType/Fallback.hs`, `test/PipelineSpec.hs`, the canonical `C2` artifact, and round notes only.
  - The only blocking defect is missing required test evidence for the non-local proxy wrapper case across both pipeline entrypoints; the ad-hoc REPL check shows the implementation already rejects that case, so the retry can stay within the same bounded files.
