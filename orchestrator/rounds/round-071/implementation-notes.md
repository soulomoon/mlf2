# Round 071 Implementation Notes

Stage task id: `N4`

## Change Summary

- Added the canonical docs-only `N4` exact-target bind artifact at
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`.
- The new artifact freezes `round-071` / `N4` / `attempt-1` / `retry: null`,
  carries forward accepted `L1` / `L2` / `N1` / `N2` / `N3` continuity, and
  binds exactly one bounded target: the preserved non-local generic
  scheme-root alias-bound / base-like `baseTarget -> baseC` packet exercised by
  `schemeAliasBaseLikeFallback False`, together with its same-lane generic
  `targetC` consumption.
- The artifact records the one owner binder / owned bound pair, the bounded
  inverse-translation-safe bound-inlining story, the structural / variance
  binding-flag reconstruction boundary, and the explicit out-of-scope record
  while leaving controller-root roadmap/state/bug-tracker artifacts unchanged.

## Verification

- `git diff --check`
  - Result: passed in
    `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071`.
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null`
  - Result: passed.
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
  - Result: matched `2:  "contract_version": 2,` and `16:  "retry": null`.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
  - Result: matched roadmap items `1` through `7`, including pending `N4`.
- `test -f ...`
  - Result: passed for the required continuity artifacts:
    `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
    `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`,
    `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`,
    `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
    `/Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`,
    `/Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`,
    and `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/retry-subloop.md`.
- `rg -n '^4\. \[pending\] Execute the `N4` exact bounded target bind for the reopened loop' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
  - Result: matched line `81`.
- `rg -n 'N4 — Exact bounded target bind for the reopened loop|bind one exact bounded slice before any implementation begins' /Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  - Result: matched line `21`.
- `rg -n 'no fresh lawful exact successor slice|Remaining generic scheme-alias / base-like `baseTarget` arm|broader scheme-alias / base-like `baseTarget`' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  - Result: matched the preserved `baseTarget` continuity packet at lines
    `153` and `214`.
- `rg -n 'stop-blocked|blocked future work only|separate roadmap amendment|fresh selection' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  - Result: matched the `stop-blocked` outcome and the separate-amendment /
    fresh-selection gate.
- `rg -n 'reopen-planning-only|implementation and verification blocked|baseTarget route becomes admissible' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  - Result: matched the bounded `N1` authority outcome and its unchanged
    implementation / verification block.
- `rg -n 'preserved generic scheme-alias / base-like `baseTarget` route|next docs-first alias-bound /|`N4` must still bind one exact bounded target' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  - Result: matched the selected `baseTarget` planning subject and the `N4`
    prerequisite language at lines `70`, `110`, and `126`.
- `rg -n 'Acceptance criteria for any later `N4` target|owner binder|owned bound|binding-flag reconstruction|No-go And Blocked-Boundary Record' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  - Result: matched the accepted `N3` contract markers for this bind.
- `rg -n '8\.3\.1 Inlining bounds|we inline the bounds which can be rebuilt unambiguously|reconstruct the binder and the binding flag|usedtwice|structuralancestor' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `8.3.1 Inlining bounds` anchors at lines `6928`,
    `6930`, `6934`, `6936`, `6975`, and `7052`.
- `rg -n '15\.6\.2 Expressivity of alias bounds|special care must be taken|entirely inline all inert bounds|forbid alias bounds entirely|cannot be expressed at all' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `15.6.2 Expressivity of alias bounds` anchors at lines
    `14855` through `14858`.
- `rg -n '17\.3 Perspectives|recursive types and second-order polymorphism alone is already tricky|Allowing cyclic term-graphs|main difficulties likely lie in the treatment of recursion in the binding structure' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `17.3 Perspectives` anchors at lines `15411` and
    `15422`.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,408p'`
  - Result: confirmed the generic `baseTarget` computation.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '521,548p'`
  - Result: confirmed the accepted local proof cluster and the
    `rootBindingIsLocalType` gate on `rootLocalEmptyCandidateSchemeAliasBaseLike`
    and `rootLocalSchemeAliasBaseLike`.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '693,728p'`
  - Result: confirmed local-only `keepTargetFinal` and the downstream generic
    `rootIsSchemeAlias && rootBoundIsBaseLike` `targetC` arm.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/test/PipelineSpec.hs | sed -n '1244,1269p'`
  - Result: confirmed `schemeAliasBaseLikeFallback`, `bodyRoot`, and
    `rebindRootTo`.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/test/PipelineSpec.hs | sed -n '1594,1625p'`
  - Result: confirmed the accepted local contrasts and the preserved
    non-local `schemeAliasBaseLikeFallback False` packet.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/test/PipelineSpec.hs | sed -n '1698,1762p'`
  - Result: confirmed the source guards that keep the local predicates and the
    generic arm separate.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/src/MLF/Frontend/Normalize.hs | sed -n '20,36p'`
  - Result: confirmed the explicit alias-bound normalization note used as the
    bounded inlining companion anchor.
- `nl -ba /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/src/MLF/Elab/Run/Scope.hs | sed -n '127,160p'`
  - Result: confirmed `schemeBodyTarget` remains a neighboring excluded route,
    not the frozen same-lane consumer.
- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|schemeAliasBaseLikeFallback|owner binder|owned bound|rootIsSchemeAlias && rootBoundIsBaseLike|rootLocalSchemeAliasBaseLike|rootLocalEmptyCandidateSchemeAliasBaseLike|binding-flag reconstruction|out-of-scope' /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-071/docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`
  - Result: matched the required framing, target identity, local-lane
    exclusions, binding-flag language, and out-of-scope record in the new
    artifact.
- `git status --short --untracked-files=all`
  - Result: reported the two files added in this attempt plus the pre-existing
    untracked round artifacts `orchestrator/rounds/round-071/plan.md` and
    `orchestrator/rounds/round-071/selection.md`.
- `cabal build all && cabal test`
  - Result: intentionally skipped. This attempt stayed docs-only and the diff
    remained out of `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`;
    the work added only the canonical docs plan artifact plus this round-local
    notes file, so the code-path Cabal gate was out of scope for `N4`.
