# Round 070 Implementation Notes

Stage task id: `N3`

## Change Summary

- Added the canonical docs-only `N3` safety / acceptance contract at
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`.
- The new artifact freezes `round-070` / `N3` / `attempt-1` / `retry: null`,
  carries forward accepted `L1` / `L2` / `N1` / `N2` continuity, records the
  threatened properties for the selected preserved generic scheme-alias /
  base-like `baseTarget` subject, derives thesis-backed invariant axes, states
  Acceptance criteria for any later `N4` target, and keeps explicit No-go
  boundaries without binding the exact `N4` target.
- Left controller-root roadmap/state/bug-tracker artifacts unchanged and did
  not modify the pre-existing round artifacts `plan.md` and `selection.md`.

## Verification

- `git diff --check`
  - Result: passed in
    `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-070`.
- `python3 -m json.tool /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json >/dev/null`
  - Result: passed.
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/state.json`
  - Result: matched `2:  "contract_version": 2,` and `16:  "retry": null`.
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/roadmap.md`
  - Result: matched roadmap items `1` through `7`, including the pending `N3`
    row at line `68`.
- `test -f ...`
  - Result: passed for the required continuity artifacts:
    `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
    `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`,
    `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`,
    `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
    `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`,
    `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`,
    and `orchestrator/retry-subloop.md`.
- `rg -n 'N3 — Safety and acceptance contract for the reopened loop|Write the reopened-loop audit / acceptance contract before authorizing any implementation slice' /Users/ares/.codex/worktrees/d432/mlf4/tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  - Result: matched line `20`.
- `rg -n 'Invariant audit document|binding/tree discipline|reconstruction/reification obligations|principality/termination risk boundaries|If any prerequisite above is missing' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - Result: matched lines `48`, `50`, `52`, `53`, and `64`.
- `rg -n 'no fresh lawful exact successor slice|baseTarget' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  - Result: matched the preserved `baseTarget` continuity packet, including
    lines `153-176` in the generic scheme-alias / base-like case analysis.
- `rg -n 'stop-blocked|separate roadmap amendment|fresh selection' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  - Result: matched the `stop-blocked` outcome and the separate-amendment /
    fresh-selection gate at lines `75`, `95`, `140`, and `144`.
- `rg -n 'reopen-planning-only|implementation and verification blocked|baseTarget route becomes admissible' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  - Result: matched `reopen-planning-only` and the implementation/verification
    block at lines `64`, `127`, and `132`.
- `rg -n 'baseTarget|alias-bound / bound-inlining / binding-structure planning subject|verifier-checkable safety and acceptance contract' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  - Result: matched the selected `baseTarget` planning subject and the `N3`
    prerequisite at lines `51`, `57`, `70`, `90`, `108`, `116`, `122`, `124`,
    and `153`.
- `rg -n '8\.3\.1 Inlining bounds|we inline the bounds which can be rebuilt unambiguously|reconstruct the binder and the binding flag|usedtwice|structuralancestor' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `8.3.1 Inlining bounds` anchors at lines `6928`,
    `6930`, `6934`, `6936`, `6975`, and `7052`.
- `rg -n '15\.6\.2 Expressivity of alias bounds|special care must be taken|entirely inline all inert bounds|forbid alias bounds entirely|cannot be expressed at all' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `15.6.2 Expressivity of alias bounds` anchors at lines
    `14855` through `14858`.
- `rg -n '17\.3 Perspectives|recursive types and second-order polymorphism alone is already tricky|Allowing cyclic term-graphs|main difficulties likely lie in the treatment of recursion in the binding structure' /Users/ares/.codex/worktrees/d432/mlf4/papers/these-finale-english.txt`
  - Result: matched the `17.3 Perspectives` anchors at lines `15411` and
    `15422`.
- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|Invariant audit|Acceptance criteria|No-go|alias-bound ownership|binding-flag reconstruction|explicit-only|non-equi-recursive|non-cyclic-graph|no-fallback' /Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-070/docs/plans/2026-03-22-automatic-iso-recursive-base-target-safety-acceptance-contract.md`
  - Result: matched the required contract markers and boundary language at
    lines `7`, `8`, `20-21`, `31-36`, `88`, `92`, `101`, `160`, `186`, `203`,
    `216`, and `281`.
- `cabal build all && cabal test`
  - Result: intentionally skipped. This round remained docs-only and the new
    diff stayed out of `src/`, `src-public/`, `app/`, `test/`, and
    `mlf2.cabal`; the work added only the canonical docs plan artifact plus
    this round-local notes file.
