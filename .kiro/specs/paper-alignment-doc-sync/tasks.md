# Implementation Plan

- [x] 1. Update paper-alignment test labels
  - Files: `test/ElaborationSpec.hs`
  - Steps:
    - Rename the `describe` block `Paper alignment baselines (expected failures)` to `Paper alignment baselines`.
    - Remove “(expected failure)” from the two κσ-related baseline test names.
  - **Verification:** `rg -n 'Paper alignment baselines \\(expected failures\\)|\\(expected failure\\)\"' test/ElaborationSpec.hs` returns no matches
  - _Requirements: 1.1, 1.2_

- [x] 2. Remove outdated “expected-failure baselines” notes from Kiro specs
  - [x] 2.1 Update spec `tasks.md` note blocks
    - Files:
      - `.kiro/specs/paper_general_raise_plan/tasks.md`
      - `.kiro/specs/edge-interior-exactness/tasks.md`
      - `.kiro/specs/paper-faithfulness-gap-closure/tasks.md`
      - `.kiro/specs/scope-model-retirement/tasks.md`
    - Steps:
      - Replace the top “Note:” blocks so they no longer claim `cabal test` has intentional failures.
      - Keep the “use `--config-file=.cabal-config` if needed” tip.
    - **Verification:** `rg -n 'intentional failing baselines|intentional expected-failure baselines|intentional failing' .kiro/specs -S --glob '!.kiro/specs/paper-alignment-doc-sync/**'` returns no matches
    - _Requirements: 2.1_
  - [x] 2.2 Update `scope-model-retirement` requirements wording
    - Files: `.kiro/specs/scope-model-retirement/requirements.md`
    - Steps:
      - Replace “non-expected-failure test suite subset” with `cabal test --test-show-details=direct`.
    - **Verification:** `rg -n 'non-expected-failure' .kiro/specs/scope-model-retirement/requirements.md` returns no matches
    - _Requirements: 2.2_

- [x] 3. Update `merge_raise_merge_plan.txt` Phase 8 status text
  - Files: `merge_raise_merge_plan.txt`
  - Steps:
    - Rename “Phase 8 — Remaining gaps (TODO)” to reflect completion.
    - Remove/refresh the outdated “Scope clarification” text about binder-only raises.
    - Keep any genuinely future work as an explicit “Future work” subsection.
  - **Verification:** `rg -n \"Phase 8 — Remaining gaps \\(TODO\\)\" merge_raise_merge_plan.txt` returns no matches
  - _Requirements: 3.1, 3.2_

- [x] 4. Run the full test suite
  - **Verification:** `cabal --config-file=.cabal-config test --test-show-details=direct` passes
  - _Requirements: 1.1, 2.1, 3.2_
