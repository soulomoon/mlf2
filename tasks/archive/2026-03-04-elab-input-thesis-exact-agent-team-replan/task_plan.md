# Task Plan — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Replan

## Goal
Close Task 38 end-to-end: track wave execution outcomes, sync thesis-exact docs,
record gate evidence, and archive the replan task folder.

## Phases
- [completed] Wave 0 planning: re-audit thesis anchors + active call path gap and publish agent-team replan.
- [completed] Wave 1 guards: strengthen thesis-exact source checks for active Elaborate/Phi boundary signatures.
- [completed] Wave 2 implementation: migrate active Φ call signatures/call sites to `χp`-native `PresolutionView` flow.
- [completed] Wave 3 verification: capture closeout gate evidence for `elab-input thesis-exact guard`, `checked-authoritative`, `Dual-path verification`, and full suite.
- [completed] Wave 4 docs closeout: update TMT/implementation notes/changelog/TODO and task tracker docs with current code refs + gate evidence.
- [completed] Archive: move `tasks/todo/2026-03-04-elab-input-thesis-exact-agent-team-replan/` to `tasks/archive/2026-03-04-elab-input-thesis-exact-agent-team-replan/`.

## Decisions
- Thesis contract anchors for elaboration input are fixed to:
  - `papers/these-finale-english.txt` Def. 15.3.12
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5
- TMT evidence must cite current active runtime boundary refs:
  - `src/MLF/Elab/Run/Pipeline.hs:112-141`
  - `src/MLF/Elab/Elaborate.hs:64-80`
  - `src/MLF/Elab/Elaborate.hs:181-205`
  - `src/MLF/Elab/Elaborate.hs:917-949`
  - `src/MLF/Elab/Phi/Translate.hs:284-317`
  - `test/PipelineSpec.hs:176-179`
  - `test/ElaborationSpec.hs:331-349`
- Closeout docs record the already-run green gate set from Wave 3:
  - `elab-input thesis-exact guard`: PASS (`2 examples, 0 failures`)
  - `checked-authoritative`: PASS (`8 examples, 0 failures`)
  - `Dual-path verification`: PASS (`4 examples, 0 failures`)
  - `cabal build all && cabal test`: PASS (`931 examples, 0 failures`)

## Errors Encountered
- None during Wave 4 docs closeout.
