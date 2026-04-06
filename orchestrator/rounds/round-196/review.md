# Round 196 Review

Decision: **APPROVED**

## Artifacts inspected

- `orchestrator/state.json`
- `orchestrator/roles/reviewer.md`
- `orchestrator/rounds/round-196/selection.md`
- `orchestrator/rounds/round-196/plan.md`
- `orchestrator/rounds/round-196/implementation-notes.md`
- `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`
- Current round diff in `/Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196`

## Commands run

- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196 status --short` (exit 0)
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196 diff --name-only` (exit 0)
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196 diff --name-only -- . ':(exclude)orchestrator/state.json'` (exit 0)
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196 diff --stat -- . ':(exclude)orchestrator/state.json'` (exit 0)
- `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-196 diff --check` (exit 0)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'` (exit 0; 6 examples, 0 failures)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'` (exit 0; 1 example, 0 failures)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'` (exit 0; 1 example, 0 failures)
- `cabal build all && cabal test` (exit 0; 1338 examples, 0 failures)

## Baseline checks

1. **Roadmap lineage / pointer consistency**: PASS
   - `orchestrator/state.json` resolves roadmap
     `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
     / `rev-001` /
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001`.
   - `selection.md` records the same roadmap identity plus
     `milestone-2`, `direction-2a-implement-the-selected-p5-lane`, and
     `implement-the-selected-p5-lane`.
   - Top-level pointer stubs in `orchestrator/roadmap.md`,
     `orchestrator/verification.md`, and `orchestrator/retry-subloop.md`
     point at the same active bundle.
   - No `roadmap_item_id` compatibility field is required or present.
   - No roadmap family or revision files are changed in the round diff.

2. **Diff hygiene**: PASS
   - `git diff --check` returned cleanly.

3. **Strategy-roadmap metadata integrity**: PASS
   - The active roadmap retains `## Goal`, `## Outcome Boundaries`,
     `## Global Sequencing Rules`, `## Parallel Lanes`, and `## Milestones`.
   - The milestone and candidate-direction blocks for the active roadmap still
     carry the required metadata fields.

4. **Build / test gate**: PASS
   - The implementation-owned diff touches `test/PipelineSpec.hs` and
     `test/Research/P5ClearBoundarySpec.hs`, so the full gate was required.
   - `cabal build all && cabal test` passed with `1338 examples, 0 failures`.

5. **Thesis conformance gate**: N/A
   - No thesis-facing docs or scripts changed.

6. **Worker-plan integrity**: N/A
   - This round used no worker fan-out.

7. **Preserved setup / control-plane discipline**: N/A
   - This round did not scaffold or revise roadmap families.

## Milestone-2 checks

1. **Frozen writable slice / diff scope**: PASS
   - The implementation-owned diff stays inside the frozen writable slice and,
     in practice, only touches:
     `test/Research/P5ClearBoundarySpec.hs` and `test/PipelineSpec.hs`.
   - The visible `orchestrator/state.json` diff in the round worktree is
     controller bookkeeping for the live round, not an implementation edit.

2. **Authoritative-surface evidence**: PASS
   - `test/Research/P5ClearBoundarySpec.hs` adds the alias-frame specimen
     `sameLaneAliasFrameClearBoundaryExpr`, a same-lane fallback harness, and
     an explicit authoritative-entrypoint check over both `runPipelineElab`
     and `runPipelineElabChecked`.
   - The focused retained-child probe suite passed with the new alias-frame
     specimen staying recursive on both authoritative entrypoints.
   - The nested-forall contrast still stays fail-closed and reports the
     expected `PhiTranslatabilityError`, so the result remains bounded and
     honest.

3. **Full gate after test changes**: PASS
   - Required full gate rerun succeeded.

4. **March 28 exact packet remains settled**: PASS
   - The diff keeps `nestedForallContrastExpr` as reject-side contrast only.
   - No change reframes that exact packet as live debt or relitigates the
     round-151 settlement.

## Plan conformance

- Step 1 completed directly: the round promoted one exact retained-child
  alias-frame specimen into the focused `P5` research and pipeline tests.
- Steps 2 and 3 became lawful no-ops rather than hidden divergence: the newly
  focused authoritative-entrypoint checks showed that the current production
  path already preserves this exact lane, so no fallback, term-closure, or
  pipeline code change was needed.
- Step 4 completed exactly: diff hygiene passed, focused retained-child checks
  passed, and the full gate passed.

## Evidence summary

- Implementation-owned diff: 2 test files, 69 insertions, no production or
  public-surface edits.
- The new tests pin the selected retained-child guard-cluster lane to the
  existing authoritative surfaces and source guards without widening scope.
- This makes the round a valid milestone-2 test-only settlement slice: the
  selected direction required reviewer-visible authoritative evidence for the
  frozen `P5` lane, and the round now supplies that evidence while staying
  inside the milestone-1-selected slice.

## Decision

**APPROVED**
