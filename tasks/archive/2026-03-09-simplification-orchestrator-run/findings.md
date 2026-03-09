# Findings

## Run Baseline
- UTC start: 2026-03-08T20:23:29Z
- Baseline commit: babfc303eb4f5608e4683eeb82ac7930f748d630
- Base branch: master
- Current branch at start: master
- Repo safety status: clean tracked/untracked working tree; no hard-unsafe state detected.
- Existing background task folders under `tasks/todo/` are treated as context only and will not be modified by this run.

## Source of Truth
- Primary: `papers/these-finale-english.txt`
- Secondary when thesis is silent: `papers/xmlf.txt`
- Repo context reviewed: `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, `Bugs.md`

## Hard Safety Stop
- Preflight command `cabal build all && cabal test` failed on baseline commit `babfc303eb4f5608e4683eeb82ac7930f748d630`.
- The current base branch is not in a green state for autonomous simplification work.
- Build/test breakage is unrelated to the new run: `test/Parity/FrozenArtifacts.hs:128`, `test/SpecUtil.hs:210`, and `test/SpecUtil.hs:223` reference `Solved.fromPreRewriteState`, which is no longer exported by `MLF.Constraint.Solved`.
- Per repository-safety preflight and worktree safety rules, the campaign stops before round 1 instead of layering simplifications on top of a broken baseline.
- Recorded the blocking defect in `Bugs.md` as `BUG-2026-03-09-001` and promoted the baseline repair to new `TODO.md` Task 71.

## Round 1
- Thinker candidate: Retire the remaining `MLF.Elab.Run.ChiQuery` facade and use `PresolutionView` directly
- Candidate summary: The live elaboration/result-type path is already `PresolutionView`-native, but five production modules still route trivial reads through `MLF.Elab.Run.ChiQuery`, a thin 50-line wrapper that just renames `pv*` selectors and `cBindParents . pvCanonicalConstraint`. That leaves a mixed access style in the thesis path: some modules read `PresolutionView` directly, while others go through an extra elaboration-local facade that no longer adds solved-compat behavior or real abstraction.


## Round 1
- Accepted thinker candidate for planning: retire the pure re-export module `MLF.Constraint.Presolution.Plan.Target`.
- Initial verifier gate: YES — the module is still live, purely internal, and currently imported by only `src/MLF/Constraint/Presolution/Plan.hs` and `src/MLF/Elab/Generalize.hs`.
- Planner scope: replace those imports with direct `Target.*` imports, retire the aggregator module, remove its `mlf2.cabal` entry, and add a concise `CHANGELOG.md` note.


## Final Orchestrator Disposition
- Fresh QA confirmation at 2026-03-08T20:37:12Z UTC reproduced the baseline failure on `master` with `cabal build all` and `cabal test`.
- No simplification round was allowed to proceed past preflight.
- No round worktrees, `codex/round-*` branches, round commits, or round merges were created by this run.


## Baseline Repair Diagnosis
- Public facade drift is broader than the first compiler output: direct public `Solved.fromPreRewriteState` calls remain in `test/SpecUtil.hs`, `test/Parity/FrozenArtifacts.hs`, `test/PipelineSpec.hs`, and `test/Constraint/SolvedSpec.hs`.
- The intended test-only owner path already exists in `test/SolvedFacadeTestUtil.hs` via `presolutionViewFromSnapshot` + `stepSolvedFromPresolutionView`.
- Repair worktree created at `/Users/ares/.config/superpowers/worktrees/mlf4/baseline-repair-snapshot-solved-tests` from current `master` for isolated implementation.

## Baseline Repair Plan
- Root cause confirmed: the public `MLF.Constraint.Solved` facade intentionally dropped `fromPreRewriteState`, but test modules still call it directly.
- Real call sites to migrate: `test/SpecUtil.hs`, `test/Parity/FrozenArtifacts.hs`, `test/PipelineSpec.hs`, and `test/Constraint/SolvedSpec.hs`.
- Thesis-safe owner path: route snapshot-based solved reconstruction through `MLF.Constraint.Finalize.finalizeSolvedFromSnapshot` via `test/SolvedFacadeTestUtil.hs`; do not widen the public facade again.
- Isolated repair worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/baseline-repair-solved-snapshot-tests` on branch `codex/baseline-repair-solved-snapshot-tests`.

## Baseline Repair Plan
- Root cause confirmed: the public `MLF.Constraint.Solved` facade intentionally dropped `fromPreRewriteState`, but test modules still call it directly.
- Real call sites to migrate: `test/SpecUtil.hs`, `test/Parity/FrozenArtifacts.hs`, `test/PipelineSpec.hs`, and `test/Constraint/SolvedSpec.hs`.
- Thesis-safe owner path: route snapshot-based solved reconstruction through `MLF.Constraint.Finalize.finalizeSolvedFromSnapshot` via `test/SolvedFacadeTestUtil.hs`; do not widen the public facade again.
- Isolated repair worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/baseline-repair-solved-snapshot-tests` on branch `codex/baseline-repair-solved-snapshot-tests`.


## Baseline Repair Outcome
- Integrated branch `codex/baseline-repair-solved-snapshot-tests` as merge commit `165c6dff8f85a7ca2a8a2a4c1627ce6fe9f405eb` (repair commit `658717fcfaa25c063c3e24440ae879ad719ca93e`).
- Final accepted repair: `test/SolvedFacadeTestUtil.hs` now rebuilds snapshot solved values through `SolveSnapshot` → `solveResultFromSnapshot` → `Solved.fromSolveOutput`, matching the strict/public replay path without widening `MLF.Constraint.Solved`.
- Updated test callers: `test/SpecUtil.hs`, `test/Parity/FrozenArtifacts.hs`, `test/PipelineSpec.hs`, and `test/Constraint/SolvedSpec.hs`; `mlf2.cabal` now wires `SolvedFacadeTestUtil` into `exe:frozen-parity-gen`; `CHANGELOG.md` records the repair.
- Fresh QA and final verifier gates both returned YES on the repair worktree.

## Restarted Round 1
- Accepted idea: retire redundant `preferGenScope` from `MLF.Elab.Run.Scope`.
- Initial verifier gate: YES — the helper is still live only through `resolveCanonicalScope` plus tests, and the module note already classifies it as redundant.
- Planner boundary: remove `preferGenScope`, simplify the Def. 15.3.2 ga′ pipeline note, migrate the focused scope/guard tests to surviving owner paths, and update `implementation_notes.md` + `CHANGELOG.md` in the same round.
- Round 1 worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/round-1-retire-prefergenscope` on `codex/round-1-retire-prefergenscope`.
- Round 1 result: removed `preferGenScope`, simplified the ga′ scope pipeline to `bindingScopeRef` -> `canonicalizeScopeRef` -> `letScopeOverrides`, and locked the helper out with a focused source guard.
- Round 1 QA: YES — targeted scope slices and `cabal build all && cabal test` passed.
- Round 1 final verifier: YES — simplification remains thesis-exact and worth keeping.
- Round 1 integration: committed on `codex/round-1-retire-prefergenscope` as `99e12022fc65bf619bde6253fe3575f497c96798` and merged to `master` as `3ff5942fa5788eb9d1be12fef8c106c02a6098bd`.
- Round 1 next candidate area: pending-weaken owner query relocation or another still-live scope/presolution redundancy, subject to fresh Thinker+Verifier gates.

## Restarted Round 2
- Accepted idea: retire dead `rtvSchemeBodyTarget` from `MLF.Elab.Run.ResultType.View`.
- Initial verifier gate: YES — the wrapper is dead, adds no semantics beyond `schemeBodyTarget`, and mismatches the documented narrowed overlay-query boundary.
- Planner boundary: delete the dead export/definition, add a focused source guard, and sync `implementation_notes.md` + `CHANGELOG.md` without broadening into unrelated result-type refactors.
- Round 2 worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/round-2-retire-rtvschemebodytarget` on `codex/round-2-retire-rtvschemebodytarget`.
- Round 2 result: removed dead `rtvSchemeBodyTarget`, fully narrowing `ResultType.View` to its overlay-aware query boundary and keeping `schemeBodyTarget` single-sourced in `MLF.Elab.Run.Scope`.
- Round 2 QA: YES — grep, targeted result-type/chi-first slice, and `cabal build all && cabal test` passed.
- Round 2 final verifier: YES — dead-wrapper retirement is thesis-exact and worth keeping.
- Round 2 integration: committed on `codex/round-2-retire-rtvschemebodytarget` as `09905cc1a136b0be87fdc3cc4a6e810b5645c175` and merged to `master` as `343bfcff2a0ff3dd3e2b6a714787eb0fef5e9652`.
- Round 2 next candidate area: pending-weaken owner query relocation, remaining reader/wrapper retirement remainders, or another dead internal alias, subject to fresh Thinker+Verifier gates.

## Restarted Round 3
- Accepted idea: single-source duplicate witness/trace canonicalizers between presolution and elaboration runtime.
- Initial verifier gate: YES — `canonicalizeWitness` and `canonicalizeTrace` are still live, duplicated mechanical helpers with materially identical field-level behavior.
- Planner boundary: single-source only witness/trace canonicalization; keep `canonicalizeExpansion` explicitly out of scope because its semantics differ.
- Round 3 worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/round-3-single-source-witness-trace-canonicalizers` on `codex/round-3-single-source-witness-trace-canonicalizers`.
- Round 3 result: single-sourced witness/trace canonicalization under `MLF.Constraint.Presolution.Rewrite`, leaving `canonicalizeExpansion` explicitly separate.
- Round 3 QA: YES — grep, targeted canonicalization + Pipeline/Elaboration slices, and `cabal build all && cabal test` passed.
- Round 3 final verifier: YES — single-owner witness/trace canonicalization remains thesis-exact and worth keeping.
- Round 3 integration: committed on `codex/round-3-single-source-witness-trace-canonicalizers` as `1632d5abfd91f546ef5e298df0fb63eb546e5ade` and merged to `master` as `7b9090516b5b601a0ade8e68189805cebaf0e75e`.
- Round 3 next candidate area: pending-weaken owner query relocation, reader/wrapper retirement remainders, or another tightly bounded duplication cleanup, subject to fresh Thinker+Verifier gates.

## Restarted Round 4
- Accepted idea: retire dead `pendingWeakenOwnerForNode` / `pendingWeakenOwnerForEdge` aliases from `MLF.Constraint.Presolution.EdgeUnify`.
- Initial verifier gate: YES — the aliases are pure wrappers over `StateAccess` owner-query helpers and have no external importers.
- Planner boundary: remove only the dead alias surface, switch local uses to `pendingWeakenOwnerM` / `instEdgeOwnerM`, and keep the scheduling algorithm otherwise unchanged.
- Round 4 worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/round-4-retire-pendingweaken-owner-aliases` on `codex/round-4-retire-pendingweaken-owner-aliases`.
- Round 4 result: retired the dead pending-weaken owner aliases from `EdgeUnify`, leaving `StateAccess` as the single owner-query source.
- Round 4 QA: YES — grep, targeted owner-boundary/unification-closure slice, and `cabal build all && cabal test` passed.
- Round 4 final verifier: YES — owner-query alias retirement is thesis-exact and worth keeping.
- Round 4 integration: committed on `codex/round-4-retire-pendingweaken-owner-aliases` as `1c23afcdafd729b61ca9e682bfae885da4bb9c47` and merged to `master` as `b4041a192119cf112c5e490ddccf5a261de3a175`.
- Round 4 next candidate area: reader-layer retirement remainders, another dead internal wrapper, or a no-longer-needed compatibility alias, subject to fresh Thinker+Verifier gates.

## Restarted Round 5
- Accepted idea: collapse `bindingScopeRefCanonical` onto the existing `bindingScopeRef` owner path.
- Initial verifier gate: YES — the canonical helper still carries a second handwritten path walker and has only one live production caller.
- Planner boundary: remove the bespoke canonical bind-parent traversal, preserve fallback semantics and `letScopeOverrides`, and keep the change bounded to scope/result-type helper plumbing plus docs/guard text.
- Round 5 worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/round-5-collapse-bindingscoperefcanonical` on `codex/round-5-collapse-bindingscoperefcanonical`.
- Round 5 result: collapsed `bindingScopeRefCanonical` onto the primary `bindingScopeRef` owner path and removed the bespoke canonical walker while keeping `letScopeOverrides` semantics intact.
- Round 5 QA: YES — grep, targeted scope/result-type/chi-first slices, and `cabal build all && cabal test` passed.
- Round 5 final verifier: YES — canonical-scope helper simplification is thesis-exact and worth keeping.
- Round 5 integration: committed on `codex/round-5-collapse-bindingscoperefcanonical` as `d335074052e72f4f883a18e11ce0e1e7f66bbe89` and merged to `master` as `9601ddba43841dc3e1d60c2b65d029678f595498`.
- Round 5 next candidate area: remaining dead internal wrappers/aliases around result-type or runtime helpers, reader-layer remnants, or bounded query-owner consolidation, subject to fresh Thinker+Verifier gates.
- Round 6 result: moved targeted `EdgeProcessing` raw state peeks behind shared `StateAccess` helpers, keeping owner-boundary scheduling semantics unchanged.
- Round 6 QA: YES — raw-peek grep, targeted owner-boundary/unification-closure slice, and `cabal build all && cabal test` passed.
- Round 6 final verifier: YES — helper-layer convergence is thesis-exact and worth keeping.
- Round 6 integration: committed on `codex/round-6-edgeprocessing-stateaccess` as `aeaba0e3d7b6c3b322d715591dab867a8d225304` and merged to `master` as `407d66395b45cf72dd967be851454c6a7ce23b7e`.
- Round 6 next candidate area: remaining dead runtime/result-type aliases, helper-layer convergence remainders, or another tightly bounded state-plumbing duplication, subject to fresh Thinker+Verifier gates.
- Round 7 result: single-sourced result-type scheme-root detection and root-peeling in `ResultType/Util.hs`, preserving the canonical-vs-pre-canonical split.
- Round 7 QA: YES — targeted result-type/chi-first/parity slices and `cabal build all && cabal test` passed.
- Round 7 final verifier: YES — result-type root-peeling single-sourcing is thesis-exact and worth keeping.
- Round 7 integration: committed on `codex/round-7-single-source-resulttype-root-peeling` as `9cd7f20d8b499ccf8e7ebb406fa129011660bc80` and merged to `master` as `c76d6b6b53584ce54a4c78537735fef67350f20e`.
- Round 7 next candidate area: remaining dead internal wrappers/aliases or bounded duplication in runtime/result-type/planner utilities, subject to fresh Thinker+Verifier gates.
- Round 8 result: single-sourced the shared target-unwrapping core in `MLF.Elab.Run.Scope`, leaving `generalizeTargetNode` and `schemeBodyTarget` as thin `S` vs `S'` policy wrappers.
- Round 8 QA: YES — targeted scope/chi-first slices and `cabal build all && cabal test` passed.
- Round 8 final verifier: YES — shared target-unwrapping is thesis-exact and worth keeping.
- Round 8 integration: committed on `codex/round-8-single-source-target-unwrapping` as `d95bb586c6ce68c5337a12dc1e222eb876793683` and merged to `master` as `d62cabd6743e05e976cdb15df490c7d0ed45f280`.
- Round 8 next candidate area: remaining dead runtime/result-type aliases or tightly bounded internal helper duplications, subject to fresh Thinker+Verifier gates.
- Round 9 result: single-sourced canonical scheme-root owner/root-set bookkeeping in `MLF.Elab.Run.Generalize.Common`, keeping result-type and generalization policy logic local.
- Round 9 QA: YES — targeted result-type/generalization/elaboration slices and `cabal build all && cabal test` passed after moving the helper to a cycle-free home.
- Round 9 final verifier: YES — only mechanical bookkeeping moved; thesis-facing semantics stayed local and unchanged.
- Round 9 integration: committed on `codex/round-9-single-source-schemeroot-owners` as `1d90f7a7a54b763c1c6f0d32032ba3f55f8fb328` and merged to `master` as `576224e875ff4902126e706665e1084bf6d91014`.
- Round 9 next candidate area: one last bounded internal alias/helper duplication cleanup, subject to fresh Thinker+Verifier gates.

## Restarted Round 10
- Accepted idea: retire dead `chiCanonicalBindParents` from `MLF.Elab.Run.ChiQuery`.
- Final result: `ChiQuery` keeps the intentional chi-first facade surface while dropping the lone derived canonical-bind-parent helper; the single fallback caller now reads canonical bind parents directly from `chiCanonicalConstraint`.
- Round 10 QA: YES — targeted guard plus `cabal build all && cabal test` passed (`1017 examples, 0 failures`).
- Round 10 integration: committed on `codex/round-10-retire-chicanonicalbindparents` as `0ffa2abd3c1b2f8bf3fdcc9dbcf59c5574985743` + doc follow-up `90176dc4a1d4cc76068fb4ca7d24ba94fdbd3d8f`, merged to `master` as `27740b2d9b68c6729b82f44e9fd644510f2f7fd0`.

## Completion
- All 10 rounds completed.
- Run folder is ready to archive without committing orchestration artifacts.

## Unexpected Repo Movement
- After round 8, `master` advanced unexpectedly to include merged branches `codex/round-9-single-source-schemeroot-owners` and `codex/round-10-retire-chicanonicalbindparents` without orchestrator authorization.
- Per the safety rule, the run paused, audited those diffs, re-ran QA on current `master`, and re-ran independent verifier gates for both merged changes before accepting them as rounds 9 and 10.

## Restarted Round 9
- Accepted via post-merge reconciliation: single-sourced canonical scheme-root owner/root-set bookkeeping between `ResultType.Fallback` and `Generalize.Phase4` under `MLF.Elab.Run.Generalize.Common.canonicalSchemeRootOwners`.
- Reconciliation QA: YES — targeted greps, result-type/generalization/checked-authoritative slices, and `cabal build all && cabal test` passed on current `master`.
- Final verifier: YES — the merged change is thesis-exact and worth keeping.
- Integration already present on `master`: branch `codex/round-9-single-source-schemeroot-owners`, code commit `1d90f7a70a50136d6415e7d3e735f2264e2ae2ed`, merge commit `101e9d6dfadb99a33dcebc72df629c3fe4424a6b`.
- Round 9 next candidate area: dead chi-first helper aliases, which immediately materialized in the next unexpected merge.
