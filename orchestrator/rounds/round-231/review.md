# Round 231 Review

Date: 2026-05-15
Round: `round-231`
Milestone: `milestone-2`
Direction: `direction-2a-phase-singletons-foundation`
Extracted item: `phase-singletons-module-split-and-smoke-proof`
Base branch: `master`
Branch: `orchestrator/round-231-phase-singletons-foundation`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Result: pass. The live round lineage matches the assigned worktree and branch: `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`, and `active_rounds[0] = round-231` at stage `review`.

- Command: `git status --short`
  Result: pass. Implementation payload is limited to the selected phase-owner files and direct evidence surfaces: [src/MLF/Constraint/Types/Phase.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase.hs:1), [src/MLF/Constraint/Types/Phase/Singletons.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase/Singletons.hs:1), [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:1), [test/Main.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/Main.hs:47), [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:57), and [docs/architecture.md](/Volumes/src/mlf4/orchestrator/worktrees/round-231/docs/architecture.md:120), plus the round-local role artifacts under [orchestrator/rounds/round-231](/Volumes/src/mlf4/orchestrator/worktrees/round-231/orchestrator/rounds/round-231/review.md:1). `orchestrator/state.json` is also dirty, but only as controller-owned stage bookkeeping and is not treated as round payload.

- Command: `rg -n "Phase\\.Singletons|SRaw|SNormalized|SAcyclic|SPresolved|SSolved|type family Next|data Phase =" -g'*.hs' src test src-public`
  Result: pass. The singleton boilerplate is isolated in [src/MLF/Constraint/Types/Phase/Singletons.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase/Singletons.hs:16), the stable caller-facing boundary re-exports that surface from [src/MLF/Constraint/Types/Phase.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase.hs:20), and no other production or test module imports `MLF.Constraint.Types.Phase.Singletons` directly.

- Command: `rg -n "MLF\\.Constraint\\.Types\\.Phase\\.Singletons|PhaseSingletonsSpec|singletons-th" mlf2.cabal test/Main.hs test/PhaseSingletonsSpec.hs src/MLF/Constraint/Types/Phase.hs src/MLF/Constraint/Types/Phase/Singletons.hs`
  Result: pass. `singletons-th` remains the only type-level dependency at [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:276), the dedicated owner module is registered in the private `mlf2-internal` library at [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:57), and the focused spec is registered in both [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:387) and [test/Main.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/Main.hs:47).

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- src-public`
  Result: pass. No public API files changed.

- Command: `python3 -c 'import json; from pathlib import Path; obj=json.loads(Path("orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap-view.json").read_text()); ms=next(m for m in obj["milestones"] if m["milestone_id"]=="milestone-2"); assert ms["status"]=="pending"; [obj["anchors"][k] for k in ["milestone-2","milestone-2-completion","roadmap-history-completed-rounds"]]; print("milestone-2 closeout anchors resolve")'`
  Result: pass. `roadmap-view.json` reports `milestone-2` currently `pending`, and the status anchor `milestone-2`, completion anchor `milestone-2-completion`, and history anchor `roadmap-history-completed-rounds` all resolve.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target builds successfully, proving [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:7) compiles against `MLF.Constraint.Types.Phase` rather than the dedicated owner module.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase singleton foundation"'`
  Result: pass. The focused smoke slice passed with `2 examples, 0 failures`, covering the exported singleton constructors and `Next`-typed phase progression at [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:27).

- Command: `cabal build all && cabal test`
  Result: pass. Full behavior-changing gate passed with `2566 examples, 0 failures` in `368.6758` seconds.

## Plan Compliance

- `Split singleton generation out of src/MLF/Constraint/Types/Phase.hs into a dedicated src/MLF/Constraint/Types/Phase/Singletons.hs owner, keeping Phase and Next owned by Phase.hs while re-exporting the usable singleton surface from the top-level phase module`: met with an acceptable ownership refinement. The dedicated owner now contains the `Phase` declaration plus generated singleton boilerplate at [src/MLF/Constraint/Types/Phase/Singletons.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase/Singletons.hs:16), while [src/MLF/Constraint/Types/Phase.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase.hs:20) remains the stable caller-facing boundary by re-exporting that surface and retaining `Next`. This does not weaken the intended boundary because `mlf2-internal` is a private library at [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:61), no caller was migrated to the dedicated module, and the focused spec imports only `MLF.Constraint.Types.Phase` at [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:7).

- `Register the new internal module in mlf2.cabal and keep the type-level dependency surface unchanged apart from the already accepted singletons-th`: met. [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:71) registers `MLF.Constraint.Types.Phase.Singletons`, and the only type-level dependency referenced by the round remains `singletons-th` at [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:276). No broader type-level plumbing or extra library dependency was introduced.

- `Add a focused Phase smoke spec that compiles against the exported singleton surface, pattern matches each phase singleton, and proves the Next progression through a typed helper rather than only scanning source text`: met. [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:10) existentially packages each exported `SPhase`, pattern matches every singleton constructor at [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:13), and proves `Next` progression through the typed helper at [test/PhaseSingletonsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/PhaseSingletonsSpec.hs:20).

- `Wire the new spec into both mlf2.cabal and test/Main.hs, and align only the directly affected phase paragraph in docs/architecture.md if the dedicated-module story changes`: met. The spec is registered in [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:387) and executed from [test/Main.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/test/Main.hs:78). The directly affected phase paragraph now records the dedicated owner plus top-level re-export boundary at [docs/architecture.md](/Volumes/src/mlf4/orchestrator/worktrees/round-231/docs/architecture.md:120) without widening unrelated architectural claims.

- `Run the focused build and singleton smoke slice first, then diff hygiene, then the full cabal gate required for a milestone-2 closeout attempt`: met. `cabal build mlf2-test`, the focused `Phase singleton foundation` slice, `git diff --check`, and the full `cabal build all && cabal test` gate all passed in the required order.

## Decision

**APPROVED**

## Evidence

- The round stays inside the selected `milestone-2` / `direction-2a-phase-singletons-foundation` scope from [orchestrator/rounds/round-231/plan.md](/Volumes/src/mlf4/orchestrator/worktrees/round-231/orchestrator/rounds/round-231/plan.md:1). The implementation edits are limited to the phase owner module split, its direct test registration, one focused smoke spec, and the directly affected architecture paragraph; `src-public/` is untouched.

- The milestone-2 completion signal is satisfied at HEAD. `singletons-th` is registered in [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:276), the `Phase` kind and generated singleton type compile from the stable boundary in [src/MLF/Constraint/Types/Phase.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase.hs:20), the dedicated singleton owner is isolated in [src/MLF/Constraint/Types/Phase/Singletons.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-231/src/MLF/Constraint/Types/Phase/Singletons.hs:12), focused smoke evidence passes, and the full cabal gate is green.

- Moving the `Phase` declaration into the dedicated owner is acceptable under the round plan and repo guidance because the boundary promise is preserved where it matters: `MLF.Constraint.Types.Phase` remains the stable import surface for callers, `mlf2-internal` is explicitly private at [mlf2.cabal](/Volumes/src/mlf4/orchestrator/worktrees/round-231/mlf2.cabal:61), and the architecture docs now state the dedicated-owner plus re-export design at [docs/architecture.md](/Volumes/src/mlf4/orchestrator/worktrees/round-231/docs/architecture.md:122). If callers had been migrated to `MLF.Constraint.Types.Phase.Singletons` directly or if a public `src-public` surface had changed, this would not be acceptable; neither happened here.

- No public API widening occurred. `git diff --name-only -- src-public` is empty, and the only new import path is inside the private internal library and remains unused by direct callers outside the re-export boundary.

- Milestone closeout can stay `status-only`. The active roadmap meaning, direction meaning, sequencing, and verification contract do not change; the round simply satisfies the existing milestone-2 completion signal with focused plus full-gate evidence. `milestone-2` may therefore move from `pending` to `done` without a semantic roadmap update.
