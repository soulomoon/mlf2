### Selected Extraction
- Milestone: `Phase Kind And Singletons Foundation`
- Milestone id: `milestone-2`
- Direction id: `direction-2a-phase-singletons-foundation`
- Extracted item id: `phase-singletons-module-split-and-smoke-proof`
- Roadmap id: `2026-05-05-00-type-level-safety-singletons-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`

### Goal
Bring milestone-2 to reviewer-closeable shape by isolating the phase singleton
Template Haskell in a dedicated `*.Singletons` module, preserving the current
`MLF.Constraint.Types.Phase` boundary, and adding focused smoke coverage that
proves the exported singleton constructors and `Next` progression are usable at
HEAD.

### Approach
Current HEAD already satisfies part of the milestone: `singletons-th` is in
`mlf2.cabal`, `Constraint` is phase-indexed, and a focused `cabal repl
mlf2-internal` inspection shows `MLF.Constraint.Types.Phase` exports the
singleton constructors. The missing pieces are repo-guideline alignment and
focused milestone-2 evidence. Keep the round serial and bounded to the phase
owner, Cabal module registration, one focused spec, and only the directly
affected architecture wording. Do not broaden into milestone-3 graph migration,
public API changes, or unrelated witness/binder work.

### Steps
1. Split singleton generation out of `src/MLF/Constraint/Types/Phase.hs` into a
   dedicated `src/MLF/Constraint/Types/Phase/Singletons.hs` owner, keeping
   `Phase` and `Next` owned by `Phase.hs` while re-exporting the usable
   singleton surface from the top-level phase module.
2. Register the new internal module in `mlf2.cabal` and keep the type-level
   dependency surface unchanged apart from the already accepted
   `singletons-th`; if the split would force broader type-level plumbing, stop
   and record that blocker instead of widening the round.
3. Add a focused `Phase` smoke spec that compiles against the exported
   singleton surface, pattern matches each phase singleton, and proves the
   `Next` progression through a typed helper rather than only scanning source
   text.
4. Wire the new spec into both `mlf2.cabal` and `test/Main.hs`, and align only
   the directly affected phase paragraph in `docs/architecture.md` if the
   dedicated-module story changes.
5. Run the focused build and singleton smoke slice first, then diff hygiene,
   then the full `cabal build all && cabal test` gate required for a
   milestone-2 closeout attempt.

### Verification
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase singleton foundation"'`
- `git diff --check`
- `cabal build all && cabal test`
