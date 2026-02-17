# Findings: BUG-2026-02-17-001 Phi Keep-Key Drift

## 2026-02-17 root-cause findings
- Deterministic failing thesis baselines (seed `529747475`):
  - `id y should have type ∀a. a -> a` fails with codomain `⊥`.
  - `term annotation can instantiate a polymorphic result` fails with extra nested bounded `forall` layers.
  - `explicit forall annotation preserves foralls in bounds` fails with body variables leaking as solver names (`t0`).
- Open bug repro commands from `Bugs.md` for `BUG-2026-02-16-010` and `BUG-2026-02-14-003` are currently green with seed `1593170056`; those entries appear stale relative to present gate failures.
- Trace-enabled pipeline evidence for failing `id y` program shows:
  - Φ edge-0 target binders are empty (`phi target binders=[]`) but keep-keys are non-empty (`keep-keys=[1]`).
  - Witness ops include `OpGraft ...`, `OpRaise ...`, `OpWeaken ...`.
  - Because binder key `1` is kept, `OpWeaken` elimination is skipped, and resulting instantiation remains bottomizing/over-quantifying.
- Candidate root cause:
  - `computeTargetBinderKeys` in `MLF.Elab.Phi.Translate` currently returns all scheme replay keys when target binders are empty; this broad fallback diverges from strict intersection semantics and appears to over-retain binders.

## 2026-02-17 implementation findings
- Restoring strict keep-key intersection removed one major drift class, but did not fully fix `id y`; trace showed an additional problematic Φ-op triple:
  - `OpGraft arg b ; OpRaise b ; OpWeaken b` on unbounded binders.
- In failing and nearby baseline cases, this triple expanded to over-complex instantiations and retained bottomization pressure; collapsing it to direct `InstApp` at binder translation restored expected paper baselines.
- Annotation failures were not solely Φ-bridge related:
  - Generic non-var fallback in `reifyInst` fixed annotation baselines but regressed `BUG-003` bounded-alias variants.
  - Safe fix: keep non-var fallback disabled globally and add targeted `AAnnF` fallback when `inst == InstId`, `expectedBound` exists, and source annotation is non-variable.
  - This restored annotation baselines while preserving BUG-003 strict-success regressions.
- Current verification snapshot:
  - `cabal build all` passes.
  - `cabal test` now fails in 3 remaining buckets unrelated to this bug’s reproducer cluster:
    - `PipelineSpec` redirect/canonicalization node-rewrite invariant.
    - Φ fail-fast contract for missing replay binder mapping (`OpWeaken` expected invariant class).
    - Φ non-spine `OpRaise` root-fallback translatability case.

## 2026-02-17 closure findings
- Residual Φ fail-fast mismatch root cause:
  - `resolveTraceBinderTarget` previously returned non-binder fallback candidates for trace-source binder ops when no replay binder candidate existed.
  - This deferred failure into `PhiTranslatabilityError ("OpWeaken targets non-binder node")` instead of the contract-level invariant.
  - Fix: require binder-candidate presence for trace-source binder ops (`requireBinder=True`) and fail fast with `PhiInvariantError` when absent.
- Residual non-spine `OpRaise` root-fallback mismatch root cause:
  - Ω required a root-fallback insertion for all non-`⊥` bounds even when a valid `C^m_n` context candidate existed.
  - This over-constrained Fig.10 non-spine translation and rejected a valid context-path witness.
  - Fix: execute context-path intro/bot/alias translation when candidate context exists; keep root fallback for no-candidate cases.
- `PipelineSpec` failure was assertion brittleness, not production drift:
  - solved `union-find` can be empty for the tested program; requiring non-empty canonicalized scheme roots was over-specific.
  - strict stale-node and root canonicalization checks remain sufficient for behavior.
- Final state:
  - `cabal build all && cabal test` is fully green (`678 examples, 0 failures`).
