# Task Plan: BUG-2026-02-11-004 Bounded-Alias InstBot/Phi Gap

## Objective
Close the remaining Phase 6 thesis-exactness gap for higher-arity bounded-alias chains (`BUG-003-V1/V2`) by identifying and fixing the root cause of the strict `InstBot`/Phi invariant failure.

## Scope
- Use systematic-debugging phases only (root cause before fix).
- Start from deterministic repro provided by user (`BUG-003-V`, seed `1481579064`).
- Keep strict thesis guardrails intact; no permissive fallback behavior.

## Phases
1. Root cause investigation: reproduce and capture exact failure path/data flow. (completed)
2. Pattern analysis: compare failing path vs nearby passing bounded-alias flows. (completed)
3. Hypothesis + minimal validation change. (completed; 4 minimal hypotheses tested and reverted)
4. Root-cause implementation + targeted verification. (completed; self-merge/self-bound guard landed, BUG-003 target matrix green)
5. Docs/task tracker sync (`Bugs.md`, notes, progress). (completed; closure evidence synced in this iteration)
6. Regression reopen cycle (2026-02-17): confirm current failure class, isolate drift boundary, and re-plan fix. (completed)

## Decisions
- Continue strict one-variable-at-a-time hypothesis testing only when it yields new causal evidence; revert speculative patches that do not close BUG-003.
- Current architectural mismatch remains: source-domain edge-0 witness targets are being interpreted through a root-biased Ω replay path (graft-to-root semantics + rigid-skip raises), which bottomizes the RHS before Phase 7.
- New confirmed mismatch boundary (2026-02-16): edge-0 trace binder keys in BUG-003 include source-domain IDs absent from solved nodes (for example binder `0`/`2`), while edge-0 scheme/subst generated for replay only retains `{4,8,38}`. Ω ops still target `0`, so replay operates on keys outside scheme-space.
- Implemented thesis-exact bridge hardening for this iteration:
  - `Translate` now computes `traceBinderSources` + `traceBinderReplayMap` in Φ prepass and threads them via `OmegaContext`.
  - Ω binder-target dispatch now resolves source targets through the bridge and fails fast on unmapped trace binder sources.
  - `OpRaise` keeps raw-source `I(r)` checks while using resolved execution target.
- Implemented hybrid follow-up iteration:
  - added presolution replay hints (`EdgeTrace.etBinderReplayHints`) and normalization-time derivation in `WitnessNorm`.
  - added replay-hint validation guard (`HintedOperandNotLiveTyVar`) in `WitnessValidation`.
  - added positional replay seeding in `computeTraceBinderReplayBridge`; this restored strict-matrix `make-app` guard.
- BUG-003 closure implementation (2026-02-17):
  - `MLF.Constraint.Presolution.EdgeUnify` now guards RaiseMerge emission against same-UF-class endpoints before recording/writing operations.
  - Edge-local bound writes now skip canonical self-root updates (`root(n) == root(m)`), preventing `n -> n` self-bound artifacts during χe execution.
  - Added `BUG-003-PRES` regression in `test/ElaborationSpec.hs` asserting no self-bound binder metas remain in edge-0 `prConstraint`.
  - Targeted verification is green: `BUG-003-PRES`, `BUG-003-V`, and all four strict anchors.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `cabal repl lib:mlf2-internal --repl-no-load` could not import `MLF.*` modules (hidden package context) | 1 | Switched to `cabal repl mlf2-test` target context and used qualified imports in GHCi script. |
| Initial presolution inspection script used non-exported helper (`getInteriorNodes`) and non-braced multiline `case`, causing GHCi parse failures | 1 | Rewrote script with `:{ ... :}` block and exported fields only (`show etInterior`). |
| Hypothesis #1 (disable no-scheme fallback in `reifyInst`) changed failure class (`TCLetTypeMismatch`) but did not restore BUG-003 behavior | 1 | Reverted patch; kept evidence in findings/progress. |
| Hypothesis #2 (binder-aware graft arg reification in Ω) changed failure class (`PhiTranslatabilityError`) but did not restore BUG-003 behavior | 1 | Reverted patch; kept evidence in findings/progress. |
| Hypothesis #3 (adopt graft arg via `copyMap[arg]`) showed no meaningful impact on BUG-003 behavior | 1 | Reverted patch; stop further speculative fixes in this cycle. |
| Build failed with undefined local (`runSourceRootFallback`) in `MLF.Elab.Phi.Omega` during BUG-003 repro | 1 | Replaced with explicit `PhiTranslatabilityError` branch so the suite compiles and failure analysis can continue. |
| Source-ID contract alignment initially regressed to `MissingNode (NodeId 0)` | 1 | Added source-ID node adoption for Ω op targets/args and tightened `Translate` scheme-derivation + binder-key computation; failure moved out of `MissingNode` buckets. |
| Current blocker after contract/adoption patches is `TCLetTypeMismatch` with bottomized RHS (`∀a. ⊥ -> t1 -> ⊥ -> ⊥`) against expected `∀a. a -> a -> a -> a` | 1 | Not yet resolved; remaining work is in Φ/Ω replay semantics (edge-0 raises currently no-op under rigid skip), not in fallback missing-node plumbing. |
| GHCi comparison/probe scripts failed with parse/layout and missing constructor imports (`NodeId`, `ew*` fields) | 2 | Rewrote scripts with qualified imports and non-ambiguous top-level `let` bindings in `cabal repl mlf2-test --repl-no-load` mode. |
| Hypothesis #4 (disable `OpRaise` bottom-to-root remap for trace binder sources) changed edge-0 Φ from `InstId` to non-noop, but BUG-003 still failed with `TCLetTypeMismatch` and extra bottomized quantifier | 1 | Reverted patch; kept causal evidence that remap contributes to collapse but is not sole root cause. |
| Internal plan introspection via `cabal repl` failed because required modules are hidden from component exports | 1 | Switched to `cabal exec runghc -- -XLambdaCase -isrc -isrc-public -iapp -itest ...` to inspect hidden plan modules directly. |
| Parallel cabal test runs collided on package-db lock (`package.conf.inplace`/`package.cache.lock`) | 1 | Re-ran affected tests sequentially; avoid parallel `cabal test` invocations in the same workspace. |
| Full gate after bridge hardening remains red (`674 examples, 47 failures`) including new bridge-contract failures outside BUG-003 | 1 | Logged as open follow-up (`BUG-2026-02-16-010`); keep BUG-003 task in progress and scoped verification focused. |
| Hypothesis #5 (allow traced `OpRaise` to bypass rigid-skip) changed BUG-003 shape to extra-bottom quantifier but did not close strict-success target | 1 | Reverted behavior to rigid-skip baseline; kept finding as causal-only evidence. |
| Hypothesis #6 (force bounded `OpGraft` replay to emit concrete instantiation on `⊥`/bound-match) failed immediately with `PhiInvariantError` (`InstBot expects ⊥`) | 2 | Reverted patch; preserve strict InstBot semantics and continue source/replay mapping-focused investigation. |
| Full gate after replay-hint + positional-seeding iteration remains red (`674 examples, 33 failures`) | 1 | Kept BUG-003/BUG-010 open; synced tracker with narrowed remaining under-coverage repros. |

## 2026-02-16 Additional Error Log (current session)
- Hypothesis A (`OpRaise` traced-rigid-skip bypass) changed BUG-003 shape (`∀a. ∀u0...`) but did not close strict-success target. Reverted.
- Hypothesis B (disable rigid-skip for all raises) produced `PhiTranslatabilityError "OpRaise target outside I(r)"` on `OpRaise 12`. Reverted.
- Hypothesis C (keep annotation-edge weakens) surfaced `WitnessNormalizationError (AmbiguousGraftWeaken ...)` for edge-0. Reverted.
- Hypothesis D (deterministic ambiguous graft/weaken resolution) removed normalization error but BUG-003 remained in baseline `TCLetTypeMismatch` bucket. Reverted.
- Hypothesis E (`reifyInst` scheme recovery from `AAnn`) showed no measurable BUG-003 impact and introduced warning churn; reverted.

## 2026-02-16 Implementation Pass: normalization-side deterministic graft+weaken

### Decisions
- Implemented annotation-edge-only deterministic synthesis in `WitnessNorm` before `normalizeInstanceStepsFull`.
- Trigger is restricted to ambiguous multi-graft/no-weaken Ω segments and preserves `StepIntro` boundaries.
- Synthesis emits explicit `OpGraft+OpWeaken` pair and fails fast with explicit `OmegaNormalizeError` when no live candidate arg can be chosen.
- Φ/Ω fail-fast semantics were not relaxed in this pass.

### Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| BUG-003 moved from `TCLetTypeMismatch` to `PhiInvariantError \"trace/replay binder key-space mismatch\"` on synthesized `OpGraft+OpWeaken` target key `6` | 1 | Added replay-key grouping and tighter trigger constraints in synthesis; BUG-003 remains open in the new bucket. |
| Parallel targeted `cabal test` invocations collided on package-db lock (`package.conf.inplace already exists`) | 1 | Re-ran the affected test matches sequentially. |

## 2026-02-16 Bridge Continuation (post-synthesis mismatch)

### Decisions
- Keep strict fail-fast replay invariants in Ω unchanged.
- Fix only bridge candidate under-coverage in Φ replay-map construction and bounded pair translation semantics.
- Preserve existing synthesis contract and tests; do not relax Φ/Ω strictness to mask mapping gaps.

### Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| BUG-003 replay mismatch on synthesized source key `6` (`trace/replay binder key-space mismatch`) | 1 | Added hint-class peer candidate seeding in `MLF.Elab.Phi.Translate.computeTraceBinderReplayBridge`; BUG-003 moved past mismatch bucket. |
| New downstream `PhiInvariantError`: `OpGraft+OpWeaken(bound-match): InstBot expects ⊥` | 1 | Updated bounded `OpGraft+OpWeaken` branch in `MLF.Elab.Phi.Omega` to emit `InstElim` (elimination-equivalent) instead of bounded `InstApp`; error removed. |
| BUG-003 remains open (`TCLetTypeMismatch` bottomized RHS) after removing both new regression buckets | 1 | Marked as unresolved baseline semantic gap; preserved fixes that restore pre-regression bucket and kept task in progress. |

## 2026-02-16 Baseline Root-Cause Trace Update (pre-Φ bottomization)

### Decisions
- Treat this cycle as Phase-1/Phase-2 debugging only (no speculative fix patch): gather edge-local execution evidence for BUG-003 edge-0 and stop at first irreversible semantic drift.
- Use internal-component probes (`cabal repl mlf2:mlf2-internal`) to inspect hidden presolution modules directly.
- Record the earliest mutation boundary as the current blocker for implementation:
  - edge-0 `runExpansionUnify` emits self-merge extra ops (`OpMerge 2 2`, `OpMerge 0 0`);
  - canonical bound writes then create eliminated self-bound metas (`35`, `37`);
  - solve rewrites eliminated self-bound `35` to `TyBottom 46`, bottomizing scheme-bound arrows before Φ.

### Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `runghc` direct probes failed because repo-wide Cabal `default-extensions` (for example `LambdaCase`) were not active in plain `runghc` module loading | 1 | Switched to `cabal repl mlf2:mlf2-internal` scripted execution (`:load ...; main`) so probes run under component flags/options. |
| First edge-0 probe omitted several hidden-module selectors/imports (`epr*`, `rte*`, `et*`, `emptyTrace`, `getPresolutionState`) and failed to compile | 1 | Added explicit imports from `EdgeProcessing.Plan`, `Presolution.Base`, and `Witness`; reran until probe produced extra-op and state snapshots. |
| Initial implementation with only RaiseMerge emission guard fixed `BUG-003-V` but still failed new `BUG-003-PRES` (`self-bound binder metas in prConstraint: [35]`) | 1 | Added edge-local bound-write same-root no-op guard in `EdgeUnify.setVarBoundM` (skip when canonical roots are equal); reran full targeted matrix to green. |

## 2026-02-17 Regression Reopen (new evidence)

### Decisions
- Reopened `BUG-2026-02-11-004` in `/Volumes/src/mlf4/Bugs.md` after deterministic red repro in the current workspace.
- Continue strict `systematic-debugging`: no implementation fix until drift source is isolated.
- Use clean-worktree A/B checks to distinguish baseline branch behavior from current workspace-local edits.

### Evidence Snapshot
- Current workspace deterministic failures:
  - `cabal test ... --match "BUG-003-V" --seed 1925916871` => `2 failures` (both variants bottomized).
  - `cabal test ... --match "BUG-003-V" --seed 1481579064` => same `2 failures`.
  - `cabal test ... --match "BUG-003-PRES" --seed 1925916871` => PASS.
  - `cabal test ... --match "interleaves StepIntro with Omega ops in Φ translation" --seed 1925916871` => FAIL (`"O"` vs `"O; ∀(u0 ⩾) N"`).
- Clean detached `HEAD` worktree (`487db7d`) A/B:
  - `interleaves StepIntro ...` => PASS.
  - This confirms at least part of current Φ/Σ drift is workspace-local and not present in clean `HEAD`.

### Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Long-running clean-worktree `cabal test` timed out at first poll | 1 | Continued via interactive session polling (`write_stdin`) until full result completed. |

## 2026-02-17 Closure Update (regression reopen cycle)

### Decisions
- Closed the companion Φ drift first: restored trace-free keep-key behavior in `MLF.Elab.Phi.Translate.computeTargetBinderKeys` (`mTrace = Nothing` now returns `IntSet.empty`).
- Isolated BUG-003 regression root cause to `MLF.Elab.Elaborate` annotation-instantiation adjustment:
  - variable-annotation path converted `InstInside (InstBot t)` into `InstApp t`,
  - BUG-003 V1/V2 then instantiated `c` with `InstApp TBottom` and bottomized the result.
- Removed that variable-only conversion while preserving existing expected-bound adjustment (`InstInside (InstBot _)` with expected bound still maps to `InstApp expectedBound`).

### Targeted Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "interleaves StepIntro with Omega ops in Φ translation" --seed 1925916871'` => PASS.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-PRES" --seed 1925916871'` => PASS.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003-V" --seed 1925916871'` => PASS (`2 examples, 0 failures`).
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004" --seed 1925916871'` => PASS (`4 examples, 0 failures`).

### Notes
- The separate baseline test `term annotation can instantiate a polymorphic result` remains failing in this workspace both before and after this BUG-003 fix; tracked as unrelated drift.
