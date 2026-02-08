# Findings & Decisions

## Requirements
- Diagnose Phase 7 `TCLetTypeMismatch` failure mode where `make`'s let-scheme specializes to `... -> Int` but RHS elaboration stays polymorphic
- Report likely root causes, enumerate 2-3 fix candidates with tradeoffs, list exact file/function touch points, and recommend one approach
- Focus on `MLF.Elab.Elaborate`, `MLF.Elab.Generalize`, `MLF.Elab.Run.Scope`, and presolution plan modules (`GammaPlan`, `ReifyPlan`)

## Research Findings
- `Bugs.md` currently tracks `BUG-2026-02-06-002` as a Phase 6 Φ translation failure using `make`, with the Phase 7 `TCLetTypeMismatch` regression described as `BUG-2026-02-08-004`; both point at `MLF.Elab.Elaborate` and presolution plan modules (GammaPlan/ReifyPlan).
- `docs/notes/bug-2026-02-06-003-trace-report-2026-02-07.md` demonstrates how missing alias-bound metadata in presolution/Φ leads to Poly→⊥ mismatches, emphasizing the need to keep GammaPlan/ReifyPlan and let-scope reification aligned.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
|          |           |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| Catchup script path missing | Noted; proceeding without it |

## Resources
-

## Visual/Browser Findings
-

---
*Update this file after every 2 view/browser/search operations*
*This prevents visual information from being lost*

## 2026-02-08 BUG-2026-02-06-002 Reproduction Update
- Exact reproducer from `Bugs.md` now fails at **Phase 7**, not Phase 6.
- Repro command:
  - `cabal exec runghc /tmp/repro-bug-2026-02-06-002.hs`
- Stable observed error:
  - `Phase 7 (type checking): TCLetTypeMismatch (TForall "a" Nothing (TForall "b" Nothing (TArrow (TVar "a") (TArrow (TVar "t23") (TVar "a"))))) (TForall "a" Nothing (TForall "b" Nothing (TArrow (TVar "a") (TArrow (TVar "b") (TBase Int)))))`
- Interpretation:
  - Pipeline reaches elaboration successfully; mismatch appears between let annotation (more polymorphic body) and elaborated RHS body (specialized to `Int` in codomain).
  - Existing `BUG-2026-02-06-002` tracker entry is stale on phase/error details and overlaps with Phase-7 issue family.

## Pattern Analysis (Systematic Debugging Phase 2)
- Comparison matrix (`/tmp/repro-make-matrix.hs`):
  - `let make = \x.\y.x in ...` variants consistently fail (Phase 7 `TCLetTypeMismatch`, and one `SchemeFreeVars` in Phase 6 depending on body shape).
  - `let id = \x.x in id id` succeeds.
- This isolates the regression to polymorphic let-generalization/elaboration interactions for higher-order constant functions, not a global pipeline failure.
- Trace evidence (`/tmp/repro-bug-2026-02-06-002-trace.out`):
  - Let-scheme for `make` becomes `∀a b. a -> b -> Int` during elaboration generalization.
  - Elaborated RHS remains `λx:a. λy:t23. x` (type `a -> t23 -> a`).
  - Phase 7 mismatch is therefore genuine and deterministic, not a type-checker false positive.

## Data-Flow Evidence (Generalization Mapping)
- Internal dump (`/tmp/repro-constraint-dump.out`) shows `constraintForGeneralization` produces suspicious solved→base collisions:
  - `s2b[0] = 5`, `s2b[1] = 5` (distinct solved binders collapsed to same base key).
- `instCopyMapFull` contains entries that look non-copy / ownership-overriding:
  - `fromList [..., (0 -> 5), (1 -> 5), ...]`
- These entries are consumed as left-biased `copyOverrides` in `Phase2` node mapping, potentially overriding valid solved→base associations for lambda parameters.
- This aligns with observed let-scheme corruption (`make` specialized to `... -> Int`) while RHS remains polymorphic (`a -> t23 -> a`).

## Hypothesis Tests (Phase 3)
- H1 (tested): change `Phase2` copy-override precedence (`solvedToBase1` union order) to preserve existing solved→base mapping.
  - Outcome: no user-visible change for `BUG-2026-02-06-002`.
- H2 (tested): generalize lets at scheme root (`NodeId 5`) instead of `schemeBodyTarget` (`NodeId 4`).
  - Outcome: regressed broadly (`TCTypeAbsBoundMentionsVar`, `PhiInvariantError`); reverted.
  - Additional evidence: direct generalization differs strongly by target:
    - target `5` → `Right (Forall ... TVar "c", ...)`
    - target `4` → `Right (Forall [a,b] (a -> b -> Int), ...)`
- H3 (tested): always prefer base TyVar in `Phase1.restoreSchemeNodes` (`preferBaseVar`).
  - Outcome: shifts failure to `SchemeFreeVars` (`t25`) and reveals free-var pressure; reverted.
- H4 (tested): change final aligned solved→base precedence in `Finalize` to preserve `solvedToBaseAligned0` before `copyOverrides`.
  - Outcome: mapping collisions improved (`s2b[0]=0`, `s2b[1]=1`), but end-user bug still reproduces; reverted.

## Current Root-Cause Direction
- Primary corruption point appears in binder selection within `MLF.Constraint.Presolution.Plan.BinderPlan.Build`:
  - Candidate `NodeId 25` (reachable/in-gamma) is filtered out by representative gating tied to `solvedToBasePref` + `baseGammaRep`.
  - The representative picked for that base key is not ultimately retained for the same scope, which leaves either:
    - over-specialized scheme (`... -> Int`) when fallback representatives survive, or
    - `SchemeFreeVars` when restoration prefers base-polymorphic vars.
- This explains why symptoms oscillate between Phase 7 `TCLetTypeMismatch` and Phase 6 `SchemeFreeVars` under small mapping-policy tweaks.
- H5 (tested): relax representative suppression gate in `BinderPlan.Build` to keep reachable binders.
  - Outcome: no behavior change on reproducer (`TCLetTypeMismatch` unchanged).
- H6 (tested): run generalization on `constraintForGen` with empty union-find (in `Run.Pipeline`).
  - Outcome: no behavior change on reproducer (`TCLetTypeMismatch` unchanged); reverted.
