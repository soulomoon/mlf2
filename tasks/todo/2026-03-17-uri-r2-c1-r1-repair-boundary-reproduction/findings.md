# Findings

## Current Observations

- `D2` fixed the first divergence boundary to `witness-replay/applyInstantiation-instbot-precondition` and recorded the no-fallback replay shape as `t5 -> t5` while witness replay dies with `InstBot expects ⊥, got: t9 -> t9`.
- `D3` converted that localization into one bounded repair-supporting direction, but did not change production code; `R1` must only restate the failure in implementation-facing terms.
- The bounded `URI-R2-C1` fixture used by the predecessor prototype is `EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))`, so a focused production-path reproducer can stay inside existing elaboration/test helpers.
- The new implementation-facing helper reconstructs the live owner-call inputs as:
  - `schemeToType`: `∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b`
  - no-fallback replay lane: `t5 -> t5`
  - witness replay instantiation: `∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N)`
- Those live inputs confirm the owner-local failure still occurs exactly at `applyInstantiation` with `InstantiationError "InstBot expects ⊥, got: t9 -> t9"`.
