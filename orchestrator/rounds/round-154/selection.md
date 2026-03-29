# Round 154 — Task Selection

## Selected Item

**Item 4**: Upgrade pipeline entrypoint test from expected-failure to success

## Rationale

Items 1–3 are complete. Item 4 is the next unfinished item and depends on
items 2 and 3 (both done). It upgrades the PipelineSpec:2303 blocker test
from `expectStrictPipelineFailure` to a success assertion.

## Critical Context

The round-153 reviewer noted that **PipelineSpec:2303 still passes with
`expectStrictPipelineFailure`** after items 2+3. This means there is at least
one additional downstream blocker beyond the two fixes already merged. The
implementer must:

1. **Investigate** what the remaining failure is (run the pipeline for the
   non-local proxy expression, inspect the error)
2. **Fix** any additional blockers found
3. **Then** upgrade the test from expected-failure to success
4. **Survey** ElaborationSpec PhiTranslatabilityError tests (12+ sites) to
   determine which are the same non-local proxy pattern vs legitimate
   untranslatable cases

## Expression Under Test

```haskell
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
    expr = ELet "g" (ELamAnn "x" recursiveAnn (EVar "x")) (EApp (EVar "g") (EVar "g"))
```

## Dependencies

- item-2: ✅ done (round 152)
- item-3: ✅ done (round 153)
