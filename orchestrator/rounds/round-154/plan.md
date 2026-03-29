# Round 154 Plan — Item 4: Upgrade pipeline entrypoint test from expected-failure to success

## Investigation Results

Running the pipeline in GHCi reveals the actual error:

```haskell
runPipelineElab Set.empty (unsafeNormalizeExpr expr)
-- Result:
Left (PipelineTypeCheckError 
  (TCArgumentMismatch (TVar "a") 
    (TForall "b" Nothing (TArrow TBottom (TVar "b")))))
```

**Key finding**: The error is NOT a `PhiTranslatabilityError` (items 2+3 fixed
those). The elaboration now succeeds, but the elaborated term **fails type
checking**. The type checker finds that in an application `f a`, the function
expects domain type `TVar "a"` but the argument has type
`∀b. ⊥→b`.

This is a **type soundness** issue in the elaborated output. For the expression:

```haskell
let g = (λx:(μa. a→Int). x) in g g
```

The expected correct behavior with iso-recursive types would be:
1. `g : μa. a→Int` (the annotated identity returns its input with the μ-type)
2. `g g` should work because `μa. a→Int` unfolds to `(μa. a→Int)→Int`
3. The application `g g` should produce type `Int` after appropriate
   fold/unfold coercions

The type checker mismatch (`TVar "a"` vs `∀b. ⊥→b`) suggests the
elaboration is not correctly inserting fold/unfold coercions or the
generalization is producing an incorrect scheme.

## Assessment

This is a **deeper elaboration correctness issue** that goes beyond the scope
of "flip a test from expected-failure to success." The roadmap item assumed
items 2+3 would make the pipeline succeed, but a third issue exists: the
elaborated term is ill-typed.

## Recommended Approach

Since fixing this type soundness issue may require significant investigation
into the elaboration/generalization pipeline, and the roadmap item says:

> "Survey test/ElaborationSpec.hs PhiTranslatabilityError-asserting tests
> (12+ sites) and determine which, if any, are testing the same non-local
> proxy pattern vs legitimate untranslatable cases — document findings"

The plan should:

### Step 1: Document the actual error
- Record that items 2+3 resolved PhiTranslatabilityError but revealed a
  deeper TCArgumentMismatch
- Update the test description to reflect the actual current error
  (TCArgumentMismatch, not PhiTranslatabilityError)

### Step 2: Survey ElaborationSpec PhiTranslatabilityError tests
- Examine all 12+ sites in test/ElaborationSpec.hs that assert
  PhiTranslatabilityError
- Classify each as: (a) same non-local proxy pattern, (b) legitimate
  untranslatable case, or (c) other
- Document findings in implementation-notes.md

### Step 3: Attempt to fix TCArgumentMismatch
- Investigate what the elaborated term looks like (the term before type
  check)
- Determine if the issue is in generalization (wrong scheme), elaboration
  (missing fold/unfold), or type checking (overly strict)
- If the fix is localized (≤20 lines), implement it and upgrade the test
- If the fix is complex, update the test description to document the
  remaining blocker and mark item-4 as partially complete with findings

### Step 4: Upgrade test or update description
- If Step 3 succeeds: change `expectStrictPipelineFailure` to a success
  assertion validating the output type
- If Step 3 reveals a deeper issue: update the test description from
  "hits elaboration blocker" to "produces ill-typed elaboration for
  non-local proxy" and document the remaining gap

### Step 5: Build and test
- `cabal build all && cabal test` must pass with 0 failures

## Files to Modify

- `test/PipelineSpec.hs` — line 2336 (test description + assertion change)
- `test/ElaborationSpec.hs` — survey only (document findings)
- `orchestrator/rounds/round-154/implementation-notes.md` — findings

## Verification Criteria

- `cabal build all && cabal test` passes
- Test at PipelineSpec:2336 either:
  - Asserts success with correct recursive output type, OR
  - Has updated description accurately reflecting TCArgumentMismatch blocker
- ElaborationSpec survey findings documented
- No regressions from 1176 examples baseline
