# Review: round-290

### Checks Run
- Command: `git diff --check`
  Result: PASS. No whitespace errors reported.
- Command: `CARGO_TARGET_DIR=/tmp/round290-review-cargo-target cabal test mlf2-test --test-options='--match "named functions returning List Char source check without PhiReorder binder identity failure"'`
  Result: PASS. The prerequisite public checker regression ran 1 example with 0 failures.
- Command: `CARGO_TARGET_DIR=/tmp/round290-review-cargo-target cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution"'`
  Result: PASS. The focused native `stringToList` matcher ran 1 example with 0 failures.
- Command: `CARGO_TARGET_DIR=/tmp/round290-review-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  Result: PASS. The primitive inventory matcher ran 1 example with 0 failures.
- Command: `rg -n 'stringToList|__string_to_list|List Char|namedListCharReturnSourceProgram' src/MLF/Elab/Phi/Omega/Interpret/Internal.hs src/MLF/Elab src/MLF/Constraint src/MLF/Reify`
  Result: PASS. No matches and exit code 1, confirming the checker/elaboration fix does not special-case the selected primitive, `List Char`, or the fixture.
- Command: `rg -n -e 'stringToList : String -> List Char' -e '__string_to_list' -e 'stringToListPrimitiveName' -e 'PrimitiveNativeStringToList' -e 'RuntimeStringToList' src test docs CHANGELOG.md`
  Result: PASS. Expected matches appear in Prelude, primitive inventory, runtime, backend test/docs/changelog, and inventory tests.
- Command: `rg -n 'Unicode scalar|stringToList|String/List Char|String -> List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  Result: PASS. Docs/changelog stay bounded to the new conversion and do not claim formatting, parser parity, platform contracts, roadmap status, or self-boot completion.
- Command: `git diff master -- runtime/mlfp_io/target/release/libmlfp_io.d`
  Result: PASS. Empty diff; no generated depfile churn remains.
- Command: `git diff master -- orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap.md orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap-view.json orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/verification.md`
  Result: PASS. Empty diff; no active roadmap edits are included.

### Plan Compliance
- Recovery prerequisite: met. The public test named `named functions returning List Char source check without PhiReorder binder identity failure` checks a named `String -> List Char` function through `checkProgramFile`.
- Checker root cause: met. The diff in `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs` narrows binder-identity requirements to graph-origin binders by parsed binder id or `siSubst` membership; it does not special-case `stringToList`, `__string_to_list`, `List Char`, or the test fixture.
- Public Prelude and primitive inventory: met. `stringToList : String -> List Char` is exported from Prelude and backed by `__string_to_list`, with `PrimitiveNativeStringToList` covered by the shared primitive inventory matcher.
- Runtime/backend/native behavior: met. `run-program` constructs ordinary Prelude `Nil`/`Cons` `Char` cells, and the backend/native lowering decodes valid UTF-8 scalar bytes into list cells. The focused matcher proves source checking, `run-program`, backend LLVM/object validation, `emit-native`/native-object validation, and linked native execution for non-ASCII lambda and empty string behavior.
- Scope and docs: met. Docs/changelog describe the first native-capable `String -> List Char` conversion tracer and keep formatting, broader collection APIs, parser parity, platform contracts, driver work, and self-boot proof out of scope.
- Diff hygiene: met. No generated depfile diff remains. `orchestrator/state.json` is controller-owned active-round state and was not edited by this reviewer.

### Decision
**APPROVED**

### Evidence
The integrated recovery round satisfies both ordered acceptance targets. The prerequisite public checker regression passes without checker-layer special casing, and `stringToList` passes through public source checking, interpreter/runtime, backend/object validation, emit-native/native object validation, and linked native execution. I reused the implementer-recorded broad validation from `implementation-notes.md` (`cabal build all`, full `cabal test` with 2595 examples and 0 failures, and thesis gate passed) after independently rerunning the focused review targets and auditing the diff.
