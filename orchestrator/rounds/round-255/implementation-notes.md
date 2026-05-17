## Round 255 Implementation Notes

### Changes Made

- Updated `docs/mlfp-self-boot-readiness.md` with a seed-driven primitive and standard-library gap budget tied to the merged lexer/parser seed evidence from `test/programs/compiler-seed/frontend-contract/`.
- Classified the required gap categories:
  - text, characters, or bytes: `needed-before-larger-compiler`
  - collection operations: `needed-before-larger-compiler`
  - maps and sets: `deferred`
  - parser helpers: `needed-before-larger-compiler`
  - error accumulation: `needed-before-larger-compiler`
  - IO helpers: `deferred`
  - diagnostics: `needed-before-larger-compiler`
- Marked broad package-manager, persisted-interface, ABI, linker, separate-compilation, general FFI, backend-native redesign, and full compiler-driver work as `roadmap-update-required` rather than in-round implementation scope.
- Updated readiness wording so the existing primitive and standard-library rows point to the budget without claiming new primitives, Prelude helpers, parser combinators, backend/native support, package-manager behavior, ABI/linker support, or separate compilation.

### Scope Notes

- No parser seed, checker, backend/native, package manager, ABI/linker, separate-compilation, primitive, or Prelude implementation was added.
- No roadmap artifact was edited.
- No structural guard was added. The existing low-brittleness guard pattern in this checkout is `test/RepoGuardSpec.hs`, which guards module wiring/import/public-surface structure. This docs ledger would require prose-substring checks, so a guard would be brittle for this round.
- Validation regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with the round worktree path; that generated churn was restored to the checked-in root path and is not part of the final diff.

### Validation

- Manual ledger validation: PASS. Every required category from the round plan is classified and tied to seed/source/runtime/backend evidence or fail-closed behavior.
- `git diff --check`: PASS (no output).
- `cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract`: PASS. Output:

  ```text
  lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol
  parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true
  ```

- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`: PASS. Result: `Finished in 77.0563 seconds`, `2 examples, 0 failures`, `Test suite mlf2-test: PASS`.
- `cabal build all`: PASS.
- `cabal test`: PASS. Result: `Finished in 486.6404 seconds`, `2562 examples, 0 failures`, `Test suite mlf2-test: PASS`, `1 of 1 test suites (1 of 1 test cases) passed.`
- `./scripts/thesis-conformance-gate.sh`: PASS. Final markers:
  - `[thesis-obligations] PASS: all obligations are mapped and green`
  - `[thesis-claims] PASS: all validations green`
  - `[thesis-gate] PASS: thesis conformance anchors are green`
