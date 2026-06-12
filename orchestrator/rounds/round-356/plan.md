### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-356-recursive-method-row-continuation-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Finish the method-row substrate left dependency-ready by round 355 by replacing
the remaining class and instance method-row exact-count continuation ladders
with true self-recursive continuation helpers.

This is milestone-4 parser/compiler-frontend ergonomics substrate only. It
does not claim full parser parity, compiler-package implementation,
platform/proof progress, native/backend completion, package-manager/linker
work, self-boot completion, or milestone closeout.

### Approach
Use the established recursive parser-library pattern already present for
module-body declaration rows and recursive constructor rows:

- collapse `parseEqClassMethodRowsMoreOrClose3/2/1/0` into one
  `parseEqClassMethodRowsMoreOrClose` helper that either closes the class body
  or parses the next method signature row and recurs;
- collapse `appendClassMethodRowsAndContinue2/1/0` into one recursive
  `appendClassMethodRowsAndContinue` helper;
- collapse `parseEqNatInstanceMethodRowsMoreOrClose3/2/1/0` into one
  `parseEqNatInstanceMethodRowsMoreOrClose` helper that either closes the
  instance body or parses the next method definition row and recurs;
- collapse `appendInstanceMethodRowsAndContinue2/1/0` into one recursive
  `appendInstanceMethodRowsAndContinue` helper;
- replace the temporary `eq`/`neq` method-name parser with ordinary method
  identifiers inside the existing class/instance method-row surface, then
  strengthen the recursive method-row source text so the dynamic parser-parity
  check covers more unique method rows than the old fixed cap;
- update the static guard to require the self-recursive helpers and reject the
  retired numbered continuations.

Do not widen the parser grammar outside the existing class/instance method-row
surface. Do not add fixture-name shortcuts, pre-rendered projections,
canonical-parser bypasses, compatibility aliases, retired syntax shims, or
parser-private hacks that hide missing reusable substrate.

### Execution Profile
- Complexity: simple
- Verification profile: focused
- Reason: The selected task has a clear goal, follows an established local
  recursive parser pattern, and has a clear verification boundary in the
  existing method-row dynamic Hspec, method-row static guard, and aggregate
  parser-parity group. It touches only the shared parser-library test source
  and its Hspec guard, without new production Haskell behavior or a new shared
  abstraction.

### Steps
1. Confirm the assigned worktree is
   `orchestrator/worktrees/round-356` on
   `orchestrator/round-356-next-m4-parser-slice`.
2. Replace numbered class method-row `MoreOrClose` and append continuations in
   `ParserParityParser.mlfp` with one self-recursive pair.
3. Replace numbered instance method-row `MoreOrClose` and append continuations
   in `ParserParityParser.mlfp` with one self-recursive pair.
4. Replace the temporary `eq`/`neq` method-name parser with ordinary method
   identifiers, then extend `recursiveMethodRowsSourceText` beyond the old
   four-row ceiling for both class signatures and instance definitions.
5. Update `ProgramParserParitySpec.hs` static guard phrases to require the
   recursive helpers and reject the retired numbered continuations.
6. Run the focused verification commands and record direct evidence in
   `implementation-notes.md`.

### Verification
Focused checks:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences class and instance method rows"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser guards recursive class and instance method row substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

Full closeout gates are not selected because this round changes only
parser-owned milestone-4 test parser-library substrate, does not touch
production Haskell semantics, and does not close milestone 4 or make
compiler-package, platform/proof, native/backend, package-manager/linker,
self-boot, or full-parser-parity claims.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
