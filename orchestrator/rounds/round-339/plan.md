### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-339-compiler-seed-lexer-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend bounded parser-owned `.mlfp` parser parity to the existing
compiler-seed frontend lexer module:
`test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp`.

This round should prove that the canonical parser and shared parser-parity
library agree on `SeedLexer.mlfp` as ordinary package source text. It must not
start milestone 6 compiler-package implementation, claim full parser parity,
or change lexer, checker, resolver, backend, package execution, platform, or
proof semantics.

### Approach
Build on round 338's compiler-seed data-model parser-parity slice. Add a new
parser-parity conformance fixture under
`test/conformance/mlfp/parser-parity/compiler-seed-lexer/` whose source file
is a byte-for-byte copy of:

- `test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp`

Commit the canonical parser projection for that source in
`expected/parser-program.txt`. Add a thin parser-owned program under
`test/programs/compiler-parser-parity/compiler-seed-lexer/` that exposes only
the source path/text and calls the shared parser library.

Extend `test/programs/compiler-parser-parity/parser-library/` only as needed
to parse the selected lexer module structurally:

- preserve the `SeedLexer` export surface, including result/evidence data
  types and the selected public lexer functions;
- parse imports from `SeedSource`, `SeedToken`, and `SeedDiagnostic` through
  the established exposed-type and exposed-value import rows;
- render lexer-result, evidence, and classifier data declarations from parsed
  source structure rather than fixture labels;
- parse the lexer state-machine definitions with nested lambda and case
  expressions over `SeedInput`, `SeedInputSymbol`, `SeedTokenStream`,
  `SourceSpan`, and `SourcePosition` constructors;
- parse the nested token-stream constructors and diagnostic constructor
  applications used in positive and negative evidence; and
- add one meaningful malformed lexer-state-machine negative case, preferably a
  missing case-branch arrow or malformed nested token-stream expression,
  through `renderParserNegativeEvidenceFromSourceText`.

Do not add fixture-name shortcuts, pre-rendered projection rows, static
negative evidence, token-stream shortcuts, canonical-parser bypasses,
compatibility aliases, package resolver behavior, or compiler-package
implementation. Update repo-facing notes only with bounded parser-parity
language. Respect `orchestrator/project-contract.md` for shared invariants and
do not claim full parser parity, checker/resolver/backend progress,
compiler-package progress, platform work, driver work, proof work, or
self-boot completion.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task is bounded to one compiler-seed source module, but
  the task content requires shared parser-library design judgment for a large
  lexer state-machine module with multiple data declarations, import/export
  rows, nested case classifiers, token-stream constructors, span-sensitive
  evidence, and dynamic negative parsing. Focused verification is sufficient
  because this is a non-closeout parser-parity slice with no production parser
  replacement, no lexer/checker/resolver/backend/package behavior claim, no
  platform/proof/compiler-package work, and no milestone completion claim.

### Steps
1. Copy `test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp` into
   `test/conformance/mlfp/parser-parity/compiler-seed-lexer/src/SeedLexer.mlfp`
   byte-for-byte.
2. Generate and commit
   `test/conformance/mlfp/parser-parity/compiler-seed-lexer/expected/parser-program.txt`
   from the current Haskell canonical parser projection for the selected
   source.
3. Add `test/programs/compiler-parser-parity/compiler-seed-lexer/` with a thin
   `ParserParityFixture.mlfp` and `Main.mlfp` that route the selected source
   text through the shared parser-library entrypoint.
4. Extend `ParserParityParser.mlfp` and related parser-library modules only
   for structural grammar needed by `SeedLexer.mlfp`. Reuse existing
   projection, import/export, case-expression, lambda, constructor
   application, and package-source rendering paths where they apply.
5. Extend `ProgramParserParitySpec` with source/expected/root constants, a
   direct shared-parser equality check, aggregate positive registration, one
   malformed selected-syntax negative case, a source-copy equality check, and
   shortcut/static guards covering `SeedLexer`, selected lexer definition
   names, projection text, static negative evidence, token-stream shortcuts,
   and canonical-parser bypasses.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded parser-parity evidence and
   explicit non-claims for full parser parity, lexer/checker/resolver/backend
   behavior, compiler-package implementation, platform work, driver work,
   proof work, and self-boot completion.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- Source-copy check proving
  `test/conformance/mlfp/parser-parity/compiler-seed-lexer/src/SeedLexer.mlfp`
  is byte-for-byte equal to
  `test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp`.
- Static guard check over
  `test/programs/compiler-parser-parity/parser-library/` and
  `test/ProgramParserParitySpec.hs` proving this slice did not add
  `SeedLexer` fixture shortcuts, pre-rendered parser-program rows, static
  negative evidence, token-stream shortcuts, canonical-parser bypasses, or
  compiler-package implementation hooks.
- Docs overclaim check over `implementation_notes.md`, `CHANGELOG.md`, and
  `docs/mlfp-self-boot-readiness.md`.

The aggregate parser-parity Hspec group is the focused parser/conformance gate
for this bounded slice. It exercises the selected shared parser-library path
through direct assertions and the public generated `run-program` batch. Full
closeout gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-parity fixture/library/docs scope or
claims milestone completion.

### Scheduler
- Depends on round ids: round-338
- Merge after item ids: item-338-compiler-seed-data-model-parser-parity
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
