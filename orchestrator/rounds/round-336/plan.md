### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-336-authoritative-unified-exact-source-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend the shared parser-owned `.mlfp` parser parity library with exact
authoritative unified source corpus variants whose grammar families are already
partly represented by parser-parity fixtures, but whose canonical corpus paths
are not yet covered as first-class parser-parity fixtures:

- `test/programs/unified/authoritative-case-analysis.mlfp`
- `test/programs/unified/authoritative-let-polymorphism.mlfp`
- `test/programs/unified/authoritative-nullary-overloaded-method.mlfp`
- `test/programs/unified/authoritative-overloaded-method.mlfp`

This round should prove canonical parser parity for those exact source files
without treating the older `case-expression-constructor-patterns`,
`let-lambda-application`, `typeclass-instance-nullary-method`, or
`typeclass-deriving-method` fixtures as sufficient evidence for the
authoritative unified corpus names.

### Approach
Use the existing shared parser-library path. Do not add fixture-specific
whole-source recognizers, pre-rendered projection rows, compatibility aliases,
static negative evidence, token-stream shortcuts, or canonical-parser bypasses.

Add parser-parity conformance fixtures under:

- `test/conformance/mlfp/parser-parity/authoritative-case-analysis/`
- `test/conformance/mlfp/parser-parity/authoritative-let-polymorphism/`
- `test/conformance/mlfp/parser-parity/authoritative-nullary-overloaded-method/`
- `test/conformance/mlfp/parser-parity/authoritative-overloaded-method/`

Add matching thin `.mlfp` package roots under
`test/programs/compiler-parser-parity/`. Each package root should expose only
`sourceFile` and `sourceText`, then call
`renderParserParityProjectionFromSourceText`.

Extend `test/programs/compiler-parser-parity/parser-library/` only as needed
to parse these exact source variants through shared grammar paths:

- reuse the established constructor-pattern case-expression parser path for
  `authoritative-case-analysis`;
- support the exact importless `authoritative-let-polymorphism` module through
  the shared let/lambda/application parser path, rendering no import rows;
- reuse the established class/instance parser path for
  `authoritative-nullary-overloaded-method`;
- reuse the established deriving `Eq` parser path for
  `authoritative-overloaded-method`; and
- render module, export, declaration, and definition rows from parsed source
  structure and source spans, not fixture keys or static projection names.

Add one dynamic negative case for the authoritative unified batch, preferably a
malformed importless let-polymorphism expression that runs through
`renderParserNegativeEvidenceFromSourceText`. If implementation discovers that
the importless let path is already covered mechanically, keep the negative case
on the exact authoritative source family instead of reusing an older fixture
path.

Update repo-facing notes only with bounded parser-parity language. Respect
`orchestrator/project-contract.md` for shared invariants and do not claim full
parser parity, checker/resolver/backend progress, compiler-package progress,
platform work, proof work, driver work, or self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected batch is bounded to exact authoritative unified corpus
  variants, but it is not only mechanical fixture registration. It requires
  preserving exact corpus paths across case, let/lambda, deriving, and
  instance surfaces, and the authoritative let-polymorphism source lacks the
  Prelude import carried by the older `let-lambda-application` parser fixture.
  That requires shared parser-library behavior rather than a pure copy of
  existing fixtures. Focused verification is sufficient because this is a
  non-closeout parser-parity slice with no production parser replacement, no
  checker/backend behavior claim, and no milestone completion claim.

### Steps
1. Copy the four exact unified corpus sources into the matching
   `test/conformance/mlfp/parser-parity/<fixture>/src/Main.mlfp` paths listed
   above.
2. Commit canonical parser projections for each source under
   `expected/parser-program.txt`; projections must preserve the exact source
   contents and source spans for each new parser-parity path.
3. Add thin parser-owned package roots under
   `test/programs/compiler-parser-parity/authoritative-case-analysis/`,
   `authoritative-let-polymorphism/`,
   `authoritative-nullary-overloaded-method/`, and
   `authoritative-overloaded-method/`.
4. Extend `ProgramParserParitySpec` with source/expected/root constants, direct
   shared-parser assertions for the four exact authoritative fixtures,
   aggregate positive batch registrations, one authoritative-unified dynamic
   negative assertion, and shortcut/static guard phrases for selected
   fixture-name, whole-source, pre-rendered row, exact-expression, and static
   negative shortcuts.
5. Extend or refactor `ParserParityParser.mlfp` and related parser-library
   modules so the four exact fixtures parse through shared token,
   parser-state, projection-row, diagnostic, and dynamic negative-evidence
   paths. Keep any support for the importless let-polymorphism variant generic
   enough that it is not keyed to the fixture name.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded evidence for the exact
   authoritative unified parser-parity fixtures and explicit non-claims for
   full parser parity, checker/resolver/backend, compiler-package, platform,
   driver, proof, and self-boot completion.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

The focused Hspec group is the aggregate parser/conformance run for this owner
surface. It compiles the test suite, checks canonical projections, runs the
generated public CLI parser batch, covers the selected dynamic negative
evidence, and exercises the shared shortcut/static guards.

Do not run full closeout gates for this round unless implementation widens
beyond the selected parser slice. This plan does not authorize milestone
closeout, production parser replacement, checker policy changes, platform
work, compiler-package work, driver work, or proof claims. Run
`./scripts/thesis-conformance-gate.sh` only if implementation edits thesis
obligation ledgers or makes a thesis/readiness claim beyond the bounded
parser-parity notes above; otherwise record the explicit deferral reason in
implementation evidence.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
