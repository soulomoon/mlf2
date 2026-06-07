### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-341-parser-diagnostic-combinator-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build a reusable parser-combinator diagnostic expectation substrate for the
shared `.mlfp` parser-library path.

The round should reduce the copied current-token diagnostic and relabeling
plumbing in `ParserParityParserCombinator.mlfp` while preserving the existing
parser-parity behavior proved through the aggregate parser Hspec group. This
is milestone-4 parser/compiler-frontend ergonomics substrate only. It must not
claim full parser parity, compiler-package implementation, platform/proof
progress, native/backend completion, package-manager/linker work, or
self-boot completion.

### Approach
Use the rev-007 roadmap adjustment approved by round 340: milestone 4 remains
in progress, and the next lawful work should improve reusable ergonomics and
library substrate instead of adding another fixture-shaped syntax slice. The
assigned worktree's controller state snapshot still names rev-006, so keep
`orchestrator/state.json` untouched and treat this plan's selected extraction
as the controller/user-assigned rev-007 planning context.

Introduce one owner-local expectation representation in
`test/programs/compiler-parser-parity/parser-library/`, such as a
`ParserExpectation` value, with helpers that:

- construct the corresponding `ParserDiagnostic` from a source span;
- produce the diagnostic at the current token span with the existing fallback;
- relabel only retryable unexpected-source failures while preserving the
  parser-library's existing non-backtracking behavior for committed expected
  diagnostics; and
- keep source-span rendering and diagnostic payloads in the existing
  parser-library owner surface.

Migrate the parser-library call sites that currently depend on repeated
`label...` and `parserFailExpected...AtCurrent` helpers to the generic
expectation helper. Remove duplicated relabeling definitions and exports once
their call sites have moved; do not add compatibility aliases, fixture-name
shortcuts, pre-rendered projections, canonical-parser bypasses, or
parser-private shortcuts that hide missing reusable substrate.

Keep the implementation bounded to the shared parser-library combinator and
diagnostic surface plus focused regression guards in `ProgramParserParitySpec`.
Update repo-facing notes only if they help future planners understand the
bounded substrate evidence, and keep all non-claims explicit.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a new shared parser-library
  diagnostic expectation abstraction and migrates existing parser-library call
  sites across that abstraction, so it needs implementer/reviewer design
  judgment and is not simple. Focused verification is sufficient because the
  work is confined to the parser-parity test-program library/spec/docs owner
  surface, preserves existing behavior, does not touch production parser,
  checker, resolver, backend, package, platform, or proof code, and makes no
  milestone closeout or self-boot claim.

### Steps
1. Inspect the existing diagnostic helper duplication in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
   including `parserFailExpected...AtCurrent` helpers and `label...`
   relabeling functions.
2. Add a reusable expectation representation and constructors in the
   parser-library owner surface, preferably in
   `ParserParityParserCombinator.mlfp` unless a small owner-local diagnostic
   helper in `ParserParityDiagnostic.mlfp` keeps the boundary cleaner.
3. Add generic helpers for current-token expected diagnostics and relabeling
   unexpected-source failures to a selected expectation. Preserve the existing
   behavior that committed expected diagnostics are not silently backtracked by
   `parserChoice`.
4. Update `ParserParityParser.mlfp` and any other parser-library call sites to
   use the generic expectation helper instead of the duplicated expectation
   functions. Remove unused duplicated helpers and exports rather than leaving
   compatibility aliases behind.
5. Extend `test/ProgramParserParitySpec.hs` with focused regression/static
   coverage proving the new substrate is used and that the round did not add
   fixture-name shortcuts, pre-rendered parser rows, static negative evidence,
   canonical-parser bypasses, or parser-private compiler-seed shortcuts.
6. Update `implementation_notes.md`, `CHANGELOG.md`, or
   `docs/mlfp-self-boot-readiness.md` only if the implementation needs a
   durable bounded-substrate note; any note must explicitly avoid full parser
   parity, compiler-package, platform, proof, native/backend, driver,
   package-manager, linker, and self-boot claims.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the generic expectation helper
  is used and the removed duplicated relabeling helpers were not reintroduced
  as compatibility aliases.
- A shortcut/overclaim guard over the changed parser-library, spec, and docs
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-package hooks, platform/proof
  hooks, native/backend claims, package-manager/linker claims, or self-boot
  claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises the shared parser-library through
the existing direct projections, package projections, public generated
`run-program` batch, negative diagnostics, and shortcut guards. Full closeout
gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-library/spec/docs scope or makes
production parser, thesis-facing semantic, milestone closeout, package,
platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
