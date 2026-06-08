### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-344-bounded-application-argument-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for bounded application
argument accumulation, then use it for the existing general and simple
application parser paths.

The round should reduce the copied numbered application-argument continuation
ladders in `ParserParityParser.mlfp` while preserving the existing
parser-parity behavior proved through the aggregate parser Hspec group. This
is milestone-4 parser/compiler-frontend ergonomics substrate only. It must not
claim full parser parity, compiler-package implementation, platform/proof
progress, native/backend completion, package-manager/linker work, or
self-boot completion.

### Approach
Use the rev-007 roadmap adjustment approved after round 340: milestone 4
remains in progress, and lawful work may target reusable parser ergonomics
substrate instead of another fixture-shaped syntax slice. The assigned
worktree's checked-in `orchestrator/state.json` still names rev-006, so keep it
untouched and treat this plan's selected extraction as the controller/user
assigned rev-007 planning context.

Rounds 341-343 already centralized diagnostic expectations, bounded projection
row lists, and bounded case-branch rows. Build on those substrate slices
without reopening them.

Target the repeated application argument pattern now visible in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseApplicationArgumentOrDone`,
  `parseApplicationSecondArgumentOrDone`,
  `parseApplicationThirdArgumentOrDone`, and the numbered continuation family
  through `parseApplicationThirteenthArgumentOrDone`;
- `parseSimpleApplicationArgumentOrDone`,
  `parseTwoSimpleApplicationArgumentOrDone`,
  `parseSimpleApplicationSecondArgumentOrDone`, and
  `parseSimpleApplicationThirdArgumentOrDone`; and
- direct callers such as `parseApplicationOrAtomExpression`,
  `parseApplicationOrSimpleAtomExpression`, and
  `parseApplicationOrTwoSimpleAtomExpression`.

Introduce an owner-local bounded application-argument helper family in
`ParserParityParser.mlfp`. Preserve the current maximum argument budget, left
associative text construction through `finishApplicationExpression`, atom
parser choice boundaries, and the existing stop behavior when no next argument
is present. Prefer a small first-order selector or two focused helper entry
points for general-expression atoms versus simple-expression atoms; do not
make this a broad parser framework or move application semantics into
`ParserParityParserCombinator.mlfp` unless the implementation proves the
generic boundary stays parser-library-owned and does not leak source-language
policy into the combinator module.

Migrate only the general and simple application argument ladders in this
round. Keep the nested parenthesized application-argument depth helpers
(`parseApplicationOrNestedParenthesizedArgumentExpression*`,
`parseNestedParenthesizedApplicationArgumentOrDone*`, and matching
parenthesized append helpers) for a later slice unless they fall out
mechanically after the core bounded helper is stable and the diff stays
obviously within the same owner surface and failure mode.

Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, static negative evidence, canonical-parser bypasses, retired
syntax shims, parser-private shortcuts, or compiler-seed/package/platform/proof
hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content introduces a reusable bounded application
  parser substrate and migrates multiple existing parser-library continuation
  families across it, so it needs implementer/reviewer design judgment and is
  not simple. Focused verification is sufficient because the work is confined
  to the shared parser-parity library/spec/docs owner surface, preserves
  existing behavior, does not touch production parser, checker, resolver,
  backend, package, platform, proof, or native code, and makes no milestone
  closeout or self-boot claim.

### Steps
1. Inspect the existing general and simple application ladders in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the behavior that must be preserved: left-associative
   accumulation, argument budget, expression-atom versus simple-atom parser
   boundaries, and stop-on-no-next-argument behavior.
2. Add one owner-local bounded application-argument helper family in
   `ParserParityParser.mlfp`. Keep it reusable for both the general atom path
   and the simple atom path, but do not make it a production parser API or a
   broad parser framework.
3. Migrate `parseApplicationOrAtomExpression` and the
   `parseApplication*ArgumentOrDone` numbered ladder through
   `parseApplicationThirteenthArgumentOrDone` onto the helper. Remove migrated
   duplicated definitions instead of leaving compatibility aliases.
4. Migrate `parseApplicationOrSimpleAtomExpression`,
   `parseApplicationOrTwoSimpleAtomExpression`, and their
   `parseSimpleApplication*ArgumentOrDone` helpers onto the same substrate or
   the smallest helper entry point needed for simple-atom parsing. Preserve the
   one-argument and two-argument simple parser entrypoint behavior expected by
   existing parenthesized-expression callers.
5. Leave nested parenthesized application-depth helpers unchanged unless the
   migration is a direct call-site replacement to the new bounded helper with
   no semantic redesign. If they remain unchanged, document them as the next
   candidate substrate surface rather than stretching this round.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   bounded application-argument substrate exists, general and simple
   application call sites use it, and the migrated numbered application helper
   names were not reintroduced as compatibility aliases.
7. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the bounded application helper
  is present, general and simple application paths call it, and migrated
  numbered application helper aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises application parsing through existing
higher-order partial applications, recursive ADT/typeclass cases, package
source layouts, complex recursive programs, compiler-seed data-model evidence,
and compiler-seed lexer positive/negative parser evidence. Full closeout
gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-library/spec/docs scope or makes
production parser, thesis-facing semantic, milestone closeout, package,
platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
