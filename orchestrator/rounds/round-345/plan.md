### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-345-nested-parenthesized-application-parser-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Build one reusable shared parser-library substrate for nested parenthesized
application argument depth handling, then use it for the existing
depth-indexed parenthesized application argument parser paths.

The round should reduce the copied nested parenthesized application
continuation plumbing in `ParserParityParser.mlfp` while preserving the
existing parser-parity behavior proved through the aggregate parser Hspec
group. This is milestone-4 parser/compiler-frontend ergonomics substrate only.
It must not claim full parser parity, compiler-package implementation,
platform/proof progress, native/backend completion, package-manager/linker
work, or self-boot completion.

### Approach
Use the rev-007 roadmap adjustment: milestone 4 remains in progress, and lawful
work may target reusable parser ergonomics substrate instead of another
fixture-shaped syntax slice. Keep `orchestrator/state.json` untouched and use
the controller/user-assigned rev-007 planning context recorded above.

Rounds 341-344 already centralized diagnostic expectations, bounded projection
row lists, bounded case-branch rows, and bounded general/simple application
arguments. Build on round 344's bounded application helper without reopening
the general or simple application migration.

Target the remaining nested parenthesized application-depth family in
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:

- `parseApplicationOrNestedParenthesizedArgumentExpression4` through
  `parseApplicationOrNestedParenthesizedArgumentExpression0`;
- `parseNestedParenthesizedApplicationArgumentOrDone4` through
  `parseNestedParenthesizedApplicationArgumentOrDone0`;
- the matching `parseParenthesizedNestedApplicationArgument*` body wrappers;
  and
- depth-specific append/second-argument continuations such as
  `appendParenthesizedApplicationArgument*`,
  `parseParenthesizedApplicationSimpleSecondOrDone*`, and
  `parseParenthesizedApplicationSecondArgumentOrSimpleDone*`.

Introduce the smallest owner-local helper family that expresses the repeated
shape: parse a simple function atom, optionally consume a depth-bounded
parenthesized application argument, optionally consume a simple argument, and
otherwise return the accumulated application. Keep it in
`ParserParityParser.mlfp`; do not move source-language application policy into
`ParserParityParserCombinator.mlfp` or expose a production parser API.

Prefer a first-order selector or focused helper entry points when they keep
depth boundaries explicit. Preserve the existing nested parenthesis budget,
parenthesized-expression close handling, simple-atom fallback behavior,
left-associative text construction through `finishApplicationExpression`, and
the existing final depth-0 two-simple-argument path. Do not redesign expression
precedence, lambda/case parsing, application budgets outside this nested
parenthesized path, source-span rendering, or diagnostic payloads in this
round.

Do not add compatibility aliases, fixture-name shortcuts, pre-rendered
projections, static negative evidence, canonical-parser bypasses, retired
syntax shims, parser-private shortcuts, or compiler-seed/package/platform/proof
hooks.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task content changes a shared parser-library grammar
  path across nested parenthesized depth variants and needs implementation
  judgment to preserve recursion boundaries, depth budgets, fallback behavior,
  and left-associative accumulation. Focused verification is sufficient because
  the work is confined to the shared parser-parity library/spec/docs owner
  surface, preserves existing behavior, does not touch production parser,
  checker, resolver, backend, package, platform, proof, or native code, and
  makes no milestone closeout or self-boot claim.

### Steps
1. Inspect the existing nested parenthesized application argument family in
   `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
   and confirm the behavior that must be preserved: nested parenthesis depth,
   simple-atom fallback, final depth-0 two-simple-argument handling, close
   diagnostics, stop-on-no-next-argument behavior, and left-associative
   accumulation.
2. Add one owner-local helper family in `ParserParityParser.mlfp` for nested
   parenthesized application argument depth handling. Keep the helper bounded to
   the parser-library source-expression path and reuse round 344's
   `parseBoundedSingleApplicationArgument` or `parseBoundedTwoApplicationArguments`
   only where that preserves existing behavior directly.
3. Migrate the depth-4 through depth-1 nested parenthesized argument paths onto
   the helper or focused helper entry points while preserving each depth's body
   parser boundary and close-parenthesis handling.
4. Migrate the depth-0 path while preserving the existing
   `parseParenthesizedTwoSimpleApplicationArgument` behavior and the final
   simple-argument fallback.
5. Remove migrated duplicated depth-specific append and second-argument helper
   definitions instead of leaving compatibility aliases.
6. Add focused static coverage in `test/ProgramParserParitySpec.hs` proving the
   nested parenthesized application-depth substrate exists, depth call sites use
   it, and the migrated helper names were not reintroduced as compatibility
   aliases.
7. Run the focused parser-parity gate and static shortcut/overclaim guards.
   Update repo-facing notes only if they record bounded substrate evidence, and
   keep all non-claims explicit.

### Verification
- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
- A focused static guard over
  `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`,
  `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`,
  and `test/ProgramParserParitySpec.hs` showing the nested parenthesized
  application-depth helper is present, depth-indexed call sites use it, and
  migrated helper aliases were not reintroduced.
- A shortcut/overclaim guard over changed parser-library, spec, and docs lines
  showing no fixture-name shortcuts, pre-rendered projections, static negative
  evidence, canonical-parser bypasses, compiler-seed/package/platform/proof
  hooks, native/backend claims, package-manager/linker claims, self-boot
  claims, or full parser parity claims were added.

The aggregate parser-parity Hspec group is the focused owner-surface gate for
this substrate slice because it exercises parenthesized and nested application
parsing through existing higher-order partial applications, recursive
ADT/typeclass cases, package source layouts, complex recursive programs,
compiler-seed data-model evidence, and compiler-seed lexer positive/negative
parser evidence. Full closeout gates, `cabal build all && cabal test`, and
`./scripts/thesis-conformance-gate.sh` are not required unless the
implementation widens beyond parser-library/spec/docs scope or makes
production parser, thesis-facing semantic, milestone closeout, package,
platform, proof, native/backend, or self-boot claims.

### Scheduler
- Depends on round ids: round-340, round-341, round-342, round-343, round-344
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
