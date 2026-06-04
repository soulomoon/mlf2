### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-331-recursive-adt-typeclass-integration-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend the shared parser-owned `.mlfp` parser parity library with one bounded
recursive ADT/typeclass integration slice based on
`test/programs/recursive-adt/typeclass-integration.mlfp`.

This round should prove canonical parser parity for a single-module source that
combines an `Eq` class, recursive `Nat` declaration, explicit `Eq Nat`
instance method with nested case expressions, a `same` wrapper definition, and
a nested constructor-application `main`.

### Approach
Use the existing shared parser-library path instead of adding fixture-specific
shortcuts. Add a new canonical parser-parity fixture under
`test/conformance/mlfp/parser-parity/typeclass-integration/`, add the matching
thin `.mlfp` package root under
`test/programs/compiler-parser-parity/typeclass-integration/`, and register the
case in the generated aggregate parser-parity public CLI driver.

Extend `test/programs/compiler-parser-parity/parser-library/` only as far as
this selected syntax family needs:

- parse the `Eq, Nat(..), eq, same, main` export surface through the shared
  projection list path;
- parse an `Eq` class declaration and recursive `Nat` data declaration through
  shared declaration-row helpers where possible;
- parse the explicit `instance Eq Nat` body with the `eq` method definition;
- render the method body expression with nested `case left` / `case right`
  expressions and recursive `eq leftRest rightRest` calls;
- parse `same : Nat -> Nat -> Bool` and the selected nested
  `same (Succ (Succ Zero)) (Succ (Succ Zero))` `main` expression;
- add one malformed nested-case or instance-method negative case through
  `renderParserNegativeEvidenceFromSourceText`.

Keep the parser result dynamic: no whole-fixture source recognizer, no
pre-rendered `typeclass-integration` projection rows, no static negative
evidence, and no bypass around the canonical parser projection.

Update repo-facing notes only with bounded parser-parity language. The docs
must not claim full parser parity, checker/resolver/backend progress,
compiler-package progress, platform work, proof work, or self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The selected task is bounded to one parser syntax family, but it is
  not mechanically local: it changes the shared parser-library grammar,
  aggregate Hspec driver wiring, fixtures, negative evidence, and guard audits.
  Focused verification is sufficient because this is a non-closeout
  parser-parity slice with no production Haskell behavior claim and no
  milestone completion claim.

### Steps
1. Add
   `test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp`
   from the recursive ADT typeclass integration source and commit the canonical
   parser projection at
   `test/conformance/mlfp/parser-parity/typeclass-integration/expected/parser-program.txt`.
2. Add the thin parser-owned package root under
   `test/programs/compiler-parser-parity/typeclass-integration/` that exposes
   `sourceFile` and `sourceText`, then calls
   `renderParserParityProjectionFromSourceText`.
3. Extend `ProgramParserParitySpec` with source/expected/root constants,
   positive batch registration, direct shared-parser assertion for the new
   fixture, and one negative batch case for malformed nested case or malformed
   instance-method syntax.
4. Extend `ParserParityParser.mlfp` and related parser-library modules so the
   new fixture is parsed through shared token, parser-state, projection-row,
   diagnostic, and dynamic negative-evidence paths.
5. Extend parser shortcut/static guards in `ProgramParserParitySpec` so
   fixture-name, whole-source, pre-rendered-row, and static-negative shortcuts
   for this slice are rejected.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded parser-parity evidence and
   explicit non-claims.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

The focused Hspec group is the aggregate parser/conformance run for this owner
surface. It compiles the test suite, runs the canonical projection checks, runs
the generated public CLI parser batch, covers the selected negative evidence,
and exercises the shared shortcut/static guards.

Do not run the full closeout gates for this round unless the implementation
widens beyond the selected parser slice. This plan does not authorize milestone
closeout, production parser replacement, checker policy changes, or proof
claims. Run `./scripts/thesis-conformance-gate.sh` only if the implementation
edits thesis obligation ledgers or makes a thesis/readiness claim beyond the
bounded parser-parity notes above; otherwise record the explicit deferral
reason in implementation evidence.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
