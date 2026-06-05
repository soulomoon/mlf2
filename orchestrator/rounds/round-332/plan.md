### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4a-canonical-parser-parity
- Extracted item id: item-332-abstract-recursive-adt-module-use-parser-parity
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006

### Goal
Extend the shared parser-owned `.mlfp` parser parity library with one bounded
multi-module recursive ADT slice based on
`test/programs/recursive-adt/abstract-module-use.mlfp`.

This round should prove canonical parser parity for a source package where
`Core` exports `Nat` abstractly with constructor-building and destructor
functions, and `User` imports that abstract surface to compute
`isZero (peel (succ zero))`.

### Approach
Use the existing shared parser-library path. Do not add a fixture-specific
whole-source recognizer, pre-rendered projection rows, compatibility alias, or
canonical-parser bypass.

Add a parser-parity conformance fixture under
`test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/`, add
the matching thin `.mlfp` package root under
`test/programs/compiler-parser-parity/abstract-recursive-adt-module-use/`, and
register the case in the aggregate public CLI parser-parity driver.

Extend `test/programs/compiler-parser-parity/parser-library/` only for the
selected syntax family:

- parse a two-module program with `Core` exporting an abstract `Nat` type plus
  `zero`, `succ`, `peel`, and `isZero`;
- parse the existing recursive `Nat` declaration and four following
  definitions in one shared module body path;
- parse annotated lambda definitions whose bodies are constructor application
  or case expressions;
- parse imported `User` modules whose `main` expression is a generic
  function-application expression over imported values, not a hard-coded
  fixture expression;
- keep import/export row rendering dynamic for the selected five-item abstract
  surface; and
- add one malformed import/body or destructor-case negative case through
  `renderParserNegativeEvidenceFromSourceText`.

Update repo-facing notes only with bounded parser-parity language. Respect
`orchestrator/project-contract.md` for shared invariants and do not claim full
parser parity, checker/resolver/backend progress, compiler-package progress,
platform work, proof work, or self-boot progress.

### Execution Profile
- Complexity: standard
- Verification profile: focused
- Reason: The task is bounded to one parser syntax family, but it is not
  mechanically local: it changes shared parser-library module-body parsing,
  imported-definition parsing, fixture registration, aggregate Hspec driver
  wiring, negative evidence, and shortcut/static audits. Focused verification
  is sufficient because this is a non-closeout parser-parity slice with no
  production parser replacement, no checker/backend behavior claim, and no
  milestone completion claim.

### Steps
1. Add
   `test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/src/Main.mlfp`
   from `test/programs/recursive-adt/abstract-module-use.mlfp` and commit the
   canonical parser projection at
   `test/conformance/mlfp/parser-parity/abstract-recursive-adt-module-use/expected/parser-program.txt`.
2. Add the thin parser-owned package root under
   `test/programs/compiler-parser-parity/abstract-recursive-adt-module-use/`
   that exposes `sourceFile` and `sourceText`, then calls
   `renderParserParityProjectionFromSourceText`.
3. Extend `ProgramParserParitySpec` with source/expected/root constants,
   positive batch registration, direct shared-parser assertion for the new
   fixture, and one negative batch case for malformed abstract import/body or
   destructor-case syntax.
4. Extend `ParserParityParser.mlfp` and related parser-library modules so the
   new fixture is parsed through shared token, parser-state, projection-row,
   diagnostic, and dynamic negative-evidence paths.
5. Extend parser shortcut/static guards in `ProgramParserParitySpec` so
   fixture-name, whole-source, pre-rendered-row, imported-main-expression, and
   static-negative shortcuts for this slice are rejected.
6. Update `implementation_notes.md`, `CHANGELOG.md`, and
   `docs/mlfp-self-boot-readiness.md` with bounded parser-parity evidence and
   explicit non-claims.

### Verification
Run:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

The focused Hspec group is the aggregate parser/conformance run for this owner
surface. It compiles the test suite, runs canonical projection checks, runs the
generated public CLI parser batch, covers the selected negative evidence, and
exercises the shared shortcut/static guards.

Do not run full closeout gates for this round unless implementation widens
beyond the selected parser slice. This plan does not authorize milestone
closeout, production parser replacement, checker policy changes, platform
work, compiler-package work, or proof claims. Run
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
