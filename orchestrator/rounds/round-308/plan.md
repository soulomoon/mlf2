### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-308-parser-parity-typed-annotation-types`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Implement the next bounded source-text parser parity tracer. Parser-owned
`.mlfp` modules must parse one canonical module that carries the already-proven
module/import/definition/expression baseline and expands only typed source
annotation syntax:

```mlfp
module Main export (main) {
  import Prelude exposing (Int);
  def main : Int = let id : ∀a. a -> a = λ(x : Int) x in (id 1 : Int);
}
```

The `.mlfp` parser package must emit the same committed parsed-program syntax
projection and carried source-span evidence as the current Haskell canonical
parser for that fixture. This round proves typed let annotations, annotated
lambda parameter syntax, expression annotations, and arrow/forall source type
rendering. It does not claim checker, backend, package-manager, driver,
platform, proof, parser-combinator library, or full parser parity scope.

### Approach
Use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Keep the cycle vertical: add one public
parser-parity behavior test first, confirm RED, implement only enough
parser-owned `.mlfp` package behavior and test projection support to pass,
then add the focused malformed-annotation negative evidence.

First public behavior and focused failing test before coding:

```text
MLF.Program parser parity / parser-owned .mlfp parser matches canonical parser for typed let, annotated lambda, and expression annotations
```

Focused RED command:

```sh
cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for typed let, annotated lambda, and expression annotations/"'
```

The test should exercise the public package path by running the parser-owned
`.mlfp` tracer through `runProgramArgs`, compare its output with the Haskell
canonical parser projection from `parseLocatedProgramWithFile`, and compare
both against the committed expected parser-program artifact under
`test/conformance/mlfp/parser-parity/typed-annotation-types/`.

### Steps
1. Add the focused RED matcher above to `ProgramParserParitySpec`, preserving
   the round-304 through round-307 parser-parity tests and malformed-input
   evidence.
2. Add the selected parser-parity conformance fixture and committed expected
   parser projection under
   `test/conformance/mlfp/parser-parity/typed-annotation-types/`.
3. Add a parser-parity `.mlfp` package under
   `test/programs/compiler-parser-parity/typed-annotation-types/`. Any shared
   helpers must stay parser-owned and test-package-owned; do not widen Prelude,
   public facades, or production parser APIs.
4. Extend only the test-owned Haskell projection renderer as needed for this
   selected syntax: `STVar`, `STArrow`, `STForall`, typed `ELet`, `ELamAnn`,
   and `EAnn`. Keep the renderer public-behavior oriented and fixture-scoped.
5. Implement parser-owned source/token/AST/parser modules for exactly this
   grammar family: carried module header/export, carried single import exposing
   `Int`, one `def main : Int`, typed `let id : ∀a. a -> a`, parenthesized
   lambda parameter annotation `λ(x : Int)`, expression annotation
   `(id 1 : Int)`, and carried application/integer/value-reference syntax.
6. Add malformed annotation negative evidence through the public `run-program`
   path, for example a missing type after `:` or malformed forall binder that
   renders a stable parser category/span projection without matching exact
   Megaparsec prose.
7. Refactor only after the focused typed-annotation behavior is green. If this
   slice exposes repeated type-rendering or expectation code, keep it inside
   test support or the parser-parity package unless a later roadmap item
   authorizes a broader parser helper.
8. Update relevant readiness/progress docs for only this bounded parser-only
   tracer. Carry forward the existing round-307 let/lambda/application fixture
   when touching `test/conformance/mlfp/README.md`; do not claim full parser
   parity, parser-combinator completion, checker/backend/driver/platform/proof
   work, or self-boot readiness.

### Verification
- RED evidence: the focused `ProgramParserParitySpec` matcher above fails
  before adding the typed-annotation parser-parity fixture/package.
- Focused positive:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for typed let, annotated lambda, and expression annotations/"'`
- Focused negative:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed annotation syntax through public run-program/"'`
- Neighbor parser-parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smoke for the new fixture:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types`
- Regression package smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application`
- Diff hygiene:
  `git diff --check`
- Full behavior-changing closeout:
  `cabal build all && cabal test`
- Thesis/parser semantics gate:
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also wrote `selection-record.json` and `round-plan-record.json` beside this
plan. The selected lineage is
`item-308-parser-parity-typed-annotation-types`.
