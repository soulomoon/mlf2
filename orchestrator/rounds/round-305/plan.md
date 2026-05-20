### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-305-parser-parity-import-exposing-spans`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Implement the next bounded source-text parser parity tracer. Parser-owned
`.mlfp` modules must parse one canonical module with a single import exposing
list before the existing Bool definition:

```text
module Main export (main) {
  import Prelude exposing (Bool);
  def main : Bool = true;
}
```

The `.mlfp` parser package must emit the same committed parsed-program syntax
projection and source-span evidence as the current canonical Haskell parser for
that fixture. This round expands only the import-declaration grammar family:
module header/export, the existing Bool value definition, and round-304
lexer/parser mismatch evidence remain carried baseline behavior.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Work in
vertical RED -> GREEN -> refactor cycles, starting from one public-interface
behavior test before coding.

First public behavior and focused failing test:

```text
MLF.Program parser parity / parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans
```

Focused command:

```bash
cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans/"'
```

The test should exercise the public package path through `runProgramArgs` and
compare against the Haskell canonical parser projection from
`parseLocatedProgramWithFile`. Keep projection expansion deliberately small and
stable: module name/export, one import module span, the import exposing item
span for `Bool`, and the existing `main : Bool = true` definition span.

Add import-specific negative evidence through the parser-owned `.mlfp` package,
for example a misspelled `import` keyword or missing import semicolon that
renders the failing source span. The diagnostic evidence should be committed
or asserted by the Haskell spec, but it must not become checker, resolver,
package-manager, backend, driver, platform, compiler-package, or proof scope.

### Steps
1. Add the focused RED test named above to `ProgramParserParitySpec`, preserving
   the round-304 basic Bool positive and retry mismatch tests.
2. Add the selected parser-parity conformance fixture and committed expected
   parser projection under
   `test/conformance/mlfp/parser-parity/import-exposing-def-bool/`.
3. Add or extend a parser-parity `.mlfp` package under
   `test/programs/compiler-parser-parity/` for this fixture. Any shared helpers
   must stay parser-owned and test-package-owned; do not widen production
   facades or Prelude with parser helpers.
4. Extend parser-owned source/token/AST/parser modules for exactly this grammar
   family: one `import` declaration, one upper-case imported module name, the
   `exposing` keyword, a parenthesized type exposing item, the import
   semicolon, and the carried Bool definition grammar.
5. Extend the Haskell-side projection helper only as much as needed to render
   import module and import exposing-item spans from the canonical parser. Keep
   helper code in the spec or test support.
6. Add import-specific negative evidence through the public `run-program` path.
   The evidence must prove the parser-owned `.mlfp` tokenizer/parser no longer
   accepts malformed import syntax as the positive fixture.
7. Refactor only after the focused import test is green. If the import grammar
   forces reusable cursor/expectation code, keep it parser-owned under the
   parser-parity package and do not promote it to a general Prelude API.
8. Update relevant readiness/progress docs to record only this bounded import
   parser-parity tracer and explicitly avoid claiming full parser parity,
   parser combinator completion, checker/backend/driver/platform/proof work,
   compiler-in-`.mlfp`, or self-boot.

### Verification
- RED evidence: the focused `ProgramParserParitySpec` matcher above fails
  before import parser-parity implementation.
- Focused GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for a single import declaration and source spans/"'`
- Neighbor parser-parity slice:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smoke for the new import fixture, for example:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool`
- Round-304 regression smoke:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
- Diff hygiene: `git diff --check`
- Full behavior-changing closeout: `cabal build all && cabal test`
- Thesis/language-semantics gate:
  `./scripts/thesis-conformance-gate.sh`
- Manual closeout checks: confirm the implementation changed no checker,
  backend, driver, platform, package-manager, ABI, proof, or self-hosting
  scope; confirm any new Haskell spec/support module is registered in both
  `mlf2.cabal` and `test/Main.hs`; confirm docs do not overclaim beyond
  `item-305-parser-parity-import-exposing-spans`.

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-305/selection-record.json`
- `orchestrator/rounds/round-305/round-plan-record.json`
