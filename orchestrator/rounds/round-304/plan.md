### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-304-parser-parity-basic-module-def-bool-spans`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Implement the first bounded source-text parser parity tracer. Parser-owned
`.mlfp` modules must parse one canonical source fixture:

```text
module Main export (main) {
  def main : Bool = true;
}
```

The `.mlfp` parser package must emit the same committed parsed-program syntax
projection and source-span evidence as the current canonical Haskell parser for
that fixture. This round proves the source cursor, tokenization, minimal
program AST, and renderer path for one grammar family only; it does not claim
full parser parity.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Work in
vertical RED -> GREEN -> refactor cycles, starting from one public-interface
behavior test before coding.

First public behavior and focused failing test:

```text
MLF.Program parser parity / parser-owned .mlfp parser matches canonical parser for a basic Bool definition and source spans
```

Focused command:

```bash
cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for a basic Bool definition and source spans/"'
```

The test should exercise the public package path by running the parser-parity
`.mlfp` package through `runProgramArgs`, not by calling implementation
internals. It should also render the same selected fixture through
`parseLocatedProgramWithFile` and compare both outputs to a committed expected
parser-parity artifact under `test/conformance/mlfp/parser-parity/`.

Keep the artifact projection deliberately small and stable for this first
slice: module name/export list, one `def` declaration name, its parsed type,
the `true` literal expression, and source spans for the module, exported
`main`, and `main` definition. Put any Haskell-side projection helper in the
test module or test support only; do not widen production facades for this
round.

### Steps
1. Add `ProgramParserParitySpec` with the focused RED test named above, wire it
   into `test/Main.hs`, and register it in the `mlf2-test` stanza of
   `mlf2.cabal`.
2. Add the selected parser-parity conformance fixture and committed expected
   artifact, for example under
   `test/conformance/mlfp/parser-parity/basic-module-def-bool/`.
3. Add a new ordinary `.mlfp` parser-parity package, separate from the older
   symbolic compiler seed, for example under
   `test/programs/compiler-parser-parity/basic-module-def-bool/`.
4. In that package, implement parser-owned modules for the selected slice:
   source cursor over `String`, token/result/diagnostic data, minimal parsed
   syntax data, tokenization for `module`, `export`, `def`, `Bool`, `true`,
   punctuation, and lower/upper identifiers, plus a parser for the one-module
   value-definition grammar family.
5. Make `Main.main` render the selected parsed syntax projection and source
   spans exactly as the committed expected artifact.
6. Refactor only after the focused test is green. Keep parser combinators
   parser-owned; do not promote them to Prelude or add checker/backend/driver
   behavior.
7. Update relevant readiness/progress docs to record only this bounded parser
   parity tracer and explicitly avoid claiming full parser parity,
   checker-in-`.mlfp`, backend/native support for arbitrary compiler
   workloads, driver behavior, platform contracts, or self-boot.

### Verification
- RED evidence: the focused `ProgramParserParitySpec` matcher above fails
  before parser-parity package implementation.
- Focused GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser matches canonical parser for a basic Bool definition and source spans/"'`
- Neighbor parser-parity slice:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smoke:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool`
- Diff hygiene: `git diff --check`
- Full behavior-changing closeout: `cabal build all && cabal test`
- Thesis/language-semantics gate:
  `./scripts/thesis-conformance-gate.sh`
- Manual closeout checks: confirm the implementation changed no checker,
  backend, driver, platform, package-manager, ABI, proof, or self-hosting
  scope; confirm any new Haskell spec module is registered in both
  `mlf2.cabal` and `test/Main.hs`; confirm docs do not overclaim beyond
  `item-304-parser-parity-basic-module-def-bool-spans`.

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-304/selection-record.json`
- `orchestrator/rounds/round-304/round-plan-record.json`
