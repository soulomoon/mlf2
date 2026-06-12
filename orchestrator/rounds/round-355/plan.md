### Selected Extraction
- Milestone: Full Canonical .mlfp Parser Parity
- Milestone id: milestone-4-full-canonical-mlfp-parser-parity
- Direction id: direction-4b-compiler-seed-parser-ergonomics-substrate
- Extracted item id: item-355-recursive-class-instance-method-row-substrate
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-007
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007

### Goal
Supersede the stale parser-only round-355 plan with a bounded same-round
recovery slice: add the smallest presolution witness-normalization support
needed for generated `.mlfp` parser packages to accept recursive class and
instance method-row continuations, then finish the shared parser-library and
spec substrate for those method rows.

The failed parser-only implementation in
`orchestrator/rounds/round-355/implementation-notes.md` is blocker evidence,
not the current plan. Its smallest recorded reproducer is
`parseEqClassMethodRowsMoreOrClose3 : String -> String -> ParserValue -> Parser ParserValue`,
whose recursive arm appends a parsed method row and continues under the parser
abstraction, failing the focused dynamic gate with
`WitnessNormalizationError (OpUnderRigid ...)`. The prior parser-only helper
attempts are exhausted unless the round retreats to exact-count wrappers or
avoids the selected recursive method-row substrate.

This remains milestone-4 parser/compiler-frontend ergonomics substrate. It
must not claim full parser parity, compiler-package implementation,
platform/proof progress, native/backend completion, package-manager/linker
work, or self-boot completion.

### Approach
Widen round-355, but keep it bounded to the support surface directly exposed
by the failing parser-package check.

Primary owned production surface:

- `src/MLF/Constraint/Presolution/WitnessNorm.hs`: normalize edge witnesses
  against finalized live graph binders so dead rewritten binder copies from
  trace source entries do not widen no-replay interiors and leave
  `OpUnderRigid` residue.

Primary owned test/support surface:

- `test/Presolution/WitnessSpec.hs`: add a direct regression for no-replay
  witness normalization with dead rewritten binder copies.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  keep and finish the recursive class/instance method-row parser-library
  substrate from the failed attempt.
- `test/ProgramParserParitySpec.hs`: keep and finish the focused dynamic
  two-method class/instance parser-package check plus static helper, call-site,
  alias-removal, and shortcut/overclaim guards.
- `orchestrator/rounds/round-355/implementation-notes.md`: preserve the
  current failure notes and append the new implementation and verification
  evidence during implementation.

Optional owned runtime surface, only if the focused recursive method-row
parser-package gate advances past presolution and then fails on delayed
top-level recursion through lambda closures:

- `src/MLF/Frontend/Program/Run.hs`
- `test/ProgramSpec.hs`

Do not preemptively port unrelated parent-checkout frontend changes. Do not
touch `src/MLF/Frontend/Program/Elaborate.hs` unless the focused method-row
gate exposes a new exact type-compatibility blocker and the implementer records
why that blocker is part of this same reproducer. Do not edit the parent
checkout, `orchestrator/state.json`, active roadmap files, `CHANGELOG.md`, root
`implementation_notes.md`, package/platform/proof/native/backend surfaces, or
public parser APIs.

The parent checkout's uncommitted `WitnessNorm.hs` and `WitnessSpec.hs` diff is
evidence for the smallest likely presolution support shape only. Recreate the
needed change in this assigned worktree; do not overwrite or revert anyone
else's parent-checkout work.

### Execution Profile
- Complexity: standard
- Verification profile: standard
- Reason: The task goal and verification boundary are clear, but the selected
  content now changes shared production presolution witness normalization and
  parser-library behavior across checker and parser-package surfaces. That is
  structural implementation work with a new failure mode, so it is not
  planner-direct simple work. Standard verification is required because the
  support slice touches thesis-facing presolution witness behavior in addition
  to the focused parser-library reproducer.

### Steps
1. Confirm the worktree branch is
   `orchestrator/round-355-ergonomics-substrate` and leave parent checkout
   files untouched.
2. Preserve the current parser/spec candidate and the failure notes in
   `orchestrator/rounds/round-355/implementation-notes.md` as blocker
   evidence before editing.
3. In `WitnessNorm.hs`, filter rewritten binder arguments used for witness
   normalization to finalized live node keys before using them to widen
   `interiorWithBinders` or populate `OmegaNormalizeEnv.binderArgs`. Keep
   trace source entries available for replay-contract completeness; only the
   normalization interior/binder environment should drop dead rewritten binder
   copies.
4. Add a focused `Presolution.WitnessSpec` regression where an edge trace has
   two source binders rewritten to dead copies, no replay contract, and an
   interior containing only the root. The normalized witness should eliminate
   the dead-copy `OpWeaken` operations instead of failing or widening the
   no-replay interior.
5. Re-run the recorded focused method-row parser-package check. If it still
   fails with `WitnessNormalizationError (OpUnderRigid ...)`, inspect the new
   edge/node evidence and adjust only `WitnessNorm.hs` or the parser-library
   helper shape directly implicated by that same reproducer.
6. Once presolution passes, finish the parser-library method-row substrate:
   class declarations parse a first supported method signature row and then
   parse another method row or close on `}`; instance declarations do the same
   for method definition rows. Preserve `ValueProjectionRows`,
   `appendProjectionValues`, projection ordering, and source-span rendering.
7. Keep the two-method class/instance dynamic Hspec check comparing shared
   parser output to `renderCanonicalProjection`; keep static guards for helper
   surface, representative class/instance call sites, retired one-method alias
   absence, stale exact-count helper absence, and prohibited shortcuts or
   overclaims.
8. If the focused gate advances to a runtime-only delayed-recursion failure,
   make the smallest `Run.hs` closure-stack change needed for delayed top-level
   recursion through lambda closures and add the corresponding `ProgramSpec`
   regression. Do not make this runtime change if the focused gate does not
   require it.
9. Do not use fixture-name shortcuts, pre-rendered projections, canonical
   parser bypasses, retired syntax aliases, compatibility wrappers for removed
   method-row helpers, or parser-private hacks that hide missing reusable
   substrate.
10. Append implementation evidence and all command results to
    `orchestrator/rounds/round-355/implementation-notes.md`.

### Verification
Required focused checks:

- `git diff --check`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "normalization does not widen no-replay interiors with dead rewritten binder copies"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences class and instance method rows"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser guards recursive class and instance method row substrate"'`
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`

Required static guards:

- A helper/call-site/alias-removal guard over
  `ParserParityParser.mlfp` and `ProgramParserParitySpec.hs` proving the
  recursive method-row helper surface and representative class/instance call
  sites are present, retired one-method continuation aliases are absent, and
  the stale `parseDataLedSourceDefinitionSuffixRows` exact-count helper is not
  revived.
- A changed-line shortcut/overclaim guard for fixture-name shortcuts,
  pre-rendered projections, canonical-parser bypasses, static-negative-only
  evidence, retired syntax aliases, compiler-package/platform/proof hooks,
  native/backend claims, package-manager/linker claims, self-boot claims, and
  full parser parity claims.

Required standard gates after the focused checks pass:

- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
- `./scripts/thesis-conformance-gate.sh`

If the optional runtime surface is touched, also run:

- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "allows delayed top-level recursion through lambda closures"'`

Full closeout gates are not selected because this round does not close
milestone 4 and must not make compiler-package, platform/proof,
native/backend, package-manager/linker, self-boot, or full-parser-parity
claims. The standard gates are required because production presolution witness
normalization is thesis-facing shared checker behavior.

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
