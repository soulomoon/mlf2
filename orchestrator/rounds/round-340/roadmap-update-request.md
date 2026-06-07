### Roadmap Update Required
- Round id: round-340
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Roadmap revision: rev-006
- Roadmap dir: orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006
- Reason: The active roadmap still frames the next bottleneck as more core
  canonical syntax expressibility under milestone 4. Current seed and
  parser-parity evidence show a more precise boundary: `.mlfp` syntax is
  sufficient for a correctness seed, but not yet ergonomic enough for a full
  self-host compiler source path. The next implementation round needs an
  active roadmap direction for compiler-seed/parser-library ergonomics and
  library substrate, not another round selected as if core syntax
  expressibility is the blocker.

### Current Evidence
- Docs/ADRs/context/code inspected: `orchestrator/role-contract.md`,
  `orchestrator/roles/planner.md`, `orchestrator/artifact-manifest.md`,
  `orchestrator/active-roadmap-bundle.md`,
  `orchestrator/state-schema.md`, `orchestrator/project-contract.md`,
  `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006/roadmap.md`,
  `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006/verification.md`,
  `orchestrator/rounds/round-339/plan.md`,
  `orchestrator/rounds/round-339/implementation-notes.md`,
  `orchestrator/rounds/round-339/review.md`,
  `README.md`, `docs/mlfp-self-boot-readiness.md`,
  `docs/adr/2026-05-18-full-self-boot-end-to-end-roadmap.md`,
  `test/programs/compiler-seed/frontend-contract/`, and
  `test/programs/compiler-parser-parity/parser-library/`.
- Codebase or test boundaries inspected: The compiler seed frontend contract
  contains ordinary package-mode `.mlfp` modules for source positions/spans,
  tokens, diagnostics, AST, lexer, parser, and the root evidence program.
  `ghcup run --ghc 9.14.1 -- cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract`
  passed with `OK`.
  `ghcup run --ghc 9.14.1 -- cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract`
  passed and emitted
  `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
  plus
  `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`.
  Round 339 also approved bounded canonical parser/shared parser-library
  parity for `SeedLexer.mlfp`; its review records direct projection equality,
  source-copy equality, shortcut guards, and focused parser-parity Hspec
  evidence, while explicitly avoiding full parser parity, compiler-package,
  platform, proof, or self-boot claims.
- Why current milestone/direction is too coarse: Rev-006 milestone 4 treats
  "full canonical `.mlfp` parser parity" as the active dependency before
  platform and compiler-package work, and direction 4a still points planners
  toward the next smallest syntax family. That framing was correct for the
  previous parser-parity campaign, but it no longer captures the current
  blocker. Round 339 did not show that `SeedLexer.mlfp` cannot be parsed or
  checked by the Haskell canonical parser; it showed that the shared `.mlfp`
  parser-library path becomes painful around complex expression structures,
  long application chains, nested case/lambda shapes, source-span rendering,
  token streams, and diagnostics. Continuing to select rounds as core syntax
  expressibility work risks accumulating fixture-sized parser parity slices
  instead of improving the substrate needed to make a real compiler frontend
  package pleasant and maintainable in `.mlfp`.

### Requested Split
Request a semantic roadmap revision that makes the next active direction the
ergonomics/library substrate for the compiler-seed and full parser path.

The revision should preserve the completed parser-parity evidence as truthful
bounded evidence, not erase or overclaim it. It should explicitly separate:

- syntax enough for a correctness seed: yes, supported by
  `compiler-seed/frontend-contract` check/run evidence and the bounded
  parser-parity rounds through round 339;
- syntax pleasant enough for a full self-host compiler: not yet, because the
  practical blocker is missing ergonomic/library substrate.

The update-roadmap stage should reframe the active milestone/direction so the
next selected implementation work may target parser-combinator helpers,
stronger string/char/stream/list APIs, reduced case/lambda boilerplate, and
better source-span/diagnostic helpers for the compiler-seed/full parser path.
This should stay strategic: name the substrate direction and dependency
relationship, but leave detailed implementation choices to later round plans.

Preserve dependency order where it still matters. Platform, proof, and broad
compiler-package work should not start merely because the syntax seed is
checkable/runnable. The revision should instead define the lawful bridge
between bounded parser-parity evidence and later compiler-package/platform
work, so the controller can select the next substrate round without pretending
core syntax expressibility remains the immediate blocker.

### Non-Goals
Do not claim full canonical parser parity is complete.

Do not claim a compiler-package implementation exists beyond the current
bounded frontend seed fixtures.

Do not start or authorize platform substrate, driver, proof, native/backend,
package-manager, linker, or self-boot completion work.

Do not add broad compatibility aliases, retired syntax shims, fixture-name
shortcuts, canonical-parser bypasses, pre-rendered parser outputs, or other
shortcuts that would weaken thesis-faithful evidence.

Do not turn this request into a detailed implementation plan; the next roadmap
revision should adjust coordination semantics and leave concrete extraction to
future planner rounds.
