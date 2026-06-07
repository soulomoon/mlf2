### Source Round
- Round id: round-340
- Trigger: planner-request
- Merged commit: none
- Evidence: `orchestrator/rounds/round-340/roadmap-update-request.md` records
  that rev-006 still directed milestone-4 planners toward more core canonical
  syntax expressibility even though current evidence shows a sharper boundary:
  syntax is sufficient for a correctness seed, but not yet ergonomic enough
  for a maintainable full self-host compiler source path. The request cites
  compiler-seed frontend-contract `check-program` / `run-program` evidence,
  bounded parser-parity evidence through round 339, and the explicit round-339
  non-claims for full parser parity, compiler-package, platform, proof, and
  self-boot completion.

### Roadmap Change
- Roadmap id: 2026-05-18-00-full-self-boot-end-to-end-roadmap
- Prior revision: rev-006
- Proposed revision: rev-007
- Files changed:
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007/roadmap.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007/verification.md`
  - `orchestrator/roadmap-updates/round-340-roadmap-update.md`

### Rationale

Rev-007 preserves the full-self-boot milestone order and keeps milestone 4
open. It updates milestone-4 semantics so the next lawful implementation round
can improve the reusable ergonomics/library substrate needed by the
compiler-seed/full-parser path: parser state/result/combinator helpers,
string/char/stream/list APIs, source-span/diagnostic helpers, or a narrowly
justified reduction of repeated case/lambda plumbing.

The revision keeps bounded parser-parity evidence truthful. Rounds 304-339,
including round-339 `SeedLexer.mlfp` equality, source-copy, shortcut-guard,
malformed-case negative, and aggregate parser Hspec evidence, are recorded as
bounded seed/parser-path evidence only. They do not complete full parser
parity and do not authorize compiler-package implementation, platform,
driver, native/backend, proof, package-manager, linker, or self-boot work.

The practical coordination change is to stop treating raw core syntax
expressibility as the immediate bottleneck. The roadmap now says the current
bridge work is reusable substrate quality: enough syntax exists for a
correctness seed, but the compiler source path still needs better library and
ergonomic support before full canonical parser parity can be closed honestly.

### State Activation
- Requires state.json roadmap metadata update: yes; after review approval and
  merge, set `roadmap_revision` to `rev-007` and `roadmap_dir` to
  `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007`.
- New roadmap_dir when applicable:
  `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-007`
