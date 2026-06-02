### Source Round
- Round id: `round-324`
- Trigger: planner-request
- Merged commit: none
- Evidence: `orchestrator/rounds/round-324/roadmap-update-request.md` requested a semantic coordination update because future checker, checker-parity, compiler-source-package, driver, and conformance-validation planning needs a shared-context single-program-run rule. The request cites current parser-parity batching evidence in `test/ProgramParserParitySpec.hs`, checker-facing `checkProgramArgs`/`runProgramArgs` boundaries in `src/MLF/Program/CLI.hs`, timing and batching evidence in `implementation_notes.md` and `bench/parser-library-performance.md`, and the absence of a general checker-related batching rule in rev-004.

### Roadmap Change
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Prior revision: `rev-004`
- Proposed revision: `rev-005`
- Files changed:
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005/roadmap.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005/roadmap-view.json`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005/verification.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/roadmap-history.md`

### Rationale

Rev-004 already requires parser parity to grow one shared parser-owned library and route parser fixtures through shared entrypoints, but it does not encode a general planning/review rule for future checker-facing public program runs. Current repo evidence shows this distinction matters: parser-parity validation now uses a generated aggregate public CLI driver with labelled per-case sections, while performance notes and benchmark records show repeated checker setup/checking can dominate parser/checker-adjacent validation.

Rev-005 adds a cross-cutting coordination constraint without changing milestone order or implementation scope. Future checker, checker-parity, compiler-source-package, driver, and conformance-validation rounds should prefer one aggregate public program run with labelled per-case output/evidence whenever multiple cases can share checker or program setup context. Multiple invocations remain lawful when required for semantic isolation, diagnostic boundaries, failure independence, stage-owned output isolation, or the public interface under test.

The update keeps public-interface evidence strict: batching must not hide diagnostics, spans, labels, failure evidence, committed expected outputs, or stage-owned records. It also preserves the existing proof oracle, trusted-substrate boundary, milestone dependencies, and self-boot completion criteria.

### State Activation
- Requires state.json roadmap metadata update: yes
- New roadmap_dir when applicable: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`
