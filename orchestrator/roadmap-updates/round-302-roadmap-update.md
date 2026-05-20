### Source Round
- Round id: `round-302`
- Trigger: planner-request
- Merged commit: `none`
- Evidence:
  - `orchestrator/rounds/round-302/roadmap-update-request.md` showed that rev-003 allowed milestone-3 broad text work but did not define a bounded whole-library extraction for the remaining Broad String Library.
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/roadmap.md` listed milestone 3 as in progress and recorded rounds 265-301 as one-function tracers under `direction-3a-broad-string-char-substrate`.
  - `docs/adr/2026-05-18-native-broad-string-library-before-parser-parity.md` requires a Prelude-level Broad String Library with Unicode scalar `Char`, `String`/`List Char` conversion, plain search, explicit formatting, Unicode-default text operations, and full native slicing/classification before parser parity.
  - `CONTEXT.md`, `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `src/MLF/Frontend/Program/Prelude.hs`, `src/MLF/Primitive/Inventory.hs`, `test/BackendLLVMSpec.hs`, and `test/PrimitiveInventorySpec.hs` show that the current surface already covers the rounds 265-301 inventory, while remaining broad-library categories needed exact inclusion, deferral, and closeout rules before a whole-library implementation round could be selected.

### Roadmap Change
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Prior revision: `rev-003`
- Proposed revision: `rev-004`
- Files changed:
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004/roadmap.md`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004/roadmap-view.json`
  - `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004/verification.md`

### Rationale

Rev-004 preserves the roadmap family, milestone order, dependency order, and
serial controller posture. Milestone 3 still precedes full parser parity and
platform work.

The semantic change is limited to milestone 3 coordination. Rev-004 replaces
the implicit one-function tracer extraction pattern with one exact next item:
`item-302-broad-string-library-completion`. That item is a coherent
whole-library completion round for the initial native-capable Broad String
Library, not another per-function tracer.

The new milestone-3 contract carries forward the completed rounds 265-301
inventory and defines the remaining public API/behavior matrix:

- included now: `stringJoin`, `stringSplitChar`, `stringCompare`, explicit
  slicing/cursor boundary behavior, `String`/`List Char` round-trip evidence,
  parser-needed ASCII classification helpers, explicit ASCII char/string case
  helpers, grouped search/split/replace edge behavior, and exact native
  metadata for string-producing helpers;
- intentionally deferred or excluded: locale-sensitive APIs, regex, parser
  combinators, full parser parity, platform contracts, compiler package work,
  driver work, proof work, Unicode normalization, Unicode collation, Unicode
  default case mapping, generic `List` library work, parser-owned source
  cursor/combinator modules, maps/sets, filesystem/process IO, package locks,
  ABI/linker contracts, and proof records.

Rev-004 also updates verification meaning for milestone 3: the next planner
can select one whole-library implementation item with grouped public behavior
tests, and the reviewer has concrete closeout criteria for deciding whether
milestone 3 is complete enough to unlock milestone 4 without inventing new
Prelude-level string scope.

### State Activation
- Requires state.json roadmap metadata update: yes, after reviewer approval and merge, set `roadmap_revision` to `rev-004` and `roadmap_dir` to `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`.
- New roadmap_dir when applicable: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`
