# Task Plan — Fig. 15.3.4 Raise Normalization/Emission Full Alignment

## Metadata

- **Task ID:** `2026-02-10-fig-15-3-4-raise-normalization-emission-full-alignment`
- **Created:** `2026-02-10`
- **Status:** `IN_PROGRESS`
- **Primary Plan:** `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-10-fig-15-3-4-raise-normalization-emission-full-alignment/docs/plans/2026-02-10-fig-15-3-4-raise-normalization-emission-full-alignment-implementation-plan.md`

## Objective

Execute the Fig. 15.3.4 witness alignment plan end-to-end with strict TDD, closing the full 15-row matrix contract across unit and presolution paths, and finish with matrix + full project verification gates.

## Phases

- [x] **Phase 1 — Matrix harness scaffolding (Task 1)**
  - Add row-id naming and complete 15-row test matrix stubs/assertions.
  - Prove RED on uncovered rows before fixes.

- [x] **Phase 2 — Invalid rows closure (Task 2)**
  - Close all `*-INVALID` rows with exact constructor assertions.
  - Apply minimal thesis-valid production fixes only if needed.

- [x] **Phase 3 — Valid rows closure (Task 3)**
  - Close all `*-VALID` rows for acceptance semantics.
  - Add presolution-path assertions for expected witness shape/success.

- [x] **Phase 4 — Normalization rows closure (Task 4)**
  - Close all `*-NORM` rows with canonicalization/idempotence checks.
  - Ensure presolution replay stability.

- [x] **Phase 5 — Verification gates (Task 5)**
  - Run matrix gate, witness-focused gate, and full gate.

- [x] **Phase 6 — Documentation/task sync (Task 6)**
  - Update `.kiro`, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and `/Volumes/src/mlf4/Bugs.md` to reflect matrix evidence and closure status.

## Critical Review Notes

- Plan asks for intermediate `git commit` steps; this session will execute all implementation/test/doc steps but will not create commits unless explicitly requested by the user.
- Worktree/branch isolation requirement is satisfied (`codex/2026-02-10-fig-15-3-4-raise-normalization-emission-full-alignment`).

## Error Log

- 2026-02-11: `--match` regex with alternation/whitespace did not select tests as expected under Hspec CLI parsing (`0 examples` / argument split). Recovery: standardized matrix gate to `--match R-` and witness-focused gate to `--match normalization`, both producing explicit non-empty passing suites.
