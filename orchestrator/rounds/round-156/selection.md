# Round 156 — Task Selection

## Identity

| Field              | Value |
|--------------------|-------|
| roadmap_id         | `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap` |
| roadmap_revision   | `rev-001` |
| roadmap_dir        | `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001` |
| roadmap_item_id    | `item-1` |
| round_id           | `round-156` |
| base_branch        | `codex/automatic-recursive-type-inference` |

## Selected Item

**item-1 — Freeze bounded matrix scope and repair the current thesis-conformance baseline**

## Why Now

- `item-1` is the lowest-numbered unfinished roadmap item, has no
  dependencies, and no retry state is active — it is the only valid choice.
- Every subsequent item (`item-2` through `item-4`) depends on `item-1`
  completing first; the matrix cannot be expanded until the bounded scope
  decision is recorded and the thesis-conformance baseline is repaired.
- The current CI has exactly one workflow
  (`.github/workflows/thesis-conformance.yml`) running a single
  `ubuntu-latest` / GHC 9.12.2 lane. The thesis-conformance gate within that
  lane is the only verification beyond `cabal build all && cabal test`, and it
  is currently broken at the source-of-truth level (`docs/thesis-obligations.md`
  drift), not at the implementation level.

## Current Repo Baseline

- `cabal build all && cabal test`: **PASS** — 1177 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: **FAIL** — the gate script fails
  because `docs/thesis-obligations.md` is out of date relative to the current
  codebase. The implementation itself is not broken; the obligations document
  has not been maintained through recent campaigns.
- Open bugs: BUG-2026-03-16-001 (InstBot replay-mode defect) — unrelated to
  CI matrix scope or thesis-conformance gate; does not block this item.

## Bounded Matrix Scope Decision

The following matrix scope is selected for this campaign based on the current
workflow shape and runner/toolchain constraints:

### Included lanes (to be formalized in item-2)

| Runner          | GHC Version | Rationale |
|-----------------|-------------|-----------|
| `ubuntu-latest` | `9.12.2`    | Current single lane; known-green for `cabal build all && cabal test`. |

Additional lanes (e.g., `ubuntu-latest` × older GHC, `macos-latest`) may be
proposed in `item-2` after the baseline is repaired. The scope decision here
is deliberately minimal: freeze the current single lane and repair it first.

### Explicitly excluded lanes

| Runner    | Reason |
|-----------|--------|
| **Windows** (`windows-latest`, `windows-*`) | The thesis-conformance gate (`./scripts/thesis-conformance-gate.sh`) and other shell-driven verification scripts are Unix-only. Adding Windows requires making those scripts cross-platform, which is out of scope for this campaign. |

### Matrix expansion boundary

No GitHub Actions matrix expansion lands in this round beyond what is needed
to support the repaired baseline. Matrix expansion is the explicit scope of
`item-2`.

## Scope Boundaries and Exclusions

### In scope for item-1

1. Record the bounded matrix scope decision (this document).
2. Repair `docs/thesis-obligations.md` at the source of truth so that
   `./scripts/thesis-conformance-gate.sh` passes on the current codebase.
3. Verify the full gate: `cabal build all && cabal test` continues to pass
   and `./scripts/thesis-conformance-gate.sh` passes after repair.

### Explicitly out of scope

- Any new GitHub Actions workflow files or matrix changes.
- Any production code changes (Haskell source under `src/`, `src-public/`,
  `app/`).
- Fixing BUG-2026-03-16-001 or any other open implementation bugs.
- Changes to `AGENTS.md`, `README.md`, `CHANGELOG.md`, or `TODO.md` (those
  belong to `item-4`).
- Any Windows or macOS runner support.
