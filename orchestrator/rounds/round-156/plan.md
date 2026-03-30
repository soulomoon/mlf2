# Round 156 — Plan

## Identity

| Field              | Value |
|--------------------|-------|
| roadmap_id         | `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap` |
| roadmap_revision   | `rev-001` |
| roadmap_dir        | `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001` |
| roadmap_item_id    | `item-1` |
| round_id           | `round-156` |
| base_branch        | `codex/automatic-recursive-type-inference` |
| branch             | `orchestrator/round-156-freeze-scope-repair-baseline` |

## Goal

Repair the thesis-conformance baseline so that
`./scripts/thesis-conformance-gate.sh` passes on the current codebase, and
record the bounded matrix scope decision via the selection document.

## Diagnosis

The gate fails at its first sub-check:

```
[thesis-obligations] Checking generated markdown drift
thesis-obligations render: markdown is out of date: /Volumes/src/mlf4/docs/thesis-obligations.md
```

Root cause: `docs/thesis-obligations.md` (a generated file) is stale relative to
`docs/thesis-obligations.yaml`. The render script
(`scripts/render-thesis-obligations-ledger.rb`) has not been re-run since the
YAML was last updated.

### Secondary issue — hardcoded ROOT in check script

`scripts/check-thesis-obligations-ledger.sh` line 5 hardcodes:

```bash
ROOT="/Volumes/src/mlf4"
```

This causes three problems:

1. **Worktree execution**: running the gate from the round-156 worktree
   (`orchestrator/worktrees/round-156/`) will validate files under
   `/Volumes/src/mlf4` instead of the worktree, so changes made in the
   worktree are invisible to the check.
2. **CI execution**: on GitHub Actions (`ubuntu-latest`), the checkout lives
   under `/home/runner/work/...`, so the absolute path does not resolve and
   every `File.exist?` call on code anchors and test files will fail.
3. **Portability**: any developer cloning to a different path will hit the
   same failure.

The sister script `scripts/check-thesis-claims.sh` already uses the portable
pattern `ROOT="$(cd "$(dirname "$0")/.." && pwd)"`. Aligning the obligations
check script to the same pattern is the minimal fix.

### YAML absolute-path note

`docs/thesis-obligations.yaml` stores `code_anchors` and many
`test_anchor.file` entries as absolute paths under `/Volumes/src/mlf4/...`.
These paths resolve on the current development machine because
`/Volumes/src/mlf4` exists, so they do **not** block the local gate. They
**will** break on CI (item-2/item-3 scope). This plan does **not** modify the
YAML source data; the YAML absolute-path cleanup is deferred to a later item
where CI path resolution is the explicit deliverable.

## Scope Boundaries

### In scope (this plan)

1. Fix hardcoded `ROOT` in `scripts/check-thesis-obligations-ledger.sh`.
2. Regenerate `docs/thesis-obligations.md` from `docs/thesis-obligations.yaml`.
3. Verify the full thesis-conformance gate passes.
4. Verify `cabal build all && cabal test` continues to pass.

### Out of scope

- YAML source data changes (`docs/thesis-obligations.yaml`,
  `docs/thesis-claims.yaml`, `docs/thesis-deviations.yaml`).
- Production Haskell code changes (`src/`, `src-public/`, `app/`).
- GitHub Actions workflow changes (`.github/workflows/`).
- CI-only path resolution for YAML absolute paths (item-2/item-3).
- `AGENTS.md`, `README.md`, `CHANGELOG.md`, `TODO.md` updates (item-4).

## Steps

### Step 1 — Fix hardcoded ROOT in `scripts/check-thesis-obligations-ledger.sh`

**File**: `scripts/check-thesis-obligations-ledger.sh`

**Change**: Replace line 5:

```bash
ROOT="/Volumes/src/mlf4"
```

with:

```bash
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
```

This matches the portable pattern already used in
`scripts/check-thesis-claims.sh` (line 5). The rest of the script derives
`LEDGER` and `RENDER` from `ROOT`, so no other lines need to change.

**Justification**: Without this fix, the gate cannot validate changes made in
the worktree. The hardcoded path also prevents CI execution (item-2
dependency). This is a one-line infrastructure fix to a shell script, not a
production code change.

**Verification**:

```bash
# From the worktree root:
bash -n scripts/check-thesis-obligations-ledger.sh   # syntax check
head -7 scripts/check-thesis-obligations-ledger.sh    # visual confirm
```

### Step 2 — Regenerate `docs/thesis-obligations.md`

**File**: `docs/thesis-obligations.md` (output, not hand-edited)

**Command** (run from the worktree root or main repo root):

```bash
ruby scripts/render-thesis-obligations-ledger.rb
```

This reads `docs/thesis-obligations.yaml`, validates its schema, and writes
the regenerated markdown to `docs/thesis-obligations.md`.

**What it does NOT do**: It does not modify the YAML source. The render script
is read-only with respect to the YAML.

**Verification**:

```bash
ruby scripts/render-thesis-obligations-ledger.rb --check
# Expected: exits 0 (no output, no error)
```

If `--check` fails after regeneration, the render script or YAML has a problem
that needs diagnosis before proceeding.

### Step 3 — Run the full thesis-conformance gate

**Command** (from the worktree root):

```bash
./scripts/thesis-conformance-gate.sh
```

This executes all sub-checks in order:

1. `./scripts/check-thesis-obligations-ledger.sh` — markdown drift (step 2
   fix), schema validation, code anchor existence, 104 obligation test
   matchers via `cabal test`.
2. `./scripts/check-thesis-claims.sh` — claims/deviations YAML validation,
   cross-links, code paths.
3. Ten `run_anchor` calls with specific Hspec matchers and minimum example
   counts.

**Expected result**: `[thesis-gate] PASS: thesis conformance anchors are green`

**If this step fails**: The failure will identify which sub-check broke. The
implementer should diagnose the specific sub-check failure:

- **Obligations ledger sub-check fails** (not markdown drift): Likely a
  code_anchor pointing to a moved/renamed symbol, or a test matcher that no
  longer matches any examples. Investigate the specific obligation ID reported
  in the error. The YAML may need targeted edits (justified by broken
  anchors), but the plan should document each edit.
- **Claims sub-check fails**: Likely a cross-link or code_path issue in
  `docs/thesis-claims.yaml`. Same targeted-edit approach.
- **Anchor matcher fails**: A specific Hspec matcher returned 0 examples or
  had failures. This indicates a test regression, not a doc issue — escalate
  per retry-subloop contract.

### Step 4 — Run the build/test gate

**Command** (from the worktree root):

```bash
cabal build all && cabal test
```

**Expected result**: Build succeeds, 1177+ examples, 0 failures.

This step confirms no regressions were introduced by the doc/script changes.
Since this plan modifies only a shell script (step 1) and a generated markdown
file (step 2), there should be zero risk of build/test impact. This step is
required by the verification contract regardless.

### Step 5 — Commit

**Files to commit**:

| File | Change type |
|------|-------------|
| `scripts/check-thesis-obligations-ledger.sh` | Modified (ROOT fix) |
| `docs/thesis-obligations.md` | Regenerated |

**Commit message** (suggested):

```
Repair thesis-conformance baseline: regenerate obligations markdown and fix hardcoded ROOT
```

No other files should be modified. In particular, do NOT modify:
- `docs/thesis-obligations.yaml` (YAML source)
- `docs/thesis-claims.yaml`
- `docs/thesis-deviations.yaml`
- Any `.hs` files
- Any `.yml` workflow files

## Execution Constraints

### Worktree path awareness

The round-156 worktree is at `orchestrator/worktrees/round-156/`. After
step 1 (ROOT fix), all scripts should work correctly from this path. Before
step 1, the check script would validate against `/Volumes/src/mlf4` instead.

**Critical**: Execute step 1 BEFORE step 2, so that verification in step 2
validates the worktree's regenerated markdown, not the main repo's stale copy.

### Ruby dependency

The render script requires Ruby with the `yaml` standard library. This is
available on macOS by default and on the CI runner (ubuntu-latest ships Ruby).

### No YAML edits without justification

The YAML files (`docs/thesis-obligations.yaml`, `docs/thesis-claims.yaml`,
`docs/thesis-deviations.yaml`) are the source of truth for thesis conformance.
This plan does not modify them. If step 3 reveals broken anchors in the YAML,
the implementer must document each required edit with:
- The specific obligation/claim ID
- The old vs new anchor value
- Why the anchor is broken (e.g., file moved, symbol renamed)

### Gate timeout

The full thesis-conformance gate runs `cabal test` once per obligation (104
times) plus 10 additional anchor matchers. This can take 15-30+ minutes
depending on cache state. The implementer should ensure `dist-newstyle/` is
warm before running the gate.

## Verification Checklist

Per `roadmap_dir/verification.md`, the reviewer must confirm:

- [ ] `git diff --check` — no whitespace or conflict-marker damage
- [ ] `python3 -m json.tool orchestrator/state.json >/dev/null` — state JSON valid
- [ ] Roadmap bundle files exist at `roadmap_dir/`
- [ ] `cabal build all && cabal test` — PASS
- [ ] `./scripts/thesis-conformance-gate.sh` — PASS
- [ ] The round records the exact selected matrix scope and explicit excluded
      lanes (in `selection.md`)
- [ ] The thesis gate failure was fixed at the source of truth (regenerated
      markdown from YAML, not suppressed or bypassed)
- [ ] No matrix-widening workflow changes landed
- [ ] Only `scripts/check-thesis-obligations-ledger.sh` and
      `docs/thesis-obligations.md` were modified
