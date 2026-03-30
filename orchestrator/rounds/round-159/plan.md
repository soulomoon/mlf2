# Round 159 Plan — Item-4: Update repo guidance and handoff for ongoing CI maintenance

roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
roadmap_revision: `rev-001`
roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
roadmap_item_id: `item-4`

## Scope

Doc-only changes to three files: `README.md`, `TODO.md`, `CHANGELOG.md`. No source, test, workflow, or script changes.

---

## Step 1 — Add CI section to README.md

**File:** `README.md`

**Action:** Insert a new `## Continuous integration` section between the existing `## Build and test` section (ends ~line 35) and `## Public entry points` (starts ~line 37). Content:

```markdown
## Continuous integration

GitHub Actions runs two matrix-parameterized jobs on every push and pull
request, defined in `.github/workflows/thesis-conformance.yml`:

| Job                  | What it runs                                      |
|----------------------|---------------------------------------------------|
| `build-and-test`     | `cabal build all` then `cabal test`               |
| `thesis-conformance` | `./scripts/thesis-conformance-gate.sh` (after build) |

**Supported matrix lane:** `ubuntu-latest` / GHC 9.12.2.

**Excluded lane:** Windows — the thesis-conformance gate and supporting
scripts under `scripts/` are Unix shell; no Windows lane is promised until
those scripts are made portable.

### Authoritative verification commands

These two commands are the authoritative verification gates both locally and
in CI:

```bash
cabal build all && cabal test
./scripts/thesis-conformance-gate.sh
```

CI reuses the same repo commands; there is no CI-only verification logic.
```

**Why:** README currently has zero CI documentation. This fulfils the handoff requirement that the bounded matrix, excluded lanes, and local-vs-CI authority are explicitly documented.

---

## Step 2 — Update TODO.md Task 106 to completed

**File:** `TODO.md`

**Action:** Replace the Task 106 block (lines 28–48) to mark it completed and record what was delivered. Change the header to:

```
## Task 106 CI test-matrix and failure-repair campaign (completed 2026-03-30)
```

Replace the body with a completed summary:

- Completed:
  - Item-1 (round-156): Froze bounded matrix scope (ubuntu-latest / GHC 9.12.2, Windows excluded). Repaired thesis-conformance baseline: fixed 18 stale code_anchor paths in thesis-obligations.yaml, fixed thesis-claims.yaml code_path, resolved orphan deviation, made check script ROOT-portable.
  - Item-2 (round-157): Split `.github/workflows/thesis-conformance.yml` into two matrix-parameterized jobs: `build-and-test` (cabal build all && cabal test) and `thesis-conformance` (thesis-conformance gate script). Both use strategy.matrix with os: [ubuntu-latest], ghc: ['9.12.2'].
  - Item-3 (round-158): Converted 262 absolute `/Volumes/src/mlf4/` paths in `docs/thesis-obligations.yaml` to repo-relative paths. Updated `check-thesis-obligations-ledger.sh` to use ROOT-join pattern. Regenerated `docs/thesis-obligations.md`.
  - Item-4 (round-159): Updated README.md, TODO.md, and CHANGELOG.md to document the CI matrix, runner boundaries, and local-vs-CI authoritative commands.
- Verification:
  - `cabal build all && cabal test`: PASS
  - `./scripts/thesis-conformance-gate.sh`: PASS

Remove the "Rolling priorities (next)" sub-section since all four items are now done.

**Why:** The CI campaign is complete; the TODO entry should reflect that.

---

## Step 3 — Add CHANGELOG entries for the CI matrix campaign

**File:** `CHANGELOG.md`

**Action:** Add a new bullet under `## Unreleased` → `### Changed` (after line 6, before the existing iso-recursive entry on line 6). Insert:

```markdown
- Completed CI test-matrix and failure-repair campaign (rounds 156–159): froze a bounded GitHub Actions matrix (ubuntu-latest / GHC 9.12.2, Windows excluded while thesis-conformance gates remain Unix-only), repaired the thesis-conformance baseline by fixing 18 stale code-anchor paths and converting 262 absolute paths to repo-relative, split the workflow into two matrix-parameterized jobs (`build-and-test` and `thesis-conformance`), and documented the CI matrix scope, runner boundaries, and authoritative verification commands in README.md, TODO.md, and CHANGELOG.md. Validated with `cabal build all && cabal test` (1176+ examples, 0 failures) and `./scripts/thesis-conformance-gate.sh` (PASS).
```

**Why:** The CHANGELOG needs an entry for the entire CI campaign spanning items 1–4.

---

## Step 4 — Verify

Run the two authoritative verification gates:

```bash
cabal build all && cabal test
./scripts/thesis-conformance-gate.sh
```

Also confirm the docs changes are clean:

```bash
git diff --check
```

**Expected:** Both gates green, no whitespace damage.

---

## Completion criteria checklist

- [ ] `README.md` has a CI section describing the two-job matrix, the supported lane, the excluded lane, and the authoritative verification commands
- [ ] `TODO.md` Task 106 is marked completed with item-by-item summary
- [ ] `CHANGELOG.md` has an entry for the CI matrix campaign (rounds 156–159)
- [ ] The handoff is explicit: `cabal build all && cabal test` and `./scripts/thesis-conformance-gate.sh` are authoritative both locally and in CI; CI reuses repo commands with no CI-only logic
- [ ] Windows exclusion is documented explicitly (Unix-only shell gates)
- [ ] `cabal build all && cabal test` passes
- [ ] `./scripts/thesis-conformance-gate.sh` passes
- [ ] `git diff --check` clean
