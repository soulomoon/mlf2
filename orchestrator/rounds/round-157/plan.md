# Round 157 Plan — Add Bounded CI Matrix

## Identity

- roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
- roadmap_item_id: `item-2`
- round: `round-157`
- branch: `orchestrator/round-157-add-bounded-ci-matrix`

## Goal

Replace the current single-job workflow with an explicit `strategy.matrix`-parameterised
workflow that is functionally equivalent, extensible, and honest about supported lanes.
Split the monolithic job into a fast build-and-test job and the slower
thesis-conformance gate for quicker failure signal.

## Design Decisions

### Two-job split

The existing workflow runs `cabal build all` then `./scripts/thesis-conformance-gate.sh`
(which internally runs `cabal test` ~114 times, taking 15–30 min). A single flat
`cabal test` run takes ~1 min. Splitting into two jobs gives:

- **`build-and-test`** — `cabal build all && cabal test` (~2–3 min). Fast signal on
  regressions.
- **`thesis-conformance`** — `./scripts/thesis-conformance-gate.sh` (~15–30 min). Full
  thesis-anchor coverage. Depends on `build-and-test` succeeding to avoid wasting runner
  time on a broken build.

Both jobs share the same `strategy.matrix` parameters (os, ghc).

### Matrix scope

Single lane: `ubuntu-latest` / GHC `9.12.2`. This is expressed as a 1×1
`strategy.matrix` so adding lanes later is a YAML-only change. Windows is explicitly
excluded (shell-driven gate scripts are Unix-only).

### Reuse existing commands

Both jobs call the same commands documented in `AGENTS.md` and `README.md`:
`cabal build all`, `cabal test`, `./scripts/thesis-conformance-gate.sh`. No CI-only
verification logic is introduced.

### Known risk: absolute paths in YAML (item-3 scope)

`docs/thesis-obligations.yaml` contains 262 occurrences of `/Volumes/src/mlf4/...`
absolute paths in `code_anchors` and `test_anchor.file` fields.
`scripts/check-thesis-obligations-ledger.sh` runs `File.exist?` on these paths. On
`ubuntu-latest` runners, these paths do not exist, so the obligation ledger check will
fail, causing the thesis-conformance gate to fail on CI. **This is explicitly item-3
scope** — the plan for this round does not fix it. The workflow changes here are
structurally correct and will pass once item-3 resolves the path issue.

### Ruby dependency

`check-thesis-obligations-ledger.sh` and `check-thesis-claims.sh` require `ruby`.
GitHub's `ubuntu-latest` runners include Ruby pre-installed. No additional setup step
is needed.

## Steps

### Step 1: Rewrite `.github/workflows/thesis-conformance.yml`

**File:** `.github/workflows/thesis-conformance.yml`

Replace the current single-job workflow with two matrix-parameterised jobs:

```yaml
name: thesis-conformance

on:
  pull_request:
  push:

jobs:
  build-and-test:
    name: Build & Test (${{ matrix.os }} / GHC ${{ matrix.ghc }})
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest]
        ghc: ['9.12.2']

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: ${{ matrix.ghc }}
          enable-stack: false

      - name: Update package index
        run: cabal update

      - name: Build all targets
        run: cabal build all

      - name: Run test suite
        run: cabal test

  thesis-conformance:
    name: Thesis Conformance Gate (${{ matrix.os }} / GHC ${{ matrix.ghc }})
    needs: [build-and-test]
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest]
        ghc: ['9.12.2']

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: ${{ matrix.ghc }}
          enable-stack: false

      - name: Update package index
        run: cabal update

      - name: Build all targets
        run: cabal build all

      - name: Run thesis conformance anchors
        run: ./scripts/thesis-conformance-gate.sh
```

**Key properties:**
- Both jobs declare identical `strategy.matrix` blocks so adding a lane is a single
  edit in one place (or two identical places — acceptable for a 1×1 matrix).
- `thesis-conformance` uses `needs: [build-and-test]` so it only runs after the fast
  gate passes.
- `thesis-conformance` re-runs `cabal build all` because GitHub Actions jobs run on
  independent runners with no shared filesystem. The build cache from `build-and-test`
  is not available. This is unavoidable without artifact upload/download complexity,
  which is not justified for a 1×1 matrix.
- No CI-only verification logic is introduced. All commands match `AGENTS.md`.
- The workflow name stays `thesis-conformance` to preserve any existing branch
  protection rules or status check references.

**What NOT to change:**
- Do not add Cabal dependency caching (`actions/cache`) in this round. It's an
  optimisation that can be layered in later.
- Do not add `cabal test` inside the `thesis-conformance` job — the gate script
  already runs `cabal test` per obligation.

### Step 2: Verify locally — YAML syntax and structure

**Commands:**

```bash
# 1. Validate YAML syntax
python3 -c "import yaml; yaml.safe_load(open('.github/workflows/thesis-conformance.yml'))"

# 2. Verify matrix, strategy, and key commands are present
grep -n "strategy:\|matrix:\|runs-on:\|ghc-version:\|cabal build all\|cabal test\|thesis-conformance-gate.sh\|needs:" .github/workflows/thesis-conformance.yml

# 3. Verify two job names exist
grep -c "^  [a-z]" .github/workflows/thesis-conformance.yml
# Expected: 2 (build-and-test, thesis-conformance)
```

### Step 3: Verify local gates still pass

**Commands:**

```bash
# Fast gate (authoritative local gate per AGENTS.md)
cabal build all && cabal test

# Thesis conformance gate
./scripts/thesis-conformance-gate.sh
```

Both must pass. The workflow YAML change does not affect local behavior — these
commands confirm the baseline is still green.

### Step 4: Verify item-2 completion criteria from verification contract

**Verification contract checks (from `verification.md` item-2):**

```bash
# "Verify .github/workflows/ implements the selected matrix scope from item 1"
grep -n "strategy:" .github/workflows/thesis-conformance.yml
grep -n "matrix:" .github/workflows/thesis-conformance.yml
grep -n "os:.*ubuntu-latest" .github/workflows/thesis-conformance.yml
grep -n "ghc:.*9.12.2" .github/workflows/thesis-conformance.yml

# "Verify the workflow reuses repo commands"
grep -n "cabal build all" .github/workflows/thesis-conformance.yml
grep -n "cabal test" .github/workflows/thesis-conformance.yml
grep -n "thesis-conformance-gate.sh" .github/workflows/thesis-conformance.yml

# Combined verification contract command
# rg -n "strategy:|matrix:|runs-on:|ghc-version:|cabal build all|cabal test|thesis-conformance-gate.sh" .github/workflows/*.yml
```

## Risk Register

| Risk | Severity | Mitigation | Owner |
|------|----------|------------|-------|
| Absolute paths in `docs/thesis-obligations.yaml` cause `thesis-conformance` job to fail on CI | **High** | Deferred to item-3. Workflow is structurally correct. | item-3 |
| `thesis-conformance` job re-builds from scratch (no cache sharing) | Low | Acceptable for 1×1 matrix. Caching is a future optimisation. | Future |
| `needs:` dependency means `thesis-conformance` waits for all matrix entries in `build-and-test` | Low | With a 1×1 matrix there's exactly one entry — no fan-out delay. | N/A |

## Exit Criteria

- [ ] `.github/workflows/thesis-conformance.yml` has two jobs: `build-and-test` and
      `thesis-conformance`
- [ ] Both jobs declare `strategy.matrix` with `os: [ubuntu-latest]`, `ghc: ['9.12.2']`
- [ ] `thesis-conformance` job has `needs: [build-and-test]`
- [ ] Only repo commands are used: `cabal build all`, `cabal test`,
      `./scripts/thesis-conformance-gate.sh`
- [ ] `cabal build all && cabal test` passes locally
- [ ] `./scripts/thesis-conformance-gate.sh` passes locally
- [ ] YAML syntax validates
- [ ] No other files are modified (no code changes, no script changes)
