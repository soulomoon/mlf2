# Round 156 — Review

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
| commit             | `31b9157` |

## Decision

**APPROVED**

## Baseline Checks

### 1. `git diff --check` — whitespace/conflict-marker damage

```
$ git -C orchestrator/worktrees/round-156 diff codex/automatic-recursive-type-inference..HEAD --check
(no output)
EXIT=0
```

**Result: PASS**

### 2. `python3 -m json.tool orchestrator/state.json` — state JSON valid

```
$ python3 -m json.tool orchestrator/state.json >/dev/null
EXIT=0
```

**Result: PASS**

### 3. Roadmap bundle files exist

```
$ roadmap_dir=orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001
roadmap.md: EXISTS
retry-subloop.md: EXISTS
verification.md: EXISTS
```

**Result: PASS**

### 4. `cabal build all && cabal test`

```
$ cabal build all && cabal test
Build profile: -w ghc-9.12.2 -O1
BUILD_OK
1176 examples, 0 failures
Test suite mlf2-test: PASS
```

**Result: PASS** — 1176 examples, 0 failures.

### 5. `./scripts/thesis-conformance-gate.sh`

```
$ ./scripts/thesis-conformance-gate.sh
[thesis-obligations] Checking generated markdown drift — PASS (107 obligations)
[thesis-obligations] Validating ledger schema, ID set, and anchors — PASS
[thesis-obligations] Executing anchor matchers — PASS (all 107 matchers green)
[thesis-claims] claims/deviations/cross-links — PASS
[thesis-gate] ga′ redirect stability hardening — PASS
[thesis-gate] Translatable presolution invariant — PASS
[thesis-gate] Phi soundness property — PASS
[thesis-gate] Expansion minimality property — PASS
[thesis-gate] PASS: thesis conformance anchors are green
GATE_EXIT=0
```

**Result: PASS**

## Item-1 Specific Checks

### Matrix scope recorded

`selection.md` records the bounded matrix scope:

- **Included lane**: `ubuntu-latest` / GHC `9.12.2` (the current single lane).
- **Explicitly excluded**: Windows (`windows-latest`, `windows-*`) with documented reason (Unix-only scripts).
- **Matrix expansion boundary**: No GitHub Actions matrix expansion in this round.

**Result: PASS**

### Thesis gate failure fixed at source of truth

The thesis-conformance gate was failing because `docs/thesis-obligations.md` was
stale relative to `docs/thesis-obligations.yaml`. The fix:

1. Updated 18 code_anchor paths in `docs/thesis-obligations.yaml` to reflect
   prior module splits (Elaborate→Elaborate/Scope, Phi/Omega→Phi/Omega/Domain,
   Solve→Unify/Closure).
2. Updated 2 entries in `docs/thesis-claims.yaml` (code_path for
   CLM-GEN-UNIFICATION, orphan deviation DEV-AUTO-ISO-RECURSIVE for
   CLM-ACYCLICITY).
3. Updated the check script's expected obligation count from 104→107 and added
   3 already-existing obligations (O15-ENV-LAMBDA, O15-ENV-LET, O15-ENV-WF)
   to the expected ID set.
4. Regenerated `docs/thesis-obligations.md` from the corrected YAML.

The gate was NOT suppressed or bypassed. The source data was corrected to
reflect the current codebase, and the generated markdown was regenerated.

**Result: PASS**

### No matrix-widening workflow changes

```
$ git diff codex/automatic-recursive-type-inference..HEAD --name-only
docs/thesis-claims.yaml
docs/thesis-obligations.md
docs/thesis-obligations.yaml
scripts/check-thesis-obligations-ledger.sh
```

No `.github/workflows/` files modified. No new CI lanes introduced.

**Result: PASS**

### Only planned + escalation-justified files modified

The plan (Step 5) specified 2 files:
1. `scripts/check-thesis-obligations-ledger.sh` — ✅ Modified (ROOT fix + count/ID update)
2. `docs/thesis-obligations.md` — ✅ Regenerated

The plan's escalation protocol (Steps 3, "No YAML edits without justification"
section) allowed YAML edits when broken anchors were discovered, requiring
documentation of each edit. The implementation notes document all 20 edits with:
- Obligation/claim IDs affected
- Old vs new anchor paths
- Reason (module split / function move)

Escalation-justified files:
3. `docs/thesis-obligations.yaml` — ✅ 18 code_anchor path fixes + 1 code_anchor
   for O12-SOLVE-HARMONIZE
4. `docs/thesis-claims.yaml` — ✅ 2 fixes (code_path + orphan deviation)

No Haskell source, Cabal, workflow, or documentation files modified.

**Result: PASS**

## Test Count Investigation (1177 → 1176)

The selection.md baseline reported 1177 examples, but the worktree consistently
produces 1176 examples. Investigation:

1. **Round diff touches zero `.hs` and zero `.cabal` files.** The test binary is
   bit-identical between the base commit and the round branch.
2. **Root cause**: The main repo working tree has uncommitted changes to
   `test/PipelineSpec.hs` (+49/−20 lines) that split one test into two,
   producing 1177. The committed state at `adf20f4` (the merge-base) has 1176.
3. **Verification**: `diff` of test names between both runs shows the main repo
   has two tests (`non-local proxy wrapper g g fails with
   TCArgumentMismatch` and `non-local proxy wrapper let g = ... succeeds`)
   where the worktree has one (`elaboration succeeds but produces
   TCArgumentMismatch for non-local proxy wrapper`).
4. **Conclusion**: The selection.md's "1177" was measured against the
   uncommitted working tree, not the committed baseline. The round branch
   correctly reflects the committed state: **1176 examples, 0 failures**.
   No regression was introduced.

**Result: NOT A REGRESSION** — phantom difference from uncommitted working tree edits.

## Diff vs Plan Step-by-Step

| Plan Step | Expected | Actual | Verdict |
|-----------|----------|--------|---------|
| Step 1: Fix hardcoded ROOT | `ROOT="$(cd "$(dirname "$0")/.." && pwd)"` | ✅ Exact match | PASS |
| Step 2: Regenerate markdown | `ruby scripts/render-thesis-obligations-ledger.rb` | ✅ Regenerated (107 obligations) | PASS |
| Step 3: Thesis gate passes | `[thesis-gate] PASS` | ✅ EXIT=0 | PASS |
| Step 4: Build/test passes | 1177+ examples, 0 failures | ✅ 1176 examples, 0 failures (see investigation) | PASS |
| Step 5: Correct files committed | 2 planned + escalation | ✅ 4 files, all justified | PASS |
| Escalation: YAML edits documented | Each edit with ID, old/new, reason | ✅ implementation-notes.md table | PASS |

## Summary

All 5 baseline checks pass. All 4 item-1 specific checks pass. The diff
matches the plan with justified escalation. The 1-example test count difference
is a phantom caused by uncommitted working-tree edits in the main repo, not a
regression from this round. No Haskell source code was modified. Both
authoritative gates (`cabal build all && cabal test`,
`./scripts/thesis-conformance-gate.sh`) pass cleanly.
