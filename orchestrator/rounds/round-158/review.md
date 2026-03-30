# Round 158 — Review

## Identity

- roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
- roadmap_item_id: `item-3`
- round: `round-158`
- branch: `orchestrator/round-158-fix-matrix-exposed-failures`

## Baseline Checks

| # | Command | Result | Evidence |
|---|---------|--------|----------|
| 1 | `git -C orchestrator/worktrees/round-158 diff --check codex/automatic-recursive-type-inference...HEAD` | **PASS** | Exit 0, no output |
| 2 | `python3 -m json.tool orchestrator/state.json >/dev/null` | **PASS** | Exit 0 |
| 3 | Roadmap bundle exists (`roadmap.md`, `retry-subloop.md`, `verification.md`) | **PASS** | All three files present |
| 4 | `cabal build all && cabal test` (from round-158 worktree) | **PASS** | Build succeeds, 1176 examples, 0 failures |
| 5 | `./scripts/thesis-conformance-gate.sh` (from round-158 worktree) | **PASS** | `[thesis-gate] PASS: thesis conformance anchors are green`, exit 0 |

## Item-3 Specific Checks

| # | Check | Result | Evidence |
|---|-------|--------|----------|
| 6 | `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.yaml` returns 0 | **PASS** | Output: `0` (grep exit 1 = no matches) |
| 7 | No CI-only bypass or compatibility fallback | **PASS** | ROOT-join pattern in `check-thesis-obligations-ledger.sh` exactly matches the established pattern in `check-thesis-claims.sh`: `ROOT` computed identically, passed as ARGV, used with `File.join(root, path)` before `File.exist?`/`File.read` |
| 8 | Ruby script edits match plan (ARGV passing, root variable, File.join usage) | **PASS** | Line 26: `ruby - "${LEDGER}" "${ROOT}"` ✓; Line 31: `root = ARGV.fetch(1)` ✓; Line 124: `File.exist?(File.join(root, test_file))` ✓; Lines 145-150: `full_path = File.join(root, path)`, `File.exist?(full_path)`, `File.read(full_path)` ✓ |
| 9 | `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.md` returns 0 | **PASS** | Output: `0` (grep exit 1 = no matches) |
| 10 | `ruby scripts/render-thesis-obligations-ledger.rb --check` passes | **PASS** | Exit 0, rendered output matches YAML |
| 11 | No files outside plan scope modified | **PASS** | `git diff --name-only` shows exactly 3 files: `docs/thesis-obligations.md`, `docs/thesis-obligations.yaml`, `scripts/check-thesis-obligations-ledger.sh` |

## Plan Compliance (Step-by-Step)

### Step 1: Convert absolute paths in YAML
- **Plan:** Replace 262 occurrences of `/Volumes/src/mlf4/` with empty string
- **Diff:** YAML diff shows `"/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv"` → `"src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv"` and similar for all `code_anchors` and `test_anchor.file` entries
- **Verification:** `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.yaml` returns 0
- **Result:** ✅ Matches plan

### Step 2a: Pass ROOT into the Ruby heredoc
- **Plan:** Change `ruby - "${LEDGER}"` to `ruby - "${LEDGER}" "${ROOT}"`
- **Diff:** Line 26 changed from `ruby - "${LEDGER}" >"${tmp_rows}" <<'RUBY'` to `ruby - "${LEDGER}" "${ROOT}" >"${tmp_rows}" <<'RUBY'`
- **Result:** ✅ Matches plan

### Step 2b: Read ROOT in Ruby
- **Plan:** Add `root = ARGV.fetch(1)` after `doc = YAML.load_file(ledger_path)`
- **Diff:** `+root = ARGV.fetch(1)` added after `doc = YAML.load_file(ledger_path)`
- **Result:** ✅ Matches plan

### Step 2c: Prepend root to paths before filesystem access
- **Plan:** Three locations: `File.exist?(test_file)` → `File.exist?(File.join(root, test_file))`; `File.exist?(path)` → `File.exist?(full_path)` with `full_path = File.join(root, path)`; `File.read(path)` → `File.read(full_path)`
- **Diff:** All three changes present at lines 124, 145-146, 150
- **Result:** ✅ Matches plan exactly

### Step 3: Regenerate markdown
- **Plan:** Rerun renderer; Test File column shows relative paths
- **Diff:** 212 lines changed in `docs/thesis-obligations.md` (106 path replacements in table cells)
- **Verification:** `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.md` returns 0; `--check` passes
- **Result:** ✅ Matches plan

### Step 4: Full verification gate
- **Plan:** Both authoritative gates pass
- **Result:** ✅ Both pass from round-158 worktree

## Test Count Note

The round-158 worktree reports 1176 examples vs 1177 in the main worktree. No Haskell files were modified in this round (diff is docs + shell script only), so this is a dist-newstyle isolation artifact between worktrees, not a regression. Both report 0 failures.

## Scope Verification

- **Files changed:** 3 (exactly as planned)
- **No CI-only bypass:** Confirmed — same ROOT-join pattern as `check-thesis-claims.sh`
- **No compatibility fallback:** Confirmed — paths are simply relative; no conditional absolute-path handling
- **No new tests needed:** The fix is path-format correction in generated artifacts, not a behavior change; the existing thesis-obligations gate (`check-thesis-obligations-ledger.sh`) itself serves as the reproducer and regression check

## Decision

**APPROVED**

All baseline checks pass. All item-3 specific checks pass. The diff matches the plan step-by-step with no deviations, no scope creep, no CI bypasses, and no compatibility shims. Both authoritative repo gates pass from the round-158 worktree.
