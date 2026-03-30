# Round 158 — Plan

## Identity

- roadmap_id: `2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-30-00-test-matrix-and-failure-repair-successor-roadmap/rev-001`
- roadmap_item_id: `item-3`
- round: `round-158`
- branch: `orchestrator/round-158-fix-matrix-exposed-failures`

## Goal

Fix the matrix-exposed CI failure caused by 262 absolute `/Volumes/src/mlf4/`
paths in `docs/thesis-obligations.yaml` and the corresponding direct
`File.exist?`/`File.read` calls in `scripts/check-thesis-obligations-ledger.sh`.
These paths only resolve on the developer's local machine and break on any CI
runner (e.g. `ubuntu-latest`).

The fix has two parts:
1. Convert all 262 absolute paths in the YAML to repo-relative paths.
2. Update the Ruby validation code in the ledger checker to prepend `ROOT`
   before filesystem access, matching the pattern already used in
   `scripts/check-thesis-claims.sh`.
3. Regenerate `docs/thesis-obligations.md` so the rendered markdown stays in
   sync.

## Root Cause

`scripts/check-thesis-obligations-ledger.sh` embeds a Ruby heredoc that uses
paths from `docs/thesis-obligations.yaml` verbatim for `File.exist?` and
`File.read`. The YAML contains absolute paths like
`/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv` and
`/Volumes/src/mlf4/test/TypeCheckSpec.hs`. On any machine where
`/Volumes/src/mlf4/` does not exist, validation fails immediately.

In contrast, `scripts/check-thesis-claims.sh` already handles this correctly:
it passes `ROOT` as `ARGV[3]` into its Ruby heredoc and uses
`File.join(root, path)` before `File.exist?` (line 207).

## Design Decisions

1. **Relative paths in YAML, ROOT-join in scripts.** This is the same pattern
   `thesis-claims.yaml` + `check-thesis-claims.sh` already use. No new pattern
   invented.

2. **Bulk `sed` replacement in YAML.** All 262 occurrences share the identical
   prefix `/Volumes/src/mlf4/`. A single `sed -i '' 's|/Volumes/src/mlf4/||g'`
   on macOS (or `sed -i 's|/Volumes/src/mlf4/||g'` on Linux) converts them.
   Result: `code_anchors` entries become e.g. `src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv`,
   `test_anchor.file` entries become e.g. `test/TypeCheckSpec.hs`.

3. **Pass ROOT into the Ruby heredoc.** The shell script already computes
   `ROOT` on line 5. Pass it as an additional argument to `ruby -` so the
   Ruby code can prepend it before `File.exist?`, `File.read`.

4. **Regenerate rendered markdown.** `docs/thesis-obligations.md` is generated
   from the YAML by `scripts/render-thesis-obligations-ledger.rb`. After the
   YAML changes, re-run the renderer so the `--check` mode passes. The
   rendered markdown currently contains 106 absolute paths in the Test File
   column; after regeneration these will be relative.

5. **No compatibility shim.** Per AGENTS.md: "you should not add new
   compatibility layers or convenience fallbacks." If someone has tooling
   depending on the absolute paths, they update it.

## Concrete Steps

### Step 1: Convert absolute paths in `docs/thesis-obligations.yaml`

**File:** `docs/thesis-obligations.yaml`

**Operation:** Replace every occurrence of `/Volumes/src/mlf4/` with the empty
string. This is a global string replacement — 262 occurrences.

```bash
sed -i '' 's|/Volumes/src/mlf4/||g' docs/thesis-obligations.yaml
```

**Resulting path examples:**
- Before: `"/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv"`
- After:  `"src/MLF/Elab/TypeCheck.hs#typeCheckWithEnv"`
- Before: `"/Volumes/src/mlf4/test/TypeCheckSpec.hs"`
- After:  `"test/TypeCheckSpec.hs"`

**Verification:**
```bash
grep -c '/Volumes/src/mlf4' docs/thesis-obligations.yaml
# Expected: 0
```

### Step 2: Update `scripts/check-thesis-obligations-ledger.sh` Ruby code to use ROOT

**File:** `scripts/check-thesis-obligations-ledger.sh`

**Operation:** Three changes inside the Ruby heredoc (lines 26–163):

#### 2a. Pass ROOT into the Ruby heredoc

Change line 26 from:
```bash
ruby - "${LEDGER}" >"${tmp_rows}" <<'RUBY'
```
to:
```bash
ruby - "${LEDGER}" "${ROOT}" >"${tmp_rows}" <<'RUBY'
```

#### 2b. Read ROOT in Ruby

After line 30 (`doc = YAML.load_file(ledger_path)`), add:
```ruby
root = ARGV.fetch(1)
```

#### 2c. Prepend root to paths before filesystem access

**test_anchor.file (around line 123):** Change:
```ruby
  elsif !File.exist?(test_file)
    groups['missing-files'] << "#{id}: test file not found: #{test_file}"
```
to:
```ruby
  elsif !File.exist?(File.join(root, test_file))
    groups['missing-files'] << "#{id}: test file not found: #{test_file}"
```

**code_anchors path existence (around line 144):** Change:
```ruby
      unless File.exist?(path)
        groups['missing-files'] << "#{id}: code file not found: #{path}"
        next
      end
      contents = File.read(path)
```
to:
```ruby
      full_path = File.join(root, path)
      unless File.exist?(full_path)
        groups['missing-files'] << "#{id}: code file not found: #{path}"
        next
      end
      contents = File.read(full_path)
```

Note: error messages keep the relative `path`/`test_file` for readability (no
need to show the absolute resolved path in errors since the relative path is
the source-of-truth value).

**Verification:**
```bash
# Syntax check the modified script
bash -n scripts/check-thesis-obligations-ledger.sh
# Expected: exit 0, no output

# Run the ledger checker
./scripts/check-thesis-obligations-ledger.sh
# Expected: PASS
```

### Step 3: Regenerate `docs/thesis-obligations.md`

**File:** `docs/thesis-obligations.md` (generated output)

**Operation:** Run the renderer to update the markdown from the now-relative
YAML paths:

```bash
ruby scripts/render-thesis-obligations-ledger.rb
```

This rewrites `docs/thesis-obligations.md`. The Test File column will now show
relative paths like `test/BindingSpec.hs` instead of
`/Volumes/src/mlf4/test/BindingSpec.hs`.

**Verification:**
```bash
grep -c '/Volumes/src/mlf4' docs/thesis-obligations.md
# Expected: 0

# The --check mode must now pass:
ruby scripts/render-thesis-obligations-ledger.rb --check
# Expected: exit 0
```

### Step 4: Full verification gate

Run the complete verification suite to confirm nothing is broken:

```bash
cabal build all && cabal test
# Expected: build succeeds, 1177+ examples, 0 failures

./scripts/thesis-conformance-gate.sh
# Expected: PASS (this runs check-thesis-obligations-ledger.sh,
#           check-thesis-claims.sh, and all anchor matchers)

git diff --check
# Expected: no whitespace or conflict-marker issues
```

## Risk Register

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| `sed` on macOS vs Linux differs (`-i ''` vs `-i`) | Low — implementer runs locally on macOS | Use `sed -i '' 's|...|...|g'` for macOS. The CI runner difference is moot because this is a one-time file edit committed to git. |
| Render script schema validation rejects relative paths | Very low — render script validates `#` presence in anchors but does not validate path prefix | Confirmed by reading `render-thesis-obligations-ledger.rb`: no absolute-path assumption in validation. |
| Other scripts read `thesis-obligations.yaml` paths directly | Low — grepped for consumers; only `check-thesis-obligations-ledger.sh` and `render-thesis-obligations-ledger.rb` consume these paths | `render-thesis-obligations-ledger.rb` only validates schema (has `#`) and renders text; it does no `File.exist?` on the paths. No fix needed there. |
| The YAML has paths not starting with `/Volumes/src/mlf4/` that should also be relative | None — confirmed all 262 matches share the identical prefix; zero non-matching absolute paths exist. | N/A |
| `thesis-conformance-gate.sh` or other callers pass different ROOT assumptions | Very low — `ROOT` is computed identically (`cd "$(dirname "$0")/.." && pwd`) in all scripts | The pattern is already established and working in `check-thesis-claims.sh`. |

## Exit Criteria

All of the following must be true:

1. `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.yaml` returns `0`
2. `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.md` returns `0`
3. `bash -n scripts/check-thesis-obligations-ledger.sh` exits 0
4. `./scripts/check-thesis-obligations-ledger.sh` exits 0 (PASS)
5. `ruby scripts/render-thesis-obligations-ledger.rb --check` exits 0
6. `cabal build all && cabal test` passes (1177+ examples, 0 failures)
7. `./scripts/thesis-conformance-gate.sh` passes
8. `git diff --check` clean
9. No CI-only bypass or compatibility shim introduced
10. No absolute `/Volumes/` paths remain in any `docs/thesis-*.yaml` file
