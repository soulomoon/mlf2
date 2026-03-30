# Round 158 — Implementation Notes

## Change Summary

Fixed 262 absolute `/Volumes/src/mlf4/` paths in `docs/thesis-obligations.yaml`
and updated `scripts/check-thesis-obligations-ledger.sh` to prepend `ROOT`
before filesystem access. Regenerated `docs/thesis-obligations.md` to stay in
sync. These absolute paths only resolved on the developer's local machine and
broke on any CI runner (e.g. `ubuntu-latest`).

The fix follows the identical pattern already established in
`scripts/check-thesis-claims.sh`, which passes `ROOT` as an ARGV parameter and
uses `File.join(root, path)` before `File.exist?`/`File.read`.

## Files Modified

| File | Change |
|------|--------|
| `docs/thesis-obligations.yaml` | Replaced 262 occurrences of `/Volumes/src/mlf4/` with empty string (paths now repo-relative) |
| `scripts/check-thesis-obligations-ledger.sh` | Passed `"${ROOT}"` as second arg to Ruby heredoc; added `root = ARGV.fetch(1)`; changed `File.exist?(test_file)` → `File.exist?(File.join(root, test_file))`; changed `File.exist?(path)`/`File.read(path)` → `File.exist?(full_path)`/`File.read(full_path)` with `full_path = File.join(root, path)` |
| `docs/thesis-obligations.md` | Regenerated from updated YAML (106 absolute paths in Test File column now relative) |

## Verification Results

| Gate | Result |
|------|--------|
| `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.yaml` | `0` ✅ |
| `grep -c '/Volumes/src/mlf4' docs/thesis-obligations.md` | `0` ✅ |
| `bash -n scripts/check-thesis-obligations-ledger.sh` | exit 0 ✅ |
| `ruby scripts/render-thesis-obligations-ledger.rb --check` | exit 0 ✅ |
| `cabal build all` | Up to date ✅ |
| `cabal test` | 1176 examples, 0 failures ✅ |
| `./scripts/thesis-conformance-gate.sh` | PASS ✅ |
| `git diff --check` | clean ✅ |

## Commit

- Hash: `c958d72`
- Branch: `orchestrator/round-158-fix-matrix-exposed-failures`
- Message: `Fix absolute paths in thesis-obligations for CI portability`

## Known Limitations

None. The change is a straightforward path-prefix removal + root-join pattern
that exactly matches the existing `check-thesis-claims.sh` approach.
