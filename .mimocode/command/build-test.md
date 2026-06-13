---
description: "Standardized cabal build and test cycle for mlf2. Usage: /build-test build, /build-test test [pattern], /build-test [pattern], /build-test all"
---

# Build-Test Command

Run the standard mlf2 build and/or test cycle with clean, filtered output.

## Arguments

`$ARGUMENTS` — one of:
- `build` — build only, show errors
- `test [pattern]` — run tests, optionally matching a hspec pattern
- `[pattern]` — build then run tests matching the pattern (default behavior)
- `all` — build then run full test suite
- `errors` — build only, count and list distinct errors

## Procedure

### Build only (`build` or `errors`)

```bash
cabal build 2>&1 | tail -20
```

If there are errors, filter and display them:

```bash
cabal build 2>&1 | grep -E '^\S+\.hs:[0-9]+:[0-9]+: error:' | sort -u | head -30
```

For `errors`, also show the count:

```bash
cabal build 2>&1 | grep -c 'error:' || echo "0 errors"
```

### Test only (`test [pattern]`)

Run the test suite with `--test-show-details=direct`. If a pattern is provided, match it:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='-m "PATTERN"' 2>&1 | tail -40
```

Without a pattern, run the full suite:

```bash
cabal test mlf2-test --test-show-details=direct 2>&1 | tail -20
```

### Build then test (default — `[pattern]` or `all`)

Build first, check for errors, then run tests:

```bash
cabal build 2>&1 | tail -5 && cabal test mlf2-test --test-show-details=direct --test-options='-m "PATTERN"' 2>&1 | tail -30
```

For `all`, omit the `-m` flag.

## Conventions

- Always use `mlf2-test` as the explicit test suite name.
- Always pass `--test-show-details=direct` to get per-test output.
- Use `tail -20` for build output, `tail -30` for test output (tests are noisier).
- On error, show the first 30 unique error locations sorted by file.
- Timeout: 300s for build, 600s for test.
