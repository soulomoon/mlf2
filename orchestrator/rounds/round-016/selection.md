# Round 016 Selection

Date: 2026-03-15
Round: 016
Role: guider
Roadmap: `URI-R2-C1` prototype evidence successor roadmap
Selected roadmap item: 1
Selected stage: `P1` subject-discovery prototype
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: round selection only

## Selection

This round selects roadmap item 1: execute the bounded `P1` subject-discovery prototype for `URI-R2-C1`.

## Why This Stage Now

- Item 1 is the lowest-numbered unfinished roadmap item.
- No prior review artifact for round `016` forces a same-round retry.
- The accepted prototype-free stop from `RE4` (`not-yet-reopen`) and `RE5` (`remain-stop`) remains controlling, and the approved successor design admits only a bounded prototype-evidence lane that starts with `P1`.
- `P2`, `P3`, and `P4` all depend on `P1`, so no later stage is admissible before `P1` runs.

## Fixed Boundary

- Keep the active subject fixed to `URI-R2-C1` only.
- Keep the active scenario fixed to `uri-r2-c1-only-v1` only.
- Keep the investigation bounded to one local obligation SCC only.
- Keep ownership bounded to one `single-binder-family` only.
- Keep recursion obligation-level only; do not admit equi-recursive reasoning or implicit unfolding.
- Keep the structural graph non-cyclic; do not admit cyclic structural encoding.
- Keep prototype execution isolated behind the shared research entrypoint with selector-based staging only; do not admit a second executable interface or any default-path production behavior change.

## Round Outcome Target

The selected stage should establish whether the shared-entrypoint `P1` lane can produce exactly one canonical subject token for `uri-r2-c1-only-v1`, or else record the bounded `semantic-negative` or `inconclusive` result required by the roadmap without heuristic choice, widened search, or boundary drift.
