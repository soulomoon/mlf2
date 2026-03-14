# Squash Commit Title

`Deliver P2 authoritative attempt-2 provenance-preservation result for URI-R2-C1`

# Summary

Round `017` is ready for squash merge.

This round delivers `P2` only for `URI-R2-C1`, promoting authoritative attempt `2` after review approval. The accepted result is a bounded `semantic-negative` for `P2` under the shared research entrypoint `uri-r2-c1-prototype-entrypoint-v1`, with the fixed scenario `uri-r2-c1-only-v1`, preserved rejected `attempt-1` audit history, and authoritative `attempt-2` evidence showing that replay fails honestly as `partial-replay` instead of being misclassified as a pass.

The merged result preserves prototype isolation and inherited continuity. It does not widen into `P3` or `P4`, does not alter the default executable path, does not add a second entrypoint or public API surface, and does not rewrite the accepted `RE4` `not-yet-reopen` / `RE5` `remain-stop` history that still controls the predecessor track.

# Follow-Up Notes

- Keep `P1` attempt `2` from round `016` as the authoritative carried input and keep `P2` attempt `2` from this round as the authoritative bounded non-pass result.
- Preserve `P2` `attempt-1` as rejected historical evidence only; later stages must consume the reviewer-authoritative `P2` result from `review-record.json`.
- Future rounds may advance only to `P3` and then `P4` under the same bounded `URI-R2-C1` / `uri-r2-c1-only-v1` prototype lane and inherited isolation constraints.

# Continuity Note

This round inherits the accepted prototype-free control record, including `RE4` `not-yet-reopen`, `RE5` `remain-stop`, the successor roadmap design, and the authoritative `P1` record from round `016`, without reopening or rewriting them. Round `017` contributes only the bounded `P2` authoritative attempt `2` provenance result and preserves both shared-entrypoint isolation and inherited continuity for later stage-gated rounds.

# Readiness Statement

Review recorded `approve`, `review-record.json` marks `P1` authoritative from round `016` and `P2` authoritative at attempt `2` with result `semantic-negative`, and later stages remain `not-yet-run`. The approved round stays inside the shared-entrypoint, single-subject, bounded-scenario contract. `round-017` is ready for squash merge.
