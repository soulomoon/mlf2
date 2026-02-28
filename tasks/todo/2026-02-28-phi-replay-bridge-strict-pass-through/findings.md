# Findings: Phi Replay Bridge Strict Pass-Through

## Session Findings
- Plan requires replacing runtime fallback projection in `computeTraceBinderReplayBridge` with strict validation + pass-through.
- Existing domain parity check must remain unchanged.
- New regression should assert fail-fast when replay codomain points outside replay binder domain.
- `Translate.hs` still had a projection-era helper name (`projectOne`) even though fallback mapping logic was already removed; renamed to `validateTarget` and retained strict pass-through semantics.
- Replay-bridge invariant grep is now clean for removed helpers/patterns:
  - no `projectReplayTarget`, `projectOne`, `sourceNameByKey`, `replayKeyByName`, `positionalReplayMap`, `defaultReplayTarget`.
- Full gate initially failed only on stale test message prefixes after strict error-string cleanup; behavior was correct and resolved by expectation updates.
- Required gate `cabal build all && cabal test` is green (`894 examples, 0 failures`).
