# Findings

- The authoritative `P2` stage result is the narrowest live failure boundary: the bounded pipeline reaches generalization, scheme conversion, and no-fallback reification, then fails in witness replay with `partial-replay` / `InstBot expects ⊥, got: t9 -> t9`.
- `P3` and `P4` are downstream consequences of that `P2` failure, not primary root causes. A successor control plane should therefore target the replay mismatch directly.
- The new track should stay bounded to the exact historical subject and scenario:
  - `subject_id` from round-016 authoritative `P1`;
  - `scenario_id = uri-r2-c1-only-v1`;
  - no widened subject search, no new subset, no production implementation.
- The retry-subloop contract is most useful on three stages here:
  - failure reproduction,
  - mismatch localization,
  - bounded fixability probe.
- The terminal stage should not retry semantically. It should decide whether the evidence justifies a separate repair track or whether the earlier `remain-stop` boundary still stands.
- The live control plane is now scaffolded accordingly:
  - roadmap items are `D1` through `D4`,
  - the shared research entrypoint is `uri-r2-c1-p2-replay-root-cause-v1`,
  - the live retry-eligible stages are `D1`, `D2`, and `D3`,
  - the terminal outcomes are `reopen-repair-track` or `remain-stop`.
