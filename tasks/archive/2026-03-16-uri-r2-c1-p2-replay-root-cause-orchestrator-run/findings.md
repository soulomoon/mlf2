# Findings

- The live control plane is now lawful to run again because the earlier prototype-evidence roadmap is no longer the live roadmap; it is predecessor evidence for the new bounded `D1` through `D4` replay root-cause track.
- The authoritative inherited failure boundary remains narrow:
  - `P1 = pass`
  - `P2 = semantic-negative`
  - `P2-W` failed with `partial-replay`
  - observed mismatch: `InstBot expects ⊥, got: t9 -> t9`
- The live retry-subloop contract applies to `D1`, `D2`, and `D3`, with `D4` remaining terminal.
- The first lawful stage for the resumed controller is `D1`, replay-failure reproduction.
- `D1` completed as an authoritative `pass`: the successor track reproduced the same bounded replay-failure continuity under the locked `URI-R2-C1` / `uri-r2-c1-only-v1` lane without widening scope.
- `D2` completed as an authoritative `pass`: the replay divergence is now localized to `witness-replay/applyInstantiation-instbot-precondition`, with owner account assigned to `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- `D3` completed as an authoritative `pass`: one bounded repair-supporting direction exists without widening, namely aligning the `InstBot` precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized `applyInstantiation` boundary.
- `D4` completed as an authoritative `pass` with final outcome `reopen-repair-track`.
- The successor roadmap therefore did not repair production behavior directly; it established that a separate bounded repair-track roadmap is now justified and should stay locked to the localized `applyInstantiation` / `InstBot` replay boundary.
