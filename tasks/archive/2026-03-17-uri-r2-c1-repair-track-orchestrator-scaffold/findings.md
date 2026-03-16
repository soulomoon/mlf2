# Findings

- The completed `D1` through `D4` diagnostic track is no longer live work. It is predecessor evidence for the new repair-track scaffold.
- The accepted controlling defect remains `BUG-2026-03-16-001`:
  - localized boundary: `witness-replay/applyInstantiation-instbot-precondition`
  - owner account: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
  - observed mismatch: `InstBot expects ⊥, got: t9 -> t9`
- `D3` justified one bounded repair direction without widening:
  - align `InstBot` precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized boundary
- `D4` finalized the successor-track outcome as `reopen-repair-track`, which justifies a new bounded repair roadmap but does not itself authorize broad implementation or regression expansion.
- The new live scaffold should therefore:
  - preserve `URI-R2-C1` and `uri-r2-c1-only-v1`
  - keep `R1` through `R3` retry-eligible under `contract_version: 2`
  - keep `R4` terminal
  - avoid introducing a second executable interface or convenience fallback path
