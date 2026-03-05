# Goal Mechanism Table (Current vs Target)

Last updated (UTC): 2026-03-05
Goal: `<REPLACE_WITH_GOAL>`
Source of truth: `<REPLACE_WITH_PRIMARY_SPEC_OR_PAPER>`

Use this table to track what the codebase does now vs what it must do to satisfy the goal.

| Mechanism | Current codebase behavior | Target behavior | Gap summary | Evidence (spec/code/tests) | Gate (YES/NO) | Next action |
|---|---|---|---|---|---|---|
| `<Mechanism 1>` | `<What exists now>` | `<What must be true>` | `<Why current != target>` | `<spec section>; <file path>; <test>` | `NO` | `<Concrete implementation step>` |
| `<Mechanism 2>` | `<What exists now>` | `<What must be true>` | `<Why current != target>` | `<spec section>; <file path>; <test>` | `NO` | `<Concrete implementation step>` |
| `<Mechanism 3>` | `<What exists now>` | `<What must be true>` | `<Why current != target>` | `<spec section>; <file path>; <test>` | `NO` | `<Concrete implementation step>` |
| `<Mechanism 4>` | `<What exists now>` | `<What must be true>` | `<Why current != target>` | `<spec section>; <file path>; <test>` | `NO` | `<Concrete implementation step>` |

## Gate Rules

- A row is `YES` only when behavior is objectively aligned and required verification passes.
- Use only `YES` or `NO` (no `PARTIAL`, `N/A`, or prose values).
- Keep mechanism order fixed for orchestration rounds.

## Verification Cadence

- Mechanism-specific required checks
- Safety/regression checks
- Full gate: `cabal build all && cabal test`

## Notes

- Add dated evidence entries when a row flips `NO -> YES`.
- If a row regresses, flip it back to `NO` with failing evidence.
