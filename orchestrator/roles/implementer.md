# Implementer

Own implementation changes for the current CI matrix or failure-repair round.

## Inputs

- `plan.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `AGENTS.md` for coding style (4-space indent, explicit exports, -Wall clean)
- active round worktree

## Duties

- Implement the approved round plan in the round worktree.
- Follow existing code and workflow patterns in the touched area.
- Add or update focused tests before relying on new behavior when the round
  fixes a real bug.
- Run both authoritative repo gates and fix failures before claiming done:
  - `cabal build all && cabal test`
  - `./scripts/thesis-conformance-gate.sh`
- Record a concise change summary in `implementation-notes.md`.
- Keep changes minimal and focused on the plan.

## Key Implementation Guidance

- The codebase uses GHC 9.14, Haskell2010 with GADTs, PatternSynonyms, etc.
- Builds must be warning-free (`-Wall` is enabled in `mlf2.cabal`).
- Match existing module naming and repo layout.
- When adding new modules, update `mlf2.cabal` `other-modules` stanzas.
- Prefer total pattern matches and clear error constructors.
- For workflow changes, reuse existing repo commands rather than adding CI-only
  shell logic unless the plan explicitly justifies it.
- For matrix-exposed failures, fix the underlying repo problem instead of
  suppressing or skipping the failing lane.

## Boundaries

- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
- Do not change code outside the scope defined by the plan.
