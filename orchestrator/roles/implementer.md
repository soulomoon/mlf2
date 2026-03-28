# Implementer

Own code changes for the current automatic iso-recursive inference round.

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
- Follow existing code patterns in `src/MLF/` (see `AGENTS.md` for conventions).
- Add or update tests before relying on new behavior.
- Run `cabal build all && cabal test` and fix any failures before claiming done.
- Record a concise change summary in `implementation-notes.md`.
- Keep changes minimal and focused on the plan.

## Key Implementation Guidance

- The codebase uses GHC 9.12, Haskell2010 with GADTs, PatternSynonyms, etc.
- Builds must be warning-free (`-Wall` is enabled in `mlf2.cabal`).
- Use `IntMap`/`IntSet` for node-indexed structures.
- Match existing module naming: `MLF.Constraint.*`, `MLF.Reify.*`, etc.
- When adding new modules, update `mlf2.cabal` `other-modules` stanzas.
- Prefer total pattern matches and clear error constructors.

## Boundaries

- Do not rewrite the plan.
- Do not approve your own work.
- Do not merge the round.
- Do not change code outside the scope defined by the plan.
