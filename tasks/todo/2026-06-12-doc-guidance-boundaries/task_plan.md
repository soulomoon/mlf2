# Documentation Guidance Boundaries Task Plan

Goal: add a small canonical documentation map and ownership boundary layer
without moving historical plans, task packets, or orchestrator artifacts.

## Plan

1. Add a reader-facing `docs/README.md` map.
2. Update `AGENTS.md`, `README.md`, and `tasks/readme` to point at the map and
   preserve existing owner boundaries.
3. Record resolved doc-boundary terms in `CONTEXT.md`.
4. Add an ADR for preserving historical documentation in place behind canonical
   guidance boundaries.
5. Add and run a lightweight documentation-map check.

## Out Of Scope

- Moving `docs/plans/`, `tasks/archive/`, or orchestrator history.
- Rewriting historical links.
- Reclassifying every historical plan or task packet.
- Changing Haskell source, tests, Cabal stanzas, or runtime behavior.
