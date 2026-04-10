# Repository Guidelines

## Purpose & Precedence

- `AGENTS.md` is the concise entry point for how to work in this repo. Keep it accurate and synchronized with adjacent guidance surfaces when their contract changes.
- Before substantial work, review this file and update it if it is stale, contradictory, or missing an important workflow constraint.
- Apply instruction precedence in this order: direct system/developer/user instructions, deeper nested `AGENTS.md` files, this file, then supporting project docs.
- Keep durable workflow and policy rules here. Put repo layout, module ownership, and type/module reference detail in `docs/architecture.md`.

## Core Project Invariants

- Source of truth: `papers/these-finale-english.txt`.
- Prefer thesis faithfulness over code convenience. Document and test any intentional deviation.
- Fix root causes rather than adding compatibility layers, convenience fallbacks, or migration shims unless the paper-backed design requires them.
- Backwards compatibility is not a default goal; the priority is a clean, paper-faithful implementation.
- Guidance-only cleanups belong in docs/changelog, not `Bugs.md`, unless they reveal a real implementation defect or thesis-faithfulness gap.

## Working Norms

- Update tests and relevant docs in the same change whenever behavior, architecture, or thesis-alignment expectations change.
- Prefer focused validation while iterating, but run `cabal build all && cabal test` before claiming behavior-changing work is complete. Pure guidance/docs edits do not require the full gate.
- Prefer the narrowest test slice that covers the change before expanding to the full gate.
- Standard local commands are `cabal build`, `cabal test`, `cabal repl mlf2`, `cabal repl mlf2-test`, `cabal run mlf2`, and `cabal run frozen-parity-gen -- --generated-on YYYY-MM-DD --source-commit <sha>`.
- Use `haskell-pro` (`/Users/ares/.agents/skills/haskell-pro/SKILL.md`) as the default Haskell style guide.
- Match existing formatting: 4-space indentation, explicit module export lists, and GHC-style `{- Note [...] -}` blocks when design rationale needs to stay close to code.
- Keep builds warning-free (`-Wall` is enabled in `mlf2.cabal`).
- When adding modules under `src/`, `src-public/`, or `test/`, update the corresponding `mlf2.cabal` stanzas.
- When adding a new spec module, wire it into both `mlf2.cabal` and `test/Main.hs`.
- Keep the production surface narrow: if tests need low-level internal helpers, add a test-support seam rather than widening a production facade.
- Commit messages should be imperative and descriptive. Update `CHANGELOG.md` only when the change records meaningful project progress rather than pure wording cleanup.
- When using `spawn_agent`, use the builtin subagent with `model: "gpt-5.4"` and `reasoning_effort: "xhigh"` unless the user explicitly requests something else.

## Guidance Ownership Map

- `docs/architecture.md`: repo layout, public/internal boundaries, module ownership, and key shared abstractions.
- `tasks/readme`: task-folder quick start.
- `TODO.md`: rolling next goals.
- `implementation_notes.md`: dated behavior, architecture, and thesis-alignment notes.
- `CHANGELOG.md`: meaningful project progress.
- `Bugs.md`: canonical bug tracker.
- `README.md`: user-facing build and usage guidance.
