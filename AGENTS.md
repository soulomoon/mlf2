# Repository Guidelines

## Project Structure & Module Organization

- `src/` contains the `mlf2` library. Most logic lives in `src/MLF/` and roughly follows the pipeline phases: constraint generation → normalization → acyclicity → presolution → solving → elaboration.
- `src/MyLib.hs` is the library’s public entry point (re-exports common types and helpers).
- `app/` contains the executable entry point (`app/Main.hs`) for the `mlf2` binary.
- `test/` contains the Hspec suite (`*Spec.hs`) and a manual test runner (`test/Main.hs`).
- `papers/` holds reference material (PDF/TXT) used to align the implementation with the xMLF/MLF papers; it is not required to build.

## Build, Test, and Development Commands

- `cabal build` — build the library and executable into `dist-newstyle/`.
- `cabal test` — run the `mlf2-test` suite (Hspec).
- `cabal test --test-show-details=direct` — rerun tests with per-example output (useful when debugging failures).
- `cabal run mlf2` — run the demo executable.
- `cabal repl mlf2` / `cabal repl mlf2-test` — open GHCi with the chosen target loaded.

## Coding Style & Naming Conventions

- Match existing formatting: 4-space indentation, explicit module export lists, and GHC-style `{- Note [...] -}` blocks for design rationale.
- Keep builds warning-free (`-Wall` is enabled in `mlf2.cabal`). Prefer total pattern matches and clear error constructors.
- Naming conventions:
  - Modules: `src/MLF/Foo.hs` defines `module MLF.Foo`.
  - Tests: `test/FooSpec.hs` defines `spec :: Spec`.

## Testing Guidelines

- Framework: Hspec (`hspec`).
- When adding a new spec module, wire it into both:
  - `mlf2.cabal` → `test-suite mlf2-test` → `other-modules`
  - `test/Main.hs` (import the module and call `spec`)

## Commit & Pull Request Guidelines

- Commit messages typically use imperative, descriptive subjects (examples from history: `Add …`, `Improve …`, `Phase6: …`).
- PRs should include: a short problem statement, approach summary, new/updated tests, and any relevant doc updates (`implementation_notes.md`, `roadmap.md`) when changing algorithmic behavior.
