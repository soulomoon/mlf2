# Findings — Roadmap/codebase sync

- Initial finding: `roadmap.md` already describes the thesis-faithful pipeline, but may lag the current cleanup/stabilization status recorded in `TODO.md`.
- `roadmap.md` references `.kiro/specs/...` for known deviations, but `.kiro/specs` does not exist in this repo anymore; the live deviation register is `docs/thesis-deviations.yaml`, with broader paper-to-code mapping in `docs/paper-map.md`.
- The roadmap’s module blueprint is broadly right, but it under-describes the current public API split: `MLF.API` for surface syntax/parsing/pretty-printing, `MLF.Pipeline` for normalized inference/elaboration/runtime helpers, and `MLF.XMLF` for xMLF syntax tooling.
- The implementation has a sharper internal split than the roadmap currently shows: `MLF.Elab.Pipeline` is mostly a facade over `MLF.Elab.Run`, `MLF.Elab.Phi`, `MLF.Elab.Sigma`, `MLF.Reify.Core`, `MLF.Elab.TypeCheck`, and `MLF.Elab.Reduce`.
- Phase 3 (`MLF.Constraint.Acyclicity`) and Phase 4 (`MLF.Constraint.Presolution`) remain live modules and match the roadmap, but the codebase also has stable ownership helpers around canonicalization/finalization (`MLF.Constraint.Canonicalizer`, `MLF.Constraint.Finalize`) that are worth naming in a codebase-fit roadmap.
- `TODO.md` shows the current active work is no longer feature construction but cleanup/stabilization: dead-export retirement, warning-free rebuilds, and keeping strict/paper-faithful boundaries green.
