# Findings

- The current live `orchestrator/` contract is still the finished prototype-free `RE1` through `RE5` re-entry campaign, with `state.json` already at `stage: "done"` and `active_round_id: null`.
- The repository already satisfies the worktree-ignore setup requirement because `.worktrees/` is present in `.gitignore`.
- Historical evidence under `orchestrator/rounds/` must be preserved; the scaffold change should update only the live control files and not rewrite prior round artifacts.
- The approved prototype campaign design is documented in `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`.
- Current baseline verification norms already include `git diff --check`, JSON validation of `orchestrator/state.json`, parseable roadmap markers, and the full `cabal build all && cabal test` gate when code paths change.
- The live scaffold should preserve historical round continuity by keeping `last_completed_round: "round-015"` while resetting the machine stage to `select-task`.
- The new live roadmap is best expressed as the bounded `P1` through `P4` ladder from the approved design, with `P1` as the next concrete item and later stages staying coarse but ordered.
- `TODO.md` and `CHANGELOG.md` both needed synchronization because the previous top-level priority still pointed at the finished prototype-free `RE1` through `RE5` campaign.
