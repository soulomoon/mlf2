# Findings

- The live prototype-evidence campaign starts from `orchestrator/state.json` with `active_round_id = null`, `stage = "select-task"`, and `last_completed_round = "round-015"`.
- The live roadmap contains four pending items in strict order: `P1` through `P4`.
- The approved design source for the campaign is `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`.
- The accepted prototype-free predecessor stop remains authoritative through `docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md` (`not-yet-reopen`) and `docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md` (`remain-stop`).
- The current repo worktree is clean on branch `codex/automatic-recursive-type-inference`, while the linked root worktree `/Volumes/src/mlf4` remains on `master`.
- Existing historical round branches/worktrees `round-006` through `round-015` and the preserved `codex/rt-r06-m5-surface-mu` artifact still exist; the new controller run must avoid disturbing them except where the control contract explicitly permits inherited continuity references.
- `round-016` is the next legal round id. The controller successfully registered it in `orchestrator/state.json` and recovered an initial worktree-creation race by reusing the precreated branch `codex/round-016`.
- The resumed guider completed `select-task` successfully on `codex/round-016`: round `016` is anchored to roadmap item 1, the bounded `P1` subject-discovery prototype for `URI-R2-C1`.
- `round-016` is now complete on base branch `codex/automatic-recursive-type-inference`: the roadmap marks `P1` done, and the base branch head advanced through round-content commit `7e3fbd9fbbd04f7bd8ce9528178b5738f6dff49a` plus control-plane commit `e4db3862f5596440ca0bce5a95ab158d0b18f55d`.
- `round-017` is the current live round. Its first `P2` implementation failed review because `P2.hs` synthesized pass evidence instead of executing the required bounded pipeline operations and also touched `src/MLF/Research/URI/R2/C1/Prototype/P1.hs` outside the approved narrow slice.
