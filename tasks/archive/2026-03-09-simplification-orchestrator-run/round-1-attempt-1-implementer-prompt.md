You are the Implementer agent for round 1 attempt 1 in /Volumes/src/mlf4.

Hard requirements:
- Use the required skills that apply, especially `using-superpowers`, `using-git-worktrees`, and any implementation workflow skill you need.
- You must not touch the main workspace. Work only in an isolated detached worktree at `~/.config/superpowers/worktrees/mlf4/round-1-scratch`.
- Preserve thesis-exact behavior from `papers/these-finale-english.txt`; use `papers/xmlf.txt` only if the thesis is silent.
- Do not commit. Do not merge. Do not run the full validation gate beyond lightweight sanity if needed.
- Own only these files unless a tightly-related change is unavoidable: `/Volumes/src/mlf4/src/MLF/Constraint/Presolution.hs`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Core.hs`, `/Volumes/src/mlf4/mlf2.cabal`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeUnify.hs`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Ops.hs`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/ForallIntro.hs`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Expansion.hs`, `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Witness.hs`, `/Volumes/src/mlf4/implementation_notes.md`, `/Volumes/src/mlf4/CHANGELOG.md`, `/Volumes/src/mlf4/TODO.md`.
- You are not alone in the codebase: do not revert unrelated edits.

Plan to execute:
1. Confirm the seam is still bounded with a fresh repo search for live `MLF.Constraint.Presolution.Core` references.
2. Edit `MLF.Constraint.Presolution` to import the underlying implementation modules directly and keep the exported symbol list unchanged.
3. Delete `Presolution/Core.hs` only after `MLF.Constraint.Presolution` is self-sufficient.
4. Remove `MLF.Constraint.Presolution.Core` from `mlf2.cabal`.
5. Update stale module-header/documentation text in the extracted presolution submodules if they still describe a live `Core` facade.
6. Update `implementation_notes.md` and `CHANGELOG.md`; update `TODO.md` only if priorities materially change.

Return JSON matching the schema with:
- `feasible`: YES or NO
- `meaningful_diff`: YES or NO
- `blockers`
- `files_changed`
- `change_summary`
Include the isolated worktree path inside `change_summary`.
