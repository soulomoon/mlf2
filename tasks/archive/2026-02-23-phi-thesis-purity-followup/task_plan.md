# Task Plan: 2026-02-23 Phi Thesis Purity Follow-up

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-02-23-phi-thesis-purity-followup-implementation-plan.md` in an isolated worktree, with thesis-faithfulness and regression safety.

## Scope
- Register deviations (items 4/5/6)
- Remove 3-op fusion (`mergeIntoApp`)
- Move bottom rescue into reification layer
- Replace bound-match `InstElim` with `InstApp boundTy`
- Add/adjust tests and docs

## Phases
- [x] Phase 0: Setup isolated worktree + task files
- [x] Phase 1: Task 1 (deviation registration + claim linking)
- [x] Phase 2: Task 2 (add failing tests)
- [x] Phase 3: Task 3 (remove fusion)
- [x] Phase 4: Task 4 (reification rescue move)
- [x] Phase 5: Task 5 (bound-match emission switch)
- [x] Phase 6: Task 6/7 (docs + full verification + optional phi soundness)

## Constraints
- Maintain thesis-faithful behavior relative to `papers/these-finale-english.txt`
- Preserve load-bearing behavior where noted
- Keep claims/deviation checker green
- Run explicit verification commands before completion claims

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---:|---|
| 2026-02-23 | plan file missing in fresh worktree | 1 | Copied plan from source workspace into task folder |
| 2026-02-23 | de-fused `OpGraft;OpRaise;OpWeaken` produced `a -> ⊥` regression (`\\y. let id = (\\x.x) in id y`) | 1 | Added targeted `normalizeInst` rule for the left-associated de-fused sequence to canonicalize to `InstApp` when arg forms match |
| 2026-02-23 | temporary stash-verify script used reserved zsh variable name `status` and aborted before pop | 1 | Restored with explicit `git stash pop stash@{0}` and switched follow-up scripts to `set -e` without reserved variable assignments |

## Error Addendum
- Running multiple `cabal test` commands in parallel caused build artifact races in `dist-newstyle` (missing/rename collisions). Mitigation: run test slices serially for this task.
