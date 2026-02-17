# Task Plan: BUG-2026-02-16-009 OpRaise Non-Spine Context

## Objective
Fix BUG-2026-02-16-009 so explicit forall annotation on let-bound variables round-trips through elaboration, aligned with thesis Def. 15.3.4 non-spine raise context reconstruction.

## Scope
- Follow systematic-debugging phases (root cause first).
- Apply TDD: add/lock failing regression before implementation.
- Restrict code changes to the minimal root-cause fix in Phi/Omega context reconstruction path.
- Keep existing strict translatability checks intact unless evidence requires change.

## Phases
1. Root cause investigation: reproduce pinned failure and capture concrete failing path in Phi/Omega. (completed)
2. Pattern analysis: compare failing non-spine raise path with working spine/context paths and paper behavior. (completed)
3. Hypothesis + minimal validation: state one hypothesis and perform smallest proving change/test. (completed)
4. Implementation + verification: implement root-cause fix + run targeted and regression anchors. (completed)
5. Docs and tracker sync: update Bugs/TODO/implementation notes/task files and mark status. (completed)

## Decisions
- Use source-domain `OpRaise` node as fallback for non-spine context reconstruction when copy-map adopted target has no valid context.
- Keep copy-map adoption path in place when it can produce a valid context, to avoid regressing prior BUG-004 behavior.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| GHCi probe script parse error (`incorrect indentation or mismatched brackets`) while inspecting BUG-004-V2 witness path | 1 | Rewrote script with single-line `let` bindings and explicit helper definitions. |
| Missing `lookupBindParent`/`NodeId` imports in temporary GHCi probes | 1 | Added explicit imports (`MLF.Binding.Tree`, `MLF.Constraint.Types (NodeId(..))`) and reran probes. |
