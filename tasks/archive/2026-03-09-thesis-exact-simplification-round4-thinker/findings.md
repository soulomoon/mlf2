# Findings

- The current working tree is not clean: unrelated tracked edits exist in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`; this thinker pass should avoid those files.
- `TODO.md` shows simplification tasks 66–70 already completed on 2026-03-08, centered on shrinking the `Solved` facade and removing fallback machinery; new proposals should avoid reopening those completed themes unless a distinct owner-local seam remains.
- The repo already contains dedicated round-2 and round-3 thinker task folders under `tasks/todo/2026-03-09-*`; round 4 needs a fresh, non-duplicative simplification candidate.
- `implementation_notes.md` confirms several simplifications have already landed on 2026-03-09, including the accepted `WithCanonicalT` retirement and earlier presolution facade cleanup, so round 4 must find a different live seam.
- `CHANGELOG.md` / `implementation_notes.md` continue to emphasize single-owner boundaries (for example `schemeBodyTarget` living in `MLF.Elab.Run.Scope`) and removal of compatibility surfaces, which is a useful filter for candidate simplifications.
- `Bugs.md` currently shows no open bug entries, so the best proposal is likely a bounded owner-boundary or dead-wrapper cleanup rather than an active defect repair.
- `src/MLF/Elab/Run/ResultType/View.hs` still exports `rtvSchemeBodyTarget`, but `rg -n '\brtvSchemeBodyTarget\b' src test` shows it is defined/exported only in that module and has no callers elsewhere.
- `src/MLF/Elab/Run/Scope.hs` already documents `schemeBodyTarget` as the owner-local implementation of the thesis `S′`-style target-selection rule, and all live result-type/phi/elaboration call sites import `schemeBodyTarget` directly from `MLF.Elab.Run.Scope`.
- This makes `rtvSchemeBodyTarget` a dead forwarding wrapper that conflicts with the documented single-owner boundary for scheme-target selection while carrying no live thesis behavior itself.
