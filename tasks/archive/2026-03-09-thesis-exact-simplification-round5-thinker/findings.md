# Findings

- The working tree already contains unrelated tracked edits in `src/MLF/Constraint/Solved.hs` and `test/SolvedFacadeTestUtil.hs`; this thinker pass should avoid those files.
- `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` confirm the recent simplification wave already retired the solved-helper quarantine theme, the `Reify.Core` `solvedFromView` round-trip theme, the `WithCanonicalT` reader layer, and the dead `rtvSchemeBodyTarget` wrapper theme; round 5 needs a different seam.
- `Bugs.md` currently shows only resolved bugs, so the best remaining candidate is a bounded internal simplification rather than an active defect repair.
- `src/MLF/Elab/Run/Scope.hs` contains a note titled `ga′ scope selection — Def. 15.3.2 alignment` that explicitly describes `preferGenScope` as re-running the same lookup as `bindingScopeRef` on the same constraint and labels it "redundant but harmless".
- The same note claims `resolveCanonicalScope` is a "3-step pipeline" while enumerating four steps only because `preferGenScope` sits between `bindingScopeRef` and `canonicalizeScopeRef`.
- Production references show `preferGenScope` is only consumed by `resolveCanonicalScope`; all other references are its export and one dedicated `ScopeSpec` regression.
- `bindingScopeRef` already computes the nearest gen ancestor (or falls back to `TypeRef root`) directly from `Binding.bindingPathToRoot`, so a second pass over the identical path cannot strengthen thesis behavior.
- Git history reinforces the redundancy: `git blame` shows the current note calling `preferGenScope` redundant predates the 2026-03-05 error-propagation hardening that merely stopped the helper from swallowing binding-tree failures.
- The bounded simplification still needed is therefore to retire `preferGenScope` entirely and make `resolveCanonicalScope` canonicalize the single authoritative `bindingScopeRef` result directly.
