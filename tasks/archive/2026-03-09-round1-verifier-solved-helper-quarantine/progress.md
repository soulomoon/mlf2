# Progress — Round 1 verifier

- Initialized task folder and planning files.
- Ran repo-wide search for the six proposed helper names.
- Inspected `src/MLF/Constraint/Solved/Internal.hs`, `test/SolvedFacadeTestUtil.hs`, and `test/Constraint/SolvedSpec.hs` at a high level.
- Confirmed the duplicate helper bundle exists in both `src/MLF/Constraint/Solved/Internal.hs` and `test/SolvedFacadeTestUtil.hs`.
- Confirmed tests call the utility module rather than importing the internal definitions directly.
- Checked `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` for Task 67 and found they describe the helper bundle as already quarantined behind the test utility.
- Identified the remaining non-test `MLF.Constraint.Solved.Internal` import sites to verify whether they still need the six helper names.
- Ran the proposed boundary test command; Hspec accepted the invocation but matched `0 examples`, so the current command is not a reliable targeted gate as written.
- Reran the exact helper-name grep and confirmed the duplicate bundle still exists in `src/MLF/Constraint/Solved/Internal.hs` alongside the test utility.
- Verified targeted Hspec runs: exact facade-absence example passes (`1 example, 0 failures`), and `MLF.Constraint.Solved` passes (`51 examples, 0 failures`).
- Reproduced the baseline build break with `cabal build mlf2-test`; GHC reports `Solved.fromPreRewriteState` missing from the public facade at `test/Parity/FrozenArtifacts.hs:128` and `test/SpecUtil.hs:210,223`.
- Verified `MLF.Constraint.Finalize` is exposed to tests via the internal library in `mlf2.cabal`, so tests can switch to owner-local finalize helpers without re-expanding the public facade.
- Compared implementations and found `SolvedFacadeTestUtil.solvedFromSnapshot` is lighter than `Solved.fromPreRewriteState`; it does not run eliminated-binder rewrite, UF substitution repair, bind-parent pruning, or final validation.
