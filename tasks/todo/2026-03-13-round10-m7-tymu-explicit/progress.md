# Progress

- 2026-03-13: Read repository guidance, required roadmap/design docs, and the current explicit-recursive annotation pipeline to scope the Round 10 M7 implementation slice.
- 2026-03-13: Added focused RED tests for recursive surface annotations, checked-authoritative reconstruction, and acyclicity; the first attempt to run them in parallel hit Cabal package-db collisions, so subsequent verification will be strictly sequential.
- 2026-03-13: Implemented the explicit-only acyclic `TyMu` path across graph construction, scope/planner traversal, structural unification, reification, and result-type reconstruction; removed the old Phase 1 `RecursiveAnnotationNotSupported` rejection for explicit `STMu` annotations only.
- 2026-03-13: Synced behavior docs in `CHANGELOG.md` and `implementation_notes.md` to record the M7 explicit-only lowering boundary.
- 2026-03-13: Verified the required checks sequentially:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "recursive surface annotations"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "annotation-heavy path still reports checked-authoritative type"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Acyclicity Check"'`
  - `cabal build all && cabal test`
