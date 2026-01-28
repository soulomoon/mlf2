# Tasks

- [x] 1. Add ConstraintGenSpec test for REQ-1 (annotated let-bound var has a single TyExp) in `test/ConstraintGenSpec.hs`. (REQ-1)
- [x] 2. Add ElaborationSpec test for REQ-2 (explicit forall annotation round-trips) in `test/ElaborationSpec.hs`. (REQ-2)
- [x] 3. Add ElaborationSpec test for REQ-3 (forall-in-bound preserved) in `test/ElaborationSpec.hs`. (REQ-3)
- [x] 4. Run targeted Hspec selection: `cabal test --test-show-details=direct --test-option=--match --test-option="explicit forall annotation"`. (REQ-1, REQ-2, REQ-3)
