# Implementation Plan

> Note: For per-task verification, prefer `cabal test --test-show-details=direct --test-options='--match <pattern>'`.
> If `cabal test` cannot write its default build logs on your machine, prefix commands with `cabal --config-file=.cabal-config`.

- [x] 1. Remove obsolete plan files and references
  - Files: `scope_tracking_redesign_plan.txt`, `phase6.plan`, plus any refs
  - Steps:
    - Ensure `scope_tracking_redesign_plan.txt` and `phase6.plan` are deleted.
    - Remove any remaining references to those filenames.
  - **Verification:** `test ! -f scope_tracking_redesign_plan.txt && test ! -f phase6.plan && rg -n 'scope_tracking_redesign_plan\\.txt|phase6\\.plan' -S .` passes
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 2. Fix stale `gBinds` comment in presolution
  - Files: `src/MLF/Presolution.hs`
  - Steps:
    - Update the `applyExpansionEdgeTraced` comment to reference `cVarBounds`/`MLF.VarStore` (not `gBinds`).
  - **Verification:** `rg -n 'gBinds' src/MLF/Presolution.hs` returns no matches
  - _Requirements: 3.1_

- [x] 3. Fix stale “approximate I(r)” comment in presolution tests
  - Files: `test/PresolutionSpec.hs`
  - Steps:
    - Update the Phase 1 comment to reflect `EdgeTrace.etInterior` is exact in binding-edge mode.
  - **Verification:** `rg -n 'approximate I\\(r\\)|approx I\\(r\\)' test/PresolutionSpec.hs` returns no matches
  - _Requirements: 3.2_

- [x] 4. Update `paper_general_raise_plan.txt` to match current implementation
  - Files: `paper_general_raise_plan.txt`
  - Steps:
    - Replace legacy “current repo model is `tnVarLevel`/`gBinds`/`cGNodes`” phrasing with binding-edge wording.
    - Keep only the remaining follow-ups that are still true (e.g. further ω executor centralization).
  - **Verification:** `rg -n 'tnVarLevel|gBinds|RankAdjustment|cGNodes|GNodeId|GNode\\b' paper_general_raise_plan.txt` returns no matches
  - _Requirements: 1.2_

- [x] 5. Update `implementation_notes.md` to match current implementation
  - Files: `implementation_notes.md`
  - Steps:
    - Remove/replace sections claiming rank-adjustment via `tnVarLevel`/`gBinds`.
    - Update paper↔repo mapping table to reflect binding edges and the new stores.
  - **Verification:** `rg -n 'tnVarLevel|gBinds|RankAdjustment|cGNodes|GNodeId|GNode\\b' implementation_notes.md` returns no matches
  - _Requirements: 1.1_

- [x] 6. Update remaining top-level docs that mention the legacy model
  - Files: `roadmap.md`, `incompatibility_report.md`
  - Steps:
    - Replace legacy `GNode`/level-tree references with binding-edge/VarStore terminology.
  - **Verification:** `rg -n 'tnVarLevel|gBinds|RankAdjustment|cGNodes|GNodeId|GNode\\b' roadmap.md incompatibility_report.md` returns no matches
  - _Requirements: 1.3_

- [x] 7. Run repo-wide legacy-token check + full test suite
  - Steps:
    - Verify no legacy model tokens remain outside `papers/` and `merge_raise_merge_plan.txt`.
    - Run the full test suite.
  - **Verification:**
    - `rg -n 'GNode\\.gBinds|tnVarLevel|RankAdjustment|cGNodes|GNodeId|GNode\\b|gBinds' --glob '!papers/**' --glob '!merge_raise_merge_plan.txt' .` returns no matches
    - `cabal --config-file=.cabal-config test --test-show-details=direct` passes
  - _Requirements: 1.1, 1.2, 1.3, 3.1, 3.2, 4.1_
