# Doc Audit Findings B

1. **Row/topic:** Header metadata (`Source revision`)  
   **Status:** outdated  
   **Evidence:** `/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md:4` lists `165e2bf`, but current branch head is `cfe7e8a...` at `/Volumes/src/mlf4/.git/refs/heads/master:1`.

2. **Row/topic:** Graph mutation during solve/presolution  
   **Status:** outdated  
   **Evidence:** `/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md:19` says snapshot replay/finalization is centralized in `MLF.Elab.Run.PipelineBoundary`, but current production code performs finalization directly in `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:88` and `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:110`.

3. **Row/topic:** Dual-path verification mechanism  
   **Status:** partly  
   **Evidence:** `/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md:20` is accurate about single-path production (`/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:75`), but "legacy parity only as frozen artifacts" is too narrow because parity-style checks also exist in `/Volumes/src/mlf4/test/PipelineSpec.hs:175` and `/Volumes/src/mlf4/test/PipelineSpec.hs:247` (in addition to `/Volumes/src/mlf4/test/FrozenParitySpec.hs:13`).

4. **Row/topic:** Canonicalization source used by Phi  
   **Status:** accurate  
   **Evidence:** `/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md:16`; bridge/domain checks use raw trace keyspace in `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs:519` and identity canonical key helpers in `/Volumes/src/mlf4/src/MLF/Elab/Phi/IdentityBridge.hs:69`.

5. **Row/topic:** Non-root weaken/raise binder resolution  
   **Status:** accurate  
   **Evidence:** `/Volumes/src/mlf4/docs/notes/2026-02-27-transformation-mechanism-table.md:18`; non-root `OpWeaken` fail-fast/recovery path at `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:1396`, and unresolved trace-source `OpRaise` invariant failure at `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:856`.
