# Code Findings A

1. **Row/topic:** Elaboration input  
   **Status:** partly  
   **Finding:** `eePresolutionView` is present, but snapshot finalization is still performed inline in `Pipeline.hs`; the table’s wording about centralization in `MLF.Elab.Run.PipelineBoundary` appears stale relative to current tree.  
   **Evidence:**
   - `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs:75`
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:88`
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:110`
   - `/Volumes/src/mlf4/test/PipelineSpec.hs:1169`
   - `/Volumes/src/mlf4/test/PipelineSpec.hs:1190`

2. **Row/topic:** Result-type context wiring  
   **Status:** partly  
   **Finding:** `ResultTypeInputs` remains active and checked type is still authoritative, but the row text mentioning `rtcSolved` is outdated (current bundle uses `rtcPresolutionView` and related fields instead).  
   **Evidence:**
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Types.hs:23`
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Types.hs:28`
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:182`
   - `/Volumes/src/mlf4/src/MLF/Elab/Run/Pipeline.hs:195`
   - `/Volumes/src/mlf4/test/ElaborationSpec.hs:191`

3. **Row/topic:** Per-edge propagation transform  
   **Status:** accurate  
   **Finding:** Implementation still has a synthesized-wrapper branch that forces `ExpIdentity` with direct body/target unification, while non-synth edges use minimal expansion + merge/unify path.  
   **Evidence:**
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:76`
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:78`
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs:89`
   - `/Volumes/src/mlf4/test/Presolution/EdgeInterpreterSpec.hs:74`
   - `/Volumes/src/mlf4/test/Presolution/EdgeInterpreterSpec.hs:105`

4. **Row/topic:** Replay-map producer normalization (upfront strict contract)  
   **Status:** accurate  
   **Finding:** Producer-side normalization/validation enforces replay-map completeness/injectivity/codomain constraints and driver re-validates replay-map contracts.  
   **Evidence:**
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs:501`
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessNorm.hs:542`
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/WitnessValidation.hs:86`
   - `/Volumes/src/mlf4/src/MLF/Constraint/Presolution/Driver.hs:158`
   - `/Volumes/src/mlf4/test/Presolution/WitnessSpec.hs:1267`

5. **Row/topic:** Replay-map consumer bridge in Phi  
   **Status:** accurate  
   **Finding:** `computeTraceBinderReplayBridge` exists with strict domain/codomain checks, and Phi/Omega still performs constrained non-root replay-alias recovery with fail-fast behavior when unresolved.  
   **Evidence:**
   - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs:511`
   - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs:557`
   - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:1331`
   - `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs:1396`
   - `/Volumes/src/mlf4/test/ElaborationSpec.hs:2354`
