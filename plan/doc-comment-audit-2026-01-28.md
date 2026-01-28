# Doc/Comment Audit Log (2026-01-28)

This log tracks the parallel doc/comment audit against `papers/these-finale-english.txt`
(primary) and the current implementation. Each entry records the thesis anchor,
implementation anchor, and action.

## Agent A — Binding Tree & Binding Ops (src/MLF/Binding/**)

- Action: Updated semantics note to match thesis on rigid edges and Raise legality.
  - Thesis anchor: §3.4 (binding edges, Raise/Weaken) + §5.2.3 (nodes with rigid edge).
  - Implementation anchor: `src/MLF/Binding/GraphOps.hs` (Note [Instantiable vs Locked Nodes]).
  - Rationale: Implementation allows Raise on restricted nodes but forbids operations under
    rigid ancestors; prior note incorrectly stated Raise only applies to instantiable nodes.
- Status: Line-by-line audit complete; no further outdated comments found in `src/MLF/Binding/**`.

## Agent B — Constraint Core (src/MLF/Constraint/**)

- Action: Updated OpRaise comment to match current presolution behavior.
  - Thesis anchor: §3.4 (Fig. 10) instantiation witnesses / Raise translation.
  - Implementation anchor: `src/MLF/Constraint/Types.hs` (`InstanceOp`/`OpRaise` docs).
  - Rationale: Presolution now records Raise steps for interior nodes (not just binders);
    prior comment claimed interior raises were not emitted.
- Status: Line-by-line audit complete; no further outdated comments found in `src/MLF/Constraint/**`.

## Agent C — Witness + Elaboration (src/MLF/Witness/**, src/MLF/Elab/**)

- Action: Removed stale “Phase 2” scope note to make the module rationale timeless.
  - Thesis anchor: §15.3 (witness translation).
  - Implementation anchor: `src/MLF/Elab/Phi.hs` (module header note).
- Status: Line-by-line audit complete; no further outdated comments found in `src/MLF/Witness/**` or `src/MLF/Elab/**`.

## Agent D — Frontend/Public API/Pipeline (src/MLF/Frontend/**, src-public/**, app/**)

- Status: Line-by-line audit complete; no outdated comments found in frontend/public/pipeline modules.

## Agent E — Docs/Plans/Notes

- Status: Line-by-line audit complete for `implementation_notes.md`, `docs/**`,
  `plans/**`, and `plan/**`; no outdated statements found relative to the current
  implementation or thesis.

---

## Second-pass scope (additional thesis sections/modules)

### Agent F — Presolution witnesses & ω execution
Scope: `src/MLF/Constraint/Presolution/Witness.hs`, `src/MLF/Witness/OmegaExec.hs`  
Thesis anchors: §15.2–§15.3 (normalized witnesses; Φ/Σ translation).  
Status: Line-by-line audit complete; no outdated comments found.

### Agent G — Inert/rigidity rules
Scope: `src/MLF/Constraint/Inert.hs`, `src/MLF/Binding/GraphOps.hs`  
Thesis anchors: §5.2.1–§5.2.3 (inert nodes; rigid edges).  
Status: Line-by-line audit complete; notes already updated in the first pass.

### Agent H — Instantiation contexts & application
Scope: `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/Types.hs`, `src/MLF/Elab/Phi/Context.hs`  
Thesis anchors: §3.4 (instantiation grammar/contexts), §15.3 (Φ translation).  
Status: Line-by-line audit complete; no outdated comments found.
