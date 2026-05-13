# mlf2 Thesis Pipeline

Domain language for the paper-faithful MLF to xMLF implementation and its checked `.mlfp` pipeline.

## Language

**Acyclic Base Constraint**:
The post-acyclicity constraint graph that preserves original binding-tree ownership for thesis ga-prime recovery.
_Avoid_: raw graph, old constraint

**Presolution View**:
The canonical read model built from a presolution or solved snapshot for elaboration and reification.
_Avoid_: solved wrapper, query bag, chi-query adapter

**Snapshot Finalization**:
The construction authority that turns a constraint snapshot plus canonical equivalence data into a **Presolution View** and/or solved handle. `MLF.Constraint.Finalize` owns this construction; `MLF.Constraint.Presolution.View` owns the read-model surface.
_Avoid_: snapshot materialization, view setup, solved wrapper construction

**Legacy Surface Retirement**:
The cleanup direction that deletes outdated compatibility surfaces, including internal solved/view adapters, parser aliases, and parser support for non-canonical legacy syntax, when the paper-backed pipeline no longer needs them.
_Avoid_: compatibility preservation, syntax grandfathering, parser transition mode, ASCII alias mode

**Generalization Preparation**:
The elaboration-side alignment step that turns presolution outputs, the **Acyclic Base Constraint**, and annotated terms into the shared generalization input.
_Avoid_: pipeline glue, setup tuple

**Prepared Generalization Artifact**:
The single artifact produced by **Generalization Preparation** and consumed by elaboration, result-type reconstruction, and root-scheme generalization.
_Avoid_: constraint tuple, prep bag

**Backend Structural Recursive Data Matching**:
The backend-owned decision that a structural recursive type represents the same exact canonical backend data identity and constructor payload shape as a nominal backend data type.
_Avoid_: source-local recovery, unqualified data fallback

**Backend Structural Recursive Data Match**:
The evidence produced by **Backend Structural Recursive Data Matching**, carrying the canonical data identity and recovered payload information needed by backend validation, conversion, and lowering.
_Avoid_: boolean compatibility result, ad hoc payload lookup, stored IR cache

**Backend Structural Recursive Data Mismatch**:
The structured backend-domain reason produced when **Backend Structural Recursive Data Matching** fails.
_Avoid_: diagnostic text, frontend source error, LLVM lowering error

## Relationships

- **Snapshot Finalization** produces **Presolution View** artifacts for elaboration and solved handles for finalized solved-graph validation.
- **Snapshot Finalization** is not a compatibility home for outdated raw-view adapters; stale legacy consumers should be removed rather than relocated when the paper-backed pipeline no longer needs them.
- **Legacy Surface Retirement** frames the broader cleanup; **Snapshot Finalization** is one internal sub-slice when read-model construction is part of the retired surface.
- **Legacy Surface Retirement** may broaden a cleanup beyond one internal seam when a legacy parser or surface-syntax compatibility path preserves an outdated non-canonical language shape.
- **Legacy Surface Retirement** treats ASCII aliases for canonical tokens as compatibility syntax; the target accepted syntax is the paper-aligned canonical spelling.
- **Legacy Surface Retirement** applies consistently to frontend eMLF and explicit xMLF parsers; parser families should not keep different compatibility alias policies.
- **Legacy Surface Retirement** is enforced by rejection tests for retired syntax, not only by deleting old acceptance tests.
- **Legacy Surface Retirement** rejection tests assert parse failure, not exact parser diagnostic text.
- **Generalization Preparation** consumes one **Acyclic Base Constraint** and one **Presolution View**.
- **Generalization Preparation** produces one **Prepared Generalization Artifact**.
- A **Prepared Generalization Artifact** is shared by elaboration, result-type reconstruction, and root-scheme generalization.
- **Backend Structural Recursive Data Matching** happens after conversion has produced canonical backend data identities and uses exact identity; source-local recovery remains a conversion adapter concern.
- **Backend Structural Recursive Data Matching** produces a **Backend Structural Recursive Data Match** rather than a bare boolean.
- **Backend Structural Recursive Data Matching** has metadata-light and metadata-backed modes that share one core matcher.
- Metadata-light **Backend Structural Recursive Data Matching** proves only canonical data identity and structural recursive skeleton compatibility.
- Metadata-backed **Backend Structural Recursive Data Matching** uses nominal `BackendData` declarations to prove constructor-set equality, recovered data-parameter substitution, and aligned field evidence.
- Metadata-light **Backend Structural Recursive Data Matching** is not a fallback for missing nominal declarations when validation, conversion, or lowering needs constructor sets, substitutions, payload fields, handler validation, or lowering field types.
- Adapter operations that need metadata-backed evidence must acquire nominal declarations or fail closed.
- **Backend Structural Recursive Data Matching** supports whole-data and focused-constructor modes through the same core identity, substitution, and payload-comparison rules.
- A **Backend Structural Recursive Data Match** carries backend data-parameter and constructor-payload evidence, but not runtime tags, field offsets, closure-record layout, or LLVM/native lowering facts.
- **Backend Structural Recursive Data Matching** owns substitution-aware constructor-payload comparison; adapters provide declarations and structural shapes, and the matcher returns the recovered data-parameter substitution plus aligned field evidence.
- **Backend Structural Recursive Data Matching** recursively compares structural recursive payload fields after substitution, using a visited-pair guard keyed by canonical data identity and structural recursive binder/name so genuinely recursive fields terminate.
- Constructor identity inside **Backend Structural Recursive Data Matching** is owner-qualified by canonical data identity plus constructor name; bare constructor names are not backend identities.
- Whole-data matching proves the structural recursive data shape matches the nominal data declaration as a constructor set; focused-constructor matching proves only the constructor or handler shape needed by the current conversion or lowering operation.
- Whole-data **Backend Structural Recursive Data Matching** requires exact constructor-set equality; missing or extra structural constructors are mismatches.
- Backend IR validation requires metadata-backed whole-data **Backend Structural Recursive Data Matching** whenever a structural recursive type claims a canonical backend data identity and the nominal declaration is available.
- Focused-constructor matching does not relax whole-data validity; it is an operation-local proof for conversion or lowering after the canonical backend shape has been established.
- A **Backend Structural Recursive Data Match** is derived evidence returned by the matcher for the current adapter operation; it is not stored on `BackendProgram`, `BackendData`, or constructor metadata.
- A failed **Backend Structural Recursive Data Matching** attempt produces a **Backend Structural Recursive Data Mismatch** that adapters translate into validation, conversion, or lowering errors.
- **Backend Structural Recursive Data Mismatch** reasons stay at the backend structural-data level: data-identity mismatch, non-structural body, data-parameter mismatch, missing constructor, handler-shape mismatch, and constructor-payload mismatch.
- Conversion must normalize source-local structural recursive names to canonical backend data identities before backend validation; unresolved or ambiguous names fail in conversion rather than widening **Backend Structural Recursive Data Matching**.
- Conversion normalization rewrites backend type shapes to canonical backend names; it does not preserve source-local structural names behind a side map or later recovery hint.
- Conversion normalization also resolves representation-level nominal data aliases such as `BTCon` versus `BTBase` before **Backend Structural Recursive Data Matching**; the shared matcher compares one canonical backend representation rather than treating representation aliases as peer identity rules.
- **Backend Structural Recursive Data Matching** tests cover the private matcher directly for exact identity, recursive payload cycle guards, substitution mismatch, extra or missing constructors, and metadata-light evidence limits.
- Adapter regression tests also validate, convert, and lower the same recursive ADT shape through public backend paths so the validation, conversion, and LLVM lowering adapters cannot drift independently.

## Example Dialogue

> **Dev:** "Should `Pipeline` build copy maps and scope overrides before elaboration?"
> **Domain expert:** "No. `Pipeline` should ask **Generalization Preparation** for a **Prepared Generalization Artifact** and pass that artifact to the consumers."

## Flagged Ambiguities

- "generalization input" can mean either individual maps or the **Prepared Generalization Artifact**; prefer the artifact name when talking about the shared Interface.
- "recursive ADT matching" can mean either source-local recovery during conversion or **Backend Structural Recursive Data Matching** over canonical backend data identities; keep the shared backend matcher on the canonical backend meaning.
- "constructor name" inside **Backend Structural Recursive Data Matching** means owner-qualified constructor identity, not a bare textual name.
- "metadata-light match" is not a full ADT proof; constructor-payload evidence requires metadata-backed matching.
- "`BTCon` versus `BTBase` equivalence" is a conversion-normalization concern, not a second identity rule inside **Backend Structural Recursive Data Matching**.
- "mismatch reason" means **Backend Structural Recursive Data Mismatch** until an adapter renders it into a validation, conversion, or lowering diagnostic.
- "matcher coverage" means both private matcher unit coverage and public adapter regression coverage; one does not replace the other.
- "snapshot materialization" conflicts with presolution expansion materialization; use **Snapshot Finalization** for canonical read-model and solved-handle construction.
- `ChiQuery` is an adapter name, not a domain boundary; prefer direct **Presolution View** accessors or **Snapshot Finalization** entrypoints.
- "legacy bridge" does not imply a supported compatibility contract; prefer deleting obsolete legacy code over preserving adapters for older internal shapes.
- "legacy syntax" is not protected by default; canonical parser and pretty-printer behavior should follow the paper-aligned syntax unless an explicit thesis-faithfulness reason keeps an alias.
- "alias" is not a separate exception from "legacy syntax"; parser aliases are retired with other compatibility surfaces unless explicitly justified.
