# mlf2 Thesis Pipeline

Domain language for the paper-faithful MLF to xMLF implementation and its checked `.mlfp` pipeline.

## Language

**Acyclic Base Constraint**:
The post-acyclicity constraint graph that preserves original binding-tree ownership for thesis ga-prime recovery.
_Avoid_: raw graph, old constraint

**Presolution View**:
The canonical read model built from a presolution or solved snapshot for elaboration and reification.
_Avoid_: solved wrapper, query bag

**Generalization Preparation**:
The elaboration-side alignment step that turns presolution outputs, the **Acyclic Base Constraint**, and annotated terms into the shared generalization input.
_Avoid_: pipeline glue, setup tuple

**Prepared Generalization Artifact**:
The single artifact produced by **Generalization Preparation** and consumed by elaboration, result-type reconstruction, and root-scheme generalization.
_Avoid_: constraint tuple, prep bag

## Relationships

- **Generalization Preparation** consumes one **Acyclic Base Constraint** and one **Presolution View**.
- **Generalization Preparation** produces one **Prepared Generalization Artifact**.
- A **Prepared Generalization Artifact** is shared by elaboration, result-type reconstruction, and root-scheme generalization.

## Example Dialogue

> **Dev:** "Should `Pipeline` build copy maps and scope overrides before elaboration?"
> **Domain expert:** "No. `Pipeline` should ask **Generalization Preparation** for a **Prepared Generalization Artifact** and pass that artifact to the consumers."

## Flagged Ambiguities

- "generalization input" can mean either individual maps or the **Prepared Generalization Artifact**; prefer the artifact name when talking about the shared Interface.
