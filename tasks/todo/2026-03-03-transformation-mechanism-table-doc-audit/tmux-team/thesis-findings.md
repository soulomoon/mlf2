# Thesis-Faithfulness Findings

No major mismatches were found between `docs/notes/2026-02-27-transformation-mechanism-table.md` and `papers/these-finale-english.txt` for the audited rows. Most thesis-pipeline statements are aligned; two rows would benefit from wording tightening.

| Row/topic | Status | Thesis evidence (file:line) | Finding |
|---|---|---|---|
| Ordering of transformations | accurate | `papers/these-finale-english.txt:9511`, `papers/these-finale-english.txt:9516`, `papers/these-finale-english.txt:9521`, `papers/these-finale-english.txt:9524` | Thesis `SolveConstraint` explicitly uses dependency order, then per-edge propagation followed by unification. |
| Per-edge propagation transform | accurate | `papers/these-finale-english.txt:8261`, `papers/these-finale-english.txt:8262`, `papers/these-finale-english.txt:8263`, `papers/these-finale-english.txt:8267` | Propagation is defined as expansion plus adding a unification edge; this matches the table’s thesis-side summary. |
| Result-type context wiring | accurate | `papers/these-finale-english.txt:13631`, `papers/these-finale-english.txt:13633`, `papers/these-finale-english.txt:14112`, `papers/these-finale-english.txt:14114` | The thesis defines subterm types from `χp` translations (`Typ(a')`) and elaborates from translatable presolution `χp`; no split solved-artifact model is introduced. |
| Replay-map producer normalization (upfront strict contract) | overclaim | `papers/these-finale-english.txt:13925`, `papers/these-finale-english.txt:13935`, `papers/these-finale-english.txt:14095`, `papers/these-finale-english.txt:14097` | Thesis defines translation from normalized propagation witnesses and allows non-deterministic witness choice; it does not specify runtime replay-map completeness/injectivity contracts. |
| Non-root weaken/raise binder resolution | overclaim | `papers/these-finale-english.txt:13978`, `papers/these-finale-english.txt:13997`, `papers/these-finale-english.txt:14003` | Thesis translation resolves placement via ordering/context (`<P`, computed insertion points); wording that each operation “already names its target binder” is stronger than the thesis text. |

## Wording Tweaks (max 2)

1. For **Replay-map producer normalization** thesis column: replace with “Normalized propagation witnesses are translated operation-by-operation; thesis does not define separate runtime replay-map contracts.”
2. For **Non-root weaken/raise binder resolution** change text from “already names its target binder” to “provides node operands whose binder placement is resolved during translation via `<P` ordering/context.”
