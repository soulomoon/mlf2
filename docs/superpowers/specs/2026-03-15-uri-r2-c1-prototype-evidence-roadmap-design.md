# Staged Roadmap Design: `URI-R2-C1` Prototype Evidence Track After `remain-stop`

Date: 2026-03-15
Status: Proposed design

Amendment note (2026-03-16): future rounds under this control plane now inherit `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`. That amendment supersedes the earlier three-attempt cap and v1 reviewer-record/control-plane assumptions for new rounds while preserving historical rounds `round-016` through `round-019` as valid v1 evidence.

## Purpose

Define a separate experimental branch-of-record for `URI-R2-C1` that follows the completed prototype-free `RE1` through `RE5` track.

This design does not rewrite the accepted prototype-free result. It creates a new bounded research track whose sole purpose is to determine whether the blockers for `URI-R2-C1` are actually fixable under a tightly isolated prototype lane.

## Relationship To The Finished Prototype-Free Track

This roadmap sits alongside the finished prototype-free branch of record. It does not supersede it automatically.

The completed prototype-free track remains authoritative for what was established without prototype-backed evidence:

- `RE4` ended `not-yet-reopen`;
- `RE5` ended `remain-stop`;
- no handoff-track or implementation authorization was opened.

This new roadmap changes only one thing:

- a bounded prototype-evidence lane is now admissible for `URI-R2-C1` only, under explicit isolation and stop conditions.

If this new track fails, the accepted `remain-stop` result remains intact.

If this new track succeeds, it may justify a later, separate handoff-track roadmap. It does not itself authorize implementation.

## Starting Point

The starting point is the completed `URI-R2-C1` re-entry track:

- `docs/plans/2026-03-14-uri-r2-c1-re1-provenance-authority-evidence-contract.md`;
- `docs/plans/2026-03-14-uri-r2-c1-re2-uniqueness-evidence-contract.md`;
- `docs/plans/2026-03-14-uri-r2-c1-re3-positive-evidence-contract.md`;
- `docs/plans/2026-03-14-uri-r2-c1-re4-bounded-reentry-gate.md`;
- `docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md`.

Those artifacts remain controlling for the prototype-free branch of record and define the blocker classes that this experimental track must address:

- provenance-stable subject identity;
- uniqueness without heuristic ranking; and
- positive safety evidence for acyclicity, ownership, and constructor-directed termination behavior.

Inherited definitions carried forward into this roadmap:

- provenance-stable subject identity means one finite local root or one equivalent local cluster that remains the same subject across `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback` and witness replay, with no manufactured authority, surrogate substitution, replay-domain widening, or late repair;
- uniqueness means exactly one admissible bounded subject inside `URI-R2-C1`, with no competing roots or clusters, no heuristic ranking, no tie-breaking, and no widened comparison class;
- positive safety evidence means that this same subject stays inside one local obligation SCC, one acyclic binder-mediated structural slice, one deterministic `single-binder-family` ownership account, and one constructor-directed local reasoning path without widened ownership or search, cyclic encoding, implicit unfolding, equi-recursive reasoning, or termination weakening.

## Final Goal Of The New Roadmap

The new roadmap ends at a decisive bounded research result for `URI-R2-C1` under a prototype lane.

That result must choose exactly one of:

- `reopen-handoff-track`: the bounded prototype evidence is strong enough that a later, separate roadmap may design an implementation-handoff track for `URI-R2-C1`; or
- `hard-stop`: the blockers cannot be cleared within the bounded model even with the permitted prototype lane, so `URI-R2-C1` remains closed and the negative result becomes the new controlling research outcome for this subject.

This roadmap does not itself write an implementation-handoff design.

## Non-Goals

- No production implementation milestone.
- No silent change to production solver behavior.
- No widening beyond `URI-R2-C1`.
- No multi-SCC support.
- No cross-family SCC linking.
- No equi-recursive reasoning or implicit unfolding semantics.
- No cyclic structural-graph encoding.
- No conversion of research instrumentation into default product behavior.
- No rewriting of accepted `RE1` through `RE5` artifacts or predecessor packet history.

## Prototype Lane

The prototype lane is allowed, but only under strict isolation.

Execution model:

- the prototype lane uses one shared research entrypoint with explicit stage selectors for `P1`, `P2`, and `P3`;
- the shared research entrypoint is the only admissible executable interface for prototype evidence in this roadmap;
- stage-local instrumentation, traces, and reports may differ by selector, but reproducibility must always point back to the same shared research entrypoint plus selector and bounded scenario input.

Research entrypoint interface:

- `research_entrypoint_id`: `uri-r2-c1-prototype-entrypoint-v1`;
- invocation shape: one research entrypoint command or function with arguments `{ stage_selector, scenario_id, attempt_id }`;
- `stage_selector` enum: `P1-subject-discovery`, `P2-provenance-preservation`, `P3-safety-validation`;
- `scenario_id`: must be exactly `uri-r2-c1-only-v1`; no aliases or alternate scenario names are admissible in this roadmap;
- `attempt_id`: integer `>= 1`, monotonically increasing within the current stage under the active retry-subloop budget;
- emitted outputs:
  - machine-readable trace bundle under `orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/`;
  - reviewer-facing artifact draft inputs under the same attempt directory;
  - no writes to production outputs or default runtime caches;
- deterministic reproduction rule:
  - the same `{ stage_selector, scenario_id, attempt_id }` over unchanged code must regenerate the same trace bundle or explicitly classify the run as `inconclusive`;
  - every reviewer-facing artifact and machine-readable stage output must repeat the exact `research_entrypoint_id` and `scenario_id` verbatim.

Permitted prototype facilities:

- research-only modules behind the shared research entrypoint;
- stable provenance IDs and candidate-selection diagnostics;
- trace snapshots across generalization, reconstruction, reification, and witness replay;
- research-only witness metadata and ownership or termination audit output.

Forbidden prototype behavior:

- changing production behavior on the default path;
- treating research instrumentation as production semantics;
- silently widening subject scope, ownership scope, or graph semantics;
- using prototype results to skip later review gates.

Operational isolation rules:

- every prototype capability must enter through the shared research entrypoint and no second executable interface is admissible in this roadmap;
- non-default flags are permitted only as internal modifiers of the shared research entrypoint and never as standalone prototype entrypoints;
- research-only metadata may flow into research-only reports, traces, or test fixtures, but may not become required input for ordinary solver execution;
- shared pure helper code may be reused only if it remains semantics-preserving for the production path and the prototype-specific behavior still lives behind research-only constructors, wrappers, or entrypoints;
- no review may treat a production-path dependency on research-only metadata as acceptable isolation;
- if a prototype result cannot be reproduced without enabling the research path explicitly, that is acceptable; if it changes the default path, it is out of bounds.

## Fixed Boundary Model

These constraints remain mandatory for the entire roadmap:

- the active subject remains only `URI-R2-C1`;
- the subject remains `single-SCC` and `single-binder-family` only;
- recursion remains obligation-level rather than cyclic structural-graph encoding;
- no cross-family SCC linking is admitted;
- no equi-recursive reasoning or implicit unfolding is admitted;
- no default-on widening is admitted;
- the structural `TyNode` / constraint graph must remain non-cyclic;
- negative controls may be observed only if they do not widen the acceptance target beyond `URI-R2-C1`;
- the authoritative inherited invariant audit remains controlling for safety boundaries.

Normative safety input:

- `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` is the controlling inherited artifact for acyclicity, binding-tree discipline, occurs-check or termination boundaries, and reconstruction or reification or witness replay constraints.

## Chosen Roadmap Shape

Use a prototype-first evidence ladder rooted in the accepted `remain-stop` result.

The roadmap uses:

- the accepted prototype-free stop state as `P0`;
- three bounded prototype evidence stages (`P1` through `P3`);
- one bounded prototype decision gate (`P4`).

## Artifact Contract

Each executable stage writes exactly one reviewer-facing artifact under `docs/plans/`:

- `P1`: `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`
- `P2`: `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `P3`: `docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md`
- `P4`: `docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`

Each stage artifact must contain these sections at minimum:

- metadata: stage, date, active subject, artifact kind;
- inherited inputs: exact prior artifacts and invariant sources consumed;
- stage input interface: the exact subject token or failure classification received from the previous stage;
- method: prototype entrypoint, instrumentation used, and bounded scenario definition;
- evidence: concrete observations, traces, or bounded negative findings;
- rejection triggers: forbidden behaviors observed or ruled out;
- stage result: `pass`, `semantic-negative`, or `inconclusive`;
- next-stage handoff: the exact artifact consumer and carried subject token if the stage passed.

`P4` is the only exception because it is terminal. Its artifact must contain:

- metadata, inherited inputs, method, evidence, rejection triggers, and reviewer justification; and
- final decision: `reopen-handoff-track` or `hard-stop`, instead of `stage result` and `next-stage handoff`.

Round-local planning, implementation, review, and merge notes continue to live under `orchestrator/rounds/<round-id>/`.

Artifact versioning rule:

- the canonical stage artifact path always points to the latest accepted attempt for that stage;
- failed or inconclusive reruns do not overwrite prior evidence silently; they must be recorded in the round-local review or progress artifacts for the current attempt;
- if a stage is rerun, the stage artifact must include an `Attempt: N` line in metadata, and the reviewer record must note which attempt became authoritative;
- the original three-attempt cap is superseded for future rounds by the 2026-03-16 retry-subloop amendment; new rounds use that amendment's budget, finalization, and carry-forward rules.

Attempt storage model:

- canonical accepted artifact path in `docs/plans/` always contains the latest accepted stage narrative;
- per-attempt raw evidence and failed or inconclusive outputs live only under `orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/`;
- failed or inconclusive attempts must never replace prior accepted `docs/plans/` artifacts without an explicit successful rerun;
- if a stage never reaches an accepted attempt, the canonical `docs/plans/` path is still created once, at the terminal attempt, and must record the final non-pass outcome explicitly as `semantic-negative` or `inconclusive`;
- the reviewer record is the authoritative map from stage to accepted attempt number or terminal non-pass attempt number.

Reviewer record:

- path: `orchestrator/rounds/<round-id>/review-record.json`;
- owner: the reviewer for the active round;
- purpose: authoritative stage map for the active round, including the accepted attempt and result for every stage that ran.

Reviewer record schema:

For `contract_version: 2` rounds, this schema is extended by the 2026-03-16 retry-subloop amendment with retry summaries and finalization mode. The schema below remains valid historical evidence for the earlier v1 rounds.

```json
{
  "research_entrypoint_id": "uri-r2-c1-prototype-entrypoint-v1",
  "scenario_id": "uri-r2-c1-only-v1",
  "stages": {
    "P1": {
      "authoritative_attempt": 1,
      "authoritative_result": "pass|semantic-negative|inconclusive",
      "artifact_path": "docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md",
      "evidence_dir": "orchestrator/rounds/<round-id>/evidence/P1/attempt-1/",
      "terminal_reason": "none|budget-exhausted-inconclusive|blocking-stop-condition"
    },
    "P2": {
      "authoritative_attempt": 1,
      "authoritative_result": "pass|semantic-negative|inconclusive",
      "artifact_path": "docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md",
      "evidence_dir": "orchestrator/rounds/<round-id>/evidence/P2/attempt-1/",
      "terminal_reason": "none|budget-exhausted-inconclusive|blocking-stop-condition"
    },
    "P3": {
      "authoritative_attempt": 1,
      "authoritative_result": "pass|semantic-negative|inconclusive",
      "artifact_path": "docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md",
      "evidence_dir": "orchestrator/rounds/<round-id>/evidence/P3/attempt-1/",
      "terminal_reason": "none|budget-exhausted-inconclusive|blocking-stop-condition"
    },
    "P4": {
      "authoritative_attempt": 1,
      "authoritative_result": "reopen-handoff-track|hard-stop",
      "artifact_path": "docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md",
      "evidence_dir": "orchestrator/rounds/<round-id>/evidence/P4/attempt-1/",
      "consumed_stage_results": {
        "P1": "pass|semantic-negative|inconclusive",
        "P2": "pass|semantic-negative|inconclusive",
        "P3": "pass|semantic-negative|inconclusive"
      },
      "terminal_reason": "none|budget-exhausted-inconclusive|blocking-stop-condition"
    }
  }
}
```

`P1` input special case:

- `P1` has no prior executable-stage token;
- its `stage input interface` section must instead name the fixed inherited subject boundary, the shared research entrypoint selector for `P1`, and the candidate universe definition it will evaluate.

## Stage Interface

`P1`, `P2`, and `P3` all operate on the same research-only canonical subject token.

Canonical subject token schema:

- the canonical subject token is a normative machine-readable object, not an illustrative example;
- `subject_id`:
  required from `P1` onward; stable canonical identifier for the candidate recursive subject, and it must equal `uri-r2-c1/<candidate_id>`;
- `subject_kind`:
  required from `P1` onward; exactly `local-root` or `equivalent-local-cluster`;
- `subject_scope`:
  required from `P1` onward; object proving that the subject remains inside `URI-R2-C1` only;
- `provenance_anchor`:
  required from `P1` onward; object naming the `P1` discovery basis that created the token;
- `owner_family_status`:
  required from `P1` onward; object with `kind` equal to `unknown`, `single-family`, or `invalid`;
- `trace_handles`:
  required from `P1` onward; array of research trace references that justify identity across stages, possibly empty at `P1` only if the discovery stage produced no persisted discovery trace.

Normative canonical subject token shape:

```json
{
  "subject_id": "uri-r2-c1/cand-01",
  "subject_kind": "local-root|equivalent-local-cluster",
  "subject_scope": {
    "scenario_id": "uri-r2-c1-only-v1",
    "bounded_subject": "URI-R2-C1"
  },
  "provenance_anchor": {
    "origin_stage": "P1",
    "candidate_id": "cand-01",
    "candidate_inventory_ref": "orchestrator/rounds/<round-id>/evidence/P1/attempt-<n>/candidate-inventory.json",
    "normalization_basis": "cluster-equivalence-v1",
    "discovery_trace_ref": "trace://uri-r2-c1/p1/discovery-01"
  },
  "owner_family_status": {
    "kind": "unknown|single-family|invalid",
    "family_id": "binder-family-01|null"
  },
  "trace_handles": [
    "trace://uri-r2-c1/p1/discovery-01"
  ]
}
```

Normative token rules:

- `subject_scope.scenario_id` must equal `uri-r2-c1-only-v1`;
- `subject_scope.bounded_subject` must equal `URI-R2-C1`;
- `provenance_anchor.origin_stage` must equal `P1`;
- `provenance_anchor.candidate_id` must name an admissible candidate from the authoritative `P1` candidate inventory;
- `provenance_anchor.candidate_inventory_ref` must point to the exact attempt-local inventory file that justified the token;
- `owner_family_status.family_id` must be `null` when `kind` is `unknown` or `invalid`, and must be present only when `kind` is `single-family`;
- no later stage may alter `subject_id`, `subject_kind`, `subject_scope`, or `provenance_anchor`; later stages may only append `trace_handles` and refine `owner_family_status` according to the interface rules below.

Candidate-selection rule representation:

```yaml
candidate_selection_rule:
  candidate_universe: "local roots or equivalent local clusters inside URI-R2-C1"
  normalization: "cluster-equivalence-v1"
  admissibility_test: "exclude widened, repaired, or heuristic candidates"
  outcome: "exactly-one-subject" # or "bounded-ambiguity"
```

Normalized rejection-trigger vocabulary:

```json
[
  "none",
  "widened-search",
  "multi-scc",
  "cross-family",
  "heuristic-choice",
  "late-repair",
  "manufactured-provenance",
  "surrogate-substitution",
  "replay-domain-widening",
  "non-local-salvage",
  "structural-cycle",
  "implicit-unfolding",
  "equi-recursive-reasoning",
  "termination-weakening",
  "nondeterministic-output",
  "inconsistent-trace",
  "partial-replay",
  "production-path-dependence",
  "blocking-stop-condition"
]
```

Trace bundle schema:

```json
{
  "research_entrypoint_id": "uri-r2-c1-prototype-entrypoint-v1",
  "scenario_id": "uri-r2-c1-only-v1",
  "stage": "P1|P2|P3",
  "attempt_id": 1,
  "correlation_id": "corr-01|null",
  "subject_id": "uri-r2-c1/cand-01|null",
  "trace_refs": [
    "trace://uri-r2-c1/p1/discovery-01"
  ]
}
```

Checker result schema:

```json
{
  "check_id": "P1-C|P1-N|P1-U|P2-G|P2-S|P2-R|P2-W|P3-S|P3-A|P3-B|P3-C",
  "subject_id": "uri-r2-c1/cand-01|null",
  "evidence_ref": "orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/...",
  "verdict": "pass|semantic-negative|inconclusive",
  "rejection_trigger": "none|widened-search|multi-scc|cross-family|heuristic-choice|late-repair|manufactured-provenance|surrogate-substitution|replay-domain-widening|non-local-salvage|structural-cycle|implicit-unfolding|equi-recursive-reasoning|termination-weakening|nondeterministic-output|inconsistent-trace|partial-replay|production-path-dependence|blocking-stop-condition"
}
```

Aggregated stage verdict schema:

```json
{
  "research_entrypoint_id": "uri-r2-c1-prototype-entrypoint-v1",
  "scenario_id": "uri-r2-c1-only-v1",
  "stage": "P1|P2|P3",
  "attempt_id": 1,
  "subject_token_ref": "orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/subject-token.json|null",
  "checker_results": [
    "orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/check-*.json"
  ],
  "stage_result": "pass|semantic-negative|inconclusive",
  "terminal_reason": "none|budget-exhausted-inconclusive|blocking-stop-condition"
}
```

Interface rules:

- `P1` either emits exactly one canonical subject token or emits no token and records `semantic-negative` or `inconclusive`;
- `P2` may consume only the exact token emitted by `P1`;
- `P3` may consume only the exact token emitted or reaffirmed by `P2`;
- later stages may refine `owner_family_status` from `unknown` to `single-family(<id>)`, but may not rewrite a previously valid `subject_id`, `subject_kind`, `subject_scope`, or `provenance_anchor`;
- `owner_family_status: invalid` may be emitted only by `P2` or `P3` when evidence shows the subject cannot remain inside one deterministic binder family; once emitted, the stage must return `semantic-negative` and no later-stage handoff token is permitted;
- no later stage may replace the token by heuristic choice, repair, or widened search;
- if identity changes, the stage must classify that as failure rather than silently rewriting the token.

## Implementation Units

The prototype lane should stay decomposed into a small set of bounded units:

- research entrypoint:
  - input: `{ stage_selector, scenario_id, attempt_id }`;
  - output: bounded execution context and attempt-local evidence directory;
  - forbidden to depend on: production default entrypoints or widened scenario discovery.
- trace collector:
  - input: execution context plus stage-local events;
  - output: machine-readable trace bundle with stable trace handles;
  - forbidden to depend on: production-only logging assumptions or post hoc trace rewriting.
- canonical subject token producer or validator:
  - input: candidate or trace evidence from the active stage;
  - output: canonical subject token or bounded failure classification;
  - forbidden to depend on: heuristic ranking, widened search, or manual repair.
- stage checker:
  - input: canonical subject token plus the exact trace class required for one check;
  - output: one machine-readable checker result and one bounded pass or fail judgment;
  - forbidden to depend on: outputs from unrelated stages except through the declared token and trace interfaces.
- stage orchestrator:
  - input: stage selector, canonical subject token where applicable, and all per-check outputs for the active stage;
  - output: correlation IDs, aggregated stage verdict, and handoff payload for the artifact writer;
  - forbidden to depend on: production-path state, heuristic selection, or undocumented cross-stage shortcuts.
- artifact writer:
  - input: aggregated stage verdict plus attempt-local trace bundle;
  - output: reviewer-facing stage artifact under `docs/plans/`;
  - forbidden to depend on: implicit runtime state outside the accepted attempt directory.

No single unit should combine prototype execution, token rewriting, and reviewer artifact authoring.

No placeholder text remains intentionally in this spec; every stage is expected to be plannable from the named units, stage selectors, and artifact contracts above.

## Roadmap Milestones

### `P0` Existing Prototype-Free Stop

Accepted prior result:

- `URI-R2-C1` ended at `remain-stop` on the prototype-free branch of record.

This is the starting point, not a new executable milestone.

### `P1` Subject Discovery Prototype

Deliverable:

- one artifact that records whether a research-only prototype can identify one canonical local recursive subject for `URI-R2-C1`;
- one exact rule for candidate selection if successful, otherwise one bounded ambiguity proof;
- one rejection list for heuristic ranking, tie-breaking, widened search, and post hoc repair.

Purpose:

- test whether `URI-R2-C1` has an exact recursive subject at all, before replay or ownership questions are considered.

Exit condition:

- a reviewer can point to either one exact candidate-selection rule for `URI-R2-C1`, or an accepted bounded reason why such a rule cannot exist inside the fixed model.
- if `P1` passes, it emits exactly one canonical subject token for `P2`; otherwise it emits no token.

Candidate universe:

- every candidate must be a local recursive root inside `URI-R2-C1` or one equivalent local cluster wholly induced by that root inside the same bounded region;
- no candidate may be introduced from outside `URI-R2-C1`, from a second SCC, from a second binder family, or from widened search.

Normalization rules for `equivalent-local-cluster`:

- cluster membership must be derivable from the same bounded local discovery slice as the root;
- the normalized cluster must be finite, connected to one local recursive root, and closed without post hoc salvage;
- two candidates are equivalent only if they induce the same bounded local subject after normalization.

Required `P1` checks:

- `P1-C` candidate enumeration check:
  - input: shared research entrypoint selector `P1` plus bounded scenario for `URI-R2-C1`;
  - output: finite candidate set inside the fixed subject boundary;
  - reject if: candidate discovery requires widened search, multi-SCC reasoning, or cross-family reasoning.
- `P1-N` candidate normalization check:
  - input: finite candidate set from `P1-C`;
  - output: normalized root-or-cluster candidates under the equivalence rule above;
  - reject if: normalization needs late repair, post hoc cluster assembly, or non-local salvage.
- `P1-U` ambiguity exclusion check:
  - input: normalized candidate set from `P1-N`;
  - output: exactly one admissible subject token or one bounded ambiguity proof;
  - reject if: uniqueness depends on heuristic ranking, tie-breaking, or solver-choice preference.

Minimum evidence for “no competing roots or clusters”:

- the stage artifact must list the normalized candidate set explicitly;
- it must explain why every non-selected candidate is inadmissible inside the fixed boundary; and
- if more than one candidate remains admissible, the stage must return `semantic-negative` rather than pick one.

Required `P1` machine-readable outputs:

- canonical subject token if the stage passes;
- `candidate_selection_rule` object in the form defined in [Stage Interface];
- normalized candidate inventory with one record per candidate and one admissibility verdict per record.

Normalized candidate inventory schema:

```json
{
  "candidate_id": "cand-01",
  "candidate_kind": "local-root|equivalent-local-cluster",
  "normalization_basis": "cluster-equivalence-v1",
  "admissibility_verdict": "admissible|inadmissible",
  "rejection_trigger": "<value from normalized rejection-trigger vocabulary>"
}
```

### `P2` Provenance Preservation Prototype

Deliverable:

- one artifact that records whether the `P1` subject survives
  `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback`
  and witness replay as the same subject;
- one rejection list for manufactured authority, surrogate substitution, replay-domain widening, and late repair.

Purpose:

- test whether the selected candidate can actually carry replay and reification authority through the existing pipeline.

Exit condition:

- a reviewer can point to either one stable subject-identity account across the full bounded path, or an accepted bounded reason why identity is lost.
- if `P2` passes, it reaffirms the exact canonical subject token for `P3`; otherwise it emits no token.

Required `P2` checks:

- `P2-G` generalization-preservation check:
  - input: canonical subject token from `P1` plus a `generalizeWithPlan` trace from the shared research entrypoint selector `P2`;
  - output: one post-generalization subject account;
  - reject if: the subject splits, vanishes, or survives only by manufactured provenance or late repair.
- `P2-S` reconstruction-preservation check:
  - input: post-generalization subject account plus `schemeToType` trace;
  - output: one post-reconstruction subject account;
  - reject if: reconstruction introduces surrogate substitution, replay-domain widening, or identity ambiguity.
- `P2-R` reification-preservation check:
  - input: post-reconstruction subject account plus `reifyTypeWithNamedSetNoFallback` trace;
  - output: one post-reification subject account;
  - reject if: no-fallback reification cannot preserve the same subject token.
- `P2-W` witness-replay check:
  - input: post-reification subject account plus witness replay trace;
  - output: one replay-stable subject account;
  - reject if: witness replay is partial, ambiguous, or requires widened domains or repair.

`P2` passes only if all four checks pass for the same canonical subject token.

Trace correlation rule for `P2`:

- every `P2` subcheck must carry the same `subject_id` and a shared `correlation_id`;
- the `correlation_id` links the `generalizeWithPlan`, `schemeToType`, `reifyTypeWithNamedSetNoFallback`, and witness replay traces for the same subject;
- if any subcheck cannot attach the same `correlation_id`, the stage must classify the result as `inconclusive` or `semantic-negative`, not as a pass.

Required `P2` machine-readable outputs:

- one correlated trace bundle keyed by `{ subject_id, correlation_id }`;
- one per-subcheck result object conforming to the shared checker result schema in [Stage Interface];
- one final subject-preservation verdict for the exact canonical subject token.

### `P3` Safety Validation Prototype

Deliverable:

- one artifact that records whether the same `P1` and `P2` subject satisfies:
  - one local obligation SCC only,
  - one acyclic binder-mediated structural slice only,
  - one deterministic `single-binder-family` ownership account only, and
  - one constructor-directed local reasoning path only;
- one rejection list for widened ownership, widened search, owner repair, cyclic encoding, implicit unfolding, equi-recursive reasoning, and termination weakening.

Purpose:

- test whether the bounded subject can be made safe under the current semantic boundary rather than merely identifiable.

Exit condition:

- a reviewer can point to either affirmative bounded safety evidence for the same subject, or an accepted bounded reason why one or more safety obligations fail.

Required checks inside `P3`:

- `P3-S` local obligation-SCC check:
  - input: canonical subject token plus bounded dependency trace for that subject;
  - output: one SCC classification or one bounded negative finding;
  - reject if: a second dependency component, multi-cluster salvage, or multi-SCC salvage is required;
  - expected evidence: one local obligation SCC only for the canonical subject, no second dependency component, and no multi-cluster or multi-SCC salvage.
- `P3-A` structural-acyclicity check:
  - input: canonical subject token plus bounded structural-slice trace;
  - output: one acyclicity judgment or one bounded negative finding;
  - reject if: structural back-edge, cyclic `TyNode` encoding, or non-binder-mediated recursion is required;
  - expected evidence: one bounded structural slice, no structural back-edge, no cyclic `TyNode` encoding, and binder-mediated recursion only.
- `P3-B` single-family ownership check:
  - input: canonical subject token plus ownership or parent-chain trace;
  - output: one ownership judgment or one bounded negative finding;
  - reject if: mixed-owner interpretation, cross-family drift, or parent-chain repair is required;
  - expected evidence: one deterministic parent-chain owner for every obligation, no mixed-owner interpretation, and no cross-family drift.
- `P3-C` constructor-directed reasoning and termination check:
  - input: canonical subject token plus local reasoning or occurs-check trace;
  - output: one reasoning-path judgment or one bounded negative finding;
  - reject if: implicit unfolding, equi-recursive search, speculative recursive chase, or termination weakening is required;
  - expected evidence: one local constructor-directed reasoning path, no implicit unfolding, no equi-recursive search, and no termination weakening.

`P3` passes only if all four checks pass for the same canonical subject token.

Required `P3` machine-readable outputs:

- one checker result object per `P3-S`, `P3-A`, `P3-B`, and `P3-C`, each conforming to the shared checker result schema in [Stage Interface];
- one stage summary object conforming to the aggregated stage verdict schema in [Stage Interface].

### `P4` Prototype Decision Gate

Deliverable:

- one artifact that evaluates `P1`, `P2`, and `P3`;
- one explicit bounded decision outcome:
  - `reopen-handoff-track`, or
  - `hard-stop`.

Required contents:

- the prototype evidence actually gathered;
- which prototype stages succeeded and which failed;
- explicit stop conditions that were triggered, if any;
- reviewer-visible explanation for the final decision.

Purpose:

- decide whether the blockers for `URI-R2-C1` are actually fixable inside the bounded model once a prototype lane is permitted.

Exit condition:

- the repository has one explicit bounded decision that either opens a later handoff-track roadmap or records a hard negative result for this subject.

Decision threshold:

- `reopen-handoff-track` is permitted only if `P1`, `P2`, and `P3` each returned `pass`, all evidence is reproducible through the research entrypoint, the same canonical subject token survived all stages unchanged, and no unresolved caveat remains on provenance, uniqueness, acyclicity, ownership, or constructor-directed termination;
- `hard-stop` is required if any stage returns `semantic-negative`, if a blocking stop condition is triggered, or if an `inconclusive` result cannot be repaired and rerun without violating the fixed boundary.
- a stage that remains `inconclusive` after exhausting its three-attempt budget is represented at `P4` as `budget-exhausted-inconclusive` and is folded directly into `hard-stop`, never into `reopen-handoff-track`.

## Validation Model

Each stage must produce a reviewer-visible artifact with an explicit gate.

- `P1` gate: candidate selection is exact and bounded, or bounded ambiguity is demonstrated exactly.
- `P2` gate: subject identity is preserved exactly across the bounded replay or reification path, or the failure mode is bounded and explicit.
- `P3` gate: safety evidence is affirmative and bounded for the same subject, or the failing obligation is bounded and explicit.
- `P4` gate: the final result is explicit and justified against the actual prototype evidence.

## Failure Classification

Each executable non-terminal stage must classify its result as exactly one of:

- `pass`: the stage produced affirmative bounded evidence and, where required, an exact canonical subject token for the next stage;
- `semantic-negative`: the stage produced bounded evidence that the target property does not hold within the fixed model;
- `inconclusive`: the stage did not yield valid evidence because of prototype defects, inconsistent traces, partial replay, contradictory runs, nondeterministic output, or inability to regenerate the evidence.

Handling rules:

- `semantic-negative` is an admissible research result and may feed `hard-stop`;
- `inconclusive` is not evidence for reopening and is not automatically a semantic negative;
- an `inconclusive` stage must first be rerun or repaired within the same bounded track;
- the original three-attempt rerun policy is superseded for future rounds by the 2026-03-16 retry-subloop amendment; `P4` still consumes only authoritative finalized upstream stage results.
- if the inconclusive condition cannot be eliminated without violating the fixed boundary, the later gate must resolve to `hard-stop` rather than manufacturing a positive result.

`P4` does not use the non-terminal stage-result enum. It consumes prior stage classifications and emits only the terminal decision enum:

- `reopen-handoff-track`; or
- `hard-stop`.

## Stop Conditions

The roadmap must stop and record `hard-stop` if any stage discovers that progress requires:

- widening beyond `URI-R2-C1`;
- multi-SCC or cross-family exploration;
- heuristic ranking or solver-choice preference;
- manufactured provenance, surrogate substitution, or late repair;
- widened ownership, widened search, or non-local salvage;
- cyclic graph representation;
- implicit unfolding or equi-recursive reasoning;
- termination weakening;
- silent dependence on production-path behavior changes;
- nondeterministic prototype output that cannot be stabilized within the bounded research lane;
- contradictory evidence across repeated runs that cannot be resolved within the bounded research lane; or
- inability to regenerate the claimed evidence from the declared research entrypoint and instrumentation.

## Recommendation

Adopt this roadmap only as a separate experimental branch of record for `URI-R2-C1`.

It is the smallest honest next step because it:

- preserves the completed prototype-free `remain-stop` result as valid history;
- allows exactly the kind of bounded prototype evidence that the finished track forbade;
- attacks the blocker stack in dependency order: subject, provenance, then safety;
- keeps the subject fixed to `URI-R2-C1`; and
- still ends at a hard yes or hard no rather than drifting directly into implementation.
