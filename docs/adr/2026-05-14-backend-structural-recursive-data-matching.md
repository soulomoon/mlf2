# ADR: Centralize Backend Structural Recursive Data Matching

Date: 2026-05-14
Status: Accepted

## Context

Recursive constructor and data-shape matching exists in multiple backend
places: backend IR validation, checked-program conversion, and LLVM lowering.
Those paths have already diverged on questions such as qualified data identity,
source-local recovery, `BTCon` versus `BTBase` representation handling, and how
much recursive payload structure is compared before field types are trusted.

The backend needs one private authority for deciding when a structural
recursive type represents the same canonical backend data identity and
constructor payload shape as a nominal backend data declaration. The public
backend boundary should remain `MLF.Backend.IR`; this decision does not create
a second public IR.

## Decision

Introduce one private backend matcher module, named
`MLF.Backend.StructuralRecursiveData`, as the owner of **Backend Structural
Recursive Data Matching**.

Backend IR validation, conversion, and LLVM lowering become adapters over that
matcher:

- conversion normalizes source-local or unqualified structural names to
  canonical backend data identities before backend validation;
- conversion rewrites backend type shapes to canonical backend names and does
  not preserve source-local recovery state in a side map;
- conversion also resolves representation-level nominal aliases such as
  `BTCon` versus `BTBase` before matching;
- backend validation uses metadata-backed whole-data matching when a structural
  recursive type claims a canonical backend data identity and the nominal
  declaration is available;
- conversion and lowering can use focused-constructor matching for operation
  local evidence, but focused matching does not relax whole-data validity.

The matcher uses exact canonical backend data identity. Constructor identity is
owner-qualified by canonical data identity plus constructor name; bare
constructor names are not backend identities.

The matcher has metadata-light and metadata-backed entrypoints over one core:

- metadata-light matching proves only canonical data identity and structural
  recursive skeleton compatibility;
- metadata-backed matching uses nominal `BackendData` declarations to prove
  constructor-set equality, recovered data-parameter substitution, and aligned
  constructor-field evidence.

Metadata-light matching is not a fallback for missing declarations when an
operation needs constructor sets, substitutions, payload fields, handler
validation, or lowering field types. Adapter operations that need metadata-backed
evidence must acquire nominal declarations or fail closed.

The matcher returns derived evidence rather than storing match state in backend
IR. A **Backend Structural Recursive Data Match** carries canonical data
identity, recovered data-parameter substitution, and aligned payload evidence
for the current adapter operation. It is not cached on `BackendProgram`,
`BackendData`, or constructor metadata.

Payload comparison is substitution-aware and recursive. Recursive structural
payload fields are compared after substitution with a visited-pair guard keyed
by canonical data identity and structural recursive binder/name so recursive ADT
fields terminate without accepting unrelated aliases.

Whole-data matching requires exact constructor-set equality. Missing or extra
structural constructors are mismatches. Focused-constructor matching proves only
the constructor or handler shape needed by the current adapter operation.

Failed matching returns structured **Backend Structural Recursive Data
Mismatch** reasons. Adapters translate those reasons into validation,
conversion, or LLVM lowering errors; the matcher does not own frontend source
diagnostics or LLVM-specific diagnostic text.

## Rejected Alternatives

- Keep separate matching helpers in IR validation, conversion, and lowering.
  This preserves the current drift risk and makes recursive ADT support depend
  on which backend phase sees the shape first.
- Put source-local or unqualified recovery into the shared matcher. That would
  make backend matching depend on conversion context and would weaken the
  post-conversion canonical backend invariant.
- Treat `BTCon` and `BTBase` as peer identity rules inside the matcher. That
  would move representation normalization into the shared comparison logic
  instead of keeping one canonical backend representation after conversion.
- Store match evidence in backend IR metadata. Stored evidence can go stale and
  widens the backend IR boundary for an operation-local proof.
- Use metadata-light matching as a fallback when declarations are missing. That
  would let validation or lowering rely on skeleton evidence where full ADT
  evidence is required.

## Consequences

- The private matcher module depends on backend IR types but remains an
  internal backend helper, not a public facade.
- Existing structural recursive data helpers should move toward adapters that
  call the shared matcher instead of reimplementing name, constructor, payload,
  or recursive-field comparison.
- Backend IR validation should surface structured mismatch reasons through its
  own validation error type.
- Conversion remains responsible for source-module recovery and canonicalization
  before backend validation.
- LLVM lowering remains responsible for runtime layout and field lowering, but
  obtains constructor field evidence from metadata-backed matching instead of
  reconstructing payload compatibility.
- Tests should cover the private matcher directly for exact identity, recursive
  payload cycle guards, substitution mismatch, extra or missing constructors,
  and metadata-light evidence limits.
- Adapter regression tests should validate, convert, and lower the same
  recursive ADT shape through public backend paths so validation, conversion,
  and LLVM lowering cannot drift independently.
