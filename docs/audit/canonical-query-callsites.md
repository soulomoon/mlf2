# Canonical-Domain Query Call Sites — Elaboration Path Audit

**Date:** 2026-02-27
**Task:** Phase B, Task 6 — Audit and tag canonical-domain query call sites
**Scope:** `src/MLF/Elab/` and `src/MLF/Reify/`

## Summary

This audit enumerates every call site in the elaboration and reification paths
that uses a canonical-domain query from `Solved`. These are the sites that may
need migration to original-domain (projection-first) queries in Tasks 7–8.

**Note:** `Solved.canonical` (the canonicalization mapping function) appears
pervasively as a helper to translate NodeIds. It is _not_ a query over the
canonical constraint and is excluded from this table. Similarly,
`Solved.canonicalMap` (the raw UF map) is excluded — it is infrastructure, not
a domain query.

## Call Sites

### Reify/Core.hs — `reifyWith` (core reification engine)

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Reify/Core.hs | 94 | `Solved.canonicalNodes` | `reifyWith` — top-level node map | Yes — Task 7 target | High |
| Reify/Core.hs | 98 | `Solved.canonicalGenNodes` | `reifyWith` — gen-node list | Yes — Task 7 target | High |
| Reify/Core.hs | 136 | `Solved.canonicalizedBindParents` | `reifyWith` — bind-parent map | Yes — Task 7 target | High |
| Reify/Core.hs | 156 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goBoundRoot` | Yes — Task 7 target | High |
| Reify/Core.hs | 165 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goBoundRoot` | Yes — Task 7 target | High |
| Reify/Core.hs | 177 | `Solved.canonicalGenNodes` | `reifyWith` / `goType` — gen membership check | Yes — Task 7 target | High |
| Reify/Core.hs | 216 | `Solved.canonicalGenNodes` | `reifyWith` / `goType` — gen membership check | Yes — Task 7 target | High |
| Reify/Core.hs | 273 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goType` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 292 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goType` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 298 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goType` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 306 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goType` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 419 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goScheme` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 451 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goScheme` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 464 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goScheme` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 476 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goScheme` — bound lookup | Yes — Task 7 target | High |
| Reify/Core.hs | 515 | `Solved.lookupCanonicalVarBound` | `reifyWith` / `goScheme` — bound lookup | Yes — Task 7 target | High |

### Elab/Run/ResultType/Fallback.hs — result-type fallback logic

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/ResultType/Fallback.hs | 231 | `Solved.canonicalNodes` | `computeResultTypeFallback` — node map | Yes, but Fallback is legacy (Task 14) | Medium |
| Run/ResultType/Fallback.hs | 240 | `Solved.lookupCanonicalNode` | `computeResultTypeFallback` / `resolveBaseBoundCanonical` | Yes, but Fallback is legacy | Medium |
| Run/ResultType/Fallback.hs | 244 | `Solved.lookupCanonicalVarBound` | `computeResultTypeFallback` / `resolveBaseBoundCanonical` | Yes, but Fallback is legacy | Medium |
| Run/ResultType/Fallback.hs | 507 | `Solved.canonicalNodes` | `computeResultTypeFallback` — final bounded nodes | Yes, but Fallback is legacy | Medium |
| Run/ResultType/Fallback.hs | 508 | `Solved.canonicalGenNodes` | `computeResultTypeFallback` — final gen nodes | Yes, but Fallback is legacy | Medium |
| Run/ResultType/Fallback.hs | 635 | `Solved.canonicalBindParents` | `computeResultTypeFallback` — bind-parent traversal | Yes, but Fallback is legacy | Medium |
| Run/ResultType/Fallback.hs | 638 | `Solved.lookupCanonicalVarBound` | `computeResultTypeFallback` — bound lookup in candidates | Yes, but Fallback is legacy | Medium |

### Elab/Run/Generalize.hs — generalization entry point

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize.hs | 105 | `Solved.canonicalConstraint` | `buildGeneralizeEnv` — stores as `geSolvedConstraint` | Indirect — feeds all Phase1–4 via env | High |

### Elab/Run/Generalize/Phase1.hs — accessed via `geSolvedConstraint`

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize/Phase1.hs | 33 | `geSolvedConstraint` (= `canonicalConstraint`) | `restoreSchemeNodes` — node map | Indirect canonical use | Medium |
| Run/Generalize/Phase1.hs | 36 | `cNodes solvedConstraint` | `restoreSchemeNodes` — nodesSolved0 | Indirect canonical use | Medium |
| Run/Generalize/Phase1.hs | 75 | `cGenNodes solvedConstraint` | `restoreSchemeNodes` — scheme roots | Indirect canonical use | Medium |

### Elab/Run/Generalize/Phase2.hs — accessed via `geSolvedConstraint`

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize/Phase2.hs | 37 | `geSolvedConstraint` (= `canonicalConstraint`) | `buildNodeMappings` | Indirect canonical use | Medium |
| Run/Generalize/Phase2.hs | 44 | `cGenNodes solvedConstraint` | `buildNodeMappings` — gen nodes | Indirect canonical use | Medium |
| Run/Generalize/Phase2.hs | 47 | `cBindParents solvedConstraint` | `buildNodeMappings` — bind parents | Indirect canonical use | Medium |
| Run/Generalize/Phase2.hs | 94 | `VarStore.isEliminatedVar solvedConstraint` | `buildNodeMappings` — elimination check | Indirect canonical use | Low |

### Elab/Run/Generalize/Phase3.hs — accessed via `geSolvedConstraint`

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize/Phase3.hs | 34 | `geSolvedConstraint` (= `canonicalConstraint`) | `computeBindParentsBase` | Indirect canonical use | Medium |
| Run/Generalize/Phase3.hs | 44 | `solvedConstraint { cNodes = ... }` | `computeBindParentsBase` — upperConstraint | Indirect canonical use | Medium |

### Elab/Run/Generalize/Phase4.hs — accessed via `geSolvedConstraint`

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize/Phase4.hs | 60 | `geSolvedConstraint` (= `canonicalConstraint`) | `computeSchemeOwnership` | Indirect canonical use | Medium |
| Run/Generalize/Phase4.hs | 77 | `solvedConstraint { cNodes = ... }` | `computeSchemeOwnership` — upperConstraint | Indirect canonical use | Medium |
| Run/Generalize/Phase4.hs | 622 | `solvedConstraint` | `computeSchemeOwnership` / `attachOrphans` | Indirect canonical use | Medium |

### Elab/Run/Generalize/Finalize.hs — accessed via `geSolvedConstraint`

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Generalize/Finalize.hs | 52 | `geSolvedConstraint` (= `canonicalConstraint`) | `finalizeConstraint` | Indirect canonical use | Medium |
| Run/Generalize/Finalize.hs | 65 | `solvedConstraint { cNodes = ... }` | `finalizeConstraint` — upperConstraint | Indirect canonical use | Medium |
| Run/Generalize/Finalize.hs | 109 | `solvedConstraint` | `finalizeConstraint` — constraint0 | Indirect canonical use | Medium |

### Elab/Run/Scope.hs — binding scope resolution

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/Scope.hs | 78 | `Solved.canonicalBindParents` | `bindingScopeRefCanonical` | Yes — explicit canonical variant | Medium |

### Elab/Run/ResultType/Ann.hs — annotation result type

| File | Line | Function Called | Calling Context | Could Use Original? | Priority |
|------|------|----------------|-----------------|---------------------|----------|
| Run/ResultType/Ann.hs | 121 | `Solved.lookupVarBound` | `computeResultTypeAnn` — ann bound lookup | No — already uses original-domain | N/A |

## Excluded: `Solved.canonical` (canonicalization mapping)

The following files use `Solved.canonical` as a NodeId-to-NodeId mapping
function. This is the UF canonicalization helper, not a query over the
canonical constraint. These are **not** migration targets but are listed
for completeness:

- `Reify/Core.hs` — lines 95, 694, 717, 733, 749, 765, 790, 834
- `Reify/TypeOps.hs` — line 353
- `Elab/Phi/Translate.hs` — lines 71, 439
- `Elab/Phi/Omega.hs` — line 86
- `Elab/Phi/Context.hs` — line 59
- `Elab/Phi/IdentityBridge.hs` — line 96
- `Elab/Legacy.hs` — line 48
- `Elab/Elaborate.hs` — lines 93, 203, 1160
- `Elab/Run/Pipeline.hs` — line 201
- `Elab/Run/TypeOps.hs` — line 57
- `Elab/Run/Scope.hs` — lines 110, 171, 182
- `Elab/Run/Generalize.hs` — line 106
- `Elab/Run/Generalize/Constraint.hs` — line 54

## Excluded: `Solved.canonicalMap` (raw UF map)

Used in `Elab/Run/Pipeline.hs` at lines 103 and 109 to build the canonical
constraint and the canonicalizer. This is infrastructure plumbing, not a
domain query.

## Migration Priority Summary

| Priority | Count | Description |
|----------|-------|-------------|
| High | 17 | Reify/Core.hs (16) + Generalize.hs entry point (1) — direct canonical-domain queries |
| Medium | 20 | Fallback.hs (7) + Generalize phases (12) + Scope.hs (1) — indirect or legacy |
| Low | 1 | Phase2.hs elimination check — minor |
| N/A | 1 | Ann.hs — already original-domain |

**Total canonical-domain query call sites: 38** (excluding `Solved.canonical`
mapping and `Solved.canonicalMap` infrastructure).

The highest-priority migration target is `Reify/Core.hs` (Task 7), which
accounts for 16 of the 17 high-priority sites. The single
`Solved.canonicalConstraint` call in `Generalize.hs:105` is the root that
feeds all generalization phases — migrating it would cascade through
Phase1–Phase4 and Finalize.
