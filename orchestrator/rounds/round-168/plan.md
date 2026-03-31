# Round 168 – Plan: {- Note -} block audit and documentation sync

**Roadmap item**: item-9 — Audit all `{- Note [...] -}` blocks across `src/` for stale references, add Notes to new submodules, and sync `implementation_notes.md`.  
**Constraint**: Documentation-only changes. No behavioral changes, no new tests.

---

## Audit findings

### A. Stale function/type references in Note blocks

**Result: No stale references found.**

All 47 `{- Note` blocks across 26 files in `src/` were audited. Every function name, type name, and module path referenced within Note text was verified against the current codebase:

| Note title | File | References checked | Status |
|---|---|---|---|
| `[Normalization / Local Transformations]` | `Normalize.hs` | grafting, merging terminology | ✓ current |
| `[Grafting]` | `Normalize/Graft.hs` | TyVar, TyArrow, TyForall, TyExp, TyBottom | ✓ all in Types/Graph |
| `[Grafting Cases]` | `Normalize/Graft.hs` | constraint pattern descriptions | ✓ conceptual |
| `[Merging]` | `Normalize/Merge.hs` | TLDI'07, ICFP'08 refs | ✓ current |
| `[Normalize representative choice]` | `Normalize/Merge.hs` | `TyVar`, `TyForall`, `TyArrow` | ✓ all in Types/Graph |
| `[Phase 3: Acyclicity Check]` | `Acyclicity.hs` | `InstEdges`, node types | ✓ current |
| `[Dependency Graph Construction]` | `Acyclicity.hs` | `TyArrow`, `TyForall`, `TyExp`, `TyVar`, `TyBase` | ✓ all in Types/Graph |
| `[Topological Sort and Cycle Detection]` | `Acyclicity.hs` | DFS algorithm concepts | ✓ conceptual |
| `[Presolution foundation]` | `Presolution/Base.hs` | `MonadPresolution`, `PresolutionM`, `EdgeUnifyM`, `Ops`, `StateAccess` | ✓ current |
| `[Constraint simplification: Var-Let]` | `Presolution/Base.hs` | `allocExpNode`, `buildExprRaw`, `Translate.hs` | ✓ all exist in ConstraintGen/Translate.hs + Emit.hs |
| `[ML-Extrude omitted]` | `Presolution/Base.hs` | thesis refs only | ✓ conceptual |
| `[Binding tree]` | `Types/Graph.hs` | `TyVar`, `TyForall` | ✓ current |
| `[Edge-local omega execution]` | `EdgeProcessing/Unify.hs` | `Unify.Decompose` | ✓ module exists |
| `[Weaken suppression]` | `EdgeProcessing/Unify.hs` | conceptual | ✓ current |
| `[Representation overview]` | `Types/Graph/NodeEdge.hs` | core type constructors | ✓ current |
| `[Expansion nodes]` | `Types/Graph/NodeEdge.hs` | `TyExp` | ✓ current |
| `[Unification edges (=)]` | `Types/Graph/NodeEdge.hs` | `UnifyEdge` | ✓ current |
| `[ExpForall materialization]` | `Presolution/ForallIntro.hs` | internal | ✓ current |
| `[Minimal Expansion Decision]` | `Presolution/Expansion.hs` | presolution concepts | ✓ current |
| `[Forall Level Mismatch → Compose]` | `Presolution/Expansion.hs` | presolution concepts | ✓ current |
| `[Edge-local Raise/Merge emission]` | `EdgeUnify/Unify.hs` | edge-unify concepts | ✓ current |
| `[instantiateScheme]` | `Presolution/Copy.hs` | paper refs, `copyNode` | ✓ current |
| `[checkNoGenFallback: defense-in-depth]` | `Binding/Validation.hs` | `checkNoGenFallback` | ✓ exists in same file |
| `[Inter-binder alias bounds in recursive types]` | `Plan/ReifyPlan.hs` | `rpSubst`, `TVar` | ✓ current |
| `[Frontend type normalization]` | `Frontend/Normalize.hs` | normalization concepts | ✓ current |
| `[Recursive let reduction]` | `Elab/Reduce.hs` | reduction concepts | ✓ current |
| `[κσ coercions and desugaring]` | `Frontend/Desugar.hs` | coercion concepts | ✓ current |
| `[Phase 1: Constraint Generation]` | `Frontend/ConstraintGen.hs` | paper refs | ✓ current |
| `[Lambda vs Let Polymorphism]` | `Frontend/ConstraintGen.hs` | paper concepts | ✓ current |
| `[Coercion domain/codomain semantics]` | `ConstraintGen/Translate.hs` | coercion concepts | ✓ current |
| `[Lambda Translation]` | `ConstraintGen/Translate.hs` | `Var-Abs` rule concept | ✓ current |
| `[Constraint simplification: Var-Abs]` | `ConstraintGen/Translate.hs` | thesis refs | ✓ current |
| `[Application and Instantiation Edges]` | `ConstraintGen/Translate.hs` | paper concepts | ✓ current |
| `[Let Bindings and Expansion Variables]` | `ConstraintGen/Translate.hs` | paper concepts | ✓ current |
| `[Alternative let scoping]` | `ConstraintGen/Translate.hs` | Figure 15.2.6 | ✓ current |
| `[Mu-type annotation override]` | `Elaborate/Algebra.hs` | `generalizeAtNode`, `reifyNodeTypePreferringBound` | ✓ both exist |
| `[Selective codomain override for μ-annotated lambdas]` | `Elaborate/Algebra.hs` | μ-type concepts | ✓ current |
| `[Trace-First Copied Set]` | `Elab/Phi/Translate.hs` | `EdgeTrace.etCopyMap`, `copiedNodes` | ✓ current |
| `[Surface syntax and paper alignment]` | `Frontend/Syntax.hs` | `MLF.Frontend.Desugar`, `MLF.Constraint.Types` | ✓ both exist |
| `[Staged frontend types]` | `Frontend/Syntax.hs` | `SrcTy`, `SrcNorm`, `NormSrcType` etc. | ✓ all in Syntax.hs |
| `[InstBot replay-bound match]` | `Elab/Inst.hs` | `allowReplayBoundMatch`, `resolveReplayVars` | ✓ both in same file |
| `[binding-parent projection — ga′ invariants]` | `Elab/Run/Generalize.hs` | `chooseMapping`, `resolveScopeRoot`, `quotientBindParentsUnder`, `Generalize/Phase2.hs`, `Phase3.hs`, `Binding/Canonicalization.hs`, `Plan/Context.hs` | ✓ all exist |
| `[ga′ scope selection — Def. 15.3.2 alignment]` | `Elab/Run/Scope.hs` | `bindingScopeRef`, `canonicalizeScopeRef`, `letScopeOverrides` | ✓ all in Scope.hs |
| `[S vs S' target selection]` | `Elab/Run/Scope.hs` | `schemeBodyTarget`, `generalizeTargetNode` | ✓ both in Scope.hs |
| `[ga′ preservation across redirects]` | `Elab/Run/Scope.hs` | scope resolution concepts | ✓ current |
| `[Bound overlay for fallback target refinement]` | `Fallback/Core.hs` | `rebuildWithNodes` (descriptive, "avoids") | ✓ used descriptively |
| `[Recursive type opening for non-local fallback]` | `Fallback/Core.hs` | `schemeBodyTarget`, `rootFinal`, `TyMu` | ✓ all exist |

### B. New submodules from round-163 (item-4) that lack `{- Note` blocks

Round-163 created 9 new submodules by splitting 5 large modules. Of these, **6 lack design-rationale Notes**:

| Submodule | Lines | Suggested Note topic |
|---|---|---|
| `MLF.Constraint.Normalize.Internal` | 114 | `[Normalization state and shared helpers]` — Describes `NormalizeState`, `NormalizeM`, and the shared state helpers (`freshVar`, `insertNode`, `findRoot`, `unionNodes`) used by both Graft and Merge submodules. |
| `MLF.Reify.Type.Core` | 700 | `[Core reification algorithm — reifyWith]` — Describes the main `reifyWith` function that walks the solved constraint graph to produce `ElabType` terms, including the `ReifyRoot` dispatch, bound-vs-full reification modes, and the paper-aligned binder ordering logic (thesis §15.3). |
| `MLF.Constraint.Presolution.Plan.Env` | 94 | `[Generalization planning environment]` — Describes `PresolutionEnv` construction and `mkGeneralizeEnv` which assembles the planning context from a `PresolutionView`, including canonical map sanitization and `softenBindParents` for GA-scope computation. |
| `MLF.Constraint.Presolution.Plan.Generalize` | 618 | `[Generalization plan construction — planGeneralizeAt]` — Describes the algorithm that builds a `GeneralizePlan` for a single scope, determining which variables to generalize, how to order binders, and how to handle scheme roots (thesis §15.3). |
| `MLF.Constraint.Presolution.Plan.ReifyStep` | 156 | `[Reification step planning — planReify]` — Describes how `planReify` constructs a `ReifyPlan` determining how to reify the type structure for a generalized scope, including extra binder candidates and adjusted type roots. |
| `MLF.Elab.Phi.Omega.Interpret.Internal` | 1273 | `[Omega/Step witness interpretation]` — Describes the `phiWithSchemeOmega` function that interprets Ω instance-operation lists into xMLF instantiation terms, including binder reordering (Σ), the `go` loop over operations, and `continueRaise` for OpRaise handling (thesis §15.3.4, xMLF Fig. 10). |

The 3 submodules that already have Notes (inherited from the split) are fine:
- `Normalize/Graft.hs` — `[Grafting]`, `[Grafting Cases]`
- `Normalize/Merge.hs` — `[Merging]`, `[Normalize representative choice]`
- `Fallback/Core.hs` — `[Bound overlay for fallback target refinement]`, `[Recursive type opening for non-local fallback]`

### C. `implementation_notes.md` stale/missing module structure references

**One entry documents the Normalize split** (line 1999):
> `MLF.Constraint.Normalize` is now a thin façade over `MLF.Constraint.Normalize.Internal`, `MLF.Constraint.Normalize.Graft`, and `MLF.Constraint.Normalize.Merge`

**Missing**: The other 4 round-163 splits are **not documented** in `implementation_notes.md`:
1. `MLF.Reify.Type` → `MLF.Reify.Type.Core`
2. `MLF.Elab.Run.ResultType.Fallback` → `MLF.Elab.Run.ResultType.Fallback.Core`
3. `MLF.Constraint.Presolution.Plan` → `Plan.Env`, `Plan.Generalize`, `Plan.ReifyStep`
4. `MLF.Elab.Phi.Omega.Interpret` → `Interpret.Internal`

These should be documented alongside the Normalize entry for consistency.

---

## Implementation steps

### Step 1: Add `{- Note` blocks to 6 new submodules

For each file below, add a `{- Note [...] -}` block immediately after the module header doc comment (before imports, or after imports if more natural). Follow existing GHC-style format.

#### 1a. `src/MLF/Constraint/Normalize/Internal.hs`

Add after line 18 (after `where`), before imports:

```haskell
{- Note [Normalization state and shared helpers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module provides the shared mutable state ('NormalizeState', 'NormalizeM')
and primitive operations used by both the grafting ('Normalize.Graft') and
merging ('Normalize.Merge') normalization submodules.

Key state components:
  * 'nsConstraint'        — the constraint being normalized (modified in place)
  * 'nsUnionFind'         — union-find forest for merging (applied at end)
  * 'nsNextNodeId'        — fresh node-ID counter for grafted copies
  * 'nsSynthExpVarSupply'  — fresh expansion-variable allocator

The helpers ('freshVar', 'insertNode', 'findRoot', 'unionNodes', etc.) are
intentionally low-level primitives; higher-level normalization logic lives in
'Normalize.Graft' and 'Normalize.Merge'.
-}
```

#### 1b. `src/MLF/Reify/Type/Core.hs`

Add after line 7 (after module `where`), before imports:

```haskell
{- Note [Core reification algorithm — reifyWith]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module contains the core graph-to-type reification algorithm, implementing
the walk from constraint-graph nodes to elaborated 'ElabType' terms.

'reifyWith' is the main entry point.  Given a 'ReifyRoot' mode, a set of named
nodes, and a starting node, it walks the solved/canonical constraint graph and
produces an 'ElabType'.  The algorithm handles:

  * TyVar → TVar (with optional bound reification)
  * TyArrow → TArrow (recursive descent into domain/codomain)
  * TyForall → TForall (binder name assignment + body reification)
  * TyBase → TBase
  * TyBottom → TBottom
  * TyExp → body reification (expansion nodes are transparent)
  * TyMu → TMu (recursive type binder)

Binder ordering follows the presolution plan's <P order (thesis §15.3) via
topological sort of flex children.  The 'RootType' vs 'RootTypeNoFallback'
dispatch controls whether missing-node fallbacks are permitted.

Paper references:
  * Yakobowski PhD thesis (2008), Chapter 15 — type reification from solved
    graphic constraints
  * Rémy & Yakobowski (FLOPS 2010) — xMLF type structure
-}
```

#### 1c. `src/MLF/Constraint/Presolution/Plan/Env.hs`

Add after line 12 (after module `where`), before imports:

```haskell
{- Note [Generalization planning environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'PresolutionEnv' bundles the inputs needed by the generalization planner
('planGeneralizeAt') and the reification planner ('planReify'):

  * The original and canonical constraints
  * Presolution view (node maps, canonical function, bind parents)
  * Trace config for conditional debug output
  * The node-lookup helper 'lookupNodeInMap'

'mkGeneralizeEnv' constructs a 'GeneralizeEnv' for a single generalization
scope by sanitizing the canonical map and projecting scope-local binding
structure.  'softenBindParents' downgrades rigid bindings to flex when nodes
are outside the generalization scope, ensuring the planner sees only
scope-relevant rigidity.
-}
```

#### 1d. `src/MLF/Constraint/Presolution/Plan/Generalize.hs`

Add after line 14 (after module `where`), before imports:

```haskell
{- Note [Generalization plan construction — planGeneralizeAt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'planGeneralizeAt' builds a 'GeneralizePlan' for a single generalization scope
(one gen node in the constraint graph).  This is the core of the thesis §15.3
generalization algorithm:

  1. Identify the scope root and type root for the gen node.
  2. Collect bindable children (variables under the gen scope).
  3. Build binder plans: determine which variables become ∀-binders, their
     ordering (via the <P topological sort), and their bounds.
  4. Determine scheme roots and handle alias/wrapper nodes.
  5. Compute the reify plan for type structure reconstruction.

The plan is a pure data structure ('GeneralizePlan') consumed later by
elaboration ('MLF.Elab.Elaborate') and reification ('MLF.Reify.Type.Core').
Separating planning from execution keeps presolution logic deterministic and
testable independently of the elaboration pipeline.
-}
```

#### 1e. `src/MLF/Constraint/Presolution/Plan/ReifyStep.hs`

Add after line 14 (after module `where`), before imports:

```haskell
{- Note [Reification step planning — planReify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'planReify' constructs a 'ReifyPlan' that determines how to reify the type
structure for a generalized scope.  Given a 'GeneralizePlan' (from
'planGeneralizeAt'), it computes:

  * 'rpSubst' — the binder name substitution map (NodeId → String)
  * 'rpExtraBinders' — additional binder candidates beyond the plan's flex
    children (used for alias binders that must appear in the reified type)
  * 'rpTypeRoot' — the adjusted type root for reification (may differ from
    the plan root when alias/wrapper nodes are present)

This module is separate from 'Plan.Generalize' because reification planning
depends on the completed generalization plan, including finalized binder
names and scheme structure, while generalization planning is independent of
reification details.
-}
```

#### 1f. `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`

Add after line 17 (after module `where`), before imports:

```haskell
{- Note [Omega/Step witness interpretation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'phiWithSchemeOmega' is the core of the Φ translation for witness edges with
Ω (instance-operation) payloads, as described in the thesis §15.3.4 and
xMLF paper (Fig. 10).

The interpretation proceeds in three stages:

  1. **Binder reordering (Σ)**: Reorder quantifier binders from graph <P order
     to the target scheme's binder order, emitting 'InstUnder' context steps
     where the two orders differ (thesis Def. 15.3.4 / Fig. 15.3.5).

  2. **Forall introduction**: Emit 'InstInside' steps for the ∀-intro count
     recorded per edge during presolution.

  3. **Omega loop ('go')**: Walk the list of 'InstanceOp' values (OpRaise,
     OpMerge, OpWeaken, OpGraft) and translate each into xMLF instantiation
     steps.  'continueRaise' handles the multi-step OpRaise translation that
     involves context navigation and binder application.

The function captures the presolution view, edge trace, scheme info, and
constraint structure in a large closure environment.  All reification helpers
('reifyTypeArg', 'inferredOmegaInst', 'applyInferredArgs', etc.) are local
to this closure because they depend on the shared canonical/constraint/trace
context.

Paper references:
  * Yakobowski PhD thesis (2008), §15.3.4 — Φ translation and Ω execution
  * xMLF paper (Rémy & Yakobowski, FLOPS 2010), Fig. 10 — Ω operational rules
-}
```

### Step 2: Update `implementation_notes.md` — document remaining round-163 splits

**File**: `implementation_notes.md`

**Location**: After the existing Normalize split note at line 1999, add documentation for the other 4 splits. Insert after line 1999 (the existing `MLF.Constraint.Normalize` split note):

```markdown
- **Module split**: `MLF.Reify.Type` is now a thin façade over `MLF.Reify.Type.Core`; the core reification algorithm (`reifyWith`, `reifyWithAs`, `ReifyRoot`) lives in `Type.Core`, while public wrapper functions (`reifyType`, `reifyTypeWithNames*`, `solvedFromView`, `freeVars`) remain in the façade.
- **Module split**: `MLF.Elab.Run.ResultType.Fallback` is now a thin façade over `MLF.Elab.Run.ResultType.Fallback.Core`; the bulk fallback computation (`computeResultTypeFallbackCore`) lives in `Fallback.Core`, while entry points (`computeResultTypeFallback`, `computeResultTypeFallbackWithView`) remain in the façade.
- **Module split**: `MLF.Constraint.Presolution.Plan` is now a thin façade over `MLF.Constraint.Presolution.Plan.Env`, `MLF.Constraint.Presolution.Plan.Generalize`, and `MLF.Constraint.Presolution.Plan.ReifyStep`; environment construction, generalization planning, and reification planning are separated into focused submodules, while `buildGeneralizePlans` remains in the façade.
- **Module split**: `MLF.Elab.Phi.Omega.Interpret` is now a pure re-export façade over `MLF.Elab.Phi.Omega.Interpret.Internal`; the full `phiWithSchemeOmega` implementation lives in the Internal submodule.
```

### Step 3: Add CHANGELOG.md entry

**File**: `CHANGELOG.md`

**Location**: In the `## Unreleased / ### Changed` section (after line 6), add:

```markdown
- Documentation hygiene: audited all 47 `{- Note [...] -}` blocks across `src/` for stale references after round-163 module splits, added design-rationale Notes to 6 new submodules that lacked them (`Normalize.Internal`, `Reify.Type.Core`, `Plan.Env`, `Plan.Generalize`, `Plan.ReifyStep`, `Interpret.Internal`), and documented the remaining 4 round-163 module splits in `implementation_notes.md`.
```

### Step 4: Verification

```bash
cd /path/to/worktree
cabal build all && cabal test
```

**Expected**: 1302 examples, 0 failures. No behavioral changes — all modifications are comments and documentation.

**Post-build check**: Grep for function names mentioned in new Notes to confirm no dangling references:

```bash
grep -r 'NormalizeState\|NormalizeM\|reifyWith\|PresolutionEnv\|planGeneralizeAt\|planReify\|phiWithSchemeOmega' src/ --include='*.hs' -l
```

All names should resolve to at least one definition site.

---

## Scope boundaries

- **In scope**: Adding `{- Note` blocks (comments only), updating `implementation_notes.md`, updating `CHANGELOG.md`
- **Out of scope**: Any `.hs` code changes beyond comments, new tests, module moves, behavioral changes
- **No files removed or renamed**
- **No imports changed**
- **No cabal changes**

## Files modified (complete list)

| File | Change type |
|---|---|
| `src/MLF/Constraint/Normalize/Internal.hs` | Add `{- Note` block (comment only) |
| `src/MLF/Reify/Type/Core.hs` | Add `{- Note` block (comment only) |
| `src/MLF/Constraint/Presolution/Plan/Env.hs` | Add `{- Note` block (comment only) |
| `src/MLF/Constraint/Presolution/Plan/Generalize.hs` | Add `{- Note` block (comment only) |
| `src/MLF/Constraint/Presolution/Plan/ReifyStep.hs` | Add `{- Note` block (comment only) |
| `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs` | Add `{- Note` block (comment only) |
| `implementation_notes.md` | Add 4 module-split entries after line 1999 |
| `CHANGELOG.md` | Add documentation-hygiene entry in Unreleased section |
