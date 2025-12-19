# Conversation Log

This file captures the chat transcript for this repository up to 2025-12-15.

## User

```text
<environment_context>
  <cwd>/Volumes/src/mlf4</cwd>
  <approval_policy>on-request</approval_policy>
  <sandbox_mode>workspace-write</sandbox_mode>
  <network_access>restricted</network_access>
  <shell>zsh</shell>
</environment_context>
```

## User

```text
<environment_context>
  <approval_policy>never</approval_policy>
  <sandbox_mode>danger-full-access</sandbox_mode>
  <network_access>enabled</network_access>
  <shell>zsh</shell>
</environment_context>
```

## User

```text
Generate a file named AGENTS.md that serves as a contributor guide for this repository.
Your goal is to produce a clear, concise, and well-structured document with descriptive headings and actionable explanations for each section.
Follow the outline below, but adapt as needed — add sections if relevant, and omit those that do not apply to this project.

Document Requirements

- Title the document "Repository Guidelines".
- Use Markdown headings (#, ##, etc.) for structure.
- Keep the document concise. 200-400 words is optimal.
- Keep explanations short, direct, and specific to this repository.
- Provide examples where helpful (commands, directory paths, naming patterns).
- Maintain a professional, instructional tone.

Recommended Sections

Project Structure & Module Organization

- Outline the project structure, including where the source code, tests, and assets are located.

Build, Test, and Development Commands

- List key commands for building, testing, and running locally (e.g., npm test, make build).
- Briefly explain what each command does.

Coding Style & Naming Conventions

- Specify indentation rules, language-specific style preferences, and naming patterns.
- Include any formatting or linting tools used.

Testing Guidelines

- Identify testing frameworks and coverage requirements.
- State test naming conventions and how to run tests.

Commit & Pull Request Guidelines

- Summarize commit message conventions found in the project’s Git history.
- Outline pull request requirements (descriptions, linked issues, screenshots, etc.).

(Optional) Add other sections if relevant, such as Security & Configuration Tips, Architecture Overview, or Agent-Specific Instructions.
```

## User

```text
Does not desugaring via coercions κσ cause problems
Not inherently. The κσ trick in papers/xmlf.txt is mainly a presentation/device to keep the core term grammar small: instead of giving separate typing/translation rules for (b : σ) and λ(x : σ). b, the paper rewrites them into ordinary core terms that express the same constraint.

Why the paper uses κσ
Eliminates special cases: annotations become ordinary terms, so the “constraints from terms” translation only has to handle one language.
Matches xMLF’s core mechanism: in xMLF, changing a term’s type is done via explicit instantiation a φ. κσ is a definable coercion that packages “check against σ and then use as an instance” into the core language.
No runtime cost: the paper emphasizes coercions can be reduced away after elaboration (they’re operationally identity-like).
When keeping annotations explicit is equivalent (safe)
It’s safe as long as your direct handling enforces the same relationship that κσ would enforce:

(term ascription) (e : σ) should succeed iff type(e) can be instantiated to σ (i.e. type(e) ≤ σ in the paper’s instance sense).
(annotated λ parameter) λ(x : σ). body must allow x to be used at instances of σ inside body (this is exactly why the paper desugars it to λ(x). let x = κσ x in body).
What this repo does today
EAnn e σ (term annotation): MLF.ConstraintGen directly internalizes σ into the constraint graph and adds an instantiation constraint type(e) ≤ type(σ), then treats the annotated term as having type σ. This is the same checking effect the κσ rewrite is meant to produce.
ELamAnn x σ body (parameter annotation): we internalize σ directly for the parameter. Also, variable occurrences are wrapped at use sites (via TyExp), so the solver has a per-use instantiation handle—functionally similar to the paper’s “make it let-bound via let x = κσ x”.
So for type inference / constraint solving, this design choice is fine.

Where it can become a problem (future-facing)
If you want paper-faithful xMLF elaboration: xMLF doesn’t have term-ascription; the paper uses κσ precisely so annotations become expressible in xMLF as ordinary terms + instantiations.
In this repo, elaboration currently drops AAnn (AAnn expr _ -> go env expr), meaning we don’t emit an explicit instantiation/coercion at the annotation site. That’s fine as long as you don’t type-check the output in xMLF—but it could break if you later add an xMLF checker.
If you want Φ/Σ witnesses for annotation sites: the paper’s story is “each solved instantiation edge e gets a witness Ω, translated to Φ(e) (often Σ(g); Φχe(Ω))”.
Our annotation nodes don’t currently preserve an EdgeId for the annotation-instantiation edge, so there’s nothing to attach/translate there.
Bottom line
No problem for the current solver/inference pipeline (and your tests already cover annotated terms).
Potential mismatch only if you later require the elaborated term to be type-checkable under xMLF rules, in which case you’ll want to elaborate annotations as explicit instantiations/coercions (either via actual κσ desugaring, or by emitting the corresponding ETyInst using recorded edge IDs/witnesses).
```

## User

```text
Align the implementation with the paper papers/xmlf.txt  by adding back coercions κσ step by step
```

## User

```text
Verify **Witness translation (`Φ`)**  in implementation_notes.md
```

## User

```text
Does "Currently, the recorded witnesses cover the operations induced by the presolution lattice we implement (`Raise`, `Graft`+`Weaken`); `Merge`/`RaiseMerge` are represented at the type level but are not yet emitted by presolution."
```

## User

```text
Does "Currently, the recorded witnesses cover the operations induced by the presolution lattice we implement (`Raise`, `Graft`+`Weaken`); `Merge`/`RaiseMerge` are represented at the type level but are not yet emitted by presolution."   cause problem
```

## User

```text
study and come up with a fix
```

## User

```text
Does "`Merge`/`RaiseMerge` are not emitted by presolution." cause problem ?
```

## User

```text
Does "presolution not emitting `Merge`/`RaiseMerge`." make our implemenation not align to the paper, does it make our implementation not valid ?
```

## User

```text
Why is our impelmentation not emit `Merge`/`RaiseMerge`, Give a detailed plan and write it down to a new txt file.
```

## User

```text
Does “we are incomplete vs the paper’s witness machinery” make our type system weaker than the papers
```

## User

```text
Can you think of examples of `valid paper derivations would require “aliasing/escaping”
    behavior that the paper expresses as Merge/RaiseMerge,` that would make our type system fail. Think of examples and add those tests, it is fine we fail these tests, it can be served as a baseline for us to align our implementation to the paper in the future.
```

## User

```text
now start the alignment implementation following merge_raise_merge_plan
```

## User

```text
continue
```

## User

```text
check if Phase 1 in merge_raise_merge_plan.txt is correctly implemented
```

## User

```text
Give me more detail about the different
```

## User

```text
Can you think of examples of `The key differences (plan vs implementation)` that would make our type system fail. Think of examples and add those tests, it is fine we fail these tests, it can be served as a baseline for us to align our implementation to the paper in the future.
```

## User

```text
The key differences (plan vs implementation)

  1. How I(r) is computed
      - Plan: derive I(r) from instantiateScheme/copyNode’s copy cache (exact “copied nodes” + which nodes were shared).
      - Impl: derive I(r) from a NodeId allocation interval (startNext..endNext-1) (src/MLF/Presolution.hs:308).
  2. Loss of “copied-from” provenance
      - Plan implies you can know “this new node came from copying old node X”.
      - Impl does not record an originalNode -> copiedNode map; it only records the set/range of newly allocated nodes.
  3. I(r) is a superset-by-construction
      - Today it’s effectively “everything allocated during edge solving” (fresh instantiation args, copied body nodes, any ExpForall wrappers allocated by applyExpansion, etc.).
      - That’s usually fine for “interior vs exterior” classification, but it’s not the same object as the paper’s I(r) (which is defined on the expansion interior, not “whatever we allocated during this edge”).
  4. Not future-proof to changes in allocation
      - If later we allocate nodes during unification, or share expansions across edges, the “NodeId range = interior” heuristic can become wrong.
  5. No canonicalization step for traces
      - Witnesses are canonicalized in rewriteConstraint, but psEdgeTraces are currently not rewritten/canonicalized. If we start consuming traces for Phase 2 logic, we’ll likely want to canonicalize trace NodeIds too.

  Bottom line

  - Phase 1 is implemented enough to support the next step (Phase 2) for many useful cases (e.g. detecting merges among fresh instantiation metas via etBinderArgs + etCreated membership).
  - It is not implemented in the “paper-faithful / copyNode-cache-based” way described in the plan, so it won’t support more subtle provenance needs without further work.
```

## User

```text
• Yes—this is an accurate description of the current Phase 1 provenance gap vs merge_raise_merge_plan.txt.

  A couple small additions/clarifications:

  - Your point (4) is already observable: EdgeTrace can “miss” the binder arg it reports when an ExpVarId is reused across edges, because etBinderArgs comes from the final merged Expansion, while etCreated is just “allocated during this
  edge” (src/MLF/Presolution.hs:308). The baseline failure in test/PresolutionSpec.hs:311 is exactly that.
  - Traces are also not redirected through the final prRedirects map (only witnesses are canonicalized in rewriteConstraint), so any Phase 2 logic that consumes traces will need a redirect/canonicalization pass first.

  Fix to align with the plan (what I’d implement next

)  - Add a traced instantiation API: instantiateSchemeWithTrace that returns (root, copyMap, interiorSet) by instrumenting copyNode’s cache (exactly what was copied vs shared).
  - Thread that trace up through expansion materialization when processing an inst edge (so EdgeTrace’s “I(r)” comes from copy provenance, not the NodeId interval).
  - Extend EdgeTrace to include the originalNode -> copiedNode map and store a real I(r) set (not just etCreated).
  - Canonicalize/redirect psEdgeTraces during rewriteConstraint the same way we canonicalize witnesses, so Phase 2 can safely look at trace NodeIds.
```

## User

```text
First verify if Phase 1 — Add provenance: track expansion interiors and copied nodes is correctly implemented, if not continue to implement it. Then implment Phase 2 — Record merges caused by instantiation-edge unifications
```

## User

```text
continue to Phase 3 — Normalize the recorded ops (lightweight normalization)
```

## User

```text
does  Phase 3 fully implemented?
```

## User

```text
does  Phase 2 fully implemented?
```

## User

```text
update merge_raise_merge_plan.txt to close this gap in phase 2 and phase 3, then implement full Phase 2 and phase 3. also should come up with examples to for the gap.
```

## User

```text
continue to fully implement Phase 4 — Decide where “Raise(n)” (non-root) should come from as hard as you can if the Phase 2 and Phase 3 are perfectly implemented and match the papers
```

## User

```text
does the ICFP’08 “raising binding edges”/rank-adjustment behavior) confliting to the papers/xmlf.txt? if so, choose the method of papers/xmlf.txt and update relevant notes and implementation
```

## System

```text
Another language model started to solve this problem and produced a summary of its thinking process. You also have access to the state of the tools that were used by that language model. Use this to build on the work that has already been done and avoid duplicating work. Here is the summary produced by the other language model, use the information in this summary to assist with your own analysis:

Handoff Summary — Phase 4 “Raise(n)” / Level-Raising vs `xmlf.txt` Alignment

## User request (current)
User asks whether the newly implemented ICFP’08-style “raising binding edges” / rank-adjustment (lowering variable levels to LCA before var-var unions) conflicts with `papers/xmlf.txt`. If so, switch to the `xmlf.txt` method and update notes + implementation accordingly.

## What was implemented so far (Phase 4 attempt)
Instead of emitting paper-style non-root `Raise(n)` ops in witnesses, we implemented the **binding-tree effect of “Raise” as level adjustment**:

- **Normalize**: before unioning two `TyVar` roots, compute **LCA** of their `tnVarLevel` in `Constraint.cGNodes` (`GNode.gParent`) and set both vars’ levels to that LCA; move `(var,bound)` in `GNode.gBinds` when present.
  - Code: `src/MLF/Normalize.hs` inside `processUnifyEdges` (local `unionNodes` wrapper + helpers).
- **Presolution**: same LCA level harmonization is run in `unifyAcyclic` before union-find insert.
  - Code: `src/MLF/Presolution.hs:~1520+` (`harmonizeVarLevels`, `setVarLevel`, `moveVarBindIfPresent`, `lowestCommonAncestorLevel`).
- **Solve**: same LCA harmonization before var-var union in Phase 5.
  - Code: `src/MLF/Solve.hs` (`harmonizeVarLevels` called in `processEdge` for `(TyVar,TyVar)`).

### Tests added
- `test/NormalizeSpec.hs`: “raises variable levels to the lowest common binder before merging” (constructs g0→g1 nesting, unifies vars so UF would otherwise pick inner rep).
- `test/SolveSpec.hs`: similar test ensuring solve lowers chosen representative’s level to LCA.

### Docs updated
- `merge_raise_merge_plan.txt`: Phase 4 section rewritten to describe this LCA-based level raising (ICFP’08 anchor) across Normalize/Presolution/Solve; explicitly says this is prerequisite for paper-faithful non-root Raise witnesses.
- `implementation_notes.md`: added bullets noting var-var unification now “raises” levels in Normalize and Solve; updated section numbering.

### Cleanups
- Removed `finalizePresolution` unused helper and an unused `root` binding; removed redundant pattern match to eliminate warnings (kept builds warning-free).
- Tests still have 2 *intentional* “expected failure” baselines in `test/ElaborationSpec.hs:602` and `test/ElaborationSpec.hs:623`.

## Key decisions / rationale
- Interpreted Raise(n) implementation as **binding-tree manipulation** via `GNodeId` ancestry (consistent with ICFP’08 discussion: “raise to lowest common binder before merging vars”).
- Did **not** implement explicit non-root `OpRaise(n)` witness emission nor `Φ` translation for non-root Raise per `xmlf.txt` Fig. 10. Current `OpRaise` in witnesses is still used only as a stand-in for `ExpForall` “Intro/O” at the expansion root.

## Important context / constraints
- Repo uses GHC `-Wall`; now clean after removing warnings.
- There is already extensive Merge/RaiseMerge emission + normalization (Phases 1–3 of merge/raise-merge plan) and tests around that.
- User preference: baseline tests that fail are acceptable (they explicitly said it’s okay for some tests to fail as future alignment targets).

## What remains / next steps (to answer user question)
1. **Analyze conflict vs `papers/xmlf.txt`**:
   - Determine whether LCA-based level lowering is compatible with `xmlf.txt`’s Raise semantics (Fig. 10) or if it changes meaning (e.g., should only occur under specific flexible/instantiable nodes; xmlf assumes full binding edges for *all nodes*, not just vars).
2. If there is a mismatch, **choose xmlf’s method**:
   - Likely means: do not mutate levels globally as a unification heuristic; instead record explicit non-root `Raise(n)` in per-edge witnesses and translate them via `Φ` as in Fig. 10.
   - Might require adding explicit representation of flexible/rigid/locked binding edges or a better model than `GNodeId`-only.
3. **Update implementation + notes accordingly**:
   - Potentially revert or restrict LCA raising to only contexts that correspond to paper Raise operations (e.g., only during instantiation-edge solving and only for instantiable nodes).
   - Update `merge_raise_merge_plan.txt` Phase 4 and `implementation_notes.md` to reflect the final decision.
4. Add a small **paper-faithfulness test** to detect the mismatch:
   - Example idea: a scenario where blindly lowering var levels to LCA changes quantification/generalization in a way that xmlf’s Raise witness would not, or violates instantiability constraints.

## Critical references
- `papers/xmlf.txt`:
  - Raising discussed near Fig. 10 translation (`Φξ(Raise(n))`) around `~1408–1470`.
- `papers/Remy-Yakobowski@icfp08_mlf-type-inference.txt`:
  - Raising as rank adjustment / lowering to lowest common binder discussed around `~280–320` and comparison at `~2159+`.

## Current modified files (relevant to this question)
- `src/MLF/Normalize.hs`
- `src/MLF/Presolution.hs`
- `src/MLF/Solve.hs`
- `test/NormalizeSpec.hs`
- `test/SolveSpec.hs`
- `merge_raise_merge_plan.txt`
- `implementation_notes.md`
```

## User

```text
Do we actually need rank adjustment from the point of view in  papers/xmlf.txt and our implementation, what would be the problem if we do not use it?
```

## Assistant

```text
`papers/xmlf.txt` doesn’t require *rank adjustment as an algorithm*, but it does require the **graph effect** that rank adjustment implements: *raising binding edges* (`Raise(n)` / “extruding” a node to a higher binder) so that merges remain well-scoped and witnesses stay normalizable/translatable.

In this repo, if you drop LCA rank-adjustment (the “harmonize levels before var-var union” step), you get real problems because we represent scope both in `TyVar.tnVarLevel` and in `GNode.gBinds`, and union-find otherwise picks representatives arbitrarily:

- **Scope escapes via wrong generalization**: a let-level binder can unify with an outer-scope var; if you don’t “raise/adjust” it, it can stay listed in the inner level’s `gBinds`, so `generalizeAt` may quantify something that is actually tied to the outer environment.
- **Ill-scoped / unstable solved graphs**: the same constraint can elaborate differently depending on UF union direction / edge order (inner rep vs outer rep), producing free vars, missing binders, or over-generalized schemes.
- **Wrong witness decisions**: Phase 2’s `OpRaiseMerge` heuristic uses `tnVarLevel` ancestry; if levels are wrong after merges, you’ll miss/emit `RaiseMerge` incorrectly.

Concrete shape that triggers the issue:
- `\y -> let f = \x -> x y in f`
  - The type variable for `y` (outer) gets unified with a type variable created under the let/lambda (inner). Without rank adjustment, the inner binder can “capture” the outer var and get generalized, as if `f` didn’t depend on `y`.

So: we don’t need *this specific* rank-adjustment implementation if we replace it with an equivalent invariant (e.g. track per-UF-class minimum binder and update `gBinds` accordingly), but we **do** need the raising/level-adjustment behavior somewhere, or we diverge from the paper and our own generalization/elaboration becomes incorrect and order-dependent.
```

## User

```text
So does papers/xmlf.txt offer different operation way to implement rank adjustment than our current implementation ?
```

## Assistant

```text
`papers/xmlf.txt` doesn’t really give a *solver algorithm* for rank adjustment. What it gives is a **witness-level operation** `Raise(n)` (and `RaiseMerge`) plus a translation `Φ` that turns those ops into xMLF instantiations (Fig. 10).

So there are two “operationalizations”, at different layers:

- **xmlf.txt layer (witness/elaboration):** “rank adjustment” shows up as explicit `Raise(n)` steps inside Ω; `Φ` explains how to elaborate those raises (introduce a quantifier one level higher, alias/eliminate, etc.). It assumes you can normalize Ω.
- **our current solver layer (graph/UF):** we *implement the graph effect* of raising by mutating the binding info before var-var unions (lower both vars to the LCA `GNodeId`, move `gBinds`). This is essentially “perform enough Raise steps eagerly”, but we **don’t** emit those non-root `Raise(n)` ops in witnesses.

So xmlf doesn’t propose a different UF trick; it proposes an explicit **operation language** for raises. If we wanted to be paper-faithful in witnesses, we’d need to **record** `Raise(n)` (not just adjust levels silently), because `Φ` needs those steps to reconstruct the instantiation.
```

## User

```text
if we record Raise(n), do we still need our rank adjustment ?
```

## Assistant

```text
Yes—you still need the **rank-adjustment effect** (i.e. actually *raising the binding edge / fixing levels*), even if you start **recording** `Raise(n)`.

- In `papers/xmlf.txt`, `Raise(n)` is not “just evidence”: it denotes a **real graph transformation** (a node changes which binder/G-node it belongs to) so later merges are well-scoped and the presolution stays valid.
- If we only *record* `Raise(n)` but don’t *apply* its effect to our graph (`TyVar.tnVarLevel` + `GNode.gBinds`), our solver becomes **order-dependent** (UF rep choice), and `generalizeAt` can quantify the wrong things because it relies on those scope structures.
- Also, `xmlf.txt`’s `Raise(n)` lives inside **instantiation-edge witnesses Ω**; it doesn’t cover all the var-var merges we do in Normalize/Solve. So “recording Raise(n) for Φ” can’t replace keeping the whole solved graph scope-correct.

What can change is *how* we implement the effect:
- Eager LCA adjustment (what we do now), or
- Union-find that maintains a per-class “current binder = LCA/min” and updates `gBinds`/levels accordingly (same effect, different mechanics).
```

## User

```text
Which one is more faithful to the  papers/xmlf.txt ?
```

## Assistant

```text
More faithful to `papers/xmlf.txt` is: **treat `Raise(n)` as an explicit instance-operation in the per-edge witness Ω (and apply its graph effect), then translate it with `Φ`**.

Reason: `xmlf.txt` is describing *witness machinery + elaboration*. Its notion of “raising” is an operation in Ω with a specific role in normalization and in `Φ` (Fig. 10). It does **not** describe an implementation tactic like “before unioning two vars, LCA their levels”.

Our current LCA “rank adjustment” is:
- faithful to the **graph semantics** of raising binding edges (keeps scope well-formed), and
- inspired by the older inference papers / ML rank adjustment,
but it’s **less faithful to xmlf’s story** because it performs the raising implicitly (as a UF-side-effect) and not as an explicit, ordered, normalizable Ω sequence tied to a particular instantiation edge.

So:
- If the goal is **paper-faithful witnesses + Φ elaboration**, explicit `Raise(n)` ops (with normalization) is the faithful route.
- If the goal is **just keep the solved graph well-scoped**, LCA adjustment is a fine implementation strategy, but it’s not “xmlf-faithful” in how it explains/records the work.
```

## User

```text
the goal is paper-faithful witnesses + Φ elaboration, explicit Raise(n) ops (with normalization) is the faithful route.
```

## Assistant

```text
`papers/xmlf.txt` doesn’t require *rank adjustment as an algorithm*, but it does require the **graph effect** that rank adjustment implements: *raising binding edges* (`Raise(n)` / “extruding” a node to a higher binder) so that merges remain well-scoped and witnesses stay normalizable/translatable.

In this repo, if you drop LCA rank-adjustment (the “harmonize levels before var-var union” step), you get real problems because we represent scope both in `TyVar.tnVarLevel` and in `GNode.gBinds`, and union-find otherwise picks representatives arbitrarily:

- **Scope escapes via wrong generalization**: a let-level binder can unify with an outer-scope var; if you don’t “raise/adjust” it, it can stay listed in the inner level’s `gBinds`, so `generalizeAt` may quantify something that is actually tied to the outer environment.
- **Ill-scoped / unstable solved graphs**: the same constraint can elaborate differently depending on UF union direction / edge order (inner rep vs outer rep), producing free vars, missing binders, or over-generalized schemes.
- **Wrong witness decisions**: Phase 2’s `OpRaiseMerge` heuristic uses `tnVarLevel` ancestry; if levels are wrong after merges, you’ll miss/emit `RaiseMerge` incorrectly.

Concrete shape that triggers the issue:
- `\y -> let f = \x -> x y in f`
  - The type variable for `y` (outer) gets unified with a type variable created under the let/lambda (inner). Without rank adjustment, the inner binder can “capture” the outer var and get generalized, as if `f` didn’t depend on `y`.

So: we don’t need *this specific* rank-adjustment implementation if we replace it with an equivalent invariant (e.g. track per-UF-class minimum binder and update `gBinds` accordingly), but we **do** need the raising/level-adjustment behavior somewhere, or we diverge from the paper and our own generalization/elaboration becomes incorrect and order-dependent.
```

## User

```text
你需要把我们到目前为止的全部对话，
```

## Assistant

```text
你希望我把“到目前为止的全部对话”做成什么形式？

- 原样整理成一份文本/Markdown并写入仓库文件（例如 `conversation_log.md`）？
- 只做精炼总结（按阶段/结论/下一步）？
- 翻译成中文/英文版本？
- 需要包含哪些内容：只要结论与计划，还是也要包含所有代码片段/命令/测试输出？
```

## User

```text
does not we already have OpRaise, why we are not perform rank-adjustment in OpRaise
```

## Assistant

```text
`OpRaise` is a *witness op* (paper Ω), not an operation we currently “execute” to maintain the solver’s binding invariants.

Why rank-adjustment isn’t implemented “inside `OpRaise`” today:

- We **don’t emit non-root `OpRaise(n)`** yet. Presolution uses `ExpForall` → `OpIntro(root)` (xMLF `O`), and `OpRaise` is reserved for paper `Raise(n)` later (`src/MLF/Presolution.hs:838`, `src/MLF/Types.hs:446`).
- Rank-adjustment must happen **while unifying** to keep the solved graph scope-correct (`tnVarLevel` + `gBinds`) regardless of UF representative choice; that’s why it’s done in the union paths (`src/MLF/Normalize.hs:593`, `src/MLF/Presolution.hs:1510`, `src/MLF/Solve.hs:212`).
- Even if we recorded `Raise(n)` witnesses, many var-var merges happen **outside any single instantiation edge** (Normalize/Solve), so there’s nowhere paper-faithful to attach those raises in Ω unless we redesign witness attribution.
- Also, paper `Raise(n)`’s `Φ` translation is non-trivial (Fig. 10); we haven’t implemented `Φ` for `OpRaise` yet, so emitting it prematurely wouldn’t elaborate correctly.

When we make witnesses paper-faithful, the right direction is: **apply the same raising effect in the solver AND record corresponding `OpRaise(n)` steps in the specific edge witness Ω (where it’s justified), then implement `Φ` for `OpRaise`.**
```
