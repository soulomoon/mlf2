# Findings & Decisions

## Requirements
- Read the thesis and the codebase.
- Produce a design for adding recursive types.
- Ground the design in the current implementation rather than generic PL advice.
- Keep thesis-backed facts separate from new extension proposals.

## Research Findings
- Recursive types are not part of the implemented/thesis calculus as currently presented. The thesis discusses them only as future work and explicitly says adding recursive types to MLF would be challenging; cyclic type term-graphs may be possible, but recursion in the binding structure is likely the hard part. See `/Volumes/src/mlf4/papers/these-finale-english.txt:15412`.
- The thesis/xMLF grammars do not contain any `μ`/fixpoint form. The current xMLF grammar is limited to variables, arrows, constructors, bounded `forall`, and bottom. See `/Volumes/src/mlf4/papers/these-finale-english.txt:11245` and `/Volumes/src/mlf4/papers/these-finale-english.txt:11259`.
- The thesis relies on explicit type computations witnessing instantiation and keeps xMLF type equivalence intentionally small. This is a strong argument against adding equi-recursive equality as the first step. See `/Volumes/src/mlf4/papers/these-finale-english.txt:11293` and `/Volumes/src/mlf4/papers/these-finale-english.txt:14726`.
- The graphic-constraint formalism assumes acyclic term-graphs and an acyclic binding structure. Recursive types therefore cross a core invariant rather than fitting as a small local tweak. See `/Volumes/src/mlf4/papers/these-finale-english.txt:7408`.
- The current surface/frontend type AST is closed over vars, arrows, base types, constructor application, bounded/unbounded `forall`, and bottom. See `/Volumes/src/mlf4/src/MLF/Frontend/Syntax.hs:159`, `/Volumes/src/mlf4/src/MLF/Frontend/Parse.hs:135`, `/Volumes/src/mlf4/src/MLF/Frontend/Pretty.hs:19`, `/Volumes/src/mlf4/src/MLF/Frontend/Normalize.hs:120`.
- The current constraint graph is also closed: `TyNode` only supports var/bottom/arrow/base/con/forall/exp, and Phase 3 acyclicity treats recursive dependency cycles as failure. See `/Volumes/src/mlf4/src/MLF/Constraint/Types/Graph/NodeEdge.hs:225` and `/Volumes/src/mlf4/src/MLF/Constraint/Acyclicity.hs:74`.
- Elaborated/runtime types are another closed grammar, and the checker/reducer assume arrow/forall/bottom-specific structure. There is no existing roll/unroll/fold/unfold term form. See `/Volumes/src/mlf4/src/MLF/Types/Elab.hs:86`, `/Volumes/src/mlf4/src/MLF/Types/Elab.hs:281`, `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs:34`, `/Volumes/src/mlf4/src/MLF/Elab/Reduce.hs:16`.
- XMLF syntax/tooling is paper-faithful and similarly closed; exposing recursive types publicly would touch XMLF syntax/parse/pretty as well as reification back into elaborated types. See `/Volumes/src/mlf4/src/MLF/XMLF/Syntax.hs:11`, `/Volumes/src/mlf4/src/MLF/XMLF/Parse.hs:127`, `/Volumes/src/mlf4/src/MLF/Reify/Type.hs:358`, `/Volumes/src/mlf4/src/MLF/Reify/TypeOps.hs:211`.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Treat recursive types as an extension, not a thesis feature | The thesis and xMLF grammar do not include them |
| Prefer a staged iso-recursive design | Preserves explicitness and avoids forcing equi-recursive equality into the solver/checker immediately |
| Keep the graph/binding acyclicity invariant initially | The thesis and current implementation both rely on it heavily |
| Stage rollout through explicit syntax/checking before inference | The smallest safe change is explicit recursive types with explicit fold/unfold operations |

## Issues Encountered
| Issue | Resolution |
|-------|------------|
| Initial grep for "recursive" in the thesis mostly found recursively-solved constraints, not recursive types | Read the actual grammar/future-work/soundness sections directly |
| An early shell heredoc executed backticks while updating planning files | Switched to quoted heredocs / plain file writes |

## Resources
- `/Volumes/src/mlf4/AGENTS.md`
- `/Volumes/src/mlf4/papers/these-finale-english.txt`
- `/Volumes/src/mlf4/papers/xmlf.txt`
- `/Volumes/src/mlf4/src/MLF/Frontend/Syntax.hs`
- `/Volumes/src/mlf4/src/MLF/Frontend/Normalize.hs`
- `/Volumes/src/mlf4/src/MLF/Constraint/Types/Graph/NodeEdge.hs`
- `/Volumes/src/mlf4/src/MLF/Constraint/Acyclicity.hs`
- `/Volumes/src/mlf4/src/MLF/Types/Elab.hs`
- `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs`
- `/Volumes/src/mlf4/src/MLF/Elab/Reduce.hs`
- `/Volumes/src/mlf4/src/MLF/XMLF/Syntax.hs`
- `/Volumes/src/mlf4/src/MLF/Reify/TypeOps.hs`

## Visual/Browser Findings
- No browser/PDF tools used; thesis inspection was done from local text files.
