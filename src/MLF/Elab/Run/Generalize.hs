module MLF.Elab.Run.Generalize (
    pruneBindParentsConstraint,
    instantiationCopyNodes,
    constraintForGeneralization,
    generalizeAtWithBuilder
) where

import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Presolution
    ( PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Solve (SolveResult, frWith, srConstraint, srUnionFind)
import MLF.Constraint.Types
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , typeRef
    )
import MLF.Elab.Generalize (GaBindParents(..), applyGeneralizePlan)
import MLF.Elab.Run.Generalize.Constraint
    ( instantiationCopyNodes
    , pruneBindParentsConstraint
    )
import MLF.Elab.Run.Generalize.Finalize (finalizeConstraint)
import MLF.Elab.Run.Generalize.Phase1 (restoreSchemeNodes)
import MLF.Elab.Run.Generalize.Phase2 (buildNodeMappings)
import MLF.Elab.Run.Generalize.Phase3 (computeBindParentsBase)
import MLF.Elab.Run.Generalize.Phase4 (computeSchemeOwnership)
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , NodeKeySet
    )
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Util.Trace (TraceConfig)
import MLF.Frontend.ConstraintGen (AnnExpr)
import MLF.Elab.Types (ElabScheme)
import MLF.Util.ElabError (ElabError)

{- Note [binding-parent projection — ga′ invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The generalization pipeline (Phases 2–3) and `resolveContext` must preserve
the thesis ga′ (Def. 15.3.2) through the base↔solved mapping.

Phase 2 (`buildNodeMappings`, Generalize/Phase2.hs):
  `chooseMapping` prefers named base nodes that still exist in solved, falls
  back to `adoptRef` (redirect+UF).  The `solvedToBase` map inserts both
  canonical and raw keys (lines 110-113) using `keepOld` — first-inserted
  wins.  Risk: if a redirect merges two base nodes with different gen
  ancestors, the first-inserted mapping wins.  In practice this is safe
  because redirects only merge structurally equivalent nodes (TyExp
  elimination), which share the same gen ancestor.

Phase 3 (`computeBindParentsBase`, Generalize/Phase3.hs):
  Merges base binding parents (via `insertBindParentBase` with KeepOld) then
  overlays solved parents (via `insertBindParentSolved`).  Base-domain
  structure takes priority.  Solved parents only fill gaps or override
  self-parents/copies.  The `isUpperRef` check (line 94) rejects parents
  that are not structurally above the child in the solved constraint.  This
  is safe: if a valid base parent is rejected, the base-domain KeepOld
  insertion already captured it.

`quotientBindParentsUnder` (Binding/Canonicalization.hs):
  Drops self-edges, merges duplicates by keeping the first parent and taking
  max flag.  "Keep first parent" (lines 92-94) is deterministic.  When UF
  merges nodes from different gen scopes, the first parent seen may not be
  the thesis-correct one.  However, this only applies to the solved-domain
  quotient; the base-domain binding parents (which define ga′) are computed
  separately in Phase 3 and are not subject to this quotient.

`resolveContext` (Presolution/Plan/Context.hs):
  `resolveScopeRoot` maps solved TypeRef through `gaSolvedToBase`, then runs
  `bindingPathToRootLocal` on base binding parents to find the gen ancestor.
  This is the authoritative ga′ recovery path.  The `Nothing -> root`
  fallback (line 119) preserves the solved-domain scope when no base mapping
  exists — this occurs only for nodes introduced during solving (not present
  in the original constraint), which correctly have no thesis ga′.

Conclusion: the binding-parent projection preserves ga′ for all nodes that
existed in the original constraint.  The `keepOld`/`keep first parent`
policies are safe because (a) redirects merge structurally equivalent nodes
sharing the same gen ancestor, and (b) the base-domain binding parents
(which define ga′) are computed independently of the solved-domain quotient.
-}
constraintForGeneralization :: TraceConfig -> SolveResult -> IntMap.IntMap NodeId -> NodeKeySet -> IntMap.IntMap NodeId -> Constraint -> AnnExpr -> (Constraint, GaBindParents)
constraintForGeneralization traceCfg solved redirects instCopyNodes instCopyMap base _ann =
    let env = buildGeneralizeEnv traceCfg solved redirects instCopyNodes instCopyMap base
        phase1 = restoreSchemeNodes env
        phase2 = buildNodeMappings env phase1
        phase3 = computeBindParentsBase env phase1 phase2
        phase4 = computeSchemeOwnership env phase1 phase2 phase3
    in finalizeConstraint env phase1 phase2 phase3 phase4

buildGeneralizeEnv
    :: TraceConfig
    -> SolveResult
    -> IntMap.IntMap NodeId
    -> NodeKeySet
    -> IntMap.IntMap NodeId
    -> Constraint
    -> GeneralizeEnv
buildGeneralizeEnv traceCfg solved redirects instCopyNodes instCopyMap base =
    let solvedConstraint = srConstraint solved
        canonical = frWith (srUnionFind solved)
        applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        canonicalRef = Canonicalize.canonicalRef canonical
        adoptRef = canonicalRef . applyRedirectsToRef
        adoptNodeId nid =
            case adoptRef (typeRef nid) of
                TypeRef nid' -> nid'
                GenRef _ -> nid
    in GeneralizeEnv
        { geBaseConstraint = base
        , geSolvedConstraint = solvedConstraint
        , geRedirects = redirects
        , geInstCopyNodes = instCopyNodes
        , geInstCopyMap = instCopyMap
        , geCanonical = canonical
        , geApplyRedirectsToRef = applyRedirectsToRef
        , geAdoptRef = adoptRef
        , geAdoptNodeId = adoptNodeId
        , geTraceConfig = traceCfg
        }

generalizeAtWithBuilder
    :: PresolutionPlanBuilder
    -> Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtWithBuilder planBuilder mbBindParentsGa res scopeRoot targetNode =
    let PresolutionPlanBuilder buildPlans = planBuilder
        go mbGa res' scope target = do
            (genPlan, reifyPlan) <- buildPlans res' mbGa scope target
            let fallback scope' target' = fst <$> go mbGa res' scope' target'
            applyGeneralizePlan fallback genPlan reifyPlan
    in go mbBindParentsGa res scopeRoot targetNode
