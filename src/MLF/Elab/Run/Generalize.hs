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
