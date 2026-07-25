module MLF.Elab.Run.Generalize.Phase1.TestSupport (
    Phase1ResultTestView(..),
    restoreSchemeNodesForTest,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Plan.Requirements
    ( emptyExpansionConstructionPlacements
    )
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId
    , NodeRef(..)
    , TyNode
    )
import MLF.Elab.Run.Generalize.Phase1 (restoreSchemeNodes)
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , Phase1Result(..)
    )
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Util.Trace (defaultTraceConfig)

data Phase1ResultTestView = Phase1ResultTestView
    { phase1TestRestoredNodes :: IntMap.IntMap TyNode
    , phase1TestRestoredBaseSchemeRoots :: [NodeId]
    , phase1TestRestoredSchemeRootTargets :: IntMap.IntMap NodeId
    }

restoreSchemeNodesForTest
    :: Constraint p
    -> Constraint p
    -> IntMap.IntMap NodeId
    -> Phase1ResultTestView
restoreSchemeNodesForTest base solvedConstraint redirects =
    let applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        adoptNodeId nid =
            case applyRedirectsToRef (TypeRef nid) of
                TypeRef nid' -> nid'
                GenRef _ -> nid
        phase1 =
            restoreSchemeNodes
                GeneralizeEnv
                    { geBaseConstraint = base
                    , geSolvedConstraint = solvedConstraint
                    , geRedirects = redirects
                    , geInstCopyNodes = IntSet.empty
                    , geInstCopyMap = IntMap.empty
                    , geExpansionConstructionPlacements = emptyExpansionConstructionPlacements
                    , geCanonical = id
                    , geApplyRedirectsToRef = applyRedirectsToRef
                    , geAdoptRef = applyRedirectsToRef
                    , geAdoptNodeId = adoptNodeId
                    , geTraceConfig = defaultTraceConfig
                    }
    in Phase1ResultTestView
        { phase1TestRestoredNodes = p1NodesSolved phase1
        , phase1TestRestoredBaseSchemeRoots = p1SchemeRootsBase phase1
        , phase1TestRestoredSchemeRootTargets = p1RestoredSchemeRootTargets phase1
        }
