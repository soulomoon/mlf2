module MLF.Elab.Run.Generalize.Types (
    NodeKey,
    NodeKeySet,
    NodeMap,
    GenMap,
    BindParents,
    NodeMapping(..),
    GeneralizeEnv(..),
    Phase1Result(..),
    Phase2Result(..),
    Phase3Result(..),
    Phase4Result(..),
    InsertMode(..)
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
    ( BindFlag
    , Constraint
    , GenNode
    , GenNodeId
    , NodeId
    , NodeRef
    , TyNode
    )
import MLF.Util.Trace (TraceConfig)

type NodeKey = Int
type NodeKeySet = IntSet.IntSet
type NodeMap = IntMap.IntMap TyNode
type GenMap = IntMap.IntMap GenNode
type BindParents = IntMap.IntMap (NodeRef, BindFlag)

data NodeMapping = NodeMapping
    { mapBaseToSolved :: IntMap.IntMap NodeId
    , mapSolvedToBase :: IntMap.IntMap NodeId
    }

data GeneralizeEnv = GeneralizeEnv
    { geBaseConstraint :: Constraint
    , geSolvedConstraint :: Constraint
    , geRedirects :: IntMap.IntMap NodeId
    , geInstCopyNodes :: NodeKeySet
    , geInstCopyMap :: IntMap.IntMap NodeId
    , geCanonical :: NodeId -> NodeId
    , geApplyRedirectsToRef :: NodeRef -> NodeRef
    , geAdoptRef :: NodeRef -> NodeRef
    , geAdoptNodeId :: NodeId -> NodeId
    , geTraceConfig :: TraceConfig
    }

data Phase1Result = Phase1Result
    { p1NodesSolved :: NodeMap
    , p1SchemeRootsBase :: [NodeId]
    , p1SchemeRootsBaseSet :: IntSet.IntSet
    , p1SchemeRootsAllSet :: IntSet.IntSet
    }

data Phase2Result = Phase2Result
    { p2GenMerged :: GenMap
    , p2NodeMapping :: NodeMapping
    , p2CopyOverrides :: IntMap.IntMap NodeId
    , p2BindParentsBase :: BindParents
    , p2BindParentsSolved :: BindParents
    , p2StickyTypeParentsBase :: IntSet.IntSet
    , p2BaseNamedKeys :: IntSet.IntSet
    }

data Phase3Result = Phase3Result
    { p3BindParentsBaseAdjusted :: BindParents
    , p3BindParentsWithCopies :: BindParents
    }

data Phase4Result = Phase4Result
    { p4BindParentsFinalAligned :: BindParents
    , p4GenMerged :: GenMap
    , p4SchemeRootsMerged :: [(GenNodeId, [NodeId])]
    , p4RootGenIdBase :: GenNodeId
    }

data InsertMode = KeepOld | Override | SelfOrEmpty
