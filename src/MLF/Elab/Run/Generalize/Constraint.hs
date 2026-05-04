module MLF.Elab.Run.Generalize.Constraint (
    pruneBindParentsConstraint,
    instantiationCopyNodes
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionView(..)
    )
import MLF.Constraint.Presolution.Base (InteriorNodes(..), copiedNodes)
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , genNodeKey
    , getNodeId
    , nodeRefFromKey
    )
import qualified MLF.Constraint.Types.Graph as Types
import MLF.Elab.Run.Generalize.Common (nodeMapToIntMap)
import MLF.Elab.Run.Generalize.Types (NodeKeySet)
import MLF.Elab.Run.Util (chaseRedirects)

pruneBindParentsConstraint :: Constraint -> Constraint
pruneBindParentsConstraint c =
    let liveNodes = nodeMapToIntMap (cNodes c)
        liveGens = Types.getGenNodeMap (cGenNodes c)
        liveRef ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        liveChild childKey =
            liveRef (nodeRefFromKey childKey)
        liveParent = liveRef
        bindParents' =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    liveChild childKey && liveParent parentRef
                )
                (cBindParents c)
    in c { cBindParents = bindParents' }

instantiationCopyNodes
    :: PresolutionView
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap EdgeTrace
    -> NodeKeySet
instantiationCopyNodes presolutionView redirects edgeTraces =
    let canonical = pvCanonical presolutionView
        adoptNode nid = canonical (chaseRedirects redirects nid)
        collectTrace tr =
            let InteriorNodes interiorKeys = etInterior tr
                copyRaw =
                    [ getNodeId node
                    | node <- copiedNodes (etCopyMap tr)
                    ]
                copyCanon =
                    [ getNodeId (adoptNode node)
                    | node <- copiedNodes (etCopyMap tr)
                    ]
                interiorRaw = IntSet.toList interiorKeys
                interiorCanon =
                    [ getNodeId (adoptNode (NodeId nid))
                    | nid <- IntSet.toList interiorKeys
                    ]
                replayRaw =
                    [ getNodeId node
                    | node <- etReplayDomainBinders tr
                    ]
                replayCanon =
                    [ getNodeId (adoptNode node)
                    | node <- etReplayDomainBinders tr
                    ]
                rootRaw = getNodeId (etRoot tr)
                rootCanon = getNodeId (adoptNode (etRoot tr))
            in IntSet.fromList
                (rootRaw : rootCanon : copyRaw ++ copyCanon ++ interiorRaw ++ interiorCanon ++ replayRaw ++ replayCanon)
    in IntSet.unions (map collectTrace (IntMap.elems edgeTraces))
