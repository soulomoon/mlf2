module MLF.Elab.Run.Provenance (
    collectBaseNamedKeys,
    buildTraceCopyMap
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), EdgeSourceInterior(..), toListInterior)
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cNodes
    , getNodeId
    , lookupNodeIn
    , nodeRefFromKey
    )

-- | Collect base named keys (variables bound by Gen nodes).
collectBaseNamedKeys :: Constraint p -> IntSet.IntSet
collectBaseNamedKeys c =
    let baseNodes = cNodes c
        baseBindParents = cBindParents c
    in IntSet.fromList
        [ childKey
        | (childKey, (parentRef, _flag)) <- IntMap.toList baseBindParents
        , case parentRef of
            GenRef _ -> True
            _ -> False
        , TypeRef child <- [nodeRefFromKey childKey]
        , case lookupNodeIn baseNodes child of
            Just TyVar{} -> True
            _ -> False
        ]

-- | Build copy map from a single edge trace.
buildTraceCopyMap
    :: Constraint p
    -> IntSet.IntSet
    -> (NodeId -> NodeId)
    -> EdgeTrace
    -> IntMap.IntMap NodeId
buildTraceCopyMap c baseNamedKeysAll adoptNode tr =
    let copyMap0 = getCopyMapping (etCopyMap tr)
        rootBase = etRoot tr
        baseNodes = cNodes c
        binderCopyOverrides =
            IntMap.fromList
                [ (getNodeId (adoptNode copyN), NodeId baseKey)
                | (baseKey, copyN) <- IntMap.toList copyMap0
                , IntSet.member baseKey baseNamedKeysAll
                ]
        binderMetaOverrides =
            IntMap.fromList
                [ (getNodeId (adoptNode meta), binder)
                | (binder, _arg) <- etBinderArgs tr
                , Just meta <- [IntMap.lookup (getNodeId binder) copyMap0]
                ]
        invMap =
            IntMap.fromListWith
                (\_ old -> old)
                [ (getNodeId (adoptNode copyN), NodeId baseKey)
                | (baseKey, copyN) <- IntMap.toList copyMap0
                ]
        replayMetaOverrides =
            IntMap.fromList
                [ (getNodeId (adoptNode replayBinder), NodeId sourceKey)
                | (sourceKey, replayBinder) <- IntMap.toList (etBinderReplayMap tr)
                ]
        ensureRoot acc =
            let rootCopyKey = getNodeId (adoptNode rootBase)
            in IntMap.insertWith (\_ old -> old) rootCopyKey rootBase acc
        addInterior acc baseN =
            let copyKey = getNodeId (adoptNode baseN)
            in if IntMap.member copyKey acc
                then acc
                else case lookupNodeIn baseNodes baseN of
                    -- A trace may retain original source nodes alongside its
                    -- fresh expansion nodes. Their provenance is their own
                    -- source identity, even when they are not direct binding
                    -- descendants of the scheme root.
                    Just _ -> IntMap.insert copyKey baseN acc
                    -- Fresh interior nodes have provenance only when an
                    -- explicit copy/replay map says so.  Finalized trace
                    -- interiors may overlap, so guessing the trace root here
                    -- can steal a node owned by a different instantiation.
                    Nothing -> acc
    in foldl'
            addInterior
            ( ensureRoot
                ( IntMap.union replayMetaOverrides
                    (IntMap.union binderMetaOverrides (IntMap.union binderCopyOverrides invMap))
                )
            )
            (toListInterior (getEdgeSourceInterior (etInterior tr)))
