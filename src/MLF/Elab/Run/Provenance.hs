module MLF.Elab.Run.Provenance (
    collectBaseNamedKeys,
    buildTraceCopyMap
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), toListInterior)
import MLF.Constraint.Types
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cNodes
    , getNodeId
    , lookupNodeIn
    , nodeRefFromKey
    , typeRef
    )

-- | Collect base named keys (variables bound by Gen nodes).
collectBaseNamedKeys :: Constraint -> IntSet.IntSet
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

-- | Build interior set from edge trace root.
buildInteriorSet :: Constraint -> (NodeId -> NodeId) -> NodeId -> IntSet.IntSet
buildInteriorSet c _adoptNode rootBase =
    case Binding.interiorOf c (typeRef rootBase) of
        Right s ->
            IntSet.insert
                (getNodeId rootBase)
                (IntSet.fromList
                    [ getNodeId nid
                    | key <- IntSet.toList s
                    , TypeRef nid <- [nodeRefFromKey key]
                    ]
                )
        Left _ -> IntSet.singleton (getNodeId rootBase)

-- | Build copy map from a single edge trace.
buildTraceCopyMap
    :: Constraint
    -> IntSet.IntSet
    -> (NodeId -> NodeId)
    -> EdgeTrace
    -> IntMap.IntMap NodeId
buildTraceCopyMap c baseNamedKeysAll adoptNode tr =
    let copyMap0 = getCopyMapping (etCopyMap tr)
        rootBase = etRoot tr
        baseNodes = cNodes c
        baseInteriorSet = buildInteriorSet c adoptNode rootBase
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
        ensureRoot acc =
            let rootCopyKey = getNodeId (adoptNode rootBase)
            in IntMap.insertWith (\_ old -> old) rootCopyKey rootBase acc
        addInterior acc baseN =
            let baseKey = getNodeId baseN
                copyKey = getNodeId (adoptNode baseN)
            in if IntMap.member copyKey acc
                then acc
                else if case lookupNodeIn baseNodes baseN of
                        Just _ -> True
                        Nothing -> False
                    && IntSet.member baseKey baseInteriorSet
                    then IntMap.insert copyKey baseN acc
                else IntMap.insert copyKey rootBase acc
    in foldl'
            addInterior
            (ensureRoot (IntMap.union binderMetaOverrides (IntMap.union binderCopyOverrides invMap)))
            (toListInterior (etInterior tr))
