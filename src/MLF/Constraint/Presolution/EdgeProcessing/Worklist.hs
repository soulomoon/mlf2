{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Worklist
Description : Indexed edge worklist for presolution edge processing
Copyright   : (c) 2024
License     : BSD-3-Clause

This module is the first step toward an inert/worklist presolution solver.
It preserves the current left-to-right edge processing order, but records the
canonical roots, scheme owner, and expansion variable observed when an edge is
planned. Later invalidation can use these indexes to requeue only affected
edges after union/copy operations.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Worklist
    ( EdgePlanSeed(..)
    , EdgeFingerprint(..)
    , EdgeWorkItem(..)
    , EdgeWorklist
    , WorklistInvalidation(..)
    , buildEdgeWorklist
    , buildIndexedEdgeWorklist
    , buildIndexedEdgeWorklistWithRootOwnership
    , popEdgeWorkItem
    , notePlannedEdge
    , noteProcessedEdge
    , noteInertEdge
    , invalidateRoots
    , invalidateRootsExcept
    , invalidateRootsWithinRootOwnersExcept
    , invalidateQueuedRootsExcept
    , invalidateOwners
    , invalidateOwnersExcept
    , invalidateOwnersWithinRootOwnersExcept
    , invalidateQueuedOwnersExcept
    , invalidateExpansions
    , invalidateExpansionsExcept
    , invalidateExpansionsWithinRootOwnersExcept
    , invalidateQueuedExpansionsExcept
    , queuedEdgeCount
    , indexedEdgeCount
    , rootOwnershipOfWorklist
    , rootOwnersForPlan
    ) where

import Control.Monad (forM)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import MLF.Constraint.Presolution.Base (PresolutionM)
import MLF.Constraint.Presolution.EdgeProcessing.Plan
    ( EdgePlan(..)
    , ResolvedTyExp(..)
    , mkResolvedTyExp
    )
import MLF.Constraint.Presolution.StateAccess
    ( PresolutionBindingSnapshot(..)
    , bindingSnapshotFindSchemeIntroducer
    , getBindingSnapshot
    )
import MLF.Constraint.Types.Graph
import MLF.Constraint.RootOwnership
import MLF.Constraint.Types.Witness (Expansion)
import qualified MLF.Util.IndexedWorklist as Indexed

data EdgePlanSeed = EdgePlanSeed
    { epsLeftTyExp :: !ResolvedTyExp
    , epsLeftRoot :: !NodeId
    , epsRightRoot :: !NodeId
    , epsBodyRoot :: !NodeId
    , epsSchemeOwnerGen :: !GenNodeId
    , epsExpansionVar :: !ExpVarId
    }
    deriving (Eq, Show)

data EdgeFingerprint = EdgeFingerprint
    { efLeftRoot :: !NodeId
    , efRightRoot :: !NodeId
    , efBodyRoot :: !NodeId
    , efSchemeOwnerGen :: !GenNodeId
    , efExpansionVar :: !ExpVarId
    , efCurrentExpansion :: !Expansion
    }
    deriving (Eq, Show)

data EdgeWorkItem = EdgeWorkItem
    { ewiEdge :: !InstEdge
    , ewiPlanSeed :: !(Maybe EdgePlanSeed)
    , ewiFingerprint :: !(Maybe EdgeFingerprint)
    , ewiRootOwners :: !IntSet
    , ewiStale :: !Bool
    }
    deriving (Eq, Show)

data EdgeWorklist = EdgeWorklist
    { ewCore :: !(Indexed.IndexedWorklist EdgeWorkItem)
    , ewPlannedEdges :: !IntSet
    , ewRootOwnership :: !RootOwnershipIndex
    }
    deriving (Eq, Show)

data WorklistInvalidation = WorklistInvalidation
    { wiRoots :: !IntSet
    , wiOwners :: !IntSet
    , wiExpansions :: !IntSet
    , wiEdges :: !IntSet
    }
    deriving (Eq, Show)

buildEdgeWorklist :: [InstEdge] -> EdgeWorklist
buildEdgeWorklist edges =
    buildEdgeWorklistFromItems
        [ EdgeWorkItem
            { ewiEdge = edge
            , ewiPlanSeed = Nothing
            , ewiFingerprint = Nothing
            , ewiRootOwners = IntSet.empty
            , ewiStale = False
            }
        | edge <- edges
        ]

buildIndexedEdgeWorklist :: [InstEdge] -> PresolutionM p EdgeWorklist
buildIndexedEdgeWorklist =
    buildIndexedEdgeWorklistWithRootOwnership emptyRootOwnershipIndex

buildIndexedEdgeWorklistWithRootOwnership :: RootOwnershipIndex -> [InstEdge] -> PresolutionM p EdgeWorklist
buildIndexedEdgeWorklistWithRootOwnership ownership edges = do
    snapshot <- getBindingSnapshot
    items <- forM edges (workItemFromSnapshot ownership snapshot)
    pure (buildEdgeWorklistFromItemsWithRootOwnership ownership items)

buildEdgeWorklistFromItems :: [EdgeWorkItem] -> EdgeWorklist
buildEdgeWorklistFromItems =
    buildEdgeWorklistFromItemsWithRootOwnership emptyRootOwnershipIndex

buildEdgeWorklistFromItemsWithRootOwnership :: RootOwnershipIndex -> [EdgeWorkItem] -> EdgeWorklist
buildEdgeWorklistFromItemsWithRootOwnership ownership items =
    foldl' indexSeed base items
  where
    base =
        EdgeWorklist
            { ewCore =
                Indexed.buildIndexedWorklist
                    [ (edgeKeyOfItem item, item)
                    | item <- items
                    ]
            , ewPlannedEdges = IntSet.empty
            , ewRootOwnership = ownership
            }

    indexSeed worklist item =
        case ewiPlanSeed item of
            Nothing -> worklist
            Just seed ->
                indexEdge
                    (edgeKeyOfItem item)
                    [epsLeftRoot seed, epsRightRoot seed, epsBodyRoot seed]
                    (Just (epsSchemeOwnerGen seed))
                    (Just (epsExpansionVar seed))
                    (ewiRootOwners item)
                    worklist

workItemFromSnapshot :: RootOwnershipIndex -> PresolutionBindingSnapshot p -> InstEdge -> PresolutionM p EdgeWorkItem
workItemFromSnapshot ownership snapshot edge = do
    let constraint0 = pbsConstraint snapshot
        canonical = pbsCanonical snapshot
        leftId = instLeft edge
        rightId = instRight edge
        mbLeftTyExp = lookupNode leftId (cNodes constraint0) >>= mkResolvedTyExp
    seed <- case mbLeftTyExp of
        Nothing -> pure Nothing
        Just leftTyExp -> do
            owner <- bindingSnapshotFindSchemeIntroducer snapshot (rteBodyId leftTyExp)
            pure $
                Just
                    EdgePlanSeed
                        { epsLeftTyExp = leftTyExp
                        , epsLeftRoot = canonical leftId
                        , epsRightRoot = canonical rightId
                        , epsBodyRoot = canonical (rteBodyId leftTyExp)
                        , epsSchemeOwnerGen = owner
                        , epsExpansionVar = rteExpVar leftTyExp
                        }
    let rootOwners =
            case seed of
                Nothing -> ownersForEdge ownership (getEdgeId (instEdgeId edge))
                Just seed0 -> rootOwnersForSeed ownership edge seed0
    pure
        EdgeWorkItem
            { ewiEdge = edge
            , ewiPlanSeed = seed
            , ewiFingerprint = Nothing
            , ewiRootOwners = rootOwners
            , ewiStale = False
            }

popEdgeWorkItem :: EdgeWorklist -> Maybe (EdgeWorkItem, EdgeWorklist)
popEdgeWorkItem worklist =
    case Indexed.popIndexedWorkItem (ewCore worklist) of
        Nothing -> Nothing
        Just (item, core') ->
            Just
                ( (Indexed.iwiValue item) { ewiStale = Indexed.iwiStale item }
                , worklist { ewCore = core' }
                )

notePlannedEdge :: EdgePlan -> EdgeWorklist -> EdgeWorklist
notePlannedEdge plan worklist =
    let worklist' =
            indexEdge
                edgeKey
                [eprLeftCanonical plan, eprRightCanonical plan, rteBodyId (eprLeftTyExp plan)]
                (Just (eprSchemeOwnerGen plan))
                (Just (rteExpVar (eprLeftTyExp plan)))
                (rootOwnersForPlan (ewRootOwnership worklist) plan)
                worklist
        core' = Indexed.markIndexedWorkItemClean edgeKey (ewCore worklist')
    in worklist'
        { ewCore = core'
        , ewPlannedEdges = IntSet.insert edgeKey (ewPlannedEdges worklist')
        }
  where
    edgeKey = getEdgeId (instEdgeId (eprEdge plan))

noteProcessedEdge
    :: InstEdge
    -> Maybe (EdgePlanSeed, EdgeFingerprint)
    -> EdgeWorklist
    -> EdgeWorklist
noteProcessedEdge edge mbFacts worklist =
    case mbFacts of
        Nothing -> clearStaleEdge edgeKey (updateStoredItem clearedItem worklist)
        Just (seed, fingerprint) ->
            let processedItem = baseItem
                    { ewiPlanSeed = Just seed
                    , ewiFingerprint = Just fingerprint
                    , ewiRootOwners = rootOwners
                    , ewiStale = False
                    }
            in
            indexEdge
                edgeKey
                [epsLeftRoot seed, epsRightRoot seed, epsBodyRoot seed]
                (Just (epsSchemeOwnerGen seed))
                (Just (epsExpansionVar seed))
                rootOwners
                (clearStaleEdge edgeKey (updateStoredItem processedItem worklist))
  where
    edgeKey = getEdgeId (instEdgeId edge)
    rootOwners =
        case mbFacts of
            Nothing -> ownersForEdge (ewRootOwnership worklist) edgeKey
            Just (seed, _) -> rootOwnersForSeed (ewRootOwnership worklist) edge seed
    baseItem =
        case Indexed.lookupIndexedWorkItem edgeKey (ewCore worklist) of
            Just item -> Indexed.iwiValue item
            Nothing ->
                EdgeWorkItem
                    { ewiEdge = edge
                    , ewiPlanSeed = Nothing
                    , ewiFingerprint = Nothing
                    , ewiRootOwners = rootOwners
                    , ewiStale = False
                    }
    clearedItem = baseItem { ewiStale = False }

noteInertEdge :: EdgeWorkItem -> EdgeWorklist -> EdgeWorklist
noteInertEdge item worklist =
    let edgeKey = getEdgeId (instEdgeId (ewiEdge item))
        item' = item { ewiStale = False }
    in clearStaleEdge edgeKey (updateStoredItem item' worklist)

invalidateRoots :: IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateRoots = invalidateRootsExcept IntSet.empty

invalidateRootsExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateRootsExcept excluded roots worklist =
    let (invalidation, core') =
            Indexed.invalidateIndexedKeysExcept excluded rootIndex roots (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = roots
            , wiOwners = IntSet.empty
            , wiExpansions = IntSet.empty
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

invalidateRootsWithinRootOwnersExcept :: IntSet -> IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateRootsWithinRootOwnersExcept excluded roots rootOwners worklist
    | IntSet.null rootOwners = invalidateRootsExcept excluded roots worklist
    | otherwise =
        let (invalidation, core') =
                Indexed.invalidateIndexedKeysExceptWithin excluded rootIndex roots rootOwnerIndex rootOwners (ewCore worklist)
        in ( WorklistInvalidation
                { wiRoots = roots
                , wiOwners = IntSet.empty
                , wiExpansions = IntSet.empty
                , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
                }
           , worklist { ewCore = core' }
           )

invalidateQueuedRootsExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateQueuedRootsExcept excluded roots worklist =
    let (invalidation, core') =
            Indexed.invalidateQueuedIndexedKeysExcept excluded rootIndex roots (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = roots
            , wiOwners = IntSet.empty
            , wiExpansions = IntSet.empty
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

invalidateOwners :: IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateOwners = invalidateOwnersExcept IntSet.empty

invalidateOwnersExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateOwnersExcept excluded owners worklist =
    let (invalidation, core') =
            Indexed.invalidateIndexedKeysExcept excluded ownerIndex owners (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = IntSet.empty
            , wiOwners = owners
            , wiExpansions = IntSet.empty
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

invalidateOwnersWithinRootOwnersExcept :: IntSet -> IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateOwnersWithinRootOwnersExcept excluded owners rootOwners worklist
    | IntSet.null rootOwners = invalidateOwnersExcept excluded owners worklist
    | otherwise =
        let (invalidation, core') =
                Indexed.invalidateIndexedKeysExceptWithin excluded ownerIndex owners rootOwnerIndex rootOwners (ewCore worklist)
        in ( WorklistInvalidation
                { wiRoots = IntSet.empty
                , wiOwners = owners
                , wiExpansions = IntSet.empty
                , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
                }
           , worklist { ewCore = core' }
           )

invalidateQueuedOwnersExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateQueuedOwnersExcept excluded owners worklist =
    let (invalidation, core') =
            Indexed.invalidateQueuedIndexedKeysExcept excluded ownerIndex owners (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = IntSet.empty
            , wiOwners = owners
            , wiExpansions = IntSet.empty
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

invalidateExpansions :: IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateExpansions = invalidateExpansionsExcept IntSet.empty

invalidateExpansionsExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateExpansionsExcept excluded expansions worklist =
    let (invalidation, core') =
            Indexed.invalidateIndexedKeysExcept excluded expansionIndex expansions (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = IntSet.empty
            , wiOwners = IntSet.empty
            , wiExpansions = expansions
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

invalidateExpansionsWithinRootOwnersExcept :: IntSet -> IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateExpansionsWithinRootOwnersExcept excluded expansions rootOwners worklist
    | IntSet.null rootOwners = invalidateExpansionsExcept excluded expansions worklist
    | otherwise =
        let (invalidation, core') =
                Indexed.invalidateIndexedKeysExceptWithin excluded expansionIndex expansions rootOwnerIndex rootOwners (ewCore worklist)
        in ( WorklistInvalidation
                { wiRoots = IntSet.empty
                , wiOwners = IntSet.empty
                , wiExpansions = expansions
                , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
                }
           , worklist { ewCore = core' }
           )

invalidateQueuedExpansionsExcept :: IntSet -> IntSet -> EdgeWorklist -> (WorklistInvalidation, EdgeWorklist)
invalidateQueuedExpansionsExcept excluded expansions worklist =
    let (invalidation, core') =
            Indexed.invalidateQueuedIndexedKeysExcept excluded expansionIndex expansions (ewCore worklist)
    in ( WorklistInvalidation
            { wiRoots = IntSet.empty
            , wiOwners = IntSet.empty
            , wiExpansions = expansions
            , wiEdges = Indexed.iwiInvalidatedItemKeys invalidation
            }
       , worklist { ewCore = core' }
       )

queuedEdgeCount :: EdgeWorklist -> Int
queuedEdgeCount = Indexed.queuedIndexedItemCount . ewCore

indexedEdgeCount :: EdgeWorklist -> Int
indexedEdgeCount = IntSet.size . ewPlannedEdges

rootOwnershipOfWorklist :: EdgeWorklist -> RootOwnershipIndex
rootOwnershipOfWorklist = ewRootOwnership

rootOwnersForPlan :: RootOwnershipIndex -> EdgePlan -> IntSet
rootOwnersForPlan ownership plan =
    rootOwnersForSeed ownership (eprEdge plan) seed
  where
    leftTyExp = eprLeftTyExp plan
    seed =
        EdgePlanSeed
            { epsLeftTyExp = leftTyExp
            , epsLeftRoot = eprLeftCanonical plan
            , epsRightRoot = eprRightCanonical plan
            , epsBodyRoot = rteBodyId leftTyExp
            , epsSchemeOwnerGen = eprSchemeOwnerGen plan
            , epsExpansionVar = rteExpVar leftTyExp
            }

indexEdge
    :: Int
    -> [NodeId]
    -> Maybe GenNodeId
    -> Maybe ExpVarId
    -> IntSet
    -> EdgeWorklist
    -> EdgeWorklist
indexEdge edgeKey roots mbOwner mbExpVar rootOwners worklist =
    worklist
        { ewCore =
            indexRootOwners $
                indexExpansion $
                    indexOwner $
                        Indexed.indexWorkItemKeyList
                            edgeKey
                            rootIndex
                            (map getNodeId roots)
                            (ewCore worklist)
        }
  where
    indexOwner core =
        case mbOwner of
            Nothing -> core
            Just owner ->
                Indexed.indexWorkItemKey edgeKey ownerIndex (getGenNodeId owner) core

    indexExpansion core =
        case mbExpVar of
            Nothing -> core
            Just expVar ->
                Indexed.indexWorkItemKey edgeKey expansionIndex (getExpVarId expVar) core

    indexRootOwners core =
        Indexed.indexWorkItemKeys edgeKey rootOwnerIndex rootOwners core

rootOwnersForSeed :: RootOwnershipIndex -> InstEdge -> EdgePlanSeed -> IntSet
rootOwnersForSeed ownership edge seed =
    IntSet.unions
        [ ownersForEdge ownership (getEdgeId (instEdgeId edge))
        , ownersForNodes ownership $
            IntSet.fromList
                [ getNodeId (epsLeftRoot seed)
                , getNodeId (epsRightRoot seed)
                , getNodeId (epsBodyRoot seed)
                , getNodeId (instLeft edge)
                , getNodeId (instRight edge)
                ]
        , ownersForGens ownership (IntSet.singleton (getGenNodeId (epsSchemeOwnerGen seed)))
        , ownersForExpVars ownership (IntSet.singleton (getExpVarId (epsExpansionVar seed)))
        ]

updateStoredItem :: EdgeWorkItem -> EdgeWorklist -> EdgeWorklist
updateStoredItem item worklist =
    let edgeKey = getEdgeId (instEdgeId (ewiEdge item))
        stored =
            Indexed.IndexedWorkItem
                { Indexed.iwiKey = edgeKey
                , Indexed.iwiValue = item
                , Indexed.iwiStale = False
                }
    in worklist { ewCore = Indexed.updateIndexedWorkItem stored (ewCore worklist) }

clearStaleEdge :: Int -> EdgeWorklist -> EdgeWorklist
clearStaleEdge edgeKey worklist =
    worklist { ewCore = Indexed.markIndexedWorkItemClean edgeKey (ewCore worklist) }

edgeKeyOfItem :: EdgeWorkItem -> Int
edgeKeyOfItem = getEdgeId . instEdgeId . ewiEdge

rootIndex :: Indexed.WorklistIndex
rootIndex = Indexed.WorklistIndex 0

ownerIndex :: Indexed.WorklistIndex
ownerIndex = Indexed.WorklistIndex 1

expansionIndex :: Indexed.WorklistIndex
expansionIndex = Indexed.WorklistIndex 2

rootOwnerIndex :: Indexed.WorklistIndex
rootOwnerIndex = Indexed.WorklistIndex 3
