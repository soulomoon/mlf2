{- |
Module      : MLF.Util.IndexedWorklist
Description : Small indexed worklist core shared by incremental passes
Copyright   : (c) 2024
License     : BSD-3-Clause

This module owns the reusable mechanics of an incremental worklist:

* a FIFO-ish queue of keyed work items,
* a stored item table used for future requeues,
* a stale set observed when an item is popped, and
* one or more secondary indexes from domain keys to work item keys.

It deliberately does not encode what it means to process, refresh, or replay an
item. Solver-specific modules should keep those semantics local and use this
module only for scheduling and invalidation.
-}
module MLF.Util.IndexedWorklist
    ( WorklistIndex(..)
    , WorklistIndexEntry(..)
    , IndexedWorkItem(..)
    , IndexedWorklist
    , IndexedWorklistInvalidation(..)
    , buildIndexedWorklist
    , popIndexedWorkItem
    , lookupIndexedWorkItem
    , updateIndexedWorkItem
    , markIndexedWorkItemClean
    , indexWorkItem
    , indexWorkItemKey
    , indexWorkItemKeyList
    , indexWorkItemKeys
    , invalidateIndexedKeysExcept
    , invalidateIndexedKeysExceptWithin
    , invalidateQueuedIndexedKeysExcept
    , queuedIndexedItemCount
    ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL((:<)))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

newtype WorklistIndex = WorklistIndex { getWorklistIndex :: Int }
    deriving (Eq, Ord, Show)

data WorklistIndexEntry = WorklistIndexEntry
    { wieIndex :: !WorklistIndex
    , wieKeys :: !IntSet
    }
    deriving (Eq, Show)

data IndexedWorkItem a = IndexedWorkItem
    { iwiKey :: !Int
    , iwiValue :: !a
    , iwiStale :: !Bool
    }
    deriving (Eq, Show)

data IndexedWorklist a = IndexedWorklist
    { iwQueue :: !(Seq (IndexedWorkItem a))
    , iwQueuedKeys :: !IntSet
    , iwItemsByKey :: !(IntMap (IndexedWorkItem a))
    , iwStaleKeys :: !IntSet
    , iwIndexes :: !(IntMap (IntMap IntSet))
    }
    deriving (Eq, Show)

data IndexedWorklistInvalidation = IndexedWorklistInvalidation
    { iwiInvalidatedIndexKeys :: !IntSet
    , iwiInvalidatedItemKeys :: !IntSet
    }
    deriving (Eq, Show)

buildIndexedWorklist :: [(Int, a)] -> IndexedWorklist a
buildIndexedWorklist keyedItems =
    IndexedWorklist
        { iwQueue = Seq.fromList items
        , iwQueuedKeys = IntSet.fromList (map fst keyedItems)
        , iwItemsByKey =
            IntMap.fromList
                [ (key, item)
                | item@IndexedWorkItem{ iwiKey = key } <- items
                ]
        , iwStaleKeys = IntSet.empty
        , iwIndexes = IntMap.empty
        }
  where
    items =
        [ IndexedWorkItem
            { iwiKey = key
            , iwiValue = value
            , iwiStale = False
            }
        | (key, value) <- keyedItems
        ]

{-# INLINE popIndexedWorkItem #-}
popIndexedWorkItem :: IndexedWorklist a -> Maybe (IndexedWorkItem a, IndexedWorklist a)
popIndexedWorkItem worklist =
    case Seq.viewl (iwQueue worklist) of
        Seq.EmptyL -> Nothing
        item :< rest ->
            let key = iwiKey item
                stale = IntSet.member key (iwStaleKeys worklist)
            in Just
                ( item { iwiStale = stale }
                , worklist
                    { iwQueue = rest
                    , iwQueuedKeys = IntSet.delete key (iwQueuedKeys worklist)
                    }
                )

lookupIndexedWorkItem :: Int -> IndexedWorklist a -> Maybe (IndexedWorkItem a)
lookupIndexedWorkItem key worklist =
    IntMap.lookup key (iwItemsByKey worklist)

updateIndexedWorkItem :: IndexedWorkItem a -> IndexedWorklist a -> IndexedWorklist a
updateIndexedWorkItem item worklist =
    worklist { iwItemsByKey = IntMap.insert (iwiKey item) item (iwItemsByKey worklist) }

markIndexedWorkItemClean :: Int -> IndexedWorklist a -> IndexedWorklist a
markIndexedWorkItemClean key worklist =
    worklist { iwStaleKeys = IntSet.delete key (iwStaleKeys worklist) }

indexWorkItem :: Int -> [WorklistIndexEntry] -> IndexedWorklist a -> IndexedWorklist a
indexWorkItem itemKey entries worklist =
    worklist
        { iwIndexes =
            foldl'
                insertEntry
                (iwIndexes worklist)
                entries
        }
  where
    insertEntry indexes WorklistIndexEntry{ wieIndex = WorklistIndex indexKey, wieKeys = keys } =
        IntSet.foldl'
            (\acc key -> insertIndexedItem indexKey key acc)
            indexes
            keys

    insertIndexedItem indexKey key indexes =
        IntMap.alter
            (Just . insertIntoNamespace)
            indexKey
            indexes
      where
        insertIntoNamespace Nothing =
            IntMap.singleton key (IntSet.singleton itemKey)
        insertIntoNamespace (Just namespace) =
            IntMap.insertWith IntSet.union key (IntSet.singleton itemKey) namespace

indexWorkItemKey :: Int -> WorklistIndex -> Int -> IndexedWorklist a -> IndexedWorklist a
indexWorkItemKey itemKey (WorklistIndex indexKey) key worklist =
    worklist
        { iwIndexes = insertIndexedItem indexKey key (iwIndexes worklist)
        }
  where
    insertIndexedItem indexName key0 indexes =
        IntMap.alter
            (Just . insertIntoNamespace)
            indexName
            indexes
      where
        insertIntoNamespace Nothing =
            IntMap.singleton key0 (IntSet.singleton itemKey)
        insertIntoNamespace (Just namespace) =
            IntMap.insertWith IntSet.union key0 (IntSet.singleton itemKey) namespace

indexWorkItemKeys :: Int -> WorklistIndex -> IntSet -> IndexedWorklist a -> IndexedWorklist a
indexWorkItemKeys itemKey indexName keys worklist =
    IntSet.foldl'
        (\acc key -> indexWorkItemKey itemKey indexName key acc)
        worklist
        keys

indexWorkItemKeyList :: Int -> WorklistIndex -> [Int] -> IndexedWorklist a -> IndexedWorklist a
indexWorkItemKeyList itemKey indexName keys worklist =
    foldl'
        (\acc key -> indexWorkItemKey itemKey indexName key acc)
        worklist
        keys

invalidateIndexedKeysExcept
    :: IntSet
    -> WorklistIndex
    -> IntSet
    -> IndexedWorklist a
    -> (IndexedWorklistInvalidation, IndexedWorklist a)
invalidateIndexedKeysExcept excluded indexName keys worklist =
    let affected = IntSet.difference (itemsFor indexName keys worklist) excluded
    in ( IndexedWorklistInvalidation
            { iwiInvalidatedIndexKeys = keys
            , iwiInvalidatedItemKeys = affected
            }
       , markStaleAndRequeue affected worklist
       )

invalidateIndexedKeysExceptWithin
    :: IntSet
    -> WorklistIndex
    -> IntSet
    -> WorklistIndex
    -> IntSet
    -> IndexedWorklist a
    -> (IndexedWorklistInvalidation, IndexedWorklist a)
invalidateIndexedKeysExceptWithin excluded indexName keys withinIndex withinKeys worklist =
    let affected =
            IntSet.difference
                (IntSet.intersection (itemsFor indexName keys worklist) (itemsFor withinIndex withinKeys worklist))
                excluded
    in ( IndexedWorklistInvalidation
            { iwiInvalidatedIndexKeys = keys
            , iwiInvalidatedItemKeys = affected
            }
       , markStaleAndRequeue affected worklist
       )

invalidateQueuedIndexedKeysExcept
    :: IntSet
    -> WorklistIndex
    -> IntSet
    -> IndexedWorklist a
    -> (IndexedWorklistInvalidation, IndexedWorklist a)
invalidateQueuedIndexedKeysExcept excluded indexName keys worklist =
    let affected =
            IntSet.intersection
                (iwQueuedKeys worklist)
                (IntSet.difference (itemsFor indexName keys worklist) excluded)
    in ( IndexedWorklistInvalidation
            { iwiInvalidatedIndexKeys = keys
            , iwiInvalidatedItemKeys = affected
            }
       , markStale affected worklist
       )

queuedIndexedItemCount :: IndexedWorklist a -> Int
queuedIndexedItemCount = Seq.length . iwQueue

{-# INLINE itemsFor #-}
itemsFor :: WorklistIndex -> IntSet -> IndexedWorklist a -> IntSet
itemsFor (WorklistIndex indexName) keys worklist =
    case IntMap.lookup indexName (iwIndexes worklist) of
        Nothing -> IntSet.empty
        Just index ->
            IntSet.foldl'
                (\acc key -> IntSet.union acc (IntMap.findWithDefault IntSet.empty key index))
                IntSet.empty
                keys

markStale :: IntSet -> IndexedWorklist a -> IndexedWorklist a
markStale affected worklist
    | IntSet.null affected = worklist
    | otherwise =
        worklist { iwStaleKeys = IntSet.union affected (iwStaleKeys worklist) }

{-# INLINABLE markStaleAndRequeue #-}
markStaleAndRequeue :: IntSet -> IndexedWorklist a -> IndexedWorklist a
markStaleAndRequeue affected worklist
    | IntSet.null affected = worklist
    | otherwise =
        let toQueue = IntSet.difference affected (iwQueuedKeys worklist)
            queue' =
                IntSet.foldl'
                    enqueueStoredItem
                    (iwQueue worklist)
                    toQueue
        in worklist
            { iwQueue = queue'
            , iwQueuedKeys = IntSet.union (iwQueuedKeys worklist) toQueue
            , iwStaleKeys = IntSet.union affected (iwStaleKeys worklist)
            }
  where
    enqueueStoredItem queue key =
        case IntMap.lookup key (iwItemsByKey worklist) of
            Nothing -> queue
            Just item -> queue Seq.|> item
