{- |
Module      : MLF.Util.IntMapUtils
Description : Utility functions for IntMap operations on constraint structures
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides utility functions for common IntMap iteration patterns,
particularly for bind parent operations that appear frequently across the codebase.

= Design Rationale

Previously, 75+ locations duplicated patterns like:
@
[(childKey, (parent, flag)) | (childKey, (parent, flag)) <- IntMap.toList bindParents, ...]
@

This module centralizes these patterns, making code more readable and reducing
duplication by ~150-200 lines across the codebase.

= Common Patterns

* Filtering bind parents by parent node
* Collecting children with specific flags
* Building reverse maps (parent -> children)
* Iterating with predicates
-}
module MLF.Util.IntMapUtils (
    -- * Bind parent utilities
    childrenOf,
    childrenOfWithFlag,
    filterBindParents,
    filterBindParentsBy,
    forBindParents,
    forBindParentsMaybe,
    -- * Type-specific child queries
    typeChildrenOf,
    typeChildrenOfWithFlag,
    flexTypeChildrenOf,
    typeChildrenWithFlagOf,
    typeChildrenWithKeyOf,
    flexTypeChildrenWithKeyOf,
    -- * Gen-parent queries
    flexTypeChildrenOfGen,
    rigidTypeChildrenOfGen,
    typeChildrenOfGenWithFlag,
    typeChildrenOfGen,
    -- * Bind flag utilities
    allBindFlags,
    rigidTypeChildren,
    childrenWithTypeParent,
    typeParentNodes,
    typeChildNodes,
    allTypeChildrenWithKey,
    -- * Reverse mapping
    reverseBindParents,
    reverseBindParentsWithFlag,
    -- * Generic IntMap utilities
    filterMapWithKey,
    collectValues,
    keepOld
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)

import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindParents
    , GenNodeId
    , NodeId
    , NodeRef(..)
    , nodeRefFromKey
    , nodeRefKey
    )

-- -----------------------------------------------------------------------------
-- Bind parent utilities
-- -----------------------------------------------------------------------------

-- | Get all children of a specific parent node.
--
-- Returns a list of (childKey, flag) pairs for all nodes that have the given
-- parent in the bind parents map.
childrenOf :: BindParents -> NodeRef -> [(Int, BindFlag)]
childrenOf bindParents parentRef =
    [ (childKey, flag)
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    ]

-- | Get children of a specific parent node with a specific binding flag.
--
-- Returns a list of child keys.
childrenOfWithFlag :: BindParents -> NodeRef -> BindFlag -> [Int]
childrenOfWithFlag bindParents parentRef targetFlag =
    [ childKey
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , flag == targetFlag
    ]

-- | Filter bind parents by a predicate on (childKey, parent, flag).
filterBindParents
    :: (Int -> NodeRef -> BindFlag -> Bool)
    -> BindParents
    -> BindParents
filterBindParents predicate bindParents =
    IntMap.filterWithKey
        (\childKey (parent, flag) -> predicate childKey parent flag)
        bindParents

-- | Filter bind parents and extract values.
--
-- Similar to filterBindParents but allows transforming the result.
filterBindParentsBy
    :: (Int -> NodeRef -> BindFlag -> Maybe a)
    -> BindParents
    -> [a]
filterBindParentsBy f bindParents =
    [ result
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , Just result <- [f childKey parent flag]
    ]

-- | Map over bind parents with access to child key, parent, and flag.
--
-- This is the most general form for processing bind parents.
forBindParents
    :: BindParents
    -> (Int -> NodeRef -> BindFlag -> a)
    -> [a]
forBindParents bindParents f =
    [ f childKey parent flag
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    ]

-- | Map over bind parents with Maybe result (filters out Nothings).
forBindParentsMaybe
    :: BindParents
    -> (Int -> NodeRef -> BindFlag -> Maybe a)
    -> [a]
forBindParentsMaybe bindParents f = filterBindParentsBy f bindParents

-- -----------------------------------------------------------------------------
-- Type-specific child queries
-- -----------------------------------------------------------------------------

-- | Get type node children of a specific parent.
--
-- Returns NodeIds for all type nodes (not gen nodes) that have the given parent.
typeChildrenOf :: BindParents -> NodeRef -> [NodeId]
typeChildrenOf bindParents parentRef =
    [ child
    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get type node children of a specific parent with a specific flag.
typeChildrenOfWithFlag :: BindParents -> NodeRef -> BindFlag -> [NodeId]
typeChildrenOfWithFlag bindParents parentRef targetFlag =
    [ child
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , flag == targetFlag
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get flexibly-bound type node children of a specific parent.
--
-- This is a common pattern: finding all type variables that are flexibly
-- bound to a given parent (for binder selection).
flexTypeChildrenOf :: BindParents -> NodeRef -> [NodeId]
flexTypeChildrenOf bindParents parentRef =
    typeChildrenOfWithFlag bindParents parentRef BindFlex

-- | Get type node children with their flags.
--
-- Returns (NodeId, BindFlag) pairs for all type nodes under the given parent.
typeChildrenWithFlagOf :: BindParents -> NodeRef -> [(NodeId, BindFlag)]
typeChildrenWithFlagOf bindParents parentRef =
    [ (child, flag)
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get type node children with their raw IntMap keys.
--
-- Returns (childKey, NodeId) pairs. The childKey is the raw IntMap key,
-- which is needed when the caller uses the key for additional lookups.
typeChildrenWithKeyOf :: BindParents -> NodeRef -> [(Int, NodeId)]
typeChildrenWithKeyOf bindParents parentRef =
    [ (childKey, child)
    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get flexibly-bound type node children with their raw IntMap keys.
--
-- Returns (childKey, NodeId) pairs for flex-bound children.
flexTypeChildrenWithKeyOf :: BindParents -> NodeRef -> [(Int, NodeId)]
flexTypeChildrenWithKeyOf bindParents parentRef =
    [ (childKey, child)
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , flag == BindFlex
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- -----------------------------------------------------------------------------
-- Gen-parent queries
-- -----------------------------------------------------------------------------

-- | Get flexibly-bound type children of a gen node.
--
-- Returns (GenNodeId, NodeId) pairs for all type nodes that are flexibly
-- bound to any gen node parent.
flexTypeChildrenOfGen :: BindParents -> [(GenNodeId, NodeId)]
flexTypeChildrenOfGen bindParents =
    [ (gid, child)
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , flag == BindFlex
    , GenRef gid <- [parent]
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get rigidly-bound type children of a gen node.
--
-- Returns (GenNodeId, NodeId) pairs for all type nodes that are rigidly
-- bound to any gen node parent.
rigidTypeChildrenOfGen :: BindParents -> [(GenNodeId, NodeId)]
rigidTypeChildrenOfGen bindParents =
    [ (gid, child)
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , flag == BindRigid
    , GenRef gid <- [parent]
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get type children of a specific gen node with a specific flag.
typeChildrenOfGenWithFlag :: BindParents -> GenNodeId -> BindFlag -> [NodeId]
typeChildrenOfGenWithFlag bindParents gid targetFlag =
    [ child
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == GenRef gid
    , flag == targetFlag
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- -----------------------------------------------------------------------------
-- Reverse mapping
-- -----------------------------------------------------------------------------

-- | Build a reverse map from parent nodes to their children.
--
-- Returns an IntMap from parent key to the set of child keys.
-- Useful for efficiently finding all children of a given parent.
reverseBindParents :: BindParents -> IntMap IntSet
reverseBindParents bindParents =
    IntMap.fromListWith IntSet.union
        [ (nodeRefKey parent, IntSet.singleton childKey)
        | (childKey, (parent, _flag)) <- IntMap.toList bindParents
        ]

-- | Build a reverse map, filtering by binding flag.
--
-- Only includes children with the specified flag.
reverseBindParentsWithFlag :: BindFlag -> BindParents -> IntMap IntSet
reverseBindParentsWithFlag targetFlag bindParents =
    IntMap.fromListWith IntSet.union
        [ (nodeRefKey parent, IntSet.singleton childKey)
        | (childKey, (parent, flag)) <- IntMap.toList bindParents
        , flag == targetFlag
        ]

-- -----------------------------------------------------------------------------
-- Bind flag utilities
-- -----------------------------------------------------------------------------

-- | Extract all bind flags as an IntMap from childKey to flag.
--
-- This is useful when you need to look up flags by child key.
-- Replaces: @IntMap.fromList [(childKey, flag) | (childKey, (_parent, flag)) <- IntMap.toList bindParents]@
allBindFlags :: BindParents -> IntMap BindFlag
allBindFlags bindParents =
    IntMap.map snd bindParents

-- | Get all type children of a specific gen node (any flag).
--
-- Returns NodeIds for all type nodes bound to the given gen node,
-- regardless of binding flag.
typeChildrenOfGen :: BindParents -> GenNodeId -> [NodeId]
typeChildrenOfGen bindParents gid =
    [ child
    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
    , parent == GenRef gid
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get all rigidly-bound type children (any parent).
--
-- Returns NodeIds for all type nodes that are rigidly bound,
-- regardless of what they're bound to.
rigidTypeChildren :: BindParents -> [NodeId]
rigidTypeChildren bindParents =
    [ child
    | (childKey, (_parent, flag)) <- IntMap.toList bindParents
    , flag == BindRigid
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get children where the parent is a TypeRef.
--
-- Returns (childKey, parentNodeId, flag) for all entries where the parent
-- is a type node (not a gen node).
childrenWithTypeParent :: BindParents -> [(Int, NodeId, BindFlag)]
childrenWithTypeParent bindParents =
    [ (childKey, parent, flag)
    | (childKey, (parentRef, flag)) <- IntMap.toList bindParents
    , TypeRef parent <- [parentRef]
    ]

-- | Get all type parent keys from bind parents.
--
-- Returns the NodeId of all parents that are TypeRefs.
typeParentNodes :: BindParents -> [NodeId]
typeParentNodes bindParents =
    [ parent
    | (_childKey, (TypeRef parent, _flag)) <- IntMap.toList bindParents
    ]

-- | Get all type child nodes from bind parents.
--
-- Returns the NodeId of all children that are TypeRefs.
typeChildNodes :: BindParents -> [NodeId]
typeChildNodes bindParents =
    [ child
    | (childKey, _parent) <- IntMap.toList bindParents
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- | Get all type child nodes with their raw IntMap keys.
--
-- Returns (childKey, NodeId) pairs for all type children, regardless of parent.
allTypeChildrenWithKey :: BindParents -> [(Int, NodeId)]
allTypeChildrenWithKey bindParents =
    [ (childKey, child)
    | (childKey, _parent) <- IntMap.toList bindParents
    , TypeRef child <- [nodeRefFromKey childKey]
    ]

-- -----------------------------------------------------------------------------
-- Generic IntMap utilities
-- -----------------------------------------------------------------------------

-- | Filter and map an IntMap in a single pass.
--
-- Combines filterWithKey and mapWithKey for efficiency.
filterMapWithKey
    :: (Int -> a -> Maybe b)
    -> IntMap a
    -> IntMap b
filterMapWithKey f m =
    IntMap.mapMaybeWithKey f m

-- | Collect all values from a map that satisfy a predicate.
collectValues
    :: (Int -> a -> Maybe b)
    -> IntMap a
    -> [b]
collectValues f m =
    [ result
    | (key, value) <- IntMap.toList m
    , Just result <- [f key value]
    ]

-- | Merge strategy for IntMap.union/insertWith that prefers old values.
--
-- Use with 'IntMap.unionWith' or 'IntMap.insertWith' when you want to
-- keep existing values and ignore new ones:
-- @
--   IntMap.unionWith keepOld map1 map2
--   IntMap.insertWith keepOld key value map
-- @
keepOld :: a -> a -> a
keepOld _ old = old
