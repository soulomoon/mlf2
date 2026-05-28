module MLF.Constraint.RootOwnership
    ( ModuleRootId(..)
    , RootOwnershipIndex(..)
    , emptyRootOwnershipIndex
    , ensureRootOwner
    , insertNodeOwner
    , insertGenOwner
    , insertExpVarOwner
    , insertEdgeOwner
    , ownersForNode
    , ownersForGen
    , ownersForExpVar
    , ownersForEdge
    , ownersForNodes
    , ownersForGens
    , ownersForExpVars
    , ownersForEdges
    , rootOwnershipRootCount
    , rootOwnershipOwnedNodeCount
    , rootOwnershipOwnedGenCount
    , rootOwnershipOwnedExpVarCount
    , rootOwnershipOwnedEdgeCount
    , rootOwnershipOwnedEdgeCounts
    , rootOwnershipSharedEdgeCount
    ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

newtype ModuleRootId = ModuleRootId {getModuleRootId :: Int}
    deriving (Eq, Ord, Show)

data RootOwnershipIndex = RootOwnershipIndex
    { roiRoots :: !IntSet
    , roiNodeOwners :: !(IntMap IntSet)
    , roiGenOwners :: !(IntMap IntSet)
    , roiExpVarOwners :: !(IntMap IntSet)
    , roiEdgeOwners :: !(IntMap IntSet)
    }
    deriving (Eq, Show)

emptyRootOwnershipIndex :: RootOwnershipIndex
emptyRootOwnershipIndex =
    RootOwnershipIndex
        { roiRoots = IntSet.empty
        , roiNodeOwners = IntMap.empty
        , roiGenOwners = IntMap.empty
        , roiExpVarOwners = IntMap.empty
        , roiEdgeOwners = IntMap.empty
    }

ensureRootOwner :: ModuleRootId -> RootOwnershipIndex -> RootOwnershipIndex
ensureRootOwner owner index =
    index {roiRoots = IntSet.insert (getModuleRootId owner) (roiRoots index)}

insertNodeOwner :: ModuleRootId -> Int -> RootOwnershipIndex -> RootOwnershipIndex
insertNodeOwner = insertOwnerInto roiNodeOwners $ \owners index -> index {roiNodeOwners = owners}

insertGenOwner :: ModuleRootId -> Int -> RootOwnershipIndex -> RootOwnershipIndex
insertGenOwner = insertOwnerInto roiGenOwners $ \owners index -> index {roiGenOwners = owners}

insertExpVarOwner :: ModuleRootId -> Int -> RootOwnershipIndex -> RootOwnershipIndex
insertExpVarOwner = insertOwnerInto roiExpVarOwners $ \owners index -> index {roiExpVarOwners = owners}

insertEdgeOwner :: ModuleRootId -> Int -> RootOwnershipIndex -> RootOwnershipIndex
insertEdgeOwner = insertOwnerInto roiEdgeOwners $ \owners index -> index {roiEdgeOwners = owners}

insertOwnerInto ::
    (RootOwnershipIndex -> IntMap IntSet) ->
    (IntMap IntSet -> RootOwnershipIndex -> RootOwnershipIndex) ->
    ModuleRootId ->
    Int ->
    RootOwnershipIndex ->
    RootOwnershipIndex
insertOwnerInto getOwners setOwners owner key index =
    let ownerKey = getModuleRootId owner
        owners = IntMap.insertWith IntSet.union key (IntSet.singleton ownerKey) (getOwners index)
     in setOwners owners (ensureRootOwner owner index)

ownersForNode :: RootOwnershipIndex -> Int -> IntSet
ownersForNode index key = IntMap.findWithDefault IntSet.empty key (roiNodeOwners index)

ownersForGen :: RootOwnershipIndex -> Int -> IntSet
ownersForGen index key = IntMap.findWithDefault IntSet.empty key (roiGenOwners index)

ownersForExpVar :: RootOwnershipIndex -> Int -> IntSet
ownersForExpVar index key = IntMap.findWithDefault IntSet.empty key (roiExpVarOwners index)

ownersForEdge :: RootOwnershipIndex -> Int -> IntSet
ownersForEdge index key = IntMap.findWithDefault IntSet.empty key (roiEdgeOwners index)

ownersForNodes :: RootOwnershipIndex -> IntSet -> IntSet
ownersForNodes index = ownersForKeys (ownersForNode index)

ownersForGens :: RootOwnershipIndex -> IntSet -> IntSet
ownersForGens index = ownersForKeys (ownersForGen index)

ownersForExpVars :: RootOwnershipIndex -> IntSet -> IntSet
ownersForExpVars index = ownersForKeys (ownersForExpVar index)

ownersForEdges :: RootOwnershipIndex -> IntSet -> IntSet
ownersForEdges index = ownersForKeys (ownersForEdge index)

ownersForKeys :: (Int -> IntSet) -> IntSet -> IntSet
ownersForKeys lookupOwners =
    IntSet.foldl' (\acc key -> IntSet.union acc (lookupOwners key)) IntSet.empty

rootOwnershipRootCount :: RootOwnershipIndex -> Int
rootOwnershipRootCount = IntSet.size . roiRoots

rootOwnershipOwnedNodeCount :: RootOwnershipIndex -> Int
rootOwnershipOwnedNodeCount = IntMap.size . roiNodeOwners

rootOwnershipOwnedGenCount :: RootOwnershipIndex -> Int
rootOwnershipOwnedGenCount = IntMap.size . roiGenOwners

rootOwnershipOwnedExpVarCount :: RootOwnershipIndex -> Int
rootOwnershipOwnedExpVarCount = IntMap.size . roiExpVarOwners

rootOwnershipOwnedEdgeCount :: RootOwnershipIndex -> Int
rootOwnershipOwnedEdgeCount = IntMap.size . roiEdgeOwners

rootOwnershipOwnedEdgeCounts :: RootOwnershipIndex -> IntMap Int
rootOwnershipOwnedEdgeCounts index =
    IntMap.foldl' addEdgeOwners IntMap.empty (roiEdgeOwners index)
  where
    addEdgeOwners counts owners =
        IntSet.foldl' (\acc root -> IntMap.insertWith (+) root 1 acc) counts owners

rootOwnershipSharedEdgeCount :: RootOwnershipIndex -> Int
rootOwnershipSharedEdgeCount index =
    length
        [ ()
        | owners <- IntMap.elems (roiEdgeOwners index)
        , IntSet.size owners > 1
        ]
