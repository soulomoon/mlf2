{-# LANGUAGE BangPatterns #-}
module MLF.Binding.Path (
    bindingPathToRootWithLookup,
    bindingPathToRoot,
    bindingPathToRootLocal,
    validateBindingPathsAcyclicWithLookup,
    validateBindingPathsAcyclic,
    firstGenAncestorFromPath,
) where

import Control.Monad (foldM, void)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types.Graph

bindingPathToRootWithLookup
    :: (Int -> Maybe (NodeRef, BindFlag))
    -> NodeRef
    -> Either BindingError [NodeRef]
bindingPathToRootWithLookup lookupParent start =
    go IntSet.empty [start] (nodeRefKey start)
  where
    go seen path key
        | IntSet.member key seen =
            Left (BindingCycleDetected (reverse path))
        | otherwise =
            case lookupParent key of
                Nothing -> Right (reverse path)
                Just (parentRef, _flag) ->
                    go (IntSet.insert key seen) (parentRef : path) (nodeRefKey parentRef)

bindingPathToRoot :: Constraint p -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRoot c =
    bindingPathToRootWithLookup (\key -> IntMap.lookup key (cBindParents c))

bindingPathToRootLocal :: BindParents -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRootLocal bindParents =
    bindingPathToRootWithLookup (`IntMap.lookup` bindParents)

data VisitState
    = Visiting
    | Done
    deriving (Eq, Show)

-- | Validate that every binding-parent path reachable from the supplied starts
-- is acyclic.  This is the linear counterpart to calling
-- 'bindingPathToRootWithLookup' for every node: the parent relation has at most
-- one outgoing edge per node, so completed suffixes can be shared.
validateBindingPathsAcyclicWithLookup
    :: (Int -> Maybe (NodeRef, BindFlag))
    -> [NodeRef]
    -> Either BindingError ()
validateBindingPathsAcyclicWithLookup lookupParent starts =
    void (foldM visit IntMap.empty starts)
  where
    visit !states ref =
        visitKey [] states (nodeRefKey ref)

    visitKey !trail !states !key =
        case IntMap.lookup key states of
            Just Done ->
                let !states' = markDone trail states
                in Right states'
            Just Visiting ->
                Left $
                    BindingCycleDetected $
                        map nodeRefFromKey (reverse (key : trail))
            Nothing ->
                case lookupParent key of
                    Nothing ->
                        let !states' = markDone (key : trail) states
                        in Right states'
                    Just (parentRef, _flag) -> do
                        let !states' = IntMap.insert key Visiting states
                            !parentKey = nodeRefKey parentRef
                        visitKey (key : trail) states' parentKey

    markDone trail states =
        foldl'
            (\states' key -> IntMap.insert key Done states')
            states
            trail

validateBindingPathsAcyclic :: BindParents -> [NodeRef] -> Either BindingError ()
validateBindingPathsAcyclic bindParents =
    validateBindingPathsAcyclicWithLookup (`IntMap.lookup` bindParents)

firstGenAncestorFromPath
    :: (NodeRef -> Either BindingError [NodeRef])
    -> NodeRef
    -> Maybe GenNodeId
firstGenAncestorFromPath pathLookup start =
    case pathLookup start of
        Left _ -> Nothing
        Right path -> listToMaybe [ gid | GenRef gid <- drop 1 path ]
