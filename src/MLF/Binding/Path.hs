module MLF.Binding.Path (
    bindingPathToRootWithLookup,
    bindingPathToRoot,
    bindingPathToRootLocal,
    firstGenAncestorFromPath,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types

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

bindingPathToRoot :: Constraint -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRoot c =
    bindingPathToRootWithLookup (\key -> IntMap.lookup key (cBindParents c))

bindingPathToRootLocal :: BindParents -> NodeRef -> Either BindingError [NodeRef]
bindingPathToRootLocal bindParents =
    bindingPathToRootWithLookup (`IntMap.lookup` bindParents)

firstGenAncestorFromPath
    :: (NodeRef -> Either BindingError [NodeRef])
    -> NodeRef
    -> Maybe GenNodeId
firstGenAncestorFromPath pathLookup start =
    case pathLookup start of
        Left _ -> Nothing
        Right path -> listToMaybe [ gid | GenRef gid <- drop 1 path ]
