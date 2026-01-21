module MLF.Elab.Generalize.BindingUtil (
    bindingScopeFor,
    bindingPathToRootLocal,
    firstGenAncestorFrom
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Types (ElabError(..))

bindingScopeFor :: Constraint -> NodeRef -> Maybe GenNodeId
bindingScopeFor constraint ref =
    case Binding.bindingPathToRoot constraint ref of
        Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]
        Left _ -> Nothing

bindingPathToRootLocal :: BindParents -> NodeRef -> Either ElabError [NodeRef]
bindingPathToRootLocal bindParents start =
    walkBindingParents bindParents IntSet.empty [start] (nodeRefKey start)

walkBindingParents :: BindParents -> IntSet.IntSet -> [NodeRef] -> Int -> Either ElabError [NodeRef]
walkBindingParents bindParents visited path key
    | IntSet.member key visited =
        Left (BindingTreeError (BindingCycleDetected (reverse path)))
    | otherwise =
        case IntMap.lookup key bindParents of
            Nothing -> Right (reverse path)
            Just (parentRef, _) ->
                walkBindingParents
                    bindParents
                    (IntSet.insert key visited)
                    (parentRef : path)
                    (nodeRefKey parentRef)

firstGenAncestorFrom :: BindParents -> NodeRef -> Maybe GenNodeId
firstGenAncestorFrom bindParents start =
    case bindingPathToRootLocal bindParents start of
        Left _ -> Nothing
        Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]
