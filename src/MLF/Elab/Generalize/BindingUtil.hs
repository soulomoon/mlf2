module MLF.Elab.Generalize.BindingUtil (
    bindingScopeFor,
    bindingPathToRootLocal,
    firstGenAncestorFrom
) where

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
    case Binding.bindingPathToRootLocal bindParents start of
        Left err -> Left (BindingTreeError err)
        Right path -> Right path

firstGenAncestorFrom :: BindParents -> NodeRef -> Maybe GenNodeId
firstGenAncestorFrom bindParents start =
    case bindingPathToRootLocal bindParents start of
        Left _ -> Nothing
        Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]
