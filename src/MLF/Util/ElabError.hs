module MLF.Util.ElabError (
    ElabError(..),
    bindingToElab
) where

import MLF.Constraint.Types.Graph (BindingError, NodeId)
import MLF.Frontend.Syntax (VarName)

-- | Errors that can arise during elaboration or reification.
data ElabError
    = ResidualTyExp NodeId
    | MissingNode NodeId
    | FreeVarOutOfScope NodeId
    | EnvLookup VarName
    | ValidationFailed [String]
    | BindingTreeError BindingError
    | NameConflict String
    | InstantiationError String
    | SchemeFreeVars NodeId [String]
    deriving (Eq, Show)

bindingToElab :: Either BindingError a -> Either ElabError a
bindingToElab = either (Left . BindingTreeError) Right
