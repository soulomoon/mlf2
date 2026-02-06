module MLF.Util.ElabError (
    ElabError(..),
    bindingToElab
) where

import MLF.Constraint.Types.Graph (BindingError, EdgeId, NodeId)
import MLF.Frontend.Syntax (VarName)

-- | Errors that can arise during elaboration or reification.
data ElabError
    = ResidualTyExp NodeId
    | MissingNode NodeId
    | FreeVarOutOfScope NodeId
    | EnvLookup VarName
    | ValidationFailed [String]
    | PhiTranslatabilityError [String]
    | PhiInvariantError String
    | BindingTreeError BindingError
    | NameConflict String
    | InstantiationError String
    | SchemeFreeVars NodeId [String]
    | MissingEdgeTrace EdgeId
    -- ^ Edge has a witness but no corresponding trace; Φ translation requires trace
    deriving (Eq, Show)

bindingToElab :: Either BindingError a -> Either ElabError a
bindingToElab = either (Left . BindingTreeError) Right
