module MLF.Frontend.ConstraintGen.Types (
    ConstraintError(..),
    ConstraintResult(..),
    AnnExpr(..),
    Binding(..),
    Env,
    replaceScopeRoot
) where

import Data.Map.Strict (Map)

import MLF.Constraint.Types
import MLF.Frontend.Syntax (Lit, VarName)

-- | Errors that can surface during constraint generation.
data ConstraintError
    = UnknownVariable VarName
    deriving (Eq, Show)

-- | Successful constraint generation returns the full constraint graph and the
-- root 'NodeId' that represents the program's type.
data ConstraintResult = ConstraintResult
  { crConstraint :: Constraint
  , crRoot :: NodeId
  , crAnnotated :: AnnExpr
  }
    deriving (Eq, Show)

-- | Expression annotated with the NodeIds allocated during constraint generation.
-- The NodeIds are stable and match the ones found in the constraint graph, so
-- later phases (e.g., elaboration) can recover binder types.
data AnnExpr
    = AVar VarName NodeId
    | ALit Lit NodeId
    | ALam VarName NodeId GenNodeId AnnExpr NodeId
      -- ^ param name, param node, scope root (gen), body, result node
    | AApp AnnExpr AnnExpr EdgeId EdgeId NodeId
      -- ^ fun, arg, fun inst edge id, arg inst edge id, result node
    | ALet VarName GenNodeId NodeId ExpVarId GenNodeId AnnExpr AnnExpr NodeId
      -- ^ binder name, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    | AAnn AnnExpr NodeId EdgeId
      -- ^ expression, annotation node
    deriving (Eq, Show)

data Binding = Binding
  { bindingNode :: NodeId
  , bindingGen :: Maybe GenNodeId
  }

type Env = Map VarName Binding

replaceScopeRoot :: GenNodeId -> GenNodeId -> AnnExpr -> AnnExpr
replaceScopeRoot from to = go
  where
    repGen gid = if gid == from then to else gid
    go ann = case ann of
        AVar v nid -> AVar v nid
        ALit lit nid -> ALit lit nid
        ALam v param scopeRoot body nid ->
            ALam v param (repGen scopeRoot) (go body) nid
        AApp fun arg funEid argEid nid ->
            AApp (go fun) (go arg) funEid argEid nid
        ALet v schemeNode schemeRoot expVar scopeRoot rhs body nid ->
            ALet v (repGen schemeNode) schemeRoot expVar (repGen scopeRoot) (go rhs) (go body) nid
        AAnn expr annNode eid ->
            AAnn (go expr) annNode eid
