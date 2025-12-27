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
    | ALam VarName NodeId NodeId AnnExpr NodeId
      -- ^ param name, param node, scope root, body, result node
    | AApp AnnExpr AnnExpr EdgeId EdgeId NodeId
      -- ^ fun, arg, fun inst edge id, arg inst edge id, result node
    | ALet VarName NodeId ExpVarId NodeId AnnExpr AnnExpr NodeId
      -- ^ binder name, scheme node, expansion var, RHS scope root, rhs, body, result node
    | AAnn AnnExpr NodeId EdgeId
      -- ^ expression, annotation node
    deriving (Eq, Show)

data Binding = Binding
  { bindingNode :: NodeId
  }

type Env = Map VarName Binding

replaceScopeRoot :: NodeId -> NodeId -> AnnExpr -> AnnExpr
replaceScopeRoot from to = go
  where
    rep nid = if nid == from then to else nid
    go ann = case ann of
        AVar v nid -> AVar v (rep nid)
        ALit lit nid -> ALit lit (rep nid)
        ALam v param scopeRoot body nid ->
            ALam v (rep param) (rep scopeRoot) (go body) (rep nid)
        AApp fun arg funEid argEid nid ->
            AApp (go fun) (go arg) funEid argEid (rep nid)
        ALet v schemeNode expVar scopeRoot rhs body nid ->
            ALet v (rep schemeNode) expVar (rep scopeRoot) (go rhs) (go body) (rep nid)
        AAnn expr annNode eid ->
            AAnn (go expr) (rep annNode) eid
