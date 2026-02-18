{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module MLF.Frontend.ConstraintGen.Types (
    ConstraintError(..),
    ConstraintResult(..),
    AnnExpr(..),
    AnnExprF(..),
    Binding(..),
    Env,
    replaceScopeRoot
) where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), cata)
import Data.Map.Strict (Map)

import MLF.Constraint.Types
import MLF.Frontend.Syntax (Lit, VarName)

-- | Errors that can surface during constraint generation.
data ConstraintError
    = UnknownVariable VarName
    | InternalConstraintError String  -- ^ Unexpected internal state
    | UnexpectedBareCoercionConst
    | TypeConstructorArityMismatch BaseTy Int Int  -- ^ Constructor, expected arity, actual arity
    | ForallBoundMentionsBinder String  -- ^ Binder name that appears in its own bound
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

data AnnExprF a
    = AVarF VarName NodeId
    | ALitF Lit NodeId
    | ALamF VarName NodeId GenNodeId a NodeId
      -- ^ param name, param node, scope root (gen), body, result node
    | AAppF a a EdgeId EdgeId NodeId
      -- ^ fun, arg, fun inst edge id, arg inst edge id, result node
    | ALetF VarName GenNodeId NodeId ExpVarId GenNodeId a a NodeId
      -- ^ binder name, scheme gen node, scheme root, expansion var, RHS scope gen, rhs, body, result node
    | AAnnF a NodeId EdgeId
      -- ^ expression, annotation node
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base AnnExpr = AnnExprF

instance Recursive AnnExpr where
    project expr = case expr of
        AVar v nid -> AVarF v nid
        ALit l nid -> ALitF l nid
        ALam v param scopeRoot body nid -> ALamF v param scopeRoot body nid
        AApp fun arg funEid argEid nid -> AAppF fun arg funEid argEid nid
        ALet v schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
            ALetF v schemeGenId schemeRootId expVar scopeRoot rhs body nid
        AAnn inner annNode eid -> AAnnF inner annNode eid

instance Corecursive AnnExpr where
    embed expr = case expr of
        AVarF v nid -> AVar v nid
        ALitF l nid -> ALit l nid
        ALamF v param scopeRoot body nid -> ALam v param scopeRoot body nid
        AAppF fun arg funEid argEid nid -> AApp fun arg funEid argEid nid
        ALetF v schemeGenId schemeRootId expVar scopeRoot rhs body nid ->
            ALet v schemeGenId schemeRootId expVar scopeRoot rhs body nid
        AAnnF inner annNode eid -> AAnn inner annNode eid

data Binding = Binding
  { bindingNode :: NodeId
  , bindingGen :: Maybe GenNodeId
  }

type Env = Map VarName Binding

replaceScopeRoot :: GenNodeId -> GenNodeId -> AnnExpr -> AnnExpr
replaceScopeRoot from to = cata alg
  where
    repGen gid = if gid == from then to else gid
    alg ann = case ann of
        AVarF v nid -> AVar v nid
        ALitF lit nid -> ALit lit nid
        ALamF v param scopeRoot body nid ->
            ALam v param (repGen scopeRoot) body nid
        AAppF fun arg funEid argEid nid ->
            AApp fun arg funEid argEid nid
        ALetF v schemeNode schemeRootId expVar scopeRoot rhs body nid ->
            ALet v (repGen schemeNode) schemeRootId expVar (repGen scopeRoot) rhs body nid
        AAnnF expr annNode eid ->
            AAnn expr annNode eid
