module MLF.Elab.Run.Scope (
    bindingScopeRef,
    preferGenScope,
    schemeBodyTarget,
    canonicalizeScopeRef,
    resolveCanonicalScope,
    letScopeOverrides
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (listToMaybe)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Solve (SolveResult, frWith, srConstraint, srUnionFind)
import MLF.Constraint.Types
    ( BindingError
    , Constraint
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , getNodeId
    , gnSchemes
    , typeRef
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))

bindingScopeRef :: Constraint -> NodeId -> Either BindingError NodeRef
bindingScopeRef constraint root = do
    path <- Binding.bindingPathToRoot constraint (typeRef root)
    case listToMaybe [gid | GenRef gid <- drop 1 path] of
        Just gid -> Right (GenRef gid)
        Nothing -> Right (TypeRef root)

preferGenScope :: Constraint -> NodeRef -> NodeRef
preferGenScope constraint ref = case ref of
    GenRef _ -> ref
    TypeRef nid ->
        case Binding.bindingPathToRoot constraint (typeRef nid) of
            Right path ->
                case listToMaybe [gid | GenRef gid <- drop 1 path] of
                    Just gid -> GenRef gid
                    Nothing -> ref
            Left _ -> ref

schemeBodyTarget :: SolveResult -> NodeId -> NodeId
schemeBodyTarget res target =
    let constraint = srConstraint res
        canonical = frWith (srUnionFind res)
        targetC = canonical target
        isSchemeRoot =
            any
                (\gen -> any (\root -> canonical root == targetC) (gnSchemes gen))
                (NodeAccess.allGenNodes constraint)
        schemeRootByBody =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | gen <- NodeAccess.allGenNodes constraint
                , root <- gnSchemes gen
                , Just bnd <- [VarStore.lookupVarBound constraint root]
                , case NodeAccess.lookupNode constraint (canonical bnd) of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
    in case NodeAccess.lookupNode constraint targetC of
        Just TyVar{ tnBound = Just bnd } ->
            let bndC = canonical bnd
                boundIsSchemeBody = IntMap.member (getNodeId bndC) schemeRootByBody
            in if isSchemeRoot || boundIsSchemeBody
                then
                    case NodeAccess.lookupNode constraint bndC of
                        Just TyForall{ tnBody = body } -> canonical body
                        _ -> bndC
                else targetC
        Just TyForall{ tnBody = body } -> canonical body
        _ -> targetC

canonicalizeScopeRef :: SolveResult -> IntMap.IntMap NodeId -> NodeRef -> NodeRef
canonicalizeScopeRef solved redirects scopeRef =
    case scopeRef of
        GenRef gid -> GenRef gid
        TypeRef nid ->
            let canonical = frWith (srUnionFind solved)
            in TypeRef (canonical (chaseRedirects redirects nid))

resolveCanonicalScope :: Constraint -> SolveResult -> IntMap.IntMap NodeId -> NodeId -> Either BindingError NodeRef
resolveCanonicalScope constraint solved redirects scopeRoot = do
    scope0 <- bindingScopeRef constraint scopeRoot
    let scopeBase = preferGenScope constraint scope0
    pure (canonicalizeScopeRef solved redirects scopeBase)

letScopeOverrides :: Constraint -> Constraint -> SolveResult -> IntMap.IntMap NodeId -> AnnExpr -> IntMap.IntMap NodeRef
letScopeOverrides base solvedForGen solved redirects ann =
    let canonical = frWith (srUnionFind solved)
        addOverride acc schemeRootId =
            case bindingScopeRef base schemeRootId of
                Right scope0 ->
                    let scope = canonicalizeScopeRef solved redirects scope0
                        schemeRootC = canonical (chaseRedirects redirects schemeRootId)
                        postScope =
                            case bindingScopeRef solvedForGen schemeRootC of
                                Right ref -> canonicalizeScopeRef solved redirects ref
                                Left _ -> scope
                    in if scope == postScope
                        then acc
                        else IntMap.insert (getNodeId schemeRootC) scope acc
                Left _ -> acc
        alg expr = case expr of
            AVarF _ _ -> IntMap.empty
            ALitF _ _ -> IntMap.empty
            ALamF _ _ _ body _ -> body
            AAppF fun arg _ _ _ -> IntMap.union arg fun
            ALetF _ _ schemeRootId _ _ rhs body _ ->
                let baseMap = addOverride IntMap.empty schemeRootId
                in IntMap.union body (IntMap.union rhs baseMap)
            AAnnF inner _ _ -> inner
    in cata alg ann
