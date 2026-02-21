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

{- Note [ga′ scope selection — Def. 15.3.2 alignment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (Def. 15.3.2) defines ga′ as the nearest gen ancestor of the
binding root in the original constraint χ_p.  The 3-step pipeline
`resolveCanonicalScope` implements this as:

  1. `bindingScopeRef`: Computes `bindingPathToRoot`, takes the first GenRef
     after `drop 1` (skipping the node itself).  This is the nearest gen
     ancestor — thesis-aligned.  The `TypeRef root` fallback handles binding
     roots that have no gen ancestor (top-level nodes).

  2. `preferGenScope`: Re-runs the same path lookup that `bindingScopeRef`
     already performed.  When `bindingScopeRef` returned GenRef, this is a
     no-op.  When it returned TypeRef, this re-attempts the same lookup on
     the same constraint — redundant but harmless.
     Known deviation: the `Left _ -> ref` fallback silently swallows
     binding-tree errors rather than propagating them.

  3. `canonicalizeScopeRef`: GenRef passes through unchanged (gen nodes are
     stable identifiers not subject to redirect/UF).  TypeRef gets
     redirect-chased then UF-canonicalized.

  4. `letScopeOverrides`: Computes scope on the base constraint c1, compares
     with the solved constraint's scope.  Prefers the base scope when they
     diverge.  This is correct: the thesis defines ga′ on the original χ_p,
     not the solved version.

Conclusion: the pipeline is thesis-aligned.  The `preferGenScope` error
swallowing is a known deviation that is harmless in practice (the same
lookup succeeded in step 1).
-}
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

{- Note [ga′ preservation across redirects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Redirect chasing and UF canonicalization preserve ga′ through the pipeline:

  1. GenRef passthrough: `canonicalizeScopeRef` passes GenRef through
     unchanged (line above).  Gen nodes are stable identifiers not subject
     to redirect or union-find — they cannot be merged or redirected.

  2. TypeRef edge case: When `bindingScopeRef` returns TypeRef (no gen
     ancestor), `canonicalizeScopeRef` applies redirect+UF.  If the redirect
     changes the node to one that *does* have a gen ancestor, the scope
     remains a TypeRef (not upgraded to GenRef).  However, `letScopeOverrides`
     detects this divergence between base and solved scopes and records the
     base scope, so the thesis ga′ is preserved.

  3. Redirect chain stability: `chaseRedirectsStable`
     (MLF.Constraint.Canonicalizer) uses `stableChase` with cycle detection
     via IntSet.  Cycles resolve to the smallest NodeId, ensuring
     deterministic and stable canonicalization.

  4. Annotation rewriting: `applyRedirectsToAnn` (MLF.Elab.Run.Annotation)
     maps `chaseRedirects` over all node IDs in the annotated expression.
     Existing tests in PipelineSpec verify no stale (pre-redirect) nodes
     remain after annotation rewriting.
-}
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
