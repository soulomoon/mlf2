{- |
Module      : MLF.Constraint.Presolution.ForallIntro
Description : Materialize ∀-introductions during presolution

This module hosts the helper routines that turn presolution’s `ExpForall`
recipes into concrete `TyForall` nodes plus binding-tree / bound-store updates.
It is extracted from `MLF.Constraint.Presolution.Core` to keep that module
focused on orchestration.
-}
module MLF.Constraint.Presolution.ForallIntro (
    introduceForallFromSpec,
    bindForallBindersFromSpec
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Data.List (partition, sortBy)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.Order as Order
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), PresolutionState(..))
import MLF.Constraint.Presolution.Ops (createFreshNodeId, registerNode, setBindParentM, setVarBound)

{- Note [ExpForall materialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we materialize `ExpForall`, we introduce a fresh `TyForall` wrapper node
in the term-DAG and update the binding tree + variable-bound store to match the
`ForallSpec`.

In the paper (`papers/xmlf.txt`), the ω operations act on χe’s binding edges,
and binder shapes are derived from binding edges (Q(n)). We therefore interpret
`ForallSpec` as describing which *existing* variables in the body become
quantified at the new binder, rather than allocating disconnected “fresh”
binders (which would violate the binding-tree invariant that term-DAG roots
have no binding parent).
-}

introduceForallFromSpec :: ForallSpec -> NodeId -> PresolutionM NodeId
introduceForallFromSpec spec bodyRoot = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        bodyC = canonical bodyRoot
        oldParent = Binding.lookupBindParent c0 (typeRef bodyC)
    newId <- createFreshNodeId
    let node = TyForall newId bodyRoot
    registerNode newId node
    -- The body is now inside the binder.
    setBindParentM (typeRef bodyRoot) (typeRef newId, BindFlex)
    -- Preserve the body's former binding parent (if any) on the new binder.
    case oldParent of
        Just parentInfo -> setBindParentM (typeRef newId) parentInfo
        Nothing -> pure ()
    bindForallBindersFromSpec newId bodyRoot spec
    pure newId

bindForallBindersFromSpec :: NodeId -> NodeId -> ForallSpec -> PresolutionM ()
bindForallBindersFromSpec forallId bodyRoot ForallSpec{ fsBinderCount = k, fsBounds = bounds } = do
    when (k /= length bounds) $
        throwError $
            InternalError $
                "bindForallBindersFromSpec: fsBounds length mismatch: expected "
                    ++ show k ++ ", got " ++ show (length bounds)

    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        nodes0 = cNodes c0
        bodyC = canonical bodyRoot

        reachable =
            Traversal.reachableFromUnderLenient
                canonical
                (\nid -> IntMap.lookup (getNodeId nid) nodes0)
                bodyC
        orderKeys = Order.orderKeysFromRootWith canonical nodes0 bodyC Nothing

    bp <- case Binding.canonicalizeBindParentsUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right bp0 -> pure bp0

    let isLiveVar nid =
            case IntMap.lookup (getNodeId nid) nodes0 of
                Just TyVar{} ->
                    not (VarStore.isEliminatedVar c0 nid)
                _ -> False

        liveVarsReachable =
            [ NodeId nid
            | nid <- IntSet.toList reachable
            , let n = NodeId nid
            , isLiveVar n
            ]
        missing =
            [ nid
            | nid <- liveVarsReachable
            , not (IntMap.member (getNodeId nid) orderKeys)
            ]

        parentInfoOf nid = IntMap.lookup (nodeRefKey (typeRef nid)) bp

        -- Prefer variables whose current binding parent is outside the body
        -- subgraph, i.e. “free wrt bodyRoot”. This matches the common shape in
        -- constraints where polymorphic binders are attached above the body.
        isFreeLike nid =
            case parentInfoOf nid of
                Nothing -> True
                Just (p, flag) ->
                    flag == BindFlex && case p of
                        TypeRef pN -> not (IntSet.member (getNodeId pN) reachable)
                        GenRef _ -> True

        isFlexBound nid =
            case parentInfoOf nid of
                Nothing -> True
                Just (_p, flag) -> flag == BindFlex

        (freeLike0, other0) = partition isFreeLike liveVarsReachable
        freeLike = sortBy (Order.compareNodesByOrderKey orderKeys) freeLike0
        other = sortBy (Order.compareNodesByOrderKey orderKeys) (filter isFlexBound other0)
        candidates = freeLike ++ other

    unless (null missing) $
        throwError $
            InternalError $
                "bindForallBindersFromSpec: missing order keys for " ++ show missing

    let binders = take k candidates
        binderByIndex = IntMap.fromList (zip [0..] binders)

    forM_ binders $ \bv ->
        setBindParentM (typeRef bv) (typeRef forallId, BindFlex)

    forM_ (zip [0..] bounds) $ \(i, mbRef) ->
        case IntMap.lookup i binderByIndex of
            Nothing -> pure ()  -- arity mismatch: defer to later unification
            Just bv ->
                case mbRef of
                    Nothing -> setVarBound bv Nothing
                    Just (BoundNode bnd) -> setVarBound bv (Just (canonical bnd))
                    Just (BoundBinder j) ->
                        case IntMap.lookup j binderByIndex of
                            Nothing -> pure ()  -- arity mismatch: defer to later unification
                            Just bnd -> setVarBound bv (Just bnd)
