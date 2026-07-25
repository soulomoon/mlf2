{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.ForallIntro
Description : Materialize ∀-introductions during presolution

This module hosts the helper routines that turn presolution’s `ExpForall`
recipes into concrete `TyForall` nodes plus binding-tree / bound-store updates.
It keeps binder surgery localized so the public presolution entrypoint can stay
focused on orchestration.
-}
module MLF.Constraint.Presolution.ForallIntro (
    DestinationOwnedRoot,
    destinationOwnedRootNode,
    requireDestinationOwnedRoot,
    introduceForallFromSpec,
    bindForallBindersFromSpec
) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (throwError)
import Data.List (partition)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.Order as Order
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..))
import MLF.Constraint.Presolution.Copy (copyForallBoundProjectionAtBinder)
import MLF.Constraint.Presolution.Ops (createFreshNodeId, registerNode, setBindParentM, setVarBound)
import MLF.Constraint.Presolution.StateAccess
    ( getBindingSnapshot
    , getConstraintAndCanonical
    , liftBindingError
    )

{- Note [ExpForall materialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we materialize `ExpForall`, we introduce a fresh `TyForall` wrapper node
in the term-DAG and update the binding tree + variable-bound store to match the
`ForallSpec`.

In the paper (`papers/these-finale-english.txt`; see `papers/xmlf.txt`), the ω
operations act on χe’s binding edges,
and binder shapes are derived from binding edges (Q(n)). We therefore interpret
`ForallSpec` as describing which *existing* variables in the body become
quantified at the new binder, rather than allocating disconnected “fresh”
binders (which would violate the binding-tree invariant that term-DAG roots
have no binding parent).
-}

-- | A root whose binding parent has already been constructed at the expansion
-- destination.  Keep the constructor private: forall introduction may only
-- wrap a destination-owned copy, never acquire ownership by rebinding a shared
-- source node after the fact.
data DestinationOwnedRoot = DestinationOwnedRoot NodeRef NodeId

destinationOwnedRootNode :: DestinationOwnedRoot -> NodeId
destinationOwnedRootNode (DestinationOwnedRoot _ root) = root

requireDestinationOwnedRoot
    :: NodeRef
    -> NodeId
    -> PresolutionM p DestinationOwnedRoot
requireDestinationOwnedRoot expectedOwner root = do
    (c0, canonical) <- getConstraintAndCanonical
    requireDestinationOwner c0 canonical expectedOwner root

introduceForallFromSpec
    :: ForallSpec
    -> DestinationOwnedRoot
    -> PresolutionM p DestinationOwnedRoot
introduceForallFromSpec spec (DestinationOwnedRoot expectedOwner bodyRoot) = do
    (c0, canonical) <- getConstraintAndCanonical
    DestinationOwnedRoot expectedOwnerC bodyC <-
        requireDestinationOwner c0 canonical expectedOwner bodyRoot
    newId <- createFreshNodeId
    let node = TyForall newId bodyC
    registerNode newId node
    -- Preserve the body's destination parent on the new binder.
    -- Attach the new binder first so moving a bounded body never passes
    -- through a disconnected temporary scope.
    setBindParentM (typeRef newId) (expectedOwnerC, BindFlex)
    -- The body is now inside the fully attached binder.
    setBindParentM (typeRef bodyC) (typeRef newId, BindFlex)
    bindForallBindersFromSpec newId bodyC spec
    pure (DestinationOwnedRoot expectedOwnerC newId)

requireDestinationOwner
    :: Constraint p
    -> (NodeId -> NodeId)
    -> NodeRef
    -> NodeId
    -> PresolutionM p DestinationOwnedRoot
requireDestinationOwner c0 canonical expectedOwner root =
    let rootC = canonical root
        canonicalRef ref =
            case ref of
                TypeRef node -> typeRef (canonical node)
                GenRef gen -> genRef gen
        expectedOwnerC = canonicalRef expectedOwner
    in case Binding.lookupBindParent c0 (typeRef rootC) of
        Nothing ->
            throwError
                (BindingTreeError (MissingBindParent (typeRef rootC)))
        Just (actualOwner, actualFlag)
            | canonicalRef actualOwner == expectedOwnerC
            , actualFlag == BindFlex ->
                pure (DestinationOwnedRoot expectedOwnerC rootC)
            | otherwise ->
                throwError $
                    BindingTreeError $
                        InvalidBindingTree $
                            "destination-owned expansion root "
                                ++ show rootC
                                ++ " expected flexible parent "
                                ++ show expectedOwnerC
                                ++ ", got "
                                ++ show (canonicalRef actualOwner, actualFlag)

bindForallBindersFromSpec :: NodeId -> NodeId -> ForallSpec -> PresolutionM p ()
bindForallBindersFromSpec forallId bodyRoot ForallSpec{ fsBounds = bounds } = do
    (c0, canonical) <- getConstraintAndCanonical
    let nodes0 = cNodes c0
        bodyC = canonical bodyRoot

        -- Lower bounds are part of the quantified type.  In particular, a
        -- destination-owned wrapper @alpha >= tau@ exposes the binders in
        -- @tau@ to Q(n), even though ordinary term-DAG reachability stops at
        -- alpha.  Binder discovery and paper-order keys must use the same
        -- bound-aware projection as 'Binding.orderedBinders'.
        reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes0)
                bodyC
        orderKeys = Order.orderKeysFromConstraintWith canonical c0 bodyC Nothing

    bp <- liftBindingError $ Binding.canonicalizeBindParentsUnder canonical c0

    let isLiveVar nid =
            case lookupNodeIn nodes0 nid of
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
    freeLike <- case Order.sortByOrderKey orderKeys freeLike0 of
        Left err -> throwError $ InternalError ("bindForallBindersFromSpec: order key error: " ++ show err)
        Right sorted -> pure sorted
    other <- case Order.sortByOrderKey orderKeys (filter isFlexBound other0) of
        Left err -> throwError $ InternalError ("bindForallBindersFromSpec: order key error: " ++ show err)
        Right sorted -> pure sorted
    let candidates0 = freeLike ++ other
        bodyIsWrapper =
            case lookupNodeIn nodes0 bodyC of
                Just TyVar{} ->
                    case VarStore.lookupVarBound c0 bodyC of
                        Just _ -> True
                        Nothing -> False
                _ -> False
        candidates =
            if bodyIsWrapper
                then filter (/= bodyC) candidates0
                else candidates0

    unless (null missing) $
        throwError $
            InternalError $
                "bindForallBindersFromSpec: missing order keys for " ++ show missing

    let binderCount = length bounds
        availableCount = length candidates

    unless (availableCount >= binderCount) $
        throwError $
            ArityMismatch
                "bindForallBindersFromSpec"
                binderCount
                availableCount

    -- The target-derived Q(n) shape selects the binders introduced by this
    -- wrapper.  Additional live variables in the copied body remain owned by
    -- the destination projection; they are not an arity error.
    let binders = take binderCount candidates
        binderByIndex = IntMap.fromList (zip [0..] binders)

    forM_ binders $ \bv ->
        setBindParentM (typeRef bv) (typeRef forallId, BindFlex)

    snapshot <- getBindingSnapshot
    let resolveBound destinationBinder = \case
            Nothing -> pure Nothing
            Just (BoundNode bnd) -> pure (Just (canonical bnd))
            Just (BoundProjection sourceForall0 bnd0) -> do
                let sourceForall = canonical sourceForall0
                    bnd = canonical bnd0
                sourceBinders <-
                    case Binding.orderedBinders canonical c0 (typeRef sourceForall) of
                        Left err -> throwError (BindingTreeError err)
                        Right ordered -> pure ordered
                unless (length sourceBinders == binderCount) $
                    throwError $
                        ArityMismatch
                            "bindForallBindersFromSpec/source projection"
                            binderCount
                            (length sourceBinders)
                copiedBound <-
                    copyForallBoundProjectionAtBinder
                        snapshot
                        (typeRef destinationBinder)
                        bnd
                        (zip sourceBinders binders)
                pure (Just copiedBound)
            Just (BoundBinder j) ->
                case IntMap.lookup j binderByIndex of
                    Nothing ->
                        throwError $
                            InternalError $
                                "bindForallBindersFromSpec: invalid BoundBinder index "
                                    ++ show j
                                    ++ " for binder count "
                                    ++ show binderCount
                    Just bnd -> pure (Just bnd)

    forM_ (zip binders bounds) $ \(binder, boundRef) -> do
        mbBound <- resolveBound binder boundRef
        setVarBound binder mbBound
