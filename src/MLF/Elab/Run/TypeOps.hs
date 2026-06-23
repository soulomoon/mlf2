{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.TypeOps (
    InlineBoundVarsContext,
    mkInlineBoundVarsContext,
    mkInlineBoundVarsContextWithReadModel,
    inlineBoundVarsType,
    inlineBoundVarsTypeForBound,
    inlineBoundVarsTypeWithContext,
    inlineBoundVarsTypeForBoundWithContext,
    simplifyAnnotationType
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import MLF.Constraint.Presolution (PresolutionView(..))
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types.Graph (NodeMap, TyNode(..), cNodes, fromListNode, toListNode)
import MLF.Elab.ReadModel (ElabReadModel(..))
import MLF.Reify.Core
    ( namedNodes
    )
import MLF.Reify.Type
    ( reifyTypeWithNamedSetRefsNoFallback
    , reifyTypeWithNamedSetRefsNoFallbackReadModel
    )
import MLF.Reify.TypeOps (
    freeTypeVarRefsType,
    inlineAliasBoundsWithBySeen,
    inlineBaseBoundsType,
    resolveBoundBodyConstraint,
    splitForallsRefs,
    substTypeSimpleRef
    )
import MLF.Elab.Types
    ( ElabType
    , Ty(..)
    , TypeBinderRef
    , tyToElab
    , mapBoundType
    , tForallWithRef
    , typeBinderRefsSameIdentity
    )

data InlineBoundVarsContext p = InlineBoundVarsContext
    { ibvcPresolutionView :: PresolutionView p
    , ibvcNamedSet :: IntSet.IntSet
    , ibvcNodesVarOnly :: NodeMap TyNode
    , ibvcReadModel :: Maybe (ElabReadModel p)
    }

mkInlineBoundVarsContext :: PresolutionView p -> IntSet.IntSet -> InlineBoundVarsContext p
mkInlineBoundVarsContext presolutionView namedSet =
    InlineBoundVarsContext
        { ibvcPresolutionView = presolutionView
        , ibvcNamedSet = namedSet
        , ibvcNodesVarOnly =
            fromListNode
                [ (nid, node)
                | (nid, node) <- toListNode (cNodes constraint)
                , isTyVar node
                ]
        , ibvcReadModel = Nothing
        }
  where
    constraint = pvConstraint presolutionView
    isTyVar node = case node of
        TyVar{} -> True
        _ -> False

mkInlineBoundVarsContextWithReadModel :: ElabReadModel p -> InlineBoundVarsContext p
mkInlineBoundVarsContextWithReadModel readModel =
    InlineBoundVarsContext
        { ibvcPresolutionView = presolutionView
        , ibvcNamedSet = ermNamedNodes readModel
        , ibvcNodesVarOnly = ermNodesVarOnly readModel
        , ibvcReadModel = Just readModel
        }
  where
    presolutionView = ermPresolutionView readModel

inlineBoundVarsType :: PresolutionView p -> ElabType -> ElabType
inlineBoundVarsType = inlineBoundVarsTypeWith False

inlineBoundVarsTypeForBound :: PresolutionView p -> ElabType -> ElabType
inlineBoundVarsTypeForBound = inlineBoundVarsTypeWith True

inlineBoundVarsTypeWithContext :: InlineBoundVarsContext p -> ElabType -> ElabType
inlineBoundVarsTypeWithContext = inlineBoundVarsTypeWithPrepared False

inlineBoundVarsTypeForBoundWithContext :: InlineBoundVarsContext p -> ElabType -> ElabType
inlineBoundVarsTypeForBoundWithContext = inlineBoundVarsTypeWithPrepared True

-- See Note [Scope-aware bound/alias inlining] in
-- docs/notes/2026-01-27-elab-changes.md.
inlineBoundVarsTypeWith :: Bool -> PresolutionView p -> ElabType -> ElabType
inlineBoundVarsTypeWith unboundToBottom presolutionView =
    inlineBoundVarsTypeWithPrepared unboundToBottom (mkInlineBoundVarsContext presolutionView namedSet)
  where
    namedSet = either (const IntSet.empty) id (namedNodes presolutionView)

inlineBoundVarsTypeWithPrepared :: Bool -> InlineBoundVarsContext p -> ElabType -> ElabType
inlineBoundVarsTypeWithPrepared unboundToBottom context =
    inlineAliasBoundsWithBySeen
        unboundToBottom
        canonical
        nodesVarOnly
        (VarStore.lookupVarBound constraint)
        reifyBoundWithSeen
  where
    presolutionView = ibvcPresolutionView context
    constraint = pvConstraint presolutionView
    canonical = pvCanonical presolutionView
    namedSet = ibvcNamedSet context
    nodesVarOnly = ibvcNodesVarOnly context
    reifyBoundWithSeen seen bnd = do
        let bndRoot = resolveBoundBodyConstraint canonical constraint seen bnd
        t0 <-
            case ibvcReadModel context of
                Just readModel ->
                    reifyTypeWithNamedSetRefsNoFallbackReadModel readModel IntMap.empty namedSet bndRoot
                Nothing ->
                    reifyTypeWithNamedSetRefsNoFallback presolutionView IntMap.empty namedSet bndRoot
        pure (inlineBaseBoundsType constraint canonical t0)

simplifyAnnotationType :: ElabType -> ElabType
simplifyAnnotationType = go
  where
    go ty = case ty of
        TVarRef _ -> ty
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap go args)
        TVarAppRef ref args -> TVarAppRef ref (fmap go args)
        TBaseWithIdentity _ _ -> ty
        TBottom -> ty
        TArrow a b -> TArrow (go a) (go b)
        TMuRef ref body -> TMuRef ref (go body)
        TForallRef{} ->
            normalizeForalls (stripForalls ty)

    stripForalls = splitForallsRefs

    normalizeForalls (binds0, body0) =
        let binds1 =
                [ (ref, fmap (mapBoundType go) mb)
                | (ref, mb) <- binds0
                ]
            body1 = go body0
            (binds2, body2) = mergeBaseBounds binds1 body1
            (binds3, body3) = dropUnusedBinds binds2 body2
            ty = foldr (\(ref, b) t -> tForallWithRef ref b t) body3 binds3
        in inlineAlias ty

    mergeBaseBounds binds body =
        let baseKey bound = case bound of
                TBase b -> Just (Just b)
                TBottom -> Just Nothing
                _ -> Nothing
            usedInBounds =
                concat
                    [ freeTypeVarRefsType bnd
                    | (_, Just bnd) <- binds
                    ]
            goMerge _ [] body' = ([], body')
            goMerge seen ((ref, mb):rest) body' =
                let mb' = mb
                    vUsed = refMember ref usedInBounds
                in case mb' >>= baseKey of
                    Just key ->
                        case Map.lookup key seen of
                            Just (rep, repUsed) ->
                                if repUsed
                                    then
                                        if vUsed
                                            then
                                                let rest' = map (substBind ref rep) rest
                                                    body'' = substTypeSimpleRef ref (TVarRef rep) body'
                                                in goMerge seen rest' body''
                                            else
                                                let rest' = map (substBindType ref (baseFromKey key)) rest
                                                    body'' = substTypeSimpleRef ref (baseFromKey key) body'
                                                in goMerge seen rest' body''
                                    else
                                        let rest' = map (substBind ref rep) rest
                                            body'' = substTypeSimpleRef ref (TVarRef rep) body'
                                            repUsed' = repUsed || vUsed
                                            seen' = Map.insert key (rep, repUsed') seen
                                        in goMerge seen' rest' body''
                            Nothing ->
                                let seen' = Map.insert key (ref, vUsed) seen
                                    (rest', body'') = goMerge seen' rest body'
                                in ((ref, mb') : rest', body'')
                    Nothing ->
                        let (rest', body'') = goMerge seen rest body'
                        in ((ref, mb') : rest', body'')
        in goMerge Map.empty binds body

    baseFromKey key = case key of
        Just b -> TBase b
        Nothing -> TBottom

    dropUnusedBinds binds body =
        let freeInBound = maybe [] freeTypeVarRefsType
            used =
                unionRefs
                    (freeTypeVarRefsType body)
                    (concat [ freeInBound mb | (_, mb) <- binds ])
            keep (ref, mb) =
                refMember ref used || maybe False (refMember ref . freeTypeVarRefsType) mb
        in (filter keep binds, body)

    inlineAlias ty = case ty of
        TForallRef ref mb body ->
            let mb' = fmap (mapBoundType go) mb
                body' = go body
                mb'' = case mb' of
                    Just bound
                        | TVarRef ref' <- tyToElab bound
                        , typeBinderRefsSameIdentity ref' ref -> Nothing
                    _ -> mb'
            in case (mb'', body') of
                (Just bound, TVarRef ref')
                    | typeBinderRefsSameIdentity ref' ref
                    , inlineAliasBound (tyToElab bound) ->
                        tyToElab bound
                _ -> TForallRef ref mb'' body'
        _ -> ty

    inlineAliasBound bound = case bound of
        TArrow (TVarRef ref1) (TVarRef ref2) ->
            typeBinderRefsSameIdentity ref1 ref2
        _ -> False

    substBind ref rep (name, mb) =
        let mb' = fmap (mapBoundType (substTypeSimpleRef ref (TVarRef rep))) mb
        in (name, mb')

    substBindType ref replacement (name, mb) =
        let mb' = fmap (mapBoundType (substTypeSimpleRef ref replacement)) mb
        in (name, mb')

    unionRefs left right =
        foldr insertRef right left

    insertRef ref refs
        | refMember ref refs = refs
        | otherwise = ref : refs

    refMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
    refMember ref =
        any (typeBinderRefsSameIdentity ref)
