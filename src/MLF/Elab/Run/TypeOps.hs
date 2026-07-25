{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.TypeOps (
    InlineBoundVarsContext,
    mkInlineBoundVarsContext,
    mkInlineBoundVarsContextWithReadModel,
    mkInlineBoundVarsContextWithReadModelCanonical,
    inlineBoundVarsType,
    inlineBoundVarsTypeWithCanonical,
    inlineBoundVarsTypeWithCanonicalExcept,
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
import MLF.Constraint.Types.Graph (NodeId, NodeMap, TyNode, cNodes)
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
    inlineAliasBoundsWithBySeenProtected,
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
    , ibvcNodes :: NodeMap TyNode
    , ibvcCanonical :: NodeId -> NodeId
    , ibvcReadModel :: Maybe (ElabReadModel p)
    }

mkInlineBoundVarsContext :: PresolutionView p -> IntSet.IntSet -> InlineBoundVarsContext p
mkInlineBoundVarsContext presolutionView namedSet =
    InlineBoundVarsContext
        { ibvcPresolutionView = presolutionView
        , ibvcNamedSet = namedSet
        , ibvcNodes = cNodes constraint
        , ibvcCanonical = pvCanonical presolutionView
        , ibvcReadModel = Nothing
        }
  where
    constraint = pvConstraint presolutionView

mkInlineBoundVarsContextWithReadModel :: ElabReadModel p -> InlineBoundVarsContext p
mkInlineBoundVarsContextWithReadModel readModel =
    mkInlineBoundVarsContextWithReadModelCanonical
        (pvCanonical (ermPresolutionView readModel))
        readModel

mkInlineBoundVarsContextWithReadModelCanonical
    :: (NodeId -> NodeId)
    -> ElabReadModel p
    -> InlineBoundVarsContext p
mkInlineBoundVarsContextWithReadModelCanonical canonical readModel =
    InlineBoundVarsContext
        { ibvcPresolutionView = presolutionView
        , ibvcNamedSet = ermNamedNodes readModel
        , ibvcNodes = ermNodes readModel
        , ibvcCanonical = canonical
        , ibvcReadModel = Just readModel
        }
  where
    presolutionView = ermPresolutionView readModel

inlineBoundVarsType :: PresolutionView p -> ElabType -> ElabType
inlineBoundVarsType = inlineBoundVarsTypeWith False

-- | Inline graph aliases and bounds using the construction's authoritative
-- canonicalizer.  The plain 'PresolutionView' canonicalizer does not include
-- annotation redirects, so using it at a prepared edge can leave an alias to
-- a concrete TyMu/TyBase owner as a free graph variable.
inlineBoundVarsTypeWithCanonical
    :: (NodeId -> NodeId)
    -> PresolutionView p
    -> ElabType
    -> ElabType
inlineBoundVarsTypeWithCanonical canonical presolutionView =
    inlineBoundVarsTypeWithCanonicalExcept [] canonical presolutionView

-- | Inline graph bounds while preserving references that an independently
-- checked construction endpoint proves are ambient Gamma binders.
inlineBoundVarsTypeWithCanonicalExcept
    :: [TypeBinderRef]
    -> (NodeId -> NodeId)
    -> PresolutionView p
    -> ElabType
    -> ElabType
inlineBoundVarsTypeWithCanonicalExcept protectedRefs canonical presolutionView =
    inlineAliasBoundsWithBySeenProtected
        protectedRefs
        False
        (ibvcCanonical context)
        (ibvcNodes context)
        (VarStore.lookupVarBound constraint)
        reifyBoundWithSeen
  where
    baseContext =
        mkInlineBoundVarsContext presolutionView
            (either (const IntSet.empty) id (namedNodes presolutionView))
    context = baseContext { ibvcCanonical = canonical }
    constraint = pvConstraint presolutionView
    reifyBoundWithSeen seen bnd = do
        let bndRoot = resolveBoundBodyConstraint canonical constraint seen bnd
        t0 <- reifyTypeWithNamedSetRefsNoFallback presolutionView IntMap.empty (ibvcNamedSet context) bndRoot
        pure (inlineBaseBoundsType constraint canonical t0)

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
        nodes
        (VarStore.lookupVarBound constraint)
        reifyBoundWithSeen
  where
    presolutionView = ibvcPresolutionView context
    constraint = pvConstraint presolutionView
    canonical = ibvcCanonical context
    namedSet = ibvcNamedSet context
    nodes = ibvcNodes context
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
        let baseEntry bound = case bound of
                TBaseWithIdentity identity base ->
                    Just (Just identity, TBaseWithIdentity identity base)
                TBottom -> Just (Nothing, TBottom)
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
                in case mb' >>= baseEntry of
                    Just (key, boundTy) ->
                        case Map.lookup key seen of
                            Just (rep, repUsed, repTy) ->
                                if repUsed
                                    then
                                        if vUsed
                                            then
                                                let rest' = map (substBind ref rep) rest
                                                    body'' = substTypeSimpleRef ref (TVarRef rep) body'
                                                in goMerge seen rest' body''
                                            else
                                                let rest' = map (substBindType ref repTy) rest
                                                    body'' = substTypeSimpleRef ref repTy body'
                                                in goMerge seen rest' body''
                                    else
                                        let rest' = map (substBind ref rep) rest
                                            body'' = substTypeSimpleRef ref (TVarRef rep) body'
                                            repUsed' = repUsed || vUsed
                                            seen' = Map.insert key (rep, repUsed', repTy) seen
                                        in goMerge seen' rest' body''
                            Nothing ->
                                let seen' = Map.insert key (ref, vUsed, boundTy) seen
                                    (rest', body'') = goMerge seen' rest body'
                                in ((ref, mb') : rest', body'')
                    Nothing ->
                        let (rest', body'') = goMerge seen rest body'
                        in ((ref, mb') : rest', body'')
        in goMerge Map.empty binds body

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
