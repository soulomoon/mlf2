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
import qualified Data.Set as Set

import MLF.Constraint.Presolution (PresolutionView(..))
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Types.Graph (NodeMap, TyNode(..), cNodes, fromListNode, toListNode)
import MLF.Elab.ReadModel (ElabReadModel(..))
import MLF.Reify.Core
    ( namedNodes
    , reifyTypeWithNamedSetNoFallback
    , reifyTypeWithNamedSetNoFallbackReadModel
    )
import MLF.Reify.TypeOps (
    freeTypeVarsType,
    inlineAliasBoundsWithBySeen,
    inlineBaseBoundsType,
    resolveBoundBodyConstraint,
    renameTypeVar,
    splitForalls,
    substTypeSimple
    )
import MLF.Elab.Types
    ( ElabType
    , Ty(..)
    , tyToElab
    , mapBoundType
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
                    reifyTypeWithNamedSetNoFallbackReadModel readModel IntMap.empty namedSet bndRoot
                Nothing ->
                    reifyTypeWithNamedSetNoFallback presolutionView IntMap.empty namedSet bndRoot
        pure (inlineBaseBoundsType constraint canonical t0)

simplifyAnnotationType :: ElabType -> ElabType
simplifyAnnotationType = go
  where
    go ty = case ty of
        TVar _ -> ty
        TCon c args -> TCon c (fmap go args)
        TVarApp v args -> TVarApp v (fmap go args)
        TBase _ -> ty
        TBottom -> ty
        TArrow a b -> TArrow (go a) (go b)
        TMu v body -> TMu v (go body)
        TForall{} ->
            normalizeForalls (stripForalls ty)

    stripForalls = splitForalls

    normalizeForalls (binds0, body0) =
        let binds1 =
                [ (v, fmap (mapBoundType go) mb)
                | (v, mb) <- binds0
                ]
            body1 = go body0
            (binds2, body2) = mergeBaseBounds binds1 body1
            (binds3, body3) = dropUnusedBinds binds2 body2
            ty = foldr (\(v, b) t -> TForall v b t) body3 binds3
        in inlineAlias ty

    mergeBaseBounds binds body =
        let baseKey bound = case bound of
                TBase b -> Just (Just b)
                TBottom -> Just Nothing
                _ -> Nothing
            usedInBounds =
                Set.unions
                    [ freeTypeVarsType bnd
                    | (_, Just bnd) <- binds
                    ]
            goMerge _ [] body' = ([], body')
            goMerge seen ((v, mb):rest) body' =
                let mb' = mb
                    vUsed = Set.member v usedInBounds
                in case mb' >>= baseKey of
                    Just key ->
                        case Map.lookup key seen of
                            Just (rep, repUsed) ->
                                if repUsed
                                    then
                                        if vUsed
                                            then
                                                let rest' = map (substBind v rep) rest
                                                    body'' = renameTypeVar v rep body'
                                                in goMerge seen rest' body''
                                            else
                                                let rest' = map (substBindType v (baseFromKey key)) rest
                                                    body'' = substTypeSimple v (baseFromKey key) body'
                                                in goMerge seen rest' body''
                                    else
                                        let rest' = map (substBind v rep) rest
                                            body'' = renameTypeVar v rep body'
                                            repUsed' = repUsed || vUsed
                                            seen' = Map.insert key (rep, repUsed') seen
                                        in goMerge seen' rest' body''
                            Nothing ->
                                let seen' = Map.insert key (v, vUsed) seen
                                    (rest', body'') = goMerge seen' rest body'
                                in ((v, mb') : rest', body'')
                    Nothing ->
                        let (rest', body'') = goMerge seen rest body'
                        in ((v, mb') : rest', body'')
        in goMerge Map.empty binds body

    baseFromKey key = case key of
        Just b -> TBase b
        Nothing -> TBottom

    dropUnusedBinds binds body =
        let freeInBound = maybe Set.empty freeTypeVarsType
            used = Set.union (freeTypeVarsType body)
                (Set.unions [ freeInBound mb | (_, mb) <- binds ])
            keep (v, mb) = Set.member v used || maybe False (Set.member v . freeTypeVarsType) mb
        in (filter keep binds, body)

    inlineAlias ty = case ty of
        TForall v mb body ->
            let mb' = fmap (mapBoundType go) mb
                body' = go body
                mb'' = case mb' of
                    Just bound
                        | TVar v' <- tyToElab bound
                        , v' == v -> Nothing
                    _ -> mb'
            in case (mb'', body') of
                (Just bound, TVar v')
                    | v' == v
                    , inlineAliasBound (tyToElab bound) ->
                        tyToElab bound
                _ -> TForall v mb'' body'
        _ -> ty

    inlineAliasBound bound = case bound of
        TArrow (TVar v1) (TVar v2) -> v1 == v2
        _ -> False

    substBind v v0 (name, mb) =
        let mb' = fmap (mapBoundType (renameTypeVar v v0)) mb
        in (name, mb')

    substBindType v replacement (name, mb) =
        let mb' = fmap (mapBoundType (substTypeSimple v replacement)) mb
        in (name, mb')
