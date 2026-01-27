{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.TypeOps (
    inlineBoundVarsType,
    inlineBoundVarsTypeForBound,
    simplifyAnnotationType
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Solve (SolveResult, frWith, srConstraint, srUnionFind)
import MLF.Constraint.Types (NodeId(..), getNodeId)
import MLF.Util.Names (parseNameId)
import MLF.Reify.Core (namedNodes, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (
    freeTypeVarsFrom,
    inlineBaseBoundsType,
    renameTypeVar,
    splitForalls,
    substTypeSimple
    )
import MLF.Elab.Types

freeVarsType :: ElabType -> Set.Set String
freeVarsType = freeTypeVarsFrom Set.empty

mapBound :: (ElabType -> ElabType) -> BoundType -> BoundType
mapBound f bound = case bound of
    TArrow a b -> TArrow (f a) (f b)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall v mb body ->
        let mb' = fmap (mapBound f) mb
        in TForall v mb' (f body)

inlineBoundVarsType :: SolveResult -> ElabType -> ElabType
inlineBoundVarsType = inlineBoundVarsTypeWith False

inlineBoundVarsTypeForBound :: SolveResult -> ElabType -> ElabType
inlineBoundVarsTypeForBound = inlineBoundVarsTypeWith True

-- See Note [Scope-aware bound/alias inlining] in
-- docs/notes/2026-01-27-elab-changes.md.
inlineBoundVarsTypeWith :: Bool -> SolveResult -> ElabType -> ElabType
inlineBoundVarsTypeWith unboundToBottom res = go Set.empty IntSet.empty
  where
    constraint = srConstraint res
    canonical = frWith (srUnionFind res)
    namedSet = either (const IntSet.empty) id (namedNodes res)
    resolveBoundBody seen nid0 =
        let nid = canonical nid0
            key = getNodeId nid
        in if IntSet.member key seen
            then nid
            else case VarStore.lookupVarBound constraint nid of
                Just bnd -> resolveBoundBody (IntSet.insert key seen) bnd
                Nothing -> nid
    go boundNames seen ty = case ty of
        TVar v
            | Set.member v boundNames -> ty
            | otherwise ->
                case parseNameId v of
                    Just nidInt ->
                        let nidC = canonical (NodeId nidInt)
                            key = getNodeId nidC
                        in if IntSet.member key seen
                            then ty
                            else case VarStore.lookupVarBound constraint nidC of
                                Just bnd ->
                                    let bndRoot = resolveBoundBody (IntSet.insert key seen) bnd
                                    in case reifyTypeWithNamedSetNoFallback res IntMap.empty namedSet bndRoot of
                                        Right t0 ->
                                            let t1 = inlineBaseBoundsType constraint canonical t0
                                            in go boundNames (IntSet.insert key seen) t1
                                        Left _ -> ty
                                Nothing -> if unboundToBottom then TBottom else ty
                    Nothing -> ty
        TArrow a b -> TArrow (go boundNames seen a) (go boundNames seen b)
        TForall v mb body ->
            let boundNames' = Set.insert v boundNames
            in TForall v (fmap (goBound boundNames' seen) mb) (go boundNames' seen body)
        TBase _ -> ty
        TBottom -> ty

    goBound boundNames seen bound = case bound of
        TArrow a b -> TArrow (go boundNames seen a) (go boundNames seen b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body ->
            let boundNames' = Set.insert v boundNames
            in TForall v (fmap (goBound boundNames' seen) mb) (go boundNames' seen body)

simplifyAnnotationType :: ElabType -> ElabType
simplifyAnnotationType = go
  where
    go ty = case ty of
        TVar _ -> ty
        TBase _ -> ty
        TBottom -> ty
        TArrow a b -> TArrow (go a) (go b)
        TForall{} ->
            normalizeForalls (stripForalls ty)

    stripForalls = splitForalls

    normalizeForalls (binds0, body0) =
        let binds1 =
                [ (v, fmap (mapBound go) mb)
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
                    [ freeTypeVarsTy bnd
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
        let freeInBound = maybe Set.empty freeTypeVarsTy
            used = Set.union (freeVarsType body)
                (Set.unions [ freeInBound mb | (_, mb) <- binds ])
            keep (v, mb) = Set.member v used || maybe False (Set.member v . freeTypeVarsTy) mb
        in (filter keep binds, body)

    inlineAlias ty = case ty of
        TForall v mb body ->
            let mb' = fmap (mapBound go) mb
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
        let mb' = fmap (mapBound (renameTypeVar v v0)) mb
        in (name, mb')

    substBindType v replacement (name, mb) =
        let mb' = fmap (mapBound (substTypeSimple v replacement)) mb
        in (name, mb')
