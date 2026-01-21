module MLF.Elab.Run.TypeOps (
    inlineBoundVarsType,
    simplifyAnnotationType
) where

import Data.Functor.Foldable (cata, para)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Solve (SolveResult, frWith, srConstraint, srUnionFind)
import MLF.Constraint.Types (NodeId(..), getNodeId)
import MLF.Elab.Generalize.Names (parseNameId)
import MLF.Elab.Reify (reifyType)
import MLF.Elab.TypeOps (inlineBaseBoundsType, substTypeSimple)
import MLF.Elab.Types

inlineBoundVarsType :: SolveResult -> ElabType -> ElabType
inlineBoundVarsType res = go IntSet.empty
  where
    constraint = srConstraint res
    canonical = frWith (srUnionFind res)
    resolveBoundBody seen nid0 =
        let nid = canonical nid0
            key = getNodeId nid
        in if IntSet.member key seen
            then nid
            else case VarStore.lookupVarBound constraint nid of
                Just bnd -> resolveBoundBody (IntSet.insert key seen) bnd
                Nothing -> nid
    go seen ty = case ty of
        TVar v ->
            case parseNameId v of
                Just nidInt ->
                    let nidC = canonical (NodeId nidInt)
                        key = getNodeId nidC
                    in if IntSet.member key seen
                        then ty
                        else case VarStore.lookupVarBound constraint nidC of
                            Just bnd ->
                                let bndRoot = resolveBoundBody (IntSet.insert key seen) bnd
                                in case reifyType res bndRoot of
                                    Right t0 ->
                                        let t1 = inlineBaseBoundsType constraint canonical t0
                                        in go (IntSet.insert key seen) t1
                                    Left _ -> ty
                            Nothing -> ty
                Nothing -> ty
        TArrow a b -> TArrow (go seen a) (go seen b)
        TForall v mb body -> TForall v (fmap (go seen) mb) (go seen body)
        TBase _ -> ty
        TBottom -> ty

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

    stripForalls = para alg
      where
        alg ty = case ty of
            TForallF v mb bodyPair ->
                let mbOrig = fmap fst mb
                    (binds, inner) = snd bodyPair
                in ((v, mbOrig) : binds, inner)
            TVarF v -> ([], TVar v)
            TArrowF d c -> ([], TArrow (fst d) (fst c))
            TBaseF b -> ([], TBase b)
            TBottomF -> ([], TBottom)

    normalizeForalls (binds0, body0) =
        let binds1 =
                [ (v, fmap go mb)
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
                    [ freeVarsType bnd
                    | (_, Just bnd) <- binds
                    ]
            goMerge _ [] body' = ([], body')
            goMerge seen ((v, mb):rest) body' =
                let mb' =
                        case mb of
                            Just (TVar v') | v' == v -> Nothing
                            _ -> mb
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
                                                    body'' = substVar v rep body'
                                                in goMerge seen rest' body''
                                            else
                                                let rest' = map (substBindType v (baseFromKey key)) rest
                                                    body'' = substTypeSimple v (baseFromKey key) body'
                                                in goMerge seen rest' body''
                                    else
                                        let rest' = map (substBind v rep) rest
                                            body'' = substVar v rep body'
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
        let freeInBound = maybe Set.empty freeVarsType
            used = Set.union (freeVarsType body)
                (Set.unions [ freeInBound mb | (_, mb) <- binds ])
            keep (v, mb) = Set.member v used || maybe False (Set.member v . freeVarsType) mb
        in (filter keep binds, body)

    inlineAlias ty = case ty of
        TForall v mb body ->
            let mb' = fmap go mb
                body' = go body
                mb'' = case mb' of
                    Just (TVar v') | v' == v -> Nothing
                    _ -> mb'
            in case (mb'', body') of
                (Just bound, TVar v') | v' == v && inlineAliasBound bound ->
                    bound
                _ -> TForall v mb'' body'
        _ -> ty

    inlineAliasBound bound = case bound of
        TArrow (TVar v1) (TVar v2) -> v1 == v2
        _ -> False

    substBind v v0 (name, mb) =
        let mb' = fmap (substVar v v0) mb
        in (name, mb')

    substBindType v replacement (name, mb) =
        let mb' = fmap (substTypeSimple v replacement) mb
        in (name, mb')

    substVar v v0 = para alg
      where
        alg ty = case ty of
            TVarF name
                | name == v -> TVar v0
                | otherwise -> TVar name
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF name mb body
                | name == v -> TForall name (fmap fst mb) (fst body)
                | otherwise -> TForall name (fmap snd mb) (snd body)

    freeVarsType = freeVarsFrom Set.empty
      where
        freeVarsFrom bound ty = (cata alg ty) bound
        alg ty = case ty of
            TVarF v ->
                \bound' ->
                    if Set.member v bound'
                        then Set.empty
                        else Set.singleton v
            TArrowF a b -> \bound' -> Set.union (a bound') (b bound')
            TBaseF _ -> const Set.empty
            TBottomF -> const Set.empty
            TForallF v mb body ->
                \bound' ->
                    let bound'' = Set.insert v bound'
                        freeBound = maybe Set.empty ($ bound'') mb
                        freeBody = body bound''
                    in Set.union freeBound freeBody
