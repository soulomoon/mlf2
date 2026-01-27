{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module MLF.Reify.TypeOps (
    splitForalls,
    stripForallsType,
    freeTypeVarsFrom,
    freeTypeVarsType,
    freeTypeVarsList,
    substTypeCapture,
    substTypeSimple,
    renameTypeVar,
    freshNameLike,
    freshTypeName,
    freshTypeNameFromCounter,
    alphaEqType,
    matchType,
    parseNameId,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    resolveBoundBodyConstraint,
    inlineBaseBoundsType,
    inlineAliasBoundsWithBy,
    inlineAliasBoundsWithBySeen
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError(..))
import MLF.Util.Names (parseNameId)

newtype BoundFun (i :: TopVar) =
    BoundFun { runBoundFun :: Set.Set String -> Set.Set String }

splitForalls :: Ty v -> ([(String, Maybe BoundType)], ElabType)
splitForalls = go
  where
    go :: forall w. Ty w -> ([(String, Maybe BoundType)], ElabType)
    go ty = case ty of
        TForall v mb body ->
            let (binds, body') = go body
            in ((v, mb) : binds, body')
        _ -> ([], tyToElab ty)

stripForallsType :: Ty v -> ElabType
stripForallsType = snd . splitForalls

freeTypeVarsType :: Ty v -> Set.Set String
freeTypeVarsType = freeTypeVarsFromWith False Set.empty

freeTypeVarsFrom :: Set.Set String -> Ty v -> Set.Set String
freeTypeVarsFrom = freeTypeVarsFromWith True

freeTypeVarsList :: Ty v -> [String]
freeTypeVarsList = Set.toList . freeTypeVarsType

freeTypeVarsFromWith :: Bool -> Set.Set String -> Ty v -> Set.Set String
freeTypeVarsFromWith bindInBound bound0 ty =
    runBoundFun (cataIx alg ty) bound0
  where
    alg :: TyIF i BoundFun -> BoundFun i
    alg node = case node of
        TVarIF v ->
            BoundFun $ \boundSet ->
                if Set.member v boundSet
                    then Set.empty
                    else Set.singleton v
        TArrowIF d c ->
            BoundFun $ \boundSet ->
                Set.union (runBoundFun d boundSet) (runBoundFun c boundSet)
        TBaseIF _ -> BoundFun (const Set.empty)
        TBottomIF -> BoundFun (const Set.empty)
        TForallIF v mb body ->
            BoundFun $ \boundSet ->
                let boundBody = Set.insert v boundSet
                    boundBound = if bindInBound then boundBody else boundSet
                    freeBound = maybe Set.empty (\f -> runBoundFun f boundBound) mb
                    freeBody = runBoundFun body boundBody
                in Set.union freeBound freeBody

substTypeCapture :: String -> ElabType -> ElabType -> ElabType
substTypeCapture x s = goSub
  where
    freeTypeVarsTypeLocal :: ElabType -> Set.Set String
    freeTypeVarsTypeLocal = freeTypeVarsTy

    freeS = freeTypeVarsTypeLocal s

    substBoundCaptureLocal :: String -> ElabType -> BoundType -> BoundType
    substBoundCaptureLocal name replacement bound = case bound of
        TArrow a b ->
            TArrow (substTypeCapture name replacement a) (substTypeCapture name replacement b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body
            | v == name ->
                let mb' = fmap (substBoundCaptureLocal name replacement) mb
                in TForall v mb' body
            | v `Set.member` freeS ->
                let used =
                        Set.unions
                            [ freeS
                            , freeTypeVarsTypeLocal body
                            , maybe Set.empty freeTypeVarsTy mb
                            , Set.singleton v
                            ]
                    v' = freshNameLike v used
                    body' = substTypeCapture v (TVar v') body
                    mb' = fmap (substBoundCaptureLocal name replacement) mb
                in TForall v' mb' (substTypeCapture name replacement body')
            | otherwise ->
                let mb' = fmap (substBoundCaptureLocal name replacement) mb
                in TForall v mb' (substTypeCapture name replacement body)

    goSub = paraIx alg
      where
        alg :: TyIF i (IxPair Ty Ty) -> Ty i
        alg ty = case ty of
            TVarIF v
                | v == x -> s
                | otherwise -> TVar v
            TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
            TBaseIF b -> TBase b
            TBottomIF -> TBottom
            TForallIF v mb body
                | v == x ->
                    let mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                    in TForall v mb' (fst (unIxPair body))
                | v `Set.member` freeS ->
                    let used =
                            Set.unions
                                [ freeS
                                , freeTypeVarsTypeLocal (fst (unIxPair body))
                                , maybe Set.empty (freeTypeVarsTy . fst . unIxPair) mb
                                , Set.singleton v
                                ]
                        v' = freshNameLike v used
                        body' = substTypeCapture v (TVar v') (fst (unIxPair body))
                        mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                    in TForall v' mb' (substTypeCapture x s body')
                | otherwise ->
                    let mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                    in TForall v mb' (snd (unIxPair body))

substTypeSimple :: String -> ElabType -> ElabType -> ElabType
substTypeSimple name replacement = paraIx alg
  where
    substBoundSimpleLocal :: String -> ElabType -> BoundType -> BoundType
    substBoundSimpleLocal name0 replacement0 bound = case bound of
        TArrow a b ->
            TArrow (substTypeSimple name0 replacement0 a) (substTypeSimple name0 replacement0 b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body
            | v == name0 ->
                let mb' = fmap (substBoundSimpleLocal name0 replacement0) mb
                in TForall v mb' body
            | otherwise ->
                let mb' = fmap (substBoundSimpleLocal name0 replacement0) mb
                in TForall v mb' (substTypeSimple name0 replacement0 body)

    alg :: TyIF i (IxPair Ty Ty) -> Ty i
    alg ty = case ty of
        TVarIF v
            | v == name -> replacement
            | otherwise -> TVar v
        TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
        TBaseIF b -> TBase b
        TBottomIF -> TBottom
        TForallIF v mb body
            | v == name ->
                let mb' = fmap (substBoundSimpleLocal name replacement . fst . unIxPair) mb
                in TForall v mb' (fst (unIxPair body))
            | otherwise ->
                let mb' = fmap (substBoundSimpleLocal name replacement . fst . unIxPair) mb
                in TForall v mb' (snd (unIxPair body))

freshNameLike :: String -> Set.Set String -> String
freshNameLike base used =
    let candidates = base : [base ++ show i | i <- [(1::Int)..]]
    in case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        [] -> base

freshTypeName :: Set.Set String -> String
freshTypeName used =
    let candidates = ["u" ++ show i | i <- [(0::Int)..]]
    in case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        [] -> "u0"

freshTypeNameFromCounter :: Int -> Set.Set String -> (String, Int)
freshTypeNameFromCounter n used =
    let candidate = "u" ++ show n
    in if candidate `Set.member` used
        then freshTypeNameFromCounter (n + 1) used
        else (candidate, n + 1)


renameTypeVar :: String -> String -> ElabType -> ElabType
renameTypeVar old new = substTypeSimple old (TVar new)

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go Map.empty Map.empty
  where
    go envL envR t1 t2 = case (t1, t2) of
        (TVar a, TVar b) ->
            case Map.lookup a envL of
                Just b' -> b == b'
                Nothing ->
                    case Map.lookup b envR of
                        Just a' -> a == a'
                        Nothing -> a == b
        (TArrow a1 b1, TArrow a2 b2) ->
            go envL envR a1 a2 && go envL envR b1 b2
        (TBase b1, TBase b2) -> b1 == b2
        (TBottom, TBottom) -> True
        (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
            let envL' = Map.insert v1 v2 envL
                envR' = Map.insert v2 v1 envR
            in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
        _ -> False

    alphaEqMaybeBound envL envR mb1 mb2 = case (mb1, mb2) of
        (Nothing, Nothing) -> True
        (Just b1, Just b2) -> alphaEqBound envL envR b1 b2
        _ -> False

    alphaEqBound envL envR b1 b2 = case (b1, b2) of
        (TArrow a1 b1', TArrow a2 b2') ->
            go envL envR a1 a2 && go envL envR b1' b2'
        (TBase b1', TBase b2') -> b1' == b2'
        (TBottom, TBottom) -> True
        (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
            let envL' = Map.insert v1 v2 envL
                envR' = Map.insert v2 v1 envR
            in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
        _ -> False

matchType
    :: Set.Set String
    -> ElabType
    -> ElabType
    -> Either ElabError (Map.Map String ElabType)
matchType binderSet = goMatch Map.empty Map.empty
  where
    goMatch env subst tyP tyT = case (tyP, tyT) of
        (TVar v, _) | Set.member v binderSet ->
            case Map.lookup v subst of
                Nothing -> Right (Map.insert v tyT subst)
                Just ty0 ->
                    if alphaEqType ty0 tyT
                        then Right subst
                        else Left (InstantiationError "matchType: binder mismatch")
        (TVar v, TVar v')
            | Just v'' <- Map.lookup v env ->
                if v' == v'' then Right subst else Left (InstantiationError "matchType: bound var mismatch")
        (TVar v, TVar v')
            | v == v' -> Right subst
        (TArrow a b, TArrow a' b') -> do
            subst1 <- goMatch env subst a a'
            goMatch env subst1 b b'
        (TBase b0, TBase b1)
            | b0 == b1 -> Right subst
        (TBottom, TBottom) -> Right subst
        (TForall v mb b, TForall v' mb' b') -> do
            subst1 <- case (mb, mb') of
                (Nothing, Nothing) -> Right subst
                (Just x, Just y) -> matchBound env subst x y
                _ -> Left (InstantiationError "matchType: forall bound mismatch")
            goMatch (Map.insert v v' env) subst1 b b'
        _ -> Left (InstantiationError "matchType: structure mismatch")

    matchBound env subst boundP boundT = case (boundP, boundT) of
        (TArrow a b, TArrow a' b') -> do
            subst1 <- goMatch env subst a a'
            goMatch env subst1 b b'
        (TBase b0, TBase b1)
            | b0 == b1 -> Right subst
        (TBottom, TBottom) -> Right subst
        (TForall v mb b, TForall v' mb' b') -> do
            subst1 <- case (mb, mb') of
                (Nothing, Nothing) -> Right subst
                (Just x, Just y) -> matchBound env subst x y
                _ -> Left (InstantiationError "matchType: forall bound mismatch")
            goMatch (Map.insert v v' env) subst1 b b'
        _ -> Left (InstantiationError "matchType: structure mismatch")

resolveBaseBoundForInstConstraint
    :: Constraint
    -> (NodeId -> NodeId)
    -> NodeId
    -> Maybe NodeId
resolveBaseBoundForInstConstraint constraint canonical start =
    let nodes = cNodes constraint
        goResolve visited nid0 =
            let nid = canonical nid0
                key = getNodeId nid
            in if IntSet.member key visited
                then Nothing
                else
                    case IntMap.lookup key nodes of
                        Just TyBase{} -> Just nid
                        Just TyBottom{} -> Just nid
                        Just TyVar{} ->
                            case VarStore.lookupVarBound constraint nid of
                                Just bnd -> goResolve (IntSet.insert key visited) bnd
                                Nothing -> Nothing
                        _ -> Nothing
    in goResolve IntSet.empty start

resolveBaseBoundForInstSolved :: SolveResult -> NodeId -> Maybe NodeId
resolveBaseBoundForInstSolved res =
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
    in resolveBaseBoundForInstConstraint constraint canonical

resolveBoundBodyConstraint
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
resolveBoundBodyConstraint canonical constraint visited0 start =
    let go visited nid0 =
            let nid = canonical nid0
                key = getNodeId nid
            in if IntSet.member key visited
                then nid
                else case VarStore.lookupVarBound constraint nid of
                    Just bnd -> go (IntSet.insert key visited) bnd
                    Nothing -> nid
    in go visited0 start

inlineBaseBoundsType :: Constraint -> (NodeId -> NodeId) -> ElabType -> ElabType
inlineBaseBoundsType constraint canonical = cataIx alg
  where
    alg :: TyIF i Ty -> Ty i
    alg ty = case ty of
        TVarIF v ->
            case parseNameId v of
                Just nidInt ->
                    let nid = NodeId nidInt
                    in case resolveBaseBoundForInstConstraint constraint canonical nid of
                        Just baseN ->
                            case NodeAccess.lookupNode constraint baseN of
                                Just TyBase{ tnBase = b } -> TBase b
                                Just TyBottom{} -> TBottom
                                _ -> TVar v
                        Nothing -> TVar v
                Nothing -> TVar v
        TArrowIF a b -> TArrow a b
        TForallIF v mb body -> TForall v mb body
        TBaseIF b -> TBase b
        TBottomIF -> TBottom

-- | Inline alias/bound nodes in an ElabType using the supplied lookup and reify
-- functions. This is the shared implementation for scope-aware bound/alias
-- inlining; callers can wrap it with concrete environment data.
inlineAliasBoundsWithBy
    :: Bool
    -> (NodeId -> NodeId)
    -> IntMap.IntMap TyNode
    -> (NodeId -> Maybe NodeId)
    -> (NodeId -> Either err ElabType)
    -> ElabType
    -> ElabType
inlineAliasBoundsWithBy fallbackToBottom canonical nodes lookupBound reifyBound =
    inlineAliasBoundsWithBySeen
        fallbackToBottom
        canonical
        nodes
        lookupBound
        (\_ nid -> reifyBound nid)

inlineAliasBoundsWithBySeen
    :: Bool
    -> (NodeId -> NodeId)
    -> IntMap.IntMap TyNode
    -> (NodeId -> Maybe NodeId)
    -> (IntSet.IntSet -> NodeId -> Either err ElabType)
    -> ElabType
    -> ElabType
inlineAliasBoundsWithBySeen fallbackToBottom canonical nodes lookupBound reifyBound =
    goAlias IntSet.empty Set.empty
  where
    goAlias seen boundNames ty = case ty of
        TVar v
            | Set.member v boundNames -> ty
            | otherwise ->
                case parseNameId v of
                    Just nidInt ->
                        let nidC = canonical (NodeId nidInt)
                            key = getNodeId nidC
                            seen' = IntSet.insert key seen
                        in if IntSet.member key seen
                            then ty
                            else
                                case IntMap.lookup key nodes of
                                    Just TyVar{} ->
                                        case lookupBound nidC of
                                            Just bnd ->
                                                case reifyBound seen' (canonical bnd) of
                                                    Right ty' -> goAlias seen' boundNames ty'
                                                    Left _ -> ty
                                            Nothing -> if fallbackToBottom then TBottom else ty
                                    Just _ ->
                                        case reifyBound seen' nidC of
                                            Right ty' -> goAlias seen' boundNames ty'
                                            Left _ -> ty
                                    Nothing -> ty
                    Nothing -> ty
        TArrow a b -> TArrow (goAlias seen boundNames a) (goAlias seen boundNames b)
        TForall v mb body ->
            let boundNames' = Set.insert v boundNames
                mb' = fmap (goBound seen boundNames') mb
                body' = goAlias seen boundNames' body
            in TForall v mb' body'
        TBase _ -> ty
        TBottom -> ty

    goBound seen boundNames bound = case bound of
        TArrow a b -> TArrow (goAlias seen boundNames a) (goAlias seen boundNames b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body ->
            let boundNames' = Set.insert v boundNames
                mb' = fmap (goBound seen boundNames') mb
                body' = goAlias seen boundNames' body
            in TForall v mb' body'
