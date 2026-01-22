module MLF.Elab.TypeOps (
    splitForalls,
    stripForallsType,
    freeTypeVarsType,
    substTypeCapture,
    substTypeSimple,
    freshNameLike,
    freshTypeName,
    freshTypeNameFromCounter,
    alphaEqType,
    matchType,
    parseNameId,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    inlineBaseBoundsType
) where

import Data.Functor.Foldable (cata, para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Elab.Generalize.Names (parseNameId)
import MLF.Elab.Types

splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)
splitForalls = para alg
  where
    alg ty = case ty of
        TForallF v mb bodyPair ->
            let mbOrig = fmap fst mb
                (binds, body) = snd bodyPair
            in ((v, mbOrig) : binds, body)
        TVarF v -> ([], TVar v)
        TArrowF (d, _) (c, _) -> ([], TArrow d c)
        TBaseF b -> ([], TBase b)
        TBottomF -> ([], TBottom)

stripForallsType :: ElabType -> ElabType
stripForallsType = snd . splitForalls

freeTypeVarsType :: ElabType -> Set.Set String
freeTypeVarsType = cata alg
  where
    alg ty = case ty of
        TVarF v -> Set.singleton v
        TArrowF a b -> Set.union a b
        TBaseF _ -> Set.empty
        TBottomF -> Set.empty
        TForallF v mb body ->
            let boundFv = maybe Set.empty id mb
                bodyFv = Set.delete v body
            in Set.union boundFv bodyFv

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

substTypeCapture :: String -> ElabType -> ElabType -> ElabType
substTypeCapture x s = goSub
  where
    freeS = freeTypeVarsType s

    goSub = para alg
      where
        alg ty = case ty of
            TVarF v
                | v == x -> s
                | otherwise -> TVar v
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF v mb body
                | v == x ->
                    let mb' = fmap snd mb
                    in TForall v mb' (fst body)
                | v `Set.member` freeS ->
                    let used =
                            Set.unions
                                [ freeS
                                , freeTypeVarsType (fst body)
                                , maybe Set.empty (freeTypeVarsType . fst) mb
                                , Set.singleton v
                                ]
                        v' = freshNameLike v used
                        body' = substTypeCapture v (TVar v') (fst body)
                    in TForall v' (fmap snd mb) (goSub body')
                | otherwise ->
                    TForall v (fmap snd mb) (snd body)

substTypeSimple :: String -> ElabType -> ElabType -> ElabType
substTypeSimple name replacement = para alg
  where
    alg ty = case ty of
        TVarF v
            | v == name -> replacement
            | otherwise -> TVar v
        TArrowF d c -> TArrow (snd d) (snd c)
        TBaseF b -> TBase b
        TBottomF -> TBottom
        TForallF v mb t'
            | v == name -> TForall v (fmap fst mb) (fst t')
            | otherwise -> TForall v (fmap snd mb) (snd t')

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go Map.empty Map.empty
  where
    boundType = maybe TBottom id
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
            let bound1 = boundType mb1
                bound2 = boundType mb2
                envL' = Map.insert v1 v2 envL
                envR' = Map.insert v2 v1 envR
            in go envL envR bound1 bound2 && go envL' envR' body1 body2
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
                (Just x, Just y) -> goMatch env subst x y
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

inlineBaseBoundsType :: Constraint -> (NodeId -> NodeId) -> ElabType -> ElabType
inlineBaseBoundsType constraint canonical = cata alg
  where
    alg ty = case ty of
        TVarF v ->
            case parseNameId v of
                Just nidInt ->
                    let nid = NodeId nidInt
                    in case resolveBaseBoundForInstConstraint constraint canonical nid of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) (cNodes constraint) of
                                Just TyBase{ tnBase = b } -> TBase b
                                Just TyBottom{} -> TBottom
                                _ -> TVar v
                        Nothing -> TVar v
                Nothing -> TVar v
        TArrowF a b -> TArrow a b
        TForallF v mb body -> TForall v mb body
        TBaseF b -> TBase b
        TBottomF -> TBottom
