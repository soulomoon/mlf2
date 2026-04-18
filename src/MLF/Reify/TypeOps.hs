{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MLF.Reify.TypeOps
  ( splitForalls,
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
    churchMuEquivalent,
    churchAwareEqType,
    firstNonContractiveRecursiveType,
    matchType,
    parseNameId,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    resolveBoundBodyConstraint,
    inlineBaseBoundsType,
    inlineAliasBoundsWithBy,
    inlineAliasBoundsWithBySeen,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Names (freshNameLike, parseNameId)

newtype BoundFun (i :: TopVar) = BoundFun {runBoundFun :: Set.Set String -> Set.Set String}

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
      TConIF _ args ->
        BoundFun $ \boundSet ->
          foldr
            (\arg acc -> Set.union (runBoundFun arg boundSet) acc)
            Set.empty
            args
      TBaseIF _ -> BoundFun (const Set.empty)
      TBottomIF -> BoundFun (const Set.empty)
      TForallIF v mb body ->
        BoundFun $ \boundSet ->
          let boundBody = Set.insert v boundSet
              boundBound = if bindInBound then boundBody else boundSet
              freeBound = maybe Set.empty (\f -> runBoundFun f boundBound) mb
              freeBody = runBoundFun body boundBody
           in Set.union freeBound freeBody
      TMuIF v body ->
        BoundFun $ \boundSet ->
          runBoundFun body (Set.insert v boundSet)

substTypeCapture :: String -> ElabType -> ElabType -> ElabType
substTypeCapture x s = goSub
  where
    freeS = freeTypeVarsType s

    substBoundCaptureLocal :: String -> ElabType -> BoundType -> BoundType
    substBoundCaptureLocal name replacement bound = case bound of
      TArrow a b ->
        TArrow (substTypeCapture name replacement a) (substTypeCapture name replacement b)
      TCon c args -> TCon c (fmap (substTypeCapture name replacement) args)
      TBase b -> TBase b
      TBottom -> TBottom
      TForall v mb body
        | v == name ->
            let mb' = fmap (substBoundCaptureLocal name replacement) mb
             in TForall v mb' body
        | v `Set.member` freeS ->
            let used =
                  Set.unions
                    [ freeS,
                      freeTypeVarsType body,
                      maybe Set.empty freeTypeVarsType mb,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                body' = substTypeCapture v (TVar v') body
                mb' = fmap (substBoundCaptureLocal name replacement) mb
             in TForall v' mb' (substTypeCapture name replacement body')
        | otherwise ->
            let mb' = fmap (substBoundCaptureLocal name replacement) mb
             in TForall v mb' (substTypeCapture name replacement body)
      TMu v body
        | v == name -> TMu v body
        | v `Set.member` freeS ->
            let used =
                  Set.unions
                    [ freeS,
                      freeTypeVarsType body,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                body' = substTypeCapture v (TVar v') body
             in TMu v' (substTypeCapture name replacement body')
        | otherwise ->
            TMu v (substTypeCapture name replacement body)

    goSub = paraIx alg
      where
        alg :: TyIF i (IxPair Ty Ty) -> Ty i
        alg ty = case ty of
          TVarIF v
            | v == x -> s
            | otherwise -> TVar v
          TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
          TConIF c args -> TCon c (fmap (snd . unIxPair) args)
          TBaseIF b -> TBase b
          TBottomIF -> TBottom
          TForallIF v mb body
            | v == x ->
                let mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                 in TForall v mb' (fst (unIxPair body))
            | v `Set.member` freeS ->
                let used =
                      Set.unions
                        [ freeS,
                          freeTypeVarsType (fst (unIxPair body)),
                          maybe Set.empty (freeTypeVarsType . fst . unIxPair) mb,
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    body' = substTypeCapture v (TVar v') (fst (unIxPair body))
                    mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                 in TForall v' mb' (substTypeCapture x s body')
            | otherwise ->
                let mb' = fmap (substBoundCaptureLocal x s . fst . unIxPair) mb
                 in TForall v mb' (snd (unIxPair body))
          TMuIF v body
            | v == x -> TMu v (fst (unIxPair body))
            | v `Set.member` freeS ->
                let used =
                      Set.unions
                        [ freeS,
                          freeTypeVarsType (fst (unIxPair body)),
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    body' = substTypeCapture v (TVar v') (fst (unIxPair body))
                 in TMu v' (substTypeCapture x s body')
            | otherwise ->
                TMu v (snd (unIxPair body))

substTypeSimple :: String -> ElabType -> ElabType -> ElabType
substTypeSimple name replacement = paraIx alg
  where
    substBoundSimpleLocal :: String -> ElabType -> BoundType -> BoundType
    substBoundSimpleLocal name0 replacement0 bound = case bound of
      TArrow a b ->
        TArrow (substTypeSimple name0 replacement0 a) (substTypeSimple name0 replacement0 b)
      TCon c args -> TCon c (fmap (substTypeSimple name0 replacement0) args)
      TBase b -> TBase b
      TBottom -> TBottom
      TForall v mb body
        | v == name0 ->
            let mb' = fmap (substBoundSimpleLocal name0 replacement0) mb
             in TForall v mb' body
        | otherwise ->
            let mb' = fmap (substBoundSimpleLocal name0 replacement0) mb
             in TForall v mb' (substTypeSimple name0 replacement0 body)
      TMu v body
        | v == name0 -> TMu v body
        | otherwise -> TMu v (substTypeSimple name0 replacement0 body)

    alg :: TyIF i (IxPair Ty Ty) -> Ty i
    alg ty = case ty of
      TVarIF v
        | v == name -> replacement
        | otherwise -> TVar v
      TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
      TConIF c args -> TCon c (fmap (snd . unIxPair) args)
      TBaseIF b -> TBase b
      TBottomIF -> TBottom
      TForallIF v mb body
        | v == name ->
            let mb' = fmap (substBoundSimpleLocal name replacement . fst . unIxPair) mb
             in TForall v mb' (fst (unIxPair body))
        | otherwise ->
            let mb' = fmap (substBoundSimpleLocal name replacement . fst . unIxPair) mb
             in TForall v mb' (snd (unIxPair body))
      TMuIF v body
        | v == name -> TMu v (fst (unIxPair body))
        | otherwise -> TMu v (snd (unIxPair body))

freshTypeName :: Set.Set String -> String
freshTypeName used =
  let candidates = ["u" ++ show i | i <- [(0 :: Int) ..]]
   in case filter (`Set.notMember` used) candidates of
        (x : _) -> x
        [] -> "u0"

freshTypeNameFromCounter :: Int -> Set.Set String -> (String, Int)
freshTypeNameFromCounter n used =
  let candidate = "u" ++ show n
   in if candidate `Set.member` used
        then freshTypeNameFromCounter (n + 1) used
        else (candidate, n + 1)

renameTypeVar :: String -> String -> ElabType -> ElabType
renameTypeVar old new = substTypeSimple old (TVar new)

-- | Return the first explicit recursive type that violates the M4 v1
-- contractiveness policy.
--
-- Contractive occurrences must be guarded by an arrow or constructor node.
-- `forall` binders do not introduce a guard, but they still shadow the
-- recursive variable when names coincide.
firstNonContractiveRecursiveType :: ElabType -> Maybe ElabType
firstNonContractiveRecursiveType = goType
  where
    goType :: ElabType -> Maybe ElabType
    goType ty = case ty of
      TVar _ -> Nothing
      TArrow a b -> goType a <|> goType b
      TCon _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TBase _ -> Nothing
      TBottom -> Nothing
      TForall _ mb body -> maybe Nothing goBound mb <|> goType body
      TMu v body
        | muBodyContractive v body -> goType body
        | otherwise -> Just ty

    goBound :: BoundType -> Maybe ElabType
    goBound bound = case bound of
      TArrow a b -> goType a <|> goType b
      TCon _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TBase _ -> Nothing
      TBottom -> Nothing
      TForall _ mb body -> maybe Nothing goBound mb <|> goType body
      TMu v body
        | muBodyContractive v body -> goType body
        | otherwise -> Just (tyToElab bound)

    muBodyContractive :: String -> ElabType -> Bool
    muBodyContractive needle = bodyType False False
      where
        bodyType :: Bool -> Bool -> ElabType -> Bool
        bodyType guarded shadowed ty = case ty of
          TVar v -> shadowed || v /= needle || guarded
          TArrow a b -> bodyType True shadowed a && bodyType True shadowed b
          TCon _ args -> all (bodyType True shadowed) args
          TBase _ -> True
          TBottom -> True
          TForall v mb body ->
            let shadowed' = shadowed || v == needle
                boundOk = maybe True (bodyBound guarded shadowed') mb
             in boundOk && bodyType guarded shadowed' body
          TMu v body ->
            let shadowed' = shadowed || v == needle
             in bodyType guarded shadowed' body

        bodyBound :: Bool -> Bool -> BoundType -> Bool
        bodyBound guarded shadowed bound = case bound of
          TArrow a b -> bodyType True shadowed a && bodyType True shadowed b
          TCon _ args -> all (bodyType True shadowed) args
          TBase _ -> True
          TBottom -> True
          TForall v mb body ->
            let shadowed' = shadowed || v == needle
                boundOk = maybe True (bodyBound guarded shadowed') mb
             in boundOk && bodyType guarded shadowed' body
          TMu v body ->
            let shadowed' = shadowed || v == needle
             in bodyType guarded shadowed' body

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
      (TCon c1 args1, TCon c2 args2) ->
        c1 == c2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBase b1, TBase b2) -> b1 == b2
      (TBottom, TBottom) -> True
      (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMu v1 body1, TMu v2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in go envL' envR' body1 body2
      _ -> False

    alphaEqArgs envL envR as bs = case (as, bs) of
      ([], []) -> True
      (a : as', b : bs') -> go envL envR a b && alphaEqArgs envL envR as' bs'
      _ -> False

    alphaEqMaybeBound envL envR mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> True
      (Just b1, Just b2) -> alphaEqBound envL envR b1 b2
      _ -> False

    alphaEqBound envL envR b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') ->
        go envL envR a1 a2 && go envL envR b1' b2'
      (TCon c1 args1, TCon c2 args2) ->
        c1 == c2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBase b1', TBase b2') -> b1' == b2'
      (TBottom, TBottom) -> True
      (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMu v1 body1, TMu v2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in go envL' envR' body1 body2
      _ -> False

{- Note [churchMuEquivalent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Church-encoded recursive ADTs produce μ types of the form

  μself. ∀result. body(self, result)

When the presolver processes constraint graphs, it correctly instantiates
the leading ∀result inside the μ body.  The reified inferred type is then

  μself'. body'(selfAlias, result')

where result' is a free variable (the instantiated result binder) and
selfAlias may differ from self' due to presolution variable aliasing.

This helper recognises the two representations as equivalent by:
  1. Stripping leading unbounded TForall from each μ body.
  2. Repairing a "self alias" — a free var in the body that structurally
     occupies the self-reference position but has a different name than the
     μ binder (a presolution/reification artifact).
  3. Using matchType with the stripped quantifier vars as matchable
     placeholders to align the remaining free variables.

IMPORTANT: This is NOT a general semantic equivalence.  It is a narrow
Church-encoding-specific comparison, intentionally kept out of alphaEqType.
-}

-- | Check whether two types are equivalent Church-encoded μ types.
-- Handles the specific mismatch between annotated μ types (which retain
-- inner ∀result) and inferred μ types (where ∀result was instantiated and
-- the self-variable may be aliased).
churchMuEquivalent :: ElabType -> ElabType -> Bool
churchMuEquivalent t1 t2 =
  case (t1, t2) of
    (TMu v1 body1, TMu v2 body2) ->
      let (qs1, core1) = stripChurchForalls body1
          (qs2, core2) = stripChurchForalls body2
          qset1 = Set.fromList qs1
          qset2 = Set.fromList qs2
          -- All quantifier vars from both sides are matchable
          allQs = Set.union qset1 qset2
       in or
            [ tryMatch allQs (TMu v1 c1) (TMu v2 c2)
              | c1 <- selfAliasVariants qset1 v1 core1,
                c2 <- selfAliasVariants qset2 v2 core2
            ]
    _ -> False
  where
    -- Strip leading unbounded TForall from a μ body.
    stripChurchForalls :: ElabType -> ([String], ElabType)
    stripChurchForalls ty = case ty of
      TForall v Nothing body ->
        let (vs, core) = stripChurchForalls body
         in (v : vs, core)
      _ -> ([], ty)

    -- Generate candidate cores where a free-variable self-alias is replaced
    -- by the actual μ binder name.
    selfAliasVariants :: Set.Set String -> String -> ElabType -> [ElabType]
    selfAliasVariants quantVars muVar core
      | muVar `Set.member` freeTypeVarsType core = [core]
      | otherwise =
          -- The mu binder doesn't appear in the body — some alias does.
          -- Try each free var (that isn't a stripped quantifier) as the alias.
          core
            : [ substTypeSimple alias (TVar muVar) core
                | alias <- Set.toList (freeTypeVarsType core `Set.difference` quantVars)
              ]

    tryMatch :: Set.Set String -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchType matchableVars lhs rhs)
        || isRight (matchType matchableVars rhs lhs)

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

-- | Structural equality that falls back to 'churchMuEquivalent' at TMu-vs-TMu
-- junctions.  Keeps 'alphaEqType' strict while allowing Church-μ relaxation
-- at every nesting depth.
churchAwareEqType :: ElabType -> ElabType -> Bool
churchAwareEqType = go Map.empty Map.empty
  where
    stripChurchForalls :: ElabType -> ([String], ElabType)
    stripChurchForalls ty = case ty of
      TForall v Nothing body ->
        let (vs, core) = stripChurchForalls body
         in (v : vs, core)
      _ -> ([], ty)

    selfAliasVariants :: Set.Set String -> String -> ElabType -> [ElabType]
    selfAliasVariants quantVars muVar core
      | muVar `Set.member` freeTypeVarsType core = [core]
      | otherwise =
          core
            : [ substTypeSimple alias (TVar muVar) core
                | alias <- Set.toList (freeTypeVarsType core `Set.difference` quantVars)
              ]

    tryMatch :: Set.Set String -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchType matchableVars lhs rhs)
        || isRight (matchType matchableVars rhs lhs)

    churchMuMatchesCore :: ElabType -> ElabType -> Bool
    churchMuMatchesCore muTy@(TMu v body) otherTy =
      let muTy' = tyToElab muTy
          (quantVars, coreBody) = stripChurchForalls (tyToElab body)
          (_, unfoldedCoreBody) = stripChurchForalls (substTypeSimple v muTy' (tyToElab body))
          qset = Set.fromList quantVars
          candidateBodies =
            selfAliasVariants qset v coreBody
              ++ selfAliasVariants qset v unfoldedCoreBody
          (_, otherCore) = stripChurchForalls otherTy
       in or
            [ tryMatch qset candidate otherTy || tryMatch qset candidate otherCore
              | candidate <- candidateBodies
            ]
    churchMuMatchesCore _ _ = False

    unfoldMuOnce :: Ty v -> Maybe ElabType
    unfoldMuOnce muTy@(TMu v body) =
      let muTy' = tyToElab muTy
          unfolded = substTypeSimple v muTy' (tyToElab body)
       in if alphaEqType unfolded muTy' then Nothing else Just unfolded
    unfoldMuOnce _ = Nothing

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
      (TCon c1 args1, TCon c2 args2) ->
        c1 == c2 && eqArgs envL envR (toList args1) (toList args2)
      (TBase b1, TBase b2) -> b1 == b2
      (TBottom, TBottom) -> True
      (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in eqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMu v1 body1, TMu v2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in go envL' envR' body1 body2
              || churchMuEquivalent t1 t2
      (TMu {}, _) ->
        churchMuMatchesCore t1 t2
          || maybe False (\unfolded -> go envL envR unfolded t2) (unfoldMuOnce t1)
      (_, TMu {}) ->
        churchMuMatchesCore t2 t1
          || maybe False (\unfolded -> go envL envR t1 unfolded) (unfoldMuOnce t2)
      _ -> False

    eqArgs envL envR as bs = case (as, bs) of
      ([], []) -> True
      (a : as', b : bs') -> go envL envR a b && eqArgs envL envR as' bs'
      _ -> False

    eqMaybeBound envL envR mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> True
      (Just b1, Just b2) -> eqBound envL envR b1 b2
      _ -> False

    eqBound :: Map.Map String String -> Map.Map String String -> BoundType -> BoundType -> Bool
    eqBound envL envR b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') ->
        go envL envR a1 a2 && go envL envR b1' b2'
      (TCon c1 args1, TCon c2 args2) ->
        c1 == c2 && eqArgs envL envR (toList args1) (toList args2)
      (TBase b1', TBase b2') -> b1' == b2'
      (TBottom, TBottom) -> True
      (TForall v1 mb1 body1, TForall v2 mb2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in eqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMu v1 body1, TMu v2 body2) ->
        let envL' = Map.insert v1 v2 envL
            envR' = Map.insert v2 v1 envR
         in go envL' envR' body1 body2
              || churchMuEquivalent (TMu v1 body1) (TMu v2 body2)
      (TMu {}, _) ->
        churchMuMatchesCore (tyToElab b1) (tyToElab b2)
          || maybe False (\unfolded -> go envL envR unfolded (tyToElab b2)) (unfoldMuOnce b1)
      (_, TMu {}) ->
        churchMuMatchesCore (tyToElab b2) (tyToElab b1)
          || maybe False (\unfolded -> go envL envR (tyToElab b1) unfolded) (unfoldMuOnce b2)
      _ -> False

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

matchType ::
  Set.Set String ->
  ElabType ->
  ElabType ->
  Either ElabError (Map.Map String ElabType)
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
      (TCon c0 args0, TCon c1 args1)
        | c0 == c1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TBase b0, TBase b1)
        | b0 == b1 -> Right subst
      (TBottom, TBottom) -> Right subst
      (TForall v mb b, TForall v' mb' b') -> do
        subst1 <- case (mb, mb') of
          (Nothing, Nothing) -> Right subst
          (Just x, Just y) -> matchBound env subst x y
          _ -> Left (InstantiationError "matchType: forall bound mismatch")
        goMatch (Map.insert v v' env) subst1 b b'
      (TMu v b, TMu v' b') ->
        goMatch (Map.insert v v' env) subst b b'
      _ -> Left (InstantiationError "matchType: structure mismatch")

    matchArgs env subst0 argsP argsT = case (argsP, argsT) of
      ([], []) -> Right subst0
      (a : as, b : bs) -> do
        subst1 <- goMatch env subst0 a b
        matchArgs env subst1 as bs
      _ -> Left (InstantiationError "matchType: structure mismatch")

    matchBound env subst boundP boundT = case (boundP, boundT) of
      (TArrow a b, TArrow a' b') -> do
        subst1 <- goMatch env subst a a'
        goMatch env subst1 b b'
      (TCon c0 args0, TCon c1 args1)
        | c0 == c1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TBase b0, TBase b1)
        | b0 == b1 -> Right subst
      (TBottom, TBottom) -> Right subst
      (TForall v mb b, TForall v' mb' b') -> do
        subst1 <- case (mb, mb') of
          (Nothing, Nothing) -> Right subst
          (Just x, Just y) -> matchBound env subst x y
          _ -> Left (InstantiationError "matchType: forall bound mismatch")
        goMatch (Map.insert v v' env) subst1 b b'
      (TMu v b, TMu v' b') ->
        goMatch (Map.insert v v' env) subst b b'
      _ -> Left (InstantiationError "matchType: structure mismatch")

resolveBaseBoundForInstConstraint ::
  Constraint ->
  (NodeId -> NodeId) ->
  NodeId ->
  Maybe NodeId
resolveBaseBoundForInstConstraint constraint canonical start =
  let nodes = cNodes constraint
      goResolve visited nid0 =
        let nid = canonical nid0
            key = getNodeId nid
         in if IntSet.member key visited
              then Nothing
              else case lookupNodeIn nodes nid of
                Just TyBase {} -> Just nid
                Just TyBottom {} -> Just nid
                Just TyVar {} ->
                  case VarStore.lookupVarBound constraint nid of
                    Just bnd -> goResolve (IntSet.insert key visited) bnd
                    Nothing -> Nothing
                _ -> Nothing
   in goResolve IntSet.empty start

resolveBaseBoundForInstSolved :: Solved.Solved -> NodeId -> Maybe NodeId
resolveBaseBoundForInstSolved solved =
  let constraint = Solved.originalConstraint solved
      canonical = Solved.canonical solved
   in resolveBaseBoundForInstConstraint constraint canonical

resolveBoundBodyConstraint ::
  (NodeId -> NodeId) ->
  Constraint ->
  IntSet.IntSet ->
  NodeId ->
  NodeId
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
                      Just TyBase {tnBase = b} -> TBase b
                      Just TyBottom {} -> TBottom
                      _ -> TVar v
                  Nothing -> TVar v
          Nothing -> TVar v
      TArrowIF a b -> TArrow a b
      TForallIF v mb body -> TForall v mb body
      TMuIF v body -> TMu v body
      TConIF c args -> TCon c args
      TBaseIF b -> TBase b
      TBottomIF -> TBottom

-- | Inline alias/bound nodes in an ElabType using the supplied lookup and reify
-- functions. This is the shared implementation for scope-aware bound/alias
-- inlining; callers can wrap it with concrete environment data.
inlineAliasBoundsWithBy ::
  Bool ->
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  (NodeId -> Maybe NodeId) ->
  (NodeId -> Either err ElabType) ->
  ElabType ->
  ElabType
inlineAliasBoundsWithBy fallbackToBottom canonical nodes lookupBound reifyBound =
  inlineAliasBoundsWithBySeen
    fallbackToBottom
    canonical
    nodes
    lookupBound
    (\_ nid -> reifyBound nid)

inlineAliasBoundsWithBySeen ::
  Bool ->
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  (NodeId -> Maybe NodeId) ->
  (IntSet.IntSet -> NodeId -> Either err ElabType) ->
  ElabType ->
  ElabType
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
                      else case lookupNodeIn nodes nidC of
                        Just TyVar {} ->
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
      TCon c args -> TCon c (fmap (goAlias seen boundNames) args)
      TForall v mb body ->
        let boundNames' = Set.insert v boundNames
            mb' = fmap (goBound seen boundNames') mb
            body' = goAlias seen boundNames' body
         in TForall v mb' body'
      TMu v body ->
        let boundNames' = Set.insert v boundNames
         in TMu v (goAlias seen boundNames' body)
      TBase _ -> ty
      TBottom -> ty

    goBound seen boundNames bound = case bound of
      TArrow a b -> TArrow (goAlias seen boundNames a) (goAlias seen boundNames b)
      TCon c args -> TCon c (fmap (goAlias seen boundNames) args)
      TBase b -> TBase b
      TBottom -> TBottom
      TForall v mb body ->
        let boundNames' = Set.insert v boundNames
            mb' = fmap (goBound seen boundNames') mb
            body' = goAlias seen boundNames' body
         in TForall v mb' body'
      TMu v body ->
        let boundNames' = Set.insert v boundNames
         in TMu v (goAlias seen boundNames' body)
