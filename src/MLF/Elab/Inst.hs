{-# LANGUAGE GADTs #-}
module MLF.Elab.Inst (
    InstEvalSpec(..),
    applyInstantiation,
    composeInst,
    evalInstantiationWith,
    instMany,
    renameInstBound,
    schemeToType,
    splitForalls
) where

import qualified Data.Map.Strict as Map
import Data.Functor.Foldable (para)

import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, freshTypeNameFromCounter, substTypeCapture, splitForalls)

-- | Turn a scheme into its corresponding type (nested `∀`).
schemeToType :: ElabScheme -> ElabType
schemeToType (Forall binds body) =
    buildForalls binds body

composeInst :: Instantiation -> Instantiation -> Instantiation
composeInst InstId i = i
composeInst i InstId = i
composeInst i1 i2 = InstSeq i1 i2

instMany :: [Instantiation] -> Instantiation
instMany = foldr composeInst InstId

data InstEvalSpec env err = InstEvalSpec
    { instBot :: ElabType -> (Int, env, ElabType) -> Either err (Int, env, ElabType)
    , instAbstr :: String -> (Int, env, ElabType) -> Either err (Int, env, ElabType)
    , instElimError :: Instantiation -> ElabType -> err
    , instInsideError :: Instantiation -> ElabType -> err
    , instUnderError :: Instantiation -> ElabType -> err
    , instElimEnv :: String -> ElabType -> env -> env
    , instUnderEnv :: String -> ElabType -> env -> env
    , renameBound :: String -> String -> Instantiation -> Instantiation
    }

evalInstantiationWith
    :: InstEvalSpec env err
    -> Instantiation
    -> (Int, env, ElabType)
    -> Either err (Int, env, ElabType)
evalInstantiationWith spec inst = eval inst
  where
    eval = para instAlg

    instElimFn errInst (k, env', t) = case t of
        TForall v mbBound body -> do
            let bTy = maybe TBottom tyToElab mbBound
                env'' = instElimEnv spec v bTy env'
            Right (k, env'', substTypeCapture v bTy body)
        _ -> Left (instElimError spec errInst t)

    instInsideFn errInst phiFn (k, env', t) = case t of
        TForall v mbBound body -> do
            let b0 = maybe TBottom tyToElab mbBound
            (k1, _env'', b1) <- phiFn (k, env', b0)
            let mb' = case b1 of
                    TBottom -> Nothing
                    TVar{} -> Nothing
                    _ -> either (const Nothing) Just (elabToBound b1)
            Right (k1, env', TForall v mb' body)
        _ -> Left (instInsideError spec errInst t)

    -- InstApp applies a concrete type argument directly to the front forall,
    -- but first validates it against the binder bound via instBot semantics.
    -- For explicit non-bottom bounds, a bound-matching InstApp is accepted
    -- directly and substitutes the binder with that bound type.
    instAppFn argTy (k, env', t) = case t of
        TForall v mbBound body -> do
            let b0 = maybe TBottom tyToElab mbBound
            (k1, env'', checkedArg) <-
                case mbBound of
                    Just _ | alphaEqType argTy b0 ->
                        Right (k, env', b0)
                    _ ->
                        instBot spec argTy (k, env', b0)
            let env''' = instElimEnv spec v checkedArg env''
            Right (k1, env''', substTypeCapture v checkedArg body)
        _ ->
            Left
                (instElimError spec (InstSeq (InstInside (InstBot argTy)) InstElim) t)

    instAlg inst0 = case inst0 of
        InstIdF -> \(k, env', t) -> Right (k, env', t)
        InstSeqF (left, i1) (right, i2) ->
            \(k, env', t) ->
                case (left, right) of
                    (InstInside (InstAbstr v), InstElim) ->
                        case t of
                            TForall name _mbBound body ->
                                let env'' = instElimEnv spec name (TVar v) env'
                                in Right (k, env'', substTypeCapture name (TVar v) body)
                            _ -> Left (instElimError spec InstElim t)
                    _ -> do
                        (k1, env'', t1) <- i1 (k, env', t)
                        i2 (k1, env'', t1)
        InstAppF argTy -> instAppFn argTy
        InstBotF tArg -> instBot spec tArg
        InstAbstrF v -> instAbstr spec v
        InstIntroF ->
            \(k, env', t) -> do
                let used = freeTypeVarsType t
                    (fresh, k') = freshTypeNameFromCounter k used
                Right (k', env', TForall fresh Nothing t)
        InstElimF -> instElimFn InstElim
        InstInsideF (_, phiFn) -> instInsideFn InstId phiFn
        InstUnderF vParam (phiInst, _phiFn) ->
            \(k, env', t) -> case t of
                TForall v mbBound body -> do
                    let b0 = maybe TBottom tyToElab mbBound
                        env'' = instUnderEnv spec v b0 env'
                        phi' = renameBound spec vParam v phiInst
                    (k1, _env''', body') <- eval phi' (k, env'', body)
                    Right (k1, env', TForall v mbBound body')
                _ -> Left (instUnderError spec phiInst t)

-- | Apply an xMLF instantiation to an xMLF type (xmlf Fig. 3).
--
-- This is a *partial* function: it fails if the instantiation expects a certain
-- type form (e.g. ∀ for `N`) but the type does not match.
applyInstantiation :: ElabType -> Instantiation -> Either ElabError ElabType
applyInstantiation ty inst = (\(_, _, ty') -> ty') <$> evalInstantiationWith spec inst (0, Map.empty, ty)
  where
    resolveReplayVars :: Map.Map String ElabType -> ElabType -> ElabType
    resolveReplayVars replayEnv ty0 =
        Map.foldlWithKey'
            (\tyAcc var replacement -> substTypeCapture var replacement tyAcc)
            ty0
            replayEnv

    allowReplayBoundMatch :: Map.Map String ElabType -> ElabType -> ElabType -> Bool
    allowReplayBoundMatch replayEnv tArg t =
        let resolvedArg = resolveReplayVars replayEnv tArg
        in not (alphaEqType resolvedArg tArg)
            && alphaEqType resolvedArg t

    spec = InstEvalSpec
        { instBot = \tArg (k, replayEnv, t) -> case t of
            TBottom -> Right (k, replayEnv, tArg)
            _
                | allowReplayBoundMatch replayEnv tArg t ->
                    Right (k, replayEnv, t)
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t))
        , instAbstr = \v (k, replayEnv, _t) -> Right (k, replayEnv, TVar v)
        , instElimError = \_inst0 t ->
            InstantiationError ("InstElim expects ∀, got: " ++ pretty t)
        , instInsideError = \_inst0 t ->
            InstantiationError ("InstInside expects ∀, got: " ++ pretty t)
        , instUnderError = \_inst0 t ->
            InstantiationError ("InstUnder expects ∀, got: " ++ pretty t)
        , instElimEnv = \v replacement replayEnv -> Map.insert v replacement replayEnv
        , instUnderEnv = \_v _bound replayEnv -> replayEnv
        , renameBound = renameInstBound
        }


-- Rename bound variable occurrences inside an instantiation body.
-- This is α-renaming of the instantiation’s binder: occurrences of `old`
-- are renamed to `new`, except under a nested `∀(old ⩾)` which re-binds it.
renameInstBound :: String -> String -> Instantiation -> Instantiation
renameInstBound old new = para alg
  where
    alg inst0 = case inst0 of
        InstIdF -> InstId
        InstAppF t -> InstApp t
        InstBotF t -> InstBot t
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v -> InstAbstr (if v == old then new else v)
        InstInsideF i -> InstInside (snd i)
        InstSeqF a b -> InstSeq (snd a) (snd b)
        InstUnderF v i
            | v == old -> InstUnder v (fst i)  -- shadowing: stop renaming under this binder
            | otherwise -> InstUnder v (snd i)
