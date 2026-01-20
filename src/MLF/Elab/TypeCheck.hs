module MLF.Elab.TypeCheck (
    Env(..),
    emptyEnv,
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation
) where

import Data.Functor.Foldable (cata)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types (BaseTy(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.Types
import MLF.Frontend.Syntax (Lit(..))

data Env = Env
    { termEnv :: Map.Map String ElabType
    , typeEnv :: Map.Map String ElabType
    } deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

typeCheck :: ElabTerm -> Either TypeCheckError ElabType
typeCheck = typeCheckWithEnv emptyEnv

typeCheckWithEnv :: Env -> ElabTerm -> Either TypeCheckError ElabType
typeCheckWithEnv env term = case term of
    EVar v ->
        case Map.lookup v (termEnv env) of
            Just ty -> Right ty
            Nothing -> Left (TCUnboundVar v)
    ELit lit -> Right (litType lit)
    ELam v ty body -> do
        let env' = env { termEnv = Map.insert v ty (termEnv env) }
        bodyTy <- typeCheckWithEnv env' body
        Right (TArrow ty bodyTy)
    EApp f a -> do
        fTy <- typeCheckWithEnv env f
        aTy <- typeCheckWithEnv env a
        case fTy of
            TArrow argTy resTy ->
                if alphaEqType argTy aTy
                    then Right resTy
                    else Left (TCArgumentMismatch argTy aTy)
            _ -> Left (TCExpectedArrow fTy)
    ELet v sch rhs body -> do
        rhsTy <- typeCheckWithEnv env rhs
        let schTy = schemeToType sch
        if alphaEqType rhsTy schTy
            then do
                let env' = env { termEnv = Map.insert v rhsTy (termEnv env) }
                typeCheckWithEnv env' body
            else Left (TCLetTypeMismatch rhsTy schTy)
    ETyAbs v mbBound body -> do
        let boundTy = boundType mbBound
        if v `Set.member` freeTypeVarsType boundTy
            then Left (TCTypeAbsBoundMentionsVar v)
            else if v `Set.member` freeTypeVarsEnv env
                then Left (TCTypeAbsVarInScope v)
                else do
                    let env' = env { typeEnv = Map.insert v boundTy (typeEnv env) }
                    bodyTy <- typeCheckWithEnv env' body
                    Right (TForall v mbBound bodyTy)
    ETyInst e inst -> do
        ty <- typeCheckWithEnv env e
        checkInstantiation env ty inst

checkInstantiation :: Env -> ElabType -> Instantiation -> Either TypeCheckError ElabType
checkInstantiation env ty inst = snd <$> go 0 env ty inst
  where
    go :: Int -> Env -> ElabType -> Instantiation -> Either TypeCheckError (Int, ElabType)
    go k env' t i = case i of
        InstId -> Right (k, t)
        InstSeq i1 i2 -> do
            (k1, t1) <- go k env' t i1
            go k1 env' t1 i2
        InstApp argTy ->
            go k env' t (InstSeq (InstInside (InstBot argTy)) InstElim)
        InstBot tArg -> case t of
            TBottom -> Right (k, tArg)
            _ -> Left (TCInstantiationError i t ("InstBot expects TBottom, got " ++ pretty t))
        InstAbstr v ->
            case Map.lookup v (typeEnv env') of
                Nothing -> Left (TCUnboundTypeVar v)
                Just bound ->
                    if alphaEqType t bound
                        then Right (k, TVar v)
                        else Left (TCInstantiationError i t ("InstAbstr expects bound " ++ pretty bound))
        InstIntro -> do
            let used = freeTypeVarsType t
                (fresh, k') = freshName k used
            Right (k', TForall fresh Nothing t)
        InstElim -> case t of
            TForall v mbBound body -> do
                let bound = boundType mbBound
                Right (k, substType v bound body)
            _ -> Left (TCInstantiationError i t ("InstElim expects forall, got " ++ pretty t))
        InstInside phi -> case t of
            TForall v mbBound body -> do
                let bound0 = boundType mbBound
                (k1, bound1) <- go k env' bound0 phi
                let mb' = if bound1 == TBottom then Nothing else Just bound1
                Right (k1, TForall v mb' body)
            _ -> Left (TCInstantiationError i t ("InstInside expects forall, got " ++ pretty t))
        InstUnder vParam phi -> case t of
            TForall v mbBound body -> do
                let bound0 = boundType mbBound
                    phi' = renameInstBound vParam v phi
                    env'' = env' { typeEnv = Map.insert v bound0 (typeEnv env') }
                (k1, body') <- go k env'' body phi'
                Right (k1, TForall v mbBound body')
            _ -> Left (TCInstantiationError i t ("InstUnder expects forall, got " ++ pretty t))

litType :: Lit -> ElabType
litType lit = case lit of
    LInt _ -> TBase (BaseTy "Int")
    LBool _ -> TBase (BaseTy "Bool")
    LString _ -> TBase (BaseTy "String")

boundType :: Maybe ElabType -> ElabType
boundType = maybe TBottom id

freeTypeVarsEnv :: Env -> Set.Set String
freeTypeVarsEnv env =
    Set.union
        (Set.unions (map freeTypeVarsType (Map.elems (termEnv env))))
        (Set.unions (map freeTypeVarsType (Map.elems (typeEnv env))))

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

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go Map.empty Map.empty
  where
    go envL envR t1 t2 = case (t1, t2) of
        (TVar a, TVar b) ->
            case Map.lookup a envL of
                Just b' -> b == b'
                Nothing -> case Map.lookup b envR of
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

freshName :: Int -> Set.Set String -> (String, Int)
freshName n used =
    let candidate = "u" ++ show n
    in if candidate `Set.member` used
        then freshName (n + 1) used
        else (candidate, n + 1)

substType :: String -> ElabType -> ElabType -> ElabType
substType x s t0 = case t0 of
    TVar v | v == x -> s
    TVar v -> TVar v
    TArrow a b -> TArrow (substType x s a) (substType x s b)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall v mb body
        | v == x -> TForall v (fmap (substType x s) mb) body
        | v `Set.member` freeS ->
            let v' = pickFresh v (Set.unions [freeS, freeTypeVarsType body, maybe Set.empty freeTypeVarsType mb])
                body' = substType v (TVar v') body
            in TForall v' (fmap (substType x s) mb) (substType x s body')
        | otherwise ->
            TForall v (fmap (substType x s) mb) (substType x s body)
  where
    freeS = freeTypeVarsType s

pickFresh :: String -> Set.Set String -> String
pickFresh base used =
    let candidates = base : [base ++ show i | i <- [(1::Int)..]]
    in case filter (`Set.notMember` used) candidates of
        (x:_) -> x
        [] -> base

renameInstBound :: String -> String -> Instantiation -> Instantiation
renameInstBound old new = goR
  where
    goR inst0 = case inst0 of
        InstId -> InstId
        InstApp t -> InstApp t
        InstBot t -> InstBot t
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstAbstr v -> InstAbstr (if v == old then new else v)
        InstInside i -> InstInside (goR i)
        InstSeq a b -> InstSeq (goR a) (goR b)
        InstUnder v i
            | v == old -> InstUnder v i
            | otherwise -> InstUnder v (goR i)
