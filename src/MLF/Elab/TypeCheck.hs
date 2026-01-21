module MLF.Elab.TypeCheck (
    Env(..),
    emptyEnv,
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation
) where

import Data.Functor.Foldable (para)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types (BaseTy(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.Types
import MLF.Elab.TypeOps
    ( alphaEqType
    , freeTypeVarsType
    , freshTypeNameFromCounter
    , substTypeCapture
    )
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
                (fresh, k') = freshTypeNameFromCounter k used
            Right (k', TForall fresh Nothing t)
        InstElim -> case t of
            TForall v mbBound body -> do
                let bound = boundType mbBound
                Right (k, substTypeCapture v bound body)
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
            | v == old -> InstUnder v (fst i)
            | otherwise -> InstUnder v (snd i)
