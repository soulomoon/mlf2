module MLF.Elab.TypeCheck (
    Env(..),
    emptyEnv,
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Inst (InstEvalSpec(..), evalInstantiationWith, renameInstBound, schemeToType)
import MLF.Elab.Types
import MLF.Reify.TypeOps
    ( alphaEqType
    , freeTypeVarsType
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
checkInstantiation env ty inst = snd <$> evalInstantiationWith spec inst (0, env, ty)
  where
    spec :: InstEvalSpec Env TypeCheckError
    spec = InstEvalSpec
        { instBot = \tArg (k, _env', t) -> case t of
            TBottom -> Right (k, tArg)
            _ -> Left (TCInstantiationError (InstBot tArg) t ("InstBot expects TBottom, got " ++ pretty t))
        , instAbstr = \v (k, env', t) ->
            case Map.lookup v (typeEnv env') of
                Nothing -> Left (TCUnboundTypeVar v)
                Just bound ->
                    if alphaEqType t bound
                        then Right (k, TVar v)
                        else Left (TCInstantiationError (InstAbstr v) t ("InstAbstr expects bound " ++ pretty bound))
        , instElimError = \inst0 t ->
            TCInstantiationError inst0 t ("InstElim expects forall, got " ++ pretty t)
        , instInsideError = \_inst0 t ->
            TCInstantiationError InstId t ("InstInside expects forall, got " ++ pretty t)
        , instUnderError = \phiInst t ->
            TCInstantiationError phiInst t ("InstUnder expects forall, got " ++ pretty t)
        , instUnderEnv = \v bound env' ->
            env' { typeEnv = Map.insert v bound (typeEnv env') }
        , renameBound = renameInstBound
        }

litType :: Lit -> ElabType
litType lit = case lit of
    LInt _ -> TBase (BaseTy "Int")
    LBool _ -> TBase (BaseTy "Bool")
    LString _ -> TBase (BaseTy "String")

boundType :: Maybe BoundType -> ElabType
boundType = maybe TBottom tyToElab

freeTypeVarsEnv :: Env -> Set.Set String
freeTypeVarsEnv env =
    Set.union
        (Set.unions (map freeTypeVarsType (Map.elems (termEnv env))))
        (Set.unions (map freeTypeVarsType (Map.elems (typeEnv env))))
