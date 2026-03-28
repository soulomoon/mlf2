module MLF.Elab.TypeCheck
  ( Env (..),
    emptyEnv,
    typeCheck,
    typeCheckWithEnv,
    checkInstantiation,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Inst (InstEvalSpec (..), evalInstantiationWith, renameInstBound, schemeToType)
import MLF.Elab.Types
import MLF.Frontend.Syntax (Lit (..))
import MLF.Reify.TypeOps
  ( alphaEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarsType,
    matchType,
    splitForalls,
    substTypeCapture,
  )

data Env = Env
  { termEnv :: Map.Map String ElabType,
    typeEnv :: Map.Map String ElabType
  }
  deriving (Eq, Show)

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
    ensureContractiveType ty
    let env' = env {termEnv = Map.insert v ty (termEnv env)}
    bodyTy <- typeCheckWithEnv env' body
    Right (TArrow ty bodyTy)
  EApp f a -> do
    fTy <- typeCheckWithEnv env f
    aTy <- typeCheckWithEnv env a
    case fTy of
      TArrow argTy resTy ->
        if argTy == TBottom || alphaEqType argTy aTy
          then Right resTy
          else Left (TCArgumentMismatch argTy aTy)
      _ -> Left (TCExpectedArrow fTy)
  ELet v sch rhs body -> do
    ensureContractiveType (schemeToType sch)
    let schTy = schemeToType sch
        env' = env {termEnv = Map.insert v schTy (termEnv env)}
    rhsTy <- typeCheckWithEnv env' rhs
    if letSchemeAccepts rhsTy schTy
      then do
        typeCheckWithEnv env' body
      else Left (TCLetTypeMismatch rhsTy schTy)
  ETyAbs v mbBound body -> do
    maybe (Right ()) (ensureContractiveType . tyToElab) mbBound
    let boundTy = boundType mbBound
    if v `Set.member` freeTypeVarsType boundTy
      then Left (TCTypeAbsBoundMentionsVar v)
      else
        if v `Set.member` freeTypeVarsEnv env
          then Left (TCTypeAbsVarInScope v)
          else do
            let env' = env {typeEnv = Map.insert v boundTy (typeEnv env)}
            bodyTy <- typeCheckWithEnv env' body
            Right (TForall v mbBound bodyTy)
  ETyInst e inst -> do
    ensureContractiveInstantiation inst
    ty <- typeCheckWithEnv env e
    checkInstantiation env ty inst
  ERoll recursiveTy body -> do
    ensureContractiveType recursiveTy
    case recursiveTy of
      TMu name unfoldedBody -> do
        bodyTy <- typeCheckWithEnv env body
        let expectedBodyTy = substTypeCapture name recursiveTy unfoldedBody
            expectedBodyTyAlias = collapseRecursiveAlias name recursiveTy expectedBodyTy
        if alphaEqType expectedBodyTy bodyTy
          || alphaEqType expectedBodyTyAlias bodyTy
          || alphaEqType (TVar name) bodyTy
          then Right recursiveTy
          else Left (TCRollBodyMismatch expectedBodyTy bodyTy)
      _ -> Left (TCExpectedRecursive recursiveTy)
  EUnroll e -> do
    ty <- typeCheckWithEnv env e
    case ty of
      TMu name body -> Right (substTypeCapture name ty body)
      _ -> Left (TCExpectedRecursive ty)

ensureContractiveType :: ElabType -> Either TypeCheckError ()
ensureContractiveType ty = case firstNonContractiveRecursiveType ty of
  Just badTy -> Left (TCNonContractiveRecursiveType badTy)
  Nothing -> Right ()

ensureContractiveInstantiation :: Instantiation -> Either TypeCheckError ()
ensureContractiveInstantiation inst = case inst of
  InstId -> Right ()
  InstApp ty -> ensureContractiveType ty
  InstBot ty -> ensureContractiveType ty
  InstIntro -> Right ()
  InstElim -> Right ()
  InstAbstr _ -> Right ()
  InstUnder _ inner -> ensureContractiveInstantiation inner
  InstInside inner -> ensureContractiveInstantiation inner
  InstSeq a b -> ensureContractiveInstantiation a >> ensureContractiveInstantiation b

checkInstantiation :: Env -> ElabType -> Instantiation -> Either TypeCheckError ElabType
checkInstantiation env ty inst =
  (\(_, _, ty') -> ty') <$> evalInstantiationWith spec inst (0, env, ty)
  where
    spec :: InstEvalSpec Env TypeCheckError
    spec =
      InstEvalSpec
        { instBot = \tArg (k, env', t) -> case t of
            TBottom -> Right (k, env', tArg)
            _ -> Left (TCInstantiationError (InstBot tArg) t ("InstBot expects TBottom, got " ++ pretty t)),
          instAbstr = \v (k, env', t) ->
            case Map.lookup v (typeEnv env') of
              Nothing -> Left (TCUnboundTypeVar v)
              Just bound ->
                if alphaEqType t bound
                  then Right (k, env', TVar v)
                  else Left (TCInstantiationError (InstAbstr v) t ("InstAbstr expects bound " ++ pretty bound)),
          instElimError = \inst0 t ->
            TCInstantiationError inst0 t ("InstElim expects forall, got " ++ pretty t),
          instInsideError = \_inst0 t ->
            TCInstantiationError InstId t ("InstInside expects forall, got " ++ pretty t),
          instUnderError = \phiInst t ->
            TCInstantiationError phiInst t ("InstUnder expects forall, got " ++ pretty t),
          instElimEnv = \_v _replacement env' -> env',
          instUnderEnv = \v bound env' ->
            env' {typeEnv = Map.insert v bound (typeEnv env')},
          renameBound = renameInstBound
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

letSchemeAccepts :: ElabType -> ElabType -> Bool
letSchemeAccepts rhsTy schTy =
  alphaEqType rhsTy schTy || rhsIsInstanceOfScheme rhsTy schTy

rhsIsInstanceOfScheme :: ElabType -> ElabType -> Bool
rhsIsInstanceOfScheme rhsTy schTy =
  let (schBinds, schBody) = splitForalls schTy
      (rhsBinds, rhsBody) = splitForalls rhsTy
      schBinderNames = map fst schBinds
      sameBinderSpine =
        length schBinds == length rhsBinds
          && alphaEqType
            (rebuildForalls schBinds (TVar "_rhs_instance"))
            (rebuildForalls rhsBinds (TVar "_rhs_instance"))
   in sameBinderSpine
        && case matchType (Set.fromList schBinderNames) schBody rhsBody of
          Right _ -> True
          Left _ -> False

rebuildForalls :: [(String, Maybe BoundType)] -> ElabType -> ElabType
rebuildForalls binds body = foldr (\(v, bnd) t -> TForall v bnd t) body binds

collapseRecursiveAlias :: String -> ElabType -> ElabType -> ElabType
collapseRecursiveAlias muName recursiveTy = go
  where
    go ty
      | alphaEqType ty recursiveTy = TVar muName
      | otherwise =
          case ty of
            TArrow dom cod -> TArrow (go dom) (go cod)
            TCon con args -> TCon con (fmap go args)
            TForall v mb body -> TForall v (fmap goBound mb) (go body)
            TMu v body -> TMu v (go body)
            _ -> ty

    goBound bound = case bound of
      TArrow dom cod -> TArrow (go dom) (go cod)
      TCon con args -> TCon con (fmap go args)
      TForall v mb body -> TForall v (fmap goBound mb) (go body)
      TMu v body -> TMu v (go body)
      _ -> bound
