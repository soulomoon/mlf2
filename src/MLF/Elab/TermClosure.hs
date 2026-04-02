{-# LANGUAGE GADTs #-}
module MLF.Elab.TermClosure (
    closeTermWithSchemeSubstIfNeeded,
    alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    preserveRetainedChildAuthoritativeResult,
    substInTerm
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (Env(..), emptyEnv, typeCheck, typeCheckWithEnv)
import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, freshNameLike, substTypeSimple)
import MLF.Util.Names (parseNameId)

closeTermWithSchemeSubstIfNeeded :: IntMap.IntMap String -> ElabScheme -> ElabTerm -> ElabTerm
closeTermWithSchemeSubstIfNeeded subst sch term =
    let (subst', sch', renames) = freshenSchemeAndSubstAgainstTerm term subst sch
        termSubst = renameTermTypeVars renames (substInTerm subst' term)
        schemeTy = schemeToType sch'
    in case typeCheck termSubst of
        Right ty | alphaEqType ty schemeTy -> termSubst
        Right _ ->
            case alignTermTypeVarsToScheme sch' termSubst of
                Just termAligned -> termAligned
                Nothing ->
                    let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
                    in wrapTermWithScheme sch' termAlignedBody
        _ ->
            let termAlignedBody = maybe termSubst id (alignTermTypeVarsToSchemeBody sch' termSubst)
            in wrapTermWithScheme sch' termAlignedBody

preserveRetainedChildAuthoritativeResult :: ElabTerm -> Maybe ElabTerm
preserveRetainedChildAuthoritativeResult = go emptyEnv
  where
    go env term = case term of
        ELet v sch rhs body
            | isTrivialRetainedChildBody v body
            , isForallIdentityScheme sch ->
                case typeCheckWithEnv env rhs of
                    Right rhsTy
                        | hasRecursiveComponent rhsTy ->
                            Just rhs
                    _ -> descend env v sch rhs body
            | Just bodyPreserved <- preserveRetainedChildAliasBoundary env v sch rhs body ->
                Just bodyPreserved
            | otherwise -> descend env v sch rhs body
        ETyAbs v mbBound body ->
            let env' =
                    env
                        { typeEnv =
                            Map.insert v (maybe TBottom tyToElab mbBound) (typeEnv env)
                        }
            in fmap (ETyAbs v mbBound) (go env' body)
        _ -> Nothing

    descend env v sch rhs body =
        let env' = env { termEnv = Map.insert v (schemeToType sch) (termEnv env) }
        in fmap (ELet v sch rhs) (go env' body)

    preserveRetainedChildAliasBoundary env v sch rhs body
        | isAliasFrameRhs rhs
        , hasRetainedChildAliasBoundary v body 1 =
            case typeCheckWithEnv env (ELet v sch rhs body) of
                Left (TCLetTypeMismatch _ _) ->
                    case typeCheckWithEnv env rhs of
                        Right rhsTy
                            | hasRecursiveComponent rhsTy ->
                                Just rhs
                        _ -> Nothing
                _ -> Nothing
        | otherwise = Nothing

    hasRetainedChildAliasBoundary :: String -> ElabTerm -> Int -> Bool
    hasRetainedChildAliasBoundary source term remainingAliasFrames = case term of
        ELet child childSch childRhs childBody
            | isClearBoundaryRetainedChildRhs source childRhs
                && isForallIdentityScheme childSch
                && isTrivialRetainedChildBody child childBody ->
                    True
            | usesTermVar source childRhs
                && remainingAliasFrames > 0
                && isAliasFrameRhs childRhs
                && hasRetainedChildAliasBoundary child childBody (remainingAliasFrames - 1) ->
                    True
        _ -> False

isTrivialRetainedChildBody :: String -> ElabTerm -> Bool
isTrivialRetainedChildBody v body = case body of
    EVar bodyV -> bodyV == v
    _ -> False

isForallIdentityScheme :: ElabScheme -> Bool
isForallIdentityScheme sch = case schemeToType sch of
    TForall v Nothing body -> body == TVar v
    _ -> False

hasRecursiveComponent :: ElabType -> Bool
hasRecursiveComponent ty = case ty of
    TMu _ _ -> True
    TArrow dom cod -> hasRecursiveComponent dom || hasRecursiveComponent cod
    TCon _ args -> any hasRecursiveComponent args
    TForall _ mb body -> maybe False hasRecursiveBound mb || hasRecursiveComponent body
    _ -> False
  where
    hasRecursiveBound bound = case bound of
        TArrow dom cod -> hasRecursiveComponent dom || hasRecursiveComponent cod
        TBase _ -> False
        TCon _ args -> any hasRecursiveComponent args
        TForall _ mb body -> maybe False hasRecursiveBound mb || hasRecursiveComponent body
        TMu _ _ -> True
        TBottom -> False

wrapTermWithScheme :: ElabScheme -> ElabTerm -> ElabTerm
wrapTermWithScheme (Forall binds _) term =
    foldr (\(name, bound) acc -> ETyAbs name bound acc) term binds

freshenSchemeAndSubstAgainstTerm
    :: ElabTerm
    -> IntMap.IntMap String
    -> ElabScheme
    -> (IntMap.IntMap String, ElabScheme, [(String, String)])
freshenSchemeAndSubstAgainstTerm term subst sch@(Forall binds body0) =
    let reservedNames = typeAbsNamesInTerm term
        binderNames = map fst binds
        binderDomain = Set.fromList binderNames
        renames = chooseBinderRenames binderDomain reservedNames binderNames
        renameMap =
            Map.fromList
                [ (old, new)
                | (old, new) <- renames
                , old /= new
                ]
    in if Map.null renameMap
        then (subst, sch, [])
        else
            let subst' = IntMap.map (\name -> Map.findWithDefault name name renameMap) subst
                fullRenames = [(old, new) | (old, new) <- renames, old /= new]
                binds' = renameSchemeBinds renames binds
                body' = applyTypeRenames fullRenames body0
            in (subst', Forall binds' body', fullRenames)

chooseBinderRenames
    :: Set.Set String
    -> Set.Set String
    -> [String]
    -> [(String, String)]
chooseBinderRenames binderDomain = go
  where
    go _ [] = []
    go used (binder : rest) =
        let needsRename = Set.member binder used
            usedForFresh = Set.union used binderDomain
            binder' =
                if needsRename
                    then freshNameLike binder usedForFresh
                    else binder
            used' = Set.insert binder' used
        in (binder, binder') : go used' rest

renameSchemeBinds
    :: [(String, String)]
    -> [(String, Maybe BoundType)]
    -> [(String, Maybe BoundType)]
renameSchemeBinds renames binds = go [] renames binds
  where
    go _ [] [] = []
    go prev ((old, new) : restRenames) ((_, mbBound) : restBinds) =
        let mbBound' = fmap (renameBound prev) mbBound
            prev'
                | old == new = prev
                | otherwise = prev ++ [(old, new)]
        in (new, mbBound') : go prev' restRenames restBinds
    go _ _ _ = binds

renameBound :: [(String, String)] -> BoundType -> BoundType
renameBound renames bound =
    case elabToBound (applyTypeRenames renames (tyToElab bound)) of
        Right bound' -> bound'
        Left _ -> bound

applyTypeRenames :: [(String, String)] -> ElabType -> ElabType
applyTypeRenames renames ty0 =
    foldl'
        (\ty (old, new) ->
            substTypeSimple old (TVar new) ty
        )
        ty0
        renames

renameTermTypeVars :: [(String, String)] -> ElabTerm -> ElabTerm
renameTermTypeVars renames0 = go renames0
  where
    go renames term = case term of
        EVar v -> EVar v
        ELit lit -> ELit lit
        ELam v ty body ->
            ELam v (applyTypeRenames renames ty) (go renames body)
        EApp f a ->
            EApp (go renames f) (go renames a)
        ELet v sch rhs body ->
            let sch' = schemeFromType (applyTypeRenames renames (schemeToType sch))
            in ELet v sch' (go renames rhs) (go renames body)
        ETyAbs v mbBound body ->
            let mbBound' = fmap (renameBound renames) mbBound
                renamesBody = filter (\(old, _) -> old /= v) renames
            in ETyAbs v mbBound' (go renamesBody body)
        ETyInst e inst ->
            ETyInst (go renames e) (renameInst renames inst)
        ERoll ty body ->
            ERoll (applyTypeRenames renames ty) (go renames body)
        EUnroll body ->
            EUnroll (go renames body)

alignTermTypeVarsToScheme :: ElabScheme -> ElabTerm -> Maybe ElabTerm
alignTermTypeVarsToScheme sch term =
    let binderNames = map fst (case sch of Forall binds _ -> binds)
    in case typeCheck term of
        Right ty ->
            let freeVars = Set.toList (freeTypeVarsType ty)
                renames = zip freeVars binderNames
                termAligned = renameTermTypeVars renames term
            in case typeCheck termAligned of
                Right tyAligned
                    | alphaEqType tyAligned (schemeToType sch) -> Just termAligned
                _ -> Nothing
        Left _ -> Nothing

topTyAbsNames :: ElabTerm -> [String]
topTyAbsNames term = case term of
    ETyAbs v _ body -> v : topTyAbsNames body
    _ -> []

alignTermTypeVarsToTopTyAbs :: ElabTerm -> Maybe ElabTerm
alignTermTypeVarsToTopTyAbs term =
    let binders = topTyAbsNames term
    in case (binders, typeCheck term) of
        ([], _) -> Nothing
        (_, Left _) -> Nothing
        (_, Right ty) ->
            let freeVars = Set.toList (freeTypeVarsType ty)
                renames = zip freeVars binders
                termAligned = renameTermTypeVars renames term
            in case typeCheck termAligned of
                Right tyAligned
                    | Set.null (freeTypeVarsType tyAligned) || length freeVars <= length binders -> Just termAligned
                _ -> Nothing

alignTermTypeVarsToSchemeBody :: ElabScheme -> ElabTerm -> Maybe ElabTerm
alignTermTypeVarsToSchemeBody sch term =
    let Forall binds body = sch
        binderNames = map fst binds
    in case typeCheck term of
        Right ty ->
            let freeVars = Set.toList (freeTypeVarsType ty)
                renames = zip freeVars binderNames
                termAligned = renameTermTypeVars renames term
            in case typeCheck termAligned of
                Right tyAligned
                    | alphaEqType tyAligned body -> Just termAligned
                _ -> Nothing
        Left _ -> Nothing


renameInst :: [(String, String)] -> Instantiation -> Instantiation
renameInst renames inst = case inst of
    InstId -> InstId
    InstApp ty -> InstApp (applyTypeRenames renames ty)
    InstBot ty -> InstBot (applyTypeRenames renames ty)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstr v -> InstAbstr (renameName renames v)
    InstUnder v inner -> InstUnder (renameName renames v) (renameInst renames inner)
    InstInside inner -> InstInside (renameInst renames inner)
    InstSeq i1 i2 -> InstSeq (renameInst renames i1) (renameInst renames i2)

renameName :: [(String, String)] -> String -> String
renameName renames name =
    case lookup name renames of
        Just renamed -> renamed
        Nothing -> name

isAliasFrameRhs :: ElabTerm -> Bool
isAliasFrameRhs rhs = case rhs of
    EVar _ -> True
    ETyAbs _ _ body -> isAliasFrameRhs body
    _ -> False

isClearBoundaryRetainedChildRhs :: String -> ElabTerm -> Bool
isClearBoundaryRetainedChildRhs source rhs = case rhs of
    EApp f arg -> isIdentityBoundaryLambda f && usesTermVar source arg
    ETyAbs _ _ body -> isClearBoundaryRetainedChildRhs source body
    ETyInst e _ -> isClearBoundaryRetainedChildRhs source e
    _ -> False

isIdentityBoundaryLambda :: ElabTerm -> Bool
isIdentityBoundaryLambda term = case term of
    ELam v _ body -> isIdentityBoundaryBody v body
    ETyAbs _ _ body -> isIdentityBoundaryLambda body
    ETyInst e _ -> isIdentityBoundaryLambda e
    _ -> False

isIdentityBoundaryBody :: String -> ElabTerm -> Bool
isIdentityBoundaryBody v body = case body of
    EVar bodyV -> bodyV == v
    ETyAbs _ _ inner -> isIdentityBoundaryBody v inner
    ETyInst e _ -> isIdentityBoundaryBody v e
    _ -> False

usesTermVar :: String -> ElabTerm -> Bool
usesTermVar target = go
  where
    go term = case term of
        EVar v -> v == target
        ELit _ -> False
        ELam v _ body
            | v == target -> False
            | otherwise -> go body
        EApp f a -> go f || go a
        ELet v _ rhs body ->
            go rhs || ((v /= target) && go body)
        ETyAbs _ _ body -> go body
        ETyInst e _ -> go e
        ERoll _ body -> go body
        EUnroll body -> go body

typeAbsNamesInTerm :: ElabTerm -> Set.Set String
typeAbsNamesInTerm = cata alg
  where
    alg term = case term of
        EVarF _ -> Set.empty
        ELitF _ -> Set.empty
        ELamF _ _ body -> body
        EAppF f a -> Set.union f a
        ELetF _ _ rhs body -> Set.union rhs body
        ETyAbsF v _ body -> Set.insert v body
        ETyInstF e _ -> e
        ERollF _ body -> body
        EUnrollF body -> body

substInTerm :: IntMap.IntMap String -> ElabTerm -> ElabTerm
substInTerm subst = cata alg
  where
    alg term = case term of
        EVarF v -> EVar v
        ELitF l -> ELit l
        ELamF v ty body -> ELam v (substInTy subst ty) body
        EAppF f a -> EApp f a
        ELetF v sch rhs body -> ELet v (substInScheme subst sch) rhs body
        ETyAbsF v b body -> ETyAbs v (fmap (substInTy subst) b) body
        ETyInstF e i -> ETyInst e (substInInst subst i)
        ERollF ty body -> ERoll (substInTy subst ty) body
        EUnrollF body -> EUnroll body

substInTy :: IntMap.IntMap String -> Ty v -> Ty v
substInTy subst = cataIx alg
  where
    alg :: TyIF i Ty -> Ty i
    alg node = case node of
        TVarIF v -> TVar (applySubst v)
        TArrowIF d c -> TArrow d c
        TConIF c args -> TCon c args
        TBaseIF b -> TBase b
        TForallIF v mb body -> TForall v mb body
        TMuIF v body -> TMu v body
        TBottomIF -> TBottom

    applySubst name =
        case parseNameId name of
            Just nid ->
                case IntMap.lookup nid subst of
                    Just newName -> newName
                    Nothing -> name
            Nothing -> name

substInScheme :: IntMap.IntMap String -> ElabScheme -> ElabScheme
substInScheme subst scheme =
    schemeFromType (substInTy subst (schemeToType scheme))

substInInst :: IntMap.IntMap String -> Instantiation -> Instantiation
substInInst subst = cata alg
  where
    alg inst = case inst of
        InstIdF -> InstId
        InstAppF t -> InstApp (substInTy subst t)
        InstBotF t -> InstBot (substInTy subst t)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v -> InstAbstr v
        InstUnderF v i' -> InstUnder v i'
        InstInsideF i' -> InstInside i'
        InstSeqF i1 i2 -> InstSeq i1 i2
