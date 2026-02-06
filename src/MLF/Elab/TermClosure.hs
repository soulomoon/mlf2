{-# LANGUAGE GADTs #-}
module MLF.Elab.TermClosure (
    closeTermWithSchemeSubst,
    closeTermWithSchemeSubstIfNeeded,
    substInTerm
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Elab.Inst (schemeToType)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, freshNameLike, substTypeSimple)
import MLF.Util.Names (parseNameId)

closeTermWithSchemeSubst :: IntMap.IntMap String -> ElabScheme -> ElabTerm -> ElabTerm
closeTermWithSchemeSubst subst sch term =
    let (subst', sch', renames) = freshenSchemeAndSubstAgainstTerm term subst sch
        termSubst = renameTermTypeVars renames (substInTerm subst' term)
    in wrapTermWithScheme sch' termSubst

closeTermWithSchemeSubstIfNeeded :: IntMap.IntMap String -> ElabScheme -> ElabTerm -> ElabTerm
closeTermWithSchemeSubstIfNeeded subst sch term =
    let (subst', sch', renames) = freshenSchemeAndSubstAgainstTerm term subst sch
        termSubst = renameTermTypeVars renames (substInTerm subst' term)
        schemeTy = schemeToType sch'
    in case typeCheck termSubst of
        Right ty | alphaEqType ty schemeTy -> termSubst
        _ -> wrapTermWithScheme sch' termSubst

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
