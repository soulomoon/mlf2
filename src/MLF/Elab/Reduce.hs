module MLF.Elab.Reduce (
    step,
    normalize,
    isValue
) where

import qualified Data.Set as Set

import MLF.Elab.Inst (applyInstantiation, schemeToType, splitForalls)
import MLF.Elab.Types
import Data.Functor.Foldable (cata)

isValue :: ElabTerm -> Bool
isValue term = case term of
    ELit{} -> True
    ELam{} -> True
    ETyAbs{} -> True
    _ -> False

step :: ElabTerm -> Maybe ElabTerm
step term = case term of
    EApp f a
        | not (isValue f) -> (`EApp` a) <$> step f
        | not (isValue a) -> EApp f <$> step a
        | otherwise ->
            case f of
                ELam v _ body -> Just (substTermVar v a body)
                _ -> Nothing
    ELet v sch rhs body
        | not (isValue rhs) -> (\rhs' -> ELet v sch rhs' body) <$> step rhs
        | otherwise -> Just (substTermVar v rhs body)
    ETyInst e inst
        | not (isValue e) -> (`ETyInst` inst) <$> step e
        | otherwise -> reduceInst e inst
    _ -> Nothing

normalize :: ElabTerm -> ElabTerm
normalize term = case step term of
    Nothing -> term
    Just term' -> normalize term'

reduceInst :: ElabTerm -> Instantiation -> Maybe ElabTerm
reduceInst v inst = case inst of
    InstId -> Just v
    InstSeq i1 i2 -> Just (ETyInst (ETyInst v i1) i2)
    InstApp ty ->
        Just (ETyInst v (InstSeq (InstInside (InstBot ty)) InstElim))
    InstIntro ->
        let fresh = freshTypeName (freeTypeVarsTerm v)
        in Just (ETyAbs fresh Nothing v)
    InstElim -> case v of
        ETyAbs name mbBound body ->
            let bound = boundType mbBound
                body' = replaceAbstrInTerm name InstId body
            in Just (substTypeVarTerm name bound body')
        _ -> Nothing
    InstUnder vParam phi -> case v of
        ETyAbs name mbBound body ->
            let phi' = renameInstBound vParam name phi
            in Just (ETyAbs name mbBound (ETyInst body phi'))
        _ -> Nothing
    InstInside phi -> case v of
        ETyAbs name mbBound body -> do
            let bound0 = boundType mbBound
            bound1 <- either (const Nothing) Just (applyInstantiation bound0 phi)
            let mb' = if bound1 == TBottom then Nothing else Just bound1
                body' = replaceAbstrInTerm name (InstSeq phi (InstAbstr name)) body
            Just (ETyAbs name mb' body')
        _ -> Nothing
    _ -> Nothing

boundType :: Maybe ElabType -> ElabType
boundType = maybe TBottom id

freeTermVars :: ElabTerm -> Set.Set String
freeTermVars = cata alg
  where
    alg term = case term of
        EVarF v -> Set.singleton v
        ELitF _ -> Set.empty
        ELamF v _ body -> Set.delete v body
        EAppF f a -> Set.union f a
        ELetF v _ rhs body ->
            Set.union rhs (Set.delete v body)
        ETyAbsF _ _ body -> body
        ETyInstF e _ -> e

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

freeTypeVarsScheme :: ElabScheme -> Set.Set String
freeTypeVarsScheme sch = freeTypeVarsType (schemeToType sch)

freeTypeVarsInst :: Instantiation -> Set.Set String
freeTypeVarsInst = cata alg
  where
    alg inst = case inst of
        InstIdF -> Set.empty
        InstAppF t -> freeTypeVarsType t
        InstBotF t -> freeTypeVarsType t
        InstIntroF -> Set.empty
        InstElimF -> Set.empty
        InstAbstrF v -> Set.singleton v
        InstInsideF i -> i
        InstSeqF a b -> Set.union a b
        InstUnderF v i -> Set.delete v i

freeTypeVarsTerm :: ElabTerm -> Set.Set String
freeTypeVarsTerm = cata alg
  where
    alg term = case term of
        EVarF _ -> Set.empty
        ELitF _ -> Set.empty
        ELamF _ ty body -> Set.union (freeTypeVarsType ty) body
        EAppF f a -> Set.union f a
        ELetF _ sch rhs body ->
            Set.unions [freeTypeVarsScheme sch, rhs, body]
        ETyAbsF v mb body ->
            let boundFv = maybe Set.empty freeTypeVarsType mb
                bodyFv = Set.delete v body
            in Set.union boundFv bodyFv
        ETyInstF e inst ->
            Set.union e (freeTypeVarsInst inst)

freshTermNameFrom :: String -> Set.Set String -> String
freshTermNameFrom base used =
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

substTermVar :: String -> ElabTerm -> ElabTerm -> ElabTerm
substTermVar x s term = go term
  where
    freeS = freeTermVars s
    go t = case t of
        EVar v | v == x -> s
        EVar v -> EVar v
        ELit l -> ELit l
        ELam v ty body
            | v == x -> ELam v ty body
            | v `Set.member` freeS ->
                let used = Set.unions [freeS, freeTermVars body, Set.singleton x]
                    v' = freshTermNameFrom v used
                    body' = substTermVar v (EVar v') body
                in ELam v' ty (go body')
            | otherwise -> ELam v ty (go body)
        EApp f a -> EApp (go f) (go a)
        ELet v sch rhs body
            | v == x -> ELet v sch (go rhs) body
            | v `Set.member` freeS ->
                let used = Set.unions [freeS, freeTermVars body, Set.singleton x]
                    v' = freshTermNameFrom v used
                    body' = substTermVar v (EVar v') body
                in ELet v' sch (go rhs) (go body')
            | otherwise -> ELet v sch (go rhs) (go body)
        ETyAbs v b body -> ETyAbs v b (go body)
        ETyInst e i -> ETyInst (go e) i

substTypeVarTerm :: String -> ElabType -> ElabTerm -> ElabTerm
substTypeVarTerm x s term = go term
  where
    freeS = freeTypeVarsType s
    go t = case t of
        EVar v -> EVar v
        ELit l -> ELit l
        ELam v ty body -> ELam v (substTypeVar x s ty) (go body)
        EApp f a -> EApp (go f) (go a)
        ELet v sch rhs body ->
            ELet v (substTypeVarScheme x s sch) (go rhs) (go body)
        ETyAbs v mb body
            | v == x -> ETyAbs v (fmap (substTypeVar x s) mb) body
            | v `Set.member` freeS ->
                let used = Set.unions [freeS, freeTypeVarsTerm body, Set.singleton x]
                    v' = freshTermNameFrom v used
                    body' = substTypeVarTerm v (TVar v') body
                in ETyAbs v' (fmap (substTypeVar x s) mb) (go body')
            | otherwise -> ETyAbs v (fmap (substTypeVar x s) mb) (go body)
        ETyInst e i -> ETyInst (go e) (substTypeVarInst x s i)

substTypeVarScheme :: String -> ElabType -> ElabScheme -> ElabScheme
substTypeVarScheme x s sch =
    let ty = schemeToType sch
        ty' = substTypeVar x s ty
        (binds, body) = splitForalls ty'
    in Forall binds body

substTypeVarInst :: String -> ElabType -> Instantiation -> Instantiation
substTypeVarInst x s inst = case inst of
    InstId -> InstId
    InstApp t -> InstApp (substTypeVar x s t)
    InstBot t -> InstBot (substTypeVar x s t)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstr v -> InstAbstr v
    InstInside i -> InstInside (substTypeVarInst x s i)
    InstSeq a b -> InstSeq (substTypeVarInst x s a) (substTypeVarInst x s b)
    InstUnder v i
        | v == x -> InstUnder v i
        | otherwise -> InstUnder v (substTypeVarInst x s i)

substTypeVar :: String -> ElabType -> ElabType -> ElabType
substTypeVar x s ty = case ty of
    TVar v | v == x -> s
    TVar v -> TVar v
    TArrow a b -> TArrow (substTypeVar x s a) (substTypeVar x s b)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall v mb body
        | v == x -> TForall v (fmap (substTypeVar x s) mb) body
        | v `Set.member` freeS ->
            let v' = freshTermNameFrom v (Set.unions [freeS, freeTypeVarsType body, maybe Set.empty freeTypeVarsType mb])
                body' = substTypeVar v (TVar v') body
            in TForall v' (fmap (substTypeVar x s) mb) (substTypeVar x s body')
        | otherwise -> TForall v (fmap (substTypeVar x s) mb) (substTypeVar x s body)
  where
    freeS = freeTypeVarsType s

replaceAbstrInTerm :: String -> Instantiation -> ElabTerm -> ElabTerm
replaceAbstrInTerm target replacement term = go term
  where
    go t = case t of
        EVar v -> EVar v
        ELit l -> ELit l
        ELam v ty body -> ELam v ty (go body)
        EApp f a -> EApp (go f) (go a)
        ELet v sch rhs body -> ELet v sch (go rhs) (go body)
        ETyAbs v mb body
            | v == target -> ETyAbs v mb body
            | otherwise -> ETyAbs v mb (go body)
        ETyInst e inst -> ETyInst (go e) (replaceAbstrInInst target replacement inst)

replaceAbstrInInst :: String -> Instantiation -> Instantiation -> Instantiation
replaceAbstrInInst target replacement inst = case inst of
    InstId -> InstId
    InstApp t -> InstApp t
    InstBot t -> InstBot t
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstr v
        | v == target -> replacement
        | otherwise -> InstAbstr v
    InstInside i -> InstInside (replaceAbstrInInst target replacement i)
    InstSeq a b -> InstSeq (replaceAbstrInInst target replacement a) (replaceAbstrInInst target replacement b)
    InstUnder v i
        | v == target -> InstUnder v i
        | otherwise -> InstUnder v (replaceAbstrInInst target replacement i)

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
