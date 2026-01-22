module MLF.Elab.Reduce (
    step,
    normalize,
    isValue
) where

import qualified Data.Set as Set
import Data.Functor.Foldable (para)

import MLF.Elab.Inst (applyInstantiation, renameInstBound, schemeToType, splitForalls)
import MLF.Reify.TypeOps (freeTypeVarsType, freshTypeName, substTypeCapture)
import MLF.Elab.Types
import MLF.Util.RecursionSchemes (cataMaybe, foldElabTerm, foldInstantiation)

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
reduceInst v inst = do
    (_inst, applyTo) <- cataMaybe alg inst
    applyTo v
  where
    alg node = case node of
        InstIdF -> Just (InstId, Just)
        InstSeqF (i1, _) (i2, _) ->
            Just (InstSeq i1 i2, \term -> Just (ETyInst (ETyInst term i1) i2))
        InstAppF ty ->
            Just (InstApp ty, \term -> Just (ETyInst term (InstSeq (InstInside (InstBot ty)) InstElim)))
        InstIntroF ->
            Just (InstIntro, \term ->
                let fresh = freshTypeName (freeTypeVarsTerm term)
                in Just (ETyAbs fresh Nothing term))
        InstElimF ->
            Just (InstElim, \term -> case term of
                ETyAbs name mbBound body ->
                    let bound = boundType mbBound
                        body' = replaceAbstrInTerm name InstId body
                    in Just (substTypeVarTerm name bound body')
                _ -> Nothing)
        InstUnderF vParam (phi, _) ->
            Just (InstUnder vParam phi, \term -> case term of
                ETyAbs name mbBound body ->
                    let phi' = renameInstBound vParam name phi
                    in Just (ETyAbs name mbBound (ETyInst body phi'))
                _ -> Nothing)
        InstInsideF (phi, _) ->
            Just (InstInside phi, \term -> case term of
                ETyAbs name mbBound body -> do
                    let bound0 = boundType mbBound
                    bound1 <- either (const Nothing) Just (applyInstantiation bound0 phi)
                    let mb' = if bound1 == TBottom then Nothing else Just bound1
                        body' = replaceAbstrInTerm name (InstSeq phi (InstAbstr name)) body
                    Just (ETyAbs name mb' body')
                _ -> Nothing)
        InstBotF ty -> Just (InstBot ty, const Nothing)
        InstAbstrF vParam -> Just (InstAbstr vParam, const Nothing)

boundType :: Maybe ElabType -> ElabType
boundType = maybe TBottom id

freeTermVars :: ElabTerm -> Set.Set String
freeTermVars = foldElabTerm alg
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

freeTypeVarsScheme :: ElabScheme -> Set.Set String
freeTypeVarsScheme sch = freeTypeVarsType (schemeToType sch)

freeTypeVarsInst :: Instantiation -> Set.Set String
freeTypeVarsInst = foldInstantiation alg
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
freeTypeVarsTerm = foldElabTerm alg
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

substTermVar :: String -> ElabTerm -> ElabTerm -> ElabTerm
substTermVar x s = goSub
  where
    freeS = freeTermVars s
    goSub = para alg
      where
        alg term = case term of
            EVarF v
                | v == x -> s
                | otherwise -> EVar v
            ELitF l -> ELit l
            ELamF v ty body
                | v == x -> ELam v ty (fst body)
                | v `Set.member` freeS ->
                    let used = Set.unions [freeS, freeTermVars (fst body), Set.singleton x]
                        v' = freshTermNameFrom v used
                        body' = substTermVar v (EVar v') (fst body)
                    in ELam v' ty (goSub body')
                | otherwise -> ELam v ty (snd body)
            EAppF f a -> EApp (snd f) (snd a)
            ELetF v sch rhs body
                | v == x -> ELet v sch (snd rhs) (fst body)
                | v `Set.member` freeS ->
                    let used = Set.unions [freeS, freeTermVars (fst body), Set.singleton x]
                        v' = freshTermNameFrom v used
                        body' = substTermVar v (EVar v') (fst body)
                    in ELet v' sch (snd rhs) (goSub body')
                | otherwise -> ELet v sch (snd rhs) (snd body)
            ETyAbsF v b body -> ETyAbs v b (snd body)
            ETyInstF e i -> ETyInst (snd e) i

substTypeVarTerm :: String -> ElabType -> ElabTerm -> ElabTerm
substTypeVarTerm x s = goSub
  where
    freeS = freeTypeVarsType s
    goSub = para alg
      where
        alg term = case term of
            EVarF v -> EVar v
            ELitF l -> ELit l
            ELamF v ty body -> ELam v (substTypeVar x s ty) (snd body)
            EAppF f a -> EApp (snd f) (snd a)
            ELetF v sch rhs body ->
                ELet v (substTypeVarScheme x s sch) (snd rhs) (snd body)
            ETyAbsF v mb body
                | v == x -> ETyAbs v (fmap (substTypeVar x s) mb) (fst body)
                | v `Set.member` freeS ->
                    let used = Set.unions [freeS, freeTypeVarsTerm (fst body), Set.singleton x]
                        v' = freshTermNameFrom v used
                        body' = substTypeVarTerm v (TVar v') (fst body)
                    in ETyAbs v' (fmap (substTypeVar x s) mb) (goSub body')
                | otherwise -> ETyAbs v (fmap (substTypeVar x s) mb) (snd body)
            ETyInstF e i -> ETyInst (snd e) (substTypeVarInst x s i)

substTypeVarScheme :: String -> ElabType -> ElabScheme -> ElabScheme
substTypeVarScheme x s sch =
    let ty = schemeToType sch
        ty' = substTypeVar x s ty
        (binds, body) = splitForalls ty'
    in Forall binds body

substTypeVarInst :: String -> ElabType -> Instantiation -> Instantiation
substTypeVarInst x s = para alg
  where
    alg inst = case inst of
        InstIdF -> InstId
        InstAppF t -> InstApp (substTypeVar x s t)
        InstBotF t -> InstBot (substTypeVar x s t)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v -> InstAbstr v
        InstInsideF i -> InstInside (snd i)
        InstSeqF a b -> InstSeq (snd a) (snd b)
        InstUnderF v i
            | v == x -> InstUnder v (fst i)
            | otherwise -> InstUnder v (snd i)

substTypeVar :: String -> ElabType -> ElabType -> ElabType
substTypeVar = substTypeCapture

replaceAbstrInTerm :: String -> Instantiation -> ElabTerm -> ElabTerm
replaceAbstrInTerm target replacement = para alg
  where
    alg term = case term of
        EVarF v -> EVar v
        ELitF l -> ELit l
        ELamF v ty body -> ELam v ty (snd body)
        EAppF f a -> EApp (snd f) (snd a)
        ELetF v sch rhs body -> ELet v sch (snd rhs) (snd body)
        ETyAbsF v mb body
            | v == target -> ETyAbs v mb (fst body)
            | otherwise -> ETyAbs v mb (snd body)
        ETyInstF e inst -> ETyInst (snd e) (replaceAbstrInInst target replacement inst)

replaceAbstrInInst :: String -> Instantiation -> Instantiation -> Instantiation
replaceAbstrInInst target replacement = para alg
  where
    alg inst = case inst of
        InstIdF -> InstId
        InstAppF t -> InstApp t
        InstBotF t -> InstBot t
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrF v
            | v == target -> replacement
            | otherwise -> InstAbstr v
        InstInsideF i -> InstInside (snd i)
        InstSeqF a b -> InstSeq (snd a) (snd b)
        InstUnderF v i
            | v == target -> InstUnder v (fst i)
            | otherwise -> InstUnder v (snd i)
