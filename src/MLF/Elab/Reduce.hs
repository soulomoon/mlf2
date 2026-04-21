{-# LANGUAGE GADTs #-}

module MLF.Elab.Reduce
  ( step,
    normalize,
    isValue,
  )
where

import Data.Functor.Foldable (para)
import qualified Data.Set as Set
import MLF.Elab.Inst (applyInstantiation, renameInstBound, schemeToType)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Frontend.Syntax (Lit (..))
import MLF.Reify.TypeOps (freeTypeVarsType, freshTypeName, substTypeCapture)
import MLF.Util.RecursionSchemes (cataMaybe, foldElabTerm, foldInstantiation)

isValue :: ElabTerm -> Bool
isValue term = case term of
  ELit {} -> True
  ELam {} -> True
  ETyAbs {} -> True
  ERoll _ body -> isValue body
  _ -> False

step :: ElabTerm -> Maybe ElabTerm
step term = case term of
  EApp (EApp (EVar "__mlfp_and") (ELit (LBool left))) (ELit (LBool right)) ->
    Just (ELit (LBool (left && right)))
  EApp (EApp (EVar "__mlfp_and") left) right
    | not (isValue left) ->
        (\left' -> EApp (EApp (EVar "__mlfp_and") left') right) <$> step left
    | not (isValue right) ->
        EApp (EApp (EVar "__mlfp_and") left) <$> step right
  EApp (ETyAbs v _ body) a
    | v `Set.notMember` freeTypeVarsTerm body ->
        Just (EApp body a)
  EApp f a
    | not (isValue f) -> (`EApp` a) <$> step f
    | not (isValue a || isNeutralValue a) -> EApp f <$> step a
    | otherwise ->
        case f of
          ELam v _ body -> Just (substTermVar v a body)
          ETyInst f' inst
            | instConsumesForall inst,
              not (termHasLeadingTyAbs f') ->
                Just (EApp f' a)
          _ -> Nothing
  ELet v sch rhs body
    | not (isValue rhs) -> (\rhs' -> ELet v sch rhs' body) <$> step rhs
    | v `Set.member` freeTermVars rhs ->
        -- Recursive let: unfold one step.
        -- See Note [Recursive let reduction]
        let selfRef = ELet v sch rhs (EVar v)
            rhs' = substTermVar v selfRef rhs
         in Just (substTermVar v rhs' body)
    | otherwise -> Just (substTermVar v rhs body)
  ETyInst e inst
    | not (isValue e) ->
        case step e of
          Just e' -> Just (ETyInst e' inst)
          Nothing
            | instConsumesForall inst,
              not (termHasLeadingTyAbs e) ->
                Just e
          Nothing -> Nothing
    | otherwise ->
        case reduceInst e inst of
          Just term' -> Just term'
          Nothing
            | instConsumesForall inst,
              not (valueHasLeadingTyAbs e) ->
                Just e
          Nothing -> Nothing
  ERoll ty body
    | not (isValue body) -> ERoll ty <$> step body
    | otherwise -> Nothing
  EUnroll e
    | ELam {} <- e -> Just e
    | ETyAbs {} <- e -> Just e
    | ETyInst e' inst <- e,
      instConsumesForall inst,
      termHasLeadingTyAbs e' ->
        Just (ETyInst (EUnroll e') inst)
    | ETyInst e' inst <- e,
      instConsumesForall inst,
      not (termHasLeadingTyAbs e') ->
        Just (EUnroll e')
    | not (isValue e) ->
        case step e of
          Just e' -> Just (EUnroll e')
          Nothing ->
            case typeCheck e of
              Right TMu {} -> Nothing
              Right _ -> Just e
              Left _ -> Nothing
    | otherwise ->
        case e of
          ERoll _ body | isValue body -> Just body
          _ ->
            case typeCheck e of
              Right TMu {} -> Nothing
              Right _ -> Just e
              Left _ -> Nothing
  _ -> Nothing

isNeutralValue :: ElabTerm -> Bool
isNeutralValue term = case term of
  EVar {} -> True
  _ -> False

normalize :: ElabTerm -> ElabTerm
normalize term = case step term of
  Nothing -> term
  Just term' -> normalize term'

{- Note [Recursive let reduction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For recursive bindings (where the bound variable v appears free in rhs),
we cannot simply substitute rhs for v in body because rhs itself contains
v — that occurrence would become free.

Instead we perform a standard one-step letrec unfolding:

  let v = V in body  →  body[v := V[v := let v = V in v]]

where V is the evaluated rhs (a value). This replaces each occurrence of v
in rhs with a "re-entry point" (the original letrec applied to just v),
producing rhs'. Then rhs' is substituted for v in body.

For example:
  let f = \x. f x in f
  → (\x. (let f = \x. f x in f) x)

The result is a lambda (value), so normalize stops. When the lambda is later
applied, the inner letrec unfolds again — giving lazy recursive unfolding
without infinite expansion.

Non-recursive lets (v not free in rhs) use the original direct substitution
path and are completely unaffected. -}

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
        Just
          ( InstIntro,
            \term ->
              let fresh = freshTypeName (freeTypeVarsTerm term)
               in Just (ETyAbs fresh Nothing term)
          )
      InstElimF ->
        Just
          ( InstElim,
            \term -> case term of
              ETyAbs name mbBound body ->
                let bound = boundType mbBound
                    body' = replaceAbstrInTerm name InstId body
                 in Just (substTypeVarTerm name bound body')
              _ -> Nothing
          )
      InstUnderF vParam (phi, _) ->
        Just
          ( InstUnder vParam phi,
            \term -> case term of
              ETyAbs name mbBound body ->
                let phi' = renameInstBound vParam name phi
                 in Just (ETyAbs name mbBound (ETyInst body phi'))
              _ -> Nothing
          )
      InstInsideF (phi, _) ->
        Just
          ( InstInside phi,
            \term -> case term of
              ETyAbs name mbBound body -> do
                let bound0 = boundType mbBound
                bound1 <- either (const Nothing) Just (applyInstantiation bound0 phi)
                let mb' = case bound1 of
                      TBottom -> Nothing
                      TVar {} -> Nothing
                      _ -> either (const Nothing) Just (elabToBound bound1)
                    body' = replaceAbstrInTerm name (InstSeq phi (InstAbstr name)) body
                Just (ETyAbs name mb' body')
              _ -> Nothing
          )
      InstBotF ty -> Just (InstBot ty, const Nothing)
      InstAbstrF vParam -> Just (InstAbstr vParam, const Nothing)

boundType :: Maybe BoundType -> ElabType
boundType = maybe TBottom tyToElab

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
      ERollF _ body -> body
      EUnrollF body -> body

freeTypeVarsScheme :: ElabScheme -> Set.Set String
freeTypeVarsScheme sch = freeTypeVarsType (schemeToType sch)

valueHasLeadingTyAbs :: ElabTerm -> Bool
valueHasLeadingTyAbs term = case term of
  ETyAbs {} -> True
  ERoll _ body -> valueHasLeadingTyAbs body
  _ -> False

termHasLeadingTyAbs :: ElabTerm -> Bool
termHasLeadingTyAbs term = case term of
  ETyAbs {} -> True
  ERoll _ body -> termHasLeadingTyAbs body
  EUnroll body -> termHasLeadingTyAbs body || valueHasLeadingTyAbs body
  ELet _ _ rhs body -> termHasLeadingTyAbs rhs || termHasLeadingTyAbs body
  _ -> False

instConsumesForall :: Instantiation -> Bool
instConsumesForall inst = case inst of
  InstId -> False
  InstApp _ -> True
  InstIntro -> False
  InstElim -> True
  InstInside inner -> instConsumesForall inner || True
  InstSeq i1 i2 -> instConsumesForall i1 || instConsumesForall i2
  InstUnder _ inner -> instConsumesForall inner
  InstBot _ -> False
  InstAbstr _ -> False

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
      ERollF ty body ->
        Set.union (freeTypeVarsType ty) body
      EUnrollF body -> body

freshTermNameFrom :: String -> Set.Set String -> String
freshTermNameFrom base used =
  let candidates = base : [base ++ show i | i <- [(1 :: Int) ..]]
   in case filter (`Set.notMember` used) candidates of
        (x : _) -> x
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
          ERollF ty body -> ERoll ty (snd body)
          EUnrollF body -> EUnroll (snd body)

substTypeVarTerm :: String -> ElabType -> ElabTerm -> ElabTerm
substTypeVarTerm x s = goSub
  where
    freeS = freeTypeVarsType s
    substBoundVar mb = do
      bnd <- mb
      let result = substTypeCapture x s (tyToElab bnd)
      case result of
        TVar {} -> Nothing
        TBottom -> Nothing
        _ -> either (const Nothing) Just (elabToBound result)
    goSub = para alg
      where
        alg term = case term of
          EVarF v -> EVar v
          ELitF l -> ELit l
          ELamF v ty body -> ELam v (substTypeCapture x s ty) (snd body)
          EAppF f a -> EApp (snd f) (snd a)
          ELetF v sch rhs body ->
            ELet v (substTypeVarScheme x s sch) (snd rhs) (snd body)
          ETyAbsF v mb body
            | v == x -> ETyAbs v (substBoundVar mb) (fst body)
            | v `Set.member` freeS ->
                let used = Set.unions [freeS, freeTypeVarsTerm (fst body), Set.singleton x]
                    v' = freshTermNameFrom v used
                    body' = substTypeVarTerm v (TVar v') (fst body)
                 in ETyAbs v' (substBoundVar mb) (goSub body')
            | otherwise -> ETyAbs v (substBoundVar mb) (snd body)
          ETyInstF e i -> ETyInst (snd e) (substTypeVarInst x s i)
          ERollF ty body -> ERoll (substTypeCapture x s ty) (snd body)
          EUnrollF body -> EUnroll (snd body)

substTypeVarScheme :: String -> ElabType -> ElabScheme -> ElabScheme
substTypeVarScheme x s sch =
  let ty = schemeToType sch
      ty' = substTypeCapture x s ty
   in schemeFromType ty'

substTypeVarInst :: String -> ElabType -> Instantiation -> Instantiation
substTypeVarInst x s = para alg
  where
    alg inst = case inst of
      InstIdF -> InstId
      InstAppF t -> InstApp (substTypeCapture x s t)
      InstBotF t -> InstBot (substTypeCapture x s t)
      InstIntroF -> InstIntro
      InstElimF -> InstElim
      InstAbstrF v -> InstAbstr v
      InstInsideF i -> InstInside (snd i)
      InstSeqF a b -> InstSeq (snd a) (snd b)
      InstUnderF v i
        | v == x -> InstUnder v (fst i)
        | otherwise -> InstUnder v (snd i)

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
      ERollF ty body -> ERoll ty (snd body)
      EUnrollF body -> EUnroll (snd body)

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
