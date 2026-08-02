{-# LANGUAGE GADTs #-}

module MLF.Elab.Reduce
  ( step,
    normalize,
    reduceLeadingTypeInstantiationRedexes,
    freeResolvedTermVars,
    collectApplicationSpineThroughHeadTypeRedexes,
    freeTypeVarRefsTerm,
    isValue,
  )
where

import Data.Functor.Foldable (Recursive (project), para)
import qualified Data.Set as Set
import MLF.Elab.Inst (applyInstantiation, renameInstBoundRef, schemeToType)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Frontend.Program.Builtins (builtinValueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarRefsType, substTypeCaptureRef)
import MLF.Types.Identity (IdDetails (..), primitiveRefSymbol)
import MLF.Util.RecursionSchemes (cataMaybe, foldXmlfTerm, foldInstantiation)

isValue :: XmlfTerm -> Bool
isValue term = case term of
  ELit {} -> True
  ELam {} -> True
  ETyAbsRef {} -> True
  ERoll _ body -> isValue body
  _ -> False

step :: XmlfTerm -> Maybe XmlfTerm
step term = case term of
  EApp (EApp (EVarNode andVar) (ELit (LBool left))) (ELit (LBool right))
    | isAndPrimitive andVar ->
        Just (ELit (LBool (left && right)))
  EApp (EApp (EVarNode andVar) left) right
    | isAndPrimitive andVar
        && not (isValue left) ->
        (\left' -> EApp (EApp (EVarNode andVar) left') right) <$> step left
    | isAndPrimitive andVar
        && not (isValue right) ->
        EApp (EApp (EVarNode andVar) left) <$> step right
  EApp (ETyAbsRef ref _ body) a
    | not (typeRefMember ref (freeTypeVarRefsTerm body)) ->
        Just (EApp body a)
  EApp f a
    | not (isValue f) -> (`EApp` a) <$> step f
    | not (isValue a || isNeutralValue a) -> EApp f <$> step a
    | otherwise ->
      case f of
        ELam resolved body ->
          Just (substResolvedTermVar resolved a body)
        _ -> Nothing
  ELet resolved sch rhs body
    | not (isValue rhs) -> (\rhs' -> ELet resolved sch rhs' body) <$> step rhs
    | termMentionsFreeResolvedVar resolved rhs ->
        let selfRef = ELet resolved sch rhs (EVarNode resolved)
            rhs' = substResolvedTermVar resolved selfRef rhs
         in Just (substResolvedTermVar resolved rhs' body)
    | otherwise -> Just (substResolvedTermVar resolved rhs body)
  ETyInst e inst
    | not (isValue e) ->
      case step e of
        Just e' -> Just (ETyInst e' inst)
        Nothing -> Nothing
    | otherwise ->
      case reduceInst e inst of
        Just term' -> Just term'
        Nothing -> Nothing
  ERoll ty body
    | not (isValue body) -> ERoll ty <$> step body
    | otherwise -> Nothing
  EUnroll e
    | ELam {} <- e -> Just e
    | ETyAbsRef {} <- e -> Just e
    | ETyInst e' inst <- e,
      instConsumesForall inst,
      termHasLeadingTyAbs e' ->
        Just (ETyInst (EUnroll e') inst)
    | not (isValue e) ->
        case step e of
          Just e' -> Just (EUnroll e')
          Nothing ->
            case typeCheck e of
              Right TMuRef {} -> Nothing
              Right _ -> Just e
              Left _ -> Nothing
    | otherwise ->
        case e of
          ERoll _ body | isValue body -> Just body
          _ ->
            case typeCheck e of
              Right TMuRef {} -> Nothing
              Right _ -> Just e
              Left _ -> Nothing
  _ -> Nothing

isAndPrimitive :: ResolvedVar -> Bool
isAndPrimitive resolved =
  case resolvedVarDetails resolved of
    PrimitiveId ref ->
      primitiveRefSymbol ref == andSymbol
    TopLevelId symbol ->
      symbol == andSymbol
    _ -> False
  where
    andSymbol =
      builtinValueIdentity PrimitiveInventory.nativeAndPrimitiveName

isNeutralValue :: XmlfTerm -> Bool
isNeutralValue term = case term of
  EVarNode {} -> True
  _ -> False

normalize :: XmlfTerm -> XmlfTerm
normalize term = case step term of
  Nothing -> term
  Just term' -> normalize term'

-- | Reduce only the quantifier-elimination computation at the head of an
-- explicit type-instantiation spine.  In particular, this does not descend
-- through type abstractions or evaluate value-level lets/applications.
--
-- Returning 'Nothing' for an unchanged term lets callers preserve their
-- ordinary structural traversal when the head is not a type redex.
reduceLeadingTypeInstantiationRedexes :: XmlfTerm -> Maybe XmlfTerm
reduceLeadingTypeInstantiationRedexes term =
  case go term of
    (True, reduced) -> Just reduced
    (False, _) -> Nothing
  where
    go current =
      case current of
        ETyInst inner inst ->
          let (innerChanged, inner') = go inner
              rebuilt = ETyInst inner' inst
           in case inst of
                -- Identity is a proof-only computation and can be erased
                -- without evaluating its operand.  This matters after a
                -- surrounding abstraction/elimination beta-redex substitutes
                -- an abstract computation with InstId underneath a value
                -- lambda.
                InstId -> (True, inner')
                _
                  | isValue inner' ->
                      -- This boundary is intentionally type-only: do not route
                      -- through 'step', whose value-level beta rules may grow
                      -- independently of xMLF type-instantiation reduction.
                      case reduceInst inner' inst of
                        Just reduced ->
                          let (_, reduced') = go reduced
                           in (True, reduced')
                        Nothing -> (innerChanged, rebuilt)
                  | otherwise -> (innerChanged, rebuilt)
        _ -> (False, current)

-- | Collect one value-application spine modulo explicit type beta reduction
-- at its function head.  An elaborated partial application may be generalized
-- and immediately instantiated before the remaining value arguments are
-- applied:
--
-- @
-- (\@a. f x y) [t] z
-- @
--
-- The type redex does not end the value-application spine.  Reducing only that
-- redex while carrying the already collected outer arguments recovers the
-- semantic spine @(f, [x, y, z])@ without evaluating value-level terms.
collectApplicationSpineThroughHeadTypeRedexes :: XmlfTerm -> (XmlfTerm, [XmlfTerm])
collectApplicationSpineThroughHeadTypeRedexes = go []
  where
    go args current =
      case current of
        EApp fun arg -> go (arg : args) fun
        _ ->
          case reduceLeadingTypeInstantiationRedexes current of
            Just reduced -> go args reduced
            Nothing -> (current, args)

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

reduceInst :: XmlfTerm -> Instantiation -> Maybe XmlfTerm
reduceInst v inst = do
  (_inst, applyTo) <- cataMaybe alg inst
  applyTo v
  where
    alg node = case node of
      InstIdF -> Just (InstId, Just)
      InstSeqF (i1, _) (i2, _) ->
        case (i1, i2) of
          -- InstApp is definitionally @Bot ty ; N@.  Reduce that pair as one
          -- positional type application instead of materializing the
          -- intermediate forall bound.  In particular, a type-variable
          -- argument is an opened Gamma identity; 'InstInside' deliberately
          -- does not encode such a variable as a closed 'BoundType', so
          -- splitting the pair would turn the following N into @Bottom@.
          (InstInside (InstBot ty), InstElim) ->
            Just
              ( InstApp ty,
                \term ->
                  case project term of
                    ETyAbsFRef ref mbBound body ->
                      let directApplication binderComputation =
                            let body' =
                                  replaceAbstrInTermRef
                                    ref
                                    binderComputation
                                    body
                             in Just
                                  (substTypeVarTermRef ref ty body')
                       in case mbBound of
                            Nothing ->
                              directApplication (InstBot ty)
                            Just bound
                              | alphaEqType ty (tyToElab bound) ->
                                  directApplication InstId
                            _ -> Nothing
                    _ -> Nothing
              )
          _ ->
            Just (InstSeq i1 i2, \term -> Just (ETyInst (ETyInst term i1) i2))
      InstAppF ty ->
        Just
          ( InstApp ty,
            \term ->
              case project term of
                ETyAbsFRef ref (Just bound) body
                  | alphaEqType ty (tyToElab bound) ->
                      let body' = replaceAbstrInTermRef ref InstId body
                       in Just (substTypeVarTermRef ref ty body')
                _ ->
                  Just (ETyInst term (InstSeq (InstInside (InstBot ty)) InstElim))
          )
      InstIntroF ->
        Just
          ( InstIntro,
            \term ->
              let (ref, _) = freshTypeBinderRefFromNames (freeTypeVarAliasNamesTerm term) (identityGeneratorAfterTerm term)
               in Just (eTyAbsWithRef ref Nothing term)
          )
      InstElimF ->
        Just
          ( InstElim,
            \term -> case project term of
              ETyAbsFRef ref mbBound body ->
                let bound = boundType mbBound
                    body' = replaceAbstrInTermRef ref InstId body
                 in Just (substTypeVarTermRef ref bound body')
              _ -> Nothing
          )
      InstUnderFRef underRef (phi, _) ->
        Just
          ( instUnderWithRef underRef phi,
            \term -> case project term of
              ETyAbsFRef absRef mbBound body ->
                let phi' = renameInstBoundRef underRef absRef phi
                 in Just (eTyAbsWithRef absRef mbBound (ETyInst body phi'))
              _ -> Nothing
          )
      InstInsideF (phi, _) ->
        Just
          ( InstInside phi,
            \term -> case project term of
              ETyAbsFRef ref mbBound body -> do
                let bound0 = boundType mbBound
                bound1 <- either (const Nothing) Just (applyInstantiation bound0 phi)
                let mb' = case bound1 of
                      TBottom -> Nothing
                      TVarRef {} -> Nothing
                      _ -> either (const Nothing) Just (elabToBound bound1)
                    body' = replaceAbstrInTermRef ref (InstSeq phi (instAbstrWithRef ref)) body
                Just (eTyAbsWithRef ref mb' body')
              _ -> Nothing
          )
      InstBotF ty -> Just (InstBot ty, const Nothing)
      InstAbstrFRef ref -> Just (instAbstrWithRef ref, const Nothing)

boundType :: Maybe BoundType -> ElabType
boundType = maybe TBottom tyToElab

freeResolvedTermVars :: XmlfTerm -> [ResolvedVar]
freeResolvedTermVars =
  go []
  where
    go bound term =
      case term of
        EVarNode resolved
          | resolvedVarBoundBy bound resolved -> []
          | otherwise -> [resolved]
        ELit {} -> []
        ELam resolved body ->
          go (resolved : bound) body
        EApp fun arg ->
          go bound fun ++ go bound arg
        ELet resolved _ rhs body ->
          let bound' = resolved : bound
           in go bound' rhs ++ go bound' body
        ETyAbsRef _ _ body ->
          go bound body
        ETyInst inner _ ->
          go bound inner
        ERoll _ body ->
          go bound body
        EUnroll body ->
          go bound body

freeResolvedTermReferenceNames :: XmlfTerm -> Set.Set String
freeResolvedTermReferenceNames =
  Set.unions . map resolvedVarAliasNames . freeResolvedTermVars

freeResolvedTermIdentityKeys :: XmlfTerm -> Set.Set ResolvedTermIdentityKey
freeResolvedTermIdentityKeys =
  Set.fromList . map resolvedVarIdentityKey . freeResolvedTermVars

termMentionsFreeResolvedVar :: ResolvedVar -> XmlfTerm -> Bool
termMentionsFreeResolvedVar expected term =
  Set.member (resolvedVarIdentityKey expected) (freeResolvedTermIdentityKeys term)

valueHasLeadingTyAbs :: XmlfTerm -> Bool
valueHasLeadingTyAbs term = case term of
  ETyAbsRef {} -> True
  ERoll _ body -> valueHasLeadingTyAbs body
  _ -> False

termHasLeadingTyAbs :: XmlfTerm -> Bool
termHasLeadingTyAbs term = case term of
  ETyAbsRef {} -> True
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
  InstUnderRef _ inner -> instConsumesForall inner
  InstBot _ -> False
  InstAbstrRef _ -> False

freeTypeVarAliasNamesTerm :: XmlfTerm -> Set.Set String
freeTypeVarAliasNamesTerm =
  Set.unions . map typeBinderRefAliasNames . freeTypeVarRefsTerm

freeTypeVarRefsInst :: Instantiation -> [TypeBinderRef]
freeTypeVarRefsInst = foldInstantiation alg
  where
    alg inst = case inst of
      InstIdF -> []
      InstAppF ty -> freeTypeVarRefsType ty
      InstBotF ty -> freeTypeVarRefsType ty
      InstIntroF -> []
      InstElimF -> []
      InstAbstrFRef ref -> [ref]
      InstInsideF refs -> refs
      InstSeqF left right -> unionRefs left right
      InstUnderFRef ref refs -> filter (not . typeBinderRefsSameIdentity ref) refs

freeTypeVarRefsTerm :: XmlfTerm -> [TypeBinderRef]
freeTypeVarRefsTerm = foldXmlfTerm alg
  where
    alg term = case term of
      EVarNodeF resolved -> freeTypeVarRefsType (resolvedVarType resolved)
      ELitF _ -> []
      ELamF resolved body -> unionRefs (freeTypeVarRefsType (resolvedVarType resolved)) body
      EAppF f a -> unionRefs f a
      ELetF resolved sch rhs body ->
        unionRefs
          (freeTypeVarRefsType (resolvedVarType resolved))
          (unionRefs (freeTypeVarRefsType (schemeToType sch)) (unionRefs rhs body))
      ETyAbsFRef ref mb body ->
        unionRefs
          (maybe [] freeTypeVarRefsType mb)
          (filter (not . typeBinderRefsSameIdentity ref) body)
      ETyInstF e inst -> unionRefs e (freeTypeVarRefsInst inst)
      ERollF ty body -> unionRefs (freeTypeVarRefsType ty) body
      EUnrollF body -> body

unionRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | typeRefMember ref refs = refs
      | otherwise = ref : refs

typeRefMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
typeRefMember ref =
  any (typeBinderRefsSameIdentity ref)

freshTermNameFrom :: String -> Set.Set String -> String
freshTermNameFrom base used =
  let candidates = base : [base ++ show i | i <- [(1 :: Int) ..]]
   in case filter (`Set.notMember` used) candidates of
        (x : _) -> x
        [] -> base

type TermVarKey = ResolvedVar

substResolvedTermVar :: ResolvedVar -> XmlfTerm -> XmlfTerm -> XmlfTerm
substResolvedTermVar = substTermVarWithKey

substTermVarWithKey :: TermVarKey -> XmlfTerm -> XmlfTerm -> XmlfTerm
substTermVarWithKey key s = goSub
  where
    x = termVarKeyName key
    freeSVarKeys = freeResolvedTermIdentityKeys s
    replacementFreeNames = freeResolvedTermReferenceNames s
    termVarKeyName = resolvedVarReferenceName
    resolvedMatches = resolvedVarSameIdentity
    goSub = para alg
      where
        alg term = case term of
          EVarNodeF resolved
            | resolvedMatches key resolved -> s
            | otherwise -> EVarNode resolved
          ELitF l -> ELit l
          ELamF resolved body
            | resolvedMatches key resolved -> ELam resolved (fst body)
            | Set.member (resolvedVarIdentityKey resolved) freeSVarKeys ->
                let used = Set.unions [replacementFreeNames, freeResolvedTermReferenceNames (fst body), Set.singleton x]
                    v' = freshTermNameFrom binderName used
                    (resolved', _) = freshenResolvedLocalVar v' (identityGeneratorAfterTerm (EApp s (fst body))) resolved
                    body' = substResolvedTermVar resolved (EVarNode resolved') (fst body)
                 in ELam resolved' (goSub body')
            | otherwise -> ELam resolved (snd body)
            where
              binderName = resolvedVarReferenceName resolved
          EAppF f a -> EApp (snd f) (snd a)
          ELetF resolved sch rhs body
            | resolvedMatches key resolved -> ELet resolved sch (snd rhs) (fst body)
            | Set.member (resolvedVarIdentityKey resolved) freeSVarKeys ->
                let used = Set.unions [replacementFreeNames, freeResolvedTermReferenceNames (fst body), Set.singleton x]
                    v' = freshTermNameFrom binderName used
                    (resolved', _) = freshenResolvedLocalVar v' (identityGeneratorAfterTerm (EApp s (ELet resolved sch (fst rhs) (fst body)))) resolved
                    body' = substResolvedTermVar resolved (EVarNode resolved') (fst body)
                 in ELet resolved' sch (snd rhs) (goSub body')
            | otherwise -> ELet resolved sch (snd rhs) (snd body)
            where
              binderName = resolvedVarReferenceName resolved
          ETyAbsFRef ref b body -> eTyAbsWithRef ref b (snd body)
          ETyInstF e i -> ETyInst (snd e) i
          ERollF ty body -> ERoll ty (snd body)
          EUnrollF body -> EUnroll (snd body)

substTypeVarTermRef :: TypeBinderRef -> ElabType -> XmlfTerm -> XmlfTerm
substTypeVarTermRef target s = goSub
  where
    x = typeBinderRefName target
    freeSRefs = freeTypeVarRefsType s
    freeSNames = Set.unions (map typeBinderRefAliasNames freeSRefs)
    freshCaptureRef name mb body =
      fst (freshTypeBinderRef name (identityGeneratorAfterTerm seed))
      where
        seed =
          ERoll
            (maybe s (\bound -> TArrow s (tyToElab bound)) mb)
            body
    substBoundVar mb = do
      bnd <- mb
      let result = substTypeCaptureRef target s (tyToElab bnd)
      case result of
        TVarRef {} -> Nothing
        TBottom -> Nothing
        _ -> either (const Nothing) Just (elabToBound result)
    goSub = para alg
      where
        alg term = case term of
          EVarNodeF resolved ->
            EVarNode (mapResolvedVarType (substTypeCaptureRef target s) resolved)
          ELitF l -> ELit l
          ELamF resolved body ->
            ELam (mapResolvedVarType (substTypeCaptureRef target s) resolved) (snd body)
          EAppF f a -> EApp (snd f) (snd a)
          ELetF resolved sch rhs body ->
            ELet
              (mapResolvedVarType (substTypeCaptureRef target s) resolved)
              (substTypeVarSchemeRef target s sch)
              (snd rhs)
              (snd body)
          ETyAbsFRef ref mb body
            | typeBinderRefsSameIdentity ref target -> eTyAbsWithRef ref (substBoundVar mb) (fst body)
            | typeRefMember ref freeSRefs ->
                let used = Set.unions [freeSNames, freeTypeVarAliasNamesTerm (fst body), Set.singleton x]
                    v' = freshTermNameFrom v used
                    ref' = freshCaptureRef v' mb (fst body)
                    body' = substTypeVarTermRef ref (tVarWithRef ref') (fst body)
                 in eTyAbsWithRef ref' (substBoundVar mb) (goSub body')
            | otherwise -> eTyAbsWithRef ref (substBoundVar mb) (snd body)
            where
              v = typeBinderRefName ref
          ETyInstF e i -> ETyInst (snd e) (substTypeVarInstRef target s i)
          ERollF ty body -> ERoll (substTypeCaptureRef target s ty) (snd body)
          EUnrollF body -> EUnroll (snd body)

substTypeVarSchemeRef :: TypeBinderRef -> ElabType -> ElabScheme -> ElabScheme
substTypeVarSchemeRef target s sch =
  let ty = schemeToType sch
      ty' = substTypeCaptureRef target s ty
   in schemeFromType ty'

substTypeVarInstRef :: TypeBinderRef -> ElabType -> Instantiation -> Instantiation
substTypeVarInstRef target s = para alg
  where
    alg inst = case inst of
      InstIdF -> InstId
      InstAppF t -> InstApp (substTypeCaptureRef target s t)
      InstBotF t -> InstBot (substTypeCaptureRef target s t)
      InstIntroF -> InstIntro
      InstElimF -> InstElim
      InstAbstrFRef ref -> instAbstrWithRef ref
      InstInsideF i -> InstInside (snd i)
      InstSeqF a b -> InstSeq (snd a) (snd b)
      InstUnderFRef ref i
        | typeBinderRefsSameIdentity ref target -> instUnderWithRef ref (fst i)
        | otherwise -> instUnderWithRef ref (snd i)

replaceAbstrInTermRef :: TypeBinderRef -> Instantiation -> XmlfTerm -> XmlfTerm
replaceAbstrInTermRef target replacement = para alg
  where
    alg term = case term of
      EVarNodeF resolved -> EVarNode resolved
      ELitF l -> ELit l
      ELamF resolved body -> ELam resolved (snd body)
      EAppF f a -> EApp (snd f) (snd a)
      ELetF resolved sch rhs body -> ELet resolved sch (snd rhs) (snd body)
      ETyAbsFRef ref mb body
        | typeBinderRefsSameIdentity ref target -> eTyAbsWithRef ref mb (fst body)
        | otherwise -> eTyAbsWithRef ref mb (snd body)
      ETyInstF e inst ->
        case replaceAbstrInInstRef target replacement inst of
          InstId -> snd e
          inst' -> ETyInst (snd e) inst'
      ERollF ty body -> ERoll ty (snd body)
      EUnrollF body -> EUnroll (snd body)

replaceAbstrInInstRef :: TypeBinderRef -> Instantiation -> Instantiation -> Instantiation
replaceAbstrInInstRef target replacement = para alg
  where
    alg inst = case inst of
      InstIdF -> InstId
      InstAppF t -> InstApp t
      InstBotF t -> InstBot t
      InstIntroF -> InstIntro
      InstElimF -> InstElim
      InstAbstrFRef ref
        | typeBinderRefsSameIdentity ref target -> replacement
        | otherwise -> instAbstrWithRef ref
      InstInsideF i -> InstInside (snd i)
      InstSeqF a b -> InstSeq (snd a) (snd b)
      InstUnderFRef ref i
        | typeBinderRefsSameIdentity ref target -> instUnderWithRef ref (fst i)
        | otherwise -> instUnderWithRef ref (snd i)
