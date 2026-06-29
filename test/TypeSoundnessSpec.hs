{-# LANGUAGE GADTs #-}
module TypeSoundnessSpec (spec) where

import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , arbitrary
    , checkCoverage
    , chooseInt
    , cover
    , counterexample
    , elements
    , forAll
    , frequency
    , property
    , sized
    , withMaxSuccess
    , (===)
    )

import MLF.Constraint.Types.Graph (BaseTy(..), NodeId(..))
import MLF.Elab.Pipeline
    ( BoundType
    , XmlfTerm(..)
    , ElabType
    , Instantiation(..)
    , Ty(..)
    , isValue
    , normalize
    , schemeFromType
    , step
    , typeCheck
    )
import ElabTermTestSupport (generatedResolvedLocal, mkTestDeferredVar, mkTestLocalLam, mkTestLocalLet, mkTestTyAbs, testTMu, testTVar)
import MLF.Types.Elab
    ( ResolvedVar
    , TypeBinderRef
    , instUnderWithRef
    , renameResolvedLocalVar
    , resolvedVarSameIdentity
    , typeBinderIdentityFromNode
    , typeBinderRefFromIdentity
    )
import MLF.Frontend.Syntax (Lit(..))

spec :: Spec
spec = describe "Phase 7 theorem obligations" $ do
    it "checks closedness by resolved identity when names are stale" $ do
        let intType = TBase (BaseTy "Int")
            binder = generatedResolvedLocal 91701 "x" "x" intType
            occurrence = renameResolvedLocalVar "$stale_x" binder
        isClosedTerm (ELam binder (EVarNode occurrence)) `shouldBe` True

    it "Preservation proxy: if typeCheck t = Right tau and step t = Just t', then typeCheck t' = Right tau" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedXmlfTerm $ \term ->
                    checkCoverage $
                        cover 20 (isJust (step term)) "reducible" $
                        cover 20 (isValue term) "value" $
                        cover 5 (isETyAbsBounded term) "ETyAbs-bounded" $
                        cover 5 (isETyInst term) "ETyInst" $
                        cover 3 (hasInstInside term) "InstInside" $
                        cover 3 (hasInstUnder term) "InstUnder" $
                        cover 3 (hasInstElim term) "InstElim" $
                            case typeCheck term of
                                Left err ->
                                    counterexample
                                        ( "generator produced ill-typed term:\n"
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show err
                                        )
                                        False
                                Right ty ->
                                    case step term of
                                        Nothing -> property True
                                        Just term' ->
                                            counterexample
                                                ( "preservation failed\nterm: "
                                                    ++ show term
                                                    ++ "\nterm': "
                                                    ++ show term'
                                                    ++ "\ntype(term): "
                                                    ++ show ty
                                                    ++ "\ntype(term'): "
                                                    ++ show (typeCheck term')
                                                )
                                                (typeCheck term' === Right ty)

    it "Progress proxy for closed terms: well-typed term is value or steps" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedXmlfTerm $ \term ->
                    checkCoverage $
                        cover 20 (isValue term) "value" $
                        cover 20 (isJust (step term)) "steps" $
                        cover 5 (isETyAbsBounded term) "ETyAbs-bounded" $
                        cover 5 (isETyInst term) "ETyInst" $
                        cover 3 (hasInstInside term) "InstInside" $
                        cover 3 (hasInstUnder term) "InstUnder" $
                        cover 3 (hasInstElim term) "InstElim" $
                            case typeCheck term of
                                Left err ->
                                    counterexample
                                        ( "generator produced ill-typed term:\n"
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show err
                                        )
                                        False
                                Right _ ->
                                    if not (isClosedTerm term)
                                        then
                                            counterexample
                                                ("expected closed generator output, got open term: " ++ show term)
                                                False
                                        else
                                            counterexample
                                                ("progress failed on term: " ++ show term)
                                                (isValue term || isJust (step term))

    it "Multi-step preservation proxy: typeCheck t = Right tau => typeCheck (normalize t) = Right tau" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedXmlfTerm $ \term ->
                    checkCoverage $
                        cover 5 (isETyAbsBounded term) "ETyAbs-bounded" $
                        cover 5 (isETyInst term) "ETyInst" $
                        cover 3 (hasInstInside term) "InstInside" $
                        cover 3 (hasInstUnder term) "InstUnder" $
                        cover 3 (hasInstElim term) "InstElim" $
                            case typeCheck term of
                        Left err ->
                            counterexample
                                ( "generator produced ill-typed term:\n"
                                    ++ show term
                                    ++ "\nerror: "
                                    ++ show err
                                )
                                False
                        Right ty ->
                            let term' = normalize term
                            in counterexample
                                ( "multi-step preservation failed\nterm: "
                                    ++ show term
                                    ++ "\nnormalize(term): "
                                    ++ show term'
                                    ++ "\ntype(term): "
                                    ++ show ty
                                    ++ "\ntype(normalize(term)): "
                                    ++ show (typeCheck term')
                                )
                                (typeCheck term' === Right ty)

    it "One-step normalization proxy: stepping preserves the final normal form" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedXmlfTerm $ \term ->
                    checkCoverage $
                        cover 20 (isJust (step term)) "reducible" $
                        cover 20 (not (isJust (step term))) "normal form" $
                        cover 5 (isETyAbsBounded term) "ETyAbs-bounded" $
                        cover 5 (isETyInst term) "ETyInst" $
                        cover 3 (hasInstInside term) "InstInside" $
                        cover 3 (hasInstUnder term) "InstUnder" $
                        cover 3 (hasInstElim term) "InstElim" $
                            case typeCheck term of
                                Left err ->
                                    counterexample
                                        ( "generator produced ill-typed term:\n"
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show err
                                        )
                                        False
                                Right _ ->
                                    case step term of
                                        Nothing ->
                                            counterexample
                                                ( "normal-form term changed under normalize\nterm: "
                                                    ++ show term
                                                    ++ "\nnormalize(term): "
                                                    ++ show (normalize term)
                                                )
                                                (normalize term === term)
                                        Just term' ->
                                            counterexample
                                                ( "single step changed final normal form\nterm: "
                                                    ++ show term
                                                    ++ "\nterm': "
                                                    ++ show term'
                                                    ++ "\nnormalize(term): "
                                                    ++ show (normalize term)
                                                    ++ "\nnormalize(term'): "
                                                    ++ show (normalize term')
                                                )
                                                (normalize term' === normalize term)

    it "Canonical-forms proxy: values at base type are the expected literals" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedXmlfTerm $ \term ->
                    let checked = typeCheck term
                        valueTerm = isValue term
                        baseValue =
                            case checked of
                                Right (TBase (BaseTy "Int")) -> valueTerm
                                Right (TBase (BaseTy "Bool")) -> valueTerm
                                _ -> False
                    in checkCoverage $
                        cover 20 valueTerm "value" $
                        cover 10 baseValue "base-value" $
                            case checked of
                                Left err ->
                                    counterexample
                                        ( "generator produced ill-typed term:\n"
                                            ++ show term
                                            ++ "\nerror: "
                                            ++ show err
                                        )
                                        False
                                Right ty
                                    | not valueTerm -> property True  -- only check values
                                    | ty == TBase (BaseTy "Int") ->
                                        counterexample
                                            ("canonical-forms (Int) failed on value: " ++ show term)
                                            (isLitInt term)
                                    | ty == TBase (BaseTy "Bool") ->
                                        counterexample
                                            ("canonical-forms (Bool) failed on value: " ++ show term)
                                            (isLitBool term)
                                    | otherwise -> property True  -- non-base types: skip

    it "Recursive-runtime obligations carry matching ERoll/EUnroll/context evidence" $
        property $
            withMaxSuccess 120 $
                forAll genRecursiveRuntimeWitness $ \witness ->
                    checkCoverage $
                        cover 25 (hasRuntimeObligation RollObligation witness) "roll-obligation" $
                        cover 25 (hasRuntimeObligation UnrollObligation witness) "unroll-obligation" $
                        cover 25 (hasRuntimeObligation RecursiveContextObligation witness) "recursive-context-obligation" $
                        cover 20 (termHasRollEvidence (rrTerm witness)) "ERoll-evidence" $
                        cover 20 (termHasUnrollEvidence (rrTerm witness)) "EUnroll-evidence" $
                        cover 20 (hasRecursiveContextEvidence (rrContext witness)) "recursive-context-evidence" $
                            counterexample
                                ( "recursive-runtime obligation mismatch\nobligations: "
                                    ++ show (rrObligations witness)
                                    ++ "\nterm: "
                                    ++ show (rrTerm witness)
                                    ++ "\ncontext: "
                                    ++ show (rrContext witness)
                                )
                                (recursiveRuntimeObligationsSatisfied witness)

    it "Recursive-runtime obligation matcher fails when roll/unroll/context evidence is missing" $
        property $
            withMaxSuccess 90 $
                forAll genBrokenRecursiveRuntimeWitness $ \witness ->
                    checkCoverage $
                        cover 20 (hasRuntimeObligation RollObligation witness) "missing-roll-obligation" $
                        cover 20 (hasRuntimeObligation UnrollObligation witness) "missing-unroll-obligation" $
                        cover 20 (hasRuntimeObligation RecursiveContextObligation witness) "missing-context-obligation" $
                        cover 20 (not (termHasRollEvidence (rrTerm witness))) "missing-ERoll-evidence" $
                        cover 20 (not (termHasUnrollEvidence (rrTerm witness))) "missing-EUnroll-evidence" $
                        cover 20 (not (hasRecursiveContextEvidence (rrContext witness))) "missing-recursive-context-evidence" $
                            counterexample
                                ( "recursive-runtime obligation unexpectedly passed\nobligations: "
                                    ++ show (rrObligations witness)
                                    ++ "\nterm: "
                                    ++ show (rrTerm witness)
                                    ++ "\ncontext: "
                                    ++ show (rrContext witness)
                                )
                                (recursiveRuntimeObligationsSatisfied witness === False)

data RecursiveRuntimeObligation
    = RollObligation
    | UnrollObligation
    | RecursiveContextObligation
    deriving (Eq, Show)

data RuntimeContextEvidence
    = RuntimeCtxRoll ElabType
    | RuntimeCtxUnroll
    deriving (Eq, Show)

data RecursiveRuntimeWitness = RecursiveRuntimeWitness
    { rrObligations :: [RecursiveRuntimeObligation]
    , rrTerm :: XmlfTerm
    , rrContext :: [RuntimeContextEvidence]
    }
    deriving (Eq, Show)

recursiveRuntimeTy :: ElabType
recursiveRuntimeTy = testTMu "self" (TArrow (testTVar "self") intTy)

recursiveRuntimeBody :: XmlfTerm
recursiveRuntimeBody = mkTestLocalLam "self" recursiveRuntimeTy (ELit (LInt 1))

recursiveRollTerm :: XmlfTerm
recursiveRollTerm = ERoll recursiveRuntimeTy recursiveRuntimeBody

recursiveUnrollTerm :: XmlfTerm
recursiveUnrollTerm = EUnroll recursiveRollTerm

genRecursiveRuntimeWitness :: Gen RecursiveRuntimeWitness
genRecursiveRuntimeWitness =
    elements
        [ RecursiveRuntimeWitness
            [RollObligation]
            recursiveRollTerm
            []
        , RecursiveRuntimeWitness
            [UnrollObligation]
            recursiveUnrollTerm
            []
        , RecursiveRuntimeWitness
            [RecursiveContextObligation]
            recursiveRollTerm
            [RuntimeCtxUnroll]
        , RecursiveRuntimeWitness
            [RollObligation, UnrollObligation, RecursiveContextObligation]
            recursiveUnrollTerm
            [RuntimeCtxRoll recursiveRuntimeTy, RuntimeCtxUnroll]
        ]

genBrokenRecursiveRuntimeWitness :: Gen RecursiveRuntimeWitness
genBrokenRecursiveRuntimeWitness =
    elements
        [ RecursiveRuntimeWitness
            [RollObligation]
            recursiveRuntimeBody
            []
        , RecursiveRuntimeWitness
            [UnrollObligation]
            recursiveRollTerm
            []
        , RecursiveRuntimeWitness
            [RecursiveContextObligation]
            recursiveRollTerm
            []
        ]

hasRuntimeObligation :: RecursiveRuntimeObligation -> RecursiveRuntimeWitness -> Bool
hasRuntimeObligation obligation witness = obligation `elem` rrObligations witness

recursiveRuntimeObligationsSatisfied :: RecursiveRuntimeWitness -> Bool
recursiveRuntimeObligationsSatisfied witness =
    all obligationSatisfied (rrObligations witness)
  where
    obligationSatisfied obligation = case obligation of
        RollObligation -> termHasRollEvidence (rrTerm witness)
        UnrollObligation -> termHasUnrollEvidence (rrTerm witness)
        RecursiveContextObligation -> hasRecursiveContextEvidence (rrContext witness)

termHasRollEvidence :: XmlfTerm -> Bool
termHasRollEvidence term = case term of
    EVarNode _ -> False
    ELit _ -> False
    ELam _ body -> termHasRollEvidence body
    EApp f a -> termHasRollEvidence f || termHasRollEvidence a
    ELet _ _ rhs body -> termHasRollEvidence rhs || termHasRollEvidence body
    ETyAbsRef _ _ body -> termHasRollEvidence body
    ETyInst e _ -> termHasRollEvidence e
    ERoll _ _ -> True
    EUnroll body -> termHasRollEvidence body

termHasUnrollEvidence :: XmlfTerm -> Bool
termHasUnrollEvidence term = case term of
    EVarNode _ -> False
    ELit _ -> False
    ELam _ body -> termHasUnrollEvidence body
    EApp f a -> termHasUnrollEvidence f || termHasUnrollEvidence a
    ELet _ _ rhs body -> termHasUnrollEvidence rhs || termHasUnrollEvidence body
    ETyAbsRef _ _ body -> termHasUnrollEvidence body
    ETyInst e _ -> termHasUnrollEvidence e
    ERoll _ body -> termHasUnrollEvidence body
    EUnroll _ -> True

hasRecursiveContextEvidence :: [RuntimeContextEvidence] -> Bool
hasRecursiveContextEvidence = any isRecursiveContextFrame
  where
    isRecursiveContextFrame evidence = case evidence of
        RuntimeCtxRoll _ -> True
        RuntimeCtxUnroll -> True

-- ---------------------------------------------------------------------------
-- Sized typed-by-construction generator
-- ---------------------------------------------------------------------------

-- | Typing context: maps term variables to their types.
type TyCtx = Map.Map String ElabType

-- | Base types used in generation.
intTy, boolTy :: ElabType
intTy  = TBase (BaseTy "Int")
boolTy = TBase (BaseTy "Bool")

-- | Pick a random ground type.
genGroundTy :: Gen ElabType
genGroundTy = elements [intTy, boolTy]

-- | Pick a random type (ground or arrow at depth).
genTy :: Int -> Gen ElabType
genTy n
    | n <= 0    = genGroundTy
    | otherwise = frequency
        [ (3, genGroundTy)
        , (1, TArrow <$> genTy (n - 1) <*> genTy (n - 1))
        ]

-- | Fresh variable name from an index.
freshVar :: Int -> String
freshVar i = "v" ++ show i

-- | Generate a well-typed closed term at a given type, using a typing context.
--   The Int parameter is the size budget; the second Int is a fresh-name counter.
genTermAtType :: TyCtx -> Int -> Int -> ElabType -> Gen (XmlfTerm, Int)
genTermAtType ctx size fresh goalTy
    | size <= 0 = genAtom ctx fresh goalTy
    | otherwise = frequency $
        atomWeight ++ lamWeight ++ appWeight ++ letWeight
  where
    -- Atoms: variables in scope at the right type, or literals
    atomWeight = [(3, genAtom ctx fresh goalTy)]

    -- Lambda: if goal is TArrow a b, generate an identity-backed local lambda.
    lamWeight = case goalTy of
        TArrow argTy resTy ->
            let v = freshVar fresh
                ctx' = Map.insert v argTy ctx
            in [(2, do
                    (body, fresh') <- genTermAtType ctx' (size - 1) (fresh + 1) resTy
                    pure (mkTestLocalLam v argTy body, fresh')
               )]
        _ -> []

    -- Application: pick a random argument type, generate f : argTy -> goalTy, a : argTy
    appWeight =
        [(2, do
            argTy <- genTy 0
            (f, fresh1) <- genTermAtType ctx (size - 1) fresh (TArrow argTy goalTy)
            (a, fresh2) <- genTermAtType ctx (size - 1) fresh1 argTy
            pure (EApp f a, fresh2)
        )]

    -- Let: generate rhs at some type, bind it, generate body at goalTy
    letWeight =
        [(1, do
            rhsTy <- genTy 0
            let v = freshVar fresh
            (rhs, fresh1) <- genTermAtType ctx (size - 1) (fresh + 1) rhsTy
            let ctx' = Map.insert v rhsTy ctx
            (body, fresh2) <- genTermAtType ctx' (size - 1) fresh1 goalTy
            pure (mkTestLocalLet v (schemeFromType rhsTy) rhs body, fresh2)
        )]

-- | Generate an atomic term at a given type (leaf of the generation tree).
genAtom :: TyCtx -> Int -> ElabType -> Gen (XmlfTerm, Int)
genAtom ctx fresh goalTy =
    let varCandidates =
            [ mkTestDeferredVar v
            | (v, ty) <- Map.toList ctx
            , ty == goalTy
            ]
        litCandidates
            | goalTy == intTy  = [genIntLit]
            | goalTy == boolTy = [genBoolLit]
            | otherwise        = []
        -- Fallback: generate a lambda value at arrow types,
        -- or a type-correct literal at ground types
        fallback = case goalTy of
            TArrow argTy resTy -> do
                let v = freshVar fresh
                    ctx' = Map.insert v argTy ctx
                (body, fresh') <- genAtom ctx' (fresh + 1) resTy
                pure (mkTestLocalLam v argTy body, fresh')
            _ -> do
                lit <- genLitAtType goalTy
                pure (lit, fresh)
        candidates =
            map (\t -> (2, pure (t, fresh))) varCandidates
            ++ map (\g -> (3, (\t -> (t, fresh)) <$> g)) litCandidates
    in if null candidates
        then fallback
        else frequency candidates

genIntLit :: Gen XmlfTerm
genIntLit = do
    n <- chooseInt (-10, 10)
    pure (ELit (LInt (fromIntegral n)))

genBoolLit :: Gen XmlfTerm
genBoolLit = ELit . LBool <$> arbitrary

-- | Generate a literal at a specific ground type. Falls back to int for
--   types that have no literal form.
genLitAtType :: ElabType -> Gen XmlfTerm
genLitAtType ty
    | ty == intTy  = genIntLit
    | ty == boolTy = genBoolLit
    | otherwise    = genIntLit  -- unreachable for our ground types

-- | Top-level generator: pick a random goal type, generate a closed well-typed
--   term at that type using the sized combinator. Uses frequency to cover both
--   monomorphic and polymorphic (ETyAbs, ETyInst) forms.
genClosedWellTypedXmlfTerm :: Gen XmlfTerm
genClosedWellTypedXmlfTerm = sized $ \s -> do
    goalTy <- genTy (min s 1)
    let depth = min s 3  -- cap depth to keep terms tractable
    (term, fresh) <- genTermAtType Map.empty depth 0 goalTy
    frequency
        [ (4, pure term)                                       -- plain monomorphic
        , (1, genUnboundedTyAbsWrap fresh term)                -- ETyAbs-unbounded
        , (1, genBoundedTyAbsWrap fresh goalTy term)           -- ETyAbs-bounded
        , (1, genInstElim fresh term)                          -- InstElim
        , (1, genInstInsideBot fresh goalTy term)              -- InstInside
        , (1, genInstApp fresh goalTy term)                    -- InstApp
        , (1, genInstIntroElim fresh term)                     -- InstIntro + round-trip
        , (1, genInstUnderTrivial fresh term)                  -- InstUnder
        ]

-- | Wrap in a vacuous unbounded type abstraction: Λα. term
genUnboundedTyAbsWrap :: Int -> XmlfTerm -> Gen XmlfTerm
genUnboundedTyAbsWrap fresh term = do
    let tv = "t" ++ show fresh
    pure (mkTestTyAbs tv Nothing term)

-- | Wrap in a bounded type abstraction: Λ(α ⩾ τ). term
--   The bound is a ground type, and the body doesn't mention α.
genBoundedTyAbsWrap :: Int -> ElabType -> XmlfTerm -> Gen XmlfTerm
genBoundedTyAbsWrap fresh _goalTy term = do
    let tv = "t" ++ show fresh
    bound <- genGroundBound
    pure (mkTestTyAbs tv (Just bound) term)

-- | InstElim: (Λα. body) N — eliminate a vacuous type abstraction
genInstElim :: Int -> XmlfTerm -> Gen XmlfTerm
genInstElim fresh term = do
    let tv = "t" ++ show fresh
    pure (ETyInst (mkTestTyAbs tv Nothing term) InstElim)

-- | InstInside with InstBot: (Λα. body) ∀(⩾ τ)
genInstInsideBot :: Int -> ElabType -> XmlfTerm -> Gen XmlfTerm
genInstInsideBot fresh _goalTy term = do
    let tv = "t" ++ show fresh
    ty <- genGroundTy
    pure (ETyInst (mkTestTyAbs tv Nothing term) (InstInside (InstBot ty)))

-- | InstApp: (Λα. body) ⟨τ⟩
genInstApp :: Int -> ElabType -> XmlfTerm -> Gen XmlfTerm
genInstApp fresh goalTy term = do
    let tv = "t" ++ show fresh
    pure (ETyInst (mkTestTyAbs tv Nothing term) (InstApp goalTy))

-- | InstIntro + InstElim round-trip: ((term O) N)
genInstIntroElim :: Int -> XmlfTerm -> Gen XmlfTerm
genInstIntroElim _fresh term =
    pure (ETyInst (ETyInst term InstIntro) InstElim)

-- | InstUnder with trivial InstId: (Λα. body) ∀(β ⩾) 1
genInstUnderTrivial :: Int -> XmlfTerm -> Gen XmlfTerm
genInstUnderTrivial fresh term = do
    let tv = "t" ++ show fresh
        tv2 = "t" ++ show (fresh + 1)
    pure (ETyInst (mkTestTyAbs tv Nothing term) (instUnderWithRef (typeRef (fresh + 1) tv2) InstId))

-- | Generate a ground BoundType (TBase is polymorphic in TopVar).
genGroundBound :: Gen BoundType
genGroundBound = elements [TBase (BaseTy "Int"), TBase (BaseTy "Bool")]

typeRef :: Int -> String -> TypeBinderRef
typeRef key name =
    typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isClosedTerm :: XmlfTerm -> Bool
isClosedTerm = null . freeTermVars

freeTermVars :: XmlfTerm -> [ResolvedVar]
freeTermVars term = case term of
    EVarNode resolved -> [resolved]
    ELit _ -> []
    ELam resolved body -> filter (not . resolvedVarSameIdentity resolved) (freeTermVars body)
    EApp f a -> freeTermVars f ++ freeTermVars a
    ELet resolved _ rhs body ->
        freeTermVars rhs ++ filter (not . resolvedVarSameIdentity resolved) (freeTermVars body)
    ETyAbsRef _ _ body -> freeTermVars body
    ETyInst e _ -> freeTermVars e
    ERoll _ body -> freeTermVars body
    EUnroll body -> freeTermVars body

-- | Cover-label predicates for polymorphic constructor families.
isETyAbsBounded :: XmlfTerm -> Bool
isETyAbsBounded (ETyAbsRef _ (Just _) _) = True
isETyAbsBounded _ = False

isETyInst :: XmlfTerm -> Bool
isETyInst ETyInst{} = True
isETyInst _ = False

hasInstInside :: XmlfTerm -> Bool
hasInstInside (ETyInst _ (InstInside _)) = True
hasInstInside _ = False

hasInstUnder :: XmlfTerm -> Bool
hasInstUnder (ETyInst _ (InstUnderRef _ _)) = True
hasInstUnder _ = False

hasInstElim :: XmlfTerm -> Bool
hasInstElim (ETyInst _ InstElim) = True
hasInstElim _ = False

isLitInt :: XmlfTerm -> Bool
isLitInt (ELit (LInt _)) = True
isLitInt _ = False

isLitBool :: XmlfTerm -> Bool
isLitBool (ELit (LBool _)) = True
isLitBool _ = False
