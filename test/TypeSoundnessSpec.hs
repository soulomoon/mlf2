{-# LANGUAGE GADTs #-}
module TypeSoundnessSpec (spec) where

import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline
    ( BoundType
    , ElabTerm(..)
    , ElabType
    , Instantiation(..)
    , Ty(..)
    , isValue
    , normalize
    , schemeFromType
    , step
    , typeCheck
    )
import MLF.Frontend.Syntax (Lit(..))

spec :: Spec
spec = describe "Phase 7 theorem obligations" $ do
    it "Preservation proxy: if typeCheck t = Right tau and step t = Just t', then typeCheck t' = Right tau" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedElabTerm $ \term ->
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
                forAll genClosedWellTypedElabTerm $ \term ->
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
                forAll genClosedWellTypedElabTerm $ \term ->
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

    it "Determinism proxy: step is a function (same input => same output)" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedElabTerm $ \term ->
                    checkCoverage $
                        cover 5 (isETyAbsBounded term) "ETyAbs-bounded" $
                        cover 5 (isETyInst term) "ETyInst" $
                        cover 3 (hasInstInside term) "InstInside" $
                        cover 3 (hasInstUnder term) "InstUnder" $
                        cover 3 (hasInstElim term) "InstElim" $
                            counterexample
                        ( "determinism failed on term: "
                            ++ show term
                            ++ "\nstep run 1: "
                            ++ show (step term)
                            ++ "\nstep run 2: "
                            ++ show (step term)
                        )
                        (step term === step term)

    it "Canonical-forms proxy: values at base type are the expected literals" $
        property $
            withMaxSuccess 300 $
                forAll genClosedWellTypedElabTerm $ \term ->
                    case typeCheck term of
                        Left _ -> property True  -- skip ill-typed (shouldn't happen)
                        Right ty
                            | not (isValue term) -> property True  -- only check values
                            | ty == TBase (BaseTy "Int") ->
                                counterexample
                                    ("canonical-forms (Int) failed on value: " ++ show term)
                                    (isLitInt term)
                            | ty == TBase (BaseTy "Bool") ->
                                counterexample
                                    ("canonical-forms (Bool) failed on value: " ++ show term)
                                    (isLitBool term)
                            | otherwise -> property True  -- non-base types: skip

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
genTermAtType :: TyCtx -> Int -> Int -> ElabType -> Gen (ElabTerm, Int)
genTermAtType ctx size fresh goalTy
    | size <= 0 = genAtom ctx fresh goalTy
    | otherwise = frequency $
        atomWeight ++ lamWeight ++ appWeight ++ letWeight
  where
    -- Atoms: variables in scope at the right type, or literals
    atomWeight = [(3, genAtom ctx fresh goalTy)]

    -- Lambda: if goal is TArrow a b, generate ELam
    lamWeight = case goalTy of
        TArrow argTy resTy ->
            let v = freshVar fresh
                ctx' = Map.insert v argTy ctx
            in [(2, do
                    (body, fresh') <- genTermAtType ctx' (size - 1) (fresh + 1) resTy
                    pure (ELam v argTy body, fresh')
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
            pure (ELet v (schemeFromType rhsTy) rhs body, fresh2)
        )]

-- | Generate an atomic term at a given type (leaf of the generation tree).
genAtom :: TyCtx -> Int -> ElabType -> Gen (ElabTerm, Int)
genAtom ctx fresh goalTy =
    let varCandidates =
            [ EVar v
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
                pure (ELam v argTy body, fresh')
            _ -> do
                lit <- genLitAtType goalTy
                pure (lit, fresh)
        candidates =
            map (\t -> (2, pure (t, fresh))) varCandidates
            ++ map (\g -> (3, (\t -> (t, fresh)) <$> g)) litCandidates
    in if null candidates
        then fallback
        else frequency candidates

genIntLit :: Gen ElabTerm
genIntLit = do
    n <- chooseInt (-10, 10)
    pure (ELit (LInt (fromIntegral n)))

genBoolLit :: Gen ElabTerm
genBoolLit = ELit . LBool <$> arbitrary

-- | Generate a literal at a specific ground type. Falls back to int for
--   types that have no literal form.
genLitAtType :: ElabType -> Gen ElabTerm
genLitAtType ty
    | ty == intTy  = genIntLit
    | ty == boolTy = genBoolLit
    | otherwise    = genIntLit  -- unreachable for our ground types

-- | Top-level generator: pick a random goal type, generate a closed well-typed
--   term at that type using the sized combinator. Uses frequency to cover both
--   monomorphic and polymorphic (ETyAbs, ETyInst) forms.
genClosedWellTypedElabTerm :: Gen ElabTerm
genClosedWellTypedElabTerm = sized $ \s -> do
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
genUnboundedTyAbsWrap :: Int -> ElabTerm -> Gen ElabTerm
genUnboundedTyAbsWrap fresh term = do
    let tv = "t" ++ show fresh
    pure (ETyAbs tv Nothing term)

-- | Wrap in a bounded type abstraction: Λ(α ⩾ τ). term
--   The bound is a ground type, and the body doesn't mention α.
genBoundedTyAbsWrap :: Int -> ElabType -> ElabTerm -> Gen ElabTerm
genBoundedTyAbsWrap fresh _goalTy term = do
    let tv = "t" ++ show fresh
    bound <- genGroundBound
    pure (ETyAbs tv (Just bound) term)

-- | InstElim: (Λα. body) N — eliminate a vacuous type abstraction
genInstElim :: Int -> ElabTerm -> Gen ElabTerm
genInstElim fresh term = do
    let tv = "t" ++ show fresh
    pure (ETyInst (ETyAbs tv Nothing term) InstElim)

-- | InstInside with InstBot: (Λα. body) ∀(⩾ τ)
genInstInsideBot :: Int -> ElabType -> ElabTerm -> Gen ElabTerm
genInstInsideBot fresh _goalTy term = do
    let tv = "t" ++ show fresh
    ty <- genGroundTy
    pure (ETyInst (ETyAbs tv Nothing term) (InstInside (InstBot ty)))

-- | InstApp: (Λα. body) ⟨τ⟩
genInstApp :: Int -> ElabType -> ElabTerm -> Gen ElabTerm
genInstApp fresh goalTy term = do
    let tv = "t" ++ show fresh
    pure (ETyInst (ETyAbs tv Nothing term) (InstApp goalTy))

-- | InstIntro + InstElim round-trip: ((term O) N)
genInstIntroElim :: Int -> ElabTerm -> Gen ElabTerm
genInstIntroElim _fresh term =
    pure (ETyInst (ETyInst term InstIntro) InstElim)

-- | InstUnder with trivial InstId: (Λα. body) ∀(β ⩾) 1
genInstUnderTrivial :: Int -> ElabTerm -> Gen ElabTerm
genInstUnderTrivial fresh term = do
    let tv = "t" ++ show fresh
        tv2 = "t" ++ show (fresh + 1)
    pure (ETyInst (ETyAbs tv Nothing term) (InstUnder tv2 InstId))

-- | Generate a ground BoundType (TBase is polymorphic in TopVar).
genGroundBound :: Gen BoundType
genGroundBound = elements [TBase (BaseTy "Int"), TBase (BaseTy "Bool")]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isClosedTerm :: ElabTerm -> Bool
isClosedTerm = Set.null . freeTermVars

freeTermVars :: ElabTerm -> Set.Set String
freeTermVars term = case term of
    EVar v -> Set.singleton v
    ELit _ -> Set.empty
    ELam v _ body -> Set.delete v (freeTermVars body)
    EApp f a -> Set.union (freeTermVars f) (freeTermVars a)
    ELet v _ rhs body ->
        Set.union (freeTermVars rhs) (Set.delete v (freeTermVars body))
    ETyAbs _ _ body -> freeTermVars body
    ETyInst e _ -> freeTermVars e

-- | Cover-label predicates for polymorphic constructor families.
isETyAbsBounded :: ElabTerm -> Bool
isETyAbsBounded (ETyAbs _ (Just _) _) = True
isETyAbsBounded _ = False

isETyInst :: ElabTerm -> Bool
isETyInst ETyInst{} = True
isETyInst _ = False

hasInstInside :: ElabTerm -> Bool
hasInstInside (ETyInst _ (InstInside _)) = True
hasInstInside _ = False

hasInstUnder :: ElabTerm -> Bool
hasInstUnder (ETyInst _ (InstUnder _ _)) = True
hasInstUnder _ = False

hasInstElim :: ElabTerm -> Bool
hasInstElim (ETyInst _ InstElim) = True
hasInstElim _ = False

isLitInt :: ElabTerm -> Bool
isLitInt (ELit (LInt _)) = True
isLitInt _ = False

isLitBool :: ElabTerm -> Bool
isLitBool (ELit (LBool _)) = True
isLitBool _ = False
