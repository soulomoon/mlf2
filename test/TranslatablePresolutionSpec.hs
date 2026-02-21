{-# LANGUAGE GADTs #-}
module TranslatablePresolutionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck
    ( Gen
    , counterexample
    , elements
    , forAll
    , frequency
    , property
    , sized
    , withMaxSuccess
    )

import MLF.Constraint.Presolution (PresolutionResult(..), validateTranslatablePresolution)
import MLF.Frontend.Syntax (Expr(..), Lit(..), SurfaceExpr)
import SpecUtil (runToPresolutionDefault)

spec :: Spec
spec = describe "Translatable presolution" $ do
    it "pipeline-generated presolutions satisfy Def. 15.2.10" $
        property $
            withMaxSuccess 200 $
                forAll genSurfaceExpr $ \expr ->
                    case runToPresolutionDefault Set.empty expr of
                        Left _ -> property True  -- skip constraint/presolution failures
                        Right pres ->
                            case validateTranslatablePresolution (prConstraint pres) of
                                Right () -> property True
                                Left err ->
                                    counterexample
                                        ("Def. 15.2.10 violation on: " ++ show expr
                                        ++ "\nerror: " ++ show err)
                                        False

    it "identity expression produces translatable presolution" $
        case runToPresolutionDefault Set.empty (ELam "x" (EVar "x")) of
            Left err -> expectationFailure $ "pipeline failed: " ++ err
            Right pres ->
                validateTranslatablePresolution (prConstraint pres)
                    `shouldBe` Right ()

-- | Generator: small surface expressions covering lambda, app, let, lit.
--   Variables drawn from a small fixed set to encourage binding hits.
genSurfaceExpr :: Gen SurfaceExpr
genSurfaceExpr = sized go
  where
    vars :: [String]
    vars = ["x", "y", "z"]

    go :: Int -> Gen SurfaceExpr
    go n
        | n <= 0 = frequency
            [ (3, ELit . LInt <$> elements [0..5])
            , (2, ELit . LBool <$> elements [True, False])
            , (2, EVar <$> elements vars)
            ]
        | otherwise =
            let half = n `div` 2
            in frequency
                [ (3, ELit . LInt <$> elements [0..5])
                , (2, EVar <$> elements vars)
                , (2, ELam <$> elements vars <*> go (n - 1))
                , (2, EApp <$> go half <*> go half)
                , (1, ELet <$> elements vars <*> go half <*> go half)
                ]
