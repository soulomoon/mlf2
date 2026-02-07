module GeneralizeSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Phi.TestOnly
    ( selectSolvedOrderWithShadowTestOnly
    , shadowCompareTypesTestOnly
    )
import MLF.Elab.Pipeline (ElabError(..), Ty(..))

spec :: Spec
spec = do
    describe "Generalize shadow comparator" $ do
        it "accepts alpha-equivalent types" $ do
            let solvedTy = TForall "a" Nothing (TVar "a")
                baseTy = TForall "b" Nothing (TVar "b")
            shadowCompareTypesTestOnly "ctx" solvedTy baseTy `shouldBe` Right ()

        it "rejects semantic mismatch with shadow reify mismatch diagnostics" $ do
            let solvedTy = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
                baseTy = TForall "a" Nothing (TArrow (TVar "a") (TBase (BaseTy "Int")))
            case shadowCompareTypesTestOnly "ctx" solvedTy baseTy of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)

    describe "selectSolvedOrderWithShadow" $ do
        it "returns solved type when solved/base shadow comparison succeeds" $ do
            let solvedTy = TForall "a" Nothing (TVar "a")
                baseTy = TForall "b" Nothing (TVar "b")
            selectSolvedOrderWithShadowTestOnly "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

        it "fails hard on solved/base shadow mismatch when base shadow is present" $ do
            let solvedTy = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
                baseTy = TForall "a" Nothing (TArrow (TVar "a") (TBase (BaseTy "Int")))
            case selectSolvedOrderWithShadowTestOnly "ctx" solvedTy (Just baseTy) of
                Left (ValidationFailed msgs) ->
                    msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
                other ->
                    expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)
