module GeneralizeSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Generalize (shadowCompareTypes)
import MLF.Elab.Pipeline (ElabError(..), Ty(..))

spec :: Spec
spec = describe "Generalize shadow comparator" $ do
    it "accepts alpha-equivalent types" $ do
        let solvedTy = TForall "a" Nothing (TVar "a")
            baseTy = TForall "b" Nothing (TVar "b")
        shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

    it "rejects semantic mismatch with shadow reify mismatch diagnostics" $ do
        let solvedTy = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
            baseTy = TForall "a" Nothing (TArrow (TVar "a") (TBase (BaseTy "Int")))
        case shadowCompareTypes "ctx" solvedTy baseTy of
            Left (ValidationFailed msgs) ->
                msgs `shouldSatisfy` any (isInfixOf "shadow reify mismatch")
            other ->
                expectationFailure ("Expected ValidationFailed shadow mismatch, got: " ++ show other)
