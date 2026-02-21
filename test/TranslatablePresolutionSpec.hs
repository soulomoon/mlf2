{-# LANGUAGE GADTs #-}
module TranslatablePresolutionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (PresolutionResult(..), validateTranslatablePresolution)
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import SpecUtil (runToPresolutionDefault)

spec :: Spec
spec = describe "Translatable presolution" $ do
    it "identity expression produces translatable presolution" $
        case runToPresolutionDefault Set.empty (ELam "x" (EVar "x")) of
            Left err -> expectationFailure $ "pipeline failed: " ++ err
            Right pres ->
                validateTranslatablePresolution (prConstraint pres)
                    `shouldBe` Right ()

    it "application expression produces translatable presolution" $
        case runToPresolutionDefault Set.empty
                (EApp (ELam "x" (EVar "x")) (ELit (LInt 1))) of
            Left err -> expectationFailure $ "pipeline failed: " ++ err
            Right pres ->
                validateTranslatablePresolution (prConstraint pres)
                    `shouldBe` Right ()

    it "let-polymorphic expression produces translatable presolution" $ do
        let expr = ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (ELit (LInt 1)))
        case runToPresolutionDefault Set.empty expr of
            Left err -> expectationFailure $ "pipeline failed: " ++ err
            Right pres ->
                validateTranslatablePresolution (prConstraint pres)
                    `shouldBe` Right ()
