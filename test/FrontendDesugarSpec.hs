module FrontendDesugarSpec (spec) where

import Test.Hspec

import MLF.Frontend.Desugar (desugarSurface)
import MLF.Frontend.Syntax

spec :: Spec
spec = describe "MLF.Frontend.Desugar" $ do
    it "desugars annotated terms to coercion application" $ do
        let ty = STBase "Int"
            expr = EAnn (EVar "x") ty
        desugarSurface expr
            `shouldBe` EApp (ECoerceConst ty) (EVar "x")

    it "desugars annotated lambdas to lambda plus let-bound coercion" $ do
        let ty = STBase "Int"
            expr = ELamAnn "x" ty (EVar "x")
        desugarSurface expr
            `shouldBe`
                ELam "x"
                    (ELet "x"
                        (EApp (ECoerceConst ty) (EVar "x"))
                        (EVar "x"))

    it "recurses structurally through let, lambda, and application" $ do
        let intTy = STBase "Int"
            boolTy = STBase "Bool"
            expr =
                ELet "f"
                    (ELamAnn "x" intTy (EVar "x"))
                    (EApp (EVar "f") (EAnn (EVar "y") boolTy))
        desugarSurface expr
            `shouldBe`
                ELet "f"
                    (ELam "x"
                        (ELet "x"
                            (EApp (ECoerceConst intTy) (EVar "x"))
                            (EVar "x")))
                    (EApp (EVar "f") (EApp (ECoerceConst boolTy) (EVar "y")))

    it "keeps typed-let sugar coercion-only on the RHS" $ do
        let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            rhs = EAnn (ELam "x" (EVar "x")) ann
            expr = ELet "id" rhs (EVar "id")
        desugarSurface expr
            `shouldBe`
                ELet "id"
                    (EApp (ECoerceConst ann) (ELam "x" (EVar "x")))
                    (EVar "id")
