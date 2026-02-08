module FrontendPrettySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import MLF.API
    ( Expr (..)
    , NormSrcType
    , SrcTy (..)
    , mkSrcBound
    , prettyEmlfExpr
    , prettyEmlfType
    , parseRawEmlfExpr
    , parseRawEmlfType
    )

spec :: Spec
spec = describe "Frontend eMLF pretty printer" $ do
    it "pretty-prints normalized staged types" $ do
        let ty :: NormSrcType
            ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
        prettyEmlfType ty `shouldBe` "∀a. a -> a"

    it "prints canonical annotated lambda syntax" $ do
        let ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            expr = ELamAnn "id" ty (EVar "id")
        prettyEmlfExpr expr `shouldBe` "λ(id : ∀a. a -> a) id"

    it "prints canonical let + annotation syntax" $ do
        let expr =
                ELet "id"
                    (ELam "x" (EVar "x"))
                    (EAnn (EVar "id") (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
        prettyEmlfExpr expr `shouldBe` "let id = λ(x) x in (id : ∀a. a -> a)"

    it "prints canonical bounded forall syntax for source types" $ do
        let ty = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
        prettyEmlfType ty `shouldBe` "∀(a ⩾ Int). a -> a"

    it "roundtrips expression parse(pretty(expr))" $ do
        let expr =
                ELet "f"
                    (ELamAnn "x" (STBase "Int") (EVar "x"))
                    (EApp (EVar "f") (EVar "f"))
        parseRawEmlfExpr (prettyEmlfExpr expr) `shouldBe` Right expr

    it "roundtrips type parse(pretty(type))" $ do
        let ty = STForall "a" Nothing (STCon "List" (STVar "a" :| []))
        parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty
