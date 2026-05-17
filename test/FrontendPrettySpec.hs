module FrontendPrettySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import MLF.API
    ( Expr (..)
    , NormSrcType
    , SrcTy (..)
    , SrcType
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

    it "prints recursive types canonically with unicode mu" $ do
        let ty = STMu "a" (STArrow (STVar "a") (STBase "Int"))
        prettyEmlfType ty `shouldBe` "μa. a -> Int"

    it "prints canonical recursive annotation syntax" $ do
        let expr = EAnn (EVar "x") (STMu "a" (STCon "List" (STVar "a" :| [])))
        prettyEmlfExpr expr `shouldBe` "(x : μa. List a)"

    it "pretty-prints variable-headed type application ASTs" $ do
        let unary :: SrcType
            unary = STVarApp "f" (STVar "a" :| [])
            binary :: SrcType
            binary = STVarApp "p" (STVar "a" :| [STVar "b"])
        prettyEmlfType unary `shouldBe` "f a"
        prettyEmlfType binary `shouldBe` "p a b"

    it "pretty-prints type lambdas and explicit type application" $ do
        prettyEmlfType (STTyLam "a" (STArrow (STVar "a") (STVar "a")))
            `shouldBe` "Λa. a -> a"
        prettyEmlfType (STTyApp (STTyLam "a" (STVar "a")) (STBase "Int"))
            `shouldBe` "(Λa. a) Int"

    it "roundtrips expression parse(pretty(expr))" $ do
        let expr =
                ELet "f"
                    (ELamAnn "x" (STBase "Int") (EVar "x"))
                    (EApp (EVar "f") (EVar "f"))
        parseRawEmlfExpr (prettyEmlfExpr expr) `shouldBe` Right expr

    it "roundtrips type parse(pretty(type))" $ do
        let ty = STForall "a" Nothing (STCon "List" (STVar "a" :| []))
        parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

    it "roundtrips recursive type parse(pretty(type))" $ do
        let ty = STMu "a" (STCon "List" (STVar "a" :| []))
        parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

    it "roundtrips variable-headed type application parse(pretty(type))" $ do
        let ty =
                STVarApp
                    "f"
                    ( STVarApp "g" (STVar "a" :| [])
                        :| [STArrow (STVar "a") (STVar "b")]
                    )
        parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

    it "roundtrips type-lambda application parse(pretty(type))" $ do
        let ty = STTyApp (STTyLam "a" (STArrow (STVar "a") (STVar "a"))) (STBase "Int")
        parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

    it "roundtrips recursive expression parse(pretty(expr))" $ do
        let expr = EAnn (EVar "x") (STMu "a" (STArrow (STVar "a") (STBase "Int")))
        parseRawEmlfExpr (prettyEmlfExpr expr) `shouldBe` Right expr
