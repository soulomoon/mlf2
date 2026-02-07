module FrontendParseSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec

import MLF.API
    ( Expr (..)
    , SrcType (..)
    , parseEmlfExpr
    , parseEmlfType
    )

spec :: Spec
spec = describe "Frontend eMLF parser" $ do
    describe "expressions" $ do
        it "parses variables" $
            parseEmlfExpr "x" `shouldBe` Right (EVar "x")

        it "parses lambda and application with precedence" $
            parseEmlfExpr "λ(x) x y" `shouldBe` Right (ELam "x" (EApp (EVar "x") (EVar "y")))

        it "parses annotated lambda" $ do
            let ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            parseEmlfExpr "λ(x : ∀a. a -> a) x" `shouldBe` Right (ELamAnn "x" ty (EVar "x"))

        it "parses let-expression" $
            parseEmlfExpr "let id = λ(x) x in id" `shouldBe` Right (ELet "id" (ELam "x" (EVar "x")) (EVar "id"))

        it "parses annotation expression" $
            parseEmlfExpr "(x : Int)" `shouldBe` Right (EAnn (EVar "x") (STBase "Int"))

        it "parses typed let extension and desugars to annotation" $
            parseEmlfExpr "let x : Int = y in x"
                `shouldBe` Right (ELet "x" (EAnn (EVar "y") (STBase "Int")) (EVar "x"))

        it "rejects malformed let syntax" $
            parseEmlfExpr "let x = in x" `shouldSatisfy` isLeft

    describe "types" $ do
        it "parses arrow types as right associative" $
            parseEmlfType "a -> b -> c"
                `shouldBe` Right (STArrow (STVar "a") (STArrow (STVar "b") (STVar "c")))

        it "parses bounded and unbounded foralls" $
            parseEmlfType "∀a (b ⩾ Int). a -> b"
                `shouldBe` Right
                    ( STForall "a" Nothing
                        (STForall "b" (Just (STBase "Int")) (STArrow (STVar "a") (STVar "b")))
                    )

        it "parses constructor application" $
            parseEmlfType "List Int" `shouldBe` Right (STCon "List" (STBase "Int" :| []))

        it "rejects malformed forall binders" $
            parseEmlfType "∀(a) a" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
