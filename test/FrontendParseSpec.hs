module FrontendParseSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Test.Hspec

import MLF.API
    ( BaseTy (..)
    , Expr (..)
    , NormSrcType
    , NormParseError (..)
    , SrcTy (..)
    , SrcType
    , Ty (..)
    , mkNormBound
    , mkSrcBound
    , parseNormEmlfExpr
    , parseNormEmlfType
    , parseRawEmlfExpr
    , parseRawEmlfType
    , renderEmlfParseError
    , renderNormParseError
    , renderPipelineError
    , runPipelineElab
    )

spec :: Spec
spec = describe "Frontend eMLF parser" $ do
    describe "raw expressions" $ do
        it "parses variables" $
            parseRawEmlfExpr "x" `shouldBe` Right (EVar "x")

        it "parses lambda and application with precedence" $
            parseRawEmlfExpr "λ(x) x y" `shouldBe` Right (ELam "x" (EApp (EVar "x") (EVar "y")))

        it "parses annotated lambda" $ do
            let ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            parseRawEmlfExpr "λ(x : ∀a. a -> a) x" `shouldBe` Right (ELamAnn "x" ty (EVar "x"))

        it "parses let-expression" $
            parseRawEmlfExpr "let id = λ(x) x in id" `shouldBe` Right (ELet "id" (ELam "x" (EVar "x")) (EVar "id"))

        it "parses annotation expression" $
            parseRawEmlfExpr "(x : Int)" `shouldBe` Right (EAnn (EVar "x") (STBase "Int"))

        it "parses typed let extension and desugars to annotation" $
            parseRawEmlfExpr "let x : Int = y in x"
                `shouldBe` Right (ELet "x" (EAnn (EVar "y") (STBase "Int")) (EVar "x"))

        it "rejects malformed let syntax" $
            parseRawEmlfExpr "let x = in x" `shouldSatisfy` isLeft

    describe "raw types" $ do
        it "parses raw forall binder and keeps raw alias type" $
            parseRawEmlfType "forall a. a -> a"
                `shouldBe` Right (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))

        it "parses arrow types as right associative" $
            parseRawEmlfType "a -> b -> c"
                `shouldBe` Right (STArrow (STVar "a") (STArrow (STVar "b") (STVar "c")))

        it "parses bounded and unbounded foralls" $
            parseRawEmlfType "∀a (b ⩾ Int). a -> b"
                `shouldBe` Right
                    ( STForall "a" Nothing
                        (STForall "b" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "b")))
                    )

        it "parses constructor application" $
            parseRawEmlfType "List Int" `shouldBe` Right (STCon "List" (STBase "Int" :| []))

        it "rejects malformed forall binders" $
            parseRawEmlfType "∀(a) a" `shouldSatisfy` isLeft

    describe "normalized types" $ do
        it "normalizes simple variable type" $
            parseNormEmlfType "a" `shouldBe` Right (STVar "a")

        it "normalizes arrow type" $
            parseNormEmlfType "a -> b"
                `shouldBe` Right (STArrow (STVar "a") (STVar "b"))

        it "normalizes unbounded forall" $
            parseNormEmlfType "∀a. a -> a"
                `shouldBe` Right (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))

        it "normalizes structural bound to StructBound" $
            parseNormEmlfType "∀(a ⩾ Int). a"
                `shouldBe` Right (STForall "a" (Just (mkNormBound (STBase "Int"))) (STVar "a"))

        it "inlines alias bound during normalization" $
            parseNormEmlfType "∀(b ⩾ a). b"
                `shouldBe` Right (STVar "a")

        it "rejects self-bound forall" $
            parseNormEmlfType "∀(a ⩾ a). a" `shouldSatisfy` isNormErr

        it "normalizes base type" $
            parseNormEmlfType "Int" `shouldBe` Right (STBase "Int")

        it "normalizes constructor application" $
            parseNormEmlfType "List Int"
                `shouldBe` Right (STCon "List" (STBase "Int" :| []))

    describe "normalized expressions" $ do
        it "normalizes unannotated expression unchanged" $
            parseNormEmlfExpr "x" `shouldBe` Right (EVar "x")

        it "normalizes annotation type in expression" $
            parseNormEmlfExpr "(x : Int)"
                `shouldBe` Right (EAnn (EVar "x") (STBase "Int"))

        it "normalizes annotated lambda type" $
            parseNormEmlfExpr "λ(x : ∀a. a -> a) x"
                `shouldBe` Right
                    (ELamAnn "x"
                        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                        (EVar "x"))

        it "rejects expression with self-bound annotation" $
            parseNormEmlfExpr "(x : ∀(a ⩾ a). a)" `shouldSatisfy` isNormExprErr

        it "parseNormEmlfExpr output feeds runPipelineElab normalized-only API" $ do
            let input = "let id = λ(x) x in let a = id 1 in id 2"
            case parseNormEmlfExpr input of
                Left err ->
                    expectationFailure ("parseNormEmlfExpr failed: " ++ renderNormParseError err)
                Right normExpr ->
                    case runPipelineElab Set.empty normExpr of
                        Left err ->
                            expectationFailure ("runPipelineElab failed: " ++ renderPipelineError err)
                        Right (_term, ty) ->
                            ty `shouldBe` TBase (BaseTy "Int")

    describe "API exports" $ do
        it "exports staged SrcTy aliases for raw and normalized paths" $ do
            let _raw :: SrcType
                _raw = STBase "Int"
                _norm :: NormSrcType
                _norm = STBase "Int"
            show _raw `shouldNotBe` ""
            show _norm `shouldNotBe` ""

    describe "error rendering" $ do
        it "renderEmlfParseError produces non-empty output" $
            case parseRawEmlfType "∀(a) a" of
                Left err -> renderEmlfParseError err `shouldSatisfy` (not . null)
                Right _ -> expectationFailure "expected parse error"

        it "renderNormParseError renders parse errors" $
            case parseNormEmlfType "∀(a) a" of
                Left err -> renderNormParseError err `shouldSatisfy` (not . null)
                Right _ -> expectationFailure "expected parse error"

        it "renderNormParseError renders normalization errors" $
            case parseNormEmlfType "∀(a ⩾ a). a" of
                Left err -> renderNormParseError err `shouldSatisfy` (not . null)
                Right _ -> expectationFailure "expected normalization error"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isNormErr :: Either NormParseError a -> Bool
isNormErr (Left (NormNormErr _)) = True
isNormErr _ = False

isNormExprErr :: Either NormParseError a -> Bool
isNormExprErr = isNormErr
