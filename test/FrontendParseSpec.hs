module FrontendParseSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Test.Hspec

import MLF.API
    ( Expr (..)
    , NormSrcType
    , NormParseError (..)
    , SrcTy (..)
    , SrcType
    , mkNormBound
    , mkSrcBound
    , parseNormEmlfExpr
    , parseNormEmlfType
    , parseRawEmlfExpr
    , parseRawEmlfType
    , renderEmlfParseError
    , renderNormParseError
    )
import MLF.Pipeline (BaseTy (..), Ty (..), renderPipelineError, runPipelineElab)

spec :: Spec
spec = describe "Frontend eMLF parser" $ do
    describe "parser scaffolding dedup guards" $ do
        it "frontend and XMLF parsers share lexer/type scaffolding modules" $ do
            frontendSrc <- readFile "src/MLF/Frontend/Parse.hs"
            xmlfSrc <- readFile "src/MLF/XMLF/Parse.hs"
            frontendSrc `shouldSatisfy` isInfixOf "MLF.Parse.Common"
            frontendSrc `shouldSatisfy` isInfixOf "MLF.Parse.Type"
            xmlfSrc `shouldSatisfy` isInfixOf "MLF.Parse.Common"
            xmlfSrc `shouldSatisfy` isInfixOf "MLF.Parse.Type"
            forM_
                [ "sc :: Parser ()"
                , "lexeme :: Parser a -> Parser a"
                , "symbol :: String -> Parser String"
                , "parens :: Parser a -> Parser a"
                , "identifier :: Parser String"
                , "lowerIdent :: Parser String"
                , "upperIdent :: Parser String"
                , "forallTok :: Parser ()"
                , "lambdaTok :: Parser ()"
                , "geTok :: Parser ()"
                , "bottomTok :: Parser ()"
                , "pForallType ::"
                , "pForallBinder ::"
                , "pArrowType ::"
                , "pTypeApp ::"
                , "pTypeArg ::"
                , "pTypeAtom ::"
                , "pLit :: Parser Lit"
                , "pString :: Parser String"
                ] $ \marker -> do
                    frontendSrc `shouldSatisfy` (not . isInfixOf marker)
                    xmlfSrc `shouldSatisfy` (not . isInfixOf marker)
    describe "raw expressions" $ do
        it "parses variables" $
            parseRawEmlfExpr "x" `shouldBe` Right (EVar "x")

        it "parses lambda and application with precedence" $
            parseRawEmlfExpr "λ(x) x y" `shouldBe` Right (ELam "x" (EApp (EVar "x") (EVar "y")))

        it "parses annotated lambda" $ do
            let ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            parseRawEmlfExpr "λ(x : ∀a. a -> a) x" `shouldBe` Right (ELamAnn "x" ty (EVar "x"))

        it "parses recursive annotations with ascii mu syntax" $ do
            let ty = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            parseRawEmlfExpr "λ(x : mu a. a -> Int) x"
                `shouldBe` Right (ELamAnn "x" ty (EVar "x"))

        it "parses let-expression" $
            parseRawEmlfExpr "let id = λ(x) x in id" `shouldBe` Right (ELet "id" (ELam "x" (EVar "x")) (EVar "id"))

        it "parses annotation expression" $
            parseRawEmlfExpr "(x : Int)" `shouldBe` Right (EAnn (EVar "x") (STBase "Int"))

        it "parses typed let extension and desugars to annotation" $
            parseRawEmlfExpr "let x : Int = y in x"
                `shouldBe` Right (ELet "x" (EAnn (EVar "y") (STBase "Int")) (EVar "x"))

        it "rejects malformed let syntax" $
            parseRawEmlfExpr "let x = in x" `shouldSatisfy` isLeft

        it "treats mu as a reserved frontend keyword" $
            parseRawEmlfExpr "let mu = x in mu" `shouldSatisfy` isLeft

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

        it "parses unicode mu recursive types" $
            parseRawEmlfType "μa. List a"
                `shouldBe` Right (STMu "a" (STCon "List" (STVar "a" :| [])))

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

        it "normalizes alias bounds nested under recursive wrappers" $
            parseNormEmlfType "μa. ∀(b ⩾ a). b"
                `shouldBe` Right (STMu "a" (STVar "a"))

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

        it "normalizes recursive annotations in expressions" $
            parseNormEmlfExpr "(x : μa. ∀(b ⩾ a). b)"
                `shouldBe` Right (EAnn (EVar "x") (STMu "a" (STVar "a")))

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
