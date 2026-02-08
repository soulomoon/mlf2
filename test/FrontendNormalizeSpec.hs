{-# LANGUAGE GADTs #-}
module FrontendNormalizeSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))

import MLF.Frontend.Syntax
import MLF.Frontend.Normalize

spec :: Spec
spec = describe "MLF.Frontend.Normalize" $ do
    freeVarsSpec
    substSpec
    normalizeTypeSpec
    normalizeExprSpec

-- -----------------------------------------------------------------------
-- Free variables
-- -----------------------------------------------------------------------

freeVarsSpec :: Spec
freeVarsSpec = describe "freeVarsSrcType" $ do
    it "variable is free" $
        freeVarsSrcType (STVar "a") `shouldBe` Set.fromList ["a"]

    it "base type has no free vars" $
        freeVarsSrcType (STBase "Int") `shouldBe` Set.empty

    it "arrow collects from both sides" $
        freeVarsSrcType (STArrow (STVar "a") (STVar "b"))
            `shouldBe` Set.fromList ["a", "b"]

    it "forall binds its variable in the body" $
        freeVarsSrcType (STForall "a" Nothing (STVar "a"))
            `shouldBe` Set.empty

    it "forall does not bind in the bound" $
        freeVarsSrcType (STForall "a" (Just (mkSrcBound (STVar "a"))) (STVar "b"))
            `shouldBe` Set.fromList ["a", "b"]

    it "nested foralls shadow correctly" $
        freeVarsSrcType
            (STForall "a" Nothing
                (STForall "b" (Just (mkSrcBound (STVar "a"))) (STVar "c")))
            `shouldBe` Set.fromList ["c"]

-- -----------------------------------------------------------------------
-- Capture-avoiding substitution
-- -----------------------------------------------------------------------

substSpec :: Spec
substSpec = describe "substSrcType" $ do
    it "substitutes a free variable" $
        substSrcType "a" (STBase "Int") (STVar "a")
            `shouldBe` STBase "Int"

    it "leaves other variables alone" $
        substSrcType "a" (STBase "Int") (STVar "b")
            `shouldBe` STVar "b"

    it "substitutes inside arrow" $
        substSrcType "a" (STBase "Int") (STArrow (STVar "a") (STVar "b"))
            `shouldBe` STArrow (STBase "Int") (STVar "b")

    it "does not substitute under shadowing binder" $
        substSrcType "a" (STBase "Int")
            (STForall "a" Nothing (STVar "a"))
            `shouldBe` STForall "a" Nothing (STVar "a")

    it "alpha-renames to avoid capture" $
        -- subst a -> b in forall b. a -> b
        let input = STForall "b" Nothing (STArrow (STVar "a") (STVar "b"))
            result = substSrcType "a" (STVar "b") input
        in case result of
            STForall v' Nothing (STArrow (STVar r) (STVar inner)) -> do
                v' `shouldNotBe` "b"
                r `shouldBe` "b"
                inner `shouldBe` v'
            other -> expectationFailure ("unexpected shape: " ++ show other)

    it "substitutes in forall bound" $
        substSrcType "a" (STBase "Int")
            (STForall "b" (Just (mkSrcBound (STVar "a"))) (STVar "b"))
            `shouldBe` STForall "b" (Just (mkSrcBound (STBase "Int"))) (STVar "b")

-- -----------------------------------------------------------------------
-- Type normalization
-- -----------------------------------------------------------------------

normalizeTypeSpec :: Spec
normalizeTypeSpec = describe "normalizeType" $ do
    it "normalizes a simple variable" $
        normalizeType (STVar "a")
            `shouldBe` Right (STVar "a")

    it "normalizes an arrow" $
        normalizeType (STArrow (STVar "a") (STBase "Int"))
            `shouldBe` Right (STArrow (STVar "a") (STBase "Int"))

    it "normalizes bottom" $
        normalizeType STBottom
            `shouldBe` Right STBottom

    it "normalizes unbounded forall" $
        normalizeType (STForall "a" Nothing (STVar "a"))
            `shouldBe` Right (STForall "a" Nothing (STVar "a"))

    it "normalizes structural bound to StructBound alias over SrcTy" $
        normalizeType (STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a"))
            `shouldBe` Right (STForall "a" (Just (mkNormBound (STBase "Int"))) (STVar "a"))

    it "inlines alias bound (forall (b >= a). b -> a -> a -> a)" $
        let input = STForall "b" (Just (mkSrcBound (STVar "a")))
                        (STArrow (STVar "b") (STVar "a"))
        in normalizeType input
            `shouldBe` Right (STArrow (STVar "a") (STVar "a"))

    it "inlines nested alias: forall a. forall (b >= a). a -> b -> a" $
        let input = STForall "a" Nothing
                        (STForall "b" (Just (mkSrcBound (STVar "a")))
                            (STArrow (STVar "a")
                                (STArrow (STVar "b") (STVar "a"))))
        in normalizeType input
            `shouldBe` Right (STForall "a" Nothing
                        (STArrow (STVar "a")
                            (STArrow (STVar "a") (STVar "a"))))

    it "rejects self-bound variable" $
        let input = STForall "a" (Just (mkSrcBound (STVar "a"))) (STVar "a")
        in normalizeType input
            `shouldBe` Left (SelfBoundVariable "a" (STVar "a"))

    it "handles alpha-capture during alias inlining" $
        let input = STForall "a" Nothing
                        (STForall "b" (Just (mkSrcBound (STVar "a")))
                            (STForall "a" Nothing
                                (STArrow (STVar "b") (STVar "a"))))
        in case normalizeType input of
            Right (STForall "a" Nothing
                    (STForall v' Nothing
                        (STArrow (STVar outer) (STVar inner)))) -> do
                outer `shouldBe` "a"
                inner `shouldBe` v'
                v' `shouldNotBe` "a"
            Right other -> expectationFailure ("unexpected normalized shape: " ++ show other)
            Left err -> expectationFailure ("unexpected error: " ++ show err)

    it "normalizes constructor types" $
        normalizeType (STCon "List" (STVar "a" :| []))
            `shouldBe` Right (STCon "List" (STVar "a" :| []))

    it "normalizes structural arrow bound" $
        normalizeType
            (STForall "a" (Just (mkSrcBound (STArrow (STBase "Int") (STBase "Bool"))))
                (STVar "a"))
            `shouldBe` Right
                (STForall "a"
                    (Just (mkNormBound (STArrow (STBase "Int") (STBase "Bool"))))
                    (STVar "a"))

-- -----------------------------------------------------------------------
-- Expression normalization
-- -----------------------------------------------------------------------

normalizeExprSpec :: Spec
normalizeExprSpec = describe "normalizeExpr" $ do
    it "normalizes annotation in EAnn" $
        let ty = STForall "b" (Just (mkSrcBound (STVar "a")))
                    (STArrow (STVar "b") (STVar "a"))
            expr = EAnn (EVar "x") ty
        in normalizeExpr expr
            `shouldBe` Right (EAnn (EVar "x")
                (STArrow (STVar "a") (STVar "a")))

    it "normalizes annotation in ELamAnn" $
        let ty = STForall "a" Nothing (STVar "a")
            expr = ELamAnn "x" ty (EVar "x")
        in normalizeExpr expr
            `shouldBe` Right (ELamAnn "x"
                (STForall "a" Nothing (STVar "a"))
                (EVar "x"))

    it "propagates error from annotation" $
        let ty = STForall "a" (Just (mkSrcBound (STVar "a"))) (STVar "a")
            expr = EAnn (EVar "x") ty
        in normalizeExpr expr
            `shouldBe` Left (SelfBoundVariable "a" (STVar "a"))

    it "normalizes nested expressions" $
        let expr = ELet "f"
                    (ELam "x" (EVar "x"))
                    (EApp (EVar "f") (ELit (LInt 42)))
        in normalizeExpr expr
            `shouldBe` Right (ELet "f"
                (ELam "x" (EVar "x"))
                (EApp (EVar "f") (ELit (LInt 42))))
