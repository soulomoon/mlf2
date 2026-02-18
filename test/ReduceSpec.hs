{-# LANGUAGE GADTs #-}
module ReduceSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline
    ( ElabTerm(..)
    , Ty(..)
    , Instantiation(..)
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , schemeFromType
    , step
    , typeCheck
    )
import MLF.Frontend.Syntax (Lit(..))
import qualified MLF.Frontend.Syntax as Surf (Expr(..), SrcTy(..))
import SpecUtil (mkForalls, requireRight, unsafeNormalizeExpr)

spec :: Spec
spec = do
    let intTy = TBase (BaseTy "Int")
        idLam = ELam "x" intTy (EVar "x")
        polyLam = ELam "x" (TVar "a") (EVar "x")
        boundFromType ty = case ty of
            TVar v -> error ("boundFromType: unexpected variable bound " ++ show v)
            TArrow a b -> TArrow a b
            TCon c args -> TCon c args
            TBase b -> TBase b
            TBottom -> TBottom
            TForall v mb body -> TForall v mb body

    describe "Phase 7 reduce" $ do
        it "beta-reduces lambda applications" $ do
            let term = EApp idLam (ELit (LInt 1))
            step term `shouldBe` Just (ELit (LInt 1))

        it "reduces let bindings when the rhs is a value" $ do
            let term = ELet "x" (schemeFromType intTy) (ELit (LInt 1)) (EVar "x")
            step term `shouldBe` Just (ELit (LInt 1))

        it "reduces instantiation elimination" $ do
            let term = ETyInst (ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))) InstElim
            step term `shouldBe` Just (ELam "x" TBottom (EVar "x"))

        it "reduces instantiation introduction (O) to type abstraction" $ do
            let term = ETyInst (ELit (LInt 1)) InstIntro
            step term `shouldBe` Just (ETyAbs "u0" Nothing (ELit (LInt 1)))

        it "reduces instantiation trans to nested instantiations" $ do
            let inst = InstSeq InstIntro InstElim
                term = ETyInst (ELit (LInt 1)) inst
            step term `shouldBe` Just (ETyInst (ETyInst (ELit (LInt 1)) InstIntro) InstElim)

        it "reduces instantiation under (∀(α ⩾) φ) by pushing under type abstraction" $ do
            let body = ELam "x" (TVar "a") (EVar "x")
                term = ETyInst (ETyAbs "a" Nothing body) (InstUnder "b" (InstApp intTy))
            step term `shouldBe` Just (ETyAbs "a" Nothing (ETyInst body (InstApp intTy)))

        it "reduces instantiation inside (∀(⩾ φ)) by rewriting the bound" $ do
            let body = EVar "x"
                term = ETyInst (ETyAbs "a" Nothing body) (InstInside (InstBot intTy))
            step term `shouldBe` Just (ETyAbs "a" (Just (boundFromType intTy)) body)

        it "reduces instantiation application (⟨τ⟩) to inside+elim" $ do
            let body = ELam "x" (TVar "a") (EVar "x")
                term = ETyInst (ETyAbs "a" Nothing body) (InstApp intTy)
                expectedInst = InstSeq (InstInside (InstBot intTy)) InstElim
            step term `shouldBe` Just (ETyInst (ETyAbs "a" Nothing body) expectedInst)

        it "steps inside argument position under call-by-value" $ do
            let inner = EApp (ELam "y" intTy (EVar "y")) (ELit (LInt 1))
                term = EApp idLam inner
            step term `shouldBe` Just (EApp idLam (ELit (LInt 1)))

    describe "Phase 7 preservation (sanity)" $ do
        it "preserves types across normalization for a fixed set of terms" $ do
            let term1 = EApp idLam (ELit (LInt 1))
                term2 = ELet "x" (schemeFromType intTy) (ELit (LInt 1)) (EApp idLam (EVar "x"))
                term3 = ETyInst (ETyAbs "a" Nothing polyLam) InstElim
                terms = [term1, term2, term3]
            forM_ terms $ \term -> do
                ty <- requireRight (typeCheck term)
                let term' = normalize term
                ty' <- requireRight (typeCheck term')
                ty' `shouldBe` ty

    describe "Phase 7 bounded/coercion-heavy parity regressions (A6)" $ do
        it "normalization preserves parity for bounded-alias coercion path" $ do
            let rhs = Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (Surf.STVar "a"))
                        ]
                        (Surf.STArrow (Surf.STVar "a") (Surf.STArrow (Surf.STVar "b") (Surf.STVar "a")))
                ann =
                    Surf.STForall "a" Nothing
                        (Surf.STArrow (Surf.STVar "a") (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a")))
                expr =
                    Surf.ELet "c" (Surf.EAnn rhs schemeTy)
                        (Surf.EAnn (Surf.EVar "c") ann)
                normExpr = unsafeNormalizeExpr expr
                isPolyBinaryId ty =
                    case ty of
                        TForall v Nothing (TArrow dom (TArrow dom' cod)) ->
                            dom == TVar v && dom' == TVar v && cod == TVar v
                        _ -> False

            uncheckedRes <- case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err) >> fail "unchecked pipeline failed"
                Right out -> pure out
            checkedRes <- case runPipelineElabChecked Set.empty normExpr of
                Left err -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError err) >> fail "checked pipeline failed"
                Right out -> pure out

            let (uncheckedTerm, uncheckedTy) = uncheckedRes
                (checkedTerm, checkedTy) = checkedRes
                uncheckedNorm = normalize uncheckedTerm
                checkedNorm = normalize checkedTerm

            uncheckedTy `shouldSatisfy` isPolyBinaryId
            checkedTy `shouldSatisfy` isPolyBinaryId
            uncheckedTy `shouldBe` checkedTy
            uncheckedNormTy <- requireRight (typeCheck uncheckedNorm)
            checkedNormTy <- requireRight (typeCheck checkedNorm)
            uncheckedNormTy `shouldSatisfy` isPolyBinaryId
            checkedNormTy `shouldSatisfy` isPolyBinaryId

        it "normalization preserves parity for dual annotated coercion consumers" $ do
            let useInt =
                    Surf.ELamAnn "f" (Surf.STArrow (Surf.STBase "Int") (Surf.STBase "Int"))
                        (Surf.EApp (Surf.EVar "f") (Surf.ELit (LInt 0)))
                useBool =
                    Surf.ELamAnn "f" (Surf.STArrow (Surf.STBase "Bool") (Surf.STBase "Bool"))
                        (Surf.EApp (Surf.EVar "f") (Surf.ELit (LBool True)))
                expr =
                    Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x"))
                        (Surf.ELet "useI" useInt
                            (Surf.ELet "useB" useBool
                                (Surf.ELet "_" (Surf.EApp (Surf.EVar "useI") (Surf.EVar "id"))
                                    (Surf.EApp (Surf.EVar "useB") (Surf.EVar "id")))))
                expected = TBase (BaseTy "Bool")
                normExpr = unsafeNormalizeExpr expr

            uncheckedRes <- case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err) >> fail "unchecked pipeline failed"
                Right out -> pure out
            checkedRes <- case runPipelineElabChecked Set.empty normExpr of
                Left err -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError err) >> fail "checked pipeline failed"
                Right out -> pure out

            let (uncheckedTerm, uncheckedTy) = uncheckedRes
                (checkedTerm, checkedTy) = checkedRes
                uncheckedNorm = normalize uncheckedTerm
                checkedNorm = normalize checkedTerm

            uncheckedTy `shouldBe` expected
            checkedTy `shouldBe` expected
            uncheckedTy `shouldBe` checkedTy
            typeCheck uncheckedNorm `shouldBe` Right expected
            typeCheck checkedNorm `shouldBe` Right expected
