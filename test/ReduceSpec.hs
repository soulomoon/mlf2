{-# LANGUAGE GADTs #-}
module ReduceSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline (ElabTerm(..), Ty(..), Instantiation(..), normalize, schemeFromType, step, typeCheck)
import MLF.Frontend.Syntax (Lit(..))
import SpecUtil (requireRight)

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
