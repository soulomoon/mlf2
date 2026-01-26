module ReduceSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import MLF.Constraint.Types (BaseTy(..))
import MLF.Elab.Pipeline (ElabTerm(..), Ty(..), Instantiation(..), normalize, schemeFromType, step, typeCheck)
import MLF.Frontend.Syntax (Lit(..))
import SpecUtil (requireRight)

spec :: Spec
spec = do
    let intTy = TBase (BaseTy "Int")
        idLam = ELam "x" intTy (EVar "x")
        polyLam = ELam "x" (TVar "a") (EVar "x")

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
