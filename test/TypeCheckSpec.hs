module TypeCheckSpec (spec) where

import Test.Hspec

import MLF.Constraint.Types (BaseTy(..))
import MLF.Elab.Pipeline (ElabScheme(..), ElabTerm(..), ElabType(..), Instantiation(..), TypeCheckError(..), typeCheck)
import MLF.Frontend.Syntax (Lit(..))

spec :: Spec
spec = describe "Phase 7 typecheck" $ do
    let intTy = TBase (BaseTy "Int")

    it "reports unbound variables" $ do
        case typeCheck (EVar "x") of
            Left (TCUnboundVar "x") -> pure ()
            other -> expectationFailure ("Expected unbound variable error, got: " ++ show other)

    it "typechecks lambdas" $ do
        let term = ELam "x" intTy (EVar "x")
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "typechecks applications" $ do
        let term = EApp (ELam "x" intTy (EVar "x")) (ELit (LInt 1))
        typeCheck term `shouldBe` Right intTy

    it "typechecks let bindings" $ do
        let term = ELet "x" (Forall [] intTy) (ELit (LInt 1)) (EVar "x")
        typeCheck term `shouldBe` Right intTy

    it "typechecks type abstractions" $ do
        let term = ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))
        typeCheck term `shouldBe` Right (TForall "a" Nothing (TArrow (TVar "a") (TVar "a")))

    it "typechecks instantiations" $ do
        let term = ETyInst (ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))) (InstApp intTy)
        typeCheck term `shouldBe` Right (TArrow intTy intTy)

    it "reports instantiation errors" $ do
        case typeCheck (ETyInst (ELit (LInt 1)) InstElim) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected instantiation error, got: " ++ show other)
