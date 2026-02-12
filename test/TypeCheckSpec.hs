module TypeCheckSpec (spec) where

import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline (ElabTerm(..), Ty(..), Instantiation(..), TypeCheckError(..), schemeFromType, typeCheck)
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
        let term = ELet "x" (schemeFromType intTy) (ELit (LInt 1)) (EVar "x")
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

    it "rejects InstApp that violates an explicit bound" $ do
        let boolTy = TBase (BaseTy "Bool")
            term =
                ETyInst
                    (ETyAbs "a" (Just intTy) (ELam "x" (TVar "a") (EVar "x")))
                    (InstApp boolTy)
        case typeCheck term of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected bounded instantiation error, got: " ++ show other)

    it "preserves type-variable InstApp arguments" $ do
        let idTyAbs = ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))
            term = ETyAbs "b" Nothing (ETyInst idTyAbs (InstApp (TVar "b")))
        typeCheck term `shouldBe` Right (TForall "b" Nothing (TArrow (TVar "b") (TVar "b")))

    it "rejects InstBot on alpha-equal non-bottom type (checker strictness)" $ do
        let poly = ETyAbs "a" (Just intTy) (ELam "x" (TVar "a") (EVar "x"))
            polyTy = TForall "a" (Just intTy) (TArrow (TVar "a") (TVar "a"))
        case typeCheck (ETyInst poly (InstBot polyTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)
