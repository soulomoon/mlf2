module TypeCheckSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Elab.Pipeline
    ( ElabTerm(..)
    , Ty(..)
    , Instantiation(..)
    , TypeCheckError(..)
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , schemeFromType
    , typeCheck
    )
import MLF.Frontend.Syntax (Lit(..))
import qualified MLF.Frontend.Syntax as Surf (Expr(..), SrcTy(..))
import SpecUtil (mkForalls, unsafeNormalizeExpr)

spec :: Spec
spec = describe "Phase 7 typecheck" $ do
    let intTy = TBase (BaseTy "Int")

    describe "Formal obligations ledger anchors (Chapter 14 typing/instance)" $ do
        it "O14-WF-EMPTY O14-WF-TVAR O14-WF-VAR: environment well-formedness proxies" $ do
            typeCheck (ELit (LInt 0)) `shouldBe` Right intTy
            case typeCheck (ETyAbs "a" (Just (TArrow (TVar "a") intTy)) (ELit (LInt 1))) of
                Left (TCTypeAbsBoundMentionsVar "a") -> pure ()
                other -> expectationFailure ("Expected bound self-reference rejection, got: " ++ show other)
            case typeCheck (EVar "missing") of
                Left (TCUnboundVar "missing") -> pure ()
                other -> expectationFailure ("Expected unbound variable rejection, got: " ++ show other)

        it "O14-T-VAR O14-T-ABS O14-T-APP O14-T-TABS O14-T-TAPP O14-T-LET: typing-rule anchors" $ do
            typeCheck (ELam "x" intTy (EVar "x")) `shouldBe` Right (TArrow intTy intTy)
            typeCheck (EApp (ELam "x" intTy (EVar "x")) (ELit (LInt 1))) `shouldBe` Right intTy
            typeCheck
                (ELet "x" (schemeFromType intTy) (ELit (LInt 1)) (EVar "x"))
                `shouldBe` Right intTy
            let polyId = ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))
            typeCheck polyId `shouldBe` Right (TForall "a" Nothing (TArrow (TVar "a") (TVar "a")))
            typeCheck (ETyInst polyId (InstApp intTy)) `shouldBe` Right (TArrow intTy intTy)

        it "O14-INST-REFLEX O14-INST-TRANS O14-INST-BOT O14-INST-HYP O14-INST-INNER O14-INST-OUTER O14-INST-QUANT-ELIM O14-INST-QUANT-INTRO: instantiation-rule anchors" $ do
            let polyId = ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))
                boundArrow = TArrow TBottom TBottom
                polyOuterHyp =
                    ETyAbs "a" (Just boundArrow) (ELam "x" TBottom (EVar "x"))
            typeCheck (ETyInst (ELit (LInt 1)) InstId) `shouldBe` Right intTy
            typeCheck (ETyInst (ELit (LInt 1)) (InstSeq InstIntro InstElim)) `shouldBe` Right intTy
            typeCheck (ETyInst (ELit (LInt 1)) InstIntro) `shouldBe` Right (TForall "u0" Nothing intTy)
            typeCheck (ETyInst polyId InstElim) `shouldBe` Right (TArrow TBottom TBottom)
            typeCheck (ETyInst polyId (InstInside (InstBot intTy)))
                `shouldBe` Right (TForall "a" (Just intTy) (TArrow (TVar "a") (TVar "a")))
            typeCheck (ETyInst polyOuterHyp (InstUnder "x" (InstAbstr "x")))
                `shouldBe` Right (TForall "a" (Just boundArrow) (TVar "a"))
            case typeCheck (ETyInst (ELit (LInt 1)) (InstBot intTy)) of
                Left TCInstantiationError{} -> pure ()
                other -> expectationFailure ("Expected InstBot rejection on non-bottom term type, got: " ++ show other)

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

    -- BUG-004 strict InstBot regressions
    it "accepts InstInside(InstBot) updating unbounded forall's bound" $ do
        -- ∀(a ⩾ ⊥).a→a  with InstInside(InstBot Int) → ∀(a ⩾ Int).a→a
        let poly = ETyAbs "a" Nothing (ELam "x" (TVar "a") (EVar "x"))
        typeCheck (ETyInst poly (InstInside (InstBot intTy)))
            `shouldBe` Right (TForall "a" (Just intTy) (TArrow (TVar "a") (TVar "a")))

    it "rejects InstInside(InstBot) when bound is already non-bottom" $ do
        -- ∀(a ⩾ Int).a→a  with InstInside(InstBot Int) — bound is Int, not ⊥
        let poly = ETyAbs "a" (Just intTy) (ELam "x" (TVar "a") (EVar "x"))
        case typeCheck (ETyInst poly (InstInside (InstBot intTy))) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)

    it "rejects bare InstBot on non-bottom bound even when types match" $ do
        -- ∀(a:Int).a→a  with InstBot Int — the forall type itself is not ⊥
        let poly = ETyAbs "a" (Just intTy) (ELam "x" (TVar "a") (EVar "x"))
        case typeCheck (ETyInst poly (InstBot intTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)

    describe "A6 parity regressions (bounded/coercion-heavy)" $ do
        it "typeCheck agrees for unchecked vs checked bounded-alias coercion path" $ do
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

            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
                Right (uncheckedTerm, uncheckedTy) -> do
                    uncheckedTy `shouldSatisfy` isPolyBinaryId
                    case runPipelineElabChecked Set.empty normExpr of
                        Left errChecked -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError errChecked)
                        Right (checkedTerm, checkedTy) -> do
                            checkedTy `shouldSatisfy` isPolyBinaryId
                            uncheckedTy `shouldBe` checkedTy
                            typeCheck uncheckedTerm `shouldBe` Right checkedTy
                            typeCheck checkedTerm `shouldBe` Right checkedTy

        it "typeCheck agrees for dual annotated coercion consumers" $ do
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

            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
                Right (uncheckedTerm, uncheckedTy) -> do
                    uncheckedTy `shouldBe` expected
                    case runPipelineElabChecked Set.empty normExpr of
                        Left errChecked -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError errChecked)
                        Right (checkedTerm, checkedTy) -> do
                            checkedTy `shouldBe` expected
                            uncheckedTy `shouldBe` checkedTy
                            typeCheck uncheckedTerm `shouldBe` Right checkedTy
                            typeCheck checkedTerm `shouldBe` Right checkedTy
