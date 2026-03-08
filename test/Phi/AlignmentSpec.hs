module Phi.AlignmentSpec (spec) where

import Control.Monad (forM_, when)
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..))
import MLF.Constraint.Types.Graph (typeRef)
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax (Expr(..), SrcTy(..), Lit(..))
import qualified MLF.Binding.Tree as Binding
import SpecUtil (unsafeNormalizeExpr, runPipelineArtifactsDefault, PipelineArtifacts(..), mkForalls)

spec :: Spec
spec = describe "Phi alignment" $ do
    describe "C1: witness-driven Phi produces valid instantiations" $ do
        let corpus =
                [ ("let-poly"
                  , ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                  )
                , ("ann-id"
                  , EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                  )
                ]
        forM_ corpus $ \(label, expr) ->
            it ("pipeline succeeds for: " ++ label) $ do
                let result = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                case result of
                    Left err -> expectationFailure (show err)
                    Right (term, ty) -> do
                        show term `shouldNotBe` ""
                        show ty `shouldNotBe` ""

        it "pipeline fails fast for nested-let when only expansion-derived instantiation remains" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g")))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err ->
                    show err `shouldSatisfy`
                        (\msg ->
                            "PhiTranslatabilityError" `isInfixOf` msg
                                || "ValidationFailed" `isInfixOf` msg
                        )
                Right (_, ty) ->
                    expectationFailure ("Expected strict failure, got type: " ++ show ty)

    describe "C2: replay contract fields are omitted when replay binder domain is empty" $ do
        it "let-poly traces with empty replay binder domains have empty binder args and replay-map" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                result = runPipelineArtifactsDefault Set.empty expr
            case result of
                Left err -> expectationFailure err
                Right pa -> do
                    let pres = paPresolution pa
                        traces = IntMap.elems (prEdgeTraces pres)
                        replayBinderDomain tr =
                            case Binding.orderedBinders id (prConstraint pres) (typeRef (etRoot tr)) of
                                Left _ -> []
                                Right binders -> binders
                    forM_ traces $ \tr ->
                        when (null (replayBinderDomain tr)) $ do
                            etBinderArgs tr `shouldBe` []
                            etBinderReplayMap tr `shouldBe` IntMap.empty

    describe "C3: Omega resolves binders without class-member fallback when trace available" $ do
        let corpus =
                [ ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("pipeline still succeeds for: " ++ label) $ do
                let result = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                case result of
                    Left err -> expectationFailure (show err)
                    Right _ -> pure ()

    describe "C4: A6 bounded-alias coercion regressions stay green" $ do
        it "bounded-alias coercion path succeeds in unchecked and checked pipelines" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr =
                    ELet "c" (EAnn rhs schemeTy)
                        (EAnn (EVar "c") ann)
                normExpr = unsafeNormalizeExpr expr
            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure (show err)
                Right (_, tyUnchecked) ->
                    case runPipelineElabChecked Set.empty normExpr of
                        Left err -> expectationFailure (show err)
                        Right (_, tyChecked) ->
                            show tyUnchecked `shouldBe` show tyChecked

        it "applied bounded-coercion path succeeds in unchecked and checked pipelines" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr =
                    ELet "c" (EAnn rhs schemeTy)
                        (EApp
                            (EApp (EAnn (EVar "c") ann) (ELit (LInt 1)))
                            (ELit (LInt 2)))
                normExpr = unsafeNormalizeExpr expr
            let expectCoercionMismatch label result =
                    case result of
                        Left err ->
                            let rendered = show err
                            in rendered `shouldSatisfy`
                                (\msg ->
                                    "TCLetTypeMismatch" `isInfixOf` msg
                                        || "TCInstantiationError" `isInfixOf` msg
                                )
                        Right (_, ty) ->
                            expectationFailure
                                ("Expected let-type mismatch for " ++ label ++ ", but pipeline succeeded with " ++ show ty)
            expectCoercionMismatch "unchecked pipeline" (runPipelineElab Set.empty normExpr)
            expectCoercionMismatch "checked pipeline" (runPipelineElabChecked Set.empty normExpr)
