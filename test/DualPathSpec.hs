module DualPathSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.Set as Set

import MLF.Frontend.Syntax (Expr(..), SrcTy(..))
import MLF.Elab.Pipeline (runPipelineElab)
import MLF.Constraint.Presolution (prConstraint)
import MLF.Constraint.Solve (solveUnifyWithSnapshot)
import qualified MLF.Constraint.Solved as Solved
import SpecUtil (PipelineArtifacts(..), defaultTraceConfig, runPipelineArtifactsDefault, unsafeNormalizeExpr)

spec :: Spec
spec = describe "Dual-path verification" $ do
    describe "E1: native solved vs legacy snapshot equivalence" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("const", ELam "x" (ELam "y" (EVar "x")))
                , ("app-id", EApp (ELam "x" (EVar "x")) (ELam "y" (EVar "y")))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x"))
                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                , ("nested-let"
                  , ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g"))))
                , ("higher-rank-app"
                  , EApp
                        (ELam "f" (EVar "f"))
                        (ELam "x" (EVar "x")))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("native solved and legacy snapshot agree for: " ++ label) $ do
                case runPipelineArtifactsDefault Set.empty expr of
                    Left err ->
                        expectationFailure ("runPipelineArtifactsDefault failed for " ++ label ++ ": " ++ err)
                    Right PipelineArtifacts{ paPresolution = pres, paSolved = nativeSolved } ->
                        case solveUnifyWithSnapshot defaultTraceConfig (prConstraint pres) of
                            Left err ->
                                expectationFailure ("solveUnifyWithSnapshot failed for " ++ label ++ ": " ++ show err)
                            Right legacyOut ->
                                case Solved.fromSolveOutput legacyOut of
                                    Left err ->
                                        expectationFailure ("fromSolveOutput failed for " ++ label ++ ": " ++ show err)
                                    Right legacySolved -> do
                                        Solved.canonicalConstraint nativeSolved `shouldBe` Solved.canonicalConstraint legacySolved
                                        Solved.originalConstraint nativeSolved `shouldBe` Solved.originalConstraint legacySolved
                runPipelineElab Set.empty (unsafeNormalizeExpr expr) `shouldSatisfy` either (const False) (const True)

    describe "E2: native solved projection agreement holds" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x"))
                    (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("original/canonical agreement holds for: " ++ label) $ do
                case runPipelineArtifactsDefault Set.empty expr of
                    Left err -> expectationFailure err
                    Right artifacts ->
                        Solved.validateOriginalCanonicalAgreement (paSolved artifacts) `shouldBe` []
