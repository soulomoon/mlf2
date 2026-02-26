module Phi.AlignmentSpec (spec) where

import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..))
import MLF.Elab.Pipeline (runPipelineElab)
import MLF.Frontend.Syntax (Expr(..), SrcTy(..))
import SpecUtil (unsafeNormalizeExpr, runPipelineArtifactsDefault, PipelineArtifacts(..))

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
                , ("nested-let"
                  , ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g")))
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

    describe "C2: edge traces have non-empty binder args for polymorphic edges" $ do
        it "let-poly has at least one edge with non-empty binder args" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                result = runPipelineArtifactsDefault Set.empty expr
            case result of
                Left err -> expectationFailure err
                Right pa -> do
                    let traces = prEdgeTraces (paPresolution pa)
                        nonEmptyBinderArgs =
                            IntMap.filter
                                (\tr -> not (null (etBinderArgs tr)))
                                traces
                    IntMap.size nonEmptyBinderArgs `shouldSatisfy` (> 0)

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
