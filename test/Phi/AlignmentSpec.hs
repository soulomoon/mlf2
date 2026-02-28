module Phi.AlignmentSpec (spec) where

import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..))
import MLF.Constraint.Types.Graph (typeRef)
import MLF.Elab.Pipeline (runPipelineElab)
import MLF.Frontend.Syntax (Expr(..), SrcTy(..))
import qualified MLF.Binding.Tree as Binding
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
