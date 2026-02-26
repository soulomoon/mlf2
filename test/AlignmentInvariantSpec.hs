module AlignmentInvariantSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Types.Graph
    ( TyNode(..) )
import qualified MLF.Constraint.Solved as Solved
import MLF.Frontend.Syntax (Expr(..), SrcTy(..))
import SpecUtil

spec :: Spec
spec = describe "Thesis alignment invariants" $ do
    describe "A1: no residual TyExp after presolution" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("const", ELam "x" (ELam "y" (EVar "x")))
                , ("app-id", EApp (ELam "x" (EVar "x")) (ELam "y" (EVar "y")))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("no TyExp nodes in solved constraint for: " ++ label) $ do
                let result = runPipelineArtifactsDefault Set.empty expr
                case result of
                    Left err -> expectationFailure err
                    Right pa -> do
                        let solved = paSolved pa
                            nodes = Solved.allNodes solved
                            tyExpNodes = [ n | n@TyExp{} <- nodes ]
                        tyExpNodes `shouldBe` []

    describe "A2: every non-trivial edge has witness and trace" $ do
        let corpus =
                [ ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("all inst edges have witness+trace for: " ++ label) $ do
                let result = runPipelineArtifactsDefault Set.empty expr
                case result of
                    Left err -> expectationFailure err
                    Right pa -> do
                        let pres = paPresolution pa
                            witnesses = prEdgeWitnesses pres
                            traces = prEdgeTraces pres
                            witnessKeys = IntMap.keysSet witnesses
                            traceKeys = IntMap.keysSet traces
                        -- Every witness should have a corresponding trace
                        let missingTraces = IntSet.difference witnessKeys traceKeys
                        missingTraces `shouldBe` IntSet.empty
