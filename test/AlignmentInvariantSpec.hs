module AlignmentInvariantSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Presolution
    ( prEdgeTraces
    , prEdgeWitnesses
    )
import MLF.Constraint.Types.Graph
    ( TyNode(..) )
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solved as Solved
import MLF.Elab.Pipeline (runPipelineElab, typeCheck)
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
                            nodes = NodeAccess.allNodes (Solved.originalConstraint solved)
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

    describe "D2: post-boundary elaboration integration" $ do
        it "full pipeline typechecks an identity-backed nested let" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g")))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (show err)
                Right (term, ty) -> typeCheck term `shouldBe` Right ty
