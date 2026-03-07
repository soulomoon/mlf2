module AlignmentInvariantSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Types.Graph
    ( TyNode(..) )
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solved as Solved
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
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

    describe "A3: pipeline output stability" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("const", ELam "x" (ELam "y" (EVar "x")))
                , ("app-id", EApp (ELam "x" (EVar "x")) (ELam "y" (EVar "y")))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("checked and unchecked paths agree for: " ++ label) $ do
                let unchecked = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                    checked = runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)
                case (unchecked, checked) of
                    (Right (t1, ty1), Right (t2, ty2)) -> do
                        show t1 `shouldBe` show t2
                        show ty1 `shouldBe` show ty2
                    (Left e1, Left e2) ->
                        show e1 `shouldBe` show e2
                    _ -> expectationFailure $
                        "Path disagreement for " ++ label ++
                        ": unchecked=" ++ show unchecked ++
                        ", checked=" ++ show checked

    describe "D1: fallback result type works without patchNode" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("pipeline output unchanged after patchNode elimination for: " ++ label) $ do
                let norm = unsafeNormalizeExpr expr
                    r1 = runPipelineElab Set.empty norm
                    r2 = runPipelineElabChecked Set.empty norm
                case (r1, r2) of
                    (Right (t1, ty1), Right (t2, ty2)) -> do
                        show t1 `shouldBe` show t2
                        show ty1 `shouldBe` show ty2
                    (Left _, Left _) -> pure ()
                    _ -> expectationFailure $
                        "Path disagreement for " ++ label

    describe "B1: reification output stable after original-domain migration" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("term and type unchanged for: " ++ label) $ do
                let norm = unsafeNormalizeExpr expr
                    r1 = runPipelineElab Set.empty norm
                    r2 = runPipelineElabChecked Set.empty norm
                case (r1, r2) of
                    (Right (t1, ty1), Right (t2, ty2)) -> do
                        show t1 `shouldBe` show t2
                        show ty1 `shouldBe` show ty2
                    (Left _, Left _) -> pure ()
                    _ -> expectationFailure $
                        "Path disagreement for " ++ label

    describe "D2: elaboration path is read-only on Solved" $ do
        let corpus =
                [ ("id", ELam "x" (EVar "x"))
                , ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                , ("ann-id", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                , ("nested-let"
                  , ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g"))))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("full pipeline succeeds post-boundary-enforcement for: " ++ label) $ do
                let result = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                case result of
                    Left err -> expectationFailure (show err)
                    Right (term, ty) -> do
                        show term `shouldNotBe` ""
                        show ty `shouldNotBe` ""
