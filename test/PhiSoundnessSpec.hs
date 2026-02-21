{-# LANGUAGE GADTs #-}
module PhiSoundnessSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Elab.Pipeline
    ( runPipelineElab
    , typeCheck
    )
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import SpecUtil (unsafeNormalizeExpr)

spec :: Spec
spec = describe "Phi soundness" $ do
    it "identity application: Phi(e) produces well-typed elaboration" $ do
        let expr = unsafeNormalizeExpr
                (EApp (ELam "x" (EVar "x")) (ELit (LInt 1)))
        case runPipelineElab Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ show err
            Right (term, _ty) ->
                case typeCheck term of
                    Left err -> expectationFailure $ "typeCheck: " ++ show err
                    Right _ -> pure ()

    it "let-polymorphic application: Phi(e) produces well-typed elaboration" $ do
        let expr = unsafeNormalizeExpr
                (ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (ELit (LInt 1))))
        case runPipelineElab Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ show err
            Right (term, _ty) ->
                case typeCheck term of
                    Left err -> expectationFailure $ "typeCheck: " ++ show err
                    Right _ -> pure ()

    it "lambda with body application: Phi(e) produces well-typed elaboration" $ do
        let expr = unsafeNormalizeExpr
                (ELam "f" (EApp (EVar "f") (ELit (LInt 1))))
        case runPipelineElab Set.empty expr of
            Left err -> expectationFailure $ "pipeline: " ++ show err
            Right (term, _ty) ->
                case typeCheck term of
                    Left err -> expectationFailure $ "typeCheck: " ++ show err
                    Right _ -> pure ()
