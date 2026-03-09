module PublicSurfaceSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.API
import qualified MLF.Pipeline as Pipeline
import MLF.XMLF

spec :: Spec
spec = describe "Public surface contracts" $ do
    describe "MLF.API" $ do
        it "roundtrips raw surface expressions through parse(pretty(expr))" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
            parseRawEmlfExpr (prettyEmlfExpr expr) `shouldBe` Right expr

        it "normalizes raw surface types through the umbrella API" $ do
            expectRight (parseRawEmlfType "∀(b ⩾ a). b") $ \ty ->
                normalizeType ty `shouldBe` Right (STVar "a")

        it "keeps frontend-only conveniences on the umbrella API" $ do
            parseNormEmlfExpr "λ(x) x"
                `shouldBe` Right (ELam "x" (EVar "x"))

    describe "MLF.Pipeline" $ do
        it "elaborates normalized programs through the focused pipeline surface" $ do
            expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
                expectRight (Pipeline.runPipelineElab Set.empty expr) $ \(term, ty) -> do
                    Pipeline.typeCheck term `shouldBe` Right ty
                    Pipeline.isValue term `shouldBe` True

        it "builds constraint graphs through the normalized-only API" $ do
            expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
                case Pipeline.inferConstraintGraph Set.empty expr of
                    Left err -> expectationFailure ("Expected constraint graph, got " ++ show err)
                    Right _ -> pure ()

        it "owns checked runtime helpers and pipeline diagnostics" $ do
            expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
                expectRight (Pipeline.runPipelineElabChecked Set.empty expr) $ \(term, ty) -> do
                    Pipeline.typeCheck term `shouldBe` Right ty
                    Pipeline.isValue term `shouldBe` True

    describe "MLF.XMLF" $ do
        it "roundtrips parsed xMLF terms through pretty printing" $ do
            let src = "let id = λ(x : Int) x in id 1"
            expectRight (parseXmlfTerm src) $ \term ->
                parseXmlfTerm (prettyXmlfTerm term) `shouldBe` Right term

        it "roundtrips parsed xMLF types through pretty printing" $ do
            let src = "∀(a ⩾ Int) a -> a"
            expectRight (parseXmlfType src) $ \ty ->
                parseXmlfType (prettyXmlfType ty) `shouldBe` Right ty

expectRight :: Show err => Either err a -> (a -> Expectation) -> Expectation
expectRight result k =
    case result of
        Left err -> expectationFailure ("Expected Right, got Left " ++ show err)
        Right value -> k value
