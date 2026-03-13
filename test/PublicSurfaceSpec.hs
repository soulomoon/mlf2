module PublicSurfaceSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
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

        it "roundtrips recursive surface types through parse(pretty(type))" $ do
            let ty = STMu "a" (STCon "List" (STVar "a" :| []))
            parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

        it "normalizes raw surface types through the umbrella API" $ do
            expectRight (parseRawEmlfType "∀(b ⩾ a). b") $ \ty ->
                normalizeType ty `shouldBe` Right (STVar "a")

        it "normalizes recursive raw surface types through the umbrella API" $ do
            expectRight (parseRawEmlfType "mu a. ∀(b ⩾ a). b") $ \ty ->
                normalizeType ty `shouldBe` Right (STMu "a" (STVar "a"))

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

        it "accepts recursive surface annotations on the explicit-only path" $ do
            expectRight (parseNormEmlfExpr "λ(x : μa. a -> Int) x") $ \expr -> do
                case Pipeline.inferConstraintGraph Set.empty expr of
                    Left err -> expectationFailure ("Expected constraint graph, got " ++ show err)
                    Right _ -> pure ()
                expectRight (Pipeline.runPipelineElab Set.empty expr) $ \(term, ty) -> do
                    Pipeline.typeCheck term `shouldBe` Right ty
                    ty `shouldSatisfy` hasRecursiveArrow

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

        it "exports explicit recursive XMLF term constructors" $ do
            let term = XUnroll (XRoll (XTMu "self" (XTArrow (XTVar "self") (XTBase "Int"))) (XVar "x"))
            prettyXmlfTerm term `shouldBe` "unroll (roll[μself. self -> Int] x)"
            parseXmlfTerm (prettyXmlfTerm term) `shouldBe` Right term

expectRight :: Show err => Either err a -> (a -> Expectation) -> Expectation
expectRight result k =
    case result of
        Left err -> expectationFailure ("Expected Right, got Left " ++ show err)
        Right value -> k value

hasRecursiveArrow :: Pipeline.ElabType -> Bool
hasRecursiveArrow ty = case ty of
    Pipeline.TArrow (Pipeline.TMu _ _) (Pipeline.TMu _ _) -> True
    _ -> False
