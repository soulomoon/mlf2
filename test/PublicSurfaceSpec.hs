{-# LANGUAGE LambdaCase #-}

module PublicSurfaceSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import MLF.API
import MLF.API qualified as API
import MLF.Pipeline qualified as Pipeline
import MLF.XMLF
import Test.Hspec

spec :: Spec
spec = describe "Public surface contracts" $ do
  describe "MLF.API" $ do
    it "roundtrips raw surface expressions through parse(pretty(expr))" $ do
      let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
      parseRawEmlfExpr (prettyEmlfExpr expr) `shouldBe` Right expr

    it "roundtrips recursive surface types through parse(pretty(type))" $ do
      let ty = STMu "a" (STCon "List" (STVar "a" :| []))
      parseRawEmlfType (prettyEmlfType ty) `shouldBe` Right ty

    it "roundtrips variable-headed surface types through parse(pretty(type))" $ do
      let ty = STVarApp "f" (STVarApp "g" (STVar "a" :| []) :| [STVar "b"])
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

    it "roundtrips .mlfp programs through the unified frontend API" $ do
      let src =
            unlines
              [ "module Main export (main) {",
                "  def main : Int = let id = \\x x in id 1;",
                "}"
              ]
      expectRight (parseRawProgram src) $ \program ->
        parseRawProgram (prettyProgram program) `shouldBe` Right program

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

    it "runs unified .mlfp programs through the shared eMLF/typecheck surface" $ do
      src <- readFile "test/programs/unified/authoritative-let-polymorphism.mlfp"
      expectRight (parseRawProgram src) $ \program ->
        expectRight (Pipeline.runProgram program) $ \value ->
          Pipeline.prettyValue value `shouldBe` "1"

    it "checks cross-module .mlfp programs through the shared eMLF/typecheck surface" $ do
      src <- readFile "test/programs/unified/authoritative-cross-module-let-polymorphism.mlfp"
      expectRight (parseRawProgram src) $ \program ->
        expectRight (Pipeline.runProgram program) $ \value ->
          Pipeline.prettyValue value `shouldBe` "1"

    it "runs case-bearing .mlfp programs through the shared eMLF/typecheck surface" $ do
      src <- readFile "test/programs/unified/authoritative-case-analysis.mlfp"
      expectRight (parseRawProgram src) $ \program ->
        expectRight (Pipeline.runProgram program) $ \value ->
          Pipeline.prettyValue value `shouldBe` "1"

    it "runs overloaded-method .mlfp programs through the shared eMLF/typecheck surface" $ do
      src <- readFile "test/programs/unified/authoritative-overloaded-method.mlfp"
      expectRight (parseRawProgram src) $ \program ->
        expectRight (Pipeline.runProgram program) $ \value ->
          Pipeline.prettyValue value `shouldBe` "true"

    it "runs recursive-let .mlfp programs through the shared eMLF/typecheck surface" $ do
      src <- readFile "test/programs/unified/authoritative-recursive-let.mlfp"
      expectRight (parseRawProgram src) $ \program ->
        expectRight (Pipeline.runProgram program) $ \value ->
          Pipeline.prettyValue value `shouldBe` "true"

  describe "MLF.Pipeline (error formatting)" $ do
    it "formatPipelineError produces structured Text output" $ do
      let err = Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x")
          formatted = Pipeline.formatPipelineError err
      T.unpack formatted `shouldContain` "[Phase 1]"
      T.unpack formatted `shouldContain` "constraint generation"
      T.unpack formatted `shouldContain` "UnknownVariable"

    it "pipelineErrorPhase returns correct phase numbers" $ do
      Pipeline.pipelineErrorPhase
        (Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x"))
        `shouldBe` 1
      Pipeline.pipelineErrorPhase
        ( Pipeline.PipelineCycleError
            (Pipeline.CycleError [] "test")
        )
        `shouldBe` 3

    it "pipelineErrorPhaseName returns human-readable names" $ do
      Pipeline.pipelineErrorPhaseName
        (Pipeline.PipelineConstraintError (Pipeline.UnknownVariable "x"))
        `shouldBe` "constraint generation"

  describe "MLF.API (constraint graph introspection)" $ do
    it "constraintNodeCount returns a positive count for a real graph" $ do
      expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
        expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
          let c = Pipeline.crConstraint cr
          API.constraintNodeCount c `shouldSatisfy` (> 0)

    it "constraintEdgeCount returns a non-negative count" $ do
      expectRight (parseNormEmlfExpr "let id = λ(x) x in id 1") $ \expr ->
        expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
          let c = Pipeline.crConstraint cr
          API.constraintEdgeCount c `shouldSatisfy` (>= 0)

    it "lookupNode retrieves the root node from a constraint graph" $ do
      expectRight (parseNormEmlfExpr "λ(x) x") $ \expr ->
        expectRight (Pipeline.inferConstraintGraph Set.empty expr) $ \cr -> do
          let c = Pipeline.crConstraint cr
              rootId = Pipeline.crRoot cr
          API.lookupNode rootId (API.cNodes c) `shouldSatisfy` \case
            Nothing -> False
            Just _ -> True

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

expectRight :: (Show err) => Either err a -> (a -> Expectation) -> Expectation
expectRight result k =
  case result of
    Left err -> expectationFailure ("Expected Right, got Left " ++ show err)
    Right value -> k value

hasRecursiveArrow :: Pipeline.ElabType -> Bool
hasRecursiveArrow ty = case ty of
  Pipeline.TArrow (Pipeline.TMu _ _) (Pipeline.TMu _ _) -> True
  _ -> False
