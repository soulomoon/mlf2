{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Reify.TypeSpec (spec) where

import Data.IntSet qualified as IntSet
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import MLF.Constraint.Presolution.View (PresolutionView, fromSolved)
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..), getNodeId)
import MLF.Frontend.Syntax (Expr (..), Lit (..), SurfaceExpr)
import MLF.Reify.Type
  ( ReifyRoot (..),
    freeVars,
    reifyType,
    reifyWith,
    reifyWithAs,
    solvedFromView,
  )
import MLF.Types.Elab (ElabType, Ty (..))
import MLF.Util.ElabError (ElabError (..))
import SpecUtil
  ( PipelineArtifacts (..),
    expectRight,
    requireRight,
    runPipelineArtifactsDefault,
  )
import Test.Hspec

-- | Helper: run the full pipeline on a surface expression and return artifacts.
pipelineFor :: SurfaceExpr -> IO PipelineArtifacts
pipelineFor expr = requireRight (runPipelineArtifactsDefault Set.empty expr)

-- | Helper: build PresolutionView 'Raw from a Solved value.
viewFor :: Solved.Solved -> PresolutionView 'Raw
viewFor = fromSolved

spec :: Spec
spec = describe "MLF.Reify.Type" $ do
  describe "reifyType" $ do
    it "reifies a literal expression to a base type" $ do
      artifacts <- pipelineFor (ELit (LInt 42))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
      expectRight (reifyType view root) $ \ty ->
        -- Literal 42 should reify to int base type
        ty `shouldSatisfy` isBaseType

    it "reifies identity function \\x.x to its representative type variable" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isVarType

    it "reifies let-polymorphism (let id = \\x.x in id 1)" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (ELit (LInt 1)))
      artifacts <- pipelineFor expr
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
      expectRight (reifyType view root) $ \ty ->
        -- Should reify to some type (int)
        ty `shouldSatisfy` isBaseType

    it "reifies application (\\x.x) 1 to a base type" $ do
      let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 1))
      artifacts <- pipelineFor expr
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isBaseType

  describe "solvedFromView" $ do
    it "produces a Solved with matching original constraint for identity" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          solved' = solvedFromView view
      -- The original constraint should match
      Solved.originalConstraint solved'
        `shouldBe` Solved.originalConstraint solved

    it "produces a Solved with matching canonical constraint for identity" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          solved' = solvedFromView view
      Solved.canonicalConstraint solved'
        `shouldBe` Solved.canonicalConstraint solved

    it "roundtrips through view for literal expression" $ do
      artifacts <- pipelineFor (ELit (LInt 0))
      let solved = paSolved artifacts
          view = viewFor solved
          solved' = solvedFromView view
      Solved.originalConstraint solved'
        `shouldBe` Solved.originalConstraint solved

  describe "freeVars" $ do
    it "returns empty for a literal node" $ do
      artifacts <- pipelineFor (ELit (LInt 99))
      let solved = paSolved artifacts
          root = paRoot artifacts
          fvs = freeVars solved root IntSet.empty
      -- Literal nodes have no free vars
      fvs `shouldSatisfy` IntSet.null

    it "returns non-empty for identity function root" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          root = paRoot artifacts
          fvs = freeVars solved root IntSet.empty
      -- Identity has a binder node reachable from the reified root.
      fvs `shouldSatisfy` (not . IntSet.null)
    it "respects visited set by skipping already-seen nodes" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          root = paRoot artifacts
          -- Pre-populate visited with the root itself
          visited = IntSet.singleton (getNodeId (Solved.canonical solved root))
          fvs = freeVars solved root visited
      -- With root already visited, should return empty
      fvs `shouldBe` IntSet.empty

  describe "reifyWith" $ do
    it "reifies identity with custom var names" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "v" ++ show i
          isNamed _ = False
      expectRight (reifyWith "test" solved nameFor isNamed RootType root) $ \ty ->
        ty `shouldSatisfy` isPrefixedVarType "v"
    it "reifies literal with RootType" $ do
      artifacts <- pipelineFor (ELit (LInt 7))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "n" ++ show i
          isNamed _ = False
      expectRight (reifyWith "test" solved nameFor isNamed RootType root) $ \ty ->
        ty `shouldSatisfy` isBaseType

    it "reifies literal with RootBound" $ do
      artifacts <- pipelineFor (ELit (LInt 7))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "n" ++ show i
          isNamed _ = False
      expectRight (reifyWith "test" solved nameFor isNamed RootBound root) $ \ty ->
        ty `shouldSatisfy` isBaseType

  describe "reifyWithAs" $ do
    it "applies conversion function after reification" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "t" ++ show i
          isNamed _ = False
          asString :: ElabType -> Either ElabError String
          asString ty = Right (show ty)
      expectRight (reifyWithAs "test" solved nameFor isNamed RootType asString root) $ \s ->
        s `shouldBe` show (TBase (BaseTy "Int"))

    it "propagates conversion failure" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "t" ++ show i
          isNamed _ = False
          failConvert :: ElabType -> Either ElabError String
          failConvert _ = Left (InstantiationError "test-fail")
      case reifyWithAs "test" solved nameFor isNamed RootType failConvert root of
        Left _ -> pure () -- expected
        Right _ -> expectationFailure "Expected conversion failure"

    it "succeeds with identity conversion" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          root = paRoot artifacts
          nameFor (NodeId i) = "t" ++ show i
          isNamed _ = False
      expectRight (reifyWithAs "test" solved nameFor isNamed RootType Right root) $ \ty ->
        ty `shouldSatisfy` isBaseType

-- Predicates for structural assertions on ElabType
isBaseType :: ElabType -> Bool
isBaseType (TBase _) = True
isBaseType _ = False

isVarType :: ElabType -> Bool
isVarType (TVar _) = True
isVarType _ = False

isPrefixedVarType :: String -> ElabType -> Bool
isPrefixedVarType prefix ty = case ty of
  TVar name -> prefix `isPrefixOf` name
  _ -> False
