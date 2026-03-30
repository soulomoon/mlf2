module Reify.CoreSpec (spec) where

import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import MLF.Constraint.Presolution.View (PresolutionView, fromSolved)
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Graph (NodeId (..), getNodeId)
import MLF.Frontend.Syntax (Expr (..), Lit (..))
import MLF.Reify.Bound qualified as Bound
import MLF.Reify.Core qualified as Core
import MLF.Reify.Named qualified as Named
import MLF.Reify.Type qualified as Type
import SpecUtil
  ( PipelineArtifacts (..),
    requireRight,
    runPipelineArtifactsDefault,
  )
import Test.Hspec

-- | Run the full pipeline on an expression and return artifacts.
pipelineFor :: PipelineArtifacts -> (Solved.Solved, PresolutionView, NodeId)
pipelineFor artifacts =
  let solved = paSolved artifacts
      view = fromSolved solved
      root = paRoot artifacts
   in (solved, view, root)

spec :: Spec
spec = describe "MLF.Reify.Core (re-export facade)" $ do
  describe "reifyType delegates to Type.reifyType" $ do
    it "agrees on literal" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELit (LInt 1)))
      let (_, view, root) = pipelineFor artifacts
      Core.reifyType view root `shouldBe` Type.reifyType view root

    it "agrees on identity function" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      let (_, view, root) = pipelineFor artifacts
      Core.reifyType view root `shouldBe` Type.reifyType view root

    it "agrees on let-polymorphism" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (ELit (LInt 1)))
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (_, view, root) = pipelineFor artifacts
      Core.reifyType view root `shouldBe` Type.reifyType view root

  describe "freeVars delegates to Bound.freeVars" $ do
    it "agrees on literal" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELit (LInt 1)))
      let (solved, _, root) = pipelineFor artifacts
      Core.freeVars solved root IntSet.empty
        `shouldBe` Bound.freeVars solved root IntSet.empty

    it "agrees on identity function" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      let (solved, _, root) = pipelineFor artifacts
      Core.freeVars solved root IntSet.empty
        `shouldBe` Bound.freeVars solved root IntSet.empty

    it "agrees with visited set" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      let (solved, _, root) = pipelineFor artifacts
          visited = IntSet.singleton (getNodeId root)
      Core.freeVars solved root visited
        `shouldBe` Bound.freeVars solved root visited

  describe "namedNodes delegates to Named.namedNodes" $ do
    it "agrees on literal" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELit (LInt 1)))
      let (_, view, _) = pipelineFor artifacts
      Core.namedNodes view `shouldBe` Named.namedNodes view

    it "agrees on identity function" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      let (_, view, _) = pipelineFor artifacts
      Core.namedNodes view `shouldBe` Named.namedNodes view

    it "agrees on let-polymorphism" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (ELit (LInt 1)))
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
      let (_, view, _) = pipelineFor artifacts
      Core.namedNodes view `shouldBe` Named.namedNodes view
