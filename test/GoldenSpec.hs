module GoldenSpec (spec) where

{- Note [Golden test workflow]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These tests compare pipeline output against checked-in golden files under
test/golden/. To regenerate golden files after an intentional output change:

    GOLDEN_ACCEPT=1 cabal test

This overwrites each .golden file with the current output. Review the diff
with `git diff test/golden/` before committing.
-}

import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Set qualified as Set
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    Constraint (..),
    NodeId (..),
    TyNode (..),
    getNodeId,
    toListNode,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), SurfaceExpr)
import MLF.Pipeline
  ( ConstraintResult (..),
    Pretty (..),
    inferConstraintGraph,
    runPipelineElab,
  )
import SpecUtil (unsafeNormalizeExpr)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Canonical test expressions
-- ---------------------------------------------------------------------------

-- | λx. x — identity
identityExpr :: SurfaceExpr
identityExpr = ELam "x" (EVar "x")

-- | λt. λf. t — Church true
churchTrueExpr :: SurfaceExpr
churchTrueExpr = ELam "t" (ELam "f" (EVar "t"))

-- | let id = λx. x in let a = id 1 in id True — polymorphic let
polyLetExpr :: SurfaceExpr
polyLetExpr =
  ELet
    "id"
    (ELam "x" (EVar "x"))
    ( ELet
        "a"
        (EApp (EVar "id") (ELit (LInt 1)))
        (EApp (EVar "id") (ELit (LBool True)))
    )

-- | (λx. x) 42 — simple application (id applied to int literal)
simpleAppExpr :: SurfaceExpr
simpleAppExpr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))

-- | λx. λy. x — choose (always-first)
chooseExpr :: SurfaceExpr
chooseExpr = ELam "x" (ELam "y" (EVar "x"))

-- ---------------------------------------------------------------------------
-- Golden comparison helper
-- ---------------------------------------------------------------------------

-- | Compare actual output against a golden file.
-- When @GOLDEN_ACCEPT=1@ is set, overwrite the golden file with actual output.
goldenTest :: FilePath -> String -> Expectation
goldenTest goldenPath actual = do
  accept <- lookupEnv "GOLDEN_ACCEPT"
  case accept of
    Just "1" -> do
      createDirectoryIfMissing True (takeDirectory goldenPath)
      writeFile goldenPath actual
    _ -> do
      expected <- readFile goldenPath
      -- Force full evaluation to avoid lazy IO handle issues
      length expected `seq` actual `shouldBe` expected

-- ---------------------------------------------------------------------------
-- xMLF pretty-print golden tests
-- ---------------------------------------------------------------------------

goldenXmlfTest :: String -> SurfaceExpr -> Spec
goldenXmlfTest name expr = it ("golden xMLF output: " ++ name) $ do
  let normExpr = unsafeNormalizeExpr expr
  case runPipelineElab Set.empty normExpr of
    Left err -> expectationFailure ("Pipeline failed: " ++ show err)
    Right (term, ty) -> do
      let output =
            unlines
              [ "-- xMLF golden output for: " ++ name,
                "term: " ++ pretty term,
                "type: " ++ pretty ty
              ]
      goldenTest ("test/golden/xmlf-" ++ name ++ ".golden") output

-- ---------------------------------------------------------------------------
-- Constraint graph summary golden tests
-- ---------------------------------------------------------------------------

-- | Produce a deterministic text summary of a constraint graph.
constraintSummary :: Constraint -> NodeId -> String
constraintSummary c root =
  unlines
    [ "root: " ++ show (getNodeId root),
      "node-count: " ++ show (length nodeList),
      "inst-edge-count: " ++ show (length (cInstEdges c)),
      "unify-edge-count: " ++ show (length (cUnifyEdges c)),
      "bind-parent-count: " ++ show (IntMap.size (cBindParents c)),
      "nodes:",
      unlines (map showNode (sort (map (\(nid, n) -> (getNodeId nid, nodeTag n)) nodeList)))
    ]
  where
    nodeList = toListNode (cNodes c)
    nodeTag :: TyNode -> String
    nodeTag n = case n of
      TyVar {} -> "TyVar"
      TyBottom {} -> "TyBottom"
      TyArrow {} -> "TyArrow"
      TyBase _ (BaseTy b) -> "TyBase(" ++ b ++ ")"
      TyCon {} -> "TyCon"
      TyForall {} -> "TyForall"
      TyExp {} -> "TyExp"
      TyMu {} -> "TyMu"
    showNode :: (Int, String) -> String
    showNode (nid, tag) = "  " ++ show nid ++ ": " ++ tag

goldenConstraintTest :: String -> SurfaceExpr -> Spec
goldenConstraintTest name expr = it ("golden constraint summary: " ++ name) $ do
  let normExpr = unsafeNormalizeExpr expr
  case inferConstraintGraph Set.empty normExpr of
    Left err -> expectationFailure ("Constraint gen failed: " ++ show err)
    Right ConstraintResult {crConstraint = c, crRoot = root} -> do
      let output =
            unlines
              [ "-- Constraint graph summary for: " ++ name,
                constraintSummary c root
              ]
      goldenTest ("test/golden/constraint-" ++ name ++ ".golden") output

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Golden tests" $ do
  describe "xMLF pretty-print output" $ do
    goldenXmlfTest "identity" identityExpr
    goldenXmlfTest "church-true" churchTrueExpr
    goldenXmlfTest "poly-let" polyLetExpr
    goldenXmlfTest "simple-app" simpleAppExpr
    goldenXmlfTest "choose" chooseExpr

  describe "Constraint graph summaries" $ do
    goldenConstraintTest "identity" identityExpr
    goldenConstraintTest "poly-let" polyLetExpr
    goldenConstraintTest "choose" chooseExpr
