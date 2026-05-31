{-# LANGUAGE DataKinds #-}

module Elab.ResultTypeUtilSpec (spec) where

import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    Constraint (..),
    EdgeId (..),
    ExpVarId (..),
    GenNode (..),
    GenNodeId (..),
    NodeId (..),
    fromListGen,
  )
import MLF.Elab.Run.ResultType.Util
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.Syntax (Lit (..))
import SpecUtil (emptyConstraint)
import Test.Hspec

intTy :: ElabType
intTy = TBase (BaseTy "Int")

boolTy :: ElabType
boolTy = TBase (BaseTy "Bool")

intBound :: BoundType
intBound = TBase (BaseTy "Int")

forallTy :: ElabType
forallTy = TForall "a" Nothing (TVar "a")

boundedByForall :: ElabType
boundedByForall =
  TForall
    "a"
    (Just (TForall "b" Nothing (TVar "b")))
    (TVar "a")

spec :: Spec
spec = describe "MLF.Elab.Run.ResultType.Util" $ do
  describe "candidate selection" $ do
    it "tracks absence, uniqueness, ambiguity, and selected values" $ do
      candidateSelectionValue (NoCandidateSelection :: CandidateSelection Int)
        `shouldBe` Nothing
      candidateSelectionValue (UniqueCandidateSelection 7)
        `shouldBe` Just (7 :: Int)
      candidateSelectionIsAmbiguous (AmbiguousCandidateSelection :: CandidateSelection Int)
        `shouldBe` True
      candidateSelectionIsAmbiguous (UniqueCandidateSelection (7 :: Int))
        `shouldBe` False

    it "selects exactly one equivalence class" $ do
      selectUniqueCandidate ([] :: [Int]) `shouldBe` NoCandidateSelection
      selectUniqueCandidate [1 :: Int, 1, 1] `shouldBe` UniqueCandidateSelection 1
      selectUniqueCandidate [1 :: Int, 2] `shouldBe` AmbiguousCandidateSelection
      selectUniqueCandidateBy (\left right -> even left == even right) [2 :: Int, 4, 6]
        `shouldBe` UniqueCandidateSelection 2
      selectUniqueCandidateBy (\left right -> even left == even right) [2 :: Int, 3]
        `shouldBe` AmbiguousCandidateSelection

  describe "forall detection and implicit instantiation" $ do
    it "detects foralls that appear in explicit bounds" $ do
      containsBoundForall intTy `shouldBe` False
      containsBoundForall (TForall "a" (Just intBound) (TVar "a"))
        `shouldBe` False
      containsBoundForall boundedByForall `shouldBe` True
      containsBoundForall (TArrow intTy boundedByForall) `shouldBe` True
      containsBoundForall (TCon (BaseTy "Box") (boundedByForall :| [boolTy]))
        `shouldBe` True
      containsBoundForall (TVarApp "F" (boundedByForall :| []))
        `shouldBe` True

    it "detects forall-bearing instantiation arguments through the whole inst tree" $ do
      instHasBoundForall InstId `shouldBe` False
      instHasBoundForall (InstApp forallTy) `shouldBe` True
      instHasBoundForall (InstBot intTy) `shouldBe` False
      instHasBoundForall (InstSeq (InstInside (InstBot forallTy)) (InstUnder "a" InstElim))
        `shouldBe` True
      instHasBoundForall (InstUnder "a" InstIntro) `shouldBe` False

    it "eliminates implicit bounded foralls while preserving explicit unbounded binders" $ do
      let implicit = TForall "a" (Just intBound) (TVar "a")
      instantiateImplicitForalls implicit `shouldBe` intTy
      instantiateImplicitForalls (TArrow implicit (TForall "b" Nothing implicit))
        `shouldBe` TArrow intTy (TForall "b" Nothing intTy)

  describe "annotation helpers" $ do
    it "strips only outer annotation and unfold wrappers" $ do
      let bare = AVar "x" (NodeId 0)
          wrapped = AAnn (AUnfold bare (NodeId 1) (EdgeId 10)) (NodeId 2) (EdgeId 11)
          app = AApp wrapped bare (EdgeId 1) (EdgeId 2) (NodeId 3)
      stripAnn wrapped `shouldBe` bare
      stripAnn app `shouldBe` app

    it "collects edge ids in traversal order and excludes let metadata" $ do
      let fun = AAnn (AVar "f" (NodeId 0)) (NodeId 1) (EdgeId 20)
          arg = AUnfold (ALit (LInt 1) (NodeId 2)) (NodeId 3) (EdgeId 30)
          rhs = AApp fun arg (EdgeId 10) (EdgeId 11) (NodeId 4)
          body =
            ALam
              "x"
              (NodeId 5)
              (GenNodeId 0)
              (AAnn (AVar "x" (NodeId 6)) (NodeId 7) (EdgeId 40))
              (NodeId 8)
          expr =
            ALet
              "x"
              (GenNodeId 1)
              (NodeId 9)
              (ExpVarId 0)
              (GenNodeId 0)
              rhs
              body
              (NodeId 10)
      collectEdges expr `shouldBe` [EdgeId 10, EdgeId 11, EdgeId 20, EdgeId 30, EdgeId 40]

  describe "result type root peeling" $ do
    it "peels let-result annotations when the annotation edge is a let edge" $ do
      let inner = AVar "body" (NodeId 20)
          wrapped = AAnn inner (NodeId 10) (EdgeId 7)
          rhs = AVar "rhs" (NodeId 21)
          expr =
            ALet
              "x"
              (GenNodeId 0)
              (NodeId 10)
              (ExpVarId 0)
              (GenNodeId 1)
              rhs
              wrapped
              (NodeId 10)
          sourceConstraint =
            emptyConstraint
              { cGenNodes = fromListGen [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 10])]
              }
          baseConstraint = emptyConstraint {cLetEdges = IntSet.singleton 7}
      resultTypeRoots id sourceConstraint baseConstraint expr expr
        `shouldBe` (inner, inner)

    it "preserves scheme-root annotations when no let edge authorizes peeling" $ do
      let inner = AVar "body" (NodeId 20)
          wrapped = AUnfold inner (NodeId 10) (EdgeId 8)
          rhs = AVar "rhs" (NodeId 21)
          expr =
            ALet
              "x"
              (GenNodeId 0)
              (NodeId 10)
              (ExpVarId 0)
              (GenNodeId 1)
              rhs
              wrapped
              (NodeId 10)
          sourceConstraint =
            emptyConstraint
              { cGenNodes = fromListGen [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 10])]
              }
      resultTypeRoots id sourceConstraint emptyConstraint expr expr
        `shouldBe` (wrapped, wrapped)

    it "leaves non-let roots unchanged" $ do
      let ann = AAnn (AVar "x" (NodeId 0)) (NodeId 1) (EdgeId 1)
      resultTypeRoots id emptyConstraint emptyConstraint ann ann
        `shouldBe` (ann, ann)
