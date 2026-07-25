{-# LANGUAGE DataKinds #-}

module Elab.ResultTypeUtilSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import Data.List.NonEmpty (NonEmpty (..))
import ElabTermTestSupport (generatedLocalRefForName, testTForall, testTVar, testTVarApp)
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    EdgeId (..),
    ExpVarId (..),
    GenNodeId (..),
    NodeId (..),
  )
import MLF.Elab.Pipeline (authoritativeRootAnn)
import MLF.Elab.Run.ResultType.Util
import MLF.Types.Elab
import MLF.Frontend.ConstraintGen (AnnExpr (..), InstantiationSite (..), mkInstantiationSite)
import MLF.Frontend.Syntax (Lit (..))
import MLF.Types.Identity (IdDetails (LocalId), localRefFromNodeId)
import SpecUtil (emptyConstraint)
import Test.Hspec

intTy :: ElabType
intTy = TestElab.tBase (BaseTy "Int")

boolTy :: ElabType
boolTy = TestElab.tBase (BaseTy "Bool")

intBound :: BoundType
intBound = TestElab.tBase (BaseTy "Int")

forallTy :: ElabType
forallTy = testTForall "a" Nothing (testTVar "a")

typeRef :: Int -> String -> TypeBinderRef
typeRef key name =
  typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

annDetails :: String -> IdDetails
annDetails =
  LocalId . generatedLocalRefForName

annVar :: String -> NodeId -> AnnExpr
annVar name =
  AResolvedVar (annDetails name) name

instSite :: Int -> NodeId -> NodeId -> InstantiationSite
instSite edgeKey source target =
  mkInstantiationSite (EdgeId edgeKey) source target

boundedByForall :: ElabType
boundedByForall =
  testTForall
    "a"
    (Just (testTForall "b" Nothing (testTVar "b")))
    (testTVar "a")

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
      containsBoundForall (testTForall "a" (Just intBound) (testTVar "a"))
        `shouldBe` False
      containsBoundForall boundedByForall `shouldBe` True
      containsBoundForall (TArrow intTy boundedByForall) `shouldBe` True
      containsBoundForall (TestElab.tCon (BaseTy "Box") (boundedByForall :| [boolTy]))
        `shouldBe` True
      containsBoundForall (testTVarApp "F" (boundedByForall :| []))
        `shouldBe` True

    it "detects forall-bearing instantiation arguments through the whole inst tree" $ do
      instHasBoundForall InstId `shouldBe` False
      instHasBoundForall (InstApp forallTy) `shouldBe` True
      instHasBoundForall (InstBot intTy) `shouldBe` False
      instHasBoundForall (InstSeq (InstInside (InstBot forallTy)) (instUnderWithRef (typeRef 85 "a") InstElim))
        `shouldBe` True
      instHasBoundForall (instUnderWithRef (typeRef 87 "a") InstIntro) `shouldBe` False

    it "eliminates implicit bounded foralls while preserving explicit unbounded binders" $ do
      let implicit = testTForall "a" (Just intBound) (testTVar "a")
      instantiateImplicitForalls implicit `shouldBe` intTy
      instantiateImplicitForalls (TArrow implicit (testTForall "b" Nothing implicit))
        `shouldBe` TArrow intTy (testTForall "b" Nothing intTy)

    it "preserves refs while walking implicit forall helpers" $ do
      let refA = typeRef 10 "a"
          refB = typeRef 11 "b"
          implicit = tForallWithRef refA (Just intBound) (tVarWithRef refA)
          explicit = tForallWithRef refB Nothing implicit
       in do
            containsBoundForall (tVarAppWithRef refB (boundedByForall :| []))
              `shouldBe` True
            instantiateImplicitForalls explicit
              `shouldBe` tForallWithRef refB Nothing intTy

  describe "annotation helpers" $ do
    it "strips outer coercion, let-scope, and unfold wrappers" $ do
      let bare = annVar "x" (NodeId 0)
          wrapped = AAnn (ALetScope (AUnfold bare (NodeId 1) (EdgeId 10)) (NodeId 2) (EdgeId 12)) (NodeId 3) (EdgeId 11)
          app = AApp wrapped bare (instSite 1 (NodeId 3) (NodeId 3)) (instSite 2 (NodeId 0) (NodeId 3)) (NodeId 3)
      stripAnn wrapped `shouldBe` bare
      stripAnn app `shouldBe` app

    it "collects source and let-scope edge ids in traversal order" $ do
      let fun = AAnn (annVar "f" (NodeId 0)) (NodeId 1) (EdgeId 20)
          arg = AUnfold (ALit (LInt 1) (NodeId 2)) (NodeId 3) (EdgeId 30)
          rhs = AApp fun arg (instSite 10 (NodeId 1) (NodeId 4)) (instSite 11 (NodeId 3) (NodeId 4)) (NodeId 4)
          body =
            ALetScope
              ( ALam
                  "x"
                  (annDetails "x")
                  (NodeId 5)
                  (GenNodeId 0)
                  (AAnn (annVar "x" (NodeId 6)) (NodeId 7) (EdgeId 40))
                  (EdgeId 60)
                  (NodeId 8)
              )
              (NodeId 10)
              (EdgeId 50)
          expr =
            ALet
              "x"
              (annDetails "x")
              (GenNodeId 1)
              (NodeId 9)
              (ExpVarId 0)
              (GenNodeId 0)
              rhs
              body
              (NodeId 10)
      collectEdges expr `shouldBe` [EdgeId 10, EdgeId 11, EdgeId 20, EdgeId 30, EdgeId 50, EdgeId 60, EdgeId 40]

  describe "authoritative result root projection" $ do
    let localDetailsAt node =
          LocalId (localRefFromNodeId "x" node)
        resolvedAt node =
          ResolvedVar
            { resolvedVarType = intTy,
              resolvedVarDetails = localDetailsAt node
            }
        bodyNode = NodeId 20
        bodyTerm = EVarNode (resolvedAt bodyNode)
        bodyAnn = AResolvedVar (localDetailsAt bodyNode) "x" bodyNode
        letTerm =
          ELet
            (resolvedAt (NodeId 7))
            (mkElabSchemeWithRefs [] intTy)
            (ELit (LInt 1))
            bodyTerm
        letAnnWith binderNode resultAnn =
          ALet
            "x"
            (localDetailsAt binderNode)
            (GenNodeId 0)
            binderNode
            (ExpVarId 0)
            (GenNodeId 1)
            (ALit (LInt 1) (NodeId 9))
            resultAnn
            (NodeId 10)
        letAnn binderNode = letAnnWith binderNode bodyAnn

    it "preserves a let rejected by the authoritative binder-identity guard" $ do
      let staleAnn = letAnn (NodeId 8)
          selected = authoritativeRootAnn letTerm staleAnn
      selected `shouldBe` staleAnn

    it "projects the body after authoritative selection accepts the binder identity" $ do
      let matchingAnn = letAnn (NodeId 7)
          selected = authoritativeRootAnn letTerm matchingAnn
      selected `shouldBe` bodyAnn

    it "peels constraint-only let-scope metadata" $ do
      let scoped = ALetScope bodyAnn (NodeId 10) (EdgeId 7)
      authoritativeRootAnn bodyTerm scoped `shouldBe` bodyAnn

    it "checks every nested let identity across transparent let scopes" $ do
      let innerBinder = NodeId 12
          innerTerm =
            ELet
              (resolvedAt innerBinder)
              (mkElabSchemeWithRefs [] intTy)
              (ELit (LInt 1))
              bodyTerm
          nestedTerm =
            ELet
              (resolvedAt (NodeId 7))
              (mkElabSchemeWithRefs [] intTy)
              (ELit (LInt 1))
              innerTerm
          nestedAnn =
            letAnnWith
              (NodeId 7)
              (ALetScope (letAnn innerBinder) (NodeId 30) (EdgeId 31))
      authoritativeRootAnn nestedTerm nestedAnn `shouldBe` bodyAnn

    it "preserves a nested let whose identity mismatches after a transparent let scope" $ do
      let innerBinder = NodeId 12
          staleInnerAnn = letAnn (NodeId 13)
          innerTerm =
            ELet
              (resolvedAt innerBinder)
              (mkElabSchemeWithRefs [] intTy)
              (ELit (LInt 1))
              bodyTerm
          nestedTerm =
            ELet
              (resolvedAt (NodeId 7))
              (mkElabSchemeWithRefs [] intTy)
              (ELit (LInt 1))
              innerTerm
          nestedAnn =
            letAnnWith
              (NodeId 7)
              (ALetScope staleInnerAnn (NodeId 30) (EdgeId 31))
      authoritativeRootAnn nestedTerm nestedAnn `shouldBe` staleInnerAnn

    it "preserves an unfold that survives authoritative selection" $ do
      let wrapped = AUnfold bodyAnn (NodeId 10) (EdgeId 8)
          selected = authoritativeRootAnn (ELit (LInt 1)) wrapped
      selected `shouldBe` wrapped

  describe "result type root peeling" $ do
    it "peels a let-scope root selected by term authority" $ do
      let inner = annVar "body" (NodeId 20)
          scoped = ALetScope inner (NodeId 10) (EdgeId 7)
      resultTypeRoots id emptyConstraint emptyConstraint scoped scoped
        `shouldBe` (inner, inner)

    it "peels a paired generated let without authoritative term matching" $ do
      let inner = annVar "body" (NodeId 20)
          rhs = annVar "rhs" (NodeId 21)
          expr =
            ALet
              "x"
              (annDetails "x")
              (GenNodeId 0)
              (NodeId 10)
              (ExpVarId 0)
              (GenNodeId 1)
              rhs
              inner
              (NodeId 10)
      resultTypeRoots id emptyConstraint emptyConstraint expr expr
        `shouldBe` (inner, inner)

    it "peels explicit let-scope metadata" $ do
      let inner = annVar "body" (NodeId 20)
          scoped = ALetScope inner (NodeId 10) (EdgeId 7)
          rhs = annVar "rhs" (NodeId 21)
          expr =
            ALet
              "x"
              (annDetails "x")
              (GenNodeId 0)
              (NodeId 10)
              (ExpVarId 0)
              (GenNodeId 1)
              rhs
              scoped
              (NodeId 10)
      resultTypeRoots id emptyConstraint emptyConstraint expr expr
        `shouldBe` (inner, inner)

    it "preserves a source unfold inside let-scope metadata" $ do
      let inner = annVar "body" (NodeId 20)
          wrapped = AUnfold inner (NodeId 10) (EdgeId 8)
          scoped = ALetScope wrapped (NodeId 10) (EdgeId 7)
          rhs = annVar "rhs" (NodeId 21)
          expr =
            ALet
              "x"
              (annDetails "x")
              (GenNodeId 0)
              (NodeId 10)
              (ExpVarId 0)
              (GenNodeId 1)
              rhs
              scoped
              (NodeId 10)
      resultTypeRoots id emptyConstraint emptyConstraint expr expr
        `shouldBe` (wrapped, wrapped)

    it "leaves non-let roots unchanged" $ do
      let ann = AAnn (annVar "x" (NodeId 0)) (NodeId 1) (EdgeId 1)
      resultTypeRoots id emptyConstraint emptyConstraint ann ann
        `shouldBe` (ann, ann)
