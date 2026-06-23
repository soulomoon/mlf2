{-# LANGUAGE GADTs #-}

module Reify.TypeOpsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import ElabTermTestSupport (testTForall, testTMu, testTVar)
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    Constraint (..),
    GenNodeMap (..),
    NodeId (..),
    NodeMap (..),
    TyNode (..),
    fromListNode,
  )
import MLF.Reify.TypeOps
  ( alphaEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarRefsList,
    freeTypeVarRefsFrom,
    freeTypeVarRefsType,
    freeTypeVarsType,
    inlineBaseBoundsType,
    inlineAliasBoundsWithBySeen,
    matchTypeRefs,
    parseNameId,
    splitForallsRefs,
    stripForallsType,
    substTypeCaptureRef,
    substTypeSimpleRef,
  )
import MLF.Types.Elab
  ( ElabType,
    Ty (..),
    TypeBinderRef,
    tForallWithRef,
    tMuWithRef,
    tVarAppWithRef,
    tVarWithRef,
    typeBinderIdentityFromNode,
    typeBinderIdentityFromUnique,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefName,
  )
import MLF.Types.Identity (UniqueIdentity (..))
import Test.Hspec

intTy :: ElabType
intTy = TBase (BaseTy "int")

boolTy :: ElabType
boolTy = TBase (BaseTy "bool")

typeRef :: Int -> String -> TypeBinderRef
typeRef key name =
  typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name

generatedTypeRef :: Int -> String -> TypeBinderRef
generatedTypeRef key name =
  typeBinderRefFromIdentity (typeBinderIdentityFromUnique (UniqueIdentity key)) name

emptyConstraint :: Constraint p
emptyConstraint =
  Constraint
    { cNodes = NodeMap mempty,
      cInstEdges = [],
      cUnifyEdges = [],
      cBindParents = mempty,
      cPolySyms = Set.empty,
      cEliminatedVars = mempty,
      cWeakenedVars = mempty,
      cAnnEdges = mempty,
      cLetEdges = mempty,
      cGenNodes = GenNodeMap mempty
    }

spec :: Spec
spec = describe "MLF.Reify.TypeOps" $ do
  describe "splitForallsRefs" $ do
    it "preserves binder refs in the ref-aware split" $ do
      let refA = typeRef 23 "a"
          ty = tForallWithRef refA Nothing (tVarWithRef refA)
       in splitForallsRefs ty `shouldBe` ([(refA, Nothing)], tVarWithRef refA)

  describe "stripForallsType" $ do
    it "returns non-forall type unchanged" $
      stripForallsType (testTVar "x") `shouldBe` testTVar "x"

    it "strips a single forall" $
      stripForallsType (testTForall "a" Nothing (testTVar "a"))
        `shouldBe` testTVar "a"

    it "strips nested foralls" $
      stripForallsType (testTForall "a" Nothing (testTForall "b" Nothing (TArrow (testTVar "a") (testTVar "b"))))
        `shouldBe` TArrow (testTVar "a") (testTVar "b")

  describe "freeTypeVarsType" $ do
    it "finds the single free variable" $
      freeTypeVarsType (testTVar "a")
        `shouldBe` Set.singleton "a"

    it "finds free vars in an arrow" $
      freeTypeVarsType (TArrow (testTVar "a") (testTVar "b"))
        `shouldBe` Set.fromList ["a", "b"]

    it "excludes bound variables" $
      freeTypeVarsType (testTForall "a" Nothing (testTVar "a"))
        `shouldBe` Set.empty

    it "finds only unbound vars in mixed types" $
      freeTypeVarsType (testTForall "a" Nothing (TArrow (testTVar "a") (testTVar "b")))
        `shouldBe` Set.singleton "b"

    it "tracks free variables by identity before projecting names" $ do
      let bound = typeRef 21 "a"
          free = typeRef 22 "a"
          renamedBound = typeRef 21 "renamed"
       in do
            freeTypeVarRefsType (tForallWithRef bound Nothing (tVarWithRef free))
              `shouldBe` [free]
            freeTypeVarRefsType (tForallWithRef bound Nothing (tVarWithRef renamedBound))
              `shouldBe` []
            freeTypeVarsType (tForallWithRef bound Nothing (tVarWithRef free))
              `shouldBe` Set.singleton "a"

  describe "freeTypeVarRefsList" $ do
    it "keeps identity refs in list form" $ do
      let first = typeRef 23 "a"
          second = typeRef 24 "a"
      freeTypeVarRefsList (TArrow (tVarWithRef first) (tVarWithRef second))
        `shouldBe` [first, second]

  describe "freeTypeVarRefsFrom" $ do
    it "treats first argument as bound refs; bound vars excluded from result" $ do
      let refA = typeRef 71 "a"
          refB = typeRef 72 "b"
          refC = typeRef 73 "c"
      freeTypeVarRefsFrom [refA, refB] (TArrow (tVarWithRef refA) (tVarWithRef refC))
        `shouldBe` [refC]

    it "returns empty when all vars are bound via forall plus bound refs" $ do
      let refA = typeRef 74 "a"
      freeTypeVarRefsFrom [refA] (tForallWithRef refA Nothing (tVarWithRef refA))
        `shouldBe` []

    it "respects forall binders on top of bound refs" $ do
      let refA = typeRef 75 "a"
          refB = typeRef 76 "b"
      freeTypeVarRefsFrom [refA, refB] (tForallWithRef refA Nothing (TArrow (tVarWithRef refA) (tVarWithRef refB)))
        `shouldBe` []

  describe "substTypeCaptureRef" $ do
    it "substitutes a matching variable" $ do
      let refA = typeRef 81 "a"
      substTypeCaptureRef refA intTy (tVarWithRef refA)
        `shouldBe` intTy

    it "leaves a different variable untouched" $ do
      let refA = typeRef 82 "a"
          refB = typeRef 83 "b"
      substTypeCaptureRef refA intTy (tVarWithRef refB)
        `shouldBe` tVarWithRef refB

    it "keeps same-spelled replacement free when identities differ" $ do
      let refA = typeRef 84 "a"
          refB = typeRef 85 "b"
          replacement = typeRef 86 "b"
          result =
            substTypeCaptureRef
              refA
              (tVarWithRef replacement)
              (tForallWithRef refB Nothing (tVarWithRef refA))
       in do
            freeTypeVarRefsType result `shouldBe` [replacement]
            result `shouldBe` tForallWithRef refB Nothing (tVarWithRef replacement)

    it "preserves identity refs on untouched binders and heads" $
      let refA = typeRef 1 "a"
          refF = typeRef 2 "f"
          refZ = typeRef 87 "z"
          ty :: ElabType
          ty =
            tForallWithRef
              refA
              Nothing
              (tVarAppWithRef refF (tVarWithRef refA NE.:| []))
       in substTypeCaptureRef refZ intTy ty `shouldBe` ty

    it "preserves binder identity when capture freshening renames display name" $
      let refA = typeRef 88 "a"
          refB = typeRef 3 "b"
          ty :: ElabType
          ty = tForallWithRef refB Nothing (tVarWithRef refA)
          result = substTypeCaptureRef refA (tVarWithRef refB) ty
       in case result of
            TForallRef ref' Nothing (TVarRef bodyRef) -> do
              typeBinderRefIdentity ref' `shouldBe` typeBinderRefIdentity refB
              typeBinderRefName ref' `shouldBe` "b1"
              bodyRef `shouldBe` refB
            other -> expectationFailure ("expected freshened forall, got: " ++ show other)

    it "does not freshen same-named binders with different identities" $
      let target = typeRef 36 "x"
          binder = typeRef 37 "a"
          replacement = typeRef 38 "a"
          ty = tForallWithRef binder Nothing (tVarWithRef target)
       in substTypeCaptureRef target (tVarWithRef replacement) ty
            `shouldBe` tForallWithRef binder Nothing (tVarWithRef replacement)

    it "does not substitute a same-named variable with a different identity" $
      let refA = typeRef 31 "a"
          refB = typeRef 32 "a"
          ty = tVarWithRef refB
       in substTypeCaptureRef refA intTy ty `shouldBe` ty

  describe "substTypeSimpleRef" $ do
    it "substitutes a matching variable" $ do
      let refA = typeRef 89 "a"
      substTypeSimpleRef refA intTy (tVarWithRef refA)
        `shouldBe` intTy

    it "leaves a different variable untouched" $ do
      let refA = typeRef 90 "a"
          refB = typeRef 91 "b"
      substTypeSimpleRef refA intTy (tVarWithRef refB)
        `shouldBe` tVarWithRef refB

    it "does NOT rename binders (may capture)" $
      let refA = typeRef 92 "a"
          refB = typeRef 93 "b"
          result =
            substTypeSimpleRef
              refA
              (tVarWithRef refB)
              (tForallWithRef refB Nothing (tVarWithRef refA))
       in result `shouldBe` tForallWithRef refB Nothing (tVarWithRef refB)

    it "preserves identity refs on untouched binders and heads" $
      let refA = typeRef 4 "a"
          refF = typeRef 5 "f"
          refZ = typeRef 94 "z"
          ty :: ElabType
          ty =
            tForallWithRef
              refA
              Nothing
              (tVarAppWithRef refF (tVarWithRef refA NE.:| []))
       in substTypeSimpleRef refZ intTy ty `shouldBe` ty

    it "does not substitute a same-named variable with a different identity through a ref target" $
      let refA = typeRef 33 "a"
          refB = typeRef 34 "a"
       in substTypeSimpleRef refA intTy (tVarWithRef refB)
            `shouldBe` tVarWithRef refB

    it "substitutes a variable with the same identity after a display rename" $
      let refA = typeRef 35 "a"
          refA' = typeRef 35 "a1"
       in substTypeSimpleRef refA intTy (tVarWithRef refA')
            `shouldBe` intTy

  describe "inlineAliasBoundsWithBySeen" $ do
    it "preserves identity refs while walking untouched types" $
      let refA = typeRef 6 "a"
          refF = typeRef 7 "f"
          refM = typeRef 8 "m"
          ty :: ElabType
          ty =
            tForallWithRef
              refA
              (Just (tVarAppWithRef refF (tVarWithRef refA NE.:| [])))
              (tMuWithRef refM (tVarWithRef refA))
          result =
            inlineAliasBoundsWithBySeen
              False
              id
              (NodeMap mempty)
              (const Nothing)
              (\_ _ -> Left ())
              ty
       in result `shouldBe` ty

    it "does not shadow same-named free refs while inlining aliases" $
      let boundRef = typeRef 106 "t9"
          freeRef = typeRef 9 "t9"
          ty = tForallWithRef boundRef Nothing (tVarWithRef freeRef)
          nodes = fromListNode [(NodeId 9, TyBase (NodeId 9) (BaseTy "int"))]
          result =
            inlineAliasBoundsWithBySeen
              False
              id
              nodes
              (const Nothing)
              (\_ nid -> if nid == NodeId 9 then Right intTy else Left ())
              ty
       in result `shouldBe` tForallWithRef boundRef Nothing intTy

    it "does not inline identity-bearing alias refs by stale parsed names" $
      let staleRef = typeRef 109 "t9"
          ty = tVarWithRef staleRef
          nodes = fromListNode [(NodeId 9, TyBase (NodeId 9) (BaseTy "int"))]
          result =
            inlineAliasBoundsWithBySeen
              False
              id
              nodes
              (const Nothing)
              (\_ nid -> if nid == NodeId 9 then Right intTy else Left ())
              ty
       in result `shouldBe` ty

    it "does not inline generated identity alias refs by parsed names" $
      let staleRef = generatedTypeRef 991601 "t9"
          ty = tVarWithRef staleRef
          nodes = fromListNode [(NodeId 9, TyBase (NodeId 9) (BaseTy "int"))]
          result =
            inlineAliasBoundsWithBySeen
              False
              id
              nodes
              (const Nothing)
              (\_ nid -> if nid == NodeId 9 then Right intTy else Left ())
              ty
       in result `shouldBe` ty

  describe "inlineBaseBoundsType" $ do
    it "preserves identity refs when no base bound is inlined" $
      let refA = typeRef 9 "a"
          refF = typeRef 10 "f"
          refM = typeRef 11 "m"
          ty :: ElabType
          ty =
            tForallWithRef
              refA
              (Just (tVarAppWithRef refF (tVarWithRef refA NE.:| [])))
              (tMuWithRef refM (tVarWithRef refA))
       in inlineBaseBoundsType emptyConstraint id ty `shouldBe` ty

    it "inlines base bounds by ref identity instead of binder spelling" $
      let boundRef = typeRef 109 "t9"
          freeRef = typeRef 9 "t9"
          ty = tForallWithRef boundRef Nothing (TArrow (tVarWithRef boundRef) (tVarWithRef freeRef))
          constraint =
            emptyConstraint
              { cNodes = fromListNode [(NodeId 9, TyBase (NodeId 9) (BaseTy "int"))]
              }
       in inlineBaseBoundsType constraint id ty
            `shouldBe` tForallWithRef boundRef Nothing (TArrow (tVarWithRef boundRef) intTy)

  describe "alphaEqType" $ do
    it "recognises equal variables" $
      alphaEqType (testTVar "a") (testTVar "a") `shouldBe` True

    it "distinguishes different variables" $
      alphaEqType (testTVar "a") (testTVar "b") `shouldBe` False

    it "recognises alpha-equivalent foralls" $
      alphaEqType
        (testTForall "a" Nothing (testTVar "a"))
        (testTForall "b" Nothing (testTVar "b"))
        `shouldBe` True

    it "rejects non-alpha-equivalent foralls" $
      alphaEqType
        (testTForall "a" Nothing (testTVar "a"))
        (testTForall "a" Nothing (testTVar "b"))
        `shouldBe` False

    it "distinguishes same-named free variables with different identities" $
      let refA = typeRef 41 "a"
          refB = typeRef 42 "a"
       in alphaEqType (tVarWithRef refA) (tVarWithRef refB)
            `shouldBe` False

    it "recognises alpha-equivalent bound variables by binder position" $
      let refA = typeRef 43 "a"
          refB = typeRef 44 "b"
       in alphaEqType
            (tForallWithRef refA Nothing (tVarWithRef refA))
            (tForallWithRef refB Nothing (tVarWithRef refB))
            `shouldBe` True

  describe "matchTypeRefs" $ do
    it "matches a pattern variable against a concrete type" $ do
      let refA = typeRef 61 "a"
      matchTypeRefs [refA] (tVarWithRef refA) intTy
        `shouldBe` Right (Map.singleton refA intTy)

    it "rejects a structural mismatch" $ do
      let refA = typeRef 62 "a"
          refB = typeRef 63 "b"
      case matchTypeRefs [] (TArrow (tVarWithRef refA) (tVarWithRef refB)) intTy of
        Left _ -> pure ()
        Right s -> expectationFailure ("Expected mismatch, got: " ++ show s)

    it "matches multiple pattern variables in an arrow" $
      let refA = typeRef 64 "a"
          refB = typeRef 65 "b"
          pat = TArrow (tVarWithRef refA) (tVarWithRef refB)
          target = TArrow intTy boolTy
       in matchTypeRefs [refA, refB] pat target
            `shouldBe` Right (Map.fromList [(refA, intTy), (refB, boolTy)])

    it "matches ref targets after display renames by identity" $
      let refA = typeRef 66 "a"
          refA' = typeRef 66 "a1"
       in matchTypeRefs [refA] (tVarWithRef refA') intTy
            `shouldBe` Right (Map.singleton refA intTy)

    it "does not match same-named variables with different identities" $
      let refA = typeRef 67 "a"
          refB = typeRef 68 "a"
       in case matchTypeRefs [refA] (tVarWithRef refB) intTy of
            Left _ -> pure ()
            Right subst -> expectationFailure ("Expected identity mismatch, got: " ++ show subst)

  describe "firstNonContractiveRecursiveType" $ do
    it "returns Nothing for a type without TMu" $
      firstNonContractiveRecursiveType (TArrow (testTVar "a") (testTVar "b"))
        `shouldBe` Nothing

    it "returns Nothing for a contractive TMu" $
      firstNonContractiveRecursiveType (testTMu "a" (TArrow (testTVar "a") (testTVar "a")))
        `shouldBe` Nothing

    it "returns the non-contractive TMu" $
      firstNonContractiveRecursiveType (testTMu "a" (testTVar "a"))
        `shouldBe` Just (testTMu "a" (testTVar "a"))

    it "ignores same-named non-recursive variables with different identities" $
      let self = typeRef 51 "a"
          other = typeRef 52 "a"
       in firstNonContractiveRecursiveType (tMuWithRef self (tVarWithRef other))
            `shouldBe` Nothing

  describe "parseNameId" $ do
    it "parses t42" $
      parseNameId "t42" `shouldBe` Just 42

    it "rejects non-t-prefixed name" $
      parseNameId "abc" `shouldBe` Nothing

    it "parses t0" $
      parseNameId "t0" `shouldBe` Just 0
