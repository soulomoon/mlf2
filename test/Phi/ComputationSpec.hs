module Phi.ComputationSpec (spec) where

import Test.Hspec

import qualified ElabTypeTestSupport as TestElab
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Elab.Phi.Computation
import MLF.Elab.Types
  ( ElabType,
    Instantiation (..),
    TypeBinderRef,
    Ty (..),
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
  )
import SpecUtil (requireRight)

spec :: Spec
spec = describe "paper-shaped Phi computations" $ do
  describe "component endpoint validation" $ do
    it "constructs a quantifier reordering only after its endpoint checks" $ do
      let source = intTy
          target = forallType 7001 "a" intTy
      reordering <-
        requireRight
          (mkQuantifierReordering source InstIntro target)
      quantifierReorderingSource reordering `shouldBe` source
      quantifierReorderingInstantiation reordering `shouldBe` InstIntro
      quantifierReorderingTarget reordering `shouldBe` target

    it "constructs an edge translation only after its endpoint checks" $ do
      let source = forallType 7002 "a" intTy
      translation <-
        requireRight
          (mkEdgeTranslation source InstElim intTy)
      edgeTranslationSource translation `shouldBe` source
      edgeTranslationInstantiation translation `shouldBe` InstElim
      edgeTranslationTarget translation `shouldBe` intTy

    it "rejects a computation that cannot apply to its source" $ do
      case mkEdgeTranslation intTy (InstApp boolTy) boolTy of
        Left (PhiComputationApplicationFailed role source inst _) -> do
          role `shouldBe` EdgeTranslationRole
          source `shouldBe` intTy
          inst `shouldBe` InstApp boolTy
        Left other ->
          expectationFailure ("unexpected validation error: " ++ show other)
        Right _ ->
          expectationFailure "expected an inapplicable edge translation to fail"

    it "rejects a computation whose declared destination is wrong" $ do
      let sourceRef = binderRef 7003 "a"
          source = TForallRef sourceRef Nothing (TVarRef sourceRef)
      case mkEdgeTranslation source (InstApp intTy) boolTy of
        Left (PhiComputationEndpointMismatch role actual expected) -> do
          role `shouldBe` EdgeTranslationRole
          actual `shouldBe` intTy
          expected `shouldBe` boolTy
        Left other ->
          expectationFailure ("unexpected validation error: " ++ show other)
        Right _ ->
          expectationFailure "expected a mismatched edge destination to fail"

  describe "occurrence composition" $ do
    it "composes phi_R with T(e) and retains both validated parts" $ do
      let sharedIdentity = typeBinderIdentityFromNode (NodeId 7010)
          reorderingTarget =
            TForallRef
              (typeBinderRefFromIdentity sharedIdentity "a")
              Nothing
              intTy
          edgeSource =
            TForallRef
              (typeBinderRefFromIdentity sharedIdentity "stale-display-name")
              Nothing
              intTy
      reordering <-
        requireRight
          (mkQuantifierReordering intTy InstIntro reorderingTarget)
      translation <-
        requireRight
          (mkEdgeTranslation edgeSource InstElim intTy)
      occurrence <-
        requireRight
          (composeOccurrenceComputation reordering translation)

      occurrenceComputationSource occurrence `shouldBe` intTy
      occurrenceComputationInstantiation occurrence
        `shouldBe` InstSeq InstIntro InstElim
      occurrenceComputationTarget occurrence `shouldBe` intTy
      quantifierReorderingTarget
        (occurrenceComputationReordering occurrence)
        `shouldBe` reorderingTarget
      edgeTranslationSource
        (occurrenceComputationEdgeTranslation occurrence)
        `shouldBe` edgeSource

    it "rejects an alpha-equivalent seam with different binder identities" $ do
      let reorderingTarget = forallType 7020 "a" intTy
          edgeSource = forallType 7021 "a" intTy
      reordering <-
        requireRight
          (mkQuantifierReordering intTy InstIntro reorderingTarget)
      translation <-
        requireRight
          (mkEdgeTranslation edgeSource InstElim intTy)

      case composeOccurrenceComputation reordering translation of
        Left (PhiComputationSeamMismatch actualTarget actualSource) -> do
          actualTarget `shouldBe` reorderingTarget
          actualSource `shouldBe` edgeSource
        Left other ->
          expectationFailure ("unexpected validation error: " ++ show other)
        Right _ ->
          expectationFailure
            "expected distinct binder identities at the Phi seam to be rejected"

intTy :: ElabType
intTy = TestElab.tBase (BaseTy "Int")

boolTy :: ElabType
boolTy = TestElab.tBase (BaseTy "Bool")

forallType :: Int -> String -> ElabType -> ElabType
forallType key name body =
  TForallRef (binderRef key name) Nothing body

binderRef :: Int -> String -> TypeBinderRef
binderRef key name =
  typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name
