{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thesis.ObligationPropertySpec (spec) where

import IdentityTestSupport
import qualified ElabTypeTestSupport as TestElab
import Control.Monad (foldM, forM_)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import MLF.Binding.GraphOps qualified as GraphOps
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity (AcyclicityResult (..))
import MLF.Constraint.Inert qualified as Inert
import MLF.Constraint.Presolution
  ( EdgeTrace (..),
    PresolutionError (..),
    PresolutionView (..),
    prEdgeWitnesses,
    prConstraint,
  )
import MLF.Constraint.Presolution.TestSupport
  ( CopyMapping (..),
    OmegaNormalizeEnv (..),
    OmegaNormalizeError,
    PresolutionState (..),
    coalesceRaiseMergeWithEnv,
    normalizeInstanceOpsFull,
    psEdgeTraces,
    reorderWeakenWithEnv,
    decideMinimalExpansion,
    sourceInteriorFromList,
    instantiateScheme,
    instantiateSchemeWithTrace,
    lookupCopy,
    processInstEdge,
    runPresolutionM,
    unifyStructureForTest,
    unifyAcyclic,
    validateNormalizedWitness,
    validateTranslatablePresolution,
  )
import MLF.Constraint.Solve (frWith)
import MLF.Constraint.Solve.TestSupport (SolveResult (..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Unify.Decompose (decomposeUnifyChildren)
import ElabTermTestSupport (generatedResolvedLocal, mkTestDeferredVar, mkTestLocalLam, mkTestLocalLet, mkTestTyAbs, testTForall, testTVar)
import MLF.Elab.Pipeline qualified as Elab
import MLF.Elab.Run.Pipeline.TestSupport qualified as PipelineTest
import MLF.Elab.Phi.TestSupport qualified as PhiTestSupport
import MLF.Elab.Types qualified as ElabTypes
import MLF.Frontend.ConstraintGen
  ( ConstraintError,
    ConstraintResult (..),
    generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply,
  )
import MLF.Frontend.Program.Builtins qualified as Builtins
import MLF.Frontend.Syntax qualified as Surf
import MLF.Reify.TypeOps qualified as TypeOps
import MLF.Types.Identity
  ( StructuralTypeBinderRole (StructuralSelfBinder),
    TypeBinderIdentity,
    UniqueIdentity (..),
    initialIdentityGenerator,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
  )
import Presolution.Util (mkNormalizeConstraint, mkNormalizeEnv)
import SpecUtil
  ( PipelineArtifacts (..),
    bindParentsFromPairs,
    checkAcyclicityRaw,
    defaultTraceConfig,
    emptyConstraint,
    nodeMapFromList,
    normalizeRaw,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionDefault,
    rootedConstraint,
    solveUnifyRaw,
    unsafeNormalizeExpr,
  )
import Test.Hspec
import Test.QuickCheck

normalizeInstanceOpsForTest
  :: OmegaNormalizeEnv p
  -> [InstanceOp]
  -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsForTest env ops =
  getValidatedInstanceOps <$> normalizeInstanceOpsFull env ops

spec :: Spec
spec = do
  describe "Thesis obligation property evidence" $
    forM_ obligations $ \case
      FixedObligation obligationId fixedEvidence ->
        it obligationId $
          property $
            once (fixedEvidence 0)
      SizedObligation obligationId sizedEvidence ->
        it obligationId $
          property $
            withMaxSuccess 100 $
              forAll (chooseInt (3, 16)) $ \size ->
                counterexample (obligationId ++ " failed at size " ++ show size) $
                  sizedEvidence size

  describe "Thesis fixed annotation evidence" $ do
    forM_ (zip [1 :: Int ..] annotationErasureCases) $ \(caseIndex, expr) ->
      it ("preserves annotation erasure case " ++ show caseIndex) $
        expectElabAnnotationErasure expr
    it "constructs a bounded annotation abstraction" $
      expectElabBoundedAnnotationAbs
    it "constructs the paper's mixed existential/universal annotation" $
      expectElabMixedAnnotation
    it "keeps a nested mixed annotation local to its let RHS" $
      expectNestedMixedAnnotationLocal
    it "constructs the paper's annotated self-application" $
      expectElabAnnotatedSelfApp
    it "constructs the paper's annotated self-application as a direct identity argument" $
      expectElabAnnotationErasureAtType annotatedSelfAppType
        annotatedSelfAppThroughDirectIdentityApplicationExpr
    it "keeps an annotated lambda closed under an applied unused outer lambda" $
      expectElabAnnotationErasure nestedAnnotatedLambdaUnderUnusedApplicationExpr
    it "keeps a mixed annotation closed under an applied unused outer lambda" $
      expectElabAnnotationErasure mixedAnnotationUnderUnusedApplicationExpr
    it "keeps a mixed annotation nested beneath applied ground wrappers" $
      expectElabAnnotationErasure
        mixedAnnotationBeneathAppliedGroundWrappersExpr
    it "keeps a bounded annotation through an applied annotated unused lambda" $
      expectElabAnnotationErasure boundedAnnotationUnderAppliedAnnotatedLambdaExpr
    it "keeps a bounded annotation through nested unused constructions" $
      expectElabAnnotationErasure boundedAnnotationThroughNestedUnusedConstructionsExpr
    it "keeps annotated self-application typable under an unused outer lambda" $
      expectElabAnnotationErasure nestedAnnotatedSelfAppUnderUnusedLambdaExpr
    it "keeps annotated self-application typable under an applied annotated outer lambda" $
      expectElabAnnotationErasure nestedAnnotatedSelfAppUnderAppliedAnnotatedLambdaExpr
    it "retains the paper g g packet binder through an annotated application and unused let" $
      expectElabAnnotationErasure
        annotatedSelfAppThroughAnnotatedApplicationAndUnusedLetExpr
    it "keeps annotated self-application typable under an applied unused outer lambda" $
      expectElabAnnotationErasure nestedAnnotatedSelfAppUnderAppliedUnusedLambdaExpr
    it "keeps an applied annotated self-application typable through a let result" $
      expectElabAnnotationErasure appliedAnnotatedSelfAppLetResultExpr
    it "keeps an applied annotated self-application typable after an unused let" $
      expectElabAnnotationErasure appliedAnnotatedSelfAppAfterUnusedLetExpr
    it "keeps annotated self-application typable through nested let and application owners" $
      expectElabAnnotationErasure nestedAnnotatedSelfAppThroughLetAndApplicationOwnersExpr
    it "keeps annotated self-application source identities through nested unused applications" $
      expectElabAnnotationErasure annotatedSelfAppThroughNestedUnusedApplicationsExpr
    it "keeps an annotated lambda construction through identity application" $
      expectElabAnnotationErasure annotatedLambdaThroughIdentityApplicationExpr
    it "keeps a let-wrapped annotated lambda through identity application" $
      expectElabAnnotationErasure letWrappedAnnotatedLambdaThroughIdentityApplicationExpr
    it "keeps an identity-wrapped annotated lambda under an outer application" $
      expectElabAnnotationErasure identityWrappedAnnotatedLambdaUnderOuterApplicationExpr
    it "keeps a nested mixed annotation through enclosing unused constructions" $
      expectElabAnnotationErasure nestedMixedAnnotationThroughUnusedConstructionsExpr
    it "keeps an existential mixed annotation through a returned lambda and identity application" $
      expectElabAnnotationErasure mixedAnnotationThroughReturnedLambdaIdentityExpr
    it "generalizes an unused lambda parameter through nested annotated lets" $
      expectElabAnnotationErasure nestedAnnotatedLetLambdaRoundTripExpr
    it "specializes a bounded application result through nested let round trips" $
      expectElabAnnotationErasure nestedAnnotatedPolymorphicLambdaRoundTripExpr
    it "keeps a mixed annotation through nested annotated applications" $
      expectElabAnnotationErasure mixedAnnotationThroughNestedAnnotatedApplicationsExpr
    it "keeps nested annotated applications through an outer lambda and let" $
      expectElabAnnotationErasure nestedAnnotatedApplicationsThroughOuterConstructionsExpr
    it "uses the direct application endpoint for an identity-wrapped ground let result" $
      expectElabAnnotationErasure identityWrappedGroundLetResultExpr
    it "keeps a polymorphic annotation through three unused lambdas" $
      expectElabAnnotationErasure annotatedIdentityThroughThreeUnusedLambdasExpr
    it "keeps source-projected annotation binders ordered inside a ground-result lambda" $
      expectElabAnnotationErasure sourceProjectedAnnotationInGroundResultLambdaExpr
    it "generalizes an identity-applied mixed annotation at its let boundary" $
      expectElabAnnotationErasure identityAppliedMixedAnnotationLetExpr
    it "preserves an identity-applied mixed annotation under an unused lambda" $
      expectElabAnnotationErasure identityAppliedMixedAnnotationLetUnderUnusedLambdaExpr
    it "preserves a bounded annotation returned by an unused application in a let" $
      expectElabAnnotationErasure boundedAnnotationFromUnusedApplicationLetExpr
    it "preserves that bounded let through an enclosing unused application" $
      expectElabAnnotationErasure boundedAnnotationLetUnderUnusedApplicationExpr
    it "preserves that bounded let through identity application" $
      expectElabAnnotationErasure identityAppliedBoundedAnnotationLetExpr
    it "preserves that identity-applied bounded let through an annotated application" $
      expectElabAnnotationErasure annotatedApplicationAroundBoundedAnnotationLetExpr
    it "lets one application Gamma closure cover its owned requirement subset" $
      expectElabAnnotationErasure sharedApplicationGammaClosureExpr
    it "keeps a forwarded ground application Gamma at its source occurrence" $
      expectElabAnnotationErasure forwardedGroundApplicationGammaExpr
    it "generalizes an annotation existential through nested lambda packet preparation" $
      expectElabAnnotationErasure mixedAnnotationThroughNestedLambdaPacketExpr
    it "closes a let-bound annotation existential beneath nested unused lambdas" $
      expectElabAnnotationErasure letBoundMixedAnnotationThroughUnusedLambdasExpr
    it "instantiates both binders of the let-bound mixed annotation" $
      expectElabAnnotationErasure appliedMixedAnnotationLetExpr
    it "keeps a descendant application Gamma distinct through nested application owners" $
      expectElabAnnotationErasure
        nestedApplicationOwnersAroundAppliedMixedAnnotationExpr
    it "constructs nested unused lambdas around an applied polymorphic result" $
      expectElabAnnotationErasure nestedUnusedLambdasAroundAppliedPolymorphicResultExpr
    it "constructs an application certificate's free ambient at the root" $
      expectElabAnnotationErasure rootAmbientApplicationCertificateExpr
    it "completes an identity application's direct Gamma from its checked argument" $
      expectElabAnnotationErasure identityApplicationWithNestedAnnotatedUseExpr
    it "keeps independent descendant application Gammas separate at a let owner" $
      expectElabAnnotationErasure independentApplicationGammasAtLetOwnerExpr
    it "routes an annotated polymorphic parameter through enclosing unused owners" $
      expectElabAnnotationErasure annotatedPolymorphicParameterThroughUnusedOwnersExpr
    it "selects an annotated polymorphic parameter body's exact endpoint through applied owners" $
      expectElabAnnotationErasure
        annotatedPolymorphicParameterBodyEndpointThroughAppliedOwnersExpr
    it "keeps an annotated polymorphic parameter through a partially applied four-lambda spine" $
      expectElabAnnotationErasure
        annotatedPolymorphicParameterThroughPartiallyAppliedFourLambdaSpineExpr
    it "keeps a higher-rank parameter binder local through identity application" $
      expectElabAnnotationErasure
        higherRankParameterThroughIdentityApplicationExpr
    it "constructs distinct nested forall scopes for an identity endpoint through applied wrappers" $
      expectElabAnnotationErasure
        higherRankIdentityEndpointThroughAppliedWrappersExpr
    it "keeps a bounded annotation local through an identity-applied lambda let" $
      expectElabAnnotationErasure
        boundedAnnotationThroughIdentityAppliedLambdaLetExpr
    it "retains a term-used root binder through nested identity lets" $
      expectElabAnnotationErasure termUsedRootBinderThroughNestedIdentityLetsExpr
    it "constructs a multi-use annotated let beneath an unused lambda" $
      expectElabAnnotationErasure multiUseAnnotationUnderUnusedLambdaLetExpr
    it "completes a bounded annotation through nested unused and annotated applications" $
      expectElabAnnotationErasure boundedAnnotationThroughNestedApplicationOwnersExpr
    it "selects the exact specialization of a vacuous lambda body endpoint" $
      expectElabAnnotationErasure vacuousLambdaBodyEndpointThroughNestedOwnersExpr
    it "carries an identity-applied multi-use annotation through a let and unused application" $
      expectElabAnnotationErasure identityAppliedMultiUseAnnotationThroughUnusedApplicationExpr
    it "closes a polymorphic let dependency through an identity-applied nested lambda" $
      expectElabAnnotationErasure
        polymorphicLetDependencyThroughIdentityAppliedNestedLambdaExpr
    it "carries an annotated identity application through nested let and unused application owners" $
      expectElabAnnotationErasure annotatedIdentityApplicationThroughUnusedApplicationExpr
    it "retains a mixed-annotation root RaiseMerge through an unused application let" $
      expectElabAnnotationErasure mixedAnnotationRootRaiseMergeUnderUnusedApplicationLetExpr
    it "keeps nested exact lambda binder spines through ground wrappers" $
      expectElabAnnotationErasure nestedExactLambdaBinderSpineThroughGroundWrappersExpr
    it "aligns root and packet result routes through identity-applied annotated lets" $
      expectElabAnnotationErasure rootPacketRouteThroughIdentityAppliedAnnotatedLetExpr
    it "restores an administrative lambda parameter beneath a nested source forall" $
      expectElabAnnotationErasure administrativeLambdaParameterUnderSourceForallExpr
    it "specializes a completed forall packet at its enclosing lambda consumer" $
      expectElabAnnotationErasure completedForallPacketAtEnclosingLambdaConsumerExpr
    it "reorders graph-owned body foralls without source-order sidecars" $
      expectElabAnnotationErasure graphOwnedBodyForallsWithoutSourceOrderExpr
    it "keeps distinct let-consumer bounds that share a graph exterior" $
      expectElabAnnotationErasure distinctLetConsumerBoundsAtSharedExteriorExpr
    it "constructs one final let Gamma for vacuous shared packet consumers" $
      expectElabAnnotationErasure
        vacuousSharedPacketConsumersAtFinalLetGammaExpr
    it "completes a lambda consumer through its certified source occurrence route" $
      expectElabAnnotationErasure lambdaConsumerThroughSourceOccurrenceRouteExpr
    it "retains application Gamma authority for a constructed binder" $
      expectElabAnnotationErasure constructedApplicationGammaBinderAuthorityExpr
    it "completes a nested lambda consumer after annotated parameter application" $
      expectElabAnnotationErasure
        nestedLambdaConsumerAfterAnnotatedParameterApplicationExpr
    it "constructs a nested lambda result through its exact enclosing bound" $
      expectElabAnnotationErasure nestedLambdaResultAtExactEnclosingBoundExpr
    it "closes an application Gamma dependency through an annotated let result" $
      expectElabAnnotationErasure
        applicationGammaDependencyThroughAnnotatedLetResultExpr
    it "prepares a topology consumer through its exact enclosing route" $
      expectElabAnnotationErasure
        topologyConsumerThroughExactEnclosingRouteExpr
    it "closes a restored annotation binder during subterm generalization" $
      expectElabAnnotationErasure
        restoredAnnotationBinderDuringSubtermGeneralizationExpr
    it "closes an applied mixed annotation beneath nested unused lambdas" $
      expectElabAnnotationErasure
        appliedMixedAnnotationBeneathNestedUnusedLambdasExpr
    it "keeps independent Gamma endpoints distinct at a nested lambda owner" $
      expectElabAnnotationErasure
        independentGammaEndpointsAtNestedLambdaOwnerExpr
    it "discharges a vacuous enclosing consumer around an annotated let result" $
      expectElabAnnotationErasure
        vacuousEnclosingConsumerAroundAnnotatedLetResultExpr
    it "completes a local topology result through an identity-applied higher-order lambda" $
      expectElabAnnotationErasure
        localTopologyResultThroughIdentityAppliedHigherOrderLambdaExpr
    it "publishes application Gamma order for an identity lambda parameter" $
      expectElabAnnotationErasure
        applicationGammaOrderForIdentityLambdaParameterExpr
    it "retains the paper g g topology result through nested applied wrappers" $
      expectElabAnnotationErasure
        paperSelfApplicationThroughNestedAppliedWrappersExpr
    it "returns a let-bound paper g g lambda through nested lambda owners" $
      expectElabAnnotationErasure
        letBoundPaperSelfApplicationThroughNestedLambdaOwnersExpr
    it "constructs paper g g through an applied annotated lambda and nested lets" $
      expectElabAnnotationErasure
        paperSelfApplicationThroughAppliedAnnotatedLambdaAndNestedLetsExpr
    it "constructs a source-owned application Gamma without post-hoc closure" $
      expectElabAnnotationErasure
        sourceOwnedApplicationGammaConstructionExpr
    it "closes a mixed annotation through the complete nested owner chain" $
      expectElabAnnotationErasure
        deepMixedAnnotationOwnerClosureExpr
    it "constructs paper g g beneath nested lambda owners" $
      expectElabAnnotationErasure
        paperSelfApplicationBeneathNestedLambdaOwnersExpr
    it "specializes an owner-final bounded declaration at let publication" $
      expectElabAnnotationErasure
        ownerFinalBoundedPublicationExpr
    it "preserves a bounded annotation returned through an identity-applied nested lambda" $
      expectElabAnnotationErasure
        boundedAnnotationThroughIdentityAppliedNestedLambdaExpr
    it "keeps a consumed application Gamma disjoint from ambient annotation binders" $
      expectElabAnnotationErasure
        consumedApplicationGammaBesideAmbientAnnotationExpr
    it "returns paper g g through an identity application and nested lets" $
      expectElabAnnotationErasure
        paperSelfApplicationThroughIdentityAndNestedLetsExpr
    it "places paper g g topology through an identity-applied let chain" $
      expectElabAnnotationErasureAtType annotatedSelfAppType
        paperSelfApplicationThroughIdentityAppliedLetChainExpr
    it "preserves paper g g's principal type through an identity-applied unused let" $
      expectElabAnnotationErasureAtType annotatedSelfAppType
        paperSelfApplicationThroughIdentityAppliedUnusedLetExpr
    it "preserves paper g g's principal type through an unused let" $
      expectElabAnnotationErasureAtType annotatedSelfAppType
        paperSelfApplicationThroughUnusedLetExpr
    it "retains nested lambda packet consumers after applying an unused parameter" $
      expectElabAnnotationErasure
        nestedLambdaPacketAfterUnusedParameterApplicationExpr
    it "prepares a root packet around a nested mixed-annotation application" $
      expectElabAnnotationErasure
        rootPacketAroundNestedMixedAnnotationApplicationExpr
    it "constructs paper g g beneath nested applied unused lambdas" $
      expectElabAnnotationErasure
        paperSelfApplicationBeneathNestedAppliedUnusedLambdasExpr
    it "constructs paper g g beneath an applied ground wrapper" $
      expectElabAnnotationErasure
        paperSelfApplicationBeneathAppliedGroundWrapperExpr
    it "constructs paper g g beneath five applied unused lambdas and a let owner" $
      expectElabAnnotationErasure
        paperSelfApplicationBeneathFiveAppliedUnusedLambdasAndLetExpr
    it "closes a consumed bounded result through applied and root owners" $
      expectElabAnnotationErasure
        consumedBoundedResultThroughAppliedAndRootOwnersExpr
    it "advances a zero-local application from its completed mixed bound" $
      expectElabAnnotationErasure
        zeroLocalApplicationWithCompletedMixedBoundExpr
    it "publishes a source-renamed local application binder as exact Gamma authority" $
      expectElabAnnotationErasure
        sourceRenamedLocalApplicationBinderAuthorityExpr
    it "consumes a pending root declaration after its owner finalizes it" $
      expectElabAnnotationErasure
        consumedPendingRootAfterOwnerFinalizationExpr
    it "generalizes an unused lambda parameter inside an applied annotated lambda" $
      expectElabAnnotationErasure
        unusedLambdaParameterInsideAppliedAnnotatedLambdaExpr
    it "routes an applied lambda codomain through its selected reify root" $
      expectElabAnnotationErasure
        appliedLambdaCodomainThroughSelectedReifyRootExpr
    it "keeps a future owner-emitted refinement out of ambient Gamma" $
      expectElabAnnotationErasure
        futureOwnerRefinementOutsideAmbientGammaExpr
    it "closes a mixed annotation through a nested lambda let result" $
      expectElabAnnotationErasure
        mixedAnnotationThroughNestedLambdaLetResultExpr
    it "closes a mixed annotation through nested identity applications" $
      expectElabAnnotationErasure
        mixedAnnotationThroughNestedIdentityApplicationsExpr
    it "keeps a mixed annotation beneath an applied five-lambda spine" $
      expectElabAnnotationErasure
        mixedAnnotationBeneathAppliedFiveLambdaSpineExpr
    it "keeps a polymorphic let ambient through nested applied lambda owners" $
      expectElabAnnotationErasure
        polymorphicLetAmbientThroughNestedAppliedLambdaOwnersExpr
    it "keeps an annotated identity through nested identity and applied owners" $
      expectElabAnnotationErasure
        annotatedIdentityThroughNestedIdentityAndAppliedOwnersExpr
    it "keeps a mixed annotation through nested let and identity owners" $
      expectElabAnnotationErasure
        mixedAnnotationThroughNestedLetAndIdentityOwnersExpr
    it "returns a bounded identity lambda through an unused application and nested lets" $
      expectElabAnnotationErasure
        boundedIdentityLambdaThroughUnusedApplicationAndNestedLetsExpr
    it "returns a bounded identity lambda from an unused application" $
      expectElabAnnotationErasure
        boundedIdentityLambdaFromUnusedApplicationExpr
    it "returns a bounded identity lambda through an ignored let in an applied lambda" $
      expectElabAnnotationErasure
        boundedIdentityLambdaThroughIgnoredLetApplicationExpr
    it "opens an annotated parameter placeholder at its exact lambda owner" $
      expectElabAnnotationErasure
        annotatedParameterPlaceholderAtExactLambdaOwnerExpr
    it "eliminates a vacuous root binder before publishing a lambda codomain" $
      expectElabAnnotationErasure
        vacuousRootBinderBeforeLambdaCodomainExpr
    it "closes a bounded lambda result before an enclosing identity application" $
      expectElabAnnotationErasure
        boundedLambdaResultBeforeEnclosingIdentityApplicationExpr
    it "keeps root-owned lambda binders outside a transparent let owner" $
      expectElabAnnotationErasure
        rootOwnedLambdaBindersOutsideTransparentLetExpr
    it "carries an identity topology consumer through nested applied lambdas" $
      expectElabAnnotationErasure
        identityTopologyConsumerThroughNestedAppliedLambdasExpr
    it "deduplicates a consumer between a declaration and its own bound" $
      expectElabAnnotationErasure
        duplicateConsumerClosureInsideOwnBoundExpr
    it "orders returned-lambda binders without a synthetic dependency cycle" $
      expectElabAnnotationErasure
        returnedLambdaWithoutSyntheticBinderCycleExpr
    it "constructs a returned higher-rank lambda through annotated applications" $
      expectElabAnnotationErasure
        returnedHigherRankLambdaThroughAnnotatedApplicationsExpr
    it "generalizes a returned free annotation binder at its enclosing lambda" $
      expectElabAnnotationErasure
        returnedFreeAnnotationBinderAtEnclosingLambdaExpr
    it "returns annotated self-application through a transparent lambda let without a binder cycle" $
      expectElabAnnotationErasure
        annotatedSelfAppThroughTransparentLambdaLetExpr
    it "retains paper g g through an identity-applied nested lambda and lets" $
      expectElabAnnotationErasure
        annotatedSelfAppThroughIdentityAppliedNestedLambdaLetsExpr
    it "routes a bounded identity annotation through applied annotated lambdas" $
      expectElabAnnotationErasure
        boundedIdentityThroughAppliedAnnotatedLambdasExpr
    it "composes paper g g through an identity-applied opaque lambda result" $
      expectElabAnnotationErasure
        paperSelfApplicationThroughIdentityAppliedOpaqueLambdaExpr
    it "inherits coalesced Gamma closures through nested identity applications" $
      expectElabAnnotationErasure
        polymorphicIdentityThroughCoalescedGammaClosuresExpr
    it "retains paper g g beneath partially applied nested lambdas" $
      expectElabAnnotationErasure
        paperSelfApplicationBeneathPartiallyAppliedNestedLambdasExpr
    it "keeps a let-generalized mixed annotation closed through identity application" $
      expectElabAnnotationErasure
        letGeneralizedMixedAnnotationThroughIdentityApplicationExpr
    it "inherits a descendant application Gamma through coalesced identity applications" $
      expectElabAnnotationErasure
        descendantApplicationGammaThroughCoalescedIdentityApplicationsExpr
    it "projects a consumed root declaration through nested mixed-annotation owners" $
      expectElabAnnotationErasure
        consumedRootDeclarationThroughNestedMixedAnnotationOwnersExpr
    it "retains paper g g through a let-bound direct identity application" $
      expectElabAnnotationErasureAtType annotatedSelfAppType
        letBoundPaperSelfApplicationThroughDirectIdentityApplicationExpr
    it "keeps an identity-applied lambda Gamma bound through a mixed annotation" $
      expectElabAnnotationErasure
        identityAppliedLambdaWithMixedAnnotationExpr
    it "keeps a higher-rank lambda endpoint lexical through nested identity applications" $
      expectElabAnnotationErasure
        higherRankLambdaEndpointThroughNestedIdentityApplicationsExpr
    it "keeps an identity-wrapped mixed annotation through a partially applied lambda spine" $
      expectElabAnnotationErasure
        identityWrappedMixedAnnotationThroughPartiallyAppliedLambdaExpr
    it "aligns a mixed-annotation packet through nested identity and let owners" $
      expectElabAnnotationErasure
        mixedAnnotationPacketThroughNestedIdentityAndLetOwnersExpr
    it "constructs a ground annotation beneath nested let and lambda owners" $
      expectElabAnnotationErasure
        groundAnnotationThroughNestedLetAndLambdaOwnersExpr
    it "retains paper g g beneath an administrative body target" $
      expectElabAnnotationErasure
        paperSelfApplicationThroughAdministrativeBodyTargetExpr
    it "constructs an administrative arrow outside an inner lambda spine" $
      expectElabAnnotationErasure
        groundAnnotationThroughAdministrativeLambdaSpineExpr
    it "constructs nested administrative lambdas around a bounded higher-rank parameter" $
      expectElabAnnotationErasure
        boundedHigherRankParameterThroughAdministrativeLambdaSpineExpr
    it "aligns a shadowed bounded forall in an application argument" $
      expectElabAnnotationErasure
        shadowedBoundedForallThroughApplicationArgumentExpr
    it "preserves source forall order through a direct identity application" $
      expectElabAnnotationErasure
        sourceForallOrderThroughDirectIdentityApplicationExpr
    it "completes source forall order through an applied lambda and identity" $
      expectElabAnnotationErasure
        sourceForallOrderThroughAppliedLambdaAndIdentityExpr
    it "specializes a bounded forall beneath applied unused lambda results" $
      expectElabAnnotationErasure
        boundedForallThroughAppliedUnusedLambdaResultsExpr
    it "keeps an identity argument principal over a provisional result specialization" $
      expectElabAnnotationErasure
        identityArgumentPrincipalOverProvisionalResultExpr
    it "reinstalls a paper g g RHS ambient construction at let publication" $
      expectElabAnnotationErasure
        paperSelfApplicationAmbientConstructionAtLetPublicationExpr
    it "constructs identity application over a ground-annotated constant lambda" $
      expectElabAnnotationErasure
        identityApplicationOverGroundAnnotatedConstantLambdaExpr
    it "preserves an explicitly polymorphic identity through identity application and let" $
      expectElabAnnotationErasure
        polymorphicIdentityThroughIdentityApplicationAndLetExpr
    it "closes an applied mixed annotation through two unused lambda packets" $
      expectElabAnnotationErasure
        appliedMixedAnnotationThroughTwoUnusedLambdasExpr
    it "closes that nested result through an applied ground-annotated lambda" $
      expectElabAnnotationErasure
        appliedMixedAnnotationThroughGroundAnnotatedApplicationExpr
    it "closes that applied result under an unused lambda and annotated let" $
      expectElabAnnotationErasure
        appliedMixedAnnotationUnderUnusedLambdaAndAnnotatedLetExpr
    it "keeps that closure stable under an outer ignored let" $
      expectElabAnnotationErasure
        appliedMixedAnnotationUnderOuterIgnoredLetExpr
    it "freshens a source-polymorphic let occurrence beneath an applied annotated lambda" $
      expectElabAnnotationErasure
        sourcePolymorphicLetOccurrenceUnderAppliedAnnotatedLambdaExpr
    it "specializes a bounded source annotation beneath nested lambda construction" $
      expectElabAnnotationErasure
        boundedSourceAnnotationUnderNestedLambdaConstructionExpr
    it "retains the generalized completion shared by sibling consumer edges" $
      expectElabAnnotationErasure
        generalizedCompletionAcrossSiblingConsumerEdgesExpr
    it "keeps inherited Gamma dependencies ambient in a completed packet bound" $
      expectElabAnnotationErasure
        inheritedGammaDependencyInCompletedPacketBoundExpr
    it "publishes root RaiseMerge order for a completed local binder" $
      expectElabAnnotationErasure
        rootRaiseMergeCompletedBinderOrderExpr
    it "derives local Gamma dependencies from the completed bound" $
      expectElabAnnotationErasure
        locallyCompletedGammaDependencyExpr
    it "opens vacuous forall binders while completing a nested lambda spine" $
      expectElabAnnotationErasure
        vacuousForallThroughNestedLambdaSpineExpr
    it "coalesces one construction consumer across a nested lower bound" $
      expectElabAnnotationErasure
        sharedConstructionConsumerAcrossNestedBoundExpr
    it "validates direct application Gamma in its source construction domain" $
      expectElabAnnotationErasure
        boundedAnnotationThroughDirectApplicationGammaExpr
    it "keeps child source forall payloads aligned at enclosing lambda completion" $
      expectElabAnnotationErasure
        sourceForallPayloadThroughEnclosingLambdaCompletionExpr
    it "keeps annotation-owned forall abstract beneath constructed lambdas" $
      expectElabAnnotationErasure
        annotationForallBeneathConstructedLambdasExpr
    it "closes a direct identity-application Gamma bound at its annotated forall" $
      expectElabAnnotationErasure
        annotatedForallThroughDirectIdentityApplicationExpr
    it "keeps direct application Gamma routes unique through an identity-applied let result" $
      expectElabAnnotationErasure
        annotatedForallThroughIdentityAppliedLetResultExpr
    it "keeps a returned polymorphic let abstraction closed through an annotated applied lambda" $
      expectElabAnnotationErasure
        annotatedForallThroughAnnotatedAppliedLambdaExpr
    it "keeps a mixed annotation closed through an applied nested lambda and transparent lets" $
      expectElabAnnotationErasure
        mixedAnnotationThroughAppliedNestedLambdaLetChainExpr
    it "generalizes a specialized pending owner bound through nested applied lambdas" $
      expectElabAnnotationErasure
        specializedPendingOwnerBoundThroughNestedApplicationsExpr
    it "keeps a nested identity application's direct endpoint through an outer identity application" $
      expectElabAnnotationErasure
        nestedIdentityApplicationThroughOuterIdentityExpr
    it "closes a pending owner bound over its free lambda parameter" $
      expectElabAnnotationErasure
        pendingOwnerClosureOverFreeLambdaParameterExpr
    it "retains a nested lambda dependency while preparing a mixed-annotation packet" $
      expectElabAnnotationErasure
        nestedLambdaDependencyInMixedAnnotationPacketExpr
    it "constructs a let-used mixed annotation through identity application" $
      expectElabAnnotationErasure
        mixedAnnotationLetUseThroughIdentityApplicationExpr
    it "constructs a returned mixed-annotation let through two identity applications" $
      expectElabAnnotationErasure
        returnedMixedAnnotationLetThroughTwoIdentityApplicationsExpr
    it "constructs a ground annotation through nested applied lambda consumers" $
      expectElabAnnotationErasure
        groundAnnotationThroughNestedAppliedLambdaConsumersExpr
    it "constructs a ground annotation through a partially applied lambda spine" $
      expectElabAnnotationErasure
        groundAnnotationThroughPartiallyAppliedLambdaSpineExpr
    it "constructs annotated self-application through an applied let and identity chain" $
      expectElabAnnotationErasure
        annotatedSelfApplicationThroughAppliedLetIdentityChainExpr
    it "constructs annotated self-application through nested annotated applications and a let" $
      expectElabAnnotationErasure
        annotatedSelfApplicationThroughNestedAnnotatedApplicationsExpr
    it "constructs paper g g through nested identity arguments beneath applied annotated lambdas" $
      expectElabAnnotationErasure
        paperGgThroughNestedIdentityArgumentsBeneathAnnotatedLambdasExpr
    it "returns a let-bound bounded identity through an unapplied lambda beneath an applied annotation" $
      expectElabAnnotationErasure
        letBoundBoundedIdentityThroughNestedLambdaExpr
    it "returns that bounded identity through two unapplied lambdas" $
      expectElabAnnotationErasure
        letBoundBoundedIdentityThroughTwoNestedLambdasExpr
    it "returns that bounded identity through an ignored let and one unapplied lambda" $
      expectElabAnnotationErasure
        letBoundBoundedIdentityThroughIgnoredLetAndNestedLambdaExpr
    it "returns that bounded identity through an ignored let and two unapplied lambdas" $
      expectElabAnnotationErasure
        letBoundBoundedIdentityThroughIgnoredLetAndTwoNestedLambdasExpr
    it "returns an annotated identity through a nested applied let chain" $
      expectElabAnnotationErasure
        annotatedIdentityThroughNestedAppliedLetChainExpr
    it "constructs a ground identity application through nested lambdas and annotated lets" $
      expectElabAnnotationErasure
        groundIdentityApplicationThroughNestedLambdasExpr
    it "returns paper g g through nested transparent lets and identity applications" $
      expectElabAnnotationErasure
        paperGgThroughNestedTransparentLetsAndIdentityApplicationsExpr
    it "specializes a mixed-annotation constant through a partially applied lambda spine" $
      expectElabAnnotationErasure
        mixedAnnotationConstantThroughPartiallyAppliedLambdaSpineExpr
    it "advances a prepared source bound through its checked body completion" $
      expectElabAnnotationErasure
        preparedSourceBoundThroughCheckedBodyCompletionExpr
    it "opens a prepared application Gamma along an exact forwarded edge" $
      expectElabAnnotationErasure
        preparedApplicationGammaAlongForwardedEdgeExpr
    it "carries a let-aliased paper g g result through an applied lambda" $
      expectElabAnnotationErasure
        letAliasedPaperGgThroughAppliedLambdaResultExpr
    it "specializes a mixed annotation through nested applied lambda owners" $
      expectElabAnnotationErasure
        mixedAnnotationThroughNestedAppliedLambdaOwnersExpr
    it "retains paper g g Gamma through a direct lambda application" $
      expectElabAnnotationErasure
        paperGgGammaThroughDirectLambdaApplicationExpr
    it "authorizes an application Gamma alias through its owner-final route" $
      expectElabAnnotationErasure
        applicationGammaAliasThroughOwnerFinalRouteExpr
    it "keeps a source-owned let scheme lexical beneath an application Gamma" $
      expectElabAnnotationErasure
        sourceOwnedLetSchemeBeneathApplicationGammaExpr
    it "constructs paper g g beneath an applied annotated lambda" $
      expectElabAnnotationErasure
        paperGgBeneathAppliedAnnotatedLambdaExpr
    it "constructs a mixed-annotation let result through an applied annotated lambda" $
      expectElabAnnotationErasure
        mixedAnnotationLetThroughAppliedAnnotatedLambdaExpr
    it "completes a returned polymorphic parameter before publishing its application owner" $
      expectElabAnnotationErasure
        returnedPolymorphicParameterThroughApplicationOwnerExpr
    it "constructs identity-applied paper g g through an applied outer lambda" $
      expectElabAnnotationErasure
        identityAppliedPaperGgThroughAppliedOuterLambdaExpr
    it "completes a nested returned application result before outer publication" $
      expectElabAnnotationErasure
        nestedReturnedApplicationResultBeforeOuterPublicationExpr
    it "carries paper g g through an applied lambda result owner chain" $
      expectElabAnnotationErasure
        paperGgThroughAppliedLambdaResultOwnerChainExpr
    it "completes a returned higher-rank application result before enclosing lambda Gamma" $
      expectElabAnnotationErasure
        returnedHigherRankApplicationBeforeEnclosingLambdaGammaExpr
    it "keeps a returned higher-rank parameter beneath ignored application and let publication" $
      expectElabAnnotationErasure
        returnedHigherRankParameterBeneathIgnoredApplicationExpr
    it "keeps repeated source occurrences distinct beneath partially applied wrappers" $
      expectElabAnnotationErasure
        repeatedSourceOccurrencesBeneathPartialWrappersExpr
    it "constructs a completed returned-function bound beneath nested value lambdas" $
      expectElabAnnotationErasure
        completedReturnedFunctionBoundBeneathNestedLambdasExpr
    it "keeps a multi-use polymorphic let lexical through returned lambda owners" $
      expectElabAnnotationErasure
        multiUsePolymorphicLetThroughReturnedLambdaOwnersExpr
    it "constructs paper g g through nested annotated applications and transparent lets" $
      expectElabAnnotationErasure
        paperGgThroughNestedAnnotatedApplicationsAndTransparentLetsExpr
    it "selects the initial generalized consumer through nested applied wrappers" $
      expectElabAnnotationErasure
        polymorphicLambdaApplicationThroughNestedAppliedWrappersExpr
    it "shares a mixed-annotation closure across forwarded and direct application edges" $
      expectElabAnnotationErasure
        mixedAnnotationThroughForwardedAndDirectApplicationEdgesExpr
    it "closes an exact parameter beneath an unrelated Gamma forall" $
      expectElabAnnotationErasure
        exactParameterBeneathUnrelatedGammaForallExpr
    it "closes a direct ambient source root at its checked parameter endpoint" $
      expectElabAnnotationErasure
        directAmbientSourceRootAtCheckedParameterEndpointExpr
    it "retains a bounded declaration after the body has already applied Hyp" $
      expectElabAnnotationErasure
        boundedDeclarationAlreadyAbstractedByCheckedBodyExpr
    it "commutes a source-bounded forall across an applied value lambda" $
      expectElabAnnotationErasure
        sourceBoundedForallAcrossAppliedLambdaExpr
    it "preserves an implicitly generalized annotation identity through an applied value lambda" $
      expectElabAnnotationErasure
        implicitlyGeneralizedAnnotationThroughAppliedLambdaExpr
    it "forwards a completed constant-function result through an identity application" $
      expectElabAnnotationErasure
        completedConstantFunctionThroughIdentityApplicationExpr
    it "carries a returned polymorphic let through a root identity application" $
      expectElabAnnotationErasure
        returnedPolymorphicLetThroughRootIdentityApplicationExpr
    it "preserves a completed result bound through a partially applied lambda spine" $
      expectElabAnnotationErasure
        completedResultBoundThroughPartiallyAppliedLambdaSpineExpr
    it "carries a child-constructed lambda tail through four enclosing packets" $
      expectElabAnnotationErasure
        childConstructedLambdaTailThroughFourPacketsExpr
    it "carries an implicit annotation binder through an annotated applied lambda" $
      expectElabAnnotationErasure
        implicitAnnotationBinderThroughAnnotatedAppliedLambdaExpr
    it "retains an enclosing source forall while placing a nested packet tail" $
      expectElabAnnotationErasure
        enclosingSourceForallWithNestedPacketTailExpr
    it "projects independently ordered source binders before placing a packet tail" $
      expectElabAnnotationErasure
        independentlyOrderedSourceBindersBeforePacketTailExpr
    it "carries paper g g through nested applied let publication" $
      expectElabAnnotationErasure
        paperGgThroughNestedAppliedLetPublicationExpr
    it "closes a source existential after transparent root applications" $
      expectElabAnnotationErasure
        sourceExistentialAfterTransparentRootApplicationsExpr
    it "carries a bounded source declaration through an applied lambda spine" $
      expectElabAnnotationErasure
        boundedSourceDeclarationThroughAppliedLambdaSpineExpr
    it "coalesces a direct identity endpoint with its child owner endpoint" $
      expectElabAnnotationErasure
        directIdentityEndpointWithChildOwnerEndpointExpr
    it "carries a checked child result plan through an annotated applied lambda" $
      expectElabAnnotationErasure
        checkedChildResultPlanThroughAnnotatedAppliedLambdaExpr
    it "carries paper g g through an applied lambda and transparent lets" $
      expectElabAnnotationErasure
        paperGgThroughAppliedLambdaAndTransparentLetsExpr
    it "advances a completed body-consumer bound before entering administrative Gamma" $
      expectElabAnnotationErasure
        completedBodyConsumerBoundBeforeAdministrativeGammaExpr
    it "constructs a specialized polymorphic parameter beneath an ignored applied lambda" $
      expectElabAnnotationErasure
        specializedPolymorphicParameterBeneathIgnoredApplicationExpr
    it "carries paper g g through a nested application argument Gamma" $
      expectElabAnnotationErasure
        paperGgThroughNestedApplicationArgumentGammaExpr
    it "constructs an ambient application result at its exact function endpoint" $
      expectElabAnnotationErasure
        ambientApplicationResultAtExactFunctionEndpointExpr
    it "carries an annotated constant result through an applied let body" $
      expectElabAnnotationErasure
        annotatedConstantResultThroughAppliedLetBodyExpr
    it "carries one implicit annotation binder through an identity argument" $
      expectElabAnnotationErasure
        implicitAnnotationBinderThroughIdentityArgumentExpr
    it "carries an applied annotated lambda result through an identity argument" $
      expectElabAnnotationErasure
        appliedAnnotatedLambdaResultThroughIdentityArgumentExpr
    it "carries an annotated polymorphic argument through nested identity applications" $
      expectElabAnnotationErasure
        annotatedPolymorphicArgumentThroughNestedIdentityApplicationsExpr
    it "carries paper g g through nested discarded applications" $
      expectElabAnnotationErasure
        paperGgThroughNestedDiscardedApplicationsExpr
    it "carries a returned Bool construction through an applied lambda result binder" $
      expectElabAnnotationErasure
        returnedBoolThroughAppliedLambdaResultBinderExpr
    it "keeps a child forall distinct from an enclosing applied-lambda Gamma" $
      expectElabAnnotationErasure
        childForallThroughEnclosingAppliedLambdaGammaExpr
    it "keeps a returned annotated lambda above its body result packet" $
      expectElabAnnotationErasure
        returnedAnnotatedLambdaAboveBodyResultPacketExpr
    it "keeps a consumed implicit annotation binder out of an enclosing lambda result" $
      expectElabAnnotationErasure
        consumedImplicitBinderOutsideEnclosingLambdaResultExpr
    it "retains a source declaration outside its constructed RaiseMerge consumer bound" $
      expectElabAnnotationErasure
        sourceDeclarationOutsideConstructedRaiseMergeBoundExpr
    it "preserves a source bound while an enclosing application specializes its consumer" $
      expectElabAnnotationErasure
        sourceBoundThroughEnclosingApplicationSpecializationExpr
    it "retains a child source declaration in enclosing lambda generalization" $
      expectElabAnnotationErasure
        childSourceDeclarationThroughEnclosingLambdaGeneralizationExpr
    it "projects a consumed child declaration through a returned let scheme" $
      expectElabAnnotationErasure
        consumedChildDeclarationThroughReturnedLetSchemeExpr
    it "completes an application result before enclosing lambda type checking" $
      expectElabAnnotationErasure
        completedApplicationResultThroughEnclosingLambdasExpr
    it "keeps a source-polymorphic result local through an outer application" $
      expectElabAnnotationErasure
        sourcePolymorphicResultThroughOuterApplicationExpr
    it "completes an application body result through its enclosing lambda packet" $
      expectElabAnnotationErasure
        completedApplicationBodyThroughLambdaPacketExpr
    it "carries a polymorphic result through nested applied annotated parameters" $
      expectElabAnnotationErasure
        polymorphicResultThroughAppliedAnnotatedParametersExpr
    it "carries a multi-use polymorphic let through nested applied annotated parameters" $
      expectElabAnnotationErasure
        multiUsePolymorphicLetThroughAppliedAnnotatedParametersExpr
    it "publishes paper g g through partially applied annotated parameters" $
      expectElabAnnotationErasure
        paperGgThroughPartiallyAppliedAnnotatedParametersExpr
    it "plans an annotated ground result through nested identity applications" $
      expectElabAnnotationErasure
        annotatedGroundResultThroughNestedIdentityApplicationsExpr
    it "carries paper g g through an applied let returning a nested lambda" $
      expectElabAnnotationErasure
        paperGgThroughAppliedLetReturningNestedLambdaExpr
    it "carries a bounded identity annotation through nested application Gamma" $
      expectElabAnnotationErasure
        boundedIdentityAnnotationThroughNestedApplicationGammaExpr
    it "carries a returned polymorphic annotation through nested lambda result packets" $
      expectElabAnnotationErasure
        returnedPolymorphicAnnotationThroughNestedLambdaResultPacketsExpr
    it "routes an implicit source dependency through a returned lambda consumer" $
      expectElabAnnotationErasure
        implicitSourceDependencyThroughReturnedLambdaConsumerExpr
    it "publishes paper g g through nested ground applications and identity" $
      expectElabAnnotationErasure
        paperGgThroughNestedGroundApplicationsAndIdentityExpr
    it "coalesces a let result variable with its later ground application endpoints" $
      expectElabAnnotationErasure
        letResultVariableThroughGroundApplicationEndpointsExpr
    it "returns an identity-applied higher-rank lambda through applied wrappers" $
      expectElabAnnotationErasure
        identityAppliedHigherRankLambdaThroughAppliedWrappersExpr
    it "retains an annotated identity ambient through unused and identity applications" $
      expectElabAnnotationErasure
        annotatedIdentityAmbientThroughUnusedAndIdentityApplicationsExpr
    it "returns an annotated ground seed through identity and applied wrappers" $
      expectElabAnnotationErasure
        annotatedGroundSeedThroughIdentityAndAppliedWrappersExpr
    it "retains an annotated identity through discarded applied lambdas" $
      expectElabAnnotationErasure
        annotatedIdentityThroughDiscardedAppliedLambdasExpr
    it "returns paper g g through an applied nested lambda and let owner" $
      expectElabAnnotationErasure
        paperGgThroughAppliedNestedLambdaAndLetOwnerExpr
    it "returns paper g g through identity and annotated applications" $
      expectElabAnnotationErasure
        paperGgThroughIdentityAndAnnotatedApplicationsExpr
    it "returns a multi-use polymorphic let through an applied lambda owner" $
      expectElabAnnotationErasure
        multiUsePolymorphicLetThroughAppliedLambdaOwnerExpr
    it "returns an annotated higher-rank parameter through application owners" $
      expectElabAnnotationErasure
        annotatedHigherRankParameterThroughApplicationOwnersExpr
    it "returns paper g g through an unapplied lambda beneath an annotated application" $
      expectElabAnnotationErasure
        paperGgThroughUnappliedLambdaBeneathAnnotatedApplicationExpr
    it "returns that lambda through an applied annotated let" $
      expectElabAnnotationErasure
        paperGgThroughAppliedAnnotatedLetAndUnappliedLambdaExpr
    it "returns that applied let through an outer identity application" $
      expectElabAnnotationErasure
        paperGgThroughReturnedLetUnderIdentityApplicationExpr
    it "composes sequential application Gamma bounds around paper g g" $
      expectElabAnnotationErasure
        paperGgThroughSequentialApplicationGammaExpr
    it "constructs paper g g through an identity-applied deep lambda spine" $
      expectElabAnnotationErasure
        paperGgThroughIdentityAppliedDeepLambdaSpineExpr
    it "preserves a source boundary around an applied paper g g lambda" $
      expectElabAnnotationErasure
        paperGgThroughAppliedSourceBoundaryExpr
    it "publishes an identity-applied paper g g lambda through a returned let" $
      expectElabAnnotationErasure
        paperGgThroughIdentityAppliedReturnedLetExpr
    it "returns an annotated ground identity application through nested lets" $
      expectElabAnnotationErasure
        annotatedGroundIdentityApplicationThroughNestedLetsExpr
    it "returns an annotated ground result through an applied lambda spine" $
      expectElabAnnotationErasure
        annotatedGroundResultThroughAppliedLambdaSpineExpr
    it "projects a consumed polymorphic bound through nested applied lambdas" $
      expectElabAnnotationErasure
        consumedPolymorphicBoundThroughNestedAppliedLambdasExpr
    it "coalesces sibling application Gamma bounds around returned paper g g" $
      expectElabAnnotationErasure
        paperGgThroughSharedNestedApplicationExteriorExpr
    it "coalesces identity and annotated application stages at a returned let" $
      expectElabAnnotationErasure
        annotatedApplicationThroughSharedReturnedLetExteriorExpr
    it "carries a higher-rank result through an applied identity spine" $
      expectElabAnnotationErasure
        higherRankResultThroughAppliedIdentitySpineExpr
    it "constructs paper g g through a direct identity application and let" $
      expectElabAnnotationErasure
        paperGgThroughDirectIdentityApplicationAndLetExpr
    it "returns paper g g through an annotated application and transparent lets" $
      expectElabAnnotationErasure
        paperGgThroughAnnotatedApplicationAndTransparentLetsExpr
    it "returns an annotated identity through a partially applied lambda spine" $
      expectElabAnnotationErasure
        annotatedIdentityThroughPartiallyAppliedLambdaSpineExpr
    it "returns paper g g through nested identities under an applied annotation" $
      expectElabAnnotationErasure
        paperGgThroughNestedIdentitiesUnderAppliedAnnotationExpr
    it "returns a bounded identity through nested applications under an applied annotation" $
      expectElabAnnotationErasure
        boundedIdentityThroughNestedApplicationsUnderAppliedAnnotationExpr
    it "carries a bounded identity through a partially constructed forall declaration" $
      expectElabAnnotationErasure
        boundedIdentityThroughPartiallyConstructedForallExpr
    it "closes a child-constructed forall spine beneath a partially applied lambda tail" $
      expectElabAnnotationErasure
        childConstructedForallSpineThroughPartiallyAppliedLambdaTailExpr
    it "generalizes a source-polymorphic let through an identity-applied nested lambda" $
      expectElabAnnotationErasure
        sourcePolymorphicLetThroughIdentityAppliedNestedLambdaExpr
    it "closes a mixed-annotation dependency through an identity-applied let alias" $
      expectElabAnnotationErasure
        mixedAnnotationDependencyThroughIdentityAppliedLetAliasExpr
    it "returns paper g g through an applied lambda beneath unused let owners" $
      expectElabAnnotationErasure
        paperGgThroughAppliedLambdaBeneathUnusedLetOwnersExpr
    it "coalesces sequential application Gamma states beneath a let publication" $
      expectElabAnnotationErasure
        paperGgThroughSequentialApplicationsBeneathLetPublicationExpr
    it "carries a specialized higher-rank parameter through nested owners" $
      expectElabAnnotationErasure
        specializedHigherRankParameterThroughNestedOwnersExpr
    it "retains a source forall in a root RaiseMerge bound" $
      expectElabAnnotationErasure
        sourceForallThroughRootRaiseMergeExpr
    it "retains a local consumer through nested applications around paper g g" $
      expectElabAnnotationErasure
        paperGgLocalConsumerThroughNestedApplicationsExpr
    it "completes a multi-use polymorphic result through nested identities" $
      expectElabAnnotationErasure
        multiUsePolymorphicResultThroughNestedIdentitiesExpr
    it "carries a mixed source declaration through nested lambda application" $
      expectElabAnnotationErasure
        mixedSourceDeclarationThroughNestedLambdaApplicationExpr
    it "advances paper g g through nested applied lambda owners" $
      expectElabAnnotationErasure
        paperGgThroughNestedAppliedLambdaOwnersExpr
    it "aligns a shared application closure through nested identity lets" $
      expectElabAnnotationErasure
        sharedApplicationClosureThroughNestedIdentityLetsExpr
    it "retains a constructed root bound beside an unbounded dependency" $
      expectElabAnnotationErasure
        constructedRootBoundBesideUnboundedDependencyExpr
    it "constructs a returned paper g g lambda through applied owners" $
      expectElabAnnotationErasure
        returnedPaperGgLambdaThroughAppliedOwnersExpr
    it "completes an unbounded enclosing consumer before wrapper publication" $
      expectElabAnnotationErasure
        unboundedEnclosingConsumerThroughAppliedWrappersExpr
    it "retains an opaque mixed-annotation result through application requirements" $
      expectElabAnnotationErasure
        opaqueMixedAnnotationResultThroughApplicationsExpr
    it "reorders a pending owner declaration before its terminal Hyp" $
      expectElabAnnotationErasure
        reorderedPendingOwnerDeclarationBeforeHypExpr
    it "selects a final let Gamma bound through a retained binder-spine coercion" $
      expectElabAnnotationErasure
        retainedBinderSpineAtFinalLetGammaExpr
    it "constructs returned paper g g through an administrative lambda packet" $
      expectElabAnnotationErasure
        returnedPaperGgThroughAdministrativeLambdaPacketExpr
    it "coalesces descendant paper g g bounds before an annotated outer application" $
      expectElabAnnotationErasure
        paperGgDescendantBoundsBeforeAnnotatedOuterApplicationExpr
    it "routes a nested mixed source declaration to its enclosing lambda packet" $
      expectElabAnnotationErasure
        nestedMixedSourceDeclarationAtEnclosingLambdaPacketExpr
    it "aligns a retained root consumer with its owner-constructed binder" $
      expectElabAnnotationErasure
        retainedRootConsumerAtOwnerConstructedBinderExpr
    it "completes a consumed RaiseMerge before root identity publication" $
      expectElabAnnotationErasure
        consumedRaiseMergeThroughRootIdentityApplicationExpr
    it "closes an opaque paper g g carrier through nested unused lambdas" $
      expectElabAnnotationErasure
        opaquePaperGgCarrierThroughNestedUnusedLambdasExpr
    it "routes a locally constructed Gamma dependency through a returned let" $
      expectElabAnnotationErasure
        locallyConstructedGammaDependencyThroughReturnedLetExpr
    it "constructs paper g g through nested annotated application owners" $
      expectElabAnnotationErasure
        paperGgThroughNestedAnnotatedApplicationOwnersExpr
    it "routes a returned mixed source parameter through nested applications" $
      expectElabAnnotationErasure
        returnedMixedSourceParameterThroughNestedApplicationsExpr
    it "completes a specialized higher-rank result before packet recovery" $
      expectElabAnnotationErasure
        specializedHigherRankResultBeforePacketRecoveryExpr
    it "completes a higher-rank result before an enclosing annotated application" $
      expectElabAnnotationErasure
        higherRankResultBeforeEnclosingAnnotatedApplicationExpr
    it "retains a bounded source declaration through an enclosing application" $
      expectElabAnnotationErasure
        boundedSourceDeclarationThroughEnclosingApplicationExpr
    it "routes a root RaiseMerge alias through nested annotated applications" $
      expectElabAnnotationErasure
        rootRaiseMergeAliasThroughNestedAnnotatedApplicationsExpr
    it "publishes paper g g through nested annotated applications and lambdas" $
      expectElabAnnotationErasure
        paperGgThroughNestedAnnotatedApplicationsAndLambdasExpr
    it "returns a bounded identity annotation through nested lambda applications" $
      expectElabAnnotationErasure
        boundedIdentityAnnotationThroughNestedLambdaApplicationsExpr
    it "confirms a polymorphic identity through a nested lambda result application" $
      expectElabAnnotationErasure
        polymorphicIdentityThroughNestedLambdaResultApplicationExpr
    it "carries a mixed annotation through identity and nested lambda applications" $
      expectElabAnnotationErasure
        mixedAnnotationThroughIdentityAndNestedLambdaApplicationsExpr
    it "returns a let-bound mixed annotation lambda through identity applications" $
      expectElabAnnotationErasure
        letBoundMixedAnnotationLambdaThroughIdentityApplicationsExpr
    it "publishes paper g g through unused lets and an identity application" $
      expectElabAnnotationErasure
        paperGgThroughUnusedLetsAndIdentityApplicationExpr
    it "constructs paper g g through nested annotated application results" $
      expectElabAnnotationErasure
        paperGgThroughNestedAnnotatedApplicationResultsExpr
    it "publishes paper g g from an annotated application through a let" $
      expectElabAnnotationErasure
        paperGgFromAnnotatedApplicationThroughLetExpr
    it "constructs an annotated constant result at an applied lambda boundary" $
      expectElabAnnotationErasure
        annotatedConstantResultAtAppliedLambdaBoundaryExpr
    it "selects the completed outer-let Gamma after nested paper g g" $
      expectElabAnnotationErasure
        completedOuterLetGammaAfterNestedPaperGgExpr
    it "freshens a returned polymorphic value before ambient specialization" $
      expectElabAnnotationErasure
        returnedPolymorphicValueBeforeAmbientSpecializationExpr
    it "routes a carried polymorphic result binder into root publication" $
      expectElabAnnotationErasure
        carriedPolymorphicResultBinderIntoRootPublicationExpr
    it "retains source order for a projected higher-rank parameter" $
      expectElabAnnotationErasure
        projectedHigherRankParameterConstructionOrderExpr
    it "advances a direct lambda's mixed result through application specialization" $
      expectElabAnnotationErasure
        directLambdaMixedResultThroughApplicationSpecializationExpr
    it "shares a completed mixed result across direct and forwarded closure edges" $
      expectElabAnnotationErasure
        completedMixedResultAcrossClosureEdgesExpr
    it "reconciles a returned source forall with its published graph identity" $
      expectElabAnnotationErasure
        returnedSourceForallThroughAppliedLambdaPublicationExpr
    it "consumes a completed nested-lambda consumer before the operated view" $
      expectElabAnnotationErasure
        completedNestedLambdaConsumerBeforeOperatedViewExpr
    it "freshens a nested annotation binder beside its outer publication" $
      expectElabAnnotationErasure
        nestedAnnotationBinderBesideOuterPublicationExpr
    it "carries a returned source forall through a refined lambda body" $
      expectElabAnnotationErasure
        returnedSourceForallThroughRefinedLambdaBodyExpr
    it "keeps paper g g lexical declarations distinct through nested owners" $
      expectElabAnnotationErasure
        annotatedSelfAppThroughNestedOwnersSeed314159Expr
    it "keeps an annotation forall local through nested application owners" $
      expectElabAnnotationErasure
        annotationForallLocalThroughNestedOwnersSeed314159Expr
    it "keeps an annotation forall local through direct lambda owners" $
      expectElabAnnotationErasure
        annotationForallLocalThroughDirectLambdaOwnersSeed314159Expr
    it "uses a lambda's boundary scope when its result node has nested scope candidates" $
      expectElabAnnotationErasure
        lambdaBoundaryScopeThroughNestedApplicationSeed123456789Expr
    it "constructs a let-returned bounded identity through an applied nested lambda" $
      expectElabAnnotationErasure
        letReturnedBoundedIdentityThroughAppliedNestedLambdaSeed123456789Expr
    it "constructs an annotated constant through a partially applied five-lambda spine" $
      expectElabAnnotationErasure
        annotatedConstantThroughPartiallyAppliedFiveLambdaSeed987654321Expr
    it "constructs paper g g through a let-returned two-lambda wrapper" $
      expectElabAnnotationErasure
        paperGgThroughLetReturnedTwoLambdaWrapperSeed987654321Expr
    it "orders local lambda Gamma through an applied four-lambda let result" $
      expectElabAnnotationErasure
        localLambdaGammaThroughAppliedFourLambdaSeed135791357Expr
    it "projects a consumed bounded identity through a returned lambda let" $
      expectElabAnnotationErasure
        consumedBoundedIdentityThroughReturnedLambdaLetSeed19088743Expr
    it "constructs a returned bounded identity lambda under an applied annotation" $
      expectElabAnnotationErasure
        returnedBoundedIdentityLambdaUnderAppliedAnnotationSeed2000000001Expr
    it "retains an implicit annotation binder through a descendant consumer" $
      expectElabAnnotationErasure
        implicitAnnotationBinderThroughDescendantConsumerSeed999999999Expr
    it "constructs a packet-owned result dependency inside an ambient lambda bound" $
      expectElabAnnotationErasure
        packetOwnedResultDependencyInsideAmbientLambdaBoundSeed999999999Expr
    it "discharges a nested mixed-annotation topology through an applied lambda" $
      expectElabAnnotationErasure
        nestedMixedAnnotationTopologyThroughAppliedLambdaSeed161803398Expr
    it "constructs a returned paper g g lambda through a partially applied annotated spine" $
      expectElabAnnotationErasure
        returnedPaperGgLambdaThroughPartiallyAppliedAnnotatedSpineSeed161803398Expr
    it "opens a consumed closed forall in its certified lexical copy" $
      expectElabAnnotationErasure
        consumedClosedForallAtCertifiedLexicalCopySeed937635187Expr
    it "retains a nested closed source dependency before a later lexical copy" $
      expectElabAnnotationErasure
        nestedClosedSourceDependencyBeforeLexicalCopySeed937635187Expr
    it "keeps exact child type abstractions closed during enclosing lambda generalization" $
      expectElabAnnotationErasure
        childTypeAbstractionsClosedDuringEnclosingLambdaSeed20260809Expr
    it "constructs returned polymorphism before let scheme publication" $
      expectElabAnnotationErasure
        returnedPolymorphismBeforeLetPublicationSeed449181304Expr
    it "records an explicit forall introduced by nested let publication" $
      expectElabAnnotationErasure
        explicitForallThroughNestedLetPublicationSeed1015Expr
    it "keeps paper g g closed through a partially applied triple lambda" $
      expectElabAnnotationErasure
        paperGgThroughPartiallyAppliedTripleLambdaSeed1021Expr
    it "carries pending-owner Gamma history through a wrapped paper g g" $
      expectElabAnnotationErasure
        pendingOwnerGammaHistoryThroughWrappedPaperGgSeed1022Expr
    it "constructs paper g g through an applied lambda with two returned parameters" $
      expectElabAnnotationErasure
        paperGgThroughAppliedLambdaAndTwoReturnedParametersSeed1022Expr
    it "commutes checked body foralls across an applied annotated lambda" $
      expectElabAnnotationErasure
        checkedBodyForallsAcrossAppliedAnnotatedLambdaSeed1023Expr
    it "freshens a paper g g parameter against its bounded result" $
      expectElabAnnotationErasure
        paperGgParameterAgainstBoundedResultSeed1024Expr
    it "retains a carried route through a polymorphic lambda argument" $
      expectElabAnnotationErasure
        carriedRouteThroughPolymorphicLambdaArgumentSeed1008Expr
    it "carries paper g g through a let-bound returned-lambda spine" $
      expectElabAnnotationErasure
        paperGgThroughLetBoundReturnedLambdaSpineSeed1008Expr
    it "publishes a bounded identity through an identity-returned lambda" $
      expectElabAnnotationErasure
        boundedIdentityThroughIdentityReturnedLambdaSeed2026081303Expr
    it "constructs an annotated returned lambda before application endpoint checking" $
      expectElabAnnotationErasure
        annotatedReturnedLambdaBeforeApplicationEndpointSeed2026081304Expr
    it "excludes a consumed descendant declaration before application Gamma refinement" $
      expectElabAnnotationErasure
        consumedDescendantBeforeApplicationGammaSeed1496664322Expr
    it "constructs a closed returned forall before sibling lambda publication" $
      expectElabAnnotationErasure
        closedReturnedForallBeforeSiblingLambdaPublicationSeed1987654321Expr
    it "keeps an identity-lambda forall copy erasure-free" $
      expectElabAnnotationErasure
        identityLambdaNestedForallCopySeed1006873496Expr
    it "carries identity sibling-copy provenance through let publication" $
      expectElabAnnotationErasure
        identitySiblingCopyThroughLetSeed1006873496Expr
    it "copies a consumed closed forall into distinct sibling bounds" $
      expectElabAnnotationErasure
        consumedClosedForallAcrossSiblingBoundsSeed1006873496Expr
    it "lets the current owner route shadow a carried result alias" $
      expectElabAnnotationErasure
        currentOwnerRouteShadowsCarriedAliasSeed145673209Expr
    it "closes a provisional application Gamma at its checked forall endpoint" $
      expectElabAnnotationErasure
        checkedForallEndpointClosesApplicationGammaSeed1907094151Expr
    it "constructs a returned paper g g domain beneath a source forall" $
      expectElabAnnotationErasure
        returnedPaperGgDomainUnderForallSeed1799129115Expr
    it "completes a future wrapper Gamma from its owner-final paper g g bound" $
      expectElabAnnotationErasure
        futureWrapperGammaCompletionSeed1936552889Expr
    it "carries a generalized paper g g ambient through ignored applications" $
      expectElabAnnotationErasure
        generalizedPaperGgAmbientThroughIgnoredApplicationsSeed669650106Expr
    it "aligns a nested identity annotation ambient through applied lambdas" $
      expectElabAnnotationErasure
        nestedIdentityAnnotationAmbientThroughAppliedLambdasSeed669650106Expr
    it "keeps a completed packet specialization at its selected lambda layer" $
      expectElabAnnotationErasure
        completedPacketSpecializationAtSelectedLambdaSeed839296932Expr
    it "routes a source dependency into a locally constructed root Gamma" $
      expectElabAnnotationErasure
        sourceDependencyInLocalRootGammaSeed839296932Expr
    it "keeps paper g g construction inside applied lambda wrappers" $
      expectElabAnnotationErasure
        paperGgInsideAppliedLambdaWrappersSeed839296932Expr

  describe "Thesis generated annotation evidence" $
    it "O15-ELAB-GENERATED: generated closed annotated programs elaborate, typecheck, and erase" $
      property $
        withMaxSuccess 100 $
          forAll genClosedWellTypedAnnotatedExpr $ \expr ->
            case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
              Left err ->
                counterexample
                  ( "generated annotated program failed to elaborate\nexpr: "
                      ++ show expr
                      ++ "\nerror: "
                      ++ Elab.renderPipelineError err
                  )
                  False
              Right (term, pipelineTy) ->
                conjoin
                  [ case Elab.typeCheck term of
                      Left err ->
                        counterexample
                          ( "generated elaboration failed xMLF typecheck\nexpr: "
                              ++ show expr
                              ++ "\nterm: "
                              ++ show term
                              ++ "\nerror: "
                              ++ show err
                          )
                          False
                      Right checkedTy ->
                        typeShouldMatch checkedTy pipelineTy,
                    eraseXmlfTerm term
                      === eraseSurfaceAnnotations expr
                  ]

data Obligation
  = FixedObligation String (Int -> Property)
  | SizedObligation String (Int -> Property)

obligations :: [Obligation]
obligations =
  [ FixedObligation "O14-WF-EMPTY" propWfEmpty,
    FixedObligation "O14-WF-TVAR" propWfTVar,
    FixedObligation "O14-WF-VAR" propWfVar,
    FixedObligation "O14-INST-REFLEX" propInstReflex,
    FixedObligation "O14-INST-TRANS" propInstTrans,
    FixedObligation "O14-INST-BOT" propInstBot,
    FixedObligation "O14-INST-HYP" propInstHyp,
    FixedObligation "O14-INST-INNER" propInstInner,
    FixedObligation "O14-INST-OUTER" propInstOuter,
    FixedObligation "O14-INST-QUANT-ELIM" propInstQuantElim,
    FixedObligation "O14-INST-QUANT-INTRO" propInstQuantIntro,
    FixedObligation "O14-T-VAR" propTypingVar,
    FixedObligation "O14-T-ABS" propTypingAbs,
    FixedObligation "O14-T-APP" propTypingApp,
    FixedObligation "O14-T-TABS" propTypingTAbs,
    FixedObligation "O14-T-TAPP" propTypingTApp,
    FixedObligation "O14-T-LET" propTypingLet,
    FixedObligation "O14-RED-BETA" propRedBeta,
    FixedObligation "O14-RED-BETALET" propRedBetaLet,
    FixedObligation "O14-RED-REFLEX" propRedReflex,
    FixedObligation "O14-RED-TRANS" propRedTrans,
    FixedObligation "O14-RED-QUANT-INTRO" propRedQuantIntro,
    FixedObligation "O14-RED-QUANT-ELIM" propRedQuantElim,
    FixedObligation "O14-RED-INNER" propRedInner,
    FixedObligation "O14-RED-OUTER" propRedOuter,
    FixedObligation "O14-RED-CONTEXT" propRedContext,
    FixedObligation "O14-APPLY-N" propApplyN,
    FixedObligation "O14-APPLY-O" propApplyO,
    FixedObligation "O14-APPLY-SEQ" propApplySeq,
    FixedObligation "O14-APPLY-INNER" propApplyInner,
    FixedObligation "O14-APPLY-OUTER" propApplyOuter,
    FixedObligation "O14-APPLY-HYP" propApplyHyp,
    FixedObligation "O14-APPLY-BOT" propApplyBot,
    FixedObligation "O14-APPLY-ID" propApplyId,
    SizedObligation "O15-TRANS-NO-INERT-LOCKED" propTransNoInertLocked,
    FixedObligation "O15-TRANS-SCHEME-ROOT-RIGID" propTransSchemeRootRigid,
    FixedObligation "O15-TRANS-ARROW-RIGID" propTransArrowRigid,
    FixedObligation "O15-TRANS-NON-INTERIOR-RIGID" propTransNonInteriorRigid,
    FixedObligation "O15-REORDER-REQUIRED" propSigmaReorderRequired,
    FixedObligation "O15-REORDER-IDENTITY" propSigmaReorderIdentity,
    SizedObligation "O15-CONTEXT-FIND" propContextFind,
    SizedObligation "O15-CONTEXT-REJECT" propContextReject,
    FixedObligation "O15-EDGE-TRANSLATION" propEdgeTranslation,
    FixedObligation "O15-ELAB-LAMBDA-VAR" propElabLambdaVar,
    FixedObligation "O15-ELAB-LET-VAR" propElabLetVar,
    FixedObligation "O15-ELAB-ABS" propElabAbs,
    FixedObligation "O15-ELAB-APP" propElabApp,
    FixedObligation "O15-ELAB-LET" propElabLet,
    FixedObligation "O15-ENV-LAMBDA" propEnvLambda,
    FixedObligation "O15-ENV-LET" propEnvLet,
    FixedObligation "O15-ENV-WF" propEnvWf,
    FixedObligation "O15-TR-SEQ-EMPTY" propTrSeqEmpty,
    FixedObligation "O15-TR-SEQ-CONS" propTrSeqCons,
    FixedObligation "O15-TR-RIGID-RAISE" propTrRigidRaise,
    FixedObligation "O15-TR-RIGID-MERGE" propTrRigidMerge,
    FixedObligation "O15-TR-RIGID-RAISEMERGE" propTrRigidRaiseMerge,
    FixedObligation "O15-TR-ROOT-GRAFT" propTrRootGraft,
    FixedObligation "O15-TR-ROOT-RAISEMERGE" propTrRootRaiseMerge,
    FixedObligation "O15-TR-ROOT-WEAKEN" propTrRootWeaken,
    FixedObligation "O15-TR-NODE-GRAFT" propTrNodeGraft,
    SizedObligation "O15-TR-NODE-MERGE" propTrNodeMerge,
    SizedObligation "O15-TR-NODE-RAISEMERGE" propTrNodeRaiseMerge,
    FixedObligation "O15-TR-NODE-WEAKEN" propTrNodeWeaken,
    FixedObligation "O15-TR-NODE-RAISE" propTrNodeRaise,
    FixedObligation "O04-BIND-FLEX-CHILDREN" propBindingFlexChildren,
    SizedObligation "O04-BIND-INTERIOR" propBindingInterior,
    SizedObligation "O04-BIND-ORDER" propBindingOrder,
    SizedObligation "O04-OP-WEAKEN" propGraphWeaken,
    SizedObligation "O04-OP-RAISE-STEP" propGraphRaiseStep,
    SizedObligation "O04-OP-RAISE-TO" propGraphRaiseTo,
    SizedObligation "O05-INERT-NODES" propInertNodes,
    SizedObligation "O05-INERT-LOCKED" propInertLocked,
    SizedObligation "O05-WEAKEN-INERT" propInertWeaken,
    SizedObligation "O07-UNIF-CORE" propUnifyCore,
    FixedObligation "O07-UNIF-PRESOL" propPresolutionUnify,
    SizedObligation "O07-REBIND" propRebindHarmonize,
    FixedObligation "O07-GENUNIF" propGeneralizedUnify,
    FixedObligation "O08-REIFY-TYPE" propReifyType,
    FixedObligation "O08-REIFY-NAMES" propReifyNames,
    FixedObligation "O08-BIND-MONO" propBindMono,
    SizedObligation "O08-SYN-TO-GRAPH" propSynToGraph,
    FixedObligation "O08-REIFY-INLINE" propReifyInline,
    FixedObligation "O08-INLINE-PRED" propInlinePred,
    FixedObligation "O09-CGEN-ROOT" propCgenRoot,
    FixedObligation "O09-CGEN-EXPR" propCgenExpr,
    SizedObligation "O10-EXP-DECIDE" propExpDecide,
    FixedObligation "O10-EXP-APPLY" propExpApply,
    FixedObligation "O10-PROP-SOLVE" propPropSolve,
    FixedObligation "O10-PROP-WITNESS" propPropWitness,
    SizedObligation "O10-COPY-SCHEME" propCopyScheme,
    SizedObligation "O11-UNIFY-STRUCT" propPresolutionUnifyStructure,
    SizedObligation "O11-WITNESS-NORM" propWitnessNorm,
    SizedObligation "O11-WITNESS-COALESCE" propWitnessCoalesce,
    SizedObligation "O11-WITNESS-REORDER" propWitnessReorder,
    FixedObligation "O12-SOLVE-UNIFY" propSolveVar,
    SizedObligation "O12-ACYCLIC-CHECK" propAcyclicCheck,
    SizedObligation "O12-ACYCLIC-TOPO" propAcyclicTopo,
    SizedObligation "O12-COPY-INST" propCopyInst,
    SizedObligation "O12-NORM-GRAFT" propNormGraft,
    SizedObligation "O12-NORM-MERGE" propNormMerge,
    SizedObligation "O12-NORM-DROP" propNormDrop,
    SizedObligation "O12-NORM-FIXPOINT" propNormFixpoint,
    FixedObligation "O12-SOLVE-VAR-BASE" propSolveVarBase,
    FixedObligation "O12-SOLVE-VAR-VAR" propSolveVarVar,
    FixedObligation "O12-SOLVE-HARMONIZE" propSolveHarmonize,
    FixedObligation "O12-SOLVE-ARROW" propSolveArrow,
    FixedObligation "O12-SOLVE-VALIDATE" propSolveValidate
  ]

propBindingFlexChildren :: Int -> Property
propBindingFlexChildren _size =
  let c = binderConstraint
   in case Binding.boundFlexChildren c (typeRef (NodeId 0)) of
        Right children -> counterexample (show children) (NodeId 1 `elem` children)
        Left err -> counterexample (show err) False

propBindingInterior :: Int -> Property
propBindingInterior size =
  let c = chainConstraint size
   in case Binding.interiorOf c (typeRef (NodeId 0)) of
        Right interior ->
          conjoin
            [ counterexample (show interior) (IntSet.member (nodeRefKey (typeRef (NodeId 0))) interior),
              counterexample (show interior) (IntSet.member (nodeRefKey (typeRef (NodeId 1))) interior)
            ]
        Left err -> counterexample (show err) False

propBindingOrder :: Int -> Property
propBindingOrder size =
  let (c, root, expected) = orderedBinderFixture size
   in case Binding.orderedBinders id c (typeRef root) of
        Right binders -> counterexample (show binders) (binders === expected)
        Left err -> counterexample (show err) False

propGraphWeaken :: Int -> Property
propGraphWeaken size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
   in case GraphOps.applyWeaken (TypeRefTag (NodeId (size - 1))) c of
        Right (c', _) ->
          conjoin
            [ Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (typeRef (NodeId (size - 2)), BindRigid)
            ]
        Left err -> counterexample (show err) False

propGraphRaiseStep :: Int -> Property
propGraphRaiseStep size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
      grandparent = typeRef (NodeId (size - 3))
   in case GraphOps.applyRaiseStep (TypeRefTag (NodeId (size - 1))) c of
        Right (c', Just _) ->
          conjoin
            [ Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (grandparent, BindFlex)
            ]
        other -> counterexample (show other) False

propGraphRaiseTo :: Int -> Property
propGraphRaiseTo size =
  let c = chainConstraint size
      nid = typeRef (NodeId (size - 1))
      target = typeRef (NodeId 0)
   in case GraphOps.applyRaiseTo (TypeRefTag (NodeId (size - 1))) target c of
        Right (c', ops) ->
          conjoin
            [ counterexample (show ops) (not (null ops)),
              Binding.checkBindingTree c' === Right (),
              Binding.lookupBindParent c' nid === Just (target, BindFlex)
            ]
        Left err -> counterexample (show err) False

propInertNodes :: Int -> Property
propInertNodes size =
  let c = inertConstraint size
   in case Inert.inertNodes c of
        Right nodes ->
          conjoin
            [ counterexample (show nodes) (not (IntSet.null nodes)),
              counterexample (show nodes) (IntSet.member 2 nodes)
            ]
        Left err -> counterexample (show err) False

propInertLocked :: Int -> Property
propInertLocked size =
  let c = inertConstraint size
   in case Inert.inertLockedNodes c of
        Right nodes -> counterexample (show nodes) (IntSet.member 2 nodes)
        Left err -> counterexample (show err) False

propInertWeaken :: Int -> Property
propInertWeaken size =
  let c = inertConstraint size
   in case Inert.weakenInertLockedNodes c of
        Right c' -> Inert.inertLockedNodes c' === Right IntSet.empty
        Left err -> counterexample (show err) False

propUnifyCore :: Int -> Property
propUnifyCore size =
  let lhs = TyArrow (NodeId 0) (NodeId 1) (NodeId 2)
      rhs = TyArrow (NodeId 3) (NodeId (size + 10)) (NodeId (size + 11))
   in decomposeUnifyChildren lhs rhs
        === Right [UnifyEdge (NodeId 1) (NodeId (size + 10)), UnifyEdge (NodeId 2) (NodeId (size + 11))]

propPresolutionUnifyStructure :: Int -> Property
propPresolutionUnifyStructure size =
  let base = max 3 size * 10
      root = NodeId 0
      leftArrow = NodeId (base + 1)
      leftDomain = NodeId (base + 2)
      leftCodomain = NodeId (base + 3)
      rightArrow = NodeId (base + 4)
      rightDomain = NodeId (base + 5)
      rightCodomain = NodeId (base + 6)
      c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId root, TestTyCon root (BaseTy "Pair") (leftArrow :| [rightArrow])),
                    (getNodeId leftArrow, TyArrow leftArrow leftDomain leftCodomain),
                    (getNodeId leftDomain, TyVar leftDomain Nothing),
                    (getNodeId leftCodomain, TyVar leftCodomain Nothing),
                    (getNodeId rightArrow, TyArrow rightArrow rightDomain rightCodomain),
                    (getNodeId rightDomain, TyVar rightDomain Nothing),
                    (getNodeId rightCodomain, TyVar rightCodomain Nothing)
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (leftArrow, root, BindFlex),
                    (leftDomain, leftArrow, BindFlex),
                    (leftCodomain, leftArrow, BindFlex),
                    (rightArrow, root, BindFlex),
                    (rightDomain, rightArrow, BindFlex),
                    (rightCodomain, rightArrow, BindFlex)
                  ]
            }
   in case runPresolutionM defaultTraceConfig (emptyPresolutionState c) (unifyStructureForTest leftArrow rightArrow) of
        Right ((), st) ->
          let canonical = canonicalPresolutionNode (psUnionFind st)
           in conjoin
                [ canonical leftArrow === canonical rightArrow,
                  canonical leftDomain === canonical rightDomain,
                  canonical leftCodomain === canonical rightCodomain
                ]
        Left err -> counterexample (show err) False

propSolveVar :: Int -> Property
propSolveVar _size =
  let c =
        varTripleConstraint
          { cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 3)]
          }
   in case solveUnifyRaw defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin
            [ cUnifyEdges solved === [],
              frWith uf (NodeId 1) === frWith uf (NodeId 3),
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propPresolutionUnify :: Int -> Property
propPresolutionUnify _size =
  let c = varTripleConstraint
      st0 = emptyPresolutionState c
   in case runPresolutionM defaultTraceConfig st0 (unifyAcyclic (NodeId 1) (NodeId 3)) of
        Right ((), st1) ->
          let uf = psUnionFind st1
              solved = psConstraint st1
           in conjoin
                [ frWith uf (NodeId 1) === frWith uf (NodeId 3),
                  Binding.checkBindingTree solved === Right ()
                ]
        Left err -> counterexample (show err) False

propSolveArrow :: Int -> Property
propSolveArrow _size =
  let c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (0, TestTyCon (NodeId 0) (BaseTy "Pair") (NodeId 1 :| [NodeId 4])),
                    (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
                    (2, TestTyBase (NodeId 2) (BaseTy "Int")),
                    (3, TestTyBase (NodeId 3) (BaseTy "Bool")),
                    (4, TyArrow (NodeId 4) (NodeId 5) (NodeId 6)),
                    (5, TestTyBase (NodeId 5) (BaseTy "Int")),
                    (6, TestTyBase (NodeId 6) (BaseTy "Bool"))
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (NodeId 1, NodeId 0, BindFlex),
                    (NodeId 2, NodeId 1, BindFlex),
                    (NodeId 3, NodeId 1, BindFlex),
                    (NodeId 4, NodeId 0, BindFlex),
                    (NodeId 5, NodeId 4, BindFlex),
                    (NodeId 6, NodeId 4, BindFlex)
                  ],
              cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 4)]
            }
   in case solveUnifyRaw defaultTraceConfig c of
        Right SolveResult {srConstraint = solved} ->
          conjoin
            [ cUnifyEdges solved === [],
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propRebindHarmonize :: Int -> Property
propRebindHarmonize size =
  let c = chainConstraint size
      left = typeRef (NodeId (size - 2))
      right = typeRef (NodeId (size - 1))
   in case Binding.bindingLCA c left right of
        Right lca ->
          conjoin
            [ lca === left,
              Binding.checkBindingTree c === Right ()
            ]
        other -> counterexample (show other) False

propGeneralizedUnify :: Int -> Property
propGeneralizedUnify _size =
  let c =
        varTripleConstraint
          { cUnifyEdges =
              [ UnifyEdge (NodeId 1) (NodeId 2),
                UnifyEdge (NodeId 2) (NodeId 3)
              ]
          }
   in case solveUnifyRaw defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin
            [ cUnifyEdges solved === [],
              frWith uf (NodeId 1) === frWith uf (NodeId 2),
              frWith uf (NodeId 2) === frWith uf (NodeId 3),
              Binding.checkBindingTree solved === Right ()
            ]
        Left err -> counterexample (show err) False

propWfEmpty :: Int -> Property
propWfEmpty _size =
  Elab.typeCheck (Elab.ELit (Surf.LInt 0)) === Right builtinIntTy

propWfTVar :: Int -> Property
propWfTVar _size =
  let outerRef = elabTypeRef 1289 "outer"
      innerRef = elabTypeRef 1290 "inner"
      innerBound = Elab.TVarRef outerRef
      env =
        Elab.insertTypeBindingRef innerRef innerBound
          $ Elab.insertTypeBindingRef outerRef Elab.TBottom emptyTypeCheckEnv
      freeBoundRefs = TypeOps.freeTypeVarRefsType innerBound
   in conjoin
        [ Map.size (Elab.typeEnv env) === 2,
          Map.lookup outerRef (Elab.typeEnv env) === Just Elab.TBottom,
          Map.lookup innerRef (Elab.typeEnv env) === Just innerBound,
          counterexample (show freeBoundRefs) $
            length freeBoundRefs == 1
              && any (ElabTypes.typeBinderRefsSameIdentity outerRef) freeBoundRefs,
          Elab.checkInstantiation env innerBound (ElabTypes.instAbstrWithRef innerRef)
            === Right (Elab.TVarRef innerRef)
        ]

propWfVar :: Int -> Property
propWfVar _size =
  let variableTypeRef = elabTypeRef 1293 "a"
      variableTy = Elab.TVarRef variableTypeRef
      resolved = generatedResolvedLocal 1293 "x" "x" variableTy
      env =
        Elab.insertResolvedTermBinding resolved variableTy
          $ Elab.insertTypeBindingRef variableTypeRef Elab.TBottom emptyTypeCheckEnv
      entries = Elab.resolvedTermEnvEntries (Elab.resolvedTermEnv env)
   in conjoin
        [ Map.size (Elab.typeEnv env) === 1,
          length entries === 1,
          Elab.typeCheckWithEnv env (Elab.EVarNode resolved) === Right variableTy
        ]

propInstReflex :: Int -> Property
propInstReflex _size =
  applyShouldBe intTy Elab.InstId intTy

propInstTrans :: Int -> Property
propInstTrans _size =
  applyShouldBe intTy (Elab.InstSeq Elab.InstIntro Elab.InstElim) intTy

propInstBot :: Int -> Property
propInstBot _size =
  let env = emptyTypeCheckEnv
   in conjoin
        [ Elab.checkInstantiation env Elab.TBottom (Elab.InstBot intTy) === Right intTy,
          counterexample "Inst-Bot accepted a non-bottom source" $
            case Elab.checkInstantiation env boolTy (Elab.InstBot intTy) of
              Left _ -> property True
              Right result -> counterexample (show result) False
        ]

propInstHyp :: Int -> Property
propInstHyp _size =
  let refA = elabTypeRef 417 "a"
   in applyShouldBe Elab.TBottom (ElabTypes.instAbstrWithRef refA) (ElabTypes.tVarWithRef refA)

propInstInner :: Int -> Property
propInstInner _size =
  applyShouldBe forallA (Elab.InstInside (Elab.InstBot intTy)) (testTForall "a" (Just (boundFromType intTy)) (testTVar "a"))

propInstOuter :: Int -> Property
propInstOuter _size =
  let refX = elabTypeRef 425 "x"
   in applyShouldBe
        (testTForall "a" Nothing (testTVar "z"))
        (ElabTypes.instUnderWithRef refX (ElabTypes.instAbstrWithRef refX))
        (testTForall "a" Nothing (testTVar "a"))

propInstQuantElim :: Int -> Property
propInstQuantElim _size =
  Elab.checkInstantiation emptyTypeCheckEnv forallA Elab.InstElim
    === Right Elab.TBottom

propInstQuantIntro :: Int -> Property
propInstQuantIntro _size =
  case Elab.checkInstantiation emptyTypeCheckEnv intTy Elab.InstIntro of
    Right (Elab.TForallRef _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propTypingVar :: Int -> Property
propTypingVar _size =
  let resolved = generatedResolvedLocal 0 "x" "x" intTy
      env = Elab.mkTypeCheckEnvWithResolvedTerms [(resolved, intTy)] Map.empty
   in Elab.typeCheckWithEnv env (Elab.EVarNode resolved) === Right intTy

propTypingAbs :: Int -> Property
propTypingAbs _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propTypingApp :: Int -> Property
propTypingApp _size =
  Elab.typeCheck (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Right intTy

propTypingTAbs :: Int -> Property
propTypingTAbs _size =
  typeCheckShouldMatch (Elab.typeCheck polyId) polyIdTy

propTypingTApp :: Int -> Property
propTypingTApp _size =
  Elab.typeCheck (Elab.ETyInst polyId (Elab.InstApp intTy)) === Right (Elab.TArrow intTy intTy)

propTypingLet :: Int -> Property
propTypingLet _size =
  Elab.typeCheck (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Right intTy

propRedBeta :: Int -> Property
propRedBeta _size =
  Elab.step (Elab.EApp idLam (Elab.ELit (Surf.LInt 1))) === Just (Elab.ELit (Surf.LInt 1))

propRedBetaLet :: Int -> Property
propRedBetaLet _size =
  Elab.step (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Just (Elab.ELit (Surf.LInt 1))

propRedReflex :: Int -> Property
propRedReflex _size =
  Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstId) === Just (Elab.ELit (Surf.LInt 1))

propRedTrans :: Int -> Property
propRedTrans _size =
  let term = Elab.ETyInst (Elab.ELit (Surf.LInt 1)) (Elab.InstSeq Elab.InstIntro Elab.InstElim)
   in Elab.step term === Just (Elab.ETyInst (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) Elab.InstElim)

propRedQuantIntro :: Int -> Property
propRedQuantIntro _size =
  case Elab.step (Elab.ETyInst (Elab.ELit (Surf.LInt 1)) Elab.InstIntro) of
    Just (Elab.ETyAbsRef ref Nothing (Elab.ELit (Surf.LInt 1))) -> ElabTypes.typeBinderRefName ref === "u0"
    other -> counterexample ("Expected generated InstIntro abstraction, got: " ++ show other) False

propRedQuantElim :: Int -> Property
propRedQuantElim _size =
  Elab.step (Elab.ETyInst polyId Elab.InstElim) === Just (mkTestLocalLam "x" Elab.TBottom (mkTestDeferredVar "x"))

propRedInner :: Int -> Property
propRedInner _size =
  let term = Elab.ETyInst (mkTestTyAbs "a" Nothing (mkTestDeferredVar "x")) (Elab.InstInside (Elab.InstBot intTy))
   in Elab.step term === Just (mkTestTyAbs "a" (Just (boundFromType intTy)) (mkTestDeferredVar "x"))

propRedOuter :: Int -> Property
propRedOuter _size =
  let body = mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x")
      term = Elab.ETyInst (mkTestTyAbs "a" Nothing body) (ElabTypes.instUnderWithRef (elabTypeRef 501 "b") (Elab.InstApp intTy))
   in Elab.step term === Just (mkTestTyAbs "a" Nothing (Elab.ETyInst body (Elab.InstApp intTy)))

propRedContext :: Int -> Property
propRedContext _size =
  let arg = Elab.EApp (mkTestLocalLam "y" intTy (mkTestDeferredVar "y")) (Elab.ELit (Surf.LInt 1))
   in Elab.step (Elab.EApp idLam arg) === Just (Elab.EApp idLam (Elab.ELit (Surf.LInt 1)))

propApplyN :: Int -> Property
propApplyN _size =
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propApplyO :: Int -> Property
propApplyO _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
    Right (Elab.TForallRef _ Nothing body) -> body === intTy
    other -> counterexample (show other) False

propApplySeq :: Int -> Property
propApplySeq _size =
  let first = Elab.InstIntro
      second = Elab.InstElim
      lhs = Elab.applyInstantiation intTy (Elab.InstSeq first second)
      rhs = Elab.applyInstantiation intTy first >>= \midTy -> Elab.applyInstantiation midTy second
   in conjoin
        [ lhs === rhs,
          lhs === Right intTy
        ]

propApplyInner :: Int -> Property
propApplyInner _size =
  propInstInner 0

propApplyOuter :: Int -> Property
propApplyOuter _size =
  propInstOuter 0

propApplyHyp :: Int -> Property
propApplyHyp _size =
  let refA = elabTypeRef 540 "a"
   in applyShouldBe Elab.TBottom (ElabTypes.instAbstrWithRef refA) (ElabTypes.tVarWithRef refA)

propApplyBot :: Int -> Property
propApplyBot _size =
  applyShouldBe Elab.TBottom (Elab.InstBot intTy) intTy

propApplyId :: Int -> Property
propApplyId _size =
  applyShouldBe (Elab.TArrow intTy boolTy) Elab.InstId (Elab.TArrow intTy boolTy)

propTransNoInertLocked :: Int -> Property
propTransNoInertLocked size =
  let c = inertConstraint size
   in case validateTranslatablePresolution c of
        Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("InertLockedNodes" `isInfixOf` show issues)
        other -> counterexample (show other) False

propTransSchemeRootRigid :: Int -> Property
propTransSchemeRootRigid _size =
  case validateTranslatablePresolution flexibleSchemeRootConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("SchemeRootNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propTransArrowRigid :: Int -> Property
propTransArrowRigid _size =
  case validateTranslatablePresolution flexibleArrowConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("ArrowNodeNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propTransNonInteriorRigid :: Int -> Property
propTransNonInteriorRigid _size =
  case validateTranslatablePresolution flexibleNonInteriorConstraint of
    Left (NonTranslatablePresolution issues) -> counterexample (show issues) ("NonInteriorNodeNotRigid" `isInfixOf` show issues)
    other -> counterexample (show other) False

propSigmaReorderRequired :: Int -> Property
propSigmaReorderRequired _size =
  let body = Elab.TArrow (testTVar "a") (testTVar "b")
      src = testTForall "a" Nothing (testTForall "b" Nothing body)
      tgt = testTForall "b" Nothing (testTForall "a" Nothing body)
   in case Elab.sigmaReorder src tgt of
        Right inst ->
          conjoin
            [ counterexample (show inst) (inst /= Elab.InstId),
              counterexample (show inst) (isRight (Elab.applyInstantiation src inst))
            ]
        Left err -> counterexample (show err) False

propSigmaReorderIdentity :: Int -> Property
propSigmaReorderIdentity _size =
  let src = testTForall "a" Nothing (Elab.TArrow (testTVar "a") intTy)
   in Elab.sigmaReorder src src === Right Elab.InstId

propContextFind :: Int -> Property
propContextFind size =
  let (c, root, target, expected) = contextFindFixture size
   in case Elab.contextToNodeBound (identityPresolutionView c) root target of
        Right steps -> steps === Just expected
        Left err -> counterexample (show err) False

propContextReject :: Int -> Property
propContextReject size =
  let (c, root, target) = contextRejectFixture size
   in case Elab.contextToNodeBound (identityPresolutionView c) root target of
        Right steps -> steps === Nothing
        Left err -> counterexample (show err) False

propEdgeTranslation :: Int -> Property
propEdgeTranslation _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr letIdAppExpr) of
    Right (term, ty) ->
      conjoin
        [ ty === intTy,
          Elab.typeCheck term === Right intTy
        ]
    Left err -> counterexample (Elab.renderPipelineError err) False

propElabLambdaVar :: Int -> Property
propElabLambdaVar _size =
  elaboratesTo (Surf.ELam "x" (Surf.EVar "x")) polyIdTy

propElabLetVar :: Int -> Property
propElabLetVar _size =
  elaboratesTo (Surf.ELet "x" (Surf.ELit (Surf.LInt 1)) (Surf.EVar "x")) intTy

propElabAbs :: Int -> Property
propElabAbs _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (Elab.ETyAbsRef {}, ty) -> typeShouldMatch ty polyIdTy
    other -> counterexample (show other) False

propElabApp :: Int -> Property
propElabApp _size =
  elaboratesTo (Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt 1))) intTy

propElabLet :: Int -> Property
propElabLet _size =
  elaboratesTo letIdAppExpr intTy

-- Thesis Property 15.3.14, specialized to the eMLF annotation forms whose
-- translation is introduced in §15.3.8.  Type abstractions, type computations,
-- and explicit recursive-type evidence erase; the remaining value-term shape
-- must be the original annotated source with its annotations removed.
expectElabAnnotationErasure :: Surf.SurfaceExpr -> Expectation
expectElabAnnotationErasure expr =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Right (term, _ty) ->
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations expr
    Left err ->
      expectationFailure
        ( Elab.renderPipelineError err
            ++ case
                PipelineTest.runPipelineElabDetailedUncheckedWithExternalBindings
                  Set.empty
                  Map.empty
                  (unsafeNormalizeExpr expr)
              of
                Right unchecked ->
                  "\nunchecked elaborated term: "
                    ++ show (PipelineTest.pedTerm unchecked)
                Left uncheckedErr ->
                  "\nunchecked elaboration also failed: "
                    ++ Elab.renderPipelineError uncheckedErr
        )

expectElabAnnotationErasureAtType ::
  Elab.ElabType -> Surf.SurfaceExpr -> Expectation
expectElabAnnotationErasureAtType expectedTy expr =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      if TypeOps.alphaEqType ty expectedTy
        then pure ()
        else
          expectationFailure
            ( "term: "
                ++ show term
                ++ "\nactual type: "
                ++ show ty
                ++ "\nexpected type: "
                ++ show expectedTy
            )
      case Elab.typeCheck term of
        Left err -> expectationFailure (show err)
        Right checkedTy -> checkedTy `shouldMatchType` expectedTy
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations expr

-- Thesis §§12.3.2 and 15.3.8: an annotation coercing the identity
-- abstraction to forall (a >= sigma-id). a -> a is itself an identity term.
-- Its xMLF construction therefore binds the flexible result before building
-- the lambda; it must not retrofit an unrelated outer InstIntro afterwards.
expectElabBoundedAnnotationAbs :: Expectation
expectElabBoundedAnnotationAbs =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr boundedIdentityAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` boundedIdentityAnnotationType
      case Elab.typeCheck term of
        Left err -> expectationFailure (show err)
        Right checkedTy -> checkedTy `shouldMatchType` boundedIdentityAnnotationType
      expectBoundedIdentityAnnotationShape term

-- Thesis §12.3.2 uses κ = exists beta. forall alpha.
-- beta -> (alpha -> alpha) as the representative source annotation.  The
-- existential beta is inferred and generalized outside the annotation-owned
-- universal alpha; the two binders must both be present in the checked xMLF
-- construction.
expectElabMixedAnnotation :: Expectation
expectElabMixedAnnotation =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr mixedAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      if TypeOps.alphaEqType ty mixedAnnotationType
        then pure ()
        else
          expectationFailure
            ( "mixed annotation term: "
                ++ show term
                ++ "\nactual type: "
                ++ show ty
                ++ "\nexpected type: "
                ++ show mixedAnnotationType
            )
      Elab.typeCheck term `shouldBe` Right ty
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations mixedAnnotationExpr

-- The inferred existential in the source annotation belongs to the
-- annotation's publication boundary.  Using the annotated value in an outer
-- let must instantiate that binder, not leak it into the enclosing result.
expectNestedMixedAnnotationLocal :: Expectation
expectNestedMixedAnnotationLocal =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr nestedMixedAnnotationExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` boolTy
      Elab.typeCheck term `shouldBe` Right ty
      eraseXmlfTerm term `shouldBe` eraseSurfaceAnnotations nestedMixedAnnotationExpr

-- Thesis §15.3.8: omega = lambda (g : sigma-id) . g g elaborates, up to
-- identity computations, to
--   Lambda (alpha >= sigma-id). lambda (g : sigma-id).
--     (g[sigma-id] g)[alpha]
-- and has the principal flexible result type
--   forall (alpha >= sigma-id). sigma-id -> alpha.
expectElabAnnotatedSelfApp :: Expectation
expectElabAnnotatedSelfApp =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr annotatedSelfAppExpr) of
    Left err -> expectationFailure (Elab.renderPipelineError err)
    Right (term, ty) -> do
      ty `shouldMatchType` annotatedSelfAppType
      case Elab.typeCheck term of
        Left err -> expectationFailure (show err)
        Right checkedTy -> checkedTy `shouldMatchType` annotatedSelfAppType
      expectAnnotatedSelfAppShape term

shouldMatchType :: Elab.ElabType -> Elab.ElabType -> Expectation
shouldMatchType actual expected =
  if TypeOps.alphaEqType actual expected
    then pure ()
    else expectationFailure (show actual ++ " /= " ++ show expected)

data ErasedTerm
  = ErasedVar String
  | ErasedLit Surf.Lit
  | ErasedLam String ErasedTerm
  | ErasedApp ErasedTerm ErasedTerm
  | ErasedLet String ErasedTerm ErasedTerm
  deriving (Eq, Show)

eraseSurfaceAnnotations :: Surf.SurfaceExpr -> ErasedTerm
eraseSurfaceAnnotations expr =
  case expr of
    Surf.EVarNode reference -> ErasedVar (Surf.termReferenceName reference)
    Surf.ELit lit -> ErasedLit lit
    Surf.ELamNode reference body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EApp fun arg -> ErasedApp (eraseSurfaceAnnotations fun) (eraseSurfaceAnnotations arg)
    Surf.ELetNode reference rhs body ->
      ErasedLet
        (Surf.termReferenceName reference)
        (eraseSurfaceAnnotations rhs)
        (eraseSurfaceAnnotations body)
    Surf.ELamAnnNode reference _ body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EExactLamNode reference _ body ->
      ErasedLam (Surf.termReferenceName reference) (eraseSurfaceAnnotations body)
    Surf.EAnn inner _ -> eraseSurfaceAnnotations inner
    Surf.EExactAnn inner _ _ -> eraseSurfaceAnnotations inner

eraseXmlfTerm :: Elab.XmlfTerm -> ErasedTerm
eraseXmlfTerm term =
  case term of
    Elab.EVarNode resolved -> ErasedVar (ElabTypes.resolvedVarReferenceName resolved)
    Elab.ELit lit -> ErasedLit lit
    Elab.ELam resolved body ->
      ErasedLam (ElabTypes.resolvedVarReferenceName resolved) (eraseXmlfTerm body)
    Elab.EApp fun arg -> ErasedApp (eraseXmlfTerm fun) (eraseXmlfTerm arg)
    Elab.ELet resolved _ rhs body ->
      ErasedLet
        (ElabTypes.resolvedVarReferenceName resolved)
        (eraseXmlfTerm rhs)
        (eraseXmlfTerm body)
    Elab.ETyAbsRef _ _ body -> eraseXmlfTerm body
    Elab.ETyInst inner _ -> eraseXmlfTerm inner
    Elab.ERoll _ body -> eraseXmlfTerm body
    Elab.EUnroll body -> eraseXmlfTerm body

annotationErasureCases :: [Surf.SurfaceExpr]
annotationErasureCases =
  [ Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int"),
    Surf.EAnn (Surf.ELam "x" (Surf.EVar "x")) sigmaIdSource,
    annotatedSelfAppExpr,
    Surf.ELet
      "id"
      (Surf.EAnn (Surf.ELam "x" (Surf.EVar "x")) sigmaIdSource)
      (Surf.EApp (Surf.EVar "id") (Surf.ELit (Surf.LInt 1))),
    Surf.ELamAnn
      "poly"
      sigmaIdSource
      ( Surf.ELet
          "keepInt"
          (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LInt 1)))
          (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LBool True)))
      )
  ]

-- | Closed eMLF programs generated by construction from an annotated,
-- independently typable seed.  Every wrapper preserves typability without
-- knowing the seed's inferred type:
--
--   * identity application and let round-tripping reuse the complete inferred
--     scheme;
--   * unused lets and ignored lambda arguments only extend lexical scope; and
--   * annotated ignored arguments use a closed monotype.
--
-- This deliberately includes bounded, mixed existential/universal, and paper
-- @g g@ seeds.  Unlike the older Chapter 15 obligations, the random size now
-- changes the complete source tree rather than merely repeating one fixture.
genClosedWellTypedAnnotatedExpr :: Gen Surf.SurfaceExpr
genClosedWellTypedAnnotatedExpr =
  sized $ \size -> do
    seed <- genAnnotatedSeed
    depth <- chooseInt (0, min 7 (max 0 (size `div` 3)))
    wrapGeneratedAnnotatedExpr depth seed

genAnnotatedSeed :: Gen Surf.SurfaceExpr
genAnnotatedSeed = do
  intValue <- chooseInteger (-16, 16)
  boolValue <- arbitrary
  elements
    [ Surf.EAnn
        (Surf.ELit (Surf.LInt intValue))
        (Surf.STBase "Int"),
      Surf.EAnn
        (Surf.ELit (Surf.LBool boolValue))
        (Surf.STBase "Bool"),
      Surf.EAnn
        (Surf.ELam "_generatedSeedX" (Surf.EVar "_generatedSeedX"))
        sigmaIdSource,
      boundedIdentityAnnotationExpr,
      mixedAnnotationExpr,
      nestedMixedAnnotationExpr,
      annotatedSelfAppExpr,
      Surf.ELamAnn
        "_generatedSeedPoly"
        sigmaIdSource
        ( Surf.EApp
            (Surf.EVar "_generatedSeedPoly")
            (Surf.ELit (Surf.LInt intValue))
        ),
      Surf.ELet
        "_generatedSeedId"
        ( Surf.EAnn
            (Surf.ELam "_generatedSeedArg" (Surf.EVar "_generatedSeedArg"))
            sigmaIdSource
        )
        ( Surf.ELet
            "_generatedSeedDiscard"
            ( Surf.EApp
                (Surf.EVar "_generatedSeedId")
                (Surf.ELit (Surf.LInt intValue))
            )
            ( Surf.EApp
                (Surf.EVar "_generatedSeedId")
                (Surf.ELit (Surf.LBool boolValue))
            )
        )
    ]

wrapGeneratedAnnotatedExpr ::
  Int ->
  Surf.SurfaceExpr ->
  Gen Surf.SurfaceExpr
wrapGeneratedAnnotatedExpr depth expr
  | depth <= 0 = pure expr
  | otherwise = do
      intValue <- chooseInteger (-16, 16)
      boolValue <- arbitrary
      wrapper <- chooseInt (0, 6)
      let name = "_generatedWrap" ++ show depth
          wrapped =
            case wrapper of
              0 ->
                Surf.EApp
                  (Surf.ELam name (Surf.EVar name))
                  expr
              1 ->
                Surf.ELet name expr (Surf.EVar name)
              2 ->
                Surf.ELet
                  name
                  (Surf.ELit (Surf.LInt intValue))
                  expr
              3 ->
                Surf.EApp
                  (Surf.ELam name expr)
                  (Surf.ELit (Surf.LBool boolValue))
              4 ->
                Surf.ELam name expr
              5 ->
                Surf.EApp
                  (Surf.ELamAnn name (Surf.STBase "Int") expr)
                  (Surf.ELit (Surf.LInt intValue))
              _ ->
                Surf.ELet
                  name
                  ( Surf.EAnn
                      (Surf.ELit (Surf.LBool boolValue))
                      (Surf.STBase "Bool")
                  )
                  expr
      wrapGeneratedAnnotatedExpr (depth - 1) wrapped

sigmaIdSource :: Surf.SrcType
sigmaIdSource =
  Surf.STForall "a" Nothing (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))

-- Frozen from the 70th case of generated seed 123456789.  The outer lambda's
-- result application is visible at two nested graph scopes after
-- canonicalization, but the lambda body edge and owner scope remain exact
-- source-construction provenance.
lambdaBoundaryScopeThroughNestedApplicationSeed123456789Expr
  :: Surf.SurfaceExpr
lambdaBoundaryScopeThroughNestedApplicationSeed123456789Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELam "x" (Surf.EVar "x"))
                            boundedIdentitySource
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
        (Surf.EVar "_generatedWrap2")
    )
    (Surf.EVar "_generatedWrap1")
  where
    boundedIdentitySource =
      Surf.STForall
        "a"
        (Just (Surf.mkSrcBound sigmaIdSource))
        (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))

-- Frozen from the 81st case of generated seed 123456789.  The innermost let
-- returns its annotated bounded identity through a variable owner; the
-- enclosing lambda must consume that exact body endpoint even though the
-- variable itself does not introduce a returned-result construction.
letReturnedBoundedIdentityThroughAppliedNestedLambdaSeed123456789Expr
  :: Surf.SurfaceExpr
letReturnedBoundedIdentityThroughAppliedNestedLambdaSeed123456789Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-10)))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELam "x" (Surf.EVar "x"))
                                        boundedIdentitySource
                                    )
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LInt 4))
        )
    )
  where
    boundedIdentitySource =
      Surf.STForall
        "a"
        (Just (Surf.mkSrcBound sigmaIdSource))
        (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))

-- Frozen from the 40th case of generated seed 987654321.  The application
-- consumes only the first parameter of a five-lambda value; the four
-- unapplied lambda parameters remain lexical while the innermost source
-- annotation constructs the returned Int endpoint.
annotatedConstantThroughPartiallyAppliedFiveLambdaSeed987654321Expr
  :: Surf.SurfaceExpr
annotatedConstantThroughPartiallyAppliedFiveLambdaSeed987654321Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 5))
                                    (Surf.STBase "Int")
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the 56th case of generated seed 987654321.  The paper's @g g@
-- construction is nested below two unapplied lambdas, returned through a let,
-- and then passed through an identity application.  The direct application
-- Gamma and both forwarded lambda results must remain owned by their exact
-- source occurrences.
paperGgThroughLetReturnedTwoLambdaWrapperSeed987654321Expr
  :: Surf.SurfaceExpr
paperGgThroughLetReturnedTwoLambdaWrapperSeed987654321Expr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt 9))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELamAnn
                        "g"
                        sigmaIdSource
                        (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 84th case of generated seed 135791357.  The outer lambda
-- returns a partially applied four-lambda spine whose final body is an
-- annotated let result.  The owner-final Gamma binders and the root planner
-- must publish the same identity order, independently of graph traversal.
localLambdaGammaThroughAppliedFourLambdaSeed135791357Expr
  :: Surf.SurfaceExpr
localLambdaGammaThroughAppliedFourLambdaSeed135791357Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt 13))
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                (Surf.EVar "_generatedWrap7")
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )

-- Frozen from the 18th case of generated seed 19088743.  The inner lambda
-- consumes the flexible declaration of a bounded identity while the enclosing
-- lambda and let still carry the planner's closed declaration.  Root
-- projection must use the exact consumed-at-owner transition rather than
-- requiring the stale planner bound to equal the opened local endpoint.
consumedBoundedIdentityThroughReturnedLambdaLetSeed19088743Expr
  :: Surf.SurfaceExpr
consumedBoundedIdentityThroughReturnedLambdaLetSeed19088743Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-6)))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    boundedIdentityAnnotationExpr
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 38th case of generated seed 2000000001.  The outer
-- annotated application fixes the first lambda domain, while the returned
-- nested lambda owns the bounded-identity result packet below two lets.  The
-- application must wait for that checked owner before selecting its complete
-- result endpoint.
returnedBoundedIdentityLambdaUnderAppliedAnnotationSeed2000000001Expr
  :: Surf.SurfaceExpr
returnedBoundedIdentityLambdaUnderAppliedAnnotationSeed2000000001Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-1)))
                    ( Surf.ELam
                        "_generatedWrap5"
                        boundedIdentityAnnotationExpr
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-7)))

-- Frozen from the 54th case of generated seed 999999999.  The mixed source
-- annotation contributes an implicit @beta@ below an applied lambda and an
-- identity application.  Its descendant packet completes an enclosing graph
-- consumer before @beta@ occurs in the packet's provisional Gamma bound, so
-- preparation must retain the exact source-binder capability carried by the
-- owner-completion endpoint.
implicitAnnotationBinderThroughDescendantConsumerSeed999999999Expr
  :: Surf.SurfaceExpr
implicitAnnotationBinderThroughDescendantConsumerSeed999999999Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-1)))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 8))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 92nd case of generated seed 999999999.  The applied outer
-- lambda publishes an ambient declaration whose bound is a higher-rank value
-- lambda ending at the inner application result.  That result declaration is
-- owned by the prepared packet but occurs free in the selected bound rather
-- than in its leading forall spine; exact construction must retain it from
-- packet construction order before applying the terminal ambient Hyp.
packetOwnedResultDependencyInsideAmbientLambdaBoundSeed999999999Expr
  :: Surf.SurfaceExpr
packetOwnedResultDependencyInsideAmbientLambdaBoundSeed999999999Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-6)))
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt 16))
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 23rd case of generated seed 161803398.  The inner source
-- annotation has an implicit existential and an explicit forall beneath two
-- unapplied lambdas.  Applying the enclosing annotated lambda must discharge
-- the exact frozen topology result without losing either source identity.
nestedMixedAnnotationTopologyThroughAppliedLambdaSeed161803398Expr
  :: Surf.SurfaceExpr
nestedMixedAnnotationTopologyThroughAppliedLambdaSeed161803398Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        (Surf.ELam "y" (Surf.EVar "y"))
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 12))
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt (-8)))
    )

-- Frozen from the 100th case of generated seed 161803398.  The unapplied
-- middle lambda returns the paper's annotated @g g@ lambda after an outer
-- annotated parameter is consumed.  Owner-final construction must publish
-- the source forall exactly once and use that same identity in the completed
-- result-consumer bound.
returnedPaperGgLambdaThroughPartiallyAppliedAnnotatedSpineSeed161803398Expr
  :: Surf.SurfaceExpr
returnedPaperGgLambdaThroughPartiallyAppliedAnnotatedSpineSeed161803398Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-7)))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        annotatedSelfAppExpr
                    )
                )
                (Surf.ELit (Surf.LInt 2))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 60th case of generated seed 937635187.  The inner annotated
-- result closes @forall t11. t11 -> Int@ before an enclosing lambda copies that
-- lexical declaration.  Consuming the ambient result binder must move the
-- completed bound through the certified source-to-copy route before it is
-- substituted into later Gamma declarations; otherwise the copied bound and
-- returned body declare the same graph identity twice.
consumedClosedForallAtCertifiedLexicalCopySeed937635187Expr
  :: Surf.SurfaceExpr
consumedClosedForallAtCertifiedLexicalCopySeed937635187Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt (-15)))
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 3))
                                    (Surf.STBase "Int")
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the 71st case of generated seed 937635187.  The completed
-- higher-rank bound already uses the source presentation inside an arrow when
-- a later lexical-copy route is considered.  The route is provenance for a
-- possible future scope, not authority to rewrite this still-source bound.
nestedClosedSourceDependencyBeforeLexicalCopySeed937635187Expr
  :: Surf.SurfaceExpr
nestedClosedSourceDependencyBeforeLexicalCopySeed937635187Expr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    (Surf.ELit (Surf.LInt 7))
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap6"
                            (Surf.EVar "_generatedWrap6")
                        )
                        ( Surf.EAnn
                            (Surf.ELam "x" (Surf.EVar "x"))
                            ( Surf.STForall
                                "a"
                                ( Just
                                    ( Surf.SrcBound
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                    )
                                )
                                ( Surf.STArrow
                                    (Surf.STVar "a")
                                    (Surf.STVar "a")
                                )
                            )
                        )
                    )
                )
            )
        )
    )

-- Frozen from the 38th case of generated seed 20260809.  The returned let
-- value has already constructed three explicit type abstractions.  Two of
-- them are fresh lexical identities with no graph-node route, so the
-- enclosing lambda must consume the child owner's exact endpoint-completion
-- certificate instead of treating those identities as either ambient Gamma
-- or new outer forall candidates.
childTypeAbstractionsClosedDuringEnclosingLambdaSeed20260809Expr
  :: Surf.SurfaceExpr
childTypeAbstractionsClosedDuringEnclosingLambdaSeed20260809Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.ELet
                                    "_generatedSeedId"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedArg"
                                            (Surf.EVar "_generatedSeedArg")
                                        )
                                        sigmaIdSource
                                    )
                                    ( Surf.ELet
                                        "_generatedSeedDiscard"
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LInt 5))
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LBool False))
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 8))
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

-- Frozen from the 97th case of generated seed 449181304.  The RHS lambda
-- returns an explicitly polymorphic identity.  Let publication may expose
-- both quantified identities at the scheme boundary only after the checked
-- RHS has constructed the corresponding movement across the value arrow.
returnedPolymorphismBeforeLetPublicationSeed449181304Expr
  :: Surf.SurfaceExpr
returnedPolymorphismBeforeLetPublicationSeed449181304Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    sigmaIdSource
                                )
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 93rd case of generated seed 1015.  The inner application
-- returns a monomorphic graph endpoint @t -> t@, while the transparent let
-- publishes that exact value as @forall b. b -> b@ by inserting an explicit
-- type abstraction.  The publication must advance the child's exact result
-- certificate at the same construction boundary; the final forall shape is
-- validation, not authority for reconstructing that transition later.
explicitForallThroughNestedLetPublicationSeed1015Expr
  :: Surf.SurfaceExpr
explicitForallThroughNestedLetPublicationSeed1015Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                    (Surf.ELit (Surf.LInt 15))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt (-3)))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 18th case of generated seed 1021.  The first ground
-- application exposes three returned value lambdas whose innermost result is
-- the paper self-application construction.  Its bounded result declaration
-- must remain in scope while the outer annotated application checks the
-- complete function source.
paperGgThroughPartiallyAppliedTripleLambdaSeed1021Expr
  :: Surf.SurfaceExpr
paperGgThroughPartiallyAppliedTripleLambdaSeed1021Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            annotatedSelfAppExpr
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LInt (-14)))
    )

-- Frozen from the 27th case of generated seed 1022.  The checked descendant
-- completes the paper self-application result after its future lambda owner
-- has already exposed a provisional Gamma declaration.  The pending-owner
-- certificate must carry the exact construction transition to that owner;
-- the later ambient type is only a presentation of that certified state.
pendingOwnerGammaHistoryThroughWrappedPaperGgSeed1022Expr
  :: Surf.SurfaceExpr
pendingOwnerGammaHistoryThroughWrappedPaperGgSeed1022Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap5"
                                    (Surf.EVar "_generatedWrap5")
                                )
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap6"
                                        (Surf.STBase "Int")
                                        ( Surf.ELamAnn
                                            "g"
                                            sigmaIdSource
                                            ( Surf.EApp
                                                (Surf.EVar "g")
                                                (Surf.EVar "g")
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LInt (-16)))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-10)))

-- Frozen from the 31st case of generated seed 1022.  The applied lambda
-- returns two unapplied parameters whose terminal let publishes the paper's
-- self-application construction.  The exact enclosing-lambda plan must carry
-- the copied bounded result declaration and both value parameters in the
-- same certified construction spine.
paperGgThroughAppliedLambdaAndTwoReturnedParametersSeed1022Expr
  :: Surf.SurfaceExpr
paperGgThroughAppliedLambdaAndTwoReturnedParametersSeed1022Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.ELamAnn
                                    "g"
                                    sigmaIdSource
                                    (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                                )
                                (Surf.EVar "_generatedWrap6")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 12))
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

-- Frozen from the 54th case of generated seed 1023.  The annotated Int
-- lambda has no local Gamma declarations of its own, while its checked body
-- returns two explicit forall declarations.  The exact Var-Abs constructor
-- must commute that certified body spine outside the Int arrow before the
-- enclosing application consumes it.
checkedBodyForallsAcrossAppliedAnnotatedLambdaSeed1023Expr
  :: Surf.SurfaceExpr
checkedBodyForallsAcrossAppliedAnnotatedLambdaSeed1023Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LInt (-9)))
                                        (Surf.STBase "Int")
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 10))
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 43rd case of generated seed 1024.  The paper
-- self-application sits beneath an unapplied lambda and two applied annotated
-- Int lambdas.  Its flexible result bound retains the source @forall a@ while
-- the value parameter needs a distinct lexical copy of that same source
-- declaration; both declarations must be allocated before the lambda term is
-- built.
paperGgParameterAgainstBoundedResultSeed1024Expr
  :: Surf.SurfaceExpr
paperGgParameterAgainstBoundedResultSeed1024Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap2"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap3"
                        ( Surf.ELamAnn
                            "g"
                            sigmaIdSource
                            (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
            (Surf.ELit (Surf.LInt 0))
        )
    )
    (Surf.ELit (Surf.LInt (-14)))

-- Frozen from the 18th case of generated seed 1006873496.  The identity
-- lambda's environment-owned parameter has a closed forall nested beneath an
-- arrow.  Its result uses the certified sibling copy of that forall, which is
-- alpha-conversion only and must not introduce a value-level administrative
-- let into the erased program.
identityLambdaNestedForallCopySeed1006873496Expr
  :: Surf.SurfaceExpr
identityLambdaNestedForallCopySeed1006873496Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-4)))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.ELamAnn
                                "_generatedSeedPoly"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "_generatedSeedPoly")
                                    (Surf.ELit (Surf.LInt (-13)))
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 91st case of generated seed 1006873496.  The checked
-- annotation abstraction and the identity application's result are sibling
-- copies of one source forall.  A surrounding let must carry their common
-- copy graph and align the explicit abstraction to the unique completed
-- binder instead of requiring a directed ancestor relation.
identitySiblingCopyThroughLetSeed1006873496Expr
  :: Surf.SurfaceExpr
identitySiblingCopyThroughLetSeed1006873496Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-3)))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                (Surf.ELam "y" (Surf.EVar "y"))
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the original failing case of generated seed 1006873496.  A
-- finalized consumer substitutes one closed forall into two later Gamma
-- bounds.  Those insertions are distinct lexical scopes, so the certificate
-- projector must construct a fresh declaration for the second occurrence
-- before the exact lambda plan is validated.
consumedClosedForallAcrossSiblingBoundsSeed1006873496Expr
  :: Surf.SurfaceExpr
consumedClosedForallAcrossSiblingBoundsSeed1006873496Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    sigmaIdSource
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                )
            )
        )
    )

-- Frozen from the 12th case of generated seed 1008 after the construction
-- regressions above were installed.  The identity lambda returns an annotated
-- polymorphic argument whose application result is carried to the enclosing
-- application.  That carried declaration already owns the graph-node route;
-- a coincident local binder must not erase its only construction path.
carriedRouteThroughPolymorphicLambdaArgumentSeed1008Expr
  :: Surf.SurfaceExpr
carriedRouteThroughPolymorphicLambdaArgumentSeed1008Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELamAnn
                    "_generatedSeedPoly"
                    sigmaIdSource
                    ( Surf.EApp
                        (Surf.EVar "_generatedSeedPoly")
                        (Surf.ELit (Surf.LInt (-10)))
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 34th case of generated seed 1008.  The let-bound value
-- returns three ordinary lambdas before the annotated paper self-application.
-- Each enclosing lambda must consume the direct child's owner-final result
-- construction; an already carried packet consumer is not required to invent
-- a second local declaration.
paperGgThroughLetBoundReturnedLambdaSpineSeed1008Expr
  :: Surf.SurfaceExpr
paperGgThroughLetBoundReturnedLambdaSpineSeed1008Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELamAnn
                    "g"
                    sigmaIdSource
                    (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                )
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 77th case of generated seed 1907094151.  The direct
-- application Gamma is prepared at @t -> t@, but the checked identity
-- occurrence retains the complete source endpoint @forall a. a -> a@.  The
-- checked endpoint must therefore close the declaration before its claim is
-- published; otherwise the certificate describes an N specialization which
-- the constructed xMLF term never emitted.
checkedForallEndpointClosesApplicationGammaSeed1907094151Expr
  :: Surf.SurfaceExpr
checkedForallEndpointClosesApplicationGammaSeed1907094151Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-13)))
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        sigmaIdSource
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 2))

identityApplicationOverGroundAnnotatedConstantLambdaExpr :: Surf.SurfaceExpr
identityApplicationOverGroundAnnotatedConstantLambdaExpr =
  Surf.EApp
    (Surf.ELam "identity" (Surf.EVar "identity"))
    ( Surf.ELam
        "unused"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
    )

polymorphicIdentityThroughIdentityApplicationAndLetExpr :: Surf.SurfaceExpr
polymorphicIdentityThroughIdentityApplicationAndLetExpr =
  Surf.ELet
    "ignored"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "polymorphic"
        ( Surf.EApp
            (Surf.ELam "identity" (Surf.EVar "identity"))
            ( Surf.EAnn
                (Surf.ELam "value" (Surf.EVar "value"))
                sigmaIdSource
            )
        )
        (Surf.EVar "polymorphic")
    )

appliedMixedAnnotationThroughTwoUnusedLambdasExpr :: Surf.SurfaceExpr
appliedMixedAnnotationThroughTwoUnusedLambdasExpr =
  Surf.ELam
    "firstUnused"
    ( Surf.ELam
        "secondUnused"
        ( Surf.ELet
            "result"
            nestedMixedAnnotationExpr
            (Surf.EVar "result")
        )
    )

appliedMixedAnnotationThroughGroundAnnotatedApplicationExpr :: Surf.SurfaceExpr
appliedMixedAnnotationThroughGroundAnnotatedApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "ground"
        (Surf.STBase "Int")
        appliedMixedAnnotationThroughTwoUnusedLambdasExpr
    )
    (Surf.ELit (Surf.LInt (-5)))

appliedMixedAnnotationUnderUnusedLambdaAndAnnotatedLetExpr :: Surf.SurfaceExpr
appliedMixedAnnotationUnderUnusedLambdaAndAnnotatedLetExpr =
  Surf.ELam
    "outerUnused"
    ( Surf.ELet
        "ignoredBool"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        appliedMixedAnnotationThroughGroundAnnotatedApplicationExpr
    )

appliedMixedAnnotationUnderOuterIgnoredLetExpr :: Surf.SurfaceExpr
appliedMixedAnnotationUnderOuterIgnoredLetExpr =
  Surf.ELet
    "outerIgnored"
    (Surf.ELit (Surf.LInt 15))
    appliedMixedAnnotationUnderUnusedLambdaAndAnnotatedLetExpr

-- Minimized seed-42 counterexample.  The let RHS owns the lexical source
-- binder in @forall a. a -> a@, while the returned occurrence must instantiate
-- that scheme at the fresh graph occurrence allocated by the let body edge.
-- Reusing the RHS binder as the InstApp argument lets it escape its ETyAbs.
sourcePolymorphicLetOccurrenceUnderAppliedAnnotatedLambdaExpr ::
  Surf.SurfaceExpr
sourcePolymorphicLetOccurrenceUnderAppliedAnnotatedLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                (Surf.ELit (Surf.LInt (-6)))
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        sigmaIdSource
                                    )
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 3))
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Minimized seed-42 counterexample.  The source annotation constructs the
-- bounded declaration @forall a >= (forall a. a -> a). a -> a@, while the
-- enclosing application consumes its exact lower-bound instance.  The nested
-- lambda spine must retain that construction relationship instead of treating
-- the two certified endpoints as conflicting shapes.
boundedSourceAnnotationUnderNestedLambdaConstructionExpr ::
  Surf.SurfaceExpr
boundedSourceAnnotationUnderNestedLambdaConstructionExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt (-8)))
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        ( Surf.STForall
                            "a"
                            ( Just
                                ( Surf.SrcBound
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                            ( Surf.STArrow
                                (Surf.STVar "a")
                                (Surf.STVar "a")
                            )
                        )
                    )
                )
            )
        )
    )

-- The same exterior reaches the enclosing lambda through two consumer edges:
-- one carries the closed source scheme and the other its exact open instance.
-- Their owner must select the common generalized completion independently of
-- certificate traversal order.
generalizedCompletionAcrossSiblingConsumerEdgesExpr :: Surf.SurfaceExpr
generalizedCompletionAcrossSiblingConsumerEdgesExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- An enclosing packet closes the Graph9 dependency in its source view, while
-- the descendant body uses that exact identity from inherited Gamma.  The
-- local consumer bound must project the inherited declaration out of the
-- packet's forall prefix before specializing its own bounded result.
inheritedGammaDependencyInCompletedPacketBoundExpr :: Surf.SurfaceExpr
inheritedGammaDependencyInCompletedPacketBoundExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELet
                                    "_generatedSeedId"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedArg"
                                            (Surf.EVar "_generatedSeedArg")
                                        )
                                        sigmaIdSource
                                    )
                                    ( Surf.ELet
                                        "_generatedSeedDiscard"
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LInt 8))
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LBool True))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- A root RaiseMerge can construct a bounded exterior after presolution order
-- keys have been frozen.  The construction certificate, rather than a missing
-- BindParent, owns that binder's position in the occurrence scheme.
rootRaiseMergeCompletedBinderOrderExpr :: Surf.SurfaceExpr
rootRaiseMergeCompletedBinderOrderExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- A nested result requirement first contains a provisional graph variable,
-- then its certified local construction completes that occurrence to Bool.
-- Root-scope preparation must derive dependencies from the completed bound,
-- not demand an ambient route for the vanished provisional variable.
locallyCompletedGammaDependencyExpr :: Surf.SurfaceExpr
locallyCompletedGammaDependencyExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 3))
                                    (Surf.STBase "Int")
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- The prepared lambda certificate retains an unbounded, unused forall beside
-- an ambient parameter.  The enclosing exact endpoint opens the ambient
-- binder, eliminates only the vacuous binder, and specializes the polymorphic
-- identity in the codomain.  These are explicit spine computations rather
-- than whole-function shape matching.
vacuousForallThroughNestedLambdaSpineExpr :: Surf.SurfaceExpr
vacuousForallThroughNestedLambdaSpineExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.EAnn
                        ( Surf.ELam
                            "_generatedSeedX"
                            (Surf.EVar "_generatedSeedX")
                        )
                        sigmaIdSource
                    )
                )
                (Surf.ELit (Surf.LInt 2))
            )
        )
    )

-- A single solved graph identity appears both as the enclosing result
-- declaration and inside that declaration's completed lower bound.  Those
-- occurrences are one construction route, not two independent Gamma
-- consumers.  Placement must use the outer declaration as the owner while
-- retaining the already-completed nested bound.
sharedConstructionConsumerAcrossNestedBoundExpr :: Surf.SurfaceExpr
sharedConstructionConsumerAcrossNestedBoundExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.ELamAnn
                                "_generatedSeedPoly"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "_generatedSeedPoly")
                                    (Surf.ELit (Surf.LInt 14))
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt (-2)))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt 4))

-- The direct application Gamma is initially computed from graph-domain
-- S(operated), while the bounded source annotation has already projected both
-- nested graph occurrences to its one lexical source binder.  Exact-edge
-- refinement must first route the provisional bound through that same source
-- construction domain, then publish the constructed endpoint which claim
-- validation consumes.
boundedAnnotationThroughDirectApplicationGammaExpr :: Surf.SurfaceExpr
boundedAnnotationThroughDirectApplicationGammaExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            boundedIdentityAnnotationExpr
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt (-15)))
    )

-- A nested annotated identity owns its generated source forall, while the
-- enclosing lambda independently introduces the graph binder that closes the
-- whole result.  Completing that outer lambda must keep the child payload and
-- its ETyAbs declaration in the child's identity domain; routing either one
-- through the outer parameter/local-Gamma quotient would capture it under an
-- unrelated graph abstraction.
sourceForallPayloadThroughEnclosingLambdaCompletionExpr :: Surf.SurfaceExpr
sourceForallPayloadThroughEnclosingLambdaCompletionExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    (Surf.ELit (Surf.LInt 14))
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.ELet
                            "_generatedWrap7"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "_generatedSeedX"
                                    (Surf.EVar "_generatedSeedX")
                                )
                                sigmaIdSource
                            )
                            (Surf.EVar "_generatedWrap7")
                        )
                        (Surf.EVar "_generatedWrap6")
                    )
                )
            )
        )
    )

-- The annotation owns alpha even when its inferred beta is consumed through
-- several enclosing application and lambda boundaries.  Those boundaries may
-- construct beta in Gamma, but cannot also abstract alpha outside the
-- annotation-owned ETyAbs.
annotationForallBeneathConstructedLambdasExpr :: Surf.SurfaceExpr
annotationForallBeneathConstructedLambdasExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-7)))
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.ELit (Surf.LInt (-2)))
        )
    )

-- The identity application returns its checked argument unchanged.  Its
-- direct argument edge therefore constructs the complete annotated forall,
-- while the provisional graph Gamma bound still presents only that forall's
-- body.  The application constructor must publish the checked closure before
-- it validates its direct Gamma claim; retaining the open graph body would
-- let the annotation-owned alpha escape its ETyAbs.
annotatedForallThroughDirectIdentityApplicationExpr :: Surf.SurfaceExpr
annotatedForallThroughDirectIdentityApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.EAnn
        ( Surf.ELam
            "x"
            (Surf.ELam "y" (Surf.EVar "y"))
        )
        ( Surf.STForall
            "alpha"
            Nothing
            ( Surf.STArrow
                (Surf.STVar "beta")
                ( Surf.STArrow
                    (Surf.STVar "alpha")
                    (Surf.STVar "alpha")
                )
            )
        )
    )

-- The let RHS contains the same annotated forall closure behind nested
-- identity applications, and the let result is itself returned through an
-- identity application.  Owner-selected and direct-edge Gamma planning see
-- different graph presentations of one application route; construction must
-- quotient both to one declaration before their routing maps are merged.
annotatedForallThroughIdentityAppliedLetResultExpr :: Surf.SurfaceExpr
annotatedForallThroughIdentityAppliedLetResultExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
        (Surf.EVar "_generatedWrap2")
    )

-- A locally emitted consumer declaration can carry the complete annotation
-- closure even though the body-edge projection has already opened its first
-- forall.  The lambda's Gen(Gamma,tau) construction must retain that complete
-- owner-emission bound; otherwise the opened alpha is incorrectly treated as
-- ambient while the returned let RHS still emits its ETyAbs.
annotatedForallThroughAnnotatedAppliedLambdaExpr :: Surf.SurfaceExpr
annotatedForallThroughAnnotatedAppliedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "k"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "x"
                                    (Surf.ELam "y" (Surf.EVar "y"))
                                )
                                ( Surf.STForall
                                    "alpha"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "beta")
                                        ( Surf.STArrow
                                            (Surf.STVar "alpha")
                                            (Surf.STVar "alpha")
                                        )
                                    )
                                )
                            )
                            ( Surf.EApp
                                ( Surf.EApp
                                    (Surf.EVar "k")
                                    (Surf.ELit (Surf.LInt 1))
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 14))

-- The inner lambda returns the mixed annotation unchanged and is applied
-- before an annotated outer lambda and several transparent lets publish the
-- result.  The annotation-owned alpha remains lexical to its ETyAbs; ambient
-- handoff may carry the completed result declaration but must not reinstall
-- that lexical binder as a free Gamma dependency.
mixedAnnotationThroughAppliedNestedLambdaLetChainExpr :: Surf.SurfaceExpr
mixedAnnotationThroughAppliedNestedLambdaLetChainExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-5)))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                mixedAnnotationExpr
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.ELit (Surf.LInt 0))
                )
                (Surf.EVar "_generatedWrap4")
            )
            (Surf.EVar "_generatedWrap3")
        )
        (Surf.EVar "_generatedWrap2")
    )

-- A descendant application completes the enclosing lambda declaration as a
-- bounded forall, while the pending owner scheme already carries its exact
-- Int-specialized body.  The certificate-backed specialization is a valid
-- intermediate state of the same declaration and must be advanced to the
-- owner's complete bound rather than rejected as a competing declaration.
specializedPendingOwnerBoundThroughNestedApplicationsExpr :: Surf.SurfaceExpr
specializedPendingOwnerBoundThroughNestedApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 15))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                        (Surf.ELit (Surf.LInt (-4)))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- The inner identity application constructs Int directly, while its enclosing
-- lambda/application chain still exposes the graph result variable selected by
-- the outer identity application.  The direct argument is the checked value
-- endpoint; the graph result is only its pending construction route.
nestedIdentityApplicationThroughOuterIdentityExpr :: Surf.SurfaceExpr
nestedIdentityApplicationThroughOuterIdentityExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LInt (-3)))
                            (Surf.STBase "Int")
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
    )

-- The completed descendant bound still refers to the enclosing unused lambda
-- parameter.  The pending owner scheme has already closed that exact free
-- identity with an unbounded forall, so owner publication must preserve the
-- certificate-backed closure rather than treating it as a competing bound.
pendingOwnerClosureOverFreeLambdaParameterExpr :: Surf.SurfaceExpr
pendingOwnerClosureOverFreeLambdaParameterExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELet
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap4"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap5"
                                    (Surf.EVar "_generatedWrap5")
                                )
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 0))
                                        )
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt (-1)))
                    )
                    (Surf.EVar "_generatedWrap3")
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.ELit (Surf.LInt 8))

-- The packet for the returned lambda owns the unused inner parameter that
-- remains free in a descendant Gamma requirement.  Root preparation must keep
-- that parameter in the packet's forall spine instead of opening it into an
-- unbound requirement occurrence.
nestedLambdaDependencyInMixedAnnotationPacketExpr :: Surf.SurfaceExpr
nestedLambdaDependencyInMixedAnnotationPacketExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EApp
                                        ( Surf.ELamAnn
                                            "_generatedWrap7"
                                            (Surf.STBase "Int")
                                            mixedAnnotationExpr
                                        )
                                        (Surf.ELit (Surf.LInt 4))
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.ELit (Surf.LInt 16))

-- Minimized seed-1001 counterexample.  The identity application returns a
-- let expression whose polymorphic binding is immediately instantiated twice.
-- Preparing the enclosing lambda must construct the returned let packet; it
-- must not leave the solved result carrier as a bare graph alias.
mixedAnnotationLetUseThroughIdentityApplicationExpr :: Surf.SurfaceExpr
mixedAnnotationLetUseThroughIdentityApplicationExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            nestedMixedAnnotationExpr
        )
    )

-- Minimized from the next seed-1001 failure.  The returned lambda closes over
-- a let-generalized mixed annotation and crosses two application owners.  An
-- enclosing application must not invent a solved codomain binder that is
-- absent from that owner's checked construction input.
returnedMixedAnnotationLetThroughTwoIdentityApplicationsExpr :: Surf.SurfaceExpr
returnedMixedAnnotationLetThroughTwoIdentityApplicationsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        (Surf.ELam "_generatedWrap3" nestedMixedAnnotationExpr)
    )

-- Minimized from the following seed-1001 failure.  The inner applied lambda
-- returns another lambda whose body has an exact ground annotation.  Its
-- completed packet consumer must remain owned while both enclosing
-- application constructors build their result, rather than being reopened as
-- an unowned occurrence of the same solved graph variable.
groundAnnotationThroughNestedAppliedLambdaConsumersExpr :: Surf.SurfaceExpr
groundAnnotationThroughNestedAppliedLambdaConsumersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool True))
                                (Surf.STBase "Bool")
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Minimized seed-1003 counterexample.  Two applications cross a returned
-- five-lambda spine whose terminal body has an exact Int annotation.  A
-- completed enclosing consumer can own a structured arrow bound here; it is
-- not an Eq-Free Bottom declaration merely because its local binder has
-- disappeared from the current packet view.
groundAnnotationThroughPartiallyAppliedLambdaSpineExpr :: Surf.SurfaceExpr
groundAnnotationThroughPartiallyAppliedLambdaSpineExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 8))
                                    (Surf.STBase "Int")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- The next seed-1003 counterexample.  The annotated self-application is
-- returned through three identity applications, generalized by a let, and
-- then returned from an applied lambda.  The descendant completion for the
-- let result must be joined to the owner's current exact Gamma state before
-- the outer application installs it.
annotatedSelfApplicationThroughAppliedLetIdentityChainExpr :: Surf.SurfaceExpr
annotatedSelfApplicationThroughAppliedLetIdentityChainExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        annotatedSelfAppExpr
                    )
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool False))

-- The next seed-1003 counterexample.  The paper self-application is returned
-- through two annotated-lambda applications, captured by a let, and then
-- carried across an unrelated outer application.  Its exact constructed
-- result must survive the whole application/let path.
annotatedSelfApplicationThroughNestedAnnotatedApplicationsExpr :: Surf.SurfaceExpr
annotatedSelfApplicationThroughNestedAnnotatedApplicationsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 13))
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap7"
                                        (Surf.STBase "Int")
                                        annotatedSelfAppExpr
                                    )
                                    (Surf.ELit (Surf.LInt (-9)))
                                )
                            )
                            (Surf.ELit (Surf.LInt 5))
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from the seed-1011 counterexample that follows the completed
-- result-owner regression.  The paper self-application is first returned
-- through two identity applications, then through a transparent let and an
-- unapplied lambda beneath two applied annotated lambdas.  The outer
-- application packet must preserve the source-owned @g@ forall while the
-- nested lambda construction publishes its bounded result declaration.
paperGgThroughNestedIdentityArgumentsBeneathAnnotatedLambdasExpr ::
  Surf.SurfaceExpr
paperGgThroughNestedIdentityArgumentsBeneathAnnotatedLambdasExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap3"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap4"
            ( Surf.ELet
                "_generatedWrap5"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap6"
                        (Surf.EVar "_generatedWrap6")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap7"
                            (Surf.EVar "_generatedWrap7")
                        )
                        annotatedSelfAppExpr
                    )
                )
                (Surf.EVar "_generatedWrap5")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-1)))

-- Minimized from the first seed-1012 counterexample.  The transparent let and
-- the unapplied lambda are both required: removing either lets the bounded
-- annotation follow its ordinary source path.  Together they require the
-- enclosing applied lambda to construct the source-owned bound through the
-- returned value-lambda spine.
letBoundBoundedIdentityThroughNestedLambdaExpr :: Surf.SurfaceExpr
letBoundBoundedIdentityThroughNestedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap4"
            ( Surf.ELet
                "_generatedWrap5"
                boundedIdentityAnnotationExpr
                (Surf.EVar "_generatedWrap5")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

letBoundBoundedIdentityThroughTwoNestedLambdasExpr :: Surf.SurfaceExpr
letBoundBoundedIdentityThroughTwoNestedLambdasExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    boundedIdentityAnnotationExpr
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

letBoundBoundedIdentityThroughIgnoredLetAndNestedLambdaExpr :: Surf.SurfaceExpr
letBoundBoundedIdentityThroughIgnoredLetAndNestedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt 3))
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    boundedIdentityAnnotationExpr
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

letBoundBoundedIdentityThroughIgnoredLetAndTwoNestedLambdasExpr :: Surf.SurfaceExpr
letBoundBoundedIdentityThroughIgnoredLetAndTwoNestedLambdasExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 3))
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        boundedIdentityAnnotationExpr
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

-- Minimized from seed 1004.  The explicitly polymorphic identity is returned
-- by a let beneath three applied lambdas.  Its descendant packet consumer must
-- be completed in the enclosing lambda construction without projecting away
-- either the annotation-owned forall or the returned lambda parameter.
annotatedIdentityThroughNestedAppliedLetChainExpr :: Surf.SurfaceExpr
annotatedIdentityThroughNestedAppliedLetChainExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap5"
                                    (Surf.EVar "_generatedWrap5")
                                )
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool True))
                                            (Surf.STBase "Bool")
                                        )
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedX"
                                                (Surf.EVar "_generatedSeedX")
                                            )
                                            ( Surf.STForall
                                                "a"
                                                Nothing
                                                ( Surf.STArrow
                                                    (Surf.STVar "a")
                                                    (Surf.STVar "a")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from seed 1005.  A direct identity application returns a ground
-- annotated let result beneath three unapplied lambdas.  The inherited exact
-- codomain must retain the checked body's completed bounded result instead of
-- reopening that binder at its provisional unbounded packet view.
groundIdentityApplicationThroughNestedLambdasExpr :: Surf.SurfaceExpr
groundIdentityApplicationThroughNestedLambdasExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from seed 1006.  The paper's annotated self-application passes
-- through two direct identity applications, an applied ground-parameter
-- lambda, and three result-transparent lets.  The single enclosing Gamma
-- declaration must keep its source-owned sigma-id bound and graph identity
-- throughout that whole owner chain.
paperGgThroughNestedTransparentLetsAndIdentityApplicationsExpr :: Surf.SurfaceExpr
paperGgThroughNestedTransparentLetsAndIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            annotatedSelfAppExpr
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-10)))
            )
            (Surf.EVar "_generatedWrap3")
        )
        (Surf.EVar "_generatedWrap2")
    )
    (Surf.EVar "_generatedWrap1")

-- Minimized from the next seed-1006 counterexample.  The annotated constant
-- function carries one existential source variable and one explicit forall.
-- After both annotation arguments are consumed, the surrounding partial
-- lambda application must select the specialized arrow endpoint instead of
-- treating the original forall endpoint as a competing construction.
mixedAnnotationConstantThroughPartiallyAppliedLambdaSpineExpr
  :: Surf.SurfaceExpr
mixedAnnotationConstantThroughPartiallyAppliedLambdaSpineExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.ELet
                                "k"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        ( Surf.ELam
                                            "y"
                                            (Surf.EVar "y")
                                        )
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                                ( Surf.EApp
                                    ( Surf.EApp
                                        (Surf.EVar "k")
                                        (Surf.ELit (Surf.LInt 1))
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )

-- Frozen from seed 1371007569 after the mixed-annotation partial-spine
-- regression was fixed.  The prepared administrative endpoint retains the
-- source declaration's more-general bound, while its checked body publishes
-- the exact specialized construction bound.  The latter must advance the
-- same packet declaration before final lambda construction.
preparedSourceBoundThroughCheckedBodyCompletionExpr
  :: Surf.SurfaceExpr
preparedSourceBoundThroughCheckedBodyCompletionExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap6"
                                        (Surf.STBase "Int")
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            ( Surf.EAnn
                                                (Surf.ELit (Surf.LBool False))
                                                (Surf.STBase "Bool")
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LInt (-4)))
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.ELit (Surf.LInt 10))

-- Frozen from the next counterexample under seed 1371007569.  The direct
-- application edge carries the complete prepared Lambda(Gamma), while the
-- forwarded edge observes its exact body after those declarations enter
-- scope.  Their shared closure is the authority for that opening.
preparedApplicationGammaAlongForwardedEdgeExpr
  :: Surf.SurfaceExpr
preparedApplicationGammaAlongForwardedEdgeExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool True))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                                (Surf.EVar "_generatedWrap7")
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt 9))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from case 19 of generated seed 1004.  The applied lambda returns a
-- let-bound paper @g g@ value by the let's own resolved identity.  That exact
-- alias is structural proof that the RHS packet constructs the application's
-- immediate result; the enclosing flexible result is a later consumer, not
-- an endpoint to push into the RHS before its declaration is completed.
letAliasedPaperGgThroughAppliedLambdaResultExpr
  :: Surf.SurfaceExpr
letAliasedPaperGgThroughAppliedLambdaResultExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-10)))
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.ELet
                                "_generatedWrap6"
                                annotatedSelfAppExpr
                                (Surf.EVar "_generatedWrap6")
                            )
                        )
                        (Surf.ELit (Surf.LInt (-9)))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt 15))
    )

-- Frozen from case 35 of generated seed 1005.  The inner mixed annotation
-- specializes its result to Bool, then returns that checked application
-- through two value lambdas and several consumed wrapper parameters.  The
-- exact Bool endpoint belongs to the source specialization; the surrounding
-- flexible result proxy must not replace it before EApp is constructed.
mixedAnnotationThroughNestedAppliedLambdaOwnersExpr
  :: Surf.SurfaceExpr
mixedAnnotationThroughNestedAppliedLambdaOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap2"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap3"
                        ( Surf.ELet
                            "_generatedWrap4"
                            (Surf.ELit (Surf.LInt (-15)))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap5"
                                    ( Surf.EApp
                                        ( Surf.ELamAnn
                                            "_generatedWrap6"
                                            (Surf.STBase "Int")
                                            ( Surf.ELam
                                                "_generatedWrap7"
                                                nestedMixedAnnotationExpr
                                            )
                                        )
                                        (Surf.ELit (Surf.LInt 2))
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
            (Surf.ELit (Surf.LInt 5))
        )
    )
    (Surf.ELit (Surf.LInt 11))

-- Frozen from case 95 of generated seed 1013.  Unlike the source-resolved
-- mixed application above, the direct lambda returning paper @g g@ has no
-- independent source-application result certificate.  Its bounded result is
-- constructed by the inherited administrative Gamma and must remain abstract
-- while the lambda child is built.
paperGgGammaThroughDirectLambdaApplicationExpr :: Surf.SurfaceExpr
paperGgGammaThroughDirectLambdaApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                annotatedSelfAppExpr
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-13)))
        )
    )

-- Frozen from case 96 of generated seed 1020.  The checked application owns
-- the Bool declaration at graph node 32, while the returned annotated lambda
-- still reaches that declaration through result occurrence node 44.  Root
-- preparation must retain the exact owner-final @44 -> 32@ construction route
-- instead of requiring node 44 to appear as a separate root declaration.
applicationGammaAliasThroughOwnerFinalRouteExpr :: Surf.SurfaceExpr
applicationGammaAliasThroughOwnerFinalRouteExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 7))
                ( Surf.EAnn
                    ( Surf.ELam
                        "x"
                        (Surf.ELam "y" (Surf.EVar "y"))
                    )
                    ( Surf.STForall
                        "alpha"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "beta")
                            ( Surf.STArrow
                                (Surf.STVar "alpha")
                                (Surf.STVar "alpha")
                            )
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from case 81 of generated seed 1003922807.  The outer application
-- specializes its result to Bool through graph node 34, while the nested
-- source-owned scheme uses that same graph occurrence beneath its lexical
-- alpha binder.  Entering the RHS must keep the source occurrence local
-- instead of quotienting the independent application Gamma into alpha.
sourceOwnedLetSchemeBeneathApplicationGammaExpr :: Surf.SurfaceExpr
sourceOwnedLetSchemeBeneathApplicationGammaExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt 16))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        (Surf.EVar "_generatedWrap5")
                    )
                    nestedMixedAnnotationExpr
                )
                (Surf.EVar "_generatedWrap4")
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from case 100 of generated seed 1195910434.  The outer annotated
-- application fixes only its Int parameter; the unapplied inner lambda must
-- retain the exact source-owned scheme used by paper @g g@ while its result
-- is constructed through the enclosing lambda Gamma.
paperGgBeneathAppliedAnnotatedLambdaExpr :: Surf.SurfaceExpr
paperGgBeneathAppliedAnnotatedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt (-8)))
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELamAnn
                        "g"
                        sigmaIdSource
                        (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt 12))

-- Frozen from case 97 of generated seed 1371007569.  The applied annotated
-- lambda returns a let-published source scheme with an implicit beta binder.
-- Its exact application result must be constructed from that checked body;
-- the allocated graph result is not an independently usable codomain.
mixedAnnotationLetThroughAppliedAnnotatedLambdaExpr :: Surf.SurfaceExpr
mixedAnnotationLetThroughAppliedAnnotatedLambdaExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 4))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt (-4)))
                        ( Surf.ELet
                            "_generatedWrap6"
                            mixedAnnotationExpr
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 15))
            )
        )
    )

-- Frozen from case 45 of generated seed 2147483646.  The nested application
-- specializes the body result of the annotated Int lambda after its enclosing
-- result owner has already allocated a graph binder for that result.  The
-- owner must publish its exact checked bound, not the pre-body graph
-- presentation retained by its prepared local spine.
returnedPolymorphicParameterThroughApplicationOwnerExpr :: Surf.SurfaceExpr
returnedPolymorphicParameterThroughApplicationOwnerExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 0))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 10))
            )
        )
    )

-- Frozen from case 16 of generated seed 2040442873.  The applied outer lambda
-- returns an unapplied lambda whose body transports the complete paper @g g@
-- construction through a direct identity application.  The enclosing exact
-- endpoint must start from that body's owner-final construction, rather than
-- replaying the earlier graph-edge source after the child Gamma is complete.
identityAppliedPaperGgThroughAppliedOuterLambdaExpr :: Surf.SurfaceExpr
identityAppliedPaperGgThroughAppliedOuterLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                annotatedSelfAppExpr
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from case 44 of generated seed 2040442873.  The returned inner lambda
-- has a ground application result, but the enclosing packet still names that
-- result by its unbounded graph declaration.  The child owner-final
-- certificate records the exact returned application owner and must complete
-- that declaration before the outer lambda emits its packet.
nestedReturnedApplicationResultBeforeOuterPublicationExpr
  :: Surf.SurfaceExpr
nestedReturnedApplicationResultBeforeOuterPublicationExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 1))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap7"
                                        (Surf.STBase "Int")
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool True))
                                            (Surf.STBase "Bool")
                                        )
                                    )
                                    (Surf.ELit (Surf.LInt 11))
                                )
                            )
                            (Surf.ELit (Surf.LInt (-15)))
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt (-15)))
    )

-- Frozen from case 91 of generated seed 2040442873.  A higher-rank @g g@
-- lambda is returned by an application, forwarded through a transparent let,
-- and then published through two enclosing lambda owners.  The enclosing
-- construction must retain the exact source-owned identity bound on the
-- returned result instead of replaying a provisional graph presentation.
paperGgThroughAppliedLambdaResultOwnerChainExpr :: Surf.SurfaceExpr
paperGgThroughAppliedLambdaResultOwnerChainExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            annotatedSelfAppExpr
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt 2))

-- Frozen from the 100th generated case of seed 1120133952.  The explicit
-- polymorphic identity is used at Int and Bool inside a lambda returned through
-- another lambda and a transparent let.  Its source forall must remain lexical
-- to the let scheme instead of escaping as a free graph identity when the two
-- enclosing lambda owners publish their result schemes.
multiUsePolymorphicLetThroughReturnedLambdaOwnersExpr :: Surf.SurfaceExpr
multiUsePolymorphicLetThroughReturnedLambdaOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-8)))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap6"
                        (Surf.ELit (Surf.LInt (-1)))
                        ( Surf.ELet
                            "_generatedWrap7"
                            (Surf.ELit (Surf.LInt (-2)))
                            ( Surf.ELet
                                "_generatedSeedId"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedArg"
                                        (Surf.EVar "_generatedSeedArg")
                                    )
                                    sigmaIdSource
                                )
                                ( Surf.ELet
                                    "_generatedSeedDiscard"
                                    ( Surf.EApp
                                        (Surf.EVar "_generatedSeedId")
                                        (Surf.ELit (Surf.LInt 15))
                                    )
                                    ( Surf.EApp
                                        (Surf.EVar "_generatedSeedId")
                                        (Surf.ELit (Surf.LBool True))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from case 89 of generated seed 1435051581.  The innermost exact
-- higher-rank parameter application has already constructed @Int@, while the
-- enclosing lambda Gamma still carries the application's graph result in the
-- bound of its returned-function declaration.  The completed child endpoint
-- must enter that requirement before the enclosing declaration is checked.
returnedHigherRankApplicationBeforeEnclosingLambdaGammaExpr
  :: Surf.SurfaceExpr
returnedHigherRankApplicationBeforeEnclosingLambdaGammaExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 12))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from case 33 of generated seed 195565654.  The two identity
-- applications preserve a lambda whose parameter is explicitly higher-rank;
-- an enclosing ignored-argument application and let publication must not move
-- that source forall from the parameter into the returned function's outer
-- binder spine.
returnedHigherRankParameterBeneathIgnoredApplicationExpr :: Surf.SurfaceExpr
returnedHigherRankParameterBeneathIgnoredApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELamAnn
                        "_generatedSeedPoly"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "_generatedSeedPoly")
                            (Surf.ELit (Surf.LInt 10))
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from case 58 of generated seed 937635187.  The annotated identity's
-- source forall is copied into two graph occurrences while the surrounding
-- wrapper remains partially applied.  Those occurrences may share one solved
-- representative, but endpoint projection must retain source authority until
-- the current SchemeInfo supplies an exact occurrence route rather than
-- require the representative to have only one outward binder globally.
repeatedSourceOccurrencesBeneathPartialWrappersExpr :: Surf.SurfaceExpr
repeatedSourceOccurrencesBeneathPartialWrappersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from case 66 of generated seed 449181304.  The returned inner
-- function first owns a flexible result bounded by the complete polymorphic
-- identity, while the enclosing exact Gamma specializes that bound beneath
-- an ignored value lambda.  The certified computation must be applied at the
-- returned lambda's codomain before the enclosing declaration is published;
-- an xMLF instantiation cannot cross the value arrow by itself.
completedReturnedFunctionBoundBeneathNestedLambdasExpr :: Surf.SurfaceExpr
completedReturnedFunctionBoundBeneathNestedLambdasExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.ELamAnn
                        "g"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "g")
                            (Surf.EVar "g")
                        )
                    )
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
    )

-- Frozen from the third seed-1006 counterexample.  The exact polymorphic
-- parameter body reaches the enclosing operated spine as the lower bound of
-- a flexible result proxy.  Closing that source-owned parameter must update
-- both the proxy bound and the lambda domain before the surrounding applied
-- annotated lambdas publish their result.
paperGgThroughNestedAnnotatedApplicationsAndTransparentLetsExpr
  :: Surf.SurfaceExpr
paperGgThroughNestedAnnotatedApplicationsAndTransparentLetsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt 0))
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt 7))
                                    annotatedSelfAppExpr
                                )
                                (Surf.EVar "_generatedWrap6")
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 9))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt (-7)))
    )

-- Frozen from the first seed-1007 counterexample.  Two consumed certificates
-- for one owner carry a generalized declaration and its checked instance.
-- The certificate whose previous state is Bottom is the construction origin
-- of that declaration even when xMLF Intro makes both endpoints constructible.
polymorphicLambdaApplicationThroughNestedAppliedWrappersExpr
  :: Surf.SurfaceExpr
polymorphicLambdaApplicationThroughNestedAppliedWrappersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap2"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            (Surf.EVar "_generatedWrap4")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        ( Surf.EApp
                                            ( Surf.ELam
                                                "_generatedWrap7"
                                                ( Surf.ELamAnn
                                                    "_generatedSeedPoly"
                                                    sigmaIdSource
                                                    ( Surf.EApp
                                                        (Surf.EVar "_generatedSeedPoly")
                                                        (Surf.ELit (Surf.LInt 4))
                                                    )
                                                )
                                            )
                                            (Surf.ELit (Surf.LBool False))
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                            (Surf.ELit (Surf.LInt 10))
                        )
                    )
                    (Surf.EVar "_generatedWrap3")
                )
            )
            (Surf.ELit (Surf.LInt (-9)))
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the second seed-1007 counterexample.  The direct application
-- closes the existential source binder while a forwarded edge keeps that
-- identity open beneath their shared leading forall.  Their exact closure
-- certificate must retain the closed declaration endpoint.
mixedAnnotationThroughForwardedAndDirectApplicationEdgesExpr
  :: Surf.SurfaceExpr
mixedAnnotationThroughForwardedAndDirectApplicationEdgesExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 12))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt 11))
                                    mixedAnnotationExpr
                                )
                                (Surf.EVar "_generatedWrap6")
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from the third seed-1007 counterexample.  The exact sigma-id
-- parameter is opened in a checked descendant body while an unrelated,
-- vacuous Gamma binder is already lexical.  Identity-aware scope tracking
-- permits closing sigma-id below that forall without duplicating its own @a@.
exactParameterBeneathUnrelatedGammaForallExpr :: Surf.SurfaceExpr
exactParameterBeneathUnrelatedGammaForallExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        (Surf.ELit (Surf.LInt (-13)))
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt 1))
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.ELit (Surf.LInt 11))

-- Frozen from the first seed-1008 counterexample.  The direct identity
-- application sees the exact argument lambda's source binder free in an
-- ambient result declaration.  Its carried parameter-boundary certificate
-- and graph-to-source route close that occurrence to the complete
-- @forall a. a -> a@ parameter before the direct Gamma claim is published.
directAmbientSourceRootAtCheckedParameterEndpointExpr
  :: Surf.SurfaceExpr
directAmbientSourceRootAtCheckedParameterEndpointExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELamAnn
                    "_generatedSeedPoly"
                    sigmaIdSource
                    ( Surf.EApp
                        (Surf.EVar "_generatedSeedPoly")
                        (Surf.ELit (Surf.LInt (-10)))
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the second seed-1008 counterexample.  The let-bound child has
-- already applied Hyp to publish the flexible result declaration.  The
-- enclosing packet still presents that occurrence at its bound; lambda
-- construction must retain the checked declaration occurrence instead of
-- attempting the unavailable reverse variable-to-bound computation.
boundedDeclarationAlreadyAbstractedByCheckedBodyExpr
  :: Surf.SurfaceExpr
boundedDeclarationAlreadyAbstractedByCheckedBodyExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt (-6)))
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt 16))
    )

-- Frozen from the first seed-1009 counterexample.  The annotated child has
-- already checked its source-owned @forall c >= forall a. a -> a@.  Its
-- enclosing value lambda must commute that type abstraction outward using
-- the exact bound-level M/N computation, rather than comparing the packet's
-- pre-completion graph presentation by shape or repairing it afterwards.
sourceBoundedForallAcrossAppliedLambdaExpr :: Surf.SurfaceExpr
sourceBoundedForallAcrossAppliedLambdaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            boundedIdentityAnnotationExpr
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 3))
            )
        )
        (Surf.ELit (Surf.LInt 7))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the second seed-1009 counterexample.  The annotation explicitly
-- owns @forall alpha@ while its free @beta@ is generalized by the annotation
-- owner.  A surrounding applied value lambda must retain the checked
-- @forall beta. forall alpha@ closure before replaying the outgoing edge;
-- specializing @beta@ first loses the source-owned construction.
implicitlyGeneralizedAnnotationThroughAppliedLambdaExpr :: Surf.SurfaceExpr
implicitlyGeneralizedAnnotationThroughAppliedLambdaExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.EAnn (Surf.ELit (Surf.LBool True)) (Surf.STBase "Bool"))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 0))
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        (Surf.ELam "y" (Surf.EVar "y"))
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from seed 1780810941 of O15-ELAB-GENERATED.  The innermost lambda
-- constructs @forall d. d -> Bool@ after the annotated @k@ is instantiated
-- twice.  Each enclosing lambda packet contributes an earlier unbounded
-- declaration; the next owner must recognize the exact child construction as
-- the tail of its operated packet instead of requiring the child to recreate
-- the whole enclosing prefix.
childConstructedLambdaTailThroughFourPacketsExpr :: Surf.SurfaceExpr
childConstructedLambdaTailThroughFourPacketsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "k"
                        ( Surf.EAnn
                            (Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "y")))
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                        ( Surf.EApp
                            ( Surf.EApp
                                (Surf.EVar "k")
                                (Surf.ELit (Surf.LInt 1))
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
        )
    )

-- Frozen from the second seed-1780810941 counterexample of
-- O15-ELAB-GENERATED.  The source annotation owns @forall alpha@ and its
-- free @beta@ is generalized by the enclosing construction.  The annotated
-- applied lambda must carry that exact construction-owned declaration through
-- its result packet instead of rediscovering @beta@ as a free scheme variable
-- during finalization.
implicitAnnotationBinderThroughAnnotatedAppliedLambdaExpr :: Surf.SurfaceExpr
implicitAnnotationBinderThroughAnnotatedAppliedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "y")))
                    ( Surf.STForall
                        "alpha"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "beta")
                            ( Surf.STArrow
                                (Surf.STVar "alpha")
                                (Surf.STVar "alpha")
                            )
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-11)))

-- Frozen from seed 1070269036 of O15-ELAB-GENERATED.  The source annotation
-- contributes the enclosing @forall a@ while its checked identity body is
-- published by a nested application packet.  Placement must compare that
-- packet's owned tail after retaining the exact source-owned prefix; requiring
-- the descendant packet to reconstruct the prefix gives the source binder two
-- owners.
enclosingSourceForallWithNestedPacketTailExpr :: Surf.SurfaceExpr
enclosingSourceForallWithNestedPacketTailExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap4"
                            (Surf.STBase "Int")
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "_generatedSeedX"
                                    (Surf.EVar "_generatedSeedX")
                                )
                                ( Surf.STForall
                                    "a"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "a")
                                        (Surf.STVar "a")
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 15))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from seed 2090131543 of O15-ELAB-GENERATED.  The annotation's free
-- @beta@ and quantified @alpha@ are independent unbounded source declarations.
-- Root reification may legally publish those declarations in the opposite
-- order from the enclosing Gamma.  Packet placement must project the exact
-- declarations by identity before retaining the nested consumer bound; their
-- presentation order cannot give the descendant packet ownership of either
-- source declaration.
independentlyOrderedSourceBindersBeforePacketTailExpr :: Surf.SurfaceExpr
independentlyOrderedSourceBindersBeforePacketTailExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                (Surf.ELam "y" (Surf.EVar "y"))
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
                (Surf.EVar "_generatedWrap3")
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 16))

-- Frozen from the 100th case of seed 2026081303 of O15-ELAB-GENERATED.  The
-- outer identity application returns an unapplied lambda.  Beneath that
-- lambda, an unrelated annotated let precedes a let-bound bounded identity.
-- Root publication must retain the application Gamma's exterior identity
-- while carrying the lambda's exact returned construction; the source-owned
-- bounded declaration cannot stand in for that exterior route.
boundedIdentityThroughIdentityReturnedLambdaSeed2026081303Expr
  :: Surf.SurfaceExpr
boundedIdentityThroughIdentityReturnedLambdaSeed2026081303Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.ELet
                "_generatedWrap4"
                boundedIdentityAnnotationExpr
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

-- Frozen from the 67th case of seed 2026081304 of O15-ELAB-GENERATED.  A
-- nested identity/application spine returns a lambda whose annotated
-- parameter is used at Int.  The enclosing application must select the
-- checked returned-lambda construction before validating the provisional
-- graph endpoint presentation.
annotatedReturnedLambdaBeforeApplicationEndpointSeed2026081304Expr
  :: Surf.SurfaceExpr
annotatedReturnedLambdaBeforeApplicationEndpointSeed2026081304Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        (Surf.EVar "_generatedWrap7")
                                    )
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt (-11)))
                                        )
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from the 67th case of seed 1496664322 of O15-ELAB-GENERATED.  The
-- nested annotated lambda consumes its body-consumer declaration into the
-- returned value before an identity application publishes its Gamma.  The
-- parent application must omit that consumed identity while constructing its
-- refinement binders; adding it and filtering it only after refinement tries
-- to publish a declaration which is already lexically closed by the child.
consumedDescendantBeforeApplicationGammaSeed1496664322Expr
  :: Surf.SurfaceExpr
consumedDescendantBeforeApplicationGammaSeed1496664322Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELamAnn
                        "g"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "g")
                            (Surf.EVar "g")
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt 15))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 55th case of seed 1987654321 of O15-ELAB-GENERATED.  The
-- nested applied value lambdas place alpha-equivalent returned closures in
-- distinct lexical scopes.  The child owner must close its returned forall
-- first, then publish it through the exact Inside(Hyp);N route selected by
-- the enclosing result declaration.  A direct endpoint plan cannot treat two
-- repeated binder identities in sibling scopes as one constructed endpoint.
closedReturnedForallBeforeSiblingLambdaPublicationSeed1987654321Expr
  :: Surf.SurfaceExpr
closedReturnedForallBeforeSiblingLambdaPublicationSeed1987654321Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELamAnn
                                        "g"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "g")
                                            (Surf.EVar "g")
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
                (Surf.EVar "_generatedWrap3")
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 6))

-- Frozen from the 53rd case of seed 145673209 of O15-ELAB-GENERATED.  The
-- applied annotated lambda returns a mixed-annotation construction through a
-- nested value lambda.  Its child carries an alias from the enclosing graph
-- occurrence to the child's result declaration, while the current owner
-- emits a fresh declaration for that same occurrence.  The current route may
-- shadow only that alias; the carried declaration keeps its intrinsic route
-- as the construction authority used by the enclosing lambdas.
currentOwnerRouteShadowsCarriedAliasSeed145673209Expr
  :: Surf.SurfaceExpr
currentOwnerRouteShadowsCarriedAliasSeed145673209Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt 12))
                                    mixedAnnotationExpr
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 5))
            )
        )
    )

-- Frozen from the 100th case of seed 1070269036 of O15-ELAB-GENERATED after
-- its earlier packet-tail counterexample was fixed.  The paper @g g@ result
-- crosses three applied annotated lambdas, an applied discarded lambda, and
-- a transparent let publication.  Each constructor must carry the same
-- checked result endpoint; an outer application must not fall back to the
-- pre-construction graph result after the inner owner has finalized it.
paperGgThroughNestedAppliedLetPublicationExpr :: Surf.SurfaceExpr
paperGgThroughNestedAppliedLetPublicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        ( Surf.ELamAnn
                                            "g"
                                            sigmaIdSource
                                            ( Surf.EApp
                                                (Surf.EVar "g")
                                                (Surf.EVar "g")
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool False))
                                )
                            )
                            (Surf.ELit (Surf.LInt 4))
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LInt 6))
            )
        )
    )
    (Surf.ELit (Surf.LInt 7))

-- Frozen from seed 1000 of O15-ELAB-GENERATED.  The two identity
-- applications and intervening let are result-transparent.  They must carry
-- the checked application's owner endpoint and its escaped source
-- existential to root construction; publishing only the graph result node
-- loses both pieces of positive construction evidence.
sourceExistentialAfterTransparentRootApplicationsExpr :: Surf.SurfaceExpr
sourceExistentialAfterTransparentRootApplicationsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt (-14)))
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                (Surf.ELit (Surf.LInt (-12)))
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        (Surf.ELam "y" (Surf.EVar "y"))
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-5)))
            )
        )
    )

-- Frozen from seed 1001 of O15-ELAB-GENERATED.  The recursively checked body
-- has already constructed the source-bounded annotation declaration and its
-- completed consumer beneath two returned lambdas.  The enclosing applied
-- lambda must use that child owner spine as its exact codomain construction;
-- its pre-body packet contains only the administrative outer lambda prefix.
boundedSourceDeclarationThroughAppliedLambdaSpineExpr :: Surf.SurfaceExpr
boundedSourceDeclarationThroughAppliedLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.EAnn
                                        (Surf.ELam "x" (Surf.EVar "x"))
                                        ( Surf.STForall
                                            "a"
                                            ( Just
                                                ( Surf.SrcBound
                                                    ( Surf.STForall
                                                        "a"
                                                        Nothing
                                                        ( Surf.STArrow
                                                            (Surf.STVar "a")
                                                            (Surf.STVar "a")
                                                        )
                                                    )
                                                )
                                            )
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-13)))
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )

-- Frozen from seed 1002 of O15-ELAB-GENERATED.  The outer identity
-- application sees the returned annotated identity once through its direct
-- argument edge and once through the result owner's completed construction.
-- Those are two presentations of one checked argument construction, so the
-- application must select their common exact endpoint before building EApp.
directIdentityEndpointWithChildOwnerEndpointExpr :: Surf.SurfaceExpr
directIdentityEndpointWithChildOwnerEndpointExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
    )

-- Frozen from the second seed-1002 counterexample of O15-ELAB-GENERATED.
-- The inner application completes the returned value at @Int@.  Its enclosing
-- lambda packet owns the remaining two-lambda result construction; the outer
-- annotated application must carry that checked child plan into its function
-- occurrence instead of reverting to the provisional graph result topology.
checkedChildResultPlanThroughAnnotatedAppliedLambdaExpr :: Surf.SurfaceExpr
checkedChildResultPlanThroughAnnotatedAppliedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 14))
                                    (Surf.STBase "Int")
                                )
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt 4))

-- Frozen from the third seed-1002 counterexample of O15-ELAB-GENERATED.  The
-- paper @g g@ construction is complete inside the annotated parameter lambda,
-- while transparent lets and three returned lambdas carry that construction
-- through an application of only the outermost lambda.
paperGgThroughAppliedLambdaAndTransparentLetsExpr :: Surf.SurfaceExpr
paperGgThroughAppliedLambdaAndTransparentLetsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 13))
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.ELamAnn
                                "g"
                                ( Surf.STForall
                                    "a"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "a")
                                        (Surf.STVar "a")
                                    )
                                )
                                (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from seed 1003 of O15-ELAB-GENERATED.  The annotated returned
-- identity completes a packet-owned result declaration after the enclosing
-- administrative lambda view has already installed its provisional bound.
-- The completed body-consumer transition must advance that exact Gamma
-- declaration before the completed scheme is entered.
completedBodyConsumerBoundBeforeAdministrativeGammaExpr :: Surf.SurfaceExpr
completedBodyConsumerBoundBeforeAdministrativeGammaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt 9))
    )

-- Frozen from the second seed-1003 counterexample of
-- O15-ELAB-GENERATED.  The annotated polymorphic parameter is specialized at
-- @Int@ inside a returned lambda, while the enclosing lambda is immediately
-- applied to an ignored @Bool@.  The packet-owned completed result must be
-- available while the function lambda is constructed, not recovered from its
-- final type after the application.
specializedPolymorphicParameterBeneathIgnoredApplicationExpr :: Surf.SurfaceExpr
specializedPolymorphicParameterBeneathIgnoredApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            ( Surf.STForall
                                "a"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "a")
                                    (Surf.STVar "a")
                                )
                            )
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt (-10)))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the third seed-1003 counterexample of O15-ELAB-GENERATED.  A
-- returned paper @g g@ construction is nested beneath two ordinary
-- applications and then supplied to an identity function.  The application
-- argument Gamma must retain the checked lambda owner's exact result instead
-- of rebuilding the function from the provisional graph topology.
paperGgThroughNestedApplicationArgumentGammaExpr :: Surf.SurfaceExpr
paperGgThroughNestedApplicationArgumentGammaExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-2)))
                                    ( Surf.ELamAnn
                                        "g"
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "g")
                                            (Surf.EVar "g")
                                        )
                                    )
                                )
                                (Surf.EVar "_generatedWrap6")
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt (-6)))
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the first seed-1006 counterexample of O15-ELAB-GENERATED.  The
-- nested annotated constant leaves the enclosing function with provisional
-- result @b@, while the application Gamma already owns the exact ambient
-- declaration @b >= Bool@.  The function occurrence must be constructed at
-- @Bool -> Bool@ before the application is checked; a later result rewrite
-- cannot justify the missing xMLF computation.
ambientApplicationResultAtExactFunctionEndpointExpr :: Surf.SurfaceExpr
ambientApplicationResultAtExactFunctionEndpointExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELet
                                    "k"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                    ( Surf.EApp
                                        ( Surf.EApp
                                            (Surf.EVar "k")
                                            (Surf.ELit (Surf.LInt 1))
                                        )
                                        (Surf.ELit (Surf.LBool True))
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 9))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the first seed-1008 counterexample of O15-ELAB-GENERATED.  The
-- annotated constant is applied before its result is named by the local let,
-- then that binding is returned through an enclosing applied lambda.  The
-- annotation-owned @beta@ and @alpha@ construction must therefore survive
-- both the let boundary and the outer application Gamma; reconstructing only
-- the final result shape after either boundary loses the checked computation.
annotatedConstantResultThroughAppliedLetBodyExpr :: Surf.SurfaceExpr
annotatedConstantResultThroughAppliedLetBodyExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 14))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                ( Surf.ELam
                                    "y"
                                    (Surf.EVar "y")
                                )
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

-- Frozen from the first seed-1010 counterexample of O15-ELAB-GENERATED.  The
-- identity application returns a two-lambda argument whose deepest result is
-- the mixed annotation.  Its free @beta@ is one construction-owned binder;
-- carrying the same identity once from the argument and once from the
-- application owner must coalesce that declaration before the let publishes
-- the completed RHS scheme.
implicitAnnotationBinderThroughIdentityArgumentExpr :: Surf.SurfaceExpr
implicitAnnotationBinderThroughIdentityArgumentExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-5)))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELam
                        "_generatedWrap6"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap7"
                                (Surf.STBase "Int")
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        ( Surf.ELam
                                            "y"
                                            (Surf.EVar "y")
                                        )
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt (-4)))
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the first seed-1012 counterexample of O15-ELAB-GENERATED.  The
-- inner application fixes the annotated lambda's first parameter before its
-- two ordinary lambda parameters and transparent let return a polymorphic
-- value.  The outer identity application must use that checked child result
-- construction as its function endpoint instead of the earlier graph
-- presentation of the same application result.
appliedAnnotatedLambdaResultThroughIdentityArgumentExpr :: Surf.SurfaceExpr
appliedAnnotatedLambdaResultThroughIdentityArgumentExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt (-10)))
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "x"
                                    ( Surf.ELam
                                        "y"
                                        (Surf.EVar "y")
                                    )
                                )
                                ( Surf.STForall
                                    "alpha"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "beta")
                                        ( Surf.STArrow
                                            (Surf.STVar "alpha")
                                            (Surf.STVar "alpha")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt 5))
        )
    )

-- Frozen from the second seed-1012 counterexample of O15-ELAB-GENERATED.
-- The annotated parameter fixes one source forall while two enclosing value
-- lambdas and two identity applications carry its checked @Int@ result.  The
-- enclosing Gamma must retain the exact child result plan instead of
-- rebuilding a conflicting bound for the same graph identity.
annotatedPolymorphicArgumentThroughNestedIdentityApplicationsExpr ::
  Surf.SurfaceExpr
annotatedPolymorphicArgumentThroughNestedIdentityApplicationsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                ( Surf.ELamAnn
                    "_generatedSeedPoly"
                    ( Surf.STForall
                        "a"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "a")
                            (Surf.STVar "a")
                        )
                    )
                    ( Surf.EApp
                        (Surf.EVar "_generatedSeedPoly")
                        (Surf.ELit (Surf.LInt (-4)))
                    )
                )
            )
        )
    )

-- Frozen from the third seed-1012 counterexample of O15-ELAB-GENERATED.  The
-- paper's annotated @g g@ construction is returned through a transparent let
-- and two applied lambdas whose parameters are discarded.  Each enclosing
-- Lambda(Gamma) must carry the already certified result declaration and its
-- complete source-owned forall bound without rebuilding it from graph shape.
paperGgThroughNestedDiscardedApplicationsExpr :: Surf.SurfaceExpr
paperGgThroughNestedDiscardedApplicationsExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-14)))
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELamAnn
                                "g"
                                ( Surf.STForall
                                    "a"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "a")
                                        (Surf.STVar "a")
                                    )
                                )
                                ( Surf.EApp
                                    (Surf.EVar "g")
                                    (Surf.EVar "g")
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the fourth seed-1012 counterexample of O15-ELAB-GENERATED.  A
-- direct lambda returns the checked @Bool@ result of a polymorphic let, while
-- its application publishes that value through an exact local result binder
-- bounded by @Bool@.  The application must carry the child construction via
-- the binder's own Hyp step rather than demanding that @Bool@ and the flexible
-- result variable already be the same type.
returnedBoolThroughAppliedLambdaResultBinderExpr :: Surf.SurfaceExpr
returnedBoolThroughAppliedLambdaResultBinderExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap2"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        (Surf.ELit (Surf.LInt 7))
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.ELet
                                    "_generatedSeedId"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedArg"
                                            (Surf.EVar "_generatedSeedArg")
                                        )
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                    )
                                    ( Surf.ELet
                                        "_generatedSeedDiscard"
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LInt 8))
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LBool False))
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-15)))
        )
    )
    (Surf.ELit (Surf.LInt 6))

-- Frozen from the next seed-1010 counterexample after exact returned-result
-- publication was made structural.  The returned child already owns the
-- leading forall of its function value; an enclosing applied lambda may emit
-- a different local Gamma prefix, but it must not claim that child forall as
-- one of its own declarations merely because both lead the checked type.
childForallThroughEnclosingAppliedLambdaGammaExpr :: Surf.SurfaceExpr
childForallThroughEnclosingAppliedLambdaGammaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LInt (-8)))
                                (Surf.STBase "Int")
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the first seed-1000 counterexample in the extended generated
-- audit.  The applied wrapper returns an annotated lambda whose complete
-- flexible result type belongs at the wrapper body boundary; the nested
-- application's @Int@ packet is one lambda deeper and must not compete there.
returnedAnnotatedLambdaAboveBodyResultPacketExpr :: Surf.SurfaceExpr
returnedAnnotatedLambdaAboveBodyResultPacketExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELamAnn
                    "_generatedSeedPoly"
                    ( Surf.STForall
                        "a"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "a")
                            (Surf.STVar "a")
                        )
                    )
                    ( Surf.EApp
                        (Surf.EVar "_generatedSeedPoly")
                        (Surf.ELit (Surf.LInt 2))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the first seed-1005 counterexample in the extended generated
-- audit.  The annotation's implicit @beta@ is consumed by @k 1@.  It must not
-- be republished between the two value arrows of the enclosing returned
-- lambda after @k 1 True@ has constructed @Bool@.
consumedImplicitBinderOutsideEnclosingLambdaResultExpr :: Surf.SurfaceExpr
consumedImplicitBinderOutsideEnclosingLambdaResultExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "k"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                (Surf.ELam "y" (Surf.EVar "y"))
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                        ( Surf.EApp
                            ( Surf.EApp
                                (Surf.EVar "k")
                                (Surf.ELit (Surf.LInt 1))
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the first seed-1013 counterexample in the extended generated
-- audit.  The free @beta@ in the mixed annotation is a source declaration in
-- the enclosing scheme, while the topology consumer owns the constructed
-- @forall alpha. beta -> alpha -> alpha@ bound.  Packet placement must retain
-- @beta@ outside that bound rather than copying it as packet-local Gamma.
sourceDeclarationOutsideConstructedRaiseMergeBoundExpr :: Surf.SurfaceExpr
sourceDeclarationOutsideConstructedRaiseMergeBoundExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt (-6)))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool True))
                                            (Surf.STBase "Bool")
                                        )
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "x"
                                                ( Surf.ELam
                                                    "y"
                                                    (Surf.EVar "y")
                                                )
                                            )
                                            ( Surf.STForall
                                                "alpha"
                                                Nothing
                                                ( Surf.STArrow
                                                    (Surf.STVar "beta")
                                                    ( Surf.STArrow
                                                        (Surf.STVar "alpha")
                                                        (Surf.STVar "alpha")
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 0))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt 9))

-- Frozen from the second seed-1013 counterexample in the extended generated
-- audit.  The source declaration @a >= forall a. a -> a@ is captured by the
-- annotated identity, while its enclosing lambda/application construction
-- specializes a distinct result consumer.  Re-entering that consumer must
-- keep the source-owned bound instead of replacing it with the provisional
-- graph bound @bottom@.
sourceBoundThroughEnclosingApplicationSpecializationExpr :: Surf.SurfaceExpr
sourceBoundThroughEnclosingApplicationSpecializationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EAnn
                                        (Surf.ELam "x" (Surf.EVar "x"))
                                        ( Surf.STForall
                                            "a"
                                            ( Just
                                                ( Surf.SrcBound
                                                    ( Surf.STForall
                                                        "a"
                                                        Nothing
                                                        ( Surf.STArrow
                                                            (Surf.STVar "a")
                                                            (Surf.STVar "a")
                                                        )
                                                    )
                                                )
                                            )
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt 9))
    )

-- Frozen from the third seed-1013 counterexample in the extended generated
-- audit.  The nested annotated constant-function construction publishes its
-- free @beta@ as source-owned evidence through the child let/application.
-- The enclosing ordinary lambda must include that exact declaration when it
-- generalizes the already-constructed child endpoint; otherwise its scheme is
-- open even though the child owner certificate is closed.
childSourceDeclarationThroughEnclosingLambdaGeneralizationExpr ::
  Surf.SurfaceExpr
childSourceDeclarationThroughEnclosingLambdaGeneralizationExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-3)))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool True))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )

-- Frozen from seed 1015 in the extended generated audit.  The inner lambda
-- owner consumes its result declaration after constructing @alpha -> alpha@.
-- Returning that applied lambda from a let must project the certificate's
-- completed declaration state into the RHS root scheme, rather than comparing
-- it with the pre-construction graph presentation @d -> e@.
consumedChildDeclarationThroughReturnedLetSchemeExpr :: Surf.SurfaceExpr
consumedChildDeclarationThroughReturnedLetSchemeExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EAnn
                ( Surf.ELam
                    "x"
                    (Surf.ELam "y" (Surf.EVar "y"))
                )
                ( Surf.STForall
                    "alpha"
                    Nothing
                    ( Surf.STArrow
                        (Surf.STVar "beta")
                        ( Surf.STArrow
                            (Surf.STVar "alpha")
                            (Surf.STVar "alpha")
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the second seed-1015 counterexample in the extended generated
-- audit.  The identity application constructs its result declaration at
-- @a -> Bool@ after its argument lambda fixes that endpoint.  Both enclosing
-- lambdas must typecheck the already-constructed body in the same completed
-- declaration domain; retaining the provisional graph variable @c@ in the
-- exported InstAbstr leaves the body open.
completedApplicationResultThroughEnclosingLambdasExpr :: Surf.SurfaceExpr
completedApplicationResultThroughEnclosingLambdasExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap6"
                        (Surf.ELit (Surf.LInt 6))
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                    )
                )
            )
        )
    )

-- Frozen from seed 1016 in the extended generated audit.  The annotated
-- constant-function result owns @beta@ and @alpha@ beneath two value lambdas.
-- Applying the outer lambda must not also install those source declarations
-- in the application's ambient environment before checking the function:
-- their ETyAbs nodes already provide the lexical ownership.
sourcePolymorphicResultThroughOuterApplicationExpr :: Surf.SurfaceExpr
sourcePolymorphicResultThroughOuterApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EAnn
                    ( Surf.ELam
                        "x"
                        (Surf.ELam "y" (Surf.EVar "y"))
                    )
                    ( Surf.STForall
                        "alpha"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "beta")
                            ( Surf.STArrow
                                (Surf.STVar "alpha")
                                (Surf.STVar "alpha")
                            )
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the second seed-1016 counterexample in the extended generated
-- audit.  The application used as the body of @_generatedWrap5@ has already
-- constructed its exact @Bool@ endpoint, while the enclosing lambda packet
-- still exposes that result as its provisional graph declaration.  The
-- application owner's construction certificate must complete that packet
-- declaration before the enclosing lambda is planned.
completedApplicationBodyThroughLambdaPacketExpr :: Surf.SurfaceExpr
completedApplicationBodyThroughLambdaPacketExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "k"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                    ( Surf.EApp
                                        ( Surf.EApp
                                            (Surf.EVar "k")
                                            (Surf.ELit (Surf.LInt 1))
                                        )
                                        (Surf.ELit (Surf.LBool True))
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from seed 1017 in the extended generated audit.  The inner applied
-- annotated parameter returns a source-polymorphic identity, and the
-- recursively completed child lambda has already emitted both its graph
-- parameter and result declarations.  Its enclosing annotated application
-- must generalize from that owner-final endpoint instead of replaying the
-- earlier free graph result topology.
polymorphicResultThroughAppliedAnnotatedParametersExpr :: Surf.SurfaceExpr
polymorphicResultThroughAppliedAnnotatedParametersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            ( Surf.STForall
                                "a"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "a")
                                    (Surf.STVar "a")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt (-13)))
                )
            )
        )
        (Surf.ELit (Surf.LInt (-2)))
    )

-- Frozen from seed 1019 in the extended generated audit.  The local identity
-- is instantiated independently at @Int@ and @Bool@ before its final result
-- is returned through two applied annotated parameters, an unused lambda, and
-- two transparent lets.  Every enclosing owner must preserve the completed
-- child endpoint rather than rebuilding its provisional graph result.
multiUsePolymorphicLetThroughAppliedAnnotatedParametersExpr :: Surf.SurfaceExpr
multiUsePolymorphicLetThroughAppliedAnnotatedParametersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 2))
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap7"
                                        (Surf.STBase "Int")
                                        ( Surf.ELet
                                            "_generatedSeedId"
                                            ( Surf.EAnn
                                                ( Surf.ELam
                                                    "_generatedSeedArg"
                                                    ( Surf.EVar
                                                        "_generatedSeedArg"
                                                    )
                                                )
                                                ( Surf.STForall
                                                    "a"
                                                    Nothing
                                                    ( Surf.STArrow
                                                        (Surf.STVar "a")
                                                        (Surf.STVar "a")
                                                    )
                                                )
                                            )
                                            ( Surf.ELet
                                                "_generatedSeedDiscard"
                                                ( Surf.EApp
                                                    ( Surf.EVar
                                                        "_generatedSeedId"
                                                    )
                                                    ( Surf.ELit
                                                        (Surf.LInt (-12))
                                                    )
                                                )
                                                ( Surf.EApp
                                                    ( Surf.EVar
                                                        "_generatedSeedId"
                                                    )
                                                    ( Surf.ELit
                                                        (Surf.LBool False)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LInt (-12)))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 3))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 98th case of generated seed 1019.  The paper @g g@
-- construction is returned through an applied Bool parameter, then through
-- an unapplied lambda beneath two annotated/ordinary applications.  Let
-- publication must retain the exact lower bound of the returned result
-- abstraction; publishing it as an unbounded forall makes the emitted
-- 'InstAbstrRef' ill-typed.
paperGgThroughPartiallyAppliedAnnotatedParametersExpr :: Surf.SurfaceExpr
paperGgThroughPartiallyAppliedAnnotatedParametersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap4"
                            (Surf.STBase "Int")
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        ( Surf.ELamAnn
                                            "g"
                                            sigmaIdSource
                                            ( Surf.EApp
                                                (Surf.EVar "g")
                                                (Surf.EVar "g")
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool False))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 13))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LInt (-15)))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 72nd case of generated seed 1020.  The annotated outer
-- parameter returns a let-bound lambda whose body is fixed to Bool through
-- two identity applications.  Packet planning must close the lambda-domain
-- graph node even though the returned result is already ground.
annotatedGroundResultThroughNestedIdentityApplicationsExpr :: Surf.SurfaceExpr
annotatedGroundResultThroughNestedIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        (Surf.ELam "_generatedWrap5" (Surf.EVar "_generatedWrap5"))
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LInt 5))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 97th case of generated seed 541689707.  An applied lambda
-- returns a let-bound function whose result contains the paper @g g@
-- construction beneath an unapplied lambda.  The application result must use
-- the exact child construction endpoint rather than the graph topology that
-- preceded construction of the child's complete binder spine.
paperGgThroughAppliedLetReturningNestedLambdaExpr :: Surf.SurfaceExpr
paperGgThroughAppliedLetReturningNestedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELamAnn
                                "g"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "g")
                                    (Surf.EVar "g")
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

-- Frozen from the 77th case of generated seed 1545310598.  Two forwarded
-- edges for one application exterior observe successive states of the same
-- result: the first still carries the function from the paper @g g@ value to
-- its bounded result, while the second carries that result after application.
-- They must compose in edge order rather than compete as independent Gamma
-- declarations for the shared exterior.
paperGgThroughSequentialApplicationGammaExpr :: Surf.SurfaceExpr
paperGgThroughSequentialApplicationGammaExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELamAnn
                                        "g"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "g")
                                            (Surf.EVar "g")
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt 1))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt 4))
    )

-- Frozen from the 46th case of generated seed 477090250.  The identity
-- application returns the paper @g g@ construction through three unapplied
-- lambdas.  Its terminal bounded result is opened while the inner value is
-- built, then must be closed at the exact outer lambda publication boundary;
-- an @InstAbstr@ over the closed declaration cannot be checked against the
-- stale opened bound.
paperGgThroughIdentityAppliedDeepLambdaSpineExpr :: Surf.SurfaceExpr
paperGgThroughIdentityAppliedDeepLambdaSpineExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 3))
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt 13))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                ( Surf.ELamAnn
                                    "g"
                                    sigmaIdSource
                                    ( Surf.EApp
                                        (Surf.EVar "g")
                                        (Surf.EVar "g")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-7)))

-- Minimized from the 35th case of generated seed 20260807.  Two transparent
-- lets and two unapplied lambda owners publish the result of applying the
-- ordinary identity function to a let-wrapped paper @g g@ lambda.  Removing
-- either outer let avoids the failing owner chain; construction must carry the
-- paper binder through the complete chain instead of leaving its rigid result
-- free.
paperGgThroughIdentityAppliedReturnedLetExpr :: Surf.SurfaceExpr
paperGgThroughIdentityAppliedReturnedLetExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LBool True))
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LBool False))
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap6"
                            (Surf.EVar "_generatedWrap6")
                        )
                        ( Surf.ELet
                            "_generatedWrap7"
                            (Surf.ELit (Surf.LInt 4))
                            ( Surf.ELamAnn
                                "g"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "g")
                                    (Surf.EVar "g")
                                )
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
    )

-- Frozen from the 82nd case of generated seed 20260807.  The inner annotated
-- Int lambda and annotated Int argument construct the same arrow endpoint,
-- then an ordinary identity application returns the enclosing lambda through
-- two transparent lets.  The completed xMLF term must compare those endpoints
-- in one binder-identity domain instead of rejecting two identical printed
-- types at the final application check.
annotatedGroundIdentityApplicationThroughNestedLetsExpr :: Surf.SurfaceExpr
annotatedGroundIdentityApplicationThroughNestedLetsExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.EAnn (Surf.ELit (Surf.LBool True)) (Surf.STBase "Bool"))
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap6"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 4))
                                    (Surf.STBase "Int")
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 10))
                    )
                    (Surf.EVar "_generatedWrap5")
                )
            )
        )
    )

-- Frozen from the 95th case of generated seed 20260807.  The applied annotated
-- Int lambda returns a lambda whose body completes a bounded Bool result
-- through two applications.  The enclosing lambda construction must carry
-- that completed body endpoint into its own exact forall spine rather than
-- replacing it with an unrelated provisional result binder.
annotatedGroundResultThroughAppliedLambdaSpineExpr :: Surf.SurfaceExpr
annotatedGroundResultThroughAppliedLambdaSpineExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
        (Surf.ELit (Surf.LInt (-1)))
    )

-- Frozen from the 26th case of generated seed 20260808.  The innermost
-- annotated Int result completes a polymorphic body-consumer bound, then two
-- applied lambdas consume that declaration before the outer application is
-- constructed.  Root construction must project the certified consumed state
-- instead of demanding that the no-longer-live declaration remain in Gamma.
consumedPolymorphicBoundThroughNestedAppliedLambdasExpr :: Surf.SurfaceExpr
consumedPolymorphicBoundThroughNestedAppliedLambdasExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LInt (-3)))
                                            (Surf.STBase "Int")
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 32nd case of generated seed 20260808.  Three nested
-- applications share one exterior while progressively constructing the
-- returned paper @g g@ value.  Their requirement bounds are stages of the
-- same identity-authoritative construction, so packet preparation must
-- coalesce them through the certified computation instead of requiring raw
-- type equality between the stages.
paperGgThroughSharedNestedApplicationExteriorExpr :: Surf.SurfaceExpr
paperGgThroughSharedNestedApplicationExteriorExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EApp
                                    ( Surf.ELamAnn
                                        "_generatedWrap6"
                                        (Surf.STBase "Int")
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            ( Surf.ELamAnn
                                                "g"
                                                sigmaIdSource
                                                ( Surf.EApp
                                                    (Surf.EVar "g")
                                                    (Surf.EVar "g")
                                                )
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LInt 11))
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt 5))

-- Frozen from the 54th case of generated seed 20260808.  The outer returned
-- let owns one exterior reached first through an identity application and
-- later through a source-annotated polymorphic application.  Those bounds are
-- stages of the same owner-final computation, so packet grouping must select
-- the declaration certified by that source owner rather than comparing the
-- intermediate operated types directly.
annotatedApplicationThroughSharedReturnedLetExteriorExpr :: Surf.SurfaceExpr
annotatedApplicationThroughSharedReturnedLetExteriorExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 16))
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt 1))
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 3))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt 12))
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 84th case of generated seed 20260808.  The applied outer
-- lambda returns a higher-rank annotated function through three identity
-- applications and an unrelated let.  Construction must carry the nested
-- source-owned result endpoint through the outer application's already-open
-- Gamma instead of rebuilding it from the final type.
higherRankResultThroughAppliedIdentitySpineExpr :: Surf.SurfaceExpr
higherRankResultThroughAppliedIdentitySpineExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 15))
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 0))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from the 100th case of generated seed 20260809.  The direct identity
-- application and its returned let share one application declaration.  The
-- owner-selected stage is initially unbounded, while the checked argument
-- constructs the exact paper @g g@ bound before that declaration is
-- published.
paperGgThroughDirectIdentityApplicationAndLetExpr :: Surf.SurfaceExpr
paperGgThroughDirectIdentityApplicationAndLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            annotatedSelfAppExpr
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from the 18th case of generated seed 20260810.  Two transparent
-- lets return paper @g g@ as the result of an applied annotated lambda.  The
-- child's closed ambient declaration and the enclosing application's opened
-- view are stages of one exact owner construction; let publication must carry
-- the certificate across that boundary rather than compare the two bounds as
-- unrelated Gamma entries.
paperGgThroughAnnotatedApplicationAndTransparentLetsExpr :: Surf.SurfaceExpr
paperGgThroughAnnotatedApplicationAndTransparentLetsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    annotatedSelfAppExpr
                    (Surf.EVar "_generatedWrap4")
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt 14))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 97th case of generated seed 20260813.  An explicitly
-- polymorphic identity passes through a direct identity application and is
-- returned by a partially applied three-lambda spine.  The prepared endpoint
-- keeps the complete source forall while the incoming body-owner endpoint is
-- its opened view; endpoint selection must compose the recorded owner
-- completion instead of treating the two construction stages as peers.
annotatedIdentityThroughPartiallyAppliedLambdaSpineExpr :: Surf.SurfaceExpr
annotatedIdentityThroughPartiallyAppliedLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 9))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 0))
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    sigmaIdSource
                                )
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 20th case of generated seed 20260814.  Paper @g g@ is
-- returned through two direct identity applications and a transparent let by
-- an applied lambda whose parameter is annotated at @Int@.  The child owner
-- completes the shared application declaration before the enclosing lambda
-- installs that descendant refinement; installation must follow the recorded
-- declaration construction rather than require the earlier ambient
-- presentation to remain unchanged.
paperGgThroughNestedIdentitiesUnderAppliedAnnotationExpr :: Surf.SurfaceExpr
paperGgThroughNestedIdentitiesUnderAppliedAnnotationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            annotatedSelfAppExpr
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt (-12)))
    )

-- The second minimized counterexample exposed by generated seed 20260814
-- after fixing case 20.  The inner application returns a bounded identity
-- annotation through an identity application and a let.  Its descendant
-- certificate reaches the enclosing lambda while that owner's requirement
-- still carries Bottom, so owner inheritance must use the exact declaration
-- states already recorded by construction rather than require only the
-- certificate's original or completed presentation.
boundedIdentityThroughNestedApplicationsUnderAppliedAnnotationExpr
  :: Surf.SurfaceExpr
boundedIdentityThroughNestedApplicationsUnderAppliedAnnotationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.ELit (Surf.LInt 9))
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    boundedIdentityAnnotationExpr
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-2)))

-- The first minimized counterexample exposed by generated seed 20260817.
-- Constructing the bounded annotation completes the body of its outer
-- quantifier before that quantifier itself is emitted.  The resulting arrow
-- is an exact Figure 15.3 declaration stage, so a descendant certificate must
-- carry it through the enclosing application/let spine instead of recognizing
-- it later from type shape alone.
boundedIdentityThroughPartiallyConstructedForallExpr :: Surf.SurfaceExpr
boundedIdentityThroughPartiallyConstructedForallExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 10))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            (Surf.EVar "_generatedWrap7")
                                        )
                                        boundedIdentityAnnotationExpr
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                    (Surf.ELit (Surf.LInt (-3)))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- The second counterexample exposed by generated seed 20260817.  The checked
-- innermost lambda owns the complete @forall t10. forall a >= sigma-id@ body
-- spine, while the enclosing prepared lambda endpoint still presents @t10@
-- and @a@ freely in its codomain.  The child owner-final certificate must
-- close that exact spine beneath the parent value arrow before publication.
childConstructedForallSpineThroughPartiallyAppliedLambdaTailExpr
  :: Surf.SurfaceExpr
childConstructedForallSpineThroughPartiallyAppliedLambdaTailExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            boundedIdentityAnnotationExpr
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Minimized from the 65th case of generated seed 20260818.  A source-owned
-- polymorphic let is returned by a lambda which itself passes through an
-- identity application.  The application-result node and its constructed
-- bound can share a solved representative, but they remain two stages of one
-- declaration rather than a cyclic pair of local Gamma binders.
sourcePolymorphicLetThroughIdentityAppliedNestedLambdaExpr
  :: Surf.SurfaceExpr
sourcePolymorphicLetThroughIdentityAppliedNestedLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "_generatedSeedX"
                                    (Surf.EVar "_generatedSeedX")
                                )
                                sigmaIdSource
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Minimized from the 94th case of generated seed 20260819.  The let-bound
-- mixed annotation is returned by its exact resolved identity and then passed
-- through an identity application beneath two lambdas.  The application
-- Gamma bound mentions the annotation's implicit @beta@ declaration, so the
-- prepared construction must carry that source dependency before checking
-- the enclosing lambda spine.
mixedAnnotationDependencyThroughIdentityAppliedLetAliasExpr
  :: Surf.SurfaceExpr
mixedAnnotationDependencyThroughIdentityAppliedLetAliasExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    mixedAnnotationExpr
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Minimized from the 46th case of generated seed 20260821.  A lambda applies
-- an unused parameter around the paper @g g@ result and publishes that result
-- through a let alias beneath otherwise ground let owners.
paperGgThroughAppliedLambdaBeneathUnusedLetOwnersExpr
  :: Surf.SurfaceExpr
paperGgThroughAppliedLambdaBeneathUnusedLetOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap4"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap7"
                        (Surf.ELit (Surf.LInt (-2)))
                        annotatedSelfAppExpr
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap4")
        )
    )

-- Frozen from the 90th case of generated seed 20260821.  Two applications
-- carry successive construction states of the same paper @g g@ result to an
-- enclosing let owner; their shared Gamma exterior must retain that directed
-- construction progress instead of being rejected as two unrelated bounds.
paperGgThroughSequentialApplicationsBeneathLetPublicationExpr
  :: Surf.SurfaceExpr
paperGgThroughSequentialApplicationsBeneathLetPublicationExpr =
  Surf.ELet
    "_generatedWrap2"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    annotatedSelfAppExpr
                )
                (Surf.ELit (Surf.LInt (-11)))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap2")

-- Frozen from the 66th case of generated seed 1126245027.  Two nested
-- applications expose @Bool@ and @forall t. forall (a >= Bool). t -> a@ as
-- intermediate views of one enclosing let Gamma exterior.  The live final
-- bound is @forall t. t -> Bool@, reached by retaining @t@ and eliminating
-- @a@.  The checked binder-spine coercion, rather than direct type equality,
-- identifies which requirement constructs the final declaration.
retainedBinderSpineAtFinalLetGammaExpr :: Surf.SurfaceExpr
retainedBinderSpineAtFinalLetGammaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-2)))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 83rd case of generated seed 1126245027.  The paper @g g@
-- result is returned by an identity application inside an unused let, then by
-- a lambda let-binding beneath an applied annotated lambda.  The outer
-- application must feed the completed administrative-lambda packet to its
-- function child before checking the selected exact result.
returnedPaperGgThroughAdministrativeLambdaPacketExpr :: Surf.SurfaceExpr
returnedPaperGgThroughAdministrativeLambdaPacketExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt 11))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                annotatedSelfAppExpr
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-14)))

-- Frozen from the 25th case of generated seed 1259820846.  The paper @g g@
-- lambda is returned through two applied unused lambdas and a let before an
-- annotated outer application.  Three descendant edges describe successive
-- construction states of one enclosing Gamma exterior; packet preparation
-- must coalesce that certified state path instead of rejecting the snapshots
-- as unrelated bounds.
paperGgDescendantBoundsBeforeAnnotatedOuterApplicationExpr
  :: Surf.SurfaceExpr
paperGgDescendantBoundsBeforeAnnotatedOuterApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            annotatedSelfAppExpr
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 15))

-- Frozen from the 23rd case of generated seed 2079298731.  A nested let
-- publishes the mixed source annotation @forall alpha. beta -> alpha ->
-- alpha@ beneath an applied @Int@ lambda.  The source-owned @beta@ and the
-- locally constructed @alpha@ reach the enclosing lambda packet through the
-- returned let value; that packet needs the exact descendant consumer route,
-- not a route guessed from its final type.
nestedMixedSourceDeclarationAtEnclosingLambdaPacketExpr
  :: Surf.SurfaceExpr
nestedMixedSourceDeclarationAtEnclosingLambdaPacketExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 9))
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                (Surf.ELam "y" (Surf.EVar "y"))
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
            (Surf.ELit (Surf.LInt 9))
        )
    )

-- Frozen from the 51st case of generated seed 2079298731.  A source
-- polymorphic identity is returned through three transparent lets beneath a
-- lambda.  The enclosing root retains a consumed consumer declaration whose
-- frozen graph bound names the source binder, while the checked owner has
-- constructed its exact generated representative.  Root projection must use
-- that owner route to align the bound identities before retaining it.
retainedRootConsumerAtOwnerConstructedBinderExpr :: Surf.SurfaceExpr
retainedRootConsumerAtOwnerConstructedBinderExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 10))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                        (Surf.EVar "_generatedWrap6")
                    )
                    (Surf.EVar "_generatedWrap5")
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

-- Frozen from the 92nd case of generated seed 2026081002.  A mixed source
-- annotation is returned through nested lets and an identity application,
-- then through a root identity application.  The enclosing lambda owner has
-- already consumed and completed the result RaiseMerge declaration, so root
-- validation must advance that exact declaration before comparing it with
-- S(operated).
consumedRaiseMergeThroughRootIdentityApplicationExpr :: Surf.SurfaceExpr
consumedRaiseMergeThroughRootIdentityApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                ( Surf.ELet
                    "_generatedWrap5"
                    (Surf.ELit (Surf.LInt (-3)))
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap7"
                            (Surf.ELit (Surf.LInt 0))
                            mixedAnnotationExpr
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 25th case of generated seed 2026081003.  The paper @g g@
-- lambda is returned through two nested unused-lambda pairs and transparent
-- lets, then through a root identity application.  The outer opaque carrier
-- must close from the exact descendant construction even when its current
-- bound still contains the unopened inner carrier chain.
opaquePaperGgCarrierThroughNestedUnusedLambdasExpr :: Surf.SurfaceExpr
opaquePaperGgCarrierThroughNestedUnusedLambdasExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt 15))
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELam
                        "_generatedWrap6"
                        ( Surf.ELet
                            "_generatedWrap7"
                            (Surf.ELit (Surf.LInt 13))
                            annotatedSelfAppExpr
                        )
                    )
                )
            )
        )
    )

-- Frozen from the 48th case of generated seed 2026081004.  A multi-use
-- polymorphic let is returned through an applied unused lambda, an identity
-- application, and two enclosing lambda owners before its result is
-- generalized by a returned let.  The local Gamma bound's result dependency
-- must keep the exact construction route that its owner already established.
locallyConstructedGammaDependencyThroughReturnedLetExpr :: Surf.SurfaceExpr
locallyConstructedGammaDependencyThroughReturnedLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (multiUseAnnotatedIdentitySeedWith (-8) False)
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 97th case of generated seed 2026081005.  Paper @g g@ is
-- returned through an ordinary lambda, an applied annotated lambda, two more
-- ordinary lambdas, and an applied outer annotated lambda.  Each owner must
-- construct its administrative endpoint once; the source annotation binder
-- must not be duplicated while the endpoint crosses those applications.
paperGgThroughNestedAnnotatedApplicationOwnersExpr :: Surf.SurfaceExpr
paperGgThroughNestedAnnotatedApplicationOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.ELit (Surf.LInt (-3)))
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool False))
                                            (Surf.STBase "Bool")
                                        )
                                        annotatedSelfAppExpr
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 4))
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
    )
    (Surf.ELit (Surf.LInt 15))

-- Frozen from the 27th case of generated seed 2026081006.  A source
-- annotation with an ambient @beta@ parameter and a locally quantified
-- @alpha@ is returned through several applications and a let.  The packet
-- must route the graph occurrence for @beta@ into the source identity domain
-- before checking that the generalized result is closed.
returnedMixedSourceParameterThroughNestedApplicationsExpr
  :: Surf.SurfaceExpr
returnedMixedSourceParameterThroughNestedApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "x"
                                                ( Surf.ELam
                                                    "y"
                                                    (Surf.EVar "y")
                                                )
                                            )
                                            ( Surf.STForall
                                                "alpha"
                                                Nothing
                                                ( Surf.STArrow
                                                    (Surf.STVar "beta")
                                                    ( Surf.STArrow
                                                        (Surf.STVar "alpha")
                                                        (Surf.STVar "alpha")
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 93rd case of generated seed 2026081007.  A higher-rank
-- parameter is specialized to @Int@ under two unused lambdas, two applied
-- wrappers, and a root identity application.  Source-scheme recovery must
-- consume the exact constructed result identity before comparing the packet
-- endpoint with its concrete bound; @InstBot@ cannot perform that completion.
specializedHigherRankResultBeforePacketRecoveryExpr
  :: Surf.SurfaceExpr
specializedHigherRankResultBeforePacketRecoveryExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 6))
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELamAnn
                                    "_generatedSeedPoly"
                                    sigmaIdSource
                                    ( Surf.EApp
                                        (Surf.EVar "_generatedSeedPoly")
                                        (Surf.ELit (Surf.LInt 13))
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 20th case of generated seed 2026081008.  A specialized
-- higher-rank parameter is returned through two ordinary lambdas, then an
-- applied discarded lambda, before an enclosing annotated application fixes
-- the outer parameter at @Int@.  The enclosing application must use the
-- checked child construction for its exact function endpoint; the prepared
-- administrative packet still contains the provisional graph result.
higherRankResultBeforeEnclosingAnnotatedApplicationExpr
  :: Surf.SurfaceExpr
higherRankResultBeforeEnclosingAnnotatedApplicationExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELamAnn
                                "_generatedSeedPoly"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "_generatedSeedPoly")
                                    (Surf.ELit (Surf.LInt 5))
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
        (Surf.ELit (Surf.LInt (-1)))
    )

-- Frozen from the 26th case of generated seed 2026081008.  A source
-- annotation returns a polymorphic identity whose binder has a higher-rank
-- lower bound.  Transparent lets and three unapplied lambdas carry that
-- value into two enclosing applications.  The inner lambda packet owns the
-- source declaration consumed by its result; the outer application must
-- retain that construction binder instead of treating it as an unscoped
-- free source occurrence.
boundedSourceDeclarationThroughEnclosingApplicationExpr
  :: Surf.SurfaceExpr
boundedSourceDeclarationThroughEnclosingApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt 8))
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELam "x" (Surf.EVar "x"))
                                        boundedIdentitySource
                                    )
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 14))
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))
  where
    boundedIdentitySource =
      Surf.STForall
        "a"
        ( Just
            ( Surf.SrcBound
                ( Surf.STForall
                    "a"
                    Nothing
                    ( Surf.STArrow
                        (Surf.STVar "a")
                        (Surf.STVar "a")
                    )
                )
            )
        )
        ( Surf.STArrow
            (Surf.STVar "a")
            (Surf.STVar "a")
        )

-- Frozen from the 43rd case of generated seed 2026081008.  Three annotated
-- applications return a mixed source annotation through a transparent let;
-- two enclosing lambdas then expose the result through a root RaiseMerge.
-- Packet preparation must route the root declaration to the exact nested
-- result owner instead of rejecting its provisional graph bound as a bare
-- alias before that construction is available.
rootRaiseMergeAliasThroughNestedAnnotatedApplicationsExpr
  :: Surf.SurfaceExpr
rootRaiseMergeAliasThroughNestedAnnotatedApplicationsExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-5)))
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        mixedAnnotationExpr
                                        (Surf.EVar "_generatedWrap7")
                                    )
                                )
                                (Surf.ELit (Surf.LInt 5))
                            )
                        )
                        (Surf.ELit (Surf.LInt (-3)))
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-14)))
        )
    )

-- Frozen from the 39th case of generated seed 2026081009.  Paper @g g@ is
-- returned through an unapplied lambda, two applied annotated lambdas, two
-- more unapplied lambdas, and an outer transparent let.  The final body-edge
-- computation must use the exact identity published by the constructed
-- Gamma; retaining the frozen graph exterior in @InstAbstr@ makes the
-- otherwise complete term ill-typed.
paperGgThroughNestedAnnotatedApplicationsAndLambdasExpr
  :: Surf.SurfaceExpr
paperGgThroughNestedAnnotatedApplicationsAndLambdasExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-8)))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    annotatedSelfAppExpr
                                )
                            )
                            (Surf.ELit (Surf.LInt 10))
                        )
                    )
                    (Surf.ELit (Surf.LInt 1))
                )
            )
        )
    )

-- Frozen from the 41st case of generated seed 2026081010.  A bounded
-- polymorphic identity is returned through an identity application, an
-- annotated lambda application, and two enclosing ordinary lambdas.  The
-- direct function-result packet must construct its own endpoint before the
-- parent application confirms the enclosing exact result.
boundedIdentityAnnotationThroughNestedLambdaApplicationsExpr
  :: Surf.SurfaceExpr
boundedIdentityAnnotationThroughNestedLambdaApplicationsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            boundedIdentityAnnotationExpr
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 10))
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 15th case of generated seed 2026081010.  An annotated
-- lambda application returns a source-polymorphic identity through an
-- unapplied lambda and an applied outer lambda.  The outer result plan is a
-- checking context until the nested lambda owner has published its exact
-- result; pushing that plan into the inner application asks for an unrelated
-- graph endpoint.
polymorphicIdentityThroughNestedLambdaResultApplicationExpr
  :: Surf.SurfaceExpr
polymorphicIdentityThroughNestedLambdaResultApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-5)))
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from the 75th case of generated seed 2026081011.  The mixed source
-- annotation contributes an existential @beta@ beneath two returned lambdas
-- and an applied annotated lambda, and that value is itself passed through
-- an outer identity application.  Completing the lambda-parameter boundary
-- must not quantify the same graph dependency once in the prepared bound and
-- again around that bound.
mixedAnnotationThroughIdentityAndNestedLambdaApplicationsExpr
  :: Surf.SurfaceExpr
mixedAnnotationThroughIdentityAndNestedLambdaApplicationsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    nestedMixedAnnotationExpr
                )
                (Surf.ELit (Surf.LInt 12))
            )
        )
    )

-- Frozen from the 94th case of generated seed 2026081011.  A lambda returning
-- a let-bound mixed annotation crosses an inner and an outer identity
-- application before the complete value is itself let-bound.  The exact
-- body-consumer construction route must advance its inherited declaration at
-- that construction boundary instead of validating against the stale bottom
-- bound.
letBoundMixedAnnotationLambdaThroughIdentityApplicationsExpr
  :: Surf.SurfaceExpr
letBoundMixedAnnotationLambdaThroughIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                nestedMixedAnnotationExpr
                                (Surf.EVar "_generatedWrap7")
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 43rd case of generated seed 2026081012.  Paper @g g@ is
-- returned through a lambda below an unused let, then passed through an
-- identity application below another unused let.  Root finalization must
-- retain the exact packet specialization published by that source-owned
-- annotated lambda across the transparent wrappers.
paperGgThroughUnusedLetsAndIdentityApplicationExpr :: Surf.SurfaceExpr
paperGgThroughUnusedLetsAndIdentityApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 12))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt (-4)))
                annotatedSelfAppExpr
            )
        )
    )

-- Frozen from the 89th case of generated seed 2026081013.  Paper @g g@ is
-- returned through an applied annotated lambda, an ordinary lambda, and a
-- second applied annotated lambda beneath three unapplied lambdas.  The
-- enclosing application endpoint is a result-checking context; it must not be
-- imposed on the recursively constructed function occurrence before that
-- function publishes its own exact endpoint.
paperGgThroughNestedAnnotatedApplicationResultsExpr
  :: Surf.SurfaceExpr
paperGgThroughNestedAnnotatedApplicationResultsExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                    annotatedSelfAppExpr
                                )
                            )
                            (Surf.ELit (Surf.LInt 15))
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-15)))
            )
        )
    )

-- Frozen from the 45th case of generated seed 2026081014.  An applied
-- annotated lambda returns paper @g g@ beneath an unused let and two ordinary
-- lambdas; the complete value is then published by an enclosing let.  The
-- application must construct its result from the checked function owner even
-- when no prospective parent result endpoint exists.
paperGgFromAnnotatedApplicationThroughLetExpr :: Surf.SurfaceExpr
paperGgFromAnnotatedApplicationThroughLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LInt 6))
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.ELam "_generatedWrap5" annotatedSelfAppExpr)
                )
            )
        )
        (Surf.ELit (Surf.LInt 2))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 8th case of generated seed 2026081014.  A packet owned by
-- the directly applied lambda has a transparent result path: its graph
-- endpoint is construction guidance for the lambda's bounded constant result,
-- not prospective topology belonging to a nested value lambda.
annotatedConstantResultAtAppliedLambdaBoundaryExpr :: Surf.SurfaceExpr
annotatedConstantResultAtAppliedLambdaBoundaryExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EAnn
            (Surf.ELit (Surf.LInt (-14)))
            (Surf.STBase "Int")
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 83rd case of generated seed 82301190.  The outer let owns
-- two requirements for the same result exterior: an early identity-lambda
-- view and the later endpoint that returns paper @g g@.  They are successive
-- states of one exact source owner, so construction-time planning must publish
-- the completed endpoint rather than reject the historical state as a
-- simultaneous declaration.
completedOuterLetGammaAfterNestedPaperGgExpr :: Surf.SurfaceExpr
completedOuterLetGammaAfterNestedPaperGgExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-8)))
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            (Surf.EVar "_generatedWrap7")
                                        )
                                        annotatedSelfAppExpr
                                    )
                                )
                                (Surf.ELit (Surf.LInt 1))
                            )
                        )
                        (Surf.ELit (Surf.LInt (-8)))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 28th case of generated seed 759188075.  The inner
-- annotation constructs @forall a. a -> a@ and the first application returns
-- that value before an enclosing occurrence specializes it at a free @a@.
-- The local type abstraction and the ambient specialization can share a
-- graph presentation, but they are different lexical declarations.  The
-- application computation must alpha-copy the local declaration before
-- constructing the explicit type application.
returnedPolymorphicValueBeforeAmbientSpecializationExpr
  :: Surf.SurfaceExpr
returnedPolymorphicValueBeforeAmbientSpecializationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EAnn
                        ( Surf.ELam
                            "_generatedSeedX"
                            (Surf.EVar "_generatedSeedX")
                        )
                        ( Surf.STForall
                            "a"
                            Nothing
                            ( Surf.STArrow
                                (Surf.STVar "a")
                                (Surf.STVar "a")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 71st case of generated seed 759188075.  The inner let
-- returns an annotated polymorphic identity through an unapplied lambda.  Its
-- checked owner carries the result binder together with an exact graph route;
-- root publication must use that route rather than leave the carried identity
-- free after selecting the root's binder spine.
carriedPolymorphicResultBinderIntoRootPublicationExpr
  :: Surf.SurfaceExpr
carriedPolymorphicResultBinderIntoRootPublicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-10)))
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            ( Surf.STForall
                                "a"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "a")
                                    (Surf.STVar "a")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 5))
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 47th case of generated seed 2147483646.  The annotated
-- parameter's lexical binder is projected through several graph occurrences
-- while an enclosing application also carries graph-owned binders.  Phi must
-- receive the source-order key for the projected binder together with
-- its identity route; recovering only the identity leaves the otherwise
-- certified binder unordered.
projectedHigherRankParameterConstructionOrderExpr
  :: Surf.SurfaceExpr
projectedHigherRankParameterConstructionOrderExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 1))
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 4))
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt 15))
    )

-- Frozen from the 43rd case of generated seed 2026081003.  The direct lambda
-- returns the checked mixed annotation, and the application edge immediately
-- performs its N/InstApp specialization.  The application result certificate
-- must advance through that already-constructed computation instead of
-- retaining the lambda's unspecialized forall endpoint.
directLambdaMixedResultThroughApplicationSpecializationExpr
  :: Surf.SurfaceExpr
directLambdaMixedResultThroughApplicationSpecializationExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" mixedAnnotationExpr)
                (Surf.ELit (Surf.LBool True))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 75th case of generated seed 668178538.  The inner
-- application owns one local Gamma closure whose direct edge sees the complete
-- prepared mixed-annotation result while its forwarded edge sees the result
-- after transparent let publication.  Both views must select the declaration
-- constructed by that one closure rather than compete as unrelated endpoints.
completedMixedResultAcrossClosureEdgesExpr :: Surf.SurfaceExpr
completedMixedResultAcrossClosureEdgesExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            mixedAnnotationExpr
                            (Surf.EVar "_generatedWrap6")
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from the 86th case of generated seed 668178538.  The innermost
-- applied lambda returns an explicitly polymorphic identity.  Its returned
-- construction still names the lexical source declaration while application
-- publication names the corresponding graph declaration.  The owner-final
-- construction must carry the exact identity route between those two views;
-- equality of their final forall shapes is not sufficient authority.
returnedSourceForallThroughAppliedLambdaPublicationExpr
  :: Surf.SurfaceExpr
returnedSourceForallThroughAppliedLambdaPublicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            ( Surf.STForall
                                "a"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "a")
                                    (Surf.STVar "a")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the 46th case of generated seed 2.  Descendant placement adds
-- the third lambda's enclosing consumer to the completed body packet.  That
-- consumer is construction-only at this level and is absent from the raw
-- operated view, so the operated pass must observe the completed placement
-- before deciding whether the descendant still needs to be placed.
completedNestedLambdaConsumerBeforeOperatedViewExpr
  :: Surf.SurfaceExpr
completedNestedLambdaConsumerBeforeOperatedViewExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                mixedAnnotationExpr
            )
        )
    )

-- Frozen from the 39th case of generated seed 99.  The annotation's @alpha@
-- declaration is retained in the enclosing endpoint while the complete
-- annotation also becomes the bound of the lambda body's graph result.  The
-- nested @alpha@ lies beneath @beta@, so it is a second lexical declaration;
-- the candidate-bound constructor must allocate that copy before publication.
nestedAnnotationBinderBesideOuterPublicationExpr
  :: Surf.SurfaceExpr
nestedAnnotationBinderBesideOuterPublicationExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                ( Surf.ELam
                    "x"
                    ( Surf.ELam
                        "y"
                        (Surf.EVar "y")
                    )
                )
                ( Surf.STForall
                    "alpha"
                    Nothing
                    ( Surf.STArrow
                        (Surf.STVar "beta")
                        ( Surf.STArrow
                            (Surf.STVar "alpha")
                            (Surf.STVar "alpha")
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 72nd case of generated seed 99.  The inner application
-- returns an explicitly polymorphic identity through an unapplied lambda and
-- a transparent let.  The enclosing annotated-parameter lambda has already
-- completed its body-consumer bound, so its exact administrative endpoint
-- must compose that refinement with the returned source-forall construction.
returnedSourceForallThroughRefinedLambdaBodyExpr
  :: Surf.SurfaceExpr
returnedSourceForallThroughRefinedLambdaBodyExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        ( Surf.STForall
                                            "a"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "a")
                                                (Surf.STVar "a")
                                            )
                                        )
                                    )
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-1)))

-- Frozen from the 17th case of generated seed 20260822.  A higher-rank
-- parameter is specialized inside an applied discarded lambda, forwarded by
-- an identity application and a let, then returned through an unapplied
-- lambda beneath an applied annotated lambda.
specializedHigherRankParameterThroughNestedOwnersExpr
  :: Surf.SurfaceExpr
specializedHigherRankParameterThroughNestedOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELamAnn
                                "_generatedSeedPoly"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "_generatedSeedPoly")
                                    (Surf.ELit (Surf.LInt (-1)))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-15)))

-- Frozen from the 79th case of generated seed 20260823.  Three transparent
-- root applications return a lambda whose result packet begins with a source
-- forall.  Root RaiseMerge must retain that complete constructed bound rather
-- than compare it with the opened body stored at the exterior graph node.
sourceForallThroughRootRaiseMergeExpr :: Surf.SurfaceExpr
sourceForallThroughRootRaiseMergeExpr =
  Surf.EApp
    identityLambda
    ( Surf.EApp
        identityLambda
        ( Surf.ELam
            "_generatedWrap4"
            ( Surf.ELet
                "_generatedWrap6"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELam
                    "_generatedWrap7"
                    ( Surf.ELamAnn
                        "_generatedSeedPoly"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "_generatedSeedPoly")
                            (Surf.ELit (Surf.LInt (-6)))
                        )
                    )
                )
            )
        )
    )
  where
    identityLambda =
      Surf.ELam
        "_generatedIdentity"
        (Surf.EVar "_generatedIdentity")

-- Frozen from the 31st case of generated seed 20260826.  The paper @g g@
-- lambda is returned beneath an unused annotated let and an applied unused
-- lambda, then carried through an outer identity application.  Its exact
-- lambda consumer remains the authority for the source-polymorphic packet
-- while the surrounding applications construct the enclosing result.
paperGgLocalConsumerThroughNestedApplicationsExpr :: Surf.SurfaceExpr
paperGgLocalConsumerThroughNestedApplicationsExpr =
  Surf.EApp
    identityLambda
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LBool True))
                annotatedSelfAppExpr
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
  where
    identityLambda =
      Surf.ELam
        "_generatedIdentity"
        (Surf.EVar "_generatedIdentity")

-- Frozen from the 24th case of generated seed 20260827.  A multi-use
-- polymorphic identity is specialized to both @Int@ and @Bool@; the returned
-- @Bool@ result then crosses an applied unused lambda, a returned let, an
-- unapplied lambda, and two identity applications.
multiUsePolymorphicResultThroughNestedIdentitiesExpr :: Surf.SurfaceExpr
multiUsePolymorphicResultThroughNestedIdentitiesExpr =
  Surf.EApp
    identityLambda
    ( Surf.EApp
        identityLambda
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap5"
                (multiUseAnnotatedIdentitySeedWith 8 False)
                (Surf.EVar "_generatedWrap5")
            )
        )
    )
  where
    identityLambda =
      Surf.ELam
        "_generatedIdentity"
        (Surf.EVar "_generatedIdentity")

-- Frozen from the 43rd case of generated seed 20260827.  The annotation's
-- implicit @beta@ declaration is carried through an identity application,
-- three enclosing lambda owners, and the outer application boundary.
mixedSourceDeclarationThroughNestedLambdaApplicationExpr :: Surf.SurfaceExpr
mixedSourceDeclarationThroughNestedLambdaApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EApp identityLambda mixedAnnotationExpr)
            )
        )
    )
    (Surf.ELit (Surf.LBool False))
  where
    identityLambda =
      Surf.ELam
        "_generatedIdentity"
        (Surf.EVar "_generatedIdentity")

-- Frozen from the 45th case of generated seed 20260828.  The completed
-- consumer bound for paper @g g@ crosses an applied annotated lambda, an
-- unapplied lambda, an unrelated let, and an applied lambda owner.
paperGgThroughNestedAppliedLambdaOwnersExpr :: Surf.SurfaceExpr
paperGgThroughNestedAppliedLambdaOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                (Surf.ELit (Surf.LBool False))
                ( Surf.EApp
                    (Surf.ELam "_generatedWrap4" annotatedSelfAppExpr)
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-1)))

-- Frozen from the 96th case of generated seed 20260829.  A polymorphic
-- identity crosses two identity applications inside a returned let before
-- the outer identity application publishes their shared application Gamma.
-- The forwarded and direct members see different intermediate
-- specializations of the same exact declaration bound.
sharedApplicationClosureThroughNestedIdentityLetsExpr :: Surf.SurfaceExpr
sharedApplicationClosureThroughNestedIdentityLetsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        outerIdentityLambda
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                innerIdentityLambda
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                annotatedIdentity
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )
    (Surf.ELit (Surf.LBool False))
  where
    outerIdentityLambda =
      Surf.ELam
        "_generatedWrap2"
        (Surf.EVar "_generatedWrap2")
    innerIdentityLambda =
      Surf.ELam
        "_generatedWrap4"
        (Surf.EVar "_generatedWrap4")
    annotatedIdentity =
      Surf.EAnn
        (Surf.ELam "_generatedSeedX" (Surf.EVar "_generatedSeedX"))
        sigmaIdSource

-- Frozen from the 58th case of generated seed 20260830.  The returned lambda
-- carries paper @g g@ through two identity applications and an unrelated let.
-- Its root result binder is already bounded by the checked construction when
-- inherited dependency evidence encounters the same identity as an
-- unbounded lexical declaration.
constructedRootBoundBesideUnboundedDependencyExpr :: Surf.SurfaceExpr
constructedRootBoundBesideUnboundedDependencyExpr =
  Surf.ELet
    "_generatedWrap2"
    ( Surf.EApp
        outerIdentityLambda
        ( Surf.ELam
            "_generatedWrap4"
            ( Surf.ELet
                "_generatedWrap5"
                (Surf.ELit (Surf.LInt 0))
                (Surf.EApp resultIdentityLambda annotatedSelfAppExpr)
            )
        )
    )
    (Surf.EVar "_generatedWrap2")
  where
    outerIdentityLambda =
      Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3")
    resultIdentityLambda =
      Surf.ELam "_generatedWrap7" (Surf.EVar "_generatedWrap7")

-- Frozen from the 89th case of generated seed 20260831.  A let-aliased paper
-- @g g@ lambda is returned through an applied unused lambda, a returned
-- lambda, an applied annotated lambda, a let, and the outer application.
-- The function constructor must consume the packet selected for that complete
-- result route before checking the outer application endpoint.
returnedPaperGgLambdaThroughAppliedOwnersExpr :: Surf.SurfaceExpr
returnedPaperGgLambdaThroughAppliedOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap3"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap5"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap6"
                    ( Surf.ELet
                        "_generatedWrap7"
                        annotatedSelfAppExpr
                        (Surf.EVar "_generatedWrap7")
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap5")
        )
    )
    (Surf.ELit (Surf.LInt 6))

-- Frozen from the 33rd case of generated seed 1151810963.  The checked mixed
-- annotation completes a consumer that still belongs to an enclosing lambda;
-- nested applied wrappers must carry that completion to the exact owner
-- before the surrounding lets publish their results.
unboundedEnclosingConsumerThroughAppliedWrappersExpr :: Surf.SurfaceExpr
unboundedEnclosingConsumerThroughAppliedWrappersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            ( Surf.ELet
                                                "k"
                                                mixedAnnotationExpr
                                                ( Surf.EApp
                                                    ( Surf.EApp
                                                        (Surf.EVar "k")
                                                        (Surf.ELit (Surf.LInt 1))
                                                    )
                                                    (Surf.ELit (Surf.LBool True))
                                                )
                                            )
                                        )
                                        (Surf.ELit (Surf.LBool False))
                                    )
                                )
                                (Surf.ELit (Surf.LInt 12))
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 5))
            )
        )
        (Surf.ELit (Surf.LInt 6))
    )

-- Frozen from the 54th case of generated seed 1151810963.  An applied lambda
-- returns the source-annotated value opaquely, so the enclosing application
-- requirements do not expose its result declaration directly.  The checked
-- child construction remains the positive authority for that declaration.
opaqueMixedAnnotationResultThroughApplicationsExpr :: Surf.SurfaceExpr
opaqueMixedAnnotationResultThroughApplicationsExpr =
  Surf.EApp
    identityLambda
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                innerIdentityLambda
                ( Surf.EApp
                    (Surf.ELam "_generatedWrap4" mixedAnnotationExpr)
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
  where
    identityLambda =
      Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    innerIdentityLambda =
      Surf.ELam
        "_generatedWrap3"
        (Surf.EVar "_generatedWrap3")

-- Frozen from the 81st case of generated seed 1151810963.  The recursively
-- checked body orders the annotation identities by dependency as
-- @forall alpha. forall beta@, while the pending owner declaration retains
-- source order @forall beta. forall alpha@.  The owner must select the
-- completed declaration first, so Figure 15.3.4 constructs the explicit
-- reordering before the terminal Hyp.
reorderedPendingOwnerDeclarationBeforeHypExpr :: Surf.SurfaceExpr
reorderedPendingOwnerDeclarationBeforeHypExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                identityLambda
                (Surf.EApp secondIdentityLambda mixedAnnotationExpr)
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-8)))
  where
    identityLambda =
      Surf.ELam
        "_generatedWrap3"
        (Surf.EVar "_generatedWrap3")
    secondIdentityLambda =
      Surf.ELam
        "_generatedWrap4"
        (Surf.EVar "_generatedWrap4")

-- Frozen from the 100th case of generated seed 42.  The applied body completes
-- an internal declaration carried by the paper @g g@ result, while an outer
-- provisional endpoint identity is not the target of that transition.
-- Direct endpoint construction must restrict declaration-state equality to
-- identities for which the body actually carries refinement authority.
paperGgThroughAppliedSourceBoundaryExpr :: Surf.SurfaceExpr
paperGgThroughAppliedSourceBoundaryExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELamAnn
                                "g"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "g")
                                    (Surf.EVar "g")
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt 14))
        )
    )

-- Frozen from the 98th case of generated seed 91774058.  The annotated
-- identity has a source-owned flexible binder whose bound is itself the
-- polymorphic identity type.  Returning that value through a let and three
-- nested applications must advance the same certified declaration state at
-- each Gamma boundary, rather than comparing a projected arrow presentation
-- with the earlier bounded-forall presentation after construction.
boundedIdentityAnnotationThroughNestedApplicationGammaExpr :: Surf.SurfaceExpr
boundedIdentityAnnotationThroughNestedApplicationGammaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                ( Surf.EAnn
                                    (Surf.ELam "x" (Surf.EVar "x"))
                                    ( Surf.STForall
                                        "a"
                                        (Just (Surf.mkSrcBound sigmaIdSource))
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

-- Frozen from the 53rd case of generated seed 457459717.  An applied
-- annotated lambda returns three nested lambdas whose terminal result is a
-- source-polymorphic identity.  The application must consume only the
-- annotated parameter's exact binder and preserve the constructed body
-- packet; treating that whole packet as a fresh exact application result
-- quantifies the enclosing lambda parameters a second time.
returnedPolymorphicAnnotationThroughNestedLambdaResultPacketsExpr :: Surf.SurfaceExpr
returnedPolymorphicAnnotationThroughNestedLambdaResultPacketsExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 14))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt 5))
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-7)))
        )
    )

-- Frozen from the 36th case of generated seed 1958443227.  The annotation's
-- implicit beta binder remains free in the bound of the constructed lambda
-- consumer after alpha is freshened.  Construction Gamma must route that
-- dependency to the source declaration owned by the returned body packet.
implicitSourceDependencyThroughReturnedLambdaConsumerExpr :: Surf.SurfaceExpr
implicitSourceDependencyThroughReturnedLambdaConsumerExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "x"
                                (Surf.ELam "y" (Surf.EVar "y"))
                            )
                            ( Surf.STForall
                                "alpha"
                                Nothing
                                ( Surf.STArrow
                                    (Surf.STVar "beta")
                                    ( Surf.STArrow
                                        (Surf.STVar "alpha")
                                        (Surf.STVar "alpha")
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

-- Frozen from the 22nd case of generated seed 1003922807.  Three applied
-- ground wrappers and one discarded-argument lambda carry the paper's @g g@
-- construction into an identity application and then a let publication.  The
-- enclosing lambdas must consume the completed child owner endpoint without
-- replaying its already-emitted result abstraction.
paperGgThroughNestedGroundApplicationsAndIdentityExpr :: Surf.SurfaceExpr
paperGgThroughNestedGroundApplicationsAndIdentityExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELamAnn
                                        "g"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "g")
                                            (Surf.EVar "g")
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the third seed-1009 counterexample.  The inner application
-- completes the returned function's result declaration at @Int@.  The outer
-- identity application shares that declaration across one direct edge and
-- its forwarded result edges, so all three edges must consume the completed
-- endpoint rather than a provisional unbounded graph presentation.
completedConstantFunctionThroughIdentityApplicationExpr :: Surf.SurfaceExpr
completedConstantFunctionThroughIdentityApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool True))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LInt 7))
                                (Surf.STBase "Int")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the first seed-1010 counterexample.  The root identity
-- application directly consumes a lambda whose result is a let-bound source
-- annotation.  Its application closure owns the exterior declaration at the
-- root construction boundary; omitting that declaration leaves the result
-- route free while generalizing the root scheme.
returnedPolymorphicLetThroughRootIdentityApplicationExpr :: Surf.SurfaceExpr
returnedPolymorphicLetThroughRootIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "y")))
                ( Surf.STForall
                    "alpha"
                    Nothing
                    ( Surf.STArrow
                        (Surf.STVar "beta")
                        ( Surf.STArrow
                            (Surf.STVar "alpha")
                            (Surf.STVar "alpha")
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Minimized from the first seed-1011 counterexample.  The inner annotated
-- application completes its let-bound result at Bool beneath one unused value
-- parameter.  Its finalized lambda owner emits that declaration before the
-- two-lambda body crosses a partially applied outer spine and a transparent
-- root let; the enclosing exact construction must retain the completed bound.
completedResultBoundThroughPartiallyAppliedLambdaSpineExpr
  :: Surf.SurfaceExpr
completedResultBoundThroughPartiallyAppliedLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 0))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap7"
                                (Surf.STBase "Int")
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                            )
                            (Surf.ELit (Surf.LInt (-3)))
                        )
                        (Surf.EVar "_generatedWrap6")
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

boundedIdentityAnnotationExpr :: Surf.SurfaceExpr
boundedIdentityAnnotationExpr =
  Surf.EAnn
    (Surf.ELam "x" (Surf.EVar "x"))
    ( Surf.STForall
        "a"
        (Just (Surf.mkSrcBound sigmaIdSource))
        (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))
    )

boundedIdentityAnnotationType :: Elab.ElabType
boundedIdentityAnnotationType =
  testTForall
    "a"
    (Just (boundFromType polyIdTy))
    (Elab.TArrow (testTVar "a") (testTVar "a"))

boundedAnnotationUnderAppliedAnnotatedLambdaExpr :: Surf.SurfaceExpr
boundedAnnotationUnderAppliedAnnotatedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "unused"
        (Surf.STBase "Int")
        boundedIdentityAnnotationExpr
    )
    (Surf.ELit (Surf.LInt (-1)))

boundedAnnotationThroughNestedUnusedConstructionsExpr :: Surf.SurfaceExpr
boundedAnnotationThroughNestedUnusedConstructionsExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-9)))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    boundedAnnotationUnderAppliedAnnotatedLambdaExpr
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

polymorphicLetAmbientThroughNestedAppliedLambdaOwnersExpr ::
  Surf.SurfaceExpr
polymorphicLetAmbientThroughNestedAppliedLambdaOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 10))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedSeedId"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedArg"
                                            (Surf.EVar "_generatedSeedArg")
                                        )
                                        sigmaIdSource
                                    )
                                    ( Surf.ELet
                                        "_generatedSeedDiscard"
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LInt (-2)))
                                        )
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedId")
                                            (Surf.ELit (Surf.LBool True))
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-5)))
            )
        )
    )

annotatedIdentityThroughNestedIdentityAndAppliedOwnersExpr ::
  Surf.SurfaceExpr
annotatedIdentityThroughNestedIdentityAndAppliedOwnersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.ELet
                                    "_generatedWrap6"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool False))
                                        (Surf.STBase "Bool")
                                    )
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        sigmaIdSource
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 1))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

mixedAnnotationThroughNestedLetAndIdentityOwnersExpr :: Surf.SurfaceExpr
mixedAnnotationThroughNestedLetAndIdentityOwnersExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            mixedAnnotationExpr
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 4))

boundedIdentityLambdaThroughUnusedApplicationAndNestedLetsExpr ::
  Surf.SurfaceExpr
boundedIdentityLambdaThroughUnusedApplicationAndNestedLetsExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.ELit (Surf.LInt (-4)))
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                    boundedIdentityAnnotationExpr
                                )
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-5)))

boundedIdentityLambdaFromUnusedApplicationExpr :: Surf.SurfaceExpr
boundedIdentityLambdaFromUnusedApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap3"
            boundedIdentityLambdaFromUnusedApplicationCoreExpr
            (Surf.EVar "_generatedWrap3")
        )
    )
    (Surf.ELit (Surf.LInt (-5)))

boundedIdentityLambdaFromUnusedApplicationCoreExpr :: Surf.SurfaceExpr
boundedIdentityLambdaFromUnusedApplicationCoreExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap4"
        ( Surf.ELet
            "_generatedWrap5"
            ( Surf.ELam
                "_generatedWrap6"
                boundedIdentityAnnotationExpr
            )
            (Surf.EVar "_generatedWrap5")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from the 33rd case of generated seed 1003922807.  The ignored
-- let lies between the inner applied lambda's local Gamma owner and a returned
-- lambda whose result is the bounded polymorphic identity.  A second applied
-- lambda publishes that result through another let; its enclosing consumer
-- must retain the result quantifier beneath the returned arrow.
boundedIdentityLambdaThroughIgnoredLetApplicationExpr :: Surf.SurfaceExpr
boundedIdentityLambdaThroughIgnoredLetApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.ELam
                                "_generatedWrap7"
                                boundedIdentityAnnotationExpr
                            )
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from the 41st case of generated seed 1003922807.  The identity
-- application first presents the shared let-result exterior through its
-- lambda parameter, while two enclosing applications present the same exact
-- result as Int.  The complete source construction specializes that parameter
-- occurrence; the three edges therefore name one Gamma declaration rather
-- than three incompatible bounds.
letResultVariableThroughGroundApplicationEndpointsExpr :: Surf.SurfaceExpr
letResultVariableThroughGroundApplicationEndpointsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt 1))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LInt 1))
                                    (Surf.STBase "Int")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt (-11)))
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

-- Minimized from the 55th case of generated seed 1003922807.  The inner
-- identity application returns a lambda whose body is the bounded identity
-- annotation.  Two applied wrappers must carry that already-constructed
-- higher-rank result through their function-result plans without reverting to
-- the graph's provisional arrow topology.
identityAppliedHigherRankLambdaThroughAppliedWrappersExpr :: Surf.SurfaceExpr
identityAppliedHigherRankLambdaThroughAppliedWrappersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap2"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                    boundedIdentityAnnotationExpr
                                )
                            )
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-2)))
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized from the 63rd case of generated seed 1003922807.  The let RHS
-- returns one source-annotated polymorphic identity through two direct
-- identity applications and an applied unused parameter.  The annotation's
-- exact source declaration and the graph's provisional bounded presentation
-- name one ambient consumer; owner inheritance must retain the exact
-- construction instead of reporting the two presentations as competing
-- declarations.
annotatedIdentityAmbientThroughUnusedAndIdentityApplicationsExpr
  :: Surf.SurfaceExpr
annotatedIdentityAmbientThroughUnusedAndIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.EApp
                    (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                    ( Surf.EAnn
                        ( Surf.ELam
                            "_generatedSeedX"
                            (Surf.EVar "_generatedSeedX")
                        )
                        ( Surf.STForall
                            "a"
                            Nothing
                            ( Surf.STArrow
                                (Surf.STVar "a")
                                (Surf.STVar "a")
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from case 41 of generated seed 1003.  The annotated ground seed is
-- returned through a direct identity application, two applied wrappers, and a
-- transparent let before the outer identity-like lambda is consumed.
annotatedGroundSeedThroughIdentityAndAppliedWrappersExpr :: Surf.SurfaceExpr
annotatedGroundSeedThroughIdentityAndAppliedWrappersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        (Surf.EVar "_generatedWrap6")
                                    )
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool True))
                                            (Surf.STBase "Bool")
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                    (Surf.ELit (Surf.LInt 8))
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from case 46 of generated seed 1007.  The annotated identity is
-- published by a let beneath three consumed unused lambdas, then returned
-- through an unapplied lambda and two unrelated lets.
annotatedIdentityThroughDiscardedAppliedLambdasExpr :: Surf.SurfaceExpr
annotatedIdentityThroughDiscardedAppliedLambdasExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedX"
                                                (Surf.EVar "_generatedSeedX")
                                            )
                                            sigmaIdSource
                                        )
                                        (Surf.EVar "_generatedWrap7")
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )

-- Frozen from case 29 of generated seed 1009.  The paper self-application is
-- generalized by a let beneath two returned lambdas, then the complete value
-- is consumed and republished by the outer let/lambda chain.
paperGgThroughAppliedNestedLambdaAndLetOwnerExpr :: Surf.SurfaceExpr
paperGgThroughAppliedNestedLambdaAndLetOwnerExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                annotatedSelfAppExpr
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
        (Surf.EVar "_generatedWrap2")
    )

-- Frozen from case 80 of generated seed 1011.  Two direct identity
-- applications carry paper g g into a returned lambda beneath two consumed
-- annotated parameters; a final identity application publishes that lambda.
paperGgThroughIdentityAndAnnotatedApplicationsExpr :: Surf.SurfaceExpr
paperGgThroughIdentityAndAnnotatedApplicationsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        (Surf.EVar "_generatedWrap7")
                                    )
                                    annotatedSelfAppExpr
                                )
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-1)))
            )
        )
        (Surf.ELit (Surf.LInt 7))
    )

-- Frozen from case 94 of generated seed 1012.  The multi-use polymorphic let
-- is returned through an applied annotated lambda, an ignored application,
-- two transparent lets, and an unapplied lambda owner.
multiUsePolymorphicLetThroughAppliedLambdaOwnerExpr :: Surf.SurfaceExpr
multiUsePolymorphicLetThroughAppliedLambdaOwnerExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 8))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.ELet
                                        "_generatedSeedId"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedArg"
                                                (Surf.EVar "_generatedSeedArg")
                                            )
                                            sigmaIdSource
                                        )
                                        ( Surf.ELet
                                            "_generatedSeedDiscard"
                                            ( Surf.EApp
                                                (Surf.EVar "_generatedSeedId")
                                                (Surf.ELit (Surf.LInt (-16)))
                                            )
                                            ( Surf.EApp
                                                (Surf.EVar "_generatedSeedId")
                                                (Surf.ELit (Surf.LBool False))
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-11)))
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from case 87 of generated seed 1016.  A checked higher-rank
-- parameter application is returned through an ignored let and application,
-- a consumed annotated lambda, an identity application, and an outer lambda.
annotatedHigherRankParameterThroughApplicationOwnersExpr :: Surf.SurfaceExpr
annotatedHigherRankParameterThroughApplicationOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-5)))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-2)))
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt 1))
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                    (Surf.ELit (Surf.LInt 3))
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from case 72 of generated seed 1010 (also reached by seeds 1015 and
-- 1020).  An annotated applied lambda returns one unapplied value lambda whose
-- body is the complete paper self-application construction.
paperGgThroughUnappliedLambdaBeneathAnnotatedApplicationExpr
  :: Surf.SurfaceExpr
paperGgThroughUnappliedLambdaBeneathAnnotatedApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        (Surf.ELam "_generatedWrap2" annotatedSelfAppExpr)
    )
    (Surf.ELit (Surf.LInt 5))

-- Frozen from case 59 of generated seed 1004.  The same returned lambda is
-- first produced by an applied annotated lambda, named by a transparent let,
-- and then returned by a second applied annotated lambda.
paperGgThroughAppliedAnnotatedLetAndUnappliedLambdaExpr :: Surf.SurfaceExpr
paperGgThroughAppliedAnnotatedLetAndUnappliedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    (Surf.ELam "_generatedWrap4" annotatedSelfAppExpr)
                )
                (Surf.ELit (Surf.LInt (-16)))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-8)))

-- Frozen from case 56 of generated seed 1019.  A transparent let publishes
-- the applied annotated-lambda result beneath another returned lambda, and a
-- direct identity application transports that complete value at the root.
paperGgThroughReturnedLetUnderIdentityApplicationExpr :: Surf.SurfaceExpr
paperGgThroughReturnedLetUnderIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool True))
                (Surf.STBase "Bool")
            )
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        (Surf.ELam "_generatedWrap6" annotatedSelfAppExpr)
                    )
                    (Surf.ELit (Surf.LInt (-3)))
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

mixedAnnotationExpr :: Surf.SurfaceExpr
mixedAnnotationExpr =
  Surf.EAnn
    (Surf.ELam "x" (Surf.ELam "y" (Surf.EVar "y")))
    ( Surf.STForall
        "alpha"
        Nothing
        ( Surf.STArrow
            (Surf.STVar "beta")
            (Surf.STArrow (Surf.STVar "alpha") (Surf.STVar "alpha"))
        )
    )

nestedMixedAnnotationExpr :: Surf.SurfaceExpr
nestedMixedAnnotationExpr =
  Surf.ELet
    "k"
    mixedAnnotationExpr
    ( Surf.EApp
        (Surf.EApp (Surf.EVar "k") (Surf.ELit (Surf.LInt 1)))
        (Surf.ELit (Surf.LBool True))
    )

nestedAnnotatedLambdaUnderUnusedApplicationExpr :: Surf.SurfaceExpr
nestedAnnotatedLambdaUnderUnusedApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "unused"
        ( Surf.ELamAnn
            "poly"
            sigmaIdSource
            (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LInt 7)))
        )
    )
    (Surf.ELit (Surf.LBool True))

mixedAnnotationUnderUnusedApplicationExpr :: Surf.SurfaceExpr
mixedAnnotationUnderUnusedApplicationExpr =
  Surf.EApp
    (Surf.ELam "unused" mixedAnnotationExpr)
    (Surf.ELit (Surf.LBool True))

mixedAnnotationBeneathAppliedGroundWrappersExpr :: Surf.SurfaceExpr
mixedAnnotationBeneathAppliedGroundWrappersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool True))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                (Surf.ELam "_generatedWrap7" mixedAnnotationExpr)
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 6))
                )
            )
            (Surf.ELit (Surf.LInt (-9)))
        )
    )

mixedAnnotationThroughReturnedLambdaIdentityExpr :: Surf.SurfaceExpr
mixedAnnotationThroughReturnedLambdaIdentityExpr =
  Surf.EApp
    (Surf.ELam "identity" (Surf.EVar "identity"))
    (Surf.ELam "unused" mixedAnnotationExpr)

nestedAnnotatedLetLambdaRoundTripExpr :: Surf.SurfaceExpr
nestedAnnotatedLetLambdaRoundTripExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt (-5)))
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedWrap6"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELet
                            "_generatedWrap7"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            (Surf.EVar "_generatedWrap7")
                        )
                    )
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

nestedAnnotatedPolymorphicLambdaRoundTripExpr :: Surf.SurfaceExpr
nestedAnnotatedPolymorphicLambdaRoundTripExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-15)))
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt (-16)))
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELamAnn
                                "_generatedSeedPoly"
                                sigmaIdSource
                                ( Surf.EApp
                                    (Surf.EVar "_generatedSeedPoly")
                                    (Surf.ELit (Surf.LInt 11))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                    (Surf.EVar "_generatedWrap5")
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

nestedAnnotatedSelfAppUnderUnusedLambdaExpr :: Surf.SurfaceExpr
nestedAnnotatedSelfAppUnderUnusedLambdaExpr =
  Surf.ELam
    "unused"
    annotatedSelfAppExpr

nestedAnnotatedSelfAppUnderAppliedAnnotatedLambdaExpr :: Surf.SurfaceExpr
nestedAnnotatedSelfAppUnderAppliedAnnotatedLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "unused"
        (Surf.STBase "Int")
        annotatedSelfAppExpr
    )
    (Surf.ELit (Surf.LInt 6))

annotatedSelfAppThroughAnnotatedApplicationAndUnusedLetExpr :: Surf.SurfaceExpr
annotatedSelfAppThroughAnnotatedApplicationAndUnusedLetExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                annotatedSelfAppExpr
            )
        )
        (Surf.ELit (Surf.LInt 5))
    )

-- Frozen from the 44th case of generated seed 1799129115.  The returned
-- paper @g g@ lambda has already installed its closed source parameter, while
-- the enclosing administrative endpoint presents the same parameter as
-- @forall a. result@.  The enclosing lambda must accept that transition only
-- through the exact @O; Under a (Hyp result)@ domain computation carried by
-- the returned lambda's own parameter-boundary certificate.
returnedPaperGgDomainUnderForallSeed1799129115Expr :: Surf.SurfaceExpr
returnedPaperGgDomainUnderForallSeed1799129115Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    annotatedSelfAppExpr
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 7))
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from the 70th case of generated seed 1936552889.  The enclosing
-- packet declares the outermost future lambda Gamma as an exact unbounded
-- slot before that lambda has a scheme.  Its provisional graph bound still
-- mentions the private source forall inside @g g@, so ordinary-root
-- preconstruction must reuse the carried slot rather than generalize the
-- stale bound.  After the lambda is checked, its owner-final certificate
-- supplies the closed completed bound to final construction.
futureWrapperGammaCompletionSeed1936552889Expr :: Surf.SurfaceExpr
futureWrapperGammaCompletionSeed1936552889Expr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    annotatedSelfAppExpr
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
    )

-- Frozen from the complete-suite generated seed 669650106.  The inner paper
-- @g g@ owner has already constructed its ambient declaration at
-- @forall a. a -> a@, while an enclosing ignored application temporarily
-- carries the same graph identity at one monomorphic instance.  Propagating
-- through the two wrapper lets must preserve the child's exact declaration
-- and its checked term construction rather than choosing between the two
-- bounds from type shape.
generalizedPaperGgAmbientThroughIgnoredApplicationsSeed669650106Expr ::
  Surf.SurfaceExpr
generalizedPaperGgAmbientThroughIgnoredApplicationsSeed669650106Expr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            annotatedSelfAppExpr
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-3)))

-- Frozen from the 67th case of generated seed 669650106 after the generalized
-- paper @g g@ ambient regression was repaired.  The identity annotation's
-- source forall is copied while two ignored, applied lambdas successively
-- expose its result declaration.  Both ambient views must be aligned through
-- that lexical-copy construction before the inner lambda consumes them.
nestedIdentityAnnotationAmbientThroughAppliedLambdasSeed669650106Expr ::
  Surf.SurfaceExpr
nestedIdentityAnnotationAmbientThroughAppliedLambdasSeed669650106Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.ELet
                                    "_generatedWrap6"
                                    (Surf.ELit (Surf.LInt (-12)))
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedX"
                                                ( Surf.EVar
                                                    "_generatedSeedX"
                                                )
                                            )
                                            ( Surf.STForall
                                                "a"
                                                Nothing
                                                ( Surf.STArrow
                                                    (Surf.STVar "a")
                                                    (Surf.STVar "a")
                                                )
                                            )
                                        )
                                        (Surf.EVar "_generatedWrap7")
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool True))
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt (-9)))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 43rd case of the complete-suite generated seed 839296932.
-- The innermost lambda's selected endpoint has already specialized the
-- packet's completed bounded result to @Int@.  That exact specialization must
-- remain at this lambda layer; replaying the packet completion again would
-- try to generalize the body back to its pre-specialization forall.
completedPacketSpecializationAtSelectedLambdaSeed839296932Expr ::
  Surf.SurfaceExpr
completedPacketSpecializationAtSelectedLambdaSeed839296932Expr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.EAnn (Surf.ELit (Surf.LBool True)) (Surf.STBase "Bool"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.EAnn (Surf.ELit (Surf.LBool True)) (Surf.STBase "Bool"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.ELamAnn
                                    "_generatedSeedPoly"
                                    sigmaIdSource
                                    ( Surf.EApp
                                        (Surf.EVar "_generatedSeedPoly")
                                        (Surf.ELit (Surf.LInt 2))
                                    )
                                )
                                (Surf.EVar "_generatedWrap7")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Frozen from the 50th case of the complete-suite generated seed 839296932.
-- A solve-created semantic meta remains a dependency of a sibling locally
-- constructed Gamma bound.  Root preparation must carry the exact live node
-- named by its expansion-construction placement even though that dependency is
-- not itself a root binder in the anchor scheme.
sourceDependencyInLocalRootGammaSeed839296932Expr :: Surf.SurfaceExpr
sourceDependencyInLocalRootGammaSeed839296932Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EAnn
                        ( Surf.ELam
                            "x"
                            (Surf.ELam "y" (Surf.EVar "y"))
                        )
                        ( Surf.STForall
                            "alpha"
                            Nothing
                            ( Surf.STArrow
                                (Surf.STVar "beta")
                                ( Surf.STArrow
                                    (Surf.STVar "alpha")
                                    (Surf.STVar "alpha")
                                )
                            )
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 60th case of the complete-suite generated seed 839296932.
-- The annotated self-application is constructed by the innermost lambda and
-- then carried through several applied wrappers.  Each enclosing lambda must
-- consume that exact child construction without moving its binders into a
-- freshly inferred sibling scope.
paperGgInsideAppliedLambdaWrappersSeed839296932Expr :: Surf.SurfaceExpr
paperGgInsideAppliedLambdaWrappersSeed839296932Expr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.ELamAnn
                                            "g"
                                            sigmaIdSource
                                            ( Surf.EApp
                                                (Surf.EVar "g")
                                                (Surf.EVar "g")
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-14)))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

localTopologyResultThroughIdentityAppliedHigherOrderLambdaExpr ::
  Surf.SurfaceExpr
localTopologyResultThroughIdentityAppliedHigherOrderLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELamAnn
                                    "_generatedSeedPoly"
                                    sigmaIdSource
                                    ( Surf.EApp
                                        (Surf.EVar "_generatedSeedPoly")
                                        (Surf.ELit (Surf.LInt (-13)))
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt (-4)))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-13)))

applicationGammaOrderForIdentityLambdaParameterExpr :: Surf.SurfaceExpr
applicationGammaOrderForIdentityLambdaParameterExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

paperSelfApplicationThroughNestedAppliedWrappersExpr :: Surf.SurfaceExpr
paperSelfApplicationThroughNestedAppliedWrappersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap4"
                            (Surf.STBase "Int")
                            ( Surf.ELam
                                "_generatedWrap5"
                                annotatedSelfAppExpr
                            )
                        )
                        (Surf.ELit (Surf.LInt 16))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

letBoundPaperSelfApplicationThroughNestedLambdaOwnersExpr :: Surf.SurfaceExpr
letBoundPaperSelfApplicationThroughNestedLambdaOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    annotatedSelfAppExpr
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

paperSelfApplicationThroughAppliedAnnotatedLambdaAndNestedLetsExpr ::
  Surf.SurfaceExpr
paperSelfApplicationThroughAppliedAnnotatedLambdaAndNestedLetsExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.ELit (Surf.LInt 10))
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                annotatedSelfAppExpr
                            )
                            (Surf.ELit (Surf.LInt 5))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt 12))

boundedAnnotationThroughIdentityAppliedNestedLambdaExpr ::
  Surf.SurfaceExpr
boundedAnnotationThroughIdentityAppliedNestedLambdaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt 0))
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap6"
                        (Surf.ELit (Surf.LInt (-9)))
                        boundedIdentityAnnotationExpr
                    )
                )
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

consumedApplicationGammaBesideAmbientAnnotationExpr ::
  Surf.SurfaceExpr
consumedApplicationGammaBesideAmbientAnnotationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    sigmaIdSource
                                )
                            )
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LInt 3))
    )

paperSelfApplicationThroughIdentityAndNestedLetsExpr ::
  Surf.SurfaceExpr
paperSelfApplicationThroughIdentityAndNestedLetsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    (Surf.ELit (Surf.LInt (-11)))
                    annotatedSelfAppExpr
                )
            )
        )
        (Surf.EVar "_generatedWrap2")
    )
    (Surf.EVar "_generatedWrap1")

paperSelfApplicationThroughIdentityAppliedLetChainExpr ::
  Surf.SurfaceExpr
paperSelfApplicationThroughIdentityAppliedLetChainExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt (-11)))
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt (-1)))
                annotatedSelfAppExpr
            )
        )
    )

paperSelfApplicationThroughIdentityAppliedUnusedLetExpr ::
  Surf.SurfaceExpr
paperSelfApplicationThroughIdentityAppliedUnusedLetExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-1)))
        annotatedSelfAppExpr
    )

paperSelfApplicationThroughUnusedLetExpr :: Surf.SurfaceExpr
paperSelfApplicationThroughUnusedLetExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-1)))
    annotatedSelfAppExpr

nestedLambdaPacketAfterUnusedParameterApplicationExpr ::
  Surf.SurfaceExpr
nestedLambdaPacketAfterUnusedParameterApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-8)))
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LInt (-2)))
                            (Surf.STBase "Int")
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

rootPacketAroundNestedMixedAnnotationApplicationExpr ::
  Surf.SurfaceExpr
rootPacketAroundNestedMixedAnnotationApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool True))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.ELet
                                    "k"
                                    mixedAnnotationExpr
                                    ( Surf.EApp
                                        ( Surf.EApp
                                            (Surf.EVar "k")
                                            (Surf.ELit (Surf.LInt 1))
                                        )
                                        (Surf.ELit (Surf.LBool True))
                                    )
                                )
                                (Surf.EVar "_generatedWrap7")
                            )
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

paperSelfApplicationBeneathNestedAppliedUnusedLambdasExpr ::
  Surf.SurfaceExpr
paperSelfApplicationBeneathNestedAppliedUnusedLambdasExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    annotatedSelfAppExpr
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )
    (Surf.ELit (Surf.LInt 6))

paperSelfApplicationBeneathAppliedGroundWrapperExpr :: Surf.SurfaceExpr
paperSelfApplicationBeneathAppliedGroundWrapperExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            (Surf.ELam "_generatedWrap2" annotatedSelfAppExpr)
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.ELit (Surf.LInt (-2)))

paperSelfApplicationBeneathFiveAppliedUnusedLambdasAndLetExpr ::
  Surf.SurfaceExpr
paperSelfApplicationBeneathFiveAppliedUnusedLambdasAndLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( appliedUnused "_generatedWrap3" False
            ( appliedUnused "_generatedWrap4" True
                ( appliedUnused "_generatedWrap5" False
                    ( appliedUnused "_generatedWrap6" False
                        ( appliedUnused "_generatedWrap7" True
                            annotatedSelfAppExpr
                        )
                    )
                )
            )
        )
        (Surf.EVar "_generatedWrap2")
    )
  where
    appliedUnused name argument body =
      Surf.EApp
        (Surf.ELam name body)
        (Surf.ELit (Surf.LBool argument))

consumedBoundedResultThroughAppliedAndRootOwnersExpr :: Surf.SurfaceExpr
consumedBoundedResultThroughAppliedAndRootOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-10)))
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt 16))
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap6"
                            boundedIdentityAnnotationExpr
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                    (Surf.ELit (Surf.LInt 13))
                )
            )
        )
    )

zeroLocalApplicationWithCompletedMixedBoundExpr :: Surf.SurfaceExpr
zeroLocalApplicationWithCompletedMixedBoundExpr =
  Surf.EApp
    (Surf.ELam "_generatedOuterIdentity" (Surf.EVar "_generatedOuterIdentity"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap1"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELet
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            (Surf.EVar "_generatedWrap4")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.ELet
                                "_generatedWrap6"
                                (Surf.ELit (Surf.LInt 11))
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    nestedMixedAnnotationExpr
                                )
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap3")
                )
            )
        )
        (Surf.ELit (Surf.LInt (-1)))
    )

sourceRenamedLocalApplicationBinderAuthorityExpr :: Surf.SurfaceExpr
sourceRenamedLocalApplicationBinderAuthorityExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        ( Surf.ELam
                            "_generatedSeedX"
                            (Surf.EVar "_generatedSeedX")
                        )
                        sigmaIdSource
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

consumedPendingRootAfterOwnerFinalizationExpr :: Surf.SurfaceExpr
consumedPendingRootAfterOwnerFinalizationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LInt (-11)))
                        (Surf.STBase "Int")
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

unusedLambdaParameterInsideAppliedAnnotatedLambdaExpr ::
  Surf.SurfaceExpr
unusedLambdaParameterInsideAppliedAnnotatedLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-14)))
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LInt 11))
                                        (Surf.STBase "Int")
                                    )
                                )
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt 11))
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

appliedLambdaCodomainThroughSelectedReifyRootExpr :: Surf.SurfaceExpr
appliedLambdaCodomainThroughSelectedReifyRootExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt 14))
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt (-9)))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt 9))
                            )
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.ELit (Surf.LInt 7))

futureOwnerRefinementOutsideAmbientGammaExpr :: Surf.SurfaceExpr
futureOwnerRefinementOutsideAmbientGammaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap5"
                                    (Surf.STBase "Int")
                                    ( Surf.ELet
                                        "_generatedWrap6"
                                        ( Surf.EApp
                                            ( Surf.ELam
                                                "_generatedWrap7"
                                                ( Surf.EAnn
                                                    ( Surf.ELam
                                                        "_generatedSeedX"
                                                        (Surf.EVar "_generatedSeedX")
                                                    )
                                                    sigmaIdSource
                                                )
                                            )
                                            (Surf.ELit (Surf.LBool False))
                                        )
                                        (Surf.EVar "_generatedWrap6")
                                    )
                                )
                                (Surf.ELit (Surf.LInt 10))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.ELit (Surf.LInt 6))

mixedAnnotationThroughNestedLambdaLetResultExpr :: Surf.SurfaceExpr
mixedAnnotationThroughNestedLambdaLetResultExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        (Surf.ELam "_generatedWrap3" mixedAnnotationExpr)
    )
    (Surf.EVar "_generatedWrap1")

mixedAnnotationThroughNestedIdentityApplicationsExpr :: Surf.SurfaceExpr
mixedAnnotationThroughNestedIdentityApplicationsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        mixedAnnotationExpr
    )

mixedAnnotationBeneathAppliedFiveLambdaSpineExpr :: Surf.SurfaceExpr
mixedAnnotationBeneathAppliedFiveLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 6))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            (Surf.ELam "_generatedWrap7" mixedAnnotationExpr)
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
        (Surf.EVar "_generatedWrap2")
    )

annotatedParameterPlaceholderAtExactLambdaOwnerExpr :: Surf.SurfaceExpr
annotatedParameterPlaceholderAtExactLambdaOwnerExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
            ( Surf.EApp
                (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                mixedAnnotationExpr
                            )
                            (Surf.ELit (Surf.LInt 12))
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

vacuousRootBinderBeforeLambdaCodomainExpr :: Surf.SurfaceExpr
vacuousRootBinderBeforeLambdaCodomainExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        (Surf.ELit (Surf.LInt (-4)))
                                        ( Surf.ELet
                                            "_generatedSeedId"
                                            ( Surf.EAnn
                                                ( Surf.ELam
                                                    "_generatedSeedArg"
                                                    (Surf.EVar "_generatedSeedArg")
                                                )
                                                sigmaIdSource
                                            )
                                            ( Surf.ELet
                                                "_generatedSeedDiscard"
                                                ( Surf.EApp
                                                    (Surf.EVar "_generatedSeedId")
                                                    (Surf.ELit (Surf.LInt (-13)))
                                                )
                                                ( Surf.EApp
                                                    (Surf.EVar "_generatedSeedId")
                                                    (Surf.ELit (Surf.LBool False))
                                                )
                                            )
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                        (Surf.ELit (Surf.LInt 5))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

boundedLambdaResultBeforeEnclosingIdentityApplicationExpr :: Surf.SurfaceExpr
boundedLambdaResultBeforeEnclosingIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            mixedAnnotationExpr
                        )
                        (Surf.ELit (Surf.LInt (-13)))
                    )
                )
                (Surf.ELit (Surf.LInt 14))
            )
        )
    )

rootOwnedLambdaBindersOutsideTransparentLetExpr :: Surf.SurfaceExpr
rootOwnedLambdaBindersOutsideTransparentLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool False))
                                    (Surf.STBase "Bool")
                                )
                                mixedAnnotationExpr
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
        )
        (Surf.ELit (Surf.LInt (-3)))
    )
    (Surf.EVar "_generatedWrap1")

identityTopologyConsumerThroughNestedAppliedLambdasExpr :: Surf.SurfaceExpr
identityTopologyConsumerThroughNestedAppliedLambdasExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 5))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            ( Surf.EAnn
                                                ( Surf.ELam
                                                    "_generatedSeedX"
                                                    (Surf.EVar "_generatedSeedX")
                                                )
                                                sigmaIdSource
                                            )
                                        )
                                        (Surf.ELit (Surf.LBool False))
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
        (Surf.ELit (Surf.LInt (-12)))
    )

nestedAnnotatedSelfAppUnderAppliedUnusedLambdaExpr :: Surf.SurfaceExpr
nestedAnnotatedSelfAppUnderAppliedUnusedLambdaExpr =
  Surf.EApp
    (Surf.ELam "unused" annotatedSelfAppExpr)
    (Surf.ELit (Surf.LBool False))

duplicateConsumerClosureInsideOwnBoundExpr :: Surf.SurfaceExpr
duplicateConsumerClosureInsideOwnBoundExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedX"
                                (Surf.EVar "_generatedSeedX")
                            )
                            sigmaIdSource
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt (-15)))

returnedLambdaWithoutSyntheticBinderCycleExpr :: Surf.SurfaceExpr
returnedLambdaWithoutSyntheticBinderCycleExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "_generatedWrap5"
                    (Surf.ELit (Surf.LInt 6))
                    ( Surf.ELet
                        "_generatedWrap6"
                        (Surf.ELit (Surf.LInt (-4)))
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap7"
                                (Surf.EVar "_generatedWrap7")
                            )
                            ( Surf.ELet
                                "k"
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "x"
                                        ( Surf.ELam
                                            "y"
                                            (Surf.EVar "y")
                                        )
                                    )
                                    ( Surf.STForall
                                        "alpha"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "beta")
                                            ( Surf.STArrow
                                                (Surf.STVar "alpha")
                                                (Surf.STVar "alpha")
                                            )
                                        )
                                    )
                                )
                                ( Surf.EApp
                                    ( Surf.EApp
                                        (Surf.EVar "k")
                                        (Surf.ELit (Surf.LInt 1))
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

annotatedSelfAppThroughTransparentLambdaLetExpr :: Surf.SurfaceExpr
annotatedSelfAppThroughTransparentLambdaLetExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.ELamAnn
                            "g"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "g")
                                (Surf.EVar "g")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 9))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

annotatedSelfAppThroughIdentityAppliedNestedLambdaLetsExpr :: Surf.SurfaceExpr
annotatedSelfAppThroughIdentityAppliedNestedLambdaLetsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt (-16)))
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            annotatedSelfAppExpr
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
    )

boundedIdentityThroughAppliedAnnotatedLambdasExpr :: Surf.SurfaceExpr
boundedIdentityThroughAppliedAnnotatedLambdasExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.EAnn
                        (Surf.ELam "x" (Surf.EVar "x"))
                        ( Surf.STForall
                            "a"
                            (Just (Surf.mkSrcBound sigmaIdSource))
                            ( Surf.STArrow
                                (Surf.STVar "a")
                                (Surf.STVar "a")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-14)))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt (-1)))

paperSelfApplicationThroughIdentityAppliedOpaqueLambdaExpr :: Surf.SurfaceExpr
paperSelfApplicationThroughIdentityAppliedOpaqueLambdaExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 5))
                    (Surf.ELam "_generatedWrap5" annotatedSelfAppExpr)
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
    )

polymorphicIdentityThroughCoalescedGammaClosuresExpr :: Surf.SurfaceExpr
polymorphicIdentityThroughCoalescedGammaClosuresExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 14))
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    sigmaIdSource
                                )
                            )
                            (Surf.ELit (Surf.LInt 6))
                        )
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

paperSelfApplicationBeneathPartiallyAppliedNestedLambdasExpr ::
  Surf.SurfaceExpr
paperSelfApplicationBeneathPartiallyAppliedNestedLambdasExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.ELam "_generatedWrap4" annotatedSelfAppExpr)
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

letGeneralizedMixedAnnotationThroughIdentityApplicationExpr ::
  Surf.SurfaceExpr
letGeneralizedMixedAnnotationThroughIdentityApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-4)))
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                mixedAnnotationExpr
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-5)))

descendantApplicationGammaThroughCoalescedIdentityApplicationsExpr ::
  Surf.SurfaceExpr
descendantApplicationGammaThroughCoalescedIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                (Surf.EVar "_generatedWrap3")
            )
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 3))
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt 15))
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.ELamAnn
                                        "_generatedSeedPoly"
                                        sigmaIdSource
                                        ( Surf.EApp
                                            (Surf.EVar "_generatedSeedPoly")
                                            (Surf.ELit (Surf.LInt (-15)))
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt 0))
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

consumedRootDeclarationThroughNestedMixedAnnotationOwnersExpr ::
  Surf.SurfaceExpr
consumedRootDeclarationThroughNestedMixedAnnotationOwnersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            nestedMixedAnnotationExpr
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
    )

letBoundPaperSelfApplicationThroughDirectIdentityApplicationExpr ::
  Surf.SurfaceExpr
letBoundPaperSelfApplicationThroughDirectIdentityApplicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        annotatedSelfAppExpr
    )
    (Surf.EVar "_generatedWrap1")

returnedHigherRankLambdaThroughAnnotatedApplicationsExpr :: Surf.SurfaceExpr
returnedHigherRankLambdaThroughAnnotatedApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.EAnn (Surf.ELit (Surf.LBool True)) (Surf.STBase "Bool"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.EAnn (Surf.ELit (Surf.LBool False)) (Surf.STBase "Bool"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.ELet
                                "_generatedWrap6"
                                (Surf.ELit (Surf.LInt 10))
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.ELamAnn
                                            "_generatedSeedPoly"
                                            sigmaIdSource
                                            ( Surf.EApp
                                                (Surf.EVar "_generatedSeedPoly")
                                                (Surf.ELit (Surf.LInt 8))
                                            )
                                        )
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt (-9)))
                    )
                )
                (Surf.ELit (Surf.LInt 0))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

returnedFreeAnnotationBinderAtEnclosingLambdaExpr :: Surf.SurfaceExpr
returnedFreeAnnotationBinderAtEnclosingLambdaExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-9)))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-7)))
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "x"
                                    ( Surf.ELam
                                        "y"
                                        (Surf.EVar "y")
                                    )
                                )
                                ( Surf.STForall
                                    "alpha"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "beta")
                                        ( Surf.STArrow
                                            (Surf.STVar "alpha")
                                            (Surf.STVar "alpha")
                                        )
                                    )
                                )
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

appliedAnnotatedSelfAppLetResultExpr :: Surf.SurfaceExpr
appliedAnnotatedSelfAppLetResultExpr =
  Surf.ELet
    "kept"
    nestedAnnotatedSelfAppUnderAppliedUnusedLambdaExpr
    (Surf.EVar "kept")

appliedAnnotatedSelfAppAfterUnusedLetExpr :: Surf.SurfaceExpr
appliedAnnotatedSelfAppAfterUnusedLetExpr =
  Surf.ELet
    "ignored"
    (Surf.ELit (Surf.LInt (-1)))
    nestedAnnotatedSelfAppUnderAppliedUnusedLambdaExpr

nestedAnnotatedSelfAppThroughLetAndApplicationOwnersExpr :: Surf.SurfaceExpr
nestedAnnotatedSelfAppThroughLetAndApplicationOwnersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt (-1)))
                ( Surf.EApp
                    (Surf.ELam "_generatedWrap5" annotatedSelfAppExpr)
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
        (Surf.EVar "_generatedWrap2")
    )

annotatedSelfAppThroughNestedUnusedApplicationsExpr :: Surf.SurfaceExpr
annotatedSelfAppThroughNestedUnusedApplicationsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt 5))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                (Surf.ELit (Surf.LInt 9))
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-6)))
                                    annotatedSelfAppExpr
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

annotatedLambdaThroughIdentityApplicationExpr :: Surf.SurfaceExpr
annotatedLambdaThroughIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "identity" (Surf.EVar "identity"))
    generatedAnnotatedIntLambdaExpr

letWrappedAnnotatedLambdaThroughIdentityApplicationExpr :: Surf.SurfaceExpr
letWrappedAnnotatedLambdaThroughIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "identity" (Surf.EVar "identity"))
    ( Surf.ELet
        "unused"
        (Surf.ELit (Surf.LInt 2))
        generatedAnnotatedIntLambdaExpr
    )

generatedAnnotatedIntLambdaExpr :: Surf.SurfaceExpr
generatedAnnotatedIntLambdaExpr =
  Surf.ELamAnn
    "poly"
    sigmaIdSource
    (Surf.EApp (Surf.EVar "poly") (Surf.ELit (Surf.LInt 15)))

identityWrappedAnnotatedLambdaUnderOuterApplicationExpr :: Surf.SurfaceExpr
identityWrappedAnnotatedLambdaUnderOuterApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "unused"
        annotatedLambdaThroughIdentityApplicationExpr
    )
    (Surf.ELit (Surf.LBool False))

nestedMixedAnnotationThroughUnusedConstructionsExpr :: Surf.SurfaceExpr
nestedMixedAnnotationThroughUnusedConstructionsExpr =
  Surf.ELet
    "_generatedOuterUnused"
    (Surf.ELit (Surf.LInt (-6)))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedAnnotatedUnused"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedRoundTrip"
                ( Surf.ELet
                    "_generatedInnerUnused"
                    (Surf.ELit (Surf.LInt (-11)))
                    nestedMixedAnnotationExpr
                )
                (Surf.EVar "_generatedRoundTrip")
            )
        )
        (Surf.ELit (Surf.LInt 5))
    )

mixedAnnotationThroughNestedAnnotatedApplicationsExpr :: Surf.SurfaceExpr
mixedAnnotationThroughNestedAnnotatedApplicationsExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedOuterAnnotatedUnused"
        (Surf.STBase "Int")
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedInnerAnnotatedUnused"
                (Surf.STBase "Int")
                mixedAnnotationExpr
            )
            (Surf.ELit (Surf.LInt (-14)))
        )
    )
    (Surf.ELit (Surf.LInt (-2)))

nestedAnnotatedApplicationsThroughOuterConstructionsExpr :: Surf.SurfaceExpr
nestedAnnotatedApplicationsThroughOuterConstructionsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedOuterUnused"
        ( Surf.ELet
            "_generatedRoundTrip"
            mixedAnnotationThroughNestedAnnotatedApplicationsExpr
            (Surf.EVar "_generatedRoundTrip")
        )
    )
    (Surf.ELit (Surf.LBool False))

identityWrappedGroundLetResultExpr :: Surf.SurfaceExpr
identityWrappedGroundLetResultExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.ELet
                "_generatedWrap4"
                (Surf.ELit (Surf.LInt 6))
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool False))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LInt 13))
                        (Surf.STBase "Int")
                    )
                )
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

annotatedIdentityThroughThreeUnusedLambdasExpr :: Surf.SurfaceExpr
annotatedIdentityThroughThreeUnusedLambdasExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EAnn
                ( Surf.ELam
                    "_generatedSeedX"
                    (Surf.EVar "_generatedSeedX")
                )
                sigmaIdSource
            )
        )
    )

sourceProjectedAnnotationInGroundResultLambdaExpr :: Surf.SurfaceExpr
sourceProjectedAnnotationInGroundResultLambdaExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "k"
                mixedAnnotationExpr
                ( Surf.EApp
                    ( Surf.EApp
                        (Surf.EVar "k")
                        (Surf.ELit (Surf.LInt 1))
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
    )
    (Surf.ELit (Surf.LInt (-9)))

identityAppliedMixedAnnotationLetExpr :: Surf.SurfaceExpr
identityAppliedMixedAnnotationLetExpr =
  Surf.ELet
    "_generatedWrap3"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap4"
            (Surf.EVar "_generatedWrap4")
        )
        mixedAnnotationExpr
    )
    (Surf.EVar "_generatedWrap3")

identityAppliedMixedAnnotationLetUnderUnusedLambdaExpr :: Surf.SurfaceExpr
identityAppliedMixedAnnotationLetUnderUnusedLambdaExpr =
  Surf.ELam
    "_generatedWrap2"
    identityAppliedMixedAnnotationLetExpr

boundedAnnotationFromUnusedApplicationLetExpr :: Surf.SurfaceExpr
boundedAnnotationFromUnusedApplicationLetExpr =
  Surf.ELet
    "_generatedWrap6"
    ( Surf.EApp
        (Surf.ELam "_generatedWrap7" boundedIdentityAnnotationExpr)
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap6")

boundedAnnotationLetUnderUnusedApplicationExpr :: Surf.SurfaceExpr
boundedAnnotationLetUnderUnusedApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap4"
        ( Surf.ELet
            "_generatedWrap5"
            (Surf.ELit (Surf.LInt (-9)))
            boundedAnnotationFromUnusedApplicationLetExpr
        )
    )
    (Surf.ELit (Surf.LBool False))

identityAppliedBoundedAnnotationLetExpr :: Surf.SurfaceExpr
identityAppliedBoundedAnnotationLetExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
    boundedAnnotationLetUnderUnusedApplicationExpr

annotatedApplicationAroundBoundedAnnotationLetExpr :: Surf.SurfaceExpr
annotatedApplicationAroundBoundedAnnotationLetExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        identityAppliedBoundedAnnotationLetExpr
    )
    (Surf.ELit (Surf.LInt (-15)))

sharedApplicationGammaClosureExpr :: Surf.SurfaceExpr
sharedApplicationGammaClosureExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            (Surf.ELam "_generatedWrap6" nestedMixedAnnotationExpr)
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LBool False))
        )
        (Surf.EVar "_generatedWrap2")
    )

polymorphicLetDependencyThroughIdentityAppliedNestedLambdaExpr ::
  Surf.SurfaceExpr
polymorphicLetDependencyThroughIdentityAppliedNestedLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELet
                        "_generatedSeedId"
                        ( Surf.EAnn
                            ( Surf.ELam
                                "_generatedSeedArg"
                                (Surf.EVar "_generatedSeedArg")
                            )
                            sigmaIdSource
                        )
                        ( Surf.ELet
                            "_generatedSeedDiscard"
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedId")
                                (Surf.ELit (Surf.LInt 9))
                            )
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedId")
                                (Surf.ELit (Surf.LBool False))
                            )
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Regression for a generated wrapper whose application result is copied to a
-- deeper target scope.  The two forwarded root edges and the direct
-- application edge form one source-owned Gamma closure; the copied result
-- must not move that Gamma away from this application occurrence.
forwardedGroundApplicationGammaExpr :: Surf.SurfaceExpr
forwardedGroundApplicationGammaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool False))
        (Surf.STBase "Bool")
    )
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    multiUseAnnotatedIdentitySeed
                )
                (Surf.ELit (Surf.LInt 11))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

mixedAnnotationThroughNestedLambdaPacketExpr :: Surf.SurfaceExpr
mixedAnnotationThroughNestedLambdaPacketExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt 16))
            ( Surf.ELam
                "_generatedWrap4"
                (Surf.ELam "_generatedWrap5" mixedAnnotationExpr)
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

letBoundMixedAnnotationThroughUnusedLambdasExpr :: Surf.SurfaceExpr
letBoundMixedAnnotationThroughUnusedLambdasExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            mixedAnnotationExpr
            (Surf.EVar "_generatedWrap3")
        )
    )

appliedMixedAnnotationLetExpr :: Surf.SurfaceExpr
appliedMixedAnnotationLetExpr =
  Surf.ELet
    "k"
    mixedAnnotationExpr
    ( Surf.EApp
        ( Surf.EApp
            (Surf.EVar "k")
            (Surf.ELit (Surf.LInt 1))
        )
        (Surf.ELit (Surf.LBool True))
    )

nestedApplicationOwnersAroundAppliedMixedAnnotationExpr :: Surf.SurfaceExpr
nestedApplicationOwnersAroundAppliedMixedAnnotationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            (Surf.EVar "_generatedWrap7")
                                        )
                                        appliedMixedAnnotationLetExpr
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                        (Surf.ELit (Surf.LInt (-15)))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

nestedUnusedLambdasAroundAppliedPolymorphicResultExpr :: Surf.SurfaceExpr
nestedUnusedLambdasAroundAppliedPolymorphicResultExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt 16))
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )

rootAmbientApplicationCertificateExpr :: Surf.SurfaceExpr
rootAmbientApplicationCertificateExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    ( Surf.ELam
                        "x"
                        (Surf.ELam "y" (Surf.EVar "y"))
                    )
                    ( Surf.STForall
                        "alpha"
                        Nothing
                        ( Surf.STArrow
                            (Surf.STVar "beta")
                            ( Surf.STArrow
                                (Surf.STVar "alpha")
                                (Surf.STVar "alpha")
                            )
                        )
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

identityApplicationWithNestedAnnotatedUseExpr :: Surf.SurfaceExpr
identityApplicationWithNestedAnnotatedUseExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.ELet
                    "k"
                    ( Surf.EAnn
                        ( Surf.ELam
                            "x"
                            (Surf.ELam "y" (Surf.EVar "y"))
                        )
                        ( Surf.STForall
                            "alpha"
                            Nothing
                            ( Surf.STArrow
                                (Surf.STVar "beta")
                                ( Surf.STArrow
                                    (Surf.STVar "alpha")
                                    (Surf.STVar "alpha")
                                )
                            )
                        )
                    )
                    ( Surf.EApp
                        ( Surf.EApp
                            (Surf.EVar "k")
                            (Surf.ELit (Surf.LInt 1))
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

independentApplicationGammasAtLetOwnerExpr :: Surf.SurfaceExpr
independentApplicationGammasAtLetOwnerExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool True))
                                (Surf.STBase "Bool")
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

annotatedPolymorphicParameterThroughUnusedOwnersExpr :: Surf.SurfaceExpr
annotatedPolymorphicParameterThroughUnusedOwnersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            (Surf.ELit (Surf.LInt (-2)))
            ( Surf.ELam
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt 3))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
        )
    )

annotatedPolymorphicParameterBodyEndpointThroughAppliedOwnersExpr :: Surf.SurfaceExpr
annotatedPolymorphicParameterBodyEndpointThroughAppliedOwnersExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.ELamAnn
                        "_generatedSeedPoly"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "_generatedSeedPoly")
                            (Surf.ELit (Surf.LInt 10))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )
    (Surf.ELit (Surf.LBool True))

annotatedPolymorphicParameterThroughPartiallyAppliedFourLambdaSpineExpr ::
  Surf.SurfaceExpr
annotatedPolymorphicParameterThroughPartiallyAppliedFourLambdaSpineExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELamAnn
                        "_generatedSeedPoly"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "_generatedSeedPoly")
                            (Surf.ELit (Surf.LInt (-6)))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Frozen from the 20th case of generated seed 314159.  The paper's @g g@
-- endpoint crosses an applied annotated lambda, a let, an unapplied lambda,
-- and an identity application before becoming the codomain of two enclosing
-- lambdas.  Its result-bound @sigma-id@ and parameter @sigma-id@ must remain
-- distinct lexical declarations throughout those owner publications.
annotatedSelfAppThroughNestedOwnersSeed314159Expr
  :: Surf.SurfaceExpr
annotatedSelfAppThroughNestedOwnersSeed314159Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt (-12)))
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                annotatedSelfAppExpr
                            )
                            (Surf.ELit (Surf.LInt (-13)))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

-- Frozen from the 67th case of generated seed 314159.  The annotation's
-- universal @alpha@ is emitted by the annotated value itself after several
-- enclosing application owners have already published their ambient Gamma.
-- It must be removed from the checking environment before the local ETyAbs is
-- checked; otherwise the same lexical declaration is both ambient and local.
annotationForallLocalThroughNestedOwnersSeed314159Expr
  :: Surf.SurfaceExpr
annotationForallLocalThroughNestedOwnersSeed314159Expr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            (Surf.EVar "_generatedWrap4")
                        )
                        ( Surf.ELet
                            "_generatedWrap5"
                            (Surf.ELit (Surf.LInt (-9)))
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "x"
                                            ( Surf.ELam
                                                "y"
                                                (Surf.EVar "y")
                                            )
                                        )
                                        ( Surf.STForall
                                            "alpha"
                                            Nothing
                                            ( Surf.STArrow
                                                (Surf.STVar "beta")
                                                ( Surf.STArrow
                                                    (Surf.STVar "alpha")
                                                    (Surf.STVar "alpha")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
        )
        (Surf.ELit (Surf.LBool True))
    )
    (Surf.EVar "_generatedWrap1")

-- Frozen from the 68th case of generated seed 314159.  This is the direct
-- lambda counterpart of the preceding let-bound regression: the annotation
-- forall crosses two applied administrative lambdas before the enclosing
-- lambda publishes its exact endpoint.
annotationForallLocalThroughDirectLambdaOwnersSeed314159Expr
  :: Surf.SurfaceExpr
annotationForallLocalThroughDirectLambdaOwnersSeed314159Expr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap4"
                            (Surf.EVar "_generatedWrap4")
                        )
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "x"
                                    ( Surf.ELam
                                        "y"
                                        (Surf.EVar "y")
                                    )
                                )
                                ( Surf.STForall
                                    "alpha"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "beta")
                                        ( Surf.STArrow
                                            (Surf.STVar "alpha")
                                            (Surf.STVar "alpha")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

higherRankParameterThroughIdentityApplicationExpr :: Surf.SurfaceExpr
higherRankParameterThroughIdentityApplicationExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        (Surf.EVar "_generatedWrap1")
    )
    ( Surf.ELamAnn
        "_generatedSeedPoly"
        sigmaIdSource
        ( Surf.EApp
            (Surf.EVar "_generatedSeedPoly")
            (Surf.ELit (Surf.LInt 15))
        )
    )

-- Minimized from case 15 of generated seed 99.  The two checked ground
-- wrappers make the higher-rank lambda their exact application result before
-- the ordinary identity function is entered.  Its domain and codomain are
-- alpha-equivalent but contain two distinct lexical forall declarations; the
-- identity constructor must allocate the codomain presentation before it
-- asks the lambda child to realize that endpoint.
higherRankIdentityEndpointThroughAppliedWrappersExpr :: Surf.SurfaceExpr
higherRankIdentityEndpointThroughAppliedWrappersExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool True))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt (-9)))
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-13)))
            )
        )
        (Surf.ELit (Surf.LInt 13))
    )

boundedAnnotationThroughIdentityAppliedLambdaLetExpr :: Surf.SurfaceExpr
boundedAnnotationThroughIdentityAppliedLambdaLetExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            (Surf.EVar "_generatedWrap2")
        )
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                boundedIdentityAnnotationExpr
                (Surf.EVar "_generatedWrap4")
            )
        )
    )

termUsedRootBinderThroughNestedIdentityLetsExpr :: Surf.SurfaceExpr
termUsedRootBinderThroughNestedIdentityLetsExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-4)))
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "_generatedSeedX"
                                    (Surf.EVar "_generatedSeedX")
                                )
                                sigmaIdSource
                            )
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LBool False))

multiUseAnnotationUnderUnusedLambdaLetExpr :: Surf.SurfaceExpr
multiUseAnnotationUnderUnusedLambdaLetExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        (multiUseAnnotatedIdentitySeedWith 5 True)
    )
    (Surf.EVar "_generatedWrap1")

boundedAnnotationThroughNestedApplicationOwnersExpr :: Surf.SurfaceExpr
boundedAnnotationThroughNestedApplicationOwnersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool True))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    boundedIdentityAnnotationExpr
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                            (Surf.EVar "_generatedWrap6")
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-10)))
        )
    )

vacuousLambdaBodyEndpointThroughNestedOwnersExpr :: Surf.SurfaceExpr
vacuousLambdaBodyEndpointThroughNestedOwnersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 9))
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LInt 14))
                                        (Surf.STBase "Int")
                                    )
    )
    (Surf.ELit (Surf.LInt 2))

                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

identityAppliedMultiUseAnnotationThroughUnusedApplicationExpr :: Surf.SurfaceExpr
identityAppliedMultiUseAnnotationThroughUnusedApplicationExpr =
  unusedAppliedMultiUseAnnotation
    (roundTripMultiUseAnnotation identityAppliedMultiUseAnnotation)

annotatedIdentityApplicationThroughUnusedApplicationExpr :: Surf.SurfaceExpr
annotatedIdentityApplicationThroughUnusedApplicationExpr =
  unusedAppliedMultiUseAnnotation
    (roundTripMultiUseAnnotation annotatedIdentityAppliedMultiUseAnnotation)

mixedAnnotationRootRaiseMergeUnderUnusedApplicationLetExpr :: Surf.SurfaceExpr
mixedAnnotationRootRaiseMergeUnderUnusedApplicationLetExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" mixedAnnotationExpr)
            (Surf.ELit (Surf.LBool False))
        )
        (Surf.EVar "_generatedWrap2")
    )

nestedExactLambdaBinderSpineThroughGroundWrappersExpr :: Surf.SurfaceExpr
nestedExactLambdaBinderSpineThroughGroundWrappersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                )
                                (Surf.ELit (Surf.LInt 13))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LBool True))
    )

rootPacketRouteThroughIdentityAppliedAnnotatedLetExpr :: Surf.SurfaceExpr
rootPacketRouteThroughIdentityAppliedAnnotatedLetExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                ( Surf.ELet
                    "_generatedWrap5"
                    ( Surf.ELamAnn
                        "_generatedSeedPoly"
                        sigmaIdSource
                        ( Surf.EApp
                            (Surf.EVar "_generatedSeedPoly")
                            (Surf.ELit (Surf.LInt 0))
                        )
                    )
                    (Surf.EVar "_generatedWrap5")
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

administrativeLambdaParameterUnderSourceForallExpr :: Surf.SurfaceExpr
administrativeLambdaParameterUnderSourceForallExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            (Surf.ELit (Surf.LInt 13))
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            (Surf.EVar "_generatedWrap5")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        sigmaIdSource
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LBool False))
                        )
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt 3))

completedForallPacketAtEnclosingLambdaConsumerExpr :: Surf.SurfaceExpr
completedForallPacketAtEnclosingLambdaConsumerExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.EApp
                                        ( Surf.ELam
                                            "_generatedWrap7"
                                            ( Surf.EAnn
                                                (Surf.ELit (Surf.LInt (-3)))
                                                (Surf.STBase "Int")
                                            )
                                        )
                                        (Surf.ELit (Surf.LBool False))
                                    )
                                )
                                (Surf.ELit (Surf.LBool False))
                            )
                            (Surf.EVar "_generatedWrap5")
                        )
                    )
                    (Surf.ELit (Surf.LInt 10))
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
    )
    (Surf.ELit (Surf.LInt 1))

-- Fixed forms minimized from generated O15 counterexamples.  These retain the
-- exact owner nesting that exercises construction-time Gamma authority, while
-- removing irrelevant random wrappers.
sourceOwnedApplicationGammaConstructionExpr :: Surf.SurfaceExpr
sourceOwnedApplicationGammaConstructionExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LBool True))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LInt 8))
                                (Surf.STBase "Int")
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt (-10)))
            )
        )
        (Surf.EVar "_generatedWrap2")
    )

deepMixedAnnotationOwnerClosureExpr :: Surf.SurfaceExpr
deepMixedAnnotationOwnerClosureExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap3"
                    (Surf.STBase "Int")
                    ( Surf.ELet
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.EApp
                                    ( Surf.ELam
                                        "_generatedWrap6"
                                        (Surf.ELam "_generatedWrap7" mixedAnnotationExpr)
                                    )
                                    (Surf.ELit (Surf.LBool False))
                                )
                            )
                            (Surf.ELit (Surf.LInt 1))
                        )
                        (Surf.EVar "_generatedWrap4")
                    )
                )
                (Surf.ELit (Surf.LInt 9))
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool False))

paperSelfApplicationBeneathNestedLambdaOwnersExpr :: Surf.SurfaceExpr
paperSelfApplicationBeneathNestedLambdaOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap4" annotatedSelfAppExpr)
                (Surf.ELit (Surf.LBool True))
            )
        )
    )
    (Surf.EVar "_generatedWrap1")

ownerFinalBoundedPublicationExpr :: Surf.SurfaceExpr
ownerFinalBoundedPublicationExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LInt 0))
                                        (Surf.STBase "Int")
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 7))
                    )
                )
                (Surf.EVar "_generatedWrap3")
            )
        )
        (Surf.ELit (Surf.LInt 2))
    )
    (Surf.EVar "_generatedWrap1")

graphOwnedBodyForallsWithoutSourceOrderExpr :: Surf.SurfaceExpr
graphOwnedBodyForallsWithoutSourceOrderExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool False))
                                            (Surf.STBase "Bool")
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-11)))
                            )
                        )
                        (Surf.ELit (Surf.LInt 16))
                    )
                )
                (Surf.ELit (Surf.LBool False))
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

distinctLetConsumerBoundsAtSharedExteriorExpr :: Surf.SurfaceExpr
distinctLetConsumerBoundsAtSharedExteriorExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 2))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap7"
                                    (Surf.STBase "Int")
                                    ( Surf.EAnn
                                        ( Surf.ELam
                                            "_generatedSeedX"
                                            (Surf.EVar "_generatedSeedX")
                                        )
                                        sigmaIdSource
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-5)))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap4")
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

vacuousSharedPacketConsumersAtFinalLetGammaExpr :: Surf.SurfaceExpr
vacuousSharedPacketConsumersAtFinalLetGammaExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.EAnn
                    ( Surf.ELam
                        "_generatedSeedX"
                        (Surf.EVar "_generatedSeedX")
                    )
                    sigmaIdSource
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

lambdaConsumerThroughSourceOccurrenceRouteExpr :: Surf.SurfaceExpr
lambdaConsumerThroughSourceOccurrenceRouteExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-2)))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                ( Surf.ELam
                    "_generatedSeedX"
                    ( Surf.ELam
                        "_generatedSeedY"
                        (Surf.EVar "_generatedSeedY")
                    )
                )
                ( Surf.STForall
                    "alpha"
                    Nothing
                    ( Surf.STArrow
                        (Surf.STVar "beta")
                        ( Surf.STArrow
                            (Surf.STVar "alpha")
                            (Surf.STVar "alpha")
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

constructedApplicationGammaBinderAuthorityExpr :: Surf.SurfaceExpr
constructedApplicationGammaBinderAuthorityExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-1)))
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            ( Surf.EAnn
                                (Surf.ELit (Surf.LBool False))
                                (Surf.STBase "Bool")
                            )
                            mixedAnnotationExpr
                        )
                        (Surf.EVar "_generatedWrap5")
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )
    (Surf.EVar "_generatedWrap1")

nestedLambdaConsumerAfterAnnotatedParameterApplicationExpr :: Surf.SurfaceExpr
nestedLambdaConsumerAfterAnnotatedParameterApplicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap5"
                            (Surf.STBase "Int")
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    (Surf.EVar "_generatedWrap6")
                                )
                                mixedAnnotationExpr
                            )
                        )
                        (Surf.ELit (Surf.LInt (-15)))
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-2)))
        )
    )

nestedLambdaResultAtExactEnclosingBoundExpr :: Surf.SurfaceExpr
nestedLambdaResultAtExactEnclosingBoundExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 2))
                    ( Surf.ELet
                        "_generatedWrap5"
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LBool False))
                            (Surf.STBase "Bool")
                        )
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap6"
                                (Surf.EVar "_generatedWrap6")
                            )
                            ( Surf.ELam
                                "_generatedWrap7"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt (-16)))
    )

applicationGammaDependencyThroughAnnotatedLetResultExpr :: Surf.SurfaceExpr
applicationGammaDependencyThroughAnnotatedLetResultExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-9)))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap4"
                    (Surf.EVar "_generatedWrap4")
                )
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap5"
                        (Surf.STBase "Int")
                        ( Surf.ELet
                            "_generatedWrap6"
                            boundedIdentityAnnotationExpr
                            (Surf.EVar "_generatedWrap6")
                        )
                    )
                    (Surf.ELit (Surf.LInt (-13)))
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

topologyConsumerThroughExactEnclosingRouteExpr :: Surf.SurfaceExpr
topologyConsumerThroughExactEnclosingRouteExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-5)))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EAnn
                        (Surf.ELit (Surf.LBool True))
                        (Surf.STBase "Bool")
                    )
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.ELet
                                "_generatedWrap6"
                                ( Surf.EAnn
                                    (Surf.ELit (Surf.LBool True))
                                    (Surf.STBase "Bool")
                                )
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    ( Surf.ELet
                                        "k"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "x"
                                                ( Surf.ELam
                                                    "y"
                                                    (Surf.EVar "y")
                                                )
                                            )
                                            ( Surf.STForall
                                                "alpha"
                                                Nothing
                                                ( Surf.STArrow
                                                    (Surf.STVar "beta")
                                                    ( Surf.STArrow
                                                        (Surf.STVar "alpha")
                                                        (Surf.STVar "alpha")
                                                    )
                                                )
                                            )
                                        )
                                        ( Surf.EApp
                                            ( Surf.EApp
                                                (Surf.EVar "k")
                                                (Surf.ELit (Surf.LInt 1))
                                            )
                                            (Surf.ELit (Surf.LBool True))
                                        )
                                    )
                                    (Surf.EVar "_generatedWrap7")
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt 12))
    )

restoredAnnotationBinderDuringSubtermGeneralizationExpr :: Surf.SurfaceExpr
restoredAnnotationBinderDuringSubtermGeneralizationExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-15)))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.EApp
                    ( Surf.ELamAnn
                        "_generatedWrap4"
                        (Surf.STBase "Int")
                        ( Surf.EApp
                            ( Surf.ELam
                                "_generatedWrap5"
                                (Surf.EVar "_generatedWrap5")
                            )
                            ( Surf.EApp
                                ( Surf.ELamAnn
                                    "_generatedWrap6"
                                    (Surf.STBase "Int")
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedX"
                                                (Surf.EVar "_generatedSeedX")
                                            )
                                            sigmaIdSource
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LInt (-2)))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LInt (-8)))
                )
            )
            (Surf.ELit (Surf.LInt 2))
        )
    )

appliedMixedAnnotationBeneathNestedUnusedLambdasExpr :: Surf.SurfaceExpr
appliedMixedAnnotationBeneathNestedUnusedLambdasExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELam
                            "_generatedWrap6"
                            ( Surf.ELet
                                "k"
                                mixedAnnotationExpr
                                ( Surf.EApp
                                    ( Surf.EApp
                                        (Surf.EVar "k")
                                        (Surf.ELit (Surf.LInt 1))
                                    )
                                    (Surf.ELit (Surf.LBool True))
                                )
                            )
                        )
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.ELit (Surf.LInt 0))
        )
    )

independentGammaEndpointsAtNestedLambdaOwnerExpr :: Surf.SurfaceExpr
independentGammaEndpointsAtNestedLambdaOwnerExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            sigmaIdSource
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt 12))
                            )
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
                (Surf.EVar "_generatedWrap3")
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LBool True))

vacuousEnclosingConsumerAroundAnnotatedLetResultExpr :: Surf.SurfaceExpr
vacuousEnclosingConsumerAroundAnnotatedLetResultExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap6"
                                    ( Surf.ELet
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            (Surf.ELit (Surf.LBool True))
                                            (Surf.STBase "Bool")
                                        )
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "_generatedSeedX"
                                                (Surf.EVar "_generatedSeedX")
                                            )
                                            sigmaIdSource
                                        )
                                    )
                                )
                                (Surf.ELit (Surf.LBool True))
                            )
                        )
                        (Surf.ELit (Surf.LBool False))
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

identityAppliedMultiUseAnnotation :: Surf.SurfaceExpr
identityAppliedMultiUseAnnotation =
  Surf.EApp
    (Surf.ELam "_generatedWrap7" (Surf.EVar "_generatedWrap7"))
    multiUseAnnotatedIdentitySeed

-- Minimized from generated seed 486053823.  The direct argument of the
-- identity application constructs @a -> Bool@ from a child whose source type
-- still exposes its bounded result forall.  The exact direct Gamma bound must
-- refine the same unbounded owner declaration before the application claims
-- are published.
identityAppliedLambdaWithMixedAnnotationExpr :: Surf.SurfaceExpr
identityAppliedLambdaWithMixedAnnotationExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt 9))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool True))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap5"
                        (Surf.EVar "_generatedWrap5")
                    )
                    ( Surf.ELam
                        "_generatedWrap6"
                        appliedMixedAnnotationLetExpr
                    )
                )
                (Surf.EVar "_generatedWrap4")
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- Minimized from the next seed-486053823 counterexample.  The source
-- annotation binder is lexical to the higher-rank parameter type; the direct
-- graph bound must not expose it as a free identity-application endpoint.
higherRankLambdaEndpointThroughNestedIdentityApplicationsExpr :: Surf.SurfaceExpr
higherRankLambdaEndpointThroughNestedIdentityApplicationsExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-14)))
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EApp
                (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                ( Surf.ELamAnn
                    "_generatedSeedPoly"
                    sigmaIdSource
                    ( Surf.EApp
                        (Surf.EVar "_generatedSeedPoly")
                        (Surf.ELit (Surf.LInt (-12)))
                    )
                )
            )
            (Surf.EVar "_generatedWrap3")
        )
    )

-- The third minimized seed-486053823 counterexample.  A transparent identity
-- returns the checked lambda argument; a provisional direct-lambda result
-- packet must not replace that value-owned result before the enclosing
-- partially applied lambda consumes its own construction endpoint.
identityWrappedMixedAnnotationThroughPartiallyAppliedLambdaExpr :: Surf.SurfaceExpr
identityWrappedMixedAnnotationThroughPartiallyAppliedLambdaExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        (Surf.EVar "_generatedWrap4")
                    )
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                mixedAnnotationExpr
                            )
                            (Surf.ELit (Surf.LInt 13))
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- The fourth minimized seed-486053823 counterexample.  The annotation's free
-- source @beta@ and the enclosing packet consumer are two references to one
-- construction route; packet preparation must align their identities before
-- checking lexical closure.
mixedAnnotationPacketThroughNestedIdentityAndLetOwnersExpr :: Surf.SurfaceExpr
mixedAnnotationPacketThroughNestedIdentityAndLetOwnersExpr =
  Surf.ELam
    "_generatedWrap1"
    ( Surf.EApp
        (Surf.ELam "_generatedWrap2" (Surf.EVar "_generatedWrap2"))
        ( Surf.ELam
            "_generatedWrap3"
            ( Surf.EApp
                ( Surf.ELamAnn
                    "_generatedWrap4"
                    (Surf.STBase "Int")
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELet
                            "_generatedWrap6"
                            (Surf.ELit (Surf.LInt (-1)))
                            ( Surf.EApp
                                ( Surf.ELam
                                    "_generatedWrap7"
                                    (Surf.EVar "_generatedWrap7")
                                )
                                mixedAnnotationExpr
                            )
                        )
                    )
                )
                (Surf.ELit (Surf.LInt 2))
            )
        )
    )

-- The fifth minimized seed-486053823 counterexample.  The exact @Int@
-- endpoint belongs to the innermost lambda body; the enclosing lambda packet
-- must construct its own arrow before generalization rather than treating the
-- body endpoint as the complete packet type.
groundAnnotationThroughNestedLetAndLambdaOwnersExpr :: Surf.SurfaceExpr
groundAnnotationThroughNestedLetAndLambdaOwnersExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-7)))
    ( Surf.EApp
        ( Surf.ELam
            "_generatedWrap2"
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt (-1)))
                    ( Surf.ELet
                        "_generatedWrap5"
                        (Surf.ELit (Surf.LInt 9))
                        ( Surf.EAnn
                            (Surf.ELit (Surf.LInt 1))
                            (Surf.STBase "Int")
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LBool False))
    )

-- The sixth minimized seed-486053823 counterexample.  Generalizing the
-- administrative lambda body must still carry the descendant @g g@ topology
-- consumer as an explicit term-used declaration for parent placement.
paperSelfApplicationThroughAdministrativeBodyTargetExpr :: Surf.SurfaceExpr
paperSelfApplicationThroughAdministrativeBodyTargetExpr =
  Surf.ELet
    "_generatedWrap1"
    (Surf.ELit (Surf.LInt (-11)))
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.ELam
                            "_generatedWrap5"
                            annotatedSelfAppExpr
                        )
                    )
                    (Surf.ELit (Surf.LBool True))
                )
            )
        )
        (Surf.ELit (Surf.LInt (-13)))
    )

-- The seventh minimized seed-486053823 counterexample.  The body-target
-- packet already contains the next lambda's forall/arrow spine; Var-Abs must
-- add the current certified parameter outside that complete body type.
groundAnnotationThroughAdministrativeLambdaSpineExpr :: Surf.SurfaceExpr
groundAnnotationThroughAdministrativeLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.ELet
        "_generatedWrap2"
        (Surf.ELit (Surf.LInt (-14)))
        ( Surf.EApp
            ( Surf.ELamAnn
                "_generatedWrap3"
                (Surf.STBase "Int")
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap6"
                                (Surf.STBase "Int")
                                ( Surf.ELet
                                    "_generatedWrap7"
                                    (Surf.ELit (Surf.LInt (-1)))
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 0))
                        )
                    )
                )
            )
            (Surf.ELit (Surf.LInt (-11)))
        )
    )
    (Surf.EVar "_generatedWrap1")

-- The eighth minimized seed-486053823 counterexample.  Applying the first
-- annotated parameter leaves several administrative lambdas around a bounded
-- higher-rank parameter.  Each Var-Abs step must rebuild exactly one outer
-- arrow without folding that parameter's lexical forall into the lambda spine.
boundedHigherRankParameterThroughAdministrativeLambdaSpineExpr :: Surf.SurfaceExpr
boundedHigherRankParameterThroughAdministrativeLambdaSpineExpr =
  Surf.ELet
    "_generatedWrap1"
    ( Surf.EAnn
        (Surf.ELit (Surf.LBool True))
        (Surf.STBase "Bool")
    )
    ( Surf.EApp
        ( Surf.ELamAnn
            "_generatedWrap2"
            (Surf.STBase "Int")
            ( Surf.ELet
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool False))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELam
                    "_generatedWrap4"
                    ( Surf.ELam
                        "_generatedWrap5"
                        ( Surf.ELamAnn
                            "_generatedSeedPoly"
                            ( Surf.STForall
                                "a"
                                Nothing
                                (Surf.STArrow (Surf.STVar "a") (Surf.STVar "a"))
                            )
                            ( Surf.EApp
                                (Surf.EVar "_generatedSeedPoly")
                                (Surf.ELit (Surf.LInt (-1)))
                            )
                        )
                    )
                )
            )
        )
        (Surf.ELit (Surf.LInt (-1)))
    )

-- The ninth minimized seed-486053823 counterexample.  The annotation's bound
-- has its own same-named lexical binder, distinct from the outer bounded
-- binder used by the body.  Application construction must transport those
-- identities atomically when the annotated lambda is itself an argument.
shadowedBoundedForallThroughApplicationArgumentExpr :: Surf.SurfaceExpr
shadowedBoundedForallThroughApplicationArgumentExpr =
  Surf.EApp
    ( Surf.ELam
        "_generatedWrap1"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap2"
                (Surf.EVar "_generatedWrap2")
            )
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EAnn
                    (Surf.ELam "x" (Surf.EVar "x"))
                    ( Surf.STForall
                        "a"
                        ( Just
                            ( Surf.SrcBound
                                ( Surf.STForall
                                    "a"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "a")
                                        (Surf.STVar "a")
                                    )
                                )
                            )
                        )
                        ( Surf.STArrow
                            (Surf.STVar "a")
                            (Surf.STVar "a")
                        )
                    )
                )
            )
        )
    )
    (Surf.ELit (Surf.LBool True))

-- Minimized seed-1195910434 counterexample.  The graph presentation reuses
-- the two source identities in dependency order, while the annotated value
-- constructs them in lexical order.  A direct identity application must use
-- the checked source endpoint; merely comparing the underlying identities
-- would silently permute the explicit forall ABI.
sourceForallOrderThroughDirectIdentityApplicationExpr :: Surf.SurfaceExpr
sourceForallOrderThroughDirectIdentityApplicationExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    (Surf.ELit (Surf.LInt 3))
                    ( Surf.EAnn
                        ( Surf.ELam
                            "x"
                            (Surf.ELam "y" (Surf.EVar "y"))
                        )
                        ( Surf.STForall
                            "alpha"
                            Nothing
                            ( Surf.STArrow
                                (Surf.STVar "beta")
                                ( Surf.STArrow
                                    (Surf.STVar "alpha")
                                    (Surf.STVar "alpha")
                                )
                            )
                        )
                    )
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 5))

-- The next minimized seed-1195910434 counterexample.  The applied unused
-- lambda turns the same lexical annotation into a descendant completion for
-- a future lambda owner.  Its checked completion must replace that owner's
-- provisional closure presentation before the enclosing identity is built.
sourceForallOrderThroughAppliedLambdaAndIdentityExpr :: Surf.SurfaceExpr
sourceForallOrderThroughAppliedLambdaAndIdentityExpr =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap1"
        (Surf.STBase "Int")
        ( Surf.ELet
            "_generatedWrap2"
            ( Surf.EApp
                ( Surf.ELam
                    "_generatedWrap3"
                    (Surf.EVar "_generatedWrap3")
                )
                ( Surf.ELet
                    "_generatedWrap4"
                    ( Surf.EApp
                        ( Surf.ELam
                            "_generatedWrap5"
                            ( Surf.EAnn
                                ( Surf.ELam
                                    "x"
                                    (Surf.ELam "y" (Surf.EVar "y"))
                                )
                                ( Surf.STForall
                                    "alpha"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "beta")
                                        ( Surf.STArrow
                                            (Surf.STVar "alpha")
                                            (Surf.STVar "alpha")
                                        )
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LBool True))
                    )
                    (Surf.EVar "_generatedWrap4")
                )
            )
            (Surf.EVar "_generatedWrap2")
        )
    )
    (Surf.ELit (Surf.LInt 4))

-- Minimized seed-1 counterexample.  The exact result supplied to each
-- applied unused lambda eventually reaches a bounded annotation beneath the
-- returned lambda.  Construction must emit that annotation's N computation
-- at the body boundary; an enclosing application cannot specialize beneath
-- an already-built arrow afterwards.
boundedForallThroughAppliedUnusedLambdaResultsExpr :: Surf.SurfaceExpr
boundedForallThroughAppliedUnusedLambdaResultsExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            ( Surf.ELam
                "_generatedWrap3"
                ( Surf.EApp
                    ( Surf.ELam
                        "_generatedWrap4"
                        ( Surf.EApp
                            ( Surf.ELamAnn
                                "_generatedWrap5"
                                (Surf.STBase "Int")
                                ( Surf.ELet
                                    "_generatedWrap6"
                                    ( Surf.EAnn
                                        (Surf.ELit (Surf.LBool True))
                                        (Surf.STBase "Bool")
                                    )
                                    ( Surf.ELam
                                        "_generatedWrap7"
                                        ( Surf.EAnn
                                            ( Surf.ELam
                                                "x"
                                                (Surf.EVar "x")
                                            )
                                            ( Surf.STForall
                                                "a"
                                                ( Just
                                                    ( Surf.SrcBound
                                                        ( Surf.STForall
                                                            "a"
                                                            Nothing
                                                            ( Surf.STArrow
                                                                (Surf.STVar "a")
                                                                (Surf.STVar "a")
                                                            )
                                                        )
                                                    )
                                                )
                                                ( Surf.STArrow
                                                    (Surf.STVar "a")
                                                    (Surf.STVar "a")
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                            (Surf.ELit (Surf.LInt 2))
                        )
                    )
                    (Surf.ELit (Surf.LBool False))
                )
            )
            (Surf.ELit (Surf.LBool True))
        )
    )

-- First minimized seed-2147483646 counterexample.  The identity argument has
-- a checked forall endpoint while the enclosing application still carries a
-- provisional result specialization.  Argument construction must retain the
-- principal endpoint and let the outgoing EApp construction specialize it.
identityArgumentPrincipalOverProvisionalResultExpr :: Surf.SurfaceExpr
identityArgumentPrincipalOverProvisionalResultExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELam
        "_generatedWrap2"
        ( Surf.EApp
            (Surf.ELam "_generatedWrap3" (Surf.EVar "_generatedWrap3"))
            ( Surf.ELet
                "_generatedWrap4"
                ( Surf.EAnn
                    (Surf.ELit (Surf.LBool True))
                    (Surf.STBase "Bool")
                )
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.EApp
                        ( Surf.ELamAnn
                            "_generatedWrap6"
                            (Surf.STBase "Int")
                            ( Surf.ELet
                                "_generatedWrap7"
                                (Surf.ELit (Surf.LInt 15))
                                ( Surf.EAnn
                                    ( Surf.ELam
                                        "_generatedSeedX"
                                        (Surf.EVar "_generatedSeedX")
                                    )
                                    ( Surf.STForall
                                        "a"
                                        Nothing
                                        ( Surf.STArrow
                                            (Surf.STVar "a")
                                            (Surf.STVar "a")
                                        )
                                    )
                                )
                            )
                        )
                        (Surf.ELit (Surf.LInt 0))
                    )
                )
            )
        )
    )

-- Second minimized seed-2147483646 counterexample.  The checked g g owner
-- publishes a result binder whose exact ambient bound is the higher-rank
-- parameter scheme.  Both the let publication and the enclosing let-Gamma
-- recheck must inherit that owner-final declaration before checking the
-- emitted InstAbstr computation.
paperSelfApplicationAmbientConstructionAtLetPublicationExpr
  :: Surf.SurfaceExpr
paperSelfApplicationAmbientConstructionAtLetPublicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    ( Surf.ELet
        "_generatedWrap2"
        ( Surf.EAnn
            (Surf.ELit (Surf.LBool False))
            (Surf.STBase "Bool")
        )
        ( Surf.ELet
            "_generatedWrap3"
            ( Surf.EAnn
                (Surf.ELit (Surf.LBool False))
                (Surf.STBase "Bool")
            )
            ( Surf.EApp
                (Surf.ELam "_generatedWrap4" (Surf.EVar "_generatedWrap4"))
                ( Surf.ELam
                    "_generatedWrap5"
                    ( Surf.ELam
                        "_generatedWrap6"
                        ( Surf.ELet
                            "_generatedWrap7"
                            ( Surf.ELamAnn
                                "g"
                                ( Surf.STForall
                                    "a"
                                    Nothing
                                    ( Surf.STArrow
                                        (Surf.STVar "a")
                                        (Surf.STVar "a")
                                    )
                                )
                                (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))
                            )
                            (Surf.EVar "_generatedWrap7")
                        )
                    )
                )
            )
        )
    )

annotatedIdentityAppliedMultiUseAnnotation :: Surf.SurfaceExpr
annotatedIdentityAppliedMultiUseAnnotation =
  Surf.EApp
    ( Surf.ELamAnn
        "_generatedWrap6"
        (Surf.STBase "Int")
        identityAppliedMultiUseAnnotation
    )
    (Surf.ELit (Surf.LInt 4))

multiUseAnnotatedIdentitySeed :: Surf.SurfaceExpr
multiUseAnnotatedIdentitySeed =
  multiUseAnnotatedIdentitySeedWith 3 False

multiUseAnnotatedIdentitySeedWith
  :: Integer
  -> Bool
  -> Surf.SurfaceExpr
multiUseAnnotatedIdentitySeedWith intValue boolValue =
  Surf.ELet
    "_generatedSeedId"
    ( Surf.EAnn
        ( Surf.ELam
            "_generatedSeedArg"
            (Surf.EVar "_generatedSeedArg")
        )
        sigmaIdSource
    )
    ( Surf.ELet
        "_generatedSeedDiscard"
        ( Surf.EApp
            (Surf.EVar "_generatedSeedId")
            (Surf.ELit (Surf.LInt intValue))
        )
        ( Surf.EApp
            (Surf.EVar "_generatedSeedId")
            (Surf.ELit (Surf.LBool boolValue))
        )
    )

roundTripMultiUseAnnotation :: Surf.SurfaceExpr -> Surf.SurfaceExpr
roundTripMultiUseAnnotation inner =
  Surf.ELet "_generatedWrap5" inner (Surf.EVar "_generatedWrap5")

unusedAppliedMultiUseAnnotation :: Surf.SurfaceExpr -> Surf.SurfaceExpr
unusedAppliedMultiUseAnnotation inner =
  Surf.EApp
    (Surf.ELam "_generatedWrap4" inner)
    (Surf.ELit (Surf.LBool False))

mixedAnnotationType :: Elab.ElabType
mixedAnnotationType =
  testTForall
    "beta"
    Nothing
    ( testTForall
        "alpha"
        Nothing
        ( Elab.TArrow
            (testTVar "beta")
            (Elab.TArrow (testTVar "alpha") (testTVar "alpha"))
        )
    )

expectBoundedIdentityAnnotationShape :: Elab.XmlfTerm -> Expectation
expectBoundedIdentityAnnotationShape term =
  case term of
    Elab.ETyAbsRef resultRef (Just resultBound) (Elab.ELam binder (Elab.EVarNode occurrence)) -> do
      ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
      expectIdentityLambdaAt resultRef binder occurrence
    Elab.ETyAbsRef resultRef (Just resultBound)
      ( Elab.ETyInst
          (Elab.ETyAbsRef sourceRef Nothing (Elab.ELam binder (Elab.EVarNode occurrence)))
          (Elab.InstApp (Elab.TVarRef instantiatedRef))
        ) -> do
        ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
        ElabTypes.typeBinderRefsSameIdentity resultRef instantiatedRef `shouldBe` True
        expectIdentityLambdaAt sourceRef binder occurrence
    _ -> expectationFailure ("expected direct bounded type abstraction, got " ++ show term)
  where
    expectIdentityLambdaAt expectedRef binder occurrence = do
      case ElabTypes.resolvedVarType binder of
        Elab.TVarRef binderRef ->
          ElabTypes.typeBinderRefsSameIdentity expectedRef binderRef `shouldBe` True
        _ -> expectationFailure "bounded annotation lambda does not use its quantified carrier"
      ElabTypes.resolvedVarDetails occurrence
        `shouldBe` ElabTypes.resolvedVarDetails binder
      ElabTypes.resolvedVarType occurrence
        `shouldMatchType` ElabTypes.resolvedVarType binder

annotatedSelfAppExpr :: Surf.SurfaceExpr
annotatedSelfAppExpr =
  Surf.ELamAnn
    "g"
    sigmaIdSource
    (Surf.EApp (Surf.EVar "g") (Surf.EVar "g"))

annotatedSelfAppThroughDirectIdentityApplicationExpr :: Surf.SurfaceExpr
annotatedSelfAppThroughDirectIdentityApplicationExpr =
  Surf.EApp
    (Surf.ELam "_generatedWrap1" (Surf.EVar "_generatedWrap1"))
    annotatedSelfAppExpr

annotatedSelfAppType :: Elab.ElabType
annotatedSelfAppType =
  testTForall
    "result"
    (Just (boundFromType polyIdTy))
    (Elab.TArrow polyIdTy (testTVar "result"))

expectAnnotatedSelfAppShape :: Elab.XmlfTerm -> Expectation
expectAnnotatedSelfAppShape term =
  case term of
    Elab.ETyAbsRef resultRef (Just resultBound) (Elab.ELam binder body) ->
      case body of
        Elab.ETyInst
          ( Elab.EApp
              (Elab.ETyInst (Elab.EVarNode funVar) (Elab.InstApp funArgTy))
              (Elab.EVarNode argVar)
            )
          (Elab.InstAbstrRef abstractedRef) -> do
            ElabTypes.tyToElab resultBound `shouldMatchType` polyIdTy
            funArgTy `shouldMatchType` polyIdTy
            case
                ( ElabTypes.tyToElab resultBound,
                  ElabTypes.resolvedVarType binder
                )
              of
                (Elab.TForallRef boundRef _ _, Elab.TForallRef parameterRef _ _) ->
                  ElabTypes.typeBinderRefsSameIdentity boundRef parameterRef
                    `shouldBe` False
                types ->
                  expectationFailure
                    ( "expected sigma-id declarations in the result bound and lambda parameter, got "
                        ++ show types
                    )
            ElabTypes.typeBinderRefsSameIdentity resultRef abstractedRef `shouldBe` True
            ElabTypes.resolvedVarDetails funVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
            ElabTypes.resolvedVarDetails argVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
        _ -> expectationFailure ("unexpected annotated self-application body: " ++ show body)
    _ -> expectationFailure ("unexpected annotated self-application outer form: " ++ show term)

propEnvLambda :: Int -> Property
propEnvLambda _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (term, ty) ->
      case findLambdaBindingEvidence "x" term of
        Just (binder, occurrence) ->
          conjoin
            [ ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails binder)
                === ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails occurrence),
              typeShouldMatch
                (ElabTypes.resolvedVarType binder)
                (ElabTypes.resolvedVarType occurrence),
              typeCheckShouldMatch (Elab.typeCheck term) ty
            ]
        Nothing -> counterexample ("missing live lambda binding evidence: " ++ show term) False
    Left err -> counterexample (Elab.renderPipelineError err) False

propEnvLet :: Int -> Property
propEnvLet _size =
  let expr = Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x")) (Surf.EVar "id")
   in case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Right (term, ty) ->
          case findLetBindingEvidence "id" term of
            Just (binder, scheme, occurrence) ->
              let schemeTy = Elab.schemeToType scheme
               in conjoin
                    [ ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails binder)
                        === ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails occurrence),
                      typeShouldMatch schemeTy (ElabTypes.resolvedVarType binder),
                      typeShouldMatch schemeTy (ElabTypes.resolvedVarType occurrence),
                      typeCheckShouldMatch (Elab.typeCheck term) ty
                    ]
            Nothing -> counterexample ("missing live let binding evidence: " ++ show term) False
        Left err -> counterexample (Elab.renderPipelineError err) False

propEnvWf :: Int -> Property
propEnvWf _size =
  conjoin [propEnvLambda 0, propEnvLet 0]

propTrSeqEmpty :: Int -> Property
propTrSeqEmpty _size =
  propSigmaReorderIdentity 0

propTrSeqCons :: Int -> Property
propTrSeqCons _size =
  propSigmaReorderRequired 0

propTrRigidRaise :: Int -> Property
propTrRigidRaise _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsForTest env [OpRaise (NodeId 2)] === Right []

propTrRigidMerge :: Int -> Property
propTrRigidMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsForTest env [OpMerge (NodeId 2) (NodeId 3)] === Right []

propTrRigidRaiseMerge :: Int -> Property
propTrRigidRaiseMerge _size =
  let env = mkNormalizeEnv mkNormalizeConstraint (NodeId 0) IntSet.empty
   in normalizeInstanceOpsForTest env [OpRaiseMerge (NodeId 2) (NodeId 3)] === Right []

propTrRootGraft :: Int -> Property
propTrRootGraft _size =
  let root = NodeId 0
      arg = NodeId 1
      c =
        rootedConstraint
          emptyConstraint
            { cNodes = nodeMapFromList [(0, TyArrow root arg arg), (1, TyVar {tnId = arg, tnBound = Nothing})],
              cBindParents = bindParentsFromPairs [(arg, root, BindFlex)]
            }
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId arg])
   in validateNormalizedWitness env [OpGraft arg root] === Right ()

propTrRootRaiseMerge :: Int -> Property
propTrRootRaiseMerge _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      m = NodeId 3
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in coalesceRaiseMergeWithEnv env [OpRaise n, OpMerge n m] === Right [OpRaiseMerge n m]

propTrRootWeaken :: Int -> Property
propTrRootWeaken _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in normalizeInstanceOpsForTest env [OpWeaken n] === Right [OpWeaken n]

propTrNodeGraft :: Int -> Property
propTrNodeGraft _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      binder = NodeId 2
      arg = NodeId 3
      env =
        (mkNormalizeEnv c root (IntSet.fromList [getNodeId binder]))
          { binderArgs = IntMap.fromList [(getNodeId binder, arg)],
            binderReplayMap = IntMap.fromList [(getNodeId binder, binder)],
            replayContract = ReplayContractStrict
          }
   in normalizeInstanceOpsForTest env [OpGraft arg binder, OpWeaken binder] === Right [OpGraft arg binder, OpWeaken binder]

propTrNodeMerge :: Int -> Property
propTrNodeMerge size =
  assertNodeAliasTranslation size OpMerge

propTrNodeRaiseMerge :: Int -> Property
propTrNodeRaiseMerge size =
  assertNodeAliasTranslation size OpRaiseMerge

propTrNodeWeaken :: Int -> Property
propTrNodeWeaken _size =
  let root = NodeId 0
      parent = NodeId 1
      child = NodeId 2
      sibling = NodeId 3
      nodes =
        nodeMapFromList
          [ (0, TyForall root parent),
            (1, TyForall parent child),
            (2, TyVar {tnId = child, tnBound = Nothing}),
            (3, TyVar {tnId = sibling, tnBound = Nothing})
          ]
      c =
        rootedConstraint
          emptyConstraint
            { cNodes = nodes,
              cBindParents =
                bindParentsFromPairs
                  [ (parent, root, BindFlex),
                    (child, parent, BindFlex),
                    (sibling, root, BindFlex)
                  ]
            }
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child, getNodeId sibling])
   in reorderWeakenWithEnv env [OpWeaken parent, OpGraft child child] === Right [OpGraft child child, OpWeaken parent]

propTrNodeRaise :: Int -> Property
propTrNodeRaise _size =
  let c = mkNormalizeConstraint
      root = NodeId 0
      n = NodeId 2
      env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
   in validateNormalizedWitness env [OpRaise n] === Right ()

propReifyType :: Int -> Property
propReifyType _size =
  elaboratesTo (Surf.ELit (Surf.LInt 1)) intTy

propReifyNames :: Int -> Property
propReifyNames _size =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr (Surf.ELam "x" (Surf.EVar "x"))) of
    Right (_term, Elab.TForallRef _ Nothing (Elab.TArrow dom cod)) -> counterexample (show (dom, cod)) (dom == cod)
    other -> counterexample (show other) False

propBindMono :: Int -> Property
propBindMono _size =
  case runPipelineArtifactsDefault Set.empty (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) of
    Right PipelineArtifacts {paPresolution = presolution} ->
      Binding.checkBindingTree (prConstraint presolution) === Right ()
    Left err -> counterexample err False

propSynToGraph :: Int -> Property
propSynToGraph size =
  forAll (genMixedAnnotation size) $ \annotation ->
    counterexample ("generated annotation: " ++ show annotation) $
      conjoin
        [ checkSynToGraph annotation,
          checkSynToGraph
            (Surf.STVar ("bare-existential-" ++ show size)),
          checkSynToGraph graphNormalizedEqVarAnnotation
        ]

graphNormalizedEqVarAnnotation :: Surf.SrcType
graphNormalizedEqVarAnnotation =
  Surf.STForall
    "graph-root-a"
    (Just (Surf.mkSrcBound (Surf.STBase "Int")))
    (Surf.STForall "graph-root-unused" Nothing (Surf.STVar "graph-root-a"))

data AnnotationBinderKind
  = AnnotationForallBinder
  | AnnotationMuBinder
  deriving (Eq, Show)

data CoercionCopyEvidence = CoercionCopyEvidence
  { coercionEvidenceFreeNodes :: Map.Map String NodeId,
    coercionEvidenceDomainOwned :: IntSet.IntSet,
    coercionEvidenceCodomainOwned :: IntSet.IntSet
  }
  deriving (Eq, Show)

emptyCoercionCopyEvidence :: CoercionCopyEvidence
emptyCoercionCopyEvidence =
  CoercionCopyEvidence
    { coercionEvidenceFreeNodes = Map.empty,
      coercionEvidenceDomainOwned = IntSet.empty,
      coercionEvidenceCodomainOwned = IntSet.empty
    }

checkSynToGraph :: Surf.SrcType -> Property
checkSynToGraph annotation =
  let binderIdentities = annotationBinderIdentities annotation
   in case runAnnotationConstraint binderIdentities annotation of
        Right result@ConstraintResult {crConstraint = c, crRoot = codomainRoot} ->
          case cInstEdges c of
            [InstEdge _ _ destination] ->
              case lookupNodeIn (cNodes c) destination of
                Just TyVar {tnBound = Just domainRoot} ->
                  conjoin
                    [ case
                        validateAnnotationCopies
                          binderIdentities
                          result
                          annotation
                          domainRoot
                          codomainRoot
                      of
                        Right () -> property True
                        Left err -> counterexample err False,
                      counterexample
                        "annotation source authority was not recorded at the codomain"
                        ( IntMap.member
                            (getNodeId codomainRoot)
                            (crAnnSourceTypes result)
                        ),
                      Binding.checkBindingTree c === Right ()
                    ]
                other ->
                  counterexample
                    ("annotation edge destination did not retain its domain: " ++ show other)
                    False
            edges ->
              counterexample
                ("expected one annotation edge, saw " ++ show edges)
                False
        Left err -> counterexample (show err) False

runAnnotationConstraint ::
  Map.Map String TypeBinderIdentity ->
  Surf.SrcType ->
  Either ConstraintError (ConstraintResult 'Raw)
runAnnotationConstraint binderIdentities annotation =
  generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
    initialIdentityGenerator
    Set.empty
    (Builtins.builtinSourceTypeHeadIdentities annotation)
    binderIdentities
    Map.empty
    ( unsafeNormalizeExpr
        (Surf.EAnn (Surf.ELit (Surf.LInt 1)) annotation)
    )

annotationBinderIdentities :: Surf.SrcType -> Map.Map String TypeBinderIdentity
annotationBinderIdentities annotation =
  Map.fromList
    [ (name, binderIdentity index binderKind)
      | (index, (name, binderKind)) <-
          zip [0 :: Int ..] (annotationBinders annotation)
    ]
  where
    binderIdentity index binderKind =
      let unique = UniqueIdentity (991900000 + index)
       in case binderKind of
            AnnotationForallBinder ->
              typeBinderIdentityFromUnique unique
            AnnotationMuBinder ->
              typeBinderIdentityFromStructural unique StructuralSelfBinder

annotationBinders :: Surf.SrcTy n v -> [(String, AnnotationBinderKind)]
annotationBinders sourceType =
  case sourceType of
    Surf.STVar _ -> []
    Surf.STArrow dom cod ->
      annotationBinders dom ++ annotationBinders cod
    Surf.STBase _ -> []
    Surf.STCon _ args ->
      foldMap annotationBinders args
    Surf.STVarApp _ args ->
      foldMap annotationBinders args
    Surf.STTyLam name body ->
      (name, AnnotationForallBinder) : annotationBinders body
    Surf.STTyApp fun arg ->
      annotationBinders fun ++ annotationBinders arg
    Surf.STForall name mbBound body ->
      (name, AnnotationForallBinder)
        : maybe [] (annotationBinders . Surf.unSrcBound) mbBound
          ++ annotationBinders body
    Surf.STMu name body ->
      (name, AnnotationMuBinder) : annotationBinders body
    Surf.STBottom -> []

annotationGraphicFreeVars :: Surf.SrcTy n v -> Set.Set String
annotationGraphicFreeVars sourceType =
  case sourceType of
    Surf.STVar name -> Set.singleton name
    Surf.STArrow dom cod ->
      annotationGraphicFreeVars dom <> annotationGraphicFreeVars cod
    Surf.STBase _ -> Set.empty
    Surf.STCon _ args ->
      foldMap annotationGraphicFreeVars args
    Surf.STVarApp name args ->
      Set.insert name (foldMap annotationGraphicFreeVars args)
    Surf.STTyLam name body ->
      Set.delete name (annotationGraphicFreeVars body)
    Surf.STTyApp fun arg ->
      annotationGraphicFreeVars fun <> annotationGraphicFreeVars arg
    Surf.STForall name mbBound body ->
      let bodyFree = annotationGraphicFreeVars body
       in if Set.member name bodyFree
            then
              Set.delete name bodyFree
                <> maybe
                  Set.empty
                  (annotationGraphicFreeVars . Surf.unSrcBound)
                  mbBound
            else bodyFree
    Surf.STMu name body ->
      Set.delete name (annotationGraphicFreeVars body)
    Surf.STBottom -> Set.empty

annotationGraphicRootVariable :: Surf.SrcTy n v -> Maybe String
annotationGraphicRootVariable sourceType =
  case sourceType of
    Surf.STVar name -> Just name
    Surf.STForall name mbBound body ->
      let bodyRoot = annotationGraphicRootVariable body
       in if bodyRoot == Just name
            then
              mbBound
                >>= annotationGraphicRootVariable . Surf.unSrcBound
            else
              if Set.notMember name (annotationGraphicFreeVars body)
                then bodyRoot
                else Nothing
    _ -> Nothing

genMixedAnnotation :: Int -> Gen Surf.SrcType
genMixedAnnotation requestedSize = do
  salt <- chooseInt (0, 1000000)
  let existential = annotationName "existential" salt [0]
      constructorHead = annotationName "constructor" salt [0]
      boundExistential = annotationName "bound-existential" salt [0]
      universal = annotationName "forall" salt [0]
      recursive = annotationName "mu" salt [0]
      freeNames = [existential, constructorHead, boundExistential]
      structuralBound =
        Surf.STArrow
          (Surf.STVar boundExistential)
          (Surf.STBase "Bool")
      seed =
        Surf.STForall
          universal
          (Just (Surf.mkSrcBound structuralBound))
          ( Surf.STArrow
              (Surf.STCon "List" (Surf.STVar existential :| []))
              ( Surf.STArrow
                  ( Surf.STVarApp
                      constructorHead
                      (Surf.STVar existential :| [Surf.STBase "Int"])
                  )
                  ( Surf.STMu
                      recursive
                      ( Surf.STArrow
                          (Surf.STVar recursive)
                          ( Surf.STArrow
                              (Surf.STVar universal)
                              Surf.STBottom
                          )
                      )
                  )
              )
          )
      depth = max 1 (min 4 (requestedSize `div` 4))
  growAnnotation freeNames [] salt [1] depth seed

annotationName :: String -> Int -> [Int] -> String
annotationName prefix salt path =
  prefix
    ++ "-"
    ++ show salt
    ++ concatMap (("-" ++) . show) path

growAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Surf.SrcType ->
  Gen Surf.SrcType
growAnnotation freeNames boundNames salt path depth seedType
  | depth <= 0 = pure seedType
  | otherwise =
      frequency
        [ (2, pure seedType),
          (4, do
              sibling <-
                genAnnotation
                  freeNames
                  boundNames
                  salt
                  (0 : path)
                  (depth - 1)
              wrapped <-
                elements
                  [ Surf.STArrow seedType sibling,
                    Surf.STArrow sibling seedType
                  ]
              growAnnotation
                freeNames
                boundNames
                salt
                (1 : path)
                (depth - 1)
                wrapped
          ),
          (2,
            growAnnotation
              freeNames
              boundNames
              salt
              (2 : path)
              (depth - 1)
              (Surf.STCon "List" (seedType :| []))
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              sibling <-
                genAnnotation
                  freeNames
                  boundNames
                  salt
                  (3 : path)
                  (depth - 1)
              growAnnotation
                freeNames
                boundNames
                salt
                (4 : path)
                (depth - 1)
                (Surf.STVarApp headName (seedType :| [sibling]))
          ),
          (3, do
              let binder = annotationName "forall" salt (5 : path)
              mbBound <-
                frequency
                  [ (2, pure Nothing),
                    (1,
                      Just . Surf.mkSrcBound
                        <$> genStructuralAnnotation
                          freeNames
                          boundNames
                          salt
                          (6 : path)
                          (depth - 1)
                    )
                  ]
              growAnnotation
                freeNames
                boundNames
                salt
                (7 : path)
                (depth - 1)
                ( Surf.STForall
                    binder
                    mbBound
                    (Surf.STArrow seedType (Surf.STVar binder))
                )
          ),
          (2,
            let binder = annotationName "mu" salt (8 : path)
             in growAnnotation
                  freeNames
                  boundNames
                  salt
                  (9 : path)
                  (depth - 1)
                  ( Surf.STMu
                      binder
                      (Surf.STArrow (Surf.STVar binder) seedType)
                  )
          )
        ]

genAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Gen Surf.SrcType
genAnnotation freeNames boundNames salt path depth
  | depth <= 0 =
      genAnnotationLeaf freeNames boundNames
  | otherwise =
      frequency
        [ (4, genAnnotationLeaf freeNames boundNames),
          (5,
            Surf.STArrow
              <$> recurse 0
              <*> recurse 1
          ),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> recurse 2
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              firstArg <- recurse 3
              restArgs <-
                frequency
                  [ (2, pure []),
                    (1, (: []) <$> recurse 4)
                  ]
              pure (Surf.STVarApp headName (firstArg :| restArgs))
          ),
          (3, do
              let binder = annotationName "forall" salt (5 : path)
              mbBound <-
                frequency
                  [ (2, pure Nothing),
                    (1,
                      Just . Surf.mkSrcBound
                        <$> genStructuralAnnotation
                          freeNames
                          boundNames
                          salt
                          (6 : path)
                          (depth - 1)
                    )
                  ]
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (7 : path)
                  (depth - 1)
              pure (Surf.STForall binder mbBound body)
          ),
          (2, do
              let binder = annotationName "mu" salt (8 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (9 : path)
                  (depth - 1)
              pure (Surf.STMu binder body)
          )
        ]
  where
    recurse tag =
      genAnnotation
        freeNames
        boundNames
        salt
        (tag : path)
        (depth - 1)

genAnnotationLeaf :: [String] -> [String] -> Gen Surf.SrcType
genAnnotationLeaf freeNames boundNames =
  frequency
    [ (4, Surf.STVar <$> elements (freeNames ++ boundNames)),
      (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
      (1, pure Surf.STBottom)
    ]

genStructuralAnnotation ::
  [String] ->
  [String] ->
  Int ->
  [Int] ->
  Int ->
  Gen Surf.SrcType
genStructuralAnnotation freeNames boundNames salt path depth
  | depth <= 0 =
      frequency
        [ (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> genAnnotationLeaf freeNames boundNames
          ),
          (1, pure Surf.STBottom)
        ]
  | otherwise =
      frequency
        [ (3, Surf.STBase <$> elements ["Int", "Bool", "String"]),
          (4,
            Surf.STArrow
              <$> recurse 0
              <*> recurse 1
          ),
          (2,
            (\arg -> Surf.STCon "List" (arg :| []))
              <$> recurse 2
          ),
          (2, do
              headName <- elements (freeNames ++ boundNames)
              firstArg <- recurse 3
              pure (Surf.STVarApp headName (firstArg :| []))
          ),
          (2, do
              let binder = annotationName "forall" salt (4 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (5 : path)
                  (depth - 1)
              pure (Surf.STForall binder Nothing body)
          ),
          (1, do
              let binder = annotationName "mu" salt (6 : path)
              body <-
                genAnnotation
                  freeNames
                  (binder : boundNames)
                  salt
                  (7 : path)
                  (depth - 1)
              pure (Surf.STMu binder body)
          ),
          (1, pure Surf.STBottom)
        ]
  where
    recurse tag =
      genAnnotation
        freeNames
        boundNames
        salt
        (tag : path)
        (depth - 1)

validateAnnotationCopies ::
  Map.Map String TypeBinderIdentity ->
  ConstraintResult 'Raw ->
  Surf.SrcType ->
  NodeId ->
  NodeId ->
  Either String ()
validateAnnotationCopies binderIdentities result annotation domainRoot codomainRoot = do
  evidence <-
    go Map.empty Map.empty annotation domainRoot codomainRoot
  let domainOwned = coercionEvidenceDomainOwned evidence
      codomainOwned = coercionEvidenceCodomainOwned evidence
  if IntSet.member (getNodeId domainRoot) domainOwned
    then
      expectNodeKind
        "rigid domain root"
        domainRoot
        Binding.NodeRestricted
    else Right ()
  if IntSet.member (getNodeId codomainRoot) codomainOwned
    then
      expectNodeKind
        "flexible codomain root"
        codomainRoot
        Binding.NodeInstantiable
    else Right ()
  requireEvidence
    (IntSet.null (IntSet.intersection domainOwned codomainOwned))
    ( "copy-owned nodes were shared between domain and codomain: "
        ++ show (IntSet.toList (IntSet.intersection domainOwned codomainOwned))
    )
  forM_ (Map.toList (coercionEvidenceFreeNodes evidence)) $ \(name, node) -> do
    requireEvidence
      (not (IntSet.member (getNodeId node) domainOwned))
      ("free node was owned by the rigid copy: " ++ name)
    requireEvidence
      (not (IntSet.member (getNodeId node) codomainOwned))
      ("free node was owned by the flexible copy: " ++ name)
    expectNodeKind
      ("shared existential " ++ name)
      node
      Binding.NodeInstantiable
  case Binding.checkBindingTree constraint of
    Right () -> Right ()
    Left err ->
      Left
        ( "invalid binding tree: "
            ++ show err
            ++ bindingErrorContext err
        )
  where
    constraint = crConstraint result
    nodes = cNodes constraint

    bindingErrorContext (ParentNotUpper (TypeRef child) (TypeRef parent)) =
      "; child="
        ++ show (nodeContext child)
        ++ "; parent="
        ++ show (nodeContext parent)
    bindingErrorContext _ = ""

    nodeContext node =
      ( lookupNodeIn nodes node,
        IntMap.lookup
          (nodeRefKey (typeRef node))
          (cBindParents constraint),
        [ parent
          | (parent, parentNode) <- toListNode nodes,
            node `elem` structuralChildrenWithBounds parentNode
        ]
      )

    go ::
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      Surf.SrcTy n v ->
      NodeId ->
      NodeId ->
      Either String CoercionCopyEvidence
    go domainEnv codomainEnv sourceType domainNode codomainNode =
      case sourceType of
        Surf.STVar name ->
          validateVariable
            domainEnv
            codomainEnv
            name
            domainNode
            codomainNode
        Surf.STArrow sourceDomain sourceCodomain ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyArrow {tnDom = domainDom, tnCod = domainCod},
                Just TyArrow {tnDom = codomainDom, tnCod = codomainCod}
                ) -> do
                  rootEvidence <-
                    ownedNodeEvidence "arrow" domainNode codomainNode
                  domEvidence <-
                    go
                      domainEnv
                      codomainEnv
                      sourceDomain
                      domainDom
                      codomainDom
                  codEvidence <-
                    go
                      domainEnv
                      codomainEnv
                      sourceCodomain
                      domainCod
                      codomainCod
                  mergeEvidenceList [rootEvidence, domEvidence, codEvidence]
              pair ->
                Left ("arrow copies did not match source shape: " ++ show pair)
        Surf.STBase expectedName ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyBase {tnBase = BaseTy domainName},
                Just TyBase {tnBase = BaseTy codomainName}
                )
                  | domainName == expectedName
                      && codomainName == expectedName ->
                      ownedNodeEvidence "base" domainNode codomainNode
              pair ->
                Left
                  ( "base copies did not match "
                      ++ expectedName
                      ++ ": "
                      ++ show pair
                  )
        Surf.STCon expectedName sourceArgs ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyCon {tnCon = BaseTy domainName, tnArgs = domainArgs},
                Just TyCon {tnCon = BaseTy codomainName, tnArgs = codomainArgs}
                )
                  | domainName == expectedName
                      && codomainName == expectedName -> do
                      rootEvidence <-
                        ownedNodeEvidence "constructor" domainNode codomainNode
                      argsEvidence <-
                        validateChildren
                          domainEnv
                          codomainEnv
                          (NE.toList sourceArgs)
                          (NE.toList domainArgs)
                          (NE.toList codomainArgs)
                      mergeEvidence rootEvidence argsEvidence
              pair ->
                Left
                  ( "constructor copies did not match "
                      ++ expectedName
                      ++ ": "
                      ++ show pair
                  )
        Surf.STVarApp headName sourceArgs ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyVarApp {tnVarHead = domainHead, tnArgs = domainArgs},
                Just TyVarApp {tnVarHead = codomainHead, tnArgs = codomainArgs}
                ) -> do
                  rootEvidence <-
                    ownedNodeEvidence
                      "variable-headed application"
                      domainNode
                      codomainNode
                  headEvidence <-
                    validateVariable
                      domainEnv
                      codomainEnv
                      headName
                      domainHead
                      codomainHead
                  argsEvidence <-
                    validateChildren
                      domainEnv
                      codomainEnv
                      (NE.toList sourceArgs)
                      (NE.toList domainArgs)
                      (NE.toList codomainArgs)
                  mergeEvidenceList
                    [rootEvidence, headEvidence, argsEvidence]
              pair ->
                Left
                  ( "variable-headed application copies did not match source shape: "
                      ++ show pair
                  )
        Surf.STTyLam {} ->
          Left "residual type lambda reached the O08 graph oracle"
        Surf.STTyApp {} ->
          Left "residual type application reached the O08 graph oracle"
        Surf.STForall name mbSourceBound sourceBody
          | annotationGraphicRootVariable sourceBody == Just name ->
              case mbSourceBound of
                Nothing ->
                  go
                    domainEnv
                    codomainEnv
                    Surf.STBottom
                    domainNode
                    codomainNode
                Just sourceBound ->
                  go
                    domainEnv
                    codomainEnv
                    (Surf.unSrcBound sourceBound)
                    domainNode
                    codomainNode
          | Set.notMember name (annotationGraphicFreeVars sourceBody) ->
              go
                domainEnv
                codomainEnv
                sourceBody
                domainNode
                codomainNode
          | otherwise ->
            case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyForall {tnBody = domainBody},
                Just TyForall {tnBody = codomainBody}
                ) -> do
                  identity <-
                    case Map.lookup name binderIdentities of
                      Just found -> Right found
                      Nothing ->
                        Left
                          ("missing generated identity for forall binder " ++ name)
                  domainBinder <-
                    findLexicalBinder
                      identity
                      domainNode
                      BindRigid
                  codomainBinder <-
                    findLexicalBinder
                      identity
                      codomainNode
                      BindFlex
                  ownerEvidence <-
                    ownedNodeEvidence "forall owner" domainNode codomainNode
                  binderEvidence <-
                    ownedNodeEvidence
                      ("forall binder " ++ name)
                      domainBinder
                      codomainBinder
                  boundEvidence <-
                    validateBound
                      domainEnv
                      codomainEnv
                      mbSourceBound
                      domainBinder
                      codomainBinder
                  bodyEvidence <-
                    go
                      (Map.insert name domainBinder domainEnv)
                      (Map.insert name codomainBinder codomainEnv)
                      sourceBody
                      domainBody
                      codomainBody
                  mergeEvidenceList
                    [ ownerEvidence,
                      binderEvidence,
                      boundEvidence,
                      bodyEvidence
                    ]
              pair ->
                Left
                  ("forall copies did not match source shape: " ++ show pair)
        Surf.STMu name sourceBody ->
          case
            ( lookupNodeIn nodes domainNode,
              lookupNodeIn nodes codomainNode
            )
          of
              ( Just TyMu {tnBody = domainBody},
                Just TyMu {tnBody = codomainBody}
                ) -> do
                  identity <-
                    case Map.lookup name binderIdentities of
                      Just found -> Right found
                      Nothing ->
                        Left
                          ("missing generated identity for mu binder " ++ name)
                  ownerEvidence <-
                    ownedNodeEvidence "mu owner" domainNode codomainNode
                  expectSourceIdentity "domain mu owner" identity domainNode
                  expectSourceIdentity "codomain mu owner" identity codomainNode
                  if Set.member name (annotationGraphicFreeVars sourceBody)
                    then do
                      domainBinder <-
                        findLexicalBinder
                          identity
                          domainNode
                          BindRigid
                      codomainBinder <-
                        findLexicalBinder
                          identity
                          codomainNode
                          BindFlex
                      binderEvidence <-
                        ownedNodeEvidence
                          ("mu binder " ++ name)
                          domainBinder
                          codomainBinder
                      bodyEvidence <-
                        go
                          (Map.insert name domainBinder domainEnv)
                          (Map.insert name codomainBinder codomainEnv)
                          sourceBody
                          domainBody
                          codomainBody
                      mergeEvidenceList
                        [ownerEvidence, binderEvidence, bodyEvidence]
                    else do
                      domainBinder <-
                        findGenOwnedLexicalBinder identity BindRigid
                      codomainBinder <-
                        findGenOwnedLexicalBinder identity BindFlex
                      binderEvidence <-
                        ownedNodeEvidence
                          ("vacuous mu binder " ++ name)
                          domainBinder
                          codomainBinder
                      bodyEvidence <-
                        go
                          domainEnv
                          codomainEnv
                          sourceBody
                          domainBody
                          codomainBody
                      mergeEvidenceList
                        [ownerEvidence, binderEvidence, bodyEvidence]
              pair ->
                Left ("mu copies did not match source shape: " ++ show pair)
        Surf.STBottom ->
          case
              ( lookupNodeIn nodes domainNode,
                lookupNodeIn nodes codomainNode
              )
            of
              ( Just TyVar {tnBound = Nothing},
                Just TyVar {tnBound = Nothing}
                ) ->
                  ownedNodeEvidence "bottom" domainNode codomainNode
              pair ->
                Left ("bottom copies did not match source shape: " ++ show pair)

    validateChildren ::
      forall n.
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      [Surf.SrcTy n 'Surf.TopVarAllowed] ->
      [NodeId] ->
      [NodeId] ->
      Either String CoercionCopyEvidence
    validateChildren domainEnv codomainEnv sourceChildren domainChildren codomainChildren = do
      requireEvidence
        ( length sourceChildren == length domainChildren
            && length sourceChildren == length codomainChildren
        )
        ( "copy child arity mismatch: "
            ++ show
              ( length sourceChildren,
                length domainChildren,
                length codomainChildren
              )
        )
      mergeEvidenceList
        =<< sequence
          [ go
              domainEnv
              codomainEnv
              sourceChild
              domainChild
              codomainChild
            | (sourceChild, domainChild, codomainChild) <-
                zip3 sourceChildren domainChildren codomainChildren
          ]

    validateVariable domainEnv codomainEnv name domainNode codomainNode =
      case (Map.lookup name domainEnv, Map.lookup name codomainEnv) of
        (Just expectedDomain, Just expectedCodomain) -> do
          requireEvidence
            (domainNode == expectedDomain)
            ( "domain occurrence of "
                ++ name
                ++ " did not use its lexical binder: "
                ++ show (domainNode, expectedDomain)
            )
          requireEvidence
            (codomainNode == expectedCodomain)
            ( "codomain occurrence of "
                ++ name
                ++ " did not use its lexical binder: "
                ++ show (codomainNode, expectedCodomain)
            )
          requireTyVar name domainNode
          requireTyVar name codomainNode
          Right emptyCoercionCopyEvidence
        (Nothing, Nothing) -> do
          requireEvidence
            (domainNode == codomainNode)
            ( "free annotation variable was not shared: "
                ++ name
                ++ " -> "
                ++ show (domainNode, codomainNode)
            )
          case lookupNodeIn nodes domainNode of
            Just TyVar {tnBound = Nothing} ->
              Right
                emptyCoercionCopyEvidence
                  { coercionEvidenceFreeNodes =
                      Map.singleton name domainNode
                  }
            other ->
              Left
                ( "free annotation variable was not an unbounded TyVar: "
                    ++ name
                    ++ " -> "
                    ++ show other
                )
        pair ->
          Left
            ( "source binder environment disagreed between copies for "
                ++ name
                ++ ": "
                ++ show pair
            )

    validateBound ::
      forall n.
      Map.Map String NodeId ->
      Map.Map String NodeId ->
      Maybe (Surf.SrcBound n) ->
      NodeId ->
      NodeId ->
      Either String CoercionCopyEvidence
    validateBound domainEnv codomainEnv mbSourceBound domainBinder codomainBinder =
      case
          ( mbSourceBound,
            lookupNodeIn nodes domainBinder,
            lookupNodeIn nodes codomainBinder
          )
        of
          (Nothing, Just TyVar {tnBound = Nothing}, Just TyVar {tnBound = Nothing}) ->
            Right emptyCoercionCopyEvidence
          ( Just sourceBound,
            Just TyVar {tnBound = Just domainBound},
            Just TyVar {tnBound = Just codomainBound}
            ) ->
              go
                domainEnv
                codomainEnv
                (Surf.unSrcBound sourceBound)
                domainBound
                codomainBound
          triple ->
            Left
              ( "forall bound copies did not match source presence: "
                  ++ show triple
              )

    requireTyVar name node =
      case lookupNodeIn nodes node of
        Just TyVar {} -> Right ()
        other ->
          Left
            ( "occurrence of "
                ++ name
                ++ " was not a TyVar: "
                ++ show other
            )

    findLexicalBinder identity owner expectedFlag =
      case
          [ NodeId key
            | (key, nodeIdentity) <-
                IntMap.toList (crSourceTypeBinderIdentities result),
              nodeIdentity == identity,
              Just TyVar {} <- [lookupNodeIn nodes (NodeId key)],
              IntMap.lookup
                (nodeRefKey (typeRef (NodeId key)))
                (cBindParents constraint)
                == Just (typeRef owner, expectedFlag)
          ]
        of
          [binder] -> Right binder
          candidates ->
            Left
              ( "expected exactly one lexical binder under "
                  ++ show owner
                  ++ ", saw "
                  ++ show candidates
              )

    expectSourceIdentity description identity node =
      requireEvidence
        ( IntMap.lookup
            (getNodeId node)
            (crSourceTypeBinderIdentities result)
            == Just identity
        )
        (description ++ " did not retain its semantic source identity")

    findGenOwnedLexicalBinder identity expectedFlag =
      case
          [ NodeId key
            | (key, nodeIdentity) <-
                IntMap.toList (crSourceTypeBinderIdentities result),
              nodeIdentity == identity,
              Just TyVar {} <- [lookupNodeIn nodes (NodeId key)],
              Just (GenRef _, actualFlag) <-
                [ IntMap.lookup
                    (nodeRefKey (typeRef (NodeId key)))
                    (cBindParents constraint)
                ],
              actualFlag == expectedFlag
          ]
        of
          [binder] -> Right binder
          candidates ->
            Left
              ( "expected one gen-owned vacuous mu binder with flag "
                  ++ show expectedFlag
                  ++ ", saw "
                  ++ show candidates
              )

    ownedNodeEvidence description domainNode codomainNode = do
      requireEvidence
        (domainNode /= codomainNode)
        (description ++ " was shared between coercion copies: " ++ show domainNode)
      expectDomainNodeKind description domainNode
      expectNodeKind description codomainNode Binding.NodeInstantiable
      Right
        emptyCoercionCopyEvidence
          { coercionEvidenceDomainOwned =
              IntSet.singleton (getNodeId domainNode),
            coercionEvidenceCodomainOwned =
              IntSet.singleton (getNodeId codomainNode)
          }

    expectDomainNodeKind description node =
      case Binding.nodeKind constraint (typeRef node) of
        Right Binding.NodeRestricted -> Right ()
        Right Binding.NodeLocked -> Right ()
        Right actual ->
          Left
            ( description
                ++ " remained instantiable in the rigid domain: "
                ++ show actual
            )
        Left err ->
          Left
            ( description
                ++ " had no valid node kind: "
                ++ show err
            )

    expectNodeKind description node expected =
      case Binding.nodeKind constraint (typeRef node) of
        Right actual
          | actual == expected -> Right ()
          | otherwise ->
              Left
                ( description
                    ++ " had node kind "
                    ++ show actual
                    ++ ", expected "
                    ++ show expected
                )
        Left err ->
          Left
            ( description
                ++ " had no valid node kind: "
                ++ show err
            )

mergeEvidenceList ::
  [CoercionCopyEvidence] ->
  Either String CoercionCopyEvidence
mergeEvidenceList =
  foldM mergeEvidence emptyCoercionCopyEvidence

mergeEvidence ::
  CoercionCopyEvidence ->
  CoercionCopyEvidence ->
  Either String CoercionCopyEvidence
mergeEvidence left right = do
  let leftFree = coercionEvidenceFreeNodes left
      rightFree = coercionEvidenceFreeNodes right
      conflicts =
        [ (name, leftNode, rightNode)
          | (name, leftNode) <- Map.toList leftFree,
            Just rightNode <- [Map.lookup name rightFree],
            leftNode /= rightNode
        ]
  requireEvidence
    (null conflicts)
    ("free annotation variable occurrences were not shared: " ++ show conflicts)
  Right
    CoercionCopyEvidence
      { coercionEvidenceFreeNodes = Map.union leftFree rightFree,
        coercionEvidenceDomainOwned =
          IntSet.union
            (coercionEvidenceDomainOwned left)
            (coercionEvidenceDomainOwned right),
        coercionEvidenceCodomainOwned =
          IntSet.union
            (coercionEvidenceCodomainOwned left)
            (coercionEvidenceCodomainOwned right)
      }

requireEvidence :: Bool -> String -> Either String ()
requireEvidence condition message =
  if condition
    then Right ()
    else Left message

propReifyInline :: Int -> Property
propReifyInline _size =
  elaboratesTo (Surf.EAnn (Surf.ELit (Surf.LInt 1)) (Surf.STBase "Int")) intTy

propInlinePred :: Int -> Property
propInlinePred _size =
  let inlineable :: Elab.ElabType
      inlineable =
        testTForall
          "a"
          (Just (boundFromType intTy))
          (Elab.TArrow (testTVar "a") boolTy)
      inlined :: Elab.ElabType
      inlined = Elab.TArrow intTy boolTy
      selfBound :: Elab.ElabType
      selfBound =
        testTForall
          "a"
          (Just (Elab.TArrow (testTVar "a") intTy))
          (Elab.TArrow (testTVar "a") boolTy)
   in conjoin
        [ counterexample (Elab.prettyDisplay inlineable) $
            Elab.prettyDisplay inlineable == Elab.prettyDisplay inlined,
          counterexample (Elab.prettyDisplay inlineable) $
            Elab.prettyDisplay inlineable /= Elab.pretty inlineable,
          counterexample (Elab.prettyDisplay selfBound) $
            Elab.prettyDisplay selfBound == Elab.pretty selfBound
        ]

propCgenRoot :: Int -> Property
propCgenRoot _size =
  case runConstraintDefault Set.empty (Surf.ELit (Surf.LInt 1)) of
    Right ConstraintResult {crConstraint = c, crRoot = root} ->
      case lookupNodeIn (cNodes c) root of
        Just TyVar {tnBound = Just bound} ->
          conjoin
            [ lookupNodeIn (cNodes c) bound === Just (TestTyBase bound (BaseTy "Int")),
              Binding.checkBindingTree c === Right ()
            ]
        other -> counterexample (show other) False
    Left err -> counterexample err False

propCgenExpr :: Int -> Property
propCgenExpr _size =
  case runConstraintDefault Set.empty (Surf.EApp (Surf.ELam "x" (Surf.EVar "x")) (Surf.ELit (Surf.LInt 1))) of
    Right ConstraintResult {crConstraint = c} ->
      conjoin [counterexample (show (cInstEdges c)) (not (null (cInstEdges c))), Binding.checkBindingTree c === Right ()]
    Left err -> counterexample err False

propExpDecide :: Int -> Property
propExpDecide size =
  conjoin
    [ assertMinimalDecision "identity" cId expId targetId $ \(expansion, unifications) ->
        conjoin
          [ expansion === ExpIdentity,
            unifications === [(bodyId, targetId)]
          ],
      assertMinimalDecision "instantiate" cInst expInst targetArrow $ \(expansion, unifications) ->
        case expansion of
          ExpInstantiate args ->
            conjoin
              [ counterexample (show args) (length args === 1),
                unifications === []
              ]
          other -> counterexample (show other) False,
      assertMinimalDecision "compose-polytype" cCompose expCompose targetForall2 $ \(expansion, unifications) ->
        conjoin
          [ expansion
              === ExpCompose
                ( ExpInstantiate [targetArrowC]
                    :| [ExpForall (ForallSpec [Nothing, Nothing] :| [])]
                ),
            unifications === []
          ],
      assertMinimalDecision "forall-intro" cForallIntro expForallIntro targetForallIntro $ \(expansion, unifications) ->
        conjoin
          [ expansion === ExpForall (ForallSpec [Nothing, Nothing] :| []),
            unifications === []
          ]
    ]
  where
    base = max 3 size * 20
    bodyId = NodeId (base + 1)
    targetId = NodeId (base + 2)
    expId = NodeId (base + 3)
    cId =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId bodyId, TestTyBase bodyId (BaseTy "Int")),
                  (getNodeId targetId, TestTyBase targetId (BaseTy "Int")),
                  (getNodeId expId, TyExp expId (ExpVarId base) bodyId)
                ],
            cBindParents = bindParentsFromPairs [(bodyId, expId, BindFlex)]
          }

    srcVar = NodeId (base + 10)
    srcArrow = NodeId (base + 11)
    srcForall = NodeId (base + 12)
    targetDom = NodeId (base + 13)
    targetCod = NodeId (base + 14)
    targetArrow = NodeId (base + 15)
    expInst = NodeId (base + 16)
    cInst =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcVar, TyVar {tnId = srcVar, tnBound = Nothing}),
                  (getNodeId srcArrow, TyArrow srcArrow srcVar srcVar),
                  (getNodeId srcForall, TyForall srcForall srcArrow),
                  (getNodeId targetDom, TestTyBase targetDom (BaseTy "Int")),
                  (getNodeId targetCod, TestTyBase targetCod (BaseTy "Int")),
                  (getNodeId targetArrow, TyArrow targetArrow targetDom targetCod),
                  (getNodeId expInst, TyExp expInst (ExpVarId (base + 1)) srcForall)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcVar, srcForall, BindFlex),
                  (srcArrow, srcForall, BindFlex),
                  (targetDom, targetArrow, BindFlex),
                  (targetCod, targetArrow, BindFlex),
                  (srcForall, expInst, BindFlex)
                ]
          }

    srcVarC = NodeId (base + 20)
    srcForallC = NodeId (base + 21)
    targetDomC = NodeId (base + 22)
    targetCodC = NodeId (base + 23)
    targetArrowC = NodeId (base + 24)
    targetForall2 = NodeId (base + 25)
    expCompose = NodeId (base + 26)
    cCompose =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcVarC, TyVar {tnId = srcVarC, tnBound = Nothing}),
                  (getNodeId srcForallC, TyForall srcForallC srcVarC),
                  (getNodeId targetDomC, TyVar {tnId = targetDomC, tnBound = Nothing}),
                  (getNodeId targetCodC, TyVar {tnId = targetCodC, tnBound = Nothing}),
                  (getNodeId targetArrowC, TyArrow targetArrowC targetDomC targetCodC),
                  (getNodeId targetForall2, TyForall targetForall2 targetArrowC),
                  (getNodeId expCompose, TyExp expCompose (ExpVarId (base + 2)) srcForallC)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcVarC, srcForallC, BindFlex),
                  (srcForallC, expCompose, BindFlex),
                  (targetDomC, targetForall2, BindFlex),
                  (targetCodC, targetForall2, BindFlex),
                  (targetArrowC, targetForall2, BindFlex)
                ]
          }

    srcDomF = NodeId (base + 30)
    srcCodF = NodeId (base + 31)
    srcArrowF = NodeId (base + 32)
    targetDomF = NodeId (base + 33)
    targetCodF = NodeId (base + 34)
    targetArrowF = NodeId (base + 35)
    targetForallIntro = NodeId (base + 36)
    expForallIntro = NodeId (base + 37)
    cForallIntro =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId srcDomF, TestTyBase srcDomF (BaseTy "Int")),
                  (getNodeId srcCodF, TestTyBase srcCodF (BaseTy "Bool")),
                  (getNodeId srcArrowF, TyArrow srcArrowF srcDomF srcCodF),
                  (getNodeId targetDomF, TyVar {tnId = targetDomF, tnBound = Nothing}),
                  (getNodeId targetCodF, TyVar {tnId = targetCodF, tnBound = Nothing}),
                  (getNodeId targetArrowF, TyArrow targetArrowF targetDomF targetCodF),
                  (getNodeId targetForallIntro, TyForall targetForallIntro targetArrowF),
                  (getNodeId expForallIntro, TyExp expForallIntro (ExpVarId (base + 3)) srcArrowF)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (srcDomF, srcArrowF, BindFlex),
                  (srcCodF, srcArrowF, BindFlex),
                  (srcArrowF, expForallIntro, BindFlex),
                  (targetDomF, targetForallIntro, BindFlex),
                  (targetCodF, targetForallIntro, BindFlex),
                  (targetArrowF, targetForallIntro, BindFlex)
                ]
          }

propExpApply :: Int -> Property
propExpApply _size =
  propEdgeWitnessOps letIdAppExpr (not . null)

propPropSolve :: Int -> Property
propPropSolve _size =
  propPresolutionClearsEdges letIdAppExpr

propPropWitness :: Int -> Property
propPropWitness _size =
  case runToPresolutionDefault Set.empty letIdAppExpr of
    Right presolution ->
      let c = prConstraint presolution
          entries = IntMap.toList (prEdgeWitnesses presolution)
       in conjoin
            [ counterexample (show entries) (not (null entries)),
              counterexample (show entries) $
                all
                  ( \(edgeKey, edgeWitness) ->
                      getEdgeId (ewEdgeId edgeWitness) == edgeKey
                        && isJust (lookupNodeIn (cNodes c) (ewRoot edgeWitness))
                  )
                  entries
            ]
    Left err -> counterexample err False

propCopyScheme :: Int -> Property
propCopyScheme size =
  let base = size * 100
      bound = NodeId (base + 1)
      sharedArrow = NodeId (base + 5)
      body = NodeId (base + 6)
      fresh = NodeId (base + 10)
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId bound, TyVar {tnId = bound, tnBound = Nothing}),
                    (getNodeId sharedArrow, TyArrow sharedArrow bound bound),
                    (getNodeId body, TyArrow body sharedArrow sharedArrow),
                    (getNodeId fresh, TyVar {tnId = fresh, tnBound = Nothing})
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (bound, sharedArrow, BindFlex),
                    (sharedArrow, body, BindFlex)
                  ]
            }
      st0 = emptyPresolutionState c
   in case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
        Right (root, st1) ->
          let c1 = psConstraint st1
              nodes = cNodes c1
              expectedSourceBody = TyArrow body sharedArrow sharedArrow
           in case lookupNodeIn nodes root of
                Just TyArrow {tnDom = dom, tnCod = cod} ->
                  conjoin
                    [ counterexample "scheme root is copied to a fresh node" (root /= body),
                      counterexample "shared body child is copied once and reused" (dom == cod && dom /= sharedArrow),
                      counterexample "source body remains unchanged" (lookupNodeIn nodes body == Just expectedSourceBody),
                      case lookupNodeIn nodes dom of
                        Just TyArrow {tnDom = innerDom, tnCod = innerCod} ->
                          conjoin
                            [ counterexample "substituted binder is used in copied domain" (innerDom == fresh),
                              counterexample "substituted binder is used in copied codomain" (innerCod == fresh)
                            ]
                        other -> counterexample ("expected copied shared arrow, got " ++ show other) False,
                      counterexample "fresh substitution node remains live" (isJust (lookupNodeIn nodes fresh)),
                      Binding.checkBindingTree c1 === Right ()
                    ]
                other -> counterexample ("expected copied scheme root arrow, got " ++ show other) False
        Left err -> counterexample (show err) False

witnessChainFixture
  :: Int
  -> (OmegaNormalizeEnv 'Raw, NodeId, [NodeId], NodeId)
witnessChainFixture requestedSize =
  let chainSize = max 1 requestedSize
      root = NodeId 0
      parent = NodeId 1
      children = map NodeId [2 .. chainSize + 1]
      sibling = NodeId (chainSize + 2)
      allNodes = root : parent : children ++ [sibling]
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId node, TyVar {tnId = node, tnBound = Nothing})
                  | node <- allNodes
                  ],
              cBindParents =
                bindParentsFromPairs
                  ( (parent, root, BindFlex)
                      : (sibling, root, BindFlex)
                      : [ (child, parent, BindFlex)
                        | child <- children
                        ]
                  )
            }
      env =
        mkNormalizeEnv
          c
          root
          (IntSet.fromList (map getNodeId allNodes))
   in (env, parent, children, sibling)

propWitnessNorm :: Int -> Property
propWitnessNorm size =
  let (env, parent, children, _sibling) = witnessChainFixture size
      duplicatedRaises =
        concat
          [ replicate (1 + getNodeId child `mod` 3) (OpRaise child)
          | child <- children
          ]
      input = OpWeaken parent : duplicatedRaises
      expected = map OpRaise children ++ [OpWeaken parent]
   in case normalizeInstanceOpsFull env input of
        Left err -> counterexample (show err) False
        Right validated ->
          let normalized = getValidatedInstanceOps validated
           in conjoin
                [ counterexample "normalization did not delay Weaken or remove duplicate Raises" $
                    normalized === expected,
                  counterexample "the certified output does not satisfy Definition 11.5.2" $
                    validateNormalizedWitness env normalized === Right (),
                  counterexample "certified normalization is not idempotent" $
                    normalizeInstanceOpsForTest env normalized === Right normalized
                ]

propWitnessCoalesce :: Int -> Property
propWitnessCoalesce size =
  let (env0, operated, _children, exterior) = witnessChainFixture size
      env =
        env0
          { interior =
              IntSet.delete
                (getNodeId exterior)
                (interior env0)
          }
      input = replicate (max 1 size) (OpRaise operated) ++ [OpMerge operated exterior]
   in coalesceRaiseMergeWithEnv env input === Right [OpRaiseMerge operated exterior]

propWitnessReorder :: Int -> Property
propWitnessReorder size =
  let (env, parent, children, sibling) = witnessChainFixture size
      input =
        OpRaise sibling
          : OpWeaken parent
          : concatMap
              (\child -> [OpRaise child, OpRaise sibling])
              children
      nonWeakens = filter (/= OpWeaken parent) input
      lastChildIndex =
        maximum
          [ index
          | (index, OpRaise node) <- zip [0 :: Int ..] nonWeakens
          , node `elem` children
          ]
      (prefix, suffix) = splitAt (lastChildIndex + 1) nonWeakens
      expected = prefix ++ [OpWeaken parent] ++ suffix
   in reorderWeakenWithEnv env input === Right expected

propAcyclicCheck :: Int -> Property
propAcyclicCheck size =
  let c = acyclicConstraint size
   in case checkAcyclicityRaw c of
        Right result -> counterexample (show result) (not (null (arSortedEdges result)))
        Left err -> counterexample (show err) False

propAcyclicTopo :: Int -> Property
propAcyclicTopo size =
  let c = acyclicConstraint size
   in case checkAcyclicityRaw c of
        Right result -> arSortedEdges result === [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
        Left err -> counterexample (show err) False

propCopyInst :: Int -> Property
propCopyInst size =
  let base = size * 100
      binder = NodeId (base + 1)
      outerVar = NodeId (base + 2)
      frontierArrow = NodeId (base + 3)
      bodyArrow = NodeId (base + 4)
      forallNode = NodeId (base + 5)
      expNode = NodeId (base + 6)
      meta = NodeId (base + 10)
      c =
        rootedConstraint
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing}),
                    (getNodeId outerVar, TyVar {tnId = outerVar, tnBound = Nothing}),
                    (getNodeId frontierArrow, TyArrow frontierArrow outerVar outerVar),
                    (getNodeId bodyArrow, TyArrow bodyArrow frontierArrow binder),
                    (getNodeId forallNode, TyForall forallNode bodyArrow),
                    (getNodeId expNode, TyExp expNode (ExpVarId base) forallNode),
                    (getNodeId meta, TyVar {tnId = meta, tnBound = Nothing})
                  ],
              cBindParents =
                bindParentsFromPairs
                  [ (forallNode, expNode, BindFlex),
                    (bodyArrow, forallNode, BindFlex),
                    (binder, bodyArrow, BindFlex),
                    (frontierArrow, expNode, BindFlex),
                    (outerVar, frontierArrow, BindFlex)
                  ]
            }
      st0 = emptyPresolutionState c
      directCopy =
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow [(binder, meta)]) of
          Right ((root, copyMap, interior, frontier), st1) ->
            let c1 = psConstraint st1
                nodes = cNodes c1
             in case lookupNodeIn nodes root of
                  Just TyArrow {tnDom = dom, tnCod = cod} ->
                    conjoin
                      [ counterexample "Inst-Copy root is freshly copied" (root /= bodyArrow),
                        lookupCopy bodyArrow copyMap === Just root,
                        lookupCopy binder copyMap === Just meta,
                        lookupCopy frontierArrow copyMap === Just dom,
                        counterexample "binder argument is substituted into the copied codomain" (cod == meta),
                        counterexample "frontier source is recorded in the frontier set" $
                          IntSet.member (getNodeId frontierArrow) frontier,
                        counterexample "fresh copied root is recorded in trace interior" $
                          IntSet.member (getNodeId root) interior,
                        counterexample "binder argument is recorded in trace interior" $
                          IntSet.member (getNodeId meta) interior,
                        case lookupNodeIn nodes dom of
                          Just TyBottom {tnId = bottomId} ->
                            counterexample "frontier copy is replaced with bottom" (bottomId == dom)
                          other -> counterexample ("expected bottom frontier copy, got " ++ show other) False,
                        counterexample "trace copy map records source-to-copy bookkeeping" $
                          not (IntMap.null (getCopyMapping copyMap)),
                        Binding.checkBindingTree c1 === Right ()
                      ]
                  other -> counterexample ("expected copied Inst-Copy arrow, got " ++ show other) False
          Left err -> counterexample (show err) False
      recordedTrace =
        let edgeId = EdgeId (base + 20)
            edgeSourceBinder = NodeId (base + 21)
            edgeSourceArrow = NodeId (base + 22)
            edgeSourceForall = NodeId (base + 23)
            edgeTargetDom = NodeId (base + 24)
            edgeTargetCod = NodeId (base + 25)
            edgeTargetArrow = NodeId (base + 26)
            edgeExp = NodeId (base + 27)
            edge =
              InstEdge
                edgeId
                edgeExp
                edgeTargetArrow
            edgeConstraint =
              rootedConstraint
                emptyConstraint
                  { cNodes =
                      nodeMapFromList
                        [ (getNodeId edgeSourceBinder, TyVar {tnId = edgeSourceBinder, tnBound = Nothing}),
                          (getNodeId edgeSourceArrow, TyArrow edgeSourceArrow edgeSourceBinder edgeSourceBinder),
                          (getNodeId edgeSourceForall, TyForall edgeSourceForall edgeSourceArrow),
                          (getNodeId edgeTargetDom, TestTyBase edgeTargetDom (BaseTy "Int")),
                          (getNodeId edgeTargetCod, TestTyBase edgeTargetCod (BaseTy "Int")),
                          (getNodeId edgeTargetArrow, TyArrow edgeTargetArrow edgeTargetDom edgeTargetCod),
                          (getNodeId edgeExp, TyExp edgeExp (ExpVarId (base + 28)) edgeSourceForall)
                        ],
                    cBindParents =
                      bindParentsFromPairs
                        [ (edgeSourceBinder, edgeSourceForall, BindFlex),
                          (edgeSourceArrow, edgeSourceForall, BindFlex),
                          (edgeSourceForall, edgeExp, BindFlex),
                          (edgeTargetDom, edgeTargetArrow, BindFlex),
                          (edgeTargetCod, edgeTargetArrow, BindFlex)
                        ]
                  }
            edgeSt0 = emptyPresolutionState edgeConstraint
         in case runPresolutionM defaultTraceConfig edgeSt0 (processInstEdge edge) of
              Right (_, edgeSt1) ->
                let traces = psEdgeTraces edgeSt1
                 in case IntMap.lookup (getEdgeId edgeId) traces of
                      Just tr ->
                        conjoin
                          [ counterexample ("empty binder args in trace: " ++ show tr) $
                              not (null (etBinderArgs tr)),
                            edgeTraceCopyEvidence (psConstraint edgeSt1) tr
                          ]
                      Nothing -> counterexample ("missing trace keys: " ++ show (IntMap.keys traces)) False
              Left err -> counterexample (show err) False
   in conjoin [directCopy, recordedTrace]

edgeTraceCopyEvidence :: Constraint 'Raw -> EdgeTrace -> Property
edgeTraceCopyEvidence c tr =
  let copyPairs = IntMap.toList (getCopyMapping (etCopyMap tr))
      binderPairs = etBinderArgs tr
   in conjoin
        [ counterexample "trace root is live in the presolved constraint" $
            isJust (lookupNodeIn (cNodes c) (etRoot tr)),
          counterexample ("empty trace copy map for " ++ show binderPairs) (not (null copyPairs)),
          counterexample ("binder sources are absent from the trace copy map: " ++ show (binderPairs, copyPairs)) $
            all (\(binder, _arg) -> IntMap.member (getNodeId binder) (getCopyMapping (etCopyMap tr))) binderPairs,
          counterexample ("binder arguments are not live: " ++ show binderPairs) $
            all (\(_binder, arg) -> isJust (lookupNodeIn (cNodes c) arg)) binderPairs,
          counterexample ("copy map targets are not live: " ++ show copyPairs) $
            all (\(_source, copied) -> isJust (lookupNodeIn (cNodes c) copied)) copyPairs
        ]

propNormGraft :: Int -> Property
propNormGraft size =
  let graftBase = BaseTy ("Graft" ++ show size)
      c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes =
                nodeMapFromList
                  [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                    (1, TestTyBase (NodeId 1) graftBase)
                  ],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex)],
              cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 1)]
            }
      normalized = normalizeRaw c
   in conjoin
        [ cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          counterexample "normalization lost the identity provenance of the grafted edge" $
            IntSet.member size (cGraftedEdges normalized),
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) graftBase),
          Binding.checkBindingTree normalized === Right ()
        ]

propNormMerge :: Int -> Property
propNormMerge size =
  let mergeBase = BaseTy ("Merge" ++ show size)
      c =
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TestTyBase (NodeId 1) mergeBase)
                ],
            cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
          }
      normalized = normalizeRaw c
   in conjoin
        [ cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) mergeBase)
        ]

propNormDrop :: Int -> Property
propNormDrop size =
  let node = TyVar {tnId = NodeId 0, tnBound = Nothing}
      edge = InstEdge (EdgeId size) (NodeId 0) (NodeId 0)
      c =
        emptyConstraint
          { cNodes = nodeMapFromList [(0, node)],
            cInstEdges = [edge]
          }
      normalized = normalizeRaw c
   in conjoin
        [ cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just node
        ]

propNormFixpoint :: Int -> Property
propNormFixpoint size =
  let fixpointBase = BaseTy ("Fixpoint" ++ show size)
      c =
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
                  (2, TestTyBase (NodeId 2) fixpointBase)
                ],
            cInstEdges =
              [ InstEdge (EdgeId size) (NodeId 0) (NodeId 1),
                InstEdge (EdgeId (size + 1)) (NodeId 1) (NodeId 2)
              ]
          }
      normalized = normalizeRaw c
   in conjoin
        [ normalized === normalizeRaw normalized,
          cInstEdges normalized === [],
          cUnifyEdges normalized === [],
          lookupNodeIn (cNodes normalized) (NodeId 0) === Just (TestTyBase (NodeId 0) fixpointBase),
          lookupNodeIn (cNodes normalized) (NodeId 1) === Just (TestTyBase (NodeId 1) fixpointBase)
        ]

propSolveVarBase :: Int -> Property
propSolveVarBase _size =
  let c =
        rootedConstraintLocal
          emptyConstraint
            { cNodes = nodeMapFromList [(0, TestTyCon (NodeId 0) (BaseTy "Box") (NodeId 1 :| [])), (1, TyVar (NodeId 1) Nothing), (2, TestTyBase (NodeId 2) (BaseTy "Int"))],
              cBindParents = bindParentsFromPairs [(NodeId 1, NodeId 0, BindFlex), (NodeId 2, NodeId 0, BindFlex)],
              cUnifyEdges = [UnifyEdge (NodeId 1) (NodeId 2)]
            }
   in case solveUnifyRaw defaultTraceConfig c of
        Right SolveResult {srConstraint = solved, srUnionFind = uf} ->
          conjoin [cUnifyEdges solved === [], frWith uf (NodeId 1) === frWith uf (NodeId 2)]
        Left err -> counterexample (show err) False

propSolveVarVar :: Int -> Property
propSolveVarVar _size =
  propSolveVar 0

propSolveHarmonize :: Int -> Property
propSolveHarmonize _size =
  propGeneralizedUnify 0

propSolveValidate :: Int -> Property
propSolveValidate _size =
  case solveUnifyRaw defaultTraceConfig varTripleConstraint of
    Right SolveResult {srConstraint = solved} -> Binding.checkBindingTree solved === Right ()
    Left err -> counterexample (show err) False

applyShouldBe :: Elab.ElabType -> Elab.Instantiation -> Elab.ElabType -> Property
applyShouldBe ty inst expected =
  case Elab.applyInstantiation ty inst of
    Right actual -> actual === expected
    Left err -> counterexample (show err) False

elaboratesTo :: Surf.SurfaceExpr -> Elab.ElabType -> Property
elaboratesTo expr expected =
  case Elab.runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Right (term, ty) ->
      conjoin
        [ typeShouldMatch ty expected,
          typeCheckShouldMatch (Elab.typeCheck term) expected
        ]
    Left err -> counterexample (Elab.renderPipelineError err) False

typeShouldMatch :: Elab.ElabType -> Elab.ElabType -> Property
typeShouldMatch actual expected =
  counterexample (show actual ++ " /= " ++ show expected) $
    TypeOps.alphaEqType actual expected

typeCheckShouldMatch :: Either Elab.TypeCheckError Elab.ElabType -> Elab.ElabType -> Property
typeCheckShouldMatch actual expected =
  case actual of
    Right ty -> typeShouldMatch ty expected
    Left err -> counterexample (show err) False

findLambdaBindingEvidence :: String -> Elab.XmlfTerm -> Maybe (ElabTypes.ResolvedVar, ElabTypes.ResolvedVar)
findLambdaBindingEvidence targetName = go
  where
    go term =
      case term of
        Elab.ELam binder body
          | ElabTypes.resolvedVarReferenceName binder == targetName ->
              fmap (\occurrence -> (binder, occurrence)) (findResolvedOccurrence binder body)
          | otherwise -> go body
        Elab.EApp fun arg -> firstJust (go fun) (go arg)
        Elab.ELet _ _ rhs body -> firstJust (go rhs) (go body)
        Elab.ETyAbsRef _ _ body -> go body
        Elab.ETyInst body _ -> go body
        Elab.ERoll _ body -> go body
        Elab.EUnroll body -> go body
        Elab.EVarNode _ -> Nothing
        Elab.ELit _ -> Nothing

findLetBindingEvidence :: String -> Elab.XmlfTerm -> Maybe (ElabTypes.ResolvedVar, Elab.ElabScheme, ElabTypes.ResolvedVar)
findLetBindingEvidence targetName = go
  where
    go term =
      case term of
        Elab.ELet binder scheme rhs body
          | ElabTypes.resolvedVarReferenceName binder == targetName ->
              fmap
                (\occurrence -> (binder, scheme, occurrence))
                (findResolvedOccurrence binder body)
          | otherwise -> firstJust (go rhs) (go body)
        Elab.ELam _ body -> go body
        Elab.EApp fun arg -> firstJust (go fun) (go arg)
        Elab.ETyAbsRef _ _ body -> go body
        Elab.ETyInst body _ -> go body
        Elab.ERoll _ body -> go body
        Elab.EUnroll body -> go body
        Elab.EVarNode _ -> Nothing
        Elab.ELit _ -> Nothing

findResolvedOccurrence :: ElabTypes.ResolvedVar -> Elab.XmlfTerm -> Maybe ElabTypes.ResolvedVar
findResolvedOccurrence binder = go
  where
    binderKey = ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails binder)

    go term =
      case term of
        Elab.EVarNode occurrence
          | ElabTypes.idDetailsIdentityKey (ElabTypes.resolvedVarDetails occurrence) == binderKey ->
              Just occurrence
          | otherwise -> Nothing
        Elab.ELam _ body -> go body
        Elab.EApp fun arg -> firstJust (go fun) (go arg)
        Elab.ELet _ _ rhs body -> firstJust (go rhs) (go body)
        Elab.ETyAbsRef _ _ body -> go body
        Elab.ETyInst body _ -> go body
        Elab.ERoll _ body -> go body
        Elab.EUnroll body -> go body
        Elab.ELit _ -> Nothing

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust first second =
  case first of
    Just value -> Just value
    Nothing -> second

propPresolutionClearsEdges :: Surf.SurfaceExpr -> Property
propPresolutionClearsEdges expr =
  case runToPresolutionDefault Set.empty expr of
    Right presolution ->
      let c = prConstraint presolution
       in conjoin
            [ Binding.checkBindingTree c === Right (),
              cInstEdges c === []
            ]
    Left err -> counterexample err False

propEdgeWitnessOps :: Surf.SurfaceExpr -> ([EdgeWitness] -> Bool) -> Property
propEdgeWitnessOps expr predicate =
  case runToPresolutionDefault Set.empty expr of
    Right presolution ->
      let values = IntMap.elems (prEdgeWitnesses presolution)
       in counterexample (show values) (predicate values)
    Left err -> counterexample err False

acyclicConstraint :: Int -> Constraint 'Raw
acyclicConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TestTyBase (NodeId 2) (BaseTy "Int"))
            ],
        cInstEdges = [InstEdge (EdgeId size) (NodeId 0) (NodeId 2)]
      }

flexibleSchemeRootConstraint :: Constraint 'Raw
flexibleSchemeRootConstraint =
  let rootGen = GenNodeId 0
      schemeRoot = NodeId 0
   in rootedConstraint
        emptyConstraint
          { cNodes = nodeMapFromList [(0, TyVar {tnId = schemeRoot, tnBound = Nothing})],
            cBindParents = IntMap.fromList [(nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))],
            cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
          }

flexibleArrowConstraint :: Constraint 'Raw
flexibleArrowConstraint =
  let rootGen = GenNodeId 0
      dom = NodeId 0
      cod = NodeId 1
      arr = NodeId 2
   in rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (0, TyVar {tnId = dom, tnBound = Nothing}),
                  (1, TyVar {tnId = cod, tnBound = Nothing}),
                  (2, TyArrow arr dom cod)
                ],
            cBindParents =
              IntMap.fromList
                [ (nodeRefKey (typeRef arr), (genRef rootGen, BindFlex)),
                  (nodeRefKey (typeRef dom), (typeRef arr, BindFlex)),
                  (nodeRefKey (typeRef cod), (typeRef arr, BindFlex))
                ],
            cGenNodes = fromListGen [(rootGen, GenNode rootGen [arr])]
          }

flexibleNonInteriorConstraint :: Constraint 'Raw
flexibleNonInteriorConstraint =
  let rootGen = GenNodeId 0
      schemeRoot = NodeId 0
      dom = NodeId 1
      cod = NodeId 2
      arrow = NodeId 3
      outside = NodeId 4
   in emptyConstraint
        { cNodes =
            nodeMapFromList
              [ (getNodeId schemeRoot, TyVar {tnId = schemeRoot, tnBound = Just arrow}),
                (getNodeId dom, TyVar {tnId = dom, tnBound = Nothing}),
                (getNodeId cod, TyVar {tnId = cod, tnBound = Nothing}),
                (getNodeId arrow, TyArrow arrow dom cod),
                (getNodeId outside, TyVar {tnId = outside, tnBound = Nothing})
              ],
          cBindParents =
            IntMap.fromList
              [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindRigid)),
                (nodeRefKey (typeRef arrow), (typeRef schemeRoot, BindRigid)),
                (nodeRefKey (typeRef dom), (typeRef arrow, BindFlex)),
                (nodeRefKey (typeRef cod), (typeRef arrow, BindFlex)),
                (nodeRefKey (typeRef outside), (genRef rootGen, BindFlex))
              ],
          cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
        }

forallA :: Elab.ElabType
forallA = testTForall "a" Nothing (testTVar "a")

polyIdTy :: Elab.ElabType
polyIdTy = testTForall "a" Nothing (Elab.TArrow (testTVar "a") (testTVar "a"))

letIdAppExpr :: Surf.SurfaceExpr
letIdAppExpr =
  Surf.ELet "id" (Surf.ELam "x" (Surf.EVar "x")) (Surf.EApp (Surf.EVar "id") (Surf.ELit (Surf.LInt 1)))

boundFromType :: Elab.ElabType -> Elab.BoundType
boundFromType ty =
  case ty of
    Elab.TVarRef ref -> error ("boundFromType: unexpected variable bound " ++ show ref)
    Elab.TArrow a b -> Elab.TArrow a b
    Elab.TConWithIdentity _ c args -> TestElab.tCon c args
    Elab.TVarAppRef ref args -> Elab.TVarAppRef ref args
    Elab.TBaseWithIdentity _ b -> TestElab.tBase b
    Elab.TBottom -> Elab.TBottom
    Elab.TForallRef ref mb body -> Elab.TForallRef ref mb body
    Elab.TMuRef ref body -> Elab.TMuRef ref body

emptyPresolutionState :: Constraint 'Raw -> PresolutionState 'Raw
emptyPresolutionState c =
  PresolutionState
    c
    (Presolution IntMap.empty)
    IntMap.empty
    (maxNodeIdKeyOr0 c + 1)
    IntSet.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty
    IntMap.empty

canonicalPresolutionNode :: IntMap.IntMap NodeId -> NodeId -> NodeId
canonicalPresolutionNode parents = go IntSet.empty
  where
    go seen current
      | IntSet.member (getNodeId current) seen = current
      | otherwise =
          case IntMap.lookup (getNodeId current) parents of
            Just parent
              | parent /= current ->
                  go (IntSet.insert (getNodeId current) seen) parent
            _ -> current

identityPresolutionView :: Constraint 'Raw -> PresolutionView 'Raw
identityPresolutionView c =
  PresolutionView
    { pvConstraint = c,
      pvCanonicalMap = IntMap.empty,
      pvCanonical = id,
      pvLookupNode = \nid -> lookupNodeIn (cNodes c) nid,
      pvLookupVarBound =
        \nid -> case lookupNodeIn (cNodes c) nid of
          Just TyVar {tnBound = mbBound} -> mbBound
          _ -> Nothing,
      pvLookupBindParent = Binding.lookupBindParent c,
      pvBindParents = cBindParents c,
      pvCanonicalConstraint = c
    }

assertMinimalDecision ::
  String ->
  Constraint 'Raw ->
  NodeId ->
  NodeId ->
  ((Expansion, [(NodeId, NodeId)]) -> Property) ->
  Property
assertMinimalDecision caseName c expNodeId targetNodeId checkDecision =
  case decideMinimalFor c expNodeId targetNodeId of
    Right decision -> counterexample (caseName ++ ": " ++ show decision) (checkDecision decision)
    Left err -> counterexample (caseName ++ ": " ++ err) False

decideMinimalFor :: Constraint 'Raw -> NodeId -> NodeId -> Either String (Expansion, [(NodeId, NodeId)])
decideMinimalFor c expNodeId targetNodeId =
  case (lookupNodeIn (cNodes c) expNodeId, lookupNodeIn (cNodes c) targetNodeId) of
    (Just expNode, Just targetNode) ->
      case runPresolutionM defaultTraceConfig (emptyPresolutionState c) (decideMinimalExpansion id (GenNodeId 0) True expNode targetNode) of
        Right (decision, _st) -> Right decision
        Left err -> Left (show err)
    (Nothing, _) -> Left ("missing expansion node " ++ show expNodeId)
    (_, Nothing) -> Left ("missing target node " ++ show targetNodeId)

assertNodeAliasTranslation :: Int -> (NodeId -> NodeId -> InstanceOp) -> Property
assertNodeAliasTranslation size mkOp =
  let (c, root, binderA, binderB, scheme, si, tr) = nodeAliasTranslationFixture size
      refA = elabTypeRef (getNodeId binderA) "a"
      ew =
        EdgeWitness
          { ewEdgeId = EdgeId size,
            ewLeft = root,
            ewRight = root,
            ewRoot = root,
            ewForallIntros = 0,
            ewWitness = InstanceWitness [mkOp binderB binderA]
          }
      expected =
        Elab.TForallRef
          refA
          Nothing
          (Elab.TArrow (Elab.TVarRef refA) (Elab.TVarRef refA))
      generalizeAt _ _ _ =
        Left (Elab.InstantiationError "assertNodeAliasTranslation: unexpected generalization")
   in case PhiTestSupport.phiFromEdgeWitnessWithTraceForTest defaultTraceConfig generalizeAt (identityPresolutionView c) Nothing (Just si) (Just tr) ew of
        Left err -> counterexample (show err) False
        Right phi ->
          case Elab.applyInstantiation (Elab.schemeToType scheme) phi of
            Left err -> counterexample (show err) False
            Right out -> counterexample (Elab.pretty phi ++ " => " ++ Elab.pretty out) (out === expected)

nodeAliasTranslationFixture :: Int -> (Constraint 'Raw, NodeId, NodeId, NodeId, Elab.ElabScheme, Elab.SchemeInfo, EdgeTrace)
nodeAliasTranslationFixture size =
  (c, root, binderA, binderB, scheme, si, tr)
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    binderA = NodeId (base + 1)
    refA = elabTypeRef (getNodeId binderA) "a"
    forallB = NodeId (base + 102)
    binderB = NodeId (base + 2)
    refB = elabTypeRef (getNodeId binderB) "b"
    bodyNode = NodeId (base + 103)
    c =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root forallB),
                  (getNodeId binderA, TyVar {tnId = binderA, tnBound = Nothing}),
                  (getNodeId forallB, TyForall forallB bodyNode),
                  (getNodeId binderB, TyVar {tnId = binderB, tnBound = Nothing}),
                  (getNodeId bodyNode, TyArrow bodyNode binderA binderB)
                ],
            cBindParents =
              bindParentsFromPairs
                [ (binderA, root, BindFlex),
                  (forallB, root, BindFlex),
                  (binderB, forallB, BindFlex),
                  (bodyNode, forallB, BindFlex)
                ]
          }
    scheme =
      Elab.schemeFromType
        ( Elab.TForallRef
            refA
            Nothing
            (Elab.TForallRef refB Nothing (Elab.TArrow (Elab.TVarRef refA) (Elab.TVarRef refB)))
        )
    si =
      Elab.schemeInfoFromRefSubst
        scheme
        ( IntMap.fromList
            [ (getNodeId binderA, refA),
              (getNodeId binderB, refB)
            ]
        )
    tr =
      EdgeTrace
        { etRoot = root,
          etResultRoot = root,
          etBinderArgs = [],
          etInterior = sourceInteriorFromList [root, binderA, forallB, binderB, bodyNode],
          etReplayContract = ReplayContractNone,
          etBinderReplayMap = mempty,
          etReplayDomainBinders = [],
          etCopyMap = mempty
        }

orderedBinderFixture :: Int -> (Constraint 'Raw, NodeId, [NodeId])
orderedBinderFixture size =
  (c, root, [bN, aN])
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bN = NodeId (base + 2)
    c =
      rootedConstraint
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body bN aN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bN, TyVar {tnId = bN, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bN, root, BindFlex)
                ]
          }

contextFindFixture :: Int -> (Constraint 'Raw, NodeId, NodeId, [Elab.ContextStep])
contextFindFixture size =
  (c, root, cN, [stepUnder aN, Elab.StepInside])
  where
    stepUnder nid =
      Elab.StepUnderRef
        ( ElabTypes.typeBinderRefFromIdentity
            (ElabTypes.typeBinderIdentityFromNode nid)
            ("t" ++ show (getNodeId nid))
        )
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bN = NodeId (base + 2)
    cN = NodeId (base + 3)
    c =
      rootedConstraintLocal
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body aN bN),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bN, TyVar {tnId = bN, tnBound = Just cN}),
                  (getNodeId cN, TyVar {tnId = cN, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bN, root, BindFlex),
                  (cN, bN, BindFlex)
                ]
          }

contextRejectFixture :: Int -> (Constraint 'Raw, NodeId, NodeId)
contextRejectFixture size =
  (c, root, bodyOnly)
  where
    base = max 3 size * 10
    root = NodeId (base + 100)
    body = NodeId (base + 101)
    aN = NodeId (base + 1)
    bodyOnly = NodeId (base + 2)
    c =
      rootedConstraintLocal
        emptyConstraint
          { cNodes =
              nodeMapFromList
                [ (getNodeId root, TyForall root body),
                  (getNodeId body, TyArrow body aN bodyOnly),
                  (getNodeId aN, TyVar {tnId = aN, tnBound = Nothing}),
                  (getNodeId bodyOnly, TyVar {tnId = bodyOnly, tnBound = Nothing})
                ],
            cBindParents =
              bindParentsFromPairs
                [ (body, root, BindFlex),
                  (aN, root, BindFlex),
                  (bodyOnly, body, BindFlex)
                ]
          }

chainConstraint :: Int -> Constraint 'Raw
chainConstraint rawSize =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList $
            [ (i, TyForall (NodeId i) (NodeId (i + 1)))
              | i <- [0 .. size - 2]
            ]
              ++ [(size - 1, TyVar {tnId = NodeId (size - 1), tnBound = Nothing})],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId i, NodeId (i - 1), BindFlex)
              | i <- [1 .. size - 1]
            ]
      }
  where
    size = max 3 rawSize

binderConstraint :: Constraint 'Raw
binderConstraint =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyForall (NodeId 0) (NodeId 1)),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TyVar {tnId = NodeId 2, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindFlex),
              (NodeId 2, NodeId 0, BindRigid)
            ]
      }

varTripleConstraint :: Constraint 'Raw
varTripleConstraint =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TestTyCon (NodeId 0) (BaseTy "Triple") (NodeId 1 :| [NodeId 2, NodeId 3])),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
              (2, TyVar {tnId = NodeId 2, tnBound = Nothing}),
              (3, TyVar {tnId = NodeId 3, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindFlex),
              (NodeId 2, NodeId 0, BindFlex),
              (NodeId 3, NodeId 0, BindFlex)
            ]
      }

rootedConstraintLocal :: Constraint 'Raw -> Constraint 'Raw
rootedConstraintLocal c0 =
  c0
    { cGenNodes = fromListGen [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 0])],
      cBindParents =
        IntMap.insertWith
          (\_ old -> old)
          (nodeRefKey (typeRef (NodeId 0)))
          (genRef (GenNodeId 0), BindFlex)
          (cBindParents c0)
    }

inertConstraint :: Int -> Constraint 'Raw
inertConstraint size =
  rootedConstraintLocal
    emptyConstraint
      { cNodes =
          nodeMapFromList
            [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 1)),
              (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)),
              (2, TyArrow (NodeId 2) (NodeId 4) (NodeId 3)),
              (3, TestTyBase (NodeId 3) (BaseTy ("Int" ++ show size))),
              (4, TyVar {tnId = NodeId 4, tnBound = Nothing})
            ],
        cBindParents =
          bindParentsFromPairs
            [ (NodeId 1, NodeId 0, BindRigid),
              (NodeId 2, NodeId 1, BindFlex),
              (NodeId 3, NodeId 2, BindFlex),
              (NodeId 4, NodeId 2, BindRigid)
            ]
      }

intTy :: Elab.ElabType
intTy = TestElab.tBase (BaseTy "Int")

emptyTypeCheckEnv :: Elab.Env
emptyTypeCheckEnv = Elab.mkTypeCheckEnvWithResolvedTerms [] Map.empty

builtinIntTy :: Elab.ElabType
builtinIntTy = ElabTypes.TBaseWithIdentity (Builtins.builtinTypeIdentity "Int") (BaseTy "Int")

boolTy :: Elab.ElabType
boolTy = TestElab.tBase (BaseTy "Bool")

elabTypeRef :: Int -> String -> ElabTypes.TypeBinderRef
elabTypeRef key name =
  ElabTypes.typeBinderRefFromIdentity (ElabTypes.typeBinderIdentityFromNode (NodeId key)) name

idLam :: Elab.XmlfTerm
idLam = mkTestLocalLam "x" intTy (mkTestDeferredVar "x")

polyId :: Elab.XmlfTerm
polyId = mkTestTyAbs "a" Nothing (mkTestLocalLam "x" (testTVar "a") (mkTestDeferredVar "x"))
