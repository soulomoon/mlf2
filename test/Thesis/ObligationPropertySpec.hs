{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
    forM_ obligations $ \obligation ->
      it (obligationId obligation) $
        property $
          withMaxSuccess 100 $
            forAll (chooseInt (3, 16)) $ \size ->
              counterexample (obligationId obligation ++ " failed at size " ++ show size) $
                obligationProperty obligation size

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

data Obligation = Obligation
  { obligationId :: String,
    obligationProperty :: Int -> Property
  }

obligations :: [Obligation]
obligations =
  [ Obligation "O14-WF-EMPTY" propWfEmpty,
    Obligation "O14-WF-TVAR" propWfTVar,
    Obligation "O14-WF-VAR" propWfVar,
    Obligation "O14-INST-REFLEX" propInstReflex,
    Obligation "O14-INST-TRANS" propInstTrans,
    Obligation "O14-INST-BOT" propInstBot,
    Obligation "O14-INST-HYP" propInstHyp,
    Obligation "O14-INST-INNER" propInstInner,
    Obligation "O14-INST-OUTER" propInstOuter,
    Obligation "O14-INST-QUANT-ELIM" propInstQuantElim,
    Obligation "O14-INST-QUANT-INTRO" propInstQuantIntro,
    Obligation "O14-T-VAR" propTypingVar,
    Obligation "O14-T-ABS" propTypingAbs,
    Obligation "O14-T-APP" propTypingApp,
    Obligation "O14-T-TABS" propTypingTAbs,
    Obligation "O14-T-TAPP" propTypingTApp,
    Obligation "O14-T-LET" propTypingLet,
    Obligation "O14-RED-BETA" propRedBeta,
    Obligation "O14-RED-BETALET" propRedBetaLet,
    Obligation "O14-RED-REFLEX" propRedReflex,
    Obligation "O14-RED-TRANS" propRedTrans,
    Obligation "O14-RED-QUANT-INTRO" propRedQuantIntro,
    Obligation "O14-RED-QUANT-ELIM" propRedQuantElim,
    Obligation "O14-RED-INNER" propRedInner,
    Obligation "O14-RED-OUTER" propRedOuter,
    Obligation "O14-RED-CONTEXT" propRedContext,
    Obligation "O14-APPLY-N" propApplyN,
    Obligation "O14-APPLY-O" propApplyO,
    Obligation "O14-APPLY-SEQ" propApplySeq,
    Obligation "O14-APPLY-INNER" propApplyInner,
    Obligation "O14-APPLY-OUTER" propApplyOuter,
    Obligation "O14-APPLY-HYP" propApplyHyp,
    Obligation "O14-APPLY-BOT" propApplyBot,
    Obligation "O14-APPLY-ID" propApplyId,
    Obligation "O15-TRANS-NO-INERT-LOCKED" propTransNoInertLocked,
    Obligation "O15-TRANS-SCHEME-ROOT-RIGID" propTransSchemeRootRigid,
    Obligation "O15-TRANS-ARROW-RIGID" propTransArrowRigid,
    Obligation "O15-TRANS-NON-INTERIOR-RIGID" propTransNonInteriorRigid,
    Obligation "O15-REORDER-REQUIRED" propSigmaReorderRequired,
    Obligation "O15-REORDER-IDENTITY" propSigmaReorderIdentity,
    Obligation "O15-CONTEXT-FIND" propContextFind,
    Obligation "O15-CONTEXT-REJECT" propContextReject,
    Obligation "O15-EDGE-TRANSLATION" propEdgeTranslation,
    Obligation "O15-ELAB-LAMBDA-VAR" propElabLambdaVar,
    Obligation "O15-ELAB-LET-VAR" propElabLetVar,
    Obligation "O15-ELAB-ABS" propElabAbs,
    Obligation "O15-ELAB-APP" propElabApp,
    Obligation "O15-ELAB-LET" propElabLet,
    Obligation "O15-ENV-LAMBDA" propEnvLambda,
    Obligation "O15-ENV-LET" propEnvLet,
    Obligation "O15-ENV-WF" propEnvWf,
    Obligation "O15-TR-SEQ-EMPTY" propTrSeqEmpty,
    Obligation "O15-TR-SEQ-CONS" propTrSeqCons,
    Obligation "O15-TR-RIGID-RAISE" propTrRigidRaise,
    Obligation "O15-TR-RIGID-MERGE" propTrRigidMerge,
    Obligation "O15-TR-RIGID-RAISEMERGE" propTrRigidRaiseMerge,
    Obligation "O15-TR-ROOT-GRAFT" propTrRootGraft,
    Obligation "O15-TR-ROOT-RAISEMERGE" propTrRootRaiseMerge,
    Obligation "O15-TR-ROOT-WEAKEN" propTrRootWeaken,
    Obligation "O15-TR-NODE-GRAFT" propTrNodeGraft,
    Obligation "O15-TR-NODE-MERGE" propTrNodeMerge,
    Obligation "O15-TR-NODE-RAISEMERGE" propTrNodeRaiseMerge,
    Obligation "O15-TR-NODE-WEAKEN" propTrNodeWeaken,
    Obligation "O15-TR-NODE-RAISE" propTrNodeRaise,
    Obligation "O04-BIND-FLEX-CHILDREN" propBindingFlexChildren,
    Obligation "O04-BIND-INTERIOR" propBindingInterior,
    Obligation "O04-BIND-ORDER" propBindingOrder,
    Obligation "O04-OP-WEAKEN" propGraphWeaken,
    Obligation "O04-OP-RAISE-STEP" propGraphRaiseStep,
    Obligation "O04-OP-RAISE-TO" propGraphRaiseTo,
    Obligation "O05-INERT-NODES" propInertNodes,
    Obligation "O05-INERT-LOCKED" propInertLocked,
    Obligation "O05-WEAKEN-INERT" propInertWeaken,
    Obligation "O07-UNIF-CORE" propUnifyDecompose,
    Obligation "O07-UNIF-PRESOL" propPresolutionUnify,
    Obligation "O07-REBIND" propRebindHarmonize,
    Obligation "O07-GENUNIF" propGeneralizedUnify,
    Obligation "O08-REIFY-TYPE" propReifyType,
    Obligation "O08-REIFY-NAMES" propReifyNames,
    Obligation "O08-BIND-MONO" propBindMono,
    Obligation "O08-SYN-TO-GRAPH" propSynToGraph,
    Obligation "O08-REIFY-INLINE" propReifyInline,
    Obligation "O08-INLINE-PRED" propInlinePred,
    Obligation "O09-CGEN-ROOT" propCgenRoot,
    Obligation "O09-CGEN-EXPR" propCgenExpr,
    Obligation "O10-EXP-DECIDE" propExpDecide,
    Obligation "O10-EXP-APPLY" propExpApply,
    Obligation "O10-PROP-SOLVE" propPropSolve,
    Obligation "O10-PROP-WITNESS" propPropWitness,
    Obligation "O10-COPY-SCHEME" propCopyScheme,
    Obligation "O11-UNIFY-STRUCT" propUnifyDecompose,
    Obligation "O11-WITNESS-NORM" propWitnessNorm,
    Obligation "O11-WITNESS-COALESCE" propWitnessCoalesce,
    Obligation "O11-WITNESS-REORDER" propWitnessReorder,
    Obligation "O12-SOLVE-UNIFY" propSolveVar,
    Obligation "O12-ACYCLIC-CHECK" propAcyclicCheck,
    Obligation "O12-ACYCLIC-TOPO" propAcyclicTopo,
    Obligation "O12-COPY-INST" propCopyInst,
    Obligation "O12-NORM-GRAFT" propNormGraft,
    Obligation "O12-NORM-MERGE" propNormMerge,
    Obligation "O12-NORM-DROP" propNormDrop,
    Obligation "O12-NORM-FIXPOINT" propNormFixpoint,
    Obligation "O12-SOLVE-VAR-BASE" propSolveVarBase,
    Obligation "O12-SOLVE-VAR-VAR" propSolveVarVar,
    Obligation "O12-SOLVE-HARMONIZE" propSolveHarmonize,
    Obligation "O12-SOLVE-ARROW" propSolveArrow,
    Obligation "O12-SOLVE-VALIDATE" propSolveValidate
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

propUnifyDecompose :: Int -> Property
propUnifyDecompose size =
  let lhs = TyArrow (NodeId 0) (NodeId 1) (NodeId 2)
      rhs = TyArrow (NodeId 3) (NodeId (size + 10)) (NodeId (size + 11))
   in decomposeUnifyChildren lhs rhs
        === Right [UnifyEdge (NodeId 1) (NodeId (size + 10)), UnifyEdge (NodeId 2) (NodeId (size + 11))]

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
  typeCheckShouldMatch (Elab.typeCheck polyId) polyIdTy

propWfVar :: Int -> Property
propWfVar _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propInstReflex :: Int -> Property
propInstReflex _size =
  applyShouldBe intTy Elab.InstId intTy

propInstTrans :: Int -> Property
propInstTrans _size =
  applyShouldBe intTy (Elab.InstSeq Elab.InstIntro Elab.InstElim) intTy

propInstBot :: Int -> Property
propInstBot _size =
  applyShouldBe Elab.TBottom (Elab.InstBot intTy) intTy

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
  applyShouldBe forallA Elab.InstElim Elab.TBottom

propInstQuantIntro :: Int -> Property
propInstQuantIntro _size =
  case Elab.applyInstantiation intTy Elab.InstIntro of
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
    Left err -> expectationFailure (Elab.renderPipelineError err)

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
            ElabTypes.typeBinderRefsSameIdentity resultRef abstractedRef `shouldBe` True
            ElabTypes.resolvedVarDetails funVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
            ElabTypes.resolvedVarDetails argVar
              `shouldBe` ElabTypes.resolvedVarDetails binder
        _ -> expectationFailure ("unexpected annotated self-application body: " ++ show body)
    _ -> expectationFailure ("unexpected annotated self-application outer form: " ++ show term)

propEnvLambda :: Int -> Property
propEnvLambda _size =
  Elab.typeCheck idLam === Right (Elab.TArrow intTy intTy)

propEnvLet :: Int -> Property
propEnvLet _size =
  Elab.typeCheck (mkTestLocalLet "x" (Elab.schemeFromType intTy) (Elab.ELit (Surf.LInt 1)) (mkTestDeferredVar "x")) === Right intTy

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
