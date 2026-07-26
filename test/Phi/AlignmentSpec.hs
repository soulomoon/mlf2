{-# LANGUAGE GADTs #-}

module Phi.AlignmentSpec (spec) where

import qualified ElabTypeTestSupport as TestElab
import ElabTermTestSupport (testTForall, testTVar)
import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    , PresolutionView(..)
    , prEdgeExpansions
    , prEdgeTraces
    , prEdgeWitnesses
    , prIdentityEdges
    )
import MLF.Constraint.Presolution.Base
    ( EdgeSourceInterior(..)
    , InteriorNodes(..)
    )
import qualified MLF.Constraint.Finalize.TestSupport as Finalize
import MLF.Constraint.Presolution.TestSupport
    ( defaultPlanBuilder
    , edgeArtifactsForTest
    )
import MLF.Constraint.Types.Graph (BaseTy(..), EdgeId(..), NodeId(..), cBindParents, cNodes, toListNode)
import MLF.Constraint.Types.Witness
    ( InstanceOp(..)
    , ReplayContract(..)
    , ewLeft
    , ewRight
    , ewRoot
    , ewWitness
    , getInstanceOps
    )
import MLF.Constraint.Presolution.Plan.Context (emptyExpansionConstructionPlacements)
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Pipeline
    ( Instantiation(..)
    , Ty(..)
    , applyInstantiation
    , defaultTraceConfig
    , generalizeAtWithBuilder
    , reifyType
    , runPipelineElab
    , typeBinderRefsSameIdentity
    , typeCheck
    )
import MLF.Elab.Phi.Computation
    ( edgeTranslationSource
    , occurrenceComputationEdgeTranslation
    , occurrenceComputationInstantiation
    , occurrenceComputationReordering
    , occurrenceComputationSource
    , occurrenceComputationTarget
    , quantifierReorderingTarget
    )
import MLF.Elab.Phi.TestSupport (reifyInstWithSourceScheme)
import qualified MLF.Elab.Phi.TestSupport as PhiTestSupport
import MLF.Elab.Types
    ( ResolvedVar(..)
    , resolvedVarSameIdentity
    , schemeFromType
    , schemeInfoFromRefSubst
    )
import qualified MLF.Elab.Types as Elab
import MLF.Frontend.ConstraintGen (AnnExpr(..), instantiationSiteEdgeId)
import MLF.Frontend.Syntax (Expr(..), SrcTy(..), Lit(..))
import MLF.Reify.TypeOps (alphaEqType)
import SpecUtil (requireRight, unsafeNormalizeExpr, runPipelineArtifactsDefault, PipelineArtifacts(..), mkForalls)

spec :: Spec
spec = describe "Phi alignment" $ do
    describe "C1: witness-driven Phi produces valid instantiations" $ do
        let corpus =
                [ ("let-poly"
                  , ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                  )
                , ("ann-id"
                  , EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                  )
                ]
        forM_ corpus $ \(label, expr) ->
            it ("pipeline succeeds for: " ++ label) $ do
                let result = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                case result of
                    Left err -> expectationFailure (show err)
                    Right (term, ty) -> do
                        show term `shouldNotBe` ""
                        show ty `shouldNotBe` ""

        it "constructs the let-polymorphic identity RHS without a redundant Hyp" $ do
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (EVar "id"))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (show err)
                Right (term, _) -> do
                    let stripRootTypeAbstractions candidate =
                            case candidate of
                                Elab.ETyAbsRef _ _ body ->
                                    stripRootTypeAbstractions body
                                _ -> candidate
                    case stripRootTypeAbstractions term of
                        Elab.ELet _ _
                            (Elab.ETyAbsRef resultRef _
                                (Elab.ELam binder (Elab.EVarNode occurrence)))
                            _ -> do
                                resolvedVarSameIdentity binder occurrence `shouldBe` True
                                case resolvedVarType binder of
                                    TVarRef parameterRef
                                        | typeBinderRefsSameIdentity parameterRef resultRef ->
                                            pure ()
                                    parameterTy ->
                                        expectationFailure
                                            ("expected lambda parameter to use the Gamma result binder, got " ++ show parameterTy)
                        other ->
                            expectationFailure
                                ("expected TyAbs/Lam/bare-variable let RHS, got " ++ show other)

        it "pipeline succeeds for nested-let when forall binders carry graph identities" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELet "g" (EVar "f")
                            (EApp (EVar "g") (EVar "g")))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (show err)
                Right (term, ty) -> do
                    typeCheck term `shouldBe` Right ty
                    case ty of
                        TForallRef binderRef Nothing
                            (TArrow (TVarRef domRef) (TVarRef codRef))
                                | typeBinderRefsSameIdentity binderRef domRef
                                , typeBinderRefsSameIdentity binderRef codRef ->
                                    pure ()
                        other ->
                            expectationFailure
                                ("Expected forall a. a -> a, got: " ++ show other)

    describe "C2: replay metadata stays separate from frozen source provenance" $ do
        it "let-poly no-replay traces keep replay metadata empty without erasing binder args" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                result = runPipelineArtifactsDefault Set.empty expr
            case result of
                Left err -> expectationFailure err
                Right pa -> do
                    let pres = paPresolution pa
                        traces = IntMap.elems (prEdgeTraces pres)
                    forM_ traces $ \tr ->
                        -- The producer-owned replay domain is authoritative.
                        -- It can differ from the binders currently reachable
                        -- from etRoot after copy/normalization, which is why
                        -- EdgeTrace carries it explicitly.
                        when (etReplayContract tr == ReplayContractNone) $ do
                            etBinderReplayMap tr `shouldBe` IntMap.empty
                            etReplayDomainBinders tr `shouldBe` []

    describe "paper annotated self-application edge authority" $ do
        it "records exactly one terminal root RaiseMerge on the lambda-body edge" $ do
            let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr = ELamAnn "g" sigmaId (EApp (EVar "g") (EVar "g"))
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            let presolution = paPresolution artifacts
            bodyEdge <-
                case paAnnotated artifacts of
                    ALam _ _ _ _ _ edgeId _ -> pure edgeId
                    other -> do
                        expectationFailure
                            ("expected annotated lambda for g g, got " ++ show other)
                        fail "missing lambda-body edge"
            let EdgeId bodyEdgeKey = bodyEdge
            witness <-
                maybe
                    (expectationFailure "missing lambda-body witness" >> fail "missing witness")
                    pure
                    (IntMap.lookup bodyEdgeKey (prEdgeWitnesses presolution))
            trace <-
                maybe
                    (expectationFailure "missing lambda-body trace" >> fail "missing trace")
                    pure
                    (IntMap.lookup bodyEdgeKey (prEdgeTraces presolution))
            let
                -- Other occurrence edges in the application may have their
                -- own root operations.  Figure 15.3.5's Γ construction is
                -- governed specifically by the enclosing lambda-body edge.
                ops = getInstanceOps (ewWitness witness)
                rootRaiseMerges =
                    [ (operated, exterior)
                    | OpRaiseMerge operated exterior <- ops
                    , operated == etRoot trace
                    , null (etBinderArgs trace)
                    , IntMap.null (etBinderReplayMap trace)
                    , null (etReplayDomainBinders trace)
                    , etReplayContract trace == ReplayContractNone
                    ]
                terminalRootRaiseMerge =
                    case reverse ops of
                        OpRaiseMerge operated exterior : _ -> Just (operated, exterior)
                        _ -> Nothing
            case (rootRaiseMerges, terminalRootRaiseMerge) of
                ([(operated, exterior)], Just terminal) -> do
                    terminal `shouldBe` (operated, exterior)
                    let EdgeSourceInterior (InteriorNodes sourceInterior) = etInterior trace
                    IntSet.member (getNodeId operated) sourceInterior `shouldBe` True
                    IntSet.member (getNodeId exterior) sourceInterior `shouldBe` False
                    exterior `shouldBe` etResultRoot trace
                    etBinderArgs trace `shouldBe` []
                    etBinderReplayMap trace `shouldBe` IntMap.empty
                    etReplayDomainBinders trace `shouldBe` []
                    etReplayContract trace `shouldBe` ReplayContractNone
                other ->
                    expectationFailure
                        ( "expected one terminal root RaiseMerge, got "
                            ++ show other
                            ++ "; trace="
                            ++ show trace
                            ++ "; witness="
                            ++ show witness
                        )

        it "constructs production edges through the validated phi_R/T(e) split" $ do
            let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr = ELamAnn "g" sigmaId (EApp (EVar "g") (EVar "g"))
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            edgeIds <-
                case paAnnotated artifacts of
                    ALam
                        _
                        _
                        _
                        _
                        ( ALet
                            _
                            _
                            _
                            _
                            _
                            _
                            _
                            (ALetScope (AApp _ _ funEdge argEdge _) _ _)
                            _
                          )
                        _
                        _ ->
                        pure [funEdge, argEdge]
                    other -> do
                        expectationFailure
                            ("expected annotated g g application artifacts, got " ++ show other)
                        fail "missing g g application"
            let presolution = paPresolution artifacts
                view = Finalize.presolutionViewFromSolved (paSolved artifacts)
                generalizeAt mbGa =
                    generalizeAtWithBuilder
                        (defaultPlanBuilder defaultTraceConfig)
                        mbGa
                        view
            forM_ edgeIds $ \siteEdgeId -> do
                let EdgeId edgeKey = instantiationSiteEdgeId siteEdgeId
                witness <-
                    maybe
                        (expectationFailure ("missing edge witness " ++ show edgeKey) >> fail "missing witness")
                        pure
                        (IntMap.lookup edgeKey (prEdgeWitnesses presolution))
                trace <-
                    maybe
                        (expectationFailure ("missing edge trace " ++ show edgeKey) >> fail "missing trace")
                        pure
                        (IntMap.lookup edgeKey (prEdgeTraces presolution))
                occurrence <-
                    requireRight
                        ( PhiTestSupport.phiOccurrenceFromEdgeWitnessWithTraceForTest
                            defaultTraceConfig
                            generalizeAt
                            view
                            Nothing
                            Nothing
                            (Just trace)
                            witness
                        )
                compatibilityInst <-
                    requireRight
                        ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                            defaultTraceConfig
                            generalizeAt
                            view
                            Nothing
                            Nothing
                            (Just trace)
                            witness
                        )
                compatibilityInst
                    `shouldBe` occurrenceComputationInstantiation occurrence
                quantifierReorderingTarget
                    (occurrenceComputationReordering occurrence)
                    `shouldBe` edgeTranslationSource
                        (occurrenceComputationEdgeTranslation occurrence)
                applyInstantiation
                    (occurrenceComputationSource occurrence)
                    (occurrenceComputationInstantiation occurrence)
                    `shouldBe` Right (occurrenceComputationTarget occurrence)

        it "keeps raw edge translation separate from the application-owned sigma-id endpoint" $ do
            let sigmaId = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                sigmaIdType =
                    testTForall
                        "a"
                        Nothing
                        (TArrow (testTVar "a") (testTVar "a"))
                sigmaIdSchemeInfo =
                    schemeInfoFromRefSubst
                        (schemeFromType sigmaIdType)
                        IntMap.empty
                expr = ELamAnn "g" sigmaId (EApp (EVar "g") (EVar "g"))
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
            (funAnn, argAnn, funEdgeId, argEdgeId) <-
                case paAnnotated artifacts of
                    ALam
                        _
                        _
                        _
                        _
                        ( ALet
                            _
                            _
                            _
                            _
                            _
                            _
                            _
                            (ALetScope (AApp functionAnn argumentAnn funEdge argEdge _) _ _)
                            _
                          )
                        _
                        _ ->
                        pure (functionAnn, argumentAnn, funEdge, argEdge)
                    other -> do
                        expectationFailure ("expected annotated g g application artifacts, got " ++ show other)
                        fail "missing g g application"
            let presolution = paPresolution artifacts
                solved = paSolved artifacts
                view = Finalize.presolutionViewFromSolved solved
                generalizeAt mbGa =
                    generalizeAtWithBuilder
                        (defaultPlanBuilder defaultTraceConfig)
                        mbGa
                        view
                translateRaw (EdgeId edgeKey) = do
                    witness <-
                        maybe
                            (expectationFailure ("missing edge witness " ++ show edgeKey) >> fail "missing witness")
                            pure
                            (IntMap.lookup edgeKey (prEdgeWitnesses presolution))
                    trace <-
                        maybe
                            (expectationFailure ("missing edge trace " ++ show edgeKey) >> fail "missing trace")
                            pure
                            (IntMap.lookup edgeKey (prEdgeTraces presolution))
                    phi <-
                        requireRight
                            ( PhiTestSupport.phiFromEdgeWitnessWithTraceForTest
                                defaultTraceConfig
                                generalizeAt
                                view
                                Nothing
                                Nothing
                                (Just trace)
                                witness
                            )
                    pure (phi, witness, trace)
            (functionRawPhi, functionWitness, _functionTrace) <- translateRaw (instantiationSiteEdgeId funEdgeId)
            (argumentRawPhi, argumentWitness, argumentTrace) <- translateRaw (instantiationSiteEdgeId argEdgeId)

            -- Definition 15.3.12 permits any propagation witness for T(e),
            -- whose judgment is under the edge environment Gamma_e.  The raw
            -- function edge therefore applies the graph-domain expansion
            -- argument t -> t; its free binder is owned by that surrounding
            -- Gamma and must retain one identity across the arrow.  Section
            -- 15.3.8's reduced x[sigma-id] x form is recovered only after the
            -- annotation source scheme is selected below.
            functionGammaRef <- case functionRawPhi of
                InstApp
                    (TArrow (TVarRef domainRef) (TVarRef codomainRef))
                        | typeBinderRefsSameIdentity domainRef codomainRef ->
                            pure domainRef
                other ->
                    expectationFailure
                        ("expected raw function-edge T(e) to apply one identity-bearing Gamma arrow, got " ++ show other)
                        >> fail "missing raw function-edge Gamma identity"

            -- The argument edge applies that same Gamma identity directly.
            -- Together the raw pair is g[t -> t] (g[t]); the shared identity
            -- is essential.  Source-annotation transport below turns this
            -- graph-domain presentation into section 15.3.8's g[sigma-id] g.
            case argumentRawPhi of
                InstApp (TVarRef argumentRef)
                    | typeBinderRefsSameIdentity functionGammaRef argumentRef ->
                            pure ()
                other ->
                    expectationFailure
                        ( "expected raw argument-edge T(e) to apply the function edge's Gamma identity, got "
                            ++ show other
                            ++ "; source="
                            ++ show (reifyType view (ewRoot argumentWitness))
                            ++ "; left="
                            ++ show (reifyType view (ewLeft argumentWitness))
                            ++ "; right="
                            ++ show (reifyType view (ewRight argumentWitness))
                            ++ "; trace="
                            ++ show argumentTrace
                            ++ "; witness="
                            ++ show argumentWitness
                            ++ "; expansion="
                            ++ show
                                ( IntMap.lookup
                                    (getEdgeId (instantiationSiteEdgeId argEdgeId))
                                    (prEdgeExpansions presolution)
                                )
                            ++ "; binder-args="
                            ++ show
                                [ (binder, arg, reifyType view arg)
                                | (binder, arg) <- etBinderArgs argumentTrace
                                ]
                        )

            -- The witness root is the edge destination, after the source
            -- sigma-id has been instantiated with the graph-domain identity.
            case reifyType view (ewRoot functionWitness) of
                Right (TArrow (TVarRef domainRef) (TVarRef codomainRef))
                    | typeBinderRefsSameIdentity domainRef codomainRef ->
                            pure ()
                other ->
                    expectationFailure
                        ("expected the instantiated identity-arrow destination, got " ++ show other)

            let baseConstraint = paConstraintNorm artifacts
                canonical = pvCanonical view
                baseNodeKeys =
                    [ getNodeId nodeId
                    | (nodeId, _) <- toListNode (cNodes baseConstraint)
                    ]
                baseToSolved =
                    IntMap.fromList
                        [ (baseKey, canonical (NodeId baseKey))
                        | baseKey <- baseNodeKeys
                        ]
                solvedToBase =
                    foldl'
                        (\acc (baseKey, solvedNode) ->
                            IntMap.insertWith
                                (\_ existing -> existing)
                                (getNodeId solvedNode)
                                (NodeId baseKey)
                                acc
                        )
                        IntMap.empty
                        (IntMap.toList baseToSolved)
                gaParents =
                    GaBindParents
                        { gaBindParentsBase = cBindParents baseConstraint
                        , gaBaseConstraint = baseConstraint
                        , gaBaseToSolved = baseToSolved
                        , gaSolvedToBase = solvedToBase
                        , gaRestoredSchemeRootTargets = IntMap.empty
                        , gaExpansionConstructionPlacements = emptyExpansionConstructionPlacements
                        }
                edgeArtifacts =
                    edgeArtifactsForTest
                        (prEdgeExpansions presolution)
                        (prEdgeWitnesses presolution)
                        (prEdgeTraces presolution)
                        (prIdentityEdges presolution)
                translateFull ann edgeId =
                    reifyInstWithSourceScheme
                        defaultTraceConfig
                        (prPlanBuilder presolution)
                        view
                        gaParents
                        edgeArtifacts
                        sigmaIdSchemeInfo
                        ann
                        edgeId
            functionPhi <- requireRight (translateFull funAnn (instantiationSiteEdgeId funEdgeId))
            argumentPhi <- requireRight (translateFull argAnn (instantiationSiteEdgeId argEdgeId))
            case functionPhi of
                InstApp
                    (TArrow (TVarRef domainRef) (TVarRef codomainRef))
                        | typeBinderRefsSameIdentity domainRef codomainRef
                        , typeBinderRefsSameIdentity functionGammaRef domainRef ->
                            pure ()
                other ->
                    expectationFailure
                        ("expected edge-local [phi_R;T(e)] to retain its graph-domain identity arrow, got " ++ show other)
            case argumentPhi of
                InstApp (TVarRef argumentRef)
                    | typeBinderRefsSameIdentity functionGammaRef argumentRef ->
                        pure ()
                other ->
                    expectationFailure
                        ("expected edge-local argument computation to retain the shared graph-domain identity, got " ++ show other)

            -- Section 15.3.8's reduced term needs the application topology in
            -- addition to the two edge-local computations.  The production
            -- algebra owns that endpoint and must construct (g[sigma-id] g),
            -- followed by the enclosing result abstraction.
            (productionTerm, _) <-
                requireRight
                    (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
            case productionTerm of
                Elab.ETyAbsRef _ _
                    (Elab.ELam _
                        (Elab.ETyInst
                            (Elab.EApp
                                (Elab.ETyInst _ (InstApp functionArgumentTy))
                                (Elab.EVarNode _))
                            (InstAbstrRef _))) ->
                                alphaEqType functionArgumentTy sigmaIdType `shouldBe` True
                other ->
                    expectationFailure
                        ("expected production (g[sigma-id] g)[result] construction, got " ++ show other)

    describe "C3: Omega resolves binders without class-member fallback when trace available" $ do
        let corpus =
                [ ("let-poly", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                ]
        forM_ corpus $ \(label, expr) ->
            it ("pipeline still succeeds for: " ++ label) $ do
                let result = runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                case result of
                    Left err -> expectationFailure (show err)
                    Right _ -> pure ()

    describe "C4: A6 bounded-alias coercion regressions stay green" $ do
        it "bounded-alias coercion path succeeds in the canonical pipeline" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr =
                    ELet "c" (EAnn rhs schemeTy)
                        (EAnn (EVar "c") ann)
                normExpr = unsafeNormalizeExpr expr
            case runPipelineElab Set.empty normExpr of
                Left err -> expectationFailure (show err)
                Right (term, ty) -> do
                    typeCheck term `shouldBe` Right ty
                    show ty `shouldNotBe` ""

        it "applied bounded-coercion path succeeds in the canonical pipeline" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr =
                    ELet "c" (EAnn rhs schemeTy)
                        (EApp
                            (EApp (EAnn (EVar "c") ann) (ELit (LInt 1)))
                            (ELit (LInt 2)))
                normExpr = unsafeNormalizeExpr expr
                expectedTy = TestElab.tBase (BaseTy "Int")
            let expectInt label result =
                    case result of
                        Left err ->
                            expectationFailure (label ++ " failed: " ++ show err)
                        Right (term, ty) -> do
                            ty `shouldBe` expectedTy
                            typeCheck term `shouldBe` Right expectedTy
            expectInt "canonical pipeline" (runPipelineElab Set.empty normExpr)
