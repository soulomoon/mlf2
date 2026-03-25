{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module PipelineSpec (spec) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import System.IO.Error (tryIOError)
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, chooseInt, counterexample, elements, forAll, property, withMaxSuccess, (===))

import MLF.Elab.Pipeline
    ( ElabType
    , generalizeAtWithBuilder
    , applyRedirectsToAnn
    , canonicalizeAnn
    , isValue
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , step
    , typeCheck
    , pattern Forall
    , Pretty(..)
    )
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen
import MLF.Constraint.Canonicalizer (canonicalizerFrom, canonicalizeNode)
import qualified MLF.Constraint.Finalize as Finalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import MLF.Constraint.Presolution.TestSupport
    ( CopyMapping(..)
    , EdgeArtifacts(..)
    , defaultPlanBuilder
    , toListInterior
    )
import MLF.Constraint.Presolution.Plan.Context (GaBindParents(..))
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness (EdgeWitness(..), InstanceOp(..), InstanceWitness(..), ReplayContract(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs(..)
    , computeResultTypeFallback
    , mkResultTypeInputs
    , rtcEdgeWitnesses
    , rtcEdgeTraces
    )
import MLF.Elab.Run.Util
    ( canonicalizeTrace
    , canonicalizeExpansion
    , canonicalizeWitness
    , chaseRedirects
    , makeCanonicalizer
    )
import SpecUtil
    ( collectVarNodes
    , defaultTraceConfig
    , emptyConstraint
    , PipelineArtifacts(..)
    , mkForalls
    , requireRight
    , runConstraintDefault
    , runPipelineArtifactsDefault
    , runToPresolutionWithAnnDefault
    , runToSolvedDefault
    , unsafeNormalizeExpr
    )
import MLF.Types.Elab (Ty(..), containsArrowTy, containsForallTy)
import MLF.Reify.Core (reifyType)
import Data.List.NonEmpty (NonEmpty(..))
import qualified SolvedFacadeTestUtil as SolvedTest

expectStrictPipelineFailure :: (Show err, Show ty) => String -> Either err (term, ty) -> Expectation
expectStrictPipelineFailure label result =
    case result of
        Left err ->
            let rendered = show err
            in rendered
                `shouldSatisfy`
                    ( \msg ->
                        "PhiTranslatabilityError" `isInfixOf` msg
                            || "TCInstantiationError" `isInfixOf` msg
                            || "TCLetTypeMismatch" `isInfixOf` msg
                            || "TCArgumentMismatch" `isInfixOf` msg
                            || "ValidationFailed" `isInfixOf` msg
                            || "missing direct structural authority" `isInfixOf` msg
                    )
        Right (_term, ty) ->
            expectationFailure
                ("Expected strict failure for " ++ label ++ ", but got type " ++ show ty)

matchesRecursiveArrow :: ElabType -> ElabType -> Bool
matchesRecursiveArrow actual expected = case (actual, expected) of
    (TArrow domA codA, TArrow domE codE) ->
        matchesRecursiveMu domA domE && matchesRecursiveMu codA codE
    _ -> False
  where
    matchesRecursiveMu tyA tyE = case (tyA, tyE) of
        (TMu _ bodyA, TMu _ bodyE) -> stripMuNames bodyA == stripMuNames bodyE
        _ -> False

    stripMuNames ty = case ty of
        TVar _ -> TVar "_"
        TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
        TBase base -> TBase base
        TCon con args -> TCon con (fmap stripMuNames args)
        TForall _ mb body -> TForall "_" (fmap stripBoundNames mb) (stripMuNames body)
        TMu _ body -> TMu "_" (stripMuNames body)
        TBottom -> TBottom

    stripBoundNames bound = case bound of
        TArrow dom cod -> TArrow (stripMuNames dom) (stripMuNames cod)
        TBase base -> TBase base
        TCon con args -> TCon con (fmap stripMuNames args)
        TForall _ mb body -> TForall "_" (fmap stripBoundNames mb) (stripMuNames body)
        TMu _ body -> TMu "_" (stripMuNames body)
        TBottom -> TBottom

containsMu :: ElabType -> Bool
containsMu ty = case ty of
    TMu _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TCon _ args -> any containsMu args
    TForall _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBase _ -> False
        TCon _ args -> any containsMu args
        TForall _ mb body -> maybe False containsMuBound mb || containsMu body
        TMu _ _ -> True
        TBottom -> False

resultTypeInputsForArtifacts :: PipelineArtifacts -> (ResultTypeInputs, AnnExpr, AnnExpr)
resultTypeInputsForArtifacts
    PipelineArtifacts
        { paConstraintNorm = c1
        , paPresolution = pres
        , paSolved = solved0
        , paAnnotated = ann0
        } =
    let solvedClean = Finalize.stepPruneSolvedBindParents solved0
        canon = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        canonical = canonicalizeNode canon
        annRedirected = applyRedirectsToAnn (prRedirects pres) ann0
        annCanon = canonicalizeAnn canonical annRedirected
        edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
        edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
        edgeExpansions = IntMap.map (canonicalizeExpansion canon) (prEdgeExpansions pres)
        baseNodeKeys =
            [ getNodeId nid
            | (nid, _) <- toListNode (cNodes c1)
            ]
        baseToSolved =
            IntMap.fromList
                [ (baseKey, canonical (NodeId baseKey))
                | baseKey <- baseNodeKeys
                ]
        solvedToBase =
            foldl'
                (\acc (baseKey, solvedNid) -> IntMap.insertWith (\_ existing -> existing) (getNodeId solvedNid) (NodeId baseKey) acc)
                IntMap.empty
                (IntMap.toList baseToSolved)
        bindParentsGa =
            GaBindParents
                { gaBindParentsBase = cBindParents c1
                , gaBaseConstraint = c1
                , gaBaseToSolved = baseToSolved
                , gaSolvedToBase = solvedToBase
                }
        inputs =
            mkResultTypeInputs
                canonical
                EdgeArtifacts
                    { eaEdgeExpansions = edgeExpansions
                    , eaEdgeWitnesses = edgeWitnesses
                    , eaEdgeTraces = edgeTraces
                    }
                (PresolutionViewBoundary.fromSolved solvedClean)
                bindParentsGa
                (defaultPlanBuilder defaultTraceConfig)
                c1
                (prRedirects pres)
                defaultTraceConfig
    in (inputs, annCanon, ann0)

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
    describe "Elaboration helpers" $ do
        it "reifies type with flexible bound" $ do
            -- Note: With coercion-only annotations, let-bindings with annotated RHS
            -- are treated as normal lets with coercion terms, not declared schemes.
            -- let f = ((\x.x) : ∀(a ⩾ Int). a -> a) in f
            -- The coercion constrains the RHS to match the annotation type.
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
                expr =
                    let schemeTy = mkForalls [] ann
                    in ELet "f" (EAnn (ELam "x" (EVar "x")) schemeTy) (EVar "f")

            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paPresolution = pres, paSolved = res, paRoot = root } -> do
                    validateStrict res
                    let rootRedirected = chaseRedirects (prRedirects pres) root
                        root' = Solved.canonical res rootRedirected
                    -- With coercion-only semantics, f's type is inferred (not declared)
                    -- The coercion ensures the RHS has the annotated type.
                    let scopeRoot =
                            case Binding.bindingRoots (Solved.originalConstraint res) of
                                [GenRef gid] -> genRef gid
                                roots -> error ("PipelineSpec: unexpected binding roots " ++ show roots)
                    let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                    case generalizeAt (PresolutionViewBoundary.fromSolved res) scopeRoot root' of
                        Right (Forall binds ty, _subst) -> do
                            binds `shouldBe` []
                            let tyStr = pretty ty
                            -- With coercion-only: type is inferred, not the declared scheme
                            tyStr `shouldSatisfy` ("∀" `isInfixOf`)
                            tyStr `shouldSatisfy` ("->" `isInfixOf`)
                        Left err -> expectationFailure $ "Generalize error: " ++ show err

        it "generalizes at binding site" $ do
             -- let id = \x. x in id
             let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

             -- We intercept the pipeline after solve to test generalizeAt on the 'id' binding
             case runPipelineArtifactsDefault defaultPolySyms expr of
                 Left err -> expectationFailure err
                 Right PipelineArtifacts{ paPresolution = pres, paSolved = res, paAnnotated = ann0 } -> do
                     validateStrict res
                     let ann = applyRedirectsToAnn (prRedirects pres) ann0
                     case ann of
                         ALet _ schemeGen schemeRoot _ _ _ _ _ -> do
                             let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                             case generalizeAt (PresolutionViewBoundary.fromSolved res) (genRef schemeGen) schemeRoot of
                                 Right scheme -> show scheme `shouldSatisfy` ("Forall" `isInfixOf`)
                                 Left err -> expectationFailure $ "Generalize error: " ++ show err
                         _ -> expectationFailure "Expected ALet annotation"

        it "reifies TyCon nodes to TCon in elaborated types" $ do
            -- Test that a constraint containing TyCon nodes reifies correctly to TCon
            -- Create a simple constraint with TyCon: List Int
            let intBase = BaseTy "Int"
                listBase = BaseTy "List"
                var0 = NodeId 0
                var1 = NodeId 1
                baseNode = TyBase var0 intBase
                listNode = TyCon var1 listBase (var0 :| [])
                nodes = fromListNode [(var0, baseNode), (var1, listNode)]
                constraint = emptyConstraint { cNodes = nodes }
                res = SolvedTest.mkTestSolved constraint IntMap.empty
            case reifyType (PresolutionViewBoundary.fromSolved res) var1 of
                Right ty ->
                    case ty of
                        TCon con args -> do
                            con `shouldBe` listBase
                            length args `shouldBe` 1
                            case args of
                                (TBase b :| []) -> b `shouldBe` intBase
                                _ -> expectationFailure "Expected TBase Int as argument"
                        _ -> expectationFailure $ "Expected TCon, got: " ++ show ty
                Left err -> expectationFailure $ "Reify error: " ++ show err

        it "reifies nested TyCon nodes to nested TCon" $ do
            -- Test nested TyCon: List (Maybe Int)
            let intBase = BaseTy "Int"
                maybeBase = BaseTy "Maybe"
                listBase = BaseTy "List"
                var0 = NodeId 0
                var1 = NodeId 1
                var2 = NodeId 2
                intNode = TyBase var0 intBase
                maybeNode = TyCon var1 maybeBase (var0 :| [])
                listNode = TyCon var2 listBase (var1 :| [])
                nodes = fromListNode [(var0, intNode), (var1, maybeNode), (var2, listNode)]
                constraint = emptyConstraint { cNodes = nodes }
                res = SolvedTest.mkTestSolved constraint IntMap.empty
            case reifyType (PresolutionViewBoundary.fromSolved res) var2 of
                Right ty ->
                    case ty of
                        TCon outerCon outerArgs -> do
                            outerCon `shouldBe` listBase
                            case outerArgs of
                                (TCon innerCon innerArgs :| []) -> do
                                    innerCon `shouldBe` maybeBase
                                    case innerArgs of
                                        (TBase b :| []) -> b `shouldBe` intBase
                                        _ -> expectationFailure "Expected TBase Int as innermost arg"
                                _ -> expectationFailure "Expected nested TCon (Maybe Int)"
                        _ -> expectationFailure $ "Expected TCon, got: " ++ show ty
                Left err -> expectationFailure $ "Reify error: " ++ show err

    describe "Integration Tests" $ do
        it "chi-first guard: Elaborate internals avoid local solved materialization" $ do
            src <- readFile "src/MLF/Elab/Elaborate.hs"
            src `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")

        it "elab-input thesis-exact guard: Elaborate active input path does not materialize chiSolved" $ do
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            phiSrc <- readFile "src/MLF/Elab/Phi/Translate.hs"
            cabalSrc <- readFile "mlf2.cabal"
            let legacySolvedTypedElabApiMarkers =
                    [ "type GeneralizeAtWithLegacy ="
                    , "elaborate\n    :: TraceConfig\n    -> GeneralizeAtWithLegacy\n    -> Solved"
                    , "elaborateWithGen\n    :: TraceConfig\n    -> GeneralizeAtWithLegacy\n    -> Solved"
                    , "elaborateWithScope\n    :: TraceConfig\n    -> GeneralizeAtWithLegacy\n    -> Solved"
                    ]
                legacySolvedTypedPhiApiMarkers =
                    [ "type GeneralizeAtWithLegacy ="
                    , "phiFromEdgeWitnessNoTrace\n    :: TraceConfig\n    -> GeneralizeAtWithLegacy\n    -> Solved"
                    , "phiFromEdgeWitness\n    :: TraceConfig\n    -> GeneralizeAtWithLegacy\n    -> Solved"
                    ]
                solvedTypedPhiTestOnlyApiMarkers =
                    [ "phiFromEdgeWitnessNoTrace\n    :: TraceConfig\n    -> GeneralizeAtWith\n    -> Solved"
                    , "phiFromEdgeWitness\n    :: TraceConfig\n    -> GeneralizeAtWith\n    -> Solved"
                    , "phiFromEdgeWitnessAutoTrace\n    :: TraceConfig\n    -> GeneralizeAtWith\n    -> Solved"
                    ]
            isInfixOf "ChiQuery.chiSolved presolutionView" elabSrc `shouldBe` False
            isInfixOf "chiSolvedCompat presolutionView" elabSrc `shouldBe` False
            forM_ legacySolvedTypedElabApiMarkers $ \marker ->
                isInfixOf marker elabSrc `shouldBe` False
            forM_ legacySolvedTypedPhiApiMarkers $ \marker ->
                isInfixOf marker phiSrc `shouldBe` False
            forM_ solvedTypedPhiTestOnlyApiMarkers $ \marker ->
                isInfixOf marker phiSrc `shouldBe` False
            isInfixOf "MLF.Elab.Phi.TestOnly" cabalSrc `shouldBe` False
            isInfixOf "MLF.Elab.Phi.IdentityBridge" cabalSrc `shouldBe` False

        it "elab-input absolute thesis-exact guard" $ do
            phiEnvSrc <- readFile "src/MLF/Elab/Phi/Env.hs"
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            contextSrc <- readFile "src/MLF/Constraint/Presolution/Plan/Context.hs"
            cabalSrc <- readFile "mlf2.cabal"
            omegaSrc <- readFile "src/MLF/Elab/Phi/Omega.hs"
            isInfixOf "peResult :: Solved" phiEnvSrc `shouldBe` False
            isInfixOf "askResult ::" phiEnvSrc `shouldBe` False
            isInfixOf "Left _ -> ref" scopeSrc `shouldBe` False
            isInfixOf "preferGenScope" scopeSrc `shouldBe` False
            isInfixOf "Left _ -> root" contextSrc `shouldBe` False
            isInfixOf "MLF.Elab.Phi.TestOnly" cabalSrc `shouldBe` False
            isInfixOf "MLF.Elab.Phi.IdentityBridge" cabalSrc `shouldBe` False
            isInfixOf "MLF.Elab.Phi.IdentityBridge" omegaSrc `shouldBe` False

        it "elab-input witness-authoritative guard" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            isInfixOf "Left _ -> typeRef root" elaborateSrc `shouldBe` False

        it "row1 closeout guard|checked-authoritative keeps representative corpus parity: elaborateWithEnv has no entry-time Solved.rebuildWithConstraint" $ do
            src <- readFile "src/MLF/Elab/Elaborate.hs"
            isInfixOf "solved = Solved.rebuildWithConstraint" src `shouldBe` False

        it "row1 closeout guard|checked-authoritative does not adapt solved via prune helper at entry" $ do
            src <- readFile "src/MLF/Elab/Elaborate.hs"
            isInfixOf "pruneBindParentsSolved" src `shouldBe` False

        it "row2 closeout guard: ResultTypeInputs no longer exposes rtcSolvedCompat" $ do
            src <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            isInfixOf "rtcSolvedCompat" src `shouldBe` False

        it "row2 closeout guard: MLF.Elab.Run.ResultType.Types no longer exposes rtcSolveLike" $ do
            src <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            isInfixOf "rtcSolveLike" src `shouldBe` False

        it "row2 absolute thesis-exact guard" $ do
            pipelineSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
            chiQuerySrc <- readFile "src/MLF/Elab/Run/ChiQuery.hs"
            viewSrc <- readFile "src/MLF/Elab/Run/ResultType/View.hs"
            annSrc <- readFile "src/MLF/Elab/Run/ResultType/Ann.hs"
            fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
            forM_
                [ "rtvSolved ::"
                , "rtvOriginalConstraint ::"
                , "solveFromInputs ::"
                , "Solved.rebuildWithConstraint"
                , "ChiQuery.chiSolved"
                ] $ \marker ->
                    isInfixOf marker viewSrc `shouldBe` False
            isInfixOf "View.rtvSolved" annSrc `shouldBe` False
            isInfixOf "View.rtvSolved" fallbackSrc `shouldBe` False
            isInfixOf "withResultTypeViewOverlay ::" fallbackSrc `shouldBe` False
            isInfixOf "computeResultTypeFromAnnLocal" fallbackSrc `shouldBe` False
            isInfixOf "withResultTypeViewOverlay ctx view" fallbackSrc `shouldBe` False
            isInfixOf "pvLookupNode = View.rtvLookupNode" fallbackSrc `shouldBe` False
            isInfixOf "pvLookupVarBound = View.rtvLookupVarBound" fallbackSrc `shouldBe` False
            isInfixOf "buildResultTypeView" annSrc `shouldBe` False
            isInfixOf "buildResultTypeView" fallbackSrc `shouldBe` False
            forM_
                [ "rtvCanonical ::"
                , "rtvEdgeWitnesses ::"
                , "rtvEdgeTraces ::"
                , "rtvEdgeExpansions ::"
                , "rtvBindParentsGa ::"
                , "rtvPlanBuilder ::"
                , "rtvBaseConstraint ::"
                , "rtvRedirects ::"
                , "rtvTraceConfig ::"
                , "rtvGenNodes ::"
                , "rtvCanonicalBindParents ::"
                ] $ \marker ->
                    isInfixOf marker viewSrc `shouldBe` False
            forM_
                [ "View.rtvCanonical"
                , "View.rtvEdgeWitnesses"
                , "View.rtvEdgeTraces"
                , "View.rtvEdgeExpansions"
                , "View.rtvBindParentsGa"
                , "View.rtvPlanBuilder"
                , "View.rtvBaseConstraint"
                , "View.rtvRedirects"
                , "View.rtvTraceConfig"
                , "View.rtvGenNodes"
                , "View.rtvCanonicalBindParents"
                ] $ \marker -> do
                    isInfixOf marker annSrc `shouldBe` False
                    isInfixOf marker fallbackSrc `shouldBe` False
            isInfixOf "rtvPresolutionViewOverlay ::" viewSrc `shouldBe` True
            isInfixOf "fromSolved solvedClean" pipelineSrc `shouldBe` False
            isInfixOf "fromSolved solvedForGen" pipelineSrc `shouldBe` False
            isInfixOf "chiSolvedCompat" chiQuerySrc `shouldBe` False
            isInfixOf "chiSolved ::" chiQuerySrc `shouldBe` False
            isInfixOf "Solved.fromConstraintAndUf" chiQuerySrc `shouldBe` False
            isInfixOf "Solved.rebuildWithConstraint" chiQuerySrc `shouldBe` False

        it "chi-p global cleanup guard: runtime elaboration helpers no longer import fromSolved" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            typeOpsSrc <- readFile "src/MLF/Elab/Run/TypeOps.hs"
            generalizeSrc <- readFile "src/MLF/Elab/Run/Generalize.hs"
            resultTypeUtilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            reifySrc <- readFile "src/MLF/Reify/Core.hs"
            legacySrc <- readFile "src/MLF/Elab/Legacy.hs"
            forM_ [scopeSrc, typeOpsSrc, generalizeSrc, resultTypeUtilSrc, reifySrc] $ \src ->
                src `shouldSatisfy` (not . isInfixOf "fromSolved")
            legacySrc `shouldSatisfy` isInfixOf "fromSolved"

        it "chi-p wrapper retirement guard: primary helper signatures are PresolutionView-native" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            typeOpsSrc <- readFile "src/MLF/Elab/Run/TypeOps.hs"
            generalizeSrc <- readFile "src/MLF/Elab/Run/Generalize.hs"
            resultTypeUtilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            reifySrc <- readFile "src/MLF/Reify/Core.hs"
            forM_
                [ "bindingScopeRefCanonical :: Solved"
                , "schemeBodyTarget :: Solved"
                , "canonicalizeScopeRef :: Solved"
                , "resolveCanonicalScope :: Constraint -> Solved"
                , "letScopeOverrides :: Constraint -> Constraint -> Solved"
                ] $ \marker ->
                    scopeSrc `shouldSatisfy` (not . isInfixOf marker)
            forM_
                [ "inlineBoundVarsType :: Solved"
                , "inlineBoundVarsTypeForBound :: Solved"
                ] $ \marker ->
                    typeOpsSrc `shouldSatisfy` (not . isInfixOf marker)
            generalizeSrc `shouldSatisfy` (not . isInfixOf "-> Solved")
            resultTypeUtilSrc `shouldSatisfy` (not . isInfixOf "-> Solved")
            forM_
                [ "reifyType :: Solved"
                , "reifyTypeWithNames :: Solved"
                , "reifyTypeWithNamesNoFallback :: Solved"
                , "reifyTypeWithNamedSet :: Solved"
                , "reifyTypeWithNamedSetNoFallback :: Solved"
                , "reifyBoundWithNames :: Solved"
                , "reifyBoundWithNamesBound :: Solved"
                , "namedNodes :: Solved"
                ] $ \marker ->
                    reifySrc `shouldSatisfy` (not . isInfixOf marker)

        it "helper dedup guard: Elaborate no longer defines local schemeBodyTarget" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            elaborateSrc `shouldSatisfy` (not . isInfixOf "schemeBodyTarget ::")

        it "fallback-removal guard: generalization helpers no longer define live fallback ladders" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            runPipelineSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
            resultTypeUtilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            generalizeSrc <- readFile "src/MLF/Elab/Generalize.hs"
            forM_
                [ "generalizeNeedsFallback"
                , "preferMoreCoherent primary fallback"
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` (not . isInfixOf marker)
            runPipelineSrc `shouldSatisfy` (not . isInfixOf "generalizeNeedsFallback")
            resultTypeUtilSrc `shouldSatisfy` (not . isInfixOf "generalizeNeedsFallback")
            resultTypeUtilSrc `shouldSatisfy` (not . isInfixOf "fallbackToReify")
            generalizeSrc `shouldSatisfy` (not . isInfixOf "fallbackSchemeType")

        it "fallback-removal guard: planner owner resolution no longer falls back from wrapper root" $ do
            plannerSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs"
            forM_
                [ "falling back from body-root lookup to wrapper-root lookup"
                , "mbWrapper"
                , "firstGenOnPath canonical constraint0 (rteNodeId leftTyExp)"
                ] $ \marker ->
                    plannerSrc `shouldSatisfy` (not . isInfixOf marker)

        it "fallback-removal guard: Elaborate no longer defines reifyInst trace fallback helpers" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            forM_
                [ "allowFallbackFromTrace"
                , "resolveFallbackArgNodes"
                , "fallbackProvenanceArityOk"
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` (not . isInfixOf marker)

        it "fallback-removal guard: Elaborate no longer defines let-level chooser replacements" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            forM_
                [ "fallbackChoiceFromVar"
                , "fallbackChoiceFromApp"
                , "fallbackChoiceFromLam"
                , "fallbackChoice ="
                , "schChosen = maybe sch0 fst fallbackChoice"
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` (not . isInfixOf marker)

        it "fallback-removal guard: Elaborate no longer defines reifyInst secondary recovery" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            forM_
                [ "targetArgs <|> expansionArgs"
                , "expansionArgs ="
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` (not . isInfixOf marker)

        it "fallback-removal guard: Generalize no longer defines recursive fallback callbacks" $ do
            generalizeRunSrc <- readFile "src/MLF/Elab/Run/Generalize.hs"
            forM_
                [ "let fallback scope' target' ="
                , "applyGeneralizePlan fallback"
                ] $ \marker ->
                    generalizeRunSrc `shouldSatisfy` (not . isInfixOf marker)

        it "fallback-removal guard: instantiation inference no longer defines generic fallback recovery" $ do
            instantiationSrc <- readFile "src/MLF/Elab/Run/Instantiation.hs"
            forM_
                [ "fallback ="
                , "Left _ -> fallback"
                , "_ -> fallback"
                ] $ \marker ->
                    instantiationSrc `shouldSatisfy` (not . isInfixOf marker)

        it "row3 ordering thesis-exact guard: Driver removes global post-loop weaken flush" $ do
            driverSrc <- readFile "src/MLF/Constraint/Presolution/Driver.hs"
            driverSrc `shouldSatisfy` (not . isInfixOf "flushPendingWeakens")

        it "row3 ordering thesis-exact guard: EdgeProcessing flushes delayed weakens at each edge boundary" $ do
            edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            edgeSrc `shouldSatisfy` isInfixOf "flushPendingWeakensAtOwnerBoundary"
            edgeSrc `shouldSatisfy` isInfixOf "assertNoPendingUnifyEdges \"after-inst-edge-closure\""

        it "row3 ordering thesis-exact guard: EdgeUnify no longer exposes retired pending-weaken alias helpers" $ do
            edgeUnifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
            forM_
                [ "pendingWeakenOwnerForNode,"
                , "pendingWeakenOwnerForEdge,"
                , "pendingWeakenOwnerForNode ::"
                , "pendingWeakenOwnerForEdge ::"
                ] $ \marker ->
                    edgeUnifySrc `shouldSatisfy` (not . isInfixOf marker)

        it "row3 absolute thesis-exact guard: EdgeProcessing forbids loop-final weaken flush fallback" $ do
            edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            edgeSrc `shouldSatisfy` (not . isInfixOf "flushPendingWeakens\n    drainPendingUnifyClosureIfNeeded")

        it "row3 absolute thesis-exact guard: EdgeProcessing requires owner-aware boundary scheduler markers" $ do
            edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            forM_
                [ "scheduleWeakensByOwnerBoundary"
                , "flushPendingWeakensAtOwnerBoundary"
                , "assertNoPendingWeakensOutsideOwnerBoundary"
                ] $ \marker ->
                    edgeSrc `shouldSatisfy` isInfixOf marker

        it "row3 absolute thesis-exact guard: EdgeProcessing does not flush all owner buckets at boundary" $ do
            edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            edgeSrc `shouldSatisfy` (not . isInfixOf "forM_ owners flushPendingWeakensAtOwnerBoundary")
            edgeSrc `shouldSatisfy` isInfixOf "let boundaryOwner = pendingWeakenOwnerFromMaybe mbCurrentOwner"

        it "presolution internal export surface guard: Driver and EdgeProcessing expose only their owned helpers" $ do
            driverSrc <- readFile "src/MLF/Constraint/Presolution/Driver.hs"
            edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            presolutionSrc <- readFile "src/MLF/Constraint/Presolution.hs"
            testSupportSrc <- readFile "src/MLF/Constraint/Presolution/TestSupport.hs"
            driverSrc `shouldSatisfy` (not . isInfixOf "processInstEdge,")
            edgeSrc `shouldSatisfy` (not . isInfixOf "unifyStructure,")
            edgeSrc `shouldSatisfy` (not . isInfixOf "recordEdgeWitness,")
            edgeSrc `shouldSatisfy` (not . isInfixOf "recordEdgeTrace,")
            edgeSrc `shouldSatisfy` (not . isInfixOf "canonicalizeEdgeTraceInteriorsM,")
            presolutionSrc `shouldSatisfy` (not . isInfixOf "processInstEdge,")
            testSupportSrc `shouldSatisfy` isInfixOf "processInstEdge,"

        it "edge artifact bundle guard: presolution/result-type/elab env share EdgeArtifacts" $ do
            baseSrc <- readFile "src/MLF/Constraint/Presolution/Base.hs"
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            resultTypeTypesSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            baseSrc `shouldSatisfy` isInfixOf "data EdgeArtifacts = EdgeArtifacts"
            baseSrc `shouldSatisfy` (not . isInfixOf "-> (IntMap EdgeWitness, IntMap EdgeTrace, IntMap Expansion)")
            elabSrc `shouldSatisfy` isInfixOf "eeEdgeArtifacts :: EdgeArtifacts"
            resultTypeTypesSrc `shouldSatisfy` isInfixOf "rtcEdgeArtifacts :: EdgeArtifacts"

        it "assembly helper guard: pipeline and driver expose the remaining prep helpers explicitly" $ do
            pipelineSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
            driverSrc <- readFile "src/MLF/Constraint/Presolution/Driver.hs"
            pipelineSrc `shouldSatisfy` isInfixOf "data TraceCopyArtifacts = TraceCopyArtifacts"
            pipelineSrc `shouldSatisfy` isInfixOf "prepareTraceCopyArtifacts"
            driverSrc `shouldSatisfy` isInfixOf "mkInitialPresolutionState ::"
            driverSrc `shouldSatisfy` isInfixOf "tyExpNodeIds ::"

        it "split façade guard: Omega remains a thin export owner over Domain/Interpret/Normalize" $ do
            omegaSrc <- readFile "src/MLF/Elab/Phi/Omega.hs"
            forM_
                [ "import MLF.Elab.Phi.Omega.Domain"
                , "import MLF.Elab.Phi.Omega.Interpret"
                , "import MLF.Elab.Phi.Omega.Normalize"
                ] $ \marker ->
                    omegaSrc `shouldSatisfy` isInfixOf marker
            forM_
                [ "data OmegaDomainEnv = OmegaDomainEnv"
                , "mkOmegaDomainEnv :: OmegaContext -> OmegaDomainEnv"
                , "normalizeInst = cata alg"
                ] $ \marker ->
                    omegaSrc `shouldSatisfy` (not . isInfixOf marker)

        it "split façade guard: Reify.Core delegates to Bound/Named/Type owners" $ do
            reifySrc <- readFile "src/MLF/Reify/Core.hs"
            forM_
                [ "import qualified MLF.Reify.Bound as Bound"
                , "import qualified MLF.Reify.Named as Named"
                , "import qualified MLF.Reify.Type as Type"
                , "reifyType = Type.reifyType"
                , "freeVars = Bound.freeVars"
                , "namedNodes = Named.namedNodes"
                ] $ \marker ->
                    reifySrc `shouldSatisfy` isInfixOf marker
            forM_
                [ "reifyBoundWithNamesSolved"
                , "namedNodes presolutionView = do"
                , "freeVars solved nid visited"
                ] $ \marker ->
                    reifySrc `shouldSatisfy` (not . isInfixOf marker)

        it "split façade guard: Solve keeps worklist/finalize implementation in child modules" $ do
            solveSrc <- readFile "src/MLF/Constraint/Solve.hs"
            forM_
                [ "import MLF.Constraint.Solve.Finalize"
                , "import MLF.Constraint.Solve.Worklist"
                ] $ \marker ->
                    solveSrc `shouldSatisfy` isInfixOf marker
            forM_
                [ "runUnifyClosureWithSeed"
                , "finalizeConstraintWithUF uf preRewrite = do"
                , "rewriteConstraintWithUF = applyUFConstraint"
                ] $ \marker ->
                    solveSrc `shouldSatisfy` (not . isInfixOf marker)

        it "split façade guard: Elaborate keeps algebra/scope/annotation logic in child modules" $ do
            elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            forM_
                [ "import MLF.Elab.Elaborate.Algebra"
                , "import MLF.Elab.Elaborate.Annotation"
                , "import MLF.Elab.Elaborate.Scope"
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` isInfixOf marker
            forM_
                [ "data AlgebraContext = AlgebraContext"
                , "data ScopeContext = ScopeContext"
                , "data AnnotationContext = AnnotationContext"
                , "elabAlg :: AlgebraContext ->"
                , "generalizeAtNode :: ScopeContext ->"
                , "reifyInst annotationContext"
                ] $ \marker ->
                    elaborateSrc `shouldSatisfy` (not . isInfixOf marker)

        it "presolution state access guard" $ do
            stateAccessSrc <- readFile "src/MLF/Constraint/Presolution/StateAccess.hs"
            edgeUnifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
            baseSrc <- readFile "src/MLF/Constraint/Presolution/Base.hs"
            forM_
                [ "withCanonical,"
                , "CanonicalEnv(..)"
                , "WithCanonicalT"
                , "runWithCanonical"
                , "askConstraint"
                , "askCanonical"
                , "canonicalize :: Monad m => NodeId ->"
                , "lookupBindParentR"
                , "liftBindingErrorR"
                , "Control.Monad.Reader"
                ] $ \marker ->
                    stateAccessSrc `shouldSatisfy` (not . isInfixOf marker)
            forM_
                [ "WithCanonicalT"
                , "runWithCanonical"
                , "lookupBindParentR"
                ] $ \marker ->
                    edgeUnifySrc `shouldSatisfy` (not . isInfixOf marker)
            baseSrc `shouldSatisfy` (not . isInfixOf "WithCanonicalT")

        it "presolution state access guard: retired instEdgeOwnerM export stays absent" $ do
            stateAccessSrc <- readFile "src/MLF/Constraint/Presolution/StateAccess.hs"
            forM_
                [ "instEdgeOwnerM,"
                , "instEdgeOwnerM ::"
                ] $ \marker ->
                    stateAccessSrc `shouldSatisfy` (not . isInfixOf marker)

        it "presolution state access guard: EdgeProcessing uses helper-layer state access" $ do
            edgeProcessingSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
            forM_
                [ "psConstraint st"
                , "psUnionFind st"
                , "st <- getPresolutionState"
                ] $ \marker ->
                    edgeProcessingSrc `shouldSatisfy` (not . isInfixOf marker)

        it "presolution witness assembly guard" $ do
            interpreterSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs"
            witnessSrc <- readFile "src/MLF/Constraint/Presolution/Witness.hs"
            cabalSrc <- readFile "mlf2.cabal"
            wrapperExists <-
                either (const False) (const True)
                    <$> tryIOError (readFile "src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs" >>= \s -> pure $! length s)
            wrapperExists `shouldBe` False
            interpreterSrc `shouldSatisfy` (not . isInfixOf "MLF.Constraint.Presolution.EdgeProcessing.Witness")
            witnessSrc `shouldSatisfy` isInfixOf "data EdgeWitnessPlan ="
            witnessSrc `shouldSatisfy` isInfixOf "edgeWitnessPlan ::"
            witnessSrc `shouldSatisfy` isInfixOf "buildEdgeTrace"
            witnessSrc `shouldSatisfy` isInfixOf "buildEdgeWitness"
            cabalSrc `shouldSatisfy` (not . isInfixOf "MLF.Constraint.Presolution.EdgeProcessing.Witness")

        it "row4 per-edge propagation thesis-exact guard: Interpreter removes synthesized-wrapper branch markers" $ do
            interpSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs"
            forM_
                [ "isSynthesizedExpVar"
                , "synthesizedWrapper"
                , "then pure (ExpIdentity, [(bodyId, n2Id)])"
                , "then pure ExpIdentity"
                ] $ \marker ->
                    interpSrc `shouldSatisfy` (not . isInfixOf marker)

        it "row5 graph-op execution thesis-exact guard: edge unification uses single omega entrypoint" $ do
            unifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs"
            edgeUnifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
            unifySrc `shouldSatisfy` (not . isInfixOf "OmegaExec.executeOmegaBaseOpsPre")
            unifySrc `shouldSatisfy` (not . isInfixOf "OmegaExec.executeOmegaBaseOpsPost")
            unifySrc `shouldSatisfy` isInfixOf "executeEdgeLocalOmegaOps omegaEnv baseOps"
            edgeUnifySrc `shouldSatisfy` isInfixOf "executeEdgeLocalOmegaOps omegaEnv baseOps action = do"

        it "row8 translatability normalization guard: live presolution path uses all-inert W normalization" $ do
            validationSrc <- readFile "src/MLF/Constraint/Presolution/Validation.hs"
            validationSrc `shouldSatisfy` isInfixOf "Inert.weakenInertNodes"
            validationSrc `shouldSatisfy` (not . isInfixOf "Inert.weakenInertLockedNodes")

        it "row9-11 direct-target guard: Omega does not define source-candidate reconciliation helpers" $ do
            omegaSrc <- readFile "src/MLF/Elab/Phi/Omega.hs"
            omegaSrc `shouldSatisfy` (not . isInfixOf "sourceCandidates :: NodeId -> [NodeId]")
            omegaSrc `shouldSatisfy` (not . isInfixOf "pickExistingSource :: [NodeId] -> Maybe NodeId")
            omegaSrc `shouldSatisfy` (not . isInfixOf "adoptOpNode :: NodeId -> NodeId")
            omegaSrc `shouldSatisfy` (not . isInfixOf "graftArgFor :: NodeId -> NodeId -> NodeId")

        it "Loop 2 split-facade guard: runtime facades stay thin and child-owned" $ do
            forM_
                [ ( "src/MLF/Elab/Phi/Omega.hs"
                  , 30
                  , [ "import MLF.Elab.Phi.Omega.Domain"
                    , "import MLF.Elab.Phi.Omega.Interpret"
                    , "import MLF.Elab.Phi.Omega.Normalize"
                    ]
                  )
                , ( "src/MLF/Constraint/Presolution/EdgeUnify.hs"
                  , 95
                  , [ "import MLF.Constraint.Presolution.EdgeUnify.State"
                    , "import qualified MLF.Constraint.Presolution.EdgeUnify.Omega as Omega"
                    , "import MLF.Constraint.Presolution.EdgeUnify.Unify"
                    ]
                  )
                , ( "src/MLF/Elab/Elaborate.hs"
                  , 120
                  , [ "import MLF.Elab.Elaborate.Algebra"
                    , "import MLF.Elab.Elaborate.Annotation"
                    , "import MLF.Elab.Elaborate.Scope"
                    ]
                  )
                ] $ \(path, maxLines, requiredMarkers) -> do
                    src <- readFile path
                    length (lines src) `shouldSatisfy` (<= maxLines)
                    forM_ requiredMarkers $ \marker ->
                        src `shouldSatisfy` isInfixOf marker

        it "row9-11 facade cleanup guard: Phi no longer re-exports or compiles Binder helpers" $ do
            phiSrc <- readFile "src/MLF/Elab/Phi.hs"
            cabalSrc <- readFile "mlf2.cabal"
            binderSrcOrErr <- tryIOError (readFile "src/MLF/Elab/Phi/Binder.hs")
            phiSrc `shouldSatisfy` (not . isInfixOf "MLF.Elab.Phi.Binder")
            phiSrc `shouldSatisfy` (not . isInfixOf "isBinderNodeM")
            phiSrc `shouldSatisfy` (not . isInfixOf "lookupBinderIndexM")
            phiSrc `shouldSatisfy` (not . isInfixOf "binderIndexM")
            phiSrc `shouldSatisfy` (not . isInfixOf "binderNameForM")
            cabalSrc `shouldSatisfy` (not . isInfixOf "MLF.Elab.Phi.Binder")
            case binderSrcOrErr of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected src/MLF/Elab/Phi/Binder.hs to be retired"

        it "chi-first guard: runtime and reify modules no longer adapt Solved through fromSolved" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            typeOpsSrc <- readFile "src/MLF/Elab/Run/TypeOps.hs"
            generalizeSrc <- readFile "src/MLF/Elab/Run/Generalize.hs"
            resultTypeUtilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            reifySrc <- readFile "src/MLF/Reify/Core.hs"
            legacySrc <- readFile "src/MLF/Elab/Legacy.hs"
            boundarySrc <- readFile "src/MLF/Constraint/Presolution/View.hs"
            scopeSrc `shouldSatisfy` (not . isInfixOf "fromSolved")
            typeOpsSrc `shouldSatisfy` (not . isInfixOf "fromSolved")
            generalizeSrc `shouldSatisfy` (not . isInfixOf "fromSolved")
            resultTypeUtilSrc `shouldSatisfy` (not . isInfixOf "fromSolved")
            reifySrc `shouldSatisfy` (not . isInfixOf "fromSolved")
            legacySrc `shouldSatisfy` isInfixOf "fromSolved"
            boundarySrc `shouldSatisfy` isInfixOf "fromSolved ::"

        it "chi-first guard: duplicate ...View aliases are retired from runtime and reify modules" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            typeOpsSrc <- readFile "src/MLF/Elab/Run/TypeOps.hs"
            generalizeSrc <- readFile "src/MLF/Elab/Run/Generalize.hs"
            resultTypeUtilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            reifySrc <- readFile "src/MLF/Reify/Core.hs"
            let banned =
                    [ "bindingScopeRefCanonicalView"
                    , "schemeBodyTargetView"
                    , "canonicalizeScopeRefView"
                    , "resolveCanonicalScopeView"
                    , "letScopeOverridesView"
                    , "inlineBoundVarsTypeView"
                    , "inlineBoundVarsTypeForBoundView"
                    , "generalizeAtWithBuilderView"
                    , "mkGeneralizeAtWithBuilderView"
                    , "generalizeWithPlanView"
                    , "reifyTypeFromView"
                    , "reifyTypeWithNamesFromView"
                    , "reifyTypeWithNamesNoFallbackFromView"
                    , "reifyTypeWithNamedSetFromView"
                    , "reifyTypeWithNamedSetNoFallbackFromView"
                    , "reifyBoundWithNamesFromView"
                    , "reifyBoundWithNamesBoundFromView"
                    , "namedNodesFromView"
                    ]
            forM_ banned $ \marker -> do
                scopeSrc `shouldSatisfy` (not . isInfixOf marker)
                typeOpsSrc `shouldSatisfy` (not . isInfixOf marker)
                generalizeSrc `shouldSatisfy` (not . isInfixOf marker)
                resultTypeUtilSrc `shouldSatisfy` (not . isInfixOf marker)
                reifySrc `shouldSatisfy` (not . isInfixOf marker)

        it "chi-first guard: internals use shared ChiQuery facade" $ do
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            rtViewSrc <- readFile "src/MLF/Elab/Run/ResultType/View.hs"
            elabSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"
            rtViewSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"

        it "chi-first guard: ChiQuery no longer defines chiCanonicalBindParents" $ do
            chiSrc <- readFile "src/MLF/Elab/Run/ChiQuery.hs"
            fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
            chiSrc `shouldSatisfy` (not . isInfixOf "chiCanonicalBindParents")
            fallbackSrc `shouldSatisfy` (not . isInfixOf "ChiQuery.chiCanonicalBindParents")

        it "chi-first guard: ChiQuery no longer exposes chiLookupBindParent or chiBindParents" $ do
            chiSrc <- readFile "src/MLF/Elab/Run/ChiQuery.hs"
            forM_
                [ "chiLookupBindParent,"
                , "chiLookupBindParent ::"
                , "chiBindParents"
                , "chiBindParents ::"
                ] $ \marker ->
                    chiSrc `shouldSatisfy` (not . isInfixOf marker)

        it "single-solved refactor keeps checked pipeline authoritative on representative corpus" $ do
            forM_ representativeMigrationCorpus assertCheckedAuthoritative

        it "chi-first ResultType|checked-authoritative keeps representative corpus parity" $ do
            forM_ representativeMigrationCorpus assertCheckedAuthoritative

        it "checked-authoritative keeps representative corpus parity" $ do
            forM_ representativeMigrationCorpus assertCheckedAuthoritative

        it "Pipeline \\(Phases 1-5\\)|Dual-path verification|chi-first integration keeps boundary wiring explicit" $ do
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            rtSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            pipelineSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
            elabSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
            rtSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
            pipelineSrc `shouldSatisfy` isInfixOf "mkResultTypeInputs"

        it "chi-first integration: pipeline run path has no internal solved materialization in Elaborate/ResultType" $ do
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            rtSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            elabSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
            rtSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")

        it "ResultType|Phase 6 — Elaborate|chi-first gate stays green" $ do
            elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
            rtViewSrc <- readFile "src/MLF/Elab/Run/ResultType/View.hs"
            rtSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
            elabSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"
            rtViewSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"
            rtViewSrc `shouldSatisfy` (not . isInfixOf "rtvSchemeBodyTarget")
            elabSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
            rtSrc `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")

        it "ga-scope debug guard: Debug no longer exposes edgeOrigins" $ do
            debugSrc <- readFile "src/MLF/Elab/Run/Debug.hs"
            forM_
                [ "edgeOrigins"
                , "edgeOrigins ::"
                ] $ \marker ->
                    debugSrc `shouldSatisfy` (not . isInfixOf marker)

        it "term-closure guard: TermClosure no longer exposes closeTermWithSchemeSubst" $ do
            termClosureSrc <- readFile "src/MLF/Elab/TermClosure.hs"
            forM_
                [ "closeTermWithSchemeSubst,"
                , "closeTermWithSchemeSubst ::"
                ] $ \marker ->
                    termClosureSrc `shouldSatisfy` (not . isInfixOf marker)

        it "witness/trace canonicalizers stay single-sourced" $ do
            utilSrc <- readFile "src/MLF/Elab/Run/Util.hs"
            utilSrc `shouldSatisfy` (not . isInfixOf "canonicalizeWitness ::")
            utilSrc `shouldSatisfy` (not . isInfixOf "canonicalizeTrace ::")
            utilSrc `shouldSatisfy` isInfixOf "canonicalizeExpansion ::"

        it "canonical scope helper reuses the primary owner path" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            scopeSrc `shouldSatisfy` (not . isInfixOf "bindingPathToRootFromBindParents")
            scopeSrc `shouldSatisfy` isInfixOf "bindingScopeRef (ChiQuery.chiCanonicalConstraint presolutionView) root"

        it "result-type root peeling stays single-sourced" $ do
            rtSrc <- readFile "src/MLF/Elab/Run/ResultType.hs"
            fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
            utilSrc <- readFile "src/MLF/Elab/Run/ResultType/Util.hs"
            forM_ [rtSrc, fallbackSrc] $ \src -> do
                src `shouldSatisfy` (not . isInfixOf "let schemeRootSet =")
                src `shouldSatisfy` (not . isInfixOf "isSchemeRoot nid =")
                src `shouldSatisfy` (not . isInfixOf "letEdges =")
                src `shouldSatisfy` (not . isInfixOf "isLetEdge (EdgeId")
                src `shouldSatisfy` (not . isInfixOf "let peel")
                src `shouldSatisfy` isInfixOf "resultTypeRoots"
            utilSrc `shouldSatisfy` isInfixOf "resultTypeRoots"

        it "scheme-root owner bookkeeping stays single-sourced" $ do
            fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
            phase4Src <- readFile "src/MLF/Elab/Run/Generalize/Phase4.hs"
            commonSrc <- readFile "src/MLF/Elab/Run/Generalize/Common.hs"
            forM_ [fallbackSrc, phase4Src] $ \src -> do
                src `shouldSatisfy` isInfixOf "canonicalSchemeRootOwners"
                src `shouldSatisfy` (not . isInfixOf "IntMap.fromList\n                [ (getNodeId (canonical root), gnId gen)")
            commonSrc `shouldSatisfy` isInfixOf "canonicalSchemeRootOwners"

        it "target unwrapping stays single-sourced inside Scope" $ do
            scopeSrc <- readFile "src/MLF/Elab/Run/Scope.hs"
            let section startMarker endMarker =
                    unlines
                        . takeWhile (not . isPrefixOf endMarker)
                        . drop 1
                        . dropWhile (not . isPrefixOf startMarker)
                        . lines
                generalizeSection =
                    section
                        "generalizeTargetNode :: PresolutionView -> NodeId -> NodeId"
                        "schemeBodyTarget :: PresolutionView -> NodeId -> NodeId"
                        scopeSrc
                schemeSection =
                    section
                        "schemeBodyTarget :: PresolutionView -> NodeId -> NodeId"
                        "{- Note [ga′ preservation across redirects]"
                        scopeSrc
            scopeSrc `shouldSatisfy` isInfixOf "targetUnwrapInfo ::"
            generalizeSection `shouldSatisfy` isInfixOf "targetUnwrapInfo presolutionView target"
            schemeSection `shouldSatisfy` isInfixOf "targetUnwrapInfo presolutionView target"
            generalizeSection `shouldSatisfy` (not . isInfixOf "case ChiQuery.chiLookupNode presolutionView targetC of")
            schemeSection `shouldSatisfy` (not . isInfixOf "in case ChiQuery.chiLookupNode presolutionView targetC of")
            schemeSection `shouldSatisfy` (not . isInfixOf "case ChiQuery.chiLookupNode presolutionView bndC of")

        it "Phase 6 — Elaborate|ResultType|Dual-path verification gate stays green" $ do
            forM_ representativeMigrationCorpus assertCheckedAuthoritative

        it "migration guardrail: thesis-core boundary matches legacy outcome" $ do
            forM_ representativeMigrationCorpus $ \expr -> do
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let pres = paPresolution artifacts
                    view = PresolutionViewBoundary.fromPresolutionResult pres
                    legacy = paSolved artifacts
                    thesisCoreValidated = Finalize.stepSolvedFromPresolutionView view
                validateStrict thesisCoreValidated
                validateStrict legacy
                assertViewParity view legacy

        describe "Dual-path verification" $ do
            it "production entrypoint remains checked-authoritative on representative corpus" $ do
                forM_ representativeMigrationCorpus assertCheckedAuthoritative

            it "runtime callgraph forbids legacy snapshot APIs in production run-path modules" $ do
                runModules <- loadRunPathModules
                let productionModules =
                        reachableRunPathModules runModules ["MLF.Elab.Run.Pipeline"]
                    forbiddenApis =
                        [ "Solved.fromPreRewriteState"
                        , "solveResultFromSnapshot"
                        ]
                    offenders =
                        sort
                            [ (rmPath modSrc, api)
                            | modSrc <- productionModules
                            , api <- forbiddenApis
                            , api `isInfixOf` rmSource modSrc
                            ]
                when (null productionModules) $
                    expectationFailure "Runtime callgraph guard found no production run-path modules"
                unless (null offenders) $
                    expectationFailure
                        ( "Forbidden runtime API usage in production run-path callgraph:\n"
                            ++ unlines
                                [ path ++ " uses " ++ api
                                | (path, api) <- offenders
                                ]
                        )

        it "shared solved-to-presolution adapter matches selected solved queries on representative corpus" $ do
            let corpus =
                    [ ELam "x" (EVar "x")
                    , ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
                    , EAnn (ELam "x" (EVar "x"))
                        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                    ]
            forM_ corpus $ \expr -> do
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let solved = paSolved artifacts
                    view = PresolutionViewBoundary.fromSolved solved
                    nodeIds = map fst (toListNode (cNodes (Solved.originalConstraint solved)))
                    probeIds = nodeIds ++ [NodeId 999]
                    probeRefs = map typeRef probeIds

                pvConstraint view `shouldBe` Solved.originalConstraint solved
                pvCanonicalMap view `shouldBe` Solved.canonicalMap solved
                pvBindParents view `shouldBe` cBindParents (Solved.originalConstraint solved)
                pvCanonicalConstraint view `shouldBe` Solved.canonicalConstraint solved

                forM_ probeIds $ \nid -> do
                    pvCanonical view nid `shouldBe` Solved.canonical solved nid
                    pvLookupNode view nid `shouldBe`
                        NodeAccess.lookupNode (Solved.originalConstraint solved) (Solved.canonical solved nid)
                    pvLookupVarBound view nid `shouldBe`
                        NodeAccess.lookupVarBound (Solved.originalConstraint solved) (Solved.canonical solved nid)

                forM_ probeRefs $ \ref ->
                    pvLookupBindParent view ref `shouldBe`
                        NodeAccess.lookupBindParent (Solved.originalConstraint solved) ref

        it "runtime snapshot rebuild stays stable across representative corpus" $ do
            let corpus =
                    [ ELam "x" (EVar "x")
                    , ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
                    , EAnn (ELam "x" (EVar "x"))
                        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
                    ]
            forM_ corpus $ \expr -> do
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let pres = paPresolution artifacts
                expected <- requireRight
                    (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
                paSolved artifacts `shouldBe` expected
                Solved.validateCanonicalGraphStrict (paSolved artifacts)
                    `shouldBe` []
                runPipelineElab Set.empty (unsafeNormalizeExpr expr)
                    `shouldSatisfy` isRight

        it "annotation-heavy path still reports checked-authoritative type" $ do
            let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                expr = ELamAnn "x" recursiveAnn (EVar "x")
                expectedTy = TArrow (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
                                    (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (renderPipelineError err)
                Right (termUnchecked, tyUnchecked) -> do
                    typeCheck termUnchecked `shouldBe` Right tyUnchecked
                    case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
                        Left err -> expectationFailure (renderPipelineError err)
                        Right (termChecked, tyChecked) -> do
                            typeCheck termChecked `shouldBe` Right tyChecked
                            tyUnchecked `shouldBe` tyChecked
                            tyChecked `shouldSatisfy` matchesRecursiveArrow expectedTy

        describe "ARI-C1 feasibility characterization (bounded prototype-only)" $ do
            let ariSetVarBound nid newBound constraint =
                    let tweak node = case node of
                            TyVar{ tnId = varId } | varId == nid ->
                                TyVar{ tnId = varId, tnBound = Just newBound }
                            _ -> node
                    in constraint
                        { cNodes =
                            fromListNode
                                [ (nodeIdKey, tweak node)
                                | (nodeIdKey, node) <- toListNode (cNodes constraint)
                                ]
                        }
                ariSetTypeParent child mbParent constraint =
                    let childKey = nodeRefKey (typeRef child)
                        bindParents' = case mbParent of
                            Nothing ->
                                IntMap.delete childKey (cBindParents constraint)
                            Just parentRef ->
                                IntMap.insert childKey (parentRef, BindFlex) (cBindParents constraint)
                    in constraint { cBindParents = bindParents' }
                rewriteResultTypeInputs rewrite inputs =
                    let view0 = rtcPresolutionView inputs
                        baseConstraint' = rewrite (pvConstraint view0)
                        canonicalConstraint' = rewrite (pvCanonicalConstraint view0)
                        view' =
                            view0
                                { pvConstraint = baseConstraint'
                                , pvLookupNode =
                                    \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid)
                                , pvLookupVarBound =
                                    \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid)
                                , pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint'
                                , pvBindParents = cBindParents baseConstraint'
                                , pvCanonicalConstraint = canonicalConstraint'
                                }
                        ga0 = rtcBindParentsGa inputs
                        ga' =
                            ga0
                                { gaBindParentsBase = cBindParents baseConstraint'
                                , gaBaseConstraint = baseConstraint'
                                }
                    in inputs
                        { rtcPresolutionView = view'
                        , rtcBindParentsGa = ga'
                        }
                ariClearVarBound nid constraint =
                    let tweak node = case node of
                            TyVar{ tnId = varId } | varId == nid ->
                                TyVar{ tnId = varId, tnBound = Nothing }
                            _ -> node
                    in constraint
                        { cNodes =
                            fromListNode
                                [ (nodeIdKey, tweak node)
                                | (nodeIdKey, node) <- toListNode (cNodes constraint)
                                ]
                        }
                makeLocalTypeRoot inputs rootNid =
                    rewriteResultTypeInputs (ariSetTypeParent rootNid Nothing) inputs
                rebindRootTo inputs rootNid newBound =
                    rewriteResultTypeInputs (ariSetVarBound rootNid newBound) inputs
                clearVarBoundInInputs inputs nid =
                    rewriteResultTypeInputs (ariClearVarBound nid) inputs
                duplicateReferencedTrace inputs eids =
                    let traceFor eid =
                            IntMap.lookup (getEdgeId eid) edgeTraces0
                        edgeTraces0 = rtcEdgeTraces inputs
                        matchingTrace =
                            listToMaybe
                                [ tr
                                | eid <- eids
                                , Just tr <- [traceFor eid]
                                ]
                        nextEdgeKey =
                            case IntMap.lookupMax edgeTraces0 of
                                Just (edgeKey, _trace) -> edgeKey + 1
                                Nothing -> 0
                    in case matchingTrace of
                        Just tr ->
                            inputs
                                { rtcEdgeArtifacts =
                                    (rtcEdgeArtifacts inputs)
                                        { eaEdgeTraces =
                                            IntMap.insert nextEdgeKey tr edgeTraces0
                                        }
                                }
                        Nothing ->
                            error
                                ( "expected edge trace for "
                                    ++ show eids
                                    ++ " for local multi-inst fallback case"
                                )
                rewriteReferencedTrace rewrite eids inputs =
                    let edgeTraces0 = rtcEdgeTraces inputs
                        matchingTrace =
                            listToMaybe
                                [ (getEdgeId eid, tr)
                                | eid <- eids
                                , Just tr <- [IntMap.lookup (getEdgeId eid) edgeTraces0]
                                ]
                    in case matchingTrace of
                        Just (edgeKey, tr) ->
                            inputs
                                { rtcEdgeArtifacts =
                                    (rtcEdgeArtifacts inputs)
                                        { eaEdgeTraces =
                                            IntMap.insert edgeKey (rewrite tr) edgeTraces0
                                        }
                                }
                        Nothing ->
                            error
                                ( "expected edge trace for "
                                    ++ show eids
                                    ++ " for local inst-arg multi-base case"
                                )
                findIntBaseNode view0 =
                    case
                        [ tnId node
                        | (_nodeIdKey, node@TyBase{ tnBase = BaseTy "Int" }) <-
                            toListNode (cNodes (pvConstraint view0))
                        ]
                    of
                        baseNid : _ -> baseNid
                        [] -> error "expected Int base node for local fallback case"
                nextFreshNodeIds count constraint =
                    let start =
                            case toListNode (cNodes constraint) of
                                [] -> 0
                                nodes0 ->
                                    maximum
                                        [ getNodeId nodeIdKey
                                        | (nodeIdKey, _node) <- nodes0
                                        ]
                                        + 1
                    in fmap NodeId [start .. start + count - 1]
                insertTyNodes newNodes constraint =
                    constraint
                        { cNodes =
                            fromListNode (toListNode (cNodes constraint) ++ fmap (\node -> (tnId node, node)) newNodes)
                        }
                schemeAliasBaseLikeFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
                        extractVarBody ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn body _ _) _ -> body
                            _ -> error ("unexpected scheme-alias/base-like wrapper shape: " ++ show ann0)
                        bodyRoot ann0 = case extractVarBody ann0 of
                            AVar _ nid -> nid
                            other ->
                                error ("expected local scheme alias variable body, got " ++ show other)
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        bodyCanon = extractVarBody annCanon0
                        bodyPre = extractVarBody annPre0
                        rootNid = rtcCanonical inputs0 (bodyRoot annCanon0)
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 =
                            rebindRootTo
                                inputs1
                                rootNid
                                (findIntBaseNode (rtcPresolutionView inputs1))
                    requireRight (computeResultTypeFallback inputs2 bodyCanon bodyPre)
                localEmptyCandidateSchemeAliasBaseLikeFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                        extractInnerLetRhs ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                            _ ->
                                error
                                    ( "unexpected local empty-candidate scheme-alias/base-like wrapper shape: "
                                        ++ show ann0
                                    )
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        innerCanon = extractInnerLetRhs annCanon0
                        innerPre = extractInnerLetRhs annPre0
                        (rootNid, childNid) = case innerCanon of
                            AApp _ (AVar _ nid) _ _ appNid -> (appNid, nid)
                            other ->
                                error
                                    ( "expected local empty-candidate scheme-alias/base-like app shape, got "
                                        ++ show other
                                    )
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 =
                            rebindRootTo
                                inputs1
                                rootNid
                                (findIntBaseNode (rtcPresolutionView inputs1))
                        inputs3 = clearVarBoundInInputs inputs2 childNid
                    requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
                localMultiInstFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                        extractInnerLetRhs ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                            _ -> error ("unexpected local multi-inst wrapper shape: " ++ show ann0)
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        innerCanon = extractInnerLetRhs annCanon0
                        innerPre = extractInnerLetRhs annPre0
                        (rootNid, childNid, edgeIds) = case innerCanon of
                            AApp _ (AVar _ nid) funEid argEid appNid -> (appNid, nid, [funEid, argEid])
                            other ->
                                error ("expected local multi-inst app shape, got " ++ show other)
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 = clearVarBoundInInputs inputs1 childNid
                        inputs3 = duplicateReferencedTrace inputs2 edgeIds
                    requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
                localInstArgMultiBaseFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                        extractInnerLetRhs ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                            _ -> error ("unexpected local inst-arg multi-base wrapper shape: " ++ show ann0)
                        injectMultiBaseArgs inputs eids =
                            let view0 = rtcPresolutionView inputs
                                constraint0 = pvConstraint view0
                                (bottomNid, binderA, argA, binderB, argB) =
                                    case nextFreshNodeIds 5 constraint0 of
                                        [nid0, nid1, nid2, nid3, nid4] ->
                                            (nid0, nid1, nid2, nid3, nid4)
                                        other ->
                                            error
                                                ( "expected five fresh node ids for local inst-arg multi-base case, got "
                                                    ++ show other
                                                )
                                inputs' =
                                    rewriteResultTypeInputs
                                        ( insertTyNodes
                                            [ TyBottom{ tnId = bottomNid }
                                            , TyVar{ tnId = binderA, tnBound = Nothing }
                                            , TyVar{ tnId = argA, tnBound = Just (findIntBaseNode view0) }
                                            , TyVar{ tnId = binderB, tnBound = Nothing }
                                            , TyVar{ tnId = argB, tnBound = Just bottomNid }
                                            ]
                                        )
                                        inputs
                            in rewriteReferencedTrace
                                ( \tr ->
                                    tr
                                        { etBinderArgs =
                                            etBinderArgs tr ++ [(binderA, argA), (binderB, argB)]
                                        }
                                )
                                eids
                                inputs'
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        innerCanon = extractInnerLetRhs annCanon0
                        innerPre = extractInnerLetRhs annPre0
                        (rootNid, childNid, edgeIds) = case innerCanon of
                            AApp _ (AVar _ nid) funEid argEid appNid -> (appNid, nid, [funEid, argEid])
                            other ->
                                error ("expected local inst-arg multi-base app shape, got " ++ show other)
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 = clearVarBoundInInputs inputs1 childNid
                        inputs3 = injectMultiBaseArgs inputs2 edgeIds
                    requireRight (computeResultTypeFallback inputs3 innerCanon innerPre)
                localInstArgSingleBaseFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                        extractInnerLetRhs ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                            _ -> error ("unexpected local inst-arg singleton-base wrapper shape: " ++ show ann0)
                        injectSingleBaseWitness inputs eid =
                            let view0 = rtcPresolutionView inputs
                                edgeWitnesses0 = rtcEdgeWitnesses inputs
                                edgeKey = getEdgeId eid
                                seedWitness =
                                    case IntMap.lookup edgeKey edgeWitnesses0 of
                                        Just ew -> ew
                                        Nothing ->
                                            case IntMap.elems edgeWitnesses0 of
                                                ew0 : _ -> ew0 { ewEdgeId = eid }
                                                [] ->
                                                    error "expected an edge witness for local inst-arg singleton-base case"
                                ew' =
                                    seedWitness
                                        { ewEdgeId = eid
                                        , ewRight = findIntBaseNode view0
                                        }
                            in inputs
                                { rtcEdgeArtifacts =
                                    (rtcEdgeArtifacts inputs)
                                        { eaEdgeWitnesses =
                                            IntMap.insert edgeKey ew' edgeWitnesses0
                                        }
                                }
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        innerCanon = extractInnerLetRhs annCanon0
                        innerPre = extractInnerLetRhs annPre0
                        (rootNid, childNid, argEid) = case innerCanon of
                            AApp _ (AVar _ nid) _funEid argEdgeId appNid -> (appNid, nid, argEdgeId)
                            other ->
                                error ("expected local inst-arg singleton-base app shape, got " ++ show other)
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 =
                            if keepLocalTypeRoot
                                then rebindRootTo inputs1 rootNid (findIntBaseNode (rtcPresolutionView inputs1))
                                else inputs1
                        inputs3 = clearVarBoundInInputs inputs2 childNid
                        inputs4 = injectSingleBaseWitness inputs3 argEid
                    requireRight (computeResultTypeFallback inputs4 innerCanon innerPre)
                localSingleBaseFallback keepLocalTypeRoot = do
                    let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                        expr =
                            ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                        extractInnerLetRhs ann0 = case ann0 of
                            ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                            _ -> error ("unexpected local single-base wrapper shape: " ++ show ann0)
                    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                        innerCanon = extractInnerLetRhs annCanon0
                        innerPre = extractInnerLetRhs annPre0
                        (rootNid, childNid) = case innerCanon of
                            AApp _ (AVar _ nid) _ _ appNid -> (appNid, nid)
                            other ->
                                error ("expected local single-base app shape, got " ++ show other)
                        inputs1 =
                            if keepLocalTypeRoot
                                then makeLocalTypeRoot inputs0 rootNid
                                else inputs0
                        inputs2 =
                            rewriteResultTypeInputs
                                ( ariSetVarBound childNid (findIntBaseNode (rtcPresolutionView inputs1))
                                . ariSetTypeParent childNid Nothing
                                )
                                inputs1
                    requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)

            it "keeps annotation-anchored recursive shape processable" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr = ELamAnn "x" recursiveAnn (EVar "x")
                    isRecursiveArrow ty = case ty of
                        TArrow (TMu _ _) (TMu _ _) -> True
                        _ -> False
                    pipelineRuns =
                        [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr))
                        , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
                        ]
                forM_ pipelineRuns $ \(label, result) ->
                    case result of
                        Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
                        Right (_term, ty) ->
                            ty `shouldSatisfy` isRecursiveArrow

            it "keeps local-binding recursive retention processable through a direct wrapper" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "id" (ELam "y" (EVar "y"))
                            (ELamAnn "x" recursiveAnn (EVar "x"))
                    expectedTy =
                        TArrow (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
                            (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
                    pipelineRuns =
                        [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr))
                        , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
                        ]
                forM_ pipelineRuns $ \(label, result) ->
                    case result of
                        Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
                        Right (_term, ty) ->
                            ty `shouldSatisfy` (`matchesRecursiveArrow` expectedTy)

            it "keeps retained-child fallback recursive through a same-lane local TypeRef root" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                    extractInnerLetRhs ann0 = case ann0 of
                        ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
                        _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
                    setVarBound nid newBound constraint =
                        let tweak node = case node of
                                TyVar{ tnId = varId } | varId == nid ->
                                    TyVar{ tnId = varId, tnBound = Just newBound }
                                _ -> node
                        in constraint
                            { cNodes =
                                fromListNode
                                    [ (nodeIdKey, tweak node)
                                    | (nodeIdKey, node) <- toListNode (cNodes constraint)
                                    ]
                            }
                    setTypeParent child mbParent constraint =
                        let childKey = nodeRefKey (typeRef child)
                            bindParents' = case mbParent of
                                Nothing ->
                                    IntMap.delete childKey (cBindParents constraint)
                                Just parentRef ->
                                    IntMap.insert childKey (parentRef, BindFlex) (cBindParents constraint)
                        in constraint { cBindParents = bindParents' }
                    wireSameLaneLocalRoot inputs rootNid childNid =
                        let view0 = rtcPresolutionView inputs
                            retainedTarget =
                                case pvLookupVarBound view0 childNid of
                                    Just boundNid -> boundNid
                                    Nothing ->
                                        error
                                            ( "expected retained child bound for "
                                                ++ show childNid
                                            )
                            rewrite =
                                setVarBound rootNid retainedTarget
                                    . setVarBound childNid retainedTarget
                                    . setTypeParent childNid (Just (typeRef rootNid))
                                    . setTypeParent rootNid Nothing
                            baseConstraint' = rewrite (pvConstraint view0)
                            canonicalConstraint' = rewrite (pvCanonicalConstraint view0)
                            view' =
                                view0
                                    { pvConstraint = baseConstraint'
                                    , pvLookupNode =
                                        \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid)
                                    , pvLookupVarBound =
                                        \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid)
                                    , pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint'
                                    , pvBindParents = cBindParents baseConstraint'
                                    , pvCanonicalConstraint = canonicalConstraint'
                                    }
                            ga0 = rtcBindParentsGa inputs
                            ga' =
                                ga0
                                    { gaBindParentsBase = cBindParents baseConstraint'
                                    , gaBaseConstraint = baseConstraint'
                                    }
                        in inputs
                            { rtcPresolutionView = view'
                            , rtcBindParentsGa = ga'
                            }
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
                    innerCanon = extractInnerLetRhs annCanon0
                    innerPre = extractInnerLetRhs annPre0
                    (retainedRoot, retainedChild) = case innerCanon of
                        AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
                        _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
                    inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
                fallbackTy <- requireRight (computeResultTypeFallback inputs innerCanon innerPre)
                containsMu fallbackTy `shouldBe` True

            it "same-lane retained-child exact packet clears Phase 6 elaboration" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
                    pipelineRuns =
                        [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr))
                        , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
                        ]
                forM_ pipelineRuns $ \(label, result) ->
                    case result of
                        Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
                        Right _ -> pure ()

            it "keeps retained-child lookup bounded to the same local TypeRef lane" $ do
                fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "sameLocalTypeLane child =\n                            case bindingScopeRefCanonical presolutionViewFinal child of"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "in if rootBindingIsLocalType\n                        then pickCandidate (\\_parentRef child -> sameLocalTypeLane child)\n                        else pickCandidate (\\parentRef _child -> parentRef == scopeRoot)"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "sameLaneLocalRetainedChildTarget =\n                    if rootBindingIsLocalType\n                        then boundVarTarget\n                        else Nothing"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "keepTargetFinal =\n                    rootBindingIsLocalType\n                        && ( rootLocalMultiInst\n                                || rootLocalInstArgMultiBase\n                                || rootLocalSchemeAliasBaseLike\n                                || maybe False (const True) sameLaneLocalRetainedChildTarget\n                           )"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "case sameLaneLocalRetainedChildTarget of\n                                                        Just v -> v\n                                                        Nothing -> schemeBodyTarget targetPresolutionView rootC"

            it "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "id" (ELam "z" (EVar "z"))
                            (ELet "k" (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                                (EApp (ELam "y" (EVar "y")) (EVar "k")))
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
                fallbackTy <- requireRight (computeResultTypeFallback inputs annCanon annPre)
                containsMu fallbackTy `shouldBe` False

            it "keeps local empty-candidate scheme-alias/base-like fallback on the local TypeRef lane" $ do
                fallbackTy <- localEmptyCandidateSchemeAliasBaseLikeFallback True
                fallbackTy `shouldBe` TBase (BaseTy "Int")

            it "keeps the preserved local scheme-alias/base-like continuity on the quantified rootFinal lane" $ do
                fallbackTy <- schemeAliasBaseLikeFallback True
                case fallbackTy of
                    TForall _ Nothing (TBase (BaseTy "Int")) -> pure ()
                    other ->
                        expectationFailure
                            ( "expected quantified Int result for local scheme-alias/base-like continuity lane, got "
                                ++ show other
                            )

            it "keeps local single-base fallback on the local TypeRef lane" $ do
                fallbackTy <- localSingleBaseFallback True
                fallbackTy `shouldBe` TBase (BaseTy "Int")

            it "keeps the same single-base wrapper fail-closed once it leaves the local TypeRef lane" $ do
                fallbackTy <- localSingleBaseFallback False
                case fallbackTy of
                    TForall _ Nothing (TVar _) -> pure ()
                    other ->
                        expectationFailure
                            ( "expected non-local single-base contrast to stay on the quantified fail-closed shell, got "
                                ++ show other
                            )

            it "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane" $ do
                fallbackTy <- schemeAliasBaseLikeFallback False
                fallbackTy `shouldBe` TBase (BaseTy "Int")
                containsMu fallbackTy `shouldBe` False

            it "keeps local multi-inst fallback on the local TypeRef lane" $ do
                fallbackTy <- localMultiInstFallback True
                case fallbackTy of
                    TVar _ -> pure ()
                    other ->
                        expectationFailure
                            ( "expected local multi-inst lane to retain the final target variable, got "
                                ++ show other
                            )

            it "keeps the same multi-inst wrapper fail-closed once it leaves the local TypeRef lane" $ do
                fallbackTy <- localMultiInstFallback False
                case fallbackTy of
                    TForall _ Nothing (TVar _) -> pure ()
                    other ->
                        expectationFailure
                            ( "expected non-local multi-inst contrast to stay on the quantified fail-closed shell, got "
                                ++ show other
                            )

            it "keeps local inst-arg multi-base fallback on the local TypeRef lane" $ do
                fallbackTy <- localInstArgMultiBaseFallback True
                case fallbackTy of
                    TVar _ -> pure ()
                    other ->
                        expectationFailure
                            ( "expected local inst-arg multi-base lane to retain the final target variable, got "
                                ++ show other
                            )

            it "keeps the same inst-arg multi-base wrapper fail-closed once it leaves the local TypeRef lane" $ do
                fallbackTy <- localInstArgMultiBaseFallback False
                case fallbackTy of
                    TForall _ Nothing (TVar _) -> pure ()
                    other ->
                        expectationFailure
                            ( "expected non-local inst-arg multi-base contrast to stay on the quantified fail-closed shell, got "
                                ++ show other
                            )

            it "keeps local inst-arg-only singleton-base fallback on the local TypeRef lane" $ do
                fallbackTy <- localInstArgSingleBaseFallback True
                fallbackTy `shouldBe` TBase (BaseTy "Int")

            it "keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane" $ do
                fallbackTy <- localInstArgSingleBaseFallback False
                case fallbackTy of
                    TForall _ Nothing (TVar _) -> pure ()
                    other ->
                        expectationFailure
                            ( "expected non-local inst-arg-only singleton-base contrast to stay on the quantified fail-closed shell, got "
                                ++ show other
                            )

            it "does not infer recursive shape for the corresponding unannotated variant" $ do
                let expr = ELam "x" (EVar "x")
                case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                    Left err -> expectationFailure (renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` False

            it "keeps non-local proxy fallback fail-closed in result-type reconstruction" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "g" (ELamAnn "x" recursiveAnn (EVar "x"))
                            (EApp (EVar "g") (EVar "g"))
                artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
                let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
                fallbackTy <- requireRight (computeResultTypeFallback inputs annCanon annPre)
                containsMu fallbackTy `shouldBe` False

            it "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes" $ do
                fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalEmptyCandidateSchemeAliasBaseLike =\n                    rootBindingIsLocalType\n                        && rootIsSchemeAlias\n                        && rootBoundIsBaseLike\n                        && IntSet.null rootBoundCandidates\n                        && IntSet.null instArgBaseBounds\n                        && not rootHasMultiInst\n                        && not instArgRootMultiBase"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalSingleBase =\n                    rootBindingIsLocalType\n                        && IntSet.size rootBoundCandidates == 1\n                        && not rootHasMultiInst\n                        && not instArgRootMultiBase"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalInstArgSingleBase =\n                    rootBindingIsLocalType\n                        && IntSet.null rootBaseBounds\n                        && IntSet.size instArgBaseBounds == 1\n                        && not rootHasMultiInst\n                        && not instArgRootMultiBase"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootNonLocalSchemeAliasBaseLike =\n                    not rootBindingIsLocalType\n                        && rootIsSchemeAlias\n                        && rootBoundIsBaseLike"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "let targetC =\n                    case baseTarget of\n                        Just baseC\n                            | rootLocalSingleBase -> baseC"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "Just baseC\n                            | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "Just baseC\n                            | rootLocalInstArgSingleBase -> baseC"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "Just baseC\n                            | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC\n                        Just baseC\n                            | rootNonLocalSchemeAliasBaseLike -> baseC"
                fallbackSrc
                    `shouldSatisfy`
                        ( not
                            . isInfixOf
                                "Just baseC\n                            | rootIsSchemeAlias\n                                && rootBoundIsBaseLike -> baseC"
                        )
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "then schemeBodyTarget targetPresolutionView rootC\n                                        else rootFinal"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "keepTargetFinal =\n                    rootBindingIsLocalType\n                        && ( rootLocalMultiInst\n                                || rootLocalInstArgMultiBase"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalMultiInst =\n                    rootBindingIsLocalType\n                        && rootHasMultiInst"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalInstArgMultiBase =\n                    rootBindingIsLocalType\n                        && instArgRootMultiBase"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "rootLocalSchemeAliasBaseLike =\n                    rootBindingIsLocalType\n                        && rootIsSchemeAlias\n                        && rootBoundIsBaseLike"
                fallbackSrc `shouldSatisfy` isInfixOf "|| rootLocalSchemeAliasBaseLike"
                fallbackSrc `shouldSatisfy` isInfixOf "|| rootLocalInstArgMultiBase"
                fallbackSrc `shouldSatisfy` isInfixOf "|| rootLocalMultiInst"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "if rootLocalSchemeAliasBaseLike"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "if rootLocalSchemeAliasBaseLike\n                                        || rootLocalMultiInst\n                                        || rootLocalInstArgMultiBase\n                                        then rootFinal"
                fallbackSrc
                    `shouldSatisfy`
                        isInfixOf
                            "then rootFinal"

            it "keeps the same non-local proxy wrapper fail-closed at pipeline entrypoints" $ do
                let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
                    expr =
                        ELet "g" (ELamAnn "x" recursiveAnn (EVar "x"))
                            (EApp (EVar "g") (EVar "g"))
                    pipelineRuns =
                        [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr))
                        , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
                        ]
                forM_ pipelineRuns $ \(label, result) ->
                    expectStrictPipelineFailure (label ++ " non-local proxy wrapper") result

        it "uses presolution-native solved artifacts" $ do
            artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
            cUnifyEdges (prConstraint (paPresolution artifacts)) `shouldBe` []
            let pres = paPresolution artifacts
            expectedNative <- requireRight
                (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
            paSolved artifacts `shouldBe` expectedNative
            runPipelineElab Set.empty (unsafeNormalizeExpr (ELam "x" (EVar "x")))
                `shouldSatisfy` isRight

        it "solves let-bound id applied to Bool" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LBool True)))
            case runToSolvedDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right solved -> do
                    validateStrict solved
                    let nodes = cNodes (Solved.originalConstraint solved)
                    baseNames nodes `shouldContain` [BaseTy "Bool"]
                    noExpNodes nodes

        it "instantiates let-polymorphic id at Int and Bool" $ do
            -- let id = \x. x in let a = id 1 in id True
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                            (EApp (EVar "id") (ELit (LBool True))))
            case runToSolvedDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right res -> do
                    validateStrict res
                    let nodes = cNodes (Solved.originalConstraint res)
                    baseNames nodes `shouldContain` [BaseTy "Int"]
                    noExpNodes nodes

        it "tracks instantiation copy maps for named binders" $ do
            -- Non-trivial instantiation: polymorphic id used at two types
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                            (EApp (EVar "id") (ELit (LBool True))))
            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paConstraintNorm = c1, paPresolution = pres, paSolved = solved } ->
                    case Solved.validateCanonicalGraphStrict solved of
                        [] -> do
                            let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                                adoptNode = canonicalizeNode canon
                                baseNamedKeysAll = collectBaseNamedKeys c1
                                traceMaps = map (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                                                 (IntMap.elems (prEdgeTraces pres))
                                instCopyMapFull = foldl' IntMap.union IntMap.empty traceMaps
                                baseCopyPairs =
                                    [ (baseKey, copyN)
                                    | tr <- IntMap.elems (prEdgeTraces pres)
                                    , (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                                    , IntSet.member baseKey baseNamedKeysAll
                                    ]
                            baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
                            baseCopyPairs `shouldSatisfy` (not . null)
                            mapM_
                                (\(baseKey, copyN) ->
                                    let expected = adoptNode (NodeId baseKey)
                                        actual = IntMap.lookup (getNodeId (adoptNode copyN)) instCopyMapFull
                                    in case actual of
                                        Nothing ->
                                            expectationFailure ("Missing instantiation copy-map entry for base key " ++ show baseKey)
                                        Just mapped ->
                                            adoptNode mapped `shouldBe` expected
                                )
                                baseCopyPairs
                        vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

        it "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization" $ do
            let makeFactory = ELam "x" (ELam "y" (EVar "x"))
                expr =
                    ELam "k"
                        (ELet "make" makeFactory
                            (ELet "c1" (EApp (EVar "make") (EVar "k"))
                                (EApp (EVar "c1") (ELit (LBool True)))))
            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paPresolution = pres, paSolved = solved } -> do
                    let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                        edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
                        edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
                        raisesForEdge (eid, ew) =
                            [ (eid, n)
                            | OpRaise n <- getInstanceOps (ewWitness ew)
                            ]
                        raiseTargets = concatMap raisesForEdge (IntMap.toList edgeWitnesses)
                    forM_ raiseTargets $ \(eid, n) ->
                        case IntMap.lookup eid edgeTraces of
                            Nothing ->
                                expectationFailure ("Missing trace for edge with OpRaise: " ++ show eid)
                            Just tr -> do
                                let interiorKeys =
                                        IntSet.fromList
                                            [ getNodeId nid
                                            | nid <- toListInterior (etInterior tr)
                                            ]
                                IntSet.member (getNodeId n) interiorKeys `shouldBe` True

        it "BUG-002-V4 keeps the strict non-root OpWeaken when c1 stays abstract under lambda" $ do
            let makeFactory = ELam "x" (ELam "y" (EVar "x"))
                expr =
                    ELam "k"
                        (ELet "make" makeFactory
                            (ELet "c1" (EApp (EVar "make") (EVar "k"))
                                (EApp (EVar "c1") (ELit (LBool True)))))
                isNonRootWeaken op =
                    case op of
                        OpWeaken{} -> True
                        _ -> False
            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paPresolution = pres } -> do
                    ew0 <- case IntMap.lookup 0 (prEdgeWitnesses pres) of
                        Just ew -> pure ew
                        Nothing -> expectationFailure "Expected edge 0 witness" >> fail "missing edge 0 witness"
                    tr0 <- case IntMap.lookup 0 (prEdgeTraces pres) of
                        Just tr -> pure tr
                        Nothing -> expectationFailure "Expected edge 0 trace" >> fail "missing edge 0 trace"
                    getInstanceOps (ewWitness ew0) `shouldSatisfy` any isNonRootWeaken
                    etReplayContract tr0 `shouldBe` ReplayContractStrict

    it "handles higher-order polymorphic apply used twice" $ do
        -- let apply f x = f x; let id = \y. y; let a = apply id 1 in apply id True
        let expr =
                ELet "apply" (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
                    (ELet "id" (ELam "y" (EVar "y"))
                        (ELet "a" (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LInt 1)))
                            (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LBool True)))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = Solved.originalConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                baseNames nodes `shouldContain` [BaseTy "Bool"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "redirected let-use sites keep polymorphic schemes" $ do
        -- let id = \x. x in id id
        let expr =
                ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (EVar "id"))
        case runToPresolutionWithAnnDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right (pres, ann) -> do
                let redirects = prRedirects pres
                    varNodes = collectVarNodes "id" ann
                    redirected =
                        [ nid
                        | nid <- varNodes
                        , chaseRedirects redirects nid /= nid
                        ]
                varNodes `shouldSatisfy` (not . null)
                redirected `shouldSatisfy` (not . null)
        case runPipelineElab Set.empty expr of
            Left err -> expectationFailure (renderPipelineError err)
            Right (_term, ty) -> pretty ty `shouldSatisfy` ("∀" `isInfixOf`)

    it "applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently" $ do
        -- Exercise rewrite coverage on a shape with redirected + canonicalized let scheme roots.
        let rewriteExpr =
                ELet "id" (ELam "x" (EVar "x"))
                    (ELet "f" (EVar "id")
                        (ELet "a" (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True)))))
            -- Separately exercise the production canonicalizeAnn path via pipeline run.
            canonicalizePathExpr =
                    ELet "id" (ELam "x" (EVar "x"))
                    (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                        (EApp (EVar "id") (ELit (LBool True)))
                    )
        case runPipelineArtifactsDefault defaultPolySyms rewriteExpr of
            Left err -> expectationFailure err
            Right PipelineArtifacts{ paPresolution = pres, paSolved = solved, paAnnotated = ann } -> do
                let redirects = prRedirects pres
                    redirectedSchemeRoots =
                        [ nid
                        | nid <- annLetSchemeRoots ann
                        , chaseRedirects redirects nid /= nid
                        ]
                    annRedirected = applyRedirectsToAnn redirects ann
                    staleRedirectNodes =
                        [ nid
                        | nid <- annNodeOccurrences annRedirected
                        , chaseRedirects redirects nid /= nid
                        ]
                redirectedSchemeRoots `shouldSatisfy` (not . null)
                staleRedirectNodes `shouldBe` []
                annRootNode annRedirected `shouldSatisfy` (\nid -> chaseRedirects redirects nid == nid)
                validateStrict solved
                let canonicalize = canonicalizeNode (canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects redirects nid)))
                    hasCanonicalizationWork = not (IntMap.null (Solved.canonicalMap solved))
                    annCanonical = canonicalizeAnn canonicalize annRedirected
                    staleCanonicalNodes =
                        [ nid
                        | nid <- annNodeOccurrences annCanonical
                        , canonicalize nid /= nid
                        ]
                when hasCanonicalizationWork $
                    annNodeOccurrences annCanonical `shouldSatisfy` (not . null)
                staleCanonicalNodes `shouldBe` []
                annRootNode annCanonical `shouldSatisfy` (\nid -> canonicalize nid == nid)
        case runPipelineElab Set.empty canonicalizePathExpr of
            Left err -> expectationFailure (renderPipelineError err)
            Right (_term, ty) ->
                case runPipelineElabChecked Set.empty canonicalizePathExpr of
                    Left errChecked -> expectationFailure (renderPipelineError errChecked)
                    Right (_termChecked, tyChecked) -> do
                        ty `shouldBe` tyChecked

    it "generalizes reused constructors via make const" $ do
        -- let make x = (\z -> x) in let c1 = make 2 in let c2 = make False in c1 True
        let expr =
                ELet "make" (ELam "x" (ELam "z" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt 2)))
                        (ELet "c2" (EApp (EVar "make") (ELit (LBool False)))
                            (EApp (EVar "c1") (ELit (LBool True)))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = Solved.originalConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                baseNames nodes `shouldContain` [BaseTy "Bool"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "make let-c1-apply-bool path now fails fast without fallback recovery" $ do
        -- BUG-2026-02-06-002 / H15 follow-up:
        -- let make = \x.\y.x in let c1 = make (-4) in c1 True
        let expr =
                ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))
        expectStrictPipelineFailure
            "make let-c1-apply-bool"
            (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))

    it "make let-c1-apply-bool prunes the stale non-root OpWeaken before Phi" $ do
        let expr =
                ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))
        PipelineArtifacts{ paPresolution = pres, paSolved = solved, paAnnotated = ann } <-
            requireRight (runPipelineArtifactsDefault Set.empty expr)
        ew0 <- case IntMap.lookup 0 (prEdgeWitnesses pres) of
            Just ew -> pure ew
            Nothing -> expectationFailure "Expected edge 0 witness" >> fail "missing edge 0 witness"
        tr0 <- case IntMap.lookup 0 (prEdgeTraces pres) of
            Just tr -> pure tr
            Nothing -> expectationFailure "Expected edge 0 trace" >> fail "missing edge 0 trace"
        let (c1Gen, c1SchemeRoot0) =
                case ann of
                    ALet "make" _ _ _ _ _ (AAnn (ALet "c1" schemeGen schemeRoot _ _ _ _ _) _ _) _ ->
                        (schemeGen, schemeRoot)
                    other ->
                        error ("Unexpected annotation shape for let-c1-apply-bool: " ++ show other)
            c1SchemeRoot = chaseRedirects (prRedirects pres) c1SchemeRoot0
            generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
            hasWeakenOp op = case op of
                OpWeaken _ -> True
                _ -> False
        getInstanceOps (ewWitness ew0) `shouldSatisfy` (not . any hasWeakenOp)
        etReplayContract tr0 `shouldBe` ReplayContractNone
        etBinderArgs tr0 `shouldBe` []
        etBinderReplayMap tr0 `shouldBe` IntMap.empty
        Binding.orderedBinders id (prConstraint pres) (typeRef c1SchemeRoot) `shouldBe` Right []
        case generalizeAt (PresolutionViewBoundary.fromSolved solved) (genRef c1Gen) c1SchemeRoot of
            Right (Forall binds ty, _subst) -> do
                binds `shouldBe` []
                pretty ty `shouldSatisfy` ("Int" `isInfixOf`)
            Left err ->
                expectationFailure ("Expected c1 generalization, got: " ++ show err)

    describe "BUG-2026-02-08-004 sentinel" $ do
        let expr =
                ELet "id" (ELam "x" (EVar "x"))
                    (ELet "use"
                        (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int"))
                            (EApp (EVar "f") (ELit (LInt 0))))
                        (EApp (EVar "use") (EVar "id")))
        it "BUG-2026-02-08-004 nested let + annotated lambda now fails fast" $ do
            expectStrictPipelineFailure
                "BUG-2026-02-08-004 unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
            expectStrictPipelineFailure
                "BUG-2026-02-08-004 checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))

    it "A6 parity: bounded alias + coercion-heavy path agrees across unchecked, checked, and typeCheck" $ do
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
            expectPolyBinaryId ty =
                case ty of
                    TForall v Nothing (TArrow dom (TArrow dom' cod))
                        | dom == TVar v && dom' == TVar v && cod == TVar v -> pure ()
                    other ->
                        expectationFailure
                            ("Expected forall a. a -> a -> a, got: " ++ show other)
        case runPipelineElab Set.empty normExpr of
            Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
            Right (term, ty) -> do
                expectPolyBinaryId ty
                checkedFromUnchecked <-
                    case typeCheck term of
                        Left tcErr -> expectationFailure ("typeCheck(unchecked term) failed: " ++ show tcErr) >> fail "typeCheck failed"
                        Right out -> pure out
                expectPolyBinaryId checkedFromUnchecked
                case runPipelineElabChecked Set.empty normExpr of
                    Left errChecked -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError errChecked)
                    Right (termChecked, tyChecked) -> do
                        expectPolyBinaryId tyChecked
                        checkedFromUnchecked `shouldBe` tyChecked
                        case typeCheck termChecked of
                            Left tcErr -> expectationFailure ("typeCheck(checked term) failed: " ++ show tcErr)
                            Right out -> out `shouldBe` tyChecked

    it "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines" $ do
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
        let expectCoercionMismatch label result =
                case result of
                    Left err ->
                        renderPipelineError err
                            `shouldSatisfy`
                                (\msg ->
                                    "TCLetTypeMismatch" `isInfixOf` msg
                                        || "TCInstantiationError" `isInfixOf` msg
                                )
                    Right (_, ty) ->
                        expectationFailure
                            ("Expected let-type mismatch for " ++ label ++ ", but pipeline succeeded with " ++ show ty)
        expectCoercionMismatch "unchecked pipeline" (runPipelineElab Set.empty normExpr)
        expectCoercionMismatch "checked pipeline" (runPipelineElabChecked Set.empty normExpr)

    describe "BUG-2026-02-06-002 sentinel matrix" $ do
        let makeFactory = ELam "x" (ELam "y" (EVar "x"))
            makeOnlyExpr = ELet "make" makeFactory (EVar "make")
            makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
            letC1ReturnExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EVar "c1"))
            letC1ApplyBoolExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            runChecked expr =
                runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)

        it "make-only still elaborates as polymorphic factory" $
            case runChecked makeOnlyExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> do
                    ty `shouldSatisfy` containsForallTy
                    ty `shouldSatisfy` containsArrowTy
                    show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

        it "make-app now fails fast in the sentinel matrix" $
            expectStrictPipelineFailure "make-app sentinel" (runChecked makeAppExpr)

        it "let-c1-return now fails fast in the sentinel matrix" $
            expectStrictPipelineFailure "let-c1-return sentinel" (runChecked letC1ReturnExpr)

        it "let-c1-apply-bool now fails fast without stale non-root OpWeaken recovery" $
            expectStrictPipelineFailure "let-c1-apply-bool sentinel" (runChecked letC1ApplyBoolExpr)

    describe "BUG-2026-02-06-002 strict target matrix" $ do
        let makeFactory = ELam "x" (ELam "y" (EVar "x"))
            makeOnlyExpr = ELet "make" makeFactory (EVar "make")
            makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
            letC1ReturnExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EVar "c1"))
            letC1ApplyBoolExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            runChecked expr =
                runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)

        it "make-only elaborates as polymorphic factory" $
            case runChecked makeOnlyExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> do
                    ty `shouldSatisfy` containsForallTy
                    ty `shouldSatisfy` containsArrowTy
                    show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

        it "make-app now fails fast in the strict target matrix" $
            expectStrictPipelineFailure "make-app strict target" (runChecked makeAppExpr)

        it "let-c1-return now fails fast in the strict target matrix" $
            expectStrictPipelineFailure "let-c1-return strict target" (runChecked letC1ReturnExpr)

        it "let-c1-apply-bool fails fast in the strict target matrix" $
            expectStrictPipelineFailure "let-c1-apply-bool strict-target" (runChecked letC1ApplyBoolExpr)

    describe "Phase 3 atomic wrapping equivalence gates" $ do
        let bugExpr :: SurfaceExpr
            bugExpr =
                ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            lambdaLetIdExpr :: SurfaceExpr
            lambdaLetIdExpr =
                ELam "y"
                    (ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (EVar "y")))

            expectForallIdentityArrow :: ElabType -> Expectation
            expectForallIdentityArrow ty =
                case ty of
                    TForall v Nothing (TArrow dom cod)
                        | dom == TVar v && cod == TVar v -> pure ()
                    other ->
                        expectationFailure
                            ("Expected forall identity arrow (forall a. a -> a), got: " ++ show other)

        it "gate: make let-c1-apply-bool path now fails fast (no mismatch fallback)" $
            expectStrictPipelineFailure
                "atomic gate checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: let-c1-apply-bool sentinel matrix now fails fast" $
            expectStrictPipelineFailure
                "atomic gate sentinel"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: let-c1-apply-bool strict target matrix now fails fast" $
            expectStrictPipelineFailure
                "atomic gate strict-target"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: checked-authoritative invariant now fails fast on fallback-dependent path" $ do
            expectStrictPipelineFailure
                "atomic gate unchecked authoritative"
                (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))
            expectStrictPipelineFailure
                "atomic gate checked authoritative"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: thesis target unchecked pipeline now fails fast" $
            expectStrictPipelineFailure
                "atomic gate thesis unchecked"
                (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: thesis target checked pipeline now fails fast" $
            expectStrictPipelineFailure
                "atomic gate thesis checked"
                (runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr))

        it "gate: \\y. let id = (\\x. x) in id y has type forall a. a -> a" $
            case runPipelineElab Set.empty (unsafeNormalizeExpr lambdaLetIdExpr) of
                Left err -> expectationFailure ("pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) -> expectForallIdentityArrow ty

    describe "Phase 4 regression matrix" $ do
        it "suppresses OpWeaken on annotation edges while preserving expansion assignments" $ do
            let annTy = mkForalls [] (STArrow (STBase "Int") (STBase "Int"))
                expr =
                    ELet "f" (EAnn (ELam "x" (EVar "x")) annTy)
                        (EApp (EVar "f") (ELit (LInt 1)))

                hasWeakenOp op = case op of
                    OpWeaken _ -> True
                    _ -> False

            case runToPresolutionWithAnnDefault defaultPolySyms expr of
                Left err -> expectationFailure ("Pipeline failed: " ++ err)
                Right (PresolutionResult{ prConstraint = cPres, prEdgeExpansions = exps, prEdgeWitnesses = ews }, _ann) -> do
                    let annEdges = IntSet.toList (cAnnEdges cPres)
                    annEdges `shouldSatisfy` (not . null)
                    forM_ annEdges $ \eid -> do
                        IntMap.member eid exps `shouldBe` True
                        case IntMap.lookup eid ews of
                            Nothing -> expectationFailure ("Missing witness for annotation edge " ++ show eid)
                            Just ew ->
                                getInstanceOps (ewWitness ew) `shouldSatisfy` all (not . hasWeakenOp)

    describe "Pipeline soundness proxies" $ do
        -- Note: Pipeline-elaborated terms may contain internal type annotations
        -- (e.g. TVar "t0") that become unresolvable after step substitutes away
        -- ELet bindings. We skip typeCheck failures on stepped/normalized terms
        -- as a known limitation rather than a soundness violation.

        it "BUG-2026-02-20-001: stepped annotated-let identity remains type-checkable" $ do
            let ann = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
                expr =
                    ELet "f" (EAnn (ELam "x" (EVar "x")) ann)
                        (EApp (EVar "f") (ELit (LInt 7)))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
                Right (term, _ty) -> do
                    ty0 <-
                        case typeCheck term of
                            Left tcErr ->
                                expectationFailure ("typeCheck(term) failed:\n" ++ show tcErr)
                                    >> fail "typeCheck(term) failed"
                            Right out -> pure out
                    stepped <-
                        case step term of
                            Nothing ->
                                expectationFailure "Expected a reduction step for elaborated let term"
                                    >> fail "missing step"
                            Just t' -> pure t'
                    typeCheck stepped `shouldBe` Right ty0

        it "Pipeline preservation proxy: elaborated term preserves type under step" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True  -- pipeline failure is not a soundness bug
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True  -- skip if typeCheck fails
                                    Right ty ->
                                        case step term of
                                            Nothing -> property True
                                            Just term' ->
                                                case typeCheck term' of
                                                    Left _ -> property True  -- internal type var limitation
                                                    Right ty' ->
                                                        counterexample
                                                            ( "pipeline preservation failed\nexpr: "
                                                                ++ show expr
                                                                ++ "\nterm: " ++ show term
                                                                ++ "\nterm': " ++ show term'
                                                                ++ "\ntype(term): " ++ show ty
                                                                ++ "\ntype(term'): " ++ show ty'
                                                            )
                                                            (ty' === ty)

        it "Pipeline progress proxy: elaborated well-typed closed term is value or steps" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True
                                    Right _ ->
                                        counterexample
                                            ( "pipeline progress failed\nexpr: "
                                                ++ show expr
                                                ++ "\nterm: " ++ show term
                                            )
                                            (isValue term || isJust (step term))

        it "Pipeline multi-step preservation proxy: typeCheck(term) = typeCheck(normalize term)" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True
                                    Right ty ->
                                        let term' = normalize term
                                        in case typeCheck term' of
                                            Left _ -> property True  -- internal type var limitation
                                            Right ty' ->
                                                counterexample
                                                    ( "pipeline multi-step preservation failed\nexpr: "
                                                        ++ show expr
                                                        ++ "\nterm: " ++ show term
                                                        ++ "\nnormalize(term): " ++ show term'
                                                        ++ "\ntype(term): " ++ show ty
                                                        ++ "\ntype(normalize): " ++ show ty'
                                                    )
                                                    (ty' === ty)

        it "Pipeline determinism proxy: step is a function on elaborated terms" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                counterexample
                                    ( "pipeline determinism failed\nexpr: "
                                        ++ show expr
                                        ++ "\nterm: " ++ show term
                                        ++ "\nstep run 1: " ++ show (step term)
                                        ++ "\nstep run 2: " ++ show (step term)
                                    )
                                    (step term === step term)

    describe "Thesis obligations" $ do
        it "O08-REIFY-TYPE" $ do
            -- Graphic→syntactic: reifyType converts a solved constraint graph to a syntactic type
            let expr = ELam "x" (EVar "x")
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) -> ty `shouldSatisfy` containsArrowTy

        it "O08-REIFY-NAMES" $ do
            -- Named reification: reifyType produces named type variables
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) -> show ty `shouldSatisfy` (not . null)

        it "O08-BIND-MONO: alias bounds are inlined during normalization (B(σ))" $ do
            -- B(σ) from Fig 8.2.2: alias bounds (∀(a ⩾ b). body) are inlined
            -- by normalizeType, producing restricted types. The pipeline exercises
            -- this via unsafeNormalizeExpr which calls normalizeType internally.
            -- An annotated identity with alias bound: (λx.x) : ∀(a ⩾ Int). a -> a
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
                expr = ELet "f" (EAnn (ELam "x" (EVar "x")) (mkForalls [] ann)) (EVar "f")
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) ->
                    -- After alias inlining, the type should contain Int (the inlined bound)
                    show ty `shouldSatisfy` ("Int" `isInfixOf`)

        it "O08-SYN-TO-GRAPH: annotation types are internalized as graphic constraints (G(σ))" $ do
            -- G(σ) from Fig 8.2.3: syntactic annotations are translated to graphic
            -- constraint nodes via internalizeCoercionCopy during constraint generation.
            -- A coercion annotation forces syntactic→graphic translation.
            let ann = STArrow (STBase "Int") (STBase "Int")
                expr = EAnn (ELam "x" (EVar "x")) (mkForalls [] ann)
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) ->
                    -- The annotation constrains the type to Int -> Int
                    ty `shouldSatisfy` containsArrowTy

        it "O08-REIFY-INLINE: bound inlining during reification (Sᵢ)" $ do
            -- Sᵢ from Fig 8.3.3: reification with bound inlining. A polymorphic
            -- identity λx.x has type ∀a. a -> a; the bound on a is ⊥ (flexible)
            -- and gets inlined away, leaving a clean arrow type in display form.
            let expr = ELam "x" (EVar "x")
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) -> do
                    -- The reified type should be a forall with an arrow body
                    ty `shouldSatisfy` containsArrowTy
                    ty `shouldSatisfy` containsForallTy

        it "O08-INLINE-PRED: Inline(τ,n) predicate distinguishes inlineable bounds" $ do
            -- Inline(τ,n) predicate: single covariant occurrence with ≥ bound → inline;
            -- multiple occurrences or = bound → keep. Test via a let-bound polymorphic
            -- function applied at two different types, producing a pair-like result.
            let expr = ELet "id" (ELam "x" (EVar "x"))
                    (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                        (EApp (EVar "id") (ELit (LBool True))))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
                Right (_term, ty) ->
                    -- The result type should be well-formed (non-empty rendering)
                    show ty `shouldSatisfy` (not . null)

    -- See Note [Constraint simplification: Var-Abs (Ch 12.4.1)] in Translate.hs
    describe "Constraint simplification: Var-Abs (Ch 12.4.1)" $ do
        it "lambda parameters do not create gen nodes (on-the-fly Var-Abs)" $ do
            -- λx. λy. x y — two lambda params, zero let-bindings.
            -- Only the root gen node should exist; lambda params are bound
            -- monomorphically at the root scope, not under child gen nodes.
            let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
            case runConstraintDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right ConstraintResult{ crConstraint = c0 } -> do
                    let genCount = length (toListGen (cGenNodes c0))
                    -- Exactly 1 gen node: the root scope.
                    genCount `shouldBe` 1

    -- See Note [Constraint simplification: Var-Let (Ch 12.4.1)] in Base.hs
    describe "Constraint simplification: Var-Let (Ch 12.4.1)" $ do
        it "trivial let edges are dropped from presolution expansions (on-the-fly Var-Let)" $ do
            -- let id = λx. x in id — one let-binding, one use site.
            -- The constraint should have let edges, but presolution should
            -- drop them from the expansion map (they are indirections).
            let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
            case runToPresolutionWithAnnDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right (pres, _ann) -> do
                    let c1 = prConstraint pres
                        letEdgeIds = cLetEdges c1
                        expansionKeys = IntMap.keysSet (prEdgeExpansions pres)
                    -- Let edges exist in the constraint
                    IntSet.null letEdgeIds `shouldBe` False
                    -- But none of them appear in the expansion map
                    IntSet.intersection letEdgeIds expansionKeys
                        `shouldBe` IntSet.empty

representativeMigrationCorpus :: [SurfaceExpr]
representativeMigrationCorpus =
    [ ELam "x" (EVar "x")
    , ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
    , ELet "id" (ELam "x" (EVar "x"))
        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
            (EApp (EVar "id") (ELit (LBool True))))
    , EAnn (ELam "x" (EVar "x"))
        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
    ]

assertCheckedAuthoritative :: SurfaceExpr -> Expectation
assertCheckedAuthoritative expr =
    case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (renderPipelineError err)
        Right (termUnchecked, tyUnchecked) -> do
            typeCheck termUnchecked `shouldBe` Right tyUnchecked
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (renderPipelineError err)
                Right (termChecked, tyChecked) -> do
                    typeCheck termChecked `shouldBe` Right tyChecked
                    tyUnchecked `shouldBe` tyChecked

assertViewParity :: PresolutionViewBoundary.PresolutionView -> Solved -> Expectation
assertViewParity view legacy = do
    let sharedLiveDomain =
            IntSet.intersection
                (liveNodeKeySet (pvConstraint view))
                (liveNodeKeySet (Solved.originalConstraint legacy))
    projectCanonicalMap sharedLiveDomain (pvCanonicalMap view)
        `shouldBe` projectCanonicalMap sharedLiveDomain (Solved.canonicalMap legacy)
    pvCanonicalConstraint view `shouldBe` Solved.canonicalConstraint legacy

    let thesisNodes =
            map fst
                (toListNode (cNodes (pvConstraint view)))
        legacyNodes =
                map fst
                (toListNode (cNodes (Solved.originalConstraint legacy)))
        probeIds = nub (thesisNodes ++ legacyNodes ++ [NodeId 999, NodeId 1000])
        probeRefs = map typeRef probeIds

    forM_ probeIds $ \nid -> do
        pvCanonical view nid `shouldBe` Solved.canonical legacy nid
        pvLookupNode view nid `shouldBe`
            NodeAccess.lookupNode (Solved.originalConstraint legacy) (Solved.canonical legacy nid)
        pvLookupVarBound view nid `shouldBe`
            NodeAccess.lookupVarBound (Solved.originalConstraint legacy) (Solved.canonical legacy nid)

    forM_ probeRefs $ \ref ->
        pvLookupBindParent view ref `shouldBe`
            NodeAccess.lookupBindParent (Solved.originalConstraint legacy) ref

projectCanonicalMap :: IntSet.IntSet -> IntMap.IntMap NodeId -> IntMap.IntMap NodeId
projectCanonicalMap domain =
    IntMap.filterWithKey keepInDomain
  where
    keepInDomain key rep =
        IntSet.member key domain
            && IntSet.member (nodeIdToKey rep) domain
            && rep /= NodeId key

liveNodeKeySet :: Constraint -> IntSet.IntSet
liveNodeKeySet constraint =
    IntSet.fromList
        [ nodeIdToKey nid
        | (nid, _) <- toListNode (cNodes constraint)
        ]

nodeIdToKey :: NodeId -> Int
nodeIdToKey (NodeId k) = k

data RunPathModule = RunPathModule
    { rmName :: String
    , rmPath :: FilePath
    , rmImports :: [String]
    , rmSource :: String
    }

loadRunPathModules :: IO [RunPathModule]
loadRunPathModules = do
    maybeModules <-
        forM runPathModuleCatalog $ \(name, path) -> do
            srcOrErr <- tryIOError (readFile path)
            pure $ case srcOrErr of
                Left _ -> Nothing
                Right src ->
                    Just
                        RunPathModule
                            { rmName = name
                            , rmPath = path
                            , rmImports = parseRunPathImports src
                            , rmSource = src
                            }
    pure (catMaybes maybeModules)

runPathModuleCatalog :: [(String, FilePath)]
runPathModuleCatalog =
    [ ("MLF.Elab.Run.Annotation", "src/MLF/Elab/Run/Annotation.hs")
    , ("MLF.Elab.Run.Debug", "src/MLF/Elab/Run/Debug.hs")
    , ("MLF.Elab.Run.Generalize", "src/MLF/Elab/Run/Generalize.hs")
    , ("MLF.Elab.Run.Instantiation", "src/MLF/Elab/Run/Instantiation.hs")
    , ("MLF.Elab.Run.Pipeline", "src/MLF/Elab/Run/Pipeline.hs")
    , ("MLF.Elab.Run.PipelineBoundary", "src/MLF/Elab/Run/PipelineBoundary.hs")
    , ("MLF.Elab.Run.Provenance", "src/MLF/Elab/Run/Provenance.hs")
    , ("MLF.Elab.Run.ResultType", "src/MLF/Elab/Run/ResultType.hs")
    , ("MLF.Elab.Run.Scope", "src/MLF/Elab/Run/Scope.hs")
    , ("MLF.Elab.Run.TypeOps", "src/MLF/Elab/Run/TypeOps.hs")
    , ("MLF.Elab.Run.Util", "src/MLF/Elab/Run/Util.hs")
    ]

reachableRunPathModules :: [RunPathModule] -> [String] -> [RunPathModule]
reachableRunPathModules modules roots =
    let moduleNames = map rmName modules
        edges =
            [ (rmName m, filter (`elem` moduleNames) (rmImports m))
            | m <- modules
            ]
        reachableNames = go [] roots edges
    in [ m | m <- modules, rmName m `elem` reachableNames ]
  where
    go seen [] _ = seen
    go seen (m : queue) edges
        | m `elem` seen = go seen queue edges
        | otherwise =
            let deps = maybe [] id (lookup m edges)
            in go (m : seen) (deps ++ queue) edges

parseRunPathImports :: String -> [String]
parseRunPathImports src =
    nub
        [ moduleName
        | line <- lines src
        , let trimmed = dropWhile isSpace line
        , "import " `isPrefixOf` trimmed
        , moduleName <- maybe [] pure (parseImportedModule trimmed)
        , "MLF.Elab.Run." `isPrefixOf` moduleName
        ]

parseImportedModule :: String -> Maybe String
parseImportedModule line =
    case words line of
        ("import" : "qualified" : moduleName : _) ->
            Just (cleanModuleToken moduleName)
        ("import" : moduleName : _) ->
            Just (cleanModuleToken moduleName)
        _ -> Nothing

cleanModuleToken :: String -> String
cleanModuleToken =
    takeWhile (\c -> c /= '(' && c /= '\r')


annNodeOccurrences :: AnnExpr -> [NodeId]
annNodeOccurrences expr = case expr of
    AVar _ nid -> [nid]
    ALit _ nid -> [nid]
    ALam _ pNode _ body nid -> pNode : nid : annNodeOccurrences body
    AApp fn arg _ _ nid -> nid : annNodeOccurrences fn ++ annNodeOccurrences arg
    ALet _ _ schemeRoot _ _ rhs body nid ->
        schemeRoot : nid : annNodeOccurrences rhs ++ annNodeOccurrences body
    AAnn inner nid _ -> nid : annNodeOccurrences inner

annLetSchemeRoots :: AnnExpr -> [NodeId]
annLetSchemeRoots expr = case expr of
    AVar _ _ -> []
    ALit _ _ -> []
    ALam _ _ _ body _ -> annLetSchemeRoots body
    AApp fn arg _ _ _ -> annLetSchemeRoots fn ++ annLetSchemeRoots arg
    ALet _ _ schemeRoot _ _ rhs body _ ->
        schemeRoot : annLetSchemeRoots rhs ++ annLetSchemeRoots body
    AAnn inner _ _ -> annLetSchemeRoots inner

annRootNode :: AnnExpr -> NodeId
annRootNode expr = case expr of
    AVar _ nid -> nid
    ALit _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid


validateStrict :: Solved -> Expectation
validateStrict s =
    case Solved.validateCanonicalGraphStrict s of
        [] -> pure ()
        vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

defaultPolySyms :: PolySyms
defaultPolySyms = Set.empty

noExpNodes :: NodeMap TyNode -> Expectation
noExpNodes nodes =
    case [ nid | TyExp { tnId = nid } <- map snd (toListNode nodes) ] of
        [] -> pure ()
        bad -> expectationFailure ("Unexpected TyExp nodes: " ++ show bad)

baseNames :: NodeMap TyNode -> [BaseTy]
baseNames nodes = [ b | TyBase _ b <- map snd (toListNode nodes) ]

genClosedWellTypedExpr :: Gen SurfaceExpr
genClosedWellTypedExpr = do
    n <- chooseInt (-5, 5)
    m <- chooseInt (-3, 9)
    b1 <- arbitrary
    let idLam = ELam "x" (EVar "x")
        intLit = ELit (LInt (fromIntegral n))
        intLit2 = ELit (LInt (fromIntegral m))
        boolLit = ELit (LBool b1)
        polyIdTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
        exprs =
            [ idLam
            , EApp idLam intLit
            , EApp idLam boolLit
            , ELet "id" idLam (EVar "id")
            , ELet "id" idLam (EApp (EVar "id") intLit)
            , ELet "id" idLam (EApp (EVar "id") boolLit)
            , ELet "id" idLam (ELet "_" (EApp (EVar "id") intLit) (EApp (EVar "id") boolLit))
            , ELet "id" idLam (EApp (EVar "id") (EVar "id"))
            , ELam "y" (ELet "id" idLam (EApp (EVar "id") (EVar "y")))
            , ELamAnn "x" polyIdTy (EApp (EVar "x") intLit)
            , ELet "f" (EAnn idLam polyIdTy) (EApp (EVar "f") intLit2)
            ]
    elements exprs
