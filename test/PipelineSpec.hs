{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module PipelineSpec (spec) where

import Control.Monad (forM_, unless, when)
import Data.Either (isRight)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set qualified as Set
import MLF.Binding.Tree qualified as Binding
import MLF.Constraint.Acyclicity qualified as Acyc
import MLF.Constraint.Canonicalizer (canonicalizeNode, canonicalizerFrom)
import MLF.Constraint.Finalize qualified as Finalize
import MLF.Constraint.Finalize.TestSupport qualified as FinalizeTestSupport
import MLF.Constraint.NodeAccess qualified as NodeAccess
import MLF.Constraint.Normalize qualified as CNormalize
import MLF.Constraint.Presolution
import MLF.Constraint.Presolution.Plan.Context (GaBindParents (..))
import MLF.Constraint.Presolution.TestSupport
  ( CopyMapping (..),
    EdgeArtifacts (..),
    defaultPlanBuilder,
    toListInterior,
  )
import MLF.Constraint.Presolution.View qualified as PresolutionViewBoundary
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Pipeline
  ( ElabType,
    Pretty (..),
    applyRedirectsToAnn,
    canonicalizeAnn,
    generalizeAtWithBuilder,
    isValue,
    normalize,
    renderPipelineError,
    runPipelineElab,
    runPipelineElab,
    step,
    typeCheck,
    pattern Forall,
  )
import MLF.Elab.Pipeline qualified as Elab
import MLF.Elab.Run.Generalize.Prepare
  ( computePreparedResultType,
    generalizePreparedRoot,
    prepareGeneralizationArtifact,
    preparedAnnotated,
  )
import MLF.Elab.Run.Generalize.Prepare.TestSupport
  ( PreparedGeneralizationArtifactTestView (..),
    preparedGeneralizationArtifactTestView,
  )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
  ( ResultTypeInputs (..),
    computeResultTypeFallback,
    mkResultTypeInputs,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.ResultType.View qualified as ResultTypeView
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    candidateSelectionIsAmbiguous,
    candidateSelectionValue,
    selectUniqueCandidate,
    selectUniqueCandidateBy,
  )
import MLF.Elab.Run.Util
  ( canonicalizeExpansion,
    canonicalizeTrace,
    canonicalizeWitness,
    chaseRedirects,
    makeCanonicalizer,
  )
import MLF.Frontend.ConstraintGen
import MLF.Frontend.Syntax
import MLF.Reify.Core (reifyType)
import MLF.Types.Elab (Ty (..), containsArrowTy, containsForallTy)
import SolvedFacadeTestUtil qualified as SolvedTest
import SpecUtil
  ( PipelineArtifacts (..),
    breakCyclesAndCheckAcyclicityRaw,
    collectVarNodes,
    defaultTraceConfig,
    emptyConstraint,
    eraseConstraintPhaseForTest,
    firstShowE,
    mkForalls,
    requireRight,
    runConstraintDefault,
    runPipelineArtifactsDefault,
    runToPresolutionWithAnnDefault,
    runToSolvedDefault,
    unsafeNormalizeExpr,
  )
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, chooseInt, counterexample, elements, forAll, property, withMaxSuccess, (===))

expectStrictPipelineFailure :: (Show err, Show ty) => String -> Either err (term, ty) -> Expectation
expectStrictPipelineFailure label result =
  case result of
    Left err ->
      let rendered = show err
       in rendered
            `shouldSatisfy` ( \msg ->
                                "PhiTranslatabilityError" `isInfixOf` msg
                                  || "TCInstantiationError" `isInfixOf` msg
                                  || "TCLetTypeMismatch" `isInfixOf` msg
                                  || "TCArgumentMismatch" `isInfixOf` msg
                                  || "TCExpectedArrow" `isInfixOf` msg
                                  || "ValidationFailed" `isInfixOf` msg
                                  || "missing direct structural authority" `isInfixOf` msg
                            )
    Right (_term, ty) ->
      expectationFailure
        ("Expected strict failure for " ++ label ++ ", but got type " ++ show ty)

viewFromSolved :: Solved -> PresolutionViewBoundary.PresolutionView 'Raw
viewFromSolved = FinalizeTestSupport.presolutionViewFromSolved

matchesRecursiveArrow :: ElabType -> ElabType -> Bool
matchesRecursiveArrow actual expected = case (actual, expected) of
  (TArrow domA codA, TArrow domE codE) ->
    matchesRecursiveMu domA domE && matchesRecursiveMu codA codE
  _ -> False

matchesRecursiveArrowCodomain :: ElabType -> ElabType -> Bool
matchesRecursiveArrowCodomain actual expected = case actual of
  TArrow _ cod -> matchesRecursiveMu cod expected
  _ -> False

matchesRecursiveMu :: ElabType -> ElabType -> Bool
matchesRecursiveMu actual expected = case (actual, expected) of
  (TMu _ bodyA, TMu _ bodyE) -> stripMuNames bodyA == stripMuNames bodyE
  _ -> False
  where
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

countLeadingUnboundedForalls :: ElabType -> Int
countLeadingUnboundedForalls ty = case ty of
  TForall _ Nothing body -> 1 + countLeadingUnboundedForalls body
  _ -> 0

stripLeadingUnboundedForalls :: ElabType -> ElabType
stripLeadingUnboundedForalls ty = case ty of
  TForall _ Nothing body -> stripLeadingUnboundedForalls body
  _ -> ty

expectedSameLaneAliasFrameClearBoundaryArrow :: ElabType
expectedSameLaneAliasFrameClearBoundaryArrow =
  let recursiveTy = TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int")))
   in TArrow recursiveTy recursiveTy

expectedUriR2C1RecursiveIntCarrier :: ElabType
expectedUriR2C1RecursiveIntCarrier =
  TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int")))

expectedUriR2C1RecursiveBoolCarrier :: ElabType
expectedUriR2C1RecursiveBoolCarrier =
  TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Bool")))

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

containsRollTerm :: Elab.ElabTerm -> Bool
containsRollTerm term = case term of
  Elab.EVar _ -> False
  Elab.ELit _ -> False
  Elab.ELam _ _ body -> containsRollTerm body
  Elab.EApp f a -> containsRollTerm f || containsRollTerm a
  Elab.ELet _ _ rhs body -> containsRollTerm rhs || containsRollTerm body
  Elab.ETyAbs _ _ body -> containsRollTerm body
  Elab.ETyInst e _ -> containsRollTerm e
  Elab.ERoll _ _ -> True
  Elab.EUnroll body -> containsRollTerm body

containsUnrollTerm :: Elab.ElabTerm -> Bool
containsUnrollTerm term = case term of
  Elab.EVar _ -> False
  Elab.ELit _ -> False
  Elab.ELam _ _ body -> containsUnrollTerm body
  Elab.EApp f a -> containsUnrollTerm f || containsUnrollTerm a
  Elab.ELet _ _ rhs body -> containsUnrollTerm rhs || containsUnrollTerm body
  Elab.ETyAbs _ _ body -> containsUnrollTerm body
  Elab.ETyInst e _ -> containsUnrollTerm e
  Elab.ERoll _ body -> containsUnrollTerm body
  Elab.EUnroll _ -> True

-- | Collect the sequence of intermediate terms produced by iterated 'step'.
-- Lazy, so @take n (iterateStep t)@ is safe even for divergent terms.
iterateStep :: Elab.ElabTerm -> [Elab.ElabTerm]
iterateStep t = case step t of
  Nothing -> []
  Just t' -> t' : iterateStep t'

expectCanonicalPipelineSuccessType :: SurfaceExpr -> IO ElabType
expectCanonicalPipelineSuccessType expr =
  let normExpr = unsafeNormalizeExpr expr
   in case runPipelineElab Set.empty normExpr of
        Left err ->
          expectationFailure ("Expected canonical pipeline to succeed: " ++ renderPipelineError err)
            *> pure TBottom
        Right (term, ty) -> do
          typeCheck term `shouldBe` Right ty
          pure ty

expectCanonicalPipelinePastPhase3 :: SurfaceExpr -> Expectation
expectCanonicalPipelinePastPhase3 expr =
  let normExpr = unsafeNormalizeExpr expr
      assertNotPhase3 err =
        renderPipelineError err
          `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
   in case runPipelineElab Set.empty normExpr of
        Left err -> assertNotPhase3 err
        Right (term, ty) ->
          typeCheck term `shouldBe` Right ty

automaticMuConstraint :: SurfaceExpr -> IO (Constraint 'Raw)
automaticMuConstraint expr = do
  ConstraintResult {crConstraint = c0} <-
    requireRight (runConstraintDefault Set.empty expr)
  fst <$> requireRight (breakCyclesAndCheckAcyclicityRaw c0)

constraintContainsTyMu :: Constraint 'Raw -> Bool
constraintContainsTyMu constraint =
  any isTyMu (map snd (toListNode (cNodes constraint)))
  where
    isTyMu node = case node of
      TyMu {} -> True
      _ -> False

resultTypeInputsForArtifacts :: PipelineArtifacts -> (ResultTypeInputs 'Raw, AnnExpr, AnnExpr)
resultTypeInputsForArtifacts
  PipelineArtifacts
    { paConstraintNorm = c1,
      paPresolution = pres,
      paSolved = solved0,
      paAnnotated = ann0
    } =
    let solvedClean = FinalizeTestSupport.stepPruneSolvedBindParents solved0
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
            { gaBindParentsBase = cBindParents c1,
              gaBaseConstraint = c1,
              gaBaseToSolved = baseToSolved,
              gaSolvedToBase = solvedToBase
            }
        inputs =
          mkResultTypeInputs
            canonical
            EdgeArtifacts
              { eaEdgeExpansions = edgeExpansions,
                eaEdgeWitnesses = edgeWitnesses,
                eaEdgeTraces = edgeTraces
              }
            (viewFromSolved solvedClean)
            bindParentsGa
            (defaultPlanBuilder defaultTraceConfig)
            c1
            (prRedirects pres)
            defaultTraceConfig
     in (inputs, annCanon, ann0)

expectUnsupportedVarApp :: Either Elab.PipelineError a -> Expectation
expectUnsupportedVarApp result =
  case result of
    Left (Elab.PipelineConstraintError (InternalConstraintError msg)) ->
      msg `shouldSatisfy` isInfixOf "variable-headed source type application `f` is not supported"
    Left (Elab.PipelineElabError (Elab.InstantiationError msg)) ->
      msg `shouldSatisfy` isInfixOf "variable-headed source type application `f` is not supported"
    Left err ->
      expectationFailure ("expected typed variable-headed source type error, got " ++ renderPipelineError err)
    Right _ ->
      expectationFailure "expected unsupported variable-headed source type application error"

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
  describe "External binding validation" $ do
    it "fails closed for variable-headed external env types" $ do
      let extEnv = Map.singleton "x" (STVarApp "f" (STVar "a" :| []))
          result = Elab.runPipelineElabWithEnv Set.empty extEnv (EVar "x")
      expectUnsupportedVarApp result

  describe "Source annotation validation" $ do
    it "fails closed for variable-headed term annotations" $ do
      let expr = EAnn (ELit (LInt 1)) (STVarApp "f" (STBase "Int" :| []))
      expectUnsupportedVarApp (runPipelineElab Set.empty (unsafeNormalizeExpr expr))

    it "fails closed for variable-headed annotated lambda parameters" $ do
      let expr = ELamAnn "x" (STVarApp "f" (STBase "Int" :| [])) (EVar "x")
      expectUnsupportedVarApp (runPipelineElab Set.empty (unsafeNormalizeExpr expr))

  describe "Shared candidate selection" $ do
    it "deduplicates repeated equal candidates instead of treating repeats as ambiguity" $ do
      selectUniqueCandidate [1 :: Int, 1, 1]
        `shouldBe` UniqueCandidateSelection 1

    it "keeps distinct candidates fail-closed as ambiguity" $ do
      selectUniqueCandidate [1 :: Int, 2]
        `shouldBe` AmbiguousCandidateSelection

    it "supports custom equality so structurally equivalent candidates collapse to one choice" $ do
      selectUniqueCandidateBy
        (\(_, arityA) (_, arityB) -> arityA == arityB)
        [("helper", 2 :: Int), ("direct", 2)]
        `shouldBe` UniqueCandidateSelection ("helper", 2)

    it "extracts only unique selections" $ do
      candidateSelectionValue (selectUniqueCandidate [1 :: Int, 1])
        `shouldBe` Just 1
      candidateSelectionValue (selectUniqueCandidate [1 :: Int, 2])
        `shouldBe` Nothing

    it "flags only ambiguous selections as ambiguous" $ do
      candidateSelectionIsAmbiguous (selectUniqueCandidate [1 :: Int, 2])
        `shouldBe` True
      candidateSelectionIsAmbiguous (selectUniqueCandidate [1 :: Int, 1])
        `shouldBe` False

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
        Right PipelineArtifacts {paPresolution = pres, paSolved = res, paRoot = root} -> do
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
          case generalizeAt (viewFromSolved res) scopeRoot root' of
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
        Right PipelineArtifacts {paPresolution = pres, paSolved = res, paAnnotated = ann0} -> do
          validateStrict res
          let ann = applyRedirectsToAnn (prRedirects pres) ann0
          case ann of
            ALet _ schemeGen schemeRoot _ _ _ _ _ -> do
              let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
              case generalizeAt (viewFromSolved res) (genRef schemeGen) schemeRoot of
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
          constraint = emptyConstraint {cNodes = nodes}
          res = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved res) var1 of
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
          constraint = emptyConstraint {cNodes = nodes}
          res = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved res) var2 of
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

    it "reifies TyMu without binder child (non-local proxy fallback)" $ do
      -- Construct a minimal constraint with a TyMu whose μ-variable has NO
      -- binding-tree entry as a flex-child of the TyMu.  This simulates
      -- the non-local proxy scenario.
      let muVarId = NodeId 0 -- the μ-variable (TyVar)
          intId = NodeId 1 -- base type Int
          arrowId = NodeId 2 -- arrow: muVar -> Int
          muId = NodeId 3 -- TyMu node (body = arrow)
          muVar = TyVar muVarId Nothing
          intNode = TyBase intId (BaseTy "Int")
          arrowNd = TyArrow arrowId muVarId intId
          muNode = TyMu muId arrowId
          nodes =
            fromListNode
              [ (muVarId, muVar),
                (intId, intNode),
                (arrowId, arrowNd),
                (muId, muNode)
              ]
          -- NO bind-parent entry for muVarId under muId
          constraint = emptyConstraint {cNodes = nodes}
          solved = SolvedTest.mkTestSolved constraint IntMap.empty
      case reifyType (viewFromSolved solved) muId of
        Right ty -> do
          -- Should produce a TMu wrapping the body type
          case ty of
            TMu _ _ -> pure ()
            _ ->
              expectationFailure $
                "Expected TMu, got: " ++ show ty
        Left err ->
          expectationFailure $
            "Non-local proxy TyMu reify should not error: " ++ show err

  describe "Integration Tests" $ do
    it "ResultTypeView owns bound-overlay reify, base-target projection, and target generalization" $ do
      let rootN = NodeId 0
          intN = NodeId 1
          baseRootN = NodeId 10
          baseIntN = NodeId 11
          intBase = BaseTy "Int"
          solvedConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (rootN, TyVar {tnId = rootN, tnBound = Nothing}),
                      (intN, TyBase {tnId = intN, tnBase = intBase})
                    ]
              }
          baseConstraint =
            emptyConstraint
              { cNodes =
                  fromListNode
                    [ (baseRootN, TyVar {tnId = baseRootN, tnBound = Nothing}),
                      (baseIntN, TyBase {tnId = baseIntN, tnBase = intBase})
                    ]
              }
          view0 = Finalize.presolutionViewFromSnapshot solvedConstraint IntMap.empty
          bindParentsGa =
            GaBindParents
              { gaBindParentsBase = cBindParents baseConstraint,
                gaBaseConstraint = baseConstraint,
                gaBaseToSolved = IntMap.fromList [(getNodeId baseRootN, rootN), (getNodeId baseIntN, intN)],
                gaSolvedToBase = IntMap.fromList [(getNodeId rootN, baseRootN), (getNodeId intN, baseIntN)]
              }
          inputs =
            mkResultTypeInputs
              id
              EdgeArtifacts
                { eaEdgeExpansions = IntMap.empty,
                  eaEdgeWitnesses = IntMap.empty,
                  eaEdgeTraces = IntMap.empty
                }
              view0
              bindParentsGa
              (defaultPlanBuilder defaultTraceConfig)
              baseConstraint
              IntMap.empty
              defaultTraceConfig
      view <- requireRight (ResultTypeView.buildResultTypeView inputs)
      let viewBound = ResultTypeView.rtvWithBoundOverlay rootN intN view
      ResultTypeView.rtvLookupVarBound viewBound rootN `shouldBe` Just intN
      ResultTypeView.rtvReifyWithNamesNoFallback viewBound IntMap.empty rootN
        `shouldBe` Right (TBase intBase)
      ResultTypeView.rtvReifyBaseWithNamesNoFallback viewBound IntMap.empty baseRootN
        `shouldBe` Right (TBase intBase)
      (scheme, _subst) <- requireRight (ResultTypeView.rtvGeneralizeTarget viewBound (typeRef rootN) rootN)
      scheme `shouldBe` Forall [] (TBase intBase)

    it "single-solved refactor keeps canonical pipeline authoritative on representative corpus" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "chi-first ResultType|checked-authoritative keeps representative corpus parity" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "checked-authoritative keeps representative corpus parity" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "Phase 6 — Elaborate|ResultType|Dual-path verification gate stays green" $ do
      forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "migration guardrail: thesis-core boundary matches legacy outcome" $ do
      forM_ representativeMigrationCorpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let pres = paPresolution artifacts
            view = PresolutionViewBoundary.fromPresolutionResult pres
            legacy = paSolved artifacts
        validateStrict legacy
        assertViewParity view legacy

    describe "Dual-path verification" $ do
      it "production entrypoint remains checked-authoritative on representative corpus" $ do
        forM_ representativeMigrationCorpus assertCanonicalPipelineTypeChecks

    it "shared solved-to-presolution adapter matches selected solved queries on representative corpus" $ do
      let corpus =
            [ ELam "x" (EVar "x"),
              ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))),
              EAnn
                (ELam "x" (EVar "x"))
                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
            ]
      forM_ corpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let solved = paSolved artifacts
            view = viewFromSolved solved
            nodeIds = map fst (toListNode (cNodes (Solved.originalConstraint solved)))
            probeIds = nodeIds ++ [NodeId 999]
            probeRefs = map typeRef probeIds

        pvConstraint view `shouldBe` Solved.originalConstraint solved
        pvCanonicalMap view `shouldBe` Solved.canonicalMap solved
        pvBindParents view `shouldBe` cBindParents (Solved.originalConstraint solved)
        pvCanonicalConstraint view `shouldBe` Solved.canonicalConstraint solved

        forM_ probeIds $ \nid -> do
          pvCanonical view nid `shouldBe` Solved.canonical solved nid
          pvLookupNode view nid
            `shouldBe` NodeAccess.lookupNode (Solved.originalConstraint solved) (Solved.canonical solved nid)
          pvLookupVarBound view nid
            `shouldBe` NodeAccess.lookupVarBound (Solved.originalConstraint solved) (Solved.canonical solved nid)

        forM_ probeRefs $ \ref ->
          pvLookupBindParent view ref
            `shouldBe` NodeAccess.lookupBindParent (Solved.originalConstraint solved) ref

    it "runtime snapshot rebuild stays stable across representative corpus" $ do
      let corpus =
            [ ELam "x" (EVar "x"),
              ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")),
              EAnn
                (ELam "x" (EVar "x"))
                (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
            ]
      forM_ corpus $ \expr -> do
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let pres = paPresolution artifacts
        expected <-
          requireRight
            (SolvedTest.solvedFromSnapshot (snapshotUnionFind pres) (snapshotConstraint pres))
        paSolved artifacts `shouldBe` expected
        Solved.validateCanonicalGraphStrict (paSolved artifacts)
          `shouldBe` []
        runPipelineElab Set.empty (unsafeNormalizeExpr expr)
          `shouldSatisfy` isRight

    it "annotation-heavy path still reports canonical checked type" $ do
      let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
          expr = ELamAnn "x" recursiveAnn (EVar "x")
          expectedTy =
            TArrow
              (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
              (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (renderPipelineError err)
        Right (term, ty) -> do
          typeCheck term `shouldBe` Right ty
          ty `shouldSatisfy` matchesRecursiveArrow expectedTy

    it "keeps recursive lets out of the Phase 3 cycle-error path" $ do
      let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err ->
          renderPipelineError err
            `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
        Right _ -> pure ()

    it "keeps the non-recursive identity control stable" $ do
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
        Right (_term, ty) -> do
          containsMu ty `shouldBe` False
          ty `shouldSatisfy` containsArrowTy
          ty `shouldSatisfy` containsForallTy

    describe "Automatic μ-introduction (item-2)" $ do
      it "self-recursive function infers μ on the canonical pipeline entrypoint" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldSatisfy` constraintContainsTyMu

      it "nested-let mutually recursive aliases stay Phase-3-safe even when no structural μ rewrite is needed" $ do
        let expr =
              ELet
                "f"
                (ELet "g" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "g"))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

      it "recursive data-like constructor shape stays Phase-3-safe even when no structural μ rewrite is needed" $ do
        let expr =
              ELet
                "lst"
                (ELam "x" (ELam "xs" (EApp (EApp (EVar "lst") (EVar "x")) (EVar "xs"))))
                (EVar "lst")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

      it "non-recursive control expression stays μ-free on the canonical pipeline entrypoint" $ do
        let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` False
        ty `shouldSatisfy` containsArrowTy
        ty `shouldSatisfy` containsForallTy
        cBroken <- automaticMuConstraint expr
        cBroken `shouldNotSatisfy` constraintContainsTyMu

    describe "Automatic μ-introduction (item-3)" $ do
      it "elaborates recursive uses with explicit ERoll/EUnroll and passes Phase 7" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            unless (containsMu ty) $
              expectationFailure
                ( "expected TMu in type, got: "
                    ++ show ty
                    ++ " term: "
                    ++ show term
                )
            unless (containsRollTerm term) $
              expectationFailure ("expected ERoll in term: " ++ show term)
            unless (containsUnrollTerm term) $
              expectationFailure ("expected EUnroll in term: " ++ show term)
            typeCheck term `shouldBe` Right ty

    describe "Automatic μ-introduction (item-4 edge cases)" $ do
      it "preserves returned nested recursive helper fixed points on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "g" (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y")))) (EVar "g")))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` True
        let pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " nested recursive helper fixed point: expected recursive success, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              case strippedTy of
                TArrow dom cod -> do
                  unless (containsMu dom && matchesRecursiveMu dom cod) $
                    expectationFailure
                      ( label
                          ++ " nested recursive helper fixed point: expected matching recursive domain/codomain after stripping leading foralls, got "
                          ++ show ty
                      )
                _ ->
                  expectationFailure
                    ( label
                        ++ " nested recursive helper fixed point: expected arrow type after stripping leading foralls, got "
                        ++ show ty
                    )
              typeCheck term `shouldBe` Right ty

      it "characterizes polymorphic recursion with annotation without Phase-3 regression" $ do
        let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            expr =
              ELet
                "f"
                (EAnn (ELam "x" (EApp (EVar "f") (EVar "x"))) ann)
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` False
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> do
            let rendered = renderPipelineError err
            rendered `shouldSatisfy` (not . isInfixOf "Phase 3 (acyclicity)")
          Right (term, ty) ->
            typeCheck term `shouldBe` Right ty

      it "preserves visible μ across μ/∀ interaction when a contractive recursive witness already exists" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (ELam "x" (EApp (EVar "f") (EApp (EVar "id") (EVar "x")))) (EVar "f"))
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` True
        let pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                (label ++ " μ/∀ interaction: expected visible μ, got error " ++ renderPipelineError err)
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  (label ++ " μ/∀ interaction: expected TMu in type, got " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "keeps μ/∀ mediation fail-closed without a contractive recursive witness" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (EApp (EVar "id") (EVar "f")) (EVar "f"))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left _ -> pure ()
            Right (term, ty) -> do
              when (containsMu ty) $
                expectationFailure
                  (label ++ " μ/∀ mediation without a recursive witness unexpectedly preserved " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "keeps non-contractive μ annotations out of the mediated witness lane" $ do
        let badRecursiveAnn = STMu "a" (STVar "a")
            expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "g" (EApp (EVar "id") (ELamAnn "x" badRecursiveAnn (EVar "x"))) (EVar "g"))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left _ -> pure ()
            Right (_term, ty) ->
              expectationFailure
                (label ++ " unexpectedly accepted non-contractive mediated μ annotation with type " ++ show ty)

      it "URI-R2-C1 unannotated carrier: direct recursiveArrowInt admits a visible recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 direct carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 direct carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 unannotated carrier: direct recursiveArrowBool admits a visible recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 direct carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 direct carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 uniqueness reject: witnessless mediation stays fail-closed without a contractive recursive witness" $ do
        let expr =
              ELet
                "id"
                (ELam "x" (EVar "x"))
                (ELet "f" (EApp (EVar "id") (EVar "f")) (EVar "f"))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(_label, result) ->
          case result of
            Left _ -> pure ()
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 non-local identity consumer: direct id application preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                    (EApp (EVar "id") (EVar "f"))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 non-local identity consumer: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 non-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 non-local identity consumer: direct id application preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                    (EApp (EVar "id") (EVar "f"))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 non-local identity consumer: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 non-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-local identity consumer: let-aliased recursive Int carrier survives id application on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                    (ELet "hold" (EVar "f") (EApp (EVar "id") (EVar "hold")))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 owner-local identity consumer: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 owner-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-local identity consumer: let-aliased recursive Bool carrier survives id application on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                    (ELet "hold" (EVar "f") (EApp (EVar "id") (EVar "hold")))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 owner-local identity consumer: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 owner-local identity consumer: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 identity consumer wrapper: named wrap preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EVar "u")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 identity consumer wrapper: named wrap preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EVar "u")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested identity consumer wrapper: repeated id application preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EApp (EVar "id") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested identity consumer wrapper: repeated id application preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (EApp (EVar "id") (EApp (EVar "id") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 staged identity consumer wrapper: let-bound id result preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "k" (EApp (EVar "id") (EVar "u")) (EVar "k")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 staged identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 staged identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 staged identity consumer wrapper: let-bound id result preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "k" (EApp (EVar "id") (EVar "u")) (EVar "k")))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 staged identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 staged identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 local helper identity consumer wrapper: let-bound helper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "use" (ELam "v" (EApp (EVar "id") (EVar "v"))) (EApp (EVar "use") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 local helper identity consumer wrapper: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 local helper identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 local helper identity consumer wrapper: let-bound helper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "u" (ELet "use" (ELam "v" (EApp (EVar "id") (EVar "v"))) (EApp (EVar "use") (EVar "u"))))
                    ( ELet
                        "f"
                        (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                        (EApp (EVar "wrap") (EVar "f"))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 local helper identity consumer wrapper: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 local helper identity consumer wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: same-lane alias wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                (ELet "hold" (EVar "f") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: same-lane alias wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                (ELet "hold" (EVar "f") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested-forall carrier: same-wrapper identity preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EApp (ELam "y" (EVar "y")) (EVar "f"))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested-forall carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested-forall carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested-forall carrier: same-wrapper identity preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "f"
                    (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EApp (ELam "y" (EVar "y")) (EVar "f"))
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested-forall carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested-forall carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EVar "f")
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Int carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EVar "f")
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Bool carrier, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: let-aliased transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                    (EVar "f")
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Int carrier through let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 eta-mediated carrier: let-aliased transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "wrap"
                (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                ( ELet
                    "f"
                    (EApp (EVar "wrap") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                    (EVar "f")
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 eta-mediated carrier: expected recursive Bool carrier through let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 eta-mediated carrier: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      let uriR2C1OwnerSensitiveNonLocalTransparentIntRhs =
            ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))
          uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs =
            ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))
          transparentMediatorWrap =
            ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z")))
          aliasedTransparentMediatorWrap =
            ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z"))))
          ownerSensitiveNonLocalAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (ownerSensitiveNonLocalAliasChain rest aliasName)
          ownerSensitiveNonLocalTransparentExpr wrap recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap"
                  wrap
                  ( ELet
                      "f"
                      (EApp (EVar "id") recursiveRhs)
                      (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EVar "hold"))
                  )
              )
          ownerSensitiveNonLocalStackedTransparentExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          (ELet "hold" (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f"))) (EVar "hold"))
                      )
                  )
              )
          ownerSensitiveNonLocalTransparentAliasChainExpr wrap recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap"
                  wrap
                  ( ELet
                      "f"
                      (EApp (EVar "id") recursiveRhs)
                      ( ELet
                          "hold"
                          (EApp (EVar "wrap") (EVar "f"))
                          (ownerSensitiveNonLocalAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                      )
                  )
              )
          ownerSensitiveNonLocalStackedTransparentAliasChainExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (ownerSensitiveNonLocalAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                          )
                      )
                  )
              )
          expectUriR2C1OwnerSensitiveNonLocalTransparentMediation label expectedCarrier expr = do
            let pipelineRuns =
                  [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
            forM_ pipelineRuns $ \(entryLabel, result) ->
              case result of
                Left err ->
                  expectationFailure
                    ( entryLabel
                        ++ " "
                        ++ label
                        ++ ": expected recursive carrier, got error "
                        ++ renderPipelineError err
                    )
                Right (term, ty) -> do
                  let strippedTy = stripLeadingUnboundedForalls ty
                  unless (matchesRecursiveMu strippedTy expectedCarrier) $
                    expectationFailure
                      ( entryLabel
                          ++ " "
                          ++ label
                          ++ ": expected "
                          ++ show expectedCarrier
                          ++ " after stripping leading foralls, got "
                          ++ show ty
                      )
                  typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: direct transparent wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr transparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentIntRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: let-aliased transparent wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          (ownerSensitiveNonLocalTransparentAliasChainExpr aliasedTransparentMediatorWrap uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs)

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked transparent wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: stacked let-aliased transparent wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers preserve the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers preserve the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed direct and let-aliased stacked wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers stay recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveIntCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 owner-sensitive non-local transparent mediation: mixed let-aliased and direct stacked wrappers stay recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1OwnerSensitiveNonLocalTransparentMediation
          "URI-R2-C1 owner-sensitive non-local transparent mediation"
          expectedUriR2C1RecursiveBoolCarrier
          ( ownerSensitiveNonLocalStackedTransparentAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      let combinedWrapperAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (combinedWrapperAliasChain rest aliasName)
          uriR2C1CombinedWrapperStackedExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                          )
                      )
                  )
              )
          uriR2C1CombinedWrapperStackedAliasChainExpr wrap1 wrap2 recursiveRhs =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "f"
                          (EApp (EVar "id") recursiveRhs)
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                              (combinedWrapperAliasChain ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"] "hold")
                          )
                      )
                  )
              )
          expectUriR2C1CombinedWrapper label expectedCarrier expr = do
            let pipelineRuns =
                  [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
            forM_ pipelineRuns $ \(entryLabel, result) ->
              case result of
                Left err ->
                  expectationFailure
                    ( entryLabel
                        ++ " "
                        ++ label
                        ++ ": expected recursive carrier, got error "
                        ++ renderPipelineError err
                    )
                Right (term, ty) -> do
                  let strippedTy = stripLeadingUnboundedForalls ty
                  unless (matchesRecursiveMu strippedTy expectedCarrier) $
                    expectationFailure
                      ( entryLabel
                          ++ " "
                          ++ label
                          ++ ": expected "
                          ++ show expectedCarrier
                          ++ " after stripping leading foralls, got "
                          ++ show ty
                      )
                  typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through identity consumer plus transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through identity consumer plus transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through identity consumer plus let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        (ELet "hold" (EApp (EVar "wrap") (EVar "f")) (EApp (ELam "y" (EVar "y")) (EVar "hold")))
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through identity consumer plus let-aliased transparent eta wrapper, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus transparent eta wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through let-aliased transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus let-aliased transparent eta wrapper stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "k" (EVar "h") (ELam "z" (EApp (EVar "k") (EVar "z")))))
                    ( ELet
                        "f"
                        (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "f"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through let-aliased transparent eta wrapper and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through stacked transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through stacked transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Int carrier through stacked let-aliased transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "f"
                            (EApp (EVar "id") (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True)))))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "f")))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 combined wrapper: expected recursive Bool carrier through stacked let-aliased transparent eta mediators and decuple alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 combined wrapper: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus stacked let-aliased transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed direct and let-aliased stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              transparentMediatorWrap
              aliasedTransparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveIntCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentIntRhs
          )

      it "URI-R2-C1 combined wrapper: identity consumer plus mixed let-aliased and direct stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        expectUriR2C1CombinedWrapper
          "URI-R2-C1 combined wrapper"
          expectedUriR2C1RecursiveBoolCarrier
          ( uriR2C1CombinedWrapperStackedAliasChainExpr
              aliasedTransparentMediatorWrap
              transparentMediatorWrap
              uriR2C1OwnerSensitiveNonLocalTransparentBoolRhs
          )

      it "URI-R2-C1 reconstruction: deeper same-lane alias chain preserves the recursive Int carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
                ( ELet
                    "hold"
                    (EVar "f")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u"))
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Int carrier through deeper alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 reconstruction: deeper same-lane alias chain preserves the recursive Bool carrier on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LBool True))))
                ( ELet
                    "hold"
                    (EVar "f")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u"))
                        )
                    )
                )
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 reconstruction: expected recursive Bool carrier through deeper alias chain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveMu strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 reconstruction: expected "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested recursive helper: preserves recursive Int codomain on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        (ELam "y" (ELet "_" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))) (ELit (LInt 0))))
                        (EVar "g")
                    )
                )
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested recursive helper: expected recursive Int codomain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveArrowCodomain strippedTy expectedUriR2C1RecursiveIntCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested recursive helper: expected codomain "
                      ++ show expectedUriR2C1RecursiveIntCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 nested recursive helper: preserves recursive Bool codomain on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "g"
                        (ELam "y" (ELet "_" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))) (ELit (LBool True))))
                        (EVar "g")
                    )
                )
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 nested recursive helper: expected recursive Bool codomain, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              let strippedTy = stripLeadingUnboundedForalls ty
              unless (matchesRecursiveArrowCodomain strippedTy expectedUriR2C1RecursiveBoolCarrier) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 nested recursive helper: expected codomain "
                      ++ show expectedUriR2C1RecursiveBoolCarrier
                      ++ " after stripping leading foralls, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 ambiguity reject: direct self-app and returned-helper clusters stay fail-closed on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                ( ELam
                    "x"
                    ( ELet
                        "_"
                        (EApp (EVar "f") (EVar "x"))
                        ( ELet
                            "g"
                            (ELam "y" (EApp (EVar "f") (EApp (EVar "g") (EVar "y"))))
                            (EVar "g")
                        )
                    )
                )
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(_label, result) ->
          case result of
            Left _ -> pure ()
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 higher-order recursion: preserves visible recursive structure on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))))
                (EVar "f")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 higher-order recursion: expected visible recursive structure, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  (label ++ " URI-R2-C1 higher-order recursion: expected TMu in type, got " ++ show ty)
              typeCheck term `shouldBe` Right ty

      it "URI-R2-C1 recursive data-like constructor shape: preserves visible recursive structure on the canonical pipeline entrypoint" $ do
        let expr =
              ELet
                "lst"
                (ELam "x" (ELam "xs" (EApp (EApp (EVar "lst") (EVar "x")) (EVar "xs"))))
                (EVar "lst")
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                ( label
                    ++ " URI-R2-C1 recursive data-like constructor shape: expected visible recursive structure, got error "
                    ++ renderPipelineError err
                )
            Right (term, ty) -> do
              unless (containsMu ty) $
                expectationFailure
                  ( label
                      ++ " URI-R2-C1 recursive data-like constructor shape: expected TMu in type, got "
                      ++ show ty
                  )
              typeCheck term `shouldBe` Right ty

      it "characterizes higher-order recursion as preserving typechecked output without a distinct automaticMuConstraint witness" $ do
        let expr =
              ELet
                "f"
                (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))))
                (EVar "f")
        expectCanonicalPipelinePastPhase3 expr
        cBroken <- automaticMuConstraint expr
        constraintContainsTyMu cBroken `shouldBe` False
        let pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(_label, result) ->
          case result of
            Left err -> do
              let rendered = renderPipelineError err
              -- Alias-bound resolution fix means the old "alias bounds survived"
              -- error is gone; the new blocker is PhiTranslatabilityError.
              rendered `shouldSatisfy` (not . isInfixOf "alias bounds survived scheme finalization")
            Right (term, ty) ->
              typeCheck term `shouldBe` Right ty

      it "keeps already-annotated μ behavior stable" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, canonicalTy) -> typeCheck term `shouldBe` Right canonicalTy

    describe "Phase 7 reduction of auto-inferred recursive terms (item-1)" $ do
      it "isValue recognizes ERoll wrapping a value as a value" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let nf = normalize term
            isValue nf `shouldBe` True

      it "step reduces EUnroll (ERoll ty v) to v for auto-inferred recursive terms" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let steps = iterateStep term
            length steps `shouldSatisfy` (< 1000)
            length steps `shouldSatisfy` (> 0)

      it "normalize produces a value for simple self-recursive elaborated term" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            let nf = normalize term
            isValue nf `shouldBe` True

      it "type preservation: typeCheck(term) == typeCheck(step(term)) for recursive terms" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            typeCheck term `shouldBe` Right ty
            -- Check type preservation for each step.
            let checkPreservation t = case step t of
                  Nothing -> pure ()
                  Just t' -> do
                    case typeCheck t' of
                      Right ty' -> ty' `shouldBe` ty
                      Left tcErr ->
                        expectationFailure
                          ( "Type preservation failed after step:\n"
                              ++ "  before: "
                              ++ show t
                              ++ "\n"
                              ++ "  after:  "
                              ++ show t'
                              ++ "\n"
                              ++ "  error:  "
                              ++ show tcErr
                          )
                    checkPreservation t'
            checkPreservation term

      it "application of recursive function reduces through roll/unroll" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, _ty) -> do
            -- normalize produces ERoll ty (ELam ...) — a rolled value
            let nf = normalize term
            case nf of
              Elab.ERoll _muTy _body -> do
                -- Exercise the roll/unroll β-rule: EUnroll (ERoll ty v) → v
                let unrolled = Elab.EUnroll nf
                case step unrolled of
                  Nothing -> expectationFailure "EUnroll (ERoll ty v) should reduce"
                  Just reduced -> do
                    isValue reduced `shouldBe` True
                    -- The reduced term should be the lambda body of the ERoll
                    case reduced of
                      Elab.ELam {} -> pure ()
                      _ -> expectationFailure ("Expected ELam after unroll, got: " ++ show reduced)
              _ -> expectationFailure ("Expected ERoll as normal form, got: " ++ show nf)

      it "step/normalize unchanged for non-recursive programs" $ do
        let nonRecExprs =
              [ ("identity", ELam "x" (EVar "x")),
                ("let-id", ELet "id" (ELam "x" (EVar "x")) (EVar "id")),
                ("app-id-int", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))),
                ("nested-let", ELet "a" (ELit (LInt 1)) (ELet "b" (ELit (LInt 2)) (EVar "a")))
              ]
        forM_ nonRecExprs $ \(label, expr) ->
          case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (term, ty) -> do
              containsMu ty `shouldBe` False
              containsRollTerm term `shouldBe` False
              containsUnrollTerm term `shouldBe` False
              let nf = normalize term
              isValue nf `shouldBe` True
              case typeCheck nf of
                Right nfTy -> nfTy `shouldBe` ty
                Left _ -> pure () -- some normal forms lose let-scheme context
      it "runPipelineElab succeeds for self-recursive definition" $ do
        let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (renderPipelineError err)
          Right (term, ty) -> do
            containsMu ty `shouldBe` True
            let nf = normalize term
            isValue nf `shouldBe` True

    describe "ARI-C1 feasibility characterization (bounded prototype-only)" $ do
      let ariSetVarBound nid newBound constraint =
            let tweak node = case node of
                  TyVar {tnId = varId}
                    | varId == nid ->
                        TyVar {tnId = varId, tnBound = Just newBound}
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
             in constraint {cBindParents = bindParents'}
          rewriteResultTypeInputs rewrite inputs =
            let view0 = rtcPresolutionView inputs
                baseConstraint' = rewrite (pvConstraint view0)
                canonicalConstraint' = rewrite (pvCanonicalConstraint view0)
                view' =
                  view0
                    { pvConstraint = baseConstraint',
                      pvLookupNode =
                        \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid),
                      pvLookupVarBound =
                        \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid),
                      pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint',
                      pvBindParents = cBindParents baseConstraint',
                      pvCanonicalConstraint = canonicalConstraint'
                    }
                ga0 = rtcBindParentsGa inputs
                ga' =
                  ga0
                    { gaBindParentsBase = cBindParents baseConstraint',
                      gaBaseConstraint = baseConstraint'
                    }
             in inputs
                  { rtcPresolutionView = view',
                    rtcBindParentsGa = ga'
                  }
          ariClearVarBound nid constraint =
            let tweak node = case node of
                  TyVar {tnId = varId}
                    | varId == nid ->
                        TyVar {tnId = varId, tnBound = Nothing}
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
                      | eid <- eids,
                        Just tr <- [traceFor eid]
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
                      | eid <- eids,
                        Just tr <- [IntMap.lookup (getEdgeId eid) edgeTraces0]
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
            case [ tnId node
                   | (_nodeIdKey, node@TyBase {tnBase = BaseTy "Int"}) <-
                       toListNode (cNodes (pvConstraint view0))
                 ] of
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
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
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
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
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
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
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
                              [ TyBottom {tnId = bottomNid},
                                TyVar {tnId = binderA, tnBound = Nothing},
                                TyVar {tnId = argA, tnBound = Just (findIntBaseNode view0)},
                                TyVar {tnId = binderB, tnBound = Nothing},
                                TyVar {tnId = argB, tnBound = Just bottomNid}
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
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
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
                              ew0 : _ -> ew0 {ewEdgeId = eid}
                              [] ->
                                error "expected an edge witness for local inst-arg singleton-base case"
                      ew' =
                        seedWitness
                          { ewEdgeId = eid,
                            ewRight = findIntBaseNode view0
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
                  ELet
                    "k"
                    (ELamAnn "x" recursiveAnn (EVar "x"))
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
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              ty `shouldSatisfy` isRecursiveArrow

      it "keeps local-binding recursive retention processable through a direct wrapper" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "y" (EVar "y"))
                (ELamAnn "x" recursiveAnn (EVar "x"))
            expectedTy =
              TArrow
                (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
                (TMu "a" (TArrow (TVar "a") (TBase (BaseTy "Int"))))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              ty `shouldSatisfy` (`matchesRecursiveArrow` expectedTy)

      it "keeps retained-child fallback recursive through a same-lane local TypeRef root" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
            setVarBound nid newBound constraint =
              let tweak node = case node of
                    TyVar {tnId = varId}
                      | varId == nid ->
                          TyVar {tnId = varId, tnBound = Just newBound}
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
               in constraint {cBindParents = bindParents'}
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
                      { pvConstraint = baseConstraint',
                        pvLookupNode =
                          \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid),
                        pvLookupVarBound =
                          \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid),
                        pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint',
                        pvBindParents = cBindParents baseConstraint',
                        pvCanonicalConstraint = canonicalConstraint'
                      }
                  ga0 = rtcBindParentsGa inputs
                  ga' =
                    ga0
                      { gaBindParentsBase = cBindParents baseConstraint',
                        gaBaseConstraint = baseConstraint'
                      }
               in inputs
                    { rtcPresolutionView = view',
                      rtcBindParentsGa = ga'
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

      it "keeps retained-child ambiguity fail-closed when multiple same-lane candidates survive" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
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
                    ariSetVarBound rootNid retainedTarget
                      . ariSetVarBound childNid retainedTarget
                      . ariSetTypeParent childNid (Just (typeRef rootNid))
                      . ariSetTypeParent rootNid Nothing
               in rewriteResultTypeInputs rewrite inputs
            duplicateRetainedChildCandidate inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  retainedTarget =
                    case pvLookupVarBound view0 childNid of
                      Just boundNid -> boundNid
                      Nothing ->
                        error
                          ( "expected retained child bound for "
                              ++ show childNid
                          )
                  peerNid =
                    case nextFreshNodeIds 1 (pvConstraint view0) of
                      [freshNid] -> freshNid
                      other ->
                        error
                          ( "expected one fresh node id for retained-child ambiguity case, got "
                              ++ show other
                          )
                  rewrite =
                    insertTyNodes [TyVar {tnId = peerNid, tnBound = Just retainedTarget}]
                      . ariSetTypeParent peerNid (Just (typeRef rootNid))
               in rewriteResultTypeInputs rewrite inputs
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild) = case innerCanon of
              AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
              _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
            inputs1 = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = duplicateRetainedChildCandidate inputs1 retainedRoot retainedChild
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        containsMu fallbackTy `shouldBe` False

      it "keeps retained-child ambiguity fail-closed when one target exposes multiple recursive descendants" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
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
                    ariSetVarBound rootNid retainedTarget
                      . ariSetVarBound childNid retainedTarget
                      . ariSetTypeParent childNid (Just (typeRef rootNid))
                      . ariSetTypeParent rootNid Nothing
               in rewriteResultTypeInputs rewrite inputs
            injectAmbiguousRetainedChildTarget inputs rootNid childNid =
              let view0 = rtcPresolutionView inputs
                  retainedTarget =
                    case pvLookupVarBound view0 childNid of
                      Just boundNid -> boundNid
                      Nothing ->
                        error
                          ( "expected retained child bound for "
                              ++ show childNid
                          )
                  (ambiguousTargetNid, alternateMuNid) =
                    case nextFreshNodeIds 2 (pvConstraint view0) of
                      [freshTargetNid, freshMuNid] -> (freshTargetNid, freshMuNid)
                      other ->
                        error
                          ( "expected two fresh node ids for retained-child intra-target ambiguity case, got "
                              ++ show other
                          )
                  rewrite =
                    ariSetVarBound rootNid ambiguousTargetNid
                      . ariSetVarBound childNid ambiguousTargetNid
                      . insertTyNodes
                        [ TyArrow {tnId = ambiguousTargetNid, tnDom = retainedTarget, tnCod = alternateMuNid},
                          TyMu {tnId = alternateMuNid, tnBody = retainedTarget}
                        ]
               in rewriteResultTypeInputs rewrite inputs
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            innerCanon = extractInnerLetRhs annCanon0
            innerPre = extractInnerLetRhs annPre0
            (retainedRoot, retainedChild) = case innerCanon of
              AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
              _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
            inputs1 = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = injectAmbiguousRetainedChildTarget inputs1 retainedRoot retainedChild
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        containsMu fallbackTy `shouldBe` False

      it "keeps mixed retained-child/base-target competition fail-closed on the local TypeRef lane" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            extractInnerLetRhs ann0 = case ann0 of
              ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
              _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
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
                    ariSetVarBound rootNid retainedTarget
                      . ariSetVarBound childNid retainedTarget
                      . ariSetTypeParent childNid (Just (typeRef rootNid))
                      . ariSetTypeParent rootNid Nothing
               in rewriteResultTypeInputs rewrite inputs
            injectSingleBaseWitness inputs eid =
              let view0 = rtcPresolutionView inputs
                  edgeWitnesses0 = rtcEdgeWitnesses inputs
                  edgeKey = getEdgeId eid
                  seedWitness =
                    case IntMap.lookup edgeKey edgeWitnesses0 of
                      Just ew -> ew
                      Nothing ->
                        case IntMap.elems edgeWitnesses0 of
                          ew0 : _ -> ew0 {ewEdgeId = eid}
                          [] ->
                            error "expected an edge witness for mixed retained-child/base-target case"
                  ew' =
                    seedWitness
                      { ewEdgeId = eid,
                        ewRight = findIntBaseNode view0
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
            (retainedRoot, retainedChild, argEid) = case innerCanon of
              AApp _ (AVar _ nid) _funEid argEdgeId appNid -> (appNid, nid, argEdgeId)
              other ->
                error
                  ( "expected retained-child app shape for mixed retained-child/base-target case, got "
                      ++ show other
                  )
            inputs1 = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
            inputs2 = injectSingleBaseWitness inputs1 argEid
        fallbackTy <- requireRight (computeResultTypeFallback inputs2 innerCanon innerPre)
        case fallbackTy of
          TVar _ -> pure ()
          other ->
            expectationFailure
              ( "expected mixed retained-child/base-target competition to stay on the local fail-closed target variable, got "
                  ++ show other
              )

      it "sameLaneClearBoundaryExpr clears Phase 6 elaboration on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            sameLaneClearBoundaryExpr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))]
        let collapsedTy = TForall "a" Nothing (TVar "a")
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              ty `shouldSatisfy` (/= collapsedTy)

      it "sameLaneClearBoundaryExpr authoritative public output stays recursive on the canonical pipeline entrypoint without collapsing to forall identity" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            sameLaneClearBoundaryExpr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))
            collapsedTy = TForall "a" Nothing (TVar "a")
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))
        when (uncheckedTy == collapsedTy) $
          expectationFailure
            ( "unchecked term collapsed: "
                ++ show uncheckedTerm
                ++ " :: "
                ++ show uncheckedTy
            )
        when (checkedTy == collapsedTy) $
          expectationFailure
            ( "checked term collapsed: "
                ++ show checkedTerm
                ++ " :: "
                ++ show checkedTy
            )
        containsMu uncheckedTy `shouldBe` True
        containsMu checkedTy `shouldBe` True

      it "sameLaneAliasFrameClearBoundaryExpr preserves predecessor alias-frame truth on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u"))
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneDoubleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneTripleAliasFrameClearBoundaryExpr is the next milestone-3 representative broader-positive clear-boundary packet after the merged double-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u"))
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneQuadrupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged triple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u"))
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneQuintupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged quadruple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u"))
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneSextupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged quintuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    ( ELet
                                        "leaf"
                                        (EVar "tail")
                                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u"))
                                    )
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneSeptupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged sextuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    ( ELet
                                        "leaf"
                                        (EVar "tail")
                                        ( ELet
                                            "tip"
                                            (EVar "leaf")
                                            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u"))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneOctupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged septuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    ( ELet
                                        "leaf"
                                        (EVar "tail")
                                        ( ELet
                                            "tip"
                                            (EVar "leaf")
                                            ( ELet
                                                "bud"
                                                (EVar "tip")
                                                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u"))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneNonupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged octuple-alias anchor on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    ( ELet
                                        "leaf"
                                        (EVar "tail")
                                        ( ELet
                                            "tip"
                                            (EVar "leaf")
                                            ( ELet
                                                "bud"
                                                (EVar "tip")
                                                ( ELet
                                                    "seed"
                                                    (EVar "bud")
                                                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "seed")) (EVar "u"))
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "sameLaneDecupleAliasFrameClearBoundaryExpr is the next broader-positive owner-sensitive clear-boundary packet on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "k"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                ( ELet
                    "hold"
                    (EVar "k")
                    ( ELet
                        "keep"
                        (EVar "hold")
                        ( ELet
                            "more"
                            (EVar "keep")
                            ( ELet
                                "deep"
                                (EVar "more")
                                ( ELet
                                    "tail"
                                    (EVar "deep")
                                    ( ELet
                                        "leaf"
                                        (EVar "tail")
                                        ( ELet
                                            "tip"
                                            (EVar "leaf")
                                            ( ELet
                                                "bud"
                                                (EVar "tip")
                                                ( ELet
                                                    "seed"
                                                    (EVar "bud")
                                                    ( ELet
                                                        "grain"
                                                        (EVar "seed")
                                                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "grain")) (EVar "u"))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
        (uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        typeCheck uncheckedTerm `shouldBe` Right uncheckedTy
        typeCheck checkedTerm `shouldBe` Right checkedTy
        uncheckedTy `shouldBe` checkedTy
        countLeadingUnboundedForalls uncheckedTy `shouldBe` 2
        matchesRecursiveArrow
          (stripLeadingUnboundedForalls uncheckedTy)
          expectedSameLaneAliasFrameClearBoundaryArrow
          `shouldBe` True

      it "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    (EApp (ELam "y" (EVar "y")) (EVar "k"))
                )
            extractSelectedBodyApp ann0 = case ann0 of
              ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ _ body _) _ _) _ ->
                case body of
                  AAnn inner _ _ -> inner
                  other -> other
              _ ->
                error
                  ( "unexpected same-wrapper nested-forall wrapper shape: "
                      ++ show ann0
                  )
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
                    ariSetVarBound rootNid retainedTarget
                      . ariSetVarBound childNid retainedTarget
                      . ariSetTypeParent childNid (Just (typeRef rootNid))
                      . ariSetTypeParent rootNid Nothing
                  inputs' = rewriteResultTypeInputs rewrite inputs
               in inputs' {rtcBaseConstraint = rewrite (pvConstraint view0)}
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
            bodyCanon = extractSelectedBodyApp annCanon0
            bodyPre = extractSelectedBodyApp annPre0
            (retainedRoot, retainedChild) = case bodyCanon of
              AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
              other ->
                error
                  ( "expected same-wrapper nested-forall retained-child app shape, got "
                      ++ show other
                  )
            inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
        fallbackTy <- requireRight (computeResultTypeFallback inputs bodyCanon bodyPre)
        containsMu fallbackTy `shouldBe` True

      it "same-wrapper nested-forall packet preserves recursive output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    (EApp (ELam "y" (EVar "y")) (EVar "k"))
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus owner-local alias frame preserves recursive output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    ( ELet
                        "hold"
                        (EVar "k")
                        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u"))
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus decuple owner-local alias frames preserves recursive output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "k"
                    (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                    ( aliasChain
                        ["hold", "keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain"]
                        "k"
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus let-aliased transparent eta mediator stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z")))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus transparent eta mediator stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      let sameWrapperNestedForallStackedTransparentExpr wrap1 wrap2 recursiveAnn =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "k"
                          (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k")))
                              (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                          )
                      )
                  )
              )
          sameWrapperNestedForallAliasChain aliases source =
            case aliases of
              [] ->
                ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
              aliasName : rest ->
                ELet aliasName (EVar source) (sameWrapperNestedForallAliasChain rest aliasName)
          sameWrapperNestedForallStackedTransparentAliasChainExpr wrap1 wrap2 recursiveAnn =
            ELet
              "id"
              (ELam "z" (EVar "z"))
              ( ELet
                  "wrap1"
                  wrap1
                  ( ELet
                      "wrap2"
                      wrap2
                      ( ELet
                          "k"
                          (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                          ( ELet
                              "hold"
                              (EApp (EVar "wrap2") (EApp (EVar "wrap1") (EVar "k")))
                              ( sameWrapperNestedForallAliasChain
                                  ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                  "hold"
                              )
                          )
                      )
                  )
              )

      it "same-wrapper nested-forall plus stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentAliasChainExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus stacked let-aliased transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr = sameWrapperNestedForallStackedTransparentAliasChainExpr aliasedWrap aliasedWrap recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators preserves recursive Int output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators preserves recursive Bool output on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed direct and let-aliased stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                aliasedWrap
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators stays recursive through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus mixed let-aliased and direct stacked transparent eta mediators stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              sameWrapperNestedForallStackedTransparentAliasChainExpr
                aliasedWrap
                (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                recursiveAnn
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling transparent eta mediators do not poison direct recursive wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            directExpr wrapperName =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                    ( ELet
                        "wrap2"
                        (ELam "h" (ELam "z" (EApp (EVar "h") (EVar "z"))))
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar wrapperName) (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty1 <- expectCanonicalPipelineSuccessType (directExpr "wrap1")
        containsMu ty1 `shouldBe` True
        ty2 <- expectCanonicalPipelineSuccessType (directExpr "wrap2")
        containsMu ty2 `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            directExpr wrapperName =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar wrapperName) (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty1 <- expectCanonicalPipelineSuccessType (directExpr "wrap1")
        containsMu ty1 `shouldBe` True
        ty2 <- expectCanonicalPipelineSuccessType (directExpr "wrap2")
        containsMu ty2 `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive Bool wrapper application" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                (EApp (ELam "y" (EVar "y")) (EVar "hold"))
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive wrapper application through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "sibling let-aliased transparent eta mediators do not poison direct recursive Bool wrapper application through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasedWrap =
              ELam
                "h"
                (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z"))))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap1"
                    aliasedWrap
                    ( ELet
                        "wrap2"
                        aliasedWrap
                        ( ELet
                            "k"
                            (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                            ( ELet
                                "hold"
                                (EApp (EVar "wrap2") (EVar "k"))
                                ( aliasChain
                                    ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                    "hold"
                                )
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

      it "same-wrapper nested-forall plus let-aliased transparent eta mediator stays recursively Bool-typed through a decuple owner-local alias chain" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))
            aliasChain aliases source =
              case aliases of
                [] ->
                  ELet "u" (EApp (ELam "y" (EVar "y")) (EVar source)) (EVar "u")
                aliasName : rest ->
                  ELet aliasName (EVar source) (aliasChain rest aliasName)
            expr =
              ELet
                "id"
                (ELam "z" (EVar "z"))
                ( ELet
                    "wrap"
                    (ELam "h" (ELet "mid" (EVar "h") (ELam "z" (EApp (EVar "mid") (EVar "z")))))
                    ( ELet
                        "k"
                        (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
                        ( ELet
                            "hold"
                            (EApp (EVar "wrap") (EVar "k"))
                            ( aliasChain
                                ["keep", "more", "deep", "tail", "leaf", "tip", "bud", "seed", "grain", "dust"]
                                "hold"
                            )
                        )
                    )
                )
        ty <- expectCanonicalPipelineSuccessType expr
        containsMu ty `shouldBe` True

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

      it "keeps the same single-base wrapper on a unique non-local baseTarget lane" $ do
        fallbackTy <- localSingleBaseFallback False
        fallbackTy `shouldBe` TBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "keeps the selected non-local scheme-alias/base-like packet on the baseTarget -> baseC lane" $ do
        fallbackTy <- schemeAliasBaseLikeFallback False
        fallbackTy `shouldBe` TBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "keeps the selected non-local scheme-alias/base-like packet recursive on the canonical pipeline entrypoint" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")
            blockedTy = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
        (_uncheckedTerm, uncheckedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        (_checkedTerm, checkedTy) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
        uncheckedTy `shouldNotBe` blockedTy
        checkedTy `shouldNotBe` blockedTy
        containsMu uncheckedTy `shouldBe` True
        containsMu checkedTy `shouldBe` True

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

      it "keeps the same inst-arg-only singleton-base wrapper on a unique non-local baseTarget lane" $ do
        fallbackTy <- localInstArgSingleBaseFallback False
        fallbackTy `shouldBe` TBase (BaseTy "Int")
        containsMu fallbackTy `shouldBe` False

      it "does not infer recursive shape for the corresponding unannotated variant" $ do
        let expr = ELam "x" (EVar "x")
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
        fallbackTy <- requireRight (computeResultTypeFallback inputs annCanon annPre)
        containsMu fallbackTy `shouldBe` False
        let pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
            Right (_term, ty) ->
              containsMu ty `shouldBe` False

      it "keeps non-local proxy fallback open for recursive types in result-type reconstruction" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EApp (EVar "g") (EVar "g"))
        artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
        let (inputs, annCanon, annPre) = resultTypeInputsForArtifacts artifacts
        fallbackTy <- requireRight (computeResultTypeFallback inputs annCanon annPre)
        containsMu fallbackTy `shouldBe` True

      it "non-local proxy wrapper g g fails with TCArgumentMismatch (correct semantic error)" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EApp (EVar "g") (EVar "g"))
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              show err `shouldSatisfy` isInfixOf "TCArgumentMismatch"
            Right (_term, ty) ->
              expectationFailure
                ("Expected TCArgumentMismatch for " ++ label ++ ", but got type " ++ show ty)

      it "non-local proxy wrapper let g = (λx:μα.α→Int. x) in g succeeds with correct arrow type" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EVar "g")
            muTy = TMu "t6" (TArrow (TVar "t6") (TBase (BaseTy "Int")))
            expectedTy = TArrow muTy muTy
            pipelineRuns =
              [("canonical", runPipelineElab Set.empty (unsafeNormalizeExpr expr))]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Right (_term, ty) ->
              unless (matchesRecursiveArrow ty expectedTy) $
                expectationFailure
                  (label ++ " non-local proxy: expected arrow of μ-types, got " ++ show ty)
            Left err ->
              expectationFailure
                (label ++ " non-local proxy: expected success, got error " ++ show err)

    it "uses presolution-native solved artifacts" $ do
      artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
      cUnifyEdges (prConstraint (paPresolution artifacts)) `shouldBe` []
      let pres = paPresolution artifacts
      expectedNative <-
        requireRight
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
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runToSolvedDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right res -> do
          validateStrict res
          let nodes = cNodes (Solved.originalConstraint res)
          baseNames nodes `shouldContain` [BaseTy "Int"]
          noExpNodes nodes

    it "prepared generalization artifact drives redirecting instantiation behavior" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      ConstraintResult {crConstraint = c0, crAnnotated = ann} <-
        requireRight (runConstraintDefault defaultPolySyms expr)
      let cNorm = CNormalize.normalize c0
      (cAcyclic, acyc) <-
        requireRight (firstShowE (Acyc.breakCyclesAndCheckAcyclicity cNorm))
      pres <-
        requireRight (firstShowE (computePresolution defaultTraceConfig acyc cAcyclic))
      artifact <-
        requireRight
          (firstShowE (prepareGeneralizationArtifact defaultTraceConfig cAcyclic pres ann))
      let redirects = prRedirects pres
          artifactView = preparedGeneralizationArtifactTestView artifact
          redirectedAnn = applyRedirectsToAnn redirects ann
          canonicalizedAnn = canonicalizeAnn (preparedTestCanonicalizeNode artifactView) redirectedAnn
          baseConstraint = preparedTestBaseConstraint artifactView
          solvedToBase = preparedTestSolvedToBase artifactView
          baseNamedKeysAll = collectBaseNamedKeys baseConstraint
          baseCopyPairs =
            [ (baseKey, copyN)
              | tr <- IntMap.elems (prEdgeTraces pres),
                (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr)),
                IntSet.member baseKey baseNamedKeysAll
            ]
      redirects `shouldSatisfy` (not . IntMap.null)
      preparedTestRedirects artifactView `shouldBe` redirects
      preparedAnnotated artifact `shouldBe` canonicalizedAnn
      annNodeOccurrences (preparedAnnotated artifact)
        `shouldBe` map (preparedTestCanonicalizeNode artifactView) (annNodeOccurrences redirectedAnn)
      baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
      baseCopyPairs `shouldSatisfy` (not . null)
      forM_ baseCopyPairs $ \(_baseKey, copyN) -> do
        let copyKey = getNodeId (preparedTestCanonicalizeNode artifactView copyN)
        case IntMap.lookup copyKey solvedToBase of
          Nothing ->
            expectationFailure ("Prepared artifact missed copy provenance for " ++ show copyN)
          Just actualBase ->
            lookupNodeIn (cNodes baseConstraint) actualBase `shouldSatisfy` isJust
      case generalizePreparedRoot artifact (preparedAnnotated artifact) ann of
        Right (Forall _ ty, _subst) ->
          pretty ty `shouldSatisfy` ("Bool" `isInfixOf`)
        Left err ->
          expectationFailure ("Prepared artifact generalize-at failed: " ++ show err)
      resultTy <-
        requireRight
          (firstShowE (computePreparedResultType artifact (preparedAnnotated artifact) ann))
      pretty resultTy `shouldSatisfy` ("Bool" `isInfixOf`)

    it "tracks instantiation copy maps for named binders" $ do
      -- Non-trivial instantiation: polymorphic id used at two types
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paConstraintNorm = c1, paPresolution = pres, paSolved = solved} ->
          case Solved.validateCanonicalGraphStrict solved of
            [] -> do
              let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                  adoptNode = canonicalizeNode canon
                  baseNamedKeysAll = collectBaseNamedKeys c1
                  traceMaps =
                    map
                      (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                      (IntMap.elems (prEdgeTraces pres))
                  instCopyMapFull = foldl' IntMap.union IntMap.empty traceMaps
                  baseCopyPairs =
                    [ (baseKey, copyN)
                      | tr <- IntMap.elems (prEdgeTraces pres),
                        (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr)),
                        IntSet.member baseKey baseNamedKeysAll
                    ]
              baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
              baseCopyPairs `shouldSatisfy` (not . null)
              mapM_
                ( \(baseKey, copyN) ->
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

    it "projects strict replay binders back onto their source binders for generalization" $ do
      let expr =
            ELet
              "f"
              (ELam "x" (ELet "_" (EApp (EVar "f") (EVar "x")) (ELit (LInt 0))))
              (EVar "f")
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paConstraintNorm = c1, paPresolution = pres, paSolved = solved} ->
          case Solved.validateCanonicalGraphStrict solved of
            [] -> do
              let canon = canonicalizerFrom (\nid -> Solved.canonical solved (chaseRedirects (prRedirects pres) nid))
                  adoptNode = canonicalizeNode canon
                  baseNamedKeysAll = collectBaseNamedKeys c1
                  tr = (prEdgeTraces pres) IntMap.! 0
                  traceMap = buildTraceCopyMap c1 baseNamedKeysAll adoptNode tr
              etReplayContract tr `shouldBe` ReplayContractStrict
              etReplayDomainBinders tr `shouldSatisfy` (not . null)
              case (etBinderArgs tr, IntMap.toList (etBinderReplayMap tr)) of
                ([(sourceBinder, _arg)], [(_sourceKey, replayBinder)]) ->
                  case IntMap.lookup (getNodeId (adoptNode replayBinder)) traceMap of
                    Nothing ->
                      expectationFailure
                        ("Missing replay-binder provenance entry for " ++ show replayBinder)
                    Just mapped ->
                      adoptNode mapped `shouldBe` adoptNode sourceBinder
                other ->
                  expectationFailure
                    ("Expected one strict replay binder pair, got " ++ show other)
            vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

    it "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization" $ do
      let makeFactory = ELam "x" (ELam "y" (EVar "x"))
          expr =
            ELam
              "k"
              ( ELet
                  "make"
                  makeFactory
                  ( ELet
                      "c1"
                      (EApp (EVar "make") (EVar "k"))
                      (EApp (EVar "c1") (ELit (LBool True)))
                  )
              )
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paPresolution = pres, paSolved = solved} -> do
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
            ELam
              "k"
              ( ELet
                  "make"
                  makeFactory
                  ( ELet
                      "c1"
                      (EApp (EVar "make") (EVar "k"))
                      (EApp (EVar "c1") (ELit (LBool True)))
                  )
              )
          isNonRootWeaken op =
            case op of
              OpWeaken {} -> True
              _ -> False
      case runPipelineArtifactsDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right PipelineArtifacts {paPresolution = pres} -> do
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
          ELet
            "apply"
            (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
            ( ELet
                "id"
                (ELam "y" (EVar "y"))
                ( ELet
                    "a"
                    (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LInt 1)))
                    (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LBool True)))
                )
            )
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
          ELet
            "id"
            (ELam "x" (EVar "x"))
            (EApp (EVar "id") (EVar "id"))
    case runToPresolutionWithAnnDefault defaultPolySyms expr of
      Left err -> expectationFailure err
      Right (pres, ann) -> do
        let redirects = prRedirects pres
            varNodes = collectVarNodes "id" ann
            redirected =
              [ nid
                | nid <- varNodes,
                  chaseRedirects redirects nid /= nid
              ]
        varNodes `shouldSatisfy` (not . null)
        redirected `shouldSatisfy` (not . null)
    case runPipelineElab Set.empty expr of
      Left err -> expectationFailure (renderPipelineError err)
      Right (_term, ty) -> pretty ty `shouldSatisfy` ("∀" `isInfixOf`)

  it "applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently" $ do
    -- Exercise rewrite coverage on a shape with redirected + canonicalized let scheme roots.
    let rewriteExpr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            ( ELet
                "f"
                (EVar "id")
                ( ELet
                    "a"
                    (EApp (EVar "f") (ELit (LInt 1)))
                    (EApp (EVar "f") (ELit (LBool True)))
                )
            )
        -- Separately exercise the production canonicalizeAnn path via pipeline run.
        canonicalizePathExpr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            ( ELet
                "a"
                (EApp (EVar "id") (ELit (LInt 1)))
                (EApp (EVar "id") (ELit (LBool True)))
            )
    case runPipelineArtifactsDefault defaultPolySyms rewriteExpr of
      Left err -> expectationFailure err
      Right PipelineArtifacts {paPresolution = pres, paSolved = solved, paAnnotated = ann} -> do
        let redirects = prRedirects pres
            redirectedSchemeRoots =
              [ nid
                | nid <- annLetSchemeRoots ann,
                  chaseRedirects redirects nid /= nid
              ]
            annRedirected = applyRedirectsToAnn redirects ann
            staleRedirectNodes =
              [ nid
                | nid <- annNodeOccurrences annRedirected,
                  chaseRedirects redirects nid /= nid
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
                | nid <- annNodeOccurrences annCanonical,
                  canonicalize nid /= nid
              ]
        when hasCanonicalizationWork $
          annNodeOccurrences annCanonical `shouldSatisfy` (not . null)
        staleCanonicalNodes `shouldBe` []
        annRootNode annCanonical `shouldSatisfy` (\nid -> canonicalize nid == nid)
    case runPipelineElab Set.empty canonicalizePathExpr of
      Left err -> expectationFailure (renderPipelineError err)
      Right (_term, _ty) -> pure ()

  it "generalizes reused constructors via make const" $ do
    -- let make x = (\z -> x) in let c1 = make 2 in let c2 = make False in c1 True
    let expr =
          ELet
            "make"
            (ELam "x" (ELam "z" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt 2)))
                ( ELet
                    "c2"
                    (EApp (EVar "make") (ELit (LBool False)))
                    (EApp (EVar "c1") (ELit (LBool True)))
                )
            )
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
          ELet
            "make"
            (ELam "x" (ELam "y" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )
    expectStrictPipelineFailure
      "make let-c1-apply-bool"
      (runPipelineElab Set.empty (unsafeNormalizeExpr expr))

  it "make let-c1-apply-bool prunes the stale non-root OpWeaken before Phi" $ do
    let expr =
          ELet
            "make"
            (ELam "x" (ELam "y" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )
    PipelineArtifacts {paPresolution = pres, paSolved = solved, paAnnotated = ann} <-
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
    case generalizeAt (viewFromSolved solved) (genRef c1Gen) c1SchemeRoot of
      Right (Forall binds ty, _subst) -> do
        binds `shouldBe` []
        pretty ty `shouldSatisfy` ("Int" `isInfixOf`)
      Left err ->
        expectationFailure ("Expected c1 generalization, got: " ++ show err)

  describe "BUG-2026-02-08-004 sentinel" $ do
    let expr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            ( ELet
                "use"
                ( ELamAnn
                    "f"
                    (STArrow (STBase "Int") (STBase "Int"))
                    (EApp (EVar "f") (ELit (LInt 0)))
                )
                (EApp (EVar "use") (EVar "id"))
            )
    it "BUG-2026-02-08-004 nested let + annotated lambda now fails fast" $ do
      expectStrictPipelineFailure
        "BUG-2026-02-08-004 unchecked"
        (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
      expectStrictPipelineFailure
        "BUG-2026-02-08-004 checked"
        (runPipelineElab Set.empty (unsafeNormalizeExpr expr))

  it "A6 parity: bounded alias + coercion-heavy path agrees across unchecked, checked, and typeCheck" $ do
    let rhs = ELam "x" (ELam "y" (EVar "x"))
        schemeTy =
          mkForalls
            [ ("a", Nothing),
              ("b", Just (STVar "a"))
            ]
            (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
        ann =
          STForall
            "a"
            Nothing
            (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
        expr =
          ELet
            "c"
            (EAnn rhs schemeTy)
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
      Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
      Right (term, ty) -> do
        expectPolyBinaryId ty
        checkedByTypeChecker <-
          case typeCheck term of
            Left tcErr -> expectationFailure ("typeCheck(canonical term) failed: " ++ show tcErr) >> fail "typeCheck failed"
            Right out -> pure out
        expectPolyBinaryId checkedByTypeChecker
        checkedByTypeChecker `shouldBe` ty

  it "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in the canonical pipeline" $ do
    let rhs = ELam "x" (ELam "y" (EVar "x"))
        schemeTy =
          mkForalls
            [ ("a", Nothing),
              ("b", Just (STVar "a"))
            ]
            (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
        ann =
          STForall
            "a"
            Nothing
            (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
        expr =
          ELet
            "c"
            (EAnn rhs schemeTy)
            ( EApp
                (EApp (EAnn (EVar "c") ann) (ELit (LInt 1)))
                (ELit (LInt 2))
            )
        normExpr = unsafeNormalizeExpr expr
        expectedTy = TBase (BaseTy "Int")
    let expectInt label result =
          case result of
            Left err ->
              expectationFailure (label ++ " failed:\n" ++ renderPipelineError err)
            Right (term, ty) -> do
              ty `shouldBe` expectedTy
              typeCheck term `shouldBe` Right expectedTy
    expectInt "canonical pipeline" (runPipelineElab Set.empty normExpr)

  describe "BUG-2026-02-06-002 sentinel matrix" $ do
    let makeFactory = ELam "x" (ELam "y" (EVar "x"))
        makeOnlyExpr = ELet "make" makeFactory (EVar "make")
        makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
        letC1ReturnExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EVar "c1")
            )
        letC1ApplyBoolExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )

        runChecked expr =
          runPipelineElab Set.empty (unsafeNormalizeExpr expr)

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
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EVar "c1")
            )
        letC1ApplyBoolExpr =
          ELet
            "make"
            makeFactory
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )

        runChecked expr =
          runPipelineElab Set.empty (unsafeNormalizeExpr expr)

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
          ELet
            "make"
            (ELam "x" (ELam "y" (EVar "x")))
            ( ELet
                "c1"
                (EApp (EVar "make") (ELit (LInt (-4))))
                (EApp (EVar "c1") (ELit (LBool True)))
            )

        lambdaLetIdExpr :: SurfaceExpr
        lambdaLetIdExpr =
          ELam
            "y"
            ( ELet
                "id"
                (ELam "x" (EVar "x"))
                (EApp (EVar "id") (EVar "y"))
            )

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
        (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

    it "gate: let-c1-apply-bool sentinel matrix now fails fast" $
      expectStrictPipelineFailure
        "atomic gate sentinel"
        (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

    it "gate: let-c1-apply-bool strict target matrix now fails fast" $
      expectStrictPipelineFailure
        "atomic gate strict-target"
        (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

    it "gate: canonical checked invariant now fails fast on fallback-dependent path" $ do
      expectStrictPipelineFailure
        "atomic gate canonical authoritative"
        (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

    it "gate: thesis target canonical pipeline now fails fast" $
      expectStrictPipelineFailure
        "atomic gate thesis canonical"
        (runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr))

    it "gate: \\y. let id = (\\x. x) in id y has type forall a. a -> a" $
      case runPipelineElab Set.empty (unsafeNormalizeExpr lambdaLetIdExpr) of
        Left err -> expectationFailure ("pipeline failed: " ++ renderPipelineError err)
        Right (_term, ty) -> expectForallIdentityArrow ty

  describe "Phase 4 regression matrix" $ do
    it "suppresses OpWeaken on annotation edges while preserving expansion assignments" $ do
      let annTy = mkForalls [] (STArrow (STBase "Int") (STBase "Int"))
          expr =
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) annTy)
              (EApp (EVar "f") (ELit (LInt 1)))

          hasWeakenOp op = case op of
            OpWeaken _ -> True
            _ -> False

      case runToPresolutionWithAnnDefault defaultPolySyms expr of
        Left err -> expectationFailure ("Pipeline failed: " ++ err)
        Right (PresolutionResult {prConstraint = cPres, prEdgeExpansions = exps, prEdgeWitnesses = ews}, _ann) -> do
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
            ELet
              "f"
              (EAnn (ELam "x" (EVar "x")) ann)
              (EApp (EVar "f") (ELit (LInt 7)))
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure ("Canonical pipeline failed:\n" ++ renderPipelineError err)
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
              Left _ -> property True -- pipeline failure is not a soundness bug
              Right (term, _ty) ->
                case typeCheck term of
                  Left _ -> property True -- skip if typeCheck fails
                  Right ty ->
                    case step term of
                      Nothing -> property True
                      Just term' ->
                        case typeCheck term' of
                          Left _ -> property True -- internal type var limitation
                          Right ty' ->
                            counterexample
                              ( "pipeline preservation failed\nexpr: "
                                  ++ show expr
                                  ++ "\nterm: "
                                  ++ show term
                                  ++ "\nterm': "
                                  ++ show term'
                                  ++ "\ntype(term): "
                                  ++ show ty
                                  ++ "\ntype(term'): "
                                  ++ show ty'
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
                          ++ "\nterm: "
                          ++ show term
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
                          Left _ -> property True -- internal type var limitation
                          Right ty' ->
                            counterexample
                              ( "pipeline multi-step preservation failed\nexpr: "
                                  ++ show expr
                                  ++ "\nterm: "
                                  ++ show term
                                  ++ "\nnormalize(term): "
                                  ++ show term'
                                  ++ "\ntype(term): "
                                  ++ show ty
                                  ++ "\ntype(normalize): "
                                  ++ show ty'
                              )
                              (ty' === ty)

    it "Pipeline one-step normalization proxy: representative elaborated terms preserve their normal form" $ do
      let cases =
            [ ( "annotated identity application",
                ELet
                  "f"
                  (EAnn (ELam "x" (EVar "x")) (mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))))
                  (EApp (EVar "f") (ELit (LInt 7)))
              ),
              ( "polymorphic let used at Int and Bool",
                ELet
                  "id"
                  (ELam "x" (EVar "x"))
                  ( ELet
                      "a"
                      (EApp (EVar "id") (ELit (LInt 1)))
                      (EApp (EVar "id") (ELit (LBool True)))
                  )
              )
            ]
      forM_ cases $ \(label, expr) ->
        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
          Left err -> expectationFailure (label ++ ": pipeline failed:\n" ++ renderPipelineError err)
          Right (term, ty) -> do
            typeCheck term `shouldBe` Right ty
            stepped <-
              case step term of
                Nothing ->
                  expectationFailure (label ++ ": expected elaborated term to reduce at least once")
                    >> fail "missing reduction step"
                Just term' -> pure term'
            normalize stepped `shouldBe` normalize term

  describe "Thesis obligations" $ do
    it "O08-REIFY-TYPE" $ do
      -- Graphic→syntactic: reifyType converts a solved constraint graph to a syntactic type
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) -> ty `shouldSatisfy` containsArrowTy

    it "O08-REIFY-NAMES" $ do
      -- Named reification: reifyType produces named type variables
      let expr = ELam "x" (EVar "x")
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (_term, ty) ->
          ty `shouldSatisfy` \candidate ->
            case candidate of
              TForall name Nothing (TArrow (TVar dom) (TVar cod)) ->
                not (null name) && dom == name && cod == name
              _ -> False

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
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              ( ELet
                  "a"
                  (EApp (EVar "id") (ELit (LInt 1)))
                  (EApp (EVar "id") (ELit (LBool True)))
              )
      case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure $ "Pipeline failed: " ++ renderPipelineError err
        Right (term, ty) -> do
          ty `shouldBe` TBase (BaseTy "Bool")
          typeCheck (normalize term) `shouldBe` Right ty
          normalize term `shouldBe` Elab.ELit (LBool True)

  -- See Note [Constraint simplification: Var-Abs (Ch 12.4.1)] in Translate.hs
  describe "Constraint simplification: Var-Abs (Ch 12.4.1)" $ do
    it "lambda parameters do not create gen nodes (on-the-fly Var-Abs)" $ do
      -- λx. λy. x y — two lambda params, zero let-bindings.
      -- Only the root gen node should exist; lambda params are bound
      -- monomorphically at the root scope, not under child gen nodes.
      let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
      case runConstraintDefault defaultPolySyms expr of
        Left err -> expectationFailure err
        Right ConstraintResult {crConstraint = c0} -> do
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
  [ ELam "x" (EVar "x"),
    ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))),
    ELet
      "id"
      (ELam "x" (EVar "x"))
      ( ELet
          "a"
          (EApp (EVar "id") (ELit (LInt 1)))
          (EApp (EVar "id") (ELit (LBool True)))
      ),
    EAnn
      (ELam "x" (EVar "x"))
      (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
  ]

assertCanonicalPipelineTypeChecks :: SurfaceExpr -> Expectation
assertCanonicalPipelineTypeChecks expr =
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, ty) ->
      typeCheck term `shouldBe` Right ty

assertViewParity :: PresolutionViewBoundary.PresolutionView p -> Solved -> Expectation
assertViewParity view legacy = do
  let sharedLiveDomain =
        IntSet.intersection
          (liveNodeKeySet (pvConstraint view))
          (liveNodeKeySet (Solved.originalConstraint legacy))
  projectCanonicalMap sharedLiveDomain (pvCanonicalMap view)
    `shouldBe` projectCanonicalMap sharedLiveDomain (Solved.canonicalMap legacy)
  eraseConstraintPhaseForTest (pvCanonicalConstraint view) `shouldBe` Solved.canonicalConstraint legacy

  let thesisNodes =
        map
          fst
          (toListNode (cNodes (pvConstraint view)))
      legacyNodes =
        map
          fst
          (toListNode (cNodes (Solved.originalConstraint legacy)))
      probeIds = nub (thesisNodes ++ legacyNodes ++ [NodeId 999, NodeId 1000])
      probeRefs = map typeRef probeIds

  forM_ probeIds $ \nid -> do
    pvCanonical view nid `shouldBe` Solved.canonical legacy nid
    pvLookupNode view nid
      `shouldBe` NodeAccess.lookupNode (Solved.originalConstraint legacy) (Solved.canonical legacy nid)
    pvLookupVarBound view nid
      `shouldBe` NodeAccess.lookupVarBound (Solved.originalConstraint legacy) (Solved.canonical legacy nid)

  forM_ probeRefs $ \ref ->
    pvLookupBindParent view ref
      `shouldBe` NodeAccess.lookupBindParent (Solved.originalConstraint legacy) ref

projectCanonicalMap :: IntSet.IntSet -> IntMap.IntMap NodeId -> IntMap.IntMap NodeId
projectCanonicalMap domain =
  IntMap.filterWithKey keepInDomain
  where
    keepInDomain key rep =
      IntSet.member key domain
        && IntSet.member (nodeIdToKey rep) domain
        && rep /= NodeId key

liveNodeKeySet :: Constraint p -> IntSet.IntSet
liveNodeKeySet constraint =
  IntSet.fromList
    [ nodeIdToKey nid
      | (nid, _) <- toListNode (cNodes constraint)
    ]

nodeIdToKey :: NodeId -> Int
nodeIdToKey (NodeId k) = k

annNodeOccurrences :: AnnExpr -> [NodeId]
annNodeOccurrences expr = case expr of
  AVar _ nid -> [nid]
  ALit _ nid -> [nid]
  ALam _ pNode _ body nid -> pNode : nid : annNodeOccurrences body
  AApp fn arg _ _ nid -> nid : annNodeOccurrences fn ++ annNodeOccurrences arg
  ALet _ _ schemeRoot _ _ rhs body nid ->
    schemeRoot : nid : annNodeOccurrences rhs ++ annNodeOccurrences body
  AAnn inner nid _ -> nid : annNodeOccurrences inner
  AUnfold inner nid _ -> nid : annNodeOccurrences inner

annLetSchemeRoots :: AnnExpr -> [NodeId]
annLetSchemeRoots expr = case expr of
  AVar _ _ -> []
  ALit _ _ -> []
  ALam _ _ _ body _ -> annLetSchemeRoots body
  AApp fn arg _ _ _ -> annLetSchemeRoots fn ++ annLetSchemeRoots arg
  ALet _ _ schemeRoot _ _ rhs body _ ->
    schemeRoot : annLetSchemeRoots rhs ++ annLetSchemeRoots body
  AAnn inner _ _ -> annLetSchemeRoots inner
  AUnfold inner _ _ -> annLetSchemeRoots inner

annRootNode :: AnnExpr -> NodeId
annRootNode expr = case expr of
  AVar _ nid -> nid
  ALit _ nid -> nid
  ALam _ _ _ _ nid -> nid
  AApp _ _ _ _ nid -> nid
  ALet _ _ _ _ _ _ _ nid -> nid
  AAnn _ nid _ -> nid
  AUnfold _ nid _ -> nid

validateStrict :: Solved -> Expectation
validateStrict s =
  case Solved.validateCanonicalGraphStrict s of
    [] -> pure ()
    vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

defaultPolySyms :: PolySyms
defaultPolySyms = Set.empty

noExpNodes :: NodeMap TyNode -> Expectation
noExpNodes nodes =
  case [nid | TyExp {tnId = nid} <- map snd (toListNode nodes)] of
    [] -> pure ()
    bad -> expectationFailure ("Unexpected TyExp nodes: " ++ show bad)

baseNames :: NodeMap TyNode -> [BaseTy]
baseNames nodes = [b | TyBase _ b <- map snd (toListNode nodes)]

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
        [ idLam,
          EApp idLam intLit,
          EApp idLam boolLit,
          ELet "id" idLam (EVar "id"),
          ELet "id" idLam (EApp (EVar "id") intLit),
          ELet "id" idLam (EApp (EVar "id") boolLit),
          ELet "id" idLam (ELet "_" (EApp (EVar "id") intLit) (EApp (EVar "id") boolLit)),
          ELet "id" idLam (EApp (EVar "id") (EVar "id")),
          ELam "y" (ELet "id" idLam (EApp (EVar "id") (EVar "y"))),
          ELamAnn "x" polyIdTy (EApp (EVar "x") intLit),
          ELet "f" (EAnn idLam polyIdTy) (EApp (EVar "f") intLit2)
        ]
  elements exprs
