{-# LANGUAGE DataKinds #-}
module Research.C1AuthoritativeSurfaceSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import qualified MLF.Constraint.Finalize as Finalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Presolution.View as PresolutionView
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents(..)
    )
import MLF.Constraint.Presolution.TestSupport
    ( EdgeArtifacts(..)
    , defaultPlanBuilder
    )
import MLF.Constraint.Types.Graph
    ( BaseTy(..)
    , Constraint
    , NodeId(..)
    , TyNode(..)
    , cBindParents
    , cNodes
    , fromListNode
    , getNodeId
    , toListNode
    )
import MLF.Elab.Pipeline
    ( applyRedirectsToAnn
    , canonicalizeAnn
    , runPipelineElab
    , runPipelineElabChecked
    )
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs(..)
    , computeResultTypeFallback
    , mkResultTypeInputs
    )
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.Syntax
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , Ty(..)
    )
import SpecUtil
    ( PipelineArtifacts(..)
    , defaultTraceConfig
    , requireRight
    , runPipelineArtifactsDefault
    , unsafeNormalizeExpr
    )
import MLF.Constraint.Types.Phase (Phase(Raw))

spec :: Spec
spec =
    describe "C1 authoritative-surface harness" $ do
        it "keeps the admitted non-local Int packet visibly non-recursive on the fallback surface" $ do
            fallbackTy <- c1FallbackTypeFor c1IntExpr (BaseTy "Int")
            fallbackTy `shouldBe` TBase (BaseTy "Int")
            containsMu fallbackTy `shouldBe` False

        it "keeps the exact Int source packet recursive on both current pipeline entrypoints" $ do
            let blocked = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
            (_uncheckedTerm, uncheckedTy) <-
                requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr c1IntExpr))
            (_checkedTerm, checkedTy) <-
                requireRight (runPipelineElabChecked Set.empty (unsafeNormalizeExpr c1IntExpr))
            uncheckedTy `shouldNotBe` blocked
            checkedTy `shouldNotBe` blocked
            containsMu uncheckedTy `shouldBe` True
            containsMu checkedTy `shouldBe` True

        it "keeps the admitted non-local Bool packet visibly non-recursive on the fallback surface" $ do
            fallbackTy <- c1FallbackTypeFor c1BoolExpr (BaseTy "Bool")
            fallbackTy `shouldBe` TBase (BaseTy "Bool")
            containsMu fallbackTy `shouldBe` False

        it "keeps the exact Bool source packet recursive on both current pipeline entrypoints" $ do
            let blocked = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
            (_uncheckedTerm, uncheckedTy) <-
                requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr c1BoolExpr))
            (_checkedTerm, checkedTy) <-
                requireRight (runPipelineElabChecked Set.empty (unsafeNormalizeExpr c1BoolExpr))
            uncheckedTy `shouldNotBe` blocked
            checkedTy `shouldNotBe` blocked
            containsMu uncheckedTy `shouldBe` True
            containsMu checkedTy `shouldBe` True

c1IntExpr :: SurfaceExpr
c1IntExpr = ELet "k" (ELamAnn "x" recursiveIntAnn (EVar "x")) (EVar "k")

c1BoolExpr :: SurfaceExpr
c1BoolExpr = ELet "k" (ELamAnn "x" recursiveBoolAnn (EVar "x")) (EVar "k")

recursiveIntAnn :: SrcType
recursiveIntAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))

recursiveBoolAnn :: SrcType
recursiveBoolAnn = STMu "a" (STArrow (STVar "a") (STBase "Bool"))

c1FallbackTypeFor :: SurfaceExpr -> BaseTy -> IO ElabType
c1FallbackTypeFor expr expectedBase = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        bodyCanon = extractVarBody annCanon0
        bodyPre = extractVarBody annPre0
        rootNid = rtcCanonical inputs0 (bodyRoot annCanon0)
        inputs =
            rebindRootTo inputs0 rootNid (findBaseNode expectedBase (rtcPresolutionView inputs0))
    requireRight (computeResultTypeFallback inputs bodyCanon bodyPre)

extractVarBody :: AnnExpr -> AnnExpr
extractVarBody ann0 = case ann0 of
    ALet _ _ _ _ _ _ (AAnn body _ _) _ -> body
    _ -> error ("unexpected scheme-alias/base-like wrapper shape: " ++ show ann0)

bodyRoot :: AnnExpr -> NodeId
bodyRoot ann0 = case extractVarBody ann0 of
    AVar _ nid -> nid
    other -> error ("expected scheme alias variable body, got " ++ show other)

rebindRootTo :: ResultTypeInputs -> NodeId -> NodeId -> ResultTypeInputs
rebindRootTo inputs rootNid newBound = rewriteResultTypeInputs (setVarBound rootNid newBound) inputs

rewriteResultTypeInputs :: (Constraint 'Raw -> Constraint 'Raw) -> ResultTypeInputs -> ResultTypeInputs
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

setVarBound :: NodeId -> NodeId -> Constraint 'Raw -> Constraint 'Raw
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

findBaseNode :: BaseTy -> PresolutionView 'Raw -> NodeId
findBaseNode expectedBase view0 =
    case
        [ tnId node
        | (_nodeIdKey, node@TyBase{ tnBase = baseTy }) <-
            toListNode (cNodes (pvConstraint view0))
        , baseTy == expectedBase
        ]
    of
        baseNid : _ -> baseNid
        [] -> error ("expected base node for " ++ show expectedBase ++ " in C1 fallback case")

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
                (\acc (baseKey, solvedNid) ->
                    IntMap.insertWith (\_ existing -> existing) (getNodeId solvedNid) (NodeId baseKey) acc
                )
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

containsMu :: ElabType -> Bool
containsMu ty = case ty of
    TMu _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TCon _ args -> any containsMu args
    TForall _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound :: BoundType -> Bool
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBase _ -> False
        TCon _ args -> any containsMu args
        TForall _ mb body -> maybe False containsMuBound mb || containsMu body
        TMu _ _ -> True
        TBottom -> False
