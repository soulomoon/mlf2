{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.Pipeline (
    runPipelineElab,
    runPipelineElabChecked
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    , computePresolution
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import MLF.Constraint.Types
    ( BoundRef(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    , InstEdge(..)
    , NodeId(..)
    , NodeRef(..)
    , PolySyms
    , TyNode(..)
    , cBindParents
    , cInstEdges
    , cNodes
    , getEdgeId
    , getNodeId
    , instLeft
    , instRight
    , nodeRefFromKey
    , typeRef
    )
import MLF.Elab.Elaborate (elaborateWithScope)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Elab.Run.Annotation (applyRedirectsToAnn, canonicalizeAnn)
import MLF.Elab.Run.Debug (debugWhenM, edgeOrigins)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    , pruneBindParentsConstraint
    )
import MLF.Elab.Run.Scope (letScopeOverrides)
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Elab.Run.ResultType (ResultTypeContext(..), computeResultTypeFromAnn, computeResultTypeFallback)

runPipelineElab :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElab polySyms = runPipelineElabWith (generateConstraints polySyms)

runPipelineElabChecked :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElabChecked polySyms expr = do
    (term, _ty) <- runPipelineElab polySyms expr
    tyChecked <- firstShow (typeCheck term)
    pure (term, tyChecked)

runPipelineElabWith
    :: (Expr -> Either ConstraintError ConstraintResult)
    -> Expr
    -> Either String (ElabTerm, ElabType)
runPipelineElabWith genConstraints expr = do
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- firstShow (genConstraints expr)
    let c1 = normalize c0
    debugWhenM
        (let targetEid = 3
             origins = edgeOrigins ann
             edgeDesc =
                 case IntMap.lookup targetEid origins of
                     Nothing -> "none"
                     Just msg -> msg
             instEdge =
                 listToMaybe
                     [ (instLeft e, instRight e)
                     | e <- cInstEdges c1
                     , getEdgeId (instEdgeId e) == targetEid
                     ]
         in "edge origin: eid="
                ++ show targetEid
                ++ " origin="
                ++ edgeDesc
                ++ " instEdge="
                ++ show instEdge
        )
    acyc <- firstShow (checkAcyclicity c1)
    pres <- firstShow (computePresolution acyc c1)
    let planBuilder = prPlanBuilder pres
        generalizeAtWith = generalizeAtWithBuilder planBuilder
    solved <- firstShow (solveUnify (prConstraint pres))
    let solvedClean = solved { srConstraint = pruneBindParentsConstraint (srConstraint solved) }
    case validateSolvedGraphStrict solvedClean of
        [] -> do
            let canonicalSolved = frWith (srUnionFind solvedClean)
                adoptNode = canonicalSolved . chaseRedirects (prRedirects pres)
                instCopyNodes =
                    instantiationCopyNodes solvedClean (prRedirects pres) (prEdgeTraces pres)
                instCopyMapFull =
                    let baseNodes = cNodes c1
                        baseBindParents = cBindParents c1
                        baseNamedKeysAll =
                            IntSet.fromList
                                [ childKey
                                | (childKey, (parentRef, _flag)) <- IntMap.toList baseBindParents
                                , case parentRef of
                                    GenRef _ -> True
                                    _ -> False
                                , TypeRef child <- [nodeRefFromKey childKey]
                                , case IntMap.lookup (getNodeId child) baseNodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        traceMaps =
                            [ let copyMap0 = etCopyMap tr
                                  rootBase = etRoot tr
                                  baseInteriorSet =
                                      case Binding.interiorOf c1 (typeRef rootBase) of
                                          Right s ->
                                              IntSet.insert
                                                  (getNodeId rootBase)
                                                  (IntSet.fromList
                                                      [ getNodeId nid
                                                      | key <- IntSet.toList s
                                                      , TypeRef nid <- [nodeRefFromKey key]
                                                      ]
                                                  )
                                          Left _ -> IntSet.singleton (getNodeId rootBase)
                                  binderCopyOverrides =
                                      IntMap.fromList
                                          [ (getNodeId (adoptNode copyN), NodeId baseKey)
                                          | (baseKey, copyN) <- IntMap.toList copyMap0
                                          , IntSet.member baseKey baseNamedKeysAll
                                          ]
                                  binderMetaOverrides =
                                      IntMap.fromList
                                          [ (getNodeId (adoptNode meta), binder)
                                          | (binder, _arg) <- etBinderArgs tr
                                          , Just meta <- [IntMap.lookup (getNodeId binder) copyMap0]
                                          ]
                                  invMap =
                                      IntMap.fromListWith
                                          (\_ old -> old)
                                          [ (getNodeId (adoptNode copyN), NodeId baseKey)
                                          | (baseKey, copyN) <- IntMap.toList copyMap0
                                          ]
                                  ensureRoot acc =
                                      let rootCopyKey = getNodeId (adoptNode rootBase)
                                      in IntMap.insertWith (\_ old -> old) rootCopyKey rootBase acc
                                  addInterior acc nidInt =
                                      let baseN = NodeId nidInt
                                          copyKey = getNodeId (adoptNode baseN)
                                      in if IntMap.member copyKey acc
                                          then acc
                                          else if IntMap.member nidInt baseNodes
                                              && IntSet.member nidInt baseInteriorSet
                                              then IntMap.insert copyKey baseN acc
                                          else IntMap.insert copyKey rootBase acc
                              in foldl'
                                    addInterior
                                    (ensureRoot (IntMap.union binderMetaOverrides (IntMap.union binderCopyOverrides invMap)))
                                    (IntSet.toList (etInterior tr))
                            | tr <- IntMap.elems (prEdgeTraces pres)
                            ]
                    in foldl' IntMap.union IntMap.empty traceMaps
                (constraintForGen, bindParentsGa) =
                    constraintForGeneralization solvedClean (prRedirects pres) instCopyNodes instCopyMapFull c1 ann
            let solvedForGen = solvedClean { srConstraint = constraintForGen }
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            let canonNode = canonicalSolved . chaseRedirects (prRedirects pres)
                canonOp op = case op of
                    OpGraft a b -> OpGraft (canonNode a) (canonNode b)
                    OpMerge a b -> OpMerge (canonNode a) (canonNode b)
                    OpRaise n -> OpRaise (canonNode n)
                    OpWeaken n -> OpWeaken (canonNode n)
                    OpRaiseMerge a b -> OpRaiseMerge (canonNode a) (canonNode b)
                canonStep step = case step of
                    StepOmega op -> StepOmega (canonOp op)
                    StepIntro -> StepIntro
                canonWitness w =
                    let InstanceWitness ops = ewWitness w
                    in w
                        { ewLeft = canonNode (ewLeft w)
                        , ewRight = canonNode (ewRight w)
                        , ewRoot = canonNode (ewRoot w)
                        , ewSteps = map canonStep (ewSteps w)
                        , ewWitness = InstanceWitness (map canonOp ops)
                        }
                canonTrace tr =
                    let canonPair (a, b) = (canonNode a, canonNode b)
                        canonInterior =
                            IntSet.fromList
                                [ getNodeId (canonNode (NodeId i))
                                | i <- IntSet.toList (etInterior tr)
                                ]
                        canonCopyMap =
                            IntMap.fromListWith min
                                [ ( getNodeId (canonNode (NodeId k))
                                  , canonNode v
                                  )
                                | (k, v) <- IntMap.toList (etCopyMap tr)
                                ]
                    in tr
                        { etRoot = canonNode (etRoot tr)
                        , etBinderArgs = map canonPair (etBinderArgs tr)
                        , etInterior = canonInterior
                        , etCopyMap = canonCopyMap
                        }
                canonExpansion expn = case expn of
                    ExpIdentity -> ExpIdentity
                    ExpForall specs ->
                        let canonBound bnd = case bnd of
                                BoundNode nid -> BoundNode (canonNode nid)
                                BoundBinder ix -> BoundBinder ix
                            canonSpec spec =
                                spec
                                    { fsBounds =
                                        map
                                            (fmap canonBound)
                                            (fsBounds spec)
                                    }
                        in ExpForall (NE.map canonSpec specs)
                    ExpInstantiate args -> ExpInstantiate (map canonNode args)
                    ExpCompose es -> ExpCompose (NE.map canonExpansion es)
            let annCanon = canonicalizeAnn canonNode ann'
            let edgeWitnesses = IntMap.map canonWitness (prEdgeWitnesses pres)
                edgeTraces = IntMap.map canonTrace (prEdgeTraces pres)
                edgeExpansions = IntMap.map canonExpansion (prEdgeExpansions pres)
            let scopeOverrides = letScopeOverrides c1 (srConstraint solvedForGen) solvedClean (prRedirects pres) annCanon
            term <- firstShow (elaborateWithScope generalizeAtWith solvedClean solvedClean solvedForGen bindParentsGa edgeWitnesses edgeTraces edgeExpansions scopeOverrides annCanon)

            -- Build context for result type computation
            let canonical = frWith (srUnionFind solvedClean)
                resultTypeCtx = ResultTypeContext
                    { rtcCanonical = canonical
                    , rtcEdgeWitnesses = edgeWitnesses
                    , rtcEdgeTraces = edgeTraces
                    , rtcEdgeExpansions = edgeExpansions
                    , rtcSolvedForGen = solvedForGen
                    , rtcSolvedClean = solvedClean
                    , rtcBindParentsGa = bindParentsGa
                    , rtcPlanBuilder = planBuilder
                    , rtcBaseConstraint = c1
                    , rtcRedirects = prRedirects pres
                    }

            -- Compute result type
            case annCanon of
                AAnn inner annNodeId eid -> do
                    ty <- computeResultTypeFromAnn resultTypeCtx inner inner annNodeId eid
                    pure (term, ty)
                _ -> do
                    ty <- computeResultTypeFallback resultTypeCtx annCanon ann
                    pure (term, ty)
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
