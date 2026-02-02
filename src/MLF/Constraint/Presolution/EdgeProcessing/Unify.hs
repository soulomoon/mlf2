{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Unify
Description : Edge-local unification helpers for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

Helpers for applying expansions and executing edge-local unification.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Unify (
    EdgeExpansionResult(..),
    runExpansionUnify,
    setBindParentIfUpper
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Witness.OmegaExec as OmegaExec
import MLF.Util.Trace (traceBindingM)

import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (
    CopyMap,
    FrontierSet,
    InteriorSet,
    PresolutionError(..),
    PresolutionM,
    getConstraint,
    edgeInteriorExact,
    getCopyMapping,
    lookupCopy,
    bindExpansionArgs
    )
import MLF.Constraint.Presolution.Ops (
    findRoot,
    setBindParentM
    )
import MLF.Constraint.Presolution.StateAccess (getCanonical)
import MLF.Constraint.Presolution.Expansion (
    applyExpansionEdgeTraced,
    bindExpansionRootLikeTarget
    )
import MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyState(eusOps),
    initEdgeUnifyState,
    mkOmegaExecEnv,
    unifyAcyclicEdge,
    unifyStructureEdge
    )
import MLF.Constraint.Presolution.Witness (binderArgsFromExpansion)
import qualified MLF.Constraint.VarStore as VarStore

-- | Result of applying an expansion and running edge-local unification.
data EdgeExpansionResult = EdgeExpansionResult
    { eerTrace :: (CopyMap, InteriorSet, FrontierSet)
    , eerExtraOps :: [InstanceOp]
    }

-- | Apply an expansion and run edge-local unification for a single edge.
runExpansionUnify
    :: EdgeId
    -> TyNode
    -> TyNode
    -> Expansion
    -> [InstanceOp]
    -> PresolutionM EdgeExpansionResult
runExpansionUnify edgeId leftRaw target expn baseOps =
    case leftRaw of
        TyExp{ tnBody = _bodyId } -> do
            (resNodeId, (copyMap0, interior0, frontier0)) <- applyExpansionEdgeTraced expn leftRaw
            debugBindParents
                ( "processInstEdge: expansion result resNodeId="
                    ++ show resNodeId
                    ++ " copyMap0="
                    ++ show copyMap0
                    ++ " frontier0="
                    ++ show frontier0
                )

            let targetNodeId = tnId target
            cBeforeBind <- getConstraint
            let targetParent = Binding.lookupBindParent cBeforeBind (typeRef targetNodeId)
            debugBindParents
                ( "processInstEdge: expansion root bind target="
                    ++ show targetNodeId
                    ++ " parent="
                    ++ show targetParent
                )
            targetBinder <- bindExpansionRootLikeTarget resNodeId targetNodeId

            canonical <- getCanonical
            let copyMapCanon =
                    IntMap.fromListWith
                        const
                        [ (getNodeId (canonical (NodeId orig)), copy)
                        | (orig, copy) <- IntMap.toList (getCopyMapping copyMap0)
                        ]
            forM_ (IntSet.toList frontier0) $ \nidInt ->
                case IntMap.lookup nidInt copyMapCanon of
                    Nothing -> pure ()
                    Just copy -> setBindParentIfUpper copy targetBinder

            bas <- binderArgsFromExpansion leftRaw expn
            binderMetas <- forM bas $ \(bv, _arg) ->
                case lookupCopy bv copyMap0 of
                    Just meta -> pure (bv, meta)
                    Nothing ->
                        throwError (InternalError ("runExpansionUnify: missing binder-meta copy for " ++ show bv))

            canonInterior <- getCanonical
            let canonInteriorSet =
                    IntSet.fromList
                        [ getNodeId (canonInterior (NodeId i))
                        | i <- IntSet.toList interior0
                        ]
            interiorExact <- edgeInteriorExact resNodeId
            let interior = IntSet.union canonInteriorSet interiorExact

            cForBounds <- getConstraint
            let binderBounds =
                    IntMap.fromList
                        [ (getNodeId b, bnd)
                        | (b, _arg) <- bas
                        , Just bnd <- [VarStore.lookupVarBound cForBounds b]
                        ]

            eu0 <- initEdgeUnifyState binderMetas binderBounds interior resNodeId
            let omegaEnv = mkOmegaExecEnv copyMap0
            (_a, eu1) <- runStateT
                (do
                    OmegaExec.executeOmegaBaseOpsPre omegaEnv baseOps
                    bindExpansionArgs resNodeId bas
                    forM_ (IntSet.toList frontier0) $ \nidInt ->
                        case IntMap.lookup nidInt copyMapCanon of
                            Nothing -> pure ()
                            Just copy -> unifyStructureEdge copy (NodeId nidInt)
                    unifyStructureEdge resNodeId (tnId target)
                    unifyAcyclicEdge (tnId leftRaw) resNodeId
                    OmegaExec.executeOmegaBaseOpsPost omegaEnv baseOps
                )
                eu0

            resRoot <- findRoot resNodeId
            setBindParentIfUpper resRoot targetBinder
            setBindParentIfUpper (tnId leftRaw) targetBinder

            cAfterBind <- getConstraint
            let resParent = Binding.lookupBindParent cAfterBind (typeRef resRoot)
            debugBindParents
                ( "processInstEdge: expansion root bound resRoot="
                    ++ show resRoot
                    ++ " parent="
                    ++ show resParent
                    ++ " targetBinder="
                    ++ show targetBinder
                )

            c1 <- getConstraint
            case Binding.lookupBindParent c1 (typeRef resRoot) of
                Nothing -> setBindParentIfUpper resRoot targetBinder
                Just _ -> pure ()

            pure EdgeExpansionResult
                { eerTrace = (copyMap0, interior, frontier0)
                , eerExtraOps = eusOps eu1
                }
        _ ->
            throwError (InternalError ("runExpansionUnify: expected TyExp for edge " ++ show edgeId))

-- | Set a binding parent if the parent is upper than the child in the binding tree.
setBindParentIfUpper :: NodeId -> NodeRef -> PresolutionM ()
setBindParentIfUpper child parent = do
    cBind <- getConstraint
    when (Binding.isUpper cBind parent (TypeRef child)) $
        setBindParentM (TypeRef child) (parent, BindFlex)

-- | Debug binding operations (uses explicit trace config).
debugBindParents :: String -> PresolutionM ()
debugBindParents msg = do
    cfg <- ask
    traceBindingM cfg msg
