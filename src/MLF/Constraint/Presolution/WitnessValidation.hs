{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessValidation
Description : Validation for normalized witnesses

This module provides validation functions for checking that normalized
instance operation witnesses satisfy the required invariants from the
MLF thesis (conditions 1-5).
-}
module MLF.Constraint.Presolution.WitnessValidation (
    OmegaNormalizeEnv(..),
    OmegaNormalizeError(..),
    validateNormalizedWitness,
    validateTerminalRootRaiseMerge,
    compareNodesByOrderKeyM
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types.Graph (BindFlag(..), Constraint(..), NodeId(..), NodeRef(..), TyNode(..), getNodeId, nodeRefFromKey, typeRef)
import MLF.Constraint.Types.Witness (InstanceOp(..), ReplayContract(..), isStrictReplayContract)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Util.Order (OrderKey, compareOrderKey)

data OmegaNormalizeEnv p = OmegaNormalizeEnv
    { oneRoot :: NodeId
    , interior :: IntSet.IntSet
    , interiorRaw :: IntSet.IntSet
    , weakened :: IntSet.IntSet
    , orderKeys :: IntMap.IntMap OrderKey
    , canonical :: NodeId -> NodeId
    , constraint :: Constraint p
    , binderArgs :: IntMap.IntMap NodeId
    , precomputedDescendants :: IntMap.IntMap IntSet.IntSet
    -- | Destination operands backed by an opaque construction-time
    -- `WeakenReplayCertificate`.  These are semantic non-root replay steps,
    -- even though applying Weaken has removed them from final I(r).
    , certifiedWeakens :: IntSet.IntSet
    -- | Destination presentations of source Raise operands backed by the
    -- edge's frozen all-flexible-path certificate.  Final chi_e topology may
    -- no longer contain that path, so validation must consume the captured
    -- construction fact rather than rediscover it.
    , certifiedRaises :: IntSet.IntSet
    -- | Exact operation-time copied binders retained by the same
    -- construction certificates.  These identities may have been eliminated
    -- from the finalized graph, but remain the producer-approved Φ replay
    -- domain.
    , certifiedReplayBinders :: IntSet.IntSet
    , binderReplayMap :: IntMap.IntMap NodeId
    , replayContract :: ReplayContract
    , replayDomainBinders :: [NodeId]
    , isAnnotationEdge :: Bool
    }

data OmegaNormalizeError
    = OpOutsideInterior InstanceOp
    | MergeDirectionInvalid NodeId NodeId
    | RaiseNotUnderRoot NodeId NodeId
    | RaiseMergeInsideInterior NodeId NodeId
    | GraftOnNonBottomBound NodeId NodeId
    | OpUnderRigid NodeId
    | DelayedWeakenViolation NodeId NodeId
    | MissingOrderKey NodeId
    | EqualOrderKeysDistinctNodes NodeId NodeId
    | RigidOperationInvalid InstanceOp NodeId
    | RigidOperandMismatch InstanceOp NodeId NodeId
    | NotTransitivelyFlexBound InstanceOp NodeId NodeId
    | MalformedRaiseMerge [InstanceOp]
    | AmbiguousGraftWeaken NodeId [NodeId]
    | DeterministicGraftWeakenSynthesisFailed NodeId [NodeId]
    | ReplayMapIncomplete [NodeId]
    | ReplayMapTargetOutsideReplayDomain NodeId NodeId
    | ReplayMapNonTyVarTarget NodeId NodeId
    | ReplayMapNonInjective NodeId NodeId NodeId
    | ReplayMapSourceNonFunctional NodeId [NodeId]
    | ReplayMapExpectedEmpty [NodeId]
    | ReplayContractNoneRequiresReplay InstanceOp
    | StandaloneGraftRemaining NodeId
    | AmbiguousOperatedSource NodeId [NodeId]
    | FinalOperandOutsideSourceReplayDomain NodeId
    | MultipleRootRaiseMergeTransitions NodeId [(NodeId, NodeId)]
    | RootRaiseMergeNotTerminal NodeId NodeId
    | RootRaiseMergeTraceAuthorityMissing NodeId NodeId
    | RootWeakenRaiseMergeTraceAuthorityMissing NodeId NodeId
    deriving (Eq, Show)

-- | Validate the construction-authoritative root 'OpRaiseMerge' after witness
-- operands have been restored to their exact frozen-source identities.
--
-- A root transition is identified only by exact equality with the trace root;
-- canonical aliases and ordinary interior 'OpRaiseMerge' operations are not
-- construction authority.  Figure 15.3.4 permits at most one such transition,
-- and it must close the witness.  The adjacent @Weaken(r); RaiseMerge(r,m)@
-- lane is a rigid identity computation, so it has its own trace predicate and
-- must not be mistaken for exterior Gamma authority by later consumers.
validateTerminalRootRaiseMerge
    :: NodeId
    -> (NodeId -> NodeId -> Bool)
    -> (NodeId -> NodeId -> Bool)
    -> [InstanceOp]
    -> Either OmegaNormalizeError ()
validateTerminalRootRaiseMerge traceRoot flexibleAuthority rigidAuthority ops =
    case rootTransitions of
        [] -> Right ()
        [(index, operated, exterior)]
            | lastOperation /= Just (OpRaiseMerge operated exterior) ->
                Left (RootRaiseMergeNotTerminal operated exterior)
            | immediatelyPrecededByRootWeaken index ->
                if rigidAuthority operated exterior
                    then Right ()
                    else Left (RootWeakenRaiseMergeTraceAuthorityMissing operated exterior)
            | flexibleAuthority operated exterior -> Right ()
            | otherwise ->
                Left (RootRaiseMergeTraceAuthorityMissing operated exterior)
        transitions ->
            Left
                ( MultipleRootRaiseMergeTransitions
                    traceRoot
                    [ (operated, exterior)
                    | (_index, operated, exterior) <- transitions
                    ]
                )
  where
    rootTransitions =
        [ (index, operated, exterior)
        | (index, OpRaiseMerge operated exterior) <- zip [0 :: Int ..] ops
        , operated == traceRoot
        ]
    lastOperation =
        case reverse ops of
            op : _ -> Just op
            [] -> Nothing
    immediatelyPrecededByRootWeaken index =
        case reverse (take index ops) of
            OpWeaken weakenedRoot : _ -> weakenedRoot == traceRoot
            _ -> False

compareNodesByOrderKeyM :: OmegaNormalizeEnv p -> NodeId -> NodeId -> Either OmegaNormalizeError Ordering
compareNodesByOrderKeyM env a b =
    case (IntMap.lookup (getNodeId (canon a)) (orderKeys env), IntMap.lookup (getNodeId (canon b)) (orderKeys env)) of
        (Just ka, Just kb) ->
            case compareOrderKey ka kb of
                EQ
                    | canon a == canon b -> Right EQ
                    | otherwise ->
                        Left
                            ( EqualOrderKeysDistinctNodes
                                (canon a)
                                (canon b)
                            )
                other -> Right other
        (Nothing, _) -> Left (MissingOrderKey (canon a))
        (_, Nothing) -> Left (MissingOrderKey (canon b))
  where
    canon = canonical env

validateNormalizedWitness :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError ()
validateNormalizedWitness env ops = do
    validateReplayMapContract
    mapM_ checkOp ops
    checkWeakenOrdering ops
  where
    rootC = canonical env (oneRoot env)

    canon = canonical env

    validateReplayMapContract = do
        let sourceDomain = IntSet.fromAscList (IntMap.keys (binderArgs env))
            replayMap = binderReplayMap env
            replayDomain = IntSet.fromAscList (IntMap.keys replayMap)
            missingSources = IntSet.toList (IntSet.difference sourceDomain replayDomain)
            strictContract = isStrictReplayContract (replayContract env)
        if strictContract
            then do
                if null missingSources
                    then Right ()
                    else Left (ReplayMapIncomplete (map NodeId missingSources))
                mapM_ checkReplayTargetStrict (IntMap.toList replayMap)
                case duplicateReplayTarget replayMap of
                    Nothing -> Right ()
                    Just (sourceA, sourceB, target) ->
                        Left (ReplayMapNonInjective sourceA sourceB target)
            else
                if IntMap.null replayMap
                    then Right ()
                    else Left (ReplayMapExpectedEmpty (map NodeId (IntMap.keys replayMap)))

    checkReplayTargetStrict (sourceKey, replayTargetRaw) =
        let inReplayDomain =
                IntSet.member (getNodeId replayTargetRaw) replayBinderDomain
        in if not inReplayDomain
            then Left (ReplayMapTargetOutsideReplayDomain (NodeId sourceKey) replayTargetRaw)
            else if isLiveTyVar replayTargetRaw || isCertifiedReplayBinder replayTargetRaw
                then Right ()
                else Left (ReplayMapNonTyVarTarget (NodeId sourceKey) replayTargetRaw)

    replayBindersForRoot
        | not (null (replayDomainBinders env)) =
            -- An explicit domain is a producer-owned identity domain, not a
            -- final-graph query.  Canonicalizing it here would erase a copied
            -- binder that the construction certificate deliberately retained.
            replayDomainBinders env
        | otherwise =
            let orderedUnder nid =
                    case Binding.orderedBinders canon (constraint env) (typeRef (canon nid)) of
                        Left _ -> []
                        Right binders -> map canon binders
                direct = orderedUnder rootC
            in case NodeAccess.lookupNode (constraint env) rootC of
                Just TyVar{ tnBound = Just bnd } ->
                    let viaBound = orderedUnder bnd
                    in if null direct then viaBound else direct
                Just TyMu{ tnBody = muBody } ->
                    let viaMu = orderedUnder muBody
                    in if null direct then viaMu else direct
                _ -> direct

    replayBinderDomain =
        IntSet.fromList
            [ getNodeId binder
            | binder <- replayBindersForRoot
            ]

    duplicateReplayTarget replayMap =
        let step (seen, dupFound) (sourceKey, replayTargetRaw)
                | Just _ <- dupFound = (seen, dupFound)
                | otherwise =
                    let replayKey = getNodeId replayTargetRaw
                    in case IntMap.lookup replayKey seen of
                        Nothing ->
                            (IntMap.insert replayKey (NodeId sourceKey) seen, Nothing)
                        Just sourceA ->
                            ( seen
                            , Just (sourceA, NodeId sourceKey, NodeId replayKey)
                            )
            (_, dup) = foldl' step (IntMap.empty, Nothing) (IntMap.toList replayMap)
        in dup

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    isCertifiedWeaken nid =
        IntSet.member (getNodeId (canon nid)) (certifiedWeakens env)

    isCertifiedReplayBinder nid =
        IntSet.member (getNodeId nid) (certifiedReplayBinders env)

    isCertifiedRaise nid =
        IntSet.member (getNodeId (canon nid)) (certifiedRaises env)

    weakenedByWitness =
        IntSet.fromList
            [ getNodeId (canon nid)
            | OpWeaken nid <- ops
            ]

    isRigid nid =
        case Binding.lookupBindParent (constraint env) (typeRef (canon nid)) of
            -- Validation sees the finalized binding tree, after pending
            -- Weaken operations have changed flexible edges to rigid ones.  A
            -- node weakened by this very witness was not rigid when preceding
            -- Raise/Merge operations ran, so the final flag must not exempt
            -- those operations from the transitive-flex checks.
            Just (_, BindRigid) ->
                let key = getNodeId (canon nid)
                 in IntSet.notMember key weakenedByWitness
            _ -> False

    requireInterior op nid =
        if inInterior nid
            then Right ()
            else Left (OpOutsideInterior op)

    requireTransitivelyFlexBoundToRoot op nid = go IntSet.empty (canon nid)
      where
        targetC = canon nid
        failNotFlex = Left (NotTransitivelyFlexBound op targetC rootC)

        go seen cur
            | cur == rootC = Right ()
            | IntSet.member (getNodeId cur) seen = failNotFlex
            | otherwise =
                let seen' = IntSet.insert (getNodeId cur) seen
                in case Binding.lookupBindParent (constraint env) (typeRef cur) of
                    Just (TypeRef parent, BindFlex) -> go seen' (canon parent)
                    _ -> failNotFlex

    mergeKeyNode nid =
        case IntMap.lookup (getNodeId (canon nid)) (binderArgs env) of
            Just arg ->
                let argC = canon arg
                in if IntMap.member (getNodeId argC) (orderKeys env)
                    then argC
                    else canon nid
            Nothing -> canon nid

    checkMergeDirection n m = do
        ord <- compareNodesByOrderKeyM env (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))

    isBottomNode nid =
        case NodeAccess.lookupNode (constraint env) (canon nid) of
            Just TyBottom{} -> True
            _ -> False

    isLiveTyVar nid =
        case NodeAccess.lookupNode (constraint env) (canon nid) of
            Just TyVar{} -> True
            _ -> False

    requireGraftTarget n =
        let nC = canon n
            trackedByExpansion = IntMap.member (getNodeId nC) (binderArgs env)
        in if nC == rootC
            then Right ()
            else if trackedByExpansion
                then Right ()
                else case NodeAccess.lookupNode (constraint env) nC of
                    Just TyVar{ tnBound = Just bnd }
                        | not (isBottomNode bnd) -> Left (GraftOnNonBottomBound nC (canon bnd))
                    Just TyMu{} -> Right ()
                    _ -> Right ()

    checkOp op =
        do
            case op of
                OpGraft _ n ->
                    requireInterior op n >> requireGraftTarget n
                OpWeaken n ->
                    if isCertifiedWeaken n
                        then Right ()
                        else requireInterior op n
                OpMerge n m -> do
                    if isRigid n
                        then Right ()
                        else if isRigid m
                            then Left (RigidOperandMismatch op (canon n) (canon m))
                            else do
                                requireInterior op n
                                requireInterior op m
                                checkMergeDirection n m
                                requireTransitivelyFlexBoundToRoot op n
                                requireTransitivelyFlexBoundToRoot op m
                OpRaise n ->
                    if isRigid n
                        then Right ()
                    else if not (inInterior n)
                        then Left (RaiseNotUnderRoot (canon n) rootC)
                        else if isCertifiedRaise n
                            then Right ()
                        else requireTransitivelyFlexBoundToRoot op n
                OpRaiseMerge n m -> do
                    if isRigid n
                        then Right ()
                    else if isRigid m
                        then Left (RigidOperandMismatch op (canon n) (canon m))
                    else if not (inInterior n)
                        then Left (OpOutsideInterior op)
                        else if inInterior m
                            then Left (RaiseMergeInsideInterior (canon n) (canon m))
                            else Right ()

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpRaiseMerge n m -> [n, m]

    -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4, condition (5)):
    -- "below n" means
    -- strict binding-tree descendants (exclude n itself).
    descendantsOf nid =
        case IntMap.lookup (getNodeId (canon nid)) (precomputedDescendants env) of
            Just descendants -> Right descendants
            Nothing ->
                case Binding.interiorOf (constraint env) (typeRef (canon nid)) of
                    Left _ -> Left (OpUnderRigid (canon nid))
                    Right s ->
                        let typeInterior =
                                IntSet.fromList
                                    [ getNodeId t
                                    | key <- IntSet.toList s
                                    , TypeRef t <- [nodeRefFromKey key]
                                    ]
                        in Right (IntSet.delete (getNodeId (canon nid)) typeInterior)

    firstOffender descSet rest =
        listToMaybe
            [ canon t
            | op <- rest
            , t <- opTargets op
            , IntSet.member (getNodeId (canon t)) descSet
            ]

    isRigidRootRaiseMergeTransition weakenedNode operated =
        canon weakenedNode == rootC
            && canon operated == rootC
            && isStrictReplayContract (replayContract env)
            && case
                ( IntMap.toList (binderArgs env)
                , IntMap.toList (binderReplayMap env)
                , replayDomainBinders env
                )
              of
                ( [(sourceKey, _argument)]
                  , [(replaySourceKey, replayTarget)]
                  , [domainRoot]
                  ) ->
                    sourceKey == getNodeId rootC
                        && replaySourceKey == sourceKey
                        && replayTarget == domainRoot
                _ -> False

    weakenConditionRest weakenedNode rest =
        case rest of
            OpRaiseMerge operated _ : remaining
                | isRigidRootRaiseMergeTransition weakenedNode operated ->
                    -- Figure 15.3.4's strict root lane is one transition:
                    -- Weaken(r); RaiseMerge(r,m).  The latter is identity
                    -- because the former has just made r rigid.  Its
                    -- destination representative may lie below r after UF,
                    -- so exclude only this paired step from condition (5).
                    remaining
            _ -> rest

    checkWeakenOrdering [] = Right ()
    checkWeakenOrdering (op : rest) =
        case op of
            OpWeaken n -> do
                desc <- descendantsOf n
                case firstOffender desc (weakenConditionRest n rest) of
                    Nothing -> checkWeakenOrdering rest
                    Just offender -> Left (DelayedWeakenViolation (canon n) offender)
            _ -> checkWeakenOrdering rest
