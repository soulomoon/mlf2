{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module MLF.Elab.Elaborate.Annotation (
    AnnotationContext(..),
    closeTermForAnnotation,
    stripUnusedTopTyAbs,
    sourceAnnIsPolymorphic,
    sourceAnnSchemeInfo,
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    reifyInst,
    instSeqApps
) where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)

import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Types
    ( EdgeId(..)
    , EdgeWitness(..)
    , Expansion(..)
    , NodeId(..)
    )
import MLF.Elab.Elaborate.Scope
    ( ScopeContext(..)
    , generalizeAtNode
    , reifyTargetNodeType
    , reifyTargetType
    )
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Legacy (expInstantiateArgsToInstNoFallback)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Run.Annotation (adjustAnnotationInst)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType)
import MLF.Elab.TermClosure
    ( alignTermTypeVarsToScheme
    , alignTermTypeVarsToTopTyAbs
    , closeTermWithSchemeSubstIfNeeded
    )
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
    ( ElabError(..)
    , ElabTerm(..)
    , ElabType
    , Instantiation(..)
    , SchemeInfo(..)
    , Ty(..)
    , pattern Forall
    , tyToElab
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..))
import MLF.Frontend.Syntax (VarName)
import MLF.Reify.Core (reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, parseNameId)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data AnnotationContext = AnnotationContext
    { acTraceConfig :: TraceConfig
    , acScopeContext :: ScopeContext
    , acEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , acEdgeTraces :: IntMap.IntMap EdgeTrace
    , acEdgeExpansions :: IntMap.IntMap Expansion
    }

closeTermForAnnotation :: ElabTerm -> ElabTerm
closeTermForAnnotation term =
    case typeCheck term of
        Right ty ->
            let freeVars = Set.toList (freeTypeVarsType ty)
                scheme = Forall [(v, Nothing) | v <- freeVars] ty
            in closeTermWithSchemeSubstIfNeeded IntMap.empty scheme term
        Left _ -> term

stripUnusedTopTyAbs :: ElabTerm -> ElabTerm
stripUnusedTopTyAbs term =
    case term of
        ETyAbs v mbBound body ->
            let body' = stripUnusedTopTyAbs body
                term' = ETyAbs v mbBound body'
            in case typeCheck term' of
                Right (TForall _ _ bodyTy)
                    | v `notElem` freeTypeVarsType bodyTy -> body'
                _ -> term'
        _ -> term

sourceAnnIsPolymorphic :: Map.Map VarName SchemeInfo -> AnnExpr -> Bool
sourceAnnIsPolymorphic env sourceAnn =
    case sourceAnn of
        AVar v _ ->
            case Map.lookup v env of
                Just SchemeInfo{ siScheme = Forall binds _ } -> not (null binds)
                _ -> False
        AAnn inner _ _ -> sourceAnnIsPolymorphic env inner
        _ -> False

sourceAnnSchemeInfo :: Map.Map VarName SchemeInfo -> AnnExpr -> Maybe SchemeInfo
sourceAnnSchemeInfo env sourceAnn =
    case sourceAnn of
        AVar v _ -> Map.lookup v env
        AAnn inner _ _ -> sourceAnnSchemeInfo env inner
        _ -> Nothing

desugaredAnnLambdaInfo :: VarName -> AnnExpr -> Maybe (NodeId, EdgeId, AnnExpr)
desugaredAnnLambdaInfo param bodyAnn =
    case bodyAnn of
        ALet letName _ _ _ _ rhsAnn innerBodyAnn _
            | letName == param ->
                case rhsAnn of
                    AAnn rhsInner annNodeId eid | annRefersToVar param rhsInner ->
                        Just (annNodeId, eid, innerBodyAnn)
                    _ -> Nothing
        _ -> Nothing

elaborateAnnotationTerm
    :: AnnotationContext
    -> IntSet.IntSet
    -> Map.Map VarName SchemeInfo
    -> AnnExpr
    -> NodeId
    -> EdgeId
    -> ElabTerm
    -> Either ElabError ElabTerm
elaborateAnnotationTerm annotationContext namedSetReify env exprAnn annNodeId eid expr' = do
    expectedSchemeInfo <-
        case generalizeAtNode scopeContext annNodeId of
            Right pair -> pure (Just pair)
            Left _ -> pure Nothing
    let sourceSchemeInfo = sourceAnnSchemeInfo env exprAnn
        canReuseSourceScheme =
            case (sourceSchemeInfo, expectedSchemeInfo) of
                (Just schemeInfo, Just (schemeExpected, _substExpected)) ->
                    alphaEqType (schemeToType (siScheme schemeInfo)) (schemeToType schemeExpected)
                _ -> False
        requiresExplicitAnnotationInst =
            case (sourceSchemeInfo, expectedSchemeInfo) of
                (Just SchemeInfo{ siScheme = srcScheme }, Just (schemeExpected, _substExpected)) ->
                    let sourcePoly = case srcScheme of
                            Forall binds _ -> not (null binds)
                    in sourcePoly
                        && not (alphaEqType (schemeToType srcScheme) (schemeToType schemeExpected))
                _ -> False
    inst <-
        case reifyInst annotationContext namedSetReify env exprAnn eid of
            Right inst0 -> pure inst0
            Left (PhiTranslatabilityError _)
                | canReuseSourceScheme -> pure InstId
            Left err -> Left err
    let expectedSchemeResult = fmap fst expectedSchemeInfo
        mExpectedBound =
            case expectedSchemeResult of
                Just (Forall ((_, Just bnd) : _) _) -> Just (tyToElab bnd)
                _ -> Nothing
        dropAnnotationElims inst0 = case inst0 of
            InstElim -> InstId
            InstSeq a b ->
                let a' = dropAnnotationElims a
                    b' = dropAnnotationElims b
                in case (a', b') of
                    (InstId, x) -> x
                    (x, InstId) -> x
                    _ -> InstSeq a' b'
            InstInside a -> InstInside (dropAnnotationElims a)
            InstUnder v a -> InstUnder v (dropAnnotationElims a)
            _ -> inst0
        preservesForalls =
            isJust mExpectedBound
                || isJust (alignTermTypeVarsToTopTyAbs expr')
                || case expr' of
                    ETyAbs{} -> True
                    _ -> False
        instAdjusted0 =
            if preservesForalls
                then adjustAnnotationInst inst
                else dropAnnotationElims inst
        instAdjusted =
            case (mExpectedBound, instAdjusted0) of
                (Just expectedBound, InstInside (InstBot _)) ->
                    InstInside (InstBot expectedBound)
                _ -> instAdjusted0
    exprClosed0 <-
        if instAdjusted == InstId && requiresExplicitAnnotationInst
            then
                Left
                    (PhiTranslatabilityError
                        [ "AAnnF: missing authoritative instantiation for annotation edge " ++ show eid
                        ]
                    )
            else if instAdjusted == InstId
                then
                    if canReuseSourceScheme && sourceAnnIsPolymorphic env exprAnn
                        then pure expr'
                    else case expectedSchemeInfo of
                        Just (schemeExpected, substExpected) ->
                            case expr' of
                                ETyAbs{} ->
                                    pure
                                        ( fromMaybe expr'
                                            ( alignTermTypeVarsToScheme schemeExpected expr'
                                                <|> alignTermTypeVarsToTopTyAbs expr'
                                            )
                                        )
                                _ -> pure (closeTermWithSchemeSubstIfNeeded substExpected schemeExpected expr')
                        Nothing -> pure (fromMaybe expr' (alignTermTypeVarsToTopTyAbs expr'))
                else
                    let instHasUnder inst0 = case inst0 of
                            InstUnder{} -> True
                            InstSeq a b -> instHasUnder a || instHasUnder b
                            InstInside a -> instHasUnder a
                            _ -> False
                    in if sourceAnnIsPolymorphic env exprAnn
                        then pure expr'
                        else if instHasUnder instAdjusted
                            then
                                case expectedSchemeInfo of
                                    Just (schemeExpected, substExpected) ->
                                        pure (closeTermWithSchemeSubstIfNeeded substExpected schemeExpected expr')
                                    Nothing -> pure (closeTermForAnnotation expr')
                            else pure (closeTermForAnnotation expr')
    let exprClosed = stripUnusedTopTyAbs exprClosed0
        instFinal =
            case instAdjusted of
                InstId -> InstId
                _ ->
                    case typeCheck exprClosed of
                        Right tyExpr
                            | Just expectedScheme <- expectedSchemeResult
                            , alphaEqType tyExpr (schemeToType expectedScheme) ->
                                InstId
                        Right tyExpr ->
                            case tyExpr of
                                TForall{} ->
                                    case applyInstantiation tyExpr instAdjusted of
                                        Right tyApplied
                                            | alphaEqType tyApplied tyExpr -> InstId
                                        _ -> instAdjusted
                                _ -> InstId
                        Left _ -> instAdjusted
    pure $ case instFinal of
        InstId -> exprClosed
        _ -> ETyInst exprClosed instFinal
  where
    scopeContext = acScopeContext annotationContext

reifyInst
    :: AnnotationContext
    -> IntSet.IntSet
    -> Map.Map VarName SchemeInfo
    -> AnnExpr
    -> EdgeId
    -> Either ElabError Instantiation
reifyInst annotationContext namedSetReify env funAnn (EdgeId eid) =
    debugGeneralize
        ("reifyInst: edge=" ++ show eid
            ++ " witness=" ++ show (IntMap.member eid edgeWitnesses)
            ++ " trace=" ++ show (IntMap.member eid edgeTraces)
            ++ " exp=" ++ show (IntMap.member eid edgeExpansions)
        )
        ()
        `seq`
        case IntMap.lookup eid edgeWitnesses of
            Nothing ->
                case debugGeneralize
                    ("reifyInst: missing witness for edge " ++ show eid)
                    () of
                    () -> Right InstId
            Just edgeWitness -> do
                let mTrace = IntMap.lookup eid edgeTraces
                    mExpansion = IntMap.lookup eid edgeExpansions
                    mSchemeInfo = schemeInfoForInst funAnn
                case debugGeneralize
                    ("reifyInst scheme edge=" ++ show eid
                        ++ " source=" ++ show (fmap siSubst mSchemeInfo)
                    )
                    () of
                    () -> pure ()
                phi0 <-
                    phiFromEdgeWitnessWithTrace
                        traceCfg
                        generalizeAtWith
                        presolutionView
                        (Just gaParents)
                        mSchemeInfo
                        mTrace
                        edgeWitness
                let substForPhi = maybe IntMap.empty siSubst mSchemeInfo
                    resolvePhiVar v = do
                        nid <- parseNameId v
                        bnd <- ChiQuery.chiLookupVarBound presolutionView (canonical (NodeId nid))
                        either (const Nothing) Just
                            (reifyTypeWithNamedSetNoFallback presolutionView substForPhi namedSetReify bnd)
                    normalizePhiInst inst0 = case inst0 of
                        InstApp (TVar v) -> maybe inst0 InstApp (resolvePhiVar v)
                        InstBot (TVar v) -> maybe inst0 InstBot (resolvePhiVar v)
                        _ -> inst0
                    phi = normalizePhiInst phi0
                case debugGeneralize
                    ("reifyInst phi edge=" ++ show eid ++ " phi=" ++ show phi)
                    () of
                    () -> pure ()
                instFromAuthority <-
                    case (mExpansion, mSchemeInfo) of
                        (Just (ExpInstantiate args), Just schemeInfo) -> do
                            let schemeArity =
                                    case siScheme schemeInfo of
                                        Forall binds _ -> length binds
                                targetTy = authoritativeTargetType namedSetReify edgeWitness schemeInfo
                                traceArgs =
                                    case mTrace of
                                        Just traceInfo | not (null (etBinderArgs traceInfo)) ->
                                            reifyTraceBinderInstArgs namedSetReify schemeInfo (map snd (etBinderArgs traceInfo))
                                        _ -> Nothing
                                targetArgs =
                                    if schemeArity == 0
                                        then Nothing
                                        else inferAuthoritativeInstArgs namedSetReify edgeWitness schemeInfo
                                authoritativeArgs =
                                    case targetArgs of
                                        Just inferred -> Just inferred
                                        Nothing -> traceArgs
                                needsExpansionAuthority =
                                    instNeedsAuthoritativeRefinement phi
                                shouldRefine =
                                    needsExpansionAuthority
                                        || case targetTy of
                                            Just ty -> not (alphaEqType ty (schemeToType (siScheme schemeInfo)))
                                            Nothing -> phi == InstId
                            case debugGeneralize
                                ("reifyInst authoritative edge=" ++ show eid
                                    ++ " expansionArity=" ++ show (length args)
                                    ++ " schemeArity=" ++ show schemeArity
                                    ++ " targetTy=" ++ show targetTy
                                    ++ " targetArgs=" ++ show targetArgs
                                    ++ " traceArgs=" ++ show traceArgs
                                    ++ " shouldRefine=" ++ show shouldRefine
                                )
                                () of
                                () -> pure ()
                            case authoritativeArgs of
                                Just (_binds, inferred)
                                    | shouldRefine
                                    , schemeArity > 0
                                    , not (null inferred) ->
                                        let usableLen = min schemeArity (length inferred)
                                        in pure
                                            (Just
                                                ( instSeqApps
                                                    (map (inlineBoundVarsType presolutionView) (take usableLen inferred))
                                                )
                                            )
                                _
                                    | needsExpansionAuthority
                                    , schemeArity > 0 ->
                                        case expInstantiateArgsToInstNoFallback presolutionView namedSetReify args of
                                            Right inst -> pure (Just inst)
                                            Left _ ->
                                                Left
                                                    (PhiTranslatabilityError
                                                        [ "reifyInst: missing authoritative instantiation translation for edge " ++ show eid
                                                        , "expansion args=" ++ show args
                                                        ]
                                                    )
                                _
                                    | shouldRefine
                                    , schemeArity > 0 ->
                                        Left
                                            (PhiTranslatabilityError
                                                [ "reifyInst: missing authoritative instantiation translation for edge " ++ show eid
                                                , "expansion args=" ++ show args
                                                ]
                                            )
                                _ -> pure Nothing
                        _ -> pure Nothing
                case instFromAuthority of
                    Just inst -> Right inst
                    Nothing -> Right phi
  where
    traceCfg = acTraceConfig annotationContext
    scopeContext = acScopeContext annotationContext
    presolutionView = scPresolutionView scopeContext
    gaParents = scGaParents scopeContext
    generalizeAtWith = scGeneralizeAtWith scopeContext
    edgeWitnesses = acEdgeWitnesses annotationContext
    edgeTraces = acEdgeTraces annotationContext
    edgeExpansions = acEdgeExpansions annotationContext
    canonical = ChiQuery.chiCanonical presolutionView
    debugGeneralize :: String -> a -> a
    debugGeneralize = traceGeneralize traceCfg

    schemeInfoForInst annExpr =
        case annExpr of
            AVar v _ -> Map.lookup v env
            AAnn inner _ _ -> schemeInfoForInst inner
            _ -> Nothing

    inferAuthoritativeInstArgs namedSet schemeInfoWitness schemeInfo =
        case inferFromNode (ewRight schemeInfoWitness) of
            Just args -> Just args
            Nothing -> inferFromNode (ewLeft schemeInfoWitness)
      where
        inferFromNode nodeId =
            inferAgainstTarget =<<
                ( either (const Nothing) Just (reifyTargetType scopeContext namedSet schemeInfo nodeId)
                    <|> either (const Nothing) Just (reifyTargetNodeType scopeContext namedSet schemeInfo nodeId)
                )
        inferAgainstTarget targetTy =
            let (binds, body) = Inst.splitForalls (schemeToType (siScheme schemeInfo))
                schemeTy = schemeToType (siScheme schemeInfo)
                targetHasVisibleForall = case targetTy of
                    TForall {} -> True
                    _ -> False
                inferIdentityLikeTarget =
                    case (binds, body) of
                        ([(binderName, _)], TArrow (TVar dom) (TVar cod))
                            | dom == binderName && cod == binderName ->
                                let args = [TVar binderName]
                                in case applyInstantiation schemeTy (instSeqApps args) of
                                    Right tyApplied
                                        | alphaEqType tyApplied targetTy ->
                                            Just args
                                    _ -> Nothing
                        _ -> Nothing
                normalizeArgs inferred =
                    let rewrite prefix remainingBinds remainingArgs =
                            case (remainingBinds, remainingArgs) of
                                ((binderName, _) : restBinds, argTy : restArgs) ->
                                    let normalizedArg =
                                            case argTy of
                                                TVar v
                                                    | targetHasVisibleForall
                                                    , isJust (parseNameId v) ->
                                                        let candidateArgs = prefix ++ [TVar binderName] ++ restArgs
                                                        in case applyInstantiation schemeTy (instSeqApps candidateArgs) of
                                                            Right tyApplied
                                                                | alphaEqType tyApplied targetTy ->
                                                                    TVar binderName
                                                            _ -> argTy
                                                _ -> argTy
                                     in normalizedArg : rewrite (prefix ++ [normalizedArg]) restBinds restArgs
                                (_, []) -> []
                                ([], restArgs) -> restArgs
                    in rewrite [] binds inferred
                inferredArgs =
                    fmap normalizeArgs (inferInstAppArgsFromScheme binds body targetTy)
                        <|> inferIdentityLikeTarget
            in fmap ((,) binds) inferredArgs

    authoritativeTargetType namedSet edgeWitness schemeInfo =
        case reifyTargetNodeType scopeContext namedSet schemeInfo (ewRight edgeWitness) of
            Right targetTy -> Just targetTy
            Left _ ->
                case reifyTargetNodeType scopeContext namedSet schemeInfo (ewLeft edgeWitness) of
                    Right targetTy -> Just targetTy
                    Left _ -> Nothing

    reifyTraceBinderInstArgs namedSet schemeInfo nodes0 =
        let (binds, _) = Inst.splitForalls (schemeToType (siScheme schemeInfo))
        in fmap ((,) binds) (mapM reifyArg nodes0)
      where
        subst = siSubst schemeInfo
        reifyArg nodeId =
            let nodeC = canonical nodeId
                tyE =
                    case ChiQuery.chiLookupVarBound presolutionView nodeC of
                        Just bnd -> reifyTypeWithNamedSetNoFallback presolutionView subst namedSet bnd
                        Nothing -> reifyTypeWithNamedSetNoFallback presolutionView subst namedSet nodeC
            in either (const Nothing) Just tyE

    instNeedsAuthoritativeRefinement inst =
        case collectApps inst of
            Just tys -> any isPlaceholderTy tys
            Nothing -> False

    isPlaceholderTy ty = case ty of
        TVar _ -> True
        _ -> False

    collectApps inner = case inner of
        InstId -> Just []
        InstApp ty -> Just [ty]
        InstSeq a b -> (++) <$> collectApps a <*> collectApps b
        _ -> Nothing

instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys =
    case map InstApp tys of
        [] -> InstId
        [inst] -> inst
        insts -> foldr1 InstSeq insts

annRefersToVar :: VarName -> AnnExpr -> Bool
annRefersToVar name exprAnn =
    case exprAnn of
        AVar v _ -> v == name
        AAnn inner _ _ -> annRefersToVar name inner
        _ -> False
