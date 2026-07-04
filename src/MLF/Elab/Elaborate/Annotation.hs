{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    closeTermForAnnotation,
    stripUnusedTopTyAbs,
    sourceAnnIsPolymorphic,
    sourceAnnSchemeInfo,
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    freshenTermTypeAbsAgainstEnv,
    reifyInst,
    instSeqApps,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Functor.Foldable (Recursive (project))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    EdgeId (..),
    NodeId (..),
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion (..), ewLeft, ewRight)
import MLF.Elab.Elaborate.Scope
  ( ScopeContext (..),
    generalizeAtNode,
    reifyTargetNodeType,
    reifyTargetType,
  )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTraceReadModel)
import MLF.Elab.Phi.Omega.Normalize (normalizeInst)
import MLF.Elab.Run.Annotation (adjustAnnotationInst)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeForBoundWithContext, inlineBoundVarsTypeWithContext)
import MLF.Elab.TermClosure
  ( alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    alignTopTyAbsToScheme,
    closeTermWithSchemeSubstRefsIfNeeded,
  )
import MLF.Elab.TypeCheck (typeCheck)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabScheme,
    XmlfTerm (..),
    XmlfTermF (..),
    ElabType,
    Instantiation (..),
    InstantiationF (..),
    SchemeInfo (..),
    Ty (..),
    elabToBound,
    eTyAbsWithRef,
    sourceTypeBinderRefsFromIdentities,
    sourceTypeBinderRefOrFreshInScope,
    instAbstrWithRef,
    instUnderWithRef,
    mapResolvedVarType,
    mapBoundType,
    mkElabSchemeWithRefs,
    renameTypeBinderRef,
    resolvedVarType,
    schemeBinderRefs,
    schemeBody,
    schemeFromType,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    TypeBinderRef,
    typeBinderRefAliasNames,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    tyToElab,
  )
import MLF.Frontend.ConstraintGen.Types (AnnExpr (..))
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityAlias)
import MLF.Frontend.Syntax (NormSrcType, SrcBound (..), SrcNorm (NormN), SrcTy (..), StructBound, VarName)
import MLF.Reify.Type (reifyTypeWithNamedSetRefsNoFallbackReadModel)
import MLF.Reify.TypeOps
  ( alphaEqType,
    churchAwareEqType,
    freeTypeVarAliasNamesType,
    freeTypeVarRefsType,
    freshNameLike,
    resolveBaseBoundForInstConstraint,
    substTypeCaptureRef,
  )
import MLF.Types.Identity (IdentityGenerator, TypeBinderIdentity, identityGeneratorAfter, symbolGeneratedIdentities, typeBinderGeneratedIdentities)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data AnnotationContext (p :: Phase) = AnnotationContext
  { acTraceConfig :: TraceConfig,
    acScopeContext :: ScopeContext p,
    acAnnSourceTypes :: IntMap.IntMap NormSrcType,
    acSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    acSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity,
    acEdgeWitnesses :: IntMap.IntMap EdgeWitness,
    acEdgeTraces :: IntMap.IntMap EdgeTrace,
    acEdgeExpansions :: IntMap.IntMap Expansion
  }

closeTermForAnnotation :: XmlfTerm -> XmlfTerm
closeTermForAnnotation term =
  case typeCheck term of
    Right ty ->
      let freeRefs = freeTypeVarRefsType ty
          scheme = mkElabSchemeWithRefs [(ref, Nothing) | ref <- freeRefs] ty
       in closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
    Left _ -> term

stripUnusedTopTyAbs :: XmlfTerm -> XmlfTerm
stripUnusedTopTyAbs term =
  case term of
    ETyAbsRef ref mbBound body ->
      let body' = stripUnusedTopTyAbs body
          term' = ETyAbsRef ref mbBound body'
       in case typeCheck term' of
            Right (TForallRef _ _ bodyTy)
              | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType bodyTy)) -> body'
            _ -> term'
    _ -> term

expInstantiateArgsToInstNoFallback ::
  ScopeContext p ->
  IntSet.IntSet ->
  [NodeId] ->
  Either ElabError Instantiation
expInstantiateArgsToInstNoFallback scopeContext namedSet args = do
  tys <- mapM reifyArg args
  instAppsFromTypes scopeContext tys
  where
    presolutionView = scPresolutionView scopeContext
    constraint = pvConstraint presolutionView
    canonical = pvCanonical presolutionView
    resolveBaseBound = resolveBaseBoundForInstConstraint constraint canonical
    reifyArg arg =
      let argC = canonical arg
          readModel = scReadModel scopeContext
       in case resolveBaseBound argC of
            Just baseC ->
              reifyTypeWithNamedSetRefsNoFallbackReadModel readModel IntMap.empty namedSet baseC
            Nothing ->
              reifyTypeWithNamedSetRefsNoFallbackReadModel readModel IntMap.empty namedSet argC

instAppsFromTypes :: ScopeContext p -> [ElabType] -> Either ElabError Instantiation
instAppsFromTypes scopeContext tys =
  let tys' = map (inlineBoundVarsTypeForBoundWithContext (scInlineBoundVarsContext scopeContext)) tys
   in if null tys'
        then Right InstId
        else Right $ foldr1 InstSeq (map InstApp tys')

sourceAnnIsPolymorphic :: Map.Map VarName SchemeInfo -> AnnExpr -> Bool
sourceAnnIsPolymorphic env sourceAnn =
  case sourceAnn of
    AVar v _ ->
      case Map.lookup v env of
        Just schemeInfo -> not (null (schemeBinderRefs (siScheme schemeInfo)))
        _ -> False
    AAnn inner _ _ -> sourceAnnIsPolymorphic env inner
    AUnfold inner _ _ -> sourceAnnIsPolymorphic env inner
    _ -> False

sourceAnnSchemeInfo :: Map.Map VarName SchemeInfo -> AnnExpr -> Maybe SchemeInfo
sourceAnnSchemeInfo env sourceAnn =
  case sourceAnn of
    AVar v _ -> Map.lookup v env
    AAnn inner _ _ -> sourceAnnSchemeInfo env inner
    AUnfold inner _ _ -> sourceAnnSchemeInfo env inner
    _ -> Nothing

sourceVarName :: AnnExpr -> Maybe VarName
sourceVarName annExpr =
  case annExpr of
    AVar v _ -> Just v
    AAnn inner _ _ -> sourceVarName inner
    AUnfold inner _ _ -> sourceVarName inner
    _ -> Nothing

desugaredAnnLambdaInfo :: VarName -> AnnExpr -> Maybe (NodeId, EdgeId, AnnExpr)
desugaredAnnLambdaInfo param bodyAnn =
  case bodyAnn of
    ALet letName _ _ _ _ rhsAnn innerBodyAnn _
      | letName == param ->
          case rhsAnn of
            AAnn rhsInner annNodeId eid
              | annRefersToVar param rhsInner ->
                  Just (annNodeId, eid, innerBodyAnn)
            _ -> Nothing
    _ -> Nothing

elaborateAnnotationTerm ::
  AnnotationContext p ->
  IntSet.IntSet ->
  Map.Map VarName SchemeInfo ->
  TypeCheck.Env ->
  AnnExpr ->
  NodeId ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateAnnotationTerm annotationContext namedSetReify env tcEnv exprAnn annNodeId eid expr' = do
  expectedSchemeInfo <-
    case generalizeAtNode scopeContext annNodeId of
      Right pair -> pure (Just pair)
      Left _ -> pure Nothing
  let exprFresh = freshenTermTypeAbsAgainstEnv tcEnv expr'
      freshenSchemeAgainstEnv scheme0 =
        let reserved =
              Set.unions
                ( map freeTypeVarAliasNamesType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv tcEnv)))
                    ++ [typeVarRefAliasNames (Map.keys (TypeCheck.typeEnv tcEnv))]
                )
            go _ [] bodyAcc acc = (reverse acc, bodyAcc)
            go used ((ref, mb) : rest) bodyAcc acc =
              let name = typeBinderRefName ref
                  ref' = if Set.member name used then renameTypeBinderRef (freshNameLike name used) ref else ref
                  aliases' = typeBinderRefAliasNames ref'
                  renameTy = TVarRef ref'
                  bodyAcc' =
                    if typeBinderRefsSameIdentityAndName ref' ref
                      then bodyAcc
                      else substTypeCaptureRef ref renameTy bodyAcc
                  acc' = (ref', mb) : acc
               in go (aliases' `Set.union` used) rest bodyAcc' acc'
            (binds', body') = go reserved (schemeBinderRefs scheme0) (schemeBody scheme0) []
         in mkElabSchemeWithRefs binds' body'
      sourceSchemeInfo = sourceAnnSchemeInfo env exprAnn
      canReuseSourceScheme =
        case (sourceSchemeInfo, expectedSchemeInfo) of
          (Just schemeInfo, Just (schemeExpected, _substExpected)) ->
            alphaEqType (schemeToType (siScheme schemeInfo)) (schemeToType schemeExpected)
          _ -> False
      requiresExplicitAnnotationInst =
        case (sourceSchemeInfo, expectedSchemeInfo) of
          (Just schemeInfo, Just (schemeExpected, _substExpected)) ->
            let srcScheme = siScheme schemeInfo
                sourcePoly = not (null (schemeBinderRefs srcScheme))
             in sourcePoly
                  && not (alphaEqType (schemeToType srcScheme) (schemeToType schemeExpected))
          _ -> False
  inst <-
    case (sourceVarName exprAnn, TypeCheck.typeCheckWithEnv tcEnv exprFresh) of
      (Nothing, Right ty)
        | not (case ty of TForallRef {} -> True; _ -> False) ->
            pure InstId
      _ ->
        case reifyInst annotationContext namedSetReify env exprAnn eid of
          Right inst0 -> pure inst0
          Left (PhiTranslatabilityError _)
            | canReuseSourceScheme -> pure InstId
          Left err -> Left err
  expectedSourceScheme <-
    case IntMap.lookup (getNodeId annNodeId) (acAnnSourceTypes annotationContext) of
      Just srcTy -> Just . schemeFromType <$> srcTypeToElabType annotationContext srcTy
      Nothing -> pure Nothing
  let expectedSchemeInfoForClose =
        case expectedSourceScheme of
          Just schemeExpected -> Just (schemeExpected, IntMap.empty)
          Nothing -> expectedSchemeInfo
      expectedSchemeResult = expectedSourceScheme <|> fmap fst expectedSchemeInfo
      sourceLambdaParamClosed =
        expectedSourceScheme >>= \schemeExpected ->
          closeAnnotatedLambdaParam tcEnv (schemeToType schemeExpected) exprFresh
      mExpectedBound =
        case expectedSchemeResult of
          Just schemeExpected ->
            case schemeBinderRefs schemeExpected of
              (_, Just bnd) : _ -> Just (tyToElab bnd)
              _ -> Nothing
          _ -> Nothing
      dropAnnotationElims inst0 = case project inst0 of
        InstElimF -> InstId
        InstSeqF a b ->
          let a' = dropAnnotationElims a
              b' = dropAnnotationElims b
           in case (a', b') of
                (InstId, x) -> x
                (x, InstId) -> x
                _ -> InstSeq a' b'
        InstInsideF a -> InstInside (dropAnnotationElims a)
        InstUnderFRef ref a -> instUnderWithRef ref (dropAnnotationElims a)
        InstAbstrFRef ref -> instAbstrWithRef ref
        InstIdF -> InstId
        InstAppF ty -> InstApp ty
        InstBotF ty -> InstBot ty
        InstIntroF -> InstIntro
      preservesForalls =
        isJust mExpectedBound
          || isJust (alignTermTypeVarsToTopTyAbs exprFresh)
          || case exprFresh of
            ETyAbsRef {} -> True
            _ -> False
      instAdjusted0 =
        if preservesForalls
          then normalizeInst (adjustAnnotationInst inst)
          else normalizeInst (dropAnnotationElims inst)
      instAdjusted =
        case (mExpectedBound, instAdjusted0) of
          (Just expectedBound, InstInside (InstBot _)) ->
            InstInside (InstBot expectedBound)
          _ -> instAdjusted0
  exprClosed0 <-
    if instAdjusted == InstId && requiresExplicitAnnotationInst
      then
        Left
          ( PhiTranslatabilityError
              [ "AAnnF: missing authoritative instantiation for annotation edge " ++ show eid
              ]
          )
      else
        if instAdjusted == InstId
          then
            if canReuseSourceScheme && sourceAnnIsPolymorphic env exprAnn
              then pure exprFresh
              else case sourceLambdaParamClosed of
                Just closed -> pure closed
                Nothing -> case expectedSchemeInfoForClose of
                  Just (schemeExpected, substExpected) ->
                    let alignedExpr =
                          fromMaybe
                            exprFresh
                            ( alignTopTyAbsToScheme schemeExpected exprFresh
                                <|> alignTermTypeVarsToScheme schemeExpected exprFresh
                                <|> alignTermTypeVarsToTopTyAbs exprFresh
                            )
                        alignedExprMatchesExpected =
                          case TypeCheck.typeCheckWithEnv tcEnv alignedExpr of
                            Right tyExpr ->
                              alphaEqType tyExpr (schemeToType schemeExpected)
                                || churchAwareEqType tyExpr (schemeToType schemeExpected)
                            Left _ -> False
                     in case exprFresh of
                          ETyAbsRef {}
                            | alignedExprMatchesExpected ->
                                pure alignedExpr
                            | otherwise ->
                                pure (closeTermWithSchemeSubstRefsIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) alignedExpr)
                          _ -> pure (closeTermWithSchemeSubstRefsIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                  Nothing -> pure (fromMaybe exprFresh (alignTermTypeVarsToTopTyAbs exprFresh))
          else
            let instHasUnder inst0 = case inst0 of
                  InstUnderRef {} -> True
                  InstSeq a b -> instHasUnder a || instHasUnder b
                  InstInside a -> instHasUnder a
                  _ -> False
                instLooksLikeApp inst0 = case inst0 of
                  InstApp {} -> True
                  InstInside (InstBot _) -> True
                  InstInside (InstApp _) -> True
                  InstSeq (InstInside (InstBot _)) InstElim -> True
                  InstSeq (InstInside (InstApp _)) InstElim -> True
                  _ -> False
             in if sourceAnnIsPolymorphic env exprAnn
                  then pure exprFresh
                  else
                    if instLooksLikeApp instAdjusted
                      then case (sourceVarName exprAnn, TypeCheck.typeCheckWithEnv tcEnv exprFresh) of
                        (Nothing, Right TForallRef {}) ->
                          if instHasUnder instAdjusted
                            then case expectedSchemeInfoForClose of
                              Just (schemeExpected, substExpected) ->
                                pure (closeTermWithSchemeSubstRefsIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                              Nothing -> pure (closeTermForAnnotation exprFresh)
                            else pure (closeTermForAnnotation exprFresh)
                        (Nothing, Right _) -> pure exprFresh
                        _ ->
                          if instHasUnder instAdjusted
                            then case expectedSchemeInfoForClose of
                              Just (schemeExpected, substExpected) ->
                                pure (closeTermWithSchemeSubstRefsIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                              Nothing -> pure (closeTermForAnnotation exprFresh)
                            else pure (closeTermForAnnotation exprFresh)
                      else
                        if instHasUnder instAdjusted
                          then case expectedSchemeInfoForClose of
                            Just (schemeExpected, substExpected) ->
                              pure (closeTermWithSchemeSubstRefsIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                            Nothing -> pure (closeTermForAnnotation exprFresh)
                          else pure (closeTermForAnnotation exprFresh)
  let exprClosed =
        rollExplicitMuAnnotation tcEnv expectedSchemeResult $
          stripUnusedTopTyAbs exprClosed0
      instFinal =
        case instAdjusted of
          InstId -> InstId
          _ ->
            let instLooksLikeApp inst0 = case inst0 of
                  InstApp {} -> True
                  InstInside (InstBot _) -> True
                  InstInside (InstApp _) -> True
                  InstSeq (InstInside (InstBot _)) InstElim -> True
                  InstSeq (InstInside (InstApp _)) InstElim -> True
                  _ -> False
                canonicalizeAppLikeInst inst0 = case inst0 of
                  InstApp ty -> InstApp ty
                  InstInside (InstBot ty) -> InstApp ty
                  InstInside (InstApp ty) -> InstApp ty
                  InstSeq (InstInside (InstBot ty)) InstElim -> InstApp ty
                  InstSeq (InstInside (InstApp ty)) InstElim -> InstApp ty
                  _ -> inst0
             in if instLooksLikeApp instAdjusted && sourceVarName exprAnn == Nothing
                  then InstId
                  else case TypeCheck.typeCheckWithEnv tcEnv exprClosed of
                    Right tyExpr
                      | Just expectedScheme <- expectedSchemeResult,
                        alphaEqType tyExpr (schemeToType expectedScheme) ->
                          InstId
                    Right tyExpr ->
                      let instCanon = canonicalizeAppLikeInst instAdjusted
                       in if instLooksLikeApp instAdjusted
                            then case TypeCheck.typeCheckWithEnv tcEnv (ETyInst exprClosed instCanon) of
                              Right _ -> instCanon
                              Left _ -> InstId
                            else case tyExpr of
                              TForallRef {} ->
                                case applyInstantiation tyExpr instCanon of
                                  Right tyApplied
                                    | alphaEqType tyApplied tyExpr -> InstId
                                  _ -> instCanon
                              _ -> InstId
                    Left _ -> instAdjusted
  pure $ case instFinal of
    InstId -> exprClosed
    _ -> ETyInst exprClosed instFinal
  where
    scopeContext = acScopeContext annotationContext

    rollExplicitMuAnnotation :: TypeCheck.Env -> Maybe ElabScheme -> XmlfTerm -> XmlfTerm
    rollExplicitMuAnnotation checkEnv mbExpected term =
      case schemeToType <$> mbExpected of
        Just muTy@TMuRef {} ->
          case TypeCheck.typeCheckWithEnv checkEnv term of
            Right termTy
              | alphaEqType termTy muTy -> term
              | churchAwareEqType termTy muTy,
                let rolled = ERoll muTy term,
                Right _ <- TypeCheck.typeCheckWithEnv checkEnv rolled ->
                  rolled
              | Just unfoldedTy <- unfoldMuOnce muTy,
                alphaEqType termTy unfoldedTy || churchAwareEqType termTy unfoldedTy ->
                  ERoll muTy term
            _ ->
              case unfoldMuOnce muTy of
                Just unfoldedTy ->
                  let aligned = alignTermAlongType unfoldedTy term
                   in case TypeCheck.typeCheckWithEnv tcEnv aligned of
                        Right alignedTy
                          | alphaEqType alignedTy unfoldedTy || churchAwareEqType alignedTy unfoldedTy ->
                              ERoll muTy aligned
                        _ -> term
                Nothing -> term
        _ -> term

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce ty =
      case ty of
        TMuRef ref body -> Just (substTypeCaptureRef ref ty body)
        _ -> Nothing

    alignTermAlongType :: ElabType -> XmlfTerm -> XmlfTerm
    alignTermAlongType targetTy term =
      case (targetTy, term) of
        (TForallRef targetRef _mbBound targetBody, ETyAbsRef termRef termBound body)
          | typeBinderRefsSameIdentity targetRef termRef ->
              ETyAbsRef termRef termBound (alignTermAlongType targetBody body)
        (TForallRef targetRef mbBound targetBody, _) ->
          ETyAbsRef targetRef mbBound (alignTermAlongType targetBody term)
        (TArrow dom cod, ELam resolved body) ->
          ELam (mapResolvedVarType (const dom) resolved) (alignTermAlongType cod body)
        _ -> term

    closeAnnotatedLambdaParam :: TypeCheck.Env -> ElabType -> XmlfTerm -> Maybe XmlfTerm
    closeAnnotatedLambdaParam checkEnv annotationTy term =
      case annotationTy of
        TForallRef {} ->
          let aligned = alignTermAlongType annotationTy term
           in case TypeCheck.typeCheckWithEnv checkEnv aligned of
                Right alignedTy
                  | alphaEqType alignedTy annotationTy || churchAwareEqType alignedTy annotationTy ->
                      Just aligned
                _ -> Nothing
        _ ->
          case (annotationTy, term) of
            (TArrow dom _, ELam resolved body)
              | TVarRef binderRef <- resolvedVarType resolved -> do
                  bound <- either (const Nothing) Just (elabToBound dom)
                  let closed = eTyAbsWithRef binderRef (Just bound) (ELam resolved body)
                  case TypeCheck.typeCheckWithEnv checkEnv closed of
                    Right _ -> Just closed
                    Left _ -> Nothing
            _ -> Nothing

reifyInst ::
  AnnotationContext p ->
  IntSet.IntSet ->
  Map.Map VarName SchemeInfo ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInst annotationContext namedSetReify env funAnn (EdgeId eid) =
  debugGeneralize
    ( "reifyInst: edge="
        ++ show eid
        ++ " witness="
        ++ show (IntMap.member eid edgeWitnesses)
        ++ " trace="
        ++ show (IntMap.member eid edgeTraces)
        ++ " exp="
        ++ show (IntMap.member eid edgeExpansions)
    )
    ()
    `seq` case IntMap.lookup eid edgeWitnesses of
      Nothing ->
        case debugGeneralize
          ("reifyInst: missing witness for edge " ++ show eid)
          () of
          () -> Right InstId
      Just edgeWitness -> do
        mSchemeInfo <- schemeInfoForInst funAnn
        let mTrace = IntMap.lookup eid edgeTraces
            mExpansion = IntMap.lookup eid edgeExpansions
        case debugGeneralize
          ( "reifyInst scheme edge="
              ++ show eid
              ++ " source="
              ++ show (fmap schemeInfoBinderRefSubst mSchemeInfo)
          )
          () of
          () -> pure ()
        phi0 <-
          case
            phiFromEdgeWitnessWithTraceReadModel
              traceCfg
              generalizeAtWith
              (scReadModel scopeContext)
              (Just gaParents)
              mSchemeInfo
              mTrace
              edgeWitness
            of
            Right phi0' -> pure phi0'
            Left err -> Left err
        let substForPhi = maybe IntMap.empty schemeInfoBinderRefSubst mSchemeInfo
            resolvePhiVar ref = do
              nid <- typeBinderRefNode ref
              bnd <- pvLookupVarBound presolutionView (canonical nid)
              either
                (const Nothing)
                Just
                (reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) substForPhi namedSetReify bnd)
            normalizePhiInst inst0 = case inst0 of
              InstApp (TVarRef ref) -> maybe inst0 InstApp (resolvePhiVar ref)
              InstBot (TVarRef ref) -> maybe inst0 InstBot (resolvePhiVar ref)
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
                    length (schemeBinderRefs (siScheme schemeInfo))
                  targetTy = authoritativeTargetType namedSetReify edgeWitness schemeInfo
                  traceArgs =
                    case mTrace of
                      Just traceInfo
                        | not (null (etBinderArgs traceInfo)) ->
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
                  schemeTy = schemeToType (siScheme schemeInfo)
                  longestValidAppPrefix tys =
                    let go _ acc [] = acc
                        go cur acc (tyArg : rest) =
                          case cur of
                            TForallRef {} ->
                              case applyInstantiation cur (InstApp tyArg) of
                                Right cur' -> go cur' (acc ++ [tyArg]) rest
                                Left _ -> acc
                            _ -> acc
                     in go schemeTy [] tys
                  longestValidExpansionInst nodeArgs =
                    let tryPrefixes n best
                          | n <= 0 = best
                          | otherwise =
                              case expInstantiateArgsToInstNoFallback scopeContext namedSetReify (take n nodeArgs) of
                                Right inst
                                  | Right _ <- applyInstantiation schemeTy inst -> tryPrefixes (n - 1) (Just inst)
                                _ -> tryPrefixes (n - 1) best
                     in tryPrefixes (length nodeArgs) Nothing
              case debugGeneralize
                ( "reifyInst authoritative edge="
                    ++ show eid
                    ++ " expansionArity="
                    ++ show (length args)
                    ++ " schemeArity="
                    ++ show schemeArity
                    ++ " targetTy="
                    ++ show targetTy
                    ++ " targetArgs="
                    ++ show targetArgs
                    ++ " traceArgs="
                    ++ show traceArgs
                    ++ " shouldRefine="
                    ++ show shouldRefine
                )
                () of
                () -> pure ()
              case authoritativeArgs of
                Just (_binds, inferred)
                  | shouldRefine,
                    schemeArity > 0,
                    not (null inferred) ->
                      let usable =
                            longestValidAppPrefix
                              ( map
                                  (inlineBoundVarsTypeWithContext (scInlineBoundVarsContext scopeContext))
                                  (take (min schemeArity (length inferred)) inferred)
                              )
                       in pure
                            ( Just
                                (instSeqApps usable)
                            )
                _
                  | needsExpansionAuthority,
                    schemeArity > 0 ->
                      case longestValidExpansionInst args of
                        Just inst -> pure (Just inst)
                        Nothing ->
                          Left
                            ( PhiTranslatabilityError
                                [ "reifyInst: missing authoritative instantiation translation for edge " ++ show eid,
                                  "expansion args=" ++ show args
                                ]
                            )
                _
                  | shouldRefine,
                    schemeArity > 0 ->
                      pure Nothing
                _ -> pure Nothing
            _ -> pure Nothing
        case instFromAuthority of
          Just inst -> Right inst
          Nothing ->
            case phi of
              _ -> Right phi
  where
    traceCfg = acTraceConfig annotationContext
    scopeContext = acScopeContext annotationContext
    presolutionView = scPresolutionView scopeContext
    gaParents = scGaParents scopeContext
    generalizeAtWith = scGeneralizeAtWith scopeContext
    edgeWitnesses = acEdgeWitnesses annotationContext
    edgeTraces = acEdgeTraces annotationContext
    edgeExpansions = acEdgeExpansions annotationContext
    canonical = pvCanonical presolutionView
    debugGeneralize :: String -> a -> a
    debugGeneralize = traceGeneralize traceCfg

    schemeInfoForInst annExpr =
      do
        synthetic <- syntheticLetSchemeInfo annExpr
        case synthetic of
          Just schemeInfo -> pure (Just schemeInfo)
          Nothing ->
            case annExpr of
              AVar v _ -> pure (Map.lookup v env)
              AAnn inner _ _ -> schemeInfoForInst inner
              AUnfold inner _ _ -> schemeInfoForInst inner
              _ -> pure Nothing

    syntheticLetSchemeInfo annExpr =
      case annExpr of
        ALet letName _ schemeRootId _ _ rhsAnn bodyAnn _
          | annRefersToVar letName bodyAnn ->
              firstJustE
                (explicitSourceAnnotatedScheme rhsAnn)
                ( firstJustE
                    (explicitSourceAnnotatedScheme annExpr)
                    ( pure
                        ( case generalizeAtNode scopeContext schemeRootId of
                            Right (scheme, subst) -> Just (schemeInfoFromRefSubst scheme subst)
                            Left _ -> Nothing
                        )
                    )
                )
        AAnn inner _ _ -> syntheticLetSchemeInfo inner
        AUnfold inner _ _ -> syntheticLetSchemeInfo inner
        _ -> pure Nothing

    explicitSourceAnnotatedScheme annExpr =
      case annExpr of
        AAnn inner annNodeId _ ->
          case IntMap.lookup (getNodeId annNodeId) (acAnnSourceTypes annotationContext) of
            Just srcTy -> Just <$> sourceSchemeInfoFromType srcTy
            Nothing -> explicitSourceAnnotatedScheme inner
        ALam _ _ _ body _ -> explicitSourceAnnotatedScheme body
        AApp fun arg _ _ _ ->
          firstJustE (explicitSourceAnnotatedScheme fun) (explicitSourceAnnotatedScheme arg)
        ALet _ _ _ _ _ rhs body _ ->
          firstJustE (explicitSourceAnnotatedScheme rhs) (explicitSourceAnnotatedScheme body)
        AUnfold inner _ _ -> explicitSourceAnnotatedScheme inner
        _ -> pure Nothing

    sourceSchemeInfoFromType srcTy = do
      ty <- srcTypeToElabType annotationContext srcTy
      pure (schemeInfoFromRefSubst (schemeFromType ty) IntMap.empty)

    firstJustE left right = do
      result <- left
      case result of
        Just _ -> pure result
        Nothing -> right

    inferAuthoritativeInstArgs namedSet schemeInfoWitness schemeInfo =
      case inferFromNode (ewRight schemeInfoWitness) of
        Just args -> Just args
        Nothing -> inferFromNode (ewLeft schemeInfoWitness)
      where
        inferFromNode nodeId =
          inferAgainstTarget
            =<< ( either (const Nothing) Just (reifyTargetType scopeContext namedSet schemeInfo nodeId)
                    <|> either (const Nothing) Just (reifyTargetNodeType scopeContext namedSet schemeInfo nodeId)
                )
        inferAgainstTarget targetTy =
          let binds = schemeBinderRefs (siScheme schemeInfo)
              body = schemeBody (siScheme schemeInfo)
              schemeTy = schemeToType (siScheme schemeInfo)
              targetHasVisibleForall = case targetTy of
                TForallRef {} -> True
                _ -> False
              isInternalTypeBinderRef ref =
                isJust (typeBinderRefNode ref)
              inferIdentityLikeTarget =
                case (binds, body) of
                  ([(binderRef, _)], TArrow (TVarRef domRef) (TVarRef codRef))
                    | typeBinderRefsSameIdentity binderRef domRef && typeBinderRefsSameIdentity binderRef codRef ->
                        let args = [TVarRef binderRef]
                         in case applyInstantiation schemeTy (instSeqApps args) of
                              Right tyApplied
                                | alphaEqType tyApplied targetTy ->
                                    Just args
                              _ -> Nothing
                  _ -> Nothing
              normalizeArgs inferred =
                let rewrite prefix remainingBinds remainingArgs =
                      case (remainingBinds, remainingArgs) of
                        ((binderRef, _) : restBinds, argTy : restArgs) ->
                          let normalizedArg =
                                case argTy of
                                  TVarRef argRef
                                    | targetHasVisibleForall,
                                      isInternalTypeBinderRef argRef ->
                                        let candidateArgs = prefix ++ [TVarRef binderRef] ++ restArgs
                                         in case applyInstantiation schemeTy (instSeqApps candidateArgs) of
                                              Right tyApplied
                                                | alphaEqType tyApplied targetTy ->
                                                    TVarRef binderRef
                                              _ -> argTy
                                  _ -> argTy
                           in normalizedArg : rewrite (prefix ++ [normalizedArg]) restBinds restArgs
                        (_, []) -> []
                        ([], restArgs) -> restArgs
                 in rewrite [] binds inferred
              inferredArgs =
                fmap
                  normalizeArgs
                  ( inferInstAppArgsFromSchemeRefs
                      (schemeBinderRefs (siScheme schemeInfo))
                      (schemeBody (siScheme schemeInfo))
                      targetTy
                  )
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
      fmap ((,) (schemeBinderRefs (siScheme schemeInfo))) (mapM reifyArg nodes0)
      where
        subst = schemeInfoBinderRefSubst schemeInfo
        reifyArg nodeId =
          let nodeC = canonical nodeId
              tyE =
                case pvLookupVarBound presolutionView nodeC of
                  Just bnd -> reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSet bnd
                  Nothing -> reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSet nodeC
           in either (const Nothing) Just tyE

    instNeedsAuthoritativeRefinement inst =
      case collectApps inst of
        Just tys -> any isPlaceholderTy tys
        Nothing -> False

    isPlaceholderTy ty = case ty of
      TVarRef _ -> True
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
    AUnfold inner _ _ -> annRefersToVar name inner
    _ -> False

freshenTermTypeAbsAgainstEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
freshenTermTypeAbsAgainstEnv env = go reserved
  where
    reserved =
      Set.unions
        ( map freeTypeVarAliasNamesType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv env)))
            ++ [typeVarRefAliasNames (Map.keys (TypeCheck.typeEnv env))]
        )

    go used term = case term of
      ETyAbsRef ref mb body ->
        let name = typeBinderRefName ref
            usedForBinder = Set.union used (maybe Set.empty freeTypeVarAliasNamesType mb)
            (ref', body') =
              if Set.member name usedForBinder
                then
                  let fresh = freshNameLike name usedForBinder
                      freshRef = renameTypeBinderRef fresh ref
                   in (freshRef, renameTypeVarInTerm ref freshRef body)
                else (ref, body)
            used' = typeBinderRefAliasNames ref' `Set.union` usedForBinder
         in ETyAbsRef ref' mb (go used' body')
      ELam resolved body ->
        ELam resolved (go (Set.union used (freeTypeVarAliasNamesType (resolvedVarType resolved))) body)
      EApp f a -> EApp (go used f) (go used a)
      ELet resolved sch rhs body ->
        let used' = Set.union used (freeTypeVarAliasNamesType (schemeToType sch))
         in ELet resolved sch (go used' rhs) (go used' body)
      ETyInst t inst -> ETyInst (go used t) inst
      ERoll ty body -> ERoll ty (go used body)
      EUnroll body -> EUnroll (go used body)
      _ -> term

typeVarRefAliasNames :: [TypeBinderRef] -> Set.Set String
typeVarRefAliasNames =
  Set.unions . map typeBinderRefAliasNames

renameTypeVarInTerm :: TypeBinderRef -> TypeBinderRef -> XmlfTerm -> XmlfTerm
renameTypeVarInTerm oldRef newRef term =
  let renameTy = substTypeCaptureRef oldRef (TVarRef newRef)
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameRef ref
        | typeBinderRefsSameIdentity ref oldRef = newRef
        | otherwise = ref
      renameInst inst = case project inst of
        InstIdF -> InstId
        InstAppF ty -> InstApp (renameTy ty)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstInsideF inner -> InstInside (renameInst inner)
        InstSeqF a b -> InstSeq (renameInst a) (renameInst b)
        InstUnderFRef ref inner -> instUnderWithRef (renameRef ref) (renameInst inner)
        InstBotF ty -> InstBot (renameTy ty)
        InstAbstrFRef ref -> instAbstrWithRef (renameRef ref)
   in case project term of
        EVarNodeF resolved -> EVarNode (mapResolvedVarType renameTy resolved)
        ELitF lit -> ELit lit
        ELamF resolved body ->
          ELam (mapResolvedVarType renameTy resolved) (renameTypeVarInTerm oldRef newRef body)
        EAppF f a -> EApp (renameTypeVarInTerm oldRef newRef f) (renameTypeVarInTerm oldRef newRef a)
        ELetF resolved sch rhs body ->
          ELet
            (mapResolvedVarType renameTy resolved)
            (renameScheme sch)
            (renameTypeVarInTerm oldRef newRef rhs)
            (renameTypeVarInTerm oldRef newRef body)
        ETyAbsFRef ref mb body
          | typeBinderRefsSameIdentity ref oldRef -> eTyAbsWithRef ref (fmap renameBound mb) body
          | otherwise -> eTyAbsWithRef ref (fmap renameBound mb) (renameTypeVarInTerm oldRef newRef body)
        ETyInstF t inst -> ETyInst (renameTypeVarInTerm oldRef newRef t) (renameInst inst)
        ERollF ty body -> ERoll (renameTy ty) (renameTypeVarInTerm oldRef newRef body)
        EUnrollF body -> EUnroll (renameTypeVarInTerm oldRef newRef body)

srcTypeToElabType :: AnnotationContext p -> NormSrcType -> Either ElabError ElabType
srcTypeToElabType annotationContext ty =
  let (refs, generator) =
        sourceTypeBinderRefsFromIdentities
          (acSourceTypeBinderIdentities annotationContext)
          (Set.toList (freeSrcTypeVars ty))
          (sourceTypeIdentityGenerator annotationContext ty)
   in fmap fst (srcTypeToElabTypeWith (acSourceTypeHeadIdentities annotationContext) (acSourceTypeBinderIdentities annotationContext) refs generator ty)

sourceTypeIdentityGenerator :: AnnotationContext p -> NormSrcType -> IdentityGenerator
sourceTypeIdentityGenerator annotationContext ty =
  identityGeneratorAfter
    ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
        ++ concatMap typeBinderGeneratedIdentities (Map.elems (acSourceTypeBinderIdentities annotationContext))
    )
  where
    headIdentities =
      Map.union
        (acSourceTypeHeadIdentities annotationContext)
        (Builtins.builtinSourceTypeHeadIdentities ty)

freeSrcTypeVars :: SrcTy n v -> Set.Set String
freeSrcTypeVars ty =
  go Set.empty ty
  where
    go :: Set.Set String -> SrcTy n0 v0 -> Set.Set String
    go bound srcTy =
      case srcTy of
        STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go bound dom `Set.union` go bound cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STTyLam name body -> go (Set.insert name bound) body
        STTyApp fun arg -> go bound fun `Set.union` go bound arg
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body -> go (Set.insert name bound) body
        STBottom -> Set.empty

srcTypeToElabTypeWith :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> Map.Map String TypeBinderRef -> IdentityGenerator -> NormSrcType -> Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (TVarRef ref, generator)
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator1 cod
    Right (TArrow dom' cod', generator2)
  STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    Right (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args', generator')
  STVarApp name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (TVarAppRef ref args', generator')
  STTyLam {} ->
    Left (InstantiationError "residual type lambda reached elaboration")
  STTyApp {} ->
    Left (InstantiationError "residual type application reached elaboration")
  STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator2 body
          Right (TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities (Map.insert name ref refs) generator1 body
          Right (TMuRef ref body', generator2)
  STBase name -> Right (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name), generator)
  STBottom -> Right (TBottom, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached annotation elaboration"))

    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

srcBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  SrcBound 'NormN ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator bound = case bound of
  SrcBound ty -> structBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator ty

structBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  StructBound ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
structBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator bTy = case bTy of
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator1 cod
    Right (Just (TArrow dom' cod'), generator2)
  STBase name -> Right (Just (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name)), generator)
  STCon name args -> do
    (args', generator1) <- srcTypesToElabTypesWith refs generator args
    Right (Just (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args'), generator1)
  STVarApp name args -> do
    (args', generator1) <- srcTypesToElabTypesWith refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (Just (TVarAppRef ref args'), generator1)
  STTyLam {} ->
    Left (InstantiationError "residual type lambda reached elaboration")
  STTyApp {} ->
    Left (InstantiationError "residual type application reached elaboration")
  STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator2 body
          Right (Just (TForallRef ref mb' body'), generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        boundNames' = Set.insert name boundNames
     in do
      (body', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities (Map.insert name ref refs) generator1 body
      Right (Just (TMuRef ref body'), generator2)
  STBottom -> Right (Nothing, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached annotation elaboration"))

    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name

    srcTypesToElabTypesWith refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWith headIdentities binderIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWith headIdentities binderIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference
