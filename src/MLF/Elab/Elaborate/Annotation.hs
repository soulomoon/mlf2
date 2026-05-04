{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    closeTermForAnnotation,
    stripUnusedTopTyAbs,
    sourceAnnIsPolymorphic,
    sourceAnnSchemeInfo,
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    reifyInst,
    instSeqApps,
  )
where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (EdgeTrace (..))
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    EdgeId (..),
    NodeId (..),
  )
import MLF.Constraint.Types.Witness (EdgeWitness (..), Expansion (..))
import MLF.Elab.Elaborate.Scope
  ( ScopeContext (..),
    generalizeAtNode,
    reifyTargetNodeType,
    reifyTargetType,
  )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.Legacy (expInstantiateArgsToInstNoFallback)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Phi.Omega.Normalize (normalizeInst)
import MLF.Elab.Run.Annotation (adjustAnnotationInst)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType)
import MLF.Elab.TermClosure
  ( alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    alignTopTyAbsToScheme,
    closeTermWithSchemeSubstIfNeeded,
  )
import MLF.Elab.TypeCheck (typeCheck)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabScheme,
    ElabTerm (..),
    ElabType,
    Instantiation (..),
    SchemeInfo (..),
    Ty (..),
    elabToBound,
    mapBoundType,
    schemeFromType,
    tyToElab,
    pattern Forall,
  )
import MLF.Frontend.ConstraintGen.Types (AnnExpr (..))
import MLF.Frontend.Syntax (NormSrcType, SrcBound (..), SrcNorm (NormN), SrcTy (..), StructBound, VarName)
import MLF.Reify.Core (reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarsType, freshNameLike, parseNameId, substTypeCapture)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data AnnotationContext = AnnotationContext
  { acTraceConfig :: TraceConfig,
    acScopeContext :: ScopeContext,
    acAnnSourceTypes :: IntMap.IntMap NormSrcType,
    acEdgeWitnesses :: IntMap.IntMap EdgeWitness,
    acEdgeTraces :: IntMap.IntMap EdgeTrace,
    acEdgeExpansions :: IntMap.IntMap Expansion
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
        Just SchemeInfo {siScheme = Forall binds _} -> not (null binds)
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
  AnnotationContext ->
  IntSet.IntSet ->
  Map.Map VarName SchemeInfo ->
  AnnExpr ->
  NodeId ->
  EdgeId ->
  ElabTerm ->
  Either ElabError ElabTerm
elaborateAnnotationTerm annotationContext namedSetReify env exprAnn annNodeId eid expr' = do
  expectedSchemeInfo <-
    case generalizeAtNode scopeContext annNodeId of
      Right pair -> pure (Just pair)
      Left _ -> pure Nothing
  let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
      exprFresh = freshenTermTypeAbsAgainstEnv tcEnv expr'
      freshenSchemeAgainstEnv scheme0 =
        case scheme0 of
          Forall binds body ->
            let reserved =
                  Set.unions
                    ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv tcEnv))
                        ++ [Set.fromList (Map.keys (TypeCheck.typeEnv tcEnv))]
                    )
                go _ [] bodyAcc acc = (reverse acc, bodyAcc)
                go used ((name, mb) : rest) bodyAcc acc =
                  let name' = if Set.member name used then freshNameLike name used else name
                      renameTy = TVar name'
                      bodyAcc' = if name' == name then bodyAcc else substTypeCapture name renameTy bodyAcc
                      acc' = (name', mb) : acc
                   in go (Set.insert name' used) rest bodyAcc' acc'
                (binds', body') = go reserved binds body []
             in Forall binds' body'
      sourceSchemeInfo = sourceAnnSchemeInfo env exprAnn
      canReuseSourceScheme =
        case (sourceSchemeInfo, expectedSchemeInfo) of
          (Just schemeInfo, Just (schemeExpected, _substExpected)) ->
            alphaEqType (schemeToType (siScheme schemeInfo)) (schemeToType schemeExpected)
          _ -> False
      requiresExplicitAnnotationInst =
        case (sourceSchemeInfo, expectedSchemeInfo) of
          (Just SchemeInfo {siScheme = srcScheme}, Just (schemeExpected, _substExpected)) ->
            let sourcePoly = case srcScheme of
                  Forall binds _ -> not (null binds)
             in sourcePoly
                  && not (alphaEqType (schemeToType srcScheme) (schemeToType schemeExpected))
          _ -> False
  inst <-
    case (sourceVarName exprAnn, TypeCheck.typeCheckWithEnv tcEnv exprFresh) of
      (Nothing, Right ty)
        | not (case ty of TForall {} -> True; _ -> False) ->
            pure InstId
      _ ->
        case reifyInst annotationContext namedSetReify env exprAnn eid of
          Right inst0 -> pure inst0
          Left (PhiTranslatabilityError _)
            | canReuseSourceScheme -> pure InstId
          Left err -> Left err
  expectedSourceScheme <-
    case IntMap.lookup (getNodeId annNodeId) (acAnnSourceTypes annotationContext) of
      Just srcTy -> Just . schemeFromType <$> srcTypeToElabType srcTy
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
          || isJust (alignTermTypeVarsToTopTyAbs exprFresh)
          || case exprFresh of
            ETyAbs {} -> True
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
                          ETyAbs {}
                            | alignedExprMatchesExpected ->
                                pure alignedExpr
                            | otherwise ->
                                pure (closeTermWithSchemeSubstIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) alignedExpr)
                          _ -> pure (closeTermWithSchemeSubstIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                  Nothing -> pure (fromMaybe exprFresh (alignTermTypeVarsToTopTyAbs exprFresh))
          else
            let instHasUnder inst0 = case inst0 of
                  InstUnder {} -> True
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
                        (Nothing, Right TForall {}) ->
                          if instHasUnder instAdjusted
                            then case expectedSchemeInfoForClose of
                              Just (schemeExpected, substExpected) ->
                                pure (closeTermWithSchemeSubstIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                              Nothing -> pure (closeTermForAnnotation exprFresh)
                            else pure (closeTermForAnnotation exprFresh)
                        (Nothing, Right _) -> pure exprFresh
                        _ ->
                          if instHasUnder instAdjusted
                            then case expectedSchemeInfoForClose of
                              Just (schemeExpected, substExpected) ->
                                pure (closeTermWithSchemeSubstIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
                              Nothing -> pure (closeTermForAnnotation exprFresh)
                            else pure (closeTermForAnnotation exprFresh)
                      else
                        if instHasUnder instAdjusted
                          then case expectedSchemeInfoForClose of
                            Just (schemeExpected, substExpected) ->
                              pure (closeTermWithSchemeSubstIfNeeded substExpected (freshenSchemeAgainstEnv schemeExpected) exprFresh)
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
                              TForall {} ->
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

    rollExplicitMuAnnotation :: TypeCheck.Env -> Maybe ElabScheme -> ElabTerm -> ElabTerm
    rollExplicitMuAnnotation tcEnv mbExpected term =
      case schemeToType <$> mbExpected of
        Just muTy@TMu {} ->
          case TypeCheck.typeCheckWithEnv tcEnv term of
            Right termTy
              | alphaEqType termTy muTy -> term
              | churchAwareEqType termTy muTy,
                let rolled = ERoll muTy term,
                Right _ <- TypeCheck.typeCheckWithEnv tcEnv rolled ->
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
        TMu name body -> Just (substTypeCapture name ty body)
        _ -> Nothing

    alignTermAlongType :: ElabType -> ElabTerm -> ElabTerm
    alignTermAlongType targetTy term =
      case (targetTy, term) of
        (TForall targetName _mbBound targetBody, ETyAbs termName termBound body)
          | targetName == termName ->
              ETyAbs termName termBound (alignTermAlongType targetBody body)
        (TForall targetName mbBound targetBody, _) ->
          ETyAbs targetName mbBound (alignTermAlongType targetBody term)
        (TArrow dom cod, ELam name _ body) ->
          ELam name dom (alignTermAlongType cod body)
        _ -> term

closeAnnotatedLambdaParam :: TypeCheck.Env -> ElabType -> ElabTerm -> Maybe ElabTerm
closeAnnotatedLambdaParam tcEnv annotationTy term =
  case (annotationTy, term) of
    (TArrow dom _, ELam name (TVar binder) body) -> do
      bound <- either (const Nothing) Just (elabToBound dom)
      let closed = ETyAbs binder (Just bound) (ELam name (TVar binder) body)
      case TypeCheck.typeCheckWithEnv tcEnv closed of
        Right _ -> Just closed
        Left _ -> Nothing
    _ -> Nothing

reifyInst ::
  AnnotationContext ->
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
              ++ show (fmap siSubst mSchemeInfo)
          )
          () of
          () -> pure ()
        phi0 <-
          case
            phiFromEdgeWitnessWithTrace
              traceCfg
              generalizeAtWith
              presolutionView
              (Just gaParents)
              mSchemeInfo
              mTrace
              edgeWitness
            of
            Right phi0' -> pure phi0'
            Left err -> Left err
        let substForPhi = maybe IntMap.empty siSubst mSchemeInfo
            resolvePhiVar v = do
              nid <- parseNameId v
              bnd <- ChiQuery.chiLookupVarBound presolutionView (canonical (NodeId nid))
              either
                (const Nothing)
                Just
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
                            TForall {} ->
                              case applyInstantiation cur (InstApp tyArg) of
                                Right cur' -> go cur' (acc ++ [tyArg]) rest
                                Left _ -> acc
                            _ -> acc
                     in go schemeTy [] tys
                  longestValidExpansionInst nodeArgs =
                    let tryPrefixes n best
                          | n <= 0 = best
                          | otherwise =
                              case expInstantiateArgsToInstNoFallback presolutionView namedSetReify (take n nodeArgs) of
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
                      let usable = longestValidAppPrefix (map (inlineBoundVarsType presolutionView) (take (min schemeArity (length inferred)) inferred))
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
    canonical = ChiQuery.chiCanonical presolutionView
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
                            Right (scheme, subst) -> Just SchemeInfo {siScheme = scheme, siSubst = subst}
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
      ty <- srcTypeToElabType srcTy
      pure SchemeInfo {siScheme = schemeFromType ty, siSubst = IntMap.empty}

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
                                    | targetHasVisibleForall,
                                      isJust (parseNameId v) ->
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
    AUnfold inner _ _ -> annRefersToVar name inner
    _ -> False

freshenTermTypeAbsAgainstEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
freshenTermTypeAbsAgainstEnv env = go reserved
  where
    reserved =
      Set.unions
        ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv env))
            ++ [Set.fromList (Map.keys (TypeCheck.typeEnv env))]
        )

    go used term = case term of
      ETyAbs name mb body ->
        let usedForBinder = Set.union used (maybe Set.empty freeTypeVarsType mb)
            (name', body') =
              if Set.member name usedForBinder
                then
                  let fresh = freshNameLike name usedForBinder
                   in (fresh, renameTypeVarInTerm name fresh body)
                else (name, body)
            used' = Set.insert name' usedForBinder
         in ETyAbs name' mb (go used' body')
      ELam v ty body -> ELam v ty (go (Set.union used (freeTypeVarsType ty)) body)
      EApp f a -> EApp (go used f) (go used a)
      ELet v sch rhs body ->
        let used' = Set.union used (freeTypeVarsType (schemeToType sch))
         in ELet v sch (go used' rhs) (go used' body)
      ETyInst t inst -> ETyInst (go used t) inst
      ERoll ty body -> ERoll ty (go used body)
      EUnroll body -> EUnroll (go used body)
      _ -> term

renameTypeVarInTerm :: String -> String -> ElabTerm -> ElabTerm
renameTypeVarInTerm old new term =
  let ty' = TVar new
      renameTy = substTypeCapture old ty'
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameName v
        | v == old = new
        | otherwise = v
      renameInst inst = case inst of
        InstId -> InstId
        InstApp ty -> InstApp (renameTy ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstInside inner -> InstInside (renameInst inner)
        InstSeq a b -> InstSeq (renameInst a) (renameInst b)
        InstUnder v inner -> InstUnder (renameName v) (renameInst inner)
        InstBot ty -> InstBot (renameTy ty)
        InstAbstr v -> InstAbstr (renameName v)
   in case term of
        EVar _ -> term
        ELit _ -> term
        ELam v ty body -> ELam v (renameTy ty) (renameTypeVarInTerm old new body)
        EApp f a -> EApp (renameTypeVarInTerm old new f) (renameTypeVarInTerm old new a)
        ELet v sch rhs body -> ELet v (renameScheme sch) (renameTypeVarInTerm old new rhs) (renameTypeVarInTerm old new body)
        ETyAbs v mb body
          | v == old -> ETyAbs v (fmap renameBound mb) body
          | otherwise -> ETyAbs v (fmap renameBound mb) (renameTypeVarInTerm old new body)
        ETyInst t inst -> ETyInst (renameTypeVarInTerm old new t) (renameInst inst)
        ERoll ty body -> ERoll (renameTy ty) (renameTypeVarInTerm old new body)
        EUnroll body -> EUnroll (renameTypeVarInTerm old new body)

srcTypeToElabType :: NormSrcType -> Either ElabError ElabType
srcTypeToElabType ty = case ty of
  STVar name -> Right (TVar name)
  STArrow dom cod -> TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod
  STCon name args -> TCon (BaseTy name) <$> traverse srcTypeToElabType args
  STVarApp name _ ->
    Left (unsupportedVariableHeadType name)
  STForall name mb body ->
    TForall name
      <$> maybe (Right Nothing) srcBoundToElabBound mb
      <*> srcTypeToElabType body
  STMu name body -> TMu name <$> srcTypeToElabType body
  STBase name -> Right (TBase (BaseTy name))
  STBottom -> Right TBottom

unsupportedVariableHeadType :: String -> ElabError
unsupportedVariableHeadType name =
  InstantiationError
    ("variable-headed source type application `" ++ name ++ "` is not supported before higher-kinded elaboration")

srcBoundToElabBound :: SrcBound 'NormN -> Either ElabError (Maybe BoundType)
srcBoundToElabBound bound = case bound of
  SrcBound ty -> structBoundToElabBound ty

structBoundToElabBound :: StructBound -> Either ElabError (Maybe BoundType)
structBoundToElabBound bTy = case bTy of
  STArrow dom cod -> Just <$> (TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod)
  STBase name -> Right (Just (TBase (BaseTy name)))
  STCon name args -> Just . TCon (BaseTy name) <$> traverse srcTypeToElabType args
  STVarApp name _ ->
    Left (unsupportedVariableHeadType name)
  STForall name mb body ->
    Just
      <$> ( TForall name
              <$> maybe (Right Nothing) srcBoundToElabBound mb
              <*> srcTypeToElabType body
          )
  STMu name body -> Just . TMu name <$> srcTypeToElabType body
  STBottom -> Right Nothing
