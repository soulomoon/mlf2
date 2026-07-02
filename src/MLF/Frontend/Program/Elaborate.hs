{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeInstances,
    elaborateScopeValues,
    elaborateScopeRuntimeTypeViews,
    elaborateScopeRuntimeTypes,
    elaborateScopeUniqueDataTypes,
    mkElaborateScope,
    lowerTypeView,
    lowerConstructorBinding,
    constructorBindingSourceTypeView,
    constructorTypeView,
    lowerConstrainedResolvedExprBinding,
    lowerResolvedConstrainedExprBinding,
    lowerExprBinding,
    classInfoForConstraint,
    diagnosticTypeViewDisplay,
    lowerType,
    sourceTypeIdentityInScope,
    sourceTypeBinderIdentitiesInScope,
    sourceTypeViewInScope,
    matchTypesInScope,
    matchTypeViewsAgainstIdentity,
    matchMethodTypeViews,
    resolveInstanceInfoWithIdentityType,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    resolveMethodInstanceInfoByTypeViews,
    zeroMethodConstraintCoveredByEvidenceInfo,
    lookupEvidenceMethodByClass,
    lookupEvidenceMethodByClassTypes,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>), filterM, foldM, replicateM, when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.List (partition, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Frontend.Normalize (substSrcType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Surface
  ( surfaceAnn,
    surfaceApp,
    surfaceLam,
    surfaceLamAnn,
    surfaceLet,
    surfaceLit,
    surfaceVar,
  )
import MLF.Frontend.Program.Types
import MLF.Frontend.Symbol (symbolIdentityAliasNames, symbolIdentityStableName)
import MLF.Frontend.Syntax
  ( Lit (..),
    ResolvedSrcBound (..),
    ResolvedSrcTy (..),
    ResolvedSrcType,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    SurfaceExpr,
    resolvedSrcTypeBinderName,
  )
import MLF.Frontend.Syntax.Program (ExprF (..))
import qualified MLF.Frontend.Syntax.Program as P
import MLF.Reify.TypeOps (freshNameLike)
import MLF.Types.Identity
  ( IdentityGenerator,
    StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    UniqueIdentity (..),
    freshDeferredRef,
    freshIdentity,
    freshLocalRef,
    identityGeneratorAfter,
    initialIdentityGenerator,
    localIdentityStableUnique,
    typeBinderIdentityFromStructural,
  )

data ElaborateScope = ElaborateScope
  { esValues :: Map String ValueInfo,
    esLocalValues :: Map LocalRef ValueInfo,
    esValuesByIdentity :: Map SymbolIdentity ValueInfo,
    esRuntimeTypeViews :: Map String TypeView,
    esTypes :: Map String DataInfo,
    esTypesByIdentity :: Map SymbolIdentity DataInfo,
    esTypeHeadIdentities :: Map String SymbolIdentity,
    esTypeDisplayNamesByIdentity :: Map SymbolIdentity [String],
    esClasses :: Map String ClassInfo,
    esClassesByIdentity :: Map SymbolIdentity ClassInfo,
    esClassDisplayNamesByIdentity :: Map SymbolIdentity [String],
    esEvidence :: [EvidenceInfo],
    esInstances :: [InstanceInfo]
  }

elaborateScopeValues :: ElaborateScope -> Map String ValueInfo
elaborateScopeValues = esValues

data ElaborateState = ElaborateState
  { elaborateNameGenerator :: IdentityGenerator,
    elaborateIdentityGenerator :: IdentityGenerator,
    elaborateDeferredObligations :: DeferredObligations,
    elaborateExternalTypeViews :: Map String TypeView,
    elaborateResolvedLocalIdentities :: [LoweredResolvedLocalIdentity]
  }

type ElaborateM a = ExceptT ProgramError (State ElaborateState) a

data ElaborateResult a = ElaborateResult
  { elaborateResultValue :: a,
    elaborateResultDeferredObligations :: DeferredObligations,
    elaborateResultExternalTypeViews :: Map String TypeView,
    elaborateResultResolvedLocalIdentities :: [LoweredResolvedLocalIdentity]
  }

type ClassIdentity = SymbolIdentity

runElaborateM :: ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateM =
  runElaborateMWithSeed []

runElaborateMWithSeed :: [UniqueIdentity] -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMWithSeed seedIdentities action =
  let initialState =
        ElaborateState
          { elaborateNameGenerator = initialIdentityGenerator,
            elaborateIdentityGenerator = identityGeneratorAfter seedIdentities,
            elaborateDeferredObligations = Map.empty,
            elaborateExternalTypeViews = Map.empty,
            elaborateResolvedLocalIdentities = []
          }
      (result, finalState) = runState (runExceptT action) initialState
   in case result of
        Left err -> Left err
        Right value ->
          Right
            ElaborateResult
              { elaborateResultValue = value,
                elaborateResultDeferredObligations = elaborateDeferredObligations finalState,
                elaborateResultExternalTypeViews = elaborateExternalTypeViews finalState,
                elaborateResultResolvedLocalIdentities = elaborateResolvedLocalIdentities finalState
              }

mkElaborateScope :: Map String ValueInfo -> Map String DataInfo -> Map String ClassInfo -> [InstanceInfo] -> ElaborateScope
mkElaborateScope values0 dataTypes0 classes0 instances0 =
  let values1 = values0 `Map.union` instanceRuntimeValues
   in ElaborateScope
        { esValues = values1,
          esLocalValues = Map.empty,
          esValuesByIdentity = indexInfoListByIdentity valueInfoSymbolIdentity valueIdentityInfos,
          esRuntimeTypeViews = runtimeTypeViews,
          esTypes = dataTypes,
          esTypesByIdentity = dataTypesByIdentity,
          esTypeHeadIdentities = dataTypeHeadIdentities,
          esTypeDisplayNamesByIdentity = dataTypeDisplayNamesByIdentity,
          esClasses = classes0,
          esClassesByIdentity = classesByIdentity,
          esClassDisplayNamesByIdentity = classDisplayNamesByIdentity,
          esEvidence = [],
          esInstances = instances0
        }
  where
    dataTypes =
      addIdentityTypeAliases dataTypes0

    dataTypesByIdentity =
      indexInfoByIdentity dataInfoSymbolIdentity dataTypes0

    dataTypeDisplayNamesByIdentity =
      indexDisplayNamesByIdentity dataInfoSymbolIdentity dataTypes0

    dataTypeHeadIdentities =
      dataTypeHeadIdentityAliases dataTypesByIdentity dataTypeDisplayNamesByIdentity

    classesByIdentity =
      indexInfoByIdentity classInfoSymbolIdentity classes0

    classDisplayNamesByIdentity =
      indexDisplayNamesByIdentity classInfoSymbolIdentity classes0

    shouldTrackRuntimeType ConstructorValue {valueCtorInfo = ctorInfo} =
      constructorOwnerRuntimeTypeTrackable dataTypesByIdentity ctorInfo
    shouldTrackRuntimeType OverloadedMethod {} = False
    shouldTrackRuntimeType _ = True

    runtimeTypeInfos =
      [ (runtimeNameFor info, info)
      | info <- Map.elems values0,
        shouldTrackRuntimeType info
      ]
        ++ [ (runtimeNameFor methodInfo, methodInfo)
           | methodInfo <- instanceMethodValues,
             shouldTrackRuntimeType methodInfo
           ]

    runtimeTypeViews =
      Map.fromList
        [ (runtimeName, valueRuntimeTypeViewFor info)
        | (runtimeName, infos) <- Map.toList runtimeTypeInfosByName,
          Just info <- [uniqueRuntimeTypeInfo infos]
        ]

    runtimeTypeInfosByName =
      Map.fromListWith
        (++)
        [ (runtimeName, [info])
        | (runtimeName, info) <- runtimeTypeInfos
        ]

    uniqueRuntimeTypeInfo infos =
      case (Set.toList (Set.fromList (map valueInfoSymbolIdentity infos)), infos) of
        ([_], info : rest)
          | all (== info) rest -> Just info
        _ -> Nothing

    instanceMethodValueIdentities =
      Set.fromList
        [ valueInfoSymbolIdentity methodInfo
        | methodInfo <- instanceMethodValues
        ]

    runtimeNameFor OrdinaryValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor ConstructorValue {valueRuntimeName = runtimeName} = runtimeName
    runtimeNameFor OverloadedMethod {} = error "overloaded methods do not have runtime names"

    valueRuntimeTypeViewFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints}
      | null constraints,
        not (Set.member (valueInfoSymbolIdentity valueInfo) instanceMethodValueIdentities) =
          loweredRuntimeTypeViewFor valueInfo
      | otherwise =
          constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueRuntimeTypeViewFor valueInfo =
      loweredRuntimeTypeViewFor valueInfo

    loweredRuntimeTypeViewFor valueInfo =
      TypeView
        { typeViewDisplay = lowerTypeRaw dataTypes (valueTypeFor valueInfo),
          typeViewIdentity = lowerTypeRaw dataTypes (valueTypeFor valueInfo),
          typeViewHeadIdentities = valueTypeHeadIdentitiesFor valueInfo,
          typeViewBinderIdentities = valueTypeBinderIdentitiesFor valueInfo
        }

    valueTypeFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      lowerTypeViewRaw dataTypes (constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo))
    valueTypeFor valueInfo@ConstructorValue {valueCtorInfo = ctorInfo} =
      let quantifiedTy = quantifyFreeTypeVars ty
          quantifiedIdentityTy = quantifyFreeTypeVars identityTy
          loweredTy = lowerTypeViewRaw dataTypes (mkTypeView quantifiedTy quantifiedIdentityTy)
       in if constructorOwnerHasVariableHeadApplication dataTypesByIdentity ctorInfo
            && srcTypeHasVariableHeadApplication loweredTy
            then constructorStructuralPlaceholderTypeFor dataTypesByIdentity ctorInfo
            else quantifiedTy
      where
        ty = valueType valueInfo
        identityTy = valueIdentityType valueInfo
    valueTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

    valueTypeHeadIdentitiesFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      typeViewHeadIdentities $
        constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueTypeHeadIdentitiesFor ConstructorValue {valueCtorInfo = ctorInfo} =
      typeViewHeadIdentities (ctorTypeView ctorInfo)
    valueTypeHeadIdentitiesFor OverloadedMethod {} = Map.empty

    valueTypeBinderIdentitiesFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      typeViewBinderIdentities $
        constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueTypeBinderIdentitiesFor ConstructorValue {valueCtorInfo = ctorInfo} =
      constructorTypeBinderIdentitiesFor ctorInfo
    valueTypeBinderIdentitiesFor OverloadedMethod {} = Map.empty

    constructorTypeBinderIdentitiesFor ctorInfo =
      mergeTypeBinderIdentityMaps
        [ typeViewBinderIdentities (ctorTypeView ctorInfo),
          ownerParamIdentities,
          forallIdentities
        ]
      where
        ownerParamIdentities =
          case Map.lookup (ctorOwningTypeIdentity ctorInfo) dataTypesByIdentity of
            Just dataInfo ->
              typeBinderAliasIdentityMap (dataParamBinders dataInfo)
            Nothing -> Map.empty

        forallIdentities =
          typeBinderAliasIdentityMap
            [ (constructorForallDisplayName binder, constructorForallIdentity binder)
            | binder <- ctorForallBinderInfo ctorInfo
            ]

    instanceRuntimeValues =
      Map.fromList
        [ (runtimeName, methodValue)
          | methodValue@OrdinaryValue {valueRuntimeName = runtimeName} <- instanceMethodValues
        ]

    valueIdentityInfos =
      Map.elems values0 ++ instanceMethodValues

    instanceMethodValues =
      [ methodValue
        | instanceInfo <- instances0,
          methodValue <- Map.elems (instanceMethodsByIdentity instanceInfo)
      ]

addIdentityTypeAliases :: Map String DataInfo -> Map String DataInfo
addIdentityTypeAliases dataTypes =
  foldl insertAlias dataTypes (Map.toList aliases)
  where
    aliases =
      Map.fromListWith
        (++)
        [ (name, [info])
        | info <- Map.elems dataTypes,
          name <- symbolIdentityAliasNames (dataInfoSymbolIdentity info)
        ]

    insertAlias acc (name, [info]) =
      case Map.lookup name acc of
        Just existing
          | dataInfoSymbolIdentity existing == dataInfoSymbolIdentity info -> acc
          | otherwise -> acc
        Nothing -> Map.insert name info acc
    insertAlias acc _ = acc

indexInfoByIdentity :: (Eq a) => (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity a
indexInfoByIdentity identityOf =
  uniqueInfoByIdentity identityOf

indexInfoListByIdentity :: (Eq a) => (a -> SymbolIdentity) -> [a] -> Map SymbolIdentity a
indexInfoListByIdentity identityOf =
  uniqueInfoListByIdentity identityOf

indexDisplayNamesByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity [String]
indexDisplayNamesByIdentity identityOf =
  Map.fromListWith (++) . map (\(name, info) -> (identityOf info, [name])) . Map.toList

dataTypeHeadIdentityAliases :: Map SymbolIdentity DataInfo -> Map SymbolIdentity [String] -> Map String SymbolIdentity
dataTypeHeadIdentityAliases dataTypesByIdentity displayNamesByIdentity =
  mergeSymbolIdentityMaps
    [ Map.singleton name identity
    | identity <- Map.keys dataTypesByIdentity,
      name <- symbolIdentityAliasNames identity ++ Map.findWithDefault [] identity displayNamesByIdentity
    ]

elaborateScopeRuntimeTypes :: ElaborateScope -> Map String SrcType
elaborateScopeRuntimeTypes =
  Map.map typeViewDisplay . esRuntimeTypeViews

elaborateScopeRuntimeTypeViews :: ElaborateScope -> Map String TypeView
elaborateScopeRuntimeTypeViews = esRuntimeTypeViews

elaborateScopeDataTypes :: ElaborateScope -> Map String DataInfo
elaborateScopeDataTypes = esTypes

elaborateScopeDataTypesByIdentity :: ElaborateScope -> Map SymbolIdentity DataInfo
elaborateScopeDataTypesByIdentity = esTypesByIdentity

elaborateScopeUniqueDataTypes :: ElaborateScope -> [DataInfo]
elaborateScopeUniqueDataTypes scope =
  [ info
    | info <- Map.elems (esTypesByIdentity scope)
  ]

elaborateScopeInstances :: ElaborateScope -> [InstanceInfo]
elaborateScopeInstances = esInstances

lowerType :: ElaborateScope -> SrcType -> SrcType
lowerType scope = lowerTypeRaw (esTypes scope)

lowerTypeView :: ElaborateScope -> TypeView -> SrcType
lowerTypeView scope = lowerTypeViewRaw (esTypes scope)

lowerTypeViewRaw :: Map String DataInfo -> TypeView -> SrcType
lowerTypeViewRaw dataTypes view =
  lowerTypeRaw dataTypes (visibleTypeForIdentity dataTypes (typeViewDisplay view) (typeViewIdentity view))

diagnosticTypeViewDisplay :: ElaborateScope -> TypeView -> SrcType
diagnosticTypeViewDisplay scope view =
  go (typeViewDisplay view) (typeViewIdentity view)
  where
    go display identityTy =
      case (display, identityTy) of
        (STBase displayName, STBase identityName) -> STBase (diagnosticHeadName displayName identityName)
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          STCon
            (diagnosticHeadName displayName identityName)
            (zipWithNE go displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs)
          | displayName == identityName ->
              STVarApp displayName (zipWithNE go displayArgs identityArgs)
        (STTyLam displayName displayBody, STTyLam _ identityBody) ->
          STTyLam displayName (go displayBody identityBody)
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          STTyApp (go displayFun identityFun) (go displayArg identityArg)
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          STArrow (go displayDom identityDom) (go displayCod identityCod)
        (STForall name displayBound displayBody, STForall _ identityBound identityBody) ->
          STForall
            name
            (zipBound displayBound identityBound)
            (go displayBody identityBody)
        (STMu name displayBody, STMu _ identityBody) -> STMu name (go displayBody identityBody)
        _ -> display

    diagnosticHeadName displayName identityName
      | displayName /= unqualifiedSymbolName displayName,
        Just displayIdentity <- typeHeadIdentityInScope scope displayName,
        Just identity <- typeHeadIdentityInScope scope identityName,
        displayIdentity == identity =
          displayName
      | let builtinName = Builtins.normalizeBuiltinTypeReference identityName,
        Builtins.isBuiltinTypeName builtinName =
          builtinName
      | qualifiedName : _ <- qualifiedNamesForIdentity identityName =
          qualifiedName
      | otherwise = identityName

    qualifiedNamesForIdentity identityName =
      [ dataInfoIdentityQualifiedName info
      | info <- elaborateScopeUniqueDataTypes scope,
        dataIdentityTypeName info == identityName
      ]

    zipBound (Just (SrcBound displayBound)) (Just (SrcBound identityBound)) =
      Just (SrcBound (go displayBound identityBound))
    zipBound displayBound _ = displayBound

    zipWithNE f (displayHead :| displayTail) (identityHead :| identityTail) =
      f displayHead identityHead :| zipWith f displayTail identityTail

visibleTypeForIdentity :: Map String DataInfo -> SrcType -> SrcType -> SrcType
visibleTypeForIdentity dataTypes = go
  where
    go display identityTy =
      case (display, identityTy) of
        (STBase displayName, STBase identityName) -> STBase (visibleHeadName identityName displayName)
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          STCon
            (visibleHeadName identityName displayName)
            (zipWithNE go displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs)
          | displayName == identityName ->
              STVarApp displayName (zipWithNE go displayArgs identityArgs)
        (STTyLam displayName displayBody, STTyLam _ identityBody) ->
          STTyLam displayName (go displayBody identityBody)
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          STTyApp (go displayFun identityFun) (go displayArg identityArg)
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          STArrow (go displayDom identityDom) (go displayCod identityCod)
        (STForall name displayBound displayBody, STForall _ identityBound identityBody) ->
          STForall
            name
            (zipBound displayBound identityBound)
            (go displayBody identityBody)
        (STMu name displayBody, STMu _ identityBody) -> STMu name (go displayBody identityBody)
        _ -> display

    visibleHeadName identityName displayName =
      case
        [ visibleName
          | (visibleName, info) <- Map.toList dataTypes,
            dataIdentityTypeName info == identityName,
            not (dataInfoIdentityAliasName visibleName info)
        ]
      of
        visibleName : _ -> visibleName
        [] -> displayName

    zipBound (Just (SrcBound displayBound)) (Just (SrcBound identityBound)) =
      Just (SrcBound (go displayBound identityBound))
    zipBound displayBound _ = displayBound

    zipWithNE f (displayHead :| displayTail) (identityHead :| identityTail) =
      f displayHead identityHead :| zipWith f displayTail identityTail

sourceTypeViewInScope :: ElaborateScope -> SrcType -> TypeView
sourceTypeViewInScope scope ty =
  TypeView
    { typeViewDisplay = preferVisibleSourceType scope ty,
      typeViewIdentity = sourceTypeIdentityInScope scope ty,
      typeViewHeadIdentities = sourceTypeHeadIdentitiesInScope scope ty,
      typeViewBinderIdentities = sourceTypeBinderIdentitiesInScope scope ty
    }

sourceTypeBinderIdentitiesInScope :: ElaborateScope -> SrcTy n v -> Map String TypeBinderIdentity
sourceTypeBinderIdentitiesInScope scope ty =
  sourceTypeStructuralBinderIdentities scope ty

sourceTypeBinderNames :: SrcTy n v -> Set String
sourceTypeBinderNames =
  \case
    STVar name -> Set.singleton name
    STArrow dom cod -> sourceTypeBinderNames dom `Set.union` sourceTypeBinderNames cod
    STBase {} -> Set.empty
    STCon _ args -> foldMap sourceTypeBinderNames args
    STVarApp name args -> Set.insert name (foldMap sourceTypeBinderNames args)
    STTyLam name body -> Set.insert name (sourceTypeBinderNames body)
    STTyApp fun arg -> sourceTypeBinderNames fun `Set.union` sourceTypeBinderNames arg
    STForall name mb body ->
      Set.insert name $
        maybe Set.empty (sourceTypeBinderNames . unSrcBound) mb `Set.union` sourceTypeBinderNames body
    STMu name body -> Set.insert name (sourceTypeBinderNames body)
    STBottom -> Set.empty

sourceTypeStructuralBinderIdentities :: ElaborateScope -> SrcTy n v -> Map String TypeBinderIdentity
sourceTypeStructuralBinderIdentities scope ty =
  mergeTypeBinderIdentityMaps
    [ Map.singleton name identity
    | name <- Set.toList (sourceTypeBinderNames ty),
      Just identity <- [Map.lookup name structuralIdentities]
    ]
  where
    structuralIdentities =
      mergeTypeBinderIdentityMaps (map structuralDataBinderIdentities (elaborateScopeUniqueDataTypes scope))

    structuralDataBinderIdentities info =
      typeBinderAliasIdentityMap
        [ (name, dataStructuralSelfBinderIdentity info)
          | name <- dataStructuralSelfBinderNames info
        ]
        <> typeBinderAliasIdentityMap
          [ (name, dataStructuralResultBinderIdentity info)
            | name <- dataStructuralResultBinderNames info
          ]

dataStructuralSelfBinderNames :: DataInfo -> [String]
dataStructuralSelfBinderNames info =
  map (\name -> "$" ++ name ++ "_self") (dataStructuralBinderHeadNames info)

dataStructuralResultBinderNames :: DataInfo -> [String]
dataStructuralResultBinderNames info =
  map (\name -> "$" ++ name ++ "_result") (dataStructuralBinderHeadNames info)

dataStructuralBinderHeadNames :: DataInfo -> [String]
dataStructuralBinderHeadNames info =
  symbolIdentityAliasNames (dataInfoSymbolIdentity info)

dataStructuralSelfBinderIdentity :: DataInfo -> TypeBinderIdentity
dataStructuralSelfBinderIdentity info =
  typeBinderIdentityFromStructural (symbolUniqueIdentity (dataInfoSymbolIdentity info)) StructuralSelfBinder

dataStructuralResultBinderIdentity :: DataInfo -> TypeBinderIdentity
dataStructuralResultBinderIdentity info =
  typeBinderIdentityFromStructural (symbolUniqueIdentity (dataInfoSymbolIdentity info)) StructuralResultBinder

sourceTypeViewSubstForTemplateInScope :: ElaborateScope -> TypeView -> Map String SrcType -> TypeViewSubst
sourceTypeViewSubstForTemplateInScope scope template matched =
  Map.fromList
    [ (key, sourceTypeViewInScope scope ty)
    | (name, ty) <- Map.toList matched,
      Just key <- [templateBinderKey name]
    ]
  where
    displayNamesByIdentityName =
      typeViewVarPairs template

    identitiesByDisplayName =
      mergeTypeBinderIdentityMaps
        [ Map.singleton displayName identity
        | (identityName, displayName) <- Map.toList displayNamesByIdentityName
        , Just identity <- [typeViewBinderIdentityForAlias template identityName]
        ]

    templateBinderKey name =
      case typeViewBinderIdentityForAlias template name of
        Just identity -> Just (typeViewSubstKeyForIdentity identity)
        Nothing ->
          case Map.lookup name identitiesByDisplayName of
            Just identity -> Just (typeViewSubstKeyForIdentity identity)
            Nothing
              | Map.null (typeViewBinderIdentities template) ->
                  typeViewSubstKeyForTemplateName template name
              | otherwise -> Nothing

typeViewSubstKeyForTemplateName :: TypeView -> String -> Maybe TypeViewSubstKey
typeViewSubstKeyForTemplateName template identityName =
  case typeViewBinderIdentityForAlias template identityName of
    Just identity -> Just (typeViewSubstKeyForIdentity identity)
    Nothing -> typeViewSubstKeyFor template identityName

canonicalSourceType :: ElaborateScope -> SrcType -> SrcType
canonicalSourceType = sourceTypeIdentityInScope

sourceTypeIdentityInScope :: ElaborateScope -> SrcType -> SrcType
sourceTypeIdentityInScope scope = canonical
  where
    canonical ty =
      case ty of
        STVar {} -> ty
        STBase name ->
          case sourceTypeHeadStableName name of
            Just identityName -> STBase identityName
            Nothing -> ty
        STCon name args ->
          let args' = fmap canonical args
           in case sourceTypeHeadStableName name of
                Just identityName -> STCon identityName args'
                Nothing -> STCon name args'
        STVarApp name args -> STVarApp name (fmap canonical args)
        STTyLam name body -> STTyLam name (canonical body)
        STTyApp fun arg -> STTyApp (canonical fun) (canonical arg)
        STArrow dom cod -> STArrow (canonical dom) (canonical cod)
        STForall name mb body ->
          STForall name (fmap (SrcBound . canonical . unSrcBound) mb) (canonical body)
        STMu name body -> STMu name (canonical body)
        STBottom -> STBottom

    sourceTypeHeadStableName name =
      symbolIdentityStableName <$> typeHeadIdentityInScope scope name

sourceTypeHeadIdentitiesInScope :: ElaborateScope -> SrcType -> Map String SymbolIdentity
sourceTypeHeadIdentitiesInScope scope ty =
  mergeSymbolIdentityMaps
    [ Map.singleton name identity
    | (identity, names) <- entries,
      name <- names,
      not (null name)
    ]
  where
    entries = sourceTypeHeadIdentityEntriesInScope scope ty

sourceTypeHeadIdentityEntriesInScope :: ElaborateScope -> SrcType -> [(SymbolIdentity, [String])]
sourceTypeHeadIdentityEntriesInScope scope =
  go
  where
    go =
      \case
        STVar {} -> []
        STArrow dom cod -> go dom ++ go cod
        STBase name -> headIdentity name
        STCon name args -> headIdentity name ++ foldMap go args
        STVarApp _ args -> foldMap go args
        STTyLam _ body -> go body
        STTyApp fun arg -> go fun ++ go arg
        STForall _ mb body -> maybe [] (go . unSrcBound) mb ++ go body
        STMu _ body -> go body
        STBottom -> []

    headIdentity name =
      case typeHeadIdentityInScope scope name of
        Just identity ->
          case Map.lookup identity (esTypesByIdentity scope) of
            Just info -> dataHeadIdentityEntries name info
            Nothing -> builtinHeadIdentityEntries name identity
        Nothing -> []

    dataHeadIdentityEntries name info =
      [(dataInfoSymbol info, name : symbolIdentityAliasNames (dataInfoSymbol info))]

    builtinHeadIdentityEntries name identity =
      [(identity, name : Builtins.normalizeBuiltinTypeReference name : symbolIdentityAliasNames identity)]

constrainedRuntimeTypeInfoViewRaw :: Map String DataInfo -> Map SymbolIdentity ClassInfo -> [ConstraintInfo] -> TypeView -> TypeView
constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints visibleView =
  TypeView
    { typeViewDisplay = foldForalls displayForalls (foldr STArrow displayBody evidenceDisplays),
      typeViewIdentity = foldForalls identityForalls (foldr STArrow identityBody evidenceIdentities),
      typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          (typeViewHeadIdentities visibleView : map typeViewHeadIdentities evidenceViews),
      typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          (typeViewBinderIdentities visibleView : map typeViewBinderIdentities evidenceViews)
    }
  where
    (displayForalls, displayBody) = splitForalls (typeViewDisplay visibleView)
    (identityForalls, identityBody) = splitForalls (typeViewIdentity visibleView)
    evidenceViews = concatMap constraintEvidenceTypes constraints
    evidenceDisplays = map typeViewDisplay evidenceViews
    evidenceIdentities = map typeViewIdentity evidenceViews

    constraintEvidenceTypes constraint =
      [ lowerEvidenceView (methodEvidenceSourceTypeInfoViewRaw dataTypes classesByIdentity classInfo (constraintTypeViews evidenceConstraint) methodInfo)
        | (classInfo, evidenceConstraint) <- constraintEvidenceClosureInfoRaw classesByIdentity constraint,
          methodInfo <- Map.elems (classMethodsByIdentity classInfo)
      ]

    lowerEvidenceView = id

    foldForalls foralls bodyTy =
      foldr (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc) bodyTy foralls

constraintEvidenceClosureInfoRaw :: Map SymbolIdentity ClassInfo -> ConstraintInfo -> [(ClassInfo, ConstraintInfo)]
constraintEvidenceClosureInfoRaw classesByIdentity =
  go Set.empty
  where
    go seen constraint =
      case Map.lookup (constraintClassSymbol constraint) classesByIdentity of
        Just classInfo ->
          let key = classConstraintEvidenceKeyInfo classInfo constraint
           in if key `Set.member` seen
                then []
                else
                  let seen' = Set.insert key seen
                      superclasses =
                        map
                          (applyConstraintInfoSubst (superclassSubst classInfo constraint))
                          (classSuperclassInfos classInfo)
                   in (classInfo, constraint) : concatMap (go seen') superclasses
        _ -> []

    superclassSubst classInfo constraint =
      typeViewSubstFromParamIdentities
        (classParamBinderIdentities classInfo)
        (constraintTypeViews constraint)

methodEvidenceSourceTypeInfoViewRaw :: Map String DataInfo -> Map SymbolIdentity ClassInfo -> ClassInfo -> NonEmpty TypeView -> MethodInfo -> TypeView
methodEvidenceSourceTypeInfoViewRaw dataTypes classesByIdentity classInfo classArgViews methodInfo =
  let specializedMethodView =
        specializeMethodTypeView methodInfo classArgViews
      specializedConstraints =
        map
          (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) classArgViews))
          (methodConstraintInfos methodInfo)
      headVars = freeTypeBinderIdentitiesTypeViewsFailClosed classArgViews
      (evidenceVisibleView, specializedConstraints') =
        quantifyMethodLocalVarsInfoView headVars specializedConstraints specializedMethodView
      deferredConstraints =
        filter (not . constraintInfoDeterminedByTypeBinderIdentitiesFailClosed headVars) specializedConstraints'
   in constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity deferredConstraints evidenceVisibleView

freeTypeBinderIdentitiesTypeViewsFailClosed :: NonEmpty TypeView -> Set TypeBinderIdentity
freeTypeBinderIdentitiesTypeViewsFailClosed views =
  case freeTypeBinderIdentitiesTypeViews views of
    Right identities -> identities
    Left _ -> Set.empty

constraintInfoDeterminedByTypeBinderIdentitiesFailClosed :: Set TypeBinderIdentity -> ConstraintInfo -> Bool
constraintInfoDeterminedByTypeBinderIdentitiesFailClosed typeVars constraint =
  case freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint) of
    Right identities -> identities `Set.isSubsetOf` typeVars
    Left _ -> False

constraintInfoGroundByTypeBinderIdentitiesFailClosed :: ConstraintInfo -> Bool
constraintInfoGroundByTypeBinderIdentitiesFailClosed constraint =
  constraintInfoDeterminedByTypeBinderIdentitiesFailClosed Set.empty constraint

constraintInfoDeterminedByTypeBinderIdentities :: Set TypeBinderIdentity -> ConstraintInfo -> ElaborateM Bool
constraintInfoDeterminedByTypeBinderIdentities typeVars constraint =
  (`Set.isSubsetOf` typeVars) <$> freeTypeBinderIdentitiesTypeViewsOrThrow (constraintTypeViews constraint)

constraintInfoGroundByTypeBinderIdentities :: ConstraintInfo -> ElaborateM Bool
constraintInfoGroundByTypeBinderIdentities constraint =
  Set.null <$> freeTypeBinderIdentitiesTypeViewsOrThrow (constraintTypeViews constraint)

constraintInfoHasFreeTypeBinderIdentities :: ConstraintInfo -> ElaborateM Bool
constraintInfoHasFreeTypeBinderIdentities constraint =
  not . Set.null <$> freeTypeBinderIdentitiesTypeViewsOrThrow (constraintTypeViews constraint)

freeTypeBinderIdentitiesTypeViewsOrThrow :: NonEmpty TypeView -> ElaborateM (Set TypeBinderIdentity)
freeTypeBinderIdentitiesTypeViewsOrThrow views =
  case freeTypeBinderIdentitiesTypeViews views of
    Right identities -> pure identities
    Left name ->
      throwError $
        ProgramPipelineError
          ("elaborate resolved type variable `" ++ name ++ "` is missing binder identity")

quantifyMethodLocalVarsInfoView :: Set TypeBinderIdentity -> [ConstraintInfo] -> TypeView -> (TypeView, [ConstraintInfo])
quantifyMethodLocalVarsInfoView headVars constraints view =
  ( TypeView
      { typeViewDisplay = foldr quantifyDisplay (typeViewDisplay canonicalView) localVarPairs,
        typeViewIdentity = foldr quantifyIdentity (typeViewIdentity canonicalView) localVarPairs,
        typeViewHeadIdentities = typeViewHeadIdentities canonicalView,
        typeViewBinderIdentities =
          mergeTypeBinderIdentityMaps
            (typeViewBinderIdentities canonicalView : map constraintBinderIdentities canonicalConstraints)
      },
    canonicalConstraints
  )
  where
    (identityForalls, identityBody) = splitForalls (typeViewIdentity view)
    alreadyQuantified =
      Set.fromList (map fst identityForalls)
    constraintVars = foldMap (freeTypeVarsTypeViews . constraintTypeViews) constraints
    localIdentityVars =
      sort $
        [ identityName
        | identityName <-
            Set.toList $
              (freeTypeVarsSrcType identityBody `Set.union` constraintVars)
                Set.\\ alreadyQuantified,
          not (isHeadBinderIdentity identityName)
        ]

    localVarPairs =
      [ (Map.findWithDefault identityName identityName displayNamesByIdentityName, identityName)
      | identityName <- localIdentityVars
      ]

    displayNamesByIdentityName =
      typeViewVarPairs view `Map.union` mergeUniquePairMaps (map constraintVarPairs constraints)

    canonicalDisplayNamesByIdentityName =
      Map.fromList
        [ (identityName, displayName)
        | (displayName, identityName) <- localVarPairs
        ]

    canonicalView =
      canonicalizeTypeViewVarDisplays canonicalDisplayNamesByIdentityName view

    canonicalConstraints =
      map (canonicalizeConstraintVarDisplays canonicalDisplayNamesByIdentityName) constraints

    constraintBinderIdentities =
      foldMap typeViewBinderIdentities . constraintTypeViews

    allViews =
      view : concatMap (NE.toList . constraintTypeViews) constraints

    isHeadBinderIdentity name =
      case uniqueBinderIdentityForName name of
        Just identity -> identity `Set.member` headVars
        Nothing -> False

    uniqueBinderIdentityForName name =
      case Set.toList $
        Set.fromList
          [ identity
          | view0 <- allViews,
            Just identity <- [typeViewBinderIdentityForAlias view0 name]
          ] of
        [identity] -> Just identity
        _ -> Nothing

    quantifyDisplay (displayName, _) acc = STForall displayName Nothing acc
    quantifyIdentity (_, identityName) acc = STForall identityName Nothing acc

canonicalizeConstraintVarDisplays :: Map String String -> ConstraintInfo -> ConstraintInfo
canonicalizeConstraintVarDisplays displayNamesByIdentityName constraint =
  constraint
    { constraintTypeViews =
        fmap
          (canonicalizeTypeViewVarDisplays displayNamesByIdentityName)
          (constraintTypeViews constraint)
    }

canonicalizeTypeViewVarDisplays :: Map String String -> TypeView -> TypeView
canonicalizeTypeViewVarDisplays displayNamesByIdentityName view =
  view
    { typeViewDisplay =
        canonicalizeSrcTypeVarDisplays displayNamesByIdentityName (typeViewDisplay view) (typeViewIdentity view)
    }

canonicalizeSrcTypeVarDisplays :: Map String String -> SrcType -> SrcType -> SrcType
canonicalizeSrcTypeVarDisplays displayNamesByIdentityName =
  go Set.empty
  where
    go identityBoundNames display identityTy =
      case (display, identityTy) of
        (STVar displayName, STVar identityName) ->
          STVar (displayNameFor identityBoundNames identityName displayName)
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          STArrow
            (go identityBoundNames displayDom identityDom)
            (go identityBoundNames displayCod identityCod)
        (STCon displayName displayArgs, STCon _ identityArgs) ->
          STCon displayName (NE.zipWith (go identityBoundNames) displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs) ->
          STVarApp
            (displayNameFor identityBoundNames identityName displayName)
            (NE.zipWith (go identityBoundNames) displayArgs identityArgs)
        (STTyLam displayName displayBody, STTyLam identityName identityBody) ->
          STTyLam displayName (go (Set.insert identityName identityBoundNames) displayBody identityBody)
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          STTyApp
            (go identityBoundNames displayFun identityFun)
            (go identityBoundNames displayArg identityArg)
        (STForall displayName displayBound displayBody, STForall identityName identityBound identityBody) ->
          STForall
            displayName
            (canonicalizeBound identityBoundNames displayBound identityBound)
            (go (Set.insert identityName identityBoundNames) displayBody identityBody)
        (STMu displayName displayBody, STMu identityName identityBody) ->
          STMu displayName (go (Set.insert identityName identityBoundNames) displayBody identityBody)
        _ ->
          display

    canonicalizeBound identityBound (Just (SrcBound displayBound)) (Just (SrcBound identityBoundTy)) =
      Just (SrcBound (go identityBound displayBound identityBoundTy))
    canonicalizeBound _ displayBound _ =
      displayBound

    displayNameFor identityBound identityName displayName
      | identityName `Set.member` identityBound = displayName
      | otherwise = Map.findWithDefault displayName identityName displayNamesByIdentityName

constraintVarPairs :: ConstraintInfo -> Map String String
constraintVarPairs constraint =
  mergeUniquePairMaps (map typeViewVarPairs (NE.toList (constraintTypeViews constraint)))

lowerTypeRaw :: Map String DataInfo -> SrcType -> SrcType
lowerTypeRaw dataTypes = lower Map.empty Nothing
  where
    dataTypesByIdentity =
      indexInfoByIdentity dataInfoSymbolIdentity dataTypes

    dataTypeHeadIdentities =
      dataTypeHeadIdentityAliases dataTypesByIdentity (indexDisplayNamesByIdentity dataInfoSymbolIdentity dataTypes)

    lower subst currentData = lowerWith Set.empty subst currentData

    lowerWith seen subst currentData ty = case ty of
      STVar name ->
        case Map.lookup name subst of
          Just replacement
            | replacement /= ty && not (substitutionCycle name seen replacement) ->
                lowerWith (Set.insert name seen) subst currentData replacement
          _ -> ty
      STArrow dom cod -> STArrow (lowerWith seen subst currentData dom) (lowerWith seen subst currentData cod)
      STBase name ->
        case lookupDataType name of
          Just info
            | Builtins.isOpaqueBuiltinDataInfo info -> STBase name
          Just info -> encodeDataType subst info []
          Nothing -> STBase name
      STCon name args ->
        case lookupDataType name of
          Just info
            | Builtins.isOpaqueBuiltinDataInfo info ->
                STCon name (fmap (lowerWith seen subst currentData) args)
          Just info -> encodeDataType subst info (actualArgsForData (lowerWith seen subst currentData) info (toListNE args))
          Nothing -> STCon name (fmap (lowerWith seen subst currentData) args)
      STVarApp name args ->
        let args' = fmap (lowerWith seen subst currentData) args
         in lowerAppliedTypeHead (\seen' -> lowerWith seen' subst currentData) subst seen name args'
      STTyLam name body ->
        STTyLam name (lowerWith (Set.delete name seen) (Map.delete name subst) currentData body)
      STTyApp fun arg -> STTyApp (lowerWith seen subst currentData fun) (lowerWith seen subst currentData arg)
      STForall name mb body ->
        let subst' = Map.delete name subst
            seen' = Set.delete name seen
         in STForall name (fmap (SrcBound . lowerWith seen' subst' currentData . unSrcBound) mb) (lowerWith seen' subst' currentData body)
      STMu name body -> STMu name (lowerWith (Set.delete name seen) (Map.delete name subst) currentData body)
      STBottom -> STBottom

    lookupDataType name =
      Map.lookup name dataTypes
        <|> (Map.lookup name dataTypeHeadIdentities >>= \identity -> Map.lookup identity dataTypesByIdentity)

    encodeDataType subst info actualArgs =
      let actualArgs' =
            if null actualArgs
              then map STVar (dataParams info)
              else actualArgs
          selfName = "$" ++ dataInfoIdentityHeadName info ++ "_self"
          resultName = "$" ++ dataInfoIdentityHeadName info ++ "_result"
          paramSubst = Map.union (Map.fromList (zip (dataParams info) actualArgs')) subst
       in STMu selfName (STForall resultName Nothing (handlerChain info paramSubst (STVar selfName) (STVar resultName)))

    handlerChain info subst selfTy resultTy =
      foldr
        STArrow
        resultTy
        [ foldr
            ( \(name, mbBound) acc ->
                STForall name (fmap (SrcBound . lowerCtorArg subst ownerIdentity selfTy) mbBound) acc
            )
            (foldr STArrow resultTy (map (lowerCtorArg subst ownerIdentity selfTy) (ctorArgs ctor)))
            (ctorForalls ctor)
          | ctor <- dataConstructors info
          , let ownerIdentity = Just (dataInfoSymbolIdentity info)
        ]

    lowerCtorArg subst currentData selfTy = lowerCtorArgWith Set.empty subst currentData selfTy

    lowerCtorArgWith seen subst currentData selfTy ty = case ty of
      STVar name ->
        case Map.lookup name subst of
          Just replacement
            | replacement /= ty && not (substitutionCycle name seen replacement) ->
                lowerCtorArgWith (Set.insert name seen) subst currentData selfTy replacement
          _ -> ty
      STArrow dom cod -> STArrow (lowerCtorArgWith seen subst currentData selfTy dom) (lowerCtorArgWith seen subst currentData selfTy cod)
      STBase name
        | isCurrentDataAlias currentData name -> selfTy
        | otherwise ->
            case lookupDataType name of
              Just info -> encodeDataType subst info []
              Nothing -> STBase name
      STCon name args
        | isCurrentDataAlias currentData name -> selfTy
        | otherwise ->
            case lookupDataType name of
              Just info -> encodeDataType subst info (actualArgsForData (lowerCtorArgWith seen subst currentData selfTy) info (toListNE args))
              Nothing -> STCon name (fmap (lowerCtorArgWith seen subst currentData selfTy) args)
      STVarApp name args ->
        let args' = fmap (lowerCtorArgWith seen subst currentData selfTy) args
         in lowerAppliedTypeHead (\seen' -> lowerCtorArgWith seen' subst currentData selfTy) subst seen name args'
      STTyLam name body ->
        STTyLam name (lowerCtorArgWith (Set.delete name seen) (Map.delete name subst) currentData selfTy body)
      STTyApp fun arg ->
        STTyApp (lowerCtorArgWith seen subst currentData selfTy fun) (lowerCtorArgWith seen subst currentData selfTy arg)
      STForall name mb body ->
        let subst' = Map.delete name subst
            seen' = Set.delete name seen
         in STForall name (fmap (SrcBound . lowerCtorArgWith seen' subst' currentData selfTy . unSrcBound) mb) (lowerCtorArgWith seen' subst' currentData selfTy body)
      STMu name body -> STMu name (lowerCtorArgWith (Set.delete name seen) (Map.delete name subst) currentData selfTy body)
      STBottom -> STBottom

    isCurrentDataAlias currentData name =
      case currentData of
        Nothing -> False
        Just ownerIdentity ->
          Map.lookup name dataTypeHeadIdentities == Just ownerIdentity

    actualArgsForData lowerArg info =
      zipWith
        ( \param arg ->
            if P.typeParamIsFirstOrder param
              then lowerArg arg
              else arg
        )
        (dataTypeParams info)

    substitutionCycle name seen replacement =
      name `Set.member` seen
        || maybe False (\replacementName -> replacementName == name || replacementName `Set.member` seen) (variableHeadName replacement)

    variableHeadName ty =
      case ty of
        STVar replacementName -> Just replacementName
        STVarApp replacementName _ -> Just replacementName
        _ -> Nothing

    lowerAppliedTypeHead continue subst seen name args =
      case Map.lookup name subst >>= \replacement -> applyTypeHead replacement (toListNE args) of
        Just replacementTy
          | replacementTy == STVarApp name args -> replacementTy
          | substitutionCycle name seen replacementTy -> STVarApp name args
          | otherwise -> continue (Set.insert name seen) replacementTy
        Nothing -> STVarApp name args

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

lowerConstructorBinding :: ElaborateScope -> ConstructorInfo -> LoweredBinding
lowerConstructorBinding scope ctorInfo =
  LoweredBinding
    { loweredBindingIdentity = loweredBindingIdentityFromConstructorInfo ctorInfo,
      loweredBindingSourceType = typeViewIdentity (constructorBindingSourceTypeView scope ctorInfo),
      loweredBindingSourceTypeView = Just (constructorBindingSourceTypeView scope ctorInfo),
      loweredBindingExpectedType = constructorBindingExpectedType scope ctorInfo,
      loweredBindingExpectedTypeView = Nothing,
      loweredBindingSurfaceExpr = constructorSurfaceExpr scope ctorInfo,
      loweredBindingResolvedLocalIdentities = [],
      loweredBindingDeferredObligations = Map.empty,
      loweredBindingExternalTypeViews = Map.empty,
      loweredBindingEvidenceParamCount = 0,
      loweredBindingExportedAsMain = False
    }

constructorBindingSourceTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorBindingSourceTypeView scope ctorInfo =
  ctorView
    { typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities sourceView,
            typeViewHeadIdentities ctorView
          ],
      typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities sourceView,
            typeViewBinderIdentities ctorView
          ]
    }
  where
    sourceView =
      sourceTypeViewInScope scope (typeViewDisplay ctorView)

    ctorView =
      quantifiedConstructorTypeView scope ctorInfo

lowerExprBinding :: ElaborateScope -> LoweredBindingIdentity -> SrcType -> Bool -> P.Expr -> Either ProgramError LoweredBinding
lowerExprBinding scope identity expectedTy exportedAsMain expr = do
  result <- runElaborateM (compileExpr scope (Just expectedTy) expr)
  pure
    LoweredBinding
      { loweredBindingIdentity = identity,
        loweredBindingSourceType = canonicalSourceType scope expectedTy,
        loweredBindingSourceTypeView = Nothing,
        loweredBindingExpectedType = lowerType scope expectedTy,
        loweredBindingExpectedTypeView = Nothing,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingResolvedLocalIdentities = elaborateResultResolvedLocalIdentities result,
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypeViews = elaborateResultExternalTypeViews result,
        loweredBindingEvidenceParamCount = 0,
        loweredBindingExportedAsMain = exportedAsMain
      }

lowerConstrainedResolvedExprBinding :: ElaborateScope -> LoweredBindingIdentity -> [ConstraintInfo] -> TypeView -> TypeView -> Bool -> P.ResolvedExpr -> Either ProgramError LoweredBinding
lowerConstrainedResolvedExprBinding scope identity constraints visibleView bodyExpectedView exportedAsMain expr = do
  result <- runElaborateMWithSeed (resolvedLoweringGeneratedIdentities identity constraints visibleView bodyExpectedView expr) $ do
    (scopeWithEvidence, evidenceParams) <- extendConstraintEvidenceInfo scope constraints
    bodyExpr <- compileResolvedExprWithExpectedView scopeWithEvidence (Just bodyExpectedView) expr
    pure (foldr wrapEvidence bodyExpr evidenceParams, length evidenceParams)
  let expectedView = constrainedRuntimeTypeInfoView scope constraints visibleView
      (surfaceExpr, evidenceParamCount) = elaborateResultValue result
      resolvedLocalIdentities
        | null constraints = elaborateResultResolvedLocalIdentities result
        | otherwise = []
  pure
    LoweredBinding
      { loweredBindingIdentity = identity,
        loweredBindingSourceType = canonicalSourceType scope (typeViewDisplay visibleView),
        loweredBindingSourceTypeView = Just visibleView,
        loweredBindingExpectedType = lowerTypeView scope expectedView,
        loweredBindingExpectedTypeView = Just expectedView,
        loweredBindingSurfaceExpr = surfaceExpr,
        loweredBindingResolvedLocalIdentities = resolvedLocalIdentities,
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypeViews = elaborateResultExternalTypeViews result,
        loweredBindingEvidenceParamCount = evidenceParamCount,
        loweredBindingExportedAsMain = exportedAsMain
      }
  where
    wrapEvidence (runtimeName0, evidenceTy) acc =
      surfaceLamAnn runtimeName0 evidenceTy acc

resolvedLoweringGeneratedIdentities :: LoweredBindingIdentity -> [ConstraintInfo] -> TypeView -> TypeView -> P.ResolvedExpr -> [UniqueIdentity]
resolvedLoweringGeneratedIdentities identity constraints visibleView bodyExpectedView expr =
  loweredBindingIdentityGeneratedIdentities identity
    ++ concatMap constraintInfoGeneratedIdentities constraints
    ++ typeViewGeneratedIdentities visibleView
    ++ typeViewGeneratedIdentities bodyExpectedView
    ++ resolvedExprGeneratedIdentities expr

lowerResolvedConstrainedExprBinding :: ElaborateScope -> LoweredBindingIdentity -> P.ResolvedConstrainedType -> Bool -> P.ResolvedExpr -> Either ProgramError LoweredBinding
lowerResolvedConstrainedExprBinding scope identity ty exportedAsMain expr = do
  constraints <- mapM (resolvedConstraintInfoForScope scope) (P.constrainedConstraints ty)
  bodyView <- resolvedTypeViewForScope scope (P.constrainedBody ty)
  let visibleView = constrainedVisibleTypeView constraints bodyView
  lowerConstrainedResolvedExprBinding
    scope
    identity
    constraints
    visibleView
    bodyView
    exportedAsMain
    expr

constrainedRuntimeTypeInfoView :: ElaborateScope -> [ConstraintInfo] -> TypeView -> TypeView
constrainedRuntimeTypeInfoView scope constraints visibleView =
  constrainedRuntimeTypeInfoViewRaw (esTypes scope) (esClassesByIdentity scope) constraints visibleView

constraintEvidenceClosureInfo :: ElaborateScope -> ConstraintInfo -> [(ClassInfo, ConstraintInfo)]
constraintEvidenceClosureInfo scope =
  go Set.empty
  where
    go seen constraint =
      case classInfoForConstraint scope constraint of
        Nothing -> []
        Just classInfo ->
          let key = classConstraintEvidenceKeyInfo classInfo constraint
           in if key `Set.member` seen
                then []
                else
                  let seen' = Set.insert key seen
                      superclasses =
                        map
                          (applyConstraintInfoSubst (superclassSubst classInfo constraint))
                          (classSuperclassInfos classInfo)
                   in (classInfo, constraint) : concatMap (go seen') superclasses

    superclassSubst classInfo constraint =
      typeViewSubstFromParamIdentities
        (classParamBinderIdentities classInfo)
        (constraintTypeViews constraint)

classConstraintEvidenceKeyInfo :: ClassInfo -> ConstraintInfo -> (SymbolIdentity, [SrcType])
classConstraintEvidenceKeyInfo classInfo constraint =
  (classInfoSymbolIdentity classInfo, NE.toList (typeViewsIdentity (constraintTypeViews constraint)))

resolvedConstraintInfoForScope :: ElaborateScope -> P.ResolvedClassConstraint -> Either ProgramError ConstraintInfo
resolvedConstraintInfoForScope scope constraint = do
  views <- mapM (resolvedTypeViewForScope scope) (P.constraintTypes constraint)
  ConstraintInfo
    <$> displayClassNameForResolved scope (P.constraintClassName constraint)
    <*> pure (resolvedSymbolIdentity (P.constraintClassName constraint))
    <*> pure views

resolvedTypeViewForScope :: ElaborateScope -> ResolvedSrcType -> Either ProgramError TypeView
resolvedTypeViewForScope scope ty = do
  display <- displaySrcTypeForResolved scope ty
  pure (typeViewFromResolved ty) {typeViewDisplay = display}

displayClassNameForResolved :: ElaborateScope -> ResolvedSymbol -> Either ProgramError String
displayClassNameForResolved scope symbol =
  case displayNameForSymbol (esClassDisplayNamesByIdentity scope) symbol of
    Just name -> pure name
    Nothing -> Left (ProgramUnknownClass (P.refDisplayName symbol))

displaySrcTypeForResolved :: ElaborateScope -> ResolvedSrcType -> Either ProgramError SrcType
displaySrcTypeForResolved scope = \case
  RSTVar ref -> pure (STVar (resolvedSrcTypeBinderName ref))
  RSTArrow dom cod -> STArrow <$> displaySrcTypeForResolved scope dom <*> displaySrcTypeForResolved scope cod
  RSTBase symbol -> STBase <$> displayTypeHeadNameForResolved scope symbol
  RSTCon symbol args -> STCon <$> displayTypeHeadNameForResolved scope symbol <*> traverse (displaySrcTypeForResolved scope) args
  RSTVarApp ref args -> STVarApp (resolvedSrcTypeBinderName ref) <$> traverse (displaySrcTypeForResolved scope) args
  RSTTyLam ref body -> STTyLam (resolvedSrcTypeBinderName ref) <$> displaySrcTypeForResolved scope body
  RSTTyApp fun arg -> STTyApp <$> displaySrcTypeForResolved scope fun <*> displaySrcTypeForResolved scope arg
  RSTForall ref mb body ->
    STForall (resolvedSrcTypeBinderName ref)
      <$> traverse (fmap SrcBound . displaySrcTypeForResolved scope . unResolvedSrcBound) mb
      <*> displaySrcTypeForResolved scope body
  RSTMu ref body -> STMu (resolvedSrcTypeBinderName ref) <$> displaySrcTypeForResolved scope body
  RSTBottom -> pure STBottom

displayTypeHeadNameForResolved :: ElaborateScope -> ResolvedSymbol -> Either ProgramError String
displayTypeHeadNameForResolved scope symbol =
  case displayNameForSymbol (esTypeDisplayNamesByIdentity scope) symbol of
    Just name -> pure name
    Nothing
      | isBuiltinTypeSymbol symbol -> pure (P.refDisplayName symbol)
    Nothing -> Left (ProgramUnknownType (P.refDisplayName symbol))

displayNameForSymbol :: Map SymbolIdentity [String] -> ResolvedSymbol -> Maybe String
displayNameForSymbol namesByIdentity symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) namesByIdentity of
    Just names ->
      case filter (== P.refDisplayName symbol) names of
        name : _ -> Just name
        [] -> preferredDisplayName (resolvedSymbolIdentity symbol) names
    Nothing -> Nothing

preferredDisplayName :: SymbolIdentity -> [String] -> Maybe String
preferredDisplayName identity names =
  case filter (/= symbolIdentityStableName identity) names of
    name : _ -> Just name
    [] ->
      case names of
        name : _ -> Just name
        [] -> Nothing

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol = Builtins.isBuiltinTypeSymbol

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExpr scope ctorInfo =
  surfaceAnn (constructorSurfaceExprRaw scope ctorInfo) (constructorBindingExpectedType scope ctorInfo)

constructorBindingExpectedType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorBindingExpectedType scope ctorInfo =
  let ctorView = quantifiedConstructorTypeView scope ctorInfo
      loweredTy = lowerTypeView scope ctorView
   in if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
        && srcTypeHasVariableHeadApplication loweredTy
        then constructorStructuralPlaceholderType scope ctorInfo
        else loweredTy

quantifiedConstructorTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
quantifiedConstructorTypeView scope ctorInfo =
  view
    { typeViewDisplay = quantifyFreeTypeVars (typeViewDisplay view),
      typeViewIdentity = quantifyFreeTypeVars (typeViewIdentity view)
    }
  where
    view =
      constructorTypeView scope ctorInfo

constructorSurfaceExprRaw :: ElaborateScope -> ConstructorInfo -> SurfaceExpr
constructorSurfaceExprRaw scope ctorInfo =
  let ctorIdentityName = constructorInfoIdentityName ctorInfo
      argNames = ["$" ++ ctorIdentityName ++ "_arg" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
      handlerNames = ["$" ++ ctorIdentityName ++ "_k" ++ show ix | ix <- [1 .. length handlerCtorOrder]]
      resultVar =
        if any (not . null . ctorForalls) handlerCtorOrder || constructorOwnerHasParams
          then constructorOwnerResultVar ctorInfo
          else "a"
      useStructuralTypes =
        constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
          && srcTypeHasVariableHeadApplication (lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo)))
      argTypes =
        if useStructuralTypes
          then constructorStructuralArgs ctorInfo
          else ctorArgs ctorInfo
      handlerTypes =
        if useStructuralTypes
          then map (constructorStructuralHandlerType resultVar . constructorShapeFromInfo) handlerCtorOrder
          else map (\ctor -> handlerSurfaceType scope ctor (STVar resultVar)) handlerCtorOrder
      selectedHandler =
        surfaceAnn
          ( foldl
              surfaceApp
              (surfaceVar (handlerNames !! ctorIndex ctorInfo))
              (map surfaceVar argNames)
          )
          (STVar resultVar)
      body = foldr (\(handlerName, handlerTy) acc -> surfaceLamAnn handlerName handlerTy acc) selectedHandler (zip handlerNames handlerTypes)
      lifted =
        foldr
          (\(argName, argTy) acc -> surfaceLamAnn argName (lowerType scope argTy) acc)
          body
          (zip argNames argTypes)
   in lifted
  where
    ownerInfo =
      resolveConstructorDataInfo scope ctorInfo
    ctorOrder =
      maybe [] dataConstructors ownerInfo
    handlerCtorOrder =
      map specializeHandlerConstructor ctorOrder
    constructorOwnerHasParams =
      maybe False (not . null . dataParams) ownerInfo
    specializeHandlerConstructor ctor =
      case matchTypesInScope scope Map.empty (ctorResult ctor) (ctorResult ctorInfo) of
        Just subst -> specializeConstructorInfo subst ctor
        Nothing -> ctor

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpr scope mbExpected expr = case expr of
  EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {valueMethodInfo = methodInfo} ->
        compileNullaryMethodUse scope mbExpected methodInfo
      Just valueInfo@OrdinaryValue {valueRuntimeName = runtimeName} -> do
        evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected []
        let applied = foldl surfaceApp (surfaceVar runtimeName) evidenceSurfaces
        pure $
          if null evidenceSurfaces
            then annotateExpectedBareValueUse scope mbExpected valueInfo applied
            else applied
      Just ConstructorValue {valueCtorInfo = ctorInfo} -> do
        compileConstructorHead scope ctorInfo 0 (constructorInitialSubst scope ctorInfo 0 mbExpected)
      Nothing -> throwError (ProgramUnknownValue name)
  ELit lit -> pure (surfaceLit lit)
  ELam param body -> do
    runtimeName <- freshRuntimeName (P.paramName param)
    let paramTy = case (P.paramType param, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendLocal scope (P.paramName param) runtimeName paramTy
    bodyExpr0 <- compileExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> surfaceAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      case paramTy of
        Just ty -> surfaceLamAnn runtimeName (lowerType scope ty) bodyExpr
        Nothing -> surfaceLam runtimeName bodyExpr
  EApp _ _ -> compileApp scope mbExpected expr
  ELet name mbTy rhs body -> do
    if name `notElem` collectFreeValues Set.empty body && mbTy == Nothing
      then compileExpr scope mbExpected body
      else do
        let recursive = mentionsFreeValue name rhs
        case (recursive, mbTy, inlineImmediateLetUse name rhs body) of
          (False, Nothing, Just inlined) ->
            compileExpr scope mbExpected inlined
          _ -> do
            runtimeName <- freshRuntimeName name
            localRef <- freshElaborateLocalRef name
            provisionalTy <- case (recursive, mbTy) of
              (True, Nothing) -> Just <$> freshTypeName
              _ -> pure mbTy
            selfScope <-
              if recursive
                then extendLocalWithRef scope localRef name runtimeName provisionalTy
                else pure scope
            rhsExpr <- compileExpr selfScope provisionalTy rhs
            bindingTy <- case mbTy of
              Just ty -> pure (lowerType scope ty)
              Nothing
                | Just rhsTy <- explicitExprAnnotation rhs -> pure (lowerType scope rhsTy)
                | Just rhsTy <- inferKnownExprType selfScope rhs -> pure (lowerType scope rhsTy)
                | Just ty <- provisionalTy -> pure (lowerType scope ty)
              Nothing -> freshTypeName
            let rhsExpr' =
                  case mbTy of
                    Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
                    Nothing ->
                      case inferKnownExprType selfScope rhs of
                        Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
                        Nothing -> rhsExpr
            bodyScope <- extendLocalLoweredWithRef scope localRef name runtimeName bindingTy
            bodyExpr <- compileExpr bodyScope mbExpected body
            pure (surfaceLet runtimeName rhsExpr' bodyExpr)
  EAnn inner annTy ->
    case inner of
      EVar name
        | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope),
          methodFullArity methodInfo == 0 ->
            compileExpr scope (Just annTy) inner
      _ -> do
        innerExpr <- compileExpr scope (Just annTy) inner
        pure (surfaceAnn innerExpr (lowerType scope annTy))
  ECase scrutinee alts -> compileCase scope mbExpected scrutinee alts

compileResolvedExpr :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedExpr scope mbExpected expr = case expr of
  EVar ref -> do
    valueInfo <- lookupResolvedValueInfo scope ref
    case valueInfo of
      OverloadedMethod {valueMethodInfo = methodInfo} ->
        compileNullaryMethodUse scope mbExpected methodInfo
      ordinary@OrdinaryValue {valueRuntimeName = runtimeName} -> do
        evidenceSurfaces <- valueResolvedEvidenceArgs scope ordinary mbExpected []
        pure (annotateExpectedBareValueUse scope mbExpected ordinary (foldl surfaceApp (surfaceVar runtimeName) evidenceSurfaces))
      ConstructorValue {valueCtorInfo = ctorInfo} -> do
        compileConstructorHead scope ctorInfo 0 (constructorInitialSubst scope ctorInfo 0 mbExpected)
  ELit lit -> pure (surfaceLit lit)
  ELam param body -> do
    let paramRef = P.paramName param
        paramSourceName = localRefName paramRef
    runtimeName <- freshRuntimeName paramSourceName
    recordResolvedLocalIdentity runtimeName paramRef
    paramAnn <- traverse (liftEitherElab . displaySrcTypeForResolved scope) (P.paramType param)
    let paramTy = case (paramAnn, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendResolvedLocal scope paramRef runtimeName paramTy
    bodyExpr0 <- compileResolvedExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> surfaceAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      case paramTy of
        Just ty -> surfaceLamAnn runtimeName (lowerType scope ty) bodyExpr
        Nothing -> surfaceLam runtimeName bodyExpr
  EApp _ _ -> compileResolvedApp scope mbExpected expr
  ELet localRef mbTy rhs body -> do
    mbTypeView <- traverse (liftEitherElab . resolvedTypeViewForScope scope) mbTy
    let mbDisplayTy = typeViewDisplay <$> mbTypeView
    if localRef `notElem` collectFreeResolvedValues Set.empty body && mbDisplayTy == Nothing
      then compileResolvedExpr scope mbExpected body
      else do
        let name = localRefName localRef
            recursive = mentionsFreeResolvedValue localRef rhs
        case (recursive, mbDisplayTy, inlineImmediateResolvedLetUse localRef rhs body) of
          (False, Nothing, Just inlined) ->
            compileResolvedExpr scope mbExpected inlined
          _ -> do
            runtimeName <- freshRuntimeName name
            recordResolvedLocalIdentity runtimeName localRef
            provisionalTy <- case (recursive, mbDisplayTy) of
              (True, Nothing) -> Just <$> freshTypeName
              _ -> pure mbDisplayTy
            selfScope <-
              case (recursive, mbTypeView, provisionalTy) of
                (True, Just view, _) -> extendResolvedLocalView scope localRef runtimeName (Just view)
                (True, Nothing, _) -> extendResolvedLocal scope localRef runtimeName provisionalTy
                _ -> pure scope
            rhsExpr <-
              case mbTypeView of
                Just view -> compileResolvedExprWithExpectedView selfScope (Just view) rhs
                Nothing -> compileResolvedExpr selfScope provisionalTy rhs
            bindingTy <- case mbDisplayTy of
              Just ty ->
                case mbTypeView of
                  Just view -> pure (lowerTypeView scope view)
                  Nothing -> pure (lowerType scope ty)
              Nothing
                | Just rhsTy <- explicitResolvedExprAnnotation scope rhs -> pure (lowerType scope rhsTy)
                | Just rhsTy <- inferKnownResolvedExprType selfScope rhs -> pure (lowerType scope rhsTy)
                | Just ty <- provisionalTy -> pure (lowerType scope ty)
              Nothing -> freshTypeName
            let rhsExpr' =
                  case mbTypeView of
                    Just view -> surfaceAnn rhsExpr (lowerTypeView scope view)
                    Nothing ->
                      case inferKnownResolvedExprType selfScope rhs of
                        Just ty -> surfaceAnn rhsExpr (lowerType scope ty)
                        Nothing -> rhsExpr
            bodyScope <-
              case mbTypeView of
                Just view -> extendResolvedLocalView scope localRef runtimeName (Just view)
                Nothing -> extendResolvedLocalLowered scope localRef runtimeName bindingTy
            bodyExpr <- compileResolvedExpr bodyScope mbExpected body
            pure (surfaceLet runtimeName rhsExpr' bodyExpr)
  EAnn inner annTy -> do
    annDisplayTy <- liftEitherElab (displaySrcTypeForResolved scope annTy)
    case inner of
      EVar ref
        | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
          methodFullArity methodInfo == 0 ->
            compileResolvedExpr scope (Just annDisplayTy) inner
      _ -> do
        innerExpr <- compileResolvedExpr scope (Just annDisplayTy) inner
        pure (surfaceAnn innerExpr (lowerType scope annDisplayTy))
  ECase scrutinee alts -> compileResolvedCase scope mbExpected scrutinee alts

compileResolvedExprWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedExprWithExpectedView scope mbExpectedView expr =
  case expr of
    EVar ref -> do
      valueInfo <- lookupResolvedValueInfo scope ref
      case valueInfo of
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          compileNullaryMethodUseWithView scope mbExpectedView methodInfo
        ordinary@OrdinaryValue {valueRuntimeName = runtimeName} -> do
          evidenceSurfaces <- valueResolvedEvidenceArgsWithExpectedView scope ordinary mbExpectedView []
          let applied = foldl surfaceApp (surfaceVar runtimeName) evidenceSurfaces
              bareExpr =
                if null evidenceSurfaces
                  then annotateExpectedBareValueUse scope mbExpected ordinary applied
                  else applied
          pure bareExpr
        ConstructorValue {valueCtorInfo = ctorInfo} ->
          compileConstructorHead scope ctorInfo 0 (constructorInitialViewSubst scope ctorInfo 0 mbExpectedView)
    EApp _ _ ->
      compileResolvedAppWithExpectedView scope mbExpectedView expr
    ELam param body -> do
      let paramRef = P.paramName param
          paramSourceName = localRefName paramRef
      runtimeName <- freshRuntimeName paramSourceName
      recordResolvedLocalIdentity runtimeName paramRef
      paramAnn <- traverse (liftEitherElab . resolvedTypeViewForScope scope) (P.paramType param)
      let paramView = case (paramAnn, mbExpectedView) of
            (Just view, _) -> Just view
            (Nothing, Just expectedView) -> expectedDomainTypeView expectedView
            _ -> Nothing
      scope' <- extendResolvedLocalView scope paramRef runtimeName paramView
      bodyExpr0 <- compileResolvedExprWithExpectedView scope' (mbExpectedView >>= expectedCodomainTypeView) body
      let bodyExpr =
            case typeViewDisplay <$> (mbExpectedView >>= expectedCodomainTypeView) of
              Just codTy | isRecursiveResultType codTy -> surfaceAnn bodyExpr0 (lowerType scope codTy)
              _ -> bodyExpr0
      pure $
        case paramView of
          Just view -> surfaceLamAnn runtimeName (lowerTypeView scope view) bodyExpr
          Nothing -> surfaceLam runtimeName bodyExpr
    _ ->
      compileResolvedExpr scope mbExpected expr
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

expectedDomainTypeView :: TypeView -> Maybe TypeView
expectedDomainTypeView view =
  case (typeViewDisplay view, typeViewIdentity view) of
    (STArrow displayDom _, STArrow identityDom _) ->
      Just
        ( TypeView
            { typeViewDisplay = displayDom,
              typeViewIdentity = identityDom,
              typeViewHeadIdentities = typeViewHeadIdentities view,
              typeViewBinderIdentities = typeViewBinderIdentities view
            }
        )
    _ -> Nothing

expectedCodomainTypeView :: TypeView -> Maybe TypeView
expectedCodomainTypeView view =
  case (typeViewDisplay view, typeViewIdentity view) of
    (STArrow _ displayCod, STArrow _ identityCod) ->
      Just
        ( TypeView
            { typeViewDisplay = displayCod,
              typeViewIdentity = identityCod,
              typeViewHeadIdentities = typeViewHeadIdentities view,
              typeViewBinderIdentities = typeViewBinderIdentities view
            }
        )
    _ -> Nothing

compileApp :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileApp scope mbExpected expr =
  case collectApps expr of
    (EVar name, args)
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope) ->
          compileMethodApp scope mbExpected methodInfo args
      | Just valueInfo <- Map.lookup name (esValues scope) ->
          compileValueApp scope mbExpected valueInfo args
    (headExpr, [arg])
      | Just expectedTy <- mbExpected -> do
          (expectedHeadTy, argSurface) <-
            case inferKnownExprType scope arg of
              Just argTy -> do
                argSurface <- compileExpr scope (Just argTy) arg
                pure (STArrow argTy expectedTy, argSurface)
              Nothing -> do
                argTy <- freshTypeName
                argSurface <- compileExpr scope Nothing arg
                pure (STArrow argTy expectedTy, argSurface)
          headSurface <- compileExpr scope (Just expectedHeadTy) headExpr
          pure (surfaceApp headSurface argSurface)
    (headExpr, args) -> do
      headSurface <- compileExpr scope Nothing headExpr
      argSurfaces <- mapM (compileExpr scope Nothing) args
      pure (foldl surfaceApp headSurface argSurfaces)

compileResolvedApp :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedApp scope mbExpected =
  compileResolvedAppWithExpectedView scope (sourceTypeViewInScope scope <$> mbExpected)

compileResolvedAppWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileResolvedAppWithExpectedView scope mbExpectedView expr =
  case collectResolvedApps expr of
    (EVar ref, args) -> do
      valueInfo <- lookupResolvedValueInfo scope ref
      case valueInfo of
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          compileResolvedMethodApp scope mbExpected methodInfo args
        _ ->
          compileResolvedValueAppWithExpectedView scope mbExpectedView valueInfo args
    (headExpr, [arg])
      | Just expectedTy <- mbExpected -> do
          (expectedHeadTy, argSurface) <-
            case inferKnownResolvedExprType scope arg of
              Just argTy -> do
                argSurface <- compileResolvedExpr scope (Just argTy) arg
                pure (STArrow argTy expectedTy, argSurface)
              Nothing -> do
                argTy <- freshTypeName
                argSurface <- compileResolvedExpr scope Nothing arg
                pure (STArrow argTy expectedTy, argSurface)
          headSurface <- compileResolvedExpr scope (Just expectedHeadTy) headExpr
          pure (surfaceApp headSurface argSurface)
    (headExpr, args) -> do
      headSurface <- compileResolvedExpr scope Nothing headExpr
      argSurfaces <- mapM (compileResolvedExpr scope Nothing) args
      pure (foldl surfaceApp headSurface argSurfaces)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

explicitExprAnnotation :: P.Expr -> Maybe SrcType
explicitExprAnnotation expr =
  case expr of
    EAnn _ ty -> Just ty
    _ -> Nothing

explicitResolvedExprAnnotation :: ElaborateScope -> P.ResolvedExpr -> Maybe SrcType
explicitResolvedExprAnnotation scope expr =
  case expr of
    EAnn _ ty -> either (const Nothing) Just (displaySrcTypeForResolved scope ty)
    _ -> Nothing

lookupResolvedValueInfo :: ElaborateScope -> P.ResolvedValueRef -> ElaborateM ValueInfo
lookupResolvedValueInfo scope ref =
  case ref of
    P.ResolvedLocalValue localRef ->
      case Map.lookup localRef (esLocalValues scope) of
        Just valueInfo -> pure valueInfo
        Nothing -> throwError (ProgramUnknownValue (localRefName localRef))
    P.ResolvedGlobalValue symbol ->
      case lookupValueInfoBySymbol scope symbol of
        Just valueInfo -> pure valueInfo
        Nothing -> throwError (ProgramUnknownValue (P.refDisplayName symbol))

lookupValueInfoBySymbol :: ElaborateScope -> ResolvedSymbol -> Maybe ValueInfo
lookupValueInfoBySymbol scope symbol =
  case Map.lookup (resolvedSymbolIdentity symbol) (esValuesByIdentity scope) of
    Just info -> Just info
    Nothing -> Nothing

compileValueApp :: ElaborateScope -> Maybe SrcType -> ValueInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileValueApp scope mbExpected ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgTys) = constructorArgPlan scope ctorInfo mbExpected args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  constructorHead <- compileConstructorHead scope ctorInfo (length args) constructorSubst
  pure (foldl surfaceApp constructorHead argSurfaces)
  where
    compileConstructorArg expectedTy arg = do
      case inferKnownExprType scope arg of
        Just knownTy -> do
          let specializedKnownTy = specializeKnownTypeForExpected scope expectedTy knownTy
          ensureSourceTypeCompatible scope expectedTy specializedKnownTy
          compileExpr scope (Just (constructorArgCompileExpectedType expectedTy knownTy)) arg
        Nothing -> do
          argSurface <- compileExpr scope (Just expectedTy) arg
          pure $
            if hasLeadingForall expectedTy
              then surfaceAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileValueApp scope mbExpected valueInfo args = do
  let expectedArgTys = valueExpectedArgTypes scope valueInfo mbExpected args
  argSurfaces <- zipWithM compileValueArg (expectedArgTys ++ repeat Nothing) args
  evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected args
  let headSurface =
        case valueInfo of
          OrdinaryValue {valueRuntimeName = runtimeName} -> surfaceVar runtimeName
          OverloadedMethod {} -> error "compileValueApp does not handle overloaded methods"
      headWithEvidence = foldl surfaceApp headSurface evidenceSurfaces
      applied = foldl surfaceApp headWithEvidence argSurfaces
  pure (annotateExpectedValueUse scope mbExpected valueInfo applied)
  where
    compileValueArg (Just expectedTy) arg
      | isPartialOverloadedMethodApp scope arg =
          compileKnownExpectedArg expectedTy arg
    compileValueArg (Just expectedTy) arg =
      compileKnownExpectedArg expectedTy arg
    compileValueArg _ arg =
      compileExpr scope Nothing arg

    compileKnownExpectedArg expectedTy arg = do
      case inferKnownExprType scope arg of
        Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
        Nothing -> pure ()
      compileExpr scope (Just expectedTy) arg

compileResolvedValueAppWithExpectedView :: ElaborateScope -> Maybe TypeView -> ValueInfo -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
compileResolvedValueAppWithExpectedView scope mbExpectedView ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgTys) = constructorResolvedArgPlan scope ctorInfo mbExpected args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  constructorHead <- compileConstructorHead scope ctorInfo (length args) constructorSubst
  pure (foldl surfaceApp constructorHead argSurfaces)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

    compileConstructorArg expectedTy arg = do
      case inferKnownResolvedExprType scope arg of
        Just knownTy -> do
          let specializedKnownTy = specializeKnownTypeForExpected scope expectedTy knownTy
          ensureSourceTypeCompatible scope expectedTy specializedKnownTy
          compileResolvedExpr scope (Just (constructorArgCompileExpectedType expectedTy knownTy)) arg
        Nothing -> do
          argSurface <- compileResolvedExpr scope (Just expectedTy) arg
          pure $
            if hasLeadingForall expectedTy
              then surfaceAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileResolvedValueAppWithExpectedView scope mbExpectedView valueInfo args = do
  let expectedArgViews = valueExpectedArgViews scope valueInfo mbExpectedView args
  argSurfaces <- zipWithM compileValueArg (expectedArgViews ++ repeat Nothing) args
  evidenceSurfaces <- valueResolvedEvidenceArgsWithExpectedView scope valueInfo mbExpectedView args
  let headSurface =
        case valueInfo of
          OrdinaryValue {valueRuntimeName = runtimeName} -> surfaceVar runtimeName
          OverloadedMethod {} -> error "compileResolvedValueApp does not handle overloaded methods"
      headWithEvidence = foldl surfaceApp headSurface evidenceSurfaces
      applied = foldl surfaceApp headWithEvidence argSurfaces
  pure (annotateExpectedValueUse scope mbExpected valueInfo applied)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

    compileValueArg (Just expectedView) arg
      | isPartialOverloadedResolvedMethodApp scope arg =
          compileKnownExpectedArg expectedView arg
    compileValueArg (Just expectedView) arg =
      compileKnownExpectedArg expectedView arg
    compileValueArg _ arg =
      compileResolvedExpr scope Nothing arg

    compileKnownExpectedArg expectedView arg = do
      let expectedTy = typeViewDisplay expectedView
      case inferKnownResolvedExprType scope arg of
        Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
        Nothing -> pure ()
      compileResolvedExprWithExpectedView scope (Just expectedView) arg

compileConstructorHead :: ElaborateScope -> ConstructorInfo -> Int -> TypeViewSubst -> ElaborateM SurfaceExpr
compileConstructorHead scope ctorInfo argCount constructorSubst = do
  placeholder <- deferConstructorCall scope ctorInfo argCount constructorSubst
  pure (surfaceVar placeholder)

specializeConstructorInfo :: Map String SrcType -> ConstructorInfo -> ConstructorInfo
specializeConstructorInfo subst ctorInfo =
  let forallEntries =
        [ ((name, fmap (specializeSrcType subst) mbBound), binder)
          | ((name, mbBound), binder) <- zip (ctorForalls ctorInfo) (ctorForallBinderInfo ctorInfo),
            Map.notMember name subst
        ]
      foralls' = map fst forallEntries
      forallBinderInfo' = map snd forallEntries
      args' = map (specializeSrcType subst) (ctorArgs ctorInfo)
      result' = specializeSrcType subst (ctorResult ctorInfo)
      bodyTy = foldr STArrow result' args'
      type' =
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
          bodyTy
          foralls'
   in ctorInfo
        { ctorTypeView =
            (ctorTypeView ctorInfo)
              { typeViewDisplay = type'
              },
          ctorForallBinderInfo = forallBinderInfo'
        }

ordinaryValueTypeInScope :: ElaborateScope -> ValueInfo -> SrcType
ordinaryValueTypeInScope scope valueInfo@OrdinaryValue {} =
  visibleTypeForIdentity (esTypes scope) ty identityTy
  where
    ty = valueType valueInfo
    identityTy = valueIdentityType valueInfo
ordinaryValueTypeInScope _ _ =
  STBottom

valueExpectedArgTypes :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [expr] -> [Maybe SrcType]
valueExpectedArgTypes scope valueInfo mbExpected args =
  let (argTys0, resultTy0) =
        case valueInfo of
          OrdinaryValue {} ->
            splitArrows (snd (splitForalls (ordinaryValueTypeInScope scope valueInfo)))
          _ -> ([], STBottom)
      resultTyForArity =
        foldr STArrow resultTy0 (drop (length args) argTys0)
      subst =
        case mbExpected >>= matchTypesInScope scope Map.empty resultTyForArity of
          Just matched -> matched
          Nothing -> Map.empty
      argTys = map (specializeSrcType subst) argTys0
   in map concreteExpectedTy (take (length args) argTys)
  where
    concreteExpectedTy ty
      | Set.null (freeTypeVarsSrcType ty) = Just ty
      | otherwise = Nothing

valueExpectedArgViews :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [expr] -> [Maybe TypeView]
valueExpectedArgViews scope valueInfo mbExpectedView args =
  case valueInfo of
    OrdinaryValue {} ->
      map concreteExpectedView (take (length args) argViews)
    _ ->
      map (fmap sourceView) (valueExpectedArgTypes scope valueInfo mbExpected args)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView
    valueView =
      (ordinaryValueTypeView valueInfo)
        { typeViewDisplay = ordinaryValueTypeInScope scope valueInfo
        }
    resultViewForArity =
      valueResultTypeViewForArity valueView (length args)
    subst =
      case mbExpectedView >>= matchTypeViewAgainstIdentity scope Map.empty resultViewForArity of
        Just matched -> matched
        Nothing -> Map.empty
    argViews =
      map (applyTypeViewSubst subst) (methodParamTypeViews valueView)

    concreteExpectedView view
      | Set.null (freeTypeVarsTypeView view) = Just view
      | otherwise = Nothing

    sourceView =
      sourceTypeViewInScope scope

isPartialOverloadedMethodApp :: ElaborateScope -> P.Expr -> Bool
isPartialOverloadedMethodApp scope expr =
  case collectApps expr of
    (EVar name, args)
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope) ->
          not (null args) && length args < methodFullArity methodInfo
    _ -> False

isPartialOverloadedResolvedMethodApp :: ElaborateScope -> P.ResolvedExpr -> Bool
isPartialOverloadedResolvedMethodApp scope expr =
  case collectResolvedApps expr of
    (EVar ref, args)
      | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          not (null args) && length args < methodFullArity methodInfo
    _ -> False

runElaborateLookup :: ElaborateM a -> Either ProgramError a
runElaborateLookup action = elaborateResultValue <$> runElaborateM action

specializeKnownTypeForExpected :: ElaborateScope -> SrcType -> SrcType -> SrcType
specializeKnownTypeForExpected scope expectedTy knownTy =
  case matchTypesInScope scope Map.empty knownTy expectedTy of
    Just subst -> specializeSrcType subst knownTy
    Nothing ->
      case matchTypesByShapeInScope scope Map.empty knownTy expectedTy of
        Just subst -> specializeSrcType subst knownTy
        Nothing -> knownTy

constructorArgCompileExpectedType :: SrcType -> SrcType -> SrcType
constructorArgCompileExpectedType expectedTy knownTy
  | hasLeadingForall knownTy = expectedTy
  | Set.null (freeTypeVarsSrcType knownTy) = knownTy
  | otherwise = expectedTy

matchTypesByShapeInScope :: ElaborateScope -> Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesByShapeInScope scope subst template actual = case template of
  STVar name ->
    case Map.lookup name subst of
      Nothing -> Just (Map.insert name actual subst)
      Just existing
        | semanticTypeEqual scope existing actual -> Just subst
        | otherwise -> Nothing
  STArrow dom cod ->
    case actual of
      STArrow dom' cod' -> do
        subst' <- matchTypesByShapeInScope scope subst dom dom'
        matchTypesByShapeInScope scope subst' cod cod'
      _ -> Nothing
  STBase name ->
    case actual of
      STBase name'
        | sameTypeHeadInScope scope name name' -> Just subst
      _ -> Nothing
  STCon name args ->
    case actual of
      STCon name' args'
        | sameTypeHeadInScope scope name name' && length (toListNE args) == length (toListNE args') ->
            foldM
              (\acc (templateTy, actualTy) -> matchTypesByShapeInScope scope acc templateTy actualTy)
              subst
              (zip (toListNE args) (toListNE args'))
      _ -> Nothing
  STVarApp name args ->
    matchTypeHeadApplicationWith (matchTypesByShapeInScope scope) (semanticTypeEqual scope) subst name args actual
  STTyLam name body ->
    case actual of
      STTyLam name' body'
        | name == name' -> matchTypesByShapeInScope scope subst body body'
      _ -> Nothing
  STTyApp fun arg ->
    case actual of
      STTyApp fun' arg' -> do
        subst' <- matchTypesByShapeInScope scope subst fun fun'
        matchTypesByShapeInScope scope subst' arg arg'
      _ -> Nothing
  STForall name mb body ->
    case actual of
      STForall name' mb' body'
        | name == name' -> do
            subst' <-
              case (mb, mb') of
                (Nothing, _) -> Just subst
                (Just bound, Just bound') -> matchTypesByShapeInScope scope subst (unSrcBound bound) (unSrcBound bound')
                (Just {}, Nothing) -> Nothing
            matchTypesByShapeInScope scope subst' body body'
      _ -> Nothing
  STMu name body ->
    case actual of
      STMu name' body'
        | name == name' -> matchTypesByShapeInScope scope subst body body'
      _ -> Nothing
  STBottom ->
    case actual of
      STBottom -> Just subst
      _ -> Nothing

matchTypeHeadApplicationWith ::
  (Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)) ->
  (SrcType -> SrcType -> Bool) ->
  Map String SrcType ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchTypeHeadApplicationWith matchChild sameType subst expectedName expectedArgs actual =
  case actual of
    STCon actualName actualArgs ->
      matchAppliedHead (STBase actualName) (toListNE actualArgs)
    STVarApp actualName actualArgs ->
      matchAppliedHead (STVar actualName) (toListNE actualArgs)
    _ -> Nothing
  where
    expectedArgsList = toListNE expectedArgs
    expectedArgCount = length expectedArgsList

    matchAppliedHead headTy actualArgsList
      | length actualArgsList < expectedArgCount = Nothing
      | otherwise = do
          let (headArgs, matchedArgs) = splitAt (length actualArgsList - expectedArgCount) actualArgsList
          appliedHead <- applyTypeHead headTy headArgs
          subst' <- bindTypeHeadVariable sameType subst expectedName appliedHead
          foldM
            (\acc (templateTy, actualTy) -> matchChild acc templateTy actualTy)
            subst'
            (zip expectedArgsList matchedArgs)

bindTypeHeadVariable ::
  (SrcType -> SrcType -> Bool) ->
  Map String SrcType ->
  String ->
  SrcType ->
  Maybe (Map String SrcType)
bindTypeHeadVariable sameType subst name ty =
  case Map.lookup name subst of
    Just existing
      | sameType existing ty -> Just subst
      | otherwise -> Nothing
    Nothing
      | ty == STVar name -> Just subst
      | name `Set.member` freeTypeVarsSrcType ty -> Nothing
      | otherwise -> Just (Map.insert name ty subst)

constructorArgPlan :: ElaborateScope -> ConstructorInfo -> Maybe SrcType -> [P.Expr] -> (TypeViewSubst, [SrcType])
constructorArgPlan scope ctorInfo mbExpected args =
  let (subst, argTys) = foldl step (initialSubst, []) (zip (constructorArgTypeViews scope ctorInfo) args)
   in (subst, reverse argTys)
  where
    initialSubst =
      constructorInitialSubst scope ctorInfo (length args) mbExpected

    step (subst, acc) (templateView, arg) =
      let subst' =
            case inferKnownExprType scope arg >>= matchConstructorArgViewSubst scope subst templateView of
              Just matched -> matched
              Nothing -> subst
          expectedTy = typeViewDisplay (applyTypeViewSubst subst' templateView)
       in (subst', expectedTy : acc)

constructorResolvedArgPlan :: ElaborateScope -> ConstructorInfo -> Maybe SrcType -> [P.ResolvedExpr] -> (TypeViewSubst, [SrcType])
constructorResolvedArgPlan scope ctorInfo mbExpected args =
  let (subst, argTys) = foldl step (initialSubst, []) (zip (constructorArgTypeViews scope ctorInfo) args)
   in (subst, reverse argTys)
  where
    initialSubst =
      constructorInitialSubst scope ctorInfo (length args) mbExpected

    step (subst, acc) (templateView, arg) =
      let subst' =
            case inferKnownResolvedExprTypeView scope arg >>= matchConstructorArgTypeViewSubst scope subst templateView of
              Just matched -> matched
              Nothing -> subst
          expectedTy = typeViewDisplay (applyTypeViewSubst subst' templateView)
       in (subst', expectedTy : acc)

constructorInitialSubst :: ElaborateScope -> ConstructorInfo -> Int -> Maybe SrcType -> TypeViewSubst
constructorInitialSubst scope ctorInfo argCount mbExpected =
  constructorInitialViewSubst scope ctorInfo argCount (sourceTypeViewInScope scope <$> mbExpected)

constructorInitialViewSubst :: ElaborateScope -> ConstructorInfo -> Int -> Maybe TypeView -> TypeViewSubst
constructorInitialViewSubst scope ctorInfo argCount mbExpected =
  case identityMatch <|> displayMatch of
    Just subst -> subst
    Nothing -> Map.empty
  where
    templateView = constructorOccurrenceTypeView scope ctorInfo argCount
    identityMatch =
      mbExpected >>= matchTypeViewAgainstIdentity scope Map.empty templateView
    displayMatch =
      sourceTypeViewSubstForTemplateInScope scope templateView
        <$> (typeViewDisplay <$> mbExpected >>= matchTypesInScope scope Map.empty (constructorOccurrenceType ctorInfo argCount))

matchConstructorArgViewSubst :: ElaborateScope -> TypeViewSubst -> TypeView -> SrcType -> Maybe TypeViewSubst
matchConstructorArgViewSubst scope subst templateView actualTy =
  matchTypeViewAgainstIdentity scope subst templateView (sourceTypeViewInScope scope actualTy)
    <|> do
      matched <-
        matchTypesInScope
          scope
          (typeViewSubstDisplayTypes templateView subst)
          (typeViewDisplay templateView)
          actualTy
      pure (sourceTypeViewSubstForTemplateInScope scope templateView matched `Map.union` subst)

valueEvidenceArgs :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [P.Expr] -> ElaborateM [SurfaceExpr]
valueEvidenceArgs scope valueInfo@OrdinaryValue {valueConstraints = displayConstraints, valueConstraintInfos = constraints} mbExpected args
  | null constraints = pure []
  | otherwise = do
      let valueView = ordinaryValueTypeView valueInfo
      subst <-
        case inferCallSubst scope valueView args of
          Just subst0 ->
            pure (refineValueEvidenceViewSubst scope valueInfo (sourceTypeViewInScope scope <$> mbExpected) args subst0)
          Nothing ->
            case displayConstraints of
              constraint : _ -> throwError (noMatchingDisplayConstraintError constraint)
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      if any usesLocalPolymorphicEvidence specializedConstraints
        then throwError (ProgramAmbiguousConstrainedValueUse (valueInfoIdentityName valueInfo))
        else concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      not (Set.null (freeTypeVarsTypeViews (constraintTypeViews constraint)))
        && constraintCoveredByEvidenceInfo scope constraint
valueEvidenceArgs _ _ _ _ = pure []

valueResolvedEvidenceArgs :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [P.ResolvedExpr] -> ElaborateM [SurfaceExpr]
valueResolvedEvidenceArgs scope valueInfo@OrdinaryValue {valueConstraints = displayConstraints, valueConstraintInfos = constraints} mbExpected args
  | null constraints = pure []
  | otherwise = do
      let valueView = ordinaryValueTypeView valueInfo
      subst <-
        case inferResolvedCallSubst scope valueView args of
          Just subst0 ->
            pure (refineValueEvidenceViewSubst scope valueInfo (sourceTypeViewInScope scope <$> mbExpected) args subst0)
          Nothing ->
            case displayConstraints of
              constraint : _ -> throwError (noMatchingDisplayConstraintError constraint)
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      hasLocalPolymorphicEvidence <- or <$> mapM usesLocalPolymorphicEvidence specializedConstraints
      if hasLocalPolymorphicEvidence
        then throwError (ProgramAmbiguousConstrainedValueUse (valueInfoIdentityName valueInfo))
        else concat <$> mapM (constraintResolvedEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      (&& constraintCoveredByEvidenceInfo scope constraint)
        <$> constraintInfoHasFreeTypeBinderIdentities constraint
valueResolvedEvidenceArgs _ _ _ _ = pure []

valueResolvedEvidenceArgsWithExpectedView :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [P.ResolvedExpr] -> ElaborateM [SurfaceExpr]
valueResolvedEvidenceArgsWithExpectedView scope valueInfo@OrdinaryValue {valueConstraints = displayConstraints, valueConstraintInfos = constraints} mbExpectedView args
  | null constraints = pure []
  | otherwise = do
      subst <-
        case inferResolvedCallSubst scope (ordinaryValueTypeView valueInfo) args of
          Just subst0 ->
            pure $
              refineValueEvidenceViewSubst
                scope
                valueInfo
                mbExpectedView
                args
                subst0
          Nothing ->
            case displayConstraints of
              constraint : _ -> throwError (noMatchingDisplayConstraintError constraint)
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      hasLocalPolymorphicEvidence <- or <$> mapM usesLocalPolymorphicEvidence specializedConstraints
      if hasLocalPolymorphicEvidence
        then throwError (ProgramAmbiguousConstrainedValueUse (valueInfoIdentityName valueInfo))
        else concat <$> mapM (constraintResolvedEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      (&& constraintCoveredByEvidenceInfo scope constraint)
        <$> constraintInfoHasFreeTypeBinderIdentities constraint
valueResolvedEvidenceArgsWithExpectedView _ _ _ _ = pure []

refineValueEvidenceViewSubst :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [arg] -> TypeViewSubst -> TypeViewSubst
refineValueEvidenceViewSubst scope valueInfo mbExpectedView args subst =
  case mbExpectedView >>= matchTypeViewAgainstIdentity scope subst resultViewForArity of
    Just subst' -> subst'
    Nothing -> subst
  where
    resultViewForArity =
      valueResultTypeViewForArity (ordinaryValueTypeView valueInfo) (length args)

valueResultTypeViewForArity :: TypeView -> Int -> TypeView
valueResultTypeViewForArity view argCount =
  view
    { typeViewDisplay = foldr STArrow displayResult (drop argCount displayArgs),
      typeViewIdentity = foldr STArrow identityResult (drop argCount identityArgs)
    }
  where
    (displayArgs, displayResult) = splitArrows (snd (splitForalls (typeViewDisplay view)))
    (identityArgs, identityResult) = splitArrows (snd (splitForalls (typeViewIdentity view)))

constraintEvidenceArgExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [SurfaceExpr]
constraintEvidenceArgExprsInfo scope constraint
  | shouldDeferConstraintEvidenceInfo scope constraint =
      deferConstraintEvidenceExprsInfo scope constraint
  | otherwise =
      resolveConstraintEvidenceExpr scope Set.empty constraint

constraintResolvedEvidenceArgExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [SurfaceExpr]
constraintResolvedEvidenceArgExprsInfo scope constraint = do
  shouldDefer <- shouldDeferResolvedConstraintEvidenceInfo scope constraint
  if shouldDefer
    then deferConstraintEvidenceExprsInfo scope constraint
    else resolveConstraintEvidenceExpr scope Set.empty constraint

shouldDeferConstraintEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
shouldDeferConstraintEvidenceInfo scope constraint =
  not (Set.null (freeTypeVarsTypeViews (constraintTypeViews constraint)))
    && not (constraintCoveredByEvidenceInfo scope constraint)

shouldDeferResolvedConstraintEvidenceInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM Bool
shouldDeferResolvedConstraintEvidenceInfo scope constraint =
  (&& not (constraintCoveredByEvidenceInfo scope constraint))
    <$> constraintInfoHasFreeTypeBinderIdentities constraint

constraintCoveredByEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
constraintCoveredByEvidenceInfo scope constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> False
    Just _ ->
      all ownConstraintCovered (constraintEvidenceClosureInfo scope constraint)
  where
    ownConstraintCovered (classInfo, evidenceConstraint)
      | Map.null (classMethodsByIdentity classInfo) =
          zeroMethodConstraintCoveredByEvidenceInfo scope evidenceConstraint
      | otherwise =
          all
            ( \methodInfo ->
                case lookupEvidenceMethodInfo scope evidenceConstraint (methodInfoSymbolIdentity methodInfo) of
                  Just _ -> True
                  Nothing -> False
            )
            (Map.elems (classMethodsByIdentity classInfo))

deferConstraintEvidenceExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [SurfaceExpr]
deferConstraintEvidenceExprsInfo scope constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
    Just _ ->
      concat <$> mapM deferOne (constraintEvidenceClosureInfo scope constraint)
  where
    deferOne (classInfo, evidenceConstraint)
      | Map.null (classMethodsByIdentity classInfo) =
          resolveZeroMethodEvidenceExpr scope Set.empty evidenceConstraint
      | otherwise =
          mapM (deferMethodEvidenceExpr scope (constraintTypeViews evidenceConstraint)) (Map.elems (classMethodsByIdentity classInfo))

deferMethodEvidenceExpr :: ElaborateScope -> NonEmpty TypeView -> MethodInfo -> ElaborateM SurfaceExpr
deferMethodEvidenceExpr scope classArgViews methodInfo = do
  let methodView = stripVacuousTypeViewForalls (specializeMethodTypeView methodInfo classArgViews)
      methodTy = typeViewDisplay methodView
      fullArity = methodFullArity methodInfo
      resultView = resultTypeView methodView
  placeholder <-
    if fullArity == 0
      then deferNullaryMethodCall scope methodInfo resultView
      else deferMethodCall scope methodInfo fullArity methodTy Nothing
  expanded <- etaExpandMissingArgs scope methodInfo methodTy Nothing 0 fullArity (surfaceVar placeholder)
  pure (surfaceAnn expanded (lowerTypeView scope methodView))
  where
    stripVacuousTypeViewForalls view =
      view
        { typeViewDisplay = stripVacuousSrcForalls (typeViewDisplay view),
          typeViewIdentity = stripVacuousSrcForalls (typeViewIdentity view)
        }

    resultTypeView view =
      view
        { typeViewDisplay = displayResult,
          typeViewIdentity = identityResult
        }
      where
        (_, displayBody) = splitForalls (typeViewDisplay view)
        (_, identityBody) = splitForalls (typeViewIdentity view)
        (_, displayResult) = splitArrows displayBody
        (_, identityResult) = splitArrows identityBody

inferCallSubst :: ElaborateScope -> TypeView -> [P.Expr] -> Maybe TypeViewSubst
inferCallSubst scope valueView args = do
  let knownPairs =
        [ (templateView, sourceTypeViewInScope scope actualTy)
          | (templateView, arg) <- zip (methodParamTypeViews valueView) args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  foldM (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView) Map.empty knownPairs

inferResolvedCallSubst :: ElaborateScope -> TypeView -> [P.ResolvedExpr] -> Maybe TypeViewSubst
inferResolvedCallSubst scope valueView args = do
  let knownPairs =
        [ (templateView, actualView)
          | (templateView, arg) <- zip (methodParamTypeViews valueView) args,
            Just actualView <- [inferKnownResolvedExprTypeView scope arg]
        ]
  foldM (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView) Map.empty knownPairs

inlineImmediateLetUse :: String -> P.Expr -> P.Expr -> Maybe P.Expr
inlineImmediateLetUse bindingName rhs body =
  let (headExpr, args) = collectApps body
   in case (headExpr, args) of
        (EVar name, _ : _)
          | name == bindingName,
            rhsConsumesAppliedArgs rhs (length args) ->
              Just (foldl EApp rhs args)
        _ -> Nothing

rhsConsumesAppliedArgs :: P.Expr -> Int -> Bool
rhsConsumesAppliedArgs _ 0 = True
rhsConsumesAppliedArgs expr argCount =
  case expr of
    ELam param body ->
      mentionsFreeValue (P.paramName param) body
        && rhsConsumesAppliedArgs body (argCount - 1)
    _ -> True

inlineImmediateResolvedLetUse :: LocalRef -> P.ResolvedExpr -> P.ResolvedExpr -> Maybe P.ResolvedExpr
inlineImmediateResolvedLetUse bindingRef rhs body =
  let (headExpr, args) = collectResolvedApps body
   in case (headExpr, args) of
        (EVar (P.ResolvedLocalValue name), _ : _)
          | name == bindingRef,
            resolvedRhsConsumesAppliedArgs rhs (length args) ->
              Just (foldl EApp rhs args)
        _ -> Nothing

resolvedRhsConsumesAppliedArgs :: P.ResolvedExpr -> Int -> Bool
resolvedRhsConsumesAppliedArgs _ 0 = True
resolvedRhsConsumesAppliedArgs expr argCount =
  case expr of
    ELam param body ->
      mentionsFreeResolvedValue (P.paramName param) body
        && resolvedRhsConsumesAppliedArgs body (argCount - 1)
    _ -> True

isLocalOrdinaryValue :: ValueInfo -> Bool
isLocalOrdinaryValue OrdinaryValue {valueInfoSymbol = identity} =
  symbolDefiningModule identity == "<local>"
isLocalOrdinaryValue _ = False

annotateExpectedValueUse :: ElaborateScope -> Maybe SrcType -> ValueInfo -> SurfaceExpr -> SurfaceExpr
annotateExpectedValueUse scope mbExpected valueInfo applied =
  case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        isRecursiveResultType expectedTy
          || isRecursiveResultType (lowerType scope expectedTy)
          || Builtins.srcTypeMentionsOpaqueBuiltin expectedTy ->
          surfaceAnn applied (lowerType scope expectedTy)
    _ -> applied

annotateExpectedBareValueUse :: ElaborateScope -> Maybe SrcType -> ValueInfo -> SurfaceExpr -> SurfaceExpr
annotateExpectedBareValueUse scope mbExpected valueInfo applied =
  case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        sourceTypeHasAppliedHead expectedTy ->
          surfaceAnn applied (lowerType scope expectedTy)
    _ -> applied

sourceTypeHasAppliedHead :: SrcType -> Bool
sourceTypeHasAppliedHead ty =
  case ty of
    STCon {} -> True
    STVarApp {} -> True
    STForall _ _ body -> sourceTypeHasAppliedHead body
    _ -> False

sourceTypeHasVariableHeadApplication :: SrcType -> Bool
sourceTypeHasVariableHeadApplication ty =
  case ty of
    STVar {} -> False
    STBase {} -> False
    STCon _ args -> any sourceTypeHasVariableHeadApplication args
    STVarApp {} -> True
    STTyLam _ body -> sourceTypeHasVariableHeadApplication body
    STTyApp fun arg -> sourceTypeHasVariableHeadApplication fun || sourceTypeHasVariableHeadApplication arg
    STArrow dom cod -> sourceTypeHasVariableHeadApplication dom || sourceTypeHasVariableHeadApplication cod
    STForall _ mb body -> maybe False (sourceTypeHasVariableHeadApplication . unSrcBound) mb || sourceTypeHasVariableHeadApplication body
    STMu _ body -> sourceTypeHasVariableHeadApplication body
    STBottom -> False

knownConstructorResultType :: ElaborateScope -> ConstructorInfo -> [P.Expr] -> Maybe SrcType
knownConstructorResultType scope ctorInfo args = do
  argTypes <- traverse (inferKnownExprType scope) args
  subst <- matchConstructorArgTypeViews scope ctorInfo argTypes
  pure (typeViewDisplay (applyTypeViewSubst subst (constructorVisibleResultTypeView scope ctorInfo)))

knownResolvedConstructorResultType :: ElaborateScope -> ConstructorInfo -> [P.ResolvedExpr] -> Maybe SrcType
knownResolvedConstructorResultType scope ctorInfo args =
  typeViewDisplay <$> knownResolvedConstructorResultTypeView scope ctorInfo args

knownResolvedConstructorResultTypeView :: ElaborateScope -> ConstructorInfo -> [P.ResolvedExpr] -> Maybe TypeView
knownResolvedConstructorResultTypeView scope ctorInfo args = do
  argViews <- traverse (inferKnownResolvedExprTypeView scope) args
  subst <-
    foldM
      (\acc (templateView, actualView) -> matchConstructorArgTypeViewSubst scope acc templateView actualView)
      Map.empty
      (zip (constructorArgTypeViews scope ctorInfo) argViews)
  pure (applyTypeViewSubst subst (constructorVisibleResultTypeView scope ctorInfo))

matchConstructorArgTypeViews :: ElaborateScope -> ConstructorInfo -> [SrcType] -> Maybe TypeViewSubst
matchConstructorArgTypeViews scope ctorInfo argTypes =
  foldM
    (\acc (templateView, actualTy) -> matchConstructorArgViewSubst scope acc templateView actualTy)
    Map.empty
    (zip (constructorArgTypeViews scope ctorInfo) argTypes)

matchConstructorArgTypeViewSubst :: ElaborateScope -> TypeViewSubst -> TypeView -> TypeView -> Maybe TypeViewSubst
matchConstructorArgTypeViewSubst scope subst templateView actualView =
  matchTypeViewAgainstIdentity scope subst templateView actualView

constructorVisibleResultTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorVisibleResultTypeView scope ctorInfo =
  resultView
    { typeViewDisplay =
        case resolveConstructorDataInfo scope ctorInfo of
          Just info -> visibleDataHeadType scope info
          Nothing -> typeViewDisplay resultView
    }
  where
    resultView = constructorResultTypeView scope ctorInfo

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM SurfaceExpr
compileMethodApp scope mbExpected methodInfo args
  | null args = compileNullaryMethodUse scope mbExpected methodInfo
  | otherwise = do
      let fullArity = methodFullArity methodInfo
          suppliedArity = length args
          mbExpectedResult =
            if suppliedArity >= fullArity
              then mbExpected
              else Nothing
          knownClassArgs = knownMethodClassArgs scope methodInfo args mbExpectedResult
          placeholderTy = placeholderMethodType scope methodInfo args mbExpectedResult
          knownArgTys =
            case knownClassArgs of
              Just _ -> Just (take suppliedArity (methodArgumentTypes placeholderTy))
              Nothing -> Nothing
      argSurfaces <-
        case knownArgTys of
          Just argTys -> zipWithM (compileExpectedMethodArg scope) argTys args
          Nothing -> mapM (compileExpr scope Nothing) args
      case knownClassArgs of
        Just classArgTys
          | shouldResolveMethodBeforeInference scope methodInfo classArgTys -> do
              methodHead <- resolveMethodHeadForCall scope Set.empty methodInfo classArgTys args
              let applied = foldl surfaceApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          when (NE.length (methodParamNames methodInfo) > 1) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          when (sourceTypeHasVariableHeadApplication placeholderTy) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          placeholder <- deferMethodCall scope methodInfo fullArity placeholderTy (sourceTypeViewInScope scope <$> mbExpectedResult)
          let applied = foldl surfaceApp (surfaceVar placeholder) argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpected of
            Just expectedTy -> surfaceAnn expanded (lowerType scope expectedTy)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> surfaceAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise = expanded

compileResolvedMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
compileResolvedMethodApp scope mbExpected methodInfo args
  | null args = compileNullaryMethodUse scope mbExpected methodInfo
  | otherwise = do
      let fullArity = methodFullArity methodInfo
          suppliedArity = length args
          mbExpectedResult =
            if suppliedArity >= fullArity
              then mbExpected
              else Nothing
          knownClassArgViews = knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResult
          placeholderTy = placeholderResolvedMethodType scope methodInfo args mbExpectedResult
          knownArgTys =
            case knownClassArgViews of
              Just _ -> Just (take suppliedArity (methodArgumentTypes placeholderTy))
              Nothing -> Nothing
      argSurfaces <-
        case knownArgTys of
          Just argTys -> zipWithM (compileExpectedResolvedMethodArg scope) argTys args
          Nothing -> mapM (compileResolvedExpr scope Nothing) args
      case knownClassArgViews of
        Just classArgViews
          | shouldResolveMethodBeforeInferenceViews scope methodInfo classArgViews -> do
              methodHead <- resolveResolvedMethodHeadForCall scope Set.empty methodInfo classArgViews args
              let applied = foldl surfaceApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          when (NE.length (methodParamNames methodInfo) > 1) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          when (sourceTypeHasVariableHeadApplication placeholderTy) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          placeholder <- deferMethodCall scope methodInfo fullArity placeholderTy (sourceTypeViewInScope scope <$> mbExpectedResult)
          let applied = foldl surfaceApp (surfaceVar placeholder) argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpected of
            Just expectedTy -> surfaceAnn expanded (lowerType scope expectedTy)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> surfaceAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise = expanded

compileNullaryMethodUse :: ElaborateScope -> Maybe SrcType -> MethodInfo -> ElaborateM SurfaceExpr
compileNullaryMethodUse scope mbExpected methodInfo =
  compileNullaryMethodUseWithView scope (sourceTypeViewInScope scope <$> mbExpected) methodInfo

compileNullaryMethodUseWithView :: ElaborateScope -> Maybe TypeView -> MethodInfo -> ElaborateM SurfaceExpr
compileNullaryMethodUseWithView scope mbExpected methodInfo =
  if NE.length (methodParamNames methodInfo) > 1
    then throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
    else case nullaryMethodExpectedResultView scope mbExpected methodInfo of
      Just expectedView -> do
        placeholder <- deferNullaryMethodCall scope methodInfo expectedView
        pure (surfaceVar placeholder)
      Nothing -> throwError (ProgramAmbiguousMethodUse (methodName methodInfo))

nullaryMethodExpectedResultView :: ElaborateScope -> Maybe TypeView -> MethodInfo -> Maybe TypeView
nullaryMethodExpectedResultView scope mbExpected methodInfo = do
  expectedView <- mbExpected
  _ <- inferNullaryMethodClassArgView scope methodInfo expectedView
  pure expectedView

inferNullaryMethodClassArgView :: ElaborateScope -> MethodInfo -> TypeView -> Maybe TypeView
inferNullaryMethodClassArgView scope methodInfo expectedView
  | methodFullArity methodInfo /= 0 = Nothing
  | otherwise = do
      subst <- matchMethodTypeViews scope Map.empty (methodResultTypeView methodInfo :| []) (expectedView :| [])
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

compileExpectedMethodArg :: ElaborateScope -> SrcType -> P.Expr -> ElaborateM SurfaceExpr
compileExpectedMethodArg scope expectedTy expr = do
  case inferKnownExprType scope expr of
    Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
    Nothing -> pure ()
  case expr of
    EAnn {} ->
      compileExpr scope (Just expectedTy) expr
    EApp (ELam param (EVar bodyName)) actual
      | bodyName == P.paramName param ->
          compileExpr scope (Just expectedTy) actual
    EApp (ELam param body) actual -> do
      runtimeName <- freshRuntimeName (P.paramName param)
      actualExpr <- compileExpr scope (Just expectedTy) actual
      scope' <- extendLocal scope (P.paramName param) runtimeName (Just expectedTy)
      bodyExpr <- compileExpr scope' (Just expectedTy) body
      pure (surfaceLet runtimeName actualExpr (surfaceAnn bodyExpr (lowerType scope expectedTy)))
    EVar name
      | Just ConstructorValue {} <- Map.lookup name (esValues scope) ->
          compileExpr scope (Just expectedTy) expr
    EVar name
      | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope),
        methodFullArity methodInfo == 0 ->
          compileExpr scope (Just expectedTy) expr
    EVar {} ->
      compileExpr scope Nothing expr
    _
      | (EVar name, _) <- collectApps expr,
        Just ConstructorValue {} <- Map.lookup name (esValues scope) ->
          compileExpr scope (Just expectedTy) expr
    _ -> do
      argExpr <- compileExpr scope Nothing expr
      pure (surfaceAnn argExpr (lowerType scope expectedTy))

compileExpectedResolvedMethodArg :: ElaborateScope -> SrcType -> P.ResolvedExpr -> ElaborateM SurfaceExpr
compileExpectedResolvedMethodArg scope expectedTy expr = do
  case inferKnownResolvedExprType scope expr of
    Just actualTy -> ensureSourceTypeCompatible scope expectedTy actualTy
    Nothing -> pure ()
  case expr of
    EAnn (EVar ref) _ ->
      compileResolvedExpr scope (Just expectedTy) (EVar ref)
    EAnn {} ->
      compileResolvedExpr scope (Just expectedTy) expr
    EApp (ELam param (EVar (P.ResolvedLocalValue bodyName))) actual
      | bodyName == P.paramName param ->
          compileResolvedExpr scope (Just expectedTy) actual
    EApp (ELam param body) actual -> do
      let paramRef = P.paramName param
      runtimeName <- freshRuntimeName (localRefName paramRef)
      actualExpr <- compileResolvedExpr scope (Just expectedTy) actual
      scope' <- extendResolvedLocal scope paramRef runtimeName (Just expectedTy)
      bodyExpr <- compileResolvedExpr scope' (Just expectedTy) body
      pure (surfaceLet runtimeName actualExpr (surfaceAnn bodyExpr (lowerType scope expectedTy)))
    EVar ref
      | Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExpr scope (Just expectedTy) expr
    EVar ref
      | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
        methodFullArity methodInfo == 0 ->
          compileResolvedExpr scope (Just expectedTy) expr
    EVar {} ->
      compileResolvedExpr scope Nothing expr
    _
      | (EVar ref, _) <- collectResolvedApps expr,
        Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExpr scope (Just expectedTy) expr
    _ -> do
      argExpr <- compileResolvedExpr scope Nothing expr
      pure (surfaceAnn argExpr (lowerType scope expectedTy))

ensureSourceTypeCompatible :: ElaborateScope -> SrcType -> SrcType -> ElaborateM ()
ensureSourceTypeCompatible scope expectedTy actualTy =
  when (sourceTypesNeedRejection scope expectedTy actualTy) $
    throwError (ProgramTypeMismatch actualTy expectedTy)

sourceTypesNeedRejection :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesNeedRejection scope expectedTy actualTy =
  not (sourceTypesCompatible scope expectedTy actualTy)
    || sourceTypesNeedNominalRejection scope expectedTy actualTy

sourceTypesCompatible :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesCompatible scope expectedTy actualTy =
  sourceTypesCompatibleMono scope expectedTy actualTy
    || sourceTypesCompatibleInstantiatingActual scope expectedTy actualTy

sourceTypesCompatibleMono :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesCompatibleMono scope expectedTy actualTy =
  matchTypesInScope scope Map.empty expectedTy actualTy /= Nothing
    || matchTypesInScope scope Map.empty actualTy expectedTy /= Nothing
    || lowerType scope expectedTy == lowerType scope actualTy

sourceTypesCompatibleInstantiatingActual :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesCompatibleInstantiatingActual scope expectedTy actualTy =
  case actualTy of
    STForall _ _ body -> sourceTypesCompatible scope expectedTy body
    _ -> False

sourceTypesNeedNominalRejection :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypesNeedNominalRejection scope expectedTy actualTy =
  sourceTypeMentionsVisibleData scope expectedTy
    && sourceTypeMentionsVisibleData scope actualTy
    && lowerType scope expectedTy == lowerType scope actualTy
    && matchTypesInScope scope Map.empty expectedTy actualTy == Nothing
    && matchTypesInScope scope Map.empty actualTy expectedTy == Nothing

sourceTypeMentionsVisibleData :: ElaborateScope -> SrcType -> Bool
sourceTypeMentionsVisibleData scope ty =
  case ty of
    STVar {} -> False
    STBase name -> sourceTypeHeadIsVisibleData scope name
    STCon name args ->
      sourceTypeHeadIsVisibleData scope name
        || any (sourceTypeMentionsVisibleData scope) args
    STVarApp _ args -> any (sourceTypeMentionsVisibleData scope) args
    STTyLam _ body -> sourceTypeMentionsVisibleData scope body
    STTyApp fun arg ->
      sourceTypeMentionsVisibleData scope fun
        || sourceTypeMentionsVisibleData scope arg
    STArrow dom cod ->
      sourceTypeMentionsVisibleData scope dom
        || sourceTypeMentionsVisibleData scope cod
    STForall _ mb body ->
      maybe False (sourceTypeMentionsVisibleData scope . unSrcBound) mb
        || sourceTypeMentionsVisibleData scope body
    STMu _ body -> sourceTypeMentionsVisibleData scope body
    STBottom -> False

sourceTypeHeadIsVisibleData :: ElaborateScope -> String -> Bool
sourceTypeHeadIsVisibleData scope name =
  case typeHeadIdentityInScope scope name of
    Just identity -> Map.member identity (esTypesByIdentity scope)
    Nothing -> False

resolveMethodHeadExprInfo :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> MethodInfo -> NonEmpty TypeView -> ElaborateM SurfaceExpr
resolveMethodHeadExprInfo scope seen methodInfo classArgViews =
  resolveMethodHeadExprInfoWith (pure . constraintInfoGroundByTypeBinderIdentitiesFailClosed) scope seen methodInfo classArgViews

resolveResolvedMethodHeadExprInfo :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> MethodInfo -> NonEmpty TypeView -> ElaborateM SurfaceExpr
resolveResolvedMethodHeadExprInfo scope seen methodInfo classArgViews =
  resolveMethodHeadExprInfoWith constraintInfoGroundByTypeBinderIdentities scope seen methodInfo classArgViews

resolveMethodHeadExprInfoWith ::
  (ConstraintInfo -> ElaborateM Bool) ->
  ElaborateScope ->
  Set (SymbolIdentity, [SrcType]) ->
  MethodInfo ->
  NonEmpty TypeView ->
  ElaborateM SurfaceExpr
resolveMethodHeadExprInfoWith groundPredicate scope seen methodInfo classArgViews =
  case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence ->
      pure (surfaceVar (evidenceMethodRuntimeName methodEvidence))
    Nothing -> do
      (instanceInfo, subst) <- liftEitherElab (resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews)
      case lookupInstanceMethod methodInfo instanceInfo of
        Just OrdinaryValue {valueRuntimeName = runtimeName, valueConstraintInfos = constraints} -> do
          eagerConstraints <-
            filterM
              groundPredicate
              (map (applyConstraintInfoSubst subst) constraints)
          evidenceArgs <-
            concat
              <$> mapM
                (resolveConstraintEvidenceExpr scope seen)
                eagerConstraints
          pure (foldl surfaceApp (surfaceVar runtimeName) evidenceArgs)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))

resolveMethodHeadForCall :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> MethodInfo -> NonEmpty SrcType -> [P.Expr] -> ElaborateM SurfaceExpr
resolveMethodHeadForCall scope seen methodInfo classArgTys args =
  let classArgViews = fmap (sourceTypeViewInScope scope) classArgTys
   in case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence -> do
      evidenceArgs <- methodLocalEvidenceArgsForCall scope methodInfo classArgTys args
      pure (foldl surfaceApp (surfaceVar (evidenceMethodRuntimeName methodEvidence)) evidenceArgs)
    Nothing -> resolveMethodHeadExprInfo scope seen methodInfo classArgViews

resolveResolvedMethodHeadForCall :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> MethodInfo -> NonEmpty TypeView -> [P.ResolvedExpr] -> ElaborateM SurfaceExpr
resolveResolvedMethodHeadForCall scope seen methodInfo classArgViews args =
  case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence -> do
      evidenceArgs <- methodLocalEvidenceArgsForResolvedCall scope methodInfo classArgViews args
      pure (foldl surfaceApp (surfaceVar (evidenceMethodRuntimeName methodEvidence)) evidenceArgs)
    Nothing -> resolveResolvedMethodHeadExprInfo scope seen methodInfo classArgViews

methodLocalEvidenceArgsForCall :: ElaborateScope -> MethodInfo -> NonEmpty SrcType -> [P.Expr] -> ElaborateM [SurfaceExpr]
methodLocalEvidenceArgsForCall scope methodInfo classArgTys args = do
  subst <-
    case inferMethodCallSubst scope methodInfo classArgTys args of
      Just subst0 -> pure subst0
      Nothing -> pure Map.empty
  let classArgViews = fmap (sourceTypeViewInScope scope) classArgTys
      classArgSubst = typeViewSubstFromParamIdentities (methodParamBinderIdentities methodInfo) classArgViews
      headVars = freeTypeBinderIdentitiesTypeViewsFailClosed classArgViews
      methodLocalConstraintInfos =
        filter
          (not . constraintInfoDeterminedByTypeBinderIdentitiesFailClosed headVars)
          (map (applyConstraintInfoSubst classArgSubst) (methodConstraintInfos methodInfo))
      specializedConstraints = map (applyConstraintInfoSubst subst) methodLocalConstraintInfos
  concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints

methodLocalEvidenceArgsForResolvedCall :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> [P.ResolvedExpr] -> ElaborateM [SurfaceExpr]
methodLocalEvidenceArgsForResolvedCall scope methodInfo classArgViews args = do
  subst <-
    case inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args of
      Just subst0 -> pure subst0
      Nothing -> pure Map.empty
  headVars <- freeTypeBinderIdentitiesTypeViewsOrThrow classArgViews
  methodLocalConstraintInfos <-
    filterM
      (fmap not . constraintInfoDeterminedByTypeBinderIdentities headVars)
      (map (applyConstraintInfoSubst classArgSubst) (methodConstraintInfos methodInfo))
  let specializedConstraints = map (applyConstraintInfoSubst subst) methodLocalConstraintInfos
  concat <$> mapM (constraintResolvedEvidenceArgExprsInfo scope) specializedConstraints
  where
    classArgSubst = typeViewSubstFromParamIdentities (methodParamBinderIdentities methodInfo) classArgViews

inferMethodCallSubst :: ElaborateScope -> MethodInfo -> NonEmpty SrcType -> [P.Expr] -> Maybe TypeViewSubst
inferMethodCallSubst scope methodInfo classArgTys args = do
  let classArgViews = fmap (sourceTypeViewInScope scope) classArgTys
      specializedMethodView = specializeMethodTypeView methodInfo classArgViews
      knownPairs =
        [ (templateView, sourceTypeViewInScope scope actualTy)
          | (templateView, arg) <- zip (methodParamTypeViews specializedMethodView) args,
            Just actualTy <- [inferKnownExprType scope arg]
        ]
  foldM (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView) Map.empty knownPairs

inferResolvedMethodCallSubstWithViews :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> [P.ResolvedExpr] -> Maybe TypeViewSubst
inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args = do
  let specializedMethodView = specializeMethodTypeView methodInfo classArgViews
      knownPairs =
        [ (templateView, actualView)
          | (templateView, arg) <- zip (methodParamTypeViews specializedMethodView) args,
            Just actualView <- [inferKnownResolvedExprTypeView scope arg]
        ]
  foldM (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView) Map.empty knownPairs

shouldResolveMethodBeforeInference :: ElaborateScope -> MethodInfo -> NonEmpty SrcType -> Bool
shouldResolveMethodBeforeInference scope methodInfo classArgTys =
  shouldResolveMethodBeforeInferenceViews scope methodInfo (fmap (sourceTypeViewInScope scope) classArgTys)

shouldResolveMethodBeforeInferenceViews :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> Bool
shouldResolveMethodBeforeInferenceViews scope methodInfo classArgViews =
  case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just _ -> True
    Nothing
      | NE.length classArgViews > 1 ->
          case resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews of
            Right _ -> True
            Left _ -> False
    Nothing
      | any (sourceTypeMentionsVisibleData scope) (typeViewsDisplay classArgViews) ->
          case resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews of
            Right _ -> True
            Left _ -> False
    Nothing
      | any Builtins.srcTypeMentionsOpaqueBuiltin (typeViewsDisplay classArgViews) ->
          case resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews of
            Right _ -> True
            Left _ -> False
    Nothing -> False

resolveConstraintEvidenceExpr :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> ConstraintInfo -> ElaborateM [SurfaceExpr]
resolveConstraintEvidenceExpr scope seen constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
    Just _ ->
      concat <$> mapM resolveOne (constraintEvidenceClosureInfo scope constraint)
  where
    resolveOne (classInfo, evidenceConstraint) = do
      let key = constraintEvidenceKey evidenceConstraint
      whenSeen key
      if Map.null (classMethodsByIdentity classInfo)
        then resolveZeroMethodEvidenceExpr scope seen evidenceConstraint
        else
          mapM
            ( \methodInfo ->
                resolveMethodHeadExprInfo
                  scope
                  (Set.insert key seen)
                  methodInfo
                  (constraintTypeViews evidenceConstraint)
            )
            (Map.elems (classMethodsByIdentity classInfo))

    whenSeen key =
      when (key `Set.member` seen) $
        throwError (noMatchingInstanceError scope constraint)

resolveZeroMethodEvidenceExpr :: ElaborateScope -> Set (SymbolIdentity, [SrcType]) -> ConstraintInfo -> ElaborateM [SurfaceExpr]
resolveZeroMethodEvidenceExpr scope seen constraint
  | zeroMethodConstraintCoveredByEvidenceInfo scope constraint = pure []
  | otherwise = do
      let key = constraintEvidenceKey constraint
      (instanceInfo, subst) <- liftEitherElab (resolveInstanceInfoByConstraint scope constraint)
      _ <-
        concat
          <$> mapM
            (resolveConstraintEvidenceExpr scope (Set.insert key seen) . applyConstraintInfoSubst subst)
            (instanceConstraintInfos instanceInfo)
      pure []

zeroMethodConstraintCoveredByEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidenceInfo scope constraint =
  any
    ( \evidence ->
        evidenceClassSymbol evidence == constraintClassSymbol constraint
          && case matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) (constraintTypeViews constraint) of
            Just _ -> True
            Nothing -> False
    )
    (esEvidence scope)

lookupEvidenceMethodInfo :: ElaborateScope -> ConstraintInfo -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodInfo scope constraint =
  lookupEvidenceMethodByClassViews scope (constraintClassSymbol constraint) (constraintTypeViews constraint)

lookupEvidenceMethodByClass :: ElaborateScope -> SymbolIdentity -> SrcType -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodByClass scope classIdentity0 headIdentityTy methodIdentity =
  lookupEvidenceMethodByClassTypes scope classIdentity0 (headIdentityTy :| []) methodIdentity

lookupEvidenceMethodByClassTypes :: ElaborateScope -> SymbolIdentity -> NonEmpty SrcType -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodByClassTypes scope classIdentity0 headIdentityTys methodIdentity =
  lookupEvidenceMethodByClassViews scope classIdentity0 (fmap identityView headIdentityTys) methodIdentity
  where
    identityView ty = mkTypeView ty ty

lookupEvidenceMethodByClassViews :: ElaborateScope -> SymbolIdentity -> NonEmpty TypeView -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodByClassViews scope classIdentity0 headViews methodIdentity =
  case
    [ methodEvidence
      | evidence <- esEvidence scope,
        evidenceClassSymbol evidence == classIdentity0,
        Just _ <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) headViews],
        methodEvidence <- maybe [] (: []) (Map.lookup methodIdentity (evidenceMethodsByIdentity evidence))
    ]
  of
    methodEvidence : _ -> Just methodEvidence
    [] -> Nothing

classInfoForConstraint :: ElaborateScope -> ConstraintInfo -> Maybe ClassInfo
classInfoForConstraint scope constraint =
  case Map.lookup (constraintClassSymbol constraint) (esClassesByIdentity scope) of
    Just classInfo -> Just classInfo
    Nothing -> Nothing

constraintEvidenceKey :: ConstraintInfo -> (SymbolIdentity, [SrcType])
constraintEvidenceKey constraint =
  (constraintClassSymbol constraint, NE.toList (typeViewsIdentity (constraintTypeViews constraint)))

noMatchingInstanceError :: ElaborateScope -> ConstraintInfo -> ProgramError
noMatchingInstanceError scope constraint =
  case fmap (diagnosticTypeViewDisplay scope) (constraintTypeViews constraint) of
    ty :| [] -> ProgramNoMatchingInstance (constraintDisplayClass constraint) ty
    tys -> ProgramNoMatchingInstanceHead (constraintDisplayClass constraint) (NE.toList tys)

noMatchingDisplayConstraintError :: P.ClassConstraint -> ProgramError
noMatchingDisplayConstraintError constraint =
  case P.constraintTypes constraint of
    ty :| [] -> ProgramNoMatchingInstance (P.constraintClassName constraint) ty
    tys -> ProgramNoMatchingInstanceHead (P.constraintClassName constraint) (NE.toList tys)

liftEitherElab :: Either ProgramError a -> ElaborateM a
liftEitherElab = either throwError pure

etaExpandMissingArgs :: ElaborateScope -> MethodInfo -> SrcType -> Maybe SrcType -> Int -> Int -> SurfaceExpr -> ElaborateM SurfaceExpr
etaExpandMissingArgs scope methodInfo methodTy mbExpected suppliedArity fullArity applied = do
  let missingArity = max 0 (fullArity - suppliedArity)
  if missingArity == 0
    then pure applied
    else do
      missingNames <- replicateM missingArity (freshRuntimeName (methodInfoStableName methodInfo ++ "_arg"))
      let missingTypes = zipWith preferExpectedType methodMissingTypes (expectedMissingTypes ++ repeat Nothing)
          body = foldl surfaceApp applied (map surfaceVar missingNames)
      pure (foldr wrapMissingArg body (zip missingNames missingTypes))
  where
    methodMissingTypes =
      drop suppliedArity (methodArgumentTypes methodTy)

    expectedMissingTypes =
      case mbExpected of
        Just expectedTy ->
          map Just (fst (splitArrows (snd (splitForalls expectedTy))))
        Nothing -> []

    preferExpectedType methodTy0 (Just expectedTy)
      | Set.null (freeTypeVarsSrcType expectedTy) = expectedTy
      | otherwise = methodTy0
    preferExpectedType methodTy0 Nothing = methodTy0

    wrapMissingArg (name, ty) body
      | Set.null (freeTypeVarsSrcType ty) = surfaceLamAnn name (lowerType scope ty) body
      | otherwise = surfaceLam name body

methodFullArity :: MethodInfo -> Int
methodFullArity methodInfo =
  length (methodArgumentTypes (methodType methodInfo))

methodArgumentTypes :: SrcType -> [SrcType]
methodArgumentTypes ty =
  let (_, bodyTy) = splitForalls ty
      (argTys, _) = splitArrows bodyTy
   in argTys

deferMethodCall :: ElaborateScope -> MethodInfo -> Int -> SrcType -> Maybe TypeView -> ElaborateM String
deferMethodCall scope methodInfo fullArity placeholderSourceTy mbExpectedResult = do
  placeholder <- freshDeferredMethodName (methodInfoStableName methodInfo)
  ref <- freshElaborateDeferredRef placeholder
  let placeholderTy = lowerType scope (preferVisibleSourceType scope placeholderSourceTy)
      placeholderHeadIdentities =
        mergeSymbolIdentityMaps
          [ maybe Map.empty typeViewHeadIdentities mbExpectedResult,
            sourceTypeHeadIdentitiesInScope scope placeholderTy
          ]
      placeholderBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ maybe Map.empty typeViewBinderIdentities mbExpectedResult,
            sourceTypeBinderIdentitiesInScope scope placeholderTy
          ]
      deferred =
        DeferredMethodCall
          { deferredMethodRef = ref,
            deferredMethodInfo = methodInfo,
            deferredMethodArgCount = fullArity,
            deferredMethodFullArity = fullArity,
            deferredMethodExpectedResult = mbExpectedResult,
            deferredMethodEvidence = Nothing,
            deferredMethodLocalEvidence = esEvidence scope
          }
  registerDeferredObligation (deferredPlaceholderView placeholderTy placeholderHeadIdentities placeholderBinderIdentities) (DeferredMethod deferred)
  pure placeholder

deferNullaryMethodCall :: ElaborateScope -> MethodInfo -> TypeView -> ElaborateM String
deferNullaryMethodCall scope methodInfo expectedView = do
  placeholder <- freshDeferredMethodName (methodInfoStableName methodInfo)
  ref <- freshElaborateDeferredRef placeholder
  let placeholderTy = lowerTypeView scope expectedView
      placeholderHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities expectedView,
            sourceTypeHeadIdentitiesInScope scope placeholderTy
          ]
      placeholderBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities expectedView,
            sourceTypeBinderIdentitiesInScope scope placeholderTy
          ]
      localEvidence = nullaryMethodEvidence scope methodInfo expectedView
      deferred =
        DeferredMethodCall
          { deferredMethodRef = ref,
            deferredMethodInfo = methodInfo,
            deferredMethodArgCount = 0,
            deferredMethodFullArity = 0,
            deferredMethodExpectedResult = Just expectedView,
            deferredMethodEvidence = localEvidence,
            deferredMethodLocalEvidence = esEvidence scope
          }
  registerDeferredObligation (deferredPlaceholderView placeholderTy placeholderHeadIdentities placeholderBinderIdentities) (DeferredMethod deferred)
  pure placeholder

nullaryMethodEvidence :: ElaborateScope -> MethodInfo -> TypeView -> Maybe DeferredMethodEvidence
nullaryMethodEvidence scope methodInfo expectedView = do
  classArgView <- inferNullaryMethodClassArgView scope methodInfo expectedView
  methodEvidence <-
    lookupEvidenceMethodByClassViews
      scope
      (methodInfoOwnerClassSymbolIdentity methodInfo)
      (classArgView :| [])
      (methodInfoSymbolIdentity methodInfo)
  pure
    DeferredMethodEvidence
      { deferredMethodEvidenceClassArg = classArgView,
        deferredMethodEvidenceClassArgs = classArgView :| [],
        deferredMethodEvidenceMethod = methodEvidence
      }

deferConstructorCall :: ElaborateScope -> ConstructorInfo -> Int -> TypeViewSubst -> ElaborateM String
deferConstructorCall scope ctorInfo argCount initialViewSubst = do
  placeholder <- freshDeferredConstructorName (constructorInfoStableName ctorInfo)
  ref <- freshElaborateDeferredRef placeholder
  let quantifiedTy = quantifyFreeTypeVars (ctorType ctorInfo)
      occurrenceTy = constructorOccurrenceType ctorInfo argCount
      occurrenceView = constructorOccurrenceTypeView scope ctorInfo argCount
      initialSubst = typeViewSubstDisplayTypes occurrenceView initialViewSubst
  instBinders <- liftEitherElab (constructorInstBinders scope ctorInfo quantifiedTy)
  let initialTypeBinderSubst = typeBinderSubstFromTypeViewSubst instBinders initialViewSubst
      placeholderSourceTy = specializeQuantifiedType initialSubst quantifiedTy
      loweredPlaceholderTy = lowerType scope placeholderSourceTy
      placeholderTy =
        if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
          && srcTypeHasVariableHeadApplication loweredPlaceholderTy
          then constructorStructuralPlaceholderType scope ctorInfo
          else loweredPlaceholderTy
      placeholderHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities (applyTypeViewSubst initialViewSubst occurrenceView),
            sourceTypeHeadIdentitiesInScope scope placeholderTy
          ]
      placeholderBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeBinderAliasIdentityMap instBinders,
            sourceTypeBinderIdentitiesInScope scope placeholderTy
          ]
      bindingMode = DeferredBindingMonomorphic
      deferred =
        DeferredConstructorCall
          { deferredConstructorRef = ref,
            deferredConstructorInfo = ctorInfo,
            deferredConstructorArgCount = argCount,
            deferredConstructorSourceType = placeholderSourceTy,
            deferredConstructorOccurrenceType = specializeSrcType initialSubst occurrenceTy,
            deferredConstructorTypeHeadIdentities = placeholderHeadIdentities,
            deferredConstructorInstBinders = instBinders,
            deferredConstructorInitialSubst = initialTypeBinderSubst,
            deferredConstructorBindingMode = bindingMode
          }
  registerDeferredObligation (deferredPlaceholderView placeholderTy placeholderHeadIdentities placeholderBinderIdentities) (DeferredConstructor deferred)
  pure placeholder

constructorInstBinders :: ElaborateScope -> ConstructorInfo -> SrcType -> Either ProgramError [(String, TypeBinderIdentity)]
constructorInstBinders scope ctorInfo quantifiedTy =
  traverse binderEntry (fst (splitForalls quantifiedTy))
  where
    binderEntry (name, _) =
      case Map.lookup name binderIdentities of
        Just identity -> Right (name, identity)
        Nothing ->
          Left (ProgramPipelineError ("constructor instantiation binder `" ++ name ++ "` is missing identity"))

    binderIdentities =
      mergeTypeBinderIdentityMaps
        [ typeViewBinderIdentities (constructorTypeView scope ctorInfo),
          explicitForallIdentities,
          ownerParamIdentities
        ]

    explicitForallIdentities =
      mergeTypeBinderIdentityMaps
        [ typeBinderAliasIdentityMap [(constructorForallDisplayName binder, identity)]
        | binder <- ctorForallBinderInfo ctorInfo,
          let identity = constructorForallIdentity binder
        ]

    ownerParamIdentities =
      case resolveConstructorDataInfo scope ctorInfo of
        Just dataInfo ->
          typeBinderAliasIdentityMap (dataParamBinders dataInfo)
        Nothing ->
          Map.empty

constructorStructuralPlaceholderType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorStructuralPlaceholderType scope ctorInfo =
  constructorStructuralPlaceholderTypeFor (elaborateScopeDataTypesByIdentity scope) ctorInfo

constructorStructuralPlaceholderTypeFor :: Map SymbolIdentity DataInfo -> ConstructorInfo -> SrcType
constructorStructuralPlaceholderTypeFor dataTypesByIdentity ctorInfo =
  foldr
    STArrow
    (STVar resultVar)
    (constructorStructuralArgs ctorInfo ++ map handlerType ownerShapes)
  where
    ownerShapes =
      case Map.lookup (ctorOwningTypeIdentity ctorInfo) dataTypesByIdentity of
        Just dataInfo -> map constructorShapeFromInfo (dataConstructors dataInfo)
        Nothing -> constructorOwnerShapes ctorInfo

    resultVar = constructorOwnerResultVar ctorInfo

    handlerType shape =
      constructorStructuralHandlerType resultVar shape

constructorOwnerResultVar :: ConstructorInfo -> String
constructorOwnerResultVar ctorInfo =
  "$" ++ symbolDefiningName (ctorOwningTypeIdentity ctorInfo) ++ "_result"

constructorStructuralHandlerType :: String -> ConstructorShape -> SrcType
constructorStructuralHandlerType resultVar shape =
  foldr STArrow (STVar resultVar) (constructorStructuralShapeArgs shape)

constructorStructuralArgs :: ConstructorInfo -> [SrcType]
constructorStructuralArgs ctor =
  constructorStructuralArgsFor (constructorInfoIdentityName ctor) (length (ctorArgs ctor))

constructorStructuralShapeArgs :: ConstructorShape -> [SrcType]
constructorStructuralShapeArgs shape =
  constructorStructuralArgsFor (constructorShapeName shape) (length (constructorShapeArgs shape))

constructorStructuralArgsFor :: String -> Int -> [SrcType]
constructorStructuralArgsFor name arity =
  [ STVar ("$" ++ name ++ "_arg" ++ show ix ++ "_type")
    | ix <- [1 .. arity]
  ]

constructorOccurrenceType :: ConstructorInfo -> Int -> SrcType
constructorOccurrenceType ctorInfo argCount =
  foldr STArrow (ctorResult ctorInfo) (drop argCount (ctorArgs ctorInfo))

constructorInfoWithArgs :: ConstructorInfo -> [SrcType] -> ConstructorInfo
constructorInfoWithArgs ctorInfo args =
  ctorInfo
    { ctorTypeView =
        (ctorTypeView ctorInfo)
          { typeViewDisplay =
              foldr
                (\(name, mbBound) body -> STForall name (fmap SrcBound mbBound) body)
                (foldr STArrow (ctorResult ctorInfo) args)
                (ctorForalls ctorInfo)
          }
    }

constructorOccurrenceTypeView :: ElaborateScope -> ConstructorInfo -> Int -> TypeView
constructorOccurrenceTypeView scope ctorInfo argCount =
  let view = constructorTypeView scope ctorInfo
      (displayArgs, displayResult) = splitArrows (snd (splitForalls (typeViewDisplay view)))
      (identityArgs, identityResult) = splitArrows (snd (splitForalls (typeViewIdentity view)))
   in view
        { typeViewDisplay = foldr STArrow displayResult (drop argCount displayArgs),
          typeViewIdentity = foldr STArrow identityResult (drop argCount identityArgs)
        }

constructorArgTypeViews :: ElaborateScope -> ConstructorInfo -> [TypeView]
constructorArgTypeViews scope ctorInfo =
  let view = constructorTypeView scope ctorInfo
      (displayArgs, _) = splitArrows (snd (splitForalls (typeViewDisplay view)))
      (identityArgs, _) = splitArrows (snd (splitForalls (typeViewIdentity view)))
   in zipWith
        ( \displayTy identityTy ->
            view
              { typeViewDisplay = displayTy,
                typeViewIdentity = identityTy
              }
        )
        displayArgs
        identityArgs

constructorResultTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorResultTypeView scope ctorInfo =
  let view = constructorTypeView scope ctorInfo
      (_, displayResult) = splitArrows (snd (splitForalls (typeViewDisplay view)))
      (_, identityResult) = splitArrows (snd (splitForalls (typeViewIdentity view)))
   in view
        { typeViewDisplay = displayResult,
          typeViewIdentity = identityResult
        }

constructorTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorTypeView scope ctorInfo =
  let view = ctorTypeView ctorInfo
   in view
        { typeViewHeadIdentities =
            mergeSymbolIdentityMaps
              [ typeViewHeadIdentities view,
                sourceTypeHeadIdentitiesInScope scope (typeViewIdentity view)
              ],
          typeViewBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ typeViewBinderIdentities view,
                constructorBinderIdentities scope ctorInfo
              ]
        }

constructorBinderIdentities :: ElaborateScope -> ConstructorInfo -> Map String TypeBinderIdentity
constructorBinderIdentities scope ctorInfo =
  mergeTypeBinderIdentityMaps (ownerParamIdentities ++ forallIdentities)
  where
    ownerParamIdentities =
      case resolveConstructorDataInfo scope ctorInfo of
        Just dataInfo ->
          [typeBinderAliasIdentityMap (dataParamBinders dataInfo)]
        Nothing -> []

    forallIdentities =
      [ typeBinderAliasIdentityMap [(constructorForallDisplayName binder, identity)]
      | binder <- ctorForallBinderInfo ctorInfo,
        let identity = constructorForallIdentity binder
      ]

specializeQuantifiedType :: Map String SrcType -> SrcType -> SrcType
specializeQuantifiedType subst ty =
  let (foralls, body) = splitForalls ty
      kept =
        [ (name, fmap (specializeSrcType subst) mb)
          | (name, mb) <- foralls,
            Map.notMember name subst
        ]
   in foldr
        (\(name, mb) acc -> STForall name (fmap SrcBound mb) acc)
        (specializeSrcType subst body)
        kept

specializeSrcType :: Map String SrcType -> SrcType -> SrcType
specializeSrcType subst ty = case ty of
  STVar name -> Map.findWithDefault ty name subst
  STArrow dom cod -> STArrow (specializeSrcType subst dom) (specializeSrcType subst cod)
  STBase {} -> ty
  STCon name args -> STCon name (fmap (specializeSrcType subst) args)
  STVarApp name args ->
    let args' = fmap (specializeSrcType subst) args
     in case Map.lookup name subst >>= \replacement -> applyTypeHead replacement (toListNE args') of
          Just replacementTy -> replacementTy
          Nothing -> STVarApp name args'
  STTyLam name body ->
    STTyLam name (specializeSrcType (Map.delete name subst) body)
  STTyApp fun arg -> STTyApp (specializeSrcType subst fun) (specializeSrcType subst arg)
  STForall name mb body
    | Map.member name subst -> STForall name mb body
    | otherwise ->
        STForall name (fmap (SrcBound . specializeSrcType subst . unSrcBound) mb) (specializeSrcType subst body)
  STMu name body
    | Map.member name subst -> STMu name body
    | otherwise -> STMu name (specializeSrcType subst body)
  STBottom -> STBottom

deferCaseCall :: ElaborateScope -> DataInfo -> SrcType -> SrcType -> ElaborateM String
deferCaseCall scope dataInfo scrutineeTy resultTy = do
  placeholder <- freshDeferredCaseName (dataInfoIdentityHeadName dataInfo)
  ref <- freshElaborateDeferredRef placeholder
  let resultTyElab = lowerType scope resultTy
      handlerTys =
        [ handlerSurfaceType
            scope
            (constructorInfoWithArgs ctorInfo (specializeConstructorArgsForScrutineeType scope scrutineeTy ctorInfo))
            resultTyElab
        | ctorInfo <- dataConstructors dataInfo
        ]
      placeholderTy = foldr STArrow resultTyElab (lowerType scope scrutineeTy : handlerTys)
      placeholderBinderIdentities =
        sourceTypeBinderIdentitiesInScope scope placeholderTy
      deferred =
        DeferredCaseCall
          { deferredCaseRef = ref,
            deferredCaseDataInfo = dataInfo,
            deferredCaseScrutineeType = scrutineeTy,
            deferredCaseResultType = resultTy,
            deferredCaseExpectedArgCount = 1 + length handlerTys
          }
  registerDeferredObligation (deferredPlaceholderView placeholderTy (sourceTypeHeadIdentitiesInScope scope placeholderTy) placeholderBinderIdentities) (DeferredCase deferred)
  pure placeholder

registerDeferredObligation :: TypeView -> DeferredProgramObligation -> ElaborateM ()
registerDeferredObligation placeholderView obligation =
  modify
    ( \state ->
        let ref = deferredProgramObligationRef obligation
            placeholder = deferredRefName ref
         in
        state
          { elaborateDeferredObligations = Map.insert ref obligation (elaborateDeferredObligations state),
            elaborateExternalTypeViews = Map.insert placeholder placeholderView (elaborateExternalTypeViews state)
          }
    )

deferredPlaceholderView :: SrcType -> Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeView
deferredPlaceholderView placeholderTy headIdentities binderIdentities =
  TypeView
    { typeViewDisplay = placeholderTy,
      typeViewIdentity = placeholderTy,
      typeViewHeadIdentities = headIdentities,
      typeViewBinderIdentities = binderIdentities
    }

placeholderMethodType :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType -> SrcType
placeholderMethodType scope methodInfo args mbExpectedResult =
  let quantifiedMethodTy = quantifiedMethodType methodInfo
      knownClassArgViews = knownMethodClassArgViews scope methodInfo args mbExpectedResult
   in case knownClassArgViews of
        Just classArgViews ->
          let classArgTys = typeViewsIdentity classArgViews
              specializedTy = stripVacuousSrcForalls (typeViewDisplay (specializeMethodTypeView methodInfo classArgViews))
              callSubst =
                case inferMethodCallSubst scope methodInfo classArgTys args of
                  Just subst -> typeViewSubstDisplayTypes (methodTypeView methodInfo) subst
                  Nothing -> Map.empty
           in stripVacuousSrcForalls (specializeQuantifiedType callSubst specializedTy)
        Nothing -> quantifiedMethodTy

placeholderResolvedMethodType :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> Maybe SrcType -> SrcType
placeholderResolvedMethodType scope methodInfo args mbExpectedResult =
  let quantifiedMethodTy = quantifiedMethodType methodInfo
      knownClassArgViews = knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResult
   in case knownClassArgViews of
        Just classArgViews ->
          let specializedTy = stripVacuousSrcForalls (typeViewDisplay (specializeMethodTypeView methodInfo classArgViews))
              callSubst =
                case inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args of
                  Just subst -> typeViewSubstDisplayTypes (methodTypeView methodInfo) subst
                  Nothing -> Map.empty
           in stripVacuousSrcForalls (specializeQuantifiedType callSubst specializedTy)
        Nothing -> quantifiedMethodTy

knownMethodClassArgs :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType -> Maybe (NonEmpty SrcType)
knownMethodClassArgs scope methodInfo args mbExpectedResult =
  typeViewsIdentity <$> knownMethodClassArgViews scope methodInfo args mbExpectedResult

knownMethodClassArgViews :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType -> Maybe (NonEmpty TypeView)
knownMethodClassArgViews scope methodInfo args mbExpectedResult =
  knownMethodClassArgViewsFromArgViews scope methodInfo argViews
    <|> knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews mbExpectedResult
  where
    argViews = map (fmap (sourceTypeViewInScope scope) . inferKnownExprType scope) args

knownResolvedMethodClassArgViews :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> Maybe SrcType -> Maybe (NonEmpty TypeView)
knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResult =
  knownMethodClassArgViewsFromArgViews scope methodInfo argViews
    <|> knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews mbExpectedResult
    <|> fmap (fmap (sourceTypeViewInScope scope)) (knownMethodClassArgsFromArgs scope methodInfo argTypes)
    <|> fmap (fmap (sourceTypeViewInScope scope)) (knownMethodClassArgsFromExpected scope methodInfo argTypes mbExpectedResult)
  where
    argViews = map (inferKnownResolvedExprTypeView scope) args
    argTypes = map (inferKnownResolvedExprType scope) args

knownMethodClassArgsFromArgs :: ElaborateScope -> MethodInfo -> [Maybe SrcType] -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromArgs =
  knownMethodClassArgsFromArgsByView

knownMethodClassArgsFromArgsByView :: ElaborateScope -> MethodInfo -> [Maybe SrcType] -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromArgsByView scope methodInfo argTypes = do
  knownMethodClassArgsFromArgViews scope methodInfo (map (fmap (sourceTypeViewInScope scope)) argTypes)

knownMethodClassArgsFromArgViews :: ElaborateScope -> MethodInfo -> [Maybe TypeView] -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromArgViews scope methodInfo argViews = do
  typeViewsIdentity <$> knownMethodClassArgViewsFromArgViews scope methodInfo argViews

knownMethodClassArgViewsFromArgViews :: ElaborateScope -> MethodInfo -> [Maybe TypeView] -> Maybe (NonEmpty TypeView)
knownMethodClassArgViewsFromArgViews scope methodInfo argViews = do
  let methodView = methodTypeView methodInfo
      knownPairs =
        [ (templateView, actualView)
          | (templateView, mbActualView) <- zip (methodParamTypeViews methodView) argViews,
            Just actualView <- [mbActualView]
        ]
  subst <- foldM (\acc (templateView, actualView) -> matchMethodTypeView scope acc templateView actualView) Map.empty knownPairs
  lookupMethodClassArgViews scope methodInfo subst

knownMethodClassArgsFromExpected :: ElaborateScope -> MethodInfo -> [Maybe SrcType] -> Maybe SrcType -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromExpected _ _ _ Nothing = Nothing
knownMethodClassArgsFromExpected scope methodInfo argTypes (Just expectedTy) =
  knownMethodClassArgsFromExpectedByView scope methodInfo argTypes expectedTy

knownMethodClassArgsFromExpectedByView :: ElaborateScope -> MethodInfo -> [Maybe SrcType] -> SrcType -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromExpectedByView scope methodInfo argTypes expectedTy = do
  knownMethodClassArgsFromExpectedViews scope methodInfo (map (fmap (sourceTypeViewInScope scope)) argTypes) (Just expectedTy)

knownMethodClassArgsFromExpectedViews :: ElaborateScope -> MethodInfo -> [Maybe TypeView] -> Maybe SrcType -> Maybe (NonEmpty SrcType)
knownMethodClassArgsFromExpectedViews _ _ _ Nothing = Nothing
knownMethodClassArgsFromExpectedViews scope methodInfo argViews (Just expectedTy) = do
  typeViewsIdentity <$> knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews (Just expectedTy)

knownMethodClassArgViewsFromExpectedViews :: ElaborateScope -> MethodInfo -> [Maybe TypeView] -> Maybe SrcType -> Maybe (NonEmpty TypeView)
knownMethodClassArgViewsFromExpectedViews _ _ _ Nothing = Nothing
knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews (Just expectedTy) = do
  let methodView = methodTypeView methodInfo
      knownPairs =
        [ (templateView, actualView)
          | (templateView, mbActualView) <- zip (methodParamTypeViews methodView) argViews,
            Just actualView <- [mbActualView]
        ]
  substFromArgs <- foldM (\acc (templateView, actualView) -> matchMethodTypeView scope acc templateView actualView) Map.empty knownPairs
  subst <- matchMethodTypeView scope substFromArgs (methodResultTypeView methodInfo) (sourceTypeViewInScope scope expectedTy)
  lookupMethodClassArgViews scope methodInfo subst

matchMethodTypeViews :: ElaborateScope -> TypeViewSubst -> NonEmpty TypeView -> NonEmpty TypeView -> Maybe TypeViewSubst
matchMethodTypeViews scope subst templates actuals
  | NE.length templates /= NE.length actuals = Nothing
  | otherwise =
      foldM
        (\acc (template, actual) -> matchMethodTypeView scope acc template actual)
        subst
        (zip (NE.toList templates) (NE.toList actuals))

matchMethodTypeView :: ElaborateScope -> TypeViewSubst -> TypeView -> TypeView -> Maybe TypeViewSubst
matchMethodTypeView scope subst template actual =
  matchTypeViewAgainstIdentity scope subst template actual
    <|> do
      matched <-
        matchTypesInScope
          scope
          (typeViewSubstDisplayTypes template subst)
          (typeViewDisplay template)
          (typeViewDisplay actual)
      pure (sourceTypeViewSubstForTemplateInScope scope template matched `Map.union` subst)

lookupMethodClassArgViews :: ElaborateScope -> MethodInfo -> TypeViewSubst -> Maybe (NonEmpty TypeView)
lookupMethodClassArgViews scope methodInfo subst = do
  closedSubst <-
    case classInfoForMethod scope methodInfo of
      Just classInfo -> closeFunctionalDependencies scope classInfo subst
      Nothing -> Just subst
  lookupMethodParamViewSubst methodInfo closedSubst

classInfoForMethod :: ElaborateScope -> MethodInfo -> Maybe ClassInfo
classInfoForMethod scope methodInfo =
  case Map.lookup (methodInfoOwnerClassSymbolIdentity methodInfo) (esClassesByIdentity scope) of
    Just classInfo -> Just classInfo
    Nothing -> Nothing

closeFunctionalDependencies :: ElaborateScope -> ClassInfo -> TypeViewSubst -> Maybe TypeViewSubst
closeFunctionalDependencies scope classInfo subst0 =
  go maxFuel subst0
  where
    maxFuel = max 1 (length (classFunctionalDependencies classInfo) * max 1 (length (esInstances scope) + length (esEvidence scope)) + 1)

    go fuel subst
      | fuel <= 0 = Nothing
      | otherwise = do
          (subst', changed) <- foldM closeOne (subst, False) (classFunctionalDependencies classInfo)
          if changed
            then go (fuel - 1) subst'
            else Just subst'

    closeOne (subst, changed) fundep =
      case functionalDependencyRefsReady fundep subst of
        Nothing -> Just (subst, changed)
        Just (determiners, determined) ->
          case determinedCandidates determiners determined subst of
            [] -> Just (subst, changed)
            candidate : rest
              | all (sameDeterminedCandidate candidate) rest -> do
                  (subst', changed') <- mergeDeterminedSubst subst determined candidate
                  Just (subst', changed || changed')
              | otherwise -> Nothing

    functionalDependencyRefsReady fundep subst = do
      let determiners = functionalDependencyDeterminerRefs fundep
          determined = functionalDependencyDeterminedRefs fundep
      _ <- traverse (`lookupClassParamView` subst) determiners
      pure (determiners, determined)

    determinedCandidates determiners determined subst =
      deduplicateDeterminedCandidates
        [ candidate
          | headViews <- candidateClassHeadViews,
            Just matchSubst <- [matchDeterminers determiners subst headViews],
            Just determinedViews <- [projectClassHeadViews determined headViews],
            let candidate = fmap (applyTypeViewSubst matchSubst) determinedViews,
            typeViewsClosedByTypeBinderIdentities candidate
        ]

    candidateClassHeadViews =
      [ instanceHeadTypeViews info
        | info <- esInstances scope,
          instanceInfoClassSymbolIdentity info == classInfoSymbolIdentity classInfo
      ]
        ++ [ evidenceTypeViews evidence
             | evidence <- esEvidence scope,
               evidenceClassSymbol evidence == classInfoSymbolIdentity classInfo
           ]

    matchDeterminers determiners subst headViews =
      do
        determinerViews <- projectClassHeadViews determiners headViews
        actualViews <- traverse (`lookupClassParamView` subst) determiners
        matchTypeViewsAgainstIdentity
          scope
          Map.empty
          determinerViews
          actualViews

    mergeDeterminedSubst subst refs views = do
      foldM mergeOne (subst, False) (zip (NE.toList refs) (NE.toList views))

    mergeOne (subst, changed) (identity, view) = do
      _ <- classParamNameForIdentity identity
      let key = typeViewSubstKeyForIdentity identity
      case lookupTypeViewSubst key subst of
        Just existing
          | semanticTypeViewEqual scope existing view -> Just (subst, changed)
          | otherwise -> Nothing
        Nothing -> Just (insertTypeViewSubst key view subst, True)

    sameDeterminedCandidate left right =
      length left == length right
        && and
          [ semanticTypeViewEqual scope leftView rightView
            | (leftView, rightView) <- zip (NE.toList left) (NE.toList right)
          ]

    deduplicateDeterminedCandidates [] = []
    deduplicateDeterminedCandidates (candidate : rest) =
      candidate : deduplicateDeterminedCandidates (filter (not . sameDeterminedCandidate candidate) rest)

    classParamNameForIdentity identity =
      Map.lookup identity classParamNamesByIdentity

    lookupClassParamView identity subst = do
      _ <- classParamNameForIdentity identity
      lookupTypeViewSubst (typeViewSubstKeyForIdentity identity) subst

    classParamNamesByIdentity =
      Map.fromList
        [ (identity, name)
        | (name, identity) <- zip (NE.toList (classParamNames classInfo)) (NE.toList (classParamBinderIdentities classInfo))
        ]

    classParamIndicesByIdentity =
      Map.fromList
        [ (identity, ix)
        | (identity, ix) <- zip (NE.toList (classParamBinderIdentities classInfo)) [(0 :: Int) ..]
        ]

    projectClassHeadViews refs headViews = do
      indices <- traverse (`Map.lookup` classParamIndicesByIdentity) refs
      let values = NE.toList headViews
      pure (fmap (values !!) indices)

    typeViewsClosedByTypeBinderIdentities views =
      case freeTypeBinderIdentitiesTypeViews views of
        Right identities -> Set.null identities
        Left _ -> False

quantifiedMethodType :: MethodInfo -> SrcType
quantifiedMethodType methodInfo =
  let methodTy = methodType methodInfo
      (foralls, _) = splitForalls methodTy
      quantifiedNames = Set.fromList (map fst foralls)
      freeVars = freeTypeVarsSrcType methodTy
      addParam paramName acc
        | paramName `Set.member` quantifiedNames = acc
        | paramName `Set.notMember` freeVars = acc
        | otherwise = STForall paramName Nothing acc
   in foldr addParam methodTy (methodParamNames methodInfo)

inferKnownExprType :: ElaborateScope -> P.Expr -> Maybe SrcType
inferKnownExprType =
  inferKnownExprTypeWithLocals Map.empty

inferKnownExprTypeWithLocals :: Map String SrcType -> ElaborateScope -> P.Expr -> Maybe SrcType
inferKnownExprTypeWithLocals locals scope expr =
  case expr of
    ELit lit -> Just (litSrcType lit)
    EVar name ->
      Map.lookup name locals
        <|> case Map.lookup name (esValues scope) of
          Just valueInfo@OrdinaryValue {} -> Just (ordinaryValueTypeInScope scope valueInfo)
          Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just (constructorVisibleType scope ctorInfo)
          _ -> Nothing
    ELam param body -> do
      paramTy <- P.paramType param
      STArrow paramTy <$> inferKnownExprTypeWithLocals (Map.insert (P.paramName param) paramTy locals) scope body
    EAnn _ annTy -> Just annTy
    EApp _ _ ->
      case collectApps expr of
        (EVar name, args)
          | Just valueInfo <- Map.lookup name (esValues scope) ->
              case valueInfo of
                OrdinaryValue {}
                  | let ty = ordinaryValueTypeInScope scope valueInfo,
                    not (null args),
                    hasLeadingForall ty ->
                      appliedKnownOrdinaryValueResultType
                        scope
                        ty
                        (map (inferKnownExprTypeWithLocals locals scope) args)
                        (length args)
                ConstructorValue {valueCtorInfo = ctorInfo}
                  | length args == length (ctorArgs ctorInfo) ->
                      knownConstructorResultType scope ctorInfo args
                _ -> appliedValueResultType scope valueInfo (length args)
        _ -> Nothing
    _ -> Nothing

inferKnownResolvedExprType :: ElaborateScope -> P.ResolvedExpr -> Maybe SrcType
inferKnownResolvedExprType scope expr =
  case expr of
    ELit lit -> Just (litSrcType lit)
    EVar ref ->
      case runElaborateLookup (lookupResolvedValueInfo scope ref) of
        Right valueInfo@OrdinaryValue {} -> Just (ordinaryValueTypeInScope scope valueInfo)
        Right ConstructorValue {valueCtorInfo = ctorInfo} -> Just (constructorVisibleType scope ctorInfo)
        _ -> Nothing
    ELam param body -> do
      paramTy <- P.paramType param >>= either (const Nothing) Just . displaySrcTypeForResolved scope
      let paramRef = P.paramName param
          scope' = extendResolvedLocalSourceTypePure scope paramRef (localRefName paramRef) paramTy
      STArrow paramTy <$> inferKnownResolvedExprType scope' body
    EAnn _ annTy ->
      either (const Nothing) Just (displaySrcTypeForResolved scope annTy)
    EApp _ _ ->
      case collectResolvedApps expr of
        (EVar ref, args)
          | Right valueInfo <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
              case valueInfo of
                OrdinaryValue {}
                  | let ty = ordinaryValueTypeInScope scope valueInfo,
                    not (null args),
                    hasLeadingForall ty ->
                      appliedKnownOrdinaryValueResultType scope ty (map (inferKnownResolvedExprType scope) args) (length args)
                ConstructorValue {valueCtorInfo = ctorInfo}
                  | length args == length (ctorArgs ctorInfo) ->
                      knownResolvedConstructorResultType scope ctorInfo args
                _ -> appliedValueResultType scope valueInfo (length args)
        _ -> Nothing
    _ -> Nothing

inferKnownResolvedExprTypeView :: ElaborateScope -> P.ResolvedExpr -> Maybe TypeView
inferKnownResolvedExprTypeView scope expr =
  case expr of
    EAnn _ annTy ->
      either (const Nothing) Just (resolvedTypeViewForScope scope annTy)
    EVar ref ->
      case runElaborateLookup (lookupResolvedValueInfo scope ref) of
        Right valueInfo@OrdinaryValue {} ->
          Just (ordinaryValueTypeView valueInfo) {typeViewDisplay = ordinaryValueTypeInScope scope valueInfo}
        Right ConstructorValue {valueCtorInfo = ctorInfo} ->
          Just (constructorTypeView scope ctorInfo) {typeViewDisplay = constructorVisibleType scope ctorInfo}
        _ -> Nothing
    EApp _ _ ->
      case collectResolvedApps expr of
        (EVar ref, args)
          | Right ConstructorValue {valueCtorInfo = ctorInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
            length args == length (ctorArgs ctorInfo) ->
              knownResolvedConstructorResultTypeView scope ctorInfo args
        _ -> sourceTypeViewInScope scope <$> inferKnownResolvedExprType scope expr
    _ -> sourceTypeViewInScope scope <$> inferKnownResolvedExprType scope expr

hasLeadingForall :: SrcType -> Bool
hasLeadingForall ty =
  case ty of
    STForall {} -> True
    _ -> False

appliedKnownOrdinaryValueResultType :: ElaborateScope -> SrcType -> [Maybe SrcType] -> Int -> Maybe SrcType
appliedKnownOrdinaryValueResultType scope visibleTy mbArgTypes argCount = do
  let (_, bodyTy) = splitForalls visibleTy
      (argTys, _) = splitArrows bodyTy
  if argCount > length argTys
    then Nothing
    else do
      actualArgTypes <- sequence (take argCount mbArgTypes)
      subst <-
        foldM
          (\acc (templateTy, actualTy) -> matchTypesInScope scope acc templateTy actualTy)
          Map.empty
          (zip argTys actualArgTypes)
      peelAppliedType (specializeQuantifiedType subst visibleTy) argCount

litSrcType :: Lit -> SrcType
litSrcType lit =
  case lit of
    LInt _ -> STBase "Int"
    LBool _ -> STBase "Bool"
    LChar _ -> STBase "Char"
    LString _ -> STBase "String"

appliedValueResultType :: ElaborateScope -> ValueInfo -> Int -> Maybe SrcType
appliedValueResultType scope valueInfo argCount =
  case valueInfo of
    OrdinaryValue {} -> peelAppliedType (ordinaryValueTypeInScope scope valueInfo) argCount
    ConstructorValue {valueCtorInfo = ctorInfo} ->
      if argCount > length (ctorArgs ctorInfo)
        then Nothing
        else peelAppliedType (constructorVisibleType scope ctorInfo) argCount
    OverloadedMethod {} -> Nothing

constructorVisibleType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorVisibleType scope ctorInfo =
  rewriteSrcTypeOccurrences (ctorResult ctorInfo) visibleResult (ctorType ctorInfo)
  where
    visibleResult =
      case resolveConstructorDataInfo scope ctorInfo of
        Just info -> visibleDataHeadType scope info
        Nothing -> ctorResult ctorInfo

peelAppliedType :: SrcType -> Int -> Maybe SrcType
peelAppliedType ty argCount =
  let (_, bodyTy) = splitForalls ty
      (argTys, resultTy) = splitArrows bodyTy
   in if argCount > length argTys
        then Nothing
        else Just (foldr STArrow resultTy (drop argCount argTys))

compileCase :: ElaborateScope -> Maybe SrcType -> P.Expr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCase scope mbExpected scrutinee alts = do
  case ctorOwners alts of
    [] -> do
      let mbInferredScrutineeTy = inferKnownExprType scope scrutinee
          mbAnnotationScrutineeTy = catchAllPatternAnnotationType alts
          mbScrutineeTy =
            case mbInferredScrutineeTy of
              Just knownTy -> Just knownTy
              Nothing -> mbAnnotationScrutineeTy
          annotateScrutinee =
            case (mbInferredScrutineeTy, mbAnnotationScrutineeTy) of
              (Nothing, Just annTy) -> Just annTy
              _ -> Nothing
      rejectOpaqueBuiltinCase scope mbScrutineeTy
      mapM_ (\scrutineeTy -> mapM_ (validatePatternType scope scrutineeTy . P.altPattern) alts) mbScrutineeTy
      scrutineeExpr0 <- compileExpr scope mbScrutineeTy scrutinee
      let scrutineeExpr =
            case annotateScrutinee of
              Just annTy -> surfaceAnn scrutineeExpr0 (lowerType scope annTy)
              Nothing -> scrutineeExpr0
      compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleDataOwner scope owners
      let headTy = dataHeadType dataInfo
          scrutineeTy =
            case inferKnownExprType scope scrutinee of
              Just knownTy -> knownTy
              Nothing -> headTy
      validateOrderedPatterns scope alts
      mapM_ (validatePatternType scope scrutineeTy . P.altPattern) alts
      (resultTy, _quantifyResult) <-
        case mbExpected of
          Just expectedTy -> pure (expectedTy, False)
          Nothing -> do
            resultVar <- freshTypeVarName
            pure (STVar resultVar, True)
      case localIdentityScrutinee scrutinee of
        Just inner -> compileCase scope mbExpected inner alts
        Nothing -> do
          scrutineeExpr <- compileExpr scope (Just scrutineeTy) scrutinee
          let forceAnnotateHandlers = any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholder <- deferCaseCall scope dataInfo scrutineeTy resultTy
          pure (foldl surfaceApp (surfaceVar placeholder) (scrutineeExpr : handlers))

compileResolvedCase :: ElaborateScope -> Maybe SrcType -> P.ResolvedExpr -> [P.ResolvedAlt] -> ElaborateM SurfaceExpr
compileResolvedCase scope mbExpected scrutinee alts = do
  case resolvedCtorOwners alts of
    [] -> do
      let mbInferredScrutineeTy = inferKnownResolvedExprType scope scrutinee
      mbAnnotationScrutineeTy <- catchAllResolvedPatternAnnotationType scope alts
      let mbScrutineeTy =
            case mbInferredScrutineeTy of
              Just knownTy -> Just knownTy
              Nothing -> mbAnnotationScrutineeTy
          annotateScrutinee =
            case (mbInferredScrutineeTy, mbAnnotationScrutineeTy) of
              (Nothing, Just annTy) -> Just annTy
              _ -> Nothing
      rejectOpaqueBuiltinCase scope mbScrutineeTy
      mapM_ (\scrutineeTy -> mapM_ (validateResolvedPatternType scope scrutineeTy . P.altPattern) alts) mbScrutineeTy
      scrutineeExpr0 <- compileResolvedExpr scope mbScrutineeTy scrutinee
      let scrutineeExpr =
            case annotateScrutinee of
              Just annTy -> surfaceAnn scrutineeExpr0 (lowerType scope annTy)
              Nothing -> scrutineeExpr0
      compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleResolvedDataOwner scope owners
      let headTy = dataHeadType dataInfo
          scrutineeView =
            case inferKnownResolvedExprTypeView scope scrutinee of
              Just knownView -> knownView
              Nothing -> sourceTypeViewInScope scope headTy
          scrutineeTy = typeViewDisplay scrutineeView
      validateResolvedOrderedPatterns scope alts
      mapM_ (validateResolvedPatternType scope scrutineeTy . P.altPattern) alts
      (resultTy, _quantifyResult) <-
        case mbExpected of
          Just expectedTy -> pure (expectedTy, False)
          Nothing -> do
            resultVar <- freshTypeVarName
            pure (STVar resultVar, True)
      case localResolvedIdentityScrutinee scrutinee of
        Just inner -> compileResolvedCase scope mbExpected inner alts
        Nothing -> do
          scrutineeExpr <- compileResolvedExprWithExpectedView scope (Just scrutineeView) scrutinee
          let forceAnnotateHandlers = any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileResolvedHandler scope scrutineeExpr scrutineeView resultTy dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholder <- deferCaseCall scope dataInfo scrutineeTy resultTy
          pure (foldl surfaceApp (surfaceVar placeholder) (scrutineeExpr : handlers))

localIdentityScrutinee :: P.Expr -> Maybe P.Expr
localIdentityScrutinee expr =
  case collectApps expr of
    (ELam param (EVar bodyName), [arg])
      | bodyName == P.paramName param ->
          Just arg
    _ -> Nothing

localResolvedIdentityScrutinee :: P.ResolvedExpr -> Maybe P.ResolvedExpr
localResolvedIdentityScrutinee expr =
  case collectResolvedApps expr of
    (ELam param (EVar (P.ResolvedLocalValue bodyName)), [arg])
      | bodyName == P.paramName param ->
          Just arg
    _ -> Nothing

rejectOpaqueBuiltinCase :: ElaborateScope -> Maybe SrcType -> ElaborateM ()
rejectOpaqueBuiltinCase scope mbScrutineeTy =
  case mbScrutineeTy of
    Just scrutineeTy
      | Builtins.srcTypeMentionsOpaqueBuiltin (canonicalSourceType scope scrutineeTy) ->
          throwError (ProgramCaseOnNonDataType scrutineeTy)
    _ -> pure ()

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> SurfaceExpr -> [P.Alt] -> ElaborateM SurfaceExpr
compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileExpr scope mbExpected body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      case mbScrutineeTy of
        Just _ -> pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        Nothing -> do
          -- Keep the scrutinee binding referenced so eMLF infers its own scheme
          -- while the strict let still preserves evaluation before the body.
          forceName <- freshRuntimeName "case_scrutinee_force"
          pure
            ( surfaceLet
                scrutineeName
                scrutineeExpr
                (surfaceLet forceName (surfaceVar scrutineeName) bodyExpr)
            )
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      scope' <-
        case mbScrutineeTy of
          Just scrutineeTy -> extendLocal scope name runtimeName (Just scrutineeTy)
          Nothing -> extendLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileExpr scope' mbExpected body
      pure (surfaceLet runtimeName scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileResolvedCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> SurfaceExpr -> [P.ResolvedAlt] -> ElaborateM SurfaceExpr
compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileResolvedExpr scope mbExpected body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      case mbScrutineeTy of
        Just _ -> pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        Nothing -> do
          forceName <- freshRuntimeName "case_scrutinee_force"
          pure
            ( surfaceLet
                scrutineeName
                scrutineeExpr
                (surfaceLet forceName (surfaceVar scrutineeName) bodyExpr)
            )
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName (localRefName name)
      scope' <-
        case mbScrutineeTy of
          Just scrutineeTy -> extendResolvedLocal scope name runtimeName (Just scrutineeTy)
          Nothing -> extendResolvedLocalLowered scope name runtimeName =<< freshTypeName
      bodyExpr <- compileResolvedExpr scope' mbExpected body
      pure (surfaceLet runtimeName scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileResolvedCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileHandler :: ElaborateScope -> SurfaceExpr -> SrcType -> SrcType -> DataInfo -> [P.Alt] -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileHandler scope scrutineeExpr scrutineeTy resultTy dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgTys = specializeConstructorArgsForScrutinee scrutineeTy ctorInfo
      specializedCtorInfo = constructorInfoWithArgs ctorInfo ctorArgTys
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgTys]]
  let topArgs = zip3 (map (const P.PatWildcard) ctorArgTys) runtimeNames ctorArgTys
      candidates = matchingCandidates ctorInfo
  bodyExpr <- compileCandidates topArgs candidates
  let handlerBody =
        foldr
          (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
          bodyExpr
          (zip runtimeNames ctorArgTys)
  if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
    then pure handlerBody
    else do
      let handlerTy = handlerSurfaceType scope specializedCtorInfo (lowerType scope resultTy)
      pure (surfaceAnn handlerBody handlerTy)
  where
    matchingCandidates ctor =
      filter (patternCouldMatchConstructor scope ctor . P.altPattern) alts

    compileCandidates _ [] = throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
    compileCandidates topArgs [alt] =
      compileAltCandidate topArgs alt Nothing
    compileCandidates topArgs (alt : rest) = do
      fallback <- compileCandidates topArgs rest
      compileAltCandidate topArgs alt (Just fallback)

    compileAltCandidate topArgs (P.Alt pattern0 body) mbFallback =
      case stripPatternAnn pattern0 of
        P.PatWildcard -> compileExpr scope (Just resultTy) body
        P.PatVar name -> do
          scrutineeName <- freshRuntimeName name
          scope' <- extendLocalLowered scope name scrutineeName (lowerType scope scrutineeTy)
          bodyExpr <- compileExpr scope' (Just resultTy) body
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        P.PatCtor ctorName0 patterns
          | constructorNameMatches scope ctorName0 ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then compilePatternSequence scope (zip3 patterns (map middle topArgs) (map third topArgs)) body mbFallback
                else throwError (ProgramPatternConstructorMismatch ctorName0 (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    middle (_, value, _) = value
    third (_, _, value) = value

    compilePatternSequence scope0 [] body _ =
      compileExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, argTy) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendLocal scope0 sourceName runtimeName (Just argTy)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorName nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfo scope nestedCtorName
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch nestedCtorName argTy)
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              let forceNestedAnnotations = any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgTys = specializeConstructorArgsForScrutinee argTy nestedCtorInfo
              matchingBody <- compilePatternSequence scope0 (zip3 nestedPatterns nestedRuntimeNames nestedArgTys ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <- mapM (nestedHandler forceNestedAnnotations argTy nestedCtorInfo nestedRuntimeNames matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholder <- deferCaseCall scope0 nestedDataInfo argTy resultTy
              pure (foldl surfaceApp (surfaceVar placeholder) (surfaceVar runtimeName : handlers))
        P.PatAnn inner annTy -> compilePatternSequence scope0 ((inner, runtimeName, annTy) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeTy targetCtor nestedRuntimeNames matchingBody mbFallback ctor =
      let ctorArgTys = specializeConstructorArgsForScrutinee nestedScrutineeTy ctor
          specializedCtor = constructorInfoWithArgs ctor ctorArgTys
          targetSelected = sameConstructorInfo ctor targetCtor
          argNames = if targetSelected then nestedRuntimeNames else ["unused" ++ show ix | ix <- [1 .. length ctorArgTys]]
          selectedBody =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          handlerBody =
            foldr
              (\(name, argTy) acc -> surfaceLamAnn name (lowerType scope argTy) acc)
              selectedBody
              (zip argNames ctorArgTys)
       in if not forceNestedAnnotations && null (ctorForalls ctor)
            then pure handlerBody
            else do
              let handlerTy = handlerSurfaceType scope specializedCtor (lowerType scope resultTy)
              pure (surfaceAnn handlerBody handlerTy)

    specializeConstructorArgsForScrutinee =
      specializeConstructorArgsForScrutineeType scope

compileResolvedHandler :: ElaborateScope -> SurfaceExpr -> TypeView -> SrcType -> DataInfo -> [P.ResolvedAlt] -> Bool -> ConstructorInfo -> ElaborateM SurfaceExpr
compileResolvedHandler scope scrutineeExpr scrutineeView resultTy dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgViews = specializeConstructorArgViewsForScrutineeView scope scrutineeView ctorInfo
      ctorArgTys = map typeViewDisplay ctorArgViews
      specializedCtorInfo = constructorInfoWithArgs ctorInfo ctorArgTys
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgViews]]
  let topArgs = zip3 (map (const P.PatWildcard) ctorArgViews) runtimeNames ctorArgViews
      candidates = matchingCandidates ctorInfo
  bodyExpr <- compileCandidates topArgs candidates
  let handlerBody =
        foldr
          (\(name, argView) acc -> surfaceLamAnn name (lowerTypeView scope argView) acc)
          bodyExpr
          (zip runtimeNames ctorArgViews)
  if not forceAnnotateHandlers && null (ctorForalls ctorInfo)
    then pure handlerBody
    else do
      let handlerTy = handlerSurfaceType scope specializedCtorInfo (lowerType scope resultTy)
      pure (surfaceAnn handlerBody handlerTy)
  where
    matchingCandidates ctor =
      filter (resolvedPatternCouldMatchConstructor scope ctor . P.altPattern) alts

    compileCandidates _ [] = throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
    compileCandidates topArgs [alt] =
      compileAltCandidate topArgs alt Nothing
    compileCandidates topArgs (alt : rest) = do
      fallback <- compileCandidates topArgs rest
      compileAltCandidate topArgs alt (Just fallback)

    compileAltCandidate topArgs (P.Alt pattern0 body) mbFallback =
      case stripResolvedPatternAnn pattern0 of
        P.PatWildcard -> compileResolvedExpr scope (Just resultTy) body
        P.PatVar name -> do
          scrutineeName <- freshRuntimeName (localRefName name)
          scope' <- extendResolvedLocalView scope name scrutineeName (Just scrutineeView)
          bodyExpr <- compileResolvedExpr scope' (Just resultTy) body
          pure (surfaceLet scrutineeName scrutineeExpr bodyExpr)
        P.PatCtor ctorSymbol patterns
          | constructorSymbolMatches scope ctorSymbol ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then compilePatternSequence scope (zip3 patterns (map middle topArgs) (map third topArgs)) body mbFallback
                else throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    middle (_, value, _) = value
    third (_, _, value) = value

    compilePatternSequence scope0 [] body _ =
      compileResolvedExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, argView) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendResolvedLocalView scope0 sourceName runtimeName (Just argView)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorSymbol nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfoBySymbol scope nestedCtorSymbol
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch (P.refDisplayName nestedCtorSymbol) (typeViewDisplay argView))
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              let forceNestedAnnotations = any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgViews = specializeConstructorArgViewsForScrutineeView scope argView nestedCtorInfo
              matchingBody <- compilePatternSequence scope0 (zip3 nestedPatterns nestedRuntimeNames nestedArgViews ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <- mapM (nestedHandler forceNestedAnnotations argView nestedCtorInfo nestedRuntimeNames matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholder <- deferCaseCall scope0 nestedDataInfo (typeViewDisplay argView) resultTy
              pure (foldl surfaceApp (surfaceVar placeholder) (surfaceVar runtimeName : handlers))
        P.PatAnn inner annTy -> do
          annView <- liftEitherElab (resolvedTypeViewForScope scope annTy)
          compilePatternSequence scope0 ((inner, runtimeName, annView) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeView targetCtor nestedRuntimeNames matchingBody mbFallback ctor =
      let ctorArgViews = specializeConstructorArgViewsForScrutineeView scope nestedScrutineeView ctor
          ctorArgTys = map typeViewDisplay ctorArgViews
          specializedCtor = constructorInfoWithArgs ctor ctorArgTys
          targetSelected = sameConstructorInfo ctor targetCtor
          argNames = if targetSelected then nestedRuntimeNames else ["unused" ++ show ix | ix <- [1 .. length ctorArgViews]]
          selectedBody =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          handlerBody =
            foldr
              (\(name, argView) acc -> surfaceLamAnn name (lowerTypeView scope argView) acc)
              selectedBody
              (zip argNames ctorArgViews)
       in if not forceNestedAnnotations && null (ctorForalls ctor)
            then pure handlerBody
            else do
              let handlerTy = handlerSurfaceType scope specializedCtor (lowerType scope resultTy)
              pure (surfaceAnn handlerBody handlerTy)

specializeConstructorArgsForScrutineeType :: ElaborateScope -> SrcType -> ConstructorInfo -> [SrcType]
specializeConstructorArgsForScrutineeType scope actualScrutineeTy ctor =
  case matchTypesInScope scope Map.empty (ctorResult ctor) actualScrutineeTy of
    Just subst -> map (specializeSrcType subst) (ctorArgs ctor)
    Nothing -> ctorArgs ctor

specializeConstructorArgViewsForScrutineeView :: ElaborateScope -> TypeView -> ConstructorInfo -> [TypeView]
specializeConstructorArgViewsForScrutineeView scope actualScrutineeView ctor =
  case matchTypeViewAgainstIdentity scope Map.empty (constructorResultTypeView scope ctor) actualScrutineeView of
    Just subst -> map (applyTypeViewSubst subst) (constructorArgTypeViews scope ctor)
    Nothing -> constructorArgTypeViews scope ctor

ctorOwners :: [P.Alt] -> [String]
ctorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorName0 _ -> ctorName0 : acc
      P.PatAnn inner _ -> go (P.Alt inner (P.altExpr alt)) acc
      _ -> acc

resolvedCtorOwners :: [P.ResolvedAlt] -> [ResolvedSymbol]
resolvedCtorOwners = foldr go []
  where
    go alt acc = case P.altPattern alt of
      P.PatCtor ctorSymbol _ -> ctorSymbol : acc
      P.PatAnn inner _ -> go (P.Alt inner (P.altExpr alt)) acc
      _ -> acc

catchAllPatternAnnotationType :: [P.Alt] -> Maybe SrcType
catchAllPatternAnnotationType alts =
  case alts of
    [P.Alt pattern0 _] -> patternAnnotationType pattern0
    _ -> Nothing

catchAllResolvedPatternAnnotationType :: ElaborateScope -> [P.ResolvedAlt] -> ElaborateM (Maybe SrcType)
catchAllResolvedPatternAnnotationType scope alts =
  case alts of
    [P.Alt pattern0 _] -> resolvedPatternAnnotationType scope pattern0
    _ -> pure Nothing

patternAnnotationType :: P.Pattern -> Maybe SrcType
patternAnnotationType pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      case patternAnnotationType inner of
        Just innerTy -> Just innerTy
        Nothing -> Just annTy
    _ -> Nothing

resolvedPatternAnnotationType :: ElaborateScope -> P.ResolvedPattern -> ElaborateM (Maybe SrcType)
resolvedPatternAnnotationType scope pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      resolvedPatternAnnotationType scope inner >>= \case
        Just innerTy -> pure (Just innerTy)
        Nothing -> Just <$> liftEitherElab (displaySrcTypeForResolved scope annTy)
    _ -> pure Nothing

validatePatternType :: ElaborateScope -> SrcType -> P.Pattern -> ElaborateM ()
validatePatternType scope expectedTy pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      validatePatternAnnotation scope expectedTy annTy
      validatePatternType scope annTy inner
    P.PatCtor ctorName0 patterns -> do
      ctorInfo <- lookupConstructorInfo scope ctorName0
      subst <-
        case matchPatternTypes (ctorResult ctorInfo) expectedTy of
          Just subst0 -> pure subst0
          Nothing -> throwError (ProgramPatternConstructorMismatch ctorName0 expectedTy)
      if length patterns /= length (ctorArgs ctorInfo)
        then throwError (ProgramPatternConstructorMismatch ctorName0 expectedTy)
        else
          mapM_
            ( \(nestedPattern, argTy) ->
                validatePatternType scope (specializeSrcType subst argTy) nestedPattern
            )
            (zip patterns (ctorArgs ctorInfo))
  where
    matchPatternTypes template actual =
      case matchTypesInScope scope Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypesInScope scope Map.empty actual template of
            Just subst -> Just subst
            Nothing
              | lowerType scope template == lowerType scope actual -> Just Map.empty
            Nothing -> Nothing

validateResolvedPatternType :: ElaborateScope -> SrcType -> P.ResolvedPattern -> ElaborateM ()
validateResolvedPatternType scope expectedTy pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      annDisplayTy <- liftEitherElab (displaySrcTypeForResolved scope annTy)
      validatePatternAnnotation scope expectedTy annDisplayTy
      validateResolvedPatternType scope annDisplayTy inner
    P.PatCtor ctorSymbol patterns -> do
      ctorInfo <- lookupConstructorInfoBySymbol scope ctorSymbol
      subst <-
        case matchPatternTypes (ctorResult ctorInfo) expectedTy of
          Just subst0 -> pure subst0
          Nothing -> throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) expectedTy)
      if length patterns /= length (ctorArgs ctorInfo)
        then throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) expectedTy)
        else
          mapM_
            ( \(nestedPattern, argTy) ->
                validateResolvedPatternType scope (specializeSrcType subst argTy) nestedPattern
            )
            (zip patterns (ctorArgs ctorInfo))
  where
    matchPatternTypes template actual =
      case matchTypesInScope scope Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypesInScope scope Map.empty actual template of
            Just subst -> Just subst
            Nothing
              | lowerType scope template == lowerType scope actual -> Just Map.empty
            Nothing -> Nothing

validatePatternAnnotation :: ElaborateScope -> SrcType -> SrcType -> ElaborateM ()
validatePatternAnnotation scope expectedTy annTy =
  when (not (patternAnnotationCompatible expectedTy annTy)) $
    throwError (ProgramTypeMismatch annTy expectedTy)
  where
    patternAnnotationCompatible left right =
      lowerType scope left == lowerType scope right
        || matchTypesInScope scope Map.empty left right /= Nothing
        || matchTypesInScope scope Map.empty right left /= Nothing

validateOrderedPatterns :: ElaborateScope -> [P.Alt] -> ElaborateM ()
validateOrderedPatterns scope = go Set.empty False
  where
    go _ _ [] = pure ()
    go seen catchAllSeen (P.Alt pattern0 _ : rest)
      | catchAllSeen =
          throwError (ProgramDuplicateCaseBranch (maybe "_" id (topConstructorName pattern0)))
      | isCatchAllPattern pattern0 =
          go seen True rest
      | Just (ctorName0, ctorIdentity) <- topConstructorIdentity pattern0,
        ctorIdentity `Set.member` seen =
          throwError (ProgramDuplicateCaseBranch ctorName0)
      | Just (_, ctorIdentity) <- flatCatchAllConstructor scope pattern0 =
          go (Set.insert ctorIdentity seen) False rest
      | otherwise = go seen False rest

    topConstructorIdentity pattern0 =
      case topConstructorName pattern0 of
        Just ctorName0
          | Just ctorInfo <- lookupConstructorInfoMaybe scope ctorName0 ->
              Just (ctorName0, ctorInfoSymbol ctorInfo)
        _ -> Nothing

topConstructorName :: P.Pattern -> Maybe String
topConstructorName pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 _ -> Just ctorName0
    _ -> Nothing

validateResolvedOrderedPatterns :: ElaborateScope -> [P.ResolvedAlt] -> ElaborateM ()
validateResolvedOrderedPatterns scope = go Set.empty False
  where
    go _ _ [] = pure ()
    go seen catchAllSeen (P.Alt pattern0 _ : rest)
      | catchAllSeen =
          throwError (ProgramDuplicateCaseBranch (maybe "_" P.refDisplayName (topResolvedConstructorSymbol pattern0)))
      | isResolvedCatchAllPattern pattern0 =
          go seen True rest
      | Just ctorSymbol <- topResolvedConstructorSymbol pattern0,
        resolvedSymbolIdentity ctorSymbol `Set.member` seen =
          throwError (ProgramDuplicateCaseBranch (P.refDisplayName ctorSymbol))
      | Just (_, ctorIdentity) <- flatCatchAllResolvedConstructor scope pattern0 =
          go (Set.insert ctorIdentity seen) False rest
      | otherwise = go seen False rest

topResolvedConstructorSymbol :: P.ResolvedPattern -> Maybe ResolvedSymbol
topResolvedConstructorSymbol pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatCtor ctorSymbol _ -> Just ctorSymbol
    _ -> Nothing

flatCatchAllResolvedConstructor :: ElaborateScope -> P.ResolvedPattern -> Maybe (ResolvedSymbol, SymbolIdentity)
flatCatchAllResolvedConstructor scope pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatCtor ctorSymbol patterns
      | all isResolvedCatchAllPattern patterns,
        Just ctorInfo <- lookupConstructorInfoBySymbolMaybe scope ctorSymbol ->
          Just (ctorSymbol, ctorInfoSymbol ctorInfo)
    _ -> Nothing

isResolvedCatchAllPattern :: P.ResolvedPattern -> Bool
isResolvedCatchAllPattern pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    _ -> False

flatCatchAllConstructor :: ElaborateScope -> P.Pattern -> Maybe (String, SymbolIdentity)
flatCatchAllConstructor scope pattern0 =
  case stripPatternAnn pattern0 of
    P.PatCtor ctorName0 patterns
      | all isCatchAllPattern patterns,
        Just ctorInfo <- lookupConstructorInfoMaybe scope ctorName0 ->
          Just (ctorName0, ctorInfoSymbol ctorInfo)
    _ -> Nothing

isCatchAllPattern :: P.Pattern -> Bool
isCatchAllPattern pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    _ -> False

patternCouldMatchConstructor :: ElaborateScope -> ConstructorInfo -> P.Pattern -> Bool
patternCouldMatchConstructor scope ctorInfo pattern0 =
  case stripPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    P.PatCtor ctorName0 _ -> constructorNameMatches scope ctorName0 ctorInfo
    P.PatAnn inner _ -> patternCouldMatchConstructor scope ctorInfo inner

resolvedPatternCouldMatchConstructor :: ElaborateScope -> ConstructorInfo -> P.ResolvedPattern -> Bool
resolvedPatternCouldMatchConstructor scope ctorInfo pattern0 =
  case stripResolvedPatternAnn pattern0 of
    P.PatWildcard -> True
    P.PatVar {} -> True
    P.PatCtor ctorSymbol _ -> constructorSymbolMatches scope ctorSymbol ctorInfo
    P.PatAnn inner _ -> resolvedPatternCouldMatchConstructor scope ctorInfo inner

stripPatternAnn :: P.Pattern -> P.Pattern
stripPatternAnn pattern0 =
  case pattern0 of
    P.PatAnn inner _ -> stripPatternAnn inner
    _ -> pattern0

stripResolvedPatternAnn :: P.ResolvedPattern -> P.ResolvedPattern
stripResolvedPatternAnn pattern0 =
  case pattern0 of
    P.PatAnn inner _ -> stripResolvedPatternAnn inner
    _ -> pattern0

requireSingleDataOwner :: ElaborateScope -> [String] -> ElaborateM DataInfo
requireSingleDataOwner scope ctorNames0 = do
  owners <- mapM (lookupConstructorInfo scope >=> lookupResolvedDataInfo) ctorNames0
  case owners of
    [] -> throwError (ProgramCaseOnNonDataType STBottom)
    owner : rest
      | all (sameDataInfo owner) rest -> pure owner
      | otherwise -> throwError (ProgramCaseOnNonDataType STBottom)
  where
    lookupResolvedDataInfo ctorInfo =
      case resolveConstructorDataInfo scope ctorInfo of
        Just resolved -> pure resolved
        Nothing -> throwError (ProgramUnknownType (constructorOwnerTypeDisplayName ctorInfo))

requireSingleResolvedDataOwner :: ElaborateScope -> [ResolvedSymbol] -> ElaborateM DataInfo
requireSingleResolvedDataOwner scope ctorSymbols = do
  owners <- mapM (lookupConstructorInfoBySymbol scope >=> lookupResolvedDataInfo) ctorSymbols
  case owners of
    [] -> throwError (ProgramCaseOnNonDataType STBottom)
    owner : rest
      | all (sameDataInfo owner) rest -> pure owner
      | otherwise -> throwError (ProgramCaseOnNonDataType STBottom)
  where
    lookupResolvedDataInfo ctorInfo =
      case resolveConstructorDataInfo scope ctorInfo of
        Just resolved -> pure resolved
        Nothing -> throwError (ProgramUnknownType (constructorOwnerTypeDisplayName ctorInfo))

lookupConstructorInfo :: ElaborateScope -> String -> ElaborateM ConstructorInfo
lookupConstructorInfo scope ctorName0 =
  case lookupConstructorInfoMaybe scope ctorName0 of
    Just ctorInfo -> pure ctorInfo
    Nothing -> throwError (ProgramUnknownConstructor ctorName0)

lookupConstructorInfoMaybe :: ElaborateScope -> String -> Maybe ConstructorInfo
lookupConstructorInfoMaybe scope ctorName0 =
  case Map.lookup ctorName0 (esValues scope) of
    Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just ctorInfo
    _ -> Nothing

lookupConstructorInfoBySymbol :: ElaborateScope -> ResolvedSymbol -> ElaborateM ConstructorInfo
lookupConstructorInfoBySymbol scope symbol =
  case lookupConstructorInfoBySymbolMaybe scope symbol of
    Just ctorInfo -> pure ctorInfo
    Nothing -> throwError (ProgramUnknownConstructor (P.refDisplayName symbol))

lookupConstructorInfoBySymbolMaybe :: ElaborateScope -> ResolvedSymbol -> Maybe ConstructorInfo
lookupConstructorInfoBySymbolMaybe scope symbol =
  case lookupValueInfoBySymbol scope symbol of
    Just ConstructorValue {valueCtorInfo = ctorInfo} -> Just ctorInfo
    _ -> Nothing

lookupDataInfoForConstructor :: ElaborateScope -> ConstructorInfo -> ElaborateM DataInfo
lookupDataInfoForConstructor scope ctorInfo =
  case resolveConstructorDataInfo scope ctorInfo of
    Just info -> pure info
    Nothing -> throwError (ProgramUnknownType (constructorOwnerTypeDisplayName ctorInfo))

constructorOwnerTypeDisplayName :: ConstructorInfo -> String
constructorOwnerTypeDisplayName =
  symbolDefiningName . ctorOwningTypeIdentity

resolveConstructorDataInfo :: ElaborateScope -> ConstructorInfo -> Maybe DataInfo
resolveConstructorDataInfo scope ctorInfo =
  case Map.lookup (ctorOwningTypeIdentity ctorInfo) (esTypesByIdentity scope) of
    Just info
      | constructorBelongsToDataInfo ctorInfo info -> Just info
      | otherwise -> Nothing
    Nothing -> Nothing

sameDataInfo :: DataInfo -> DataInfo -> Bool
sameDataInfo left right =
  dataInfoSymbolIdentity left == dataInfoSymbolIdentity right

constructorBelongsToDataInfo :: ConstructorInfo -> DataInfo -> Bool
constructorBelongsToDataInfo ctorInfo =
  any (sameConstructorInfo ctorInfo) . dataConstructors

constructorNameMatches :: ElaborateScope -> String -> ConstructorInfo -> Bool
constructorNameMatches scope ctorName0 ctorInfo =
  case lookupConstructorInfoMaybe scope ctorName0 of
    Just namedCtor -> sameConstructorInfo namedCtor ctorInfo
    Nothing -> False

constructorSymbolMatches :: ElaborateScope -> ResolvedSymbol -> ConstructorInfo -> Bool
constructorSymbolMatches scope ctorSymbol ctorInfo =
  case lookupConstructorInfoBySymbolMaybe scope ctorSymbol of
    Just namedCtor -> sameConstructorInfo namedCtor ctorInfo
    Nothing -> False

sameConstructorInfo :: ConstructorInfo -> ConstructorInfo -> Bool
sameConstructorInfo left right =
  ctorInfoSymbol left == ctorInfoSymbol right

handlerSurfaceType :: ElaborateScope -> ConstructorInfo -> SrcType -> SrcType
handlerSurfaceType scope ctorInfo resultTy =
  let (foralls, argTys) = freshenCtorForallsForResult resultTy (ctorForalls ctorInfo) (ctorArgs ctorInfo)
   in stripVacuousSrcForalls $
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound (fmap lowerBound mbBound)) acc)
          (foldr STArrow resultTy (map (lowerType scope) argTys))
          foralls
  where
    lowerBound = lowerType scope

freshenCtorForallsForResult :: SrcType -> [(String, Maybe SrcType)] -> [SrcType] -> ([(String, Maybe SrcType)], [SrcType])
freshenCtorForallsForResult resultTy foralls0 args0 =
  let initialUsed = Set.union (freeTypeVarsSrcType resultTy) (foldMap (maybe Set.empty freeTypeVarsSrcType . snd) foralls0)
      step (used, accForalls, currentArgs) (name, mbBound) =
        let name' =
              if Set.member name used
                then freshNameLike name (Set.union used (foldMap freeTypeVarsSrcType currentArgs))
                else name
            renameTy =
              if name' == name
                then id
                else substSrcType name (STVar name')
            mbBound' = fmap renameTy mbBound
            args' = map renameTy currentArgs
        in (Set.insert name' used, accForalls ++ [(name', mbBound')], args')
      (_, foralls, args) = foldl' step (initialUsed, [], args0) foralls0
   in (foralls, args)

dataHeadType :: DataInfo -> SrcType
dataHeadType info =
  dataHeadTypeWithName (dataInfoIdentityName info) info

visibleDataHeadType :: ElaborateScope -> DataInfo -> SrcType
visibleDataHeadType scope info =
  dataHeadTypeWithName visibleName info
  where
    visibleName =
      case Map.lookup (dataInfoSymbolIdentity info) (esTypeDisplayNamesByIdentity scope) of
        Just names ->
          case preferredDisplayName (dataInfoSymbolIdentity info) names of
            Just name -> name
            Nothing -> dataInfoIdentityName info
        _ -> dataInfoIdentityName info

dataHeadTypeWithName :: String -> DataInfo -> SrcType
dataHeadTypeWithName name info =
  case dataParams info of
    [] -> STBase name
    p : ps -> STCon name (STVar p :| map STVar ps)

isRecursiveResultType :: SrcType -> Bool
isRecursiveResultType ty =
  case ty of
    STMu {} -> True
    STForall _ _ body -> isRecursiveResultType body
    _ -> False

resolveInstanceInfoByConstraint :: ElaborateScope -> ConstraintInfo -> Either ProgramError (InstanceInfo, TypeViewSubst)
resolveInstanceInfoByConstraint scope constraint =
  resolveInstanceInfoWithTypeViews
    scope
    (constraintDisplayClass constraint)
    (Just (constraintClassSymbol constraint))
    (constraintTypeViews constraint)

resolveInstanceInfoWithIdentityType :: ElaborateScope -> ClassIdentity -> P.ClassName -> TypeView -> Either ProgramError (InstanceInfo, TypeViewSubst)
resolveInstanceInfoWithIdentityType scope classIdentity0 className0 =
  resolveInstanceInfoWithTypeView scope className0 (Just classIdentity0)

resolveMethodInstanceInfoByTypeView :: ElaborateScope -> MethodInfo -> TypeView -> Either ProgramError (InstanceInfo, TypeViewSubst)
resolveMethodInstanceInfoByTypeView scope methodInfo =
  resolveInstanceInfoWithTypeView scope (methodClassName methodInfo) (Just (methodInfoClassIdentity methodInfo))

resolveMethodInstanceInfoByTypeViews :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> Either ProgramError (InstanceInfo, TypeViewSubst)
resolveMethodInstanceInfoByTypeViews scope methodInfo =
  resolveInstanceInfoWithTypeViews scope (methodClassName methodInfo) (Just (methodInfoClassIdentity methodInfo))

methodInfoClassIdentity :: MethodInfo -> ClassIdentity
methodInfoClassIdentity = methodInfoOwnerClassSymbolIdentity

resolveInstanceInfoWithTypeView ::
  ElaborateScope ->
  P.ClassName ->
  Maybe ClassIdentity ->
  TypeView ->
  Either ProgramError (InstanceInfo, TypeViewSubst)
resolveInstanceInfoWithTypeView scope className0 expectedClassIdentity headView =
  resolveInstanceInfoWithTypeViews scope className0 expectedClassIdentity (headView :| [])

resolveInstanceInfoWithTypeViews ::
  ElaborateScope ->
  P.ClassName ->
  Maybe ClassIdentity ->
  NonEmpty TypeView ->
  Either ProgramError (InstanceInfo, TypeViewSubst)
resolveInstanceInfoWithTypeViews scope className0 expectedClassIdentity headViews =
  case deduplicatedMatches of
    [(match, subst, _direct)] -> Right (match, subst)
    [] -> Left noMatching
    _ -> Left noMatching
  where
    deduplicatedMatches = deduplicateEquivalentMatches matches
    noMatching =
      case fmap diagnosticInstanceHeadDisplay headViews of
        ty :| [] -> ProgramNoMatchingInstance className0 ty
        tys -> ProgramNoMatchingInstanceHead className0 (NE.toList tys)

    diagnosticInstanceHeadDisplay view =
      diagnosticTypeViewDisplay scope view

    matches =
      [ (info, subst, direct)
        | info <- esInstances scope,
          instanceMatchesClassIdentity info,
          Just (subst, direct) <- [matchInstanceHead info]
      ]

    instanceMatchesClassIdentity info =
      case expectedClassIdentity of
        Just identity -> instanceInfoClassIdentity info == identity
        Nothing -> False

    instanceInfoClassIdentity info =
      instanceInfoClassSymbolIdentity info

    matchInstanceHead info =
      case matchTypeViewsAgainstIdentity scope Map.empty (instanceHeadViews info) headViews of
        Just subst -> Just (subst, True)
        Nothing -> Nothing

    instanceHeadViews =
      instanceHeadTypeViews

    deduplicateEquivalentMatches [] = []
    deduplicateEquivalentMatches (match : rest) =
      let (equivalent, different) = partition (equivalentInstanceMatch match) rest
       in foldl preferredInstanceMatch match equivalent : deduplicateEquivalentMatches different

    equivalentInstanceMatch (left, _, _) (right, _, _) =
      instanceOriginModuleIdentity left == instanceOriginModuleIdentity right
        && instanceInfoClassIdentity left == instanceInfoClassIdentity right
        && instanceHeadIdentityTypes left == instanceHeadIdentityTypes right
        && map canonicalConstraintInfo (instanceConstraintInfos left) == map canonicalConstraintInfo (instanceConstraintInfos right)
        && fmap valueInfoSymbolIdentity (instanceMethodsByIdentity left) == fmap valueInfoSymbolIdentity (instanceMethodsByIdentity right)

    canonicalConstraintInfo constraint =
      ( constraintClassSymbol constraint,
        fmap (canonicalSourceType scope) (typeViewsIdentity (constraintTypeViews constraint))
      )

    preferredInstanceMatch left@(_, leftSubst, leftDirect) right@(_, rightSubst, rightDirect)
      | rightDirect && not leftDirect = right
      | rightDirect == leftDirect && Map.size rightSubst > Map.size leftSubst = right
      | otherwise = left

matchTypeViewsAgainstIdentity :: ElaborateScope -> TypeViewSubst -> NonEmpty TypeView -> NonEmpty TypeView -> Maybe TypeViewSubst
matchTypeViewsAgainstIdentity scope subst templates actuals
  | length templates /= length actuals = Nothing
  | otherwise =
      foldM
        (\acc (template, actual) -> matchTypeViewAgainstIdentity scope acc template actual)
        subst
        (zip (NE.toList templates) (NE.toList actuals))

matchTypeViewAgainstIdentity :: ElaborateScope -> TypeViewSubst -> TypeView -> TypeView -> Maybe TypeViewSubst
matchTypeViewAgainstIdentity scope subst template actual =
  case typeViewIdentity template of
    STVar name -> do
      key <- typeViewSubstKeyForTemplateName template name
      let keyIdentity =
            typeViewSubstKeyIdentity key
      case lookupTypeViewSubst key subst of
        Nothing
          | typeViewIsBareBinderIdentity keyIdentity actual -> Just subst
          | typeViewMentionsFreeBinderIdentity keyIdentity actual -> Nothing
          | otherwise -> Just (insertTypeViewSubst key actual subst)
        Just existing
          | semanticTypeViewEqual scope existing actual -> Just subst
          | otherwise -> Nothing
    STArrow dom cod ->
      case typeViewIdentity actual of
        STArrow dom' cod' -> do
          subst' <- matchTypeViewAgainstIdentity scope subst (templateChildView (displayDom template) dom) (actualChildView (displayDom actual) dom')
          matchTypeViewAgainstIdentity scope subst' (templateChildView (displayCod template) cod) (actualChildView (displayCod actual) cod')
        _ -> Nothing
    STBase expectedName ->
      case typeViewIdentity actual of
        STBase actualName
          | sameTypeViewHeadInScope template actual expectedName actualName -> Just subst
        _ -> Nothing
    STCon expectedName args ->
      case typeViewIdentity actual of
        STCon actualName actualArgs
          | sameTypeViewHeadInScope template actual expectedName actualName,
            length (toListNE args) == length (toListNE actualArgs) ->
              foldM
                (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentity scope acc templateTy actualTy)
                subst
                (zip (zipWithTemplate args) (zipWithActual actualArgs))
        _ -> Nothing
    STVarApp expectedName args ->
      matchTypeViewHeadApplication scope subst template expectedName args actual
    STTyLam _ body ->
      case typeViewIdentity actual of
        STTyLam _ body' ->
          matchTypeViewAgainstIdentity scope subst (sameTemplateView body) (sameActualView body')
        _ -> Nothing
    STTyApp fun arg ->
      case typeViewIdentity actual of
        STTyApp fun' arg' -> do
          subst' <- matchTypeViewAgainstIdentity scope subst (sameTemplateView fun) (sameActualView fun')
          matchTypeViewAgainstIdentity scope subst' (sameTemplateView arg) (sameActualView arg')
        _ -> Nothing
    STForall _ mb body ->
      case typeViewIdentity actual of
        STForall _ mb' body' -> do
          subst' <-
            case (mb, mb') of
              (Nothing, _) -> Just subst
              (Just bound, Just bound') -> matchTypeViewAgainstIdentity scope subst (sameTemplateView (unSrcBound bound)) (sameActualView (unSrcBound bound'))
              (Just {}, Nothing) -> Nothing
          matchTypeViewAgainstIdentity scope subst' (sameTemplateView body) (sameActualView body')
        _ -> Nothing
    STMu _ body ->
      case typeViewIdentity actual of
        STMu _ body' ->
          matchTypeViewAgainstIdentity scope subst (sameTemplateView body) (sameActualView body')
        _ -> Nothing
    STBottom ->
      case typeViewIdentity actual of
        STBottom -> Just subst
        _ -> Nothing
  where
    sameTemplateView ty =
      (mkTypeView ty ty)
        { typeViewHeadIdentities = typeViewHeadIdentities template,
          typeViewBinderIdentities = typeViewBinderIdentities template
        }

    sameActualView ty =
      (mkTypeView ty ty)
        { typeViewHeadIdentities = typeViewHeadIdentities actual,
          typeViewBinderIdentities = typeViewBinderIdentities actual
        }

    templateChildView display identityTy =
      (mkTypeView display identityTy)
        { typeViewHeadIdentities = typeViewHeadIdentities template,
          typeViewBinderIdentities = typeViewBinderIdentities template
        }

    actualChildView display identityTy =
      (mkTypeView display identityTy)
        { typeViewHeadIdentities = typeViewHeadIdentities actual,
          typeViewBinderIdentities = typeViewBinderIdentities actual
        }

    displayDom view =
      case typeViewDisplay view of
        STArrow dom _ -> dom
        _ -> typeViewDisplay view

    displayCod view =
      case typeViewDisplay view of
        STArrow _ cod -> cod
        _ -> typeViewDisplay view

    zipWithActual actualArgs =
      case typeViewDisplay actual of
        STCon _ displayArgs -> zipWith actualChildView (toListNE displayArgs) (toListNE actualArgs)
        STVarApp _ displayArgs -> zipWith actualChildView (toListNE displayArgs) (toListNE actualArgs)
        _ -> map sameActualView (toListNE actualArgs)

    zipWithTemplate templateArgs =
      case typeViewDisplay template of
        STCon _ displayArgs -> zipWith templateChildView (toListNE displayArgs) (toListNE templateArgs)
        STVarApp _ displayArgs -> zipWith templateChildView (toListNE displayArgs) (toListNE templateArgs)
        _ -> map sameTemplateView (toListNE templateArgs)

    sameTypeViewHeadInScope leftView rightView leftName rightName =
      case (typeViewHeadIdentityForAlias leftView leftName, typeViewHeadIdentityForAlias rightView rightName) of
        (Just leftIdentity, Just rightIdentity) -> leftIdentity == rightIdentity
        (Nothing, Nothing) ->
          case (typeViewBinderIdentityForAlias leftView leftName, typeViewBinderIdentityForAlias rightView rightName) of
            (Just leftIdentity, Just rightIdentity) -> leftIdentity == rightIdentity
            (Nothing, Nothing) -> sameTypeHeadInScope scope leftName rightName
            _ -> False
        _ -> False

matchTypeViewHeadApplication ::
  ElaborateScope ->
  TypeViewSubst ->
  TypeView ->
  String ->
  NonEmpty SrcType ->
  TypeView ->
  Maybe TypeViewSubst
matchTypeViewHeadApplication scope subst template expectedName expectedArgs actual =
  case typeViewIdentity actual of
    STCon actualName actualArgs ->
      matchAppliedHead (STBase actualName) (displayApplicationHead (STBase actualName) actualArgs) (toListNE actualArgs)
    STVarApp actualName actualArgs ->
      matchAppliedHead (STVar actualName) (displayApplicationHead (STVar actualName) actualArgs) (toListNE actualArgs)
    _ -> Nothing
  where
    expectedArgsList = toListNE expectedArgs
    expectedDisplayArgs =
      case typeViewDisplay template of
        STVarApp _ displayArgs -> toListNE displayArgs
        _ -> expectedArgsList
    expectedArgCount = length expectedArgsList

    matchAppliedHead identityHead (displayHead, displayArgs) identityArgs
      | length identityArgs < expectedArgCount = Nothing
      | length displayArgs /= length identityArgs = Nothing
      | otherwise = do
          let prefixLength = length identityArgs - expectedArgCount
              (identityHeadArgs, matchedIdentityArgs) = splitAt prefixLength identityArgs
              (displayHeadArgs, matchedDisplayArgs) = splitAt prefixLength displayArgs
          headIdentity <- applyTypeHead identityHead identityHeadArgs
          headDisplay <- applyTypeHead displayHead displayHeadArgs
          key <- typeViewSubstKeyForTemplateName template expectedName
          subst' <-
            bindTypeViewHeadVariable
              scope
              subst
              key
              ( (mkTypeView headDisplay headIdentity)
                  { typeViewHeadIdentities = typeViewHeadIdentities actual,
                    typeViewBinderIdentities = typeViewBinderIdentities actual
                  }
              )
          foldM
            (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentity scope acc templateTy actualTy)
            subst'
            (zip (zipWith templateChildView expectedDisplayArgs expectedArgsList) (zipWith actualChildView matchedDisplayArgs matchedIdentityArgs))

    displayApplicationHead fallbackHead fallbackArgs =
      case typeViewDisplay actual of
        STCon displayName displayArgs -> (STBase displayName, toListNE displayArgs)
        STVarApp displayName displayArgs -> (STVar displayName, toListNE displayArgs)
        _ -> (fallbackHead, toListNE fallbackArgs)

    templateChildView display identityTy =
      (mkTypeView display identityTy)
        { typeViewHeadIdentities = typeViewHeadIdentities template,
          typeViewBinderIdentities = typeViewBinderIdentities template
        }

    actualChildView display identityTy =
      (mkTypeView display identityTy)
        { typeViewHeadIdentities = typeViewHeadIdentities actual,
          typeViewBinderIdentities = typeViewBinderIdentities actual
        }

bindTypeViewHeadVariable ::
  ElaborateScope ->
  TypeViewSubst ->
  TypeViewSubstKey ->
  TypeView ->
  Maybe TypeViewSubst
bindTypeViewHeadVariable scope subst key view =
  case lookupTypeViewSubst key subst of
    Just existing
      | semanticTypeViewEqual scope existing view -> Just subst
      | otherwise -> Nothing
    Nothing
      | typeViewIsBareBinderIdentity keyIdentity view -> Just subst
      | typeViewMentionsFreeBinderIdentity keyIdentity view -> Nothing
      | otherwise -> Just (insertTypeViewSubst key view subst)
  where
    keyIdentity =
      typeViewSubstKeyIdentity key

preferVisibleSourceType :: ElaborateScope -> SrcType -> SrcType
preferVisibleSourceType scope = go
  where
    go ty =
      case ty of
        STVar {} -> ty
        STBase name -> STBase (preferVisibleTypeHeadName scope name)
        STCon name args -> STCon (preferVisibleTypeHeadName scope name) (fmap go args)
        STVarApp name args -> STVarApp name (fmap go args)
        STTyLam name body -> STTyLam name (go body)
        STTyApp fun arg -> STTyApp (go fun) (go arg)
        STArrow dom cod -> STArrow (go dom) (go cod)
        STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
        STMu name body -> STMu name (go body)
        STBottom -> STBottom

preferVisibleTypeHeadName :: ElaborateScope -> String -> String
preferVisibleTypeHeadName scope name
  | Just identity <- typeHeadIdentityInScope scope name,
    Just visibleName <- Map.lookup identity (esTypeDisplayNamesByIdentity scope) >>= preferredDisplayName identity =
      visibleName
  | otherwise = name

dataInfoIdentityAliasName :: String -> DataInfo -> Bool
dataInfoIdentityAliasName name info =
  name == dataIdentityTypeName info
    || name == dataInfoIdentityQualifiedName info

rewriteSrcTypeOccurrences :: SrcType -> SrcType -> SrcType -> SrcType
rewriteSrcTypeOccurrences needle replacement = go
  where
    go ty
      | ty == needle = replacement
      | otherwise =
          case ty of
            STVar {} -> ty
            STBase {} -> ty
            STCon name args -> STCon name (fmap go args)
            STVarApp name args -> STVarApp name (fmap go args)
            STTyLam name body -> STTyLam name (go body)
            STTyApp fun arg -> STTyApp (go fun) (go arg)
            STArrow dom cod -> STArrow (go dom) (go cod)
            STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
            STMu name body -> STMu name (go body)
            STBottom -> STBottom

matchTypesInScope :: ElaborateScope -> Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesInScope scope =
  matchTypesWith (semanticTypeEqual scope) (sameTypeHeadInScope scope)

matchTypesWith ::
  (SrcType -> SrcType -> Bool) ->
  (String -> String -> Bool) ->
  Map String SrcType ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchTypesWith sameType sameTypeHead subst template actual = case template of
  STVar name ->
    case Map.lookup name subst of
      Nothing -> Just (Map.insert name actual subst)
      Just existing
        | sameType existing actual -> Just subst
        | otherwise -> Nothing
  STArrow dom cod ->
    case actual of
      STArrow dom' cod' -> do
        subst' <- matchTypesWith sameType sameTypeHead subst dom dom'
        matchTypesWith sameType sameTypeHead subst' cod cod'
      _ -> Nothing
  STBase {} ->
    case actual of
      STBase {} | sameType template actual -> Just subst
      _ -> Nothing
  STCon name args ->
    case actual of
      STCon name' args'
        | sameTypeHead name name' && length (toListNE args) == length (toListNE args') ->
            foldM
              (\acc (leftTy, rightTy) -> matchTypesWith sameType sameTypeHead acc leftTy rightTy)
              subst
              (zip (toListNE args) (toListNE args'))
      _ -> Nothing
  STVarApp name args ->
    matchTypeHeadApplicationWith
      (matchTypesWith sameType sameTypeHead)
      sameType
      subst
      name
      args
      actual
  STTyLam name body ->
    case actual of
      STTyLam name' body'
        | name == name' -> matchTypesWith sameType sameTypeHead subst body body'
      _ -> Nothing
  STTyApp fun arg ->
    case actual of
      STTyApp fun' arg' -> do
        subst' <- matchTypesWith sameType sameTypeHead subst fun fun'
        matchTypesWith sameType sameTypeHead subst' arg arg'
      _ -> Nothing
  STForall name mb body ->
    case actual of
      STForall name' mb' body'
        | maybe True (\bound -> maybe False (sameType (unSrcBound bound) . unSrcBound) mb') mb && name == name' ->
            matchTypesWith sameType sameTypeHead subst body body'
      _ -> Nothing
  STMu name body ->
    case actual of
      STMu name' body'
        | name == name' -> matchTypesWith sameType sameTypeHead subst body body'
      _ -> Nothing
  STBottom ->
    case actual of
      STBottom -> Just subst
      _ -> Nothing

semanticTypeEqual :: ElaborateScope -> SrcType -> SrcType -> Bool
semanticTypeEqual scope left right =
  canonicalSourceType scope left == canonicalSourceType scope right

semanticTypeViewEqual :: ElaborateScope -> TypeView -> TypeView -> Bool
semanticTypeViewEqual scope left right
  | typeViewHasIdentities left || typeViewHasIdentities right = left == right
  | otherwise = semanticTypeEqual scope (typeViewIdentity left) (typeViewIdentity right)

typeViewHasIdentities :: TypeView -> Bool
typeViewHasIdentities view =
  not (Map.null (typeViewHeadIdentities view))
    || not (Map.null (typeViewBinderIdentities view))

sameTypeHeadInScope :: ElaborateScope -> String -> String -> Bool
sameTypeHeadInScope scope left right =
  case (typeHeadIdentityInScope scope left, typeHeadIdentityInScope scope right) of
    (Just leftIdentity, Just rightIdentity) -> leftIdentity == rightIdentity
    (Nothing, Nothing) -> left == right
    _ -> False

typeHeadIdentityInScope :: ElaborateScope -> String -> Maybe SymbolIdentity
typeHeadIdentityInScope scope name =
  Map.lookup name (esTypeHeadIdentities scope)
    <|> Builtins.builtinTypeHeadIdentity name

dataIdentityTypeName :: DataInfo -> String
dataIdentityTypeName =
  symbolIdentityStableName . dataInfoSymbolIdentity

-- | Strip leading STForall binders that do not appear in the body.
stripVacuousSrcForalls :: SrcType -> SrcType
stripVacuousSrcForalls (STForall v _ body)
  | v `Set.notMember` freeTypeVarsSrcType body = stripVacuousSrcForalls body
stripVacuousSrcForalls ty = ty

quantifyFreeTypeVars :: SrcType -> SrcType
quantifyFreeTypeVars ty =
  foldr (\name acc -> STForall name Nothing acc) ty (sort (Set.toList (freeTypeVarsSrcType ty)))

-- | Collect free type variables in a SrcType.
freeTypeVarsSrcType :: SrcType -> Set String
freeTypeVarsSrcType = go Set.empty
  where
    go bound (STVar name)
      | name `Set.member` bound = Set.empty
      | otherwise = Set.singleton name
    go _ (STBase _) = Set.empty
    go _ STBottom = Set.empty
    go bound (STArrow dom cod) = go bound dom `Set.union` go bound cod
    go bound (STCon _ args) = foldMap (go bound) args
    go bound (STVarApp name args) =
      let headVars =
            if name `Set.member` bound
              then Set.empty
              else Set.singleton name
       in headVars `Set.union` foldMap (go bound) args
    go bound (STTyLam name body) = go (Set.insert name bound) body
    go bound (STTyApp fun arg) = go bound fun `Set.union` go bound arg
    go bound (STForall name mb body) =
      let bound' = Set.insert name bound
          mbFvs = maybe Set.empty (go bound . unSrcBound) mb
       in mbFvs `Set.union` go bound' body
    go bound (STMu name body) = go (Set.insert name bound) body

extendConstraintEvidenceInfo :: ElaborateScope -> [ConstraintInfo] -> ElaborateM (ElaborateScope, [(String, SrcType)])
extendConstraintEvidenceInfo scope constraints = do
  mapM_ requireKnownClass constraints
  built <- mapM buildEvidence (concatMap (constraintEvidenceClosureInfo scope) constraints)
  let evidenceInfos = concatMap first built
      params = concatMap second built
      runtimeTypeViews = Map.unions (map third built)
  pure
    ( scope
        { esEvidence = evidenceInfos ++ esEvidence scope,
          esRuntimeTypeViews = runtimeTypeViews `Map.union` esRuntimeTypeViews scope
        },
      params
    )
  where
    requireKnownClass constraint =
      case classInfoForConstraint scope constraint of
        Just _ -> pure ()
        Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))

    buildEvidence (classInfo, constraint) = do
      methodEntries <-
        mapM
          ( \methodInfo -> do
              runtimeName <- freshRuntimeName ("evidence_" ++ symbolIdentityStableName (constraintClassSymbol constraint) ++ "_" ++ methodInfoStableName methodInfo)
              let evidenceTypeViewRaw =
                    methodEvidenceSourceTypeInfoViewRaw (esTypes scope) (esClassesByIdentity scope) classInfo (constraintTypeViews constraint) methodInfo
                  evidenceTypeView = evidenceTypeViewRaw
                  methodEvidence =
                    EvidenceMethod
                      { evidenceMethodRuntimeName = runtimeName,
                        evidenceMethodSymbol = methodInfoSymbolIdentity methodInfo,
                        evidenceMethodResolvedVar = Nothing,
                        evidenceMethodTypeView = evidenceTypeView
                      }
              pure
                ( methodName methodInfo,
                  methodEvidence
                )
          )
          (Map.elems (classMethodsByIdentity classInfo))
      let evidenceMethodsByIdentity0 =
            uniqueInfoEntriesByIdentity
              [(evidenceMethodSymbol methodEvidence, methodEvidence) | (_, methodEvidence) <- methodEntries]
      let evidenceInfo =
            EvidenceInfo
              { evidenceClassSymbol = constraintClassSymbol constraint,
                evidenceTypeViews = constraintTypeViews constraint,
                evidenceMethodsByIdentity = evidenceMethodsByIdentity0
              }
          params =
            [ (evidenceMethodRuntimeName methodEvidence, typeViewDisplay (evidenceMethodTypeView methodEvidence))
            | (_, methodEvidence) <- methodEntries
            ]
          runtimeTypeViews =
            Map.fromList
              [ (evidenceMethodRuntimeName methodEvidence, evidenceMethodTypeView methodEvidence)
              | (_, methodEvidence) <- methodEntries
              ]
      pure ([evidenceInfo], params, runtimeTypeViews)

    first (value, _, _) = value
    second (_, value, _) = value
    third (_, _, value) = value

extendLocal :: ElaborateScope -> String -> String -> Maybe SrcType -> ElaborateM ElaborateScope
extendLocal scope sourceName runtimeName mbTy = do
  localRef <- freshElaborateLocalRef sourceName
  extendLocalWithRef scope localRef sourceName runtimeName mbTy

extendLocalWithRef :: ElaborateScope -> LocalRef -> String -> String -> Maybe SrcType -> ElaborateM ElaborateScope
extendLocalWithRef scope localRef sourceName runtimeName mbTy = do
  case mbTy of
    Just sourceTy -> pure (extendLocalSourceTypePure scope localRef sourceName runtimeName sourceTy)
    Nothing -> do
      loweredTy <- freshTypeName
      pure (extendLocalLoweredPure scope localRef sourceName runtimeName loweredTy)

extendLocalLowered :: ElaborateScope -> String -> String -> SrcType -> ElaborateM ElaborateScope
extendLocalLowered scope sourceName runtimeName loweredTy = do
  localRef <- freshElaborateLocalRef sourceName
  extendLocalLoweredWithRef scope localRef sourceName runtimeName loweredTy

extendLocalLoweredWithRef :: ElaborateScope -> LocalRef -> String -> String -> SrcType -> ElaborateM ElaborateScope
extendLocalLoweredWithRef scope localRef sourceName runtimeName loweredTy =
  pure (extendLocalLoweredPure scope localRef sourceName runtimeName loweredTy)

extendResolvedLocal :: ElaborateScope -> LocalRef -> String -> Maybe SrcType -> ElaborateM ElaborateScope
extendResolvedLocal scope localRef runtimeName mbTy =
  case mbTy of
    Just sourceTy -> pure (extendResolvedLocalSourceTypePure scope localRef runtimeName sourceTy)
    Nothing -> do
      loweredTy <- freshTypeName
      pure (extendResolvedLocalLoweredPure scope localRef runtimeName loweredTy)

extendResolvedLocalView :: ElaborateScope -> LocalRef -> String -> Maybe TypeView -> ElaborateM ElaborateScope
extendResolvedLocalView scope localRef runtimeName mbView =
  case mbView of
    Just sourceView -> pure (extendResolvedLocalTypeViewPure scope localRef runtimeName sourceView)
    Nothing -> do
      loweredTy <- freshTypeName
      pure (extendResolvedLocalLoweredPure scope localRef runtimeName loweredTy)

extendResolvedLocalLowered :: ElaborateScope -> LocalRef -> String -> SrcType -> ElaborateM ElaborateScope
extendResolvedLocalLowered scope localRef runtimeName loweredTy =
  pure (extendResolvedLocalLoweredPure scope localRef runtimeName loweredTy)

recordResolvedLocalIdentity :: String -> LocalRef -> ElaborateM ()
recordResolvedLocalIdentity runtimeName localRef =
  modify
    ( \state ->
        state
          { elaborateResolvedLocalIdentities =
              elaborateResolvedLocalIdentities state
                ++ [LoweredResolvedLocalIdentity runtimeName localRef]
          }
    )

extendLocalLoweredPure :: ElaborateScope -> LocalRef -> String -> String -> SrcType -> ElaborateScope
extendLocalLoweredPure scope localRef sourceName runtimeName loweredTy =
  insertResolvedLocalValue localRef valueInfo $
    insertLocalValue sourceName valueInfo $
      insertRuntimeTypeView runtimeName (valueTypeView valueInfo) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView =
            TypeView
              { typeViewDisplay = loweredTy,
                typeViewIdentity = loweredTy,
                typeViewHeadIdentities = typeViewHeadIdentities sourceView,
                typeViewBinderIdentities = typeViewBinderIdentities sourceView
              },
          valueConstraints = [],
          valueConstraintInfos = []
        }
    sourceView =
      sourceTypeViewInScope scope loweredTy

extendResolvedLocalLoweredPure :: ElaborateScope -> LocalRef -> String -> SrcType -> ElaborateScope
extendResolvedLocalLoweredPure scope localRef runtimeName loweredTy =
  insertResolvedLocalValue localRef valueInfo $
    insertRuntimeTypeView runtimeName (valueTypeView valueInfo) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView =
            TypeView
              { typeViewDisplay = loweredTy,
                typeViewIdentity = loweredTy,
                typeViewHeadIdentities = typeViewHeadIdentities sourceView,
                typeViewBinderIdentities = typeViewBinderIdentities sourceView
              },
          valueConstraints = [],
          valueConstraintInfos = []
        }
    sourceView =
      sourceTypeViewInScope scope loweredTy

extendLocalSourceTypePure :: ElaborateScope -> LocalRef -> String -> String -> SrcType -> ElaborateScope
extendLocalSourceTypePure scope localRef sourceName runtimeName sourceTy =
  insertResolvedLocalValue localRef valueInfo $
    insertLocalValue sourceName valueInfo $
      insertRuntimeTypeView runtimeName (lowerRuntimeTypeView scope sourceView) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView = sourceView,
          valueConstraints = [],
          valueConstraintInfos = []
        }
    sourceView =
      sourceTypeViewInScope scope sourceTy

extendResolvedLocalSourceTypePure :: ElaborateScope -> LocalRef -> String -> SrcType -> ElaborateScope
extendResolvedLocalSourceTypePure scope localRef runtimeName sourceTy =
  extendResolvedLocalTypeViewPure scope localRef runtimeName (sourceTypeViewInScope scope sourceTy)

extendResolvedLocalTypeViewPure :: ElaborateScope -> LocalRef -> String -> TypeView -> ElaborateScope
extendResolvedLocalTypeViewPure scope localRef runtimeName sourceView =
  insertResolvedLocalValue localRef valueInfo $
    insertRuntimeTypeView runtimeName (lowerRuntimeTypeView scope sourceView) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView = sourceView,
          valueConstraints = [],
          valueConstraintInfos = []
        }

insertRuntimeTypeView :: String -> TypeView -> ElaborateScope -> ElaborateScope
insertRuntimeTypeView runtimeName view scope =
  scope {esRuntimeTypeViews = Map.insert runtimeName view (esRuntimeTypeViews scope)}

lowerRuntimeTypeView :: ElaborateScope -> TypeView -> TypeView
lowerRuntimeTypeView scope view =
  view
    { typeViewDisplay = loweredDisplay,
      typeViewIdentity = loweredIdentity,
      typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities view,
            sourceTypeBinderIdentitiesInScope scope loweredDisplay,
            sourceTypeBinderIdentitiesInScope scope loweredIdentity
          ]
    }
  where
    loweredDisplay = lowerType scope (typeViewDisplay view)
    loweredIdentity = lowerType scope (typeViewIdentity view)

insertLocalValue :: String -> ValueInfo -> ElaborateScope -> ElaborateScope
insertLocalValue sourceName valueInfo scope =
  scope
    { esValues =
        Map.insert
          sourceName
          valueInfo
          (esValues scope),
      esValuesByIdentity =
        Map.insert
          (valueInfoSymbolIdentity valueInfo)
          valueInfo
          (esValuesByIdentity scope)
    }

insertResolvedLocalValue :: LocalRef -> ValueInfo -> ElaborateScope -> ElaborateScope
insertResolvedLocalValue localRef valueInfo scope =
  scope {esLocalValues = Map.insert localRef valueInfo (esLocalValues scope)}

resolvedLocalValueSymbol :: LocalRef -> String -> SymbolIdentity
resolvedLocalValueSymbol localRef runtimeName =
  symbolIdentityFromParts identity SymbolValue "<local>" runtimeName Nothing
  where
    identity = localIdentityStableUnique (localRefIdentity localRef)

expectedCodomain :: Maybe SrcType -> Maybe SrcType
expectedCodomain = \case
  Just (STArrow _ cod) -> Just cod
  _ -> Nothing

mentionsFreeValue :: String -> P.Expr -> Bool
mentionsFreeValue name = elem name . collectFreeValues Set.empty

mentionsFreeResolvedValue :: LocalRef -> P.ResolvedExpr -> Bool
mentionsFreeResolvedValue name = elem name . collectFreeResolvedValues Set.empty

collectFreeValues :: Set String -> P.Expr -> [String]
collectFreeValues bound expr = case expr of
  EVar name
    | name `Set.member` bound -> []
    | otherwise -> [name]
  ELit _ -> []
  ELam param body -> collectFreeValues (Set.insert (P.paramName param) bound) body
  EApp fun arg -> collectFreeValues bound fun ++ collectFreeValues bound arg
  ELet name _ rhs body -> collectFreeValues bound rhs ++ collectFreeValues (Set.insert name bound) body
  EAnn inner _ -> collectFreeValues bound inner
  ECase scrutinee alts ->
    collectFreeValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeValues (Set.union bound (Set.fromList (patternBinders pattern0))) body

    patternBinders = \case
      P.PatCtor _ patterns -> concatMap patternBinders patterns
      P.PatVar name -> [name]
      P.PatWildcard -> []
      P.PatAnn inner _ -> patternBinders inner

collectFreeResolvedValues :: Set LocalRef -> P.ResolvedExpr -> [LocalRef]
collectFreeResolvedValues bound expr = case expr of
  EVar (P.ResolvedLocalValue name)
    | name `Set.member` bound -> []
    | otherwise -> [name]
  EVar P.ResolvedGlobalValue {} -> []
  ELit _ -> []
  ELam param body -> collectFreeResolvedValues (Set.insert (P.paramName param) bound) body
  EApp fun arg -> collectFreeResolvedValues bound fun ++ collectFreeResolvedValues bound arg
  ELet name _ rhs body -> collectFreeResolvedValues bound rhs ++ collectFreeResolvedValues (Set.insert name bound) body
  EAnn inner _ -> collectFreeResolvedValues bound inner
  ECase scrutinee alts ->
    collectFreeResolvedValues bound scrutinee ++ concatMap collectAlt alts
  where
    collectAlt (P.Alt pattern0 body) =
      collectFreeResolvedValues (Set.union bound (Set.fromList (patternBinders pattern0))) body

    patternBinders = \case
      P.PatCtor _ patterns -> concatMap patternBinders patterns
      P.PatVar name -> [name]
      P.PatWildcard -> []
      P.PatAnn inner _ -> patternBinders inner

collectApps :: P.Expr -> (P.Expr, [P.Expr])
collectApps = go []
  where
    go acc (EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)

collectResolvedApps :: P.ResolvedExpr -> (P.ResolvedExpr, [P.ResolvedExpr])
collectResolvedApps = go []
  where
    go acc (EApp fun arg) = go (arg : acc) fun
    go acc headExpr = (headExpr, acc)


freshRuntimeName :: String -> ElaborateM String
freshRuntimeName base = do
  n <- freshNameSuffix
  pure ("$" ++ base ++ "#" ++ show n)

methodInfoStableName :: MethodInfo -> String
methodInfoStableName =
  symbolIdentityStableName . methodInfoSymbolIdentity

constructorInfoStableName :: ConstructorInfo -> String
constructorInfoStableName =
  symbolIdentityStableName . ctorInfoSymbol

freshElaborateLocalRef :: String -> ElaborateM LocalRef
freshElaborateLocalRef name = do
  state <- get
  let (localRef, generator') = freshLocalRef name (elaborateIdentityGenerator state)
  modify (\state' -> state' {elaborateIdentityGenerator = generator'})
  pure localRef

freshElaborateDeferredRef :: String -> ElaborateM DeferredRef
freshElaborateDeferredRef name = do
  state <- get
  let (ref, generator') = freshDeferredRef name (elaborateIdentityGenerator state)
  modify (\state' -> state' {elaborateIdentityGenerator = generator'})
  pure ref

freshTypeName :: ElaborateM SrcType
freshTypeName = do
  n <- freshNameSuffix
  pure (STVar ("p$" ++ show n))

freshTypeVarName :: ElaborateM String
freshTypeVarName = do
  n <- freshNameSuffix
  pure ("r$" ++ show n)

freshDeferredMethodName :: String -> ElaborateM String
freshDeferredMethodName methodName0 = do
  n <- freshNameSuffix
  pure ("$deferred_" ++ methodName0 ++ "_" ++ show n)

freshDeferredConstructorName :: String -> ElaborateM String
freshDeferredConstructorName ctorName0 = do
  n <- freshNameSuffix
  pure ("$deferred_ctor_" ++ ctorName0 ++ "_" ++ show n)

freshDeferredCaseName :: String -> ElaborateM String
freshDeferredCaseName typeName = do
  n <- freshNameSuffix
  pure ("$deferred_case_" ++ typeName ++ "_" ++ show n)

freshNameSuffix :: ElaborateM Int
freshNameSuffix = do
  state <- get
  let (UniqueIdentity n, generator') = freshIdentity (elaborateNameGenerator state)
  modify (\state' -> state' {elaborateNameGenerator = generator'})
  pure n
