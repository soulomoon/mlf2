{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeInstances,
    elaborateScopeValues,
    elaborateScopeValueInfos,
    elaborateScopeValueRuntimeAliases,
    elaborateScopeRuntimeTypeViews,
    elaborateScopeRuntimeTypes,
    elaborateScopeUniqueDataTypes,
    mkElaborateScope,
    lowerTypeView,
    lowerTypeViewWithIdentities,
    lowerTypeViewsWithIdentities,
    lowerConstructorBinding,
    constructorBindingSourceTypeView,
    constructorBindingUsesStructuralPlaceholder,
    constructorStructuralArgs,
    constructorStructuralHandlerType,
    constructorTypeView,
    lowerConstrainedResolvedExprBinding,
    lowerConstrainedResolvedExprBindingWithGenerator,
    lowerResolvedConstrainedExprBinding,
    lowerResolvedConstrainedExprBindingWithGenerator,
    lowerExprBinding,
    classInfoForConstraint,
    diagnosticTypeViewDisplay,
    lowerType,
    sourceTypeIdentityInScope,
    sourceTypeHeadIdentitiesInScope,
    sourceTypeBinderIdentitiesInScope,
    sourceTypeViewInScope,
    requireTypeViewFromSourceTypeInScope,
    typeViewFromSourceTypeInScope,
    matchTypesInScope,
    matchTypesWithHeadIdentitiesInScope,
    alphaEqTypesInScope,
    alphaEqTypesWithHeadIdentitiesInScope,
    matchTypeViewsAgainstIdentity,
    matchTypeViewsAgainstIdentityRefiningBottom,
    matchMethodTypeViews,
    rigidEvidenceTypeViewsMatch,
    resolveInstanceInfoWithIdentityType,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    resolveMethodInstanceInfoByTypeViews,
    zeroMethodConstraintCoveredByEvidenceInfo,
    lookupEvidenceMethodByClassViews,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>), filterM, foldM, replicateM, when, zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.List (find, nubBy, partition, sort, zip4)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Normalize (substSrcType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types
import MLF.Frontend.Symbol (lookupSymbolIdentityAlias, lookupSymbolIdentityExact, memberSymbolIdentityExact, sameSymbolIdentity, symbolIdentityAliasMap, symbolIdentityAliasMapWith, symbolIdentityAliasNames, symbolIdentityPayloadKey, symbolIdentityStableName)
import MLF.Frontend.Syntax
  ( Lit (..),
    ResolvedSrcBound (..),
    ResolvedSrcTy (..),
    ResolvedSrcType,
    SrcBound (..),
    SrcTy (..),
    SrcType,
    ResolvedSurfaceExpr,
    resolvedSrcTypeBinderName,
  )
import qualified MLF.Frontend.Syntax as S
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
    localIdentityStableUnique,
    renameLocalRef,
    typeBinderIdentityFromUnique,
    typeBinderIdentityFromStructural,
    typeBinderIdentityStructural,
    typeBinderIdentityStableName,
  )

data ElaborateScope = ElaborateScope
  { esValues :: Map String ValueInfo,
    esLocalValues :: Map LocalRef ValueInfo,
    esValuesByIdentity :: Map SymbolIdentity ValueInfo,
    esValueRuntimeAliasesByIdentity :: Map SymbolIdentity [String],
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

elaborateScopeValueInfos :: ElaborateScope -> [ValueInfo]
elaborateScopeValueInfos =
  Map.elems . esValuesByIdentity

elaborateScopeValueRuntimeAliases :: ElaborateScope -> ValueInfo -> [String]
elaborateScopeValueRuntimeAliases scope valueInfo =
  Map.findWithDefault [] (valueInfoSymbolIdentity valueInfo) (esValueRuntimeAliasesByIdentity scope)

data ElaborateState = ElaborateState
  { elaborateBindingIdentity :: Maybe LoweredBindingIdentity,
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
    elaborateResultResolvedLocalIdentities :: [LoweredResolvedLocalIdentity],
    elaborateResultIdentityGenerator :: IdentityGenerator
  }

type ClassIdentity = SymbolIdentity

runElaborateM :: ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateM =
  runElaborateMWithSeed []

runElaborateMWithSeed :: [UniqueIdentity] -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMWithSeed seedIdentities action =
  runElaborateMWithGenerator (identityGeneratorAfter seedIdentities) action

runElaborateMWithGenerator :: IdentityGenerator -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMWithGenerator =
  runElaborateMWithContext Nothing

runElaborateMForBinding :: LoweredBindingIdentity -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMForBinding bindingIdentity =
  runElaborateMWithContext
    (Just bindingIdentity)
    (identityGeneratorAfter [])

runElaborateMWithGeneratorForBinding :: IdentityGenerator -> LoweredBindingIdentity -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMWithGeneratorForBinding generator bindingIdentity =
  runElaborateMWithContext (Just bindingIdentity) generator

runElaborateMWithContext :: Maybe LoweredBindingIdentity -> IdentityGenerator -> ElaborateM a -> Either ProgramError (ElaborateResult a)
runElaborateMWithContext mbBindingIdentity generator action =
  let initialState =
        ElaborateState
          { elaborateBindingIdentity = mbBindingIdentity,
            elaborateIdentityGenerator = generator,
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
                elaborateResultResolvedLocalIdentities = elaborateResolvedLocalIdentities finalState,
                elaborateResultIdentityGenerator = elaborateIdentityGenerator finalState
              }

mkElaborateScope :: Map String ValueInfo -> Map String DataInfo -> Map String ClassInfo -> [InstanceInfo] -> ElaborateScope
mkElaborateScope values0 dataTypes0 classes0 instances0 =
  ElaborateScope
        { esValues = values0,
          esLocalValues = Map.empty,
          esValuesByIdentity = valuesByIdentity,
          esValueRuntimeAliasesByIdentity = valueRuntimeAliasesByIdentity,
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
      concatMap runtimeTypeInfoAliases runtimeTypeValueInfos

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
      case (Set.toList (Set.fromList (map (symbolIdentityPayloadKey . valueInfoSymbolIdentity) infos)), infos) of
        ([_], info : rest)
          | all (== info) rest -> Just info
        _ -> Nothing

    runtimeTypeValueInfos =
      filter shouldTrackRuntimeType (Map.elems values0 ++ instanceMethodValues)

    runtimeTypeInfoAliases info =
      rawRuntimeAliases
        ++ identityRuntimeAliases
      where
        rawRuntimeAliases =
          [ (runtimeName, info)
          | runtimeName <- maybe [] pure (valueInfoRawRuntimeName info)
          ]
        identityRuntimeAliases =
          [ (alias, info)
          | validRuntimeTypeInfo info,
            alias <- valueInfoIdentityRuntimeAliases info
          ]

    validRuntimeTypeInfo info =
      case lookupSymbolIdentityExact (valueInfoSymbolIdentity info) validRuntimeTypeInfosByIdentity of
        Just validInfo -> validInfo == info
        Nothing -> False

    validRuntimeTypeInfosByIdentity =
      indexInfoListByIdentity valueInfoSymbolIdentity runtimeTypeValueInfos

    instanceMethodValueIdentities =
      Set.fromList
        [ valueInfoSymbolIdentity methodInfo
        | methodInfo <- instanceMethodValues
        ]

    valueRuntimeTypeViewFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints}
      | null constraints,
        not (memberSymbolIdentityExact (valueInfoSymbolIdentity valueInfo) instanceMethodValueIdentities) =
          loweredRuntimeTypeViewFor valueInfo
      | otherwise =
          constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueRuntimeTypeViewFor valueInfo =
      loweredRuntimeTypeViewFor valueInfo

    loweredRuntimeTypeViewFor valueInfo =
      let loweredDisplayCandidate = lowerTypeRaw dataTypes (valueTypeFor valueInfo)
          loweredIdentity = lowerIdentityTypeRaw dataTypes (valueIdentityTypeFor valueInfo)
          identityViews = runtimeLoweringIdentityViewsFor valueInfo
          headIdentities =
            mergeSymbolIdentityMaps
              ( identityHeadAliases
                  : valueTypeHeadIdentitiesFor valueInfo
                  : map typeViewDirectHeadIdentityAliases identityViews
              )
          identityHeadAliases =
            Map.filter
              (`Set.member` foldMap typeViewMentionedHeadIdentities identityViews)
              dataTypeHeadIdentities
          binderIdentities =
            mergeWithAuthoritativeTypeBinderIdentities
              (structuralBinderIdentitiesForHeadAliases headIdentities)
              (valueTypeBinderIdentitiesFor valueInfo : map typeViewDirectBinderIdentityAliases identityViews)
          identityView =
            requireTypeViewFromSourceType headIdentities binderIdentities loweredIdentity
          transportedView =
            case typeViewWithDisplay loweredDisplayCandidate identityView of
              Right loweredView -> loweredView
              Left _ -> identityView
       in retainTypeViewLookupAliases identityViews transportedView

    -- Structural lowering can expose constructor field types that are not
    -- themselves visible in the importing module.  Retain their sidecars by
    -- following the exact data identities mentioned by the source TypeViews.
    -- Constructor views then supply the hidden identities reached through
    -- that exact data closure.
    runtimeLoweringIdentityViewsFor valueInfo =
      collectDataIdentityClosure dataTypesByIdentity Set.empty (valueIdentityRootViews valueInfo)

    valueIdentityRootViews valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      [constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)]
    valueIdentityRootViews ConstructorValue {valueCtorInfo = ctorInfo} =
      [ctorTypeView ctorInfo]
    valueIdentityRootViews OverloadedMethod {} = []

    valueTypeFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      lowerTypeViewRaw dataTypes (constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo))
    valueTypeFor valueInfo@ConstructorValue {valueCtorInfo = ctorInfo} =
      let quantifiedTy = quantifyFreeTypeVars ty
          loweredTy = lowerTypeViewRaw dataTypes (quantifyFreeTypeView (ctorTypeView ctorInfo))
       in if constructorOwnerHasVariableHeadApplication dataTypesByIdentity ctorInfo
            && srcTypeHasVariableHeadApplication loweredTy
            then constructorStructuralPlaceholderTypeFor dataTypesByIdentity ctorInfo
            else quantifiedTy
      where
        ty = valueType valueInfo
    valueTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

    valueIdentityTypeFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      typeViewIdentity $
        constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueIdentityTypeFor ConstructorValue {valueCtorInfo = ctorInfo} =
      let quantifiedView = quantifyFreeTypeView (ctorTypeView ctorInfo)
          identityTy = typeViewIdentity quantifiedView
          loweredTy = lowerIdentityTypeRaw dataTypes identityTy
       in if constructorOwnerHasVariableHeadApplication dataTypesByIdentity ctorInfo
            && srcTypeHasVariableHeadApplication loweredTy
            then constructorStructuralPlaceholderTypeFor dataTypesByIdentity ctorInfo
            else identityTy
    valueIdentityTypeFor OverloadedMethod {} = error "overloaded methods do not have concrete runtime types"

    valueTypeHeadIdentitiesFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      typeViewDirectHeadIdentityAliases $
        constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueTypeHeadIdentitiesFor ConstructorValue {valueCtorInfo = ctorInfo} =
      typeViewDirectHeadIdentityAliases (ctorTypeView ctorInfo)
    valueTypeHeadIdentitiesFor OverloadedMethod {} = Map.empty

    valueTypeBinderIdentitiesFor valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
      typeViewDirectBinderIdentityAliases $
        constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints (ordinaryValueTypeView valueInfo)
    valueTypeBinderIdentitiesFor ConstructorValue {valueCtorInfo = ctorInfo} =
      constructorTypeBinderIdentitiesFor ctorInfo
    valueTypeBinderIdentitiesFor OverloadedMethod {} = Map.empty

    constructorTypeBinderIdentitiesFor ctorInfo =
      mergeTypeBinderIdentityMaps
        [ typeViewDirectBinderIdentityAliases (ctorTypeView ctorInfo),
          ownerParamIdentities,
          forallIdentities
        ]
      where
        ownerParamIdentities =
          case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctorInfo) dataTypesByIdentity of
            Just dataInfo ->
              typeBinderAliasIdentityMap (dataParamBinders dataInfo)
            Nothing -> Map.empty

        forallIdentities =
          typeBinderAliasIdentityMap
            [ (constructorForallDisplayName binder, constructorForallIdentity binder)
            | binder <- ctorForallBinderInfo ctorInfo
            ]

    valueIdentityInfos =
      Map.elems values0 ++ instanceMethodValues

    valuesByIdentity =
      indexInfoListByIdentity valueInfoSymbolIdentity valueIdentityInfos

    valueRuntimeAliasesByIdentity =
      Map.map Set.toList $
        Map.fromListWith
          Set.union
          [ (valueInfoSymbolIdentity info, Set.fromList (valueInfoRuntimeAliases info))
          | info <- valueIdentityInfos,
            Just validInfo <- [lookupSymbolIdentityExact (valueInfoSymbolIdentity info) valuesByIdentity],
            validInfo == info
          ]

    instanceMethodValues =
      [ methodValue
        | instanceInfo <- instances0,
          methodValue <- Map.elems (instanceMethodsByIdentity instanceInfo)
      ]

addIdentityTypeAliases :: Map String DataInfo -> Map String DataInfo
addIdentityTypeAliases dataTypes =
  addResolvedTypeAliases aliases dataTypes
  where
    aliases =
      symbolIdentityAliasMap (Map.keys dataTypesByIdentity)

    dataTypesByIdentity =
      indexInfoByIdentity dataInfoSymbolIdentity dataTypes

addResolvedTypeAliases :: Map String SymbolIdentity -> Map String DataInfo -> Map String DataInfo
addResolvedTypeAliases aliases dataTypes =
  Map.foldlWithKey insertAlias dataTypes aliases
  where
    dataTypesByIdentity =
      indexInfoByIdentity dataInfoSymbolIdentity dataTypes

    insertAlias acc name identity =
      case Map.lookup name acc of
        Just {} -> acc
        Nothing ->
          case lookupSymbolIdentityExact identity dataTypesByIdentity of
            Just info -> Map.insert name info acc
            Nothing -> acc

indexInfoByIdentity :: (Eq a) => (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity a
indexInfoByIdentity identityOf =
  uniqueInfoByIdentity identityOf

indexInfoListByIdentity :: (Eq a) => (a -> SymbolIdentity) -> [a] -> Map SymbolIdentity a
indexInfoListByIdentity identityOf =
  uniqueInfoListByIdentity identityOf

indexDisplayNamesByIdentity :: (a -> SymbolIdentity) -> Map String a -> Map SymbolIdentity [String]
indexDisplayNamesByIdentity identityOf =
  uniqueDisplayNamesByIdentity . map (\(name, info) -> (identityOf info, name)) . Map.toList

dataTypeHeadIdentityAliases :: Map SymbolIdentity DataInfo -> Map SymbolIdentity [String] -> Map String SymbolIdentity
dataTypeHeadIdentityAliases dataTypesByIdentity displayNamesByIdentity =
  symbolIdentityAliasMapWith
    [ ( identity,
        dataInfoIdentityQualifiedName info
          : Map.findWithDefault [] identity displayNamesByIdentity
      )
    | (identity, info) <- Map.toList dataTypesByIdentity
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

lowerIdentityTypeView :: ElaborateScope -> TypeView -> SrcType
lowerIdentityTypeView scope view =
  lowerIdentityTypeRaw
    (addResolvedTypeAliases (typeViewDirectHeadIdentityAliases view) (esTypes scope))
    (typeViewIdentity view)

lowerTypeView :: ElaborateScope -> TypeView -> SrcType
lowerTypeView scope = lowerTypeViewRaw (esTypes scope)

-- | Lower a semantic type view for transport through the resolved surface
-- pipeline.  Resolved annotations must use the identity projection: using the
-- display projection here makes a same-spelled binder depend on a later alias
-- lookup and can silently freshen it to a different identity.
loweredTypeViewIdentity :: ElaborateScope -> TypeView -> SrcType
loweredTypeViewIdentity scope =
  typeViewIdentity . lowerTypeViewWithIdentities scope

lowerTypeViewWithIdentities :: ElaborateScope -> TypeView -> TypeView
lowerTypeViewWithIdentities =
  lowerTypeViewWithIdentityContext

lowerTypeViewsWithIdentities :: ElaborateScope -> Map k TypeView -> Map k TypeView
lowerTypeViewsWithIdentities scope =
  Map.map (lowerTypeViewWithIdentityContext scope)

lowerTypeViewWithIdentityContext ::
  ElaborateScope ->
  TypeView ->
  TypeView
lowerTypeViewWithIdentityContext scope view =
  retainTypeViewLookupAliases identityViews transportedView
  where
    transportedView =
      case typeViewWithDisplay loweredDisplayCandidate identityView of
        Right loweredView -> loweredView
        Left _ -> identityView
    loweredDisplayCandidate = lowerTypeView scope view
    loweredIdentity = lowerIdentityTypeView scope view
    identityViews =
      collectDataIdentityClosure (esTypesByIdentity scope) Set.empty [view]
    -- The transported view owns the identities mentioned by its syntax plus
    -- identities exposed by its exact data-lowering closure.  A TypeView may
    -- still carry context aliases from an earlier, wider scope; those aliases
    -- are lookup history, not authority for a newly introduced structural
    -- binder.  Rebuild aliases from the mentioned nodes before constructing
    -- the lowered view so a stale context spelling cannot assign another
    -- data owner's identity to @$Owner_self@.
    headIdentities =
      typeViewDirectHeadIdentityAliases view
        `Map.union` mergeSymbolIdentityMaps
          ( sourceTypeHeadIdentitiesInScope scope loweredDisplayCandidate
              : sourceTypeHeadIdentitiesInScope scope loweredIdentity
              : map typeViewDirectHeadIdentityAliases identityViews
          )
    -- Every binder introduced by nominal lowering has semantic provenance in
    -- the input view, its constructor closure, or the structural data owner.
    -- Do not repair a missing route with a local fresh identity: the required
    -- TypeView constructor below must fail closed instead.
    binderIdentities =
      mergeWithAuthoritativeTypeBinderIdentities
        ( typeViewDirectBinderIdentityAliases view
            `Map.union` structuralBinderIdentitiesForHeadAliases headIdentities
        )
        ( sourceTypeBinderIdentitiesInScope scope loweredDisplayCandidate
            : sourceTypeBinderIdentitiesInScope scope loweredIdentity
            : map typeViewDirectBinderIdentityAliases identityViews
        )
    identityView =
      requireTypeViewFromSourceType headIdentities binderIdentities loweredIdentity

-- | Preserve lookup history across a structural TypeView rebuild without
-- granting that history structural authority.  The caller first constructs a
-- complete identity projection from aliases owned by syntax nodes; only then
-- do context-only aliases re-enter as lookup metadata on the finished view.
retainTypeViewLookupAliases :: [TypeView] -> TypeView -> TypeView
retainTypeViewLookupAliases sourceViews =
  typeViewMergeBinderIdentityAliases retainedBinderAliases
    . typeViewMergeHeadIdentityAliases retainedHeadAliases
  where
    retainedHeadAliases =
      mergeSymbolIdentityMaps (map typeViewHeadIdentities sourceViews)
    retainedBinderAliases =
      mergeTypeBinderIdentityMaps (map typeViewBinderIdentities sourceViews)

-- | Reconstruct the aliases justified by nodes in this view, excluding
-- context-only aliases retained for an earlier lookup scope.  Each direct
-- spelling is paired with its payload before canonical identity aliases are
-- added, so ambiguous spellings remain absent rather than becoming
-- traversal-order dependent.
typeViewDirectHeadIdentityAliases :: TypeView -> Map String SymbolIdentity
typeViewDirectHeadIdentityAliases view =
  symbolIdentityAliasMapWith (collect view)
  where
    collect current =
      case typeViewNodeView current of
        TypeViewVarNode {} -> []
        TypeViewArrowNode domain codomain -> collect domain ++ collect codomain
        TypeViewBaseNode displayName identity -> [(identity, [displayName])]
        TypeViewConNode displayName identity args ->
          (identity, [displayName]) : concatMap collect (NE.toList args)
        TypeViewVarAppNode _ _ args -> concatMap collect (NE.toList args)
        TypeViewTyLamNode _ _ body -> collect body
        TypeViewTyAppNode fun arg -> collect fun ++ collect arg
        TypeViewForallNode _ _ mbBound body -> maybe [] collect mbBound ++ collect body
        TypeViewMuNode _ _ body -> collect body
        TypeViewBottomNode -> []

typeViewDirectBinderIdentityAliases :: TypeView -> Map String TypeBinderIdentity
typeViewDirectBinderIdentityAliases view =
  typeBinderAliasIdentityMap (collect view)
  where
    collect current =
      case typeViewNodeView current of
        TypeViewVarNode displayName identity -> [(displayName, identity)]
        TypeViewArrowNode domain codomain -> collect domain ++ collect codomain
        TypeViewBaseNode {} -> []
        TypeViewConNode _ _ args -> concatMap collect (NE.toList args)
        TypeViewVarAppNode displayName identity args ->
          (displayName, identity) : concatMap collect (NE.toList args)
        TypeViewTyLamNode displayName identity body ->
          (displayName, identity) : collect body
        TypeViewTyAppNode fun arg -> collect fun ++ collect arg
        TypeViewForallNode displayName identity mbBound body ->
          (displayName, identity) : maybe [] collect mbBound ++ collect body
        TypeViewMuNode displayName identity body ->
          (displayName, identity) : collect body
        TypeViewBottomNode -> []

structuralBinderIdentitiesForHeadAliases :: Map String SymbolIdentity -> Map String TypeBinderIdentity
structuralBinderIdentitiesForHeadAliases headIdentities =
  typeBinderAliasIdentityMap
    [ ("$" ++ alias ++ suffix, typeBinderIdentityFromStructural (symbolUniqueIdentity identity) role)
    | (alias, identity) <- Map.toList headIdentities,
      (suffix, role) <- [("_self", StructuralSelfBinder), ("_result", StructuralResultBinder)]
    ]

mergeWithAuthoritativeTypeBinderIdentities :: Map String TypeBinderIdentity -> [Map String TypeBinderIdentity] -> Map String TypeBinderIdentity
mergeWithAuthoritativeTypeBinderIdentities authoritative candidates =
  authoritative `Map.union` mergeTypeBinderIdentityMaps candidates

collectDataIdentityClosure :: Map SymbolIdentity DataInfo -> Set SymbolIdentity -> [TypeView] -> [TypeView]
collectDataIdentityClosure _ _ [] = []
collectDataIdentityClosure dataTypesByIdentity seen (view : rest) =
  view : collectDataIdentityClosure dataTypesByIdentity seen' (constructorViews ++ rest)
  where
    newDataInfos =
      [ info
      | identity <- Set.toList (typeViewMentionedHeadIdentities view),
        identity `Set.notMember` seen,
        Just info <- [lookupSymbolIdentityExact identity dataTypesByIdentity]
      ]
    seen' =
      foldr
        (Set.insert . dataInfoSymbolIdentity)
        seen
        newDataInfos
    constructorViews =
      [ ctorTypeView ctor
      | info <- newDataInfos,
        ctor <- dataConstructors info
      ]

lowerTypeViewRaw :: Map String DataInfo -> TypeView -> SrcType
lowerTypeViewRaw dataTypes view =
  lowerTypeRaw
    dataTypes
    (visibleTypeForTypeView dataTypes view)

diagnosticTypeViewDisplay :: ElaborateScope -> TypeView -> SrcType
diagnosticTypeViewDisplay scope view =
  go view
  where
    go current =
      case typeViewNodeView current of
        TypeViewVarNode displayName _ -> STVar displayName
        TypeViewArrowNode dom cod -> STArrow (go dom) (go cod)
        TypeViewBaseNode displayName identity -> STBase (diagnosticHeadName displayName identity)
        TypeViewConNode displayName identity args ->
          STCon (diagnosticHeadName displayName identity) (fmap go args)
        TypeViewVarAppNode displayName _ args ->
          STVarApp displayName (fmap go args)
        TypeViewTyLamNode displayName _ body ->
          STTyLam displayName (go body)
        TypeViewTyAppNode fun arg ->
          STTyApp (go fun) (go arg)
        TypeViewForallNode displayName _ mbBound body ->
          STForall displayName (fmap (SrcBound . go) mbBound) (go body)
        TypeViewMuNode displayName _ body ->
          STMu displayName (go body)
        TypeViewBottomNode -> STBottom

    diagnosticHeadName displayName identity
      | displayName /= unqualifiedSymbolName displayName,
        Just displayIdentity <- typeHeadIdentityInScope scope displayName,
        sameSymbolIdentity displayIdentity identity =
          displayName
      | let builtinName = Builtins.normalizeBuiltinTypeReference (symbolIdentityStableName identity),
        Builtins.isBuiltinTypeName builtinName =
          builtinName
      | Just visibleName <-
          lookupSymbolIdentityExact identity (esTypeDisplayNamesByIdentity scope)
            >>= preferredDisplayName identity,
        visibleName /= unqualifiedSymbolName visibleName =
          visibleName
      | qualifiedName : _ <- qualifiedNamesForIdentity identity =
          qualifiedName
      | otherwise = symbolIdentityStableName identity

    qualifiedNamesForIdentity identity =
      [ dataInfoIdentityQualifiedName info
      | info <- elaborateScopeUniqueDataTypes scope,
        sameSymbolIdentity (dataInfoSymbolIdentity info) identity
      ]

visibleTypeForTypeView :: Map String DataInfo -> TypeView -> SrcType
visibleTypeForTypeView dataTypes = go
  where
    dataTypesByIdentity =
      indexInfoByIdentity dataInfoSymbolIdentity dataTypes

    dataTypeDisplayNamesByIdentity =
      indexDisplayNamesByIdentity dataInfoSymbolIdentity dataTypes

    dataTypeHeadIdentities =
      dataTypeHeadIdentityAliases dataTypesByIdentity dataTypeDisplayNamesByIdentity

    go view =
      case typeViewNodeView view of
        TypeViewVarNode displayName _ -> STVar displayName
        TypeViewArrowNode dom cod -> STArrow (go dom) (go cod)
        TypeViewBaseNode displayName identity -> STBase (visibleHeadName identity displayName)
        TypeViewConNode displayName identity args ->
          STCon (visibleHeadName identity displayName) (fmap go args)
        TypeViewVarAppNode displayName _ args ->
          STVarApp displayName (fmap go args)
        TypeViewTyLamNode displayName _ body ->
          STTyLam displayName (go body)
        TypeViewTyAppNode fun arg ->
          STTyApp (go fun) (go arg)
        TypeViewForallNode displayName _ mbBound body ->
          STForall displayName (fmap (SrcBound . go) mbBound) (go body)
        TypeViewMuNode displayName _ body ->
          STMu displayName (go body)
        TypeViewBottomNode -> STBottom

    visibleHeadName identity displayName =
      case lowerTypeHeadIdentity displayName of
        Just displayIdentity
          | sameSymbolIdentity displayIdentity identity -> displayName
        _ -> symbolIdentityStableName identity

    lowerTypeHeadIdentity name =
      dataInfoSymbolIdentity <$> Map.lookup name dataTypes
        <|> lookupSymbolIdentityAlias dataTypeHeadIdentities name
        <|> Builtins.builtinTypeHeadIdentity name

sourceTypeViewInScope :: HasCallStack => ElaborateScope -> SrcType -> TypeView
sourceTypeViewInScope scope ty =
  requireTypeViewFromSourceTypeInScope scope Map.empty Map.empty ty

requireTypeViewFromSourceTypeInScope ::
  HasCallStack =>
  ElaborateScope ->
  Map String SymbolIdentity ->
  Map String TypeBinderIdentity ->
  SrcType ->
  TypeView
requireTypeViewFromSourceTypeInScope scope extraHeadIdentities extraBinderIdentities ty =
  case typeViewFromSourceTypeInScope scope extraHeadIdentities extraBinderIdentities ty of
    Right view -> view
    Left err ->
      error
        ( "identity-incomplete TypeView construction: "
            ++ show err
            ++ "; head aliases="
            ++ show (Map.keys headIdentities)
            ++ "; binder aliases="
            ++ show (Map.keys binderIdentities)
        )
  where
    headIdentities =
      extraHeadIdentities `Map.union` sourceTypeHeadIdentitiesInScope scope ty
    binderIdentities =
      extraBinderIdentities `Map.union` sourceTypeBinderIdentitiesInScope scope ty

typeViewFromSourceTypeInScope ::
  ElaborateScope ->
  Map String SymbolIdentity ->
  Map String TypeBinderIdentity ->
  SrcType ->
  Either TypeViewConstructionError TypeView
typeViewFromSourceTypeInScope scope extraHeadIdentities extraBinderIdentities ty =
  preferVisibleTypeView scope
    <$> typeViewFromSourceType
      (extraHeadIdentities `Map.union` sourceTypeHeadIdentitiesInScope scope ty)
      (extraBinderIdentities `Map.union` sourceTypeBinderIdentitiesInScope scope ty)
      ty

typeViewWithScopeAliases :: ElaborateScope -> TypeView -> TypeView
typeViewWithScopeAliases scope view =
  typeViewWithIdentityAliases
    ( mergeSymbolIdentityMaps
        [ typeViewHeadIdentities view,
          sourceTypeHeadIdentitiesInScope scope (typeViewDisplay view)
        ]
    )
    ( mergeTypeBinderIdentityMaps
        [ typeViewBinderIdentities view,
          sourceTypeBinderIdentitiesInScope scope (typeViewDisplay view)
        ]
    )
    view

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
  Set.toList $
    Set.fromList
      ( dataInfoIdentityQualifiedName info
          : symbolIdentityAliasNames (dataInfoSymbolIdentity info)
      )

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
    templateBinderKey name =
      case typeViewBinderIdentityForAlias template name of
        Just identity -> Just identity
        Nothing
          | Map.null (typeViewBinderIdentities template) ->
              typeViewSubstKeyForTemplateName template name
          | otherwise -> Nothing

typeViewSubstKeyForTemplateName :: TypeView -> String -> Maybe TypeBinderIdentity
typeViewSubstKeyForTemplateName template identityName =
  case typeViewBinderIdentityForAlias template identityName of
    Just identity -> Just identity
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
  symbolIdentityAliasMapWith entries
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
          case lookupSymbolIdentityExact identity (esTypesByIdentity scope) of
            Just info -> dataHeadIdentityEntries name info
            Nothing -> builtinHeadIdentityEntries name identity
        Nothing -> []

    dataHeadIdentityEntries name info =
      [(dataInfoSymbol info, [name])]

    builtinHeadIdentityEntries name identity =
      [(identity, [name, Builtins.normalizeBuiltinTypeReference name])]

constrainedRuntimeTypeInfoViewRaw :: Map String DataInfo -> Map SymbolIdentity ClassInfo -> [ConstraintInfo] -> TypeView -> TypeView
constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity constraints visibleView =
  typeViewAddArgumentsInsideForalls evidenceViews visibleView
  where
    evidenceViews = concatMap constraintEvidenceTypes constraints

    constraintEvidenceTypes constraint =
      [ methodEvidenceSourceTypeInfoViewRaw dataTypes classesByIdentity classInfo (constraintTypeViews evidenceConstraint) methodInfo
        | (classInfo, evidenceConstraint) <- constraintEvidenceClosureInfoRaw classesByIdentity constraint,
          methodInfo <- Map.elems (classMethodsByIdentity classInfo)
      ]

constraintEvidenceClosureInfoRaw :: Map SymbolIdentity ClassInfo -> ConstraintInfo -> [(ClassInfo, ConstraintInfo)]
constraintEvidenceClosureInfoRaw classesByIdentity =
  go []
  where
    go seen constraint =
      case lookupSymbolIdentityExact (constraintClassSymbol constraint) classesByIdentity of
        Just classInfo ->
          let key = classConstraintEvidenceKeyInfo classInfo constraint
           in if key `elem` seen
                then []
                else
                  let seen' = key : seen
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
  let specializedMethodView = specializeMethodTypeView methodInfo classArgViews
      specializedConstraints =
        map
          (applyConstraintInfoSubst (typeViewSubstFromParamIdentities (classParamBinderIdentities classInfo) classArgViews))
          (methodConstraintInfos methodInfo)
      headVars = freeTypeBinderIdentitiesTypeViews classArgViews
      (evidenceVisibleView, specializedConstraints') =
        quantifyMethodLocalVarsInfoView headVars specializedConstraints specializedMethodView
      deferredConstraints =
        filter (not . constraintInfoDeterminedByTypeBinderIdentities headVars) specializedConstraints'
   in constrainedRuntimeTypeInfoViewRaw dataTypes classesByIdentity deferredConstraints evidenceVisibleView

constraintInfoDeterminedByTypeBinderIdentities :: Set TypeBinderIdentity -> ConstraintInfo -> Bool
constraintInfoDeterminedByTypeBinderIdentities typeVars constraint =
  freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint)
    `Set.isSubsetOf` typeVars

constraintInfoGroundByTypeBinderIdentities :: ConstraintInfo -> Bool
constraintInfoGroundByTypeBinderIdentities constraint =
  Set.null (freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint))

constraintInfoHasFreeTypeBinderIdentities :: ConstraintInfo -> Bool
constraintInfoHasFreeTypeBinderIdentities constraint =
  not (Set.null (freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint)))

quantifyMethodLocalVarsInfoView :: Set TypeBinderIdentity -> [ConstraintInfo] -> TypeView -> (TypeView, [ConstraintInfo])
quantifyMethodLocalVarsInfoView headVars constraints view =
  ( typeViewQuantifyBinders
      localVarPairs
      ( typeViewMergeBinderIdentityAliases
          (mergeTypeBinderIdentityMaps (map constraintBinderIdentities canonicalConstraints))
          canonicalView
      ),
    canonicalConstraints
  )
  where
    constraintVars =
      foldMap (freeTypeBinderIdentitiesTypeViews . constraintTypeViews) constraints
    localIdentities =
      sort $
        Set.toList
          ( (freeTypeBinderIdentitiesTypeView view `Set.union` constraintVars)
              Set.\\ headVars
          )

    localVarPairs =
      [ ( Map.findWithDefault
            (typeBinderIdentityStableName identity)
            identity
            displayNamesByIdentity,
          identity
        )
      | identity <- localIdentities
      ]

    displayNamesByIdentity =
      freeTypeBinderDisplayNamesTypeView view
        `Map.union` mergeUniquePairMaps
          [ freeTypeBinderDisplayNamesTypeView constraintView
          | constraint <- constraints,
            constraintView <- NE.toList (constraintTypeViews constraint)
          ]

    canonicalDisplayNamesByIdentity =
      Map.fromList
        [ (identity, displayName)
        | (displayName, identity) <- localVarPairs
        ]

    canonicalView =
      canonicalizeTypeViewVarDisplays canonicalDisplayNamesByIdentity view

    canonicalConstraints =
      map (canonicalizeConstraintVarDisplays canonicalDisplayNamesByIdentity) constraints

    constraintBinderIdentities =
      foldMap typeViewBinderIdentities . constraintTypeViews

canonicalizeConstraintVarDisplays :: Map TypeBinderIdentity String -> ConstraintInfo -> ConstraintInfo
canonicalizeConstraintVarDisplays displayNamesByIdentity constraint =
  constraint
    { constraintTypeViews =
        fmap
          (canonicalizeTypeViewVarDisplays displayNamesByIdentity)
          (constraintTypeViews constraint)
    }

canonicalizeTypeViewVarDisplays :: Map TypeBinderIdentity String -> TypeView -> TypeView
canonicalizeTypeViewVarDisplays displayNamesByIdentity view =
  mapTypeViewDisplayBinderNames
    (\identity displayName -> Map.findWithDefault displayName identity displayNamesByIdentity)
    view

data TypeLoweringProjection
  = LowerDisplayType
  | LowerIdentityType

lowerTypeRaw :: Map String DataInfo -> SrcType -> SrcType
lowerTypeRaw = lowerTypeRawWith LowerDisplayType

lowerIdentityTypeRaw :: Map String DataInfo -> SrcType -> SrcType
lowerIdentityTypeRaw = lowerTypeRawWith LowerIdentityType

lowerTypeRawWith :: TypeLoweringProjection -> Map String DataInfo -> SrcType -> SrcType
lowerTypeRawWith projection dataTypes = lower Map.empty Nothing
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
        <|> (Map.lookup name dataTypeHeadIdentities >>= \identity -> lookupSymbolIdentityExact identity dataTypesByIdentity)

    encodeDataType subst info actualArgs =
      let actualArgs' =
            if null actualArgs
              then map STVar (dataParameterNames info)
              else actualArgs
          selfName = "$" ++ dataInfoIdentityHeadName info ++ "_self"
          resultName = "$" ++ dataInfoIdentityHeadName info ++ "_result"
          paramSubst = Map.union (dataParameterSubst info actualArgs') subst
       in STMu selfName (STForall resultName Nothing (handlerChain info paramSubst (STVar selfName) (STVar resultName)))

    -- Lowering remains a string-shaped source boundary, but the aliases that
    -- may denote a data parameter are selected by its resolved binder
    -- identity.  This keeps constructor field spellings working when the data
    -- declaration's diagnostic spelling has gone stale, without conflating a
    -- same-named constructor-local forall.
    dataParameterSubst info actualArgs =
      Map.fromList
        [ (alias, actualArg)
        | ((_, paramIdentity), actualArg) <- zip (dataParamBinders info) actualArgs
        , (alias, aliasIdentity) <- Map.toList (dataParameterAliasIdentities info)
        , aliasIdentity == paramIdentity
        ]

    dataParameterAliasIdentities info =
      typeBinderAliasIdentityMap
        ( dataParamBinders info
            ++ concatMap (typeViewBinderIdentityAliasEntries . ctorTypeView) (dataConstructors info)
        )

    handlerChain info subst selfTy resultTy =
      foldr
        STArrow
        resultTy
        [ foldr
            ( \(name, mbBound) acc ->
                STForall name (fmap (SrcBound . lowerCtorArg subst ownerIdentity selfTy) mbBound) acc
            )
            (foldr STArrow resultTy (map (lowerCtorArg subst ownerIdentity selfTy) (constructorArgs ctor)))
            (constructorForalls ctor)
          | ctor <- dataConstructors info
          , let ownerIdentity = Just (dataInfoSymbolIdentity info)
        ]

    dataParameterNames info =
      case projection of
        LowerDisplayType -> dataParams info
        LowerIdentityType -> map (typeBinderIdentityStableName . snd) (dataParamBinders info)

    constructorType ctor =
      case projection of
        LowerDisplayType -> typeViewDisplay (ctorTypeView ctor)
        LowerIdentityType -> typeViewIdentity (ctorTypeView ctor)

    constructorForalls =
      fst . splitForalls . constructorType

    constructorArgs =
      fst . splitArrows . snd . splitForalls . constructorType

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
            if checkedTypeParamIsFirstOrder param
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

lowerConstructorBinding :: IdentityGenerator -> ElaborateScope -> ConstructorInfo -> Either ProgramError (LoweredBinding, IdentityGenerator)
lowerConstructorBinding generator scope ctorInfo = do
  result <- runElaborateMWithGenerator generator (constructorSurfaceExpr scope ctorInfo)
  let surfaceExpr = elaborateResultValue result
  let sourceView = constructorBindingSourceTypeView scope ctorInfo
      expectedView = lowerTypeViewWithIdentities scope sourceView
  pure
    ( LoweredBinding
        { loweredBindingIdentity = loweredBindingIdentityFromConstructorInfo ctorInfo,
          loweredBindingSourceTypeView = sourceView,
          loweredBindingExpectedTypeView = expectedView,
          loweredBindingSurfaceExpr = surfaceExpr,
          loweredBindingResolvedLocalIdentities = [],
          loweredBindingResolvedEvidenceIdentities = [],
          loweredBindingDeferredObligations = Map.empty,
          loweredBindingExternalTypeViews = Map.empty,
          loweredBindingExportedAsMain = False
        },
      elaborateResultIdentityGenerator result
    )

constructorBindingSourceTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorBindingSourceTypeView scope ctorInfo =
  typeViewWithScopeAliases scope (quantifiedConstructorTypeView scope ctorInfo)

lowerExprBinding :: ElaborateScope -> LoweredBindingIdentity -> SrcType -> Bool -> P.Expr -> Either ProgramError LoweredBinding
lowerExprBinding scope identity expectedTy exportedAsMain expr = do
  result <- runElaborateMForBinding identity (compileExpr scope (Just expectedTy) expr)
  let sourceView = sourceTypeViewInScope scope expectedTy
  pure
    LoweredBinding
      { loweredBindingIdentity = identity,
        loweredBindingSourceTypeView = sourceView,
        loweredBindingExpectedTypeView = lowerTypeViewWithIdentities scope sourceView,
        loweredBindingSurfaceExpr = elaborateResultValue result,
        loweredBindingResolvedLocalIdentities = elaborateResultResolvedLocalIdentities result,
        loweredBindingResolvedEvidenceIdentities = [],
        loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
        loweredBindingExternalTypeViews = elaborateResultExternalTypeViews result,
        loweredBindingExportedAsMain = exportedAsMain
      }

lowerConstrainedResolvedExprBinding :: ElaborateScope -> LoweredBindingIdentity -> [ConstraintInfo] -> TypeView -> TypeView -> Bool -> P.ResolvedExpr -> Either ProgramError LoweredBinding
lowerConstrainedResolvedExprBinding scope identity constraints visibleView bodyExpectedView exportedAsMain expr =
  fst
    <$> lowerConstrainedResolvedExprBindingWithGenerator
      (identityGeneratorAfter (resolvedLoweringGeneratedIdentities identity constraints visibleView bodyExpectedView expr))
      scope
      identity
      constraints
      visibleView
      bodyExpectedView
      exportedAsMain
      expr

lowerConstrainedResolvedExprBindingWithGenerator :: IdentityGenerator -> ElaborateScope -> LoweredBindingIdentity -> [ConstraintInfo] -> TypeView -> TypeView -> Bool -> P.ResolvedExpr -> Either ProgramError (LoweredBinding, IdentityGenerator)
lowerConstrainedResolvedExprBindingWithGenerator generator scope identity constraints visibleView bodyExpectedView exportedAsMain expr = do
  result <- runElaborateMWithGeneratorForBinding generator identity $ do
    (scopeWithEvidence, evidenceParams, evidenceIdentities) <- extendConstraintEvidenceInfo scope constraints
    bodyExpr <- compileResolvedExprWithExpectedView scopeWithEvidence (Just bodyExpectedView) expr
    pure (foldr wrapEvidence bodyExpr evidenceParams, evidenceIdentities)
  let expectedView =
        typeViewMergeBinderIdentityAliases
          (resolvedExprTypeBinderIdentities expr)
          ( lowerTypeViewWithIdentities scope $
              constrainedRuntimeTypeInfoView scope constraints visibleView
          )
      (surfaceExpr, evidenceIdentities) = elaborateResultValue result
      lowered =
        LoweredBinding
          { loweredBindingIdentity = identity,
            loweredBindingSourceTypeView = visibleView,
            loweredBindingExpectedTypeView = expectedView,
            loweredBindingSurfaceExpr = surfaceExpr,
            loweredBindingResolvedLocalIdentities = elaborateResultResolvedLocalIdentities result,
            loweredBindingResolvedEvidenceIdentities = evidenceIdentities,
            loweredBindingDeferredObligations = elaborateResultDeferredObligations result,
            loweredBindingExternalTypeViews = elaborateResultExternalTypeViews result,
            loweredBindingExportedAsMain = exportedAsMain
          }
  pure (lowered, elaborateResultIdentityGenerator result)
  where
    wrapEvidence methodEvidence acc =
      S.EResolvedLamAnn
        (X.resolvedVarDetails resolved)
        (X.resolvedVarRuntimeName resolved)
        (loweredTypeViewIdentity scope (evidenceMethodTypeView methodEvidence))
        acc
      where
        resolved = evidenceMethodResolvedVar methodEvidence

resolvedLoweringGeneratedIdentities :: LoweredBindingIdentity -> [ConstraintInfo] -> TypeView -> TypeView -> P.ResolvedExpr -> [UniqueIdentity]
resolvedLoweringGeneratedIdentities identity constraints visibleView bodyExpectedView expr =
  loweredBindingIdentityGeneratedIdentities identity
    ++ concatMap constraintInfoGeneratedIdentities constraints
    ++ typeViewGeneratedIdentities visibleView
    ++ typeViewGeneratedIdentities bodyExpectedView
    ++ resolvedExprGeneratedIdentities expr

-- | Retain every type-binder identity already assigned by source resolution.
--
-- The lowered surface syntax carries the stable identity spelling, but the
-- constraint graph must receive the semantic identity itself.  Attaching this
-- root-local context to the expected 'TypeView' keeps local annotations such as
-- @let id : forall a. a -> a = ...@ identity-bearing across the
-- resolved-program -> surface-pipeline boundary.  Equal display names in
-- nested annotations are deliberately merged with
-- 'mergeTypeBinderIdentityMaps': ambiguous spellings disappear while each
-- stable identity alias remains authoritative.
resolvedExprTypeBinderIdentities :: P.ResolvedExpr -> Map String TypeBinderIdentity
resolvedExprTypeBinderIdentities =
  mergeTypeBinderIdentityMaps . goExpr
  where
    typeIdentities = typeViewBinderIdentities . typeViewFromResolved

    goExpr resolvedExpr =
      case resolvedExpr of
        EVar {} -> []
        ELit {} -> []
        ELam param body ->
          maybe [] (pure . typeIdentities) (P.paramType param)
            ++ goExpr body
        EApp fun arg -> goExpr fun ++ goExpr arg
        ELet _ mbTy rhs body ->
          maybe [] (pure . typeIdentities) mbTy
            ++ goExpr rhs
            ++ goExpr body
        EAnn inner ty -> typeIdentities ty : goExpr inner
        ECase scrutinee alts ->
          goExpr scrutinee
            ++ concatMap goAlt alts

    goAlt alt =
      goPattern (P.altPattern alt) ++ goExpr (P.altExpr alt)

    goPattern pattern0 =
      case pattern0 of
        P.PatCtor _ args -> concatMap goPattern args
        P.PatVar {} -> []
        P.PatWildcard -> []
        P.PatAnn inner ty -> typeIdentities ty : goPattern inner

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

lowerResolvedConstrainedExprBindingWithGenerator :: IdentityGenerator -> ElaborateScope -> LoweredBindingIdentity -> P.ResolvedConstrainedType -> Bool -> P.ResolvedExpr -> Either ProgramError (LoweredBinding, IdentityGenerator)
lowerResolvedConstrainedExprBindingWithGenerator generator scope identity ty exportedAsMain expr = do
  constraints <- mapM (resolvedConstraintInfoForScope scope) (P.constrainedConstraints ty)
  bodyView <- resolvedTypeViewForScope scope (P.constrainedBody ty)
  let visibleView = constrainedVisibleTypeView constraints bodyView
  lowerConstrainedResolvedExprBindingWithGenerator
    generator
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
  go []
  where
    go seen constraint =
      case classInfoForConstraint scope constraint of
        Nothing -> []
        Just classInfo ->
          let key = classConstraintEvidenceKeyInfo classInfo constraint
           in if key `elem` seen
                then []
                else
                  let seen' = key : seen
                      superclasses =
                        map
                          (applyConstraintInfoSubst (superclassSubst classInfo constraint))
                          (classSuperclassInfos classInfo)
                   in (classInfo, constraint) : concatMap (go seen') superclasses

    superclassSubst classInfo constraint =
      typeViewSubstFromParamIdentities
        (classParamBinderIdentities classInfo)
        (constraintTypeViews constraint)

classConstraintEvidenceKeyInfo :: ClassInfo -> ConstraintInfo -> ClassApplicationKey
classConstraintEvidenceKeyInfo classInfo constraint =
  classApplicationKey
    (classInfoSymbolIdentity classInfo)
    (constraintTypeViews constraint)

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
  let view = typeViewFromResolved ty
  case typeViewWithDisplay display view of
    Right displayedView -> pure displayedView
    Left err -> Left (ProgramPipelineError ("resolved type display shape mismatch: " ++ show err))

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
  case lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) namesByIdentity of
    Just names ->
      case filter (== P.refDisplayName symbol) names of
        name : _ -> Just name
        [] -> preferredDisplayName (resolvedSymbolIdentity symbol) names
    Nothing -> Nothing

preferredDisplayName :: SymbolIdentity -> [String] -> Maybe String
preferredDisplayName identity names =
  case Set.toList (Set.fromList (filter (/= stableName) names)) of
    [name] -> Just name
    [] ->
      case names of
        [] -> Nothing
        _ -> Just stableName
    _ -> Just stableName
  where
    stableName =
      symbolIdentityStableName identity

isBuiltinTypeSymbol :: ResolvedSymbol -> Bool
isBuiltinTypeSymbol = Builtins.isBuiltinTypeSymbol

constructorSurfaceExpr :: ElaborateScope -> ConstructorInfo -> ElaborateM ResolvedSurfaceExpr
constructorSurfaceExpr scope ctorInfo = do
  rawExpr <- constructorSurfaceExprRaw scope ctorInfo
  pure (S.EAnn rawExpr (constructorBindingExpectedType scope ctorInfo))

constructorBindingExpectedType :: ElaborateScope -> ConstructorInfo -> SrcType
constructorBindingExpectedType scope ctorInfo =
  let ctorView = quantifiedConstructorTypeView scope ctorInfo
   in if constructorBindingUsesStructuralPlaceholder scope ctorInfo
        then constructorStructuralPlaceholderType scope ctorInfo
        else loweredTypeViewIdentity scope ctorView

constructorBindingUsesStructuralPlaceholder :: ElaborateScope -> ConstructorInfo -> Bool
constructorBindingUsesStructuralPlaceholder scope ctorInfo =
  constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
    && srcTypeHasVariableHeadApplication (lowerType scope (quantifyFreeTypeVars (ctorType ctorInfo)))

quantifiedConstructorTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
quantifiedConstructorTypeView scope ctorInfo =
  quantifyFreeTypeView view
  where
    view =
      constructorTypeView scope ctorInfo

constructorSurfaceExprRaw :: ElaborateScope -> ConstructorInfo -> ElaborateM ResolvedSurfaceExpr
constructorSurfaceExprRaw scope ctorInfo = do
  let ctorIdentityName = constructorInfoIdentityName ctorInfo
      argNames = ["$" ++ ctorIdentityName ++ "_arg" ++ show ix | ix <- [1 .. length (ctorArgs ctorInfo)]]
      handlerNames = ["$" ++ ctorIdentityName ++ "_k" ++ show ix | ix <- [1 .. length handlerCtorOrder]]
      resultVar =
        if any (not . null . ctorForalls) handlerCtorOrder || constructorOwnerHasParams
          then constructorOwnerResultVar ctorInfo
          else "a"
      resultIdentity =
        typeBinderIdentityFromStructural
          (symbolUniqueIdentity (ctorOwningTypeIdentity ctorInfo))
          StructuralResultBinder
      resultView =
        requireTypeViewFromSourceType
          Map.empty
          (typeBinderAliasIdentityMap [(resultVar, resultIdentity)])
          (STVar resultVar)
      useStructuralTypes = constructorBindingUsesStructuralPlaceholder scope ctorInfo
      argTypes =
        if useStructuralTypes
          then map (lowerType scope) (constructorStructuralArgs ctorInfo)
          else map (loweredTypeViewIdentity scope) (constructorArgTypeViews scope ctorInfo)
      handlerTypes =
        if useStructuralTypes
          then map (constructorStructuralHandlerType resultVar . constructorShapeFromInfo) handlerCtorOrder
          else
            map
              (typeViewIdentity . (\ctor -> handlerTypeViewFromViews scope ctor resultView))
              handlerCtorOrder
  argRefs <- mapM freshElaborateLocalRef argNames
  handlerRefs <- mapM freshElaborateLocalRef handlerNames
  let handlerMetadata = zip4 handlerCtorOrder handlerNames handlerRefs handlerTypes
  (_, selectedHandlerName, selectedHandlerRef, _) <-
    case
      find
        (\(handlerCtor, _, _, _) -> sameSymbolIdentity (ctorInfoSymbol handlerCtor) (ctorInfoSymbol ctorInfo))
        handlerMetadata
    of
      Just selected -> pure selected
      Nothing ->
        throwError
          ( ProgramPipelineError
              ( "constructor handler metadata missing identity `"
                  ++ symbolIdentityStableName (ctorInfoSymbol ctorInfo)
                  ++ "`"
              )
          )
  let selectedHandler =
        S.EAnn
          ( foldl
              S.EApp
              (resolvedLocalVar selectedHandlerName selectedHandlerRef)
              (zipWith resolvedLocalVar argNames argRefs)
          )
          (typeViewIdentity resultView)
      body =
        foldr
          (\(handlerName, handlerRef, handlerTy) acc -> resolvedLocalLamSurface handlerName handlerRef (Just handlerTy) acc)
          selectedHandler
          [(handlerName, handlerRef, handlerTy) | (_, handlerName, handlerRef, handlerTy) <- handlerMetadata]
      lifted =
        foldr
          (\(argName, argRef, argTy) acc -> resolvedLocalLamSurface argName argRef (Just argTy) acc)
          body
          (zip3 argNames argRefs argTypes)
  pure lifted
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
        Just subst -> specializeConstructorInfo scope subst ctor
        Nothing -> ctor

    resolvedLocalVar runtimeName ref =
      S.EResolvedVar (resolvedLocalBinderDetails runtimeName ref) runtimeName

compileExpr :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM ResolvedSurfaceExpr
compileExpr scope mbExpected expr = case expr of
  EVar name ->
    case Map.lookup name (esValues scope) of
      Just OverloadedMethod {valueMethodInfo = methodInfo} ->
        compileNullaryMethodUse scope mbExpected methodInfo
      Just valueInfo@OrdinaryValue {} -> do
        evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected []
        valueSurface <- ordinaryValueSurface scope valueInfo
        let applied = foldl S.EApp valueSurface evidenceSurfaces
        pure $
          if null evidenceSurfaces
            then annotateExpectedBareValueUse scope mbExpected valueInfo applied
            else applied
      Just ConstructorValue {valueCtorInfo = ctorInfo} -> do
        compileConstructorHead
          scope
          ctorInfo
          0
          (constructorExpectedTypeView scope ctorInfo <$> mbExpected)
          (constructorInitialSubst scope ctorInfo 0 mbExpected)
      Nothing -> throwError (ProgramUnknownValue name)
  ELit lit -> pure (S.ELit lit)
  ELam param body -> do
    runtimeName <- freshRuntimeName (P.paramName param)
    paramRef <- freshElaborateLocalRef (P.paramName param)
    let paramTy = case (P.paramType param, mbExpected) of
          (Just ty, _) -> Just ty
          (Nothing, Just (STArrow dom _)) -> Just dom
          _ -> Nothing
    scope' <- extendLocalWithRef scope paramRef (P.paramName param) runtimeName paramTy
    bodyExpr0 <- compileExpr scope' (expectedCodomain mbExpected) body
    let bodyExpr =
          case expectedCodomain mbExpected of
            Just codTy | isRecursiveResultType codTy -> S.EAnn bodyExpr0 (lowerType scope codTy)
            _ -> bodyExpr0
    pure $
      resolvedLocalLamSurface runtimeName paramRef (lowerType scope <$> paramTy) bodyExpr
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
                    Just ty -> S.EAnn rhsExpr (lowerType scope ty)
                    Nothing ->
                      case inferKnownExprType selfScope rhs of
                        Just ty -> S.EAnn rhsExpr (lowerType scope ty)
                        Nothing -> rhsExpr
            bodyScope <- extendLocalLoweredWithRef scope localRef name runtimeName bindingTy
            bodyExpr <- compileExpr bodyScope mbExpected body
            pure (resolvedLocalLetSurface runtimeName localRef rhsExpr' bodyExpr)
  EAnn inner annTy ->
    case inner of
      EVar name
        | Just OverloadedMethod {valueMethodInfo = methodInfo} <- Map.lookup name (esValues scope),
          methodFullArity methodInfo == 0 ->
            compileExpr scope (Just annTy) inner
      _ -> do
        innerExpr <- compileExpr scope (Just annTy) inner
        pure (S.EAnn innerExpr (lowerType scope annTy))
  ECase scrutinee alts -> compileCase scope mbExpected scrutinee alts

compileResolvedExprWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedExpr -> ElaborateM ResolvedSurfaceExpr
compileResolvedExprWithExpectedView scope mbExpectedView expr = do
  case (mbExpectedView, expectedAwareKnownView) of
    (Just expectedView, Just actualView)
      | hasLeadingTypeViewForall expectedView,
        not (hasLeadingTypeViewForall actualView) -> pure ()
      | otherwise ->
          ensureTypeViewCompatible scope expectedView actualView
    _ -> pure ()
  case expr of
    EVar ref -> do
      valueInfo <- lookupResolvedValueInfo scope ref
      case valueInfo of
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          compileNullaryMethodUseWithView scope mbExpectedView methodInfo
        ordinary@OrdinaryValue {} -> do
          evidenceSurfaces <- valueResolvedEvidenceArgsWithExpectedView scope ordinary mbExpectedView []
          headSurface0 <- resolvedValueSurface scope ref ordinary
          let headSurface =
                annotateConstrainedValueHeadUse
                  scope
                  ordinary
                  mbExpectedView
                  []
                  (length evidenceSurfaces)
                  headSurface0
          let applied = foldl S.EApp headSurface evidenceSurfaces
              bareExpr =
                if null evidenceSurfaces
                  then annotateExpectedBareValueUse scope mbExpected ordinary applied
                  else applied
          pure bareExpr
        ConstructorValue {valueCtorInfo = ctorInfo} ->
          compileConstructorHead
            scope
            ctorInfo
            0
            mbExpectedView
            (constructorInitialViewSubst scope ctorInfo 0 mbExpectedView)
    ELit lit ->
      pure (S.ELit lit)
    EApp _ _ ->
      compileResolvedAppWithExpectedView scope mbExpectedView expr
    ECase scrutinee alts ->
      compileResolvedCaseWithExpectedView scope mbExpectedView scrutinee alts
    ELam param body -> do
      let paramRef = P.paramName param
          paramSourceName = localRefName paramRef
      runtimeName <- freshRuntimeName paramSourceName
      paramAnn <- traverse (liftEitherElab . resolvedTypeViewForScope scope) (P.paramType param)
      let paramView = case (paramAnn, mbExpectedView) of
            (Just view, _) -> Just view
            (Nothing, Just expectedView) -> monomorphicExpectedDomainTypeView expectedView
            _ -> Nothing
      scope' <- extendResolvedLocalView scope paramRef runtimeName paramView
      bodyExpr0 <- compileResolvedExprWithExpectedView scope' (mbExpectedView >>= expectedCodomainTypeView) body
      let bodyExpr =
            case mbExpectedView >>= expectedCodomainTypeView of
              Just codView
                | isRecursiveResultType (typeViewDisplay codView) ->
                    S.EAnn bodyExpr0 (loweredTypeViewIdentity scope codView)
              _ -> bodyExpr0
      pure $
        resolvedLocalLamSurface
          runtimeName
          paramRef
          (loweredTypeViewIdentity scope <$> paramView)
          bodyExpr
    ELet localRef mbTy rhs body -> do
      mbTypeView <- traverse (liftEitherElab . resolvedTypeViewForScope scope) mbTy
      if localRef `notElem` collectFreeResolvedValues Set.empty body && mbTypeView == Nothing
        then compileResolvedExprWithExpectedView scope mbExpectedView body
        else do
          let name = localRefName localRef
              recursive = mentionsFreeResolvedValue localRef rhs
          case (recursive, mbTypeView, inlineImmediateResolvedLetUse localRef rhs body) of
            (False, Nothing, Just inlined) ->
              compileResolvedExprWithExpectedView scope mbExpectedView inlined
            _ -> do
              runtimeName <- freshRuntimeName name
              provisionalView <-
                case (recursive, mbTypeView) of
                  (True, Nothing) -> Just <$> freshTypeVarView
                  _ -> pure mbTypeView
              selfScope <-
                if recursive
                  then extendResolvedLocalView scope localRef runtimeName provisionalView
                  else pure scope
              rhsExpr <- compileResolvedExprWithExpectedView selfScope provisionalView rhs
              let mbKnownRhsView = inferKnownResolvedExprTypeView selfScope rhs
              bindingView <-
                case mbTypeView <|> mbKnownRhsView <|> provisionalView of
                  Just view -> pure view
                  Nothing -> freshTypeVarView
              let rhsExpr' =
                    case (mbTypeView, mbKnownRhsView) of
                      (Just view, _) ->
                        resolvedLocalBindingSchemeAnnotation scope view rhsExpr
                      (Nothing, Just view) ->
                        S.EAnn rhsExpr (loweredTypeViewIdentity scope view)
                      (Nothing, Nothing) -> rhsExpr
              bodyScope <- extendResolvedLocalView scope localRef runtimeName (Just bindingView)
              bodyExpr <- compileResolvedExprWithExpectedView bodyScope mbExpectedView body
              pure (resolvedLocalLetSurface runtimeName localRef rhsExpr' bodyExpr)
    EAnn inner annTy -> do
      annView <- liftEitherElab (resolvedTypeViewForScope scope annTy)
      case inner of
        EVar ref
          | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
            methodFullArity methodInfo == 0 ->
              compileResolvedExprWithExpectedView scope (Just annView) inner
        _ -> do
          innerExpr <- compileResolvedExprWithExpectedView scope (Just annView) inner
          pure (S.EAnn innerExpr (loweredTypeViewIdentity scope annView))
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

    expectedAwareKnownView =
      case mbExpectedView of
        Just expectedView -> inferKnownResolvedExprTypeViewWithExpected scope expectedView expr
        Nothing -> inferKnownResolvedExprTypeView scope expr

hasLeadingTypeViewForall :: TypeView -> Bool
hasLeadingTypeViewForall =
  not . null . typeViewForallBinderViews

-- A binding's leading producer foralls are introduced outside its term lambda.
-- Project through that spine so a monomorphic unannotated parameter receives
-- the already-resolved binder identity carried by the expected arrow domain.
-- A polymorphic domain remains annotation-only, as required for MLF lambdas.
expectedDomainTypeView :: TypeView -> Maybe TypeView
expectedDomainTypeView view =
  case typeViewArrowArgViews view of
    domain : _ -> Just domain
    [] -> Nothing

expectedCodomainTypeView :: TypeView -> Maybe TypeView
expectedCodomainTypeView view =
  case typeViewArrowArgViews view of
    _ : _ -> Just (typeViewArrowResultViewForArity view 1)
    [] -> Nothing

monomorphicExpectedDomainTypeView :: TypeView -> Maybe TypeView
monomorphicExpectedDomainTypeView expectedView = do
  domain <- expectedDomainTypeView expectedView
  if typeViewContainsForall domain
    then Nothing
    else Just domain

typeViewContainsForall :: TypeView -> Bool
typeViewContainsForall view =
  case typeViewNodeView view of
    TypeViewVarNode {} -> False
    TypeViewArrowNode domain codomain ->
      typeViewContainsForall domain || typeViewContainsForall codomain
    TypeViewBaseNode {} -> False
    TypeViewConNode _ _ args -> any typeViewContainsForall args
    TypeViewVarAppNode _ _ args -> any typeViewContainsForall args
    TypeViewTyLamNode _ _ body -> typeViewContainsForall body
    TypeViewTyAppNode fun arg ->
      typeViewContainsForall fun || typeViewContainsForall arg
    TypeViewForallNode {} -> True
    TypeViewMuNode _ _ body -> typeViewContainsForall body
    TypeViewBottomNode -> False

compileApp :: ElaborateScope -> Maybe SrcType -> P.Expr -> ElaborateM ResolvedSurfaceExpr
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
          pure (S.EApp headSurface argSurface)
    (headExpr, args) -> do
      headSurface <- compileExpr scope Nothing headExpr
      argSurfaces <- mapM (compileExpr scope Nothing) args
      pure (foldl S.EApp headSurface argSurfaces)

compileResolvedAppWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedExpr -> ElaborateM ResolvedSurfaceExpr
compileResolvedAppWithExpectedView scope mbExpectedView expr =
  case collectResolvedApps expr of
    (EVar ref, args) -> do
      valueInfo <- lookupResolvedValueInfo scope ref
      case valueInfo of
        OverloadedMethod {valueMethodInfo = methodInfo} ->
          compileResolvedMethodAppWithExpectedView scope mbExpectedView methodInfo args
        _ ->
          compileResolvedValueAppWithExpectedView scope mbExpectedView ref valueInfo args
    (headExpr, args@(_ : _))
      | Just expectedView <- mbExpectedView -> do
          argViews <-
            mapM
              (\arg -> maybe freshTypeVarView pure (inferKnownResolvedExprTypeView scope arg))
              args
          argSurfaces <-
            zipWithM
              (\argView -> compileResolvedExprWithExpectedView scope (Just argView))
              argViews
              args
          let expectedHeadView = foldr typeViewArrow expectedView argViews
          headSurface <- compileResolvedExprWithExpectedView scope (Just expectedHeadView) headExpr
          pure (foldl S.EApp headSurface argSurfaces)
    (headExpr, args) -> do
      headSurface <- compileResolvedExprWithExpectedView scope Nothing headExpr
      argSurfaces <- mapM (compileResolvedExprWithExpectedView scope Nothing) args
      pure (foldl S.EApp headSurface argSurfaces)

explicitExprAnnotation :: P.Expr -> Maybe SrcType
explicitExprAnnotation expr =
  case expr of
    EAnn _ ty -> Just ty
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
  lookupSymbolIdentityExact (resolvedSymbolIdentity symbol) (esValuesByIdentity scope)

resolvedValueSurface :: ElaborateScope -> P.ResolvedValueRef -> ValueInfo -> ElaborateM ResolvedSurfaceExpr
resolvedValueSurface scope ref valueInfo = do
  name <- resolvedValueSurfaceName scope ref valueInfo
  details <- resolvedValueSurfaceDetails ref valueInfo name
  pure (S.EResolvedVar details name)

resolvedValueSurfaceDetails :: P.ResolvedValueRef -> ValueInfo -> String -> ElaborateM IdDetails
resolvedValueSurfaceDetails ref valueInfo runtimeName =
  case ref of
    P.ResolvedLocalValue localRef ->
      pure (resolvedLocalBinderDetails runtimeName localRef)
    P.ResolvedGlobalValue {} ->
      pure (valueInfoRuntimeDetails valueInfo)

resolvedValueSurfaceName :: ElaborateScope -> P.ResolvedValueRef -> ValueInfo -> ElaborateM String
resolvedValueSurfaceName scope ref valueInfo =
  case (ref, valueInfo) of
    (P.ResolvedGlobalValue {}, _) -> globalValueSurfaceName scope valueInfo
    (P.ResolvedLocalValue {}, ordinary@OrdinaryValue {}) -> pure (valueInfoRuntimeName ordinary)
    (P.ResolvedLocalValue localRef, _) -> pure (localRefName localRef)

globalValueSurfaceName :: ElaborateScope -> ValueInfo -> ElaborateM String
globalValueSurfaceName scope valueInfo =
  case valueInfo of
    ordinary@OrdinaryValue {} -> do
      let stableName = symbolIdentityStableName (valueInfoSymbolIdentity ordinary)
      recordExternalTypeView stableName (resolvedOrdinaryValueExternalTypeView scope ordinary)
      pure stableName
    _ ->
      pure (symbolIdentityStableName (valueInfoSymbolIdentity valueInfo))

ordinaryValueSurfaceName :: ElaborateScope -> ValueInfo -> ElaborateM String
ordinaryValueSurfaceName scope ordinary@OrdinaryValue {}
  | symbolDefiningModule (valueInfoSymbolIdentity ordinary) == "<local>" = pure (valueInfoRuntimeName ordinary)
  | otherwise = globalValueSurfaceName scope ordinary
ordinaryValueSurfaceName _ valueInfo =
  pure (symbolIdentityStableName (valueInfoSymbolIdentity valueInfo))

ordinaryValueSurface :: ElaborateScope -> ValueInfo -> ElaborateM ResolvedSurfaceExpr
ordinaryValueSurface scope valueInfo = do
  runtimeName <- ordinaryValueSurfaceName scope valueInfo
  details <- valueInfoSurfaceDetails scope runtimeName valueInfo
  pure (S.EResolvedVar details runtimeName)

valueInfoSurfaceDetails :: ElaborateScope -> String -> ValueInfo -> ElaborateM IdDetails
valueInfoSurfaceDetails scope runtimeName valueInfo
  | isLocalOrdinaryValue valueInfo =
      case lookupLocalValueRefByIdentity scope (valueInfoSymbolIdentity valueInfo) of
        Just ref -> pure (resolvedLocalBinderDetails runtimeName ref)
        Nothing ->
          throwError
            (ProgramPipelineError ("local value has no lexical identity: " ++ runtimeName))
  | otherwise =
      pure (valueInfoRuntimeDetails valueInfo)

lookupLocalValueRefByIdentity :: ElaborateScope -> SymbolIdentity -> Maybe LocalRef
lookupLocalValueRefByIdentity scope identity =
  fst
    <$> find
      (sameSymbolIdentity identity . valueInfoSymbolIdentity . snd)
      (Map.toList (esLocalValues scope))

resolvedOrdinaryValueExternalTypeView :: ElaborateScope -> ValueInfo -> TypeView
resolvedOrdinaryValueExternalTypeView scope valueInfo@OrdinaryValue {valueConstraintInfos = constraints} =
  constrainedRuntimeTypeInfoView scope constraints (ordinaryValueTypeView valueInfo)
resolvedOrdinaryValueExternalTypeView _ _ =
  typeViewBottom

recordExternalTypeView :: String -> TypeView -> ElaborateM ()
recordExternalTypeView name view =
  modify
    ( \state ->
        state
          { elaborateExternalTypeViews =
              Map.insert name view (elaborateExternalTypeViews state)
          }
    )

compileValueApp :: ElaborateScope -> Maybe SrcType -> ValueInfo -> [P.Expr] -> ElaborateM ResolvedSurfaceExpr
compileValueApp scope mbExpected ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgTys) = constructorArgPlan scope ctorInfo mbExpected args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgTys args
  constructorHead <-
    compileConstructorHead
      scope
      ctorInfo
      (length args)
      (sourceTypeViewInScope scope <$> mbExpected)
      constructorSubst
  pure (foldl S.EApp constructorHead argSurfaces)
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
              then S.EAnn argSurface (lowerType scope expectedTy)
              else argSurface

compileValueApp scope mbExpected valueInfo args = do
  let expectedArgTys = valueExpectedArgTypes scope valueInfo mbExpected args
  argSurfaces <- zipWithM compileValueArg (expectedArgTys ++ repeat Nothing) args
  evidenceSurfaces <- valueEvidenceArgs scope valueInfo mbExpected args
  headSurface <-
    case valueInfo of
      ordinary@OrdinaryValue {} -> ordinaryValueSurface scope ordinary
      OverloadedMethod {} -> error "compileValueApp does not handle overloaded methods"
  let
      headWithAnnotation =
        annotateConstrainedValueHeadUse
          scope
          valueInfo
          (sourceTypeViewInScope scope <$> mbExpected)
          (map (fmap (sourceTypeViewInScope scope) . inferKnownExprType scope) args)
          (length evidenceSurfaces)
          headSurface
      applied = foldl S.EApp (foldl S.EApp headWithAnnotation evidenceSurfaces) argSurfaces
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

compileResolvedValueAppWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedValueRef -> ValueInfo -> [P.ResolvedExpr] -> ElaborateM ResolvedSurfaceExpr
compileResolvedValueAppWithExpectedView scope mbExpectedView _ ConstructorValue {valueCtorInfo = ctorInfo} args = do
  let (constructorSubst, expectedArgViews) = constructorResolvedArgPlan scope ctorInfo mbExpectedView args
  argSurfaces <-
    zipWithM compileConstructorArg expectedArgViews args
  constructorHead <-
    compileConstructorHead
      scope
      ctorInfo
      (length args)
      mbExpectedView
      constructorSubst
  pure (foldl S.EApp constructorHead argSurfaces)
  where
    compileConstructorArg expectedView arg = do
      case inferKnownResolvedExprTypeViewWithExpected scope expectedView arg of
        Just knownView -> do
          ensureTypeViewCompatible scope expectedView knownView
          compileResolvedExprWithExpectedView scope (Just expectedView) arg
        Nothing -> do
          argSurface <- compileResolvedExprWithExpectedView scope (Just expectedView) arg
          pure $
            if hasLeadingTypeViewForall expectedView
              then S.EAnn argSurface (loweredTypeViewIdentity scope expectedView)
              else argSurface

compileResolvedValueAppWithExpectedView scope mbExpectedView ref valueInfo args = do
  let expectedArgViews =
        map
          (fmap (typeViewWithScopeAliases scope))
          (valueExpectedArgViews scope valueInfo mbExpectedView args)
  argSurfaces <- zipWithM compileValueArg (expectedArgViews ++ repeat Nothing) args
  evidenceSurfaces <- valueResolvedEvidenceArgsWithExpectedView scope valueInfo mbExpectedView args
  headSurface <-
    case valueInfo of
      OrdinaryValue {} -> resolvedValueSurface scope ref valueInfo
      OverloadedMethod {} -> error "compileResolvedValueApp does not handle overloaded methods"
  let
      headWithAnnotation =
        annotateConstrainedValueHeadUse
          scope
          valueInfo
          mbExpectedView
          (map (inferKnownResolvedExprTypeView scope) args)
          (length evidenceSurfaces)
          headSurface
      headWithEvidence = foldl S.EApp headWithAnnotation evidenceSurfaces
      applied = foldl S.EApp headWithEvidence argSurfaces
  pure (annotateExpectedValueUse scope mbExpected valueInfo applied)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

    compileValueArg (Just expectedView) arg
      | isPartialOverloadedResolvedMethodApp scope arg =
          compileKnownExpectedArg expectedView arg
    compileValueArg (Just expectedView) arg =
      compileKnownExpectedArg expectedView arg
    compileValueArg _ arg =
      compileResolvedExprWithExpectedView scope Nothing arg

    compileKnownExpectedArg expectedView arg = do
      case inferKnownResolvedExprTypeView scope arg of
        Just actualView -> ensureTypeViewCompatible scope expectedView actualView
        Nothing -> pure ()
      compileResolvedExprWithExpectedView scope (Just expectedView) arg

compileConstructorHead :: ElaborateScope -> ConstructorInfo -> Int -> Maybe TypeView -> TypeViewSubst -> ElaborateM ResolvedSurfaceExpr
compileConstructorHead scope ctorInfo argCount mbExpectedView constructorSubst = do
  deferConstructorCall scope ctorInfo argCount mbExpectedView constructorSubst

specializeConstructorInfo :: ElaborateScope -> Map String SrcType -> ConstructorInfo -> ConstructorInfo
specializeConstructorInfo scope subst ctorInfo =
  ctorInfo
    { ctorTypeView = specialized0
    }
  where
    view = constructorTypeView scope ctorInfo
    viewSubst =
      Map.fromList
        [ (key, sourceTypeViewInScope scope replacement)
        | (name, replacement) <- Map.toList subst,
          Just key <- [typeViewSubstKeyFor view name]
        ]
    specialized0 = specializeQuantifiedTypeView viewSubst view

ordinaryValueTypeInScope :: ElaborateScope -> ValueInfo -> SrcType
ordinaryValueTypeInScope scope valueInfo@OrdinaryValue {} =
  visibleTypeForTypeView (esTypes scope) view
  where
    view = ordinaryValueTypeView valueInfo
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
      preferVisibleTypeView scope (ordinaryValueTypeView valueInfo)
    resultViewForArity =
      valueResultTypeViewForArity valueView (length args)
    subst =
      case mbExpectedView >>= matchTypeViewAgainstIdentity scope Map.empty resultViewForArity of
        Just matched -> matched
        Nothing -> Map.empty
    argViews =
      map (applyTypeViewSubst subst) (methodParamTypeViews valueView)

    concreteExpectedView view
      | Set.null (freeTypeBinderIdentitiesTypeView view) = Just view
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

constructorResolvedArgPlan :: ElaborateScope -> ConstructorInfo -> Maybe TypeView -> [P.ResolvedExpr] -> (TypeViewSubst, [TypeView])
constructorResolvedArgPlan scope ctorInfo mbExpectedView args =
  let (subst, argViews) = foldl step (initialSubst, []) (zip (constructorArgTypeViews scope ctorInfo) args)
   in (subst, reverse argViews)
  where
    initialSubst =
      constructorInitialViewSubst scope ctorInfo (length args) mbExpectedView

    step (subst, acc) (templateView, arg) =
      let subst' =
            case inferKnownResolvedExprTypeView scope arg >>= matchTypeViewAgainstIdentity scope subst templateView of
              Just matched -> matched
              Nothing -> subst
          expectedView = applyTypeViewSubst subst' templateView
       in (subst', expectedView : acc)

constructorInitialSubst :: ElaborateScope -> ConstructorInfo -> Int -> Maybe SrcType -> TypeViewSubst
constructorInitialSubst scope ctorInfo argCount mbExpected =
  constructorInitialViewSubst scope ctorInfo argCount (expectedView <$> mbExpected)
  where
    constructorView = quantifiedConstructorTypeView scope ctorInfo
    expectedView =
      requireTypeViewFromSourceTypeInScope
        scope
        (typeViewHeadIdentities constructorView)
        (typeViewBinderIdentities constructorView)

constructorInitialViewSubst :: ElaborateScope -> ConstructorInfo -> Int -> Maybe TypeView -> TypeViewSubst
constructorInitialViewSubst scope ctorInfo argCount mbExpected =
  case identityMatch of
    Just subst -> subst
    Nothing -> Map.empty
  where
    templateView = constructorOccurrenceTypeView scope ctorInfo argCount
    identityMatch =
      mbExpected >>= matchTypeViewAgainstIdentity scope Map.empty templateView

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

valueEvidenceArgs :: ElaborateScope -> ValueInfo -> Maybe SrcType -> [P.Expr] -> ElaborateM [ResolvedSurfaceExpr]
valueEvidenceArgs scope valueInfo@OrdinaryValue {valueConstraintInfos = constraints} mbExpected args
  | null constraints = pure []
  | otherwise = do
      let valueView = ordinaryValueTypeView valueInfo
      subst <-
        case inferCallSubst scope valueView args of
          Just subst0 ->
            pure (refineValueEvidenceViewSubst scope valueInfo (sourceTypeViewInScope scope <$> mbExpected) args subst0)
          Nothing ->
            case constraints of
              constraint : _ -> throwError (noMatchingDisplayConstraintError (displayConstraint constraint))
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      if any usesLocalPolymorphicEvidence specializedConstraints
        then throwError (ProgramAmbiguousConstrainedValueUse (valueInfoIdentityName valueInfo))
        else concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      not (Set.null (freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint)))
        && constraintCoveredByEvidenceInfo scope constraint
valueEvidenceArgs _ _ _ _ = pure []

valueResolvedEvidenceArgsWithExpectedView :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [P.ResolvedExpr] -> ElaborateM [ResolvedSurfaceExpr]
valueResolvedEvidenceArgsWithExpectedView scope valueInfo@OrdinaryValue {valueConstraintInfos = constraints} mbExpectedView args
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
            case constraints of
              constraint : _ -> throwError (noMatchingDisplayConstraintError (displayConstraint constraint))
              [] -> pure Map.empty
      let specializedConstraints = map (applyConstraintInfoSubst subst) constraints
      let hasLocalPolymorphicEvidence = any usesLocalPolymorphicEvidence specializedConstraints
      if hasLocalPolymorphicEvidence
        then throwError (ProgramAmbiguousConstrainedValueUse (valueInfoIdentityName valueInfo))
        else concat <$> mapM (constraintResolvedEvidenceArgExprsInfo scope) specializedConstraints
  where
    usesLocalPolymorphicEvidence constraint =
      constraintInfoHasFreeTypeBinderIdentities constraint
        && constraintCoveredByEvidenceInfo scope constraint
valueResolvedEvidenceArgsWithExpectedView _ _ _ _ = pure []

annotateConstrainedValueHeadUse :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [Maybe TypeView] -> Int -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
annotateConstrainedValueHeadUse scope valueInfo mbExpectedView argViews evidenceCount headSurface =
  case constrainedValueHeadAnnotationView scope valueInfo mbExpectedView argViews evidenceCount of
    Just view -> S.EAnn headSurface (loweredTypeViewIdentity scope view)
    Nothing -> headSurface

constrainedValueHeadAnnotationView :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [Maybe TypeView] -> Int -> Maybe TypeView
constrainedValueHeadAnnotationView scope valueInfo@OrdinaryValue {valueConstraintInfos = constraints} mbExpectedView argViews evidenceCount
  | null constraints = Nothing
  | evidenceCount /= 1 = Nothing
  | length evidenceParamViews /= evidenceCount = Nothing
  | length evidenceActualViews /= evidenceCount = Nothing
  | otherwise = do
      subst <-
        foldM
          ( \acc (templateView, actualView) ->
              matchTypeViewAgainstIdentity scope acc (applyTypeViewSubst acc templateView) actualView
          )
          callSubst
          (zip evidenceParamViews evidenceActualViews)
      let specializedView = specializeQuantifiedTypeView subst constrainedView
      if hasLeadingTypeViewForall specializedView
        then Nothing
        else Just specializedView
  where
    valueView =
      ordinaryValueTypeView valueInfo

    constrainedView =
      constrainedRuntimeTypeInfoView scope constraints valueView

    callSubst =
      constrainedValueCallSubstWithViews scope valueInfo mbExpectedView argViews

    specializedConstraints =
      map (applyConstraintInfoSubst callSubst) constraints

    evidenceActualViews =
      concatMap (constraintEvidenceTypeInfoViews scope) specializedConstraints

    evidenceParamViews =
      take evidenceCount (methodParamTypeViews constrainedView)
constrainedValueHeadAnnotationView _ _ _ _ _ = Nothing

constrainedValueCallSubstWithViews :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [Maybe TypeView] -> TypeViewSubst
constrainedValueCallSubstWithViews scope valueInfo mbExpectedView argViews =
  refineValueEvidenceViewSubst scope valueInfo mbExpectedView argViews argSubst
  where
    valueView =
      ordinaryValueTypeView valueInfo

    knownPairs =
      [ (templateView, actualView)
      | (templateView, mbActualView) <- zip (methodParamTypeViews valueView) argViews,
        Just actualView <- [mbActualView]
      ]

    argSubst =
      case foldM (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView) Map.empty knownPairs of
        Just subst -> subst
        Nothing -> Map.empty

constraintEvidenceTypeInfoViews :: ElaborateScope -> ConstraintInfo -> [TypeView]
constraintEvidenceTypeInfoViews scope constraint =
  [ methodEvidenceSourceTypeInfoViewRaw (esTypes scope) (esClassesByIdentity scope) classInfo (constraintTypeViews evidenceConstraint) methodInfo
  | (classInfo, evidenceConstraint) <- constraintEvidenceClosureInfo scope constraint,
    methodInfo <- Map.elems (classMethodsByIdentity classInfo)
  ]

refineValueEvidenceViewSubst :: ElaborateScope -> ValueInfo -> Maybe TypeView -> [arg] -> TypeViewSubst -> TypeViewSubst
refineValueEvidenceViewSubst scope valueInfo mbExpectedView args subst =
  case mbExpectedView >>= matchTypeViewAgainstIdentity scope subst resultViewForArity of
    Just subst' -> subst'
    Nothing -> subst
  where
    resultViewForArity =
      valueResultTypeViewForArity (ordinaryValueTypeView valueInfo) (length args)

valueResultTypeViewForArity :: TypeView -> Int -> TypeView
valueResultTypeViewForArity =
  typeViewArrowResultViewForArity

constraintEvidenceArgExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [ResolvedSurfaceExpr]
constraintEvidenceArgExprsInfo scope constraint
  | shouldDeferConstraintEvidenceInfo scope constraint =
      deferConstraintEvidenceExprsInfo scope constraint
  | otherwise =
      resolveConstraintEvidenceExpr scope [] constraint

constraintResolvedEvidenceArgExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [ResolvedSurfaceExpr]
constraintResolvedEvidenceArgExprsInfo scope constraint = do
  let shouldDefer = shouldDeferResolvedConstraintEvidenceInfo scope constraint
  if shouldDefer
    then deferConstraintEvidenceExprsInfo scope constraint
    else resolveConstraintEvidenceExpr scope [] constraint

shouldDeferConstraintEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
shouldDeferConstraintEvidenceInfo scope constraint =
  not (Set.null (freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint)))
    && not (constraintCoveredByEvidenceInfo scope constraint)

shouldDeferResolvedConstraintEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
shouldDeferResolvedConstraintEvidenceInfo scope constraint =
  constraintInfoHasFreeTypeBinderIdentities constraint
    && not (constraintCoveredByEvidenceInfo scope constraint)

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

deferConstraintEvidenceExprsInfo :: ElaborateScope -> ConstraintInfo -> ElaborateM [ResolvedSurfaceExpr]
deferConstraintEvidenceExprsInfo scope constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> throwError (ProgramUnknownClass (constraintDisplayClass constraint))
    Just _ ->
      concat <$> mapM deferOne (constraintEvidenceClosureInfo scope constraint)
  where
    deferOne (classInfo, evidenceConstraint)
      | Map.null (classMethodsByIdentity classInfo) =
          resolveZeroMethodEvidenceExpr scope [] evidenceConstraint
      | otherwise =
          mapM (deferMethodEvidenceExpr scope (constraintTypeViews evidenceConstraint)) (Map.elems (classMethodsByIdentity classInfo))

deferMethodEvidenceExpr :: ElaborateScope -> NonEmpty TypeView -> MethodInfo -> ElaborateM ResolvedSurfaceExpr
deferMethodEvidenceExpr scope classArgViews methodInfo = do
  let methodView = stripVacuousTypeViewForalls (specializeMethodTypeView methodInfo classArgViews)
      methodTy = typeViewDisplay methodView
      fullArity = methodFullArity methodInfo
      resultView = typeViewArrowResultView methodView
  placeholderSurface <-
    if fullArity == 0
      then do
        mbLocal <- resolveLocalNullaryMethodUse scope (Just classArgViews) methodInfo resultView
        maybe (deferNullaryMethodCall scope methodInfo resultView) pure mbLocal
      else deferMethodCall scope methodInfo 0 fullArity methodView Nothing
  expanded <- etaExpandMissingArgs scope methodInfo methodTy Nothing 0 fullArity placeholderSurface
  pure (S.EAnn expanded (loweredTypeViewIdentity scope methodView))

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
            not (any (mentionsFreeValue bindingName) args),
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
            not (any (mentionsFreeResolvedValue bindingRef) args),
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

annotateExpectedValueUse :: ElaborateScope -> Maybe SrcType -> ValueInfo -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
annotateExpectedValueUse scope mbExpected valueInfo applied =
  case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        isRecursiveResultType expectedTy
          || isRecursiveResultType (lowerType scope expectedTy)
          || Builtins.srcTypeMentionsOpaqueBuiltin expectedTy ->
          S.EAnn applied (lowerType scope expectedTy)
    _ -> applied

annotateExpectedBareValueUse :: ElaborateScope -> Maybe SrcType -> ValueInfo -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
annotateExpectedBareValueUse scope mbExpected valueInfo applied =
  case mbExpected of
    Just expectedTy
      | not (isLocalOrdinaryValue valueInfo),
        sourceTypeHasAppliedHead expectedTy ->
          S.EAnn applied (lowerType scope expectedTy)
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

knownResolvedConstructorResultTypeView :: ElaborateScope -> ConstructorInfo -> [P.ResolvedExpr] -> Maybe TypeView
knownResolvedConstructorResultTypeView scope ctorInfo args = do
  argViews <- traverse (inferKnownResolvedExprTypeView scope) args
  subst <-
    foldM
      (\acc (templateView, actualView) -> matchTypeViewAgainstIdentity scope acc templateView actualView)
      Map.empty
      (zip (constructorArgTypeViews scope ctorInfo) argViews)
  pure (applyTypeViewSubst subst (constructorVisibleResultTypeView scope ctorInfo))

matchConstructorArgTypeViews :: ElaborateScope -> ConstructorInfo -> [SrcType] -> Maybe TypeViewSubst
matchConstructorArgTypeViews scope ctorInfo argTypes =
  foldM
    (\acc (templateView, actualTy) -> matchConstructorArgViewSubst scope acc templateView actualTy)
    Map.empty
    (zip (constructorArgTypeViews scope ctorInfo) argTypes)

constructorVisibleResultTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorVisibleResultTypeView scope =
  typeViewArrowResultView . constructorVisibleTypeView scope

compileMethodApp :: ElaborateScope -> Maybe SrcType -> MethodInfo -> [P.Expr] -> ElaborateM ResolvedSurfaceExpr
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
              methodHead <- resolveMethodHeadForCall scope [] methodInfo classArgTys args
              let applied = foldl S.EApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          when (NE.length (methodParamNames methodInfo) > 1) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          when (sourceTypeHasVariableHeadApplication placeholderTy) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          placeholderSurface <-
            deferMethodCall
              scope
              methodInfo
              suppliedArity
              fullArity
              (sourceTypeViewInScope scope placeholderTy)
              (sourceTypeViewInScope scope <$> mbExpectedResult)
          let applied = foldl S.EApp placeholderSurface argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpected of
            Just expectedTy -> S.EAnn expanded (lowerType scope expectedTy)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> S.EAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise = expanded

compileResolvedMethodAppWithExpectedView :: ElaborateScope -> Maybe TypeView -> MethodInfo -> [P.ResolvedExpr] -> ElaborateM ResolvedSurfaceExpr
compileResolvedMethodAppWithExpectedView scope mbExpectedView methodInfo args
  | null args = compileNullaryMethodUseWithView scope mbExpectedView methodInfo
  | otherwise = do
      let fullArity = methodFullArity methodInfo
          suppliedArity = length args
          mbExpectedResultView =
            if suppliedArity >= fullArity
              then mbExpectedView
              else Nothing
          knownClassArgViews = knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResultView
          placeholderView = placeholderResolvedMethodTypeView scope methodInfo args mbExpectedResultView
          placeholderTy = typeViewDisplay placeholderView
          expectedArgViews =
            resolvedMethodExpectedArgViews
              scope
              methodInfo
              args
              mbExpectedResultView
              knownClassArgViews
          allArgumentViewsKnown =
            all (isJust . inferKnownResolvedExprTypeView scope) args
      case knownClassArgViews of
        Just classArgViews
          | allArgumentViewsKnown
          , typeViewsAreIdentityGround classArgViews
          , Nothing <- inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args ->
              throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
        _ -> pure ()
      argSurfaces <- zipWithM compileArg (expectedArgViews ++ repeat Nothing) args
      case knownClassArgViews of
        Just classArgViews
          | shouldResolveMethodBeforeInferenceViews scope methodInfo classArgViews -> do
              methodHead <- resolveResolvedMethodHeadForCall scope [] methodInfo classArgViews args
              let applied = foldl S.EApp methodHead argSurfaces
              expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
              pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
        _ -> do
          when (NE.length (methodParamNames methodInfo) > 1) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          when (sourceTypeHasVariableHeadApplication placeholderTy) $
            throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          placeholderSurface <-
            deferMethodCall
              scope
              methodInfo
              suppliedArity
              fullArity
              placeholderView
              mbExpectedResultView
          let applied = foldl S.EApp placeholderSurface argSurfaces
          expanded <- etaExpandMissingArgs scope methodInfo placeholderTy mbExpected suppliedArity fullArity applied
          pure (annotatePartialMethod expanded placeholderTy suppliedArity fullArity)
  where
    mbExpected = typeViewDisplay <$> mbExpectedView

    compileArg (Just expectedView) =
      compileExpectedResolvedMethodArg scope expectedView
    compileArg Nothing =
      compileResolvedExprWithExpectedView scope Nothing

    annotatePartialMethod expanded placeholderTy suppliedArity fullArity
      | suppliedArity < fullArity =
          case mbExpectedView of
            Just expectedView -> S.EAnn expanded (loweredTypeViewIdentity scope expectedView)
            Nothing ->
              case peelAppliedType placeholderTy suppliedArity of
                Just remainingTy -> S.EAnn expanded (lowerType scope remainingTy)
                Nothing -> expanded
      | otherwise =
          case mbExpectedView of
            Just expectedView -> S.EAnn expanded (loweredTypeViewIdentity scope expectedView)
            Nothing -> expanded

typeViewsAreIdentityGround :: NonEmpty TypeView -> Bool
typeViewsAreIdentityGround views =
  Set.null (freeTypeBinderIdentitiesTypeViews views)

resolvedMethodExpectedArgViews ::
  ElaborateScope ->
  MethodInfo ->
  [P.ResolvedExpr] ->
  Maybe TypeView ->
  Maybe (NonEmpty TypeView) ->
  [Maybe TypeView]
resolvedMethodExpectedArgViews scope methodInfo args mbExpectedResultView mbClassArgViews =
  map (Just . typeViewWithScopeAliases scope) (take (length args) expectedViews)
  where
    expectedViews =
      case mbClassArgViews of
        Just classArgViews ->
          let callSubst =
                resolvedMethodCallSubstWithExpectedResult
                  scope
                  methodInfo
                  classArgViews
                  args
                  mbExpectedResultView
           in methodParamTypeViews
                ( specializeQuantifiedTypeView
                    callSubst
                    (specializeMethodTypeView methodInfo classArgViews)
                )
        Nothing ->
          case mbExpectedResultView >>= matchMethodTypeView scope Map.empty (methodResultTypeView methodInfo) of
            Just resultSubst ->
              methodParamTypeViews
                (specializeQuantifiedTypeView resultSubst (methodTypeView methodInfo))
            Nothing -> []

compileNullaryMethodUse :: ElaborateScope -> Maybe SrcType -> MethodInfo -> ElaborateM ResolvedSurfaceExpr
compileNullaryMethodUse scope mbExpected methodInfo =
  compileNullaryMethodUseWithView scope (sourceTypeViewInScope scope <$> mbExpected) methodInfo

compileNullaryMethodUseWithView :: ElaborateScope -> Maybe TypeView -> MethodInfo -> ElaborateM ResolvedSurfaceExpr
compileNullaryMethodUseWithView scope mbExpected methodInfo =
  case mbExpected of
    Nothing -> throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
    Just expectedView -> do
      let mbClassArgViews =
            (:| []) <$> inferNullaryMethodClassArgView scope methodInfo expectedView
      mbLocal <-
        resolveLocalNullaryMethodUse
          scope
          mbClassArgViews
          methodInfo
          expectedView
      case mbLocal of
        Just localUse -> pure localUse
        Nothing
          | NE.length (methodParamNames methodInfo) > 1 ->
              throwError (ProgramAmbiguousMethodUse (methodName methodInfo))
          | Just classArgViews <- mbClassArgViews,
            shouldResolveNullaryMethodBeforeInferenceViews scope methodInfo classArgViews ->
              resolveResolvedMethodHeadExprInfo scope [] methodInfo classArgViews
          | Just resolvedView <- nullaryMethodExpectedResultView scope mbExpected methodInfo ->
              deferNullaryMethodCall scope methodInfo resolvedView
          | otherwise ->
              throwError (ProgramAmbiguousMethodUse (methodName methodInfo))

resolveLocalNullaryMethodUse :: ElaborateScope -> Maybe (NonEmpty TypeView) -> MethodInfo -> TypeView -> ElaborateM (Maybe ResolvedSurfaceExpr)
resolveLocalNullaryMethodUse scope mbClassArgViews methodInfo expectedView
  | methodFullArity methodInfo /= 0 = pure Nothing
  | otherwise =
      case localNullaryMethodMatches scope mbClassArgViews methodInfo expectedView of
        [] -> pure Nothing
        [(classArgViews, methodEvidence, methodSubst)] -> do
          evidenceHead <- evidenceMethodSurface methodEvidence
          evidenceArgs <-
            methodLocalEvidenceArgsForNullary
              scope
              methodInfo
              classArgViews
              methodSubst
          let specializedEvidenceView =
                specializeQuantifiedTypeView
                  methodSubst
                  (evidenceMethodTypeView methodEvidence)
              specializedEvidenceHead =
                S.EAnn
                  evidenceHead
                  (loweredTypeViewIdentity scope specializedEvidenceView)
              applied = foldl S.EApp specializedEvidenceHead evidenceArgs
              resolvedUse
                | null evidenceArgs = applied
                | otherwise =
                    S.EAnn
                      applied
                      (loweredTypeViewIdentity scope expectedView)
          pure (Just resolvedUse)
        _ -> throwError (ProgramAmbiguousMethodUse (methodName methodInfo))

localNullaryMethodMatches :: ElaborateScope -> Maybe (NonEmpty TypeView) -> MethodInfo -> TypeView -> [(NonEmpty TypeView, EvidenceMethod, TypeViewSubst)]
localNullaryMethodMatches scope mbClassArgViews methodInfo expectedView =
  nubBy sameMatch
    [ (classArgViews, methodEvidence, methodSubst)
    | evidence <- esEvidence scope
    , sameSymbolIdentity
        (evidenceClassSymbol evidence)
        (methodInfoOwnerClassSymbolIdentity methodInfo)
    , Just classArgViews <- [requestedClassArgViews evidence]
    , methodEvidence <-
        maybe
          []
          (: [])
          ( lookupSymbolIdentityExact
              (methodInfoSymbolIdentity methodInfo)
              (evidenceMethodsByIdentity evidence)
          )
    , let specializedMethodView = specializeMethodTypeView methodInfo classArgViews
    , Just methodSubst <-
        [ restrictedNullaryMethodResultSubst
            scope
            methodInfo
            classArgViews
            specializedMethodView
            expectedView
        ]
    ]
  where
    requestedClassArgViews evidence =
      case mbClassArgViews of
        Nothing -> Just (evidenceTypeViews evidence)
        Just requested
          | rigidEvidenceTypeViewsMatch scope (evidenceTypeViews evidence) requested ->
              Just (evidenceTypeViews evidence)
          | otherwise -> Nothing

    sameMatch (leftViews, leftMethod, leftSubst) (rightViews, rightMethod, rightSubst) =
      leftViews == rightViews
        && leftMethod == rightMethod
        && leftSubst == rightSubst

restrictedNullaryMethodResultSubst :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> TypeView -> TypeView -> Maybe TypeViewSubst
restrictedNullaryMethodResultSubst scope methodInfo classArgViews specializedMethodView expectedView = do
  subst <-
    matchMethodTypeViews
      scope
      Map.empty
      (methodResultTypeViewFrom specializedMethodView :| [])
      (expectedView :| [])
  if Map.keysSet subst `Set.isSubsetOf` methodLocalTypeBinderIdentities methodInfo classArgViews
    then Just subst
    else Nothing

-- Local class evidence is a rigid assumption of the enclosing term.  Only
-- variables quantified by the method itself may be specialized at a use site;
-- specializing a class-head variable would turn (for example) evidence for
-- @Pick a@ into evidence for @Pick Bool@.
methodLocalTypeBinderIdentities :: MethodInfo -> NonEmpty TypeView -> Set TypeBinderIdentity
methodLocalTypeBinderIdentities methodInfo classArgViews =
  ( freeTypeBinderIdentitiesTypeView specializedMethodView
      `Set.union` explicitlyQuantifiedVars
      `Set.union` constraintVars
  )
    Set.\\ freeTypeBinderIdentitiesTypeViews classArgViews
  where
    classArgSubst =
      typeViewSubstFromParamIdentities
        (methodParamBinderIdentities methodInfo)
        classArgViews
    specializedMethodView = specializeMethodTypeView methodInfo classArgViews
    explicitlyQuantifiedVars =
      Set.fromList
        [ identity
        | (_, identity, _) <- typeViewForallBinderViews specializedMethodView
        ]
    specializedConstraints =
      map
        (applyConstraintInfoSubst classArgSubst)
        (methodConstraintInfos methodInfo)
    constraintVars =
      foldMap
        (freeTypeBinderIdentitiesTypeViews . constraintTypeViews)
        specializedConstraints

{- Note [Local evidence heads are rigid]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An 'EvidenceInfo' records an assumption introduced by the enclosing binding.
Its class-head variables are therefore rigid: evidence for @Eq a@ is not a
template that can be specialized to @Eq Bool@.  Flexible matching remains at
the global instance-selection boundary and when specializing binders quantified
by the method itself.
-}
rigidEvidenceTypeViewsMatch :: ElaborateScope -> NonEmpty TypeView -> NonEmpty TypeView -> Bool
rigidEvidenceTypeViewsMatch scope templates actuals =
  case matchMethodTypeViews scope Map.empty templates actuals of
    Just subst -> Map.null subst
    Nothing -> False

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

compileExpectedMethodArg :: ElaborateScope -> SrcType -> P.Expr -> ElaborateM ResolvedSurfaceExpr
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
      localRef <- freshElaborateLocalRef (P.paramName param)
      actualExpr <- compileExpr scope (Just expectedTy) actual
      scope' <- extendLocalWithRef scope localRef (P.paramName param) runtimeName (Just expectedTy)
      bodyExpr <- compileExpr scope' (Just expectedTy) body
      pure
        ( resolvedLocalLetSurface
            runtimeName
            localRef
            actualExpr
            (S.EAnn bodyExpr (lowerType scope expectedTy))
        )
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
      pure (S.EAnn argExpr (lowerType scope expectedTy))

compileExpectedResolvedMethodArg :: ElaborateScope -> TypeView -> P.ResolvedExpr -> ElaborateM ResolvedSurfaceExpr
compileExpectedResolvedMethodArg scope expectedView expr = do
  case inferKnownResolvedExprTypeView scope expr of
    Just actualView -> ensureTypeViewCompatible scope expectedView actualView
    Nothing -> pure ()
  case expr of
    EAnn (EVar ref) _ ->
      compileResolvedExprWithExpectedView scope (Just expectedView) (EVar ref)
    EAnn {} ->
      compileResolvedExprWithExpectedView scope (Just expectedView) expr
    EApp (ELam param (EVar (P.ResolvedLocalValue bodyName))) actual
      | bodyName == P.paramName param ->
          compileResolvedExprWithExpectedView scope (Just expectedView) actual
    EApp (ELam param body) actual -> do
      let paramRef = P.paramName param
      runtimeName <- freshRuntimeName (localRefName paramRef)
      actualExpr <- compileResolvedExprWithExpectedView scope (Just expectedView) actual
      scope' <- extendResolvedLocalView scope paramRef runtimeName (Just expectedView)
      bodyExpr <- compileResolvedExprWithExpectedView scope' (Just expectedView) body
      pure $
        resolvedLocalLetSurface
          runtimeName
          paramRef
          actualExpr
          (S.EAnn bodyExpr (loweredTypeViewIdentity scope expectedView))
    EVar ref
      | Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExprWithExpectedView scope (Just expectedView) expr
    EVar ref
      | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
        methodFullArity methodInfo == 0 ->
          compileResolvedExprWithExpectedView scope (Just expectedView) expr
    EVar {} ->
      compileResolvedExprWithExpectedView scope Nothing expr
    _
      | (EVar ref, _) <- collectResolvedApps expr,
        Right ConstructorValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) ->
          compileResolvedExprWithExpectedView scope (Just expectedView) expr
    _ -> do
      argExpr <- compileResolvedExprWithExpectedView scope Nothing expr
      pure (S.EAnn argExpr (loweredTypeViewIdentity scope expectedView))

ensureSourceTypeCompatible :: ElaborateScope -> SrcType -> SrcType -> ElaborateM ()
ensureSourceTypeCompatible scope expectedTy actualTy =
  when (sourceTypesNeedRejection scope expectedTy actualTy) $
    throwError (ProgramTypeMismatch actualTy expectedTy)

ensureTypeViewCompatible :: ElaborateScope -> TypeView -> TypeView -> ElaborateM ()
ensureTypeViewCompatible scope expectedView actualView =
  when rejection $
    throwError
      ( ProgramTypeMismatch
          (typeViewDisplay actualView)
          (typeViewDisplay expectedView)
      )
  where
    expectedViewWithIdentities =
      typeViewWithScopeAliases scope expectedView
    actualViewWithIdentities =
      typeViewWithScopeAliases scope actualView
    rejection =
      typeViewsNeedRejection scope expectedViewWithIdentities actualViewWithIdentities

typeViewsNeedRejection :: ElaborateScope -> TypeView -> TypeView -> Bool
typeViewsNeedRejection scope expectedView actualView =
  typeViewsHaveConflictingFreeBinderIdentities expectedView actualView
    || not (typeViewsCompatible expectedView actualView)
  where
    typeViewsCompatible expected actual =
      matches expected actual
        || matches actual expected
        || structuralIdentityMatches expected actual
        || case typeViewNodeView actual of
          TypeViewForallNode _ _ _ body ->
            typeViewsCompatible expected body
          _ -> False

    matches template actual =
      case matchTypeViewAgainstIdentity scope Map.empty template actual of
        Just _ -> True
        Nothing -> False

    structuralIdentityMatches expected actual =
      not (Set.null expectedStructuralIdentities)
        && expectedStructuralIdentities == actualStructuralIdentities
        && (matches loweredExpected loweredActual || matches loweredActual loweredExpected)
      where
        loweredExpected = loweredIdentityView expected
        loweredActual = loweredIdentityView actual
        expectedStructuralIdentities = structuralBinderIdentities loweredExpected
        actualStructuralIdentities = structuralBinderIdentities loweredActual

    -- Structural compatibility is only evidence when lowering preserves the
    -- TypeView's binder payloads; rebuilding its display type can lose them.
    loweredIdentityView =
      lowerTypeViewWithIdentities scope

    -- Alias contexts may retain structural binders for every visible data
    -- type.  Only binders occurring in the projected type shape are evidence
    -- that two lowered views share a nominal owner.
    structuralBinderIdentities = go
      where
        go view =
          case typeViewNodeView view of
            TypeViewVarNode _ identity -> structuralIdentity identity
            TypeViewArrowNode dom cod -> go dom <> go cod
            TypeViewBaseNode {} -> Set.empty
            TypeViewConNode _ _ args -> foldMap go args
            TypeViewVarAppNode _ identity args ->
              structuralIdentity identity <> foldMap go args
            TypeViewTyLamNode _ identity body ->
              structuralIdentity identity <> go body
            TypeViewTyAppNode fun arg -> go fun <> go arg
            TypeViewForallNode _ identity mbBound body ->
              structuralIdentity identity <> foldMap go mbBound <> go body
            TypeViewMuNode _ identity body ->
              structuralIdentity identity <> go body
            TypeViewBottomNode -> Set.empty

        structuralIdentity identity =
          maybe Set.empty Set.singleton (typeBinderIdentityStructural identity)

typeViewsHaveConflictingFreeBinderIdentities :: TypeView -> TypeView -> Bool
typeViewsHaveConflictingFreeBinderIdentities leftView rightView =
  go Set.empty Set.empty leftView rightView
  where
    go leftBound rightBound left right =
      case (typeViewNodeView left, typeViewNodeView right) of
        (TypeViewVarNode _ leftIdentity, TypeViewVarNode _ rightIdentity) ->
          freeBinderIdentityDiffers leftBound leftIdentity rightBound rightIdentity
        (TypeViewArrowNode leftDom leftCod, TypeViewArrowNode rightDom rightCod) ->
          go leftBound rightBound leftDom rightDom
            || go leftBound rightBound leftCod rightCod
        (TypeViewConNode _ _ leftArgs, TypeViewConNode _ _ rightArgs)
          | NE.length leftArgs == NE.length rightArgs ->
              or (NE.toList (NE.zipWith (go leftBound rightBound) leftArgs rightArgs))
        (TypeViewVarAppNode _ leftIdentity leftArgs, TypeViewVarAppNode _ rightIdentity rightArgs)
          | NE.length leftArgs == NE.length rightArgs ->
              freeBinderIdentityDiffers leftBound leftIdentity rightBound rightIdentity
                || or (NE.toList (NE.zipWith (go leftBound rightBound) leftArgs rightArgs))
        (TypeViewTyLamNode _ leftIdentity leftBody, TypeViewTyLamNode _ rightIdentity rightBody) ->
          go
            (Set.insert leftIdentity leftBound)
            (Set.insert rightIdentity rightBound)
            leftBody
            rightBody
        (TypeViewTyAppNode leftFun leftArg, TypeViewTyAppNode rightFun rightArg) ->
          go leftBound rightBound leftFun rightFun
            || go leftBound rightBound leftArg rightArg
        (TypeViewForallNode _ leftIdentity leftMbBound leftBody, TypeViewForallNode _ rightIdentity rightMbBound rightBody) ->
          maybeBoundsConflict leftBound rightBound leftMbBound rightMbBound
            || go
              (Set.insert leftIdentity leftBound)
              (Set.insert rightIdentity rightBound)
              leftBody
              rightBody
        (TypeViewMuNode _ leftIdentity leftBody, TypeViewMuNode _ rightIdentity rightBody) ->
          go
            (Set.insert leftIdentity leftBound)
            (Set.insert rightIdentity rightBound)
            leftBody
            rightBody
        _ -> False

    maybeBoundsConflict leftBound rightBound leftMbBound rightMbBound =
      case (leftMbBound, rightMbBound) of
        (Just leftBoundTy, Just rightBoundTy) ->
          go leftBound rightBound leftBoundTy rightBoundTy
        _ -> False

    freeBinderIdentityDiffers leftBound leftIdentity rightBound rightIdentity =
      case
          ( freeBinderIdentity leftBound leftIdentity,
            freeBinderIdentity rightBound rightIdentity
          )
        of
          (Just leftFreeIdentity, Just rightFreeIdentity) -> leftFreeIdentity /= rightFreeIdentity
          _ -> False

    freeBinderIdentity bound identity =
      if identity `Set.member` bound
        then Nothing
        else Just identity

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
    Just identity -> maybe False (const True) (lookupSymbolIdentityExact identity (esTypesByIdentity scope))
    Nothing -> False

resolveMethodHeadExprInfo :: ElaborateScope -> [ClassApplicationKey] -> MethodInfo -> NonEmpty TypeView -> ElaborateM ResolvedSurfaceExpr
resolveMethodHeadExprInfo scope seen methodInfo classArgViews =
  resolveMethodHeadExprInfoWith (pure . constraintInfoGroundByTypeBinderIdentities) scope seen methodInfo classArgViews

resolveResolvedMethodHeadExprInfo :: ElaborateScope -> [ClassApplicationKey] -> MethodInfo -> NonEmpty TypeView -> ElaborateM ResolvedSurfaceExpr
resolveResolvedMethodHeadExprInfo scope seen methodInfo classArgViews =
  resolveMethodHeadExprInfoWith (pure . constraintInfoGroundByTypeBinderIdentities) scope seen methodInfo classArgViews

resolveMethodHeadExprInfoWith ::
  (ConstraintInfo -> ElaborateM Bool) ->
  ElaborateScope ->
  [ClassApplicationKey] ->
  MethodInfo ->
  NonEmpty TypeView ->
  ElaborateM ResolvedSurfaceExpr
resolveMethodHeadExprInfoWith groundPredicate scope seen methodInfo classArgViews =
  case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence ->
      evidenceMethodSurface methodEvidence
    Nothing -> do
      (instanceInfo, subst) <- liftEitherElab (resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews)
      case lookupInstanceMethod methodInfo instanceInfo of
        Just methodValue@OrdinaryValue {valueConstraintInfos = constraints} -> do
          eagerConstraints <-
            filterM
              groundPredicate
              (map (applyConstraintInfoSubst subst) constraints)
          evidenceArgs <-
            concat
              <$> mapM
                (resolveConstraintEvidenceExpr scope seen)
                eagerConstraints
          methodHead <- resolvedInstanceMethodSurface scope methodValue
          pure (foldl S.EApp methodHead evidenceArgs)
        _ -> throwError (ProgramUnknownMethod (methodName methodInfo))

resolvedInstanceMethodSurface :: ElaborateScope -> ValueInfo -> ElaborateM ResolvedSurfaceExpr
resolvedInstanceMethodSurface _ methodValue = do
  let runtimeName = valueInfoRuntimeName methodValue
  pure (S.EResolvedVar (valueInfoRuntimeDetails methodValue) runtimeName)

resolveMethodHeadForCall :: ElaborateScope -> [ClassApplicationKey] -> MethodInfo -> NonEmpty SrcType -> [P.Expr] -> ElaborateM ResolvedSurfaceExpr
resolveMethodHeadForCall scope seen methodInfo classArgTys args =
  let classArgViews = fmap (sourceTypeViewInScope scope) classArgTys
   in case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence -> do
      let methodSubst =
            fromMaybe Map.empty (inferMethodCallSubst scope methodInfo classArgTys args)
      evidenceArgs <-
        methodLocalEvidenceArgsForCall
          scope
          methodInfo
          classArgViews
          methodSubst
      evidenceHead <- specializedEvidenceMethodSurface scope methodEvidence methodSubst
      pure (foldl S.EApp evidenceHead evidenceArgs)
    Nothing -> resolveMethodHeadExprInfo scope seen methodInfo classArgViews

resolveResolvedMethodHeadForCall :: ElaborateScope -> [ClassApplicationKey] -> MethodInfo -> NonEmpty TypeView -> [P.ResolvedExpr] -> ElaborateM ResolvedSurfaceExpr
resolveResolvedMethodHeadForCall scope seen methodInfo classArgViews args =
  case lookupEvidenceMethodByClassViews scope (methodInfoClassIdentity methodInfo) classArgViews (methodInfoSymbolIdentity methodInfo) of
    Just methodEvidence -> do
      let methodSubst =
            fromMaybe
              Map.empty
              (inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args)
      evidenceArgs <-
        methodLocalEvidenceArgsForResolvedCall
          scope
          methodInfo
          classArgViews
          methodSubst
      evidenceHead <- specializedEvidenceMethodSurface scope methodEvidence methodSubst
      pure (foldl S.EApp evidenceHead evidenceArgs)
    Nothing -> resolveResolvedMethodHeadExprInfo scope seen methodInfo classArgViews

evidenceMethodSurfaceName :: EvidenceMethod -> ElaborateM String
evidenceMethodSurfaceName =
  pure . X.resolvedVarRuntimeName . evidenceMethodResolvedVar

evidenceMethodSurface :: EvidenceMethod -> ElaborateM ResolvedSurfaceExpr
evidenceMethodSurface =
  pure . toSurface . evidenceMethodResolvedVar
  where
    toSurface resolved =
      S.EResolvedVar
        (X.resolvedVarDetails resolved)
        (X.resolvedVarRuntimeName resolved)

specializedEvidenceMethodSurface :: ElaborateScope -> EvidenceMethod -> TypeViewSubst -> ElaborateM ResolvedSurfaceExpr
specializedEvidenceMethodSurface scope methodEvidence methodSubst = do
  evidenceSurface <- evidenceMethodSurface methodEvidence
  let specializedEvidenceView =
        specializeQuantifiedTypeView
          methodSubst
          (evidenceMethodTypeView methodEvidence)
  pure $
    if Map.null methodSubst
      then evidenceSurface
      else
        S.EAnn
          evidenceSurface
          (loweredTypeViewIdentity scope specializedEvidenceView)

methodLocalEvidenceArgsForCall :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> TypeViewSubst -> ElaborateM [ResolvedSurfaceExpr]
methodLocalEvidenceArgsForCall scope methodInfo classArgViews methodSubst = do
  let specializedConstraints =
        specializeMethodLocalConstraints methodInfo classArgViews methodSubst
  concat <$> mapM (constraintEvidenceArgExprsInfo scope) specializedConstraints

methodLocalEvidenceArgsForResolvedCall :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> TypeViewSubst -> ElaborateM [ResolvedSurfaceExpr]
methodLocalEvidenceArgsForResolvedCall scope methodInfo classArgViews methodSubst = do
  let specializedConstraints =
        specializeMethodLocalConstraints methodInfo classArgViews methodSubst
  concat <$> mapM (constraintResolvedEvidenceArgExprsInfo scope) specializedConstraints

methodLocalEvidenceArgsForNullary :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> TypeViewSubst -> ElaborateM [ResolvedSurfaceExpr]
methodLocalEvidenceArgsForNullary scope methodInfo classArgViews methodSubst =
  concat
    <$> mapM
      (constraintResolvedEvidenceArgExprsInfo scope)
      (specializeMethodLocalConstraints methodInfo classArgViews methodSubst)

specializeMethodLocalConstraints :: MethodInfo -> NonEmpty TypeView -> TypeViewSubst -> [ConstraintInfo]
specializeMethodLocalConstraints methodInfo classArgViews methodSubst =
  map (applyConstraintInfoSubst methodSubst) methodLocalConstraints
  where
    headVars = freeTypeBinderIdentitiesTypeViews classArgViews
    classArgSubst =
      typeViewSubstFromParamIdentities
        (methodParamBinderIdentities methodInfo)
        classArgViews
    methodLocalConstraints =
      filter
        (not . constraintInfoDeterminedByTypeBinderIdentities headVars)
        ( map
            (applyConstraintInfoSubst classArgSubst)
            (methodConstraintInfos methodInfo)
        )

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

{- Note [Expected method results own result binder identities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a fully applied method, the enclosing expected result is the declaration
of the result binders' identities.  Argument inference may discover additional
method binders, but a structurally expanded argument must not replace an
already-known nominal result (for example, Prelude.Unit with its recursive
encoding).  Seed the substitution from the expected result and retain that
seed when a merely structural argument view disagrees.  The expected argument
checks still reject genuine source-type mismatches.
-}
resolvedMethodCallSubstWithExpectedResult :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> [P.ResolvedExpr] -> Maybe TypeView -> TypeViewSubst
resolvedMethodCallSubstWithExpectedResult scope methodInfo classArgViews args mbExpectedResultView =
  foldl refineFromArgument resultSubst knownPairs
  where
    specializedMethodView =
      specializeMethodTypeView methodInfo classArgViews

    resultSubst =
      case
        mbExpectedResultView
          >>= matchMethodTypeView
            scope
            Map.empty
            (methodResultTypeViewFrom specializedMethodView)
      of
        Just subst -> subst
        Nothing -> Map.empty

    knownPairs =
      [ (templateView, actualView)
      | (templateView, arg) <- zip (methodParamTypeViews specializedMethodView) args
      , Just actualView <- [inferKnownResolvedExprTypeView scope arg]
      ]

    refineFromArgument subst (templateView, actualView) =
      fromMaybe
        subst
        (matchTypeViewAgainstIdentity scope subst templateView actualView)

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

shouldResolveNullaryMethodBeforeInferenceViews :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> Bool
shouldResolveNullaryMethodBeforeInferenceViews scope methodInfo classArgViews
  | typeViewsAreIdentityGround classArgViews =
      case resolveMethodInstanceInfoByTypeViews scope methodInfo classArgViews of
        Right _ -> True
        Left _ -> False
  | otherwise =
      shouldResolveMethodBeforeInferenceViews scope methodInfo classArgViews

resolveConstraintEvidenceExpr :: ElaborateScope -> [ClassApplicationKey] -> ConstraintInfo -> ElaborateM [ResolvedSurfaceExpr]
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
                  (key : seen)
                  methodInfo
                  (constraintTypeViews evidenceConstraint)
            )
            (Map.elems (classMethodsByIdentity classInfo))

    whenSeen key =
      when (key `elem` seen) $
        throwError (noMatchingInstanceError scope constraint)

resolveZeroMethodEvidenceExpr :: ElaborateScope -> [ClassApplicationKey] -> ConstraintInfo -> ElaborateM [ResolvedSurfaceExpr]
resolveZeroMethodEvidenceExpr scope seen constraint
  | zeroMethodConstraintCoveredByEvidenceInfo scope constraint = pure []
  | otherwise = do
      let key = constraintEvidenceKey constraint
      (instanceInfo, subst) <- liftEitherElab (resolveInstanceInfoByConstraint scope constraint)
      _ <-
        concat
          <$> mapM
            (resolveConstraintEvidenceExpr scope (key : seen) . applyConstraintInfoSubst subst)
            (instanceConstraintInfos instanceInfo)
      pure []

zeroMethodConstraintCoveredByEvidenceInfo :: ElaborateScope -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidenceInfo scope constraint =
  any
    ( \evidence ->
        sameSymbolIdentity (evidenceClassSymbol evidence) (constraintClassSymbol constraint)
          && rigidEvidenceTypeViewsMatch
            scope
            (evidenceTypeViews evidence)
            (constraintTypeViews constraint)
    )
    (esEvidence scope)

lookupEvidenceMethodInfo :: ElaborateScope -> ConstraintInfo -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodInfo scope constraint =
  lookupEvidenceMethodByClassViews scope (constraintClassSymbol constraint) (constraintTypeViews constraint)

lookupEvidenceMethodByClassViews :: ElaborateScope -> SymbolIdentity -> NonEmpty TypeView -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodByClassViews scope classIdentity0 headViews methodIdentity =
  uniqueEvidenceMethod
    [ methodEvidence
      | evidence <- esEvidence scope,
        sameSymbolIdentity (evidenceClassSymbol evidence) classIdentity0,
        rigidEvidenceTypeViewsMatch scope (evidenceTypeViews evidence) headViews,
        methodEvidence <- maybe [] (: []) (lookupSymbolIdentityExact methodIdentity (evidenceMethodsByIdentity evidence))
    ]

classInfoForConstraint :: ElaborateScope -> ConstraintInfo -> Maybe ClassInfo
classInfoForConstraint scope constraint =
  lookupSymbolIdentityExact (constraintClassSymbol constraint) (esClassesByIdentity scope)

constraintEvidenceKey :: ConstraintInfo -> ClassApplicationKey
constraintEvidenceKey =
  constraintClassApplicationKey

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

etaExpandMissingArgs :: ElaborateScope -> MethodInfo -> SrcType -> Maybe SrcType -> Int -> Int -> ResolvedSurfaceExpr -> ElaborateM ResolvedSurfaceExpr
etaExpandMissingArgs scope methodInfo methodTy mbExpected suppliedArity fullArity applied = do
  let missingArity = max 0 (fullArity - suppliedArity)
  if missingArity == 0
    then pure applied
    else do
      missingNames <- replicateM missingArity (freshRuntimeName (methodInfoStableName methodInfo ++ "_arg"))
      missingRefs <- mapM freshElaborateLocalRef missingNames
      let missingTypes = zipWith preferExpectedType methodMissingTypes (expectedMissingTypes ++ repeat Nothing)
          missingArgs = zip3 missingNames missingRefs missingTypes
          body =
            foldl
              S.EApp
              applied
              [ S.EResolvedVar (LocalId ref) name
              | (name, ref, _) <- missingArgs
              ]
      pure (foldr wrapMissingArg body missingArgs)
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

    wrapMissingArg (name, ref, ty) body =
      resolvedLocalLamSurface
        name
        ref
        (if Set.null (freeTypeVarsSrcType ty) then Just (lowerType scope ty) else Nothing)
        body

methodFullArity :: MethodInfo -> Int
methodFullArity methodInfo =
  length (methodArgumentTypes (methodType methodInfo))

methodArgumentTypes :: SrcType -> [SrcType]
methodArgumentTypes ty =
  let (_, bodyTy) = splitForalls ty
      (argTys, _) = splitArrows bodyTy
   in argTys

deferMethodCall :: ElaborateScope -> MethodInfo -> Int -> Int -> TypeView -> Maybe TypeView -> ElaborateM ResolvedSurfaceExpr
deferMethodCall scope methodInfo suppliedArity fullArity placeholderSourceView mbExpectedResult = do
  placeholder <- freshDeferredMethodName (methodInfoStableName methodInfo)
  ref <- freshElaborateDeferredRef placeholder
  let suppliedMethodArgCount = min suppliedArity fullArity
      remainingMethodArgCount = fullArity - suppliedMethodArgCount
      visiblePlaceholderView =
        preferVisibleTypeView scope placeholderSourceView
      placeholderView = lowerTypeViewWithIdentities scope visiblePlaceholderView
      instBinders =
        [ (name, identity)
        | (name, identity, _) <- typeViewForallBinderViews placeholderView
        ]
      deferred =
        DeferredMethodCall
          { deferredMethodRef = ref,
            deferredMethodInfo = methodInfo,
            deferredMethodSuppliedArgCount = suppliedMethodArgCount,
            deferredMethodRemainingArgCount = remainingMethodArgCount,
            deferredMethodInstBinders = instBinders,
            deferredMethodExpectedResult = mbExpectedResult,
            deferredMethodEvidence = Nothing,
            deferredMethodLocalEvidence = esEvidence scope
          }
  registerDeferredObligation placeholderView (DeferredMethod deferred)
  pure (S.EResolvedVar (DeferredId ref) placeholder)

deferNullaryMethodCall :: ElaborateScope -> MethodInfo -> TypeView -> ElaborateM ResolvedSurfaceExpr
deferNullaryMethodCall scope methodInfo expectedView = do
  placeholder <- freshDeferredMethodName (methodInfoStableName methodInfo)
  ref <- freshElaborateDeferredRef placeholder
  let placeholderView = lowerTypeViewWithIdentities scope expectedView
      instBinders =
        [ (name, identity)
        | (name, identity, _) <- typeViewForallBinderViews placeholderView
        ]
      mbClassArgViews = (:| []) <$> inferNullaryMethodClassArgView scope methodInfo expectedView
      localEvidence = nullaryMethodEvidence scope methodInfo expectedView
      deferredLocalEvidence =
        case mbClassArgViews of
          Just classArgViews ->
            filter
              (retainsDeferredNullaryEvidence scope methodInfo classArgViews)
              (esEvidence scope)
          Nothing -> esEvidence scope
      deferred =
        DeferredMethodCall
          { deferredMethodRef = ref,
            deferredMethodInfo = methodInfo,
            deferredMethodSuppliedArgCount = 0,
            deferredMethodRemainingArgCount = 0,
            deferredMethodInstBinders = instBinders,
            deferredMethodExpectedResult = Just expectedView,
            deferredMethodEvidence = localEvidence,
            deferredMethodLocalEvidence = deferredLocalEvidence
          }
  registerDeferredObligation placeholderView (DeferredMethod deferred)
  pure (S.EResolvedVar (DeferredId ref) placeholder)

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

retainsDeferredNullaryEvidence :: ElaborateScope -> MethodInfo -> NonEmpty TypeView -> EvidenceInfo -> Bool
retainsDeferredNullaryEvidence scope methodInfo classArgViews evidence =
  not (sameSymbolIdentity (evidenceClassSymbol evidence) ownerClassIdentity)
    || rigidEvidenceTypeViewsMatch scope (evidenceTypeViews evidence) classArgViews
  where
    ownerClassIdentity = methodInfoOwnerClassSymbolIdentity methodInfo

deferConstructorCall :: ElaborateScope -> ConstructorInfo -> Int -> Maybe TypeView -> TypeViewSubst -> ElaborateM ResolvedSurfaceExpr
deferConstructorCall scope ctorInfo argCount mbExpectedView initialViewSubst = do
  let quantifiedView = quantifiedConstructorTypeView scope ctorInfo
      occurrenceView = constructorOccurrenceTypeView scope ctorInfo argCount
      initialSourceView = specializeQuantifiedTypeView initialViewSubst quantifiedView
      initialOccurrenceView = applyTypeViewSubst initialViewSubst occurrenceView
      bindingMode = constructorDeferredBindingMode scope ctorInfo argCount mbExpectedView
      instBinders =
        [ (name, identity)
        | (name, identity, _) <- typeViewForallBinderViews quantifiedView
        ]
      initialTypeBinderSubst = typeBinderSubstFromTypeViewSubst initialViewSubst
      missingInitialBinders =
        [ name
        | (name, identity) <- instBinders,
          Nothing <- [lookupTypeBinderSubstViewByIdentity identity initialTypeBinderSubst]
        ]
  -- A monomorphic nullary occurrence has no argument from which its quantified
  -- binders can be recovered. Its expected occurrence type must therefore
  -- select all binders before the graph pipeline; a whole-scheme alias retains
  -- those binders instead of selecting them.
  when (bindingMode == DeferredBindingMonomorphic && null (ctorArgs ctorInfo) && not (null missingInitialBinders)) $
    throwError (ProgramAmbiguousConstructorUse (ctorName ctorInfo))
  placeholder <- freshDeferredConstructorName (constructorInfoStableName ctorInfo)
  ref <- freshElaborateDeferredRef placeholder
  let placeholderSourceTy = typeViewDisplay initialSourceView
      loweredPlaceholderTy = lowerType scope placeholderSourceTy
      placeholderTy =
        if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
          && srcTypeHasVariableHeadApplication loweredPlaceholderTy
          then constructorStructuralPlaceholderType scope ctorInfo
          else loweredPlaceholderTy
      constructorHeadIdentities = constructorTypeHeadIdentityLookupAliases scope ctorInfo
      placeholderHeadIdentities =
        mergeSymbolIdentityMaps
          [ constructorHeadIdentities,
            typeViewHeadIdentityLookupAliases initialSourceView,
            typeViewHeadIdentityLookupAliases initialOccurrenceView,
            sourceTypeHeadIdentitiesInScope scope placeholderTy
          ]
      placeholderBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeBinderAliasIdentityMap instBinders,
            typeViewBinderIdentities initialSourceView,
            typeViewBinderIdentities initialOccurrenceView,
            sourceTypeBinderIdentitiesInScope scope placeholderTy
          ]
      deferred =
        DeferredConstructorCall
          { deferredConstructorRef = ref,
            deferredConstructorInfo = ctorInfo,
            deferredConstructorArgCount = argCount,
            deferredConstructorSourceTypeView = initialSourceView,
            deferredConstructorOccurrenceTypeView = initialOccurrenceView,
            deferredConstructorInstBinders = instBinders,
            deferredConstructorInitialSubst = initialTypeBinderSubst,
            deferredConstructorBindingMode = bindingMode
          }
  placeholderView <-
    if constructorOwnerHasVariableHeadApplication (elaborateScopeDataTypesByIdentity scope) ctorInfo
      && srcTypeHasVariableHeadApplication loweredPlaceholderTy
      then liftEitherElab (deferredPlaceholderView placeholderTy placeholderHeadIdentities placeholderBinderIdentities)
      else pure (lowerTypeViewWithIdentities scope initialSourceView)
  registerDeferredObligation placeholderView (DeferredConstructor deferred)
  pure (S.EResolvedVar (DeferredId ref) placeholder)

-- A bare constructor can be a Var-Let producer only when its expected type is
-- the constructor's complete quantified scheme.  Constructor applications and
-- proper specializations remain monomorphic occurrences: their forall binders
-- must be selected by the occurrence before deferred finalization.
constructorDeferredBindingMode :: ElaborateScope -> ConstructorInfo -> Int -> Maybe TypeView -> DeferredBindingMode
constructorDeferredBindingMode scope ctorInfo argCount mbExpectedView
  | argCount == 0,
    not (null (typeViewForallBinderViews quantifiedView)),
    Just expectedView <- mbExpectedView,
    alphaEqTypesWithHeadIdentitiesInScope
      scope
      headIdentities
      (typeViewIdentity quantifiedView)
      (typeViewIdentity expectedView) =
      DeferredBindingScheme
  | otherwise =
      DeferredBindingMonomorphic
  where
    quantifiedView = quantifiedConstructorTypeView scope ctorInfo
    headIdentities =
      mergeSymbolIdentityMaps
        [ typeViewHeadIdentityLookupAliases quantifiedView,
          maybe Map.empty typeViewHeadIdentityLookupAliases mbExpectedView
        ]

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
      case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctorInfo) dataTypesByIdentity of
        Just dataInfo -> map constructorShapeFromInfo (dataConstructors dataInfo)
        Nothing -> constructorOwnerShapes ctorInfo

    resultVar = constructorOwnerResultVar ctorInfo

    handlerType shape =
      constructorStructuralHandlerType resultVar shape

constructorOwnerResultVar :: ConstructorInfo -> String
constructorOwnerResultVar ctorInfo =
  "$" ++ symbolIdentityStableName (ctorOwningTypeIdentity ctorInfo) ++ "_result"

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

constructorInfoWithArgs :: ElaborateScope -> ConstructorInfo -> [SrcType] -> ConstructorInfo
constructorInfoWithArgs scope ctorInfo =
  constructorInfoWithArgViews ctorInfo . map (sourceTypeViewInScope scope)

constructorInfoWithArgViews :: ConstructorInfo -> [TypeView] -> ConstructorInfo
constructorInfoWithArgViews ctorInfo argViews =
  ctorInfo
    { ctorTypeView =
        typeViewRebuildArrowBody original argViews resultView
    }
  where
    original = ctorTypeView ctorInfo
    resultView = constructorInfoResultView ctorInfo

constructorOccurrenceTypeView :: ElaborateScope -> ConstructorInfo -> Int -> TypeView
constructorOccurrenceTypeView scope ctorInfo argCount =
  typeViewArrowResultViewForArity (constructorTypeView scope ctorInfo) argCount

constructorTypeHeadIdentityLookupAliases :: ElaborateScope -> ConstructorInfo -> Map String SymbolIdentity
constructorTypeHeadIdentityLookupAliases scope ctorInfo =
  mergeSymbolIdentityMaps
    [ constructorInfoHeadIdentityLookupAliases ctorInfo,
      typeViewHeadIdentityLookupAliases (constructorTypeView scope ctorInfo)
    ]

constructorArgTypeViews :: ElaborateScope -> ConstructorInfo -> [TypeView]
constructorArgTypeViews scope ctorInfo =
  typeViewArrowArgViews (constructorTypeView scope ctorInfo)

constructorResultTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorResultTypeView scope ctorInfo =
  typeViewArrowResultView (constructorTypeView scope ctorInfo)

constructorTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorTypeView scope ctorInfo =
  typeViewWithIdentityAliases
    ( mergeSymbolIdentityMaps
        [ typeViewHeadIdentities view,
          sourceTypeHeadIdentitiesInScope scope (typeViewDisplay view)
        ]
    )
    (mergeTypeBinderIdentityMaps [typeViewBinderIdentities view, constructorBinderAliases scope ctorInfo])
    view
  where
    view = ctorTypeView ctorInfo

constructorBinderAliases :: ElaborateScope -> ConstructorInfo -> Map String TypeBinderIdentity
constructorBinderAliases scope ctorInfo =
  mergeTypeBinderIdentityMaps (ownerParamAliases ++ forallAliases)
  where
    ownerParamAliases =
      case resolveConstructorDataInfo scope ctorInfo of
        Just dataInfo -> [typeBinderAliasIdentityMap (dataParamBinders dataInfo)]
        Nothing -> []

    forallAliases =
      [ typeBinderAliasIdentityMap [(constructorForallDisplayName binder, constructorForallIdentity binder)]
      | binder <- ctorForallBinderInfo ctorInfo
      ]

-- A constructor occurrence may be checked against an owner parameter that is
-- not otherwise lexical at the use site (for example the eta-expanded view of
-- @Some@ in an isolated elaboration test).  The constructor metadata owns that
-- binder identity.  Lexical identities remain authoritative when present; the
-- constructor carrier only completes names that the lexical scope does not
-- own.
constructorExpectedTypeView :: ElaborateScope -> ConstructorInfo -> SrcType -> TypeView
constructorExpectedTypeView scope ctorInfo ty =
  requireTypeViewFromSourceTypeInScope
    scope
    Map.empty
    ( sourceTypeBinderIdentitiesInScope scope ty
        `Map.union` constructorBinderAliases scope ctorInfo
    )
    ty

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

deferCaseCall :: ElaborateScope -> DataInfo -> TypeView -> TypeView -> ElaborateM ResolvedSurfaceExpr
deferCaseCall scope dataInfo scrutineeView resultView = do
  state <- get
  bindingIdentity <-
    case elaborateBindingIdentity state of
      Just identity -> pure identity
      Nothing ->
        throwError
          (ProgramPipelineError "deferred case construction requires a binding identity")
  placeholder <- freshDeferredCaseName (dataInfoIdentityHeadName dataInfo)
  ref <- freshElaborateDeferredRef placeholder
  let loweredResultView = lowerTypeViewWithIdentities scope resultView
      loweredScrutineeView = lowerTypeViewWithIdentities scope scrutineeView
      specializedConstructors =
        [ constructorInfoWithArgViews
            ctorInfo
            (specializeConstructorArgViewsForScrutineeView scope scrutineeView ctorInfo)
        | ctorInfo <- dataConstructors dataInfo
        ]
      handlerViews =
        [ handlerTypeViewFromViews scope specializedCtor loweredResultView
        | specializedCtor <- specializedConstructors
        ]
      placeholderView =
        foldr typeViewArrow loweredResultView (loweredScrutineeView : handlerViews)
      deferred =
        DeferredCaseCall
          { deferredCaseBindingIdentity = bindingIdentity,
            deferredCaseRef = ref,
            deferredCaseDataInfo = dataInfo,
            deferredCaseScrutineeTypeView = scrutineeView,
            deferredCaseResultTypeView = resultView,
            deferredCaseExpectedArgCount = 1 + length handlerViews
          }
  registerDeferredObligation placeholderView (DeferredCase deferred)
  pure (S.EResolvedVar (DeferredId ref) placeholder)

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

deferredPlaceholderView :: SrcType -> Map String SymbolIdentity -> Map String TypeBinderIdentity -> Either ProgramError TypeView
deferredPlaceholderView placeholderTy headIdentities binderIdentities =
  case typeViewFromSourceType headIdentities binderIdentities placeholderTy of
    Right view -> Right view
    Left err ->
      Left
        ( ProgramPipelineError
            ("identity-complete deferred placeholder construction failed: " ++ show err)
        )

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

placeholderResolvedMethodTypeView :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> Maybe TypeView -> TypeView
placeholderResolvedMethodTypeView scope methodInfo args mbExpectedResultView =
  let quantifiedMethodView =
        fst (quantifyMethodLocalVarsInfoView Set.empty [] (methodTypeView methodInfo))
      knownClassArgViews = knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResultView
   in case knownClassArgViews of
        Just classArgViews ->
          let specializedView = stripVacuousTypeViewForalls (specializeMethodTypeView methodInfo classArgViews)
              callSubst =
                resolvedMethodCallSubstWithExpectedResult
                  scope
                  methodInfo
                  classArgViews
                  args
                  mbExpectedResultView
              occurrenceView =
                stripVacuousTypeViewForalls
                  (specializeQuantifiedTypeView callSubst specializedView)
              externallyOwnedIdentities =
                freeTypeBinderIdentitiesTypeViews classArgViews
                  `Set.union` foldMap freeTypeBinderIdentitiesTypeView (Map.elems callSubst)
           in fst
                ( quantifyMethodLocalVarsInfoView
                    externallyOwnedIdentities
                    []
                    occurrenceView
                )
        Nothing -> quantifiedMethodView
knownMethodClassArgs :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType -> Maybe (NonEmpty SrcType)
knownMethodClassArgs scope methodInfo args mbExpectedResult =
  typeViewsIdentity <$> knownMethodClassArgViews scope methodInfo args mbExpectedResult

knownMethodClassArgViews :: ElaborateScope -> MethodInfo -> [P.Expr] -> Maybe SrcType -> Maybe (NonEmpty TypeView)
knownMethodClassArgViews scope methodInfo args mbExpectedResult =
  knownMethodClassArgViewsFromArgViews scope methodInfo argViews
    <|> knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews (sourceTypeViewInScope scope <$> mbExpectedResult)
  where
    argViews = map (fmap (sourceTypeViewInScope scope) . inferKnownExprType scope) args

knownResolvedMethodClassArgViews :: ElaborateScope -> MethodInfo -> [P.ResolvedExpr] -> Maybe TypeView -> Maybe (NonEmpty TypeView)
knownResolvedMethodClassArgViews scope methodInfo args mbExpectedResultView =
  result
  where
    argViews = map (inferKnownResolvedExprTypeView scope) args
    fromArgViews = knownMethodClassArgViewsFromArgViews scope methodInfo argViews
    fromExpectedView = knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews mbExpectedResultView
    result =
      case mbExpectedResultView of
        Just _ -> fromExpectedView
        Nothing -> fromArgViews

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

knownMethodClassArgViewsFromExpectedViews :: ElaborateScope -> MethodInfo -> [Maybe TypeView] -> Maybe TypeView -> Maybe (NonEmpty TypeView)
knownMethodClassArgViewsFromExpectedViews _ _ _ Nothing = Nothing
knownMethodClassArgViewsFromExpectedViews scope methodInfo argViews (Just expectedView) =
  result
  where
    methodView = methodTypeView methodInfo
    knownPairs =
      [ (templateView, actualView)
        | (templateView, mbActualView) <- zip (methodParamTypeViews methodView) argViews,
          Just actualView <- [mbActualView]
      ]
    substFromArgs = foldM (\acc (templateView, actualView) -> matchMethodTypeView scope acc templateView actualView) Map.empty knownPairs
    substAfterResult = substFromArgs >>= \subst -> matchMethodTypeView scope subst (methodResultTypeView methodInfo) expectedView
    result = substAfterResult >>= lookupMethodClassArgViews scope methodInfo

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
  matchTypeViewAgainstIdentity
    scope
    subst
    (typeViewWithScopeAliases scope template)
    (typeViewWithScopeAliases scope actual)

lookupMethodClassArgViews :: ElaborateScope -> MethodInfo -> TypeViewSubst -> Maybe (NonEmpty TypeView)
lookupMethodClassArgViews scope methodInfo subst = do
  closedSubst <-
    case lookupSymbolIdentityExact (methodInfoOwnerClassSymbolIdentity methodInfo) (esClassesByIdentity scope) of
      Just classInfo -> closeFunctionalDependencies scope classInfo subst
      Nothing -> Just subst
  lookupMethodParamViewSubst methodInfo closedSubst

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
        ( instanceCandidates
            ++ localEvidenceCandidates
        )
      where
        instanceCandidates =
          candidatesMatching
            matchDeterminers
            [ instanceHeadTypeViews info
            | info <- esInstances scope
            , sameSymbolIdentity (instanceInfoClassSymbolIdentity info) (classInfoSymbolIdentity classInfo)
            ]
        localEvidenceCandidates =
          candidatesMatching
            matchRigidEvidenceDeterminers
            [ evidenceTypeViews evidence
            | evidence <- esEvidence scope
            , sameSymbolIdentity (evidenceClassSymbol evidence) (classInfoSymbolIdentity classInfo)
            ]
        candidatesMatching matcher headViews =
          [ candidate
          | headView <- headViews
          , Just matchSubst <- [matcher determiners subst headView]
          , Just determinedViews <- [projectClassHeadViews determined headView]
          , let candidate = fmap (applyTypeViewSubst matchSubst) determinedViews
          , typeViewsClosedByTypeBinderIdentities candidate
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

    matchRigidEvidenceDeterminers determiners subst headViews = do
      determinerViews <- projectClassHeadViews determiners headViews
      actualViews <- traverse (`lookupClassParamView` subst) determiners
      if rigidEvidenceTypeViewsMatch scope determinerViews actualViews
        then Just Map.empty
        else Nothing

    mergeDeterminedSubst subst refs views = do
      foldM mergeOne (subst, False) (zip (NE.toList refs) (NE.toList views))

    mergeOne (subst, changed) (identity, view) = do
      _ <- classParamNameForIdentity identity
      let key = identity
      case lookupTypeViewSubst key subst of
        Just existing
          | semanticTypeViewEqual existing view -> Just (subst, changed)
          | otherwise -> Nothing
        Nothing -> Just (insertTypeViewSubst key view subst, True)

    sameDeterminedCandidate left right =
      length left == length right
        && and
          [ semanticTypeViewEqual leftView rightView
            | (leftView, rightView) <- zip (NE.toList left) (NE.toList right)
          ]

    deduplicateDeterminedCandidates [] = []
    deduplicateDeterminedCandidates (candidate : rest) =
      candidate : deduplicateDeterminedCandidates (filter (not . sameDeterminedCandidate candidate) rest)

    classParamNameForIdentity identity =
      Map.lookup identity classParamNamesByIdentity

    lookupClassParamView identity subst = do
      _ <- classParamNameForIdentity identity
      lookupTypeViewSubst (identity) subst

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
      Set.null (freeTypeBinderIdentitiesTypeViews views)

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

inferKnownResolvedExprTypeView :: ElaborateScope -> P.ResolvedExpr -> Maybe TypeView
inferKnownResolvedExprTypeView scope expr =
  case expr of
    ELit lit ->
      Just (sourceTypeViewInScope scope (litSrcType lit))
    EAnn _ annTy ->
      either (const Nothing) Just (resolvedTypeViewForScope scope annTy)
    EVar ref ->
      case runElaborateLookup (lookupResolvedValueInfo scope ref) of
        Right valueInfo@OrdinaryValue {} ->
          Just
            ( typeViewWithScopeAliases
                scope
                (ordinaryValueTypeView valueInfo)
            )
        Right ConstructorValue {valueCtorInfo = ctorInfo} ->
          Just (constructorVisibleTypeView scope ctorInfo)
        _ -> Nothing
    ELam param body -> do
      paramView <- P.paramType param >>= either (const Nothing) Just . resolvedTypeViewForScope scope
      let paramRef = P.paramName param
          scope' = extendResolvedLocalTypeViewPure scope paramRef (localRefName paramRef) paramView
      typeViewArrow paramView <$> inferKnownResolvedExprTypeView scope' body
    ELet localRef mbTy rhs body ->
      case mbTy >>= either (const Nothing) Just . resolvedTypeViewForScope scope of
        Just bindingView ->
          inferKnownResolvedExprTypeView
            (extendResolvedLocalTypeViewPure scope localRef (localRefName localRef) bindingView)
            body
        _ ->
          case inferKnownResolvedExprTypeView scope rhs of
            Just bindingView ->
              inferKnownResolvedExprTypeView
                (extendResolvedLocalTypeViewPure scope localRef (localRefName localRef) bindingView)
                body
            Nothing
              | localRef `notElem` collectFreeResolvedValues Set.empty body ->
                  inferKnownResolvedExprTypeView scope body
            Nothing -> Nothing
    EApp _ _ ->
      case collectResolvedApps expr of
        (EVar ref, args)
          | Right ConstructorValue {valueCtorInfo = ctorInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref),
            length args == length (ctorArgs ctorInfo) ->
              knownResolvedConstructorResultTypeView scope ctorInfo args
          | Right valueInfo@OrdinaryValue {} <- runElaborateLookup (lookupResolvedValueInfo scope ref) -> do
              let valueView =
                    typeViewWithScopeAliases
                      scope
                      (preferVisibleTypeView scope (ordinaryValueTypeView valueInfo))
              subst <- inferResolvedCallSubst scope valueView args
              let specializedView = specializeQuantifiedTypeView subst valueView
              if length args <= length (methodParamTypeViews specializedView)
                then Just (typeViewArrowResultViewForArity specializedView (length args))
                else Nothing
          | Right OverloadedMethod {valueMethodInfo = methodInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref) -> do
              classArgViews <- knownResolvedMethodClassArgViews scope methodInfo args Nothing
              let specializedView = specializeMethodTypeView methodInfo classArgViews
              subst <- inferResolvedMethodCallSubstWithViews scope methodInfo classArgViews args
              let appliedView = specializeQuantifiedTypeView subst specializedView
              if length args <= length (methodParamTypeViews appliedView)
                then Just (typeViewArrowResultViewForArity appliedView (length args))
                else Nothing
        (headExpr, args) -> do
          headView <- inferKnownResolvedExprTypeView scope headExpr
          if length args <= length (methodParamTypeViews headView)
            then Just (typeViewArrowResultViewForArity headView (length args))
            else Nothing
    ECase _ _ -> Nothing

inferKnownResolvedExprTypeViewWithExpected :: ElaborateScope -> TypeView -> P.ResolvedExpr -> Maybe TypeView
inferKnownResolvedExprTypeViewWithExpected scope expectedView expr =
  case collectResolvedApps expr of
    (EVar ref, args)
      | Right ConstructorValue {valueCtorInfo = ctorInfo} <- runElaborateLookup (lookupResolvedValueInfo scope ref)
      , length args == length (ctorArgs ctorInfo) ->
          let (subst, _) = constructorResolvedArgPlan scope ctorInfo (Just expectedView) args
           in Just (applyTypeViewSubst subst (constructorVisibleResultTypeView scope ctorInfo))
    _ -> inferKnownResolvedExprTypeView scope expr

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
constructorVisibleType scope =
  typeViewDisplay . constructorVisibleTypeView scope

constructorVisibleTypeView :: ElaborateScope -> ConstructorInfo -> TypeView
constructorVisibleTypeView scope ctorInfo =
  case resolveConstructorDataInfo scope ctorInfo of
    Nothing -> view
    Just info ->
      mapTypeViewDisplayHeadNames
        ( \identity displayName ->
            if sameSymbolIdentity identity (ctorOwningTypeIdentity ctorInfo)
              then visibleDataHeadName scope info
              else displayName
        )
        view
  where
    view = constructorTypeView scope ctorInfo

visibleDataHeadName :: ElaborateScope -> DataInfo -> String
visibleDataHeadName scope info =
  case visibleDataHeadType scope info of
    STBase name -> name
    STCon name _ -> name
    _ -> dataInfoIdentityName info

peelAppliedType :: SrcType -> Int -> Maybe SrcType
peelAppliedType ty argCount =
  let (_, bodyTy) = splitForalls ty
      (argTys, resultTy) = splitArrows bodyTy
   in if argCount > length argTys
        then Nothing
        else Just (foldr STArrow resultTy (drop argCount argTys))

compileCase :: ElaborateScope -> Maybe SrcType -> P.Expr -> [P.Alt] -> ElaborateM ResolvedSurfaceExpr
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
              Just annTy -> S.EAnn scrutineeExpr0 (lowerType scope annTy)
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
      resultView <-
        case mbExpected of
          Just expectedTy -> pure (sourceTypeViewInScope scope expectedTy)
          Nothing -> freshTypeVarView
      let scrutineeView = sourceTypeViewInScope scope scrutineeTy
      case localIdentityScrutinee scrutinee of
        Just inner -> compileCase scope mbExpected inner alts
        Nothing -> do
          scrutineeExpr <- compileExpr scope (Just scrutineeTy) scrutinee
          let forceAnnotateHandlers =
                isJust mbExpected
                  || any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileHandler scope scrutineeExpr scrutineeTy resultView dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholderSurface <- deferCaseCall scope dataInfo scrutineeView resultView
          pure (foldl S.EApp placeholderSurface (scrutineeExpr : handlers))

compileResolvedCaseWithExpectedView :: ElaborateScope -> Maybe TypeView -> P.ResolvedExpr -> [P.ResolvedAlt] -> ElaborateM ResolvedSurfaceExpr
compileResolvedCaseWithExpectedView scope mbExpectedView scrutinee alts = do
  case resolvedCtorOwners alts of
    [] -> do
      let mbInferredScrutineeView = inferKnownResolvedExprTypeView scope scrutinee
      mbAnnotationScrutineeView <- catchAllResolvedPatternAnnotationView scope alts
      let mbScrutineeView =
            case mbInferredScrutineeView of
              Just knownView -> Just knownView
              Nothing -> mbAnnotationScrutineeView
          annotateScrutinee =
            case (mbInferredScrutineeView, mbAnnotationScrutineeView) of
              (Nothing, Just annView) -> Just annView
              _ -> Nothing
      rejectOpaqueBuiltinCase scope (typeViewDisplay <$> mbScrutineeView)
      mapM_ (\scrutineeView -> mapM_ (validateResolvedPatternType scope scrutineeView . P.altPattern) alts) mbScrutineeView
      scrutineeExpr0 <- compileResolvedExprWithExpectedView scope mbScrutineeView scrutinee
      let scrutineeExpr =
            case annotateScrutinee of
              Just annView -> S.EAnn scrutineeExpr0 (loweredTypeViewIdentity scope annView)
              Nothing -> scrutineeExpr0
      compileResolvedCatchAllOnly scope mbExpectedView mbScrutineeView scrutineeExpr alts
    owners -> do
      dataInfo <- requireSingleResolvedDataOwner scope owners
      let headTy = dataHeadType dataInfo
          scrutineeView =
            case inferKnownResolvedExprTypeView scope scrutinee of
              Just knownView -> knownView
              Nothing -> sourceTypeViewInScope scope headTy
      validateResolvedOrderedPatterns scope alts
      mapM_ (validateResolvedPatternType scope scrutineeView . P.altPattern) alts
      resultView <-
        case mbExpectedView of
          Just expectedView -> pure expectedView
          Nothing -> freshTypeVarView
      case localResolvedIdentityScrutinee scrutinee of
        Just inner -> compileResolvedCaseWithExpectedView scope mbExpectedView inner alts
        Nothing -> do
          scrutineeExpr <- compileResolvedExprWithExpectedView scope (Just scrutineeView) scrutinee
          let forceAnnotateHandlers =
                isJust mbExpectedView
                  || any (not . null . ctorForalls) (dataConstructors dataInfo)
          handlers <- mapM (compileResolvedHandler scope scrutineeExpr scrutineeView resultView dataInfo alts forceAnnotateHandlers) (dataConstructors dataInfo)
          placeholderSurface <- deferCaseCall scope dataInfo scrutineeView resultView
          pure (foldl S.EApp placeholderSurface (scrutineeExpr : handlers))

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

compileCatchAllOnly :: ElaborateScope -> Maybe SrcType -> Maybe SrcType -> ResolvedSurfaceExpr -> [P.Alt] -> ElaborateM ResolvedSurfaceExpr
compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileExpr scope mbExpected body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      scrutineeRef <- freshElaborateLocalRef scrutineeName
      case mbScrutineeTy of
        Just _ ->
          pure (resolvedLocalLetSurface scrutineeName scrutineeRef scrutineeExpr bodyExpr)
        Nothing -> do
          -- Keep the scrutinee binding referenced so eMLF infers its own scheme
          -- while the strict let still preserves evaluation before the body.
          forceName <- freshRuntimeName "case_scrutinee_force"
          forceRef <- freshElaborateLocalRef forceName
          pure
            ( resolvedLocalLetSurface
                scrutineeName
                scrutineeRef
                scrutineeExpr
                ( resolvedLocalLetSurface
                    forceName
                    forceRef
                    (S.EResolvedVar (LocalId scrutineeRef) scrutineeName)
                    bodyExpr
                )
            )
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName name
      localRef <- freshElaborateLocalRef name
      scope' <-
        case mbScrutineeTy of
          Just scrutineeTy -> extendLocalWithRef scope localRef name runtimeName (Just scrutineeTy)
          Nothing -> extendLocalLoweredWithRef scope localRef name runtimeName =<< freshTypeName
      bodyExpr <- compileExpr scope' mbExpected body
      pure (resolvedLocalLetSurface runtimeName localRef scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileCatchAllOnly scope mbExpected mbScrutineeTy scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileResolvedCatchAllOnly :: ElaborateScope -> Maybe TypeView -> Maybe TypeView -> ResolvedSurfaceExpr -> [P.ResolvedAlt] -> ElaborateM ResolvedSurfaceExpr
compileResolvedCatchAllOnly scope mbExpectedView mbScrutineeView scrutineeExpr alts =
  case alts of
    [P.Alt P.PatWildcard body] -> do
      bodyExpr <- compileResolvedExprWithExpectedView scope mbExpectedView body
      scrutineeName <- freshRuntimeName "case_scrutinee"
      scrutineeRef <- freshElaborateLocalRef scrutineeName
      case mbScrutineeView of
        Just _ ->
          pure
            (resolvedLocalLetSurface scrutineeName scrutineeRef scrutineeExpr bodyExpr)
        Nothing -> do
          forceName <- freshRuntimeName "case_scrutinee_force"
          forceRef <- freshElaborateLocalRef forceName
          pure
            ( resolvedLocalLetSurface
                scrutineeName
                scrutineeRef
                scrutineeExpr
                ( resolvedLocalLetSurface
                    forceName
                    forceRef
                    (S.EResolvedVar (LocalId scrutineeRef) scrutineeName)
                    bodyExpr
                )
            )
    [P.Alt (P.PatVar name) body] -> do
      runtimeName <- freshRuntimeName (localRefName name)
      scope' <-
        case mbScrutineeView of
          Just scrutineeView -> extendResolvedLocalView scope name runtimeName (Just scrutineeView)
          Nothing -> extendResolvedLocalView scope name runtimeName Nothing
      bodyExpr <- compileResolvedExprWithExpectedView scope' mbExpectedView body
      pure (resolvedLocalLetSurface runtimeName name scrutineeExpr bodyExpr)
    [P.Alt (P.PatAnn inner _) body] -> compileResolvedCatchAllOnly scope mbExpectedView mbScrutineeView scrutineeExpr [P.Alt inner body]
    _ -> throwError (ProgramCaseOnNonDataType STBottom)

compileHandler :: ElaborateScope -> ResolvedSurfaceExpr -> SrcType -> TypeView -> DataInfo -> [P.Alt] -> Bool -> ConstructorInfo -> ElaborateM ResolvedSurfaceExpr
compileHandler scope scrutineeExpr scrutineeTy resultView dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgTys = specializeConstructorArgsForScrutinee scrutineeTy ctorInfo
      specializedCtorInfo = constructorInfoWithArgs scope ctorInfo ctorArgTys
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgTys]]
  runtimeRefs <- mapM freshElaborateLocalRef runtimeNames
  let topArgs = zip4 (map (const P.PatWildcard) ctorArgTys) runtimeNames runtimeRefs ctorArgTys
      candidates = matchingCandidates ctorInfo
  bodyExpr0 <- compileCandidates topArgs candidates
  let bodyExpr =
        if forceAnnotateHandlers
          then
            compilerExactTypeViewAnnotation
              (lowerTypeViewWithIdentities scope resultView)
              bodyExpr0
          else bodyExpr0
  let handlerBody =
        wrapResolvedCaseHandlerLambdaChain
          [ (name, ref, lowerType scope argTy)
          | (name, ref, argTy) <- zip3 runtimeNames runtimeRefs ctorArgTys
          ]
          bodyExpr
      handlerView =
        handlerTypeViewFromViews
          scope
          specializedCtorInfo
          (lowerTypeViewWithIdentities scope resultView)
  pure (compilerExactTypeViewAnnotation handlerView handlerBody)
  where
    resultTy = typeViewDisplay resultView

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
          scrutineeRef <- freshElaborateLocalRef name
          scope' <- extendLocalLoweredWithRef scope scrutineeRef name scrutineeName (lowerType scope scrutineeTy)
          bodyExpr <- compileExpr scope' (Just resultTy) body
          pure (resolvedLocalLetSurface scrutineeName scrutineeRef scrutineeExpr bodyExpr)
        P.PatCtor ctorName0 patterns
          | constructorNameMatches scope ctorName0 ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then compilePatternSequence scope (zip4 patterns (map second topArgs) (map third topArgs) (map fourth topArgs)) body mbFallback
                else throwError (ProgramPatternConstructorMismatch ctorName0 (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    second (_, value, _, _) = value
    third (_, _, value, _) = value
    fourth (_, _, _, value) = value

    compilePatternSequence scope0 [] body _ =
      compileExpr scope0 (Just resultTy) body
    compilePatternSequence scope0 ((pattern0, runtimeName, runtimeRef, argTy) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          scope' <- extendLocalWithRef scope0 runtimeRef sourceName runtimeName (Just argTy)
          compilePatternSequence scope' rest body mbFallback
        P.PatCtor nestedCtorName nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfo scope nestedCtorName
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch nestedCtorName argTy)
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              nestedRuntimeRefs <- mapM freshElaborateLocalRef nestedRuntimeNames
              let forceNestedAnnotations =
                    forceAnnotateHandlers
                      || any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgTys = specializeConstructorArgsForScrutinee argTy nestedCtorInfo
              matchingBody <- compilePatternSequence scope0 (zip4 nestedPatterns nestedRuntimeNames nestedRuntimeRefs nestedArgTys ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <- mapM (nestedHandler forceNestedAnnotations argTy nestedCtorInfo (zip nestedRuntimeNames nestedRuntimeRefs) matchingBody fallback) (dataConstructors nestedDataInfo)
              placeholderSurface <-
                deferCaseCall
                  scope0
                  nestedDataInfo
                  (sourceTypeViewInScope scope0 argTy)
                  resultView
              pure
                ( foldl
                    S.EApp
                    placeholderSurface
                    (S.EResolvedVar (LocalId runtimeRef) runtimeName : handlers)
                )
        P.PatAnn inner annTy -> compilePatternSequence scope0 ((inner, runtimeName, runtimeRef, annTy) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeTy targetCtor nestedRuntimeBinders matchingBody mbFallback ctor = do
      let ctorArgTys = specializeConstructorArgsForScrutinee nestedScrutineeTy ctor
          specializedCtor = constructorInfoWithArgs scope ctor ctorArgTys
          targetSelected = sameConstructorInfo ctor targetCtor
          argNames =
            if targetSelected
              then map fst nestedRuntimeBinders
              else ["unused" ++ show ix | ix <- [1 .. length ctorArgTys]]
      argRefs <-
        if targetSelected
          then pure (map snd nestedRuntimeBinders)
          else mapM freshElaborateLocalRef argNames
      let selectedBody0 =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          selectedBody =
            if forceNestedAnnotations
              then
                compilerExactTypeViewAnnotation
                  (lowerTypeViewWithIdentities scope resultView)
                  selectedBody0
              else selectedBody0
          handlerBody =
            wrapResolvedCaseHandlerLambdaChain
              [ (name, ref, lowerType scope argTy)
              | (name, ref, argTy) <- zip3 argNames argRefs ctorArgTys
              ]
              selectedBody
          handlerView =
            handlerTypeViewFromViews
              scope
              specializedCtor
              (lowerTypeViewWithIdentities scope resultView)
      pure (compilerExactTypeViewAnnotation handlerView handlerBody)

    specializeConstructorArgsForScrutinee =
      specializeConstructorArgsForScrutineeType scope

compileResolvedHandler :: ElaborateScope -> ResolvedSurfaceExpr -> TypeView -> TypeView -> DataInfo -> [P.ResolvedAlt] -> Bool -> ConstructorInfo -> ElaborateM ResolvedSurfaceExpr
compileResolvedHandler scope scrutineeExpr scrutineeView resultView dataInfo alts forceAnnotateHandlers ctorInfo = do
  let ctorArgViews = specializeConstructorArgViewsForScrutineeView scope scrutineeView ctorInfo
      specializedCtorInfo = constructorInfoWithArgViews ctorInfo ctorArgViews
  runtimeNames <- mapM freshRuntimeName ["case" ++ show ix | ix <- [1 .. length ctorArgViews]]
  handlerRefs <- mapM freshElaborateLocalRef runtimeNames
  let topArgs =
        [ (P.PatWildcard, runtimeName, handlerRef, argView)
        | (runtimeName, handlerRef, argView) <- zip3 runtimeNames handlerRefs ctorArgViews
        ]
      candidates = matchingCandidates ctorInfo
  bodyExpr0 <- compileCandidates topArgs candidates
  let bodyExpr =
        if forceAnnotateHandlers
          then
            compilerExactTypeViewAnnotation
              (lowerTypeViewWithIdentities scope resultView)
              bodyExpr0
          else bodyExpr0
  let handlerBody =
        wrapResolvedCaseHandlerLambdaChain
          [ (name, handlerRef, loweredTypeViewIdentity scope argView)
          | (name, handlerRef, argView) <- zip3 runtimeNames handlerRefs ctorArgViews
          ]
          bodyExpr
      handlerView =
        handlerTypeViewFromViews
          scope
          specializedCtorInfo
          (lowerTypeViewWithIdentities scope resultView)
  pure (compilerExactTypeViewAnnotation handlerView handlerBody)
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
        P.PatWildcard -> compileResolvedExprWithExpectedView scope (Just resultView) body
        P.PatVar name -> do
          scrutineeName <- freshRuntimeName (localRefName name)
          scope' <- extendResolvedLocalView scope name scrutineeName (Just scrutineeView)
          bodyExpr <- compileResolvedExprWithExpectedView scope' (Just resultView) body
          pure (resolvedLocalLetSurface scrutineeName name scrutineeExpr bodyExpr)
        P.PatCtor ctorSymbol patterns
          | constructorSymbolMatches scope ctorSymbol ctorInfo ->
              if length patterns == length (ctorArgs ctorInfo)
                then
                  compilePatternSequence
                    scope
                    [ (nestedPattern, runtimeName, runtimeRef, argView)
                    | (nestedPattern, (_, runtimeName, runtimeRef, argView)) <- zip patterns topArgs
                    ]
                    body
                    mbFallback
                else throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) (dataHeadType dataInfo))
          | otherwise ->
              case mbFallback of
                Just fallback -> pure fallback
                Nothing -> throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
        P.PatAnn inner _ -> compileAltCandidate topArgs (P.Alt inner body) mbFallback

    compilePatternSequence scope0 [] body _ =
      compileResolvedExprWithExpectedView scope0 (Just resultView) body
    compilePatternSequence scope0 ((pattern0, runtimeName, runtimeRef, argView) : rest) body mbFallback =
      case pattern0 of
        P.PatWildcard -> compilePatternSequence scope0 rest body mbFallback
        P.PatVar sourceName -> do
          patternRuntimeName <- freshRuntimeName (localRefName sourceName)
          scope' <- extendResolvedLocalView scope0 sourceName patternRuntimeName (Just argView)
          bodyExpr <- compilePatternSequence scope' rest body mbFallback
          pure $
            resolvedLocalLetSurface
              patternRuntimeName
              sourceName
              (S.EResolvedVar (LocalId runtimeRef) runtimeName)
              bodyExpr
        P.PatCtor nestedCtorSymbol nestedPatterns -> do
          nestedCtorInfo <- lookupConstructorInfoBySymbol scope nestedCtorSymbol
          nestedDataInfo <- lookupDataInfoForConstructor scope nestedCtorInfo
          if length nestedPatterns /= length (ctorArgs nestedCtorInfo)
            then throwError (ProgramPatternConstructorMismatch (P.refDisplayName nestedCtorSymbol) (typeViewDisplay argView))
            else do
              nestedRuntimeNames <- mapM freshRuntimeName ["pat" ++ show ix | ix <- [1 .. length (ctorArgs nestedCtorInfo)]]
              nestedRuntimeRefs <- mapM freshElaborateLocalRef nestedRuntimeNames
              let forceNestedAnnotations =
                    forceAnnotateHandlers
                      || any (not . null . ctorForalls) (dataConstructors nestedDataInfo)
                  nestedArgViews = specializeConstructorArgViewsForScrutineeView scope argView nestedCtorInfo
                  nestedArgs =
                    [ (nestedPattern, nestedRuntimeName, nestedRuntimeRef, nestedArgView)
                    | (nestedPattern, nestedRuntimeName, nestedRuntimeRef, nestedArgView) <-
                        zip4 nestedPatterns nestedRuntimeNames nestedRuntimeRefs nestedArgViews
                    ]
              matchingBody <- compilePatternSequence scope0 (nestedArgs ++ rest) body mbFallback
              fallback <-
                case mbFallback of
                  Just fallback0 -> pure (Just fallback0)
                  Nothing
                    | nestedPatternNeedsFallback nestedDataInfo nestedCtorInfo ->
                        throwError (ProgramNonExhaustiveCase [ctorName ctorInfo])
                  Nothing -> pure Nothing
              handlers <-
                mapM
                  ( nestedHandler
                      forceNestedAnnotations
                      argView
                      nestedCtorInfo
                      (zip nestedRuntimeNames nestedRuntimeRefs)
                      matchingBody
                      fallback
                  )
                  (dataConstructors nestedDataInfo)
              placeholderSurface <- deferCaseCall scope0 nestedDataInfo argView resultView
              pure
                ( foldl
                    S.EApp
                    placeholderSurface
                    (S.EResolvedVar (LocalId runtimeRef) runtimeName : handlers)
                )
        P.PatAnn inner annTy -> do
          annView <- liftEitherElab (resolvedTypeViewForScope scope annTy)
          ensureTypeViewCompatible scope argView annView
          compilePatternSequence scope0 ((inner, runtimeName, runtimeRef, annView) : rest) body mbFallback

    nestedPatternNeedsFallback nestedDataInfo targetCtor =
      any (not . sameConstructorInfo targetCtor) (dataConstructors nestedDataInfo)

    nestedHandler forceNestedAnnotations nestedScrutineeView targetCtor nestedRuntimeBindings matchingBody mbFallback ctor =
      let ctorArgViews = specializeConstructorArgViewsForScrutineeView scope nestedScrutineeView ctor
          specializedCtor = constructorInfoWithArgViews ctor ctorArgViews
          targetSelected = sameConstructorInfo ctor targetCtor
          selectedBody0 =
            case (targetSelected, mbFallback) of
              (True, _) -> matchingBody
              (False, Just fallback) -> fallback
              (False, Nothing) -> matchingBody
          selectedBody =
            if forceNestedAnnotations
              then
                compilerExactTypeViewAnnotation
                  (lowerTypeViewWithIdentities scope resultView)
                  selectedBody0
              else selectedBody0
       in do
            handlerBindings <-
              if targetSelected
                then pure nestedRuntimeBindings
                else do
                  let argNames = ["unused" ++ show ix | ix <- [1 .. length ctorArgViews]]
                  handlerRefs <- mapM freshElaborateLocalRef argNames
                  pure (zip argNames handlerRefs)
            let handlerBody =
                  wrapResolvedCaseHandlerLambdaChain
                    [ (name, handlerRef, loweredTypeViewIdentity scope argView)
                    | ((name, handlerRef), argView) <- zip handlerBindings ctorArgViews
                    ]
                    selectedBody
                handlerView =
                  handlerTypeViewFromViews
                    scope
                    specializedCtor
                    (lowerTypeViewWithIdentities scope resultView)
            pure (compilerExactTypeViewAnnotation handlerView handlerBody)

-- | Compiler-generated case handlers have an identity-complete producer type:
-- their argument identities come from the selected constructor and their
-- result identity comes from the enclosing case.  Preserve that authority as
-- an exact annotation.  Lowering it through source kappa-sigma would create a
-- fresh flexible codomain and make the generated handler polymorphic in a new
-- result binder, even though the enclosing case already owns that identity.
compilerExactTypeViewAnnotation :: TypeView -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
compilerExactTypeViewAnnotation view expr =
  S.EExactAnn expr (typeViewIdentity view) (typeViewToResolved view)

-- | A resolved local declaration has two distinct boundaries.  Keep the
-- source ascription as ordinary kappa semantics, then publish the resolver's
-- identity-bearing local scheme as compiler-exact construction authority for
-- the enclosing let.  Without the outer boundary, kappa's flexible codomain
-- can introduce a vacuous producer slot; the local occurrence then receives
-- an InstElim even though its declared scheme is monomorphic.
resolvedLocalBindingSchemeAnnotation :: ElaborateScope -> TypeView -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
resolvedLocalBindingSchemeAnnotation scope view expr =
  compilerExactTypeViewAnnotation
    loweredView
    (S.EAnn expr (typeViewIdentity loweredView))
  where
    loweredView = lowerTypeViewWithIdentities scope view

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

catchAllResolvedPatternAnnotationView :: ElaborateScope -> [P.ResolvedAlt] -> ElaborateM (Maybe TypeView)
catchAllResolvedPatternAnnotationView scope alts =
  case alts of
    [P.Alt pattern0 _] -> resolvedPatternAnnotationView scope pattern0
    _ -> pure Nothing

patternAnnotationType :: P.Pattern -> Maybe SrcType
patternAnnotationType pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      case patternAnnotationType inner of
        Just innerTy -> Just innerTy
        Nothing -> Just annTy
    _ -> Nothing

resolvedPatternAnnotationView :: ElaborateScope -> P.ResolvedPattern -> ElaborateM (Maybe TypeView)
resolvedPatternAnnotationView scope pattern0 =
  case pattern0 of
    P.PatAnn inner annTy ->
      resolvedPatternAnnotationView scope inner >>= \case
        Just innerView -> pure (Just innerView)
        Nothing -> Just <$> liftEitherElab (resolvedTypeViewForScope scope annTy)
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

validateResolvedPatternType :: ElaborateScope -> TypeView -> P.ResolvedPattern -> ElaborateM ()
validateResolvedPatternType scope expectedView pattern0 =
  case pattern0 of
    P.PatWildcard -> pure ()
    P.PatVar {} -> pure ()
    P.PatAnn inner annTy -> do
      annView <- liftEitherElab (resolvedTypeViewForScope scope annTy)
      ensureTypeViewCompatible scope expectedView annView
      validateResolvedPatternType scope annView inner
    P.PatCtor ctorSymbol patterns -> do
      ctorInfo <- lookupConstructorInfoBySymbol scope ctorSymbol
      subst <-
        case matchPatternTypeViews (constructorResultTypeView scope ctorInfo) expectedView of
          Just subst0 -> pure subst0
          Nothing -> throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) (typeViewDisplay expectedView))
      if length patterns /= length (ctorArgs ctorInfo)
        then throwError (ProgramPatternConstructorMismatch (P.refDisplayName ctorSymbol) (typeViewDisplay expectedView))
        else
          mapM_
            ( \(nestedPattern, argView) ->
                validateResolvedPatternType scope (applyTypeViewSubst subst argView) nestedPattern
            )
            (zip patterns (constructorArgTypeViews scope ctorInfo))
  where
    matchPatternTypeViews template actual =
      case matchTypeViewAgainstIdentity scope Map.empty template actual of
        Just subst -> Just subst
        Nothing ->
          case matchTypeViewAgainstIdentity scope Map.empty actual template of
            Just subst -> Just subst
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
  case lookupSymbolIdentityExact (ctorOwningTypeIdentity ctorInfo) (esTypesByIdentity scope) of
    Just info
      | constructorBelongsToDataInfo ctorInfo info -> Just info
      | otherwise -> Nothing
    Nothing -> Nothing

sameDataInfo :: DataInfo -> DataInfo -> Bool
sameDataInfo left right =
  sameSymbolIdentity (dataInfoSymbolIdentity left) (dataInfoSymbolIdentity right)

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
  sameSymbolIdentity (ctorInfoSymbol left) (ctorInfoSymbol right)

handlerTypeViewFromViews :: ElaborateScope -> ConstructorInfo -> TypeView -> TypeView
handlerTypeViewFromViews scope ctorInfo resultView =
  stripVacuousTypeViewForalls $
    typeViewRebuildArrowBody
      freshenedConstructorView
      (typeViewArrowArgViews freshenedConstructorView)
      resultView
  where
    freshenedConstructorView =
      freshenConstructorViewForResult scope ctorInfo (typeViewDisplay resultView)

freshenConstructorViewForResult :: ElaborateScope -> ConstructorInfo -> SrcType -> TypeView
freshenConstructorViewForResult scope ctorInfo resultTy =
  mapTypeViewDisplayBinderNames
    (\identity displayName -> Map.findWithDefault displayName identity freshNamesByIdentity)
    loweredConstructorView
  where
    loweredConstructorView =
      lowerTypeViewWithIdentities scope (constructorTypeView scope ctorInfo)
    forallViews = typeViewForallBinderViews loweredConstructorView
    displayForalls =
      [ (name, fmap typeViewDisplay mbBoundView)
      | (name, _, mbBoundView) <- forallViews
      ]
    displayArgs = map typeViewDisplay (typeViewArrowArgViews loweredConstructorView)
    (freshDisplayForalls, _) =
      freshenCtorForallsForResult
        resultTy
        displayForalls
        displayArgs
    freshNamesByIdentity =
      Map.fromList
        [ (identity, freshName)
        | ((_, identity, _), (freshName, _)) <- zip forallViews freshDisplayForalls
        ]

freshenCtorForallsForResult :: SrcType -> [(String, Maybe SrcType)] -> [SrcType] -> ([(String, Maybe SrcType)], [SrcType])
freshenCtorForallsForResult resultTy foralls0 args0 =
  go initialUsed [] args0 foralls0
  where
    initialUsed =
      Set.union
        (freeTypeVarsSrcType resultTy)
        (foldMap (maybe Set.empty freeTypeVarsSrcType . snd) foralls0)

    go _ accForalls currentArgs [] =
      (accForalls, currentArgs)
    go used accForalls currentArgs ((name, mbBound) : rest) =
      go
        (Set.insert name' used)
        (accForalls ++ [(name', mbBound')])
        args'
        rest'
      where
        name'
          | name `Set.member` used =
              freshNameLike name (Set.union used (foldMap freeTypeVarsSrcType currentArgs))
          | otherwise = name

        renameTy
          | name' == name = id
          | otherwise = substSrcType name (STVar name')

        mbBound' = fmap renameTy mbBound
        (rest', shadowedByLaterBinder) =
          renameRemainingForallBounds name renameTy rest

        args'
          | shadowedByLaterBinder = currentArgs
          | otherwise = map renameTy currentArgs

    renameRemainingForallBounds _ _ [] =
      ([], False)
    renameRemainingForallBounds renamedName renameTy ((laterName, laterBound) : rest)
      | laterName == renamedName =
          ((laterName, fmap renameTy laterBound) : rest, True)
      | otherwise =
          let (rest', shadowed) = renameRemainingForallBounds renamedName renameTy rest
           in ((laterName, fmap renameTy laterBound) : rest', shadowed)

dataHeadType :: DataInfo -> SrcType
dataHeadType info =
  dataHeadTypeWithName (dataInfoIdentityName info) info

visibleDataHeadType :: ElaborateScope -> DataInfo -> SrcType
visibleDataHeadType scope info =
  dataHeadTypeWithName visibleName info
  where
    visibleName =
      case lookupSymbolIdentityExact (dataInfoSymbolIdentity info) (esTypeDisplayNamesByIdentity scope) of
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
        Just identity -> sameSymbolIdentity (instanceInfoClassIdentity info) identity
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
        && instanceHeadTypeViews left == instanceHeadTypeViews right
        && instanceConstraintInfos left == instanceConstraintInfos right
        && fmap valueInfoSymbolIdentity (instanceMethodsByIdentity left) == fmap valueInfoSymbolIdentity (instanceMethodsByIdentity right)

    preferredInstanceMatch left@(_, leftSubst, leftDirect) right@(_, rightSubst, rightDirect)
      | rightDirect && not leftDirect = right
      | rightDirect == leftDirect && Map.size rightSubst > Map.size leftSubst = right
      | otherwise = left

matchTypeViewsAgainstIdentity :: ElaborateScope -> TypeViewSubst -> NonEmpty TypeView -> NonEmpty TypeView -> Maybe TypeViewSubst
matchTypeViewsAgainstIdentity =
  matchTypeViewsAgainstIdentityWith False

-- | Match a deferred constructor occurrence after the elaborator has emitted
-- @N@ for an as-yet unknown constructor forall.  That bottom route is a
-- computation placeholder, not evidence that the source binder is literally
-- bottom, so a later value/result occurrence may refine it.  Other callers
-- retain ordinary non-refining substitution semantics.
matchTypeViewsAgainstIdentityRefiningBottom :: ElaborateScope -> TypeViewSubst -> NonEmpty TypeView -> NonEmpty TypeView -> Maybe TypeViewSubst
matchTypeViewsAgainstIdentityRefiningBottom =
  matchTypeViewsAgainstIdentityWith True

matchTypeViewsAgainstIdentityWith :: Bool -> ElaborateScope -> TypeViewSubst -> NonEmpty TypeView -> NonEmpty TypeView -> Maybe TypeViewSubst
matchTypeViewsAgainstIdentityWith refineBottom scope subst templates actuals
  | length templates /= length actuals = Nothing
  | otherwise =
      foldM
        (\acc (template, actual) -> matchTypeViewAgainstIdentityWith refineBottom scope acc template actual)
        subst
        (zip (NE.toList templates) (NE.toList actuals))

matchTypeViewAgainstIdentity :: ElaborateScope -> TypeViewSubst -> TypeView -> TypeView -> Maybe TypeViewSubst
matchTypeViewAgainstIdentity =
  matchTypeViewAgainstIdentityWith False

matchTypeViewAgainstIdentityWith :: Bool -> ElaborateScope -> TypeViewSubst -> TypeView -> TypeView -> Maybe TypeViewSubst
matchTypeViewAgainstIdentityWith refineBottom scope subst template actual =
  case typeViewNodeView template of
    TypeViewVarNode _ key ->
      case lookupTypeViewSubst key subst of
        Nothing
          | typeViewIsBareBinderIdentity key actual -> Just subst
          | typeViewMentionsFreeBinderIdentity key actual -> Nothing
          | otherwise -> Just (insertTypeViewSubst key actual subst)
        Just existing
          | refineBottom,
            TypeViewBottomNode <- typeViewNodeView existing ->
              Just (insertTypeViewSubst key actual subst)
          | refineBottom,
            typeViewIsBareBinderIdentity key actual ->
              Just subst
          | semanticTypeViewEqualInScope scope existing actual -> Just subst
          | otherwise -> Nothing
    TypeViewArrowNode templateDom templateCod ->
      case typeViewNodeView actual of
        TypeViewArrowNode actualDom actualCod -> do
          subst' <- matchTypeViewAgainstIdentityWith refineBottom scope subst templateDom actualDom
          matchTypeViewAgainstIdentityWith refineBottom scope subst' templateCod actualCod
        _ -> Nothing
    TypeViewBaseNode _ expectedIdentity ->
      case typeViewNodeView actual of
        TypeViewBaseNode _ actualIdentity
          | sameSymbolIdentity expectedIdentity actualIdentity -> Just subst
        _ -> Nothing
    TypeViewConNode _ expectedIdentity templateArgs ->
      case typeViewNodeView actual of
        TypeViewConNode _ actualIdentity actualArgs
          | sameSymbolIdentity expectedIdentity actualIdentity,
            NE.length templateArgs == NE.length actualArgs ->
              foldM
                (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentityWith refineBottom scope acc templateTy actualTy)
                subst
                (zip (NE.toList templateArgs) (NE.toList actualArgs))
        _ -> Nothing
    TypeViewVarAppNode _ key args ->
      matchTypeViewHeadApplication refineBottom scope subst key args actual
    TypeViewTyLamNode _ _ templateBody ->
      case typeViewNodeView actual of
        TypeViewTyLamNode _ _ actualBody ->
          matchTypeViewAgainstIdentityWith refineBottom scope subst templateBody actualBody
        _ -> Nothing
    TypeViewTyAppNode templateFun templateArg ->
      case typeViewNodeView actual of
        TypeViewTyAppNode actualFun actualArg -> do
          subst' <- matchTypeViewAgainstIdentityWith refineBottom scope subst templateFun actualFun
          matchTypeViewAgainstIdentityWith refineBottom scope subst' templateArg actualArg
        _ -> Nothing
    TypeViewForallNode _ _ templateBound templateBody ->
      case typeViewNodeView actual of
        TypeViewForallNode _ _ actualBound actualBody -> do
          subst' <-
            case (templateBound, actualBound) of
              (Nothing, _) -> Just subst
              (Just expectedBound, Just foundBound) ->
                matchTypeViewAgainstIdentityWith refineBottom scope subst expectedBound foundBound
              (Just {}, Nothing) -> Nothing
          matchTypeViewAgainstIdentityWith refineBottom scope subst' templateBody actualBody
        _ -> Nothing
    TypeViewMuNode _ _ templateBody ->
      case typeViewNodeView actual of
        TypeViewMuNode _ _ actualBody ->
          matchTypeViewAgainstIdentityWith refineBottom scope subst templateBody actualBody
        _ -> Nothing
    TypeViewBottomNode ->
      case typeViewNodeView actual of
        TypeViewBottomNode -> Just subst
        _ -> Nothing

matchTypeViewHeadApplication ::
  Bool ->
  ElaborateScope ->
  TypeViewSubst ->
  TypeBinderIdentity ->
  NonEmpty TypeView ->
  TypeView ->
  Maybe TypeViewSubst
matchTypeViewHeadApplication refineBottom scope subst key expectedArgs actual = do
  (headView, actualArgs) <-
    splitTypeViewHeadApplication (NE.length expectedArgs) actual
  subst' <- bindTypeViewHeadVariable refineBottom scope subst key headView
  foldM
    (\acc (templateTy, actualTy) -> matchTypeViewAgainstIdentityWith refineBottom scope acc templateTy actualTy)
    subst'
    (zip (NE.toList expectedArgs) actualArgs)

bindTypeViewHeadVariable ::
  Bool ->
  ElaborateScope ->
  TypeViewSubst ->
  TypeBinderIdentity ->
  TypeView ->
  Maybe TypeViewSubst
bindTypeViewHeadVariable refineBottom scope subst key view =
  case lookupTypeViewSubst key subst of
    Just existing
      | refineBottom,
        TypeViewBottomNode <- typeViewNodeView existing ->
          Just (insertTypeViewSubst key view subst)
      | refineBottom,
        typeViewIsBareBinderIdentity key view ->
          Just subst
      | semanticTypeViewEqualInScope scope existing view -> Just subst
      | otherwise -> Nothing
    Nothing
      | typeViewIsBareBinderIdentity keyIdentity view -> Just subst
      | typeViewMentionsFreeBinderIdentity keyIdentity view -> Nothing
      | otherwise -> Just (insertTypeViewSubst key view subst)
  where
    keyIdentity =
      key

preferVisibleTypeView :: ElaborateScope -> TypeView -> TypeView
preferVisibleTypeView scope =
  mapTypeViewDisplayHeadNames (\_ -> preferVisibleTypeHeadName scope)

preferVisibleTypeHeadName :: ElaborateScope -> String -> String
preferVisibleTypeHeadName scope name
  | Just identity <- typeHeadIdentityInScope scope name,
    Just visibleName <- lookupSymbolIdentityExact identity (esTypeDisplayNamesByIdentity scope) >>= preferredDisplayName identity =
      visibleName
  | otherwise = name

matchTypesInScope :: ElaborateScope -> Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesInScope scope =
  matchTypesWithHeadIdentitiesInScope scope Map.empty

matchTypesWithHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity -> Map String SrcType -> SrcType -> SrcType -> Maybe (Map String SrcType)
matchTypesWithHeadIdentitiesInScope scope headIdentities =
  matchTypesWith
    (alphaEqTypesWithHeadIdentitiesInScope scope headIdentities)
    (sameTypeHeadWithIdentitiesInScope scope headIdentities)

matchTypesWith ::
  (SrcType -> SrcType -> Bool) ->
  (String -> String -> Bool) ->
  Map String SrcType ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchTypesWith sameType sameTypeHead subst template actual
  | sameType template actual = Just subst
  | otherwise =
      case template of
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
        STBase {} -> Nothing
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
semanticTypeEqual = alphaEqTypesInScope

alphaEqTypesInScope :: ElaborateScope -> SrcType -> SrcType -> Bool
alphaEqTypesInScope scope =
  alphaEqTypesWithHeadIdentitiesInScope scope Map.empty

alphaEqTypesWithHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> SrcType -> Bool
alphaEqTypesWithHeadIdentitiesInScope scope headIdentities =
  alphaEqTypesWith (sameTypeHeadWithIdentitiesInScope scope headIdentities)

alphaEqTypesWith :: (String -> String -> Bool) -> SrcType -> SrcType -> Bool
alphaEqTypesWith sameTypeHead = go Map.empty Map.empty
  where
    go leftNames rightNames left right =
      case (left, right) of
        (STVar leftName, STVar rightName) ->
          sameTypeVar leftNames rightNames leftName rightName
        (STArrow leftDom leftCod, STArrow rightDom rightCod) ->
          go leftNames rightNames leftDom rightDom
            && go leftNames rightNames leftCod rightCod
        (STBase leftName, STBase rightName) -> sameTypeHead leftName rightName
        (STCon leftName leftArgs, STCon rightName rightArgs) ->
          sameTypeHead leftName rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STVarApp leftName leftArgs, STVarApp rightName rightArgs) ->
          sameTypeVar leftNames rightNames leftName rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STTyLam leftName leftBody, STTyLam rightName rightBody) ->
          go
            (Map.insert leftName rightName leftNames)
            (Map.insert rightName leftName rightNames)
            leftBody
            rightBody
        (STTyApp leftFun leftArg, STTyApp rightFun rightArg) ->
          go leftNames rightNames leftFun rightFun
            && go leftNames rightNames leftArg rightArg
        (STForall leftName leftMb leftBody, STForall rightName rightMb rightBody) ->
          let leftNames' = Map.insert leftName rightName leftNames
              rightNames' = Map.insert rightName leftName rightNames
           in sameBounds leftNames' rightNames' leftMb rightMb
                && go leftNames' rightNames' leftBody rightBody
        (STMu leftName leftBody, STMu rightName rightBody) ->
          go
            (Map.insert leftName rightName leftNames)
            (Map.insert rightName leftName rightNames)
            leftBody
            rightBody
        (STBottom, STBottom) -> True
        _ -> False

    sameBounds _ _ Nothing Nothing = True
    sameBounds leftNames rightNames (Just (SrcBound leftBound)) (Just (SrcBound rightBound)) =
      go leftNames rightNames leftBound rightBound
    sameBounds _ _ _ _ = False

    sameTypeVar leftNames rightNames leftName rightName =
      case (Map.lookup leftName leftNames, Map.lookup rightName rightNames) of
        (Just mappedRight, Just mappedLeft) -> mappedRight == rightName && mappedLeft == leftName
        (Nothing, Nothing) -> leftName == rightName
        _ -> False

semanticTypeViewEqual :: TypeView -> TypeView -> Bool
semanticTypeViewEqual =
  (==)

semanticTypeViewEqualInScope :: ElaborateScope -> TypeView -> TypeView -> Bool
semanticTypeViewEqualInScope scope left right =
  alphaEqTypesWithHeadIdentitiesInScope
    scope
    (mergeSymbolIdentityMaps [typeViewHeadIdentities left, typeViewHeadIdentities right])
    (typeViewIdentity left)
    (typeViewIdentity right)

sameTypeHeadInScope :: ElaborateScope -> String -> String -> Bool
sameTypeHeadInScope scope =
  sameTypeHeadWithIdentitiesInScope scope Map.empty

sameTypeHeadWithIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity -> String -> String -> Bool
sameTypeHeadWithIdentitiesInScope scope headIdentities left right =
  case (typeHeadIdentity left, typeHeadIdentity right) of
    (Just leftIdentity, Just rightIdentity) -> sameSymbolIdentity leftIdentity rightIdentity
    (Nothing, Nothing) -> left == right
    _ -> False
  where
    typeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name
        <|> typeHeadIdentityInScope scope name

typeHeadIdentityInScope :: ElaborateScope -> String -> Maybe SymbolIdentity
typeHeadIdentityInScope scope name =
  lookupSymbolIdentityAlias (esTypeHeadIdentities scope) name
    <|> Builtins.builtinTypeHeadIdentity name

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

extendConstraintEvidenceInfo :: ElaborateScope -> [ConstraintInfo] -> ElaborateM (ElaborateScope, [EvidenceMethod], [LoweredResolvedLocalIdentity])
extendConstraintEvidenceInfo scope constraints = do
  mapM_ requireKnownClass constraints
  built <- mapM buildEvidence (concatMap (constraintEvidenceClosureInfo scope) constraints)
  let evidenceInfos = concatMap first built
      params = concatMap second built
      runtimeTypeViews = Map.unions (map third built)
      evidenceIdentities = concatMap fourth built
  pure
    ( scope
        { esEvidence = evidenceInfos ++ esEvidence scope,
          esRuntimeTypeViews = runtimeTypeViews `Map.union` esRuntimeTypeViews scope
        },
      params,
      evidenceIdentities
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
              evidenceRef <- freshElaborateLocalRef runtimeName
              let evidenceTypeViewRaw =
                    methodEvidenceSourceTypeInfoViewRaw (esTypes scope) (esClassesByIdentity scope) classInfo (constraintTypeViews constraint) methodInfo
                  evidenceTypeView = evidenceTypeViewRaw
                  methodEvidence =
                    EvidenceMethod
                      { evidenceMethodSymbol = methodInfoSymbolIdentity methodInfo,
                        evidenceMethodResolvedVar =
                          X.ResolvedVar
                            { X.resolvedVarType = X.TBottom,
                              X.resolvedVarDetails = EvidenceId evidenceRef
                            },
                        evidenceMethodTypeView = evidenceTypeView
                      }
                  evidenceIdentity =
                    LoweredResolvedLocalIdentity evidenceRef evidenceRef
              methodSurfaceName <- evidenceMethodSurfaceName methodEvidence
              pure
                ( methodName methodInfo,
                  methodSurfaceName,
                  methodEvidence,
                  evidenceIdentity
                )
          )
          (Map.elems (classMethodsByIdentity classInfo))
      let evidenceMethodsByIdentity0 =
            uniqueInfoEntriesByIdentity
              [(evidenceMethodSymbol methodEvidence, methodEvidence) | (_, _, methodEvidence, _) <- methodEntries]
      let evidenceInfo =
            EvidenceInfo
              { evidenceClassSymbol = constraintClassSymbol constraint,
                evidenceTypeViews = constraintTypeViews constraint,
                evidenceMethodsByIdentity = evidenceMethodsByIdentity0
              }
          params =
            [ methodEvidence
            | (_, _, methodEvidence, _) <- methodEntries
            ]
          runtimeTypeViews =
            Map.fromList
              [ (methodSurfaceName, evidenceMethodTypeView methodEvidence)
              | (_, methodSurfaceName, methodEvidence, _) <- methodEntries
              ]
          evidenceIdentities =
            [ evidenceIdentity
            | (_, _, _, evidenceIdentity) <- methodEntries
              ]
      pure ([evidenceInfo], params, runtimeTypeViews, evidenceIdentities)

    first (value, _, _, _) = value
    second (_, value, _, _) = value
    third (_, _, value, _) = value
    fourth (_, _, _, value) = value

extendLocalWithRef :: ElaborateScope -> LocalRef -> String -> String -> Maybe SrcType -> ElaborateM ElaborateScope
extendLocalWithRef scope localRef sourceName runtimeName mbTy = do
  case mbTy of
    Just sourceTy -> pure (extendLocalSourceTypePure scope localRef sourceName runtimeName sourceTy)
    Nothing -> do
      loweredTy <- freshTypeName
      pure (extendLocalLoweredPure scope localRef sourceName runtimeName loweredTy)

extendLocalLoweredWithRef :: ElaborateScope -> LocalRef -> String -> String -> SrcType -> ElaborateM ElaborateScope
extendLocalLoweredWithRef scope localRef sourceName runtimeName loweredTy =
  pure (extendLocalLoweredPure scope localRef sourceName runtimeName loweredTy)

extendResolvedLocalView :: ElaborateScope -> LocalRef -> String -> Maybe TypeView -> ElaborateM ElaborateScope
extendResolvedLocalView scope localRef runtimeName mbView = do
  recordResolvedLocalIdentity runtimeName localRef
  case mbView of
    Just sourceView -> pure (extendResolvedLocalTypeViewPure scope localRef runtimeName sourceView)
    Nothing -> do
      sourceView <- freshTypeVarView
      pure (extendResolvedLocalTypeViewPure scope localRef runtimeName sourceView)

recordResolvedLocalIdentity :: String -> LocalRef -> ElaborateM ()
recordResolvedLocalIdentity runtimeName localRef = do
  let entry = LoweredResolvedLocalIdentity (renameLocalRef runtimeName localRef) localRef
  modify
    ( \state ->
        let entries = elaborateResolvedLocalIdentities state
         in
        state
          { elaborateResolvedLocalIdentities =
              if entry `elem` entries
                then entries
                else entries ++ [entry]
          }
    )

resolvedLocalBinderDetails :: String -> LocalRef -> IdDetails
resolvedLocalBinderDetails runtimeName localRef =
  LocalId (renameLocalRef runtimeName localRef)

resolvedLocalLamSurface :: String -> LocalRef -> Maybe SrcType -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
resolvedLocalLamSurface runtimeName localRef mbType body =
  case mbType of
    Just ty -> S.EResolvedLamAnn details runtimeName ty body
    Nothing -> S.EResolvedLam details runtimeName body
  where
    details = resolvedLocalBinderDetails runtimeName localRef

-- Compiler-generated case handlers receive constructor fields as parameters.
-- A rank-2 field is already a closed value contract, so mark that lambda
-- exact at the surface boundary; source annotations continue through kappa-
-- sigma, and monomorphic handler fields retain their established path.
resolvedCaseHandlerLamSurface :: String -> LocalRef -> SrcType -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
resolvedCaseHandlerLamSurface runtimeName localRef ty body =
  case ty of
    STForall {} ->
      S.EExactLamNode
        (S.ResolvedTermReference details runtimeName)
        ty
        body
    _ -> resolvedLocalLamSurface runtimeName localRef (Just ty) body
  where
    details = resolvedLocalBinderDetails runtimeName localRef

resolvedLocalLetSurface :: String -> LocalRef -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
resolvedLocalLetSurface runtimeName localRef =
  S.EResolvedLet (resolvedLocalBinderDetails runtimeName localRef) runtimeName

wrapResolvedCaseHandlerLambdaChain :: [(String, LocalRef, SrcType)] -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
wrapResolvedCaseHandlerLambdaChain binders body =
  foldr wrapOne body binders
  where
    wrapOne (runtimeName, localRef, binderTy) acc =
      resolvedCaseHandlerLamSurface runtimeName localRef binderTy acc

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
          valueTypeView = loweredSourceTypeViewInScope scope loweredTy,
          valueConstraintInfos = []
        }

loweredSourceTypeViewInScope :: ElaborateScope -> SrcType -> TypeView
loweredSourceTypeViewInScope scope loweredTy =
  requireTypeViewFromSourceType
    (sourceTypeHeadIdentitiesInScope scope loweredTy)
    (sourceTypeBinderIdentitiesInScope scope loweredTy)
    loweredTy

extendLocalSourceTypePure :: ElaborateScope -> LocalRef -> String -> String -> SrcType -> ElaborateScope
extendLocalSourceTypePure scope localRef sourceName runtimeName sourceTy =
  insertResolvedLocalValue localRef valueInfo $
    insertLocalValue sourceName valueInfo $
      insertRuntimeTypeView runtimeName (lowerTypeViewWithIdentities scope sourceView) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView = sourceView,
          valueConstraintInfos = []
        }
    sourceView =
      sourceTypeViewInScope scope sourceTy

extendResolvedLocalTypeViewPure :: ElaborateScope -> LocalRef -> String -> TypeView -> ElaborateScope
extendResolvedLocalTypeViewPure scope localRef runtimeName sourceView =
  insertResolvedLocalValue localRef valueInfo $
    insertRuntimeTypeView runtimeName (lowerTypeViewWithIdentities scope sourceView) scope
  where
    valueInfo =
      OrdinaryValue
        { valueInfoSymbol = resolvedLocalValueSymbol localRef runtimeName,
          valueRuntimeName = runtimeName,
          valueTypeView = sourceView,
          valueConstraintInfos = []
        }

insertRuntimeTypeView :: String -> TypeView -> ElaborateScope -> ElaborateScope
insertRuntimeTypeView runtimeName view scope =
  scope {esRuntimeTypeViews = Map.insert runtimeName view (esRuntimeTypeViews scope)}

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
          (esValuesByIdentity scope),
      esValueRuntimeAliasesByIdentity =
        Map.insert
          (valueInfoSymbolIdentity valueInfo)
          (valueInfoRuntimeAliases valueInfo)
          (esValueRuntimeAliasesByIdentity scope)
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

freshTypeVarView :: ElaborateM TypeView
freshTypeVarView = do
  state <- get
  let (unique@(UniqueIdentity n), generator') = freshIdentity (elaborateIdentityGenerator state)
      identity = typeBinderIdentityFromUnique unique
      displayName = "r$" ++ show n
      identityName = typeBinderIdentityStableName identity
  modify (\state' -> state' {elaborateIdentityGenerator = generator'})
  pure
    ( requireTypeViewFromSourceType
        Map.empty
        ( Map.fromList
            [ (displayName, identity),
              (identityName, identity)
            ]
        )
        (STVar displayName)
    )

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
  let (UniqueIdentity n, generator') = freshIdentity (elaborateIdentityGenerator state)
  modify (\state' -> state' {elaborateIdentityGenerator = generator'})
  pure n
