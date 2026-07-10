{-# LANGUAGE LambdaCase #-}

{- |
Module      : MLF.Backend.StructuralRecursiveData
Description : Private matcher for structural recursive backend ADT encodings

This module is the private owner for comparing structural recursive encodings
with nominal backend ADT metadata. It returns operation-local evidence only;
conversion remains responsible for source-local recovery and representation
normalization before this matcher is used.
-}
module MLF.Backend.StructuralRecursiveData
  ( BackendParameterBounds,
    BackendDataScope (..),
    StructuralConstructorMatch (..),
    StructuralRecursiveDataMatch (..),
    StructuralRecursiveDataMismatch (..),
    alphaEqBackendType,
    backendDataScope,
    backendStructuralDataBoundaryMatches,
    backendStructuralDataBoundaryMatchesWith,
    completeBackendParameterSubstitution,
    completeDataParameterSubstitution,
    decomposeBackendTypeHead,
    isVacuousRecursiveBinderWithIdentity,
    lookupTypeBound,
    matchBackendTypeParametersWithTypeBounds,
    matchConstructorResult,
    matchFocusedStructuralConstructor,
    matchStructuralDataDeclaration,
    metadataLightStructuralDataMatches,
    metadataLightStructuralDataMatchesWithIdentity,
    structuralIdentityAllowsNameFallback,
    structuralBackendHandlerFields,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralDataSelfFieldMatches,
    structuralMuAsActualDataType,
    structuralMuAsDataType,
    structuralMuHandlerTypes,
    structuralMuNameMatches,
    structuralMuPayloadTypes,
    structuralMuTypesHaveBinderIdentityMismatch,
    structuralPayloadsMayInstantiate,
    structuralRecursiveDataName,
    recursiveBodyCompatibleWithIdentity,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR.Types
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolReferenceMode (..), lookupSymbolIdentityExact, symbolDefiningName, symbolRefMatchesWith, symbolUniqueIdentity)
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (StructuralTypeBinderRole (..), TypeBinderIdentity, typeBinderIdentityGeneratedUnique, typeBinderIdentityStableName, typeBinderIdentityStructural)
import MLF.Util.Names (freshNameLike)

type BackendParameterBounds = Map.Map BackendTypeSubstitutionKey (Maybe BackendType)

type AlphaBinderEnv = Map.Map BackendTypeSubstitutionKey (Set.Set BackendTypeSubstitutionKey)

data BackendDataScope
  = BackendDataScope
      { backendDataScopeByName :: Map.Map String BackendData,
        backendDataScopeByIdentity :: Map.Map SymbolIdentity BackendData
      }
  deriving (Eq, Show)

backendDataScope :: Map.Map String BackendData -> Map.Map SymbolIdentity BackendData -> BackendDataScope
backendDataScope dataDeclsByName =
  BackendDataScope (Map.filter ((== Nothing) . backendDataIdentity) dataDeclsByName)

typeBoundKeyNames :: BackendParameterBounds -> Set.Set String
typeBoundKeyNames =
  Set.map backendTypeSubstitutionKeyName . Map.keysSet

lookupTypeBound :: Maybe TypeBinderIdentity -> String -> BackendParameterBounds -> Maybe (Maybe BackendType)
lookupTypeBound identity name =
  Map.lookup (backendTypeSubstitutionKeyFromMaybeMetadataLight identity name)

data StructuralRecursiveDataMatch = StructuralRecursiveDataMatch
  { srdmDataName :: String,
    srdmParameterSubstitution :: Map.Map BackendTypeSubstitutionKey BackendType,
    srdmPayloadFields :: [[BackendType]]
  }
  deriving (Eq, Show)

data StructuralConstructorMatch = StructuralConstructorMatch
  { srcmDataName :: String,
    srcmConstructorIdentity :: Maybe SymbolIdentity,
    srcmConstructorName :: String,
    srcmFieldTypes :: [BackendType]
  }
  deriving (Show)

instance Eq StructuralConstructorMatch where
  left == right =
    symbolRefMatchesWith SymbolMetadataLight (srcmConstructorIdentity left) (srcmConstructorName left) (srcmConstructorIdentity right) (srcmConstructorName right)
      && case (srcmConstructorIdentity left, srcmConstructorIdentity right) of
        (Just {}, Just {}) -> True
        (Nothing, Nothing) -> srcmDataName left == srcmDataName right
        _ -> False
      && srcmFieldTypes left == srcmFieldTypes right

data StructuralRecursiveDataMismatch
  = StructuralRecursiveDataNameMismatch String String
  | StructuralRecursiveDataNameUnavailable String
  | StructuralRecursiveDataPayloadUnavailable String
  | StructuralRecursiveDataArgumentMismatch String [BackendType] [BackendType]
  | StructuralRecursiveDataConstructorSetMismatch String Int Int
  | StructuralRecursiveDataUnknownConstructor String String
  | StructuralRecursiveDataAmbiguousConstructor String String
  | StructuralRecursiveDataConstructorArityMismatch String String Int Int
  | StructuralRecursiveDataConstructorPayloadMismatch String String BackendType BackendType
  deriving (Eq, Show)

alphaEqBackendType :: BackendType -> BackendType -> Bool
alphaEqBackendType =
  go Map.empty Map.empty
  where
    go leftEnv rightEnv leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVarWithIdentity leftIdentity leftName, BTVarWithIdentity rightIdentity rightName) ->
          typeVarMatches leftEnv rightEnv leftIdentity leftName rightIdentity rightName
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go leftEnv rightEnv leftDom rightDom && go leftEnv rightEnv leftCod rightCod
        (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase) ->
          backendTypeHeadMatchesWith SymbolMetadataLight leftIdentity leftBase rightIdentity rightBase
        (BTBaseWithIdentity leftIdentity leftBase, BTMuWithIdentity rightIdentity rightName rightBody) ->
          metadataLightStructuralDataMatchesAgainstHead leftIdentity leftBase [] rightIdentity rightName rightBody
        (BTMuWithIdentity leftIdentity leftName leftBody, BTBaseWithIdentity rightIdentity rightBase) ->
          metadataLightStructuralDataMatchesAgainstHead rightIdentity rightBase [] leftIdentity leftName leftBody
        (BTConWithIdentity leftIdentity leftCon leftArgs, BTConWithIdentity rightIdentity rightCon rightArgs) ->
          backendTypeHeadMatchesWith SymbolMetadataLight leftIdentity leftCon rightIdentity rightCon
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTConWithIdentity leftIdentity leftCon leftArgs, BTMuWithIdentity rightIdentity rightName rightBody) ->
          metadataLightStructuralDataMatchesAgainstHead leftIdentity leftCon (NE.toList leftArgs) rightIdentity rightName rightBody
        (BTMuWithIdentity leftIdentity leftName leftBody, BTConWithIdentity rightIdentity rightCon rightArgs) ->
          metadataLightStructuralDataMatchesAgainstHead rightIdentity rightCon (NE.toList rightArgs) leftIdentity leftName leftBody
        (BTVarAppWithIdentity leftIdentity leftName leftArgs, BTVarAppWithIdentity rightIdentity rightName rightArgs) ->
          typeVarMatches leftEnv rightEnv leftIdentity leftName rightIdentity rightName
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTForallWithIdentity leftIdentity leftName leftBound leftBody, BTForallWithIdentity rightIdentity rightName rightBound rightBody) ->
          maybeAlphaEq leftEnv rightEnv leftBound rightBound
            && let (leftEnv', rightEnv') = extendBinderScope leftIdentity leftName rightIdentity rightName leftEnv rightEnv
                in go leftEnv' rightEnv' leftBody rightBody
        (BTMuWithIdentity leftIdentity leftName leftBody, BTMuWithIdentity rightIdentity rightName rightBody) ->
          let (leftEnv', rightEnv') = extendBinderScope leftIdentity leftName rightIdentity rightName leftEnv rightEnv
           in go leftEnv' rightEnv' leftBody rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    extendBinderScope ::
      Maybe TypeBinderIdentity ->
      String ->
      Maybe TypeBinderIdentity ->
      String ->
      AlphaBinderEnv ->
      AlphaBinderEnv ->
      (AlphaBinderEnv, AlphaBinderEnv)
    extendBinderScope leftIdentity leftName rightIdentity rightName leftEnv rightEnv =
      (insertAliasScope leftAliases rightAliases leftEnv, insertAliasScope rightAliases leftAliases rightEnv)
      where
        leftAliases = binderAliasKeys leftIdentity leftName
        rightAliases = binderAliasKeys rightIdentity rightName

    insertAliasScope :: [BackendTypeSubstitutionKey] -> [BackendTypeSubstitutionKey] -> AlphaBinderEnv -> AlphaBinderEnv
    insertAliasScope aliases targets env =
      foldr (\alias -> Map.insertWith Set.union alias targetSet) env aliases
      where
        targetSet = Set.fromList targets

    binderAliasKeys :: Maybe TypeBinderIdentity -> String -> [BackendTypeSubstitutionKey]
    binderAliasKeys identity name =
      let key = backendTypeSubstitutionKeyFromMaybeMetadataLight identity name
       in case backendTypeSubstitutionKeyIdentity key of
            Just {} -> [key]
            Nothing -> []

    typeVarMatches ::
      AlphaBinderEnv ->
      AlphaBinderEnv ->
      Maybe TypeBinderIdentity ->
      String ->
      Maybe TypeBinderIdentity ->
      String ->
      Bool
    typeVarMatches leftEnv rightEnv leftIdentity leftName rightIdentity rightName =
      case (Map.lookup leftKey leftEnv, Map.lookup rightKey rightEnv) of
        (Just expectedRights, Just expectedLefts) ->
          Set.member rightKey expectedRights && Set.member leftKey expectedLefts
        (Nothing, Nothing) ->
          leftKey == rightKey
        _ ->
          False
      where
        leftKey = backendTypeSubstitutionKeyFromMaybeMetadataLight leftIdentity leftName
        rightKey = backendTypeSubstitutionKeyFromMaybeMetadataLight rightIdentity rightName

    maybeAlphaEq _ _ Nothing Nothing =
      True
    maybeAlphaEq leftEnv rightEnv (Just leftTy) (Just rightTy) =
      go leftEnv rightEnv leftTy rightTy
    maybeAlphaEq _ _ _ _ =
      False

metadataLightStructuralDataMatches :: BaseTy -> [BackendType] -> String -> BackendType -> Bool
metadataLightStructuralDataMatches base args muName body =
  metadataLightStructuralDataMatchesWithIdentity base args Nothing muName body

metadataLightStructuralDataMatchesWithIdentity :: BaseTy -> [BackendType] -> Maybe TypeBinderIdentity -> String -> BackendType -> Bool
metadataLightStructuralDataMatchesWithIdentity base@(BaseTy dataName) args muIdentity muName body
  | PrimitiveInventory.isOpaqueBuiltinTypeName dataName = False
  | otherwise =
      case matchStructuralDataLightWithIdentity base args muIdentity muName body of
        Right _ -> True
        Left _ -> False

metadataLightStructuralDataMatchesAgainstHead :: Maybe SymbolIdentity -> BaseTy -> [BackendType] -> Maybe TypeBinderIdentity -> String -> BackendType -> Bool
metadataLightStructuralDataMatchesAgainstHead mbDataIdentity base args muIdentity muName body =
  case mbDataIdentity of
    Just dataIdentity
      | structuralMuIdentityMatches (Just dataIdentity) muIdentity ->
          identityBearingStructuralDataMatches dataIdentity base args muIdentity muName body
      | otherwise ->
          False
    Nothing ->
      metadataLightStructuralDataMatchesWithIdentity base args muIdentity muName body

structuralIdentityAllowsNameFallback :: Maybe TypeBinderIdentity -> Bool
-- Metadata-light inputs may still match by structural name. Once a binder
-- identity exists, callers must use that identity instead of generated/name
-- aliases.
structuralIdentityAllowsNameFallback Nothing =
  True
structuralIdentityAllowsNameFallback (Just _) =
  False

structuralMuRefsMatchIdentityFirst :: Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity -> String -> Bool
structuralMuRefsMatchIdentityFirst expectedIdentity expectedName actualIdentity actualName =
  typeBinderRefMatchesWith BackendTypeIdentityOnly expectedIdentity expectedName actualIdentity actualName
    || ( structuralIdentityAllowsNameFallback expectedIdentity
           && structuralIdentityAllowsNameFallback actualIdentity
           && structuralRecursiveDataNamesMatch expectedName actualName
       )

structuralRecursiveDataNamesMatch :: String -> String -> Bool
structuralRecursiveDataNamesMatch expectedName actualName =
  case (structuralRecursiveDataName expectedName, structuralRecursiveDataName actualName) of
    (Just expectedDataName, Just actualDataName) -> expectedDataName == actualDataName
    _ -> False

matchStructuralDataLightWithIdentity ::
  BaseTy ->
  [BackendType] ->
  Maybe TypeBinderIdentity ->
  String ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataLightWithIdentity (BaseTy dataName) args muIdentity muName body = do
  structuralName <-
    case structuralRecursiveDataName muName of
      Just name -> Right name
      Nothing -> Left (StructuralRecursiveDataNameUnavailable muName)
  if dataName == structuralName
    then pure ()
    else Left (StructuralRecursiveDataNameMismatch dataName structuralName)
  matchStructuralDataPayload dataName args muIdentity muName body

identityBearingStructuralDataMatches :: SymbolIdentity -> BaseTy -> [BackendType] -> Maybe TypeBinderIdentity -> String -> BackendType -> Bool
identityBearingStructuralDataMatches dataIdentity (BaseTy dataName) args muIdentity muName body =
  not (opaqueBuiltinIdentity dataIdentity)
    && case matchStructuralDataPayload dataName args muIdentity muName body of
      Right _ -> True
      Left _ -> False
  where
    opaqueBuiltinIdentity identity =
      any
        ((== Just identity) . PrimitiveInventory.builtinTypeHeadIdentity)
        (Set.toList PrimitiveInventory.builtinOpaqueTypeNames)

matchStructuralDataPayload :: String -> [BackendType] -> Maybe TypeBinderIdentity -> String -> BackendType -> Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataPayload dataName args muIdentity muName body = do
  payloadFields <-
    case structuralBackendHandlerFields body of
      Just fields -> Right fields
      Nothing -> Left (StructuralRecursiveDataPayloadUnavailable muName)
  let payloadTypes = filter (not . recursiveSelfField muIdentity muName) (concat payloadFields)
      matches
        | null args = null payloadTypes
        | null payloadTypes = all isBareTypeVariable args
        | otherwise = zipAllWith metadataLightPayloadTypeMatches args payloadTypes
  if matches
    then
      Right
        StructuralRecursiveDataMatch
          { srdmDataName = dataName,
            srdmParameterSubstitution = Map.empty,
            srdmPayloadFields = payloadFields
          }
    else Left (StructuralRecursiveDataArgumentMismatch dataName args payloadTypes)

metadataLightPayloadTypeMatches :: BackendType -> BackendType -> Bool
metadataLightPayloadTypeMatches left right =
  alphaEqBackendType left right

recursiveSelfField :: Maybe TypeBinderIdentity -> String -> BackendType -> Bool
recursiveSelfField muIdentity muName ty =
  case ty of
    BTVarWithIdentity fieldIdentity fieldName ->
      structuralSelfFieldMatches muIdentity muName fieldIdentity fieldName
    _ ->
      False

structuralSelfFieldMatches :: Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity -> String -> Bool
structuralSelfFieldMatches muIdentity muName fieldIdentity fieldName =
  typeBinderRefMatchesWith BackendTypeIdentityOnly fieldIdentity fieldName muIdentity muName
    || case (muIdentity, fieldIdentity) of
      (Nothing, Nothing) ->
        structuralRecursiveDataName fieldName == structuralRecursiveDataName muName
      _ ->
        False

structuralDataSelfFieldMatches :: String -> Maybe TypeBinderIdentity -> Maybe TypeBinderIdentity -> String -> Bool
structuralDataSelfFieldMatches dataName muIdentity fieldIdentity fieldName =
  typeBinderRefMatchesWith BackendTypeIdentityOnly fieldIdentity fieldName muIdentity ("$" ++ dataName ++ "_self")
    || case (muIdentity, fieldIdentity) of
      (Nothing, Nothing) ->
        structuralRecursiveDataName fieldName == Just dataName
      _ ->
        False

backendTypeVarMatches :: Maybe TypeBinderIdentity -> String -> BackendType -> Bool
backendTypeVarMatches expectedIdentity expectedName =
  \case
    BTVarWithIdentity actualIdentity actualName ->
      typeBinderRefMatchesWith BackendTypeMetadataLight actualIdentity actualName expectedIdentity expectedName
    _ ->
      False

matchFocusedStructuralConstructor ::
  BackendParameterBounds ->
  BackendData ->
  BackendConstructor ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralConstructorMatch
matchFocusedStructuralConstructor typeBounds dataDecl constructor substitution structuralTy = do
  wholeMatch <- matchStructuralDataDeclarationForSelectedData typeBounds dataDecl substitution structuralTy
  (constructorIndex, matchedConstructor) <-
    case indexedConstructors of
      [matched] -> Right matched
      [] -> Left (StructuralRecursiveDataUnknownConstructor dataName constructorName)
      _ -> Left (StructuralRecursiveDataAmbiguousConstructor dataName constructorName)
  fields <-
    case atMay (srdmPayloadFields wholeMatch) constructorIndex of
      Just fieldTys -> Right fieldTys
      Nothing ->
        Left
          ( StructuralRecursiveDataConstructorSetMismatch
              dataName
              (length (backendDataConstructors dataDecl))
              (length (srdmPayloadFields wholeMatch))
          )
  Right
    StructuralConstructorMatch
      { srcmDataName = dataName,
        srcmConstructorIdentity = backendConstructorIdentity matchedConstructor,
        srcmConstructorName = backendConstructorName matchedConstructor,
        srcmFieldTypes = fields
      }
  where
    dataName =
      backendDataName dataDecl
    constructorName =
      backendConstructorName constructor
    indexedConstructors =
      [ (index0, candidate)
        | (index0, candidate) <- zip [0 ..] (backendDataConstructors dataDecl),
          constructorsMatch constructor candidate
      ]

    constructorsMatch expected candidate =
      symbolRefMatchesWith
        SymbolMetadataLight
        (backendConstructorIdentity expected)
        constructorName
        (backendConstructorIdentity candidate)
        (backendConstructorName candidate)

matchStructuralDataDeclaration ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataDeclaration typeBounds dataDecl substitution =
  \case
    structuralTy@(BTMuWithIdentity muIdentity muName body)
      | structuralMuMatchesDataDecl dataDecl muIdentity muName -> do
          (resultIdentity, resultName, handlers) <-
            case structuralMuHandlerTypes body of
              Just value -> Right value
              Nothing -> Left (StructuralRecursiveDataPayloadUnavailable muName)
          if length constructors == length handlers
            then pure ()
            else
              Left
                ( StructuralRecursiveDataConstructorSetMismatch
                    (backendDataName dataDecl)
                    (length constructors)
                    (length handlers)
                )
          payloadFields <- structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity muName resultIdentity resultName handlers
          Right
            StructuralRecursiveDataMatch
              { srdmDataName = backendDataName dataDecl,
                srdmParameterSubstitution = dataSubstitution,
                srdmPayloadFields = payloadFields
              }
      | otherwise ->
          case structuralRecursiveDataName muName of
            Just actualName -> Left (StructuralRecursiveDataNameMismatch (backendDataName dataDecl) actualName)
            Nothing -> Left (StructuralRecursiveDataNameUnavailable muName)
    _ ->
      Left (StructuralRecursiveDataPayloadUnavailable (backendDataName dataDecl))
  where
    constructors =
      backendDataConstructors dataDecl
    dataParameterKeySet =
      Set.fromList (backendDataParameterKeys dataDecl)
    dataSubstitution =
      Map.filterWithKey (\key _ -> Set.member key dataParameterKeySet) substitution

matchStructuralDataDeclarationForSelectedData ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataDeclarationForSelectedData typeBounds dataDecl substitution structuralTy =
  case matchStructuralDataDeclaration typeBounds dataDecl substitution structuralTy of
    Right match -> Right match
    Left mismatch ->
      case (backendDataIdentity dataDecl, structuralTy) of
        (Just {}, _) ->
          matchSelectedDataShape mismatch structuralTy
        (Nothing, BTMuWithIdentity (Just {}) _ _) ->
          matchSelectedDataShape mismatch structuralTy
        _ ->
          Left mismatch
  where
    constructors =
      backendDataConstructors dataDecl
    dataParameterKeySet =
      Set.fromList (backendDataParameterKeys dataDecl)
    dataSubstitution =
      Map.filterWithKey (\key _ -> Set.member key dataParameterKeySet) substitution

    matchSelectedDataShape fallback =
      \case
        structuralTy'@(BTMuWithIdentity muIdentity muName body)
          | selectedMuIdentityAllowed muIdentity muName -> do
              (resultIdentity, resultName, handlers) <-
                case structuralMuHandlerTypes body of
                  Just value -> Right value
                  Nothing -> Left fallback
              if length constructors == length handlers
                then pure ()
                else Left fallback
              payloadFields <-
                case structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy' muIdentity muName resultIdentity resultName handlers of
                  Right fields -> Right fields
                  Left _ -> Left fallback
              Right
                StructuralRecursiveDataMatch
                  { srdmDataName = backendDataName dataDecl,
                    srdmParameterSubstitution = dataSubstitution,
                    srdmPayloadFields = payloadFields
                  }
        _ -> Left fallback

    selectedMuIdentityAllowed muIdentity muName =
      case backendDataIdentity dataDecl of
        Just {} ->
          case muIdentity of
            Nothing -> True
            Just identity ->
              isGeneratedTypeBinder identity
                || structuralMuMatchesDataDecl dataDecl muIdentity muName
        Nothing ->
          case muIdentity of
            Just {} -> True
            Nothing -> structuralMuMatchesDataDecl dataDecl muIdentity muName

structuralDataDeclarationMatches ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Bool
structuralDataDeclarationMatches typeBounds dataDecl substitution =
  \case
    ty@BTMu {} ->
      case matchStructuralDataDeclaration typeBounds dataDecl substitution ty of
        Right _ -> True
        Left _ -> False
    _ ->
      True

backendStructuralDataBoundaryMatches ::
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendStructuralDataBoundaryMatches =
  backendStructuralDataBoundaryMatchesWith BackendTypeMetadataLight

backendStructuralDataBoundaryMatchesWith ::
  BackendTypeReferenceMode ->
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendStructuralDataBoundaryMatchesWith referenceMode typeBounds mbDataDecls expectedTy actualTy =
  go expectedTy actualTy
  where
    typeHeadMatches =
      backendTypeHeadMatchesWith (structuralSymbolReferenceMode referenceMode)

    go expected actual
      | structuralMuTypesHaveBinderIdentityMismatch expected actual =
          False
      | otherwise =
          alphaEqWithinDataScope expected actual
            || case (expected, actual) of
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go expectedDom actualDom && go expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase) ->
                typeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
              (BTBaseWithIdentity expectedDataIdentity expectedBase, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity expectedBase [] actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTBaseWithIdentity actualDataIdentity actualBase) ->
                structuralMuMatchesKnownData actualDataIdentity actualBase [] expectedIdentity expectedName expectedBody
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
                typeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
                  && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTConWithIdentity expectedDataIdentity expectedCon expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity expectedCon (NE.toList expectedArgs) actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity actualDataIdentity actualCon actualArgs) ->
                structuralMuMatchesKnownData actualDataIdentity actualCon (NE.toList actualArgs) expectedIdentity expectedName expectedBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
                  || structuralPayloadsMayInstantiate typeBounds expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) ->
                maybeBoundaryMatches expectedBound actualBound
                  && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                         freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshName
                         expectedBody' = substituteBackendTypeForBinder expectedIdentity expectedName freshTy expectedBody
                         actualBody' = substituteBackendTypeForBinder actualIdentity actualName freshTy actualBody
                      in go expectedBody' actualBody'
              (BTBottom, BTBottom) ->
                True
              _ ->
                False

    alphaEqWithinDataScope expected actual =
      alphaEqBackendType expected actual
        && not (identityHeadNeedsScopedData expected actual)

    identityHeadNeedsScopedData expected actual =
      case (expected, actual, mbDataDecls) of
        (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod, _) ->
          identityHeadNeedsScopedData expectedDom actualDom
            || identityHeadNeedsScopedData expectedCod actualCod
        (BTConWithIdentity _ _ expectedArgs, BTConWithIdentity _ _ actualArgs, _) ->
          or (zipWith identityHeadNeedsScopedData (NE.toList expectedArgs) (NE.toList actualArgs))
        (BTVarAppWithIdentity _ _ expectedArgs, BTVarAppWithIdentity _ _ actualArgs, _) ->
          or (zipWith identityHeadNeedsScopedData (NE.toList expectedArgs) (NE.toList actualArgs))
        (BTForallWithIdentity _ _ expectedBound expectedBody, BTForallWithIdentity _ _ actualBound actualBody, _) ->
          maybe False (uncurry identityHeadNeedsScopedData) ((,) <$> expectedBound <*> actualBound)
            || identityHeadNeedsScopedData expectedBody actualBody
        (BTMuWithIdentity _ _ expectedBody, BTMuWithIdentity _ _ actualBody, _) ->
          identityHeadNeedsScopedData expectedBody actualBody
        (BTBaseWithIdentity (Just {}) _, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTBaseWithIdentity (Just {}) _, Just {}) -> True
        (BTConWithIdentity (Just {}) _ _, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTConWithIdentity (Just {}) _ _, Just {}) -> True
        (BTBaseWithIdentity Nothing base, BTMuWithIdentity _ muName _, Just dataScope) ->
          identitylessHeadNeedsScopedData dataScope base muName
        (BTMuWithIdentity _ muName _, BTBaseWithIdentity Nothing base, Just dataScope) ->
          identitylessHeadNeedsScopedData dataScope base muName
        (BTConWithIdentity Nothing base _, BTMuWithIdentity _ muName _, Just dataScope) ->
          identitylessHeadNeedsScopedData dataScope base muName
        (BTMuWithIdentity _ muName _, BTConWithIdentity Nothing base _, Just dataScope) ->
          identitylessHeadNeedsScopedData dataScope base muName
        _ -> False

    identitylessHeadNeedsScopedData dataScope (BaseTy name) muName =
      identityBearingScopedDataName dataScope name
        || maybe False (identityBearingScopedDataName dataScope) (structuralRecursiveDataName muName)

    identityBearingScopedDataName (BackendDataScope dataDecls dataDeclsByIdentity) dataName =
      identityBearingDataName dataName dataDecls
        || identityBearingDataName dataName dataDeclsByIdentity

    structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      case
        ( structuralDataDeclForMuPair expectedIdentity expectedName actualIdentity actualName,
          structuralMuHandlerTypesWithIdentity expectedBody,
          structuralMuHandlerTypesWithIdentity actualBody
        )
        of
          ( Just dataDecl,
            Just (expectedResultIdentity, expectedResultName, expectedHandlers),
            Just (actualResultIdentity, actualResultName, actualHandlers)
            )
              | length expectedHandlers == length actualHandlers,
                length expectedHandlers == length (backendDataConstructors dataDecl) ->
                  let freshSelf =
                        freshNameLike
                          expectedName
                          ( Set.unions
                              [ Set.fromList [expectedName, actualName],
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          )
                      freshResult =
                        freshNameLike
                          expectedResultName
                          ( Set.unions
                              [ Set.fromList [expectedResultName, actualResultName, freshSelf],
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          )
                      freshSelfTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshSelf
                      freshResultTy = freshBackendTypeBinderVar expectedResultIdentity actualResultIdentity freshResult
                      normalizeHandler selfIdentity selfName resultIdentity resultName =
                        substituteBackendTypesByKey
                          ( Map.fromList $
                              binderReplacement selfIdentity selfName freshSelfTy
                                ++ binderReplacement resultIdentity resultName freshResultTy
                          )
                   in zipAllWith
                        go
                        (map (normalizeHandler expectedIdentity expectedName expectedResultIdentity expectedResultName) expectedHandlers)
                        (map (normalizeHandler actualIdentity actualName actualResultIdentity actualResultName) actualHandlers)
          _ ->
            False

    structuralDataDeclForMuPair expectedIdentity expectedName actualIdentity actualName =
      case (structuralSelfIdentityUnique expectedIdentity, structuralSelfIdentityUnique actualIdentity) of
        (Just expectedOwner, Just actualOwner)
          | expectedOwner == actualOwner ->
              lookupDataByStructuralSelfIdentity expectedIdentity
        (Nothing, Nothing)
          | referenceMode == BackendTypeMetadataLight -> do
              expectedDataName <- structuralRecursiveDataName expectedName
              actualDataName <- structuralRecursiveDataName actualName
              guard (expectedDataName == actualDataName)
              lookupDataByName expectedDataName
        _ ->
          Nothing

    structuralMuHandlerTypesWithIdentity =
      \case
        BTForallWithIdentity resultIdentity resultName _ handlerTy -> do
          handlers <- collectHandlerTypes resultIdentity resultName handlerTy
          Just (resultIdentity, resultName, handlers)
        _ -> Nothing

    collectHandlerTypes resultIdentity resultName =
      collect []
      where
        collect handlers ty
          | alphaEqBackendType ty (BTVarWithIdentity resultIdentity resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> collect (handlers ++ [handlerTy]) rest
                _ -> Nothing

    binderReplacement identity name replacement =
      [(backendTypeSubstitutionKeyFromMaybeMetadataLight identity name, replacement)]

    maybeBoundaryMatches Nothing Nothing =
      True
    maybeBoundaryMatches (Just expectedBound) (Just actualBound) =
      go expectedBound actualBound
    maybeBoundaryMatches _ _ =
      False

    structuralMuMatchesKnownData dataIdentity base@(BaseTy dataName) args muIdentity muName body =
      structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity
        && ( metadataLightTrusted dataIdentity dataName muIdentity
               && metadataLightStructuralDataMatchesWithIdentity base args muIdentity muName body
               || case matchingDataDecl dataIdentity dataName muIdentity muName of
                 Just dataDecl
                   | structuralMuMatchesSelectedData dataIdentity dataDecl muIdentity muName,
                     Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                       structuralDataDeclarationMatchesSelectedByIdentity dataIdentity dataDecl substitution (BTMuWithIdentity muIdentity muName body)
                 _ ->
                   False
           )

    structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity =
      case (dataIdentity, structuralSelfIdentityUnique muIdentity) of
        (Just identity, Just unique) -> symbolUniqueIdentity identity == unique
        (Just {}, Nothing) -> False
        _ -> True

    structuralSelfIdentityPinsData dataIdentity muIdentity =
      case (dataIdentity, structuralSelfIdentityUnique muIdentity) of
        (Just identity, Just unique) -> symbolUniqueIdentity identity == unique
        _ -> False

    metadataLightTrusted dataIdentity dataName muIdentity =
      case referenceMode of
        BackendTypeIdentityOnly -> False
        BackendTypeMetadataLight ->
          metadataLightAllowed dataIdentity dataName
            || structuralSelfIdentityPinsData dataIdentity muIdentity

    metadataLightAllowed dataIdentity dataName =
      case referenceMode of
        BackendTypeIdentityOnly ->
          False
        BackendTypeMetadataLight ->
          case (dataIdentity, mbDataDecls) of
            (Just {}, _) -> False
            (Nothing, Just (BackendDataScope dataDecls dataDeclsByIdentity)) ->
              not (identityBearingDataName dataName dataDecls)
                && not (identityBearingDataName dataName dataDeclsByIdentity)
            _ -> True

    identityBearingDataName dataName =
      any
        ( \dataDecl ->
            backendDataName dataDecl == dataName
              && backendDataIdentity dataDecl /= Nothing
        )
        . Map.elems

    matchingDataDecl dataIdentity dataName muIdentity muName =
      case dataIdentity >>= lookupDataByIdentity of
        Just dataDecl -> Just dataDecl
        Nothing
          | Just {} <- dataIdentity,
            Just {} <- mbDataDecls ->
              Nothing
          | Just {} <- structuralSelfIdentityUnique muIdentity ->
              lookupDataByStructuralSelfIdentity muIdentity
        Nothing ->
          case referenceMode of
            BackendTypeIdentityOnly -> Nothing
            BackendTypeMetadataLight ->
              lookupDataByName dataName
                <|> (structuralRecursiveDataName muName >>= lookupDataByName)

    structuralSymbolReferenceMode =
      \case
        BackendTypeIdentityOnly -> SymbolIdentityOnly
        BackendTypeMetadataLight -> SymbolMetadataLight

    lookupDataByName name = do
      BackendDataScope dataDecls _ <- mbDataDecls
      Map.lookup name dataDecls

    lookupDataByIdentity identity = do
      BackendDataScope _ dataDeclsByIdentity <- mbDataDecls
      lookupSymbolIdentityExact identity dataDeclsByIdentity

    lookupDataByStructuralSelfIdentity muIdentity = do
      unique <- structuralSelfIdentityUnique muIdentity
      BackendDataScope _ dataDeclsByIdentity <- mbDataDecls
      case
        [ dataDecl
        | dataDecl <- Map.elems dataDeclsByIdentity,
          Just dataIdentity <- [backendDataIdentity dataDecl],
          symbolUniqueIdentity dataIdentity == unique
        ]
        of
        [dataDecl] -> Just dataDecl
        _ -> Nothing

    structuralSelfIdentityUnique muIdentity = do
      identity <- muIdentity
      (unique, StructuralSelfBinder) <- typeBinderIdentityStructural identity
      pure unique

    structuralSelfIdentityMatchesData muIdentity dataDecl =
      case (structuralSelfIdentityUnique muIdentity, backendDataIdentity dataDecl) of
        (Just unique, Just dataDeclIdentity) -> symbolUniqueIdentity dataDeclIdentity == unique
        (Nothing, Nothing) -> True
        _ -> False

    structuralMuMatchesSelectedData dataIdentity dataDecl muIdentity muName =
      case dataIdentity of
        Just identity ->
          backendDataIdentity dataDecl == Just identity
            && structuralSelfIdentityMatchesData muIdentity dataDecl
        Nothing ->
          structuralMuMatchesDataDecl dataDecl muIdentity muName

    structuralDataDeclarationMatchesSelectedByIdentity dataIdentity dataDecl substitution structuralTy =
      case dataIdentity of
        Just identity
          | backendDataIdentity dataDecl == Just identity ->
              structuralDataDeclarationShapeMatches dataDecl substitution structuralTy
        _ ->
          structuralDataDeclarationMatches typeBounds dataDecl substitution structuralTy

    structuralDataDeclarationShapeMatches dataDecl substitution structuralTy =
      case structuralTy of
        BTMuWithIdentity muIdentity muName body ->
          case structuralMuHandlerTypes body of
            Just (resultIdentity, resultName, handlers)
              | length handlers == length (backendDataConstructors dataDecl) ->
                  case structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity muName resultIdentity resultName handlers of
                    Right _ -> True
                    Left _ -> False
            _ -> False
        _ -> False

    freshBinderName leftName rightName leftBound rightBound leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName],
              typeBoundKeyNames typeBounds,
              maybe Set.empty freeBackendTypeVars leftBound,
              maybe Set.empty freeBackendTypeVars rightBound,
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

structuralPayloadsMayInstantiate ::
  BackendParameterBounds ->
  Maybe TypeBinderIdentity ->
  String ->
  BackendType ->
  Maybe TypeBinderIdentity ->
  String ->
  BackendType ->
  Bool
structuralPayloadsMayInstantiate typeBounds expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
  structuralOwnersMatch
    && let freshSelf =
             freshNameLike
               expectedName
               ( Set.unions
                   [ Set.fromList [expectedName, actualName],
                     typeBoundKeyNames typeBounds,
                     freeBackendTypeVars expectedBody,
                     freeBackendTypeVars actualBody
                   ]
               )
           freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshSelf
           freshKey = freshBackendTypeBinderKey expectedIdentity actualIdentity freshSelf
           expectedBody' = substituteBackendTypeForBinder expectedIdentity expectedName freshTy expectedBody
           actualBody' = substituteBackendTypeForBinder actualIdentity actualName freshTy actualBody
        in case (structuralMuPayloadTypes expectedBody', structuralMuPayloadTypes actualBody') of
             (Just expectedPayloadTypes, Just actualPayloadTypes) ->
               structuralPayloadTypesMayInstantiate
                 typeBounds
                 (Set.singleton freshKey)
                 expectedPayloadTypes
                 actualPayloadTypes
             _ ->
               False
  where
    structuralOwnersMatch =
      case (expectedIdentity, actualIdentity) of
        (Just _, Just _) ->
          structuralMuRefsMatchIdentityFirst expectedIdentity expectedName actualIdentity actualName
        (Nothing, Nothing) ->
          structuralRecursiveDataNamesMatch expectedName actualName
        _ ->
          False

structuralMuTypesHaveBinderIdentityMismatch :: BackendType -> BackendType -> Bool
structuralMuTypesHaveBinderIdentityMismatch left right =
  case (left, right) of
    (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
      structuralMuTypesHaveBinderIdentityMismatch leftDom rightDom
        || structuralMuTypesHaveBinderIdentityMismatch leftCod rightCod
    (BTConWithIdentity _ _ leftArgs, BTConWithIdentity _ _ rightArgs) ->
      any (uncurry structuralMuTypesHaveBinderIdentityMismatch) (zip (NE.toList leftArgs) (NE.toList rightArgs))
    (BTVarAppWithIdentity _ _ leftArgs, BTVarAppWithIdentity _ _ rightArgs) ->
      any (uncurry structuralMuTypesHaveBinderIdentityMismatch) (zip (NE.toList leftArgs) (NE.toList rightArgs))
    (BTForallWithIdentity _ _ leftBound leftBody, BTForallWithIdentity _ _ rightBound rightBody) ->
      maybe False (uncurry structuralMuTypesHaveBinderIdentityMismatch) ((,) <$> leftBound <*> rightBound)
        || structuralMuTypesHaveBinderIdentityMismatch leftBody rightBody
    (BTMuWithIdentity leftIdentity leftName leftBody, BTMuWithIdentity rightIdentity rightName rightBody) ->
      structuralMuBinderIdentityMismatch leftIdentity leftName rightIdentity rightName
        || structuralMuTypesHaveBinderIdentityMismatch leftBody rightBody
    _ ->
      False

structuralMuBinderIdentityMismatch :: Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity -> String -> Bool
structuralMuBinderIdentityMismatch expectedIdentity _ actualIdentity _ =
  case (expectedIdentity >>= typeBinderIdentityStructural, actualIdentity >>= typeBinderIdentityStructural) of
    (Nothing, Nothing) -> False
    (Just left, Just right) -> left /= right
    _ -> True

structuralRecursiveDataName :: String -> Maybe String
structuralRecursiveDataName name =
  case stripPrefixSimple "$$identity#" name of
    Just rest -> ("$identity#" ++) <$> stripStructuralSelfSuffix rest
    Nothing ->
      case stripPrefixSimple "$identity#" name of
        Just rest -> ("$identity#" ++) <$> stripStructuralSelfSuffix rest
        Nothing -> stripStructuralSelfSuffix (dropWhile (== '$') name)

stripStructuralSelfSuffix :: String -> Maybe String
stripStructuralSelfSuffix value =
  stripSuffixSimple "_self" (dropWhileEndSimple isDigit value)

structuralMuNameMatches :: String -> String -> Bool
structuralMuNameMatches dataName muName =
  case structuralRecursiveDataName muName of
    Just structuralName -> dataName == structuralName
    Nothing -> False

structuralMuMatchesDataDecl :: BackendData -> Maybe TypeBinderIdentity -> String -> Bool
structuralMuMatchesDataDecl dataDecl muIdentity muName =
  case backendDataIdentity dataDecl of
    Just dataIdentity ->
      structuralMuIdentityMatches (Just dataIdentity) muIdentity
    Nothing ->
      structuralIdentityAllowsNameFallback muIdentity
        && structuralMuNameMatches (backendDataName dataDecl) muName

structuralMuIdentityMatches :: Maybe SymbolIdentity -> Maybe TypeBinderIdentity -> Bool
structuralMuIdentityMatches (Just dataIdentity) (Just identity)
  | Just (unique, StructuralSelfBinder) <- typeBinderIdentityStructural identity =
      unique == symbolUniqueIdentity dataIdentity
structuralMuIdentityMatches _ _ =
  False

structuralMuAsDataType :: Maybe SymbolIdentity -> [BackendDataParameterRef] -> Maybe TypeBinderIdentity -> String -> Maybe BackendType
structuralMuAsDataType dataIdentity dataParameterRefs muIdentity muName = do
  guard (structuralMuIdentityCompatible dataIdentity muIdentity)
  dataName <-
    case dataIdentity of
      Just identity -> Just (symbolDefiningName identity)
      Nothing -> structuralRecursiveDataName muName
  let parameterArgs = map backendDataParameterRefType dataParameterRefs
  Just $
    case parameterArgs of
      [] -> BTBaseWithIdentity dataIdentity (BaseTy dataName)
      arg : rest -> BTConWithIdentity dataIdentity (BaseTy dataName) (arg :| rest)

structuralMuAsActualDataType :: Maybe SymbolIdentity -> Maybe TypeBinderIdentity -> String -> BackendType -> Maybe BackendType
structuralMuAsActualDataType dataIdentity muIdentity muName actual =
  case actual of
    BTBaseWithIdentity actualIdentity (BaseTy actualName)
      | structuralMuHeadMatches dataIdentity actualIdentity actualName muIdentity muName -> Just actual
    BTConWithIdentity actualIdentity (BaseTy actualName) _
      | structuralMuHeadMatches dataIdentity actualIdentity actualName muIdentity muName -> Just actual
    _ -> Nothing
  where
    structuralMuHeadMatches (Just expected) (Just actualIdentity) _ identity _ =
      expected == actualIdentity
        && structuralMuIdentityCompatible (Just expected) identity
    structuralMuHeadMatches Nothing (Just actualIdentity) _ (Just identity) _ =
      structuralMuIdentityMatches (Just actualIdentity) (Just identity)
    structuralMuHeadMatches Nothing Nothing actualName identity structuralName =
      structuralIdentityAllowsNameFallback identity
        && structuralMuNameMatches actualName structuralName
    structuralMuHeadMatches _ _ _ _ _ =
      False

structuralMuIdentityCompatible :: Maybe SymbolIdentity -> Maybe TypeBinderIdentity -> Bool
structuralMuIdentityCompatible (Just dataIdentity) (Just muIdentity) =
  structuralMuIdentityMatches (Just dataIdentity) (Just muIdentity)
structuralMuIdentityCompatible (Just {}) Nothing =
  False
structuralMuIdentityCompatible _ _ =
  True

isGeneratedTypeBinder :: TypeBinderIdentity -> Bool
isGeneratedTypeBinder =
  maybe False (const True) . typeBinderIdentityGeneratedUnique

nominalBackendDataIdentity :: BackendType -> Maybe SymbolIdentity
nominalBackendDataIdentity =
  \case
    BTBaseWithIdentity identity _ -> identity
    BTConWithIdentity identity _ _ -> identity
    _ -> Nothing

structuralMuPayloadTypes :: BackendType -> Maybe [BackendType]
structuralMuPayloadTypes body =
  concat <$> structuralBackendHandlerFields body

structuralMuHandlerTypes :: BackendType -> Maybe (Maybe TypeBinderIdentity, String, [BackendType])
structuralMuHandlerTypes =
  \case
    BTForallWithIdentity resultIdentity resultName _ handlerTy -> do
      handlers <- collectHandlerTypes resultIdentity resultName handlerTy
      Just (resultIdentity, resultName, handlers)
    _ -> Nothing
  where
    collectHandlerTypes resultIdentity resultName =
      go []
      where
        go handlers ty
          | backendTypeVarMatches resultIdentity resultName ty = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> go (handlers ++ [handlerTy]) rest
                _ -> Nothing

structuralBackendHandlerFields :: BackendType -> Maybe [[BackendType]]
structuralBackendHandlerFields =
  \case
    BTForallWithIdentity resultIdentity resultName _ handlerTy -> collectHandlers resultIdentity resultName handlerTy
    _ -> Nothing
  where
    collectHandlers resultIdentity resultName =
      go []
      where
        go handlers ty
          | backendTypeVarMatches resultIdentity resultName ty = Just handlers
          | otherwise =
              case ty of
                BTForall _ _ body -> go handlers body
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultIdentity resultName handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultIdentity resultName =
      go []
      where
        go fields ty
          | backendTypeVarMatches resultIdentity resultName ty = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

structuralDataArgumentSubstitution :: BackendData -> [BackendType] -> Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
structuralDataArgumentSubstitution dataDecl args
  | length dataParameterRefs == length args =
      Just (Map.fromList (zip (backendDataParameterKeys dataDecl) args))
  | otherwise =
      Nothing
  where
    dataParameterRefs =
      backendDataParameterRefs dataDecl

structuralPayloadHandlersMatchForData ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Maybe TypeBinderIdentity ->
  String ->
  Maybe TypeBinderIdentity ->
  String ->
  [BackendType] ->
  Either StructuralRecursiveDataMismatch [[BackendType]]
structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity muName resultIdentity resultName handlers =
  traverse constructorHandlerMatches (zip constructors handlers)
  where
    dataName =
      backendDataName dataDecl
    dataParameterRefs =
      backendDataParameterRefs dataDecl
    constructors =
      backendDataConstructors dataDecl
    dataParameterKeySet =
      Set.fromList (backendDataParameterKeys dataDecl)
    dataSubstitution =
      Map.filterWithKey (\key _ -> Set.member key dataParameterKeySet) substitution
    structuralTyWithData =
      substituteBackendTypesByKey dataSubstitution structuralTy
    structuralSelfTy =
      case (muIdentity >>= typeBinderIdentityGeneratedUnique, backendDataIdentity dataDecl) of
        (Just {}, dataIdentity@(Just {})) ->
          case map (substituteBackendTypesByKey dataSubstitution . backendDataParameterRefType) dataParameterRefs of
            [] -> BTBaseWithIdentity dataIdentity (BaseTy dataName)
            arg : args -> BTConWithIdentity dataIdentity (BaseTy dataName) (arg :| args)
        _ -> structuralTyWithData
    substituteKnownTypes =
      substituteStructuralSelfExact muIdentity muName structuralSelfTy . substituteBackendTypesByKey dataSubstitution
    constructorHandlerMatches (constructor, handlerTy) =
      case
        matchBackendTypeParametersWithTypeBounds
          typeBounds
          dataParameterRefs
          parameters
          Map.empty
          expectedHandlerTy
          actualHandlerTy
        of
          Just _ ->
            handlerFieldsOrPayloadError
          Nothing ->
            if structuralPayloadTypeMayInstantiate typeBounds protectedBinders expectedHandlerTy actualHandlerTy
              then handlerFieldsOrPayloadError
              else
                Left
                  ( StructuralRecursiveDataConstructorPayloadMismatch
                      dataName
                      (backendConstructorName constructor)
                      expectedHandlerTy
                      actualHandlerTy
                  )
      where
        expectedHandlerTy =
          substituteKnownTypes (constructorStructuralHandlerType resultIdentity resultName constructor)
        actualHandlerTy =
          substituteKnownTypes handlerTy
        protectedBinders =
          Set.insert (backendTypeSubstitutionKeyFromMaybeMetadataLight resultIdentity resultName) $
            case muIdentity >>= typeBinderIdentityStructural of
              Just {} -> Set.singleton (backendTypeSubstitutionKeyFromMaybeMetadataLight muIdentity muName)
              Nothing -> Set.empty
        handlerFieldsOrPayloadError =
          case structuralHandlerFields resultIdentity resultName actualHandlerTy of
            Just fields -> Right fields
            Nothing -> Left (StructuralRecursiveDataPayloadUnavailable dataName)
        parameters =
          Map.map (fmap substituteKnownTypes) $
            constructorTypeParameterBoundsForData dataDecl constructor

substituteStructuralSelfExact :: Maybe TypeBinderIdentity -> String -> BackendType -> BackendType -> BackendType
substituteStructuralSelfExact targetIdentity targetName replacement =
  go
  where
    go =
      \case
        BTVarWithIdentity identity name
          | targetBinderMatches identity name -> replacement
          | otherwise -> BTVarWithIdentity identity name
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        ty@BTBaseWithIdentity {} ->
          ty
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap go args)
        BTVarAppWithIdentity identity name args
          | targetBinderMatches identity name ->
              case applyExactReplacement (NE.toList (fmap go args)) of
                Just ty -> ty
                Nothing -> BTVarAppWithIdentity identity name (fmap go args)
          | otherwise -> BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mbBound body ->
          let mbBound' = fmap go mbBound
           in if targetBinderMatches identity name
                then BTForallWithIdentity identity name mbBound' body
                else BTForallWithIdentity identity name mbBound' (go body)
        BTMuWithIdentity identity name body
          | targetBinderMatches identity name ->
              BTMuWithIdentity identity name body
          | otherwise ->
              BTMuWithIdentity identity name (go body)
        BTBottom ->
          BTBottom

    targetBinderMatches identity name =
      identity == targetIdentity
        && (name == targetName || maybe False (not . isGeneratedTypeBinder) targetIdentity)

    applyExactReplacement args =
      case (replacement, args) of
        (_, []) -> Just replacement
        (BTVarWithIdentity identity name, arg : rest) -> Just (BTVarAppWithIdentity identity name (arg :| rest))
        (BTBaseWithIdentity identity base, arg : rest) -> Just (BTConWithIdentity identity base (arg :| rest))
        (BTConWithIdentity identity base existing, _) -> Just (BTConWithIdentity identity base (existing <> NE.fromList args))
        (BTVarAppWithIdentity identity name existing, _) -> Just (BTVarAppWithIdentity identity name (existing <> NE.fromList args))
        _ -> Nothing

constructorStructuralHandlerType :: Maybe TypeBinderIdentity -> String -> BackendConstructor -> BackendType
constructorStructuralHandlerType resultIdentity resultName constructor =
  foldr wrapForall handlerBody (backendConstructorForalls constructor)
  where
    handlerBody =
      foldr BTArrow (BTVarWithIdentity resultIdentity resultName) (backendConstructorFields constructor)

    wrapForall binder body =
      BTForallWithIdentity
        (backendTypeBinderIdentity binder)
        (backendTypeBinderName binder)
        (backendTypeBinderBound binder)
        body

structuralHandlerFields :: Maybe TypeBinderIdentity -> String -> BackendType -> Maybe [BackendType]
structuralHandlerFields resultIdentity resultName =
  go []
  where
    go fields ty
      | backendTypeVarMatches resultIdentity resultName ty = Just fields
      | otherwise =
          case ty of
            BTForall _ _ body -> go fields body
            BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
            _ -> Nothing

constructorTypeParameterBoundsForData :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataDecl constructor =
  Map.fromList $
    [(key, Nothing) | key <- backendDataParameterKeys dataDecl]
      ++ [ (backendTypeSubstitutionKeyFromMaybeMetadataLight (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

matchConstructorResult ::
  [BackendDataParameterRef] ->
  Set.Set BackendTypeSubstitutionKey ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
matchConstructorResult dataParameterOrder parameters substitution expected actual =
  case expected of
    BTVarWithIdentity identity name
      | Set.member key parameters ->
          case Map.lookup key substitution of
            Nothing -> Just (Map.insert key actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Just substitution
              | otherwise -> Nothing
      where
        key = parameterKey identity name
    _ ->
      if alphaEqBackendType expected actual
        then Just substitution
        else
            ( case (expected, actual) of
              (BTVarWithIdentity expectedIdentity expectedName, BTVarWithIdentity actualIdentity actualName)
                | typeBinderRefMatchesWith BackendTypeMetadataLight expectedIdentity expectedName actualIdentity actualName ->
                    Just substitution
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedDom actualDom
                  >>= \subst -> matchConstructorResult dataParameterOrder parameters subst expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase)
                | backendTypeHeadMatchesWith SymbolMetadataLight expectedIdentity expectedBase actualIdentity actualBase -> Just substitution
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
                | backendTypeHeadMatchesWith SymbolMetadataLight expectedIdentity expectedCon actualIdentity actualCon && length expectedArgs == length actualArgs ->
                    foldM
                      (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
                      substitution
                      (zip (NE.toList expectedArgs) (NE.toList actualArgs))
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected expectedIdentity expectedName expectedBody actualTy
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected expectedIdentity expectedName expectedBody actualTy
              (expectedTy@(BTBase {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
                matchStructuralMuActual expectedTy actualIdentity actualName actualBody
              (expectedTy@(BTCon {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
                matchStructuralMuActual expectedTy actualIdentity actualName actualBody
              (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, _) ->
                matchConstructorResultApplication dataParameterOrder parameters substitution expectedIdentity expectedName (NE.toList expectedArgs) actual
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) -> do
                subst <-
                  case (expectedBound, actualBound) of
                    (Nothing, Nothing) -> Just substitution
                    (Just expectedBoundTy, Just actualBoundTy) -> matchConstructorResult dataParameterOrder parameters substitution expectedBoundTy actualBoundTy
                    _ -> Nothing
                matchConstructorResult dataParameterOrder parameters subst expectedBody (substituteBackendTypeForBinder actualIdentity actualName (BTVarWithIdentity expectedIdentity expectedName) actualBody)
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedBody (substituteBackendTypeForBinder actualIdentity actualName (BTVarWithIdentity expectedIdentity expectedName) actualBody)
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                Nothing
          )
  where
    parameterKey identity name =
      backendTypeSubstitutionKeyFromMaybeMetadataLight identity name

    matchStructuralMuExpected muIdentity muName _body actualTy =
      let dataIdentity = nominalBackendDataIdentity actualTy
       in firstJust
            [ structuralMuAsDataType dataIdentity dataParameterOrder muIdentity muName
                >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
              structuralMuAsActualDataType dataIdentity muIdentity muName actualTy
                >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
            ]

    matchStructuralMuActual expectedTy muIdentity muName _body =
      let dataIdentity = nominalBackendDataIdentity expectedTy
       in firstJust
            [ structuralMuAsDataType dataIdentity dataParameterOrder muIdentity muName
                >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
              structuralMuAsActualDataType dataIdentity muIdentity muName expectedTy
                >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
            ]

matchConstructorResultApplication ::
  [BackendDataParameterRef] ->
  Set.Set BackendTypeSubstitutionKey ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  Maybe TypeBinderIdentity ->
  String ->
  [BackendType] ->
  BackendType ->
  Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
matchConstructorResultApplication dataParameterOrder parameters substitution identity name expectedArgs actual =
  case decomposeBackendTypeHead actual of
    Just (actualHead, actualArgs)
      | length expectedArgs == length actualArgs -> do
          substitution' <-
            if Set.member key parameters
              then insertParameterSubstitution key actualHead substitution
              else matchConstructorResult dataParameterOrder parameters substitution (BTVarWithIdentity identity name) actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
    _ -> Nothing
  where
    key =
      backendTypeSubstitutionKeyFromMaybeMetadataLight identity name

    insertParameterSubstitution paramKey actualHead substitution0 =
      case Map.lookup paramKey substitution0 of
        Nothing -> Just (Map.insert paramKey actualHead substitution0)
        Just previous
          | parameterPlaceholderMatchesKey paramKey previous ->
              Just (Map.insert paramKey actualHead substitution0)
        Just previous
          | alphaEqBackendType previous actualHead -> Just substitution0
          | otherwise -> Nothing
    parameterPlaceholderMatchesKey paramKey =
      \case
        BTVarWithIdentity (Just binderIdentity) _ ->
          backendTypeSubstitutionKeyFromIdentity binderIdentity == paramKey
        _ ->
          False

matchBackendTypeParametersWithTypeBounds ::
  BackendParameterBounds ->
  [BackendDataParameterRef] ->
  BackendParameterBounds ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
matchBackendTypeParametersWithTypeBounds typeBounds dataParameterOrder parameterBounds =
  go Set.empty
  where
    matchParameterKey identity name =
      case identity of
        Just {} ->
          if Map.member key parameterBounds
            then Just key
            else Nothing
        Nothing
          | Map.member key parameterBounds -> Just key
          | otherwise -> Nothing
      where
        key = backendTypeSubstitutionKeyFromMaybeMetadataLight identity name

    go bound substitution expected actual =
      case expected of
        BTVarWithIdentity identity name
          | Just key <- matchParameterKey identity name,
            Set.notMember key bound ->
              insertParameterSubstitution key actual substitution
        _ ->
          case (expected, actual) of
            (BTVar {}, _) ->
              requireAlphaEq substitution expected actual
            (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
              go bound substitution expectedDom actualDom
                >>= \substitution' -> go bound substitution' expectedCod actualCod
            (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase)
              | backendTypeHeadMatchesWith SymbolMetadataLight expectedIdentity expectedBase actualIdentity actualBase ->
                  Just substitution
            (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
              | backendTypeHeadMatchesWith SymbolMetadataLight expectedIdentity expectedCon actualIdentity actualCon ->
                  foldM
                    ( \(substitutionAcc, matched) (expectedArg, actualArg) ->
                        if matched
                          then fmap (\substitutionNext -> (substitutionNext, True)) (go bound substitutionAcc expectedArg actualArg)
                          else Just (substitutionAcc, False)
                    )
                    (substitution, length expectedArgsList == length actualArgsList)
                    (zip expectedArgsList actualArgsList)
                    >>= \(substitution', matched) ->
                      if matched
                        then Just substitution'
                        else Nothing
              where
                expectedArgsList = NE.toList expectedArgs
                actualArgsList = NE.toList actualArgs
            (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTBase {})) ->
              matchStructuralMuExpected bound substitution expectedIdentity expectedName expectedBody actualTy
            (BTMuWithIdentity expectedIdentity expectedName expectedBody, actualTy@(BTCon {})) ->
              matchStructuralMuExpected bound substitution expectedIdentity expectedName expectedBody actualTy
            (expectedTy@(BTBase {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
              matchStructuralMuActual bound substitution expectedTy actualIdentity actualName actualBody
            (expectedTy@(BTCon {}), BTMuWithIdentity actualIdentity actualName actualBody) ->
              matchStructuralMuActual bound substitution expectedTy actualIdentity actualName actualBody
            (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, _) ->
              matchBackendTypeApplication bound substitution expectedIdentity expectedName (NE.toList expectedArgs) actual
            (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) -> do
              substitution' <- matchMaybeBound bound substitution expectedBound actualBound
              let used =
                    Set.unions
                      [ Set.fromList [expectedName, actualName],
                        Set.map backendTypeSubstitutionKeyName (Map.keysSet substitution'),
                        freeBackendTypeVarsInKeyed substitution',
                        Set.map backendTypeSubstitutionKeyName (Map.keysSet parameterBounds),
                        freeBackendTypeVars expectedBody,
                        freeBackendTypeVars actualBody,
                        maybe Set.empty freeBackendTypeVars expectedBound,
                        maybe Set.empty freeBackendTypeVars actualBound
                      ]
                  freshName = freshNameLike expectedName used
                  freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshName
                  freshKey = freshBackendTypeBinderKey expectedIdentity actualIdentity freshName
                  expectedBody' = substituteBinder expectedIdentity expectedName freshTy expectedBody
                  actualBody' = substituteBinder actualIdentity actualName freshTy actualBody
              go (Set.insert freshKey bound) substitution' expectedBody' actualBody'
            (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
              if structuralMuBinderIdentityMismatch expectedIdentity expectedName actualIdentity actualName
                then Nothing
                else
                  case
                    ( null dataParameterOrder && sameStructuralDataRef expectedIdentity expectedName actualIdentity actualName,
                      isVacuousRecursiveBinderWithIdentity expectedIdentity expectedName expectedBody,
                      isVacuousRecursiveBinderWithIdentity actualIdentity actualName actualBody
                    )
                  of
                    (True, _, _) ->
                      Just substitution
                    (_, True, True) ->
                      go bound substitution expectedBody actualBody
                    (_, True, False)
                      | recursiveBodyCompatibleWithIdentity actualIdentity actualName actualBody expectedBody
                          && expectedBodyHasNoParameters expectedBody ->
                          Just substitution
                      | expectedBodyHasNoParameters expectedBody ->
                          Nothing
                      | otherwise ->
                          go bound substitution expectedBody actual
                    (_, False, True)
                      | recursiveBodyCompatibleWithIdentity expectedIdentity expectedName expectedBody actualBody
                          && expectedBodyHasNoParameters expectedBody ->
                          Just substitution
                      | expectedBodyHasNoParameters expectedBody ->
                          Nothing
                      | otherwise ->
                          go bound substitution expected actualBody
                    (_, False, False) -> do
                      let used =
                            Set.unions
                              [ Set.fromList [expectedName, actualName],
                                Set.map backendTypeSubstitutionKeyName (Map.keysSet substitution),
                                freeBackendTypeVarsInKeyed substitution,
                                Set.map backendTypeSubstitutionKeyName (Map.keysSet parameterBounds),
                                freeBackendTypeVars expectedBody,
                                freeBackendTypeVars actualBody
                              ]
                          freshName = freshNameLike expectedName used
                          freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshName
                          freshKey = freshBackendTypeBinderKey expectedIdentity actualIdentity freshName
                          expectedBody' = substituteBinder expectedIdentity expectedName freshTy expectedBody
                          actualBody' = substituteBinder actualIdentity actualName freshTy actualBody
                      go (Set.insert freshKey bound) substitution expectedBody' actualBody'
            (BTMuWithIdentity expectedIdentity expectedName expectedBody, _)
              | isVacuousRecursiveBinderWithIdentity expectedIdentity expectedName expectedBody ->
                  go bound substitution expectedBody actual
            (_, BTMuWithIdentity actualIdentity actualName actualBody)
              | isVacuousRecursiveBinderWithIdentity actualIdentity actualName actualBody ->
                  go bound substitution expected actualBody
            (BTBottom, BTBottom) ->
              Just substitution
            _ ->
              if alphaEqBackendType expected actual
                then Just substitution
                else Nothing
    matchMaybeBound _ substitution Nothing Nothing =
      Just substitution
    matchMaybeBound bound substitution (Just expectedBound) (Just actualBound) =
      go bound substitution expectedBound actualBound
    matchMaybeBound _ _ _ _ =
      Nothing

    matchStructuralMuExpected bound substitution muIdentity muName body actualTy =
      let dataIdentity = nominalBackendDataIdentity actualTy
       in firstJust
            [ structuralMuNominalTypeMatches actualTy muIdentity muName body >>= \() -> Just substitution,
              structuralMuAsDataTypeForBody dataIdentity muIdentity muName body
                >>= \expectedTy -> go bound substitution expectedTy actualTy,
              structuralMuAsActualDataType dataIdentity muIdentity muName actualTy
                >>= \expectedTy -> go bound substitution expectedTy actualTy
            ]

    matchStructuralMuActual bound substitution expectedTy muIdentity muName body =
      let dataIdentity = nominalBackendDataIdentity expectedTy
       in firstJust
            [ structuralMuNominalTypeMatches expectedTy muIdentity muName body >>= \() -> Just substitution,
              structuralMuAsDataTypeForBody dataIdentity muIdentity muName body
                >>= \actualTy -> go bound substitution expectedTy actualTy,
              structuralMuAsActualDataType dataIdentity muIdentity muName expectedTy
                >>= \actualTy -> go bound substitution expectedTy actualTy
            ]

    structuralMuAsDataTypeForBody dataIdentity muIdentity muName body =
      structuralMuPayloadTypes body *> structuralMuAsDataType dataIdentity dataParameterOrder muIdentity muName

    structuralMuNominalTypeMatches nominalTy muIdentity muName body =
      if nominalMatches
        then Just ()
        else Nothing
      where
        nominalMatches =
          case nominalTy of
            BTBaseWithIdentity identity base ->
              metadataLightStructuralDataMatchesAgainstHead identity base [] muIdentity muName body
            BTConWithIdentity identity base args ->
              metadataLightStructuralDataMatchesAgainstHead identity base (NE.toList args) muIdentity muName body
            _ ->
              False

    matchBackendTypeApplication bound substitution identity name expectedArgs actual =
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                case matchParameterKey identity name of
                  Just key
                    | Set.notMember key bound ->
                        insertParameterSubstitution key actualHead substitution
                  _ ->
                    go bound substitution (BTVarWithIdentity identity name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go bound substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    requireAlphaEq substitution expected actual
      | alphaEqBackendType expected actual = Just substitution
      | otherwise = Nothing

    insertParameterSubstitution key actual substitution =
      case Map.lookup key substitution of
        Nothing ->
          if backendParameterBoundMatches key actual substitution
            then Just (Map.insert key actual substitution)
            else Nothing
        Just previous
          | parameterPlaceholderMatchesKey key previous,
            backendParameterBoundMatches key actual substitution ->
              Just (Map.insert key actual substitution)
        Just previous
          | repeatedParameterTypeMatches previous actual && backendParameterBoundMatches key previous substitution ->
              Just substitution
        _ ->
          Nothing

    parameterPlaceholderMatchesKey key =
      \case
        BTVarWithIdentity identity name ->
          matchParameterKey identity name == Just key
        _ ->
          False

    repeatedParameterTypeMatches previous actual =
      not (structuralMuTypesHaveBinderIdentityMismatch previous actual)
        && (alphaEqBackendType previous actual || metadataLightSameStructuralType previous actual)

    metadataLightSameStructuralType left right =
      case (left, right) of
        (BTMuWithIdentity leftIdentity leftName leftBody, BTMuWithIdentity rightIdentity rightName rightBody)
          | not (structuralMuRefsMatchIdentityFirst leftIdentity leftName rightIdentity rightName) ->
              False
          | otherwise ->
              case (leftIdentity, rightIdentity) of
                (Just leftOwner, Just rightOwner) ->
                  identityPayloadMatches leftOwner leftName leftBody
                    && identityPayloadMatches rightOwner rightName rightBody
                (Nothing, Nothing) ->
                  case (structuralRecursiveDataName leftName, structuralRecursiveDataName rightName) of
                    (Just leftDataName, Just rightDataName)
                      | leftDataName == rightDataName ->
                          metadataLightStructuralDataMatchesWithIdentity (BaseTy leftDataName) [] Nothing leftName leftBody
                            && metadataLightStructuralDataMatchesWithIdentity (BaseTy rightDataName) [] Nothing rightName rightBody
                    _ ->
                      False
                _ ->
                  False
        _ ->
          False

    identityPayloadMatches identity name body =
      case matchStructuralDataPayload (typeBinderIdentityStableName identity) [] (Just identity) name body of
        Right _ -> True
        Left _ -> False

    backendParameterBoundMatches key actual substitution =
      case Map.lookup key parameterBounds of
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete key parameterBounds)
                      (Map.delete key substitution)
                  expectedBound = substituteBackendTypesByKey dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVarWithIdentity actualIdentity actualName ->
          case lookupTypeBound actualIdentity actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypesByKey resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution typeBounds Map.empty

    expectedBodyHasNoParameters expectedBody =
      Set.null (freeBackendTypeVarKeys expectedBody `Set.intersection` Map.keysSet parameterBounds)

    sameStructuralDataRef expectedIdentity expectedName actualIdentity actualName =
      structuralMuRefsMatchIdentityFirst expectedIdentity expectedName actualIdentity actualName

    substituteBinder identity name replacement =
      substituteBackendTypesByKey (Map.fromList (binderReplacement identity name replacement))

    binderReplacement identity name replacement =
      [(backendTypeSubstitutionKeyFromMaybeMetadataLight identity name, replacement)]

completeBackendParameterSubstitution :: BackendParameterBounds -> Map.Map BackendTypeSubstitutionKey BackendType -> Map.Map BackendTypeSubstitutionKey BackendType
completeBackendParameterSubstitution parameterBounds substitution0 =
  resolveDefaultedBounds defaultedNames substitution1
  where
    substitution1 =
      foldl insertBoundDefault substitution0 (Map.toList parameterBounds)

    defaultedNames =
      Set.fromList
        [ name
          | (name, Just boundTy) <- Map.toList parameterBounds,
            Map.notMember name substitution0,
            not (alphaEqBackendType boundTy BTBottom)
        ]

    insertBoundDefault substitution (name, Just boundTy)
      | Map.member name substitution = substitution
      | alphaEqBackendType boundTy BTBottom = substitution
      | otherwise = Map.insert name (substituteBackendTypesByKey substitution boundTy) substitution
    insertBoundDefault substitution _ =
      substitution

    resolveDefaultedBounds names =
      go (Set.size names + Map.size parameterBounds + 1)
      where
        go remaining substitution
          | remaining <= 0 = substitution
          | substitution' == substitution = substitution
          | otherwise = go (remaining - 1) substitution'
          where
            substitution' =
              foldl resolveDefaultedBound substitution (Set.toList names)

    resolveDefaultedBound substitution name =
      case Map.lookup name substitution of
        Just ty ->
          Map.insert name (substituteBackendTypesByKey (Map.delete name substitution) ty) substitution
        Nothing ->
          substitution

completeDataParameterSubstitution :: BackendData -> Map.Map BackendTypeSubstitutionKey BackendType -> Map.Map BackendTypeSubstitutionKey BackendType
completeDataParameterSubstitution _ substitution =
  substitution

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVarWithIdentity identity name -> Just (BTVarWithIdentity identity name, [])
    BTBaseWithIdentity identity name -> Just (BTBaseWithIdentity identity name, [])
    BTConWithIdentity identity name args -> Just (BTBaseWithIdentity identity name, NE.toList args)
    BTVarAppWithIdentity identity name args -> Just (BTVarWithIdentity identity name, NE.toList args)
    _ -> Nothing

isVacuousRecursiveBinderWithIdentity :: Maybe TypeBinderIdentity -> String -> BackendType -> Bool
isVacuousRecursiveBinderWithIdentity identity name body =
  Set.notMember (backendTypeSubstitutionKeyFromMaybeMetadataLight identity name) (freeBackendTypeVarKeys body)

recursiveBodyCompatibleWithIdentity :: Maybe TypeBinderIdentity -> String -> BackendType -> BackendType -> Bool
recursiveBodyCompatibleWithIdentity recursiveIdentity recursiveName recursiveBody plainBody =
  case go Set.empty Map.empty Nothing recursiveBody plainBody of
    Just _ -> True
    Nothing -> False
  where
    go patternVars patternBindings recursiveAlias leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVarWithIdentity identity name, _)
          | typeBinderRefMatchesWith BackendTypeMetadataLight identity name recursiveIdentity recursiveName ->
              matchRecursiveAlias patternVars patternBindings recursiveAlias rightTy
          | Set.member (recursiveKey identity name) patternVars ->
              matchPatternVar (recursiveKey identity name) patternBindings recursiveAlias rightTy
        (BTVarWithIdentity leftIdentity leftName, BTVarWithIdentity rightIdentity rightName)
          | typeBinderRefMatchesWith BackendTypeMetadataLight leftIdentity leftName rightIdentity rightName ->
              Just (patternBindings, recursiveAlias)
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go patternVars patternBindings recursiveAlias leftDom rightDom
            >>= \(patternBindings', recursiveAlias') ->
              go patternVars patternBindings' recursiveAlias' leftCod rightCod
        (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase)
          | backendTypeHeadMatchesWith SymbolMetadataLight leftIdentity leftBase rightIdentity rightBase ->
              Just (patternBindings, recursiveAlias)
        (BTConWithIdentity leftIdentity leftCon leftArgs, BTConWithIdentity rightIdentity rightCon rightArgs)
          | backendTypeHeadMatchesWith SymbolMetadataLight leftIdentity leftCon rightIdentity rightCon ->
              foldM
                ( \(patternBindingsAcc, recursiveAliasAcc) (leftArg, rightArg) ->
                    go patternVars patternBindingsAcc recursiveAliasAcc leftArg rightArg
                )
                (patternBindings, recursiveAlias)
                (zip (NE.toList leftArgs) (NE.toList rightArgs))
                >>= \(patternBindings', recursiveAlias') ->
                  if length leftArgs == length rightArgs
                    then Just (patternBindings', recursiveAlias')
                    else Nothing
        (BTForallWithIdentity leftIdentity leftName Nothing leftBody, BTForallWithIdentity rightIdentity rightName Nothing rightBody) ->
          let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
              freshTy = freshBackendTypeBinderVar leftIdentity rightIdentity freshName
              leftBody' = substituteBackendTypeForBinder leftIdentity leftName freshTy leftBody
              rightBody' = substituteBackendTypeForBinder rightIdentity rightName freshTy rightBody
           in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForallWithIdentity leftIdentity leftName (Just leftBound) leftBody, BTForallWithIdentity rightIdentity rightName (Just rightBound) rightBody)
          | alphaEqBackendType leftBound rightBound ->
              let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
                  freshTy = freshBackendTypeBinderVar leftIdentity rightIdentity freshName
                  leftBody' = substituteBackendTypeForBinder leftIdentity leftName freshTy leftBody
                  rightBody' = substituteBackendTypeForBinder rightIdentity rightName freshTy rightBody
               in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForallWithIdentity leftIdentity leftName Nothing leftBody, _) ->
          go (Set.insert (backendTypeSubstitutionKeyFromMaybeMetadataLight leftIdentity leftName) patternVars) patternBindings recursiveAlias leftBody rightTy
        (_, BTForallWithIdentity rightIdentity rightName Nothing rightBody)
          | Set.member recursiveBinderKey (freeBackendTypeVarKeys leftTy) ->
              let aliasName = freshNameLike rightName (freeBackendTypeVars leftTy `Set.union` freeBackendTypeVars rightBody)
                  aliasTy = BTVarWithIdentity rightIdentity aliasName
                  aliasKey = backendTypeSubstitutionKeyFromMaybeMetadataLight rightIdentity aliasName
                  rightBody' = substituteBackendTypeForBinder rightIdentity rightName aliasTy rightBody
               in case recursiveAlias of
                    Nothing ->
                      go patternVars patternBindings (Just aliasKey) leftTy rightBody'
                    Just previous
                      | previous == aliasKey ->
                          go patternVars patternBindings recursiveAlias leftTy rightBody'
                    _ ->
                      Nothing
        (BTBottom, BTBottom) ->
          Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    recursiveBinderKey =
      backendTypeSubstitutionKeyFromMaybeMetadataLight recursiveIdentity recursiveName

    recursiveKey identity name =
      backendTypeSubstitutionKeyFromMaybeMetadataLight identity name

    matchPatternVar key patternBindings recursiveAlias rightTy =
      case Map.lookup key patternBindings of
        Nothing ->
          Just (Map.insert key rightTy patternBindings, recursiveAlias)
        Just previous
          | alphaEqBackendType previous rightTy ->
              Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    matchRecursiveAlias patternVars patternBindings recursiveAlias rightTy =
      case rightTy of
        BTForallWithIdentity rightIdentity rightName Nothing rightBody ->
          let aliasName = freshNameLike rightName (freeBackendTypeVars rightBody)
              aliasTy = BTVarWithIdentity rightIdentity aliasName
              aliasKey = backendTypeSubstitutionKeyFromMaybeMetadataLight rightIdentity aliasName
              rightBody' = substituteBackendTypeForBinder rightIdentity rightName aliasTy rightBody
           in case recursiveAlias of
                Nothing ->
                  matchRecursiveAlias patternVars patternBindings (Just aliasKey) rightBody'
                Just expectedKey
                  | expectedKey == aliasKey ->
                      matchRecursiveAlias patternVars patternBindings recursiveAlias rightBody'
                _ ->
                  Nothing
        BTVarWithIdentity rightIdentity rightName ->
          let rightKey = backendTypeSubstitutionKeyFromMaybeMetadataLight rightIdentity rightName
           in case recursiveAlias of
                Nothing ->
                  Just (patternBindings, Just rightKey)
                Just expectedKey
                  | expectedKey == rightKey ->
                      Just (patternBindings, recursiveAlias)
                _ ->
                  Nothing
        _ ->
          Nothing

    freshRecursiveBodyBinder leftName rightName leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName, recursiveName],
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

structuralPayloadTypesMayInstantiate ::
  BackendParameterBounds ->
  Set.Set BackendTypeSubstitutionKey ->
  [BackendType] ->
  [BackendType] ->
  Bool
structuralPayloadTypesMayInstantiate typeBounds bound expectedPayloadTypes actualPayloadTypes =
  zipAllWith
    (structuralPayloadTypeMayInstantiate typeBounds bound)
    expectedPayloadTypes
    actualPayloadTypes

structuralPayloadTypeMayInstantiate ::
  BackendParameterBounds ->
  Set.Set BackendTypeSubstitutionKey ->
  BackendType ->
  BackendType ->
  Bool
structuralPayloadTypeMayInstantiate typeBounds bound expected actual =
  metadataLightPayloadTypeMatches expected actual
    || case (expected, actual) of
      (BTVarWithIdentity identity name, _)
        | Set.notMember key bound && Map.notMember key typeBounds ->
            True
        where
          key = backendTypeSubstitutionKeyFromMaybeMetadataLight identity name
      (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
        structuralPayloadTypeMayInstantiate typeBounds bound expectedDom actualDom
          && structuralPayloadTypeMayInstantiate typeBounds bound expectedCod actualCod
      (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
        backendTypeHeadMatchesWith SymbolMetadataLight expectedIdentity expectedCon actualIdentity actualCon
          && zipAllWith
            (structuralPayloadTypeMayInstantiate typeBounds bound)
            (NE.toList expectedArgs)
            (NE.toList actualArgs)
      (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, BTVarAppWithIdentity actualIdentity actualName actualArgs) ->
        typeBinderRefMatchesWith BackendTypeMetadataLight expectedIdentity expectedName actualIdentity actualName
          && zipAllWith
            (structuralPayloadTypeMayInstantiate typeBounds bound)
            (NE.toList expectedArgs)
            (NE.toList actualArgs)
      (BTForallWithIdentity expectedIdentity expectedBinder expectedBound expectedForallBody, BTForallWithIdentity actualIdentity actualBinder actualBound actualForallBody) ->
        structuralPayloadMaybeBoundMayInstantiate typeBounds bound expectedBound actualBound
          && let freshName =
                   freshNameLike
                     expectedBinder
                     ( Set.unions
                         [ Set.fromList [expectedBinder, actualBinder],
                           typeBoundKeyNames typeBounds,
                           maybe Set.empty freeBackendTypeVars expectedBound,
                           maybe Set.empty freeBackendTypeVars actualBound,
                           freeBackendTypeVars expectedForallBody,
                           freeBackendTypeVars actualForallBody
                         ]
                     )
                 freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshName
                 freshKey = freshBackendTypeBinderKey expectedIdentity actualIdentity freshName
                 expectedForallBody' = substituteBackendTypeForBinder expectedIdentity expectedBinder freshTy expectedForallBody
                 actualForallBody' = substituteBackendTypeForBinder actualIdentity actualBinder freshTy actualForallBody
              in structuralPayloadTypeMayInstantiate typeBounds (Set.insert freshKey bound) expectedForallBody' actualForallBody'
      (BTMuWithIdentity expectedIdentity expectedMuName expectedMuBody, BTMuWithIdentity actualIdentity actualMuName actualMuBody) ->
        structuralMuRefsMatchIdentityFirst expectedIdentity expectedMuName actualIdentity actualMuName
          && let freshName =
                   freshNameLike
                     expectedMuName
                     ( Set.unions
                         [ Set.fromList [expectedMuName, actualMuName],
                           typeBoundKeyNames typeBounds,
                           freeBackendTypeVars expectedMuBody,
                           freeBackendTypeVars actualMuBody
                         ]
                     )
                 freshTy = freshBackendTypeBinderVar expectedIdentity actualIdentity freshName
                 freshKey = freshBackendTypeBinderKey expectedIdentity actualIdentity freshName
                 expectedMuBody' = substituteBackendTypeForBinder expectedIdentity expectedMuName freshTy expectedMuBody
                 actualMuBody' = substituteBackendTypeForBinder actualIdentity actualMuName freshTy actualMuBody
              in structuralPayloadTypeMayInstantiate typeBounds (Set.insert freshKey bound) expectedMuBody' actualMuBody'
      _ ->
        backendStructuralDataBoundaryMatches typeBounds Nothing expected actual

structuralPayloadMaybeBoundMayInstantiate ::
  BackendParameterBounds ->
  Set.Set BackendTypeSubstitutionKey ->
  Maybe BackendType ->
  Maybe BackendType ->
  Bool
structuralPayloadMaybeBoundMayInstantiate _ _ Nothing Nothing =
  True
structuralPayloadMaybeBoundMayInstantiate typeBounds bound (Just expectedBound) (Just actualBound) =
  structuralPayloadTypeMayInstantiate typeBounds bound expectedBound actualBound
structuralPayloadMaybeBoundMayInstantiate _ _ _ _ =
  False

isBareTypeVariable :: BackendType -> Bool
isBareTypeVariable =
  \case
    BTVar {} -> True
    _ -> False

firstJust :: [Maybe a] -> Maybe a
firstJust =
  \case
    [] -> Nothing
    candidate : rest ->
      case candidate of
        Just value -> Just value
        Nothing -> firstJust rest

freshBackendTypeBinderVar :: Maybe TypeBinderIdentity -> Maybe TypeBinderIdentity -> String -> BackendType
freshBackendTypeBinderVar leftIdentity rightIdentity =
  BTVarWithIdentity (leftIdentity <|> rightIdentity)

freshBackendTypeBinderKey :: Maybe TypeBinderIdentity -> Maybe TypeBinderIdentity -> String -> BackendTypeSubstitutionKey
freshBackendTypeBinderKey leftIdentity rightIdentity =
  backendTypeSubstitutionKeyFromMaybeMetadataLight (leftIdentity <|> rightIdentity)

atMay :: [a] -> Int -> Maybe a
atMay xs index0
  | index0 < 0 = Nothing
  | otherwise =
      case drop index0 xs of
        value : _ -> Just value
        [] -> Nothing

zipAllWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
zipAllWith _ [] [] =
  True
zipAllWith f (left : leftRest) (right : rightRest) =
  f left right && zipAllWith f leftRest rightRest
zipAllWith _ _ _ =
  False

stripSuffixSimple :: String -> String -> Maybe String
stripSuffixSimple suffix value =
  reverse <$> stripPrefixSimple (reverse suffix) (reverse value)

dropWhileEndSimple :: (Char -> Bool) -> String -> String
dropWhileEndSimple predicate =
  reverse . dropWhile predicate . reverse

stripPrefixSimple :: String -> String -> Maybe String
stripPrefixSimple [] value =
  Just value
stripPrefixSimple _ [] =
  Nothing
stripPrefixSimple (expected : expectedRest) (actual : actualRest)
  | expected == actual = stripPrefixSimple expectedRest actualRest
  | otherwise = Nothing
