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
    StructuralConstructorMatch (..),
    StructuralRecursiveDataMatch (..),
    StructuralRecursiveDataMismatch (..),
    alphaEqBackendType,
    backendStructuralDataBoundaryMatches,
    completeBackendParameterSubstitution,
    isVacuousRecursiveBinder,
    matchBackendTypeParametersWithTypeBounds,
    matchConstructorResult,
    matchFocusedStructuralConstructor,
    matchStructuralDataDeclaration,
    metadataLightStructuralDataMatches,
    structuralBackendHandlerFields,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralMuAsActualDataType,
    structuralMuAsDataType,
    structuralMuHandlerTypes,
    structuralMuNameMatches,
    structuralMuPayloadTypes,
    structuralPayloadsMayInstantiate,
    structuralRecursiveDataName,
    recursiveBodyCompatible,
  )
where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR.Types
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Types.Identity (TypeBinderIdentity)
import MLF.Util.Names (freshNameLike)

type BackendParameterBounds = Map.Map BackendTypeSubstitutionKey (Maybe BackendType)

typeBoundKeyNames :: BackendParameterBounds -> Set.Set String
typeBoundKeyNames =
  Set.map backendTypeSubstitutionKeyName . Map.keysSet

lookupTypeBound :: Maybe TypeBinderIdentity -> String -> BackendParameterBounds -> Maybe (Maybe BackendType)
lookupTypeBound identity name =
  Map.lookup (backendTypeSubstitutionKeyFor identity name)

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
  deriving (Eq, Show)

data StructuralRecursiveDataMismatch
  = StructuralRecursiveDataNameMismatch String String
  | StructuralRecursiveDataNameUnavailable String
  | StructuralRecursiveDataPayloadUnavailable String
  | StructuralRecursiveDataArgumentMismatch String [BackendType] [BackendType]
  | StructuralRecursiveDataConstructorSetMismatch String Int Int
  | StructuralRecursiveDataUnknownConstructor String String
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
        (BTVar leftName, BTVar rightName) ->
          typeVarMatches leftEnv rightEnv Nothing leftName Nothing rightName
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go leftEnv rightEnv leftDom rightDom && go leftEnv rightEnv leftCod rightCod
        (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase) ->
          backendTypeHeadMatches leftIdentity leftBase rightIdentity rightBase
        (BTBase leftBase, BTMu rightName rightBody) ->
          metadataLightStructuralDataMatches leftBase [] rightName rightBody
        (BTMu leftName leftBody, BTBase rightBase) ->
          metadataLightStructuralDataMatches rightBase [] leftName leftBody
        (BTConWithIdentity leftIdentity leftCon leftArgs, BTConWithIdentity rightIdentity rightCon rightArgs) ->
          backendTypeHeadMatches leftIdentity leftCon rightIdentity rightCon
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTCon leftCon leftArgs, BTMu rightName rightBody) ->
          metadataLightStructuralDataMatches leftCon (NE.toList leftArgs) rightName rightBody
        (BTMu leftName leftBody, BTCon rightCon rightArgs) ->
          metadataLightStructuralDataMatches rightCon (NE.toList rightArgs) leftName leftBody
        (BTVarAppWithIdentity leftIdentity leftName leftArgs, BTVarAppWithIdentity rightIdentity rightName rightArgs) ->
          typeVarMatches leftEnv rightEnv leftIdentity leftName rightIdentity rightName
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTVarApp leftName leftArgs, BTVarApp rightName rightArgs) ->
          typeVarMatches leftEnv rightEnv Nothing leftName Nothing rightName
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTForallWithIdentity leftIdentity leftName leftBound leftBody, BTForallWithIdentity rightIdentity rightName rightBound rightBody) ->
          maybeAlphaEq leftEnv rightEnv leftBound rightBound
            && go
              (Map.insert (backendTypeSubstitutionKeyFor leftIdentity leftName) (backendTypeSubstitutionKeyFor rightIdentity rightName) leftEnv)
              (Map.insert (backendTypeSubstitutionKeyFor rightIdentity rightName) (backendTypeSubstitutionKeyFor leftIdentity leftName) rightEnv)
              leftBody
              rightBody
        (BTMuWithIdentity leftIdentity leftName leftBody, BTMuWithIdentity rightIdentity rightName rightBody) ->
          go
            (Map.insert (backendTypeSubstitutionKeyFor leftIdentity leftName) (backendTypeSubstitutionKeyFor rightIdentity rightName) leftEnv)
            (Map.insert (backendTypeSubstitutionKeyFor rightIdentity rightName) (backendTypeSubstitutionKeyFor leftIdentity leftName) rightEnv)
            leftBody
            rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    typeVarMatches ::
      Map.Map BackendTypeSubstitutionKey BackendTypeSubstitutionKey ->
      Map.Map BackendTypeSubstitutionKey BackendTypeSubstitutionKey ->
      Maybe TypeBinderIdentity ->
      String ->
      Maybe TypeBinderIdentity ->
      String ->
      Bool
    typeVarMatches leftEnv rightEnv leftIdentity leftName rightIdentity rightName =
      case (Map.lookup leftKey leftEnv, Map.lookup rightKey rightEnv) of
        (Just expectedRight, Just expectedLeft) ->
          expectedRight == rightKey && expectedLeft == leftKey
        (Nothing, Nothing) ->
          typeVarIdentityMatches leftIdentity leftName rightIdentity rightName
        _ ->
          False
      where
        leftKey = backendTypeSubstitutionKeyFor leftIdentity leftName
        rightKey = backendTypeSubstitutionKeyFor rightIdentity rightName

    typeVarIdentityMatches :: Maybe TypeBinderIdentity -> String -> Maybe TypeBinderIdentity -> String -> Bool
    typeVarIdentityMatches (Just leftIdentity) _ (Just rightIdentity) _ =
      leftIdentity == rightIdentity
    typeVarIdentityMatches Nothing leftName Nothing rightName =
      leftName == rightName
    typeVarIdentityMatches _ _ _ _ =
      False

    maybeAlphaEq _ _ Nothing Nothing =
      True
    maybeAlphaEq leftEnv rightEnv (Just leftTy) (Just rightTy) =
      go leftEnv rightEnv leftTy rightTy
    maybeAlphaEq _ _ _ _ =
      False

metadataLightStructuralDataMatches :: BaseTy -> [BackendType] -> String -> BackendType -> Bool
metadataLightStructuralDataMatches base args muName body =
  case matchStructuralDataLight base args muName body of
    Right _ -> True
    Left _ -> False

matchStructuralDataLight ::
  BaseTy ->
  [BackendType] ->
  String ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataLight (BaseTy dataName) args muName body = do
  structuralName <-
    case structuralRecursiveDataName muName of
      Just name -> Right name
      Nothing -> Left (StructuralRecursiveDataNameUnavailable muName)
  if dataName == structuralName
    then pure ()
    else Left (StructuralRecursiveDataNameMismatch dataName structuralName)
  payloadFields <-
    case structuralBackendHandlerFields body of
      Just fields -> Right fields
      Nothing -> Left (StructuralRecursiveDataPayloadUnavailable muName)
  let payloadTypes = filter (not . recursiveSelfField muName) (concat payloadFields)
      matches
        | null args = null payloadTypes
        | null payloadTypes = all isBareTypeVariable args
        | otherwise = zipAllWith alphaEqBackendType args payloadTypes
  if matches
    then
      Right
        StructuralRecursiveDataMatch
          { srdmDataName = dataName,
            srdmParameterSubstitution = Map.empty,
            srdmPayloadFields = payloadFields
          }
    else Left (StructuralRecursiveDataArgumentMismatch dataName args payloadTypes)

recursiveSelfField :: String -> BackendType -> Bool
recursiveSelfField muName ty =
  alphaEqBackendType (BTVar muName) ty
    || case ty of
      BTVar fieldName ->
        structuralRecursiveDataName fieldName == structuralRecursiveDataName muName
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
  wholeMatch <- matchStructuralDataDeclaration typeBounds dataDecl substitution structuralTy
  (constructorIndex, matchedConstructor) <-
    case indexedConstructors of
      [] -> Left (StructuralRecursiveDataUnknownConstructor dataName constructorName)
      matched : _ -> Right matched
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
      case backendConstructorIdentity expected of
        Just identity
          | backendConstructorIdentity candidate == Just identity ->
              True
        _ ->
          backendConstructorName candidate == constructorName

matchStructuralDataDeclaration ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataDeclaration typeBounds dataDecl substitution =
  \case
    structuralTy@(BTMu muName body)
      | structuralMuNameMatches (backendDataName dataDecl) muName -> do
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
          payloadFields <- structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muName resultIdentity resultName handlers
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
  Maybe (Map.Map String BackendData) ->
  BackendType ->
  BackendType ->
  Bool
backendStructuralDataBoundaryMatches typeBounds mbDataDecls expectedTy actualTy =
  go expectedTy actualTy
  where
    go expected actual =
      alphaEqBackendType expected actual
        || case (expected, actual) of
          (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
            go expectedDom actualDom && go expectedCod actualCod
          (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase) ->
            backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase
          (BTBase expectedBase, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedBase [] actualName actualBody
          (BTMu expectedName expectedBody, BTBase actualBase) ->
            structuralMuMatchesKnownData actualBase [] expectedName expectedBody
          (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
            backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
              && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTCon expectedCon expectedArgs, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedCon (NE.toList expectedArgs) actualName actualBody
          (BTMu expectedName expectedBody, BTCon actualCon actualArgs) ->
            structuralMuMatchesKnownData actualCon (NE.toList actualArgs) expectedName expectedBody
          (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
            structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
          (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) ->
            maybeBoundaryMatches expectedBound actualBound
              && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                     expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                     actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                  in go expectedBody' actualBody'
          (BTBottom, BTBottom) ->
            True
          _ ->
            False

    structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      case
        ( structuralRecursiveDataName expectedName,
          structuralRecursiveDataName actualName,
          structuralMuHandlerTypesWithIdentity expectedBody,
          structuralMuHandlerTypesWithIdentity actualBody
        )
        of
          ( Just expectedDataName,
            Just actualDataName,
            Just (expectedResultIdentity, expectedResultName, expectedHandlers),
            Just (actualResultIdentity, actualResultName, actualHandlers)
            )
              | expectedDataName == actualDataName,
                Just dataDecl <- mbDataDecls >>= Map.lookup expectedDataName,
                length expectedHandlers == length actualHandlers,
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
                      normalizeHandler selfIdentity selfName resultIdentity resultName =
                        substituteBackendTypesByKey
                          ( Map.fromList $
                              binderReplacement selfIdentity selfName (BTVar freshSelf)
                                ++ binderReplacement resultIdentity resultName (BTVar freshResult)
                          )
                   in zipAllWith
                        go
                        (map (normalizeHandler expectedIdentity expectedName expectedResultIdentity expectedResultName) expectedHandlers)
                        (map (normalizeHandler actualIdentity actualName actualResultIdentity actualResultName) actualHandlers)
          _ ->
            False

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
      (backendTypeSubstitutionKeyFor identity name, replacement)
        : case identity of
          Just _ -> [(BackendTypeSubstitutionByName name, replacement)]
          Nothing -> []

    maybeBoundaryMatches Nothing Nothing =
      True
    maybeBoundaryMatches (Just expectedBound) (Just actualBound) =
      go expectedBound actualBound
    maybeBoundaryMatches _ _ =
      False

    structuralMuMatchesKnownData base@(BaseTy dataName) args muName body =
      metadataLightStructuralDataMatches base args muName body
        || case matchingDataDecl dataName muName of
          Just dataDecl
            | structuralMuNameMatches (backendDataName dataDecl) muName,
              Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMu muName body)
          _ ->
            False

    matchingDataDecl dataName muName =
      case mbDataDecls >>= Map.lookup dataName of
        Just dataDecl -> Just dataDecl
        Nothing -> do
          structuralName <- structuralRecursiveDataName muName
          mbDataDecls >>= Map.lookup structuralName

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
  String ->
  BackendType ->
  String ->
  BackendType ->
  Bool
structuralPayloadsMayInstantiate typeBounds expectedName expectedBody actualName actualBody =
  case (structuralRecursiveDataName expectedName, structuralRecursiveDataName actualName) of
    (Just expectedDataName, Just actualDataName)
      | expectedDataName == actualDataName ->
          let freshSelf =
                freshNameLike
                  expectedName
                  ( Set.unions
                      [ Set.fromList [expectedName, actualName],
                        typeBoundKeyNames typeBounds,
                        freeBackendTypeVars expectedBody,
                        freeBackendTypeVars actualBody
                      ]
                  )
              expectedBody' = substituteBackendType expectedName (BTVar freshSelf) expectedBody
              actualBody' = substituteBackendType actualName (BTVar freshSelf) actualBody
           in case (structuralMuPayloadTypes expectedBody', structuralMuPayloadTypes actualBody') of
                (Just expectedPayloadTypes, Just actualPayloadTypes) ->
                  structuralPayloadTypesMayInstantiate
                    typeBounds
                    (Set.singleton (BackendTypeSubstitutionByName freshSelf))
                    expectedPayloadTypes
                    actualPayloadTypes
                _ ->
                  False
    _ ->
      False

structuralRecursiveDataName :: String -> Maybe String
structuralRecursiveDataName name =
  case stripPrefixSimple "$$identity#" name of
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

structuralMuAsDataType :: [String] -> String -> Maybe BackendType
structuralMuAsDataType dataParameterOrder muName = do
  dataName <- structuralRecursiveDataName muName
  let parameterArgs = map BTVar dataParameterOrder
  Just $
    case parameterArgs of
      [] -> BTBase (BaseTy dataName)
      arg : rest -> BTCon (BaseTy dataName) (arg :| rest)

structuralMuAsActualDataType :: String -> BackendType -> Maybe BackendType
structuralMuAsActualDataType muName actual =
  case actual of
    BTBase (BaseTy actualName)
      | structuralMuNameMatches actualName muName -> Just actual
    BTCon (BaseTy actualName) _
      | structuralMuNameMatches actualName muName -> Just actual
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
          | alphaEqBackendType ty (BTVarWithIdentity resultIdentity resultName) = Just handlers
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
          | alphaEqBackendType ty (BTVarWithIdentity resultIdentity resultName) = Just handlers
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
          | alphaEqBackendType ty (BTVarWithIdentity resultIdentity resultName) = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

structuralDataArgumentSubstitution :: BackendData -> [BackendType] -> Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
structuralDataArgumentSubstitution dataDecl args
  | length dataParameters == length args =
      Just (Map.fromList (zip (backendDataParameterKeys dataDecl) args))
  | otherwise =
      Nothing
  where
    dataParameters =
      backendDataParameters dataDecl

structuralPayloadHandlersMatchForData ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  String ->
  Maybe TypeBinderIdentity ->
  String ->
  [BackendType] ->
  Either StructuralRecursiveDataMismatch [[BackendType]]
structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muName resultIdentity resultName handlers =
  traverse constructorHandlerMatches (zip constructors handlers)
  where
    dataName =
      backendDataName dataDecl
    dataParameters =
      backendDataParameters dataDecl
    constructors =
      backendDataConstructors dataDecl
    dataParameterKeySet =
      Set.fromList (backendDataParameterKeys dataDecl)
    dataSubstitution =
      Map.filterWithKey (\key _ -> Set.member key dataParameterKeySet) substitution
    structuralTyWithData =
      substituteBackendTypesByKey dataSubstitution structuralTy
    knownSubstitution =
      Map.insert (BackendTypeSubstitutionByName muName) structuralTyWithData dataSubstitution
    substituteKnownTypes =
      substituteBackendTypesByKey knownSubstitution
    constructorHandlerMatches (constructor, handlerTy) =
      case
        matchBackendTypeParametersWithTypeBounds
          typeBounds
          dataParameters
          parameters
          Map.empty
          expectedHandlerTy
          actualHandlerTy
        of
          Just _ ->
            case structuralHandlerFields resultIdentity resultName actualHandlerTy of
              Just fields -> Right fields
              Nothing -> Left (StructuralRecursiveDataPayloadUnavailable dataName)
          Nothing ->
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
        parameters =
          Map.map (fmap substituteKnownTypes) $
            constructorTypeParameterBoundsForData dataDecl constructor

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
      | alphaEqBackendType ty (BTVarWithIdentity resultIdentity resultName) = Just fields
      | otherwise =
          case ty of
            BTForall _ _ body -> go fields body
            BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
            _ -> Nothing

constructorTypeParameterBoundsForData :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataDecl constructor =
  Map.fromList $
    [(key, Nothing) | key <- backendDataParameterKeys dataDecl]
      ++ [ (backendTypeSubstitutionKeyFor (backendTypeBinderIdentity binder) (backendTypeBinderName binder), backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

matchConstructorResult ::
  [String] ->
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
        key = backendTypeSubstitutionKeyFor identity name
    _ ->
      if alphaEqBackendType expected actual
        then Just substitution
        else
          ( case (expected, actual) of
              (BTVar expectedName, BTVar actualName)
                | expectedName == actualName -> Just substitution
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedDom actualDom
                  >>= \subst -> matchConstructorResult dataParameterOrder parameters subst expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity expectedBase, BTBaseWithIdentity actualIdentity actualBase)
                | backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase -> Just substitution
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
                | backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon && length expectedArgs == length actualArgs ->
                    foldM
                      (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
                      substitution
                      (zip (NE.toList expectedArgs) (NE.toList actualArgs))
              (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected expectedName expectedBody actualTy
              (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected expectedName expectedBody actualTy
              (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
                matchStructuralMuActual expectedTy actualName actualBody
              (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
                matchStructuralMuActual expectedTy actualName actualBody
              (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, _) ->
                matchConstructorResultApplication dataParameterOrder parameters substitution expectedIdentity expectedName (NE.toList expectedArgs) actual
              (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
                subst <-
                  case (expectedBound, actualBound) of
                    (Nothing, Nothing) -> Just substitution
                    (Just expectedBoundTy, Just actualBoundTy) -> matchConstructorResult dataParameterOrder parameters substitution expectedBoundTy actualBoundTy
                    _ -> Nothing
                matchConstructorResult dataParameterOrder parameters subst expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
              (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedBody (substituteBackendType actualName (BTVar expectedName) actualBody)
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                Nothing
          )
  where
    matchStructuralMuExpected muName _body actualTy =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
          structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
        ]

    matchStructuralMuActual expectedTy muName _body =
      firstJust
        [ structuralMuAsDataType dataParameterOrder muName
            >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
          structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
        ]

matchConstructorResultApplication ::
  [String] ->
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
              else matchConstructorResult dataParameterOrder parameters substitution (BTVar name) actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
    _ -> Nothing
  where
    key =
      backendTypeSubstitutionKeyFor identity name

    insertParameterSubstitution paramKey actualHead substitution0 =
      case Map.lookup paramKey substitution0 of
        Nothing -> Just (Map.insert paramKey actualHead substitution0)
        Just previous
          | alphaEqBackendType previous actualHead -> Just substitution0
          | otherwise -> Nothing

matchBackendTypeParametersWithTypeBounds ::
  BackendParameterBounds ->
  [String] ->
  BackendParameterBounds ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map BackendTypeSubstitutionKey BackendType)
matchBackendTypeParametersWithTypeBounds typeBounds dataParameterOrder parameterBounds =
  go Set.empty
  where
    dataParameterNames =
      Set.fromList dataParameterOrder

    matchParameterKey identity name =
      case identity of
        Just {} ->
          if Map.member key parameterBounds || Set.member name dataParameterNames
            then Just key
            else Nothing
        Nothing
          | Map.member nameKey parameterBounds || Set.member name dataParameterNames -> Just nameKey
          | otherwise -> Nothing
      where
        key = backendTypeSubstitutionKeyFor identity name
        nameKey = BackendTypeSubstitutionByName name

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
                | backendTypeHeadMatches expectedIdentity expectedBase actualIdentity actualBase ->
                    Just substitution
              (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs)
                | backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon ->
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
              (BTMu expectedName expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected bound substitution expectedName expectedBody actualTy
              (BTMu expectedName expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected bound substitution expectedName expectedBody actualTy
              (expectedTy@(BTBase {}), BTMu actualName actualBody) ->
                matchStructuralMuActual bound substitution expectedTy actualName actualBody
              (expectedTy@(BTCon {}), BTMu actualName actualBody) ->
                matchStructuralMuActual bound substitution expectedTy actualName actualBody
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
                    expectedBody' = substituteBinder expectedIdentity expectedName (BTVar freshName) expectedBody
                    actualBody' = substituteBinder actualIdentity actualName (BTVar freshName) actualBody
                go (Set.insert (BackendTypeSubstitutionByName freshName) bound) substitution' expectedBody' actualBody'
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) -> do
                case (null dataParameterOrder && sameStructuralDataName expectedName actualName, isVacuousRecursiveBinder expectedName expectedBody, isVacuousRecursiveBinder actualName actualBody) of
                  (True, _, _) ->
                    Just substitution
                  (_, True, True) ->
                    go bound substitution expectedBody actualBody
                  (_, True, False)
                    | recursiveBodyCompatible actualName actualBody expectedBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
                    | otherwise ->
                        go bound substitution expectedBody actual
                  (_, False, True)
                    | recursiveBodyCompatible expectedName expectedBody actualBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
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
                        expectedBody' = substituteBinder expectedIdentity expectedName (BTVar freshName) expectedBody
                        actualBody' = substituteBinder actualIdentity actualName (BTVar freshName) actualBody
                    go (Set.insert (BackendTypeSubstitutionByName freshName) bound) substitution expectedBody' actualBody'
              (BTMu expectedName expectedBody, _)
                | isVacuousRecursiveBinder expectedName expectedBody ->
                    go bound substitution expectedBody actual
              (_, BTMu actualName actualBody)
                | isVacuousRecursiveBinder actualName actualBody ->
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

    matchStructuralMuExpected bound substitution muName body actualTy =
      firstJust
        [ structuralMuNominalTypeMatches actualTy muName body >>= \() -> Just substitution,
          structuralMuAsDataTypeForBody muName body
            >>= \expectedTy -> go bound substitution expectedTy actualTy,
          structuralMuPayloadTypes body
            *> structuralMuAsActualDataType muName actualTy
            >>= \expectedTy -> go bound substitution expectedTy actualTy
        ]

    matchStructuralMuActual bound substitution expectedTy muName body =
      firstJust
        [ structuralMuNominalTypeMatches expectedTy muName body >>= \() -> Just substitution,
          structuralMuAsDataTypeForBody muName body
            >>= \actualTy -> go bound substitution expectedTy actualTy,
          structuralMuPayloadTypes body
            *> structuralMuAsActualDataType muName expectedTy
            >>= \actualTy -> go bound substitution expectedTy actualTy
        ]

    structuralMuAsDataTypeForBody muName body =
      structuralMuPayloadTypes body *> structuralMuAsDataType dataParameterOrder muName

    structuralMuNominalTypeMatches nominalTy muName body =
      if nominalMatches
        then Just ()
        else Nothing
      where
        nominalMatches =
          case nominalTy of
            BTBase base ->
              metadataLightStructuralDataMatches base [] muName body
            BTCon base args ->
              metadataLightStructuralDataMatches base (NE.toList args) muName body
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
          | repeatedParameterTypeMatches previous actual && backendParameterBoundMatches key previous substitution ->
              Just substitution
        _ ->
          Nothing

    repeatedParameterTypeMatches previous actual =
      alphaEqBackendType previous actual || metadataLightSameStructuralType previous actual

    metadataLightSameStructuralType left right =
      case (left, right) of
        (BTMu leftName leftBody, BTMu rightName rightBody) ->
          case (structuralRecursiveDataName leftName, structuralRecursiveDataName rightName) of
            (Just leftDataName, Just rightDataName)
              | leftDataName == rightDataName ->
                  metadataLightStructuralDataMatches (BaseTy leftDataName) [] leftName leftBody
                    && metadataLightStructuralDataMatches (BaseTy rightDataName) [] rightName rightBody
            _ ->
              False
        _ ->
          False

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
      Set.null (freeBackendTypeVars expectedBody `Set.intersection` Set.map backendTypeSubstitutionKeyName (Map.keysSet parameterBounds))

    sameStructuralDataName expectedName actualName =
      case (structuralRecursiveDataName expectedName, structuralRecursiveDataName actualName) of
        (Just expectedDataName, Just actualDataName) -> expectedDataName == actualDataName
        _ -> False

    substituteBinder identity name replacement =
      substituteBackendTypesByKey (Map.fromList (binderReplacement identity name replacement))

    binderReplacement identity name replacement =
      (backendTypeSubstitutionKeyFor identity name, replacement)
        : case identity of
          Just _ -> [(BackendTypeSubstitutionByName name, replacement)]
          Nothing -> []

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

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVarWithIdentity identity name -> Just (BTVarWithIdentity identity name, [])
    BTBaseWithIdentity identity name -> Just (BTBaseWithIdentity identity name, [])
    BTConWithIdentity identity name args -> Just (BTBaseWithIdentity identity name, NE.toList args)
    BTVarAppWithIdentity identity name args -> Just (BTVarWithIdentity identity name, NE.toList args)
    _ -> Nothing

isVacuousRecursiveBinder :: String -> BackendType -> Bool
isVacuousRecursiveBinder name body =
  Set.notMember name (freeBackendTypeVars body)

recursiveBodyCompatible :: String -> BackendType -> BackendType -> Bool
recursiveBodyCompatible recursiveName recursiveBody plainBody =
  case go Set.empty Map.empty Nothing recursiveBody plainBody of
    Just _ -> True
    Nothing -> False
  where
    go patternVars patternBindings recursiveAlias leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVar name, _)
          | name == recursiveName ->
              matchRecursiveAlias patternBindings recursiveAlias rightTy
          | Set.member name patternVars ->
              matchPatternVar name patternBindings recursiveAlias rightTy
        (BTVar leftName, BTVar rightName)
          | leftName == rightName ->
              Just (patternBindings, recursiveAlias)
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go patternVars patternBindings recursiveAlias leftDom rightDom
            >>= \(patternBindings', recursiveAlias') ->
              go patternVars patternBindings' recursiveAlias' leftCod rightCod
        (BTBaseWithIdentity leftIdentity leftBase, BTBaseWithIdentity rightIdentity rightBase)
          | backendTypeHeadMatches leftIdentity leftBase rightIdentity rightBase ->
              Just (patternBindings, recursiveAlias)
        (BTConWithIdentity leftIdentity leftCon leftArgs, BTConWithIdentity rightIdentity rightCon rightArgs)
          | backendTypeHeadMatches leftIdentity leftCon rightIdentity rightCon ->
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
        (BTForall leftName Nothing leftBody, BTForall rightName Nothing rightBody) ->
          let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
              leftBody' = substituteBackendType leftName (BTVar freshName) leftBody
              rightBody' = substituteBackendType rightName (BTVar freshName) rightBody
           in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForall leftName (Just leftBound) leftBody, BTForall rightName (Just rightBound) rightBody)
          | alphaEqBackendType leftBound rightBound ->
              let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
                  leftBody' = substituteBackendType leftName (BTVar freshName) leftBody
                  rightBody' = substituteBackendType rightName (BTVar freshName) rightBody
               in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForall leftName Nothing leftBody, _) ->
          go (Set.insert leftName patternVars) patternBindings recursiveAlias leftBody rightTy
        (_, BTForall rightName Nothing rightBody)
          | Set.member recursiveName (freeBackendTypeVars leftTy) ->
              let aliasName = freshNameLike rightName (freeBackendTypeVars leftTy `Set.union` freeBackendTypeVars rightBody)
                  rightBody' = substituteBackendType rightName (BTVar aliasName) rightBody
               in case recursiveAlias of
                    Nothing ->
                      go patternVars patternBindings (Just aliasName) leftTy rightBody'
                    Just previous
                      | previous == aliasName ->
                          go patternVars patternBindings recursiveAlias leftTy rightBody'
                    _ ->
                      Nothing
        (BTBottom, BTBottom) ->
          Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    matchPatternVar name patternBindings recursiveAlias rightTy =
      case Map.lookup name patternBindings of
        Nothing ->
          Just (Map.insert name rightTy patternBindings, recursiveAlias)
        Just previous
          | alphaEqBackendType previous rightTy ->
              Just (patternBindings, recursiveAlias)
        _ ->
          Nothing

    matchRecursiveAlias patternBindings recursiveAlias rightTy =
      case rightTy of
        BTVar rightName ->
          case recursiveAlias of
            Nothing ->
              Just (patternBindings, Just rightName)
            Just expectedName
              | expectedName == rightName ->
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
    || zipAllWith
      (structuralPayloadTypeMayInstantiate typeBounds bound)
      actualPayloadTypes
      expectedPayloadTypes

structuralPayloadTypeMayInstantiate ::
  BackendParameterBounds ->
  Set.Set BackendTypeSubstitutionKey ->
  BackendType ->
  BackendType ->
  Bool
structuralPayloadTypeMayInstantiate typeBounds bound expected actual =
  alphaEqBackendType expected actual
    || case (expected, actual) of
      (BTVarWithIdentity identity name, _)
        | Set.notMember key bound && Map.notMember key typeBounds ->
            True
        where
          key = backendTypeSubstitutionKeyFor identity name
      (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
        structuralPayloadTypeMayInstantiate typeBounds bound expectedDom actualDom
          && structuralPayloadTypeMayInstantiate typeBounds bound expectedCod actualCod
      (BTConWithIdentity expectedIdentity expectedCon expectedArgs, BTConWithIdentity actualIdentity actualCon actualArgs) ->
        backendTypeHeadMatches expectedIdentity expectedCon actualIdentity actualCon
          && zipAllWith
            (structuralPayloadTypeMayInstantiate typeBounds bound)
            (NE.toList expectedArgs)
            (NE.toList actualArgs)
      (BTVarApp expectedName expectedArgs, BTVarApp actualName actualArgs) ->
        expectedName == actualName
          && zipAllWith
            (structuralPayloadTypeMayInstantiate typeBounds bound)
            (NE.toList expectedArgs)
            (NE.toList actualArgs)
      (BTForall expectedBinder expectedBound expectedForallBody, BTForall actualBinder actualBound actualForallBody) ->
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
                 expectedForallBody' = substituteBackendType expectedBinder (BTVar freshName) expectedForallBody
                 actualForallBody' = substituteBackendType actualBinder (BTVar freshName) actualForallBody
              in structuralPayloadTypeMayInstantiate typeBounds (Set.insert (BackendTypeSubstitutionByName freshName) bound) expectedForallBody' actualForallBody'
      (BTMu expectedMuName expectedMuBody, BTMu actualMuName actualMuBody) ->
        let freshName =
              freshNameLike
                expectedMuName
                ( Set.unions
                    [ Set.fromList [expectedMuName, actualMuName],
                      typeBoundKeyNames typeBounds,
                      freeBackendTypeVars expectedMuBody,
                      freeBackendTypeVars actualMuBody
                    ]
                )
            expectedMuBody' = substituteBackendType expectedMuName (BTVar freshName) expectedMuBody
            actualMuBody' = substituteBackendType actualMuName (BTVar freshName) actualMuBody
         in structuralPayloadTypeMayInstantiate typeBounds (Set.insert (BackendTypeSubstitutionByName freshName) bound) expectedMuBody' actualMuBody'
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
