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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR.Types
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Util.Names (freshNameLike)

type BackendParameterBounds = Map.Map String (Maybe BackendType)

data StructuralRecursiveDataMatch = StructuralRecursiveDataMatch
  { srdmDataName :: String,
    srdmParameterSubstitution :: Map.Map String BackendType,
    srdmPayloadFields :: [[BackendType]]
  }
  deriving (Eq, Show)

data StructuralConstructorMatch = StructuralConstructorMatch
  { srcmDataName :: String,
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
        (BTVar leftName, BTVar rightName) ->
          case (Map.lookup leftName leftEnv, Map.lookup rightName rightEnv) of
            (Just expectedRight, Just expectedLeft) ->
              expectedRight == rightName && expectedLeft == leftName
            (Nothing, Nothing) ->
              leftName == rightName
            _ ->
              False
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go leftEnv rightEnv leftDom rightDom && go leftEnv rightEnv leftCod rightCod
        (BTBase leftBase, BTBase rightBase) ->
          leftBase == rightBase
        (BTBase leftBase, BTMu rightName rightBody) ->
          metadataLightStructuralDataMatches leftBase [] rightName rightBody
        (BTMu leftName leftBody, BTBase rightBase) ->
          metadataLightStructuralDataMatches rightBase [] leftName leftBody
        (BTCon leftCon leftArgs, BTCon rightCon rightArgs) ->
          leftCon == rightCon && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTCon leftCon leftArgs, BTMu rightName rightBody) ->
          metadataLightStructuralDataMatches leftCon (NE.toList leftArgs) rightName rightBody
        (BTMu leftName leftBody, BTCon rightCon rightArgs) ->
          metadataLightStructuralDataMatches rightCon (NE.toList rightArgs) leftName leftBody
        (BTVarApp leftName leftArgs, BTVarApp rightName rightArgs) ->
          typeVarNamesMatch leftEnv rightEnv leftName rightName
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTForall leftName leftBound leftBody, BTForall rightName rightBound rightBody) ->
          maybeAlphaEq leftEnv rightEnv leftBound rightBound
            && go
              (Map.insert leftName rightName leftEnv)
              (Map.insert rightName leftName rightEnv)
              leftBody
              rightBody
        (BTMu leftName leftBody, BTMu rightName rightBody) ->
          go
            (Map.insert leftName rightName leftEnv)
            (Map.insert rightName leftName rightEnv)
            leftBody
            rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    typeVarNamesMatch leftEnv rightEnv leftName rightName =
      case (Map.lookup leftName leftEnv, Map.lookup rightName rightEnv) of
        (Just expectedRight, Just expectedLeft) ->
          expectedRight == rightName && expectedLeft == leftName
        (Nothing, Nothing) ->
          leftName == rightName
        _ ->
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
  let payloadTypes = concat payloadFields
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

matchFocusedStructuralConstructor ::
  Map.Map String (Maybe BackendType) ->
  BackendData ->
  BackendConstructor ->
  Map.Map String BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralConstructorMatch
matchFocusedStructuralConstructor typeBounds dataDecl constructor substitution structuralTy = do
  wholeMatch <- matchStructuralDataDeclaration typeBounds dataDecl substitution structuralTy
  constructorIndex <-
    case indexedConstructors of
      [] -> Left (StructuralRecursiveDataUnknownConstructor dataName constructorName)
      (index0, _) : _ -> Right index0
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
        srcmConstructorName = constructorName,
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
          backendConstructorName candidate == constructorName
      ]

matchStructuralDataDeclaration ::
  Map.Map String (Maybe BackendType) ->
  BackendData ->
  Map.Map String BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataDeclaration typeBounds dataDecl substitution =
  \case
    structuralTy@(BTMu muName body)
      | structuralMuNameMatches (backendDataName dataDecl) muName -> do
          (resultName, handlers) <-
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
          payloadFields <- structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muName resultName handlers
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
    dataParameters =
      backendDataParameters dataDecl
    dataSubstitution =
      Map.filterWithKey (\name _ -> name `elem` dataParameters) substitution

structuralDataDeclarationMatches ::
  Map.Map String (Maybe BackendType) ->
  BackendData ->
  Map.Map String BackendType ->
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
  Map.Map String (Maybe BackendType) ->
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
          (BTBase expectedBase, BTBase actualBase) ->
            expectedBase == actualBase
          (BTBase expectedBase, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedBase [] actualName actualBody
          (BTMu expectedName expectedBody, BTBase actualBase) ->
            structuralMuMatchesKnownData actualBase [] expectedName expectedBody
          (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
            expectedCon == actualCon
              && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
          (BTCon expectedCon expectedArgs, BTMu actualName actualBody) ->
            structuralMuMatchesKnownData expectedCon (NE.toList expectedArgs) actualName actualBody
          (BTMu expectedName expectedBody, BTCon actualCon actualArgs) ->
            structuralMuMatchesKnownData actualCon (NE.toList actualArgs) expectedName expectedBody
          (BTMu expectedName expectedBody, BTMu actualName actualBody) ->
            structuralMuBodiesMatchKnownData expectedName expectedBody actualName actualBody
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

    structuralMuBodiesMatchKnownData expectedName expectedBody actualName actualBody =
      case
        ( structuralRecursiveDataName expectedName,
          structuralRecursiveDataName actualName,
          structuralMuHandlerTypes expectedBody,
          structuralMuHandlerTypes actualBody
        )
        of
          ( Just expectedDataName,
            Just actualDataName,
            Just (expectedResultName, expectedHandlers),
            Just (actualResultName, actualHandlers)
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
                      normalizeHandler selfName resultName =
                        substituteBackendTypes
                          ( Map.fromList
                              [ (selfName, BTVar freshSelf),
                                (resultName, BTVar freshResult)
                              ]
                          )
                   in zipAllWith
                        go
                        (map (normalizeHandler expectedName expectedResultName) expectedHandlers)
                        (map (normalizeHandler actualName actualResultName) actualHandlers)
          _ ->
            False

    maybeBoundaryMatches Nothing Nothing =
      True
    maybeBoundaryMatches (Just expectedBound) (Just actualBound) =
      go expectedBound actualBound
    maybeBoundaryMatches _ _ =
      False

    structuralMuMatchesKnownData base@(BaseTy dataName) args muName body =
      metadataLightStructuralDataMatches base args muName body
        || case mbDataDecls >>= Map.lookup dataName of
          Just dataDecl
            | structuralMuNameMatches dataName muName,
              Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                structuralDataDeclarationMatches typeBounds dataDecl substitution (BTMu muName body)
          _ ->
            False

    freshBinderName leftName rightName leftBound rightBound leftBody rightBody =
      freshNameLike
        leftName
        ( Set.unions
            [ Set.fromList [leftName, rightName],
              Map.keysSet typeBounds,
              maybe Set.empty freeBackendTypeVars leftBound,
              maybe Set.empty freeBackendTypeVars rightBound,
              freeBackendTypeVars leftBody,
              freeBackendTypeVars rightBody
            ]
        )

structuralPayloadsMayInstantiate ::
  Map.Map String (Maybe BackendType) ->
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
                        Map.keysSet typeBounds,
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
                    (Set.singleton freshSelf)
                    expectedPayloadTypes
                    actualPayloadTypes
                _ ->
                  False
    _ ->
      False

structuralRecursiveDataName :: String -> Maybe String
structuralRecursiveDataName name =
  stripSuffixSimple "_self" (dropWhile (== '$') name)

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

structuralMuHandlerTypes :: BackendType -> Maybe (String, [BackendType])
structuralMuHandlerTypes =
  \case
    BTForall resultName _ handlerTy -> do
      handlers <- collectHandlerTypes resultName handlerTy
      Just (resultName, handlers)
    _ -> Nothing
  where
    collectHandlerTypes resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> go (handlers ++ [handlerTy]) rest
                _ -> Nothing

structuralBackendHandlerFields :: BackendType -> Maybe [[BackendType]]
structuralBackendHandlerFields =
  \case
    BTForall resultName _ handlerTy -> collectHandlers resultName handlerTy
    _ -> Nothing
  where
    collectHandlers resultName =
      go []
      where
        go handlers ty
          | alphaEqBackendType ty (BTVar resultName) = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultName handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultName =
      go []
      where
        go fields ty
          | alphaEqBackendType ty (BTVar resultName) = Just fields
          | otherwise =
              case ty of
                BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
                _ -> Nothing

structuralDataArgumentSubstitution :: BackendData -> [BackendType] -> Maybe (Map.Map String BackendType)
structuralDataArgumentSubstitution dataDecl args
  | length dataParameters == length args =
      Just (Map.fromList (zip dataParameters args))
  | otherwise =
      Nothing
  where
    dataParameters =
      backendDataParameters dataDecl

structuralPayloadHandlersMatchForData ::
  Map.Map String (Maybe BackendType) ->
  BackendData ->
  Map.Map String BackendType ->
  BackendType ->
  String ->
  String ->
  [BackendType] ->
  Either StructuralRecursiveDataMismatch [[BackendType]]
structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muName resultName handlers =
  traverse constructorHandlerMatches (zip constructors handlers)
  where
    dataName =
      backendDataName dataDecl
    dataParameters =
      backendDataParameters dataDecl
    constructors =
      backendDataConstructors dataDecl
    dataSubstitution =
      Map.filterWithKey (\name _ -> name `elem` dataParameters) substitution
    structuralTyWithData =
      substituteBackendTypes dataSubstitution structuralTy
    knownSubstitution =
      Map.insert muName structuralTyWithData dataSubstitution
    substituteKnownTypes =
      substituteBackendTypes knownSubstitution
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
            case structuralHandlerFields resultName actualHandlerTy of
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
          substituteKnownTypes (constructorStructuralHandlerType resultName constructor)
        actualHandlerTy =
          substituteKnownTypes handlerTy
        parameters =
          Map.map (fmap substituteKnownTypes) $
            constructorTypeParameterBoundsForData dataParameters constructor

constructorStructuralHandlerType :: String -> BackendConstructor -> BackendType
constructorStructuralHandlerType resultName constructor =
  foldr wrapForall handlerBody (backendConstructorForalls constructor)
  where
    handlerBody =
      foldr BTArrow (BTVar resultName) (backendConstructorFields constructor)

    wrapForall binder body =
      BTForall (backendTypeBinderName binder) (backendTypeBinderBound binder) body

structuralHandlerFields :: String -> BackendType -> Maybe [BackendType]
structuralHandlerFields resultName =
  go []
  where
    go fields ty
      | alphaEqBackendType ty (BTVar resultName) = Just fields
      | otherwise =
          case ty of
            BTForall _ _ body -> go fields body
            BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
            _ -> Nothing

constructorTypeParameterBoundsForData :: [String] -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataParameters constructor =
  Map.fromList $
    [(name, Nothing) | name <- dataParameters]
      ++ [ (backendTypeBinderName binder, backendTypeBinderBound binder)
           | binder <- backendConstructorForalls constructor
         ]

matchConstructorResult ::
  [String] ->
  Set.Set String ->
  Map.Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map String BackendType)
matchConstructorResult dataParameterOrder parameters substitution expected actual =
  case expected of
    BTVar name
      | Set.member name parameters ->
          case Map.lookup name substitution of
            Nothing -> Just (Map.insert name actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Just substitution
              | otherwise -> Nothing
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
              (BTBase expectedBase, BTBase actualBase)
                | expectedBase == actualBase -> Just substitution
              (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
                | expectedCon == actualCon && length expectedArgs == length actualArgs ->
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
              (BTVarApp expectedName expectedArgs, _) ->
                matchConstructorResultApplication dataParameterOrder parameters substitution expectedName (NE.toList expectedArgs) actual
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
  Set.Set String ->
  Map.Map String BackendType ->
  String ->
  [BackendType] ->
  BackendType ->
  Maybe (Map.Map String BackendType)
matchConstructorResultApplication dataParameterOrder parameters substitution name expectedArgs actual =
  case decomposeBackendTypeHead actual of
    Just (actualHead, actualArgs)
      | length expectedArgs == length actualArgs -> do
          substitution' <-
            if Set.member name parameters
              then insertParameterSubstitution name actualHead substitution
              else matchConstructorResult dataParameterOrder parameters substitution (BTVar name) actualHead
          foldM
            (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
            substitution'
            (zip expectedArgs actualArgs)
    _ -> Nothing
  where
    insertParameterSubstitution paramName actualHead substitution0 =
      case Map.lookup paramName substitution0 of
        Nothing -> Just (Map.insert paramName actualHead substitution0)
        Just previous
          | alphaEqBackendType previous actualHead -> Just substitution0
          | otherwise -> Nothing

matchBackendTypeParametersWithTypeBounds ::
  Map.Map String (Maybe BackendType) ->
  [String] ->
  BackendParameterBounds ->
  Map.Map String BackendType ->
  BackendType ->
  BackendType ->
  Maybe (Map.Map String BackendType)
matchBackendTypeParametersWithTypeBounds typeBounds dataParameterOrder parameterBounds =
  go Set.empty
  where
    go bound substitution expected actual =
      case expected of
        BTVar name
          | Map.member name parameterBounds && Set.notMember name bound ->
              insertParameterSubstitution name actual substitution
        _ ->
          case (expected, actual) of
              (BTVar {}, _) ->
                requireAlphaEq substitution expected actual
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go bound substitution expectedDom actualDom
                  >>= \substitution' -> go bound substitution' expectedCod actualCod
              (BTBase expectedBase, BTBase actualBase)
                | expectedBase == actualBase ->
                    Just substitution
              (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs)
                | expectedCon == actualCon ->
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
              (BTVarApp expectedName expectedArgs, _) ->
                matchBackendTypeApplication bound substitution expectedName (NE.toList expectedArgs) actual
              (BTForall expectedName expectedBound expectedBody, BTForall actualName actualBound actualBody) -> do
                substitution' <- matchMaybeBound bound substitution expectedBound actualBound
                let used =
                      Set.unions
                        [ Set.fromList [expectedName, actualName],
                          Map.keysSet substitution',
                          freeBackendTypeVarsIn substitution',
                          Map.keysSet parameterBounds,
                          freeBackendTypeVars expectedBody,
                          freeBackendTypeVars actualBody,
                          maybe Set.empty freeBackendTypeVars expectedBound,
                          maybe Set.empty freeBackendTypeVars actualBound
                        ]
                    freshName = freshNameLike expectedName used
                    expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                    actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                go (Set.insert freshName bound) substitution' expectedBody' actualBody'
              (BTMu expectedName expectedBody, BTMu actualName actualBody) -> do
                case (isVacuousRecursiveBinder expectedName expectedBody, isVacuousRecursiveBinder actualName actualBody) of
                  (True, True) ->
                    go bound substitution expectedBody actualBody
                  (True, False)
                    | recursiveBodyCompatible actualName actualBody expectedBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
                    | otherwise ->
                        go bound substitution expectedBody actual
                  (False, True)
                    | recursiveBodyCompatible expectedName expectedBody actualBody
                        && expectedBodyHasNoParameters expectedBody ->
                        Just substitution
                    | otherwise ->
                        go bound substitution expected actualBody
                  (False, False) -> do
                    let used =
                          Set.unions
                            [ Set.fromList [expectedName, actualName],
                              Map.keysSet substitution,
                              freeBackendTypeVarsIn substitution,
                              Map.keysSet parameterBounds,
                              freeBackendTypeVars expectedBody,
                              freeBackendTypeVars actualBody
                            ]
                        freshName = freshNameLike expectedName used
                        expectedBody' = substituteBackendType expectedName (BTVar freshName) expectedBody
                        actualBody' = substituteBackendType actualName (BTVar freshName) actualBody
                    go (Set.insert freshName bound) substitution expectedBody' actualBody'
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

    matchBackendTypeApplication bound substitution name expectedArgs actual =
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                if Map.member name parameterBounds && Set.notMember name bound
                  then insertParameterSubstitution name actualHead substitution
                  else go bound substitution (BTVar name) actualHead
              foldM
                (\substitutionAcc (expectedArg, actualArg) -> go bound substitutionAcc expectedArg actualArg)
                substitution'
                (zip expectedArgs actualArgs)
        _ -> Nothing

    requireAlphaEq substitution expected actual
      | alphaEqBackendType expected actual = Just substitution
      | otherwise = Nothing

    insertParameterSubstitution name actual substitution =
      case Map.lookup name substitution of
        Nothing ->
          if backendParameterBoundMatches name actual substitution
            then Just (Map.insert name actual substitution)
            else Nothing
        Just previous
          | alphaEqBackendType previous actual && backendParameterBoundMatches name previous substitution ->
              Just substitution
        _ ->
          Nothing

    backendParameterBoundMatches name actual substitution =
      case Map.lookup name parameterBounds of
        Just (Just boundTy)
          | not (alphaEqBackendType boundTy BTBottom) ->
              let dependencySubstitution =
                    completeBackendParameterSubstitution
                      (Map.delete name parameterBounds)
                      (Map.delete name substitution)
                  expectedBound = substituteBackendTypes dependencySubstitution boundTy
               in typeBoundDependenciesMatch actual expectedBound || actualTypeVariableBoundMatches actual expectedBound
        _ ->
          True

    typeBoundDependenciesMatch actual expectedBound =
      alphaEqBackendType
        (resolveTypeBoundDependencies actual)
        (resolveTypeBoundDependencies expectedBound)

    actualTypeVariableBoundMatches actual expectedBound =
      case actual of
        BTVar actualName ->
          case Map.lookup actualName typeBounds of
            Just (Just actualBound) ->
              typeBoundDependenciesMatch actualBound expectedBound
            _ ->
              False
        _ ->
          False

    resolveTypeBoundDependencies =
      substituteBackendTypes resolvedTypeBounds

    resolvedTypeBounds =
      completeBackendParameterSubstitution typeBounds Map.empty

    expectedBodyHasNoParameters expectedBody =
      Set.null (freeBackendTypeVars expectedBody `Set.intersection` Map.keysSet parameterBounds)

completeBackendParameterSubstitution :: BackendParameterBounds -> Map.Map String BackendType -> Map.Map String BackendType
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
      | otherwise = Map.insert name (substituteBackendTypes substitution boundTy) substitution
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
          Map.insert name (substituteBackendTypes (Map.delete name substitution) ty) substitution
        Nothing ->
          substitution

decomposeBackendTypeHead :: BackendType -> Maybe (BackendType, [BackendType])
decomposeBackendTypeHead ty =
  case ty of
    BTVar name -> Just (BTVar name, [])
    BTBase name -> Just (BTBase name, [])
    BTCon name args -> Just (BTBase name, NE.toList args)
    BTVarApp name args -> Just (BTVar name, NE.toList args)
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
        (BTBase leftBase, BTBase rightBase)
          | leftBase == rightBase ->
              Just (patternBindings, recursiveAlias)
        (BTCon leftCon leftArgs, BTCon rightCon rightArgs)
          | leftCon == rightCon ->
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
  Map.Map String (Maybe BackendType) ->
  Set.Set String ->
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
  Map.Map String (Maybe BackendType) ->
  Set.Set String ->
  BackendType ->
  BackendType ->
  Bool
structuralPayloadTypeMayInstantiate typeBounds bound expected actual =
  alphaEqBackendType expected actual
    || case (expected, actual) of
      (BTVar name, _)
        | Set.notMember name bound && Map.notMember name typeBounds ->
            True
      (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
        structuralPayloadTypeMayInstantiate typeBounds bound expectedDom actualDom
          && structuralPayloadTypeMayInstantiate typeBounds bound expectedCod actualCod
      (BTCon expectedCon expectedArgs, BTCon actualCon actualArgs) ->
        expectedCon == actualCon
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
                           Map.keysSet typeBounds,
                           maybe Set.empty freeBackendTypeVars expectedBound,
                           maybe Set.empty freeBackendTypeVars actualBound,
                           freeBackendTypeVars expectedForallBody,
                           freeBackendTypeVars actualForallBody
                         ]
                     )
                 expectedForallBody' = substituteBackendType expectedBinder (BTVar freshName) expectedForallBody
                 actualForallBody' = substituteBackendType actualBinder (BTVar freshName) actualForallBody
              in structuralPayloadTypeMayInstantiate typeBounds (Set.insert freshName bound) expectedForallBody' actualForallBody'
      (BTMu expectedMuName expectedMuBody, BTMu actualMuName actualMuBody) ->
        let freshName =
              freshNameLike
                expectedMuName
                ( Set.unions
                    [ Set.fromList [expectedMuName, actualMuName],
                      Map.keysSet typeBounds,
                      freeBackendTypeVars expectedMuBody,
                      freeBackendTypeVars actualMuBody
                    ]
                )
            expectedMuBody' = substituteBackendType expectedMuName (BTVar freshName) expectedMuBody
            actualMuBody' = substituteBackendType actualMuName (BTVar freshName) actualMuBody
         in structuralPayloadTypeMayInstantiate typeBounds (Set.insert freshName bound) expectedMuBody' actualMuBody'
      _ ->
        backendStructuralDataBoundaryMatches typeBounds Nothing expected actual

structuralPayloadMaybeBoundMayInstantiate ::
  Map.Map String (Maybe BackendType) ->
  Set.Set String ->
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

stripPrefixSimple :: String -> String -> Maybe String
stripPrefixSimple [] value =
  Just value
stripPrefixSimple _ [] =
  Nothing
stripPrefixSimple (expected : expectedRest) (actual : actualRest)
  | expected == actual = stripPrefixSimple expectedRest actualRest
  | otherwise = Nothing
