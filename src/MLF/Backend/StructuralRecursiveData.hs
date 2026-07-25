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
    BackendDataScope,
    backendDataScopeByIdentity,
    StructuralConstructorMatch (..),
    StructuralRecursiveDataMatch (..),
    StructuralRecursiveDataMismatch (..),
    alphaEqBackendType,
    backendDataScope,
    backendStructuralDataBoundaryMatches,
    completeBackendParameterSubstitution,
    decomposeBackendTypeHead,
    isVacuousRecursiveBinderWithIdentity,
    lookupTypeBound,
    matchBackendTypeParametersWithTypeBounds,
    matchConstructorResult,
    matchFocusedStructuralConstructor,
    matchStructuralDataDeclaration,
    structuralBackendHandlerFields,
    structuralDataArgumentSubstitution,
    structuralDataDeclarationMatches,
    structuralDataSelfFieldMatches,
    structuralMuAsActualDataType,
    structuralMuAsDataType,
    structuralMuHandlerTypes,
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
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityExact, sameSymbolIdentity, symbolDefiningName, symbolUniqueIdentity)
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (StructuralTypeBinderRole (..), TypeBinderIdentity, typeBinderIdentityGeneratedUnique, typeBinderIdentityStructural)
import MLF.Util.Names (freshNameLike)

type BackendParameterBounds = Map.Map BackendTypeSubstitutionKey (Maybe BackendType)

type AlphaBinderEnv = Map.Map BackendTypeSubstitutionKey (Set.Set BackendTypeSubstitutionKey)

newtype BackendDataScope
  = BackendDataScope
      { backendDataScopeByIdentity :: Map.Map SymbolIdentity BackendData
      }
  deriving (Eq, Show)

backendDataScope :: Map.Map SymbolIdentity BackendData -> BackendDataScope
backendDataScope =
  BackendDataScope

typeBoundKeyNames :: BackendParameterBounds -> Set.Set String
typeBoundKeyNames =
  Set.map backendTypeSubstitutionKeyName . Map.keysSet

lookupTypeBound :: TypeBinderIdentity -> BackendParameterBounds -> Maybe (Maybe BackendType)
lookupTypeBound identity =
  Map.lookup (backendTypeSubstitutionKeyFromIdentity identity)

data StructuralRecursiveDataMatch = StructuralRecursiveDataMatch
  { srdmDataIdentity :: SymbolIdentity,
    srdmDataName :: String,
    srdmParameterSubstitution :: Map.Map BackendTypeSubstitutionKey BackendType,
    srdmPayloadFields :: [[BackendType]]
  }
  deriving (Show)

instance Eq StructuralRecursiveDataMatch where
  left == right =
    sameSymbolIdentity (srdmDataIdentity left) (srdmDataIdentity right)
      && srdmParameterSubstitution left == srdmParameterSubstitution right
      && srdmPayloadFields left == srdmPayloadFields right

data StructuralConstructorMatch = StructuralConstructorMatch
  { srcmDataIdentity :: SymbolIdentity,
    srcmDataName :: String,
    srcmConstructorIdentity :: SymbolIdentity,
    srcmConstructorName :: String,
    srcmFieldTypes :: [BackendType]
  }
  deriving (Show)

instance Eq StructuralConstructorMatch where
  left == right =
    sameSymbolIdentity (srcmDataIdentity left) (srcmDataIdentity right)
      && sameSymbolIdentity (srcmConstructorIdentity left) (srcmConstructorIdentity right)
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
        (BTVarWithIdentity leftIdentity _, BTVarWithIdentity rightIdentity _) ->
          typeVarMatches leftEnv rightEnv leftIdentity rightIdentity
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go leftEnv rightEnv leftDom rightDom && go leftEnv rightEnv leftCod rightCod
        (BTBaseWithIdentity leftIdentity _, BTBaseWithIdentity rightIdentity _) ->
          backendTypeHeadMatches leftIdentity rightIdentity
        (BTBaseWithIdentity leftIdentity _, BTMuWithIdentity rightIdentity _ rightBody) ->
          structuralDataMatchesHead leftIdentity [] rightIdentity rightBody
        (BTMuWithIdentity leftIdentity _ leftBody, BTBaseWithIdentity rightIdentity _) ->
          structuralDataMatchesHead rightIdentity [] leftIdentity leftBody
        (BTConWithIdentity leftIdentity _ leftArgs, BTConWithIdentity rightIdentity _ rightArgs) ->
          backendTypeHeadMatches leftIdentity rightIdentity
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTConWithIdentity leftIdentity _ leftArgs, BTMuWithIdentity rightIdentity _ rightBody) ->
          structuralDataMatchesHead leftIdentity (NE.toList leftArgs) rightIdentity rightBody
        (BTMuWithIdentity leftIdentity _ leftBody, BTConWithIdentity rightIdentity _ rightArgs) ->
          structuralDataMatchesHead rightIdentity (NE.toList rightArgs) leftIdentity leftBody
        (BTVarAppWithIdentity leftIdentity _ leftArgs, BTVarAppWithIdentity rightIdentity _ rightArgs) ->
          typeVarMatches leftEnv rightEnv leftIdentity rightIdentity
            && zipAllWith (go leftEnv rightEnv) (NE.toList leftArgs) (NE.toList rightArgs)
        (BTForallWithIdentity leftIdentity _ leftBound leftBody, BTForallWithIdentity rightIdentity _ rightBound rightBody) ->
          maybeAlphaEq leftEnv rightEnv leftBound rightBound
            && let (leftEnv', rightEnv') = extendBinderScope leftIdentity rightIdentity leftEnv rightEnv
                in go leftEnv' rightEnv' leftBody rightBody
        (BTMuWithIdentity leftIdentity _ leftBody, BTMuWithIdentity rightIdentity _ rightBody) ->
          let (leftEnv', rightEnv') = extendBinderScope leftIdentity rightIdentity leftEnv rightEnv
           in go leftEnv' rightEnv' leftBody rightBody
        (BTBottom, BTBottom) ->
          True
        _ ->
          False

    extendBinderScope ::
      TypeBinderIdentity ->
      TypeBinderIdentity ->
      AlphaBinderEnv ->
      AlphaBinderEnv ->
      (AlphaBinderEnv, AlphaBinderEnv)
    extendBinderScope leftIdentity rightIdentity leftEnv rightEnv =
      (insertAliasScope leftAliases rightAliases leftEnv, insertAliasScope rightAliases leftAliases rightEnv)
      where
        leftAliases = binderAliasKeys leftIdentity
        rightAliases = binderAliasKeys rightIdentity

    insertAliasScope :: [BackendTypeSubstitutionKey] -> [BackendTypeSubstitutionKey] -> AlphaBinderEnv -> AlphaBinderEnv
    insertAliasScope aliases targets env =
      foldr (\alias -> Map.insertWith Set.union alias targetSet) env aliases
      where
        targetSet = Set.fromList targets

    binderAliasKeys :: TypeBinderIdentity -> [BackendTypeSubstitutionKey]
    binderAliasKeys identity =
      [backendTypeSubstitutionKeyFromIdentity identity]

    typeVarMatches ::
      AlphaBinderEnv ->
      AlphaBinderEnv ->
      TypeBinderIdentity ->
      TypeBinderIdentity ->
      Bool
    typeVarMatches leftEnv rightEnv leftIdentity rightIdentity =
      case (Map.lookup leftKey leftEnv, Map.lookup rightKey rightEnv) of
        (Just expectedRights, Just expectedLefts) ->
          Set.member rightKey expectedRights && Set.member leftKey expectedLefts
        (Nothing, Nothing) ->
          leftKey == rightKey
        _ ->
          False
      where
        leftKey = backendTypeSubstitutionKeyFromIdentity leftIdentity
        rightKey = backendTypeSubstitutionKeyFromIdentity rightIdentity

    maybeAlphaEq _ _ Nothing Nothing =
      True
    maybeAlphaEq leftEnv rightEnv (Just leftTy) (Just rightTy) =
      go leftEnv rightEnv leftTy rightTy
    maybeAlphaEq _ _ _ _ =
      False

structuralDataMatchesHead :: SymbolIdentity -> [BackendType] -> TypeBinderIdentity -> BackendType -> Bool
structuralDataMatchesHead dataIdentity args muIdentity body =
  structuralMuIdentityMatches dataIdentity muIdentity
    && not (opaqueBuiltinIdentity dataIdentity)
    && structuralDataPayloadMatches args muIdentity body
  where
    opaqueBuiltinIdentity identity =
      any
        ((== Just identity) . PrimitiveInventory.builtinTypeHeadIdentity)
        (Set.toList PrimitiveInventory.builtinOpaqueTypeNames)

structuralMuRefsMatch :: TypeBinderIdentity -> TypeBinderIdentity -> Bool
structuralMuRefsMatch =
  typeBinderRefMatches

structuralDataPayloadMatches :: [BackendType] -> TypeBinderIdentity -> BackendType -> Bool
structuralDataPayloadMatches args muIdentity body =
  case structuralBackendHandlerFields body of
    Nothing -> False
    Just payloadFields ->
      let payloadTypes = filter (not . recursiveSelfField muIdentity) (concat payloadFields)
       in if null args
            then null payloadTypes
            else
              if null payloadTypes
                then all isBareTypeVariable args
                else zipAllWith structuralPayloadTypeMatches args payloadTypes

structuralPayloadTypeMatches :: BackendType -> BackendType -> Bool
structuralPayloadTypeMatches left right =
  alphaEqBackendType left right

recursiveSelfField :: TypeBinderIdentity -> BackendType -> Bool
recursiveSelfField muIdentity ty =
  case ty of
    BTVarWithIdentity fieldIdentity _ ->
      structuralSelfFieldMatches muIdentity fieldIdentity
    _ ->
      False

structuralSelfFieldMatches :: TypeBinderIdentity -> TypeBinderIdentity -> Bool
structuralSelfFieldMatches muIdentity fieldIdentity =
  typeBinderRefMatches fieldIdentity muIdentity

structuralDataSelfFieldMatches :: TypeBinderIdentity -> TypeBinderIdentity -> Bool
structuralDataSelfFieldMatches muIdentity fieldIdentity =
  typeBinderRefMatches fieldIdentity muIdentity

backendTypeVarMatches :: TypeBinderIdentity -> BackendType -> Bool
backendTypeVarMatches expectedIdentity =
  \case
    BTVarWithIdentity actualIdentity _ ->
      typeBinderRefMatches actualIdentity expectedIdentity
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
        srcmDataIdentity = backendDataIdentity dataDecl,
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
      sameSymbolIdentity
        (backendConstructorIdentity expected)
        (backendConstructorIdentity candidate)

matchStructuralDataDeclaration ::
  BackendParameterBounds ->
  BackendData ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  BackendType ->
  Either StructuralRecursiveDataMismatch StructuralRecursiveDataMatch
matchStructuralDataDeclaration typeBounds dataDecl substitution =
  \case
    structuralTy@(BTMuWithIdentity muIdentity muName body)
      | structuralMuMatchesDataDecl dataDecl muIdentity -> do
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
          payloadFields <- structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity resultIdentity resultName handlers
          Right
            StructuralRecursiveDataMatch
              { srdmDataIdentity = backendDataIdentity dataDecl,
                srdmDataName = backendDataName dataDecl,
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
      matchSelectedDataShape mismatch structuralTy
  where
    constructors =
      backendDataConstructors dataDecl
    dataParameterKeySet =
      Set.fromList (backendDataParameterKeys dataDecl)
    dataSubstitution =
      Map.filterWithKey (\key _ -> Set.member key dataParameterKeySet) substitution

    matchSelectedDataShape fallback =
      \case
        structuralTy'@(BTMuWithIdentity muIdentity _ body)
          | selectedMuIdentityAllowed muIdentity -> do
              (resultIdentity, resultName, handlers) <-
                case structuralMuHandlerTypes body of
                  Just value -> Right value
                  Nothing -> Left fallback
              if length constructors == length handlers
                then pure ()
                else Left fallback
              payloadFields <-
                case structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy' muIdentity resultIdentity resultName handlers of
                  Right fields -> Right fields
                  Left _ -> Left fallback
              Right
                StructuralRecursiveDataMatch
                  { srdmDataIdentity = backendDataIdentity dataDecl,
                    srdmDataName = backendDataName dataDecl,
                    srdmParameterSubstitution = dataSubstitution,
                    srdmPayloadFields = payloadFields
                  }
        _ -> Left fallback

    selectedMuIdentityAllowed muIdentity =
      isGeneratedTypeBinder muIdentity
        || structuralMuMatchesDataDecl dataDecl muIdentity

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
  go
  where
    go = backendStructuralDataBoundaryMatchesCore

backendStructuralDataBoundaryMatchesCore ::
  BackendParameterBounds ->
  Maybe BackendDataScope ->
  BackendType ->
  BackendType ->
  Bool
backendStructuralDataBoundaryMatchesCore typeBounds mbDataDecls expectedTy actualTy =
  go expectedTy actualTy
  where
    typeHeadMatches =
      backendTypeHeadMatches

    go expected actual
      | structuralMuTypesHaveBinderIdentityMismatch expected actual =
          False
      | otherwise =
          alphaEqWithinDataScope expected actual
            || case (expected, actual) of
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                go expectedDom actualDom && go expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity _, BTBaseWithIdentity actualIdentity _) ->
                typeHeadMatches expectedIdentity actualIdentity
              (BTBaseWithIdentity expectedDataIdentity _, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity [] actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTBaseWithIdentity actualDataIdentity _) ->
                structuralMuMatchesKnownData actualDataIdentity [] expectedIdentity expectedName expectedBody
              (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs) ->
                typeHeadMatches expectedIdentity actualIdentity
                  && zipAllWith go (NE.toList expectedArgs) (NE.toList actualArgs)
              (BTConWithIdentity expectedDataIdentity _ expectedArgs, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuMatchesKnownData expectedDataIdentity (NE.toList expectedArgs) actualIdentity actualName actualBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTConWithIdentity actualDataIdentity _ actualArgs) ->
                structuralMuMatchesKnownData actualDataIdentity (NE.toList actualArgs) expectedIdentity expectedName expectedBody
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
                structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
                  || structuralPayloadsMayInstantiate typeBounds expectedIdentity expectedName expectedBody actualIdentity actualName actualBody
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity actualName actualBound actualBody) ->
                maybeBoundaryMatches expectedBound actualBound
                  && let freshName = freshBinderName expectedName actualName expectedBound actualBound expectedBody actualBody
                         freshTy = freshBackendTypeBinderVar expectedIdentity freshName
                         expectedBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedBody
                         actualBody' = substituteBackendTypeForBinder actualIdentity freshTy actualBody
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
        (BTBaseWithIdentity {}, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTBaseWithIdentity {}, Just {}) -> True
        (BTConWithIdentity {}, BTMuWithIdentity {}, Just {}) -> True
        (BTMuWithIdentity {}, BTConWithIdentity {}, Just {}) -> True
        _ -> False

    structuralMuBodiesMatchKnownData expectedIdentity expectedName expectedBody actualIdentity actualName actualBody =
      case
        ( structuralDataDeclForMuPair expectedIdentity actualIdentity,
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
                      freshSelfTy = freshBackendTypeBinderVar expectedIdentity freshSelf
                      freshResultTy = freshBackendTypeBinderVar expectedResultIdentity freshResult
                      normalizeHandler selfIdentity resultIdentity =
                        substituteBackendTypesByKey
                          ( Map.fromList $
                              binderReplacement selfIdentity freshSelfTy
                                ++ binderReplacement resultIdentity freshResultTy
                          )
                   in zipAllWith
                        go
                        (map (normalizeHandler expectedIdentity expectedResultIdentity) expectedHandlers)
                        (map (normalizeHandler actualIdentity actualResultIdentity) actualHandlers)
          _ ->
            False

    structuralDataDeclForMuPair expectedIdentity actualIdentity =
      case (structuralSelfIdentityUnique expectedIdentity, structuralSelfIdentityUnique actualIdentity) of
        (Just expectedOwner, Just actualOwner)
          | expectedOwner == actualOwner ->
              lookupDataByStructuralSelfIdentity expectedIdentity
        _ ->
          Nothing

    structuralMuHandlerTypesWithIdentity =
      \case
        BTForallWithIdentity resultIdentity resultName _ handlerTy -> do
          handlers <- collectHandlerTypes resultIdentity handlerTy
          Just (resultIdentity, resultName, handlers)
        _ -> Nothing

    collectHandlerTypes resultIdentity =
      collect []
      where
        collect handlers ty
          | backendTypeVarMatches resultIdentity ty = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> collect (handlers ++ [handlerTy]) rest
                _ -> Nothing

    binderReplacement identity replacement =
      [(backendTypeSubstitutionKeyFromIdentity identity, replacement)]

    maybeBoundaryMatches Nothing Nothing =
      True
    maybeBoundaryMatches (Just expectedBound) (Just actualBound) =
      go expectedBound actualBound
    maybeBoundaryMatches _ _ =
      False

    structuralMuMatchesKnownData dataIdentity args muIdentity muName body =
      structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity
        && case matchingDataDecl dataIdentity muIdentity of
          Just dataDecl
            | structuralMuMatchesSelectedData dataIdentity dataDecl muIdentity,
              Just substitution <- structuralDataArgumentSubstitution dataDecl args ->
                structuralDataDeclarationMatchesSelectedByIdentity dataIdentity dataDecl substitution (BTMuWithIdentity muIdentity muName body)
          _ ->
            False

    structuralSelfIdentityCompatibleWithDataIdentity dataIdentity muIdentity =
      structuralSelfIdentityUnique muIdentity == Just (symbolUniqueIdentity dataIdentity)

    matchingDataDecl dataIdentity muIdentity =
      lookupDataByIdentity dataIdentity
        <|> lookupDataByStructuralSelfIdentity muIdentity

    lookupDataByIdentity identity = do
      BackendDataScope dataDeclsByIdentity <- mbDataDecls
      lookupSymbolIdentityExact identity dataDeclsByIdentity

    lookupDataByStructuralSelfIdentity muIdentity = do
      unique <- structuralSelfIdentityUnique muIdentity
      BackendDataScope dataDeclsByIdentity <- mbDataDecls
      case
        [ dataDecl
        | dataDecl <- Map.elems dataDeclsByIdentity,
          symbolUniqueIdentity (backendDataIdentity dataDecl) == unique
        ]
        of
        [dataDecl] -> Just dataDecl
        _ -> Nothing

    structuralSelfIdentityUnique muIdentity = do
      (unique, StructuralSelfBinder) <- typeBinderIdentityStructural muIdentity
      pure unique

    structuralSelfIdentityMatchesData muIdentity dataDecl =
      structuralSelfIdentityUnique muIdentity
        == Just (symbolUniqueIdentity (backendDataIdentity dataDecl))

    structuralMuMatchesSelectedData dataIdentity dataDecl muIdentity =
      sameSymbolIdentity (backendDataIdentity dataDecl) dataIdentity
        && structuralSelfIdentityMatchesData muIdentity dataDecl

    structuralDataDeclarationMatchesSelectedByIdentity dataIdentity dataDecl substitution structuralTy =
      if sameSymbolIdentity (backendDataIdentity dataDecl) dataIdentity
        then structuralDataDeclarationShapeMatches dataDecl substitution structuralTy
        else structuralDataDeclarationMatches typeBounds dataDecl substitution structuralTy

    structuralDataDeclarationShapeMatches dataDecl substitution structuralTy =
      case structuralTy of
        BTMuWithIdentity muIdentity _ body ->
          case structuralMuHandlerTypes body of
            Just (resultIdentity, resultName, handlers)
              | length handlers == length (backendDataConstructors dataDecl) ->
                  case structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity resultIdentity resultName handlers of
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
  TypeBinderIdentity ->
  String ->
  BackendType ->
  TypeBinderIdentity ->
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
           freshTy = freshBackendTypeBinderVar expectedIdentity freshSelf
           freshKey = freshBackendTypeBinderKey expectedIdentity
           expectedBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedBody
           actualBody' = substituteBackendTypeForBinder actualIdentity freshTy actualBody
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
      structuralMuRefsMatch expectedIdentity actualIdentity

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
    (BTMuWithIdentity leftIdentity _ leftBody, BTMuWithIdentity rightIdentity _ rightBody) ->
      structuralMuBinderIdentityMismatch leftIdentity rightIdentity
        || structuralMuTypesHaveBinderIdentityMismatch leftBody rightBody
    _ ->
      False

structuralMuBinderIdentityMismatch :: TypeBinderIdentity -> TypeBinderIdentity -> Bool
structuralMuBinderIdentityMismatch expectedIdentity actualIdentity =
  case (typeBinderIdentityStructural expectedIdentity, typeBinderIdentityStructural actualIdentity) of
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

structuralMuMatchesDataDecl :: BackendData -> TypeBinderIdentity -> Bool
structuralMuMatchesDataDecl dataDecl muIdentity =
  structuralMuIdentityMatches (backendDataIdentity dataDecl) muIdentity

structuralMuIdentityMatches :: SymbolIdentity -> TypeBinderIdentity -> Bool
structuralMuIdentityMatches dataIdentity identity
  | Just (unique, StructuralSelfBinder) <- typeBinderIdentityStructural identity =
      unique == symbolUniqueIdentity dataIdentity
structuralMuIdentityMatches _ _ =
  False

structuralMuAsDataType :: SymbolIdentity -> [BackendDataParameterRef] -> TypeBinderIdentity -> Maybe BackendType
structuralMuAsDataType dataIdentity dataParameterRefs muIdentity = do
  guard (structuralMuIdentityCompatible dataIdentity muIdentity)
  let dataName = symbolDefiningName dataIdentity
  let parameterArgs = map backendDataParameterRefType dataParameterRefs
  Just $
    case parameterArgs of
      [] -> BTBaseWithIdentity dataIdentity (BaseTy dataName)
      arg : rest -> BTConWithIdentity dataIdentity (BaseTy dataName) (arg :| rest)

structuralMuAsActualDataType :: SymbolIdentity -> TypeBinderIdentity -> BackendType -> Maybe BackendType
structuralMuAsActualDataType dataIdentity muIdentity actual =
  case actual of
    BTBaseWithIdentity actualIdentity _
      | structuralMuHeadMatches dataIdentity actualIdentity muIdentity -> Just actual
    BTConWithIdentity actualIdentity _ _
      | structuralMuHeadMatches dataIdentity actualIdentity muIdentity -> Just actual
    _ -> Nothing
  where
    structuralMuHeadMatches expected actualIdentity identity =
      sameSymbolIdentity expected actualIdentity
        && structuralMuIdentityCompatible expected identity

structuralMuIdentityCompatible :: SymbolIdentity -> TypeBinderIdentity -> Bool
structuralMuIdentityCompatible =
  structuralMuIdentityMatches

isGeneratedTypeBinder :: TypeBinderIdentity -> Bool
isGeneratedTypeBinder =
  maybe False (const True) . typeBinderIdentityGeneratedUnique

nominalBackendDataIdentity :: BackendType -> Maybe SymbolIdentity
nominalBackendDataIdentity =
  \case
    BTBaseWithIdentity identity _ -> Just identity
    BTConWithIdentity identity _ _ -> Just identity
    _ -> Nothing

structuralMuPayloadTypes :: BackendType -> Maybe [BackendType]
structuralMuPayloadTypes body =
  concat <$> structuralBackendHandlerFields body

structuralMuHandlerTypes :: BackendType -> Maybe (TypeBinderIdentity, String, [BackendType])
structuralMuHandlerTypes =
  \case
    BTForallWithIdentity resultIdentity resultName _ handlerTy -> do
      handlers <- collectHandlerTypes resultIdentity handlerTy
      Just (resultIdentity, resultName, handlers)
    _ -> Nothing
  where
    collectHandlerTypes resultIdentity =
      go []
      where
        go handlers ty
          | backendTypeVarMatches resultIdentity ty = Just handlers
          | otherwise =
              case ty of
                BTArrow handlerTy rest -> go (handlers ++ [handlerTy]) rest
                _ -> Nothing

structuralBackendHandlerFields :: BackendType -> Maybe [[BackendType]]
structuralBackendHandlerFields =
  \case
    BTForallWithIdentity resultIdentity _ _ handlerTy -> collectHandlers resultIdentity handlerTy
    _ -> Nothing
  where
    collectHandlers resultIdentity =
      go []
      where
        go handlers ty
          | backendTypeVarMatches resultIdentity ty = Just handlers
          | otherwise =
              case ty of
                BTForall _ _ body -> go handlers body
                BTArrow handlerTy rest -> do
                  fields <- collectHandlerFields resultIdentity handlerTy
                  go (handlers ++ [fields]) rest
                _ -> Nothing

    collectHandlerFields resultIdentity =
      go []
      where
        go fields ty
          | backendTypeVarMatches resultIdentity ty = Just fields
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
  TypeBinderIdentity ->
  TypeBinderIdentity ->
  String ->
  [BackendType] ->
  Either StructuralRecursiveDataMismatch [[BackendType]]
structuralPayloadHandlersMatchForData typeBounds dataDecl substitution structuralTy muIdentity resultIdentity resultName handlers =
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
      case typeBinderIdentityGeneratedUnique muIdentity of
        Just {} ->
          case map (substituteBackendTypesByKey dataSubstitution . backendDataParameterRefType) dataParameterRefs of
            [] -> BTBaseWithIdentity (backendDataIdentity dataDecl) (BaseTy dataName)
            arg : args -> BTConWithIdentity (backendDataIdentity dataDecl) (BaseTy dataName) (arg :| args)
        _ -> structuralTyWithData
    substituteKnownTypes =
      substituteStructuralSelfExact muIdentity structuralSelfTy . substituteBackendTypesByKey dataSubstitution
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
          Set.insert (backendTypeSubstitutionKeyFromIdentity resultIdentity) $
            case typeBinderIdentityStructural muIdentity of
              Just {} -> Set.singleton (backendTypeSubstitutionKeyFromIdentity muIdentity)
              Nothing -> Set.empty
        handlerFieldsOrPayloadError =
          case structuralHandlerFields resultIdentity actualHandlerTy of
            Just fields -> Right fields
            Nothing -> Left (StructuralRecursiveDataPayloadUnavailable dataName)
        parameters =
          Map.map (fmap substituteKnownTypes) $
            constructorTypeParameterBoundsForData dataDecl constructor

substituteStructuralSelfExact :: TypeBinderIdentity -> BackendType -> BackendType -> BackendType
substituteStructuralSelfExact targetIdentity replacement =
  go
  where
    go =
      \case
        BTVarWithIdentity identity name
          | targetBinderMatches identity -> replacement
          | otherwise -> BTVarWithIdentity identity name
        BTArrow dom cod ->
          BTArrow (go dom) (go cod)
        ty@BTBaseWithIdentity {} ->
          ty
        BTConWithIdentity identity con args ->
          BTConWithIdentity identity con (fmap go args)
        BTVarAppWithIdentity identity name args
          | targetBinderMatches identity ->
              case applyExactReplacement (NE.toList (fmap go args)) of
                Just ty -> ty
                Nothing -> BTVarAppWithIdentity identity name (fmap go args)
          | otherwise -> BTVarAppWithIdentity identity name (fmap go args)
        BTForallWithIdentity identity name mbBound body ->
          let mbBound' = fmap go mbBound
           in if targetBinderMatches identity
                then BTForallWithIdentity identity name mbBound' body
                else BTForallWithIdentity identity name mbBound' (go body)
        BTMuWithIdentity identity name body
          | targetBinderMatches identity ->
              BTMuWithIdentity identity name body
          | otherwise ->
              BTMuWithIdentity identity name (go body)
        BTBottom ->
          BTBottom

    targetBinderMatches identity =
      identity == targetIdentity

    applyExactReplacement args =
      case (replacement, args) of
        (_, []) -> Just replacement
        (BTVarWithIdentity identity name, arg : rest) -> Just (BTVarAppWithIdentity identity name (arg :| rest))
        (BTBaseWithIdentity identity base, arg : rest) -> Just (BTConWithIdentity identity base (arg :| rest))
        (BTConWithIdentity identity base existing, _) -> Just (BTConWithIdentity identity base (existing <> NE.fromList args))
        (BTVarAppWithIdentity identity name existing, _) -> Just (BTVarAppWithIdentity identity name (existing <> NE.fromList args))
        _ -> Nothing

constructorStructuralHandlerType :: TypeBinderIdentity -> String -> BackendConstructor -> BackendType
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

structuralHandlerFields :: TypeBinderIdentity -> BackendType -> Maybe [BackendType]
structuralHandlerFields resultIdentity =
  go []
  where
    go fields ty
      | backendTypeVarMatches resultIdentity ty = Just fields
      | otherwise =
          case ty of
            BTForall _ _ body -> go fields body
            BTArrow fieldTy rest -> go (fields ++ [fieldTy]) rest
            _ -> Nothing

constructorTypeParameterBoundsForData :: BackendData -> BackendConstructor -> BackendParameterBounds
constructorTypeParameterBoundsForData dataDecl constructor =
  Map.fromList $
    [(key, Nothing) | key <- backendDataParameterKeys dataDecl]
      ++ [ (backendTypeSubstitutionKeyFromIdentity (backendTypeBinderIdentity binder), backendTypeBinderBound binder)
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
    BTVarWithIdentity identity _
      | Set.member key parameters ->
          case Map.lookup key substitution of
            Nothing -> Just (Map.insert key actual substitution)
            Just previous
              | alphaEqBackendType previous actual -> Just substitution
              | otherwise -> Nothing
      where
        key = parameterKey identity
    _ ->
      if alphaEqBackendType expected actual
        then Just substitution
        else
            ( case (expected, actual) of
              (BTVarWithIdentity expectedIdentity _, BTVarWithIdentity actualIdentity _)
                | typeBinderRefMatches expectedIdentity actualIdentity ->
                    Just substitution
              (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedDom actualDom
                  >>= \subst -> matchConstructorResult dataParameterOrder parameters subst expectedCod actualCod
              (BTBaseWithIdentity expectedIdentity _, BTBaseWithIdentity actualIdentity _)
                | backendTypeHeadMatches expectedIdentity actualIdentity -> Just substitution
              (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs)
                | backendTypeHeadMatches expectedIdentity actualIdentity && length expectedArgs == length actualArgs ->
                    foldM
                      (\subst (expectedArg, actualArg) -> matchConstructorResult dataParameterOrder parameters subst expectedArg actualArg)
                      substitution
                      (zip (NE.toList expectedArgs) (NE.toList actualArgs))
              (BTMuWithIdentity expectedIdentity _ expectedBody, actualTy@(BTBase {})) ->
                matchStructuralMuExpected expectedIdentity expectedBody actualTy
              (BTMuWithIdentity expectedIdentity _ expectedBody, actualTy@(BTCon {})) ->
                matchStructuralMuExpected expectedIdentity expectedBody actualTy
              (expectedTy@(BTBase {}), BTMuWithIdentity actualIdentity _ actualBody) ->
                matchStructuralMuActual expectedTy actualIdentity actualBody
              (expectedTy@(BTCon {}), BTMuWithIdentity actualIdentity _ actualBody) ->
                matchStructuralMuActual expectedTy actualIdentity actualBody
              (BTVarAppWithIdentity expectedIdentity expectedName expectedArgs, _) ->
                matchConstructorResultApplication dataParameterOrder parameters substitution expectedIdentity expectedName (NE.toList expectedArgs) actual
              (BTForallWithIdentity expectedIdentity expectedName expectedBound expectedBody, BTForallWithIdentity actualIdentity _ actualBound actualBody) -> do
                subst <-
                  case (expectedBound, actualBound) of
                    (Nothing, Nothing) -> Just substitution
                    (Just expectedBoundTy, Just actualBoundTy) -> matchConstructorResult dataParameterOrder parameters substitution expectedBoundTy actualBoundTy
                    _ -> Nothing
                matchConstructorResult dataParameterOrder parameters subst expectedBody (substituteBackendTypeForBinder actualIdentity (BTVarWithIdentity expectedIdentity expectedName) actualBody)
              (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity _ actualBody) ->
                matchConstructorResult dataParameterOrder parameters substitution expectedBody (substituteBackendTypeForBinder actualIdentity (BTVarWithIdentity expectedIdentity expectedName) actualBody)
              (BTBottom, BTBottom) ->
                Just substitution
              _ ->
                Nothing
          )
  where
    parameterKey identity =
      backendTypeSubstitutionKeyFromIdentity identity

    matchStructuralMuExpected muIdentity _body actualTy =
      let dataIdentity = nominalBackendDataIdentity actualTy
       in firstJust
            [ (dataIdentity >>= \identity -> structuralMuAsDataType identity dataParameterOrder muIdentity)
                >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
              (dataIdentity >>= \identity -> structuralMuAsActualDataType identity muIdentity actualTy)
                >>= \expectedTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
            ]

    matchStructuralMuActual expectedTy muIdentity _body =
      let dataIdentity = nominalBackendDataIdentity expectedTy
       in firstJust
            [ (dataIdentity >>= \identity -> structuralMuAsDataType identity dataParameterOrder muIdentity)
                >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy,
              (dataIdentity >>= \identity -> structuralMuAsActualDataType identity muIdentity expectedTy)
                >>= \actualTy -> matchConstructorResult dataParameterOrder parameters substitution expectedTy actualTy
            ]

matchConstructorResultApplication ::
  [BackendDataParameterRef] ->
  Set.Set BackendTypeSubstitutionKey ->
  Map.Map BackendTypeSubstitutionKey BackendType ->
  TypeBinderIdentity ->
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
      backendTypeSubstitutionKeyFromIdentity identity

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
        BTVarWithIdentity binderIdentity _ ->
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
    matchParameterKey identity =
      if Map.member key parameterBounds
        then Just key
        else Nothing
      where
        key = backendTypeSubstitutionKeyFromIdentity identity

    go bound substitution expected actual =
      case expected of
        BTVarWithIdentity identity _
          | Just key <- matchParameterKey identity,
            Set.notMember key bound ->
              insertParameterSubstitution key actual substitution
        _ ->
          case (expected, actual) of
            (BTVar {}, _) ->
              requireAlphaEq substitution expected actual
            (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
              go bound substitution expectedDom actualDom
                >>= \substitution' -> go bound substitution' expectedCod actualCod
            (BTBaseWithIdentity expectedIdentity _, BTBaseWithIdentity actualIdentity _)
              | backendTypeHeadMatches expectedIdentity actualIdentity ->
                  Just substitution
            (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs)
              | backendTypeHeadMatches expectedIdentity actualIdentity ->
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
            (BTMuWithIdentity expectedIdentity _ expectedBody, actualTy@(BTBase {})) ->
              matchStructuralMuExpected bound substitution expectedIdentity expectedBody actualTy
            (BTMuWithIdentity expectedIdentity _ expectedBody, actualTy@(BTCon {})) ->
              matchStructuralMuExpected bound substitution expectedIdentity expectedBody actualTy
            (expectedTy@(BTBase {}), BTMuWithIdentity actualIdentity _ actualBody) ->
              matchStructuralMuActual bound substitution expectedTy actualIdentity actualBody
            (expectedTy@(BTCon {}), BTMuWithIdentity actualIdentity _ actualBody) ->
              matchStructuralMuActual bound substitution expectedTy actualIdentity actualBody
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
                  freshTy = freshBackendTypeBinderVar expectedIdentity freshName
                  freshKey = freshBackendTypeBinderKey expectedIdentity
                  expectedBody' = substituteBinder expectedIdentity freshTy expectedBody
                  actualBody' = substituteBinder actualIdentity freshTy actualBody
              go (Set.insert freshKey bound) substitution' expectedBody' actualBody'
            (BTMuWithIdentity expectedIdentity expectedName expectedBody, BTMuWithIdentity actualIdentity actualName actualBody) ->
              if structuralMuBinderIdentityMismatch expectedIdentity actualIdentity
                then Nothing
                else
                  case
                    ( null dataParameterOrder && sameStructuralDataRef expectedIdentity actualIdentity,
                      isVacuousRecursiveBinderWithIdentity expectedIdentity expectedBody,
                      isVacuousRecursiveBinderWithIdentity actualIdentity actualBody
                    )
                  of
                    (True, _, _) ->
                      Just substitution
                    (_, True, True) ->
                      go bound substitution expectedBody actualBody
                    (_, True, False)
                      | recursiveBodyCompatibleWithIdentity actualIdentity actualBody expectedBody
                          && expectedBodyHasNoParameters expectedBody ->
                          Just substitution
                      | expectedBodyHasNoParameters expectedBody ->
                          Nothing
                      | otherwise ->
                          go bound substitution expectedBody actual
                    (_, False, True)
                      | recursiveBodyCompatibleWithIdentity expectedIdentity expectedBody actualBody
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
                          freshTy = freshBackendTypeBinderVar expectedIdentity freshName
                          freshKey = freshBackendTypeBinderKey expectedIdentity
                          expectedBody' = substituteBinder expectedIdentity freshTy expectedBody
                          actualBody' = substituteBinder actualIdentity freshTy actualBody
                      go (Set.insert freshKey bound) substitution expectedBody' actualBody'
            (BTMuWithIdentity expectedIdentity _ expectedBody, _)
              | isVacuousRecursiveBinderWithIdentity expectedIdentity expectedBody ->
                  go bound substitution expectedBody actual
            (_, BTMuWithIdentity actualIdentity _ actualBody)
              | isVacuousRecursiveBinderWithIdentity actualIdentity actualBody ->
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

    matchStructuralMuExpected bound substitution muIdentity body actualTy =
      let dataIdentity = nominalBackendDataIdentity actualTy
       in firstJust
            [ structuralMuNominalTypeMatches actualTy muIdentity body >>= \() -> Just substitution,
              (dataIdentity >>= \identity -> structuralMuAsDataTypeForBody identity muIdentity body)
                >>= \expectedTy -> go bound substitution expectedTy actualTy,
              (dataIdentity >>= \identity -> structuralMuAsActualDataType identity muIdentity actualTy)
                >>= \expectedTy -> go bound substitution expectedTy actualTy
            ]

    matchStructuralMuActual bound substitution expectedTy muIdentity body =
      let dataIdentity = nominalBackendDataIdentity expectedTy
       in firstJust
            [ structuralMuNominalTypeMatches expectedTy muIdentity body >>= \() -> Just substitution,
              (dataIdentity >>= \identity -> structuralMuAsDataTypeForBody identity muIdentity body)
                >>= \actualTy -> go bound substitution expectedTy actualTy,
              (dataIdentity >>= \identity -> structuralMuAsActualDataType identity muIdentity expectedTy)
                >>= \actualTy -> go bound substitution expectedTy actualTy
            ]

    structuralMuAsDataTypeForBody dataIdentity muIdentity body =
      structuralMuPayloadTypes body *> structuralMuAsDataType dataIdentity dataParameterOrder muIdentity

    structuralMuNominalTypeMatches nominalTy muIdentity body =
      if nominalMatches
        then Just ()
        else Nothing
      where
        nominalMatches =
          case nominalTy of
            BTBaseWithIdentity identity _ ->
              structuralDataMatchesHead identity [] muIdentity body
            BTConWithIdentity identity _ args ->
              structuralDataMatchesHead identity (NE.toList args) muIdentity body
            _ ->
              False

    matchBackendTypeApplication bound substitution identity name expectedArgs actual =
      case decomposeBackendTypeHead actual of
        Just (actualHead, actualArgs)
          | length expectedArgs == length actualArgs -> do
              substitution' <-
                case matchParameterKey identity of
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
        BTVarWithIdentity identity _ ->
          matchParameterKey identity == Just key
        _ ->
          False

    repeatedParameterTypeMatches previous actual =
      not (structuralMuTypesHaveBinderIdentityMismatch previous actual)
        && (alphaEqBackendType previous actual || sameStructuralType previous actual)

    sameStructuralType left right =
      case (left, right) of
        (BTMuWithIdentity leftIdentity _ leftBody, BTMuWithIdentity rightIdentity _ rightBody)
          | not (structuralMuRefsMatch leftIdentity rightIdentity) ->
              False
          | otherwise ->
              identityPayloadMatches leftIdentity leftBody
                && identityPayloadMatches rightIdentity rightBody
        _ ->
          False

    identityPayloadMatches identity body =
      structuralDataPayloadMatches [] identity body

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
        BTVarWithIdentity actualIdentity _ ->
          case lookupTypeBound actualIdentity typeBounds of
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

    sameStructuralDataRef expectedIdentity actualIdentity =
      structuralMuRefsMatch expectedIdentity actualIdentity

    substituteBinder identity replacement =
      substituteBackendTypesByKey (Map.fromList (binderReplacement identity replacement))

    binderReplacement identity replacement =
      [(backendTypeSubstitutionKeyFromIdentity identity, replacement)]

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

isVacuousRecursiveBinderWithIdentity :: TypeBinderIdentity -> BackendType -> Bool
isVacuousRecursiveBinderWithIdentity identity body =
  Set.notMember (backendTypeSubstitutionKeyFromIdentity identity) (freeBackendTypeVarKeys body)

recursiveBodyCompatibleWithIdentity :: TypeBinderIdentity -> BackendType -> BackendType -> Bool
recursiveBodyCompatibleWithIdentity recursiveIdentity recursiveBody plainBody =
  case go Set.empty Map.empty Nothing recursiveBody plainBody of
    Just _ -> True
    Nothing -> False
  where
    go patternVars patternBindings recursiveAlias leftTy rightTy =
      case (leftTy, rightTy) of
        (BTVarWithIdentity identity _, _)
          | typeBinderRefMatches identity recursiveIdentity ->
              matchRecursiveAlias patternVars patternBindings recursiveAlias rightTy
          | Set.member (recursiveKey identity) patternVars ->
              matchPatternVar (recursiveKey identity) patternBindings recursiveAlias rightTy
        (BTVarWithIdentity leftIdentity _, BTVarWithIdentity rightIdentity _)
          | typeBinderRefMatches leftIdentity rightIdentity ->
              Just (patternBindings, recursiveAlias)
        (BTArrow leftDom leftCod, BTArrow rightDom rightCod) ->
          go patternVars patternBindings recursiveAlias leftDom rightDom
            >>= \(patternBindings', recursiveAlias') ->
              go patternVars patternBindings' recursiveAlias' leftCod rightCod
        (BTBaseWithIdentity leftIdentity _, BTBaseWithIdentity rightIdentity _)
          | backendTypeHeadMatches leftIdentity rightIdentity ->
              Just (patternBindings, recursiveAlias)
        (BTConWithIdentity leftIdentity _ leftArgs, BTConWithIdentity rightIdentity _ rightArgs)
          | backendTypeHeadMatches leftIdentity rightIdentity ->
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
              freshTy = freshBackendTypeBinderVar leftIdentity freshName
              leftBody' = substituteBackendTypeForBinder leftIdentity freshTy leftBody
              rightBody' = substituteBackendTypeForBinder rightIdentity freshTy rightBody
           in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForallWithIdentity leftIdentity leftName (Just leftBound) leftBody, BTForallWithIdentity rightIdentity rightName (Just rightBound) rightBody)
          | alphaEqBackendType leftBound rightBound ->
              let freshName = freshRecursiveBodyBinder leftName rightName leftBody rightBody
                  freshTy = freshBackendTypeBinderVar leftIdentity freshName
                  leftBody' = substituteBackendTypeForBinder leftIdentity freshTy leftBody
                  rightBody' = substituteBackendTypeForBinder rightIdentity freshTy rightBody
               in go patternVars patternBindings recursiveAlias leftBody' rightBody'
        (BTForallWithIdentity leftIdentity _ Nothing leftBody, _) ->
          go (Set.insert (backendTypeSubstitutionKeyFromIdentity leftIdentity) patternVars) patternBindings recursiveAlias leftBody rightTy
        (_, BTForallWithIdentity rightIdentity rightName Nothing rightBody)
          | Set.member recursiveBinderKey (freeBackendTypeVarKeys leftTy) ->
              let aliasName = freshNameLike rightName (freeBackendTypeVars leftTy `Set.union` freeBackendTypeVars rightBody)
                  aliasTy = BTVarWithIdentity rightIdentity aliasName
                  aliasKey = backendTypeSubstitutionKeyFromIdentity rightIdentity
                  rightBody' = substituteBackendTypeForBinder rightIdentity aliasTy rightBody
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
      backendTypeSubstitutionKeyFromIdentity recursiveIdentity

    recursiveKey identity =
      backendTypeSubstitutionKeyFromIdentity identity

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
              aliasKey = backendTypeSubstitutionKeyFromIdentity rightIdentity
              rightBody' = substituteBackendTypeForBinder rightIdentity aliasTy rightBody
           in case recursiveAlias of
                Nothing ->
                  matchRecursiveAlias patternVars patternBindings (Just aliasKey) rightBody'
                Just expectedKey
                  | expectedKey == aliasKey ->
                      matchRecursiveAlias patternVars patternBindings recursiveAlias rightBody'
                _ ->
                  Nothing
        BTVarWithIdentity rightIdentity _ ->
          let rightKey = backendTypeSubstitutionKeyFromIdentity rightIdentity
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
            [ Set.fromList [leftName, rightName],
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
  structuralPayloadTypeMatches expected actual
    || case (expected, actual) of
      (BTVarWithIdentity identity _, _)
        | Set.notMember key bound && Map.notMember key typeBounds ->
            True
        where
          key = backendTypeSubstitutionKeyFromIdentity identity
      (BTArrow expectedDom expectedCod, BTArrow actualDom actualCod) ->
        structuralPayloadTypeMayInstantiate typeBounds bound expectedDom actualDom
          && structuralPayloadTypeMayInstantiate typeBounds bound expectedCod actualCod
      (BTConWithIdentity expectedIdentity _ expectedArgs, BTConWithIdentity actualIdentity _ actualArgs) ->
        backendTypeHeadMatches expectedIdentity actualIdentity
          && zipAllWith
            (structuralPayloadTypeMayInstantiate typeBounds bound)
            (NE.toList expectedArgs)
            (NE.toList actualArgs)
      (BTVarAppWithIdentity expectedIdentity _ expectedArgs, BTVarAppWithIdentity actualIdentity _ actualArgs) ->
        typeBinderRefMatches expectedIdentity actualIdentity
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
                 freshTy = freshBackendTypeBinderVar expectedIdentity freshName
                 freshKey = freshBackendTypeBinderKey expectedIdentity
                 expectedForallBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedForallBody
                 actualForallBody' = substituteBackendTypeForBinder actualIdentity freshTy actualForallBody
              in structuralPayloadTypeMayInstantiate typeBounds (Set.insert freshKey bound) expectedForallBody' actualForallBody'
      (BTMuWithIdentity expectedIdentity expectedMuName expectedMuBody, BTMuWithIdentity actualIdentity actualMuName actualMuBody) ->
        structuralMuRefsMatch expectedIdentity actualIdentity
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
                 freshTy = freshBackendTypeBinderVar expectedIdentity freshName
                 freshKey = freshBackendTypeBinderKey expectedIdentity
                 expectedMuBody' = substituteBackendTypeForBinder expectedIdentity freshTy expectedMuBody
                 actualMuBody' = substituteBackendTypeForBinder actualIdentity freshTy actualMuBody
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

freshBackendTypeBinderVar :: TypeBinderIdentity -> String -> BackendType
freshBackendTypeBinderVar leftIdentity =
  BTVarWithIdentity leftIdentity

freshBackendTypeBinderKey :: TypeBinderIdentity -> BackendTypeSubstitutionKey
freshBackendTypeBinderKey leftIdentity =
  backendTypeSubstitutionKeyFromIdentity leftIdentity

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
