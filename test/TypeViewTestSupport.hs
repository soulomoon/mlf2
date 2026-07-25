{-# LANGUAGE GADTs #-}

module TypeViewTestSupport
  ( mkTypeView,
    fixtureTypeView,
    setTypeViewDisplay,
    setTypeViewIdentity,
    setTypeViewTypes,
    setTypeViewHeadIdentities,
    setTypeViewBinderIdentities,
  )
where

import Control.Applicative ((<|>))
import Data.Char (ord)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.Frontend.Program.Types
  ( SymbolIdentity,
    SymbolNamespace (SymbolType),
    TypeView,
    symbolIdentityFromParts,
    typeViewBinderIdentities,
    typeViewDisplay,
    typeViewFromSourceType,
    typeViewHeadIdentities,
    typeViewIdentity,
    typeViewWithDisplay,
  )
import MLF.Frontend.Program.Builtins (builtinTypeHeadIdentity)
import MLF.Frontend.Symbol (lookupSymbolIdentityAlias, sameSymbolIdentity, symbolIdentityStableName)
import MLF.Frontend.Syntax (SrcBound (..), SrcTy (..), SrcType)
import MLF.Types.Identity
  ( StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
    lookupTypeBinderIdentityAlias,
    typeBinderIdentityStableName,
  )
import MLF.Types.Unique (UniqueIdentity (..))

mkTypeView :: SrcType -> SrcType -> TypeView
mkTypeView display identity =
  fixtureTypeView display identity Map.empty Map.empty

fixtureTypeView :: SrcType -> SrcType -> Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeView
fixtureTypeView display identity headIdentities binderIdentities =
  case typeViewFromSourceType completeHeadIdentities completeBinderIdentities identity of
    Right identityView ->
      case typeViewWithDisplay display identityView of
        Right view -> view
        Left err -> invalidFixture err
    Left err -> invalidFixture err
  where
    invalidFixture :: Show err => err -> TypeView
    invalidFixture err =
      error
        ( "invalid TypeView test fixture: "
            ++ show err
            ++ "; head aliases="
            ++ show completeHeadIdentities
            ++ "; binder aliases="
            ++ show completeBinderIdentities
        )

    (headPairs, binderPairs) = typeReferencePairs display identity
    completeHeadIdentities =
      Map.union headIdentities . uniqueAliasMap . concatMap headEntries $ headPairs
    completeBinderIdentities =
      Map.union binderIdentities . uniqueAliasMap . concatMap binderEntries $ binderPairs

    headEntries (displayName, identityName) =
      [(displayName, headIdentity), (identityName, headIdentity)]
      where
        headIdentity =
          fromMaybe
            (testHeadIdentity identityName)
            ( lookupSymbolIdentityAlias headIdentities displayName
                <|> lookupSymbolIdentityAlias headIdentities identityName
                <|> builtinTypeHeadIdentity displayName
                <|> builtinTypeHeadIdentity identityName
            )

    binderEntries (displayName, identityName) =
      [(displayName, binderIdentity), (identityName, binderIdentity)]
      where
        binderIdentity =
          maybe
            (testBinderIdentity identityName)
            id
            (lookupTypeBinderIdentityAlias binderIdentities displayName <|> lookupTypeBinderIdentityAlias binderIdentities identityName)

testHeadIdentity :: String -> SymbolIdentity
testHeadIdentity name =
  symbolIdentityFromParts unique SymbolType "<test>" name Nothing
  where
    unique =
      fromMaybe (testUnique 0 name) $ do
        suffix <- stripPrefix "$identity#" name
        UniqueIdentity <$> readMaybe suffix

testBinderIdentity :: String -> TypeBinderIdentity
testBinderIdentity name =
  fromMaybe (typeBinderIdentityFromUnique (testUnique 1 name)) (parseBinderIdentity name)

parseBinderIdentity :: String -> Maybe TypeBinderIdentity
parseBinderIdentity name =
  parseGraph <|> parseStructural <|> parseGenerated
  where
    parseGraph = do
      suffix <- stripPrefix "$typevar#node#" name
      typeBinderIdentityFromNode . NodeId <$> readMaybe suffix

    parseStructural = do
      suffix <- stripPrefix "$typevar#structural#" name
      let (uniqueText, roleText0) = break (== '#') suffix
      roleText <- stripPrefix "#" roleText0
      unique <- UniqueIdentity <$> readMaybe uniqueText
      role <-
        case roleText of
          "self" -> Just StructuralSelfBinder
          "result" -> Just StructuralResultBinder
          _ -> Nothing
      pure (typeBinderIdentityFromStructural unique role)

    parseGenerated = do
      suffix <- stripPrefix "$typevar#" name
      typeBinderIdentityFromUnique . UniqueIdentity <$> readMaybe suffix

testUnique :: Int -> String -> UniqueIdentity
testUnique salt =
  UniqueIdentity . abs . foldl' (\acc char -> acc * 16777619 + ord char) (2166136261 + salt)

typeReferencePairs :: SrcType -> SrcType -> ([(String, String)], [(String, String)])
typeReferencePairs = go
  where
    go display identity =
      case (display, identity) of
        (STVar displayName, STVar identityName) -> ([], [(displayName, identityName)])
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          go displayDom identityDom <> go displayCod identityCod
        (STBase displayName, STBase identityName) -> ([(displayName, identityName)], [])
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          ([(displayName, identityName)], []) <> foldMap (uncurry go) (zipNE displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs) ->
          ([], [(displayName, identityName)]) <> foldMap (uncurry go) (zipNE displayArgs identityArgs)
        (STTyLam displayName displayBody, STTyLam identityName identityBody) ->
          ([], [(displayName, identityName)]) <> go displayBody identityBody
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          go displayFun identityFun <> go displayArg identityArg
        (STForall displayName displayBound displayBody, STForall identityName identityBound identityBody) ->
          ([], [(displayName, identityName)])
            <> foldMap (uncurry go) (zipBounds displayBound identityBound)
            <> go displayBody identityBody
        (STMu displayName displayBody, STMu identityName identityBody) ->
          ([], [(displayName, identityName)]) <> go displayBody identityBody
        _ -> ([], [])

    zipNE :: NonEmpty SrcType -> NonEmpty SrcType -> [(SrcType, SrcType)]
    zipNE left right = NE.toList (NE.zip left right)

    zipBounds (Just (SrcBound left)) (Just (SrcBound right)) = [(left, right)]
    zipBounds _ _ = []

setTypeViewDisplay :: SrcType -> TypeView -> TypeView
setTypeViewDisplay display view =
  setTypeViewTypes display (typeViewIdentity view) view

setTypeViewIdentity :: SrcType -> TypeView -> TypeView
setTypeViewIdentity identity view =
  setTypeViewTypes (typeViewDisplay view) identity view

setTypeViewTypes :: SrcType -> SrcType -> TypeView -> TypeView
setTypeViewTypes display identity view =
  fixtureTypeView
    display
    identity
    (typeViewHeadIdentities view)
    (typeViewBinderIdentities view)

setTypeViewHeadIdentities :: Map String SymbolIdentity -> TypeView -> TypeView
setTypeViewHeadIdentities headIdentities view =
  rebuildTypeView headIdentities Map.empty view

setTypeViewBinderIdentities :: Map String TypeBinderIdentity -> TypeView -> TypeView
setTypeViewBinderIdentities binderIdentities view =
  rebuildTypeView Map.empty binderIdentities view

rebuildTypeView :: Map String SymbolIdentity -> Map String TypeBinderIdentity -> TypeView -> TypeView
rebuildTypeView headOverrides binderOverrides view =
  fixtureTypeView
    displayTy
    rebuiltIdentityTy
    effectiveHeads
    effectiveBinders
  where
    displayTy = typeViewDisplay view
    identityTy = typeViewIdentity view
    rebuiltIdentityTy = rebuildIdentityProjection displayTy identityTy

    effectiveHeads =
      Map.unions
        [ projectedHeadAliases,
          headOverrides,
          stableSymbolAliases headOverrides,
          retainedHeadIdentities
        ]

    effectiveBinders =
      Map.unions
        [ projectedBinderAliases,
          binderOverrides,
          stableBinderAliases binderOverrides,
          retainedBinderIdentities
        ]

    (headPairs, binderPairs) = typeReferencePairs displayTy rebuiltIdentityTy
    (originalHeadPairs, originalBinderPairs) = typeReferencePairs displayTy identityTy

    retainedHeadIdentities =
      Map.filter (not . replacedHeadIdentity) (typeViewHeadIdentities view)

    retainedBinderIdentities =
      Map.filter (`Set.notMember` replacedBinderIdentities) (typeViewBinderIdentities view)

    replacedHeadIdentity identity =
      any (sameSymbolIdentity identity) replacedHeadIdentities

    replacedHeadIdentities =
      [ oldIdentity
      | (displayName, identityName) <- originalHeadPairs,
        Just oldIdentity <- [currentHeadIdentity displayName identityName],
        Just newIdentity <- [selectHeadIdentity displayName identityName],
        not (sameSymbolIdentity oldIdentity newIdentity)
      ]

    replacedBinderIdentities =
      Set.fromList
        [ oldIdentity
        | (displayName, identityName) <- originalBinderPairs,
          Just oldIdentity <- [currentBinderIdentity displayName identityName],
          Just newIdentity <- [selectBinderIdentity displayName identityName],
          oldIdentity /= newIdentity
        ]

    currentHeadIdentity displayName identityName =
      Map.lookup identityName (typeViewHeadIdentities view)
        <|> Map.lookup displayName (typeViewHeadIdentities view)

    currentBinderIdentity displayName identityName =
      Map.lookup identityName (typeViewBinderIdentities view)
        <|> Map.lookup displayName (typeViewBinderIdentities view)

    projectedHeadAliases =
      uniqueAliasMap
        [ (alias, identity)
        | (displayName, identityName) <- headPairs,
          Just identity <- [selectHeadIdentity displayName identityName],
          alias <- [displayName, identityName]
        ]

    projectedBinderAliases =
      uniqueAliasMap
        [ (alias, identity)
        | (displayName, identityName) <- binderPairs,
          Just identity <- [selectBinderIdentity displayName identityName],
          alias <- [displayName, identityName]
        ]

    rebuildIdentityProjection display identity =
      case (display, identity) of
        (STVar displayName, STVar identityName) ->
          STVar (binderIdentityName displayName identityName)
        (STArrow displayDom displayCod, STArrow identityDom identityCod) ->
          STArrow
            (rebuildIdentityProjection displayDom identityDom)
            (rebuildIdentityProjection displayCod identityCod)
        (STBase displayName, STBase identityName) ->
          STBase (headIdentityName displayName identityName)
        (STCon displayName displayArgs, STCon identityName identityArgs) ->
          STCon
            (headIdentityName displayName identityName)
            (NE.zipWith rebuildIdentityProjection displayArgs identityArgs)
        (STVarApp displayName displayArgs, STVarApp identityName identityArgs) ->
          STVarApp
            (binderIdentityName displayName identityName)
            (NE.zipWith rebuildIdentityProjection displayArgs identityArgs)
        (STTyLam displayName displayBody, STTyLam identityName identityBody) ->
          STTyLam
            (binderIdentityName displayName identityName)
            (rebuildIdentityProjection displayBody identityBody)
        (STTyApp displayFun displayArg, STTyApp identityFun identityArg) ->
          STTyApp
            (rebuildIdentityProjection displayFun identityFun)
            (rebuildIdentityProjection displayArg identityArg)
        (STForall displayName displayBound displayBody, STForall identityName identityBound identityBody) ->
          STForall
            (binderIdentityName displayName identityName)
            (rebuildBound displayBound identityBound)
            (rebuildIdentityProjection displayBody identityBody)
        (STMu displayName displayBody, STMu identityName identityBody) ->
          STMu
            (binderIdentityName displayName identityName)
            (rebuildIdentityProjection displayBody identityBody)
        (STBottom, STBottom) -> STBottom
        _ -> identity

    rebuildBound (Just (SrcBound displayBound)) (Just (SrcBound identityBound)) =
      Just (SrcBound (rebuildIdentityProjection displayBound identityBound))
    rebuildBound _ identityBound = identityBound

    headIdentityName displayName identityName =
      maybe identityName symbolIdentityStableName (selectHeadIdentity displayName identityName)

    binderIdentityName displayName identityName =
      maybe identityName typeBinderIdentityStableName (selectBinderIdentity displayName identityName)

    selectHeadIdentity displayName identityName =
      lookupLoose displayName identityName headOverrides
        <|> matchingHeadIdentity identityName
        <|> singleNodeHeadOverride

    selectBinderIdentity displayName identityName =
      lookupLoose displayName identityName binderOverrides
        <|> matchingBinderIdentity identityName
        <|> singleNodeBinderOverride

    singleNodeHeadOverride
      | hasSingleIdentity (typeViewHeadIdentities view) = soleSymbolIdentity headOverrides
      | otherwise = Nothing

    singleNodeBinderOverride
      | hasSingleIdentity (typeViewBinderIdentities view) = soleBinderIdentity binderOverrides
      | otherwise = Nothing

    matchingHeadIdentity identityName = do
      current <- Map.lookup identityName (typeViewHeadIdentities view)
      soleMatching sameSymbolIdentity current (Map.elems headOverrides)

    matchingBinderIdentity identityName = do
      current <- Map.lookup identityName (typeViewBinderIdentities view)
      soleMatching (==) current (Map.elems binderOverrides)

lookupLoose :: Eq value => String -> String -> Map String value -> Maybe value
lookupLoose displayName identityName entries =
  Map.lookup identityName entries
    <|> Map.lookup displayName entries
    <|> soleValue
      [ value
      | (name, value) <- Map.toList entries,
        looseName name == looseName identityName || looseName name == looseName displayName
      ]

looseName :: String -> String
looseName = dropWhile (== '$')

soleMatching :: Eq value => (value -> value -> Bool) -> value -> [value] -> Maybe value
soleMatching same expected =
  soleValue . filter (same expected)

soleSymbolIdentity :: Map String SymbolIdentity -> Maybe SymbolIdentity
soleSymbolIdentity =
  soleValue . Map.elems . stableSymbolAliases

soleBinderIdentity :: Map String TypeBinderIdentity -> Maybe TypeBinderIdentity
soleBinderIdentity =
  soleValue . Map.elems . stableBinderAliases

soleValue :: Eq value => [value] -> Maybe value
soleValue values =
  case foldr addUnique [] values of
    [value] -> Just value
    _ -> Nothing
  where
    addUnique value seen
      | value `elem` seen = seen
      | otherwise = value : seen

hasSingleIdentity :: Eq value => Map String value -> Bool
hasSingleIdentity =
  maybe False (const True) . soleValue . Map.elems

uniqueAliasMap :: Eq value => [(String, value)] -> Map String value
uniqueAliasMap entries =
  Map.mapMaybe soleValue $
    Map.fromListWith (<>)
      [ (name, [value])
      | (name, value) <- entries
      ]

stableSymbolAliases :: Map String SymbolIdentity -> Map String SymbolIdentity
stableSymbolAliases entries =
  Map.fromList
    [ (symbolIdentityStableName identity, identity)
    | identity <- Map.elems entries
    ]

stableBinderAliases :: Map String TypeBinderIdentity -> Map String TypeBinderIdentity
stableBinderAliases entries =
  Map.fromList
    [ (typeBinderIdentityStableName identity, identity)
    | identity <- Map.elems entries
    ]
