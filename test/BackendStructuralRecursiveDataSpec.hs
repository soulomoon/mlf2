{-# LANGUAGE LambdaCase #-}

module BackendStructuralRecursiveDataSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.IR
import MLF.Backend.StructuralRecursiveData
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), renameSymbolDefiningName, symbolIdentityFromParts, symbolIdentityStableName, symbolIdentityWithUnique)
import MLF.Types.Identity (StructuralTypeBinderRole (..), TypeBinderIdentity, typeBinderIdentityFromNode, typeBinderIdentityFromStructural, typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import MLF.Types.Unique (UniqueIdentity (..))
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.StructuralRecursiveData" $ do
  it "requires exact canonical data identity for metadata-light matches" $ do
    metadataLightStructuralDataMatches (BaseTy "Core.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` True

    metadataLightStructuralDataMatches (BaseTy "Core.T") [] "$Core.T_self1" nullaryStructuralBody
      `shouldBe` True

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$Core.T_self" nullaryStructuralBody
      `shouldBe` False

    metadataLightStructuralDataMatches (BaseTy "Other.T") [] "$T_self" nullaryStructuralBody
      `shouldBe` False

  it "preserves structural recursive data stable identity names" $
    structuralRecursiveDataName (symbolIdentityStableName listIdentity ++ "_self")
      `shouldBe` Just (symbolIdentityStableName listIdentity)

  it "does not match stable type-binder names as identities in structural alpha equality" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991350)
        stableName = typeBinderIdentityStableName identity

    alphaEqBackendType
      (BTVarWithIdentity Nothing stableName)
      (BTVarWithIdentity (Just identity) "$stale")
      `shouldBe` False

  it "matches metadata-light recursive payload parameters without counting self fields" $
    metadataLightStructuralDataMatches (BaseTy "List") [intTy] "$List_self" (listStructuralBody intTy)
      `shouldBe` True

  it "rejects metadata-light payload variables with the same spelling but different generated identities" $ do
    let expectedIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991357)
        actualIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991358)
        expected = BTVarWithIdentity (Just expectedIdentity) "a"
        actual = BTVarWithIdentity (Just actualIdentity) "a"

    metadataLightStructuralDataMatches (BaseTy "List") [expected] "$List_self" (listStructuralBody actual)
      `shouldBe` False

  it "treats freshened recursive self fields as self payloads" $
    metadataLightStructuralDataMatches (BaseTy "List") [intTy] "$List_self" (freshenedSelfListStructuralBody intTy)
      `shouldBe` True

  it "preserves data identity when rebuilding a structural mu head as nominal data" $
    structuralMuAsDataType (Just listIdentity) (backendDataParameterRefs listData) (Just listSelfIdentity) "$List_self"
      `shouldBe` Just (BTConWithIdentity (Just listIdentity) (BaseTy "List") (BTVar "a" :| []))

  it "rebuilds identity-bearing structural mu heads without parsing the binder spelling" $
    structuralMuAsDataType (Just listIdentity) (backendDataParameterRefs listData) (Just listSelfIdentity) "$not_a_data_name"
      `shouldBe` Just (BTConWithIdentity (Just listIdentity) (BaseTy "List") (BTVar "a" :| []))

  it "does not rebuild identity-bearing structural mu heads from names alone" $
    structuralMuAsDataType (Just listIdentity) (backendDataParameterRefs listData) Nothing "$List_self"
      `shouldBe` Nothing

  it "rejects same-name structural mu recovery when self identity belongs to another data type" $ do
    let wrongSelfIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 990102) StructuralSelfBinder
        actualTy =
          BTConWithIdentity (Just listIdentity) (BaseTy "$stale") (BTVar "a" :| [])

    structuralMuAsDataType (Just listIdentity) (backendDataParameterRefs listData) (Just wrongSelfIdentity) "$List_self"
      `shouldBe` Nothing

    structuralMuAsActualDataType (Just listIdentity) (Just wrongSelfIdentity) "$List_self" actualTy
      `shouldBe` Nothing

  it "does not match nominal and structural data by spelling when owners differ" $ do
    let wrongIdentity =
          symbolIdentityWithUnique (UniqueIdentity 990103) listIdentity
        wrongNominalTy =
          BTConWithIdentity (Just wrongIdentity) (BaseTy "List") (intTy :| [])
        structuralTy =
          identityStructuralListTy listSelfIdentity intTy

    matchBackendTypeParametersWithTypeBounds Map.empty [] Map.empty Map.empty wrongNominalTy structuralTy
      `shouldBe` Nothing

  it "does not match identity-bearing actual data through name-only structural mu recovery" $
    structuralMuAsActualDataType Nothing Nothing "$List_self" (BTConWithIdentity (Just listIdentity) (BaseTy "List") (BTVar "a" :| []))
      `shouldBe` Nothing

  it "matches identity-bearing actual data when structural mu recovery has the data identity" $
    structuralMuAsActualDataType (Just listIdentity) (Just listSelfIdentity) "$List_self" (BTConWithIdentity (Just listIdentity) (BaseTy "$stale") (BTVar "a" :| []))
      `shouldBe` Just (BTConWithIdentity (Just listIdentity) (BaseTy "$stale") (BTVar "a" :| []))

  it "matches structural boundaries by structural self identity instead of display name fallback" $ do
    let dataScope =
          backendDataScope
            (Map.singleton (backendDataName identityListData) identityListData)
            (Map.singleton listIdentity identityListData)
        staleNominalTy =
          BTConWithIdentity (Just listIdentity) (BaseTy "$stale_List") (intTy :| [])
        stalePayloadNominalTy =
          BTConWithIdentity (Just (renameSymbolDefiningName "$stale_List" listIdentity)) (BaseTy "$stale_List") (intTy :| [])
        wrongIdentity =
          symbolIdentityWithUnique (UniqueIdentity 990102) listIdentity
        wrongNominalTy =
          BTConWithIdentity (Just wrongIdentity) (BaseTy "List") (intTy :| [])
        identityStructuralTy =
          identityStructuralListTy listSelfIdentity intTy
        identitylessStructuralTy =
          structuralListTy intTy

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) staleNominalTy identityStructuralTy
      `shouldBe` True
    alphaEqBackendType staleNominalTy identityStructuralTy
      `shouldBe` True

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) staleNominalTy identitylessStructuralTy
      `shouldBe` False

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) stalePayloadNominalTy identityStructuralTy
      `shouldBe` False

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) wrongNominalTy identityStructuralTy
      `shouldBe` False
    alphaEqBackendType wrongNominalTy identityStructuralTy
      `shouldBe` False

  it "does not match identity-bearing scoped data through name-only structural boundaries" $ do
    let dataScope =
          backendDataScope
            (Map.singleton (backendDataName identityListData) identityListData)
            (Map.singleton listIdentity identityListData)
        nameOnlyNominalTy =
          BTCon (BaseTy "List") (intTy :| [])
        structuralTy =
          structuralListTy intTy

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) nameOnlyNominalTy structuralTy
      `shouldBe` False

  it "does not match identity-bearing scoped data through stable identity names without metadata" $ do
    let dataScope =
          backendDataScope
            (Map.singleton (backendDataName identityListData) identityListData)
            (Map.singleton listIdentity identityListData)
        stableNominalTy =
          BTCon (BaseTy (symbolIdentityStableName listIdentity)) (intTy :| [])
        structuralTy =
          structuralListTy intTy

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) stableNominalTy structuralTy
      `shouldBe` False

  it "does not match identity-bearing scoped data through graph structural mu identities" $ do
    let dataScope =
          backendDataScope
            (Map.singleton (backendDataName identityListData) identityListData)
            (Map.singleton listIdentity identityListData)
        staleNominalTy =
          BTConWithIdentity (Just listIdentity) (BaseTy "$stale_List") (intTy :| [])
        graphSelfIdentity =
          typeBinderIdentityFromNode (NodeId 991351)
        structuralTy =
          BTMuWithIdentity (Just graphSelfIdentity) "$List_self" (listStructuralBody intTy)

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) staleNominalTy structuralTy
      `shouldBe` False

  it "recovers structural mu owners by self identity before binder spelling" $ do
    let selfIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 990100) StructuralSelfBinder
        dataScope =
          backendDataScope
            (Map.singleton (backendDataName identityListData) identityListData)
            (Map.singleton listIdentity identityListData)
        staleNominalTy =
          BTCon (BaseTy "$stale_List") (intTy :| [])
        staleStructuralBody =
          BTForall
            "r"
            Nothing
            ( BTArrow
                (BTVar "r")
                (BTArrow (BTArrow intTy (BTArrow (listTy intTy) (BTVar "r"))) (BTVar "r"))
            )
        structuralTy =
          BTMuWithIdentity (Just selfIdentity) "$stale_self" staleStructuralBody

    backendStructuralDataBoundaryMatches Map.empty (Just dataScope) staleNominalTy structuralTy
      `shouldBe` True

  it "keeps identity-distinct payloads whose names look like recursive self fields" $ do
    let selfIdentity = typeBinderIdentityFromNode (NodeId 991333)
        payloadIdentity = typeBinderIdentityFromNode (NodeId 991334)
        payloadTy = BTVarWithIdentity (Just payloadIdentity) "$List_self"
        structuralTy =
          BTMuWithIdentity
            (Just selfIdentity)
            "$List_self"
            (identityListStructuralBody selfIdentity payloadTy)

    alphaEqBackendType (BTCon (BaseTy "List") (payloadTy :| [])) structuralTy
      `shouldBe` True

  it "matches recursive payloads with a self-cycle guard and returns focused field evidence" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]

    case matchStructuralDataDeclaration Map.empty listData substitution structuralTy of
      Right match -> do
        srdmDataName match `shouldBe` "List"
        srdmParameterSubstitution match `shouldBe` substitution
      Left mismatch ->
        expectationFailure ("expected recursive structural match, got " ++ show mismatch)

    case matchFocusedStructuralConstructor Map.empty listData consConstructor substitution structuralTy of
      Right match -> do
        srcmConstructorName match `shouldBe` "Cons"
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected focused constructor match, got " ++ show mismatch)

  it "matches focused constructors by identity when the constructor name is stale" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]

    case matchFocusedStructuralConstructor Map.empty identityListData staleConsConstructor substitution structuralTy of
      Right match -> do
        srcmConstructorIdentity match `shouldBe` Just consIdentity
        srcmConstructorName match `shouldBe` "Cons"
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected identity-focused constructor match, got " ++ show mismatch)

  it "does not focus duplicate constructor identities by data declaration order" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]
        nilWithConsIdentity =
          BackendConstructorWithIdentity
            { backendConstructorIdentity = Just consIdentity,
              backendConstructorNameWithIdentity = "Nil",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [],
              backendConstructorResultWithIdentity = listTy (BTVar "a")
            }
        duplicateListData =
          identityListData
            { backendDataConstructorsWithIdentity = [nilWithConsIdentity, consConstructorWithIdentity]
            }
    matchFocusedStructuralConstructor Map.empty duplicateListData consConstructorWithIdentity substitution structuralTy
      `shouldBe` Left (StructuralRecursiveDataAmbiguousConstructor "List" "Cons")

  it "does not focus identity-bearing constructors through name fallback" $ do
    let structuralTy = structuralListTy intTy
        substitution = subst [("a", intTy)]
        wrongConsIdentity =
          symbolIdentityWithUnique (UniqueIdentity 991901) consIdentity
        wrongCons =
          BackendConstructorWithIdentity
            { backendConstructorIdentity = Just wrongConsIdentity,
              backendConstructorNameWithIdentity = "Cons",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity = [BTVar "a", listTy (BTVar "a")],
              backendConstructorResultWithIdentity = listTy (BTVar "a")
            }
        wrongListData =
          identityListData
            { backendDataConstructorsWithIdentity = [nilConstructor, wrongCons]
            }
    matchFocusedStructuralConstructor Map.empty wrongListData consConstructorWithIdentity substitution structuralTy
      `shouldBe` Left (StructuralRecursiveDataUnknownConstructor "List" "Cons")

  it "compares focused constructor matches by identity when names are stale" $ do
    let match dataName name identity =
          StructuralConstructorMatch
            { srcmDataName = dataName,
              srcmConstructorIdentity = identity,
              srcmConstructorName = name,
              srcmFieldTypes = [intTy]
            }
        otherIdentity =
          symbolIdentityFromParts (UniqueIdentity 991900) SymbolConstructor "Main" "Nil" Nothing
    match "List" "Cons" (Just consIdentity) `shouldBe` match "$stale_List" "$stale_cons" (Just consIdentity)
    match "List" "Cons" (Just consIdentity) `shouldNotBe` match "List" "Cons" (Just otherIdentity)
    match "List" "Cons" Nothing `shouldNotBe` match "$stale_List" "Cons" Nothing

  it "substitutes recursive self payloads by identity when self names are stale" $ do
    let selfIdentity = typeBinderIdentityFromNode (NodeId 991330)
        structuralTy = identityStructuralListTy selfIdentity intTy
        substitution = subst [("a", intTy)]

    case matchFocusedStructuralConstructor Map.empty listData consConstructor substitution structuralTy of
      Right match ->
        srcmFieldTypes match `shouldBe` [intTy, structuralTy]
      Left mismatch ->
        expectationFailure ("expected identity recursive self substitution, got " ++ show mismatch)

  it "rejects substitution mismatches in recursive payload fields" $ do
    let structuralTy = structuralListTy boolTy
        substitution = subst [("a", intTy)]

    matchStructuralDataDeclaration Map.empty listData substitution structuralTy
      `shouldSatisfy` isLeft

  it "rejects missing or extra structural constructors under metadata-backed matching" $ do
    matchStructuralDataDeclaration Map.empty listData (subst [("a", intTy)]) missingConsStructuralListTy
      `shouldSatisfy` isLeft

    matchStructuralDataDeclaration Map.empty listData (subst [("a", intTy)]) extraConstructorStructuralListTy
      `shouldSatisfy` isLeft

  it "matches constructor parameters by identity when binder names collide" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991301)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991302)
        leftKey = backendTypeSubstitutionKeyFromIdentity leftIdentity
        rightKey = backendTypeSubstitutionKeyFromIdentity rightIdentity
        leftVar = BTVarWithIdentity (Just leftIdentity) "a"
        rightVar = BTVarWithIdentity (Just rightIdentity) "a"
        parameterBounds = Map.fromList [(leftKey, Nothing), (rightKey, Nothing)]
        expected = BTArrow leftVar rightVar
        actual = BTArrow intTy boolTy

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected actual
      `shouldBe` Just (Map.fromList [(leftKey, intTy), (rightKey, boolTy)])

  it "matches constructor result foralls by binder identity when names are stale" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991324)
        expectedIdentity = typeBinderIdentityFromNode (NodeId 991325)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991326)
        parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity
        parameters = Set.singleton parameterKey
        expected =
          BTForallWithIdentity
            (Just expectedIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just expectedIdentity) "a") (BTVarWithIdentity (Just parameterIdentity) "p"))
        actual =
          BTForallWithIdentity
            (Just actualIdentity)
            "b"
            Nothing
            (BTArrow (BTVarWithIdentity (Just actualIdentity) "stale") intTy)

    matchConstructorResult [] parameters Map.empty expected actual
      `shouldBe` Just (Map.singleton parameterKey intTy)

  it "matches constructor result recursive binders by identity when names are stale" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991327)
        expectedIdentity = typeBinderIdentityFromNode (NodeId 991328)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991329)
        parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity
        parameters = Set.singleton parameterKey
        expected =
          BTMuWithIdentity
            (Just expectedIdentity)
            "self"
            (BTArrow (BTVarWithIdentity (Just expectedIdentity) "self") (BTVarWithIdentity (Just parameterIdentity) "p"))
        actual =
          BTMuWithIdentity
            (Just actualIdentity)
            "other"
            (BTArrow (BTVarWithIdentity (Just actualIdentity) "stale") intTy)

    matchConstructorResult [] parameters Map.empty expected actual
      `shouldBe` Just (Map.singleton parameterKey intTy)

  it "does not match same-named structural recursive binders with different identities" $ do
    let expectedIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 991352) StructuralSelfBinder
        actualIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 991353) StructuralSelfBinder
        expected =
          BTMuWithIdentity
            (Just expectedIdentity)
            "$List_self"
            (BTVarWithIdentity (Just expectedIdentity) "$List_self")
        actual =
          BTMuWithIdentity
            (Just actualIdentity)
            "$List_self"
            (BTVarWithIdentity (Just actualIdentity) "$List_self")

    matchBackendTypeParametersWithTypeBounds Map.empty [] Map.empty Map.empty expected actual
      `shouldBe` Nothing
    structuralMuTypesHaveBinderIdentityMismatch expected actual `shouldBe` True

  it "does not reuse structural parameter substitutions with different identities" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991354)
        parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity
        previousIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 991355) StructuralSelfBinder
        actualIdentity =
          typeBinderIdentityFromStructural (UniqueIdentity 991356) StructuralSelfBinder
        parameterBounds = Map.singleton parameterKey Nothing
        substitution =
          Map.singleton
            parameterKey
            (identityStructuralListTy previousIdentity intTy)
        expected = BTVarWithIdentity (Just parameterIdentity) "a"
        actual = identityStructuralListTy actualIdentity intTy

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds substitution expected actual
      `shouldBe` Nothing

  it "rejects constructor result variables with the same spelling but different identities" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991331)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991332)
        expected = BTVarWithIdentity (Just expectedIdentity) "a"
        actual = BTVarWithIdentity (Just actualIdentity) "a"

    matchConstructorResult [] Set.empty Map.empty expected actual
      `shouldBe` Nothing

  it "rejects payload type applications with the same spelling but different identities" $ do
    let expectedSelfIdentity = typeBinderIdentityFromNode (NodeId 991335)
        actualSelfIdentity = typeBinderIdentityFromNode (NodeId 991336)
        expectedHeadIdentity = typeBinderIdentityFromNode (NodeId 991337)
        actualHeadIdentity = typeBinderIdentityFromNode (NodeId 991338)
        expectedBody = listStructuralBody (BTVarAppWithIdentity (Just expectedHeadIdentity) "f" (intTy :| []))
        actualBody = listStructuralBody (BTVarAppWithIdentity (Just actualHeadIdentity) "f" (intTy :| []))

    structuralPayloadsMayInstantiate
      Map.empty
      (Just expectedSelfIdentity)
      "$List_self"
      expectedBody
      (Just actualSelfIdentity)
      "$List_self"
      actualBody
      `shouldBe` False

  it "compares structural payload owners by identity when binder spellings differ" $
    structuralPayloadsMayInstantiate
      Map.empty
      (Just listSelfIdentity)
      "$stale_left"
      (listStructuralBody intTy)
      (Just listSelfIdentity)
      "$stale_right"
      (listStructuralBody intTy)
      `shouldBe` True

  it "reuses repeated structural substitutions by owner identity instead of mu spelling" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991359)
        parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity
        parameterBounds = Map.singleton parameterKey Nothing
        previous = BTMuWithIdentity (Just listSelfIdentity) "$stale_left" (listStructuralBody intTy)
        actual = BTMuWithIdentity (Just listSelfIdentity) "$stale_right" (listStructuralBody intTy)

    matchBackendTypeParametersWithTypeBounds
      Map.empty
      []
      parameterBounds
      (Map.singleton parameterKey previous)
      (BTVarWithIdentity (Just parameterIdentity) "a")
      actual
      `shouldBe` Just (Map.singleton parameterKey previous)

  it "checks actual type variable bounds by identity when names are stale" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991303)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991304)
        expectedKey = backendTypeSubstitutionKeyFromIdentity expectedIdentity
        actualKey = backendTypeSubstitutionKeyFromIdentity actualIdentity
        parameterBounds = Map.singleton expectedKey (Just intTy)
        typeBounds = Map.fromList [(actualKey, Just intTy), (backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a", Just boolTy)]
        expected = BTVarWithIdentity (Just expectedIdentity) "a"
        actual = BTVarWithIdentity (Just actualIdentity) "a"

    matchBackendTypeParametersWithTypeBounds typeBounds [] parameterBounds Map.empty expected actual
      `shouldBe` Just (Map.singleton expectedKey actual)

  it "does not match identity-bearing parameters through name-only bounds" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991305)
        parameterBounds = Map.singleton (backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a") Nothing
        expected = BTVarWithIdentity (Just identity) "a"

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected intTy
      `shouldBe` Nothing

  it "does not replace identity-bearing parameter placeholders through name-only data parameters" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991306)
        placeholderIdentity = typeBinderIdentityFromNode (NodeId 991307)
        parameterRef = backendDataParameterRefFromIdentity parameterIdentity "a"
        parameterKey = backendDataParameterRefKey parameterRef
        parameterBounds = Map.singleton parameterKey Nothing
        previousSubstitution =
          Map.singleton parameterKey (BTVarWithIdentity (Just placeholderIdentity) "a")
        expected = BTVarWithIdentity (Just parameterIdentity) "a"

    matchBackendTypeParametersWithTypeBounds
      Map.empty
      [parameterRef]
      parameterBounds
      previousSubstitution
      expected
      intTy
      `shouldBe` Nothing

  it "does not instantiate structural payload variables through name-only bounds" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991335)
        typeBounds = Map.singleton (backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a") Nothing

    structuralPayloadsMayInstantiate
      typeBounds
      Nothing
      "$List_self"
      (BTVarWithIdentity (Just identity) "a")
      Nothing
      "$List_self"
      (BTVar "a")
      `shouldBe` False

    structuralPayloadsMayInstantiate
      typeBounds
      Nothing
      "$List_self"
      (BTVar "a")
      Nothing
      "$List_self"
      (BTVarWithIdentity (Just identity) "a")
      `shouldBe` False

  it "does not instantiate structural payload variables through mixed identity/name bounds" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991336)
        typeBounds = Map.singleton (backendTypeSubstitutionKeyFromIdentity identity) Nothing

    structuralPayloadsMayInstantiate
      typeBounds
      Nothing
      "$List_self"
      (BTVarWithIdentity (Just identity) "a")
      Nothing
      "$List_self"
      (BTVar "a")
      `shouldBe` False

    structuralPayloadsMayInstantiate
      typeBounds
      Nothing
      "$List_self"
      (BTVar "a")
      Nothing
      "$List_self"
      (BTVarWithIdentity (Just identity) "a")
      `shouldBe` False

  it "does not match identity-bearing data parameters through name-only order" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991308)
        expected = BTVarWithIdentity (Just identity) "a"

    matchBackendTypeParametersWithTypeBounds Map.empty [legacyDataParameterRef "a"] Map.empty Map.empty expected intTy
      `shouldBe` Nothing

  it "does not pick an arbitrary data parameter identity for duplicate name fallbacks" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991339)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991340)
        leftKey = backendTypeSubstitutionKeyFromIdentity leftIdentity
        rightKey = backendTypeSubstitutionKeyFromIdentity rightIdentity
        dataParameterOrder = [backendDataParameterRefFromIdentity leftIdentity "a", backendDataParameterRefFromIdentity rightIdentity "a"]
        parameterBounds = Map.fromList [(leftKey, Nothing), (rightKey, Nothing)]

    matchBackendTypeParametersWithTypeBounds Map.empty dataParameterOrder parameterBounds Map.empty (BTVar "a") intTy
      `shouldBe` Nothing

  it "does not map name-only variables to unique identity-bearing data parameters" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991346)
        parameterRef = backendDataParameterRefFromIdentity identity "a"
        parameterBounds = Map.singleton (backendDataParameterRefKey parameterRef) Nothing

    matchBackendTypeParametersWithTypeBounds Map.empty [parameterRef] parameterBounds Map.empty (BTVar "a") intTy
      `shouldBe` Nothing

  it "keeps unique name-only data parameter substitutions name-keyed" $ do
    let identity = typeBinderIdentityFromNode (NodeId 991341)
        nameKey = backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a"
        substitution = Map.singleton nameKey intTy
        dataDecl = BackendDataWithIdentity Nothing "Box" [backendDataParameterRefFromIdentity identity "a"] []

    completeDataParameterSubstitution dataDecl substitution
      `shouldBe` substitution

  it "does not promote duplicate data parameter names to arbitrary identities" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991342)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991343)
        nameKey = backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a"
        substitution = Map.singleton nameKey intTy
        dataDecl =
          BackendDataWithIdentity
            Nothing
            "Pair"
            [backendDataParameterRefFromIdentity leftIdentity "a", backendDataParameterRefFromIdentity rightIdentity "a"]
            []

    completeDataParameterSubstitution dataDecl substitution
      `shouldBe` substitution

  it "does not promote across distinct same-named result placeholders" $ do
    let dataIdentity = typeBinderIdentityFromNode (NodeId 991344)
        resultIdentity = typeBinderIdentityFromNode (NodeId 991345)
        nameKey = backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing "a"
        substitution = Map.singleton nameKey intTy
        dataDecl =
          BackendDataWithIdentity
            Nothing
            "Box"
            [backendDataParameterRefFromIdentity dataIdentity "a"]
            [BackendConstructor "Box" [] [] (BTCon (BaseTy "Box") (BTVarWithIdentity (Just resultIdentity) "a" :| []))]

    completeDataParameterSubstitution dataDecl substitution
      `shouldBe` substitution

  it "does not alpha-rename free name-only variables through identity binders" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991306)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991307)
        expected =
          BTForallWithIdentity
            (Just expectedIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just expectedIdentity) "a") (BTVar "a"))
        actual =
          BTForallWithIdentity
            (Just actualIdentity)
            "b"
            Nothing
            (BTArrow (BTVarWithIdentity (Just actualIdentity) "b") (BTVar "b"))

    matchBackendTypeParametersWithTypeBounds Map.empty [] Map.empty Map.empty expected actual
      `shouldBe` Nothing

  it "alpha-renames structural forall binders by identity" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991309)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991310)
        freeIdentity = typeBinderIdentityFromNode (NodeId 991311)
        expected =
          BTForallWithIdentity
            (Just expectedIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just expectedIdentity) "stale") (BTVarWithIdentity (Just freeIdentity) "a"))
        actual =
          BTForallWithIdentity
            (Just actualIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just actualIdentity) "renamed") (BTVarWithIdentity (Just freeIdentity) "a"))

    backendStructuralDataBoundaryMatches Map.empty Nothing expected actual
      `shouldBe` True

  it "matches recursive body forall variables by identity" $ do
    let recursiveTyName = "$Box_self"
        recursiveIdentity = typeBinderIdentityFromNode (NodeId 991317)
        leftIdentity = typeBinderIdentityFromNode (NodeId 991312)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991313)
        freeIdentity = typeBinderIdentityFromNode (NodeId 991314)
        aliasIdentity = typeBinderIdentityFromNode (NodeId 991318)
        recursiveVar = BTVarWithIdentity (Just recursiveIdentity) "staleSelf"
        aliasVar = BTVarWithIdentity (Just aliasIdentity) "staleSelf"
        recursiveBody =
          BTForallWithIdentity
            (Just leftIdentity)
            "a"
            Nothing
            (BTArrow recursiveVar (BTVarWithIdentity (Just freeIdentity) "a"))
        plainBody =
          BTForallWithIdentity
            (Just rightIdentity)
            "a"
            Nothing
            (BTArrow aliasVar (BTVarWithIdentity (Just freeIdentity) "a"))

    recursiveBodyCompatibleWithIdentity (Just recursiveIdentity) recursiveTyName recursiveBody plainBody
      `shouldBe` True

    recursiveBodyCompatibleWithIdentity (Just leftIdentity) recursiveTyName recursiveBody plainBody
      `shouldBe` False

  it "detects recursive body aliases by identity when self names are stale" $ do
    let recursiveTyName = "$Box_self"
        recursiveIdentity = typeBinderIdentityFromNode (NodeId 991319)
        aliasIdentity = typeBinderIdentityFromNode (NodeId 991320)
        recursiveBody = BTVarWithIdentity (Just recursiveIdentity) "staleSelf"
        plainBody =
          BTForallWithIdentity
            (Just aliasIdentity)
            "alias"
            Nothing
            (BTVarWithIdentity (Just aliasIdentity) "alias")

    recursiveBodyCompatibleWithIdentity (Just recursiveIdentity) recursiveTyName recursiveBody plainBody
      `shouldBe` True

  it "keeps identity-keyed parameters when unwrapping vacuous recursive bodies" $ do
    let parameterIdentity = typeBinderIdentityFromNode (NodeId 991321)
        expectedSelfIdentity = typeBinderIdentityFromNode (NodeId 991322)
        actualSelfIdentity = typeBinderIdentityFromNode (NodeId 991323)
        parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity
        parameterBounds = Map.singleton parameterKey Nothing
        expected =
          BTMuWithIdentity
            (Just expectedSelfIdentity)
            "expectedSelf"
            (BTVarWithIdentity (Just parameterIdentity) "staleParam")
        actual =
          BTMuWithIdentity
            (Just actualSelfIdentity)
            "actualSelf"
            (BTVarWithIdentity (Just actualSelfIdentity) "actualSelf")

    matchBackendTypeParametersWithTypeBounds Map.empty [] parameterBounds Map.empty expected actual
      `shouldBe` Just (Map.singleton parameterKey actual)

  it "checks vacuous recursive binders by identity" $ do
    let selfIdentity = typeBinderIdentityFromNode (NodeId 991315)
        otherIdentity = typeBinderIdentityFromNode (NodeId 991316)

    isVacuousRecursiveBinderWithIdentity
      (Just selfIdentity)
      "self"
      (BTVarWithIdentity (Just otherIdentity) "self")
      `shouldBe` True

    isVacuousRecursiveBinderWithIdentity
      (Just selfIdentity)
      "self"
      (BTVarWithIdentity (Just selfIdentity) "stale")
      `shouldBe` False

  it "keeps metadata-light skeleton evidence from standing in for full ADT evidence" $ do
    metadataLightStructuralDataMatches (BaseTy "UnitLike") [] "$UnitLike_self" nullaryStructuralBody
      `shouldBe` True

    matchStructuralDataDeclaration Map.empty unitLikeData Map.empty (BTMu "$UnitLike_self" nullaryStructuralBody)
      `shouldSatisfy` isLeft

isLeft :: Either left right -> Bool
isLeft =
  \case
    Left _ -> True
    Right _ -> False

subst :: [(String, BackendType)] -> Map.Map BackendTypeSubstitutionKey BackendType
subst =
  Map.fromList . map (\(name, ty) -> (backendTypeSubstitutionKeyFromMaybeMetadataLight Nothing name, ty))

legacyDataParameterRef :: String -> BackendDataParameterRef
legacyDataParameterRef name =
  case backendDataParameterRefs (BackendData "__Legacy" [name] []) of
    [ref] -> ref
    _ -> error "expected one legacy data parameter ref"

intTy :: BackendType
intTy = BTBase (BaseTy "Int")

boolTy :: BackendType
boolTy = BTBase (BaseTy "Bool")

listTy :: BackendType -> BackendType
listTy arg =
  BTCon (BaseTy "List") (arg :| [])

listData :: BackendData
listData =
  BackendData
    { backendDataName = "List",
      backendDataParameters = ["a"],
      backendDataConstructors = [nilConstructor, consConstructor]
    }

nilConstructor :: BackendConstructor
nilConstructor =
  BackendConstructor
    { backendConstructorName = "Nil",
      backendConstructorForalls = [],
      backendConstructorFields = [],
      backendConstructorResult = listTy (BTVar "a")
    }

consConstructor :: BackendConstructor
consConstructor =
  BackendConstructor
    { backendConstructorName = "Cons",
      backendConstructorForalls = [],
      backendConstructorFields = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResult = listTy (BTVar "a")
    }

identityListData :: BackendData
identityListData =
  BackendDataWithIdentity
    { backendDataIdentity = Just listIdentity,
      backendDataNameWithIdentity = "List",
      backendDataParameterRefsWithIdentity = [legacyDataParameterRef "a"],
      backendDataConstructorsWithIdentity = [nilConstructor, consConstructorWithIdentity]
    }

consConstructorWithIdentity :: BackendConstructor
consConstructorWithIdentity =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = Just consIdentity,
      backendConstructorNameWithIdentity = "Cons",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResultWithIdentity = listTy (BTVar "a")
    }

staleConsConstructor :: BackendConstructor
staleConsConstructor =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = Just consIdentity,
      backendConstructorNameWithIdentity = "$stale_Cons",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [BTVar "a", listTy (BTVar "a")],
      backendConstructorResultWithIdentity = listTy (BTVar "a")
    }

consIdentity :: SymbolIdentity
consIdentity =
  symbolIdentityFromParts (UniqueIdentity 990101) SymbolConstructor "Main" "Cons" Nothing

listIdentity :: SymbolIdentity
listIdentity =
  symbolIdentityFromParts (UniqueIdentity 990100) SymbolType "Main" "List" Nothing

listSelfIdentity :: TypeBinderIdentity
listSelfIdentity =
  typeBinderIdentityFromStructural (UniqueIdentity 990100) StructuralSelfBinder

unitLikeData :: BackendData
unitLikeData =
  BackendData
    { backendDataName = "UnitLike",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            { backendConstructorName = "UnitLike",
              backendConstructorForalls = [],
              backendConstructorFields = [],
              backendConstructorResult = BTBase (BaseTy "UnitLike")
            }
        ]
    }

structuralListTy :: BackendType -> BackendType
structuralListTy headField =
  BTMu "$List_self" (listStructuralBody headField)

identityStructuralListTy :: TypeBinderIdentity -> BackendType -> BackendType
identityStructuralListTy selfIdentity headField =
  BTMuWithIdentity (Just selfIdentity) "$List_self" (identityListStructuralBody selfIdentity headField)

missingConsStructuralListTy :: BackendType
missingConsStructuralListTy =
  BTMu "$List_self" (BTForall "r" Nothing (BTArrow (BTVar "r") (BTVar "r")))

extraConstructorStructuralListTy :: BackendType
extraConstructorStructuralListTy =
  BTMu
    "$List_self"
    ( BTForall
        "r"
        Nothing
        ( BTArrow
            (BTVar "r")
            ( BTArrow
                (BTArrow intTy (BTArrow (BTVar "$List_self") (BTVar "r")))
                (BTArrow (BTVar "r") (BTVar "r"))
            )
        )
    )

listStructuralBody :: BackendType -> BackendType
listStructuralBody headField =
  BTForall
    "r"
    Nothing
    ( BTArrow
        (BTVar "r")
        (BTArrow (BTArrow headField (BTArrow (BTVar "$List_self") (BTVar "r"))) (BTVar "r"))
    )

identityListStructuralBody :: TypeBinderIdentity -> BackendType -> BackendType
identityListStructuralBody selfIdentity headField =
  BTForall
    "r"
    Nothing
    ( BTArrow
        (BTVar "r")
        (BTArrow (BTArrow headField (BTArrow (BTVarWithIdentity (Just selfIdentity) "stale_self") (BTVar "r"))) (BTVar "r"))
    )

freshenedSelfListStructuralBody :: BackendType -> BackendType
freshenedSelfListStructuralBody headField =
  BTForall
    "r"
    Nothing
    ( BTArrow
        (BTVar "r")
        (BTArrow (BTArrow headField (BTArrow (BTVar "$List_self1") (BTVar "r"))) (BTVar "r"))
    )

nullaryStructuralBody :: BackendType
nullaryStructuralBody =
  BTForall "r" Nothing (BTVar "r")
