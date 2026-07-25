module BackendStructuralRecursiveDataSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.Backend.IR
import MLF.Backend.StructuralRecursiveData
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Frontend.Program.Builtins (builtinTypeIdentity)
import MLF.Frontend.Symbol
  ( SymbolIdentity,
    SymbolNamespace (..),
    symbolIdentityFromParts,
    symbolIdentityStableName,
    symbolUniqueIdentity,
  )
import MLF.Types.Identity
  ( StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityFromStructural,
    typeBinderIdentityFromUnique,
  )
import MLF.Types.Unique (UniqueIdentity (..))
import Test.Hspec

spec :: Spec
spec = describe "MLF.Backend.StructuralRecursiveData" $ do
  it "uses binder identity, not spelling, for backend alpha equality" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991350)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991351)
    alphaEqBackendType
      (BTVarWithIdentity leftIdentity "a")
      (BTVarWithIdentity leftIdentity "$stale")
      `shouldBe` True
    alphaEqBackendType
      (BTVarWithIdentity leftIdentity "a")
      (BTVarWithIdentity rightIdentity "a")
      `shouldBe` False

  it "retains structural binder spelling only as a display projection" $
    structuralRecursiveDataName (symbolIdentityStableName listIdentity ++ "_self")
      `shouldBe` Just (symbolIdentityStableName listIdentity)

  it "rebuilds a structural self type only from its owner identity" $ do
    structuralMuAsDataType listIdentity (backendDataParameterRefs listData) listSelfIdentity
      `shouldBe` Just (listTy parameterTy)
    structuralMuAsDataType listIdentity (backendDataParameterRefs listData) wrongSelfIdentity
      `shouldBe` Nothing

  it "accepts an actual nominal type only when both owner identities agree" $ do
    structuralMuAsActualDataType listIdentity listSelfIdentity (listTy intTy)
      `shouldBe` Just (listTy intTy)
    structuralMuAsActualDataType otherListIdentity listSelfIdentity (listTy intTy)
      `shouldBe` Nothing

  it "indexes structural data declarations by SymbolIdentity" $ do
    let scope = backendDataScope (Map.singleton listIdentity listData)
    Map.lookup listIdentity (backendDataScopeByIdentity scope)
      `shouldBe` Just listData
    Map.lookup otherListIdentity (backendDataScopeByIdentity scope)
      `shouldBe` Nothing

  it "derives data argument substitutions from parameter identities" $ do
    structuralDataArgumentSubstitution listData [intTy]
      `shouldBe` Just (Map.singleton parameterKey intTy)
    structuralDataArgumentSubstitution listData []
      `shouldBe` Nothing

  it "matches the nominal declaration against its structural encoding" $ do
    let substitution = Map.singleton parameterKey intTy
    structuralDataDeclarationMatches Map.empty listData substitution (structuralListTy intTy)
      `shouldBe` True
    structuralDataDeclarationMatches Map.empty listData substitution (structuralListTy boolTy)
      `shouldBe` False

  it "carries selected data and constructor identities in structural evidence" $ do
    let substitution = Map.singleton parameterKey intTy
        staleNamedData = listData {backendDataNameWithIdentity = "$stale_List"}
    match <-
      case matchStructuralDataDeclaration Map.empty listData substitution (structuralListTy intTy) of
        Right value -> pure value
        Left mismatch -> expectationFailure (show mismatch) >> fail "structural data match failed"
    staleMatch <-
      case matchStructuralDataDeclaration Map.empty staleNamedData substitution (structuralListTy intTy) of
        Right value -> pure value
        Left mismatch -> expectationFailure (show mismatch) >> fail "stale-name structural data match failed"
    focused <-
      case matchFocusedStructuralConstructor Map.empty listData consConstructor substitution (structuralListTy intTy) of
        Right value -> pure value
        Left mismatch -> expectationFailure (show mismatch) >> fail "focused structural constructor match failed"
    srdmDataIdentity match `shouldBe` listIdentity
    match `shouldBe` staleMatch
    srcmDataIdentity focused `shouldBe` listIdentity
    srcmConstructorIdentity focused `shouldBe` consIdentity

  it "extracts constructor payload fields without counting recursive self" $
    structuralBackendHandlerFields (listStructuralBody intTy)
      `shouldBe` Just [[], [intTy, selfTy]]

  it "rejects same-spelled parameter occurrences with different identities" $ do
    let otherParameter = typeBinderIdentityFromUnique (UniqueIdentity 991358)
        parameterBounds = Map.singleton parameterKey Nothing
    matchBackendTypeParametersWithTypeBounds
      Map.empty
      (backendDataParameterRefs listData)
      parameterBounds
      Map.empty
      parameterTy
      (BTVarWithIdentity otherParameter "a")
      `shouldBe` Just (Map.singleton parameterKey (BTVarWithIdentity otherParameter "a"))
    alphaEqBackendType parameterTy (BTVarWithIdentity otherParameter "a")
      `shouldBe` False

  it "detects structural recursive owners that differ despite equal spelling" $
    structuralMuTypesHaveBinderIdentityMismatch
      (structuralListTy intTy)
      (BTMuWithIdentity wrongSelfIdentity "$List_self" (listStructuralBody intTy))
      `shouldBe` True

  it "matches nominal and structural representations from their carried owner identity" $ do
    let scope = backendDataScope (Map.singleton listIdentity listData)
    backendStructuralDataBoundaryMatches Map.empty (Just scope) (listTy intTy) (structuralListTy intTy)
      `shouldBe` True
    backendStructuralDataBoundaryMatches Map.empty Nothing (listTy intTy) (structuralListTy intTy)
      `shouldBe` True

  it "decomposes identity-bearing applied heads without rebuilding them from names" $
    decomposeBackendTypeHead (listTy intTy)
      `shouldBe` Just (BTBaseWithIdentity listIdentity (BaseTy "List"), [intTy])

listIdentity :: SymbolIdentity
listIdentity = testSymbol 990100 SymbolType "List"

otherListIdentity :: SymbolIdentity
otherListIdentity = testSymbol 990101 SymbolType "List"

nilIdentity :: SymbolIdentity
nilIdentity = testSymbol 990110 SymbolConstructor "Nil"

consIdentity :: SymbolIdentity
consIdentity = testSymbol 990111 SymbolConstructor "Cons"

listSelfIdentity :: TypeBinderIdentity
listSelfIdentity =
  typeBinderIdentityFromStructural (symbolUniqueIdentity listIdentity) StructuralSelfBinder

wrongSelfIdentity :: TypeBinderIdentity
wrongSelfIdentity =
  typeBinderIdentityFromStructural (symbolUniqueIdentity otherListIdentity) StructuralSelfBinder

parameterIdentity :: TypeBinderIdentity
parameterIdentity = typeBinderIdentityFromUnique (UniqueIdentity 990120)

resultIdentity :: TypeBinderIdentity
resultIdentity = typeBinderIdentityFromUnique (UniqueIdentity 990121)

parameterKey :: BackendTypeSubstitutionKey
parameterKey = backendTypeSubstitutionKeyFromIdentity parameterIdentity

parameterTy :: BackendType
parameterTy = BTVarWithIdentity parameterIdentity "a"

selfTy :: BackendType
selfTy = BTVarWithIdentity listSelfIdentity "$List_self"

intTy :: BackendType
intTy = BTBaseWithIdentity (builtinTypeIdentity "Int") (BaseTy "Int")

boolTy :: BackendType
boolTy = BTBaseWithIdentity (builtinTypeIdentity "Bool") (BaseTy "Bool")

listTy :: BackendType -> BackendType
listTy arg = BTConWithIdentity listIdentity (BaseTy "List") (arg :| [])

listData :: BackendData
listData =
  BackendDataWithIdentity
    { backendDataIdentity = listIdentity,
      backendDataNameWithIdentity = "List",
      backendDataParameterRefsWithIdentity =
        [backendDataParameterRefFromIdentity parameterIdentity "a"],
      backendDataConstructorsWithIdentity = [nilConstructor, consConstructor]
    }

nilConstructor :: BackendConstructor
nilConstructor =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = nilIdentity,
      backendConstructorNameWithIdentity = "Nil",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [],
      backendConstructorResultWithIdentity = listTy parameterTy
    }

consConstructor :: BackendConstructor
consConstructor =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = consIdentity,
      backendConstructorNameWithIdentity = "Cons",
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [parameterTy, listTy parameterTy],
      backendConstructorResultWithIdentity = listTy parameterTy
    }

structuralListTy :: BackendType -> BackendType
structuralListTy headField =
  BTMuWithIdentity listSelfIdentity "$List_self" (listStructuralBody headField)

listStructuralBody :: BackendType -> BackendType
listStructuralBody headField =
  BTForallWithIdentity
    resultIdentity
    "r"
    Nothing
    ( BTArrow
        (BTVarWithIdentity resultIdentity "r")
        ( BTArrow
            (BTArrow headField (BTArrow selfTy (BTVarWithIdentity resultIdentity "r")))
            (BTVarWithIdentity resultIdentity "r")
        )
    )

testSymbol :: Int -> SymbolNamespace -> String -> SymbolIdentity
testSymbol unique namespace name =
  symbolIdentityFromParts (UniqueIdentity unique) namespace "Test" name Nothing
