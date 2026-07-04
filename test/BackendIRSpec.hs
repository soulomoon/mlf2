module BackendIRSpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Backend.CallableShape (backendCallableRef, backendCallableRefName)
import MLF.Backend.IR
import MLF.Backend.StructuralRecursiveData (structuralDataDeclarationMatches)
import MLF.Constraint.Types.Graph (BaseTy (..), NodeId (..))
import MLF.Frontend.Program.Builtins (builtinTypeIdentity, builtinValueIdentity)
import MLF.Frontend.Symbol (SymbolIdentity, SymbolNamespace (..), renameSymbolDefiningName, symbolIdentityFromParts, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Syntax (Lit (..))
import qualified MLF.Primitive.Inventory as PrimitiveInventory
import MLF.Types.Identity (deferredRefFromIdentity, IdDetails (DeferredId, LocalId, PrimitiveId, TopLevelId), LocalIdentity (GeneratedLocalId), localRefFromIdentity, StructuralTypeBinderRole (..), TypeBinderIdentity, initialIdentityGenerator, primitiveRefFromSymbol, typeBinderIdentityFromNode, typeBinderIdentityFromStructural, typeBinderIdentityFromUnique, typeBinderIdentityStableName, uniqueIdentityStableName)
import MLF.Types.Unique (UniqueIdentity (..))
import Test.Hspec

testSymbolIdentity :: Int -> SymbolNamespace -> String -> String -> SymbolIdentity
testSymbolIdentity unique namespace moduleName name =
  symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName name Nothing

spec :: Spec
spec = describe "MLF.Backend.IR" $ do
  it "accepts a minimal checked-like backend program" $ do
    validateBackendProgram simpleProgram `shouldBe` Right ()

  it "rejects duplicate modules, data, and global bindings" $ do
    validateBackendProgram (BackendProgram [emptyModule "Main", emptyModule "Main"] "main")
      `shouldBe` Left (BackendDuplicateModule "Main")

    validateBackendProgram duplicateModuleIdentityProgram
      `shouldBe` Left (BackendDuplicateModule (symbolIdentityStableName duplicateModuleIdentity))

    validateBackendProgram duplicateDataIdentityProgram
      `shouldBe` Left (BackendDuplicateData (symbolIdentityStableName duplicateDataIdentity))

    validateBackendProgram (programWithBindings [mainLiteralBinding, mainLiteralBinding])
      `shouldBe` Left (BackendDuplicateBinding "main")

    validateBackendProgram duplicateBindingIdentityProgram
      `shouldBe` Left (BackendDuplicateBinding (symbolIdentityStableName duplicateValueIdentity))

  it "rejects backend symbol identity payload conflicts" $ do
    validateBackendProgram conflictingModuleIdentityPayloadProgram
      `shouldBe` Left (BackendConflictingIdentityPayload "module" (symbolIdentityStableName conflictingModuleIdentity))

    validateBackendProgram conflictingDataIdentityPayloadProgram
      `shouldBe` Left (BackendConflictingIdentityPayload "data" (symbolIdentityStableName conflictingDataIdentity))

    validateBackendProgram conflictingBindingIdentityPayloadProgram
      `shouldBe` Left (BackendConflictingIdentityPayload "binding" (symbolIdentityStableName conflictingValueIdentity))

    validateBackendProgram conflictingConstructorIdentityPayloadProgram
      `shouldBe` Left (BackendConflictingIdentityPayload "constructor" (symbolIdentityStableName conflictingConstructorIdentity))

  it "rejects identity-bearing data declarations with name-only parameters" $
    validateBackendProgram identityDataWithNameOnlyParameterProgram
      `shouldBe` Left (BackendDataParameterIdentityMissing "NamedBox" "a")

  it "rejects duplicate backend data parameter keys" $ do
    let parameterIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991666)
        dataIdentity =
          testSymbolIdentity 991667 SymbolType "Main" "DupParams"
        duplicateParameterData =
          BackendDataWithIdentity
            (Just dataIdentity)
            "DupParams"
            [ backendDataParameterRefFromIdentity parameterIdentity "a",
              backendDataParameterRefFromIdentity parameterIdentity "stale-a"
            ]
            []
        program =
          BackendProgram
            [ (emptyModule "Main")
                { backendModuleData = [duplicateParameterData],
                  backendModuleBindings = [mainLiteralBinding]
                }
            ]
            "main"
    validateBackendProgram program
      `shouldBe` Left (BackendDuplicateDataParameter "DupParams" (typeBinderIdentityStableName parameterIdentity))

  it "rejects identity-bearing constructor signatures with name-only parameter references" $
    validateBackendProgram identityDataWithNameOnlyConstructorParameterProgram
      `shouldBe` Left (BackendConstructorUnknownTypeVariable "NamedBox" "a")

  it "rejects identity-bearing data constructors with name-only forall binders" $
    validateBackendProgram identityDataWithNameOnlyConstructorForallProgram
      `shouldBe` Left (BackendConstructorTypeBinderIdentityMissing "NamedBox" "a")

  it "requires primitive identity for shared primitive inventory globals during backend validation" $ do
    forM_ (Map.toList PrimitiveInventory.primitiveValueSpecs) $ \(name, spec0) -> do
      let ty = primitiveTypeToBackendType (PrimitiveInventory.primitiveValueType spec0)
          programByName =
            BackendProgram
              [BackendModule "Main" [] [BackendBinding "main" ty (BackendVar ty name) True]]
              "main"
          programByIdentity =
            BackendProgram
              [ BackendModule
                  "Main"
                  []
                  [ BackendBinding
                      "main"
                      ty
                      (BackendVarWithIdentity ty (Just (PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity name)))) name)
                      True
                  ]
              ]
              "main"
      validateBackendProgram programByName `shouldBe` Left (BackendUnknownVariable name)
      validateBackendProgram programByIdentity `shouldBe` Right ()

  it "uses primitive identity rather than spelling for primitive runtime type matching" $ do
    let primitiveName = PrimitiveInventory.stringLengthPrimitiveName
        placeholderTy = BTVarWithIdentity (Just (typeBinderIdentityFromUnique (UniqueIdentity 991205))) "$runtime_placeholder"
        primitiveDetails = PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity primitiveName))
        expr = BackendVarWithIdentity placeholderTy (Just primitiveDetails) "__renamed_string_length"
    validateBackendProgram (programWithMainExpr expr) `shouldBe` Right ()

  it "does not look up primitive runtime variables through stale identity payloads" $ do
    let primitiveName = PrimitiveInventory.nativeAndPrimitiveName
        primitiveTy = BTArrow boolTy (BTArrow boolTy boolTy)
        stalePrimitiveIdentity = renameSymbolDefiningName "$stale_and" (builtinValueIdentity primitiveName)
        primitiveDetails = PrimitiveId (primitiveRefFromSymbol stalePrimitiveIdentity)
        expr = BackendVarWithIdentity primitiveTy (Just primitiveDetails) "$stale_and"
    validateBackendProgram (programWithMainExpr expr) `shouldBe` Left (BackendUnknownVariable "$stale_and")

  it "types primitive Prelude data heads by identity during backend validation" $ do
    let primitiveName = PrimitiveInventory.stringCharAtOptionPrimitiveName
        stringTy = literalBackendType (LString "")
        charTy = literalBackendType (LChar '\0')
        optionTy0 =
          BTConWithIdentity
            (Just preludeOptionIdentity)
            (BaseTy "Prelude.Option")
            (charTy :| [])
        primitiveTy = BTArrow stringTy (BTArrow intTy optionTy0)
        primitiveDetails = PrimitiveId (primitiveRefFromSymbol (builtinValueIdentity primitiveName))
        expr = BackendVarWithIdentity primitiveTy (Just primitiveDetails) "__renamed_string_char_at_option"
        preludeOptionData =
          BackendDataWithIdentity
            { backendDataIdentity = Just preludeOptionIdentity,
              backendDataNameWithIdentity = "Prelude.Option",
              backendDataParameterRefsWithIdentity = [backendDataParameterRefFromIdentity optionParamIdentity "a"],
              backendDataConstructorsWithIdentity = []
            }
        program =
          BackendProgram
            [ BackendModule "Prelude" [preludeOptionData] [],
              BackendModule "Main" [] [BackendBinding "main" primitiveTy expr True]
            ]
            "main"
    validateBackendProgram program `shouldBe` Right ()

  it "does not grant primitive runtime type matching to same-named fake identities" $ do
    let primitiveName = PrimitiveInventory.stringLengthPrimitiveName
        fakeIdentity =
          testSymbolIdentity 991206 SymbolValue "Main" primitiveName
        placeholderTy = BTVarWithIdentity (Just (typeBinderIdentityFromUnique (UniqueIdentity 991207))) "$fake_placeholder"
        expr = BackendVarWithIdentity placeholderTy (Just (TopLevelId fakeIdentity)) primitiveName
    validateBackendProgram (programWithBindings [bindingWithMetadata primitiveName fakeIdentity intTy (intLit 1), mainBinding expr])
      `shouldBe` Left (BackendVariableTypeMismatch primitiveName intTy placeholderTy)

  it "assigns identities to backend primitive type variables" $ do
    let ty =
          primitiveTypeToBackendType
            (PrimitiveInventory.PrimitiveTypeForall "a" (PrimitiveInventory.PrimitiveTypeVar "a"))
    case ty of
      BTForallWithIdentity (Just binderIdentity) "a" Nothing (BTVarWithIdentity (Just varIdentity) "a") ->
        varIdentity `shouldBe` binderIdentity
      _ ->
        expectationFailure ("expected identity-bearing primitive forall, got " ++ show ty)

  it "seeds backend primitive type binders after supplied head identities" $ do
    let headIdentity =
          testSymbolIdentity 0 SymbolType "Prelude" "Token"
        (ty, _) =
          primitiveTypeToBackendTypeFromWithHeadIdentities
            (Map.singleton "Token" headIdentity)
            initialIdentityGenerator
            ( PrimitiveInventory.PrimitiveTypeForall
                "a"
                (PrimitiveInventory.PrimitiveTypeCon "Token" (PrimitiveInventory.PrimitiveTypeVar "a" :| []))
            )
    case ty of
      BTForallWithIdentity
        (Just binderIdentity)
        "a"
        Nothing
        (BTConWithIdentity (Just actualHeadIdentity) (BaseTy actualHeadName) (BTVarWithIdentity (Just varIdentity) "a" :| [])) -> do
          actualHeadIdentity `shouldBe` headIdentity
          actualHeadName `shouldBe` "Token"
          binderIdentity `shouldBe` typeBinderIdentityFromUnique (UniqueIdentity 1)
          varIdentity `shouldBe` binderIdentity
      _ ->
        expectationFailure ("expected seeded primitive backend type, got " ++ show ty)

  it "resolves primitive type heads through identity aliases" $ do
    let headIdentity =
          testSymbolIdentity 0 SymbolType "Prelude" "Token"
        stableHeadName = symbolIdentityStableName headIdentity
        (ty, _) =
          primitiveTypeToBackendTypeFromWithHeadIdentities
            (Map.singleton "Token" headIdentity)
            initialIdentityGenerator
            (PrimitiveInventory.PrimitiveTypeCon stableHeadName (PrimitiveInventory.PrimitiveTypeVar "a" :| []))
    case ty of
      BTConWithIdentity (Just actualHeadIdentity) (BaseTy actualHeadName) (BTVarWithIdentity (Just varIdentity) "a" :| []) -> do
        actualHeadIdentity `shouldBe` headIdentity
        actualHeadName `shouldBe` stableHeadName
        varIdentity `shouldBe` typeBinderIdentityFromUnique (UniqueIdentity 1)
      _ ->
        expectationFailure ("expected stable-name primitive backend type to carry identity metadata, got " ++ show ty)

  it "does not resolve ambiguous primitive type head aliases by display name" $ do
    let leftIdentity =
          testSymbolIdentity 991622 SymbolType "Left" "Token"
        rightIdentity =
          testSymbolIdentity 991623 SymbolType "Right" "Token"
        (ty, _) =
          primitiveTypeToBackendTypeFromWithHeadIdentities
            ( Map.fromList
                [ (symbolIdentityStableName leftIdentity, leftIdentity),
                  (symbolIdentityStableName rightIdentity, rightIdentity)
                ]
            )
            initialIdentityGenerator
            (PrimitiveInventory.PrimitiveTypeBase "Token")
    ty `shouldBe` BTBaseWithIdentity Nothing (BaseTy "Token")

  it "generates backend primitive type binder identities for stable-looking names" $ do
    let stableName = "$typevar#991621"
        stableIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
        freshIdentity = typeBinderIdentityFromUnique (UniqueIdentity 1)
        ty =
          primitiveTypeToBackendType
            ( PrimitiveInventory.PrimitiveTypeForall
                stableName
                ( PrimitiveInventory.PrimitiveTypeForall
                    "a"
                    ( PrimitiveInventory.PrimitiveTypeArrow
                        (PrimitiveInventory.PrimitiveTypeVar stableName)
                        (PrimitiveInventory.PrimitiveTypeVar "a")
                    )
                )
            )
    case ty of
      BTForallWithIdentity
        (Just stableRef)
        _
        Nothing
        ( BTForallWithIdentity
            (Just freshRef)
            _
            Nothing
            (BTArrow (BTVarWithIdentity (Just stableOcc) _) (BTVarWithIdentity (Just freshOcc) _))
          ) -> do
        stableRef `shouldBe` stableIdentity
        stableOcc `shouldBe` stableIdentity
        freshRef `shouldBe` freshIdentity
        freshOcc `shouldBe` freshIdentity
      _ ->
        expectationFailure ("expected stable backend primitive binders, got " ++ show ty)

  it "collects generated identities from backend type and term refs" $ do
    let typeUnique = UniqueIdentity 991201
        termUnique = UniqueIdentity 991202
        typeIdentity = typeBinderIdentityFromUnique typeUnique
        termIdentity = LocalId (localRefFromIdentity (GeneratedLocalId termUnique) "x")
        polyTy = BTForallWithIdentity (Just typeIdentity) "a" Nothing (BTVarWithIdentity (Just typeIdentity) "a")
        expr =
          BackendLamWithIdentity
            { backendExprType = BTArrow polyTy polyTy,
              backendParamIdentity = Just termIdentity,
              backendParamName = "x",
              backendParamType = polyTy,
              backendBody = BackendVarWithIdentity polyTy (Just termIdentity) "x"
            }
        identities = Set.fromList (generatedIdentitiesInBackendProgram (programWithMainExpr expr))
    Set.member typeUnique identities `shouldBe` True
    Set.member termUnique identities `shouldBe` True

  it "keeps stable-looking backend type pattern names metadata-free" $ do
    let stableName = "$typevar#991608"
        graphIdentity = typeBinderIdentityFromNode (NodeId 991609)
        graphName = typeBinderIdentityStableName graphIdentity

    BTVar stableName `shouldBe` BTVarWithIdentity Nothing stableName
    BTVar graphName `shouldBe` BTVarWithIdentity Nothing graphName
    BTVarApp stableName (intTy :| []) `shouldBe` BTVarAppWithIdentity Nothing stableName (intTy :| [])
    BTForall stableName Nothing (BTVar stableName)
      `shouldBe` BTForallWithIdentity Nothing stableName Nothing (BTVarWithIdentity Nothing stableName)
    BTMu stableName (BTVar stableName)
      `shouldBe` BTMuWithIdentity Nothing stableName (BTVarWithIdentity Nothing stableName)

  it "promotes builtin backend type patterns to stored identities" $ do
    BTBase (BaseTy "Int") `shouldBe` BTBaseWithIdentity (Just (builtinTypeIdentity "Int")) (BaseTy "Int")
    BTCon (BaseTy "String") (intTy :| []) `shouldBe` BTConWithIdentity (Just (builtinTypeIdentity "String")) (BaseTy "String") (intTy :| [])

  it "rejects a missing main binding" $ do
    validateBackendProgram (BackendProgram [moduleWithBindings "Main" [binding "other" intTy (intLit 1)]] "main")
      `shouldBe` Left (BackendMainNotFound "main")

  it "finds the main binding by identity when the program main name is stale" $ do
    validateBackendProgram identityMainProgram
      `shouldBe` Right ()

    validateBackendProgram conflictingIdentityMainProgram
      `shouldBe` Left (BackendMainNotFound "$stale-main")

  it "does not find an identity-bearing main binding by stable identity name without metadata" $ do
    validateBackendProgram identityMainStableNameProgram
      `shouldBe` Left (BackendMainNotFound (symbolIdentityStableName duplicateValueIdentity))

  it "does not find an identity-bearing main binding by name-only entry" $ do
    validateBackendProgram identityMainNameOnlyProgram
      `shouldBe` Left (BackendMainNotFound "actual-main")

  it "checks global and lexical variable references" $ do
    validateBackendProgram (programWithMainExpr (BackendVar intTy "missing"))
      `shouldBe` Left (BackendUnknownVariable "missing")

    validateBackendProgram
      ( programWithBindings
          [ binding "helper" intTy (intLit 1),
            mainBinding (BackendVar boolTy "helper")
          ]
      )
      `shouldBe` Left (BackendVariableTypeMismatch "helper" intTy boolTy)

    validateBackendProgram
      ( programWithBindings
          [ binding "helper" intTy (intLit 1),
            mainBinding (BackendVar (BTVar "a") "helper")
          ]
      )
      `shouldBe` Left (BackendVariableTypeMismatch "helper" intTy (BTVar "a"))

    validateBackendProgram mismatchedGlobalBindingIdentityProgram
      `shouldBe` Left (BackendUnknownVariable "helper")

    validateBackendProgram conflictingGlobalBindingPayloadProgram
      `shouldBe` Left (BackendUnknownVariable "helper")

    validateBackendProgram identityGlobalReferencedByNameOnlyProgram
      `shouldBe` Left (BackendUnknownVariable "helper")

    validateBackendProgram identityGlobalReferencedByStableNameProgram
      `shouldBe` Left (BackendUnknownVariable (symbolIdentityStableName duplicateValueIdentity))

    validateBackendProgram mismatchedLocalBindingIdentityProgram
      `shouldBe` Left (BackendUnknownVariable "helper")

    validateBackendProgram mismatchedLiftedHelperIdentityProgram
      `shouldBe` Left (BackendUnknownVariable liftedHelperName)

    validateBackendProgram
      ( programWithMainExpr
          ( BackendLam
              (BTArrow (BTVar "a") (BTVar "b"))
              "x"
              (BTVar "a")
              (BackendVar (BTVar "b") "x")
          )
      )
      `shouldBe` Left (BackendVariableTypeMismatch "x" (BTVar "a") (BTVar "b"))

    validateBackendProgram
      ( programWithMainExpr
          ( BackendLam
              (BTArrow (BTVar "a") (BTVar "a1"))
              "x"
              (BTVar "a")
              (BackendVar (BTVar "a1") "x")
          )
      )
      `shouldBe` Left (BackendVariableTypeMismatch "x" (BTVar "a") (BTVar "a1"))

    validateBackendProgram (programWithMainExpr letIdentityExpr)
      `shouldBe` Right ()

  it "checks lexical variable references by resolved identity when binders carry identity" $ do
    validateBackendProgram (programWithMainExpr (identityLam localXIdentity "stale" localXIdentity))
      `shouldBe` Right ()

    validateBackendProgram (programWithMainExpr (identityLam localXIdentity "x" otherLocalIdentity))
      `shouldBe` Left (BackendUnknownVariable "x")

    validateBackendProgram (programWithMainExpr (identityLamNameOnly localXIdentity "$stale_x"))
      `shouldBe` Left (BackendUnknownVariable "$stale_x")

  it "checks case pattern binder references by resolved identity when binders carry identity" $ do
    validateBackendProgram (programWithDataAndMainExpr [boxData] (identityPatternCase patternNIdentity "stale" patternNIdentity))
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boxData] (identityPatternCase patternNIdentity "n" otherPatternNIdentity))
      `shouldBe` Left (BackendUnknownVariable "n")

  it "checks closure capture references by resolved identity when captures carry identity" $ do
    validateBackendProgram (programWithMainExpr (identityCapturedClosure localXIdentity "stale" localXIdentity))
      `shouldBe` Right ()

    validateBackendProgram (programWithMainExpr (identityCapturedClosure localXIdentity "captured" otherLocalIdentity))
      `shouldBe` Left (BackendUnknownVariable "captured")

  it "checks closure parameter references by resolved identity when params carry identity" $ do
    validateBackendProgram (programWithMainExpr (identityParamClosure localXIdentity "stale" localXIdentity))
      `shouldBe` Right ()

    validateBackendProgram (programWithMainExpr (identityParamClosure localXIdentity "x" otherLocalIdentity))
      `shouldBe` Left (BackendUnknownVariable "x")

  it "ignores same-named type bounds for identity-bearing generated case variables" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991401)
        identityVar = BTVarWithIdentity (Just binderIdentity) "a"
        expr =
          BackendTyAbsWithIdentity
            { backendExprType = BTForallWithIdentity Nothing "a" (Just intTy) (BTArrow identityVar boolTy),
              backendTyParamIdentity = Nothing,
              backendTyParamName = "a",
              backendTyParamBound = Just intTy,
              backendTyAbsBody =
                BackendLamWithIdentity
                  { backendExprType = BTArrow identityVar boolTy,
                    backendParamIdentity = Nothing,
                    backendParamName = "$case0",
                    backendParamType = identityVar,
                    backendBody = BackendVar boolTy "$case0"
                  }
            }

    validateBackendProgram (programWithMainExpr expr)
      `shouldBe` Right ()

  it "recognizes generated case placeholders by identity reference name" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991404)
        identityVar = BTVarWithIdentity (Just binderIdentity) "a"
        caseIdentity = localIdentity 991112 "$case0"
        expr =
          BackendTyAbsWithIdentity
            { backendExprType = BTForallWithIdentity Nothing "a" (Just intTy) (BTArrow identityVar boolTy),
              backendTyParamIdentity = Nothing,
              backendTyParamName = "a",
              backendTyParamBound = Just intTy,
              backendTyAbsBody =
                BackendLamWithIdentity
                  { backendExprType = BTArrow identityVar boolTy,
                    backendParamIdentity = Just caseIdentity,
                    backendParamName = "staleCase",
                    backendParamType = identityVar,
                    backendBody = BackendVarWithIdentity boolTy (Just caseIdentity) "renamedCase"
                  }
            }

    validateBackendProgram (programWithMainExpr expr)
      `shouldBe` Right ()

  it "does not freshen-match identity-bearing generated type variables by name" $ do
    let expectedIdentity = typeBinderIdentityFromNode (NodeId 991402)
        actualIdentity = typeBinderIdentityFromNode (NodeId 991403)
        expectedTy = BTVarWithIdentity (Just expectedIdentity) "$evidence0"
        actualTy = BTVarWithIdentity (Just actualIdentity) "$evidence01"
        expr =
          BackendLamWithIdentity
            { backendExprType = BTArrow expectedTy actualTy,
              backendParamIdentity = Just localXIdentity,
              backendParamName = "$evidence0",
              backendParamType = expectedTy,
              backendBody = BackendVarWithIdentity actualTy (Just localXIdentity) "$evidence01"
            }

    validateBackendProgram (programWithMainExpr expr)
      `shouldBe` Left (BackendVariableTypeMismatch "$evidence01" expectedTy actualTy)

  it "classifies closure-call heads by identity when local names shadow" $ do
    validateBackendProgram (programWithMainExpr identityShadowedClosureApp)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")

  it "does not let name-only binders shadow identity-bearing closure heads" $ do
    validateBackendProgram (programWithMainExpr identityShadowedClosureAppByNameOnlyBinder)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")

  it "classifies pattern closure heads by identity when pattern names shadow" $ do
    validateBackendProgram (programWithDataAndMainExpr [fnBoxData, boxData] identityShadowedPatternClosureApp)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")

  it "keeps resolved global closure heads through same-named pattern and let binders" $ do
    validateBackendProgram identityPatternFallbackClosureProgram
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")

  it "classifies closure-called parameters by identity through shadowing lets" $ do
    validateBackendProgram (programWithMainExpr identityShadowedClosureParam)
      `shouldBe` Right ()

  it "rejects bindings whose declared type differs from the expression type" $ do
    validateBackendBinding (BackendBinding "main" boolTy (intLit 1) True)
      `shouldBe` Left (BackendBindingTypeMismatch "main" boolTy intTy)

  it "rejects literal nodes with incorrect carried types" $ do
    validateBackendExpr (BackendLit boolTy (LInt 1))
      `shouldBe` Left (BackendLiteralTypeMismatch (LInt 1) (literalBackendType (LInt 1)) boolTy)

  it "rejects invalid application, lambda, and let nodes" $ do
    validateBackendExpr (BackendApp intTy intIdentityExpr (boolLit True))
      `shouldBe` Left (BackendApplicationArgumentMismatch intTy boolTy)

    validateBackendExpr (BackendApp intTy intIdentityExpr (BackendVar (BTVar "a") "x"))
      `shouldBe` Left (BackendApplicationArgumentMismatch intTy (BTVar "a"))

    validateBackendExpr (BackendApp (BTVar "a") intIdentityExpr (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch (BTVar "a") intTy)

    let listIntTy = listTy intTy
        listFreeTy = listTy (BTVar "a")
        arrowIntBoolTy = BTArrow intTy boolTy
        arrowFreeBoolTy = BTArrow (BTVar "a") boolTy
        varAppIntTy = BTVarApp "f" (intTy :| [])
        varAppFreeTy = BTVarApp "f" (BTVar "a" :| [])
        forallIntTy = BTForall "x" Nothing (BTArrow intTy (BTVar "x"))
        forallFreeTy = BTForall "y" Nothing (BTArrow (BTVar "a") (BTVar "y"))
        structuralBoxIntTy = BTMu "$Box_self" (singleFieldStructuralBody intTy)
        structuralBoxFreeTy = BTMu "$Box_self" (singleFieldStructuralBody (BTVar "a"))
        listIntToBoolTy = BTArrow listIntTy boolTy
        structuralBoxIntToBoolTy = BTArrow structuralBoxIntTy boolTy
        listTyAbsAppExpr =
          BackendTyAbs
            (BTForall "a" Nothing (BTArrow (listTy (BTVar "a")) boolTy))
            "a"
            Nothing
            ( BackendLam
                (BTArrow (listTy (BTVar "a")) boolTy)
                "xs"
                (listTy (BTVar "a"))
                ( BackendApp
                    boolTy
                    (BackendVar listIntToBoolTy "f")
                    (BackendVar (listTy (BTVar "a")) "xs")
                )
            )
        listIntToBoolExpr =
          BackendLam listIntToBoolTy "ys" listIntTy (boolLit True)
        structuralBoxTyAbsAppExpr =
          BackendTyAbs
            (BTForall "a" Nothing (BTArrow structuralBoxFreeTy boolTy))
            "a"
            Nothing
            ( BackendLam
                (BTArrow structuralBoxFreeTy boolTy)
                "xs"
                structuralBoxFreeTy
                ( BackendApp
                    boolTy
                    (BackendVar structuralBoxIntToBoolTy "f")
                    (BackendVar structuralBoxFreeTy "xs")
                )
            )
        structuralBoxIntToBoolExpr =
          BackendLam structuralBoxIntToBoolTy "box" structuralBoxIntTy (boolLit True)

    validateBackendExpr (BackendApp intTy (BackendVar (BTArrow listIntTy intTy) "f") (BackendVar listFreeTy "xs"))
      `shouldBe` Left (BackendApplicationArgumentMismatch listIntTy listFreeTy)

    validateBackendExpr (BackendApp listFreeTy (BackendVar (BTArrow intTy listIntTy) "f") (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch listFreeTy listIntTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow arrowIntBoolTy boolTy) "f") (BackendVar arrowFreeBoolTy "g"))
      `shouldBe` Left (BackendApplicationArgumentMismatch arrowIntBoolTy arrowFreeBoolTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow varAppIntTy boolTy) "f") (BackendVar varAppFreeTy "x"))
      `shouldBe` Left (BackendApplicationArgumentMismatch varAppIntTy varAppFreeTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow forallIntTy boolTy) "f") (BackendVar forallFreeTy "poly"))
      `shouldBe` Left (BackendApplicationArgumentMismatch forallIntTy forallFreeTy)

    validateBackendExpr (BackendApp listIntTy (BackendVar (BTArrow intTy listFreeTy) "f") (intLit 1))
      `shouldBe` Left (BackendApplicationResultMismatch listIntTy listFreeTy)

    validateBackendProgram (programWithBindings [binding "f" listIntToBoolTy listIntToBoolExpr, mainBinding listTyAbsAppExpr])
      `shouldBe` Left (BackendApplicationArgumentMismatch listIntTy (listTy (BTVar "a")))

    validateBackendProgram (programWithBindings [binding "f" structuralBoxIntToBoolTy structuralBoxIntToBoolExpr, mainBinding structuralBoxTyAbsAppExpr])
      `shouldBe` Left (BackendApplicationArgumentMismatch structuralBoxIntTy structuralBoxFreeTy)

    validateBackendExpr (BackendApp boolTy (BackendVar (BTArrow structuralBoxFreeTy boolTy) "f") (BackendVar structuralBoxIntTy "box"))
      `shouldBe` Right ()

    validateBackendExpr (BackendLam boolTy "x" intTy (BackendVar intTy "x"))
      `shouldBe` Left (BackendLambdaTypeMismatch boolTy idTy)

    validateBackendExpr (BackendLet intTy "x" boolTy (intLit 1) (BackendVar intTy "x"))
      `shouldBe` Left (BackendLetTypeMismatch "x" boolTy intTy)

  it "validates explicit closure construction and indirect closure calls" $ do
    let closure =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$id",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        capturedClosure =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$captured",
              backendClosureCaptures = [BackendClosureCapture Nothing "captured" intTy (intLit 7)],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "captured"
            }
        callClosure value =
          BackendLet
            intTy
            "f"
            idTy
            value
            (BackendClosureCall intTy (BackendVar idTy "f") [intLit 1])
        callClosureAlias value =
          BackendLet
            intTy
            "f"
            idTy
            value
            ( BackendLet
                intTy
                "g"
                idTy
                (BackendVar idTy "f")
                (BackendClosureCall intTy (BackendVar idTy "g") [intLit 1])
            )
        callCaseSelectedClosureField =
          BackendClosureCall
            intTy
            ( BackendCase
                idTy
                (BackendConstruct fnBoxTy "FnBox" [closure])
                (BackendAlternative (BackendConstructorPattern "FnBox" ["f"]) (BackendVar idTy "f") :| [])
            )
            [intLit 1]
        structuralClosureArgTy =
          BTMu "$Box_self" (singleFieldStructuralBody (BTVar "a"))
        structuralClosure =
          BackendClosureWithParamIdentities
            { backendExprType = BTArrow structuralClosureArgTy boolTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$structural",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("box", structuralClosureArgTy)],
              backendClosureBody = boolLit True
            }

    validateBackendProgram (programWithMainExpr (callClosure closure))
      `shouldBe` Right ()
    validateBackendProgram (programWithMainExpr (callClosure capturedClosure))
      `shouldBe` Right ()
    validateBackendProgram (programWithMainExpr (callClosureAlias closure))
      `shouldBe` Right ()
    validateBackendProgram
      ( programWithDataAndBindings
          [fnBoxData]
          [mainBinding callCaseSelectedClosureField]
      )
      `shouldBe` Right ()
    validateBackendExpr (BackendClosureCall boolTy structuralClosure [BackendVar structuralBoxTy "box"])
      `shouldBe` Right ()

  it "lets local non-closure binders shadow closure-valued globals during validation" $ do
    let globalClosure =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$shadowed_global",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        shadowedLetCallProgram =
          programWithBindings
            [ binding "f" idTy globalClosure,
              mainBinding
                ( BackendLet
                    intTy
                    "f"
                    idTy
                    intIdentityExpr
                    (BackendApp intTy (BackendVar idTy "f") (intLit 1))
                )
            ]
        shadowedCaseClosureGlobalProgram =
          programWithDataAndBindings
            [boxData]
            [ binding "f" idTy globalClosure,
              mainBinding
                ( BackendCase
                    intTy
                    (BackendConstruct boxTy "Box" [intLit 1])
                    (BackendAlternative (BackendConstructorPattern "Box" ["f"]) (BackendVar intTy "f") :| [])
                )
            ]

    validateBackendProgram shadowedLetCallProgram `shouldBe` Right ()
    validateBackendProgram shadowedCaseClosureGlobalProgram `shouldBe` Right ()

  it "treats function-valued case pattern binders as closure values during validation" $ do
    let fieldClosure =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$field",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        program =
          programWithDataAndBindings
            [fnBoxData]
            [ mainBinding
                ( BackendClosureCall
                    intTy
                    ( BackendCase
                        idTy
                        (BackendConstruct fnBoxTy "FnBox" [fieldClosure])
                        (BackendAlternative (BackendConstructorPattern "FnBox" ["f"]) (BackendVar idTy "f") :| [])
                    )
                    [intLit 1]
                )
            ]

    validateBackendProgram program `shouldBe` Right ()

  it "rejects malformed closure IR" $ do
    let goodClosure entryName =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = entryName,
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        higherOrderTy =
          BTArrow idTy intTy
        higherOrderClosure entryName =
          BackendClosureWithParamIdentities
            { backendExprType = higherOrderTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = entryName,
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("f", idTy)],
              backendClosureBody = BackendApp intTy (BackendVar idTy "f") (intLit 1)
            }
        captureMismatch =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$bad_capture",
              backendClosureCaptures = [BackendClosureCapture Nothing "captured" boolTy (intLit 7)],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        resultMismatch =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$bad_result",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = boolLit True
            }
        duplicateEntries =
          BackendLet
            intTy
            "f"
            idTy
            (goodClosure "__mlfp_closure$dup")
            ( BackendLet
                intTy
                "g"
                idTy
                (goodClosure "__mlfp_closure$dup")
                (intLit 0)
            )
        entryNameBindingCollision =
          programWithBindings
            [ binding "helper" intTy (intLit 0),
              mainBinding (goodClosure "helper")
            ]
        entryNameRuntimeCollision =
          programWithMainExpr (goodClosure "__mlfp_and")
        duplicateCaptureAndParameter =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$duplicate_binder",
              backendClosureCaptures = [BackendClosureCapture Nothing "x" intTy (intLit 7)],
              backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
              backendClosureBody = BackendVar intTy "x"
            }
        nonFunctionClosure =
          BackendClosureWithParamIdentities
            { backendExprType = intTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$non_function",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [],
              backendClosureBody = intLit 0
            }
        underspecifiedClosureParams =
          BackendClosureWithParamIdentities
            { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$underspecified_params",
              backendClosureCaptures = [],
              backendClosureParamsWithIdentities = backendClosureParams [],
              backendClosureBody = intIdentityExpr
            }
        badCall =
          BackendClosureCall intTy (goodClosure "__mlfp_closure$call") [boolLit True]
        nonClosureCall =
          BackendLet
            intTy
            "f"
            idTy
            intIdentityExpr
            (BackendClosureCall intTy (BackendVar idTy "f") [intLit 1])
        unlistedLocalCapture =
          BackendLet
            idTy
            "captured"
            intTy
            (intLit 7)
            ( BackendClosureWithParamIdentities
                { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$unlisted_capture",
                  backendClosureCaptures = [],
                  backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                  backendClosureBody = BackendVar intTy "captured"
                }
            )
        appCalledClosure =
          BackendLet
            intTy
            "f"
            higherOrderTy
            (higherOrderClosure "__mlfp_closure$app")
            (BackendApp intTy (BackendVar higherOrderTy "f") intIdentityExpr)
        appCalledLetHeadClosure =
          BackendApp
            intTy
            ( BackendLet
                higherOrderTy
                "f"
                higherOrderTy
                (higherOrderClosure "__mlfp_closure$app_let_head")
                (BackendVar higherOrderTy "f")
            )
            intIdentityExpr
        appCalledClosureAlias =
          BackendLet
            intTy
            "g"
            higherOrderTy
            (higherOrderClosure "__mlfp_closure$app_alias")
            ( BackendLet
                intTy
                "f"
                higherOrderTy
                (BackendVar higherOrderTy "g")
                (BackendApp intTy (BackendVar higherOrderTy "f") intIdentityExpr)
            )
        appCalledCaseHeadClosure =
          BackendApp
            intTy
            ( BackendCase
                higherOrderTy
                (BackendConstruct boxTy "Box" [intLit 0])
                ( BackendAlternative
                    (BackendConstructorPattern "Box" ["n"])
                    (higherOrderClosure "__mlfp_closure$app_case")
                    :| []
                )
            )
            intIdentityExpr
        appCalledCapturedClosure =
          BackendLet
            idTy
            "g"
            higherOrderTy
            (higherOrderClosure "__mlfp_closure$app_captured_source")
            ( BackendClosureWithParamIdentities
                { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$app_captured",
                  backendClosureCaptures = [BackendClosureCapture Nothing "capturedClosure" higherOrderTy (BackendVar higherOrderTy "g")],
                  backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
                  backendClosureBody =
                    BackendApp intTy (BackendVar higherOrderTy "capturedClosure") intIdentityExpr
                }
            )
        closureCallMixedCaseHead =
          BackendClosureCall
            intTy
            ( BackendCase
                idTy
                (BackendConstruct boxTy "Box" [intLit 0])
                ( BackendAlternative
                    (BackendConstructorPattern "Box" ["n"])
                    (goodClosure "__mlfp_closure$closure_call_case")
                    :| [BackendAlternative BackendDefaultPattern intIdentityExpr]
                )
            )
            [intLit 1]

    validateBackendProgram (programWithMainExpr captureMismatch)
      `shouldBe` Left (BackendClosureCaptureTypeMismatch "captured" boolTy intTy)
    validateBackendProgram (programWithMainExpr resultMismatch)
      `shouldBe` Left (BackendClosureTypeMismatch "__mlfp_closure$bad_result" idTy (BTArrow intTy boolTy))
    validateBackendProgram (programWithMainExpr duplicateEntries)
      `shouldBe` Left (BackendDuplicateClosureEntry "__mlfp_closure$dup")
    validateBackendProgram entryNameBindingCollision
      `shouldBe` Left (BackendClosureEntryNameCollision "helper")
    validateBackendProgram entryNameRuntimeCollision
      `shouldBe` Left (BackendClosureEntryNameCollision "__mlfp_and")
    validateBackendProgram (programWithMainExpr duplicateCaptureAndParameter)
      `shouldBe` Left (BackendDuplicateClosureParameter "x")
    validateBackendProgram (programWithMainExpr nonFunctionClosure)
      `shouldBe` Left (BackendClosureExpectedFunction "__mlfp_closure$non_function" intTy)
    validateBackendProgram (programWithMainExpr underspecifiedClosureParams)
      `shouldBe` Left (BackendClosureParameterArityMismatch "__mlfp_closure$underspecified_params" 0 1)
    validateBackendProgram (programWithMainExpr badCall)
      `shouldBe` Left (BackendClosureCallArgumentMismatch 0 intTy boolTy)
    validateBackendProgram (programWithMainExpr nonClosureCall)
      `shouldBe` Left (BackendDirectCalledWithBackendClosureCall "f")
    validateBackendProgram (programWithMainExpr unlistedLocalCapture)
      `shouldBe` Left (BackendUnknownVariable "captured")
    validateBackendProgram (programWithMainExpr appCalledClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledLetHeadClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledClosureAlias)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "f")
    validateBackendProgram (programWithMainExpr appCalledCaseHeadClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "__mlfp_closure$app_case")
    validateBackendProgram (programWithMainExpr appCalledCapturedClosure)
      `shouldBe` Left (BackendClosureCalledWithBackendApp "capturedClosure")
    validateBackendProgram (programWithMainExpr closureCallMixedCaseHead)
      `shouldBe` Left (BackendClosureCallExpectedClosureValue idTy)

  it "checks closure binder uniqueness by identity before display name" $ do
    let leftIdentity = localIdentity 991614 "x"
        rightIdentity = localIdentity 991615 "x"
        binaryIntTy = BTArrow intTy idTy
        closureCall closureExpr =
          BackendClosureCall intTy closureExpr [intLit 1, intLit 2]
        mkClosure resultTy entryName captures params body =
          BackendClosureWithParamIdentities
            { backendExprType = resultTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = entryName,
              backendClosureCaptures = captures,
              backendClosureParamsWithIdentities = params,
              backendClosureBody = body
            }
        distinctParamClosure =
          mkClosure
            binaryIntTy
            "__mlfp_closure$distinct_param_identities"
            []
            [ BackendClosureParam (Just leftIdentity) "x" intTy,
              BackendClosureParam (Just rightIdentity) "x" intTy
            ]
            (BackendVarWithIdentity intTy (Just rightIdentity) "x")
        duplicateParamClosure =
          mkClosure
            binaryIntTy
            "__mlfp_closure$duplicate_param_identity"
            []
            [ BackendClosureParam (Just leftIdentity) "x" intTy,
              BackendClosureParam (Just leftIdentity) "x" intTy
            ]
            (BackendVarWithIdentity intTy (Just leftIdentity) "x")
        distinctCaptureParamClosure =
          mkClosure
            idTy
            "__mlfp_closure$distinct_capture_param_identities"
            [BackendClosureCapture (Just leftIdentity) "x" intTy (intLit 7)]
            [BackendClosureParam (Just rightIdentity) "x" intTy]
            (BackendVarWithIdentity intTy (Just rightIdentity) "x")
        duplicateCaptureParamClosure =
          mkClosure
            idTy
            "__mlfp_closure$duplicate_capture_param_identity"
            [BackendClosureCapture (Just leftIdentity) "x" intTy (intLit 7)]
            [BackendClosureParam (Just leftIdentity) "x" intTy]
            (BackendVarWithIdentity intTy (Just leftIdentity) "x")
        duplicateCaptureClosure =
          mkClosure
            idTy
            "__mlfp_closure$duplicate_capture_identity"
            [ BackendClosureCapture (Just leftIdentity) "x" intTy (intLit 7),
              BackendClosureCapture (Just leftIdentity) "x" intTy (intLit 8)
            ]
            [BackendClosureParam (Just rightIdentity) "y" intTy]
            (BackendVarWithIdentity intTy (Just rightIdentity) "x")

    validateBackendProgram (programWithMainExpr (closureCall distinctParamClosure))
      `shouldBe` Right ()
    validateBackendProgram (programWithMainExpr (BackendClosureCall intTy distinctCaptureParamClosure [intLit 2]))
      `shouldBe` Right ()
    validateBackendProgram (programWithMainExpr (closureCall duplicateParamClosure))
      `shouldBe` Left (BackendDuplicateClosureParameter "x")
    validateBackendProgram (programWithMainExpr (BackendClosureCall intTy duplicateCaptureParamClosure [intLit 2]))
      `shouldBe` Left (BackendDuplicateClosureParameter "x")
    validateBackendProgram (programWithMainExpr (BackendClosureCall intTy duplicateCaptureClosure [intLit 2]))
      `shouldBe` Left (BackendDuplicateClosureCapture "x")

  it "checks pattern binder uniqueness by identity before display name" $ do
    let leftIdentity = localIdentity 991616 "x"
        rightIdentity = localIdentity 991617 "x"
        patternCase binders body =
          BackendCase
            { backendExprType = boolTy,
              backendScrutinee = identityPairExpr,
              backendAlternatives =
                BackendAlternative
                  (BackendConstructorPatternWithBinderIdentities (Just identityPairConstructorIdentity) "IdentityPair" binders)
                  body
                  :| []
            }
        distinctIdentityCase =
          patternCase
            [ BackendPatternBinder (Just leftIdentity) "x",
              BackendPatternBinder (Just rightIdentity) "x"
            ]
            (BackendVarWithIdentity boolTy (Just rightIdentity) "x")
        duplicateIdentityCase =
          patternCase
            [ BackendPatternBinder (Just leftIdentity) "x",
              BackendPatternBinder (Just leftIdentity) "x"
            ]
            (BackendVarWithIdentity boolTy (Just leftIdentity) "x")
        duplicateNameOnlyCase =
          patternCase
            [ BackendPatternBinder Nothing "x",
              BackendPatternBinder Nothing "x"
            ]
            (BackendVar boolTy "x")

    validateBackendProgram (programWithDataAndMainExpr [identityPairData] distinctIdentityCase)
      `shouldBe` Right ()
    validateBackendProgram (programWithDataAndMainExpr [identityPairData] duplicateIdentityCase)
      `shouldBe` Left (BackendDuplicatePatternBinding "x")
    validateBackendProgram (programWithDataAndMainExpr [identityPairData] duplicateNameOnlyCase)
      `shouldBe` Left (BackendDuplicatePatternBinding "x")

  it "checks type application against forall nodes" $ do
    validateBackendExpr
      ( BackendTyApp
          { backendExprType = idTy,
            backendTyFunction = BackendVar polyIdTy "id",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = intTy,
            backendTyFunction = intLit 1,
            backendTyArgument = intTy
          }
      )
      `shouldBe` Left (BackendTypeAppExpectedForall intTy)

    let binderIdentity = typeBinderIdentityFromNode (NodeId 991205)
        resolvedForallTy =
          BTForallWithIdentity
            (Just binderIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just binderIdentity) "a") (BTVar "a"))
        resolvedAppTy = BTArrow intTy (BTVar "a")

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = resolvedAppTy,
            backendTyFunction = BackendVar resolvedForallTy "resolvedPoly",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

  it "uses capture-avoiding substitution for type application" $ do
    let freshIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
    let sourceTy = BTForall "b" Nothing (BTArrow (BTVar "a") (BTVar "b"))
        polyTy = BTForall "a" Nothing sourceTy
        expectedTy =
          BTForallWithIdentity
            (Just freshIdentity)
            "b1"
            Nothing
            (BTArrow (BTVar "b") (BTVarWithIdentity (Just freshIdentity) "b1"))
        capturedTy = BTForall "b" Nothing (BTArrow (BTVar "b") (BTVar "b"))

    substituteBackendTypeByName "a" (BTVar "b") sourceTy
      `shouldBe` expectedTy

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = expectedTy,
            backendTyFunction = BackendVar polyTy "poly",
            backendTyArgument = BTVar "b"
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = capturedTy,
            backendTyFunction = BackendVar polyTy "poly",
            backendTyArgument = BTVar "b"
          }
      )
      `shouldBe` Left (BackendTypeAppResultMismatch capturedTy expectedTy)

  it "does not choose the substituted variable while freshening binders" $ do
    let freshIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
    substituteBackendTypeByName "a1" (BTVar "a") (BTForall "a" Nothing (BTVar "a"))
      `shouldBe` BTForallWithIdentity (Just freshIdentity) "a2" Nothing (BTVarWithIdentity (Just freshIdentity) "a2")

    substituteBackendTypeByName "a1" (BTVar "a") (BTMu "a" (BTVar "a"))
      `shouldBe` BTMuWithIdentity (Just freshIdentity) "a2" (BTVarWithIdentity (Just freshIdentity) "a2")

  it "applies multiple backend type substitutions simultaneously" $ do
    let sourceTy = pairTy (BTVar "a") (BTVar "b")
        substitutions = Map.fromList [("a", BTVar "b"), ("b", BTVar "a")]

    substituteBackendTypesByName substitutions sourceTy `shouldBe` pairTy (BTVar "b") (BTVar "a")

  it "substitutes backend type variables by identity when names collide" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991201)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991202)
        leftVar = BTVarWithIdentity (Just leftIdentity) "a"
        rightVar = BTVarWithIdentity (Just rightIdentity) "a"

    substituteBackendTypeByIdentity leftIdentity intTy (BTArrow leftVar rightVar)
      `shouldBe` BTArrow intTy rightVar

    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFor Nothing "a") boolTy) (BTArrow leftVar (BTVar "a"))
      `shouldBe` BTArrow leftVar boolTy

  it "keeps stable backend type names name-keyed without metadata" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 991602)
        stableName = typeBinderIdentityStableName identity
        key = backendTypeSubstitutionKeyFor Nothing stableName
        stableVar = BTVarWithIdentity Nothing stableName
        identityVar = BTVarWithIdentity (Just identity) "stale"

    key `shouldNotBe` backendTypeSubstitutionKeyFromIdentity identity
    backendTypeSubstitutionKeyName key `shouldBe` stableName
    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity identity) intTy) stableVar `shouldBe` stableVar
    substituteBackendTypesByKey (Map.singleton key intTy) stableVar `shouldBe` intTy
    substituteBackendTypeForBinder Nothing stableName boolTy stableVar `shouldBe` stableVar
    stableVar `shouldNotBe` identityVar
    generatedIdentitiesInBackendTypes [stableVar] `shouldBe` []
    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity identity) intTy) identityVar `shouldBe` intTy
    generatedIdentitiesInBackendTypes [identityVar] `shouldBe` [UniqueIdentity 991602]

  it "renders backend identity substitution keys from binder identity" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 991604)
    backendTypeSubstitutionKeyName (backendTypeSubstitutionKeyFromIdentity identity)
      `shouldBe` typeBinderIdentityStableName identity

  it "keys backend data parameter refs by identity when names are stale" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 991605)
        originalRef = backendDataParameterRefFromIdentity identity "a"
        staleRef = backendDataParameterRefFromIdentity identity "stale"

    backendDataParameterRefKey originalRef `shouldBe` backendDataParameterRefKey staleRef
    backendDataParameterRefIdentity staleRef `shouldBe` Just identity
    backendDataParameterRefKey staleRef `shouldBe` backendTypeSubstitutionKeyFromIdentity identity
    Set.fromList [originalRef, staleRef] `shouldBe` Set.singleton originalRef

  it "compares backend callable refs by identity when names are stale" $ do
    let identity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991606)) "f")

    backendCallableRef (Just identity) "f"
      `shouldBe` backendCallableRef (Just identity) "stale"
    backendCallableRef Nothing "f"
      `shouldNotBe` backendCallableRef Nothing "stale"

  it "compares closure callable heads by entry identity when names are stale" $ do
    let identity = Just (UniqueIdentity 991616)
        stableClosure = BackendClosureWithParamIdentities intTy identity "stable" [] [] (intLit 0)
        staleClosure = BackendClosureWithParamIdentities intTy identity "stale" [] [] (intLit 0)

    backendCallableHead (\_ _ -> BackendCallableBindingUnknown) stableClosure
      `shouldBe` backendCallableHead (\_ _ -> BackendCallableBindingUnknown) staleClosure

  it "does not pick an arbitrary callable ref when case heads differ" $ do
    let leftIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991617)) "f")
        rightIdentity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991618)) "f")
        directCase =
          BackendCase
            intTy
            (intLit 0)
            ( BackendAlternative BackendDefaultPattern (BackendVarWithIdentity intTy (Just leftIdentity) "f")
                :| [BackendAlternative BackendDefaultPattern (BackendVarWithIdentity intTy (Just rightIdentity) "f")]
            )
        closureCase =
          BackendCase
            intTy
            (intLit 0)
            ( BackendAlternative BackendDefaultPattern (BackendClosureWithParamIdentities intTy (Just (UniqueIdentity 991619)) "left" [] [] (intLit 0))
                :| [BackendAlternative BackendDefaultPattern (BackendClosureWithParamIdentities intTy (Just (UniqueIdentity 991620)) "right" [] [] (intLit 0))]
            )
    backendCallableHead (\_ _ -> BackendCallableBindingDirect) directCase
      `shouldBe` BackendDirectCallableHead Nothing
    case backendCallableHead (\_ _ -> BackendCallableBindingUnknown) closureCase of
      BackendClosureCallableHead ref ->
        backendCallableRefName ref `shouldBe` "__mlfp_unknown_closure_head"
      other ->
        expectationFailure ("expected closure callable head, got " ++ show other)

  it "compares backend binder records by identity when names are stale" $ do
    let identity = LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991607)) "x")

    BackendClosureParam (Just identity) "x" intTy
      `shouldBe` BackendClosureParam (Just identity) "stale" intTy
    BackendClosureCapture (Just identity) "x" intTy (intLit 1)
      `shouldBe` BackendClosureCapture (Just identity) "stale" intTy (intLit 1)
    BackendPatternBinder (Just identity) "x"
      `shouldBe` BackendPatternBinder (Just identity) "stale"
    BackendPatternBinder Nothing "x"
      `shouldNotBe` BackendPatternBinder Nothing "stale"

  it "compares backend expressions by identity when names are stale" $ do
    let termIdentity = localIdentity 991608 "x"
        typeIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991609)
        constructorIdentity =
          testSymbolIdentity 991610 SymbolConstructor "Main" "Mk"
        staleVar = BackendVarWithIdentity intTy (Just termIdentity) "stale-x"
        stableVar = BackendVarWithIdentity intTy (Just termIdentity) "x"

    staleVar `shouldBe` stableVar
    BackendLamWithIdentity (BTArrow intTy intTy) (Just termIdentity) "stale-x" intTy staleVar
      `shouldBe` BackendLamWithIdentity (BTArrow intTy intTy) (Just termIdentity) "x" intTy stableVar
    BackendTyAbsWithIdentity (BTForallWithIdentity (Just typeIdentity) "a" Nothing intTy) (Just typeIdentity) "stale-a" Nothing stableVar
      `shouldBe` BackendTyAbsWithIdentity (BTForallWithIdentity (Just typeIdentity) "a" Nothing intTy) (Just typeIdentity) "a" Nothing staleVar
    BackendConstructWithIdentity intTy (Just constructorIdentity) "stale-Mk" [stableVar]
      `shouldBe` BackendConstructWithIdentity intTy (Just constructorIdentity) "Mk" [staleVar]
    BackendConstructWithIdentity intTy Nothing (symbolIdentityStableName constructorIdentity) [stableVar]
      `shouldNotBe` BackendConstructWithIdentity intTy (Just constructorIdentity) "Mk" [staleVar]
    BackendCase
      intTy
      stableVar
      ( BackendAlternative
          (BackendConstructorPatternWithBinderIdentities (Just constructorIdentity) "stale-Mk" [BackendPatternBinder (Just termIdentity) "stale-x"])
          stableVar
          :| []
      )
      `shouldBe` BackendCase
        intTy
        staleVar
        ( BackendAlternative
            (BackendConstructorPatternWithBinderIdentities (Just constructorIdentity) "Mk" [BackendPatternBinder (Just termIdentity) "x"])
            staleVar
            :| []
        )
    BackendClosureWithParamIdentities
      idTy
      (Just (UniqueIdentity 991611))
      "stale-entry"
      []
      [BackendClosureParam (Just termIdentity) "stale-x" intTy]
      staleVar
      `shouldBe` BackendClosureWithParamIdentities
        idTy
        (Just (UniqueIdentity 991611))
        "entry"
        []
        [BackendClosureParam (Just termIdentity) "x" intTy]
        stableVar
    BackendClosureWithParamIdentities
      idTy
      Nothing
      (uniqueIdentityStableName (UniqueIdentity 991611))
      []
      [BackendClosureParam (Just termIdentity) "stale-x" intTy]
      staleVar
      `shouldNotBe` BackendClosureWithParamIdentities
        idTy
        (Just (UniqueIdentity 991611))
        "entry"
        []
        [BackendClosureParam (Just termIdentity) "x" intTy]
        stableVar
    BackendClosureWithParamIdentities
      idTy
      Nothing
      "stale-entry"
      []
      [BackendClosureParam (Just termIdentity) "x" intTy]
      stableVar
      `shouldNotBe` BackendClosureWithParamIdentities
        idTy
        Nothing
        "entry"
        []
        [BackendClosureParam (Just termIdentity) "x" intTy]
        stableVar

  it "compares backend metadata records by identity when names are stale" $ do
    let typeIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991611)
        dataIdentity =
          testSymbolIdentity 991612 SymbolType "Main" "Box"
        constructorIdentity =
          testSymbolIdentity 991613 SymbolConstructor "Main" "Box"
        bindingIdentity =
          testSymbolIdentity 991614 SymbolValue "Main" "box"
        stableTy = BTBaseWithIdentity (Just dataIdentity) (BaseTy "Box")
        staleTy = BTBaseWithIdentity (Just dataIdentity) (BaseTy "StaleBox")
        stableCtor =
          BackendConstructorWithIdentity
            (Just constructorIdentity)
            "Box"
            [BackendTypeBinderWithIdentity (Just typeIdentity) "a" Nothing]
            [BTVarWithIdentity (Just typeIdentity) "a"]
            stableTy
        staleCtor =
          BackendConstructorWithIdentity
            (Just constructorIdentity)
            "StaleBox"
            [BackendTypeBinderWithIdentity (Just typeIdentity) "stale-a" Nothing]
            [BTVarWithIdentity (Just typeIdentity) "stale-a"]
            staleTy

    BackendTypeBinderWithIdentity (Just typeIdentity) "a" Nothing
      `shouldBe` BackendTypeBinderWithIdentity (Just typeIdentity) "stale-a" Nothing
    stableCtor `shouldBe` staleCtor
    BackendDataWithIdentity (Just dataIdentity) "Box" [backendDataParameterRefFromIdentity typeIdentity "a"] [stableCtor]
      `shouldBe` BackendDataWithIdentity (Just dataIdentity) "StaleBox" [backendDataParameterRefFromIdentity typeIdentity "stale-a"] [staleCtor]
    BackendBindingWithMetadata (Just bindingIdentity) "box" stableTy (intLit 1) False Set.empty
      `shouldBe` BackendBindingWithMetadata (Just bindingIdentity) "stale-box" staleTy (intLit 1) False Set.empty

  it "keeps generated stable type binder names out of identity substitution scope without metadata" $ do
    let identity = typeBinderIdentityFromUnique (UniqueIdentity 991603)
        stableName = "$typevar#991603"
        sourceTy = BTForallWithIdentity Nothing stableName Nothing (BTVarWithIdentity Nothing stableName)

    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity identity) intTy) sourceTy
      `shouldBe` sourceTy
    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFor Nothing stableName) intTy) sourceTy
      `shouldBe` sourceTy

  it "does not let identity-bearing binders delete name-only substitutions" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991203)
        binderVar = BTVarWithIdentity (Just binderIdentity) "a"
        sourceTy = BTForallWithIdentity (Just binderIdentity) "a" Nothing (BTArrow (BTVar "a") binderVar)
        expectedTy = BTForallWithIdentity (Just binderIdentity) "a" Nothing (BTArrow boolTy binderVar)

    substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFor Nothing "a") boolTy) sourceTy
      `shouldBe` expectedTy

  it "freshens identity-bearing binders with a fresh identity during substitution" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991204)
        targetIdentity = typeBinderIdentityFromNode (NodeId 991206)
        replacement = BTVarWithIdentity (Just binderIdentity) "stale"
        sourceTy = BTForallWithIdentity (Just binderIdentity) "a" Nothing (BTVarWithIdentity (Just targetIdentity) "x")

    case substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity targetIdentity) replacement) sourceTy of
      BTForallWithIdentity (Just freshIdentity) "a1" Nothing body -> do
        freshIdentity `shouldNotBe` binderIdentity
        body `shouldBe` replacement
      other ->
        expectationFailure ("expected fresh identity-bearing forall, got " ++ show other)

  it "does not freshen identity-bearing binders into substitution target identities" $ do
    let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 0)
        targetIdentity = typeBinderIdentityFromUnique (UniqueIdentity 1)
        replacement = BTVarWithIdentity (Just binderIdentity) "a"
        sourceTy = BTForallWithIdentity (Just binderIdentity) "a" Nothing replacement

    case substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFromIdentity targetIdentity) replacement) sourceTy of
      BTForallWithIdentity (Just freshIdentity) "a1" Nothing body -> do
        freshIdentity `shouldNotBe` binderIdentity
        freshIdentity `shouldNotBe` targetIdentity
        body `shouldBe` BTVarWithIdentity (Just freshIdentity) "a1"
      other ->
        expectationFailure ("expected substitution-key-seeded freshening, got " ++ show other)

  it "reports free backend type variables by identity key" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991207)
        freeIdentity = typeBinderIdentityFromNode (NodeId 991208)
        ty =
          BTForallWithIdentity
            (Just binderIdentity)
            "a"
            Nothing
            (BTArrow (BTVarWithIdentity (Just binderIdentity) "stale") (BTVarWithIdentity (Just freeIdentity) "a"))

    freeBackendTypeVarKeys ty
      `shouldBe` Set.singleton (backendTypeSubstitutionKeyFromIdentity freeIdentity)

  it "compares free backend type variables by identity before spelling" $ do
    let leftIdentity = typeBinderIdentityFromNode (NodeId 991301)
        rightIdentity = typeBinderIdentityFromNode (NodeId 991302)

    alphaEqBackendType
      (BTVarWithIdentity (Just leftIdentity) "a")
      (BTVarWithIdentity (Just leftIdentity) "renamed")
      `shouldBe` True

    alphaEqBackendType
      (BTVarWithIdentity (Just leftIdentity) "a")
      (BTVarWithIdentity (Just rightIdentity) "a")
      `shouldBe` False

    alphaEqBackendType
      (BTVarWithIdentity (Just leftIdentity) "a")
      (BTVar "a")
      `shouldBe` False

    alphaEqBackendType
      (BTForallWithIdentity (Just leftIdentity) "a" Nothing (BTVarWithIdentity (Just leftIdentity) "stale"))
      (BTForallWithIdentity (Just rightIdentity) "b" Nothing (BTVarWithIdentity (Just rightIdentity) "renamed"))
      `shouldBe` True

    alphaEqBackendType
      (BTVarAppWithIdentity (Just leftIdentity) "f" (intTy :| []))
      (BTVarAppWithIdentity (Just rightIdentity) "f" (intTy :| []))
      `shouldBe` False

  it "checks bounded type application arguments" $ do
    let boundedIdTy = BTForall "a" (Just intTy) (BTArrow (BTVar "a") (BTVar "a"))
        boolIdTy = BTArrow boolTy boolTy

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = idTy,
            backendTyFunction = BackendVar boundedIdTy "boundedId",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = boolIdTy,
            backendTyFunction = BackendVar boundedIdTy "boundedId",
            backendTyArgument = boolTy
          }
      )
      `shouldBe` Left (BackendTypeAppBoundMismatch intTy boolTy)

  it "compares validator types modulo alpha-equivalence" $ do
    let exprIdentity = typeBinderIdentityFromNode (NodeId 991360)
        declaredIdentity = typeBinderIdentityFromNode (NodeId 991361)
        appResultIdentity = typeBinderIdentityFromNode (NodeId 991362)
        appArgumentIdentity = typeBinderIdentityFromNode (NodeId 991363)
        appFunctionResultIdentity = typeBinderIdentityFromNode (NodeId 991364)
        exprVar = BTVarWithIdentity (Just exprIdentity) "a"
        declaredVar = BTVarWithIdentity (Just declaredIdentity) "b"
        appResultVar = BTVarWithIdentity (Just appResultIdentity) "z"
        appArgumentVar = BTVarWithIdentity (Just appArgumentIdentity) "a"
        appFunctionResultVar = BTVarWithIdentity (Just appFunctionResultIdentity) "b"
        exprTy = BTForallWithIdentity (Just exprIdentity) "a" Nothing (BTArrow exprVar exprVar)
        declaredTy = BTForallWithIdentity (Just declaredIdentity) "b" Nothing (BTArrow declaredVar declaredVar)
        alphaIdentityExpr =
          BackendTyAbsWithIdentity
            { backendExprType = exprTy,
              backendTyParamIdentity = Just exprIdentity,
              backendTyParamName = "a",
              backendTyParamBound = Nothing,
              backendTyAbsBody =
                BackendLamWithIdentity
                  { backendParamIdentity = Nothing, backendExprType = BTArrow exprVar exprVar,
                    backendParamName = "x",
                    backendParamType = exprVar,
                    backendBody = BackendVar exprVar "x"
                  }
            }
        appExpectedTy = BTForallWithIdentity (Just appResultIdentity) "z" Nothing (BTArrow intTy appResultVar)
        appFunctionTy =
          BTForallWithIdentity
            (Just appArgumentIdentity)
            "a"
            Nothing
            (BTForallWithIdentity (Just appFunctionResultIdentity) "b" Nothing (BTArrow appArgumentVar appFunctionResultVar))

    validateBackendBinding (BackendBinding "poly" declaredTy alphaIdentityExpr False)
      `shouldBe` Right ()

    validateBackendExpr
      ( BackendTyApp
          { backendExprType = appExpectedTy,
            backendTyFunction = BackendVar appFunctionTy "poly",
            backendTyArgument = intTy
          }
      )
      `shouldBe` Right ()

  it "keeps structural recursive owner names module-qualified" $ do
    alphaEqBackendType (BTBase (BaseTy "Core.T")) (BTMu "$Core.T_self" nullaryStructuralBody)
      `shouldBe` True

    alphaEqBackendType (BTBase (BaseTy "Other.T")) (BTMu "$Core.T_self" nullaryStructuralBody)
      `shouldBe` False

    alphaEqBackendType (BTBase (BaseTy "Other.T")) (BTMu "$T_self" nullaryStructuralBody)
      `shouldBe` False

  it "rejects non-structural recursive bodies as nominal data encodings" $ do
    alphaEqBackendType (BTBase (BaseTy "Core.T")) (BTMu "$Core.T_self" BTBottom)
      `shouldBe` False

    validateBackendProgram (programWithMainExpr malformedStructuralBoxConstructExpr)
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy malformedStructuralBoxTy)

  it "allows opaque IO continuations to specialize their argument type" $ do
    let bindTailTy =
          BTArrow
            (BTArrow (BTVar "a") (ioTy BTBottom))
            (ioTy BTBottom)
        specializedBindTailTy =
          BTArrow
            (BTArrow preludeUnitStructuralTy (ioTy unitTy))
            (ioTy preludeUnitStructuralTy)

    validateBackendExpr
      ( BackendApp
          { backendExprType = specializedBindTailTy,
            backendFunction = BackendVar (BTArrow (ioTy BTBottom) bindTailTy) "__io_bind",
            backendArgument = BackendVar (ioTy BTBottom) "action"
          }
      )
      `shouldBe` Right ()

  it "does not treat same-named fake IO type identities as opaque IO" $ do
    let fakeIOIdentity =
          testSymbolIdentity 991822 SymbolType "Other" "IO"
        fakeIOTy = BTConWithIdentity (Just fakeIOIdentity) (BaseTy "IO") (intTy :| [])
        builtinIOTy = BTConWithIdentity (Just (builtinTypeIdentity "IO")) (BaseTy "IO") (intTy :| [])

    alphaEqBackendType fakeIOTy builtinIOTy `shouldBe` False

  it "does not treat name-only IO type heads as opaque IO" $ do
    let nameOnlyIOTy = BTConWithIdentity Nothing (BaseTy "IO") (intTy :| [])
        builtinIOTy = ioTy intTy
    alphaEqBackendType nameOnlyIOTy builtinIOTy `shouldBe` False
    validateBackendExpr
      ( BackendApp
          builtinIOTy
          (BackendVar (BTArrow nameOnlyIOTy builtinIOTy) "f")
          (BackendVar builtinIOTy "x")
      )
      `shouldBe` Left (BackendApplicationArgumentMismatch nameOnlyIOTy builtinIOTy)

  it "checks structural constructor result payloads against metadata" $ do
    alphaEqBackendType boxTy structuralBoxTy
      `shouldBe` False

    validateBackendProgram (programWithMainExpr structuralBoxConstructExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithMainExpr mismatchedStructuralBoxConstructExpr)
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy mismatchedStructuralBoxTy)

    validateBackendProgram (programWithMainExpr graphIdentityStructuralBoxConstructExpr)
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy graphIdentityStructuralBoxTy)

    validateBackendProgram mismatchedStructuralBoxCaseProgram
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "Box" mismatchedStructuralBoxTy boxTy)

  it "preserves nominal arguments when structural recursive payloads omit them" $ do
    alphaEqBackendType
      (BTCon (BaseTy "Core.Phantom") (intTy :| []))
      (BTMu "$Core.Phantom_self" (BTForall "r" Nothing (BTVar "r")))
      `shouldBe` False

    alphaEqBackendType
      (BTCon (BaseTy "Core.Phantom") (BTVar "a" :| []))
      (BTMu "$Core.Phantom_self" (BTForall "r" Nothing (BTVar "r")))
      `shouldBe` True

  it "recovers structural data arguments in declared parameter order" $ do
    validateBackendProgram (programWithDataAndMainExpr [outOfOrderStructuralData] outOfOrderStructuralConstructExpr)
      `shouldBe` Right ()

  it "rejects recursive roll and unroll type mismatches" $ do
    let recTy = BTMu "self" intTy

    validateBackendExpr (BackendRoll recTy (boolLit True))
      `shouldBe` Left (BackendRollPayloadMismatch intTy boolTy)

    validateBackendExpr (BackendUnroll boolTy (BackendVar recTy "boxed"))
      `shouldBe` Left (BackendUnrollResultMismatch boolTy intTy)

  it "unfolds recursive types by identity when names collide" $ do
    let selfIdentity = typeBinderIdentityFromNode (NodeId 991209)
        otherIdentity = typeBinderIdentityFromNode (NodeId 991210)
        otherVar = BTVarWithIdentity (Just otherIdentity) "self"
        recTy = BTMuWithIdentity (Just selfIdentity) "self" (BTArrow (BTVarWithIdentity (Just selfIdentity) "stale") otherVar)

    unfoldBackendRecursiveType recTy
      `shouldBe` Just (BTArrow recTy otherVar)

  it "compares vacuous recursive wrappers by their bodies" $ do
    validateBackendProgram vacuousRecursiveVariableMismatchProgram
      `shouldBe` Left (BackendVariableTypeMismatch "x" vacuousRecursiveIntTy vacuousRecursiveBoolTy)

    validateBackendProgram oneSidedVacuousRecursiveMismatchProgram
      `shouldBe` Left (BackendVariableTypeMismatch "x" recursiveArrowIntTy vacuousRecursiveBoolTy)

    validateBackendProgram vacuousRecursiveConstructorMismatchProgram
      `shouldBe` Left
        (BackendConstructorArgumentMismatch "VacuousMuBox" 0 vacuousRecursiveIntTy vacuousRecursiveBoolTy)

  it "unwraps vacuous recursive bodies with identity-bound stale names" $ do
    let bodyIdentity = typeBinderIdentityFromNode (NodeId 991211)
        selfIdentity = typeBinderIdentityFromNode (NodeId 991212)
        bodyTy =
          BTForallWithIdentity
            (Just bodyIdentity)
            "a"
            Nothing
            (BTVarWithIdentity (Just bodyIdentity) "stale")
        wrapperTy =
          BTMuWithIdentity
            (Just selfIdentity)
            "self"
            bodyTy

    validateBackendProgram
      ( programWithBindings
          [ mainBinding
              BackendLamWithIdentity
                { backendParamIdentity = Nothing,
                  backendExprType = BTArrow wrapperTy bodyTy,
                  backendParamName = "x",
                  backendParamType = wrapperTy,
                  backendBody = BackendVar bodyTy "x"
                }
          ]
      )
      `shouldBe` Right ()

  it "accepts ADT construction and case analysis through constructor metadata" $ do
    validateBackendProgram (programWithMainExpr boxCaseExpr)
      `shouldBe` Right ()

  it "accepts parameterized constructor metadata at use and case sites" $ do
    validateBackendProgram (programWithDataAndMainExpr [optionData] someIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] optionCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] someIntAsOptionVarExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [optionData] someBoolAsOptionIntExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "Some" 0 intTy boolTy)

  it "uses data parameter identities when backend parameter names collide" $ do
    validateBackendProgram (programWithDataAndMainExpr [identityPairData] identityPairExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [identityPairData] swappedIdentityPairExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "IdentityPair" 0 identityPairIntTy identityPairBoolTy)

  it "does not capture identity-bearing field placeholders by name-only data parameters" $ do
    validateBackendProgram identityPlaceholderFieldProgram
      `shouldBe` Right ()

  it "uses identity-bearing constructor result placeholders when display names collide" $ do
    validateBackendProgram (programWithDataAndMainExpr [resultPlaceholderData] resultPlaceholderConstructExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [resultPlaceholderData] resultPlaceholderCaseExpr)
      `shouldBe` Right ()

  it "rejects constructor result types named only by encoded data identity" $ do
    validateBackendProgram dataIdentityConstructorResultProgram
      `shouldBe` Left (BackendConstructorResultMismatch "IdentityBox" dataIdentityBoxCanonicalTy dataIdentityBoxStableTy)

  it "rejects constructor result types named only by identity-bearing data display name" $ do
    validateBackendProgram dataIdentityConstructorDisplayResultProgram
      `shouldBe` Left (BackendConstructorResultMismatch "IdentityBox" dataIdentityBoxCanonicalTy dataIdentityBoxNameOnlyTy)

  it "rejects name-only structural boundaries for identity-bearing data" $ do
    validateBackendProgram dataIdentityStructuralNameOnlyBoundaryProgram
      `shouldBe` Left (BackendBindingTypeMismatch "main" dataIdentityBoxNameOnlyFunctionTy dataIdentityBoxStructuralFunctionTy)

  it "rejects structural self identities that point away from same-named data" $ do
    validateBackendProgram dataIdentityStructuralMismatchedSelfProgram
      `shouldBe` Left (BackendBindingTypeMismatch "main" dataIdentityBoxCanonicalFunctionTy dataIdentityBoxMismatchedStructuralFunctionTy)

  it "does not match structural data declarations by name when self identity differs" $
    structuralDataDeclarationMatches Map.empty dataIdentityBoxData Map.empty dataIdentityBoxMismatchedStructuralTy
      `shouldBe` False

  it "does not match identity-bearing nominal data to name-only structural data" $
    alphaEqBackendType dataIdentityBoxCanonicalTy dataIdentityBoxStructuralTy
      `shouldBe` False

  it "does not treat name-only self fields as identity-bearing structural self occurrences" $ do
    alphaEqBackendType dataIdentityRecursiveBoxCanonicalTy dataIdentityRecursiveBoxNameOnlySelfTy
      `shouldBe` False
    structuralDataDeclarationMatches Map.empty dataIdentityRecursiveBoxData Map.empty dataIdentityRecursiveBoxNameOnlySelfTy
      `shouldBe` False

  it "accepts stale constructor result type heads when data identity is carried" $ do
    validateBackendProgram dataIdentityConstructorStaleResultProgram
      `shouldBe` Right ()

  it "rejects stale constructor result type heads when data identities differ" $ do
    validateBackendProgram dataIdentityConstructorMismatchedResultProgram
      `shouldBe` Left (BackendConstructorResultMismatch "IdentityBox" dataIdentityBoxCanonicalTy dataIdentityBoxMismatchedStableTy)

  it "compares backend type heads by carried identity" $ do
    alphaEqBackendType
      (BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Left"))
      (BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Right"))
      `shouldBe` True
    BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Left")
      `shouldBe` BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Right")

    alphaEqBackendType
      (BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox"))
      (BTBaseWithIdentity (Just duplicateDataIdentity) (BaseTy "IdentityBox"))
      `shouldBe` False
    BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox")
      `shouldNotBe` BTBaseWithIdentity (Just duplicateDataIdentity) (BaseTy "IdentityBox")

    alphaEqBackendType
      (BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Left") (intTy :| []))
      (BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Right") (intTy :| []))
      `shouldBe` True
    BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Left") (intTy :| [])
      `shouldBe` BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.Right") (intTy :| [])

  it "does not match identity-bearing backend heads through name-only fallback" $ do
    backendTypeHeadMatches
      (Just dataIdentityBoxIdentity)
      (BaseTy "IdentityBox")
      Nothing
      (BaseTy "IdentityBox")
      `shouldBe` False
    backendTypeHeadMatches
      Nothing
      (BaseTy "IdentityBox")
      (Just dataIdentityBoxIdentity)
      (BaseTy "IdentityBox")
      `shouldBe` False

  it "does not match identity-bearing type variables through name-only metadata bounds" $ do
    let binderIdentity = typeBinderIdentityFromNode (NodeId 991337)
        identityVar = BTVarWithIdentity (Just binderIdentity) "a"
        nameOnlyVar = BTVar "a"
        expr =
          BackendTyAbsWithIdentity
            { backendExprType = BTForallWithIdentity (Just binderIdentity) "a" Nothing (BTArrow identityVar identityVar),
              backendTyParamIdentity = Just binderIdentity,
              backendTyParamName = "a",
              backendTyParamBound = Nothing,
              backendTyAbsBody =
                BackendLamWithIdentity
                  { backendExprType = BTArrow identityVar identityVar,
                    backendParamIdentity = Nothing,
                    backendParamName = "x",
                    backendParamType = identityVar,
                    backendBody = BackendVar nameOnlyVar "x"
                  }
            }
    validateBackendExpr expr
      `shouldBe` Left (BackendLambdaTypeMismatch (BTArrow identityVar identityVar) (BTArrow identityVar nameOnlyVar))

  it "does not match identity-bearing primitive heads through normalized name fallback" $ do
    let nameOnlyIntTy = BTBaseWithIdentity Nothing (BaseTy "Int")
        expr =
          BackendApp
            nameOnlyIntTy
            (BackendVar (BTArrow intTy nameOnlyIntTy) "f")
            (BackendVar nameOnlyIntTy "x")

    validateBackendExpr expr
      `shouldBe` Left (BackendApplicationArgumentMismatch intTy nameOnlyIntTy)

  it "substitutes applied type variables in constructor metadata" $ do
    substituteBackendTypesByName
      (Map.fromList [("f", BTBase (BaseTy "BoxF")), ("a", boolTy)])
      (BTVarApp "f" (BTVar "a" :| []))
      `shouldBe` boxFTy boolTy

    let identityCon = BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox") (BTVar "a" :| [])
    substituteBackendTypesByName Map.empty identityCon `shouldBe` identityCon

    substituteBackendTypesByName
      (Map.fromList [("f", BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox"))])
      (BTVarApp "f" (intTy :| []))
      `shouldBe` BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox") (intTy :| [])

    validateBackendProgram (programWithDataAndMainExpr [boxFData, maybeFData] justFBoxBoolExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boxFData, maybeFData] maybeFCaseExpr)
      `shouldBe` Right ()

  it "uses constructor-level forall metadata when validating constructor fields" $ do
    validateBackendProgram (programWithDataAndMainExpr [packData] packIntExpr)
      `shouldBe` Right ()

  it "enforces constructor-level forall bounds at construct and case boundaries" $ do
    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackBoolExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "BoundedPack" 0 intTy boolTy)

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackRepackCaseExpr)
      `shouldBe` Right ()

    validateBackendProgram boundedPackOuterNameCollisionCaseProgram
      `shouldBe` Right ()

    validateBackendProgram boundedPackOuterNameCollisionWrongOuterUseProgram
      `shouldBe` Left (BackendVariableTypeMismatch "outer" (BTVar "a") intTy)

    validateBackendProgram (programWithDataAndMainExpr [boundedPackData] boundedPackWrongBoundUseCaseExpr)
      `shouldBe` Left (BackendVariableTypeMismatch "n" intTy boolTy)

    validateBackendProgram boundedListPackCaseProgram
      `shouldBe` Right ()

    validateBackendProgram boundedListPackWrongBoundUseCaseProgram
      `shouldBe` Left (BackendVariableTypeMismatch "n" (listTy intTy) (listTy boolTy))

    validateBackendProgram (programWithDataAndMainExpr [dependentBoundedPackData] dependentBoundedPackIntExpr)
      `shouldBe` Right ()

    validateBackendProgram (programWithDataAndMainExpr [dependentBoundedPackData] dependentBoundedPackBoolExpr)
      `shouldBe` Left (BackendConstructorArgumentMismatch "DependentBoundedPack" 0 intTy boolTy)

    validateBackendProgram dependentActualBoundPackProgram
      `shouldBe` Right ()

    validateBackendProgram dependentActualBoundPackWrongProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "DependentActualBoundPack" 0 (listTy intTy) (BTVar "b"))

  it "rejects matcher capture from inferred constructor parameters" $ do
    validateBackendProgram captureForallConstructProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "CaptureForall" 0 captureForallInstantiatedTy captureForallActualTy)

    validateBackendProgram captureMuConstructProgram
      `shouldBe` Left (BackendConstructorArgumentMismatch "CaptureMu" 0 captureMuInstantiatedTy captureMuActualTy)

    validateBackendProgram captureCaseProgram
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "CaptureCase" captureCaseScrutineeTy captureCaseTemplateTy)

  it "rejects unknown constructors and duplicate constructor metadata" $ do
    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Missing" []))
      `shouldBe` Left (BackendUnknownConstructor "Missing")

    validateBackendProgram mismatchedConstructorIdentityProgram
      `shouldBe` Left (BackendUnknownConstructor "Box")

    validateBackendProgram conflictingConstructorIdentityReferenceProgram
      `shouldBe` Left (BackendUnknownConstructor "Box")

    validateBackendProgram identityConstructorReferencedByNameOnlyProgram
      `shouldBe` Left (BackendUnknownConstructor "Box")

    validateBackendProgram identityConstructorReferencedByStableNameProgram
      `shouldBe` Left (BackendUnknownConstructor (symbolIdentityStableName duplicateConstructorIdentity))

    validateBackendProgram
      ( BackendProgram
          [ BackendModule
              { backendModuleName = "Main",
                backendModuleData =
                  [ BackendData "LeftBox" [] [BackendConstructor "Box" [] [intTy] boxTy],
                    BackendData "RightBox" [] [BackendConstructor "Box" [] [intTy] boxTy]
                  ],
                backendModuleBindings = [mainLiteralBinding]
              }
          ]
          "main"
      )
      `shouldBe` Left (BackendDuplicateConstructor "Box")

    validateBackendProgram duplicateConstructorIdentityProgram
      `shouldBe` Left (BackendDuplicateConstructor (symbolIdentityStableName duplicateConstructorIdentity))

  it "rejects constructor arity, argument, and result mismatches" $ do
    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Box" []))
      `shouldBe` Left (BackendConstructorArityMismatch "Box" 1 0)

    validateBackendProgram (programWithMainExpr (BackendConstruct boxTy "Box" [boolLit True]))
      `shouldBe` Left (BackendConstructorArgumentMismatch "Box" 0 intTy boolTy)

    validateBackendProgram (programWithMainExpr (BackendConstruct boolTy "Box" [intLit 1]))
      `shouldBe` Left (BackendConstructorResultMismatch "Box" boxTy boolTy)

  it "rejects invalid case alternative result and constructor boundaries" $ do
    validateBackendProgram (programWithMainExpr boxCaseWrongResultExpr)
      `shouldBe` Left (BackendCaseResultMismatch intTy boolTy)

    validateBackendProgram (programWithMainExpr boxCaseWrongScrutineeExpr)
      `shouldBe` Left (BackendCaseConstructorScrutineeMismatch "Box" boolTy boxTy)

    validateBackendProgram (programWithMainExpr boxCaseWrongPatternArityExpr)
      `shouldBe` Left (BackendPatternArityMismatch "Box" 1 0)

simpleProgram :: BackendProgram
simpleProgram =
  programWithBindings
    [ binding "id" idTy intIdentityExpr,
      mainBinding
        ( BackendApp
            { backendExprType = intTy,
              backendFunction = BackendVar idTy "id",
              backendArgument = intLit 1
            }
        )
    ]

programWithMainExpr :: BackendExpr -> BackendProgram
programWithMainExpr expr =
  programWithDataAndMainExpr [boxData] expr

programWithDataAndMainExpr :: [BackendData] -> BackendExpr -> BackendProgram
programWithDataAndMainExpr dataDecls expr =
  programWithDataAndBindings dataDecls [mainBinding expr]

programWithDataAndBindings :: [BackendData] -> [BackendBinding] -> BackendProgram
programWithDataAndBindings dataDecls bindings =
  BackendProgram
    [ BackendModule
        { backendModuleName = "Main",
          backendModuleData = dataDecls,
          backendModuleBindings = bindings
        }
    ]
    "main"

programWithBindings :: [BackendBinding] -> BackendProgram
programWithBindings bindings =
  BackendProgram [moduleWithBindings "Main" bindings] "main"

moduleWithBindings :: String -> [BackendBinding] -> BackendModule
moduleWithBindings name bindings =
  (emptyModule name) {backendModuleBindings = bindings}

emptyModule :: String -> BackendModule
emptyModule name =
  BackendModule
    { backendModuleName = name,
      backendModuleData = [],
      backendModuleBindings = []
    }

mainBinding :: BackendExpr -> BackendBinding
mainBinding expr =
  BackendBinding "main" (backendExprType expr) expr True

mainLiteralBinding :: BackendBinding
mainLiteralBinding =
  mainBinding (intLit 1)

duplicateBindingIdentityProgram :: BackendProgram
duplicateBindingIdentityProgram =
  programWithBindings
    [ bindingWithIdentity "left" duplicateValueIdentity,
      bindingWithIdentity "right" duplicateValueIdentity,
      mainLiteralBinding
    ]

conflictingBindingIdentityPayloadProgram :: BackendProgram
conflictingBindingIdentityPayloadProgram =
  programWithBindings
    [ bindingWithIdentity "left" duplicateValueIdentity,
      bindingWithIdentity "right" conflictingValueIdentity,
      mainLiteralBinding
    ]

mismatchedGlobalBindingIdentityProgram :: BackendProgram
mismatchedGlobalBindingIdentityProgram =
  programWithBindings
    [ bindingWithIdentity "helper" duplicateValueIdentity,
      mainBinding (BackendVarWithIdentity intTy (Just (TopLevelId otherValueIdentity)) "helper")
    ]

conflictingGlobalBindingPayloadProgram :: BackendProgram
conflictingGlobalBindingPayloadProgram =
  programWithBindings
    [ bindingWithIdentity "helper" duplicateValueIdentity,
      mainBinding (BackendVarWithIdentity intTy (Just (TopLevelId conflictingValueIdentity)) "helper")
    ]

identityGlobalReferencedByNameOnlyProgram :: BackendProgram
identityGlobalReferencedByNameOnlyProgram =
  programWithBindings
    [ bindingWithIdentity "helper" duplicateValueIdentity,
      mainBinding (BackendVar intTy "helper")
    ]

identityGlobalReferencedByStableNameProgram :: BackendProgram
identityGlobalReferencedByStableNameProgram =
  programWithBindings
    [ bindingWithIdentity "helper" duplicateValueIdentity,
      mainBinding (BackendVar intTy (symbolIdentityStableName duplicateValueIdentity))
    ]

mismatchedLocalBindingIdentityProgram :: BackendProgram
mismatchedLocalBindingIdentityProgram =
  programWithBindings
    [ binding "helper" intTy (intLit 1),
      mainBinding (BackendVarWithIdentity intTy (Just (localIdentity 991112 "helper")) "helper")
    ]

mismatchedLiftedHelperIdentityProgram :: BackendProgram
mismatchedLiftedHelperIdentityProgram =
  programWithBindings
    [ binding liftedHelperName intTy (intLit 1),
      mainBinding (BackendVarWithIdentity intTy (Just staleLiftedHelperIdentity) liftedHelperName)
    ]

liftedHelperName :: String
liftedHelperName =
  "Main__main$letrec$self$0"

staleLiftedHelperIdentity :: IdDetails
staleLiftedHelperIdentity =
  DeferredId (deferredRefFromIdentity (UniqueIdentity 991113) liftedHelperName)

bindingWithIdentity :: String -> SymbolIdentity -> BackendBinding
bindingWithIdentity name identity =
  BackendBindingWithMetadata
    { backendBindingIdentity = Just identity,
      backendBindingNameWithMetadata = name,
      backendBindingTypeWithMetadata = intTy,
      backendBindingExprWithMetadata = intLit 1,
      backendBindingExportedAsMainWithMetadata = False,
      backendBindingEvidenceParamIndices = mempty
    }

binding :: String -> BackendType -> BackendExpr -> BackendBinding
binding name ty expr =
  BackendBinding name ty expr False

bindingWithMetadata :: String -> SymbolIdentity -> BackendType -> BackendExpr -> BackendBinding
bindingWithMetadata name identity ty expr =
  BackendBindingWithMetadata
    { backendBindingIdentity = Just identity,
      backendBindingNameWithMetadata = name,
      backendBindingTypeWithMetadata = ty,
      backendBindingExprWithMetadata = expr,
      backendBindingExportedAsMainWithMetadata = False,
      backendBindingEvidenceParamIndices = mempty
    }

duplicateValueIdentity :: SymbolIdentity
duplicateValueIdentity =
  testSymbolIdentity 991001 SymbolValue "Main" "value"

duplicateModuleIdentity :: SymbolIdentity
duplicateModuleIdentity =
  testSymbolIdentity 991015 SymbolModule "Main" "Main"

conflictingValueIdentity :: SymbolIdentity
conflictingValueIdentity =
  symbolIdentityFromParts (symbolUniqueIdentity duplicateValueIdentity) SymbolValue "Main" "$stale_value" Nothing

conflictingModuleIdentity :: SymbolIdentity
conflictingModuleIdentity =
  symbolIdentityFromParts (symbolUniqueIdentity duplicateModuleIdentity) SymbolModule "Main" "$stale_Main" Nothing

otherValueIdentity :: SymbolIdentity
otherValueIdentity =
  testSymbolIdentity 991004 SymbolValue "Other" "value"

globalClosureValueIdentity :: SymbolIdentity
globalClosureValueIdentity =
  testSymbolIdentity 991111 SymbolValue "Main" "f"

localXIdentity :: IdDetails
localXIdentity =
  localIdentity 991101 "x"

otherLocalIdentity :: IdDetails
otherLocalIdentity =
  localIdentity 991102 "x"

patternNIdentity :: IdDetails
patternNIdentity =
  localIdentity 991103 "n"

otherPatternNIdentity :: IdDetails
otherPatternNIdentity =
  localIdentity 991104 "n"

outerClosureIdentity :: IdDetails
outerClosureIdentity =
  localIdentity 991105 "f"

innerDirectIdentity :: IdDetails
innerDirectIdentity =
  localIdentity 991106 "f"

outerParamFIdentity :: IdDetails
outerParamFIdentity =
  localIdentity 991110 "f"

outerClosureParamIdentity :: IdDetails
outerClosureParamIdentity =
  localIdentity 991107 "x"

outerPatternFIdentity :: IdDetails
outerPatternFIdentity =
  localIdentity 991108 "f"

innerPatternFIdentity :: IdDetails
innerPatternFIdentity =
  localIdentity 991109 "f"

localIdentity :: Int -> String -> IdDetails
localIdentity unique name =
  LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name)

identityLam :: IdDetails -> String -> IdDetails -> BackendExpr
identityLam binderIdentity occurrenceName occurrenceIdentity =
  BackendLamWithIdentity
    { backendExprType = idTy,
      backendParamIdentity = Just binderIdentity,
      backendParamName = "x",
      backendParamType = intTy,
      backendBody = BackendVarWithIdentity intTy (Just occurrenceIdentity) occurrenceName
    }

identityLamNameOnly :: IdDetails -> String -> BackendExpr
identityLamNameOnly binderIdentity occurrenceName =
  BackendLamWithIdentity
    { backendExprType = idTy,
      backendParamIdentity = Just binderIdentity,
      backendParamName = occurrenceName,
      backendParamType = intTy,
      backendBody = BackendVar intTy occurrenceName
    }

identityPatternCase :: IdDetails -> String -> IdDetails -> BackendExpr
identityPatternCase binderIdentity occurrenceName occurrenceIdentity =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = BackendConstruct boxTy "Box" [intLit 1],
      backendAlternatives =
        BackendAlternative
          ( BackendConstructorPatternWithBinderIdentities
              Nothing
              "Box"
              [BackendPatternBinder (Just binderIdentity) "n"]
          )
          (BackendVarWithIdentity intTy (Just occurrenceIdentity) occurrenceName)
          :| []
    }

identityCapturedClosure :: IdDetails -> String -> IdDetails -> BackendExpr
identityCapturedClosure binderIdentity occurrenceName occurrenceIdentity =
  BackendClosureWithParamIdentities
    { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$identity_capture",
      backendClosureCaptures =
        [BackendClosureCapture (Just binderIdentity) "captured" intTy (intLit 7)],
      backendClosureParamsWithIdentities = backendClosureParams [("x", intTy)],
      backendClosureBody = BackendVarWithIdentity intTy (Just occurrenceIdentity) occurrenceName
    }

identityParamClosure :: IdDetails -> String -> IdDetails -> BackendExpr
identityParamClosure binderIdentity occurrenceName occurrenceIdentity =
  BackendClosureWithParamIdentities
    { backendExprType = idTy,
              backendClosureEntryIdentity = Nothing,
              backendClosureEntryName = "__mlfp_closure$identity_param",
      backendClosureCaptures = [],
      backendClosureParamsWithIdentities =
        [BackendClosureParam (Just binderIdentity) "x" intTy],
      backendClosureBody = BackendVarWithIdentity intTy (Just occurrenceIdentity) occurrenceName
    }

identityShadowedClosureApp :: BackendExpr
identityShadowedClosureApp =
  BackendLetWithIdentity
    { backendExprType = intTy,
      backendLetIdentity = Just outerClosureIdentity,
      backendLetName = "f",
      backendLetType = idTy,
      backendLetRhs =
        identityParamClosure outerClosureParamIdentity "x" outerClosureParamIdentity,
      backendLetBody =
        BackendLetWithIdentity
          { backendExprType = intTy,
            backendLetIdentity = Just innerDirectIdentity,
            backendLetName = "f",
            backendLetType = idTy,
            backendLetRhs = intIdentityExpr,
            backendLetBody =
              BackendApp
                intTy
                (BackendVarWithIdentity idTy (Just outerClosureIdentity) "f")
                (intLit 1)
          }
    }

identityShadowedClosureAppByNameOnlyBinder :: BackendExpr
identityShadowedClosureAppByNameOnlyBinder =
  BackendLetWithIdentity
    { backendExprType = intTy,
      backendLetIdentity = Just outerClosureIdentity,
      backendLetName = "f",
      backendLetType = idTy,
      backendLetRhs =
        identityParamClosure outerClosureParamIdentity "x" outerClosureParamIdentity,
      backendLetBody =
        BackendLetWithIdentity
          { backendExprType = intTy,
            backendLetIdentity = Nothing,
            backendLetName = "f",
            backendLetType = idTy,
            backendLetRhs = intIdentityExpr,
            backendLetBody =
              BackendApp
                intTy
                (BackendVarWithIdentity idTy (Just outerClosureIdentity) "f")
                (intLit 1)
          }
    }

identityShadowedPatternClosureApp :: BackendExpr
identityShadowedPatternClosureApp =
  BackendApp
    intTy
    ( BackendCase
        idTy
        (BackendConstruct fnBoxTy "FnBox" [identityParamClosure outerClosureParamIdentity "x" outerClosureParamIdentity])
        ( BackendAlternative
            ( BackendConstructorPatternWithBinderIdentities
                Nothing
                "FnBox"
                [BackendPatternBinder (Just outerPatternFIdentity) "f"]
            )
            ( BackendCase
                idTy
                (BackendConstruct boxTy "Box" [intLit 1])
                ( BackendAlternative
                    ( BackendConstructorPatternWithBinderIdentities
                        Nothing
                        "Box"
                        [BackendPatternBinder (Just innerPatternFIdentity) "f"]
                    )
                    (BackendVarWithIdentity idTy (Just outerPatternFIdentity) "f")
                    :| []
                )
            )
            :| []
        )
    )
    (intLit 1)

identityPatternFallbackClosureProgram :: BackendProgram
identityPatternFallbackClosureProgram =
  programWithDataAndBindings
    [boxData]
    [ bindingWithMetadata
        "f"
        globalClosureValueIdentity
        idTy
        (identityParamClosure outerClosureParamIdentity "x" outerClosureParamIdentity),
      mainBinding
        ( BackendLetWithIdentity
            { backendExprType = intTy,
              backendLetIdentity = Just innerDirectIdentity,
              backendLetName = "f",
              backendLetType = idTy,
              backendLetRhs = intIdentityExpr,
              backendLetBody =
                BackendApp
                  intTy
                  ( BackendCase
                      idTy
                      (BackendConstruct boxTy "Box" [intLit 1])
                      ( BackendAlternative
                          ( BackendConstructorPatternWithBinderIdentities
                              Nothing
                              "Box"
                              [BackendPatternBinder (Just innerPatternFIdentity) "f"]
                          )
                          (BackendVarWithIdentity idTy (Just (TopLevelId globalClosureValueIdentity)) "f")
                          :| []
                      )
                  )
                  (intLit 1)
            }
        )
    ]

identityShadowedClosureParam :: BackendExpr
identityShadowedClosureParam =
  BackendLamWithIdentity
    { backendExprType = BTArrow idTy intTy,
      backendParamIdentity = Just outerParamFIdentity,
      backendParamName = "f",
      backendParamType = idTy,
      backendBody =
        BackendLetWithIdentity
          { backendExprType = intTy,
            backendLetIdentity = Just innerDirectIdentity,
            backendLetName = "f",
            backendLetType = idTy,
            backendLetRhs = intIdentityExpr,
            backendLetBody =
              BackendClosureCall
                intTy
                (BackendVarWithIdentity idTy (Just outerParamFIdentity) "f")
                [intLit 1]
          }
    }

duplicateDataIdentity :: SymbolIdentity
duplicateDataIdentity =
  testSymbolIdentity 991000 SymbolType "Main" "Data"

conflictingDataIdentity :: SymbolIdentity
conflictingDataIdentity =
  symbolIdentityFromParts (symbolUniqueIdentity duplicateDataIdentity) SymbolType "Main" "$stale_Data" Nothing

preludeOptionIdentity :: SymbolIdentity
preludeOptionIdentity =
  testSymbolIdentity 991622 SymbolType "Prelude" "Option"

duplicateConstructorIdentity :: SymbolIdentity
duplicateConstructorIdentity =
  testSymbolIdentity 991002 SymbolConstructor "Main" "Box"

conflictingConstructorIdentity :: SymbolIdentity
conflictingConstructorIdentity =
  symbolIdentityFromParts (symbolUniqueIdentity duplicateConstructorIdentity) SymbolConstructor "Main" "$stale_Box" Nothing

otherConstructorIdentity :: SymbolIdentity
otherConstructorIdentity =
  testSymbolIdentity 991005 SymbolConstructor "Other" "Box"

dataIdentityBoxIdentity :: SymbolIdentity
dataIdentityBoxIdentity =
  testSymbolIdentity 991003 SymbolType "Main" "IdentityBox"

intIdentityExpr :: BackendExpr
intIdentityExpr =
  BackendLamWithIdentity
    { backendParamIdentity = Nothing, backendExprType = idTy,
      backendParamName = "x",
      backendParamType = intTy,
      backendBody = BackendVar intTy "x"
    }

letIdentityExpr :: BackendExpr
letIdentityExpr =
  BackendLetWithIdentity
    { backendLetIdentity = Nothing, backendExprType = intTy,
      backendLetName = "x",
      backendLetType = intTy,
      backendLetRhs = intLit 1,
      backendLetBody = BackendVar intTy "x"
    }

boxData :: BackendData
boxData =
  BackendData
    { backendDataName = "Box",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "Box" [] [intTy] boxTy]
    }

duplicateDataIdentityProgram :: BackendProgram
duplicateDataIdentityProgram =
  programWithDataAndMainExpr
    [ dataWithIdentity "LeftBox" duplicateDataIdentity,
      dataWithIdentity "RightBox" duplicateDataIdentity
    ]
    (intLit 1)

conflictingDataIdentityPayloadProgram :: BackendProgram
conflictingDataIdentityPayloadProgram =
  programWithDataAndMainExpr
    [ dataWithIdentity "LeftBox" duplicateDataIdentity,
      dataWithIdentity "RightBox" conflictingDataIdentity
    ]
    (intLit 1)

duplicateModuleIdentityProgram :: BackendProgram
duplicateModuleIdentityProgram =
  BackendProgram
    [ moduleWithIdentity "Main" duplicateModuleIdentity,
      moduleWithIdentity "$stale_Main" duplicateModuleIdentity
    ]
    "main"

conflictingModuleIdentityPayloadProgram :: BackendProgram
conflictingModuleIdentityPayloadProgram =
  BackendProgram
    [ moduleWithIdentity "Main" duplicateModuleIdentity,
      moduleWithIdentity "$stale_Main" conflictingModuleIdentity
    ]
    "main"

moduleWithIdentity :: String -> SymbolIdentity -> BackendModule
moduleWithIdentity name identity =
  BackendModuleWithIdentity
    { backendModuleIdentity = Just identity,
      backendModuleNameWithIdentity = name,
      backendModuleDataWithIdentity = [],
      backendModuleBindingsWithIdentity = []
    }

identityMainProgram :: BackendProgram
identityMainProgram =
  BackendProgramWithIdentity
    { backendProgramModulesWithIdentity =
        [ moduleWithBindings
            "Main"
            [bindingWithMetadata "actual-main" duplicateValueIdentity intTy (intLit 1)]
        ],
      backendProgramMainIdentity = Just duplicateValueIdentity,
      backendProgramMainWithIdentity = "$stale-main"
    }

conflictingIdentityMainProgram :: BackendProgram
conflictingIdentityMainProgram =
  identityMainProgram {backendProgramMainIdentity = Just conflictingValueIdentity}

identityMainNameOnlyProgram :: BackendProgram
identityMainNameOnlyProgram =
  BackendProgram
    [ moduleWithBindings
        "Main"
        [bindingWithMetadata "actual-main" duplicateValueIdentity intTy (intLit 1)]
    ]
    "actual-main"

identityMainStableNameProgram :: BackendProgram
identityMainStableNameProgram =
  BackendProgram
    [ moduleWithBindings
        "Main"
        [bindingWithMetadata "actual-main" duplicateValueIdentity intTy (intLit 1)]
    ]
    (symbolIdentityStableName duplicateValueIdentity)

dataWithIdentity :: String -> SymbolIdentity -> BackendData
dataWithIdentity name identity =
  BackendDataWithIdentity
    { backendDataIdentity = Just identity,
      backendDataNameWithIdentity = name,
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity = []
    }

identityDataWithNameOnlyParameterProgram :: BackendProgram
identityDataWithNameOnlyParameterProgram =
  BackendProgram
    [ BackendModule
        "Main"
        [ BackendDataWithIdentity
            { backendDataIdentity = Just dataIdentityBoxIdentity,
              backendDataNameWithIdentity = "NamedBox",
              backendDataParameterRefsWithIdentity = [legacyDataParameterRef "a"],
              backendDataConstructorsWithIdentity = []
            }
        ]
        [mainLiteralBinding]
    ]
    "main"

legacyDataParameterRef :: String -> BackendDataParameterRef
legacyDataParameterRef name =
  case backendDataParameterRefs (BackendData "__Legacy" [name] []) of
    [ref] -> ref
    _ -> error "expected one legacy data parameter ref"

identityDataWithNameOnlyConstructorParameterProgram :: BackendProgram
identityDataWithNameOnlyConstructorParameterProgram =
  BackendProgram
    [ BackendModule
        "Main"
        [ BackendDataWithIdentity
            { backendDataIdentity = Just dataIdentityBoxIdentity,
              backendDataNameWithIdentity = "NamedBox",
              backendDataParameterRefsWithIdentity = [backendDataParameterRefFromIdentity dataIdentityBoxParamIdentity "a"],
              backendDataConstructorsWithIdentity =
                [ BackendConstructor
                    "NamedBox"
                    []
                    [BTVar "a"]
                    (BTConWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "NamedBox") (BTVarWithIdentity (Just dataIdentityBoxParamIdentity) "a" :| []))
                ]
            }
        ]
        [mainLiteralBinding]
    ]
    "main"

dataIdentityBoxParamIdentity :: TypeBinderIdentity
dataIdentityBoxParamIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 991750)

identityDataWithNameOnlyConstructorForallProgram :: BackendProgram
identityDataWithNameOnlyConstructorForallProgram =
  BackendProgram
    [ BackendModule
        "Main"
        [ BackendDataWithIdentity
            { backendDataIdentity = Just dataIdentityBoxIdentity,
              backendDataNameWithIdentity = "NamedBox",
              backendDataParameterRefsWithIdentity = [],
              backendDataConstructorsWithIdentity =
                [ BackendConstructor
                    "NamedBox"
                    [BackendTypeBinder "a" Nothing]
                    []
                    (BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "NamedBox"))
                ]
            }
        ]
        [mainLiteralBinding]
    ]
    "main"

duplicateConstructorIdentityProgram :: BackendProgram
duplicateConstructorIdentityProgram =
  programWithDataAndMainExpr
    [ BackendData "LeftBox" [] [constructorWithIdentity "LeftBox" duplicateConstructorIdentity],
      BackendData "RightBox" [] [constructorWithIdentity "RightBox" duplicateConstructorIdentity]
    ]
    (intLit 1)

conflictingConstructorIdentityPayloadProgram :: BackendProgram
conflictingConstructorIdentityPayloadProgram =
  programWithDataAndMainExpr
    [ BackendData "LeftBox" [] [constructorWithIdentity "LeftBox" duplicateConstructorIdentity],
      BackendData "RightBox" [] [constructorWithIdentity "RightBox" conflictingConstructorIdentity]
    ]
    (intLit 1)

mismatchedConstructorIdentityProgram :: BackendProgram
mismatchedConstructorIdentityProgram =
  programWithDataAndMainExpr
    [BackendData "Box" [] [constructorWithIdentity "Box" duplicateConstructorIdentity]]
    (BackendConstructWithIdentity boxTy (Just otherConstructorIdentity) "Box" [intLit 1])

conflictingConstructorIdentityReferenceProgram :: BackendProgram
conflictingConstructorIdentityReferenceProgram =
  programWithDataAndMainExpr
    [BackendData "Box" [] [constructorWithIdentity "Box" duplicateConstructorIdentity]]
    (BackendConstructWithIdentity boxTy (Just conflictingConstructorIdentity) "Box" [intLit 1])

identityConstructorReferencedByNameOnlyProgram :: BackendProgram
identityConstructorReferencedByNameOnlyProgram =
  programWithDataAndMainExpr
    [BackendData "Box" [] [constructorWithIdentity "Box" duplicateConstructorIdentity]]
    (BackendConstruct boxTy "Box" [intLit 1])

identityConstructorReferencedByStableNameProgram :: BackendProgram
identityConstructorReferencedByStableNameProgram =
  programWithDataAndMainExpr
    [BackendData "Box" [] [constructorWithIdentity "Box" duplicateConstructorIdentity]]
    (BackendConstruct boxTy (symbolIdentityStableName duplicateConstructorIdentity) [intLit 1])

constructorWithIdentity :: String -> SymbolIdentity -> BackendConstructor
constructorWithIdentity name identity =
  BackendConstructorWithIdentity
    { backendConstructorIdentity = Just identity,
      backendConstructorNameWithIdentity = name,
      backendConstructorForallsWithIdentity = [],
      backendConstructorFieldsWithIdentity = [intTy],
      backendConstructorResultWithIdentity = BTBase (BaseTy name)
    }

dataIdentityConstructorResultProgram :: BackendProgram
dataIdentityConstructorResultProgram =
  programWithDataAndMainExpr
    [dataIdentityBoxData]
    (BackendConstruct dataIdentityBoxStableTy "IdentityBox" [])

dataIdentityConstructorDisplayResultProgram :: BackendProgram
dataIdentityConstructorDisplayResultProgram =
  programWithDataAndMainExpr
    [dataIdentityBoxData]
    (BackendConstruct dataIdentityBoxNameOnlyTy "IdentityBox" [])

dataIdentityConstructorStaleResultProgram :: BackendProgram
dataIdentityConstructorStaleResultProgram =
  programWithDataAndMainExpr
    [dataIdentityBoxData]
    (BackendConstruct dataIdentityBoxStaleTy "IdentityBox" [])

dataIdentityConstructorMismatchedResultProgram :: BackendProgram
dataIdentityConstructorMismatchedResultProgram =
  programWithDataAndMainExpr
    [dataIdentityBoxData]
    (BackendConstruct dataIdentityBoxMismatchedStableTy "IdentityBox" [])

dataIdentityStructuralNameOnlyBoundaryProgram :: BackendProgram
dataIdentityStructuralNameOnlyBoundaryProgram =
  programWithDataAndBindings
    [dataIdentityBoxData]
    [ BackendBinding
        { backendBindingName = "main",
          backendBindingType = dataIdentityBoxNameOnlyFunctionTy,
          backendBindingExpr =
            BackendLam
              dataIdentityBoxStructuralFunctionTy
              "x"
              dataIdentityBoxStructuralTy
              (BackendVar dataIdentityBoxStructuralTy "x"),
          backendBindingExportedAsMain = True
        }
    ]

dataIdentityStructuralMismatchedSelfProgram :: BackendProgram
dataIdentityStructuralMismatchedSelfProgram =
  programWithDataAndBindings
    [dataIdentityBoxData]
    [ BackendBinding
        { backendBindingName = "main",
          backendBindingType = dataIdentityBoxCanonicalFunctionTy,
          backendBindingExpr =
            BackendLam
              dataIdentityBoxMismatchedStructuralFunctionTy
              "x"
              dataIdentityBoxMismatchedStructuralTy
              (BackendVar dataIdentityBoxMismatchedStructuralTy "x"),
          backendBindingExportedAsMain = True
        }
    ]

dataIdentityBoxData :: BackendData
dataIdentityBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = Just dataIdentityBoxIdentity,
      backendDataNameWithIdentity = "IdentityBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity = [BackendConstructor "IdentityBox" [] [] dataIdentityBoxCanonicalTy]
    }

dataIdentityBoxNameOnlyTy :: BackendType
dataIdentityBoxNameOnlyTy =
  BTBase (BaseTy "IdentityBox")

dataIdentityBoxStructuralTy :: BackendType
dataIdentityBoxStructuralTy =
  BTMu "$IdentityBox_self" nullaryStructuralBody

dataIdentityBoxMismatchedStructuralTy :: BackendType
dataIdentityBoxMismatchedStructuralTy =
  BTMuWithIdentity (Just dataIdentityBoxOtherSelfIdentity) "$IdentityBox_self" nullaryStructuralBody

dataIdentityBoxNameOnlyFunctionTy :: BackendType
dataIdentityBoxNameOnlyFunctionTy =
  BTArrow dataIdentityBoxNameOnlyTy dataIdentityBoxNameOnlyTy

dataIdentityBoxCanonicalFunctionTy :: BackendType
dataIdentityBoxCanonicalFunctionTy =
  BTArrow dataIdentityBoxCanonicalTy dataIdentityBoxCanonicalTy

dataIdentityBoxStructuralFunctionTy :: BackendType
dataIdentityBoxStructuralFunctionTy =
  BTArrow dataIdentityBoxStructuralTy dataIdentityBoxStructuralTy

dataIdentityBoxMismatchedStructuralFunctionTy :: BackendType
dataIdentityBoxMismatchedStructuralFunctionTy =
  BTArrow dataIdentityBoxMismatchedStructuralTy dataIdentityBoxMismatchedStructuralTy

dataIdentityBoxStableTy :: BackendType
dataIdentityBoxStableTy =
  BTBase (BaseTy (symbolIdentityStableName dataIdentityBoxIdentity))

dataIdentityBoxCanonicalTy :: BackendType
dataIdentityBoxCanonicalTy =
  BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "IdentityBox")

dataIdentityBoxStaleTy :: BackendType
dataIdentityBoxStaleTy =
  BTBaseWithIdentity (Just dataIdentityBoxIdentity) (BaseTy "stale.IdentityBox")

dataIdentityBoxMismatchedStableTy :: BackendType
dataIdentityBoxMismatchedStableTy =
  BTBaseWithIdentity (Just otherDataIdentityBoxIdentity) (BaseTy (symbolIdentityStableName dataIdentityBoxIdentity))

otherDataIdentityBoxIdentity :: SymbolIdentity
otherDataIdentityBoxIdentity =
  testSymbolIdentity 991006 SymbolType "Other" "IdentityBox"

dataIdentityBoxOtherSelfIdentity :: TypeBinderIdentity
dataIdentityBoxOtherSelfIdentity =
  typeBinderIdentityFromStructural (UniqueIdentity 991700) StructuralSelfBinder

dataIdentityRecursiveBoxIdentity :: SymbolIdentity
dataIdentityRecursiveBoxIdentity =
  testSymbolIdentity 991007 SymbolType "Main" "IdentityRecursiveBox"

dataIdentityRecursiveBoxData :: BackendData
dataIdentityRecursiveBoxData =
  BackendDataWithIdentity
    { backendDataIdentity = Just dataIdentityRecursiveBoxIdentity,
      backendDataNameWithIdentity = "IdentityRecursiveBox",
      backendDataParameterRefsWithIdentity = [],
      backendDataConstructorsWithIdentity =
        [BackendConstructor "IdentityRecursiveBox" [] [dataIdentityRecursiveBoxCanonicalTy] dataIdentityRecursiveBoxCanonicalTy]
    }

dataIdentityRecursiveBoxCanonicalTy :: BackendType
dataIdentityRecursiveBoxCanonicalTy =
  BTBaseWithIdentity (Just dataIdentityRecursiveBoxIdentity) (BaseTy "IdentityRecursiveBox")

dataIdentityRecursiveBoxSelfIdentity :: TypeBinderIdentity
dataIdentityRecursiveBoxSelfIdentity =
  typeBinderIdentityFromStructural (symbolUniqueIdentity dataIdentityRecursiveBoxIdentity) StructuralSelfBinder

dataIdentityRecursiveBoxNameOnlySelfTy :: BackendType
dataIdentityRecursiveBoxNameOnlySelfTy =
  BTMuWithIdentity
    (Just dataIdentityRecursiveBoxSelfIdentity)
    "$IdentityRecursiveBox_self"
    (singleFieldStructuralBody (BTVar "$IdentityRecursiveBox_self"))

fnBoxData :: BackendData
fnBoxData =
  BackendData
    { backendDataName = "FnBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "FnBox" [] [idTy] fnBoxTy]
    }

optionData :: BackendData
optionData =
  BackendDataWithIdentity
    { backendDataIdentity = Nothing,
      backendDataNameWithIdentity = "Option",
      backendDataParameterRefsWithIdentity = [backendDataParameterRefFromIdentity optionParamIdentity "a"],
      backendDataConstructorsWithIdentity = [BackendConstructor "Some" [] [optionParamTy] (optionTy optionParamTy)]
    }

optionParamIdentity :: TypeBinderIdentity
optionParamIdentity =
  typeBinderIdentityFromNode (NodeId 991213)

optionStaleParamIdentity :: TypeBinderIdentity
optionStaleParamIdentity =
  typeBinderIdentityFromNode (NodeId 991214)

optionParamTy :: BackendType
optionParamTy =
  BTVarWithIdentity (Just optionParamIdentity) "a"

optionStaleParamTy :: BackendType
optionStaleParamTy =
  BTVarWithIdentity (Just optionStaleParamIdentity) "a"

boxFData :: BackendData
boxFData =
  BackendData
    { backendDataName = "BoxF",
      backendDataParameters = ["a"],
      backendDataConstructors = [BackendConstructor "BoxF" [] [BTVar "a"] (boxFTy (BTVar "a"))]
    }

maybeFData :: BackendData
maybeFData =
  BackendData
    { backendDataName = "MaybeF",
      backendDataParameters = ["f", "a"],
      backendDataConstructors =
        [ BackendConstructor "NothingF" [] [] (maybeFTy (BTVar "f") (BTVar "a")),
          BackendConstructor "JustF" [] [BTVarApp "f" (BTVar "a" :| [])] (maybeFTy (BTVar "f") (BTVar "a"))
        ]
    }

packData :: BackendData
packData =
  BackendData
    { backendDataName = "Pack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "Pack" [BackendTypeBinder "a" Nothing] [BTVar "a"] packTy]
    }

boundedPackData :: BackendData
boundedPackData =
  BackendData
    { backendDataName = "BoundedPack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "BoundedPack" [BackendTypeBinder "a" (Just intTy)] [BTVar "a"] boundedPackTy]
    }

boundedListPackData :: BackendData
boundedListPackData =
  BackendData
    { backendDataName = "BoundedListPack",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "BoundedListPack" [BackendTypeBinder "a" (Just intTy)] [listTy (BTVar "a")] boundedListPackTy]
    }

dependentBoundedPackData :: BackendData
dependentBoundedPackData =
  BackendData
    { backendDataName = "DependentBoundedPack",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "DependentBoundedPack"
            [BackendTypeBinder "z" (Just intTy), BackendTypeBinder "a" (Just (BTVar "z"))]
            [BTVar "a"]
            dependentBoundedPackTy
        ]
    }

dependentActualBoundPackData :: BackendData
dependentActualBoundPackData =
  BackendData
    { backendDataName = "DependentActualBoundPack",
      backendDataParameters = [],
      backendDataConstructors =
        [ BackendConstructor
            "DependentActualBoundPack"
            [BackendTypeBinder "a" (Just (listTy intTy))]
            [BTVar "a"]
            dependentActualBoundPackTy
        ]
    }

captureForallData :: BackendData
captureForallData =
  BackendData
    { backendDataName = "CaptureForall",
      backendDataParameters = ["p"],
      backendDataConstructors =
        [ BackendConstructor
            "CaptureForall"
            []
            [BTForall "a" Nothing (BTVar "p")]
            (captureTy "CaptureForall" (BTVar "p"))
        ]
    }

captureMuData :: BackendData
captureMuData =
  BackendData
    { backendDataName = "CaptureMu",
      backendDataParameters = ["p"],
      backendDataConstructors =
        [ BackendConstructor
            "CaptureMu"
            []
            [BTMu "a" (BTVar "p")]
            (captureTy "CaptureMu" (BTVar "p"))
        ]
    }

captureCaseData :: BackendData
captureCaseData =
  BackendData
    { backendDataName = "CaptureCase",
      backendDataParameters = ["p"],
      backendDataConstructors = [BackendConstructor "CaptureCase" [] [] captureCaseTemplateTy]
    }

outOfOrderStructuralData :: BackendData
outOfOrderStructuralData =
  BackendData
    { backendDataName = "OutOfOrder",
      backendDataParameters = ["a", "b"],
      backendDataConstructors =
        [ BackendConstructor
            "OutOfOrder"
            []
            [BTVar "b", BTVar "a"]
            outOfOrderStructuralTy
        ]
    }

vacuousRecursiveBoxData :: BackendData
vacuousRecursiveBoxData =
  BackendData
    { backendDataName = "VacuousMuBox",
      backendDataParameters = [],
      backendDataConstructors = [BackendConstructor "VacuousMuBox" [] [vacuousRecursiveIntTy] vacuousRecursiveBoxTy]
    }

boxCaseExpr :: BackendExpr
boxCaseExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

someIntExpr :: BackendExpr
someIntExpr =
  BackendConstruct (optionTy intTy) "Some" [intLit 1]

someIntAsOptionVarExpr :: BackendExpr
someIntAsOptionVarExpr =
  BackendConstruct (optionTy optionStaleParamTy) "Some" [intLit 1]

someBoolAsOptionIntExpr :: BackendExpr
someBoolAsOptionIntExpr =
  BackendConstruct (optionTy intTy) "Some" [boolLit True]

identityPairLeftParamIdentity :: TypeBinderIdentity
identityPairLeftParamIdentity =
  typeBinderIdentityFromNode (NodeId 991201)

identityPairRightParamIdentity :: TypeBinderIdentity
identityPairRightParamIdentity =
  typeBinderIdentityFromNode (NodeId 991202)

identityPairDataIdentity :: SymbolIdentity
identityPairDataIdentity =
  testSymbolIdentity 991203 SymbolType "Main" "IdentityPair"

identityPairConstructorIdentity :: SymbolIdentity
identityPairConstructorIdentity =
  testSymbolIdentity 991204 SymbolConstructor "Main" "IdentityPair"

identityPairTy :: BackendType -> BackendType -> BackendType
identityPairTy left right =
  BTConWithIdentity (Just identityPairDataIdentity) (BaseTy "IdentityPair") (left :| [right])

identityPairIntTy :: BackendType
identityPairIntTy =
  literalBackendType (LInt 0)

identityPairBoolTy :: BackendType
identityPairBoolTy =
  literalBackendType (LBool True)

identityPairData :: BackendData
identityPairData =
  BackendDataWithIdentity
    { backendDataIdentity = Just identityPairDataIdentity,
      backendDataNameWithIdentity = "IdentityPair",
      backendDataParameterRefsWithIdentity =
        [ backendDataParameterRefFromIdentity identityPairLeftParamIdentity "a",
          backendDataParameterRefFromIdentity identityPairRightParamIdentity "a"
        ],
      backendDataConstructorsWithIdentity =
        [ BackendConstructorWithIdentity
            { backendConstructorIdentity = Just identityPairConstructorIdentity,
              backendConstructorNameWithIdentity = "IdentityPair",
              backendConstructorForallsWithIdentity = [],
              backendConstructorFieldsWithIdentity =
                [ BTVarWithIdentity (Just identityPairLeftParamIdentity) "a",
                  BTVarWithIdentity (Just identityPairRightParamIdentity) "a"
                ],
              backendConstructorResultWithIdentity =
                identityPairTy
                  (BTVarWithIdentity (Just identityPairLeftParamIdentity) "a")
                  (BTVarWithIdentity (Just identityPairRightParamIdentity) "a")
            }
        ]
    }

identityPairExpr :: BackendExpr
identityPairExpr =
  BackendConstructWithIdentity
    (identityPairTy identityPairIntTy identityPairBoolTy)
    (Just identityPairConstructorIdentity)
    "IdentityPair"
    [intLit 1, boolLit True]

swappedIdentityPairExpr :: BackendExpr
swappedIdentityPairExpr =
  BackendConstructWithIdentity
    (identityPairTy identityPairIntTy identityPairBoolTy)
    (Just identityPairConstructorIdentity)
    "IdentityPair"
    [boolLit True, intLit 1]

identityPlaceholderFieldIdentity :: TypeBinderIdentity
identityPlaceholderFieldIdentity =
  typeBinderIdentityFromNode (NodeId 991205)

identityPlaceholderFieldProgram :: BackendProgram
identityPlaceholderFieldProgram =
  programWithDataAndMainExpr
    [identityPlaceholderFieldData]
    ( BackendConstruct
        (BTCon (BaseTy "IdentityPlaceholder") (intTy :| []))
        "IdentityPlaceholder"
        [boolLit True]
    )

identityPlaceholderFieldData :: BackendData
identityPlaceholderFieldData =
  BackendData
    { backendDataName = "IdentityPlaceholder",
      backendDataParameters = ["a"],
      backendDataConstructors =
        [ BackendConstructor
            "IdentityPlaceholder"
            []
            [BTVarWithIdentity (Just identityPlaceholderFieldIdentity) "a"]
            (BTCon (BaseTy "IdentityPlaceholder") (BTVar "a" :| []))
        ]
    }

resultPlaceholderDataParamIdentity :: TypeBinderIdentity
resultPlaceholderDataParamIdentity =
  typeBinderIdentityFromNode (NodeId 991303)

resultPlaceholderIdentity :: TypeBinderIdentity
resultPlaceholderIdentity =
  typeBinderIdentityFromNode (NodeId 991304)

resultPlaceholderConstructExpr :: BackendExpr
resultPlaceholderConstructExpr =
  BackendConstruct
    (BTCon (BaseTy "ResultPlaceholder") (resultPlaceholderArgTy :| []))
    "ResultPlaceholder"
    []

resultPlaceholderCaseExpr :: BackendExpr
resultPlaceholderCaseExpr =
  BackendCase
    resultPlaceholderArgTy
    resultPlaceholderConstructExpr
    (BackendAlternative (BackendConstructorPattern "ResultPlaceholder" []) (intLit 1) :| [])

resultPlaceholderArgTy :: BackendType
resultPlaceholderArgTy =
  literalBackendType (LInt 0)

resultPlaceholderData :: BackendData
resultPlaceholderData =
  BackendDataWithIdentity
    { backendDataIdentity = Nothing,
      backendDataNameWithIdentity = "ResultPlaceholder",
      backendDataParameterRefsWithIdentity = [backendDataParameterRefFromIdentity resultPlaceholderDataParamIdentity "a"],
      backendDataConstructorsWithIdentity =
        [ BackendConstructor
            "ResultPlaceholder"
            []
            []
            (BTCon (BaseTy "ResultPlaceholder") (BTVarWithIdentity (Just resultPlaceholderIdentity) "a" :| []))
        ]
    }

malformedStructuralBoxTy :: BackendType
malformedStructuralBoxTy =
  BTMu "$Box_self" BTBottom

malformedStructuralBoxConstructExpr :: BackendExpr
malformedStructuralBoxConstructExpr =
  BackendConstruct malformedStructuralBoxTy "Box" [intLit 1]

structuralBoxTy :: BackendType
structuralBoxTy =
  BTMu "$Box_self" (singleFieldStructuralBody intTy)

structuralBoxConstructExpr :: BackendExpr
structuralBoxConstructExpr =
  BackendConstruct structuralBoxTy "Box" [intLit 1]

mismatchedStructuralBoxTy :: BackendType
mismatchedStructuralBoxTy =
  BTMu "$Box_self" (singleFieldStructuralBody boolTy)

mismatchedStructuralBoxConstructExpr :: BackendExpr
mismatchedStructuralBoxConstructExpr =
  BackendConstruct mismatchedStructuralBoxTy "Box" [intLit 1]

graphIdentityStructuralBoxTy :: BackendType
graphIdentityStructuralBoxTy =
  BTMuWithIdentity (Just (typeBinderIdentityFromNode (NodeId 991365))) "$Box_self" (singleFieldStructuralBody intTy)

graphIdentityStructuralBoxConstructExpr :: BackendExpr
graphIdentityStructuralBoxConstructExpr =
  BackendConstruct graphIdentityStructuralBoxTy "Box" [intLit 1]

mismatchedStructuralBoxCaseProgram :: BackendProgram
mismatchedStructuralBoxCaseProgram =
  programWithDataAndBindings
    [boxData]
    [ binding "badBox" mismatchedStructuralBoxTy (BackendVar mismatchedStructuralBoxTy "badBox"),
      mainBinding
        ( boxCaseExprWith
            (BackendVar mismatchedStructuralBoxTy "badBox")
            (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])
        )
    ]

justFBoxBoolExpr :: BackendExpr
justFBoxBoolExpr =
  BackendConstruct (maybeFTy (BTBase (BaseTy "BoxF")) boolTy) "JustF" [BackendConstruct (boxFTy boolTy) "BoxF" [boolLit True]]

maybeFCaseExpr :: BackendExpr
maybeFCaseExpr =
  BackendCase
    { backendExprType = boolTy,
      backendScrutinee = justFBoxBoolExpr,
      backendAlternatives =
        BackendAlternative (BackendConstructorPattern "NothingF" []) (boolLit False)
          :| [BackendAlternative (BackendConstructorPattern "JustF" ["box"]) (boolLit True)]
    }

packIntExpr :: BackendExpr
packIntExpr =
  BackendConstruct packTy "Pack" [intLit 1]

boundedPackIntExpr :: BackendExpr
boundedPackIntExpr =
  BackendConstruct boundedPackTy "BoundedPack" [intLit 1]

boundedPackBoolExpr :: BackendExpr
boundedPackBoolExpr =
  BackendConstruct boundedPackTy "BoundedPack" [boolLit True]

dependentBoundedPackIntExpr :: BackendExpr
dependentBoundedPackIntExpr =
  BackendConstruct dependentBoundedPackTy "DependentBoundedPack" [intLit 1]

dependentBoundedPackBoolExpr :: BackendExpr
dependentBoundedPackBoolExpr =
  BackendConstruct dependentBoundedPackTy "DependentBoundedPack" [boolLit True]

dependentActualBoundPackProgram :: BackendProgram
dependentActualBoundPackProgram =
  programWithDataAndMainExpr [dependentActualBoundPackData] (dependentActualBoundPackWrapper intTy)

dependentActualBoundPackWrongProgram :: BackendProgram
dependentActualBoundPackWrongProgram =
  programWithDataAndMainExpr [dependentActualBoundPackData] (dependentActualBoundPackWrapper boolTy)

dependentActualBoundPackWrapper :: BackendType -> BackendExpr
dependentActualBoundPackWrapper zBound =
  BackendTyAbsWithIdentity
    { backendExprType = dependentActualBoundPackWrapperTy zBound,
      backendTyParamIdentity = Nothing,
      backendTyParamName = "z",
      backendTyParamBound = Just zBound,
      backendTyAbsBody =
        BackendTyAbsWithIdentity
          { backendExprType = dependentActualBoundPackInnerTy,
            backendTyParamIdentity = Nothing,
            backendTyParamName = "b",
            backendTyParamBound = Just (listTy (BTVar "z")),
            backendTyAbsBody =
              BackendLamWithIdentity
                { backendParamIdentity = Nothing, backendExprType = BTArrow (BTVar "b") dependentActualBoundPackTy,
                  backendParamName = "x",
                  backendParamType = BTVar "b",
                  backendBody =
                    BackendConstruct
                      dependentActualBoundPackTy
                      "DependentActualBoundPack"
                      [BackendVar (BTVar "b") "x"]
                }
          }
    }

dependentActualBoundPackWrapperTy :: BackendType -> BackendType
dependentActualBoundPackWrapperTy zBound =
  BTForall "z" (Just zBound) dependentActualBoundPackInnerTy

dependentActualBoundPackInnerTy :: BackendType
dependentActualBoundPackInnerTy =
  BTForall "b" (Just (listTy (BTVar "z"))) (BTArrow (BTVar "b") dependentActualBoundPackTy)

boundedPackCaseExpr :: BackendExpr
boundedPackCaseExpr =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives = BackendAlternative (BackendConstructorPattern "BoundedPack" ["n"]) (BackendVar intTy "n") :| []
    }

boundedPackRepackCaseExpr :: BackendExpr
boundedPackRepackCaseExpr =
  BackendCase
    { backendExprType = boundedPackTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives =
        BackendAlternative
          (BackendConstructorPattern "BoundedPack" ["n"])
          (BackendConstruct boundedPackTy "BoundedPack" [BackendVar (BTVar "a") "n"])
          :| []
    }

boundedPackOuterNameCollisionCaseProgram :: BackendProgram
boundedPackOuterNameCollisionCaseProgram =
  boundedPackOuterNameCollisionCaseWith (BackendVar intTy "n")

boundedPackOuterNameCollisionWrongOuterUseProgram :: BackendProgram
boundedPackOuterNameCollisionWrongOuterUseProgram =
  boundedPackOuterNameCollisionCaseWith (BackendVar intTy "outer")

boundedPackOuterNameCollisionCaseWith :: BackendExpr -> BackendProgram
boundedPackOuterNameCollisionCaseWith branchBody =
  programWithDataAndMainExpr
    [boundedPackData]
    ( BackendTyAbsWithIdentity
        { backendExprType = BTForall "a" (Just boolTy) (BTArrow (BTVar "a") intTy),
          backendTyParamIdentity = Nothing,
          backendTyParamName = "a",
          backendTyParamBound = Just boolTy,
          backendTyAbsBody =
            BackendLamWithIdentity
              { backendParamIdentity = Nothing, backendExprType = BTArrow (BTVar "a") intTy,
                backendParamName = "outer",
                backendParamType = BTVar "a",
                backendBody =
                  BackendCase
                    { backendExprType = intTy,
                      backendScrutinee = boundedPackIntExpr,
                      backendAlternatives =
                        BackendAlternative
                          (BackendConstructorPattern "BoundedPack" ["n"])
                          branchBody
                          :| []
                    }
              }
        }
    )

boundedPackWrongBoundUseCaseExpr :: BackendExpr
boundedPackWrongBoundUseCaseExpr =
  BackendCase
    { backendExprType = boolTy,
      backendScrutinee = boundedPackIntExpr,
      backendAlternatives =
        BackendAlternative
          (BackendConstructorPattern "BoundedPack" ["n"])
          (BackendVar boolTy "n")
          :| []
    }

boundedListPackCaseProgram :: BackendProgram
boundedListPackCaseProgram =
  programWithDataAndBindings
    [boundedListPackData]
    [ mainBinding
        BackendCase
          { backendExprType = listTy intTy,
            backendScrutinee = boundedListPackListIntExpr,
            backendAlternatives =
              BackendAlternative
                (BackendConstructorPattern "BoundedListPack" ["n"])
                (BackendVar (listTy intTy) "n")
                :| []
          },
      listArgBinding
    ]

boundedListPackWrongBoundUseCaseProgram :: BackendProgram
boundedListPackWrongBoundUseCaseProgram =
  programWithDataAndBindings
    [boundedListPackData]
    [ mainBinding
        BackendCase
          { backendExprType = listTy boolTy,
            backendScrutinee = boundedListPackListIntExpr,
            backendAlternatives =
              BackendAlternative
                (BackendConstructorPattern "BoundedListPack" ["n"])
                (BackendVar (listTy boolTy) "n")
                :| []
          },
      listArgBinding
    ]

boundedListPackListIntExpr :: BackendExpr
boundedListPackListIntExpr =
  BackendConstruct boundedListPackTy "BoundedListPack" [BackendVar (listTy intTy) "listArg"]

listArgBinding :: BackendBinding
listArgBinding =
  binding "listArg" (listTy intTy) (BackendVar (listTy intTy) "listArg")

captureForallConstructProgram :: BackendProgram
captureForallConstructProgram =
  programWithDataAndBindings
    [captureForallData]
    [ mainBinding (BackendConstruct (captureTy "CaptureForall" (BTVar "a1")) "CaptureForall" [BackendVar captureForallActualTy "polyArg"]),
      binding "polyArg" captureForallActualTy (BackendVar captureForallActualTy "polyArg")
    ]

captureMuConstructProgram :: BackendProgram
captureMuConstructProgram =
  programWithDataAndBindings
    [captureMuData]
    [ mainBinding (BackendConstruct (captureTy "CaptureMu" (BTVar "a1")) "CaptureMu" [BackendVar captureMuActualTy "muArg"]),
      binding "muArg" captureMuActualTy (BackendVar captureMuActualTy "muArg")
    ]

captureCaseProgram :: BackendProgram
captureCaseProgram =
  programWithDataAndBindings
    [captureCaseData]
    [ mainBinding
        BackendCase
          { backendExprType = intTy,
            backendScrutinee = BackendVar captureCaseScrutineeTy "captureCaseArg",
            backendAlternatives = BackendAlternative (BackendConstructorPattern "CaptureCase" []) (intLit 1) :| []
          },
      binding "captureCaseArg" captureCaseScrutineeTy (BackendVar captureCaseScrutineeTy "captureCaseArg")
    ]

vacuousRecursiveVariableMismatchProgram :: BackendProgram
vacuousRecursiveVariableMismatchProgram =
  programWithBindings
    [ mainBinding
        BackendLamWithIdentity
          { backendParamIdentity = Nothing, backendExprType = BTArrow vacuousRecursiveIntTy vacuousRecursiveBoolTy,
            backendParamName = "x",
            backendParamType = vacuousRecursiveIntTy,
            backendBody = BackendVar vacuousRecursiveBoolTy "x"
          }
    ]

oneSidedVacuousRecursiveMismatchProgram :: BackendProgram
oneSidedVacuousRecursiveMismatchProgram =
  programWithBindings
    [ mainBinding
        BackendLamWithIdentity
          { backendParamIdentity = Nothing, backendExprType = BTArrow recursiveArrowIntTy vacuousRecursiveBoolTy,
            backendParamName = "x",
            backendParamType = recursiveArrowIntTy,
            backendBody = BackendVar vacuousRecursiveBoolTy "x"
          }
    ]

vacuousRecursiveConstructorMismatchProgram :: BackendProgram
vacuousRecursiveConstructorMismatchProgram =
  programWithDataAndBindings
    [vacuousRecursiveBoxData]
    [ mainBinding (BackendConstruct vacuousRecursiveBoxTy "VacuousMuBox" [BackendVar vacuousRecursiveBoolTy "muBoolArg"]),
      binding "muBoolArg" vacuousRecursiveBoolTy (BackendVar vacuousRecursiveBoolTy "muBoolArg")
    ]

optionCaseExpr :: BackendExpr
optionCaseExpr =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = someIntExpr,
      backendAlternatives = BackendAlternative (BackendConstructorPattern "Some" ["n"]) (BackendVar intTy "n") :| []
    }

boxCaseWrongResultExpr :: BackendExpr
boxCaseWrongResultExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendLit boolTy (LBool True)) :| [])

boxCaseWrongScrutineeExpr :: BackendExpr
boxCaseWrongScrutineeExpr =
  boxCaseExprWith
    (boolLit True)
    (BackendAlternative (BackendConstructorPattern "Box" ["n"]) (BackendVar intTy "n") :| [])

boxCaseWrongPatternArityExpr :: BackendExpr
boxCaseWrongPatternArityExpr =
  boxCaseExprWith
    (BackendConstruct boxTy "Box" [intLit 1])
    (BackendAlternative (BackendConstructorPattern "Box" []) (intLit 1) :| [])

boxCaseExprWith :: BackendExpr -> NonEmpty BackendAlternative -> BackendExpr
boxCaseExprWith scrutinee alternatives =
  BackendCase
    { backendExprType = intTy,
      backendScrutinee = scrutinee,
      backendAlternatives = alternatives
    }

intLit :: Integer -> BackendExpr
intLit n =
  BackendLit (literalBackendType (LInt n)) (LInt n)

boolLit :: Bool -> BackendExpr
boolLit b =
  BackendLit (literalBackendType (LBool b)) (LBool b)

polyIdTy :: BackendType
polyIdTy =
  BTForall "a" Nothing (BTArrow (BTVar "a") (BTVar "a"))

idTy :: BackendType
idTy =
  BTArrow intTy intTy

intTy :: BackendType
intTy =
  literalBackendType (LInt 0)

boolTy :: BackendType
boolTy =
  literalBackendType (LBool True)

unitTy :: BackendType
unitTy =
  BTBase (BaseTy "Unit")

ioTy :: BackendType -> BackendType
ioTy ty =
  BTConWithIdentity (Just (builtinTypeIdentity "IO")) (BaseTy "IO") (ty :| [])

preludeUnitStructuralTy :: BackendType
preludeUnitStructuralTy =
  BTMu "$Prelude.Unit_self" (BTForall "$Prelude.Unit_result" Nothing (BTArrow (BTVar "$Prelude.Unit_result") (BTVar "$Prelude.Unit_result")))

nullaryStructuralBody :: BackendType
nullaryStructuralBody =
  BTForall "r" Nothing (BTArrow (BTVar "r") (BTVar "r"))

singleFieldStructuralBody :: BackendType -> BackendType
singleFieldStructuralBody fieldTy =
  BTForall "r" Nothing (BTArrow (BTArrow fieldTy (BTVar "r")) (BTVar "r"))

boxTy :: BackendType
boxTy =
  BTBase (BaseTy "Box")

fnBoxTy :: BackendType
fnBoxTy =
  BTBase (BaseTy "FnBox")

packTy :: BackendType
packTy =
  BTBase (BaseTy "Pack")

boundedPackTy :: BackendType
boundedPackTy =
  BTBase (BaseTy "BoundedPack")

boundedListPackTy :: BackendType
boundedListPackTy =
  BTBase (BaseTy "BoundedListPack")

dependentBoundedPackTy :: BackendType
dependentBoundedPackTy =
  BTBase (BaseTy "DependentBoundedPack")

dependentActualBoundPackTy :: BackendType
dependentActualBoundPackTy =
  BTBase (BaseTy "DependentActualBoundPack")

vacuousRecursiveBoxTy :: BackendType
vacuousRecursiveBoxTy =
  BTBase (BaseTy "VacuousMuBox")

listTy :: BackendType -> BackendType
listTy ty =
  BTCon (BaseTy "List") (ty :| [])

optionTy :: BackendType -> BackendType
optionTy ty =
  BTCon (BaseTy "Option") (ty :| [])

boxFTy :: BackendType -> BackendType
boxFTy ty =
  BTCon (BaseTy "BoxF") (ty :| [])

maybeFTy :: BackendType -> BackendType -> BackendType
maybeFTy fTy argTy =
  BTCon (BaseTy "MaybeF") (fTy :| [argTy])

pairTy :: BackendType -> BackendType -> BackendType
pairTy left right =
  BTCon (BaseTy "Pair") (left :| [right])

captureTy :: String -> BackendType -> BackendType
captureTy name ty =
  BTCon (BaseTy name) (ty :| [])

captureForallActualTy :: BackendType
captureForallActualTy =
  BTForall "x" Nothing (BTVar "x")

captureForallInstantiatedTy :: BackendType
captureForallInstantiatedTy =
  BTForall "a" Nothing (BTVar "a1")

captureMuActualTy :: BackendType
captureMuActualTy =
  BTMu "x" (BTVar "x")

captureMuInstantiatedTy :: BackendType
captureMuInstantiatedTy =
  BTMu "a" (BTVar "a1")

vacuousRecursiveIntTy :: BackendType
vacuousRecursiveIntTy =
  BTMu "a" intTy

vacuousRecursiveBoolTy :: BackendType
vacuousRecursiveBoolTy =
  BTMu "b" boolTy

recursiveArrowIntTy :: BackendType
recursiveArrowIntTy =
  BTMu "self" (BTArrow (BTVar "self") intTy)

captureCaseTemplateTy :: BackendType
captureCaseTemplateTy =
  BTCon (BaseTy "CaptureCase") (BTVar "p" :| [BTForall "a" Nothing (BTVar "p")])

captureCaseScrutineeTy :: BackendType
captureCaseScrutineeTy =
  BTCon (BaseTy "CaptureCase") (BTVar "a1" :| [captureForallActualTy])

outOfOrderStructuralConstructExpr :: BackendExpr
outOfOrderStructuralConstructExpr =
  BackendConstruct
    (outOfOrderTy intTy boolTy)
    "OutOfOrder"
    [boolLit True, intLit 1]

outOfOrderTy :: BackendType -> BackendType -> BackendType
outOfOrderTy aTy bTy =
  BTCon (BaseTy "OutOfOrder") (aTy :| [bTy])

outOfOrderStructuralTy :: BackendType
outOfOrderStructuralTy =
  BTMu "$OutOfOrder_self" outOfOrderStructuralBody

outOfOrderStructuralBody :: BackendType
outOfOrderStructuralBody =
  BTForall
    "r"
    Nothing
    (BTArrow (BTArrow (BTVar "b") (BTArrow (BTVar "a") (BTVar "r"))) (BTVar "r"))

substituteBackendTypeByName :: String -> BackendType -> BackendType -> BackendType
substituteBackendTypeByName name replacement =
  substituteBackendTypesByKey (Map.singleton (backendTypeSubstitutionKeyFor Nothing name) replacement)

substituteBackendTypesByName :: Map.Map String BackendType -> BackendType -> BackendType
substituteBackendTypesByName replacements =
  substituteBackendTypesByKey (Map.mapKeys (backendTypeSubstitutionKeyFor Nothing) replacements)
