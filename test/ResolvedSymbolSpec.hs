{-# LANGUAGE GADTs #-}

module ResolvedSymbolSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Elaborate (classInfoForConstraint, lowerType, mkElaborateScope, sourceTypeIdentityInScope, sourceTypeViewInScope)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax.Program (resolvedExportTypeRefFromSymbols, refDisplayName)
import qualified MLF.Frontend.Symbol as Symbol
import MLF.Frontend.Symbol
  ( symbolIdentityAliasMapWith,
    symbolIdentityStableName,
    sameSymbolIdentity,
  )
import MLF.Frontend.Syntax
  ( ResolvedSrcTy (..),
    ResolvedTypeBinderRef,
    resolvedTypeBinderRefFromIdentity,
    SrcKind (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    resolvedSrcTypeIdentityType,
    resolvedTypeBinderTypeIdentity,
  )
import MLF.Types.Identity (TypeBinderIdentity, UniqueIdentity (..), typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import qualified MLF.Types.Elab as Elab
import Test.Hspec
import TypeViewTestSupport
  ( fixtureTypeView,
    mkTypeView,
    setTypeViewBinderIdentities,
    setTypeViewDisplay,
    setTypeViewHeadIdentities,
    setTypeViewTypes,
  )

generatedSymbolIdentity ::
  Int ->
  SymbolNamespace ->
  String ->
  String ->
  Maybe SymbolOwnerIdentity ->
  SymbolIdentity
generatedSymbolIdentity unique namespace moduleName name owner =
  symbolIdentityFromParts (UniqueIdentity unique) namespace moduleName name owner

valueInfoIdentity :: SymbolIdentity
valueInfoIdentity =
  generatedSymbolIdentity 101 SymbolValue "Lib" "answer" Nothing

mainValueIdentity :: SymbolIdentity
mainValueIdentity =
  generatedSymbolIdentity 102 SymbolValue "Main" "main" Nothing

tokenTypeIdentity :: SymbolIdentity
tokenTypeIdentity =
  generatedSymbolIdentity 103 SymbolType "Lib" "Token" Nothing

tokenOwnerIdentity :: SymbolOwnerIdentity
tokenOwnerIdentity =
  SymbolOwnerType tokenTypeIdentity

someCtorIdentity :: SymbolIdentity
someCtorIdentity =
  generatedSymbolIdentity 104 SymbolConstructor "Lib" "Some" (Just tokenOwnerIdentity)

higherTypeIdentity :: SymbolIdentity
higherTypeIdentity =
  generatedSymbolIdentity 105 SymbolType "Lib" "Higher" Nothing

higherOwnerIdentity :: SymbolOwnerIdentity
higherOwnerIdentity =
  SymbolOwnerType higherTypeIdentity

higherCtorIdentity :: SymbolIdentity
higherCtorIdentity =
  generatedSymbolIdentity 106 SymbolConstructor "Lib" "Higher" (Just higherOwnerIdentity)

higherFIdentity :: TypeBinderIdentity
higherFIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 110)

higherAIdentity :: TypeBinderIdentity
higherAIdentity =
  typeBinderIdentityFromUnique (UniqueIdentity 111)

eqClassIdentity :: SymbolIdentity
eqClassIdentity =
  generatedSymbolIdentity 107 SymbolClass "Lib" "Eq" Nothing

eqClassOwnerIdentity :: SymbolOwnerIdentity
eqClassOwnerIdentity =
  SymbolOwnerClass eqClassIdentity

eqMethodIdentity :: SymbolIdentity
eqMethodIdentity =
  generatedSymbolIdentity 108 SymbolMethod "Lib" "eq" (Just eqClassOwnerIdentity)

eqParamUnique :: UniqueIdentity
eqParamUnique =
  UniqueIdentity 109

eqParamIdentity :: TypeBinderIdentity
eqParamIdentity =
  typeBinderIdentityFromUnique eqParamUnique

eqParam :: CheckedTypeParam
eqParam =
  CheckedTypeParam (resolvedTypeBinderRef eqParamUnique "a") KType

resolvedTypeBinderRef :: UniqueIdentity -> String -> ResolvedTypeBinderRef
resolvedTypeBinderRef identity name =
  resolvedTypeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name

spec :: Spec
spec = do
  describe "MLF.Program resolved symbol identities" $ do
    it "keeps imported value identity stable across unqualified and aliased spellings" $ do
      let unqualified = resolvedValueInfoSymbol (SymbolUnqualifiedImport "Lib") "answer" valueInfo
          qualified =
            resolvedValueInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              "L.answer"
              valueInfo

      sameResolvedSymbol unqualified qualified `shouldBe` True
      resolvedSymbolIdentity unqualified `shouldBe` resolvedSymbolIdentity qualified
      resolvedSymbolSpelling unqualified `shouldNotBe` resolvedSymbolSpelling qualified

    it "represents imported type and constructor aliases with the same semantic identities" $ do
      let typeUnqualified = resolvedDataInfoSymbol (SymbolUnqualifiedImport "Lib") "Token" tokenDataInfo
          typeQualified = resolvedDataInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Token" tokenDataInfo
          ctorUnqualified = resolvedConstructorInfoSymbol (SymbolUnqualifiedImport "Lib") "Some" tokenDataInfo someCtor
          ctorQualified =
            resolvedConstructorInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              "L.Some"
              tokenDataInfo
              someCtor

      sameResolvedSymbol typeUnqualified typeQualified `shouldBe` True
      sameResolvedSymbol ctorUnqualified ctorQualified `shouldBe` True
      symbolOwnerIdentity (resolvedSymbolIdentity ctorQualified)
        `shouldBe` Just tokenOwnerIdentity

    it "represents imported class and method aliases with the same semantic identities" $ do
      let classUnqualified = resolvedClassInfoSymbol (SymbolUnqualifiedImport "Lib") "Eq" eqClassInfo
          classQualified = resolvedClassInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Eq" qualifiedEqClassInfo
          methodUnqualified = resolvedValueInfoSymbol (SymbolUnqualifiedImport "Lib") "eq" eqMethodValue
          methodQualified =
            resolvedValueInfoSymbol
              (SymbolQualifiedImport "Lib" "L")
              "L.eq"
              qualifiedEqMethodValue

      sameResolvedSymbol classUnqualified classQualified `shouldBe` True
      sameResolvedSymbol methodUnqualified methodQualified `shouldBe` True
      symbolOwnerIdentity (resolvedSymbolIdentity methodQualified)
        `shouldBe` Just eqClassOwnerIdentity

    it "can model local declarations and module/import identities without changing semantic keys" $ do
      let local = resolvedValueInfoSymbol (SymbolLocal "Main") "main" mainValueInfo
          importedModule = resolvedModuleSymbol (SymbolQualifiedImport "Lib" "L") (UniqueIdentity 10) "Lib" "L"

      resolvedSymbolIdentity local
        `shouldBe` mainValueIdentity
      resolvedSymbolIdentity importedModule
        `shouldBe` generatedSymbolIdentity 10 SymbolModule "Lib" "Lib" Nothing
      symbolDisplayName (resolvedSymbolSpelling importedModule) `shouldBe` "L"

    it "compares resolved symbols by exact identity payload" $ do
      let first =
            mkResolvedSymbol
              (generatedSymbolIdentity 1 SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")
          renamed =
            mkResolvedSymbol
              (generatedSymbolIdentity 1 SymbolValue "Main" "x" Nothing)
              "Main.x"
              "Main.x"
              (SymbolQualifiedImport "Main" "Main")
          conflictingPayload =
            mkResolvedSymbol
              (generatedSymbolIdentity 1 SymbolValue "Other" "stale-x" Nothing)
              "Other.x"
              "Other.x"
              (SymbolQualifiedImport "Other" "Other")
          second =
            mkResolvedSymbol
              (generatedSymbolIdentity 2 SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")

      sameResolvedSymbol first renamed `shouldBe` True
      sameResolvedSymbol first conflictingPayload `shouldBe` False
      sameResolvedSymbol first second `shouldBe` False

    it "matches resolved symbols by semantic identity, independent of display names" $ do
      let renamedPayload =
            generatedSymbolIdentity 101 SymbolValue "Lib" "stale-answer" Nothing
          distinctPayload =
            generatedSymbolIdentity 101 SymbolValue "Other" "answer" Nothing

      sameSymbolIdentity valueInfoIdentity renamedPayload `shouldBe` False
      sameSymbolIdentity valueInfoIdentity distinctPayload `shouldBe` False
      sameSymbolIdentity valueInfoIdentity valueInfoIdentity `shouldBe` True

    it "uses semantic identity for resolved symbol and reference equality" $ do
      let first =
            mkResolvedSymbol
              (generatedSymbolIdentity 901 SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")
          firstAlias =
            mkResolvedSymbol
              (generatedSymbolIdentity 901 SymbolValue "Main" "x" Nothing)
              "Main.x"
              "Main.x"
              (SymbolQualifiedImport "Main" "Main")
          conflictingPayload =
            mkResolvedSymbol
              (generatedSymbolIdentity 901 SymbolValue "Other" "stale-x" Nothing)
              "Other.x"
              "Other.x"
              (SymbolQualifiedImport "Other" "Other")
          firstRef = mkResolvedReference ResolvedValueReference "x" first
          firstAliasRef = mkResolvedReference ResolvedValueReference "Main.x" firstAlias
          conflictingPayloadRef = mkResolvedReference ResolvedValueReference "Other.x" conflictingPayload

      firstAlias `shouldBe` first
      conflictingPayload `shouldNotBe` first
      Map.lookup firstAlias (Map.singleton first "hit") `shouldBe` Just "hit"
      firstAliasRef `shouldBe` firstRef
      conflictingPayloadRef `shouldNotBe` firstRef
      Map.lookup firstAliasRef (Map.singleton firstRef "hit") `shouldBe` Just "hit"

    it "compares bare symbol identities by exact payload" $ do
      let original = generatedSymbolIdentity 991905 SymbolValue "Main" "value" Nothing
          stalePayload = generatedSymbolIdentity 991905 SymbolValue "Other" "staleValue" Nothing

      stalePayload `shouldNotBe` original
      Map.lookup stalePayload (Map.singleton original "hit") `shouldBe` Nothing

    it "does not look up class or instance methods through stale identity payloads" $ do
      let methodSymbol = resolvedMethodInfoSymbol (SymbolLocal "Lib") "eq" eqMethodInfo
          staleMethodIdentity = renameSymbolDefiningName "$stale.eq" eqMethodIdentity
          staleMethodSymbol =
            mkResolvedSymbol
              staleMethodIdentity
              "$stale.eq"
              "$stale.eq"
              (SymbolLocal "Lib")
          instanceInfo =
            InstanceInfo
              { instanceClassSymbol = eqClassIdentity,
                instanceOriginModuleIdentity = generatedSymbolIdentity 905 SymbolModule "Lib" "Lib" Nothing,
                instanceConstraintInfos = [],
                instanceHeadTypeViews = mkTypeView (STBase "Token") (STBase "Token") :| [],
                instanceMethodsByIdentity = Map.singleton eqMethodIdentity eqMethodValue
              }

      lookupClassMethod methodSymbol eqClassInfo `shouldBe` Just eqMethodInfo
      lookupClassMethod staleMethodSymbol eqClassInfo `shouldBe` Nothing
      lookupClassMethod methodSymbol (eqClassInfo {classMethodsByIdentity = Map.singleton staleMethodIdentity eqMethodInfo})
        `shouldBe` Nothing
      lookupInstanceMethod eqMethodInfo instanceInfo `shouldBe` Just eqMethodValue
      lookupInstanceMethod (eqMethodInfo {methodInfoSymbol = staleMethodIdentity}) instanceInfo
        `shouldBe` Nothing
      lookupInstanceMethod eqMethodInfo (instanceInfo {instanceMethodsByIdentity = Map.singleton staleMethodIdentity eqMethodValue})
        `shouldBe` Nothing

    it "uses exact payload identity for primitive and constructor refs" $ do
      let primitive = generatedSymbolIdentity 991800 SymbolValue "Main" "__p" Nothing
          primitiveConflict = generatedSymbolIdentity 991800 SymbolValue "Other" "__p" Nothing
          dataIdentity = generatedSymbolIdentity 991801 SymbolType "Main" "Box" Nothing
          owner = SymbolOwnerType dataIdentity
          ctor = generatedSymbolIdentity 991802 SymbolConstructor "Main" "Box" (Just owner)
          ctorConflict = generatedSymbolIdentity 991802 SymbolConstructor "Other" "Box" (Just owner)

      primitiveRefFromSymbol primitive `shouldNotBe` primitiveRefFromSymbol primitiveConflict
      Map.lookup (primitiveRefFromSymbol primitiveConflict) (Map.singleton (primitiveRefFromSymbol primitive) "hit")
        `shouldBe` Nothing
      constructorRefFromSymbol ctor `shouldNotBe` constructorRefFromSymbol ctorConflict

    it "uses exact payload identity for symbol owners" $ do
      let owner = SymbolOwnerType (generatedSymbolIdentity 991803 SymbolType "Main" "Box" Nothing)
          conflictingOwner = SymbolOwnerType (generatedSymbolIdentity 991803 SymbolType "Other" "Box" Nothing)

      owner `shouldNotBe` conflictingOwner
      Map.lookup conflictingOwner (Map.singleton owner "hit") `shouldBe` Nothing

    it "uses semantic identity for resolved export type references" $ do
      let typeUnqualified = resolvedDataInfoSymbol (SymbolUnqualifiedImport "Lib") "Token" tokenDataInfo
          typeQualified = resolvedDataInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Token" tokenDataInfo
          unqualifiedRef = resolvedExportTypeRefFromSymbols "Token" [typeUnqualified]
          qualifiedRef = resolvedExportTypeRefFromSymbols "L.Token" [typeQualified]

      qualifiedRef `shouldBe` unqualifiedRef

    it "exposes generated stable names for identity aliases" $ do
      let typeSymbol =
            mkResolvedSymbol
              (generatedSymbolIdentity 42 SymbolType "Lib" "Token" Nothing)
              "Token"
              "Token"
              (SymbolLocal "Lib")

      symbolIdentityStableName (resolvedSymbolIdentity typeSymbol) `shouldBe` "$identity#42"

    it "drops ambiguous display aliases without losing stable identity aliases" $ do
      let leftIdentity = generatedSymbolIdentity 301 SymbolType "Left" "Shared" Nothing
          rightIdentity = generatedSymbolIdentity 302 SymbolType "Right" "Shared" Nothing
          aliases =
            symbolIdentityAliasMapWith
              [ (leftIdentity, ["VisibleLeft", "Shared"]),
                (rightIdentity, ["VisibleRight", "Shared"])
              ]

      Map.lookup "Shared" aliases `shouldBe` Nothing
      Map.lookup (symbolIdentityStableName leftIdentity) aliases `shouldBe` Just leftIdentity
      Map.lookup (symbolIdentityStableName rightIdentity) aliases `shouldBe` Just rightIdentity
      Map.lookup "VisibleLeft" aliases `shouldBe` Just leftIdentity
      Map.lookup "VisibleRight" aliases `shouldBe` Just rightIdentity

    it "does not choose an arbitrary symbol alias payload when one identity has conflicting metadata" $ do
      let originalIdentity = generatedSymbolIdentity 303 SymbolType "Lib" "Token" Nothing
          conflictingIdentity = generatedSymbolIdentity 303 SymbolType "Other" "StaleToken" Nothing
          aliases =
            symbolIdentityAliasMapWith
              [ (originalIdentity, ["VisibleOriginal"]),
                (conflictingIdentity, ["VisibleRenamed"])
              ]

      Map.lookup (symbolIdentityStableName originalIdentity) aliases `shouldBe` Nothing
      fmap Symbol.symbolDefiningModule (Map.lookup "VisibleOriginal" aliases)
        `shouldBe` Just "Lib"
      fmap Symbol.symbolDefiningName (Map.lookup "VisibleRenamed" aliases)
        `shouldBe` Just "StaleToken"

    it "keeps method type head identities in type views" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  setTypeViewHeadIdentities
                    (Map.singleton stableToken tokenTypeIdentity)
                    ( setTypeViewTypes
                        (STBase "Token")
                        (STBase stableToken)
                        (methodTypeViewRaw eqMethodInfo)
                    )
              }

      Map.lookup stableToken (typeViewHeadIdentities (methodTypeView methodInfo))
        `shouldBe` Just tokenTypeIdentity
      Map.lookup stableToken (typeViewHeadIdentities (methodResultTypeView methodInfo))
        `shouldBe` Just tokenTypeIdentity
      Map.lookup "Token" (typeViewHeadIdentities (methodResultTypeView methodInfo))
        `shouldBe` Just tokenTypeIdentity

    it "keeps method result head identities by payload stable name" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  setTypeViewHeadIdentities
                    (Map.singleton "Token" tokenTypeIdentity)
                    ( setTypeViewTypes
                        (STBase stableToken)
                        (STBase stableToken)
                        (methodTypeViewRaw eqMethodInfo)
                    )
              }

      Map.lookup "Token" (typeViewHeadIdentities (methodResultTypeView methodInfo))
        `shouldBe` Just tokenTypeIdentity
      Map.lookup stableToken (typeViewHeadIdentities (methodResultTypeView methodInfo))
        `shouldBe` Just tokenTypeIdentity

    it "keeps method parameter head identities by payload stable name" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  setTypeViewHeadIdentities
                    (Map.singleton "Token" tokenTypeIdentity)
                    ( setTypeViewTypes
                        (STArrow (STBase stableToken) (STBase "Bool"))
                        (STArrow (STBase stableToken) (STBase "Bool"))
                        (methodTypeViewRaw eqMethodInfo)
                    )
              }

      map (Map.lookup "Token" . typeViewHeadIdentities) (methodParamTypeViews (methodTypeView methodInfo))
        `shouldBe` [Just tokenTypeIdentity]
      map (Map.lookup stableToken . typeViewHeadIdentities) (methodParamTypeViews (methodTypeView methodInfo))
        `shouldBe` [Just tokenTypeIdentity]

    it "keeps projected method result head identities from carried payloads" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            setTypeViewHeadIdentities
              (Map.singleton stableToken tokenTypeIdentity)
              (mkTypeView (STArrow (STBase "Bool") (STBase "Token")) (STArrow (STBase "Bool") (STBase stableToken)))
          resultView = methodResultTypeViewFrom view

      Map.lookup "Token" (typeViewHeadIdentities resultView) `shouldBe` Just tokenTypeIdentity
      Map.lookup "Bool" (typeViewHeadIdentities resultView) `shouldBe` Nothing

    it "keeps partial arrow result head identities from carried payloads" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            setTypeViewHeadIdentities
              (Map.singleton stableToken tokenTypeIdentity)
              ( mkTypeView
                  (STArrow (STBase "Bool") (STArrow (STBase "Char") (STBase "Token")))
                  (STArrow (STBase "Bool") (STArrow (STBase "Char") (STBase stableToken)))
              )
          resultView = typeViewArrowResultViewForArity view 1

      typeViewDisplay resultView `shouldBe` STArrow (STBase "Char") (STBase "Token")
      Map.lookup "Token" (typeViewHeadIdentities resultView) `shouldBe` Just tokenTypeIdentity
      Map.lookup "Bool" (typeViewHeadIdentities resultView) `shouldBe` Nothing

    it "keys method parameter binder identities by stable names" $ do
      let bodyIdentity = typeBinderIdentityFromUnique (UniqueIdentity 206)
          paramIdentity = typeBinderIdentityFromUnique (UniqueIdentity 207)
          paramStableName = typeBinderIdentityStableName paramIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  setTypeViewBinderIdentities
                    (Map.singleton "a" bodyIdentity)
                    (methodTypeViewRaw eqMethodInfo),
                methodParamBinders = ("a", paramIdentity) :| []
              }

      Map.lookup "a" (typeViewBinderIdentities (methodTypeView methodInfo))
        `shouldBe` Just bodyIdentity
      Map.lookup paramStableName (typeViewBinderIdentities (methodTypeView methodInfo))
        `shouldBe` Just paramIdentity

    it "keys resolved type head identities by stable and display names" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          typeSymbol = resolvedDataInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Token" tokenDataInfo
          heads = typeViewHeadIdentities (typeViewFromResolved (RSTBase typeSymbol))
      Map.lookup stableToken heads `shouldBe` Just tokenTypeIdentity
      Map.lookup "Token" heads `shouldBe` Just tokenTypeIdentity
      Map.lookup "L.Token" heads `shouldBe` Just tokenTypeIdentity
      Map.lookup "Lib.Token" heads `shouldBe` Just tokenTypeIdentity

    it "does not key ambiguous same-named resolved type heads by display name" $ do
      let leftIdentity = generatedSymbolIdentity 130 SymbolType "Left" "Token" Nothing
          rightIdentity = generatedSymbolIdentity 131 SymbolType "Right" "Token" Nothing
          leftSymbol = mkResolvedSymbol leftIdentity "L.Token" "L.Token" (SymbolQualifiedImport "Left" "L")
          rightSymbol = mkResolvedSymbol rightIdentity "R.Token" "R.Token" (SymbolQualifiedImport "Right" "R")
          view = typeViewFromResolved (RSTArrow (RSTBase leftSymbol) (RSTBase rightSymbol))
          heads = typeViewHeadIdentities view
      Map.lookup (symbolIdentityStableName leftIdentity) heads `shouldBe` Just leftIdentity
      Map.lookup (symbolIdentityStableName rightIdentity) heads `shouldBe` Just rightIdentity
      Map.lookup "L.Token" heads `shouldBe` Just leftIdentity
      Map.lookup "R.Token" heads `shouldBe` Just rightIdentity
      Map.lookup "Token" heads `shouldBe` Nothing
      typeViewHeadIdentityForAlias view "Token" `shouldBe` Nothing

    it "keys scope type head identities by stable and source names" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          scope = mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty []
          heads = typeViewHeadIdentities (sourceTypeViewInScope scope (STBase "Token"))
      Map.lookup stableToken heads `shouldBe` Just tokenTypeIdentity
      Map.lookup "Token" heads `shouldBe` Just tokenTypeIdentity
      Map.lookup "Lib.Token" heads `shouldBe` Just tokenTypeIdentity

    it "does not key ambiguous same-named scope type heads by display name" $ do
      let leftIdentity = generatedSymbolIdentity 132 SymbolType "Left" "Token" Nothing
          rightIdentity = generatedSymbolIdentity 133 SymbolType "Right" "Token" Nothing
          leftInfo = DataInfo leftIdentity [] []
          rightInfo = DataInfo rightIdentity [] []
          scope =
            mkElaborateScope
              Map.empty
              (Map.fromList [("L.Token", leftInfo), ("R.Token", rightInfo)])
              Map.empty
              []
          heads = typeViewHeadIdentities (sourceTypeViewInScope scope (STArrow (STBase "L.Token") (STBase "R.Token")))
      Map.lookup (symbolIdentityStableName leftIdentity) heads `shouldBe` Just leftIdentity
      Map.lookup (symbolIdentityStableName rightIdentity) heads `shouldBe` Just rightIdentity
      Map.lookup "L.Token" heads `shouldBe` Just leftIdentity
      Map.lookup "R.Token" heads `shouldBe` Just rightIdentity
      Map.lookup "Token" heads `shouldBe` Nothing

    it "uses the stable identity name instead of choosing between multiple displays" $ do
      let sharedIdentity = generatedSymbolIdentity 134 SymbolType "Lib" "Token" Nothing
          sharedInfo = DataInfo sharedIdentity [] []
          stableToken = symbolIdentityStableName sharedIdentity
          scope =
            mkElaborateScope
              Map.empty
              (Map.fromList [("L.Token", sharedInfo), ("R.Token", sharedInfo)])
              Map.empty
              []
          view = sourceTypeViewInScope scope (STBase stableToken)
      typeViewDisplay view `shouldBe` STBase stableToken
      typeViewIdentity view `shouldBe` STBase stableToken

    it "prefers scoped type head identities before builtin spellings" $ do
      let localIntIdentity = generatedSymbolIdentity 135 SymbolType "Main" "Int" Nothing
          stableInt = symbolIdentityStableName localIntIdentity
          scope =
            mkElaborateScope
              Map.empty
              (Map.singleton "Int" (DataInfo localIntIdentity [] []))
              Map.empty
              []
          heads = typeViewHeadIdentities (sourceTypeViewInScope scope (STBase "Int"))
      sourceTypeIdentityInScope scope (STBase "Int") `shouldBe` STBase stableInt
      Map.lookup stableInt heads `shouldBe` Just localIntIdentity
      Map.lookup "Int" heads `shouldBe` Just localIntIdentity

    it "keys qualified builtin type heads by builtin identity" $ do
      let intIdentity = Builtins.builtinTypeIdentity "Int"
          stableInt = symbolIdentityStableName intIdentity
          qualifiedInt = Builtins.builtinModuleName ++ ".Int"
          qualifiedSymbol = Builtins.builtinTypeSymbol qualifiedInt
          view = sourceTypeViewInScope (mkElaborateScope Map.empty Map.empty Map.empty []) (STBase qualifiedInt)
          heads = typeViewHeadIdentities view
      resolvedSymbolIdentity qualifiedSymbol `shouldBe` intIdentity
      refDisplayName qualifiedSymbol `shouldBe` "Int"
      typeViewIdentity view `shouldBe` STBase stableInt
      Map.lookup qualifiedInt heads `shouldBe` Just intIdentity
      Map.lookup "Int" heads `shouldBe` Just intIdentity
      Map.lookup stableInt heads `shouldBe` Just intIdentity

    it "keeps builtin value type head identities" $ do
      case Map.lookup "__string_from_int" Builtins.builtinValues of
        Just builtinValue@OrdinaryValue {} -> do
          let valueView = ordinaryValueTypeView builtinValue
              identityTy = typeViewIdentity valueView
              heads = typeViewHeadIdentities valueView
          let builtinHead name = Builtins.builtinModuleName ++ "." ++ name
          identityTy
            `shouldBe` STArrow
              (STBase (symbolIdentityStableName (Builtins.builtinTypeIdentity "Int")))
              (STBase (symbolIdentityStableName (Builtins.builtinTypeIdentity "String")))
          Map.lookup (builtinHead "Int") heads
            `shouldBe` Just (Builtins.builtinTypeIdentity "Int")
          Map.lookup (builtinHead "String") heads
            `shouldBe` Just (Builtins.builtinTypeIdentity "String")
        other -> expectationFailure ("expected builtin ordinary value, got " ++ show other)

    it "collects mentioned type head identities through aliases" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            setTypeViewHeadIdentities
              (Map.singleton "Lib.Token" tokenTypeIdentity)
              (mkTypeView (STBase "Token") (STBase stableToken))
      typeViewMentionedHeadIdentities view `shouldBe` Map.keysSet (Map.singleton tokenTypeIdentity ())

    it "assigns identities to builtin opaque type parameters" $ do
      case (Map.lookup "IO" Builtins.builtinOpaqueTypes, Map.lookup "IORef" Builtins.builtinOpaqueTypes) of
        (Just DataInfo {dataTypeParams = [ioParam]}, Just DataInfo {dataTypeParams = [ioRefParam]}) -> do
          checkedTypeParamName ioParam `shouldBe` "a"
          checkedTypeParamName ioRefParam `shouldBe` "a"
          checkedTypeParamIdentity ioParam `shouldNotBe` checkedTypeParamIdentity ioRefParam
        other -> expectationFailure ("expected IO and IORef opaque params, got " ++ show other)

    it "keeps same-named type binders distinct in identity types" $ do
      let outer = resolvedTypeBinderRef (UniqueIdentity 201) "a"
          inner = resolvedTypeBinderRef (UniqueIdentity 202) "a"
          ty =
            RSTForall
              outer
              Nothing
              (RSTForall inner Nothing (RSTArrow (RSTVar outer) (RSTVar inner)))

      resolvedSrcTypeIdentityType ty
        `shouldBe` STForall
          "$typevar#201"
          Nothing
          ( STForall
              "$typevar#202"
              Nothing
              (STArrow (STVar "$typevar#201") (STVar "$typevar#202"))
          )

    it "keys unique resolved type binder displays by identity" $ do
      let ref = resolvedTypeBinderRef (UniqueIdentity 203) "a"
          identity = typeBinderIdentityFromUnique (UniqueIdentity 203)
          view = typeViewFromResolved (RSTForall ref Nothing (RSTVar ref))

      resolvedTypeBinderTypeIdentity ref `shouldBe` identity
      Map.lookup "a" (typeViewBinderIdentities view) `shouldBe` Just identity
      Map.lookup "$typevar#203" (typeViewBinderIdentities view) `shouldBe` Just identity

    it "constructs resolved type views with carried head and binder identities" $ do
      let ref = resolvedTypeBinderRef (UniqueIdentity 204) "a"
          tokenSymbol = resolvedDataInfoSymbol (SymbolLocal "Lib") "Token" tokenDataInfo
          resolvedView =
            typeViewFromResolved
              (RSTForall ref Nothing (RSTArrow (RSTBase tokenSymbol) (RSTVar ref)))
      Map.lookup "Token" (typeViewHeadIdentities resolvedView)
        `shouldBe` Just (resolvedSymbolIdentity tokenSymbol)
      Map.lookup "a" (typeViewBinderIdentities resolvedView)
        `shouldBe` Just (resolvedTypeBinderTypeIdentity ref)

    it "compares type views by identity metadata when display names are stale" $ do
      let headName = symbolIdentityStableName tokenTypeIdentity
          binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 208)
          binderName = typeBinderIdentityStableName binderIdentity
          view displayName headKey binderKey =
            fixtureTypeView
              (STArrow (STBase displayName) (STVar "a"))
              (STArrow (STBase headName) (STVar binderName))
              (Map.singleton headKey tokenTypeIdentity)
              (Map.singleton binderKey binderIdentity)
          staleView = view "$stale.Token" "$stale.Token" "$stale_a"
      view "Token" "Token" "a" `shouldBe` staleView
      setTypeViewBinderIdentities Map.empty staleView `shouldBe` staleView

    it "compares type views by carried identities when identity names are stale" $ do
      let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 209)
          view headName binderName =
            fixtureTypeView
              (STForall "a" Nothing (STArrow (STBase "Token") (STVar "a")))
              (STForall binderName Nothing (STArrow (STBase headName) (STVar binderName)))
              (Map.singleton headName tokenTypeIdentity)
              (Map.singleton binderName binderIdentity)
          staleView = view "$stale.Token" "$stale_a"
      view "Token" "a" `shouldBe` staleView
      setTypeViewHeadIdentities Map.empty staleView `shouldBe` staleView

    it "does not compare type views equal when carried head payloads conflict" $ do
      let originalIdentity = generatedSymbolIdentity 210 SymbolType "Lib" "Token" Nothing
          conflictingIdentity = generatedSymbolIdentity 210 SymbolType "Other" "StaleToken" Nothing
          headName = symbolIdentityStableName originalIdentity
          view identity =
            fixtureTypeView
              (STBase "Token")
              (STBase headName)
              (Map.singleton "Token" identity)
              Map.empty
      view originalIdentity `shouldNotBe` view conflictingIdentity

    it "keys class applications and evidence methods by carried type identities" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view identityName headKey =
            setTypeViewHeadIdentities
              (Map.singleton headKey tokenTypeIdentity)
              (mkTypeView (STBase "Token") (STBase identityName))
          visibleView = view stableToken "Token"
          staleView = view "$stale.Token" "$stale.Token"
          otherIdentity = generatedSymbolIdentity 211 SymbolType "Other" "Token" Nothing
          conflictingView =
            setTypeViewHeadIdentities
              (Map.singleton "$stale.Token" otherIdentity)
              staleView
      classApplicationKey eqClassIdentity (visibleView :| [])
        `shouldBe` classApplicationKey eqClassIdentity (staleView :| [])
      classApplicationKey eqClassIdentity (visibleView :| [])
        `shouldNotBe` classApplicationKey eqClassIdentity (conflictingView :| [])
      evidenceMethodKey eqClassIdentity (visibleView :| []) eqMethodIdentity
        `shouldBe` evidenceMethodKey eqClassIdentity (staleView :| []) eqMethodIdentity

    it "compares constraint infos by class identity when display names are stale" $ do
      let tokenView = sourceTypeViewInScope (mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty []) (STBase "Token")
          constraint displayName classIdentity =
            ConstraintInfo
              { constraintDisplayClass = displayName,
                constraintClassSymbol = classIdentity,
                constraintTypeViews = tokenView :| []
              }
          otherClassIdentity = generatedSymbolIdentity 134 SymbolClass "Other" "Eq" Nothing
      constraint "Eq" eqClassIdentity
        `shouldBe` constraint "$stale.Eq" eqClassIdentity
      constraint "Eq" eqClassIdentity
        `shouldNotBe` constraint "Eq" otherClassIdentity

    it "does not look up class constraint metadata through stale identity payloads" $ do
      let tokenView = sourceTypeViewInScope (mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty []) (STBase "Token")
          staleClassIdentity = renameSymbolDefiningName "$stale.Eq" eqClassIdentity
          constraint classIdentity =
            ConstraintInfo
              { constraintDisplayClass = "Eq",
                constraintClassSymbol = classIdentity,
                constraintTypeViews = tokenView :| []
              }
          scope =
            mkElaborateScope Map.empty Map.empty (Map.singleton "Eq" eqClassInfo) []
          staleScope =
            mkElaborateScope Map.empty Map.empty (Map.singleton "Eq" (eqClassInfo {classInfoSymbol = staleClassIdentity})) []

      classInfoForConstraint scope (constraint eqClassIdentity) `shouldBe` Just eqClassInfo
      classInfoForConstraint scope (constraint staleClassIdentity) `shouldBe` Nothing
      classInfoForConstraint staleScope (constraint eqClassIdentity) `shouldBe` Nothing

    it "does not look up constructor owner metadata through stale identity payloads" $ do
      let staleTokenIdentity = renameSymbolDefiningName "$stale.Token" tokenTypeIdentity
          staleOwnerInfo =
            DataInfo
              { dataInfoSymbol = staleTokenIdentity,
                dataTypeParams = [],
                dataConstructors = [higherCtor]
              }
          staleOwnersByIdentity = Map.singleton staleTokenIdentity staleOwnerInfo

      constructorOwnerRuntimeTypeTrackable staleOwnersByIdentity someCtor `shouldBe` True
      constructorOwnerHasVariableHeadApplication staleOwnersByIdentity someCtor `shouldBe` False

    it "compares evidence methods by symbol identity" $ do
      let methodView = methodTypeView eqMethodInfo
          evidence symbol =
            EvidenceMethod
              { evidenceMethodSymbol = symbol,
                evidenceMethodResolvedVar = resolvedVarFromValueInfo valueInfo Elab.TBottom,
                evidenceMethodTypeView = methodView
              }
      evidence eqMethodIdentity `shouldBe` evidence eqMethodIdentity
      evidence eqMethodIdentity `shouldNotBe` evidence someCtorIdentity

    it "does not choose arbitrary evidence methods after duplicate collapse" $ do
      let methodView = methodTypeView eqMethodInfo
          conflictingView = mkTypeView (STBase "Bool") (STBase "Bool")
          evidence view =
            EvidenceMethod
              { evidenceMethodSymbol = eqMethodIdentity,
                evidenceMethodResolvedVar = resolvedVarFromValueInfo valueInfo Elab.TBottom,
                evidenceMethodTypeView = view
              }
          first = evidence methodView
          duplicate = evidence methodView
          conflicting = evidence conflictingView
      uniqueEvidenceMethod [first, duplicate] `shouldBe` Just first
      uniqueEvidenceMethod [first, conflicting] `shouldBe` Nothing

    it "does not choose arbitrary evidence method substitutions after duplicate collapse" $ do
      let methodView = methodTypeView eqMethodInfo
          replacement = mkTypeView (STBase "Int") (STBase "Int")
          conflictingReplacement = mkTypeView (STBase "Bool") (STBase "Bool")
          binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 210)
          binderKey = binderIdentity
          evidence =
            EvidenceMethod
              { evidenceMethodSymbol = eqMethodIdentity,
                evidenceMethodResolvedVar = resolvedVarFromValueInfo valueInfo Elab.TBottom,
                evidenceMethodTypeView = methodView
              }
          first = (evidence, Map.singleton binderKey replacement)
          duplicate = (evidence, Map.singleton binderKey replacement)
          conflicting = (evidence, Map.singleton binderKey conflictingReplacement)
      uniqueEvidenceMethodMatch [first, duplicate] `shouldBe` Just first
      uniqueEvidenceMethodMatch [first, conflicting] `shouldBe` Nothing

    it "compares method infos by symbol identity when display names are stale" $ do
      eqMethodInfo
        `shouldBe` eqMethodInfo {methodDisplayName = "$stale.eq"}
      eqMethodInfo
        `shouldNotBe` eqMethodInfo {methodInfoSymbol = someCtorIdentity}

    it "uses resolved constraint metadata for method class value and instance equality" $ do
      let scope = mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty []
          tokenView = sourceTypeViewInScope scope (STBase "Token")
          constraintInfo =
            ConstraintInfo
              { constraintDisplayClass = "Eq",
                constraintClassSymbol = eqClassIdentity,
                constraintTypeViews = tokenView :| []
              }
          staleConstraintInfo =
            constraintInfo
              { constraintDisplayClass = "$stale.Eq",
                constraintTypeViews = setTypeViewDisplay (STBase "$stale.Token") tokenView :| []
              }
          paramIdentity = typeBinderIdentityFromUnique (UniqueIdentity 211)
          methodWith infos paramName =
            eqMethodInfo
              { methodConstraintInfos = infos,
                methodParamBinders = (paramName, paramIdentity) :| []
              }
          classWith infos =
            eqClassInfo
              { classSuperclassInfos = infos
              }
          valueWith runtimeName infos =
            OrdinaryValue
              { valueRuntimeName = runtimeName,
                valueInfoSymbol = valueInfoIdentity,
                valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                valueConstraintInfos = infos
              }
          instanceOrigin = generatedSymbolIdentity 135 SymbolModule "Lib" "Lib" Nothing
          instanceWith infos =
            InstanceInfo
              { instanceClassSymbol = eqClassIdentity,
                instanceOriginModuleIdentity = instanceOrigin,
                instanceConstraintInfos = infos,
                instanceHeadTypeViews = tokenView :| [],
                instanceMethodsByIdentity = Map.empty
              }
          constraintInfos = [constraintInfo]
          staleConstraintInfos = [staleConstraintInfo]
      methodWith constraintInfos "a"
        `shouldBe` methodWith staleConstraintInfos "$stale_a"
      methodWith constraintInfos "a"
        `shouldNotBe` methodWith [] "$stale_a"
      classWith constraintInfos
        `shouldBe` classWith staleConstraintInfos
      valueWith "Lib__answer" constraintInfos
        `shouldBe` valueWith "$stale.answer" staleConstraintInfos
      instanceWith constraintInfos
        `shouldBe` instanceWith staleConstraintInfos

    it "compares export metadata by identity payloads when display maps are stale" $ do
      let exportedType =
            mkExportedTypeInfo tokenDataInfo [("Some", someCtor)]
          staleExportedType =
            exportedType
              { exportedTypeConstructorDisplaysByIdentity =
                  Map.singleton someCtorIdentity "$stale.Some"
              }
          exports =
            moduleExportsFromMaps
              (Map.singleton "answer" valueInfo)
              (Map.singleton "Token" exportedType)
              (Map.singleton "Eq" eqClassInfo)
          staleExports =
            exports
              { exportedValueDisplaysByIdentity = Map.singleton valueInfoIdentity "$stale.answer",
                exportedTypeDisplaysByIdentity = Map.singleton tokenTypeIdentity "$stale.Token",
                exportedClassDisplaysByIdentity = Map.singleton eqClassIdentity "$stale.Eq"
              }
      exportedType `shouldBe` staleExportedType
      exportedType
        `shouldNotBe` staleExportedType {exportedTypeConstructorsByIdentity = Map.empty}
      exports `shouldBe` staleExports
      exports
        `shouldNotBe` staleExports {exportedValuesByIdentity = Map.empty}

    it "compares checked modules by module identity when display names are stale" $ do
      let moduleIdentity = generatedSymbolIdentity 136 SymbolModule "Lib" "Lib" Nothing
          otherModuleIdentity = generatedSymbolIdentity 137 SymbolModule "Other" "Lib" Nothing
          exports =
            moduleExportsFromMaps
              (Map.singleton "answer" valueInfo)
              (Map.singleton "Token" (mkExportedTypeInfo tokenDataInfo [("Some", someCtor)]))
              (Map.singleton "Eq" eqClassInfo)
          checkedModule name identity =
            CheckedModule
              { checkedModuleName = name,
                checkedModuleIdentity = identity,
                checkedModuleBindings = [],
                checkedModuleData = Map.singleton tokenTypeIdentity tokenDataInfo,
                checkedModuleClasses = Map.singleton eqClassIdentity eqClassInfo,
                checkedModuleInstances = [],
                checkedModuleExports = exports
              }
      checkedModule "Lib" moduleIdentity
        `shouldBe` checkedModule "$stale.Lib" moduleIdentity
      checkedModule "Lib" moduleIdentity
        `shouldNotBe` checkedModule "Lib" otherModuleIdentity

    it "rejects metadata equality when symbol identity payloads conflict" $ do
      let staleValueIdentity = renameSymbolDefiningName "$stale.answer" valueInfoIdentity
          staleTokenIdentity = renameSymbolDefiningName "$stale.Token" tokenTypeIdentity
          staleCtorIdentity = renameSymbolDefiningName "$stale.Some" someCtorIdentity
          staleClassIdentity = renameSymbolDefiningName "$stale.Eq" eqClassIdentity
          staleMethodIdentity = renameSymbolDefiningName "$stale.eq" eqMethodIdentity
          moduleIdentity = generatedSymbolIdentity 136 SymbolModule "Lib" "Lib" Nothing
          staleModuleIdentity = renameSymbolDefiningName "$stale.Lib" moduleIdentity
          methodEvidence =
            EvidenceMethod
              { evidenceMethodSymbol = eqMethodIdentity,
                evidenceMethodResolvedVar = resolvedVarFromValueInfo valueInfo Elab.TBottom,
                evidenceMethodTypeView = methodTypeView eqMethodInfo
              }
          evidence =
            EvidenceInfo
              { evidenceClassSymbol = eqClassIdentity,
                evidenceTypeViews = methodTypeView eqMethodInfo :| [],
                evidenceMethodsByIdentity = Map.singleton eqMethodIdentity methodEvidence
              }
          tokenView =
            sourceTypeViewInScope
              (mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty [])
              (STBase "Token")
          instanceInfo =
            InstanceInfo
              { instanceClassSymbol = eqClassIdentity,
                instanceOriginModuleIdentity = moduleIdentity,
                instanceConstraintInfos = [],
                instanceHeadTypeViews = tokenView :| [],
                instanceMethodsByIdentity = Map.singleton eqMethodIdentity eqMethodValue
              }
          exportedType =
            mkExportedTypeInfo tokenDataInfo [("Some", someCtor)]
          exports =
            moduleExportsFromMaps
              (Map.singleton "answer" valueInfo)
              (Map.singleton "Token" exportedType)
              (Map.singleton "Eq" eqClassInfo)
          checkedModule =
            CheckedModule
              { checkedModuleName = "Lib",
                checkedModuleIdentity = moduleIdentity,
                checkedModuleBindings = [],
                checkedModuleData = Map.singleton tokenTypeIdentity tokenDataInfo,
                checkedModuleClasses = Map.singleton eqClassIdentity eqClassInfo,
                checkedModuleInstances = [],
                checkedModuleExports = exports
              }

      valueInfo `shouldNotBe` valueInfo {valueInfoSymbol = staleValueIdentity}
      eqMethodInfo `shouldNotBe` eqMethodInfo {methodInfoSymbol = staleMethodIdentity}
      eqClassInfo `shouldNotBe` eqClassInfo {classInfoSymbol = staleClassIdentity}
      eqClassInfo
        `shouldNotBe` eqClassInfo {classMethodsByIdentity = Map.singleton staleMethodIdentity eqMethodInfo}
      someCtor `shouldNotBe` someCtor {ctorInfoSymbol = staleCtorIdentity}
      someCtor `shouldNotBe` someCtor {ctorOwningTypeIdentity = staleTokenIdentity}
      tokenDataInfo `shouldNotBe` tokenDataInfo {dataInfoSymbol = staleTokenIdentity}
      evidence `shouldNotBe` evidence {evidenceClassSymbol = staleClassIdentity}
      evidence
        `shouldNotBe` evidence {evidenceMethodsByIdentity = Map.singleton staleMethodIdentity methodEvidence}
      instanceInfo `shouldNotBe` instanceInfo {instanceClassSymbol = staleClassIdentity}
      instanceInfo `shouldNotBe` instanceInfo {instanceOriginModuleIdentity = staleModuleIdentity}
      instanceInfo
        `shouldNotBe` instanceInfo {instanceMethodsByIdentity = Map.singleton staleMethodIdentity eqMethodValue}
      exportedType
        `shouldNotBe` exportedType {exportedTypeConstructorsByIdentity = Map.singleton staleCtorIdentity someCtor}
      exports
        `shouldNotBe` exports {exportedValuesByIdentity = Map.singleton staleValueIdentity valueInfo}
      exports
        `shouldNotBe` exports {exportedTypesByIdentity = Map.singleton staleTokenIdentity exportedType}
      exports
        `shouldNotBe` exports {exportedClassesByIdentity = Map.singleton staleClassIdentity eqClassInfo}
      checkedModule `shouldNotBe` checkedModule {checkedModuleIdentity = staleModuleIdentity}
      checkedModule
        `shouldNotBe` checkedModule {checkedModuleData = Map.singleton staleTokenIdentity tokenDataInfo}
      checkedModule
        `shouldNotBe` checkedModule {checkedModuleClasses = Map.singleton staleClassIdentity eqClassInfo}

    it "compares deferred constructor inst binders by identity when names are stale" $ do
      let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 212)
          otherBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 213)
          deferredRef = deferredRefFromIdentity (UniqueIdentity 214) "Some"
          tokenStableName = symbolIdentityStableName tokenTypeIdentity
          deferredTypeView =
            setTypeViewHeadIdentities
              ( Map.fromList
                  [ ("Token", tokenTypeIdentity),
                    (tokenStableName, tokenTypeIdentity)
                  ]
              )
              (mkTypeView (STBase "Token") (STBase tokenStableName))
          call binderName identity =
            DeferredConstructorCall
              { deferredConstructorRef = deferredRef,
                deferredConstructorInfo = someCtor,
                deferredConstructorArgCount = 0,
                deferredConstructorSourceTypeView = deferredTypeView,
                deferredConstructorOccurrenceTypeView = deferredTypeView,
                deferredConstructorInstBinders = [(binderName, identity)],
                deferredConstructorInitialSubst = emptyTypeBinderSubst,
                deferredConstructorBindingMode = DeferredBindingScheme
              }
      call "a" binderIdentity
        `shouldBe` call "$stale_a" binderIdentity
      call "a" binderIdentity
        `shouldNotBe` call "a" otherBinderIdentity

    it "compares constructor metadata by identity when runtime names are stale" $ do
      let shape =
            ConstructorShape
              { constructorShapeSymbol = someCtorIdentity,
                constructorShapeRuntimeName = "Lib__Some",
                constructorShapeTypeView = ctorTypeView someCtor,
                constructorShapeIndex = 0,
                constructorShapeOwnerTypeParams = []
              }
          staleShape =
            shape
              { constructorShapeRuntimeName = "$stale.Some",
                constructorShapeTypeView =
                  setTypeViewDisplay (STBase "$stale.Token") (constructorShapeTypeView shape)
              }
          staleCtor =
            someCtor
              { ctorRuntimeName = "$stale.Some",
                ctorTypeView = setTypeViewDisplay (STBase "$stale.Token") (ctorTypeView someCtor)
              }
      shape `shouldBe` staleShape
      shape `shouldNotBe` staleShape {constructorShapeSymbol = higherCtorIdentity}
      someCtor `shouldBe` staleCtor
      someCtor `shouldNotBe` staleCtor {ctorInfoSymbol = higherCtorIdentity}
      constructorShapeRuntimeName (constructorShapeFromInfo staleCtor) `shouldBe` "Lib__Some"
      case dataConstructors (constructorOwnerDataInfoFromShapes staleCtor) of
        [roundTrippedCtor] ->
          ctorRuntimeName roundTrippedCtor `shouldBe` "Lib__Some"
        other ->
          expectationFailure ("expected one round-tripped constructor, got " ++ show other)

    it "compares constructor forall binders by identity when display names are stale" $ do
      let identity = typeBinderIdentityFromUnique (UniqueIdentity 209)
          otherIdentity = typeBinderIdentityFromUnique (UniqueIdentity 210)
      ConstructorForallBinder "a" identity
        `shouldBe` ConstructorForallBinder "$stale_a" identity
      ConstructorForallBinder "a" identity
        `shouldNotBe` ConstructorForallBinder "a" otherIdentity

    it "does not key ambiguous same-named resolved type binder displays" $ do
      let outer = resolvedTypeBinderRef (UniqueIdentity 204) "a"
          inner = resolvedTypeBinderRef (UniqueIdentity 205) "a"
          outerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 204)
          innerIdentity = typeBinderIdentityFromUnique (UniqueIdentity 205)
          view =
            typeViewFromResolved
              (RSTForall outer Nothing (RSTForall inner Nothing (RSTArrow (RSTVar outer) (RSTVar inner))))

      Map.lookup "a" (typeViewBinderIdentities view) `shouldBe` Nothing
      Map.lookup "$typevar#204" (typeViewBinderIdentities view) `shouldBe` Just outerIdentity
      Map.lookup "$typevar#205" (typeViewBinderIdentities view) `shouldBe` Just innerIdentity

    it "lowers value sidecars from semantic identity instead of imported spelling" $ do
      let qualifiedValueIdentity = loweredBindingIdentityFromValueInfo valueInfo
          staleRuntimeValue =
            OrdinaryValue
              { valueInfoSymbol = valueInfoIdentity,
                valueRuntimeName = "$stale.answer",
                valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                valueConstraintInfos = []
              }
          staleRuntimeValueIdentity =
            loweredBindingIdentityFromValueInfo staleRuntimeValue
          localValueIdentity =
            generatedSymbolIdentity 301 SymbolValue "<local>" "$local_answer" Nothing
          localValue =
            OrdinaryValue
              { valueInfoSymbol = localValueIdentity,
                valueRuntimeName = "$stale.local_answer",
                valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                valueConstraintInfos = []
              }
          localLoweredIdentity =
            loweredBindingIdentityFromValueInfo localValue
          qualifiedMethodIdentity = loweredBindingIdentityFromValueInfo qualifiedEqMethodValue
          staleRuntimeCtor =
            someCtor
              { ctorRuntimeName = "$stale.Some"
              }
          staleRuntimeConstructorValue =
            ConstructorValue
              { valueInfoSymbol = constructorInfoSymbolIdentity tokenDataInfo staleRuntimeCtor,
                valueRuntimeName = "$also_stale.Some",
                valueCtorInfo = staleRuntimeCtor
              }
          staleRuntimeConstructorIdentity =
            loweredBindingIdentityFromValueInfo staleRuntimeConstructorValue

      loweredIdentityRuntimeName qualifiedValueIdentity `shouldBe` "Lib__answer"
      loweredIdentityDetails qualifiedValueIdentity
        `shouldBe` TopLevelId valueInfoIdentity
      loweredIdentityRuntimeName staleRuntimeValueIdentity `shouldBe` "Lib__answer"
      loweredIdentityDetails staleRuntimeValueIdentity
        `shouldBe` TopLevelId valueInfoIdentity
      valueInfoRuntimeName staleRuntimeValue `shouldBe` "Lib__answer"
      loweredIdentityRuntimeName localLoweredIdentity `shouldBe` "$local_answer"
      loweredIdentityDetails localLoweredIdentity
        `shouldBe` TopLevelId localValueIdentity
      valueInfoRuntimeName localValue `shouldBe` "$local_answer"
      loweredIdentityRuntimeName qualifiedMethodIdentity `shouldBe` "eq"
      loweredIdentityDetails qualifiedMethodIdentity
        `shouldBe` MethodId (methodInfoSymbolIdentity qualifiedEqMethodInfo)
      constructorInfoRuntimeName staleRuntimeCtor `shouldBe` "Lib__Some"
      valueInfoRuntimeName staleRuntimeConstructorValue `shouldBe` "Lib__Some"
      loweredIdentityRuntimeName staleRuntimeConstructorIdentity `shouldBe` "Lib__Some"
      loweredIdentityDetails staleRuntimeConstructorIdentity
        `shouldBe` ConstructorId (constructorRefFromInfo staleRuntimeCtor)

  describe "substituteTypeVar" $ do
    it "composes variable-headed type application heads with partially applied constructors" $
      substituteTypeVar "f" (STCon "Either" (STBase "Int" :| [])) (STVarApp "f" (STVar "a" :| []))
        `shouldBe` STCon "Either" (STBase "Int" :| [STVar "a"])

    it "composes variable-headed type application heads with partially applied variable heads" $
      substituteTypeVar "f" (STVarApp "g" (STBase "Int" :| [])) (STVarApp "f" (STVar "a" :| []))
        `shouldBe` STVarApp "g" (STBase "Int" :| [STVar "a"])

  describe "higher-kinded source type lowering" $ do
    it "composes variable-headed constructor fields with partially applied constructor arguments" $ do
      let scope = mkElaborateScope Map.empty (Map.singleton "Higher" higherDataInfo) Map.empty []
          lowered = lowerType scope (STCon "Higher" (STCon "Either" (STBase "Int" :| []) :| [STBase "Bool"]))
          expectedField = STCon "Either" (STBase "Int" :| [STBase "Bool"])
      lowered `shouldSatisfy` containsSrcType expectedField
      lowered `shouldSatisfy` (not . containsVarAppHead "f")

containsSrcType :: SrcType -> SrcType -> Bool
containsSrcType needle ty
  | needle == ty = True
  | otherwise =
      case ty of
        STArrow dom cod -> containsSrcType needle dom || containsSrcType needle cod
        STCon _ args -> any (containsSrcType needle) (toListNE args)
        STVarApp _ args -> any (containsSrcType needle) (toListNE args)
        STTyLam _ body -> containsSrcType needle body
        STTyApp fun arg -> containsSrcType needle fun || containsSrcType needle arg
        STForall _ mb body -> maybe False (containsSrcType needle . unSrcBound) mb || containsSrcType needle body
        STMu _ body -> containsSrcType needle body
        STVar {} -> False
        STBase {} -> False
        STBottom -> False

containsVarAppHead :: String -> SrcType -> Bool
containsVarAppHead needle ty =
  case ty of
    STArrow dom cod -> containsVarAppHead needle dom || containsVarAppHead needle cod
    STCon _ args -> any (containsVarAppHead needle) (toListNE args)
    STVarApp name args -> name == needle || any (containsVarAppHead needle) (toListNE args)
    STTyLam _ body -> containsVarAppHead needle body
    STTyApp fun arg -> containsVarAppHead needle fun || containsVarAppHead needle arg
    STForall _ mb body -> maybe False (containsVarAppHead needle . unSrcBound) mb || containsVarAppHead needle body
    STMu _ body -> containsVarAppHead needle body
    STVar {} -> False
    STBase {} -> False
    STBottom -> False

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

valueInfo :: ValueInfo
valueInfo =
  OrdinaryValue
    { valueInfoSymbol = valueInfoIdentity,
      valueRuntimeName = "Lib__answer",
      valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
      valueConstraintInfos = []
    }

mainValueInfo :: ValueInfo
mainValueInfo =
  OrdinaryValue
    { valueInfoSymbol = mainValueIdentity,
      valueRuntimeName = "Main__main",
      valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
      valueConstraintInfos = []
    }

someCtor :: ConstructorInfo
someCtor =
  ConstructorInfo
    { ctorInfoSymbol = someCtorIdentity,
      ctorRuntimeName = "Lib__Some",
      ctorTypeView = mkTypeView (STBase "Token") (STBase "Lib.Token"),
      ctorOwningTypeIdentity = tokenTypeIdentity,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

tokenDataInfo :: DataInfo
tokenDataInfo =
  DataInfo
    { dataInfoSymbol = tokenTypeIdentity,
      dataTypeParams = [],
      dataConstructors = [someCtor]
    }

higherCtor :: ConstructorInfo
higherCtor =
  ConstructorInfo
    { ctorInfoSymbol = higherCtorIdentity,
      ctorRuntimeName = "Lib__Higher",
      ctorTypeView =
        fixtureTypeView
          ( STArrow
              (STVarApp "f" (STVar "a" :| []))
              (STCon "Higher" (STVar "f" :| [STVar "a"]))
          )
          ( STArrow
              (STVarApp "f" (STVar "a" :| []))
              (STCon "Lib.Higher" (STVar "f" :| [STVar "a"]))
          )
          (Map.fromList [("Higher", higherTypeIdentity), ("Lib.Higher", higherTypeIdentity)])
          (Map.fromList [("f", higherFIdentity), ("a", higherAIdentity)]),
      ctorOwningTypeIdentity = higherTypeIdentity,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

higherDataInfo :: DataInfo
higherDataInfo =
  DataInfo
    { dataInfoSymbol = higherTypeIdentity,
      dataTypeParams =
        [ CheckedTypeParam (resolvedTypeBinderRefFromIdentity higherFIdentity "f") KType,
          CheckedTypeParam (resolvedTypeBinderRefFromIdentity higherAIdentity "a") KType
        ],
      dataConstructors = [higherCtor]
    }

eqMethodInfo :: MethodInfo
eqMethodInfo =
  MethodInfo
    { methodInfoSymbol = eqMethodIdentity,
      methodDisplayName = "eq",
      methodTypeViewRaw =
        mkTypeView
          (STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")))
          (STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool"))),
      methodConstraintInfos = [],
      methodParamBinders = ("a", eqParamIdentity) :| []
    }

qualifiedEqMethodInfo :: MethodInfo
qualifiedEqMethodInfo =
  eqMethodInfo

eqClassInfo :: ClassInfo
eqClassInfo =
  ClassInfo
    { classInfoSymbol = eqClassIdentity,
      classTypeParams = eqParam :| [],
      classSuperclassInfos = [],
      classFunctionalDependencies = [],
      classMethodsByIdentity = Map.singleton (methodInfoSymbolIdentity eqMethodInfo) eqMethodInfo
    }

qualifiedEqClassInfo :: ClassInfo
qualifiedEqClassInfo =
  ClassInfo
    { classInfoSymbol = eqClassIdentity,
      classTypeParams = eqParam :| [],
      classSuperclassInfos = [],
      classFunctionalDependencies = [],
      classMethodsByIdentity = Map.singleton (methodInfoSymbolIdentity qualifiedEqMethodInfo) qualifiedEqMethodInfo
    }

eqMethodValue :: ValueInfo
eqMethodValue =
  OverloadedMethod
    { valueInfoSymbol = methodInfoSymbolIdentity eqMethodInfo,
      valueMethodInfo = eqMethodInfo
    }

qualifiedEqMethodValue :: ValueInfo
qualifiedEqMethodValue =
  OverloadedMethod
    { valueInfoSymbol = methodInfoSymbolIdentity qualifiedEqMethodInfo,
      valueMethodInfo = qualifiedEqMethodInfo
    }
