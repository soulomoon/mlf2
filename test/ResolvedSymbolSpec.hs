{-# LANGUAGE GADTs #-}

module ResolvedSymbolSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Elaborate (lowerType, mkElaborateScope, sourceTypeViewInScope)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax.Program (ClassConstraintF (..), resolvedExportTypeRefFromSymbols, refDisplayName)
import qualified MLF.Frontend.Symbol as Symbol
import MLF.Frontend.Symbol
  ( symbolIdentityAliasMapWith,
    symbolIdentityStableName,
    symbolRefMatches,
  )
import MLF.Frontend.Syntax
  ( ResolvedSrcTy (..),
    ResolvedTypeBinderRef,
    resolvedTypeBinderName,
    resolvedTypeBinderRefFromIdentity,
    SrcKind (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    TypeParam (..),
    resolvedSrcTypeIdentityType,
    resolvedTypeBinderTypeIdentity,
    typeParamName,
    typeParamRef,
  )
import MLF.Types.Identity (TypeBinderIdentity, UniqueIdentity (..), typeBinderIdentityFromUnique, typeBinderIdentityStableName)
import Test.Hspec

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

eqParam :: TypeParam
eqParam =
  ResolvedTypeParam (resolvedTypeBinderRef eqParamUnique "a") KType

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

    it "does not match symbol refs through stable identity names without metadata" $ do
      let stableName = symbolIdentityStableName valueInfoIdentity

      symbolRefMatches (Just valueInfoIdentity) "stale-answer" Nothing stableName `shouldBe` False
      symbolRefMatches Nothing stableName (Just valueInfoIdentity) "answer" `shouldBe` False
      symbolRefMatches (Just valueInfoIdentity) "answer" Nothing "answer" `shouldBe` False
      symbolRefMatches
        (Just valueInfoIdentity)
        "answer"
        (Just (generatedSymbolIdentity 101 SymbolValue "Lib" "stale-answer" Nothing))
        "stale-answer"
        `shouldBe` False
      symbolRefMatches (Just valueInfoIdentity) "answer" (Just valueInfoIdentity) "renamed-answer" `shouldBe` True

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
                  (methodTypeViewRaw eqMethodInfo)
                    { typeViewDisplay = STBase "Token",
                      typeViewIdentity = STBase stableToken,
                      typeViewHeadIdentities = Map.singleton stableToken tokenTypeIdentity
                    }
              }

      typeViewHeadIdentities (methodTypeView methodInfo)
        `shouldBe` Map.singleton stableToken tokenTypeIdentity
      typeViewHeadIdentities (methodResultTypeView methodInfo)
        `shouldBe` Map.fromList [(stableToken, tokenTypeIdentity), ("Token", tokenTypeIdentity)]

    it "keeps method result head identities by payload stable name" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  (methodTypeViewRaw eqMethodInfo)
                    { typeViewDisplay = STBase stableToken,
                      typeViewIdentity = STBase stableToken,
                      typeViewHeadIdentities = Map.singleton "Token" tokenTypeIdentity
                    }
              }

      typeViewHeadIdentities (methodResultTypeView methodInfo)
        `shouldBe` Map.singleton "Token" tokenTypeIdentity

    it "keeps method parameter head identities by payload stable name" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          methodInfo =
            eqMethodInfo
              { methodTypeViewRaw =
                  (methodTypeViewRaw eqMethodInfo)
                    { typeViewDisplay = STArrow (STBase stableToken) (STBase "Bool"),
                      typeViewIdentity = STArrow (STBase stableToken) (STBase "Bool"),
                      typeViewHeadIdentities = Map.singleton "Token" tokenTypeIdentity
                    }
              }

      map typeViewHeadIdentities (methodParamTypeViews (methodTypeView methodInfo))
        `shouldBe` [Map.singleton "Token" tokenTypeIdentity]

    it "keeps projected method result head identities through display pairs" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            (mkTypeView (STArrow (STBase "Bool") (STBase "Token")) (STArrow (STBase "Bool") (STBase stableToken)))
              { typeViewHeadIdentities = Map.singleton stableToken tokenTypeIdentity
              }
          resultView = methodResultTypeViewFrom view

      Map.lookup "Token" (typeViewHeadIdentities resultView) `shouldBe` Just tokenTypeIdentity
      Map.lookup "Bool" (typeViewHeadIdentities resultView) `shouldBe` Nothing

    it "keeps partial arrow result head identities through display pairs" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            ( mkTypeView
                (STArrow (STBase "Bool") (STArrow (STBase "Char") (STBase "Token")))
                (STArrow (STBase "Bool") (STArrow (STBase "Char") (STBase stableToken)))
            )
              { typeViewHeadIdentities = Map.singleton stableToken tokenTypeIdentity
              }
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
                  (methodTypeViewRaw eqMethodInfo)
                    { typeViewBinderIdentities = Map.singleton "a" bodyIdentity
                    },
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
            `shouldBe` STArrow (STBase (builtinHead "Int")) (STBase (builtinHead "String"))
          Map.lookup (builtinHead "Int") heads
            `shouldBe` Just (Builtins.builtinTypeIdentity "Int")
          Map.lookup (builtinHead "String") heads
            `shouldBe` Just (Builtins.builtinTypeIdentity "String")
        other -> expectationFailure ("expected builtin ordinary value, got " ++ show other)

    it "collects mentioned type head identities through aliases" $ do
      let stableToken = symbolIdentityStableName tokenTypeIdentity
          view =
            (mkTypeView (STBase "Token") (STBase stableToken))
              { typeViewHeadIdentities = Map.singleton "Lib.Token" tokenTypeIdentity
              }
      typeViewMentionedHeadIdentities view `shouldBe` Map.keysSet (Map.singleton tokenTypeIdentity ())

    it "assigns identities to builtin opaque type parameters" $ do
      case (Map.lookup "IO" Builtins.builtinOpaqueTypes, Map.lookup "IORef" Builtins.builtinOpaqueTypes) of
        (Just DataInfo {dataTypeParams = [ioParam]}, Just DataInfo {dataTypeParams = [ioRefParam]}) -> do
          typeParamName ioParam `shouldBe` "a"
          typeParamName ioRefParam `shouldBe` "a"
          case (typeParamRef ioParam, typeParamRef ioRefParam) of
            (Just ioRef, Just ioRefRef) -> do
              resolvedTypeBinderName ioRef `shouldBe` "a"
              resolvedTypeBinderName ioRefRef `shouldBe` "a"
              ioRef `shouldNotBe` ioRefRef
            refs -> expectationFailure ("expected builtin opaque param identities, got " ++ show refs)
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

    it "compares type views by identity metadata when display names are stale" $ do
      let headName = symbolIdentityStableName tokenTypeIdentity
          binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 208)
          binderName = typeBinderIdentityStableName binderIdentity
          view displayName headKey binderKey =
            TypeView
              { typeViewDisplay = STArrow (STBase displayName) (STVar "a"),
                typeViewIdentity = STArrow (STBase headName) (STVar binderName),
                typeViewHeadIdentities = Map.singleton headKey tokenTypeIdentity,
                typeViewBinderIdentities = Map.singleton binderKey binderIdentity
              }
          staleView = view "$stale.Token" "$stale.Token" "$stale_a"
      view "Token" "Token" "a" `shouldBe` staleView
      view "Token" "Token" "a"
        `shouldNotBe` staleView {typeViewBinderIdentities = Map.empty}

    it "compares type views by carried identities when identity names are stale" $ do
      let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 209)
          view headName binderName =
            TypeView
              { typeViewDisplay = STForall "a" Nothing (STArrow (STBase "Token") (STVar "a")),
                typeViewIdentity = STForall binderName Nothing (STArrow (STBase headName) (STVar binderName)),
                typeViewHeadIdentities = Map.singleton headName tokenTypeIdentity,
                typeViewBinderIdentities = Map.singleton binderName binderIdentity
              }
          staleView = view "$stale.Token" "$stale_a"
      view "Token" "a" `shouldBe` staleView
      view "Token" "a"
        `shouldNotBe` staleView {typeViewHeadIdentities = Map.empty}

    it "does not compare type views equal when carried head payloads conflict" $ do
      let originalIdentity = generatedSymbolIdentity 210 SymbolType "Lib" "Token" Nothing
          conflictingIdentity = generatedSymbolIdentity 210 SymbolType "Other" "StaleToken" Nothing
          headName = symbolIdentityStableName originalIdentity
          view identity =
            TypeView
              { typeViewDisplay = STBase "Token",
                typeViewIdentity = STBase headName,
                typeViewHeadIdentities = Map.singleton "Token" identity,
                typeViewBinderIdentities = Map.empty
              }
      view originalIdentity `shouldNotBe` view conflictingIdentity

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

    it "compares evidence methods by symbol identity when runtime names are stale" $ do
      let methodView = methodTypeView eqMethodInfo
          evidence runtimeName symbol =
            EvidenceMethod
              { evidenceMethodRuntimeName = runtimeName,
                evidenceMethodSymbol = symbol,
                evidenceMethodResolvedVar = Nothing,
                evidenceMethodTypeView = methodView
              }
      evidence "Lib__eq" eqMethodIdentity
        `shouldBe` evidence "$stale.eq" eqMethodIdentity
      evidence "Lib__eq" eqMethodIdentity
        `shouldNotBe` evidence "Lib__eq" someCtorIdentity

    it "does not choose arbitrary evidence methods after duplicate collapse" $ do
      let methodView = methodTypeView eqMethodInfo
          conflictingView = mkTypeView (STBase "Bool") (STBase "Bool")
          evidence runtimeName view =
            EvidenceMethod
              { evidenceMethodRuntimeName = runtimeName,
                evidenceMethodSymbol = eqMethodIdentity,
                evidenceMethodResolvedVar = Nothing,
                evidenceMethodTypeView = view
              }
          first = evidence "Lib__eq_first" methodView
          duplicate = evidence "Lib__eq_second" methodView
          conflicting = evidence "Lib__eq_conflicting" conflictingView
      fmap evidenceMethodRuntimeName (uniqueEvidenceMethod [first, duplicate])
        `shouldBe` Just "Lib__eq_first"
      uniqueEvidenceMethod [first, conflicting] `shouldBe` Nothing

    it "does not choose arbitrary evidence method substitutions after duplicate collapse" $ do
      let methodView = methodTypeView eqMethodInfo
          replacement = mkTypeView (STBase "Int") (STBase "Int")
          conflictingReplacement = mkTypeView (STBase "Bool") (STBase "Bool")
          binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 210)
          binderKey = typeViewSubstKeyForIdentity binderIdentity
          evidence runtimeName =
            EvidenceMethod
              { evidenceMethodRuntimeName = runtimeName,
                evidenceMethodSymbol = eqMethodIdentity,
                evidenceMethodResolvedVar = Nothing,
                evidenceMethodTypeView = methodView
              }
          first = (evidence "Lib__eq_first", Map.singleton binderKey replacement)
          duplicate = (evidence "Lib__eq_second", Map.singleton binderKey replacement)
          conflicting = (evidence "Lib__eq_conflicting", Map.singleton binderKey conflictingReplacement)
      fmap (evidenceMethodRuntimeName . fst) (uniqueEvidenceMethodMatch [first, duplicate])
        `shouldBe` Just "Lib__eq_first"
      uniqueEvidenceMethodMatch [first, conflicting] `shouldBe` Nothing

    it "compares method infos by symbol identity when display names are stale" $ do
      eqMethodInfo
        `shouldBe` eqMethodInfo {methodDisplayName = "$stale.eq"}
      eqMethodInfo
        `shouldNotBe` eqMethodInfo {methodInfoSymbol = someCtorIdentity}

    it "uses resolved constraint metadata for method class value and instance equality" $ do
      let scope = mkElaborateScope Map.empty (Map.singleton "Token" tokenDataInfo) Map.empty []
          tokenView = sourceTypeViewInScope scope (STBase "Token")
          parsedConstraint = ClassConstraint "Eq" (STBase "Token" :| [])
          staleDisplayConstraint = ClassConstraint "$stale.Eq" (STBase "$stale.Token" :| [])
          constraintInfo =
            ConstraintInfo
              { constraintDisplayClass = "Eq",
                constraintClassSymbol = eqClassIdentity,
                constraintTypeViews = tokenView :| []
              }
          paramIdentity = typeBinderIdentityFromUnique (UniqueIdentity 211)
          methodWith displays infos paramName =
            eqMethodInfo
              { methodConstraints = displays,
                methodConstraintInfos = infos,
                methodParamBinders = (paramName, paramIdentity) :| []
              }
          classWith displays infos =
            eqClassInfo
              { classSuperclasses = displays,
                classSuperclassInfos = infos
              }
          valueWith runtimeName displays infos =
            OrdinaryValue
              { valueRuntimeName = runtimeName,
                valueInfoSymbol = valueInfoIdentity,
                valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
                valueConstraints = displays,
                valueConstraintInfos = infos
              }
          instanceOrigin = generatedSymbolIdentity 135 SymbolModule "Lib" "Lib" Nothing
          instanceWith displays infos =
            InstanceInfo
              { instanceClassSymbol = eqClassIdentity,
                instanceOriginModuleIdentity = instanceOrigin,
                instanceConstraints = displays,
                instanceConstraintInfos = infos,
                instanceHeadTypeViews = tokenView :| [],
                instanceMethodsByIdentity = Map.empty
              }
          displayConstraints = [parsedConstraint]
          staleDisplayConstraints = [staleDisplayConstraint]
          constraintInfos = [constraintInfo]
      methodWith displayConstraints constraintInfos "a"
        `shouldBe` methodWith staleDisplayConstraints constraintInfos "$stale_a"
      methodWith displayConstraints constraintInfos "a"
        `shouldNotBe` methodWith staleDisplayConstraints [] "$stale_a"
      classWith displayConstraints constraintInfos
        `shouldBe` classWith staleDisplayConstraints constraintInfos
      valueWith "Lib__answer" displayConstraints constraintInfos
        `shouldBe` valueWith "$stale.answer" staleDisplayConstraints constraintInfos
      instanceWith displayConstraints constraintInfos
        `shouldBe` instanceWith staleDisplayConstraints constraintInfos

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

    it "compares deferred constructor inst binders by identity when names are stale" $ do
      let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 212)
          otherBinderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 213)
          deferredRef = deferredRefFromIdentity (UniqueIdentity 214) "Some"
          call binderName identity =
            DeferredConstructorCall
              { deferredConstructorRef = deferredRef,
                deferredConstructorInfo = someCtor,
                deferredConstructorArgCount = 0,
                deferredConstructorSourceType = STBase "Token",
                deferredConstructorOccurrenceType = STBase "Token",
                deferredConstructorTypeHeadIdentities = Map.empty,
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
                constructorShapeForallBinderInfo = [],
                constructorShapeIndex = 0,
                constructorShapeOwnerTypeParams = []
              }
          staleShape =
            shape
              { constructorShapeRuntimeName = "$stale.Some",
                constructorShapeTypeView = (constructorShapeTypeView shape) {typeViewDisplay = STBase "$stale.Token"}
              }
          staleCtor =
            someCtor
              { ctorRuntimeName = "$stale.Some",
                ctorTypeView = (ctorTypeView someCtor) {typeViewDisplay = STBase "$stale.Token"}
              }
      shape `shouldBe` staleShape
      shape `shouldNotBe` staleShape {constructorShapeSymbol = higherCtorIdentity}
      someCtor `shouldBe` staleCtor
      someCtor `shouldNotBe` staleCtor {ctorInfoSymbol = higherCtorIdentity}

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
          qualifiedMethodIdentity = loweredBindingIdentityFromValueInfo qualifiedEqMethodValue

      loweredIdentityRuntimeName qualifiedValueIdentity `shouldBe` "Lib__answer"
      loweredIdentityDetails qualifiedValueIdentity
        `shouldBe` TopLevelId valueInfoIdentity
      loweredIdentityRuntimeName qualifiedMethodIdentity `shouldBe` "eq"
      loweredIdentityDetails qualifiedMethodIdentity
        `shouldBe` MethodId (methodInfoSymbolIdentity qualifiedEqMethodInfo)

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
      valueConstraints = [],
      valueConstraintInfos = []
    }

mainValueInfo :: ValueInfo
mainValueInfo =
  OrdinaryValue
    { valueInfoSymbol = mainValueIdentity,
      valueRuntimeName = "Main__main",
      valueTypeView = mkTypeView (STBase "Int") (STBase "Int"),
      valueConstraints = [],
      valueConstraintInfos = []
    }

someCtor :: ConstructorInfo
someCtor =
  ConstructorInfo
    { ctorInfoSymbol = someCtorIdentity,
      ctorRuntimeName = "Lib__Some",
      ctorTypeView = mkTypeView (STBase "Token") (STBase "Lib.Token"),
      ctorForallBinderInfo = [],
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
        mkTypeView
          ( STArrow
              (STVarApp "f" (STVar "a" :| []))
              (STCon "Higher" (STVar "f" :| [STVar "a"]))
          )
          ( STArrow
              (STVarApp "f" (STVar "a" :| []))
              (STCon "Lib.Higher" (STVar "f" :| [STVar "a"]))
          ),
      ctorForallBinderInfo = [],
      ctorOwningTypeIdentity = higherTypeIdentity,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

higherDataInfo :: DataInfo
higherDataInfo =
  DataInfo
    { dataInfoSymbol = higherTypeIdentity,
      dataTypeParams =
        [ ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 110) "f") KType,
          ResolvedTypeParam (resolvedTypeBinderRef (UniqueIdentity 111) "a") KType
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
      methodConstraints = [],
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
      classSuperclasses = [],
      classSuperclassInfos = [],
      classFunctionalDependencies = [],
      classMethodsByIdentity = Map.singleton (methodInfoSymbolIdentity eqMethodInfo) eqMethodInfo
    }

qualifiedEqClassInfo :: ClassInfo
qualifiedEqClassInfo =
  ClassInfo
    { classInfoSymbol = eqClassIdentity,
      classTypeParams = eqParam :| [],
      classSuperclasses = [],
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
