{-# LANGUAGE GADTs #-}

module ResolvedSymbolSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import MLF.Frontend.Program.Elaborate (lowerType, mkElaborateScope)
import MLF.Frontend.Program.Types
import MLF.Frontend.Syntax.Program (ResolvedExportTypeRef (..))
import MLF.Frontend.Symbol (symbolIdentityStableName)
import MLF.Frontend.Syntax
  ( ResolvedSrcTy (..),
    ResolvedTypeBinderRef (..),
    SrcBound (..),
    SrcTy (..),
    SrcType,
    firstOrderTypeParam,
    resolvedSrcTypeIdentityType,
  )
import MLF.Types.Identity (UniqueIdentity (..))
import Test.Hspec

generatedSymbolIdentity ::
  Int ->
  SymbolNamespace ->
  String ->
  String ->
  Maybe SymbolOwnerIdentity ->
  SymbolIdentity
generatedSymbolIdentity unique namespace moduleName name owner =
  SymbolIdentity (UniqueIdentity unique) namespace moduleName name owner

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
        `shouldBe` SymbolIdentity (UniqueIdentity 10) SymbolModule "Lib" "Lib" Nothing
      symbolDisplayName (resolvedSymbolSpelling importedModule) `shouldBe` "L"

    it "compares symbol identities by generated identity" $ do
      let first =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 1) SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")
          firstAlias =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 1) SymbolValue "Other" "stale-x" Nothing)
              "Main.x"
              "Main.x"
              (SymbolQualifiedImport "Main" "Main")
          second =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 2) SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")

      sameResolvedSymbol first firstAlias `shouldBe` True
      sameResolvedSymbol first second `shouldBe` False
      Map.lookup (resolvedSymbolIdentity firstAlias) (Map.singleton (resolvedSymbolIdentity first) "hit")
        `shouldBe` Just "hit"

    it "uses semantic identity for resolved symbol and reference equality" $ do
      let first =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 901) SymbolValue "Main" "x" Nothing)
              "x"
              "x"
              (SymbolLocal "Main")
          firstAlias =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 901) SymbolValue "Other" "stale-x" Nothing)
              "Other.x"
              "Other.x"
              (SymbolQualifiedImport "Other" "Other")
          firstRef = ResolvedReference ResolvedValueReference "x" first
          firstAliasRef = ResolvedReference ResolvedValueReference "Other.x" firstAlias

      firstAlias `shouldBe` first
      Map.lookup firstAlias (Map.singleton first "hit") `shouldBe` Just "hit"
      firstAliasRef `shouldBe` firstRef
      Map.lookup firstAliasRef (Map.singleton firstRef "hit") `shouldBe` Just "hit"

    it "uses semantic identity for resolved export type references" $ do
      let typeUnqualified = resolvedDataInfoSymbol (SymbolUnqualifiedImport "Lib") "Token" tokenDataInfo
          typeQualified = resolvedDataInfoSymbol (SymbolQualifiedImport "Lib" "L") "L.Token" tokenDataInfo
          unqualifiedRef = ResolvedExportTypeRef "Token" [typeUnqualified]
          qualifiedRef = ResolvedExportTypeRef "L.Token" [typeQualified]

      qualifiedRef `shouldBe` unqualifiedRef

    it "exposes generated stable names for identity aliases" $ do
      let typeSymbol =
            mkResolvedSymbol
              (SymbolIdentity (UniqueIdentity 42) SymbolType "Lib" "Token" Nothing)
              "Token"
              "Token"
              (SymbolLocal "Lib")

      symbolIdentityStableName (resolvedSymbolIdentity typeSymbol) `shouldBe` "$identity#42"

    it "keeps same-named type binders distinct in identity types" $ do
      let outer = ResolvedTypeBinderRef (UniqueIdentity 201) "a"
          inner = ResolvedTypeBinderRef (UniqueIdentity 202) "a"
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
      valueType = STBase "Int",
      valueIdentityType = STBase "Int",
      valueConstraints = [],
      valueConstraintInfos = []
    }

mainValueInfo :: ValueInfo
mainValueInfo =
  OrdinaryValue
    { valueInfoSymbol = mainValueIdentity,
      valueRuntimeName = "Main__main",
      valueType = STBase "Int",
      valueIdentityType = STBase "Int",
      valueConstraints = [],
      valueConstraintInfos = []
    }

someCtor :: ConstructorInfo
someCtor =
  ConstructorInfo
    { ctorInfoSymbol = someCtorIdentity,
      ctorRuntimeName = "Lib__Some",
      ctorType = STBase "Token",
      ctorTypeIdentity = STBase "Lib.Token",
      ctorForalls = [],
      ctorForallBinderIdentities = [],
      ctorArgs = [],
      ctorResult = STBase "Token",
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
      ctorType =
        STArrow
          (STVarApp "f" (STVar "a" :| []))
          (STCon "Higher" (STVar "f" :| [STVar "a"])),
      ctorTypeIdentity =
        STArrow
          (STVarApp "f" (STVar "a" :| []))
          (STCon "Lib.Higher" (STVar "f" :| [STVar "a"])),
      ctorForalls = [],
      ctorForallBinderIdentities = [],
      ctorArgs = [STVarApp "f" (STVar "a" :| [])],
      ctorResult = STCon "Higher" (STVar "f" :| [STVar "a"]),
      ctorOwningTypeIdentity = higherTypeIdentity,
      ctorIndex = 0,
      ctorOwnerConstructors = []
    }

higherDataInfo :: DataInfo
higherDataInfo =
  DataInfo
    { dataInfoSymbol = higherTypeIdentity,
      dataTypeParams = [firstOrderTypeParam "f", firstOrderTypeParam "a"],
      dataConstructors = [higherCtor]
    }

eqMethodInfo :: MethodInfo
eqMethodInfo =
  MethodInfo
    { methodInfoSymbol = eqMethodIdentity,
      methodType = STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")),
      methodTypeIdentity = STArrow (STVar "a") (STArrow (STVar "a") (STBase "Bool")),
      methodTypeBinderIdentities = Map.empty,
      methodConstraints = [],
      methodConstraintInfos = [],
      methodParamNames = "a" :| [],
      methodParamIdentityNames = "a" :| [],
      methodParamBinderIdentities = Nothing :| []
    }

qualifiedEqMethodInfo :: MethodInfo
qualifiedEqMethodInfo =
  eqMethodInfo

eqClassInfo :: ClassInfo
eqClassInfo =
  ClassInfo
    { classInfoSymbol = eqClassIdentity,
      classTypeParams = firstOrderTypeParam "a" :| [],
      classSuperclasses = [],
      classSuperclassInfos = [],
      classFunctionalDependencies = [],
      classMethodsByIdentity = Map.singleton (methodInfoSymbolIdentity eqMethodInfo) eqMethodInfo
    }

qualifiedEqClassInfo :: ClassInfo
qualifiedEqClassInfo =
  ClassInfo
    { classInfoSymbol = eqClassIdentity,
      classTypeParams = firstOrderTypeParam "a" :| [],
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
